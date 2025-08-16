#!/bin/bash

# Script to record WebSocket messages between VS Code and Gemini Code
# Uses websocat to create a proxy and log all messages
#
# Usage: ./record-gemini-messages.sh [working_directory]
#
# Optional argument:
#   working_directory - Directory to run Gemini Code in (defaults to current directory)

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check for help flag
if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    echo "Usage: $0 [working_directory]"
    echo ""
    echo "Record WebSocket messages between VS Code and Gemini Code"
    echo ""
    echo "Arguments:"
    echo "  working_directory  Optional directory to run Gemini Code in (defaults to current directory)"
    echo ""
    echo "Logs will be saved to: $(dirname "$0")/gemini_logs/"
    exit 0
fi

# Get optional working directory parameter
GEMINI_WORKING_DIR="${1:-$(pwd)}"

# Validate working directory
if [ ! -d "$GEMINI_WORKING_DIR" ]; then
    echo -e "${RED}Error: Directory '$GEMINI_WORKING_DIR' does not exist${NC}"
    exit 1
fi

# Convert to absolute path
GEMINI_WORKING_DIR=$(cd "$GEMINI_WORKING_DIR" && pwd)

# Store the script's directory for logs
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Check if websocat is installed
if ! command -v websocat &> /dev/null; then
    echo -e "${RED}Error: websocat is not installed${NC}"
    echo "Install it with: cargo install websocat"
    echo "Or on macOS: brew install websocat"
    exit 1
fi

# Check if gemini is installed
if ! command -v gemini &> /dev/null; then
    echo -e "${RED}Error: gemini command not found${NC}"
    echo "Make sure Gemini Code CLI is installed and in PATH"
    exit 1
fi

# Find VS Code lockfile
LOCKFILE_DIR="$HOME/.gemini/ide"
if [ ! -d "$LOCKFILE_DIR" ]; then
    echo -e "${RED}Error: Gemini IDE directory not found at $LOCKFILE_DIR${NC}"
    exit 1
fi

# List all lockfiles
ls -la "$LOCKFILE_DIR"/*.lock >/dev/null 2>&1

# Find VS Code lockfile by checking the content for "Visual Studio Code"
VSCODE_LOCKFILE=""
for lockfile in "$LOCKFILE_DIR"/*.lock; do
    if [ -f "$lockfile" ] && grep -q "Visual Studio Code" "$lockfile" 2>/dev/null; then
        VSCODE_LOCKFILE="$lockfile"
        break
    fi
done

if [ -z "$VSCODE_LOCKFILE" ]; then
    echo -e "${RED}Error: No VS Code lockfile found in $LOCKFILE_DIR${NC}"
    echo "Make sure VS Code with Gemini Code is running"
    exit 1
fi

# Extract port from filename
PORT=$(basename "$VSCODE_LOCKFILE" .lock)

if ! [[ "$PORT" =~ ^[0-9]+$ ]]; then
    echo -e "${RED}Error: Invalid port number: $PORT${NC}"
    exit 1
fi

echo -e "${GREEN}Found VS Code Gemini Code on port: $PORT${NC}"

# Read the original lockfile to get workspace info
LOCKFILE_CONTENT=$(cat "$VSCODE_LOCKFILE")

# Create logs directory in the script's directory
LOG_DIR="$SCRIPT_DIR/gemini_logs"
mkdir -p "$LOG_DIR"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
LOG_FILE="$LOG_DIR/gemini_messages_${TIMESTAMP}.log"
JSON_FILE="$LOG_DIR/gemini_messages_${TIMESTAMP}.jsonl"

# Proxy port (use a high port to avoid conflicts, but stay under 65535)
PROXY_PORT=$((PORT + 1000))

# Create a new lockfile for the proxy
PROXY_LOCKFILE="$LOCKFILE_DIR/${PROXY_PORT}.lock"

# Create lockfile for proxy with same content as VS Code but different PID
echo "$LOCKFILE_CONTENT" | jq ".pid = $$" > "$PROXY_LOCKFILE"

echo -e "${GREEN}Created proxy lockfile at: $PROXY_LOCKFILE${NC}"

# Silently check lockfiles
ls -la "$LOCKFILE_DIR"/*.lock 2>/dev/null | grep -v ".disabled" >/dev/null
echo -e "${YELLOW}Setting up proxy on port $PROXY_PORT${NC}"
echo -e "${YELLOW}Logs will be saved to:${NC}"
echo "  - Raw log: $LOG_FILE"
echo "  - JSON only: $JSON_FILE"
echo ""

# Global variables for PIDs
CLIENT_PID=""
SERVER_PID=""

# Function to extract JSON from websocat output
extract_json() {
    while IFS= read -r line; do
        echo "$line" >> "$LOG_FILE"

        # Try to extract JSON from the line
        # websocat format: [timestamp] Direction JSON
        if [[ "$line" =~ \{.*\} ]]; then
            # Extract JSON part
            json="${BASH_REMATCH[0]}"
            # Pretty print and save to JSON file
            echo "$json" | jq '.' 2>/dev/null >> "$JSON_FILE" || echo "$json" >> "$JSON_FILE"
            echo "---" >> "$JSON_FILE"
        fi
    done
}

# Start websocat proxy first
echo -e "${GREEN}Starting proxy on port $PROXY_PORT...${NC}"

# Create a temp file to store the websocat PID
WEBSOCAT_PID_FILE=$(mktemp)

# Test websocat command
websocat --version >/dev/null 2>&1

# Create FIFOs for bidirectional communication
FIFO_DIR=$(mktemp -d)
mkfifo "$FIFO_DIR/up" "$FIFO_DIR/down"
if [ ! -p "$FIFO_DIR/up" ] || [ ! -p "$FIFO_DIR/down" ]; then
    echo -e "${RED}Error: Failed to create FIFOs${NC}"
    exit 1
fi

# Start the proxy components
echo -e "${YELLOW}Starting WebSocket proxy with message logging...${NC}"

# Component 1: Listen for client connections and forward to upstream
{ websocat --text -E ws-l:127.0.0.1:$PROXY_PORT - <"$FIFO_DIR/down" | \
while IFS= read -r line; do
    timestamp=$(date '+%Y-%m-%d %H:%M:%S.%3N')
    echo "[$timestamp] [C->S] $line" >> "$LOG_FILE"
    if echo "$line" | grep -qE '^\{.*\}$'; then
        echo "{\"timestamp\": \"$timestamp\", \"direction\": \"C->S\", \"message\": $(echo "$line" | jq -c .)}" >> "$JSON_FILE" 2>/dev/null || true
    fi
    echo "$line"
done >"$FIFO_DIR/up"; } 2>"${LOG_DIR}/websocat_client_error.log" &
CLIENT_PID=$!

# Component 2: Connect to upstream and forward to client
{ websocat --text - ws://127.0.0.1:$PORT/ <"$FIFO_DIR/up" | \
while IFS= read -r line; do
    timestamp=$(date '+%Y-%m-%d %H:%M:%S.%3N')
    echo "[$timestamp] [S->C] $line" >> "$LOG_FILE"
    if echo "$line" | grep -qE '^\{.*\}$'; then
        echo "{\"timestamp\": \"$timestamp\", \"direction\": \"S->C\", \"message\": $(echo "$line" | jq -c .)}" >> "$JSON_FILE" 2>/dev/null || true
    fi
    echo "$line"
done >"$FIFO_DIR/down"; } 2>"${LOG_DIR}/websocat_server_error.log" &
SERVER_PID=$!

# Store the PIDs
echo "$CLIENT_PID" > "$WEBSOCAT_PID_FILE"
WEBSOCAT_PID=$CLIENT_PID

# Also track the FIFO directory for cleanup
echo "$FIFO_DIR" > "${WEBSOCAT_PID_FILE}.fifo"

# Give it a moment to write the PID and start
sleep 1

# Read the actual websocat PID
if [ -f "$WEBSOCAT_PID_FILE" ]; then
    WEBSOCAT_PID=$(cat "$WEBSOCAT_PID_FILE")
    rm -f "$WEBSOCAT_PID_FILE"
fi

# Give the proxy a moment to start
sleep 2

# Check both processes
if ! kill -0 $CLIENT_PID 2>/dev/null; then
    echo -e "${RED}Error: Client listener process (PID $CLIENT_PID) died immediately${NC}"
    if [ -f "${LOG_DIR}/websocat_client_error.log" ] && [ -s "${LOG_DIR}/websocat_client_error.log" ]; then
        echo -e "${RED}Client error log:${NC}"
        cat "${LOG_DIR}/websocat_client_error.log"
    fi
    # Check if port is already in use
    if lsof -i:$PROXY_PORT >/dev/null 2>&1; then
        echo -e "${RED}Port $PROXY_PORT is already in use${NC}"
    fi
    exit 1
fi

if ! kill -0 $SERVER_PID 2>/dev/null; then
    echo -e "${RED}Error: Server connector process (PID $SERVER_PID) died immediately${NC}"
    if [ -f "${LOG_DIR}/websocat_server_error.log" ] && [ -s "${LOG_DIR}/websocat_server_error.log" ]; then
        echo -e "${RED}Server error log:${NC}"
        cat "${LOG_DIR}/websocat_server_error.log"
    fi
    exit 1
fi

echo -e "${GREEN}Proxy started successfully${NC}"

# Launch Gemini Code with proper environment variables
echo -e "${GREEN}Launching Gemini Code...${NC}"
if [ "$GEMINI_WORKING_DIR" != "$(pwd)" ]; then
    echo -e "${YELLOW}Working directory: $GEMINI_WORKING_DIR${NC}"
fi

# Export the variables to ensure they're passed to gemini
export GEMINI_CLI_SSE_PORT=$PROXY_PORT
export ENABLE_IDE_INTEGRATION=true
export TERM_PROGRAM=vscode
export FORCE_CODE_TERMINAL=true

# Launch gemini in the specified directory
(cd "$GEMINI_WORKING_DIR" && gemini) &
GEMINI_PID=$!

echo -e "${GREEN}Gemini Code started${NC}"

echo "Press Ctrl+C to stop recording and exit Gemini Code"
echo ""

# Cleanup function
cleanup() {
    echo -e "\n${YELLOW}Cleaning up...${NC}"

    # Kill Gemini Code if we started it
    if [ ! -z "$GEMINI_PID" ] && kill -0 $GEMINI_PID 2>/dev/null; then
        echo -e "${YELLOW}Stopping Gemini Code...${NC}"
        kill $GEMINI_PID 2>/dev/null || true
        wait $GEMINI_PID 2>/dev/null || true
    fi

    # Kill websocat proxies
    if [ ! -z "$CLIENT_PID" ] && kill -0 $CLIENT_PID 2>/dev/null; then
        echo -e "${YELLOW}Stopping client proxy (PID: $CLIENT_PID)...${NC}"
        kill -TERM $CLIENT_PID 2>/dev/null || true
        sleep 0.5
        # Force kill if still running
        if kill -0 $CLIENT_PID 2>/dev/null; then
            kill -KILL $CLIENT_PID 2>/dev/null || true
        fi
    fi

    if [ ! -z "$SERVER_PID" ] && kill -0 $SERVER_PID 2>/dev/null; then
        echo -e "${YELLOW}Stopping server proxy (PID: $SERVER_PID)...${NC}"
        kill -TERM $SERVER_PID 2>/dev/null || true
        sleep 0.5
        # Force kill if still running
        if kill -0 $SERVER_PID 2>/dev/null; then
            kill -KILL $SERVER_PID 2>/dev/null || true
        fi
    fi

    # Also kill any websocat processes listening on our proxy port
    WEBSOCAT_PIDS=$(lsof -t -i :$PROXY_PORT 2>/dev/null | grep -v "^$CLIENT_PID$" | grep -v "^$SERVER_PID$" || true)
    if [ ! -z "$WEBSOCAT_PIDS" ]; then
        echo -e "${YELLOW}Stopping remaining proxy processes...${NC}"
        kill -TERM $WEBSOCAT_PIDS 2>/dev/null || true
        sleep 0.5
        kill -KILL $WEBSOCAT_PIDS 2>/dev/null || true
    fi

    # Clean up FIFOs
    if [ -f "${WEBSOCAT_PID_FILE}.fifo" ]; then
        FIFO_DIR=$(cat "${WEBSOCAT_PID_FILE}.fifo")
        rm -rf "$FIFO_DIR" 2>/dev/null || true
        rm -f "${WEBSOCAT_PID_FILE}.fifo"
    fi

    # Kill monitor if it exists
    if [ ! -z "$MONITOR_PID" ] && kill -0 $MONITOR_PID 2>/dev/null; then
        kill -TERM $MONITOR_PID 2>/dev/null || true
    fi

    # Kill the pipeline process
    if [ ! -z "$PIPELINE_PID" ] && kill -0 $PIPELINE_PID 2>/dev/null; then
        kill -TERM $PIPELINE_PID 2>/dev/null || true
    fi

    # Remove lockfile
    rm -f "$PROXY_LOCKFILE"
    echo -e "${GREEN}Proxy lockfile removed${NC}"

    echo -e "${GREEN}Cleanup complete. Logs saved to $LOG_DIR/${NC}"
    exit 0
}

# Set up trap to cleanup on exit
trap cleanup EXIT INT TERM

# Wait for either process to exit
wait -n $GEMINI_PID $PIPELINE_PID

# If one exits, we'll clean up both in the trap handler
