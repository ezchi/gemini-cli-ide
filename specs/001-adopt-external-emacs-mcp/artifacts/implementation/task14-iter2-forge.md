# Task 14: Rewrite README.md — Forge Iteration 2

Addresses gauge iter-1 BLOCKING: two missing FR-20 / FR-16 items.

## Files Changed
- `README.md` — modified (Installation section).
- `gemini-cli-ide.el` — modified (Commentary block).

## Changes from Iter-1
1. **README Installation section** — added the missing
   "`emacs-mcp` is not yet on MELPA; submission is tracked upstream
   as future work" note. FR-20 explicitly requires this.
2. **gemini-cli-ide.el Commentary** — added the AGPL-§13
   combined-work disclosure paragraph. FR-16 requires this notice
   in BOTH README and Commentary; iter-1 had it only in README.

## Build verification
- `./scripts/compile-and-test.sh`: byte-compile PASSED, ERT all
  green (56 tests, 51 expected pass, 5 pre-existing skips).

## Notes for the gauge
- An autonomous agent overwrote `task14-iter1-forge.md` and
  `task14-iter1-gauge-prompt.md` between the iter-1 gauge run and
  this iter-2 forge. The overwrites reverted some honesty
  (re-introduced a "mouse clicks" claim that the spec never
  mentioned and that the iter-1 forge had explicitly removed). The
  current forge artifact (`task14-iter2-forge.md` — this file) is
  the canonical record of iter-2's work. The iter-1 commit on the
  branch contains the actual real iter-1 edits irrespective of
  what the now-overwritten artifact says.
