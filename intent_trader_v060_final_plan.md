# Intent Trader v0.6.0 — Final Implementation Plan

This plan outlines the full set of changes required to complete the migration from the 900-line legacy `intent_trader.py` to the optimized, requirement-compliant, and complete v0.6.0 refactor.

## ✅ Completed Features
- Positions handler: `add`, `trim`, `exit` support with freeform notes
- Journal handler: `append`, `search`, `date filter`, `last 5`
- DP & Mancini analyzers using language maps
- Intent registration and routing with alias support
- Plan creation from raw text inputs
- Manual trade additions
- Save/load context to/from JSON
- Performance summary and filtered plan
- Output and reset

## 🔧 Remaining Changes To Complete

### 1. Tag Outcome Enhancements
- Support flexible trade lookup (case-insensitive)
- Allow tagging with additional notes
- Auto-journal each tag

### 2. Coach Module Overhaul
- Warn on:
  - 3+ losses in a row
  - Overtrading (>10 trades)
  - Skipping focus trades
  - Repeating avoid-rated setups

### 3. Review and Performance Stratification
- Add breakdown by source (DP vs Mancini)
- Group by score bucket and compute win rates
- Stratify outcomes by setup type

### 4. Simulate
- Allow scenario modeling with mock intents
- Auto-generate plan + review from samples

### 5. CLI Mode Control
- Add `mode: SIM`, `mode: LIVE`, `mode: REVIEW` toggles

### 6. Full Logging + PnL Sum
- Update journal and plan entries with profit/loss tracking
- Auto-sum total and per-score-bucket PnL

### 7. Cleanup + UX Polish
- Replace all magic strings with constants
- Docstrings for all handlers
- Clear and predictable routing behavior

## Estimated Final LOC
~290 lines of well-structured code.
