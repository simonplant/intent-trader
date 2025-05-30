---
title: Trading System Charter  
description: Defines Simon Plant’s discretionary trading framework for capital allocation, risk limits, sizing tiers, and behavioral guardrails  
tags: [system, control, behavior]  
author: Simon Plant  
last_updated: 2025-05-05  
version: 1.5  
category: system  
usage: Reference before market open and during prompt design. Produces grounding in structural capital, sizing, and discipline. Consumes behavior framework and capital profile.
status: stable  
requires: [trading-capital-profile.md]  
linked_outputs: [trading-system-sop.md]  
input_format: markdown  
output_format: markdown  
ai_enabled: true  
---

# TRADING SYSTEM CHARTER — SIMON PLANT

## I. PURPOSE

To execute a repeatable, structured discretionary trading process that preserves capital, captures asymmetric risk:reward, and grows edge over time through continuous behavioral feedback.

---

## II. CAPITAL + RISK FRAMEWORK

All values referenced below are defined in `trading-capital-profile.md`.

- `base_capital`  
- `default_trade_size_dollars` (1/3 account)  
- `max_single_trade_size_dollars` (2/3 account max cap)  
- `max_exposure_dollars` (total capital in play)  
- `daily_loss_soft_dollars` (reset trigger)  
- `daily_loss_hard_dollars` (full stop)  
- `max_options_trade_dollars` (SPX and lotto cap)  
- `swing_trade_risk_budget_dollars` and `scalp_trade_risk_budget_dollars`  

Position construction should follow `position_building_rule` — tier into size unless scalping or trading for immediate trims.

---

## III. POSITION SIZING TIERS

- **Tier 1: High Conviction Swing**  
  Build toward `default_trade_size_dollars` or up to `max_single_trade_size_dollars` for best ideas with conviction, structure, and moderator alignment

- **Tier 2: Cashflow / Structured Intraday**  
  Use `scalp_trade_risk_budget_dollars` or partial sizing depending on stop distance and expected speed

- **Tier 3: Lotto / Speculative**  
  Cap at `max_options_trade_dollars`; requires catalyst or binary trigger. No averaging.

---

## IV. BEHAVIORAL GUARDRAILS

- If drawdown > `daily_loss_soft_dollars`, trigger `midday-reset.md`  
- If drawdown > `daily_loss_hard_dollars` or 3R loss, exit all trades and lock out  
- Check `trading-behaviors-kb.md` each morning  
- Score each trade (setup, execution, emotion) in `generate-daily-trade-log.md`

Behavioral flags override all technical setups.

---

## V. PROCESS ENFORCEMENT

- All entries must pass `copilot.md` filters  
- No trading outside of Unified Trade Plan or Inner Circle structure  
- Use `capital-exposure-tracker.md` to verify exposure is within limits  
- If reset triggered, follow journaling protocol before next trade

---

## VI. ALIGNMENT WITH INNER CIRCLE

- Mirror moderator structure, not just sentiment  
- Wait for price/level alignment + conviction  
- Require at least two signals or confirmation from DP/Mancini before big tier sizing

---

## VII. REVIEW CYCLE

- Daily: Run `generate-daily-trade-log.md` at EOD  
- Weekly: Reset plan each Sunday with fresh level mapping  
- Midweek: Reflect after Wednesday’s close using `midday-reset.md` if needed  
- Monthly: Audit behavioral KPIs and trade stats

---

## VIII. NON-NEGOTIABLES

- If this Charter is violated, all trading is paused  
- Refactor the system before returning to risk  
- Do not chase. Do not force. Follow the structure.

---