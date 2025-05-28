# Intent Trader IAA Requirements & Unified Trading Language

**Version:** 1.1  
**Last Updated:** [YYYY-MM-DD]  
**Scope:** Unified PFEMRC Trading Workflow (PLAN → FOCUS → EXECUTE → MANAGE → REVIEW → COACH) for DP/Inner Circle + Mancini Blueprint systems.

---

## Table of Contents

1. System Purpose & MVP Criteria
2. Supported Trading Systems
3. PFEMRC Workflow: Phases, Intents, and Outputs
4. Unified Trading Language & Vocabulary
5. Phase-by-Phase Handler/Intent Reference
6. Example Conversation Flow

---

## 1. System Purpose & MVP Criteria

**Purpose:**  
Deliver a single, intent-driven trading assistant for solo traders that integrates:
- Analyst conviction (DP/Inner Circle) and technical blueprint (Mancini)
- Complete PFEMRC lifecycle: PLAN, FOCUS, EXECUTE, MANAGE, REVIEW, COACH
- Direct, actionable trading language mapped to your workflow

**MVP Criteria:**
- One-file, handler-based codebase (no frameworks, no bloat)
- All phases and outputs align to the vocabulary of BOTH systems
- Language and logic are tuned to modern trading practice
- Real-time and EOD feedback loops, including live COACH interventions

---

## 2. Supported Trading Systems

| System              | Strengths & Needs                                        |
|---------------------|---------------------------------------------------------|
| DP/Inner Circle     | Focus/conviction trade stack, explicit bias/levels, daily plan, confidence language, position sizing, emotional cues, post-call review, behavioral insights |
| Mancini Blueprint   | Futures/ES/SPX technical levels, blueprint setups (failed breakdown, back-test, support/resistance), confirmatory triggers, technical pattern bias, translation to SPX for options, discipline around triggers/levels |

---

## 3. PFEMRC Workflow: Phases, Intents, Outputs

### **PLAN**  
- *Intent/Action*: Ingest, clean, and summarize DP and Mancini data; assess bias, consensus/divergence, macro/news
- *Outputs*: 
    - Unified market bias
    - Stack of key levels (SPX/ES)
    - Focus/no-trade list
    - Macro drivers, consensus, conviction

---

### **FOCUS**  
- *Intent/Action*: Identify highest edge setups from both systems, grade them, filter for playbook fit, set alerts
- *Outputs*:
    - Stack-ranked setups (DP “focus trade”, Mancini “blueprint primary”)
    - Setup grading (A+, B, trap, lotto, etc.)
    - Required confirmations/triggers
    - Playbook filter (is this my edge?)

---

### **EXECUTE**  
- *Intent/Action*: Structure, place, and size trades per both conviction (DP) and technical (Mancini) logic
- *Outputs*:
    - Trade tickets (symbol, entry, size, type)
    - Stop/target details, confirmation notes
    - Execution rationale (“focus trade”, “blueprint test”)
    - Immediate feedback: fills, slippage, chasing

---

### **MANAGE**  
- *Intent/Action*: Monitor trades, adjust stops, trim/add, respond to news/level breaks, manage risk as trade evolves
- *Outputs*:
    - Scale/trim/add logs
    - Alerts for stop moves, profit taking, risk changes
    - Adherence to system rules (max loss, no add after X, correlation caps)
    - Reversal detection, character shifts

---

### **REVIEW**  
- *Intent/Action*: EOD (or intraday) analysis of plan vs. execution, emotional review, system alignment check
- *Outputs*:
    - Trade log (plan vs actual, system alignment, size, emotion)
    - Stats (win/loss, average P&L, missed setups, thesis drift)
    - Improvement points for next session

---

### **COACH**  
- *Intent/Action*: Real-time and EOD behavioral pattern detection, intervention, accountability, performance truth
- *Outputs*:
    - Intraday pattern warnings (“You’re overtrading after stops”)
    - Behavioral stats and “truth bombs” (e.g. win rate by time/setup/emotion)
    - Custom prescriptions (“Half size after loss”, “No trades after 3 stops”)
    - End-of-week/month: pattern summary, habit tracking

---

## 4. Unified Trading Language & Vocabulary

| DP/Inner Circle Terms            | Mancini Blueprint Terms         | Unified Language (IAA)            |
|----------------------------------|-------------------------------|-----------------------------------|
| Focus trade, top idea            | Blueprint primary setup        | Focus setup, A+ setup             |
| Conviction score (0-1), aggressive buy/sell | Level test, failed breakdown, confirmation trigger | Conviction/edge, technical trigger|
| Levels: support, resistance, inflection | ES/SPX key levels, blueprint, back-test | Unified key levels                |
| Full/test/scale size             | Starter, scale-in/out, risk-on/off | Position sizing, add/trim         |
| Plan vs. execution, alignment    | Execution vs. blueprint, rule compliance | Plan vs. reality, system alignment|
| Emotional state, behavioral feedback | N/A (but can add)               | COACH: pattern, emotion, discipline|

---

## 5. Phase-by-Phase Handler/Intent Reference

### PLAN
- `analyze_dp_call`
- `summarize_mancini_newsletter`
- `extract_key_levels`
- `generate_daily_plan`

### FOCUS
- `filter_a_plus_setups`
- `grade_setup_quality`
- `identify_focus_trade`
- `prioritize_blueprint_setup`
- `confirm_playbook_alignment`

### EXECUTE
- `place_trade_order`
- `size_position_by_conviction`
- `apply_blueprint_entry_rules`
- `record_execution_notes`

### MANAGE
- `monitor_position`
- `adjust_stop_loss`
- `trim_or_add_position`
- `respond_to_level_breaks`
- `manage_risk_compliance`

### REVIEW
- `log_trade_result`
- `compare_plan_vs_execution`
- `analyze_emotional_state`
- `generate_review_report`

### COACH
- `detect_behavioral_pattern`
- `real_time_intervention`
- `generate_performance_truth`
- `prescribe_behavior_change`
- `track_habit_progression`

---

## 6. Example Conversation Flow

**Morning (PLAN):**
- "What are DP’s focus trades and conviction levels?"
- "List Mancini’s blueprint levels for ES/SPX."
- "Where do DP and Mancini disagree today?"
- "Summarize macro drivers for today’s session."

**Premarket (FOCUS):**
- "Filter for A+ setups across both systems."
- "Which setups fit my playbook and edge?"
- "Grade focus trades by conviction and technical confluence."

**Execution (EXECUTE/MANAGE):**
- "Buy 100 SPX at 5800 per blueprint, size full (conviction 0.9)."
- "Place stop at ES 5775, trim half at first resistance."
- "Scale in on confirmation, move stop to breakeven after 2R."

**Review (REVIEW/COACH):**
- "Review: Did I follow my plan and both systems?"
- "Analyze missed setups and emotional triggers."
- "COACH: You chased after a stop-out, overtraded after a win. Prescription: Next loss, half size only."
- "What patterns hurt/helped P&L this week?"

---

> **This file is the single source of requirements, phase logic, and vocabulary for the Intent-Trader IAA system. All code, prompts, and handler names must be derived from and remain aligned with these structures, terms, and workflows.**
