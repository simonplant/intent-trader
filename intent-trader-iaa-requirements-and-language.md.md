# Intent Trader IAA Requirements and Language System

**Version:** 1.0  
**Owner:** [Your Name/Team]  
**Last Updated:** [YYYY-MM-DD]  
**Scope:** PFEMRC Trading Workflow (PLAN → FOCUS → EXECUTE → MANAGE → REVIEW → COACH)

---

## Table of Contents

1. Business Requirements
2. System Phases & Intents (PFEMRC)
3. Phase Language & Process Details
4. Outputs & Success Criteria

---

## 1. Business Requirements

- **Purpose:**  
  Build a solo-friendly, intent-driven trading assistant supporting the entire PFEMRC workflow—integrating analyst calls, setups, trade planning, execution, review, and continuous behavioral coaching.
- **Users:**  
  Individual traders, system builders, trading coaches (expandable to teams)
- **Core Capabilities:**  
  - Ingest and summarize daily analyst input (e.g., DP, Mancini)
  - Generate actionable daily trade plan
  - Surface and grade high-quality setups
  - Support order entry, risk management, and active trade management
  - Automate EOD review and stats tracking
  - Provide real-time and end-of-day COACH feedback (pattern recognition, behavioral intervention)
- **MVP Criteria:**  
  - Everything runs from a single file, all handlers visible, no frameworks
  - All phase language is modern, trader-centric, and pattern-based
  - Live conversation with clear, phase-specific outputs and interventions

---

## 2. System Phases & Intents (PFEMRC)

| Phase    | Core Intents/Actions                                      |
|----------|-----------------------------------------------------------|
| PLAN     | Analyze context, summarize calls/news, define bias/levels |
| FOCUS    | Surface A+ setups, grade and filter, align to playbook    |
| EXECUTE  | Place/manage orders, size positions, set stops/targets    |
| MANAGE   | Adjust positions, scale, trim, trail stops, risk off      |
| REVIEW   | Assess trades, log stats, note wins/mistakes, EOD recap   |
| COACH    | Pattern recognition, real-time warnings, truth reports    |

---

## 3. Phase Language & Process Details

### **PLAN – Market Structure & Preparation**
- *"Where’s the flow?"* *"Any overnight catalysts?"* *"Is today trending or chop?"*
- **Outputs:**  
  - Bias (bullish/bearish/chop)
  - Key levels (support/resistance/inflection)
  - Focus/no-trade list
  - Unified analyst view (DP, Mancini, consensus)
  - Market character (risk on/off, trending/range, news-driven)

---

### **FOCUS – Edge Identification & Setup Qualification**
- *"What’s setting up?"* *"Is this an A+ trade?"* *"Where’s my bread & butter?"*
- **Outputs:**  
  - Graded setup list (A+, B, trap, lotto)
  - Playbook fit (Y/N)
  - Setup checklist (confluence, volume, risk/reward)
  - Execution triggers, size plan

---

### **EXECUTE – Order Entry & Risk Initiation**
- *"Scale in starter size"* *"Limit order at key level"* *"Stop set at invalidation"*
- **Outputs:**  
  - Trade order (symbol, entry, size, order type)
  - Confirmation (“filled”, “partial”, “missed”)
  - Entry/exit plan (stop, target)
  - Immediate fill feedback (“slippage”, “chasing”, “stuffed”)

---

### **MANAGE – Position & Risk Control**
- *"Let the runner work"* *"Trim half on target"* *"Move stop to breakeven"*
- **Outputs:**  
  - Partial exits, scaling, stop moves
  - Management notes (“holding core”, “trimming on loss of momentum”)
  - Warnings (“profit evaporating”, “character shift”)
  - Adherence to risk rules (max loss, no add, correlation)

---

### **REVIEW – Honest Performance Assessment**
- *"Good process, bad outcome"* *"Should’ve scaled sooner"* *"FOMO entry mistake"*
- **Outputs:**  
  - Trade-by-trade log (thesis, entry, exit, result, lesson)
  - Stats (win rate, average win/loss, largest win/loss)
  - Missed opportunity analysis
  - EOD summary (what worked, what didn’t, improvement plan)

---

### **COACH – Pattern Recognition & Behavioral Correction**
- *"You always overtrade after two stops"* *"Win big on failed breakdowns, lose on news fades"* *"Tilt detected—size down or step away"*
- **Outputs:**  
  - Pattern log (time, setup, outcome, emotion)
  - Real-time interventions (“walk away”, “half size”, “no more trades after 3:30”)
  - Monthly “truth report” (top mistakes, strengths, habit targets)
  - Accountability feedback loop (rule violations, behavior change)

---

## 4. Outputs & Success Criteria

- **Single-file, handler-based codebase using this language system**
- **Trader can complete all six phases, with phase-appropriate guidance and interventions**
- **COACH interventions are both real-time (intraday) and EOD (review/report)**
- **Phase language is actionable, pattern-driven, and always trader-first**
- **Continuous improvement loop: all phases feed into COACH and back to PLAN**

---

## Reference: Sample Conversation Flow

- **Morning (PLAN):**  
  “What’s moving premarket?”  
  “Key levels today?”  
- **Pre-Open (FOCUS):**  
  “What’s the A+ setup?”  
- **Trading (EXECUTE/MANAGE):**  
  “Buy 100 TEM at 61”  
  “Move stop to 61.50”  
  “Take half off here”  
- **Midday (COACH):**  
  “You’re forcing trades”  
  “Stick to your plan”  
- **Close (REVIEW):**  
  “How’d I do?”  
  “What worked?”  
- **Evening (COACH):**  
  “You need to stop trading the first 30 minutes”  
  “Tomorrow: Only A+ setups, half size”  

---

> **This document is the definitive requirements and language reference for the Intent Trader IAA system. All code, prompts, and handler names should be derived from these structures and terms.**
