---
front-matter here
---

# Intent Trader IAA Requirements & Unified Trading Language

**Version:** 0.3.0  
**Last Updated:** 2025-05-28  
**Scope:** Unified PFEMRC Trading Workflow (PLAN → FOCUS → EXECUTE → MANAGE → REVIEW → COACH) for DP/Inner Circle + Mancini Blueprint systems with dual conviction scoring.

---

## Table of Contents

1. [System Purpose & MVP Criteria](#1-system-purpose--mvp-criteria)
2. [Supported Trading Systems](#2-supported-trading-systems)
3. [PFEMRC Workflow: Phases, Intents, and Outputs](#3-pfemrc-workflow-phases-intents-and-outputs)
4. [Unified Trading Language & Vocabulary](#4-unified-trading-language--vocabulary)
5. [Phase-by-Phase Handler/Intent Reference](#5-phase-by-phase-handlerintent-reference)
6. [Dual Conviction Scoring System](#6-dual-conviction-scoring-system)
7. [Example Conversation Flow](#7-example-conversation-flow)

---

## 1. System Purpose & MVP Criteria

**Purpose:**  
Deliver a single, intent-driven trading assistant for solo traders that integrates:
- Analyst conviction (DP/Inner Circle) with precise 0.0-1.0 scoring
- Technical blueprint systems (Mancini) with setup quality validation
- Complete PFEMRC lifecycle with <1ms response times
- Unified scoring: Combined_Score = (DP_Score * 0.6) + (Mancini_Score * 0.4)

**MVP Criteria:**
- One-file, stateless, handler-based codebase (no frameworks, no bloat)
- All phases use unified vocabulary from BOTH systems
- Dual conviction scoring: DP language + Mancini technical validation
- Real-time pattern detection and behavioral coaching
- Context passed as flat strings: "key:value|key2:value2"

**Core Principles:**
1. **Single Responsibility**: Each handler does one thing perfectly
2. **Zero Dependencies**: Pure language implementation only
3. **Chat-Native**: All responses readable as natural conversation
4. **<1ms Latency**: All operations in-memory, no external calls
5. **Human-First Language**: Uses trader vocabulary, not tech jargon

---

## 2. Supported Trading Systems

| System | Strengths & Integration Points |
|--------|--------------------------------|
| **DP/Inner Circle** | • 0.0-1.0 conviction scoring from language patterns<br>• Focus trades (0.90+), high conviction (0.70-0.89)<br>• Position sizing tied to conviction score<br>• Special confluence areas and key levels<br>• Day-After Trade (DAT) opportunities<br>• Emotional/behavioral coaching cues |
| **Mancini Blueprint** | • Failed Breakdown (FB) primary edge (0.85-0.95)<br>• ES/SPX precise technical levels with significance<br>• Mode 1 (trending 10%) vs Mode 2 (complex 90%) classification<br>• Acceptance pattern validation (2-30min based on volatility)<br>• 75% profit lock + runner management protocol<br>• Level-to-level exit strategy |

---

## 3. PFEMRC Workflow: Phases, Intents, and Outputs

### **PLAN** (5:30-6:00 AM)
- **Purpose**: Process morning inputs from both systems, establish unified bias
- **Intents**: `analyze dp`, `analyze mancini`, `market mode`, `unified plan`
- **Actions**: Extract DP ideas + Mancini levels, identify FB setups, classify mode
- **Outputs**: 
    - Executive summary with top 3 trades (combined scores)
    - Market mode classification (Mode 1 trending vs Mode 2 complex)
    - ES/SPX level framework with FB setup identification
    - Unified bias when systems agree, divergence alerts when not

### **FOCUS** (6:00-9:00 AM)
- **Purpose**: Filter for highest edge setups combining both systems
- **Intents**: `focus trades`, `fb setups`, `check confluence`, `mode strategy`
- **Actions**: Find DP focus trades at Mancini FB levels, validate acceptance
- **Outputs**:
    - Combined conviction scores (DP 0.90 + FB 0.85 = 0.88 unified)
    - FB setups at DP focus levels (maximum edge)
    - Mode-specific adjustments (tighter stops in Mode 2)
    - Acceptance time requirements based on volatility

### **EXECUTE** (9:30 AM - Market Hours)
- **Purpose**: Place trades with dual system validation
- **Intents**: `add fb setup`, `size by conviction`, `confirm acceptance`, `place order`
- **Actions**: Validate FB acceptance pattern, apply combined sizing rules
- **Outputs**:
    - Trade tickets with dual rationale (DP conviction + Mancini setup)
    - Stop placement below FB shelf (Mancini) or DP level
    - Position size: DP conviction × Mancini setup quality
    - Acceptance pattern confirmation before entry

### **MANAGE** (Intraday)
- **Purpose**: Monitor using both systems' management rules
- **Intents**: `lock 75%`, `trail runner`, `level to level`, `move stop`
- **Actions**: Apply Mancini 75% rule at targets, DP position management
- **Outputs**:
    - 75% profit lock at first Mancini level (mandatory)
    - 10% runner with trailing stop (Mancini protocol)
    - Stop to breakeven after first target (never red rule)
    - Level-to-level progression tracking

### **REVIEW** (3:30-4:00 PM)
- **Purpose**: Analyze performance across both systems
- **Intents**: `review fb performance`, `dp accuracy`, `mode analysis`, `unified stats`
- **Actions**: Compare FB win rate, DP conviction accuracy, mode performance
- **Outputs**:
    - FB setup statistics (expect 65%+ win rate)
    - DP conviction correlation (0.90+ should outperform)
    - Mode 1 vs Mode 2 performance breakdown
    - Combined score accuracy analysis

### **COACH** (Real-time + EOD)
- **Purpose**: Detect patterns specific to each system's rules
- **Intents**: `fb discipline check`, `conviction accuracy`, `mode mistakes`, `unified coaching`
- **Actions**: Monitor FB acceptance patience, DP score chasing, mode recognition
- **Outputs**:
    - FB entry discipline ("Waited for acceptance?")
    - DP conviction discipline ("Only 0.70+ trades")
    - Mode recognition accuracy ("Fought Mode 2?")
    - Combined system adherence score

---

## 4. Unified Trading Language & Vocabulary

### Conviction Integration Table

| DP Terms | Mancini Terms | Unified Language | Combined Score |
|----------|---------------|------------------|----------------|
| "focus trade", "get aggressive" | "big idea FB", "primary setup" | Maximum edge setup | 0.90-1.00 |
| "definitely want to buy" | "failed breakdown confirmed" | High conviction entry | 0.80-0.89 |
| "I'm a fan", "really like" | "level reclaim", "acceptance complete" | Strong setup | 0.70-0.79 |
| "I'm a buyer" | "decent test", "fb forming" | Standard entry | 0.60-0.69 |
| "worth watching" | "approaching fb level" | Monitor for setup | 0.50-0.59 |
| "if viable" | "needs acceptance", "mode 2 trap risk" | Conditional entry | 0.30-0.49 |
| "not excited" | "breakdown short", "against mode" | Avoid/minimal | 0.00-0.29 |

### Technical Integration

| Concept | DP Term | Mancini Term | Unified Usage |
|---------|---------|--------------|---------------|
| Key Level | "special area", "confluence" | "KL", "shelf", "FB level" | Critical decision point |
| Entry Trigger | "get aggressive here" | "acceptance confirmed" | Validated entry signal |
| Stop Placement | "below support" | "below shelf/flush low" | Risk definition point |
| Profit Target | "resistance", "target" | "level-to-level", "next KL" | Exit objective |
| Position Mgmt | "trim and trail" | "75% lock + runner" | Profit protection |

---

## 5. Phase-by-Phase Handler/Intent Reference

### PLAN Handlers
```python
handlers = {
    # DP Analysis
    "analyze dp": analyze_dp_transcript,
    "clean transcript": clean_dp_transcript,
    
    # Mancini Analysis  
    "analyze mancini": extract_mancini_levels,
    "market mode": classify_market_mode,
    "fb setups": identify_failed_breakdowns,
    
    # Unified
    "unified plan": create_combined_plan,
    "check divergence": compare_systems
}
```

### FOCUS Handlers
```python
handlers = {
    # Combined Filtering
    "focus trades": show_unified_focus,
    "max edge": find_dp_fb_confluence,
    
    # Mancini Specific
    "fb status": check_fb_acceptance,
    "mode strategy": adjust_for_mode,
    
    # DP Specific
    "conviction filter": filter_by_dp_score
}
```

### EXECUTE Handlers
```python
handlers = {
    # Unified Entry
    "add [ticker]": add_with_dual_validation,
    "size position": calculate_unified_size,
    
    # Mancini Validation
    "confirm fb": validate_fb_pattern,
    "check acceptance": verify_acceptance_time,
    
    # Order Management
    "place fb trade": execute_fb_setup
}
```

### MANAGE Handlers
```python
handlers = {
    # Mancini Protocol
    "lock 75": take_partial_profits,
    "trail runner": manage_final_position,
    
    # Level Management
    "next level": identify_next_target,
    "level to level": track_progression,
    
    # Unified Stops
    "breakeven stop": move_to_breakeven
}
```

---

## 6. Dual Conviction Scoring System

### DP Conviction (0.0-1.0)

| Score | Label | DP Language | Weight |
|-------|-------|-------------|---------|
| 0.90-1.00 | Exceptional | "focus trade", "get aggressive" | 60% |
| 0.70-0.89 | High | "definitely want", "I'm a fan" | 60% |
| 0.50-0.69 | Medium | "I'm a buyer", "decent setup" | 60% |
| 0.30-0.49 | Low | "if viable", "won't force" | 60% |
| 0.00-0.29 | Avoid | "not excited", "no way" | 60% |

### Mancini Technical Score (0.0-1.0)

| Score | Setup Type | Pattern Quality | Weight |
|-------|------------|-----------------|---------|
| 0.85-0.95 | Failed Breakdown | Clean flush + rapid recovery | 40% |
| 0.70-0.80 | Level Reclaim | Multi-test support reclaimed | 40% |
| 0.60-0.70 | Mode 1 Continuation | Trending day setup | 40% |
| 0.40-0.60 | Mode 2 Trap | Range trade, higher risk | 40% |
| 0.30-0.40 | Breakdown Short | 60% fail rate warned | 40% |

### Combined Scoring Formula
```
Combined_Score = (DP_Score × 0.6) + (Mancini_Score × 0.4)

Example:
- DP: "get aggressive" = 0.95
- Mancini: Failed Breakdown = 0.90
- Combined: (0.95 × 0.6) + (0.90 × 0.4) = 0.93
```

### Volatility Adjustments (Mancini)

| Volatility | Acceptance Time | Score Modifier |
|------------|----------------|----------------|
| HIGH | 2-5 minutes | +0.05 |
| MODERATE | 5-15 minutes | 0.00 |
| LOW | 15-30 minutes | -0.05 |
| VERY LOW | 30+ minutes | -0.10 |

---

## 7. Example Conversation Flow

### Morning (PLAN) - 5:35 AM
```
User: analyze dp and mancini
Assistant: Analyzing both systems...

UNIFIED PLAN - Top 3 Combined Setups:
1. QQQ $495 - Long (0.93) 
   - DP: "Get aggressive at special area" (0.95)
   - Mancini: Failed Breakdown setup forming (0.90)
   - Entry: On acceptance above $495 (2-5min hold)

2. ES 5800 - Long (0.86)
   - DP: "I'm a fan of this level" (0.80) 
   - Mancini: Key Level reclaim setup (0.95)
   - Entry: Reclaim and hold above 5800

3. UNH $290 - Long (0.84)
   - DP: "Definitely want to buy" (0.92)
   - Mancini: Shelf support test (0.70)
   - Entry: At $289-290 with FB confirmation

Market Mode: Mode 2 Complex (90% probability)
- Expect multiple reversals and traps
- Tighten stops, reduce size
- Focus on A+ setups only
```

### Pre-Market (FOCUS) - 8:30 AM
```
User: fb setups at dp levels
Assistant: MAXIMUM EDGE SETUPS (DP + Mancini Confluence):

**QQQ $495 FB Setup** (Combined: 0.93)
- Status: Approaching FB level
- Acceptance Needed: 2-5 min above $495
- DP Context: "Special area - get aggressive"
- Stop: Below $493 flush low
- Target 1: $501 (lock 75%)
- Runner Target: $508

**ES 5800 Reclaim** (Combined: 0.86)  
- Status: Testing from below
- Need: Clean break + retest hold
- Mode 2 Warning: Possible trap, wait for confirmation
```

### Execution (EXECUTE) - 10:15 AM
```
User: add qqq fb setup
Assistant: VALIDATING FB SETUP...

✓ QQQ at $495.20 - Acceptance timer started
- Minute 1: Holding above ✓
- Minute 2: Building ✓
- Minute 3: Confirmed ✓

EXECUTING: QQQ Failed Breakdown Long
- Entry: $495.50
- Conviction: 0.93 (DP 0.95 + FB 0.90)
- Size: 180 shares (90% of max due to Mode 2)
- Stop: $493.00 (below FB shelf)
- Target 1: $501 (75% exit)
- Runner: $508 (25% trail)

Mancini Protocol: Lock 75% at first target, never let winner go red
```

### Management (MANAGE) - 1:30 PM
```
User: check qqq
Assistant: QQQ FB POSITION STATUS:

Price: $500.80 (+1.1%)
- Approaching Target 1: $501

ACTION REQUIRED SOON:
- At $501: Lock 75% profits (135 shares)
- Keep 25% runner (45 shares)
- Move stop to $495.50 (breakeven)

Mancini Rule: "Take the gift at first target"
DP Context: "Special areas can run - trail the runner"
```

### Review (REVIEW) - 3:45 PM
```
User: review fb performance
Assistant: DUAL SYSTEM REVIEW:

**Failed Breakdown Performance:**
- Total FB trades: 3
- Winners: 2 (67% win rate ✓)
- Avg Winner: +1.8R
- FB at DP levels: 2/2 winners

**Conviction Accuracy:**
- 0.90+ combined: 2/2 profitable
- 0.70-0.89: 3/4 profitable  
- Below 0.70: 1/3 profitable

**Mode Analysis:**
- Correctly identified Mode 2
- Avoided trap breakouts: 3
- Patience on FB acceptance: 100%

**Key Insight**: DP + Mancini confluence (0.85+) = 100% win rate today
```

### Coach (COACH) - Real-time
```
User: buy es breakdown here
Assistant: ⚠️ COACH ALERT: System Violation

BREAKDOWN SHORT WARNING:
- Mancini: "60% fail rate on breakdown shorts"
- Mode 2 Day: Fading breakdowns is fighting the mode
- No DP conviction for shorts today

PRESCRIPTION:
- Wait for Failed Breakdown setup instead
- In Mode 2: FB setups > Breakdown shorts
- Patience for A+ setups only

Your Stats:
- FB trades: 67% winners
- Breakdown shorts: 0% winners
- Stick to your edge!
```

---

## Implementation Guidelines

### Context Format
```
"phase:execute|ticker:QQQ|dp_score:0.95|mancini_score:0.90|combined:0.93|mode:2|fb_status:confirmed"
```

### Unified Decision Logic
1. Check DP conviction (0.0-1.0)
2. Validate Mancini setup quality
3. Calculate combined score
4. Confirm pattern acceptance
5. Apply mode adjustments
6. Execute with dual rationale

### Response Hierarchy
1. Combined scores 0.85+ = Priority focus
2. System agreement = Higher confidence  
3. System divergence = Require confirmation
4. Mode 2 days = Tighter risk management

---

This unified system creates maximum edge by combining DP's market insight and conviction scoring with Mancini's technical precision and risk management protocols, all delivered through a lightweight, chat-native interface.
```
