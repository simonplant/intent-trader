# Intent Trader IAA Requirements & Unified Trading Language

**Version:** 0.5.0  
**Last Updated:** 2025-05-28  
**Scope:** Unified PFEMRC Trading Workflow for DP/Inner Circle and Mancini Blueprint systems with source-based scoring

---

## Table of Contents

1. [System Purpose & MVP Criteria](#1-system-purpose--mvp-criteria)
2. [Supported Trading Systems](#2-supported-trading-systems)
3. [PFEMRC Workflow: Phases, Intents, and Outputs](#3-pfemrc-workflow-phases-intents-and-outputs)
4. [Unified Trading Language & Vocabulary](#4-unified-trading-language--vocabulary)
5. [Phase-by-Phase Handler/Intent Reference](#5-phase-by-phase-handlerintent-reference)
6. [Source-Based Scoring System](#6-source-based-scoring-system)
7. [Dual Conviction Scoring Details](#7-dual-conviction-scoring-details)
8. [Technical Integration Reference](#8-technical-integration-reference)
9. [Example Conversation Flow](#9-example-conversation-flow)
10. [Implementation Guidelines](#10-implementation-guidelines)
11. [Key Differences from Previous Versions](#11-key-differences-from-previous-versions)

---

## 1. System Purpose & MVP Criteria

### Purpose
Deliver a single, intent-driven trading assistant for solo traders that:
- Routes trade ideas to the correct scoring system based on SOURCE
- Applies DP/Inner Circle conviction scoring (0.0-1.0) to stocks, ETFs, and DP-originated SPX trades
- Applies Mancini technical scoring to ES futures and ES-derived SPX trades
- Maintains complete PFEMRC lifecycle with <1ms response times
- Provides behavioral coaching and pattern detection

### MVP Criteria
- One-file, stateless, handler-based codebase (no frameworks, no bloat)
- Source-aware routing: track where each trade idea originated
- No mixed scoring: each trade uses ONE methodology at 100%
- Real-time pattern detection and behavioral coaching
- Context passed as flat strings: "key:value|key2:value2"

### Core Principles
1. **Single Responsibility**: Each handler does one thing perfectly
2. **Zero Dependencies**: Pure language implementation only
3. **Chat-Native**: All responses readable as natural conversation
4. **<1ms Latency**: All operations in-memory, no external calls
5. **Human-First Language**: Uses trader vocabulary, not tech jargon
6. **Source Integrity**: Never mix methodologies - respect the origin

---

## 2. Supported Trading Systems

| System | Instruments | Strengths & Integration Points |
|--------|-------------|--------------------------------|
| **DP/Inner Circle** | • All stocks (AAPL, CRM, TSLA, etc.)<br>• All ETFs (QQQ, IWM, SPY, etc.)<br>• SPX when called by DP/Rick | • 0.0-1.0 conviction scoring from language patterns<br>• Focus trades (0.90+), high conviction (0.70-0.89)<br>• Position sizing tied to conviction score<br>• Special confluence areas and key levels<br>• Day-After Trade (DAT) opportunities<br>• Emotional/behavioral coaching cues |
| **Mancini Blueprint** | • ES futures (ES_F) only<br>• SPX options when derived from ES levels | • Failed Breakdown (FB) primary edge (0.85-0.95)<br>• ES/SPX precise technical levels with significance<br>• Mode 1 (trending 10%) vs Mode 2 (complex 90%) classification<br>• Acceptance pattern validation (2-30min based on volatility)<br>• 75% profit lock + runner management protocol<br>• Level-to-level exit strategy |

---

## 3. PFEMRC Workflow: Phases, Intents, and Outputs

### PLAN Phase (5:30-6:00 AM)
**Purpose:** Process morning inputs from both systems, maintain source separation  
**Intents:** `analyze dp`, `analyze mancini`, `market mode`, `create plan`  
**Actions:**
- Extract DP ideas with conviction scores and tag as source:dp
- Extract Mancini ES levels, identify FB setups, tag as source:mancini
- Classify market mode (Mode 1 vs Mode 2) for risk management
- Create parallel plans maintaining source integrity

**Outputs:**
- DP watchlist with conviction scores (stocks/ETFs/SPX-dp)
- Mancini levels with FB identification (ES/SPX-mancini)
- Market mode classification with confidence level
- Parallel plans by source (NOT combined)

### FOCUS Phase (6:00-9:00 AM)
**Purpose:** Filter for highest edge setups within each system  
**Intents:** `focus trades`, `dp focus`, `mancini setups`, `check source`, `mode strategy`  
**Actions:**
- DP: Filter for 0.90+ conviction "focus trades"
- Mancini: Identify FB setups approaching acceptance
- Apply mode-specific adjustments (tighter stops in Mode 2)
- Maintain strict source separation

**Outputs:**
- DP focus list: Top 3 conviction trades with scores
- Mancini focus list: FB setups with acceptance requirements
- Mode-adjusted position sizing recommendations
- Clear source attribution for every setup

### EXECUTE Phase (9:30 AM - Market Hours)
**Purpose:** Place trades with source-appropriate validation  
**Intents:** `add [ticker]`, `size by source`, `confirm acceptance`, `place order`  
**Actions:**
- Verify source before any execution
- DP trades: Validate conviction threshold met
- Mancini trades: Confirm FB acceptance pattern
- Apply source-specific position sizing

**Outputs:**
- Trade tickets with single-source rationale
- Stop placement per source methodology
- Position size per source rules
- Acceptance confirmation for Mancini trades

### MANAGE Phase (Intraday)
**Purpose:** Monitor using source-specific management rules  
**Intents:** `lock 75%`, `trail runner`, `level to level`, `move stop`  
**Actions:**
- DP trades: Flexible management, sentiment-based adjustments
- Mancini trades: Mandatory 75% lock at first target
- Track level-to-level progression
- Apply "never let winner go red" rule

**Outputs:**
- Position updates with source-specific rules
- Profit taking alerts (especially 75% for Mancini)
- Stop adjustments per methodology
- Runner management guidance

### REVIEW Phase (3:30-4:00 PM)
**Purpose:** Analyze performance by source system  
**Intents:** `review performance`, `dp accuracy`, `mancini stats`, `mode analysis`  
**Actions:**
- Calculate separate P&L by source
- Analyze DP conviction accuracy by score band
- Review Mancini FB win rate and mode performance
- Compare systems without combining

**Outputs:**
- DP performance: Win rate by conviction level
- Mancini performance: FB success rate, mode accuracy
- System comparison (side by side)
- Key insights and patterns

### COACH Phase (Real-time + EOD)
**Purpose:** Detect patterns and provide system-specific coaching  
**Intents:** `behavioral check`, `discipline review`, `pattern analysis`, `coaching`  
**Actions:**
- Monitor for revenge trading after stops
- Check conviction discipline (DP) and FB patience (Mancini)
- Detect overtrading and FOMO patterns
- Calculate behavioral score

**Outputs:**
- Real-time behavioral alerts
- System-specific discipline feedback
- Pattern detection with prescriptions
- Tomorrow's process improvements

---

## 4. Unified Trading Language & Vocabulary

### DP/Inner Circle Conviction Language

| Score Range | DP Language | Label | Position Size |
|-------------|-------------|-------|---------------|
| 0.90-1.00 | "focus trade", "get aggressive", "love this", "back up the truck" | Exceptional | Full size + |
| 0.70-0.89 | "definitely want", "really like", "I'm a fan", "strong conviction" | High | Full size |
| 0.50-0.69 | "I'm a buyer", "decent setup", "like it", "worth owning" | Medium | Half size |
| 0.30-0.49 | "if viable", "worth watching", "might work", "on my radar" | Low | Quarter size |
| 0.00-0.29 | "not excited", "avoid", "no way", "stay away" | Avoid | No trade |

### Mancini Technical Scoring

| Score Range | Setup Type | Quality Indicators | Position Size |
|-------------|------------|-------------------|---------------|
| 0.85-0.95 | Failed Breakdown | Clean flush + rapid reclaim above level | Full size |
| 0.70-0.84 | Level Reclaim | Multiple tests + acceptance above | Full size |
| 0.60-0.69 | Support Test | First approach to major level | Half size |
| 0.40-0.59 | Mode 2 Trap | Range trade, higher risk | Quarter size |
| 0.00-0.39 | Against Mode | Fighting dominant trend | No trade |

### Source Attribution Rules

| Instrument | Source Determination | Scoring System |
|------------|---------------------|----------------|
| Stocks (AAPL, CRM, etc.) | Always DP | 100% DP conviction |
| ETFs (QQQ, IWM, etc.) | Always DP | 100% DP conviction |
| ES Futures | Always Mancini | 100% Mancini technical |
| SPX Options | Check source tag | 100% source system |

**SPX Special Handling:**
- From DP/Rick (e.g., "0DTE plays") → Tag as source:dp
- From Mancini ES levels (e.g., ES 5750 = SPX 575) → Tag as source:mancini
- If unclear → Ask user for clarification

---

## 5. Phase-by-Phase Handler/Intent Reference

### PLAN Phase Handlers
```python
handlers = {
    # DP Analysis
    "analyze dp": analyze_dp_transcript,
    "clean transcript": clean_dp_transcript,
    
    # Mancini Analysis  
    "analyze mancini": extract_mancini_levels,
    "market mode": classify_market_mode,
    "fb setups": identify_failed_breakdowns,
    
    # Plan Creation
    "create plan": create_parallel_plans,
    "check divergence": compare_system_bias,
    "show sources": display_source_tracking
}
```

### FOCUS Phase Handlers
```python
handlers = {
    # Combined Display
    "focus trades": show_all_focus_trades,
    "max edge": find_highest_conviction_per_source,
    
    # System-Specific
    "dp focus": filter_dp_high_conviction,
    "mancini setups": show_fb_opportunities,
    "fb status": check_fb_acceptance_status,
    
    # Mode & Source
    "mode strategy": adjust_for_market_mode,
    "check source [ticker]": verify_ticker_source,
    "conviction filter": filter_by_score_threshold
}
```

### EXECUTE Phase Handlers
```python
handlers = {
    # Trade Entry
    "add [ticker]": add_with_source_validation,
    "buy/sell [ticker]": execute_trade,
    "size position": calculate_size_by_source,
    
    # Validation
    "validate [ticker]": validate_by_source,
    "confirm fb": validate_fb_pattern,
    "check acceptance": verify_acceptance_time,
    
    # Order Management
    "place order": execute_with_source_rules
}
```

### MANAGE Phase Handlers
```python
handlers = {
    # Position Display
    "positions": show_positions_by_source,
    "manage": apply_source_management_rules,
    
    # Mancini Protocol
    "lock 75": take_partial_profits_mancini,
    "trail runner": manage_final_position,
    
    # Level Management
    "next level": identify_next_target,
    "level to level": track_progression,
    
    # Stop Management
    "move stop": adjust_stops_by_source,
    "breakeven stop": move_to_breakeven
}
```

### REVIEW Phase Handlers
```python
handlers = {
    # Performance Analysis
    "review all": comprehensive_review,
    "review performance": review_by_source,
    "dp accuracy": analyze_dp_conviction_accuracy,
    "mancini stats": analyze_fb_success_rate,
    
    # Mode Analysis
    "mode analysis": review_mode_performance,
    "compare systems": parallel_system_comparison,
    
    # Statistics
    "unified stats": show_aggregate_statistics
}
```

### COACH Phase Handlers
```python
handlers = {
    # Behavioral Detection
    "behavioral check": detect_trading_patterns,
    "discipline review": check_system_adherence,
    "pattern analysis": analyze_behavioral_patterns,
    
    # System-Specific Coaching
    "dp coaching": coach_conviction_discipline,
    "mancini coaching": coach_fb_patience,
    "mode mistakes": identify_mode_errors,
    
    # Scoring & Feedback
    "discipline score": calculate_behavioral_score,
    "coaching": provide_unified_coaching
}
```

---

## 6. Source-Based Scoring System

### Core Principle
Every trade is scored 100% by its source system. No mixing. No combining. No exceptions.

### Scoring Process by Source

**DP/Inner Circle Process:**
1. Parse transcript for ticker mentions with context
2. Identify conviction language around each ticker
3. Score 0.0-1.0 based on language intensity
4. Tag all ideas as source:dp
5. Apply DP position sizing rules

**Mancini Blueprint Process:**
1. Extract ES levels from newsletter/analysis
2. Identify setup types (FB, support, resistance)
3. Score based on technical pattern quality
4. Convert ES to SPX if needed (divide by 10)
5. Tag all ideas as source:mancini
6. Apply strict Mancini management rules

### SPX Disambiguation Protocol
```
When user says "buy SPX":
1. Check context for existing SPX ideas
2. If found at multiple price levels:
   - Display both with sources
   - Ask user to specify which one
3. If price suggests obvious choice:
   - State assumption and confirm
4. Execute only with confirmed source
```

### Validation Before Execution
- Stocks/ETFs: Automatic DP validation
- ES: Automatic Mancini validation
- SPX: MUST verify source first

---

## 7. Dual Conviction Scoring Details

### DP Conviction Scoring (0.0-1.0)

| Score | Label | DP Language Patterns | Weight | Application |
|-------|-------|---------------------|---------|-------------|
| 0.90-1.00 | Exceptional | "focus trade", "get aggressive", "love this", "back up the truck" | 100% | Full size + |
| 0.70-0.89 | High | "definitely want", "I'm a fan", "really like", "strong conviction" | 100% | Full size |
| 0.50-0.69 | Medium | "I'm a buyer", "decent setup", "worth owning", "on watch" | 100% | Half size |
| 0.30-0.49 | Low | "if viable", "worth watching", "might work", "keep an eye on" | 100% | Quarter size |
| 0.00-0.29 | Avoid | "not excited", "no way", "avoid", "stay away" | 100% | No trade |

### Mancini Technical Scoring (0.0-1.0)

| Score | Setup Type | Pattern Quality | Weight | Application |
|-------|------------|-----------------|---------|-------------|
| 0.85-0.95 | Failed Breakdown | Clean flush + rapid recovery above level | 100% | Full size |
| 0.70-0.84 | Level Reclaim | Multiple tests + acceptance above | 100% | Full size |
| 0.60-0.69 | Support Test | First approach to major level | 100% | Half size |
| 0.40-0.59 | Mode 2 Range | Choppy action, trap potential | 100% | Quarter size |
| 0.00-0.39 | Against Mode | Fighting dominant trend | 100% | No trade |

### Volatility-Based Acceptance Times (Mancini Only)

| Volatility | VIX Range | Acceptance Time | Score Modifier |
|------------|-----------|-----------------|----------------|
| HIGH | > 25 | 2-5 minutes | +0.05 to base score |
| MODERATE | 18-25 | 5-15 minutes | No adjustment |
| LOW | 12-18 | 15-30 minutes | -0.05 from base score |
| VERY LOW | < 12 | 30+ minutes | -0.10 from base score |

---

## 8. Technical Integration Reference

### Concept Mapping Between Systems

| Trading Concept | DP/Inner Circle Term | Mancini Blueprint Term | Unified Usage |
|-----------------|---------------------|------------------------|---------------|
| Key Level | "special area", "confluence" | "KL", "shelf", "FB level" | Critical decision point |
| Entry Trigger | "get aggressive here" | "acceptance confirmed" | Validated entry signal |
| Stop Placement | "below support" | "below shelf/flush low" | Risk definition point |
| Profit Target | "resistance", "target" | "level-to-level", "next KL" | Exit objective |
| Position Mgmt | "trim and trail" | "75% lock + runner" | Profit protection |

---

## 9. Example Conversation Flow

### Morning (PLAN) - 5:35 AM
```
Assistant: PERFORMANCE REVIEW BY SOURCE

📊 DP/INNER CIRCLE RESULTS:
Trades: 2
- CRM: +$280 (2.1%) ✅
- SPX 5800c: +$1,050 (140%) ✅
Winners: 2/2 (100%)

Conviction Accuracy:
- 0.90+ trades: 2/2 profitable ✅
- Avg gain on focus trades: +71%

Total DP P&L: +$1,330

📈 MANCINI BLUEPRINT RESULTS:
Trades: 1
- ES 5750 FB: +$350 (7 pts) ✅
Winners: 1/1 (100%)

Pattern Performance:
- FB setup: Confirmed successful
- 75% rule: Followed correctly
- Mode 2 adjustment: Helped avoid traps

Total Mancini P&L: +$350

🎯 KEY INSIGHTS:
1. Source separation worked perfectly
2. DP focus trades (0.90+) = 100% win rate
3. Mancini FB pattern validated in Mode 2
4. No methodology mixing = clean execution

📊 Behavioral Score: 95/100
- Followed both systems correctly
- Proper source verification on SPX
- Executed 75% rule on time

→ Tomorrow: Repeat this discipline!
```

### Coach (COACH) - Real-time
```
User: thinking about shorting ES breakdown here