# Mancini Trading Method - Complete IAA Design

## Overview
Complete abstraction of Adam Mancini's futures trading methodology into a chat-native IAA system. This design captures his entire trading style: Failed Breakdowns, Level Reclaims, market mode recognition, and level-to-level trade management.

## Core Principles (The Mancini Way)
- **90% of days are Mode 2** (choppy, non-trending)
- **Failed Breakdowns are the primary edge** (90% of trades)
- **Level-to-level profit taking** (75% at first target, always)
- **Never predict, only react** to what price shows you
- **Acceptance before entry** (no knife catching)
- **15m minimum timeframe** (avoid the noise)

## State Format
```
mode:2|phase:hunt|setup:fb_pending|low:5920|flush:5915|current:5922|acceptance:type1|targets:5930,5940,5950|runner:ES:5940:trail|level_hierarchy:major:5950,sig:5930,minor:5920
```

## The Mancini Trading Loop

### 1. Market Mode Recognition
```python
def identify_market_mode(state: str) -> str:
    """Mode 1 (trend) vs Mode 2 (chop) - critical for position sizing"""
    
    signals = []
    
    # Mode 1 Signals
    if "sector_alignment" in state:
        signals.append("Sector alignment ✓")
    if "volume_expansion" in state:
        signals.append("Volume expansion ✓")
    if "breadth_thrust" in state:
        signals.append("Breadth thrust ✓")
        
    # Mode 2 Signals  
    if "sector_rotation" in state:
        signals.append("Sector rotation →")
    if "volume_contraction" in state:
        signals.append("Volume contraction →")
    if "breadth_divergence" in state:
        signals.append("Breadth divergence →")
    
    mode_1_count = sum(1 for s in signals if "✓" in s)
    
    if mode_1_count >= 2:
        return f"Mode 1 TREND DAY - {', '.join(signals)}\n→ Size up, hold runners, trail behind swings"
    else:
        return f"Mode 2 RANGE DAY - {', '.join(signals)}\n→ Reduce size, fade extremes, quick profits"
```

### 2. Setup Identification

#### Failed Breakdown (Primary Edge - 90%)
```
PATTERN: Key level breaks to downside → Quick reclaim from below
QUALITY: Prior day low (A+) > 4hr low (A) > 2hr low (B) > 1hr low (C)
ENTRY: Price reclaims level from below (not just touch)
STOP: Below minor support after reclaim
TARGET: Previous resistance or next technical level
TIMEFRAME: Intraday primary, multi-day possible

LEVEL HIERARCHY:
- Major: Daily pivots, key round numbers
- Significant: 4hr+ consolidation boundaries  
- Minor: 1-2hr swing points
- Intraday: 15-30min micro swings
```

#### Level Reclaim (Secondary - 10%)
```
PATTERN: Horizontal acts as support → Then resistance → Reclaim above
CONTEXT: Best after "elevator down" moves
ENTRY: Above reclaimed level
```

### 3. Acceptance Validation (CRITICAL)
```python
def check_acceptance(state: str) -> str:
    """No acceptance = No trade. Three types of validation."""
    
    # Time-based acceptance
    if "held_above_5min" in state:
        return "✅ TIME acceptance (5min+ above level) - SAFE"
    
    # Volume-based acceptance  
    elif "volume_surge_on_reclaim" in state:
        return "✅ VOLUME acceptance (buyers stepping in) - SAFE"
        
    # Price action acceptance
    elif "tested_bounced_flushed_returned" in state:
        return "✅ PRICE ACTION acceptance (Type 1) - SAFE"
    
    elif "flushed_tested_failed_recovered" in state:
        return "✅ PRICE ACTION acceptance (Type 2) - SAFE"
    
    else:
        return "⏳ NO ACCEPTANCE - DO NOT ENTER\nWait for: 5min hold, volume surge, or retest pattern"
```

### 4. Entry Execution
```
NEVER: 
- Chase price
- Enter without acceptance
- Buy supports directly
- Trade between 11am-2pm (chop zone)

ALWAYS:
- Wait for setup completion
- Verify acceptance pattern
- Enter on recovery/reclaim
- Trade 7:30-8:30am or after 3pm
```

### 5. Level-to-Level Management
```python
def manage_trade(position: str) -> str:
    """The Mancini way - systematic runner management"""
    
    # First target hit (+10-15 points)
    "→ Lock 75% profits NOW"
    
    # Second target hit (+20-30 points)  
    "→ Lock another 15%, leave 10% runner"
    
    # Runner management protocols
    "TRAIL TYPES:"
    "→ Fixed: Trail 5-10 points behind"
    "→ Percentage: Trail 0.5% behind high"  
    "→ Structure: Trail behind swing lows"
    
    "ADJUSTMENT TRIGGERS:"
    "→ Time: Every 30min in profit"
    "→ Extension: At each new high"
    "→ Volatility: Widen in Mode 1, tighten Mode 2"
```

### 6. ES to SPX Mapping
```python
def map_es_spx(es_level: float) -> tuple[float, float]:
    """Convert between ES and SPX with dynamic offset based on time decay.
    
    The offset decays linearly from ~40 points at contract start to ~5 points
    at expiry, based on the quarterly roll cycle.
    
    Args:
        es_level: The ES futures level
        
    Returns:
        tuple: (spx_equivalent, offset_range)
            - spx_equivalent: The SPX level (ES/10 - offset)
            - offset_range: Tuple of (min_offset, max_offset) for validation
    """
    offset = get_es_spx_offset()
    spx_equivalent = (es_level - offset) / 10
    
    # Allow for some variation in the offset
    offset_range = (offset - 2, offset + 2)
    
    return spx_equivalent, offset_range
```

## Complete Trading Flow

### Pre-Market (The Hunt)
```
1. Identify key levels from overnight
2. Mark prior day low (most important)
3. Note any multi-hour lows
4. Set alerts at these levels
```

### Market Hours (The Execution)
```
Phase: HUNT
- Watch for flushes of key lows
- Monitor for "elevator down" moves
- Stay patient, setups will come

Phase: STALK  
- Low flushed, watching recovery
- Check for acceptance patterns
- Prepare entry orders

Phase: STRIKE
- Acceptance confirmed
- Enter above recovered low
- Set stop below flush

Phase: MANAGE
- First target: Lock 75%
- Second target: Lock 15% 
- Trail 10% runner
```

### Post-Market (The Review)
```
- Log every setup taken
- Note acceptance type
- Review level-to-level execution
- Plan tomorrow's levels
```

## Implementation as IAA

### Intent Patterns
```python
intents = {
    # Core Analysis
    "analyze mancini": handle_analyze_mancini_newsletter,
    "spot fb": handle_spot_failed_breakdown,
    "check mode": handle_check_market_mode,
    "detect mode": handle_detect_mode_signals,
    
    # Level Framework
    "key levels": handle_identify_levels,
    "level hierarchy": handle_show_level_hierarchy,
    "es spx map": handle_es_spx_conversion,
    
    # Validation
    "acceptance?": handle_check_acceptance,
    "fb quality": handle_rate_setup_quality,
    "check confluence": handle_check_dp_mancini_confluence,
    
    # Execution
    "fb triggered": handle_failed_breakdown_entry,
    "size futures": handle_size_futures_position,
    
    # Management
    "lock 75": handle_lock_profits,
    "manage runner": handle_manage_runner,
    "trail runner": handle_trail_stop,
    "runner protocol": handle_show_runner_protocol,
    
    # Review
    "mode stats": handle_mode_statistics,
    "fb performance": handle_fb_performance,
    "runner performance": handle_runner_optimization_stats
}
```

### State Management
```python
context = "mode:2|setups_today:3|fb_taken:1|locked_profits:2|runner_active:ES"
```

### Example Interaction
```
User: "ES flushed 5920 now 5925"
System: "📍 FAILED BREAKDOWN FORMING
        Quality: A (prior day low)
        Status: Waiting for acceptance
        Next: Watch for test from below"

User: "tested 5920 from below, bounced"
System: "✅ ACCEPTANCE CONFIRMED (Type 2)
        Entry: Long above 5925
        Stop: Below 5915 (flush low)
        Target 1: 5935 (lock 75%)
        Target 2: 5945 (lock 15%)
        Runner: Trail under higher lows"

User: "hit 5935"
System: "🎯 FIRST TARGET HIT
        Action: Lock 75% NOW
        Remaining: 25% position
        Next target: 5945
        Stop: Move to breakeven"
```

## The Mancini Principles (Never Violated)

### 1. The Golden Rule
"90% of moves don't follow through. Therefore, ALWAYS lock gains level-to-level."

### 2. No Predictions
"You don't know if it goes 1 level or 100 levels. Manage every trade the same way."

### 3. Failed Breakdowns Only
"After elevator down, look for Failed Breakdown. That's where money is made."

### 4. Acceptance or Nothing
"No acceptance = No trade. This isn't negotiable."

### 5. Time Matters
"7:30-8:30am and after 3pm. Avoid 11am-2pm chop."

## Chart Reading (Mancini Style)

### What He Looks For
```
1. STRUCTURE
   - Prior day low (holy grail)
   - Multi-hour lows/shelves
   - Clear, obvious levels
   
2. FLUSH QUALITY
   - Clean break (not drift)
   - Trap obvious (stops hit)
   - Quick (not grinding)
   
3. RECOVERY SPEED
   - Snap back = strong
   - Slow recovery = weak
   - V-shape best
```

### What He Ignores
```
- Indicators (except basic MAs)
- Volume (mentioned rarely)
- News/fundamentals
- Other's opinions
- Predictions/targets
```

## Success Metrics
- **FB Setup Win Rate**: Track successful FB executions
- **Mode Detection Accuracy**: Correct Mode 1 vs 2 identification  
- **Level Precision**: Accuracy of level interaction predictions
- **Runner Performance**: Optimization of trailing strategies
- **Acceptance Pattern Success**: Which acceptance type performs best
- **Time-of-Day Performance**: Morning vs afternoon FB success

## Implementation Notes
- Single file, < 500 lines
- Pure Python, no dependencies
- State in simple string format
- Every response < 1ms
- Natural language in/out
- Focus on THE SETUP (Failed Breakdown)

## Key Enhancements from Integration Plan

### 1. Mode Detection Signals
- **Mode 1**: Sector alignment + Volume expansion + Breadth thrust
- **Mode 2**: Sector rotation + Volume contraction + Breadth divergence

### 2. Level Hierarchy Framework
- **Major**: Daily pivots, key round numbers (strongest)
- **Significant**: 4hr+ consolidation boundaries
- **Minor**: 1-2hr swing points  
- **Intraday**: 15-30min micro swings (weakest)

### 3. Acceptance Types Expanded
- **Time-based**: 5+ minutes above reclaimed level
- **Volume-based**: Surge on reclaim shows real buyers
- **Price action-based**: Original test/retest patterns

### 4. Runner Management Protocols
- **Trail Types**: Fixed-point, percentage-based, structure-based
- **Adjustment Triggers**: Time-based, extension-based, volatility-based
- **Mode-specific**: Wider trails Mode 1, tighter Mode 2

### 5. ES/SPX Mapping
- Standard: ES = SPX × 10 (but varies 5-15 points)
- Always track both for confluence
- FB setups work on both instruments

## Integration with DP Analysis

### Confluence Detection
```python
def check_dp_mancini_confluence(ticker: str, state: str) -> str:
    """When DP focus aligns with Mancini FB = highest conviction"""
    
    if ticker == "SPX":
        # Check both analysts
        dp_bullish = "dp_focus_long" in state
        mancini_fb = "fb_setup_long" in state
        
        if dp_bullish and mancini_fb:
            return "🎯 CONFLUENCE ALERT: DP + Mancini aligned on SPX long!"
        elif dp_bullish or mancini_fb:
            return "Single analyst setup - standard position size"
        else:
            return "No setup from either analyst"
```

### Unified Planning
- Morning: Analyze both DP call + Mancini newsletter
- Identify: Where setups align (highest priority)
- Execute: DP methodology for stocks, Mancini for futures
- Review: Compare performance by methodology