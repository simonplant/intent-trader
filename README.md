# Intent Trader - AI-Native Trading Assistant

**Version:** 1.0.0  
**Date:** 2024-05-28  
**Author:** Solo Trader  
**License:** MIT

An AI-native trading assistant that runs entirely within AI chat applications, providing structured trading workflow with persistent memory across messages. No installation needed - just start chatting!

## What This Is

**Intent Trader turns AI into a systematic trading assistant** that:
- Remembers all your positions and P&L
- Scores trades using YOUR systems (DP/Mancini)
- Tracks behavioral patterns and coaches you
- Maintains context throughout your trading day

**This is NOT a Python app you install** - it runs inside your AI of choice!

## Overview

Intent Trader is a production-ready trading discipline system that:
- **Never mixes trading methodologies** - Each trade scored 100% by its source
- **Enforces the complete PFEMRC workflow** - PLAN → FOCUS → EXECUTE → MANAGE → REVIEW → COACH
- **Provides real-time behavioral coaching** - Detects revenge trading, overtrading, and discipline breaks
- **Maintains complete audit trail** - Every decision logged and recoverable

## Quick Start (30 Seconds)

### Start Trading Right Now:

1. **Open a new AI chat**
2. **Say this:**
   ```
  Run the intent_trader.py file and initialize a trading session
   ```
3. **Start trading:**
   ```
   analyze dp AAPL focus trade love this above 225
   buy AAPL
   positions
   ```

That's it! No installation, no Python, no terminal. Just AI + structure.

### How It Works in your AI

```
You: Initialize Intent Trader

AI: Intent Trader initialized! Ready for PLAN phase.
        What's your morning analysis?

You: analyze dp
     AAPL really like this setup above 225, strong conviction
     CRM focus trade here, get aggressive on any dip

AI: === DP ANALYSIS ===
        Bias: BULLISH
        Key Levels: 225
        Trade Ideas:
        • AAPL: High (0.80)
        • CRM: Exceptional (0.95)
        → Next: Analyze Mancini for confluence

You: buy 100 CRM at 165.50

AI: === EXECUTED ===
        LONG 100 CRM @ 165.50
        ✓ Source: DP (Focus Trade)
        ✓ Phase → MANAGE
        
        DP Rules:
        • Flexible management
        • Adjust on sentiment

You: positions

AI: === OPEN POSITIONS ===
        DP POSITIONS:
        • LONG CRM 100@165.50 → $0.00 (+0.0%)
        
        Unrealized: $0.00
        Realized: $0.00
        Total P&L: $0.00
```

### Trading Day Example

```
# Pre-market focus
[FOCUS] > focus trades
=== TODAY'S FOCUS TRADES ===
DP FOCUS (0.90+):
• CRM: Exceptional (0.95)

MANCINI FOCUS (0.85+):
• ES: FB (0.90) @ 5750

# Execute when ready
[FOCUS] > buy 100 CRM @ 165.50
=== EXECUTED ===
LONG 100 CRM @ 165.50
✓ Source: DP
✓ Phase → MANAGE

DP Rules:
• Flexible management
• Adjust on sentiment

# Quick position check
[MANAGE] > positions
=== OPEN POSITIONS ===
DP POSITIONS:
• LONG CRM 100@165.50 → $0.00 (+0.0%)

Unrealized: $0.00
Realized: $0.00
Total P&L: $0.00

# Update prices
[MANAGE] > update CRM 167.20
Updated: CRM → 167.2

[MANAGE] > positions
=== OPEN POSITIONS ===
DP POSITIONS:
• LONG CRM 100@165.50 → $170.00 (+1.0%)

# Move stop to breakeven
[MANAGE] > move stop CRM 165.50
=== STOP MOVED ===
CRM: None → 165.5
Position now RISK FREE!

# End of day
[MANAGE] > exit CRM @ 168.00
=== CLOSED POSITION ===
CRM: $250.00 (+1.5%)
Source: DP
Profit taken

→ All flat. Phase: REVIEW

[REVIEW] > review
=== SESSION REVIEW ===
Completed Trades: 1
Realized P&L: $250.00
Stops Hit: 0

DP Ideas Generated: 4
Win Rate: 100%
Positive session - good discipline

→ Ready for COACH phase

[COACH] > coach
=== COACH FEEDBACK ===
Good discipline today!
• Keep following your plan
• Size up on focus trades
• Trust your analysis

TOMORROW'S FOCUS:
1. Wait for A+ setups only
2. Respect source-based rules
3. Honor stops without revenge
4. Journal after each trade

→ Ready for next session (Phase: PLAN)

[PLAN] > save
Saved to trader_20240528_1545.json
```

## Command Reference

### PLAN Phase (Pre-Market)
- `analyze dp [text]` - Process DP morning call
- `analyze mancini [text]` - Process Mancini newsletter
- `market mode [1/2]` - Set/check market mode
- `create plan` - Generate unified trading plan

### FOCUS Phase (9:00-9:30 AM)
- `focus trades` - Show all high-conviction trades
- `dp focus` - DP trades only (0.90+)
- `mancini setups` - Mancini setups only
- `check source TICKER` - Verify ticker source

### EXECUTE Phase (Market Hours)
- `buy/sell QTY TICKER @ PRICE` - Full entry
- `buy/sell TICKER` - Quick entry (100 shares)
- `add TICKER` - Add from analyzed ideas
- `quick TICKER` - Instant position add
- `size TICKER` - Position sizing guidance

### MANAGE Phase (Intraday)
- `positions` - View all open positions
- `update AAPL 227.50 TSLA 185.20` - Update prices
- `lock 75 [TICKER]` - Take 75% profits (Mancini)
- `move stop TICKER PRICE` - Adjust stop loss
- `exit TICKER` or `exit all` - Close positions
- `note TICKER message` - Quick position notes

### REVIEW Phase (Post-Market)
- `review` - Session summary
- `performance` - Detailed performance analysis

### COACH Phase (Anytime)
- `coach` - Get behavioral feedback
- `behavioral check` - Real-time pattern check

### Utilities (Always Available)
- `save` - Save session to JSON
- `load [filename]` - Restore previous session
- `journal [note]` - Add timestamped note
- `context` - Show current state
- `reset` - Start fresh
- `help` - Show all commands

## Key Features

### Source-Based Scoring System
Every trade is scored 100% by its source system:
- **Stocks/ETFs**: Always use DP conviction scoring
- **ES Futures**: Always use Mancini technical scoring
- **SPX Options**: Must verify source (asks user)

### DP Conviction Scale
| Score | Phrases | Label | Size |
|-------|---------|-------|------|
| 0.95+ | "focus trade", "get aggressive", "love this" | Exceptional | Full+ |
| 0.80+ | "definitely want", "really like" | High | Full |
| 0.60+ | "I'm a buyer", "decent setup" | Medium | Half |
| 0.40+ | "worth watching", "might work" | Low | Quarter |

### Mancini Technical Scoring
| Score | Pattern | Label | Size |
|-------|---------|-------|------|
| 0.90 | Failed Breakdown | FB | Full |
| 0.75 | Level Reclaim | Reclaim | Full |
| 0.65 | Support Test | Support | Half |

### Behavioral Coaching
Real-time alerts for:
- Revenge trading (3+ stops hit)
- Overtrading (10+ trades)
- Low quality trades (taking <0.50 scores)
- Breaking source discipline

## Architecture

Built on IAA (Intent-Aware Assistant) principles:
- **Zero dependencies** - Pure Python stdlib only
- **<1ms response** - No external calls, pure in-memory
- **Single file** - Complete system in ~1000 lines
- **Stateless** - Context flows through, no hidden state
- **Chat-native** - Natural language, no commands to memorize

## Data Models

```python
@dataclass
class TradeIdea:
    ticker: str
    source: str  # "dp" or "mancini" - NEVER mixed
    score: ConvictionScore
    
@dataclass
class Position:
    ticker: str
    source: str
    side: str
    qty: int
    entry: float
    current: float
    stop: Optional[float]
    
@dataclass
class TradingContext:
    phase: str  # PLAN/FOCUS/EXECUTE/MANAGE/REVIEW/COACH
    ideas: List[TradeIdea]
    positions: List[Position]
    realized_pnl: float
    journal: List[str]
```

## Testing the System

### Day 1: Paper Testing
1. Run through complete PFEMRC cycle
2. Verify source separation works
3. Test behavioral alerts
4. Practice save/load workflow

### Week 1: Small Live Testing
1. Use 1/10th normal position sizes
2. Focus on process, not profits
3. Journal every trade decision
4. Review behavioral patterns

### Success Metrics
- Zero source mixing errors ✓
- All trades properly logged ✓
- Behavioral alerts helpful ✓
- Workflow feels natural ✓

## Performance

- Response time: <1ms for all operations
- Memory usage: ~10MB baseline
- File size: Single 50KB Python file
- Dependencies: Zero (Python 3.6+ only)

## License

MIT License - Use it, modify it, profit from it.

---

Built with the IAA philosophy: *"Stateless, bloatless, chat-first, human-native."*