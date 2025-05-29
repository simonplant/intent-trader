# How to Use Intent Trader in your AI Assistant

## THIS RUNS IN YOUR AI ASSISTANT CHAT, NOT PYTHON!

The Intent Trader is designed to run INSIDE your AI Assistant conversations to give you:
- **Structured trading** instead of random chat
- **Persistent memory** of all your positions
- **Systematic scoring** using your DP/Mancini rules
- **Real P&L tracking** throughout the day
- **Behavioral coaching** based on your patterns

## Quick Start (30 seconds)

### Quick Initialize

Start a new AI Assistant chat and say:
```
Run the intent_trader.py file and initialize a trading session

DP Morning Call:
AAPL focus trade love this above 225
CRM definitely want on dips to 165
TSLA not excited below 180

Mancini Newsletter:
ES 5750 failed breakdown setting up
Mode 2 market, stay nimble
```

## How to Trade Throughout the Day

Once initialized, just chat naturally:

```
Morning Planning:
> analyze dp [paste your morning call]
> analyze mancini [paste newsletter]
> create plan
> focus trades

Executing Trades:
> buy 100 AAPL at 225.50
> buy 2 ES at 5751
> show positions

Managing Positions:
> update AAPL 227 TSLA 185
> move stop AAPL 225
> lock 75% on ES
> exit AAPL

Review:
> review session
> show journal
> coach me
```

## Why This is Better Than a Basic AI Assistant Chat

### Without Intent Trader:
```
You: "I'm thinking about buying AAPL"
AI Assistant: "Here are some things to consider..." [generic advice]
You: "What positions do I have?"
AI Assistant: "I don't have that information from earlier..."
```

### With Intent Trader:
```
You: "buy AAPL"
AI Assistant: "EXECUTED: LONG 100 AAPL @ 225.50
        Source: DP (Focus Trade 0.95)
        Stop: 224 (-1.5%)
        Target: 229 (+2R)
        Position 3 of 5 today"

You: "positions"
AI Assistant: "=== OPEN POSITIONS ===
        AAPL: +$127 (+0.8%) DP Focus Trade
        ES: +$350 (7 pts) Mancini FBD
        Total P&L: +$477"
```

## What the AI Assistant Remembers

Throughout your conversation, the AI Assistant tracks:
- Every position (entry, current price, P&L)
- Your analysis (DP bias, Mancini levels)
- Trading patterns (stops hit, revenge trades)
- Journal entries with timestamps
- Behavioral coaching points

## Saving Your Session

At the end of the day:
```
You: "save session"
AI Assistant: [Provides JSON with all your data]

Tomorrow:
You: "Initialize Intent Trader with this context: [paste JSON]"
```

## ⚡ Key Commands Reference

**PLAN:**
- `analyze dp [text]` - Process DP morning call
- `analyze mancini [text]` - Process Mancini levels

**FOCUS:**
- `create plan` - Generate trading plan
- `get premarket data` - Get levels for the watchlist of tickers

**EXECUTE:**
- `buy/sell TICKER` or `buy QTY TICKER @ PRICE`
- `update AAPL 227.50` - Update prices

**MANAGE:**
- `move stop TICKER PRICE` - Adjust stops
- `lock 75` - Take Mancini profits
- `exit TICKER` or `exit all`
- `positions` - Show all positions with P&L

**REVIEW:**
- `review` - Session summary
- `journal` - Show notes

**COACH**
- `coach` - Behavioral feedback


## The Big Picture

Intent Trader turns AI Assistant from a generic chatbot into YOUR trading assistant with:
1. **Memory** - Knows all your positions
2. **Structure** - PFEMRC workflow
3. **Discipline** - Behavioral coaching
4. **Systems** - Your DP/Mancini rules
5. **Tracking** - Real P&L math

## Common Mistakes

**DON'T**: Try to save files to your computer
**DO**: Copy/paste the JSON context AI Assistant provides

**DON'T**: Run it in Python terminal
**DO**: Use it in AI Assistant conversations

**DON'T**: Worry about "installation"
**DO**: Just start chatting with structure

## Ready to Trade!

Start a new AI Assistant chat right now and say:
"Initialize Intent Trader - let's trade with discipline today!"

That's it. You now have a systematic trading assistant with perfect memory.