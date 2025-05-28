# Intent Trader IAA

A stateless, chat-native trading assistant implementing the PFEMRC workflow with dual-source scoring for DP/Inner Circle and Mancini Blueprint systems. Built on Intent-Aware Assistant (IAA) architecture - no dependencies, no bloat, just pure trading logic that responds in <1ms.

## 🎯 What It Does

Intent Trader is a production-ready trading assistant that:
- **Routes trade ideas by source** - Never mixes DP conviction scoring with Mancini technical analysis
- **Scores with system integrity** - DP ideas get 0.0-1.0 conviction scores, Mancini setups get technical pattern scores
- **Manages the complete PFEMRC lifecycle** - PLAN → FOCUS → EXECUTE → MANAGE → REVIEW → COACH
- **Detects behavioral patterns in real-time** - Alerts for revenge trading, overtrading, and discipline breaks
- **Maintains stateless operation** - Context flows through as simple strings, no database needed

## 🚀 Quick Start

```bash
# Clone and run - that's it!
git clone https://github.com/yourusername/intent-trader
cd intent-trader
python intent_trader.py
```

No installation. No dependencies. Just Python 3.6+.

## 📊 Trading Systems Integration

### DP/Inner Circle (Stocks, ETFs, SPX-when-called)
- **Conviction Scoring**: 0.0-1.0 based on language patterns
- **Focus Trades**: 0.90+ conviction ("get aggressive", "love this")
- **Position Sizing**: Tied directly to conviction score
- **Management**: Flexible, sentiment-based adjustments

### Mancini Blueprint (ES Futures, ES-derived SPX)
- **Technical Scoring**: Failed Breakdown patterns score 0.85-0.95
- **Acceptance Validation**: 2-30min based on volatility
- **75% Lock Rule**: Mandatory profit taking at first target
- **Level-to-Level**: Systematic progression tracking

## 💬 Natural Language Commands

The system understands natural trading language across all phases:

### PLAN Phase (Pre-Market)
```
"analyze dp morning call"
"what's mancini saying about ES levels?"
"create today's plan"
"what's the market mode?"
```

### FOCUS Phase (9:00-9:30 AM)
```
"show focus trades"
"dp focus list"
"any failed breakdowns setting up?"
"grade AAPL setup"
```

### EXECUTE Phase (Market Hours)
```
"buy 100 AAPL at 225.50"
"add SPX 5800c" (system asks for source verification)
"size position for CRM risk 500"
"confirm FB acceptance on ES"
```

### MANAGE Phase (Intraday)
```
"positions"
"lock 75% on ES"
"move stop AAPL 223"
"trail the runner"
```

### REVIEW & COACH Phases
```
"review session"
"show performance by source"
"behavioral check"
"coach me"
```

## 🏗️ Architecture

Built on IAA (Intent-Aware Assistant) principles:

```python
# Every user message follows this flow:
message → detect_intent() → route_to_handler() → return_response

# Context is a simple string:
"phase:PLAN|positions:AAPL:L100@225.50|pnl:450.00|dp:bias:BULLISH"

# One file, pure Python, no magic:
- intent_trader.py     # <1ms response time, zero dependencies
```

### Key Design Principles
1. **Single Responsibility**: Each handler does ONE thing perfectly
2. **Source Integrity**: Never mix DP and Mancini scoring
3. **Stateless Operation**: Context passed as flat strings
4. **Chat-Native**: All responses read like natural conversation
5. **<1ms Latency**: Everything in-memory, no external calls

## 📈 Source-Based Scoring System

### DP/Inner Circle Conviction Scale
| Score | Language Patterns | Position Size |
|-------|------------------|---------------|
| 0.90-1.00 | "focus trade", "get aggressive", "love this" | Full size + |
| 0.70-0.89 | "definitely want", "really like", "strong conviction" | Full size |
| 0.50-0.69 | "I'm a buyer", "decent setup", "worth owning" | Half size |
| 0.30-0.49 | "if viable", "worth watching", "might work" | Quarter size |
| 0.00-0.29 | "not excited", "avoid", "stay away" | No trade |

### Mancini Technical Scoring
| Score | Setup Type | Quality Indicators | Position Size |
|-------|------------|-------------------|---------------|
| 0.85-0.95 | Failed Breakdown | Clean flush + rapid reclaim | Full size |
| 0.70-0.84 | Level Reclaim | Multiple tests + acceptance | Full size |
| 0.60-0.69 | Support Test | First approach to major level | Half size |
| 0.40-0.59 | Mode 2 Range | Choppy action, trap potential | Quarter size |

## 🧠 Behavioral Coaching

Real-time pattern detection and intervention:

```
🚨 COACH ALERT: 3 stops hit - Maximum risk reached
🚨 COACH ALERT: Revenge trading detected - Step away
🚨 COACH ALERT: Overtrading after losses - Reduce size
```

The system tracks:
- Stops hit count
- Revenge trading patterns
- Position sizing discipline
- Time-of-day performance
- System adherence (DP vs Mancini rules)

## 🔧 Customization & Extension

### Adding New Intents
```python
# 1. Add to intent_patterns
"NEW_INTENT": ["keyword1", "keyword2", "trigger phrase"]

# 2. Create handler method
def handle_new_intent(self, msg, ctx):
    # Parse message, update context, return response
    return "Response text", updated_ctx

# 3. Map in handlers dict
self.handlers["NEW_INTENT"] = self.handle_new_intent
```

### Adding Broker Integration
```python
# Future API integration point in handle_execute:
if self.broker_api:
    order_id = self.broker_api.place_order(symbol, side, qty, price)
    response += f"\n🔗 Broker Order ID: {order_id}"
```

## 📁 Repository Structure

```
intent-trader/
├── intent_trader.py           # Main implementation (production-ready)
├── iaa_template.py           # Generic IAA framework template
├── requirements-and-language.md  # Full system requirements & vocabulary
├── IAA-architecture-best-practices.md  # Architecture principles
├── docs/
│   └── how-to-design-iaa-requirements.md
└── README.md                 # This file
```

## 🎓 Philosophy

This isn't just another trading bot. It's a discipline enforcement system that:
- Respects the source of each trading idea
- Prevents methodology mixing that destroys edge
- Tracks behavioral patterns that kill profits
- Maintains institutional-grade audit trails
- Responds faster than you can blink (<1ms)

Built for solo traders who want the discipline of a prop firm in a single Python file.

## 📜 License

MIT License - Use it, modify it, make money with it.

## 🤝 Contributing

The codebase is intentionally kept in a single file for easy ownership and modification. Fork it, customize it for your strategy, and keep it simple.

## ⚡ Performance

- **Response Time**: <1ms for all operations
- **Memory Usage**: ~10MB baseline
- **Code Size**: ~400 lines of pure Python
- **Dependencies**: Zero (Python stdlib only)
- **Startup Time**: Instant

---

*"Stateless, bloatless, chat-first, human-native."*

Built with the IAA philosophy: No latency, no magic, all business logic exposed.