# How to Design IAA Applications from Domain Requirements

Based on the Intent Trader development process, here's a complete guide for transforming domain requirements into production-ready IAA applications.

## 📋 Phase 1: Requirements → Feature List

### 1.1 Identify Main User "Actions" (Intents)
Every actionable item or analysis is an intent:
- **Source**: Commands catalog, domain model entities, real workflow
- **Intent Trader Example**: PFEMRC (Plan → Focus → Execute → Manage → Review → Coach)
- **Key Point**: Don't implement as "slash" commands - use natural language

```python
# Requirements say: "analyze DP morning call"
# DON'T: /analyze-dp [text]
# DO: "analyze today's DP call" → handle_analyze_dp()

# Requirements say: "lock 75% profits on Mancini trades"
# DON'T: /lock-profits --percent=75 --source=mancini
# DO: "lock 75" → handle_lock_profits()
```

### 1.2 Map System Outputs to App "Phases" or Context
Every plan, analysis, or generated output becomes a phase/context variable:
```python
# From PLAN, FOCUS, EXECUTE domains:
phase: "PLAN"  # Current workflow phase
dp_analysis: {}  # Output from analyze_dp
mancini_analysis: {}  # Output from analyze_mancini
trade_plan: {}  # Output from create_plan
active_trade_ideas: []  # List of opportunities
positions: []  # Current holdings
trade_log: []  # Historical record
```

### 1.3 Distill Each Intent to its Core Handler
Every command gets ONE handler that manages:
- Input parsing
- Domain logic
- Context updates
- Output formatting

```python
# Handler anatomy:
def handle_analyze_dp(self, message: str) -> str:
    # 1. Parse input (extract transcript)
    # 2. Apply logic (score with DP_CONVICTION_MAP)
    # 3. Update context (self.context.dp_analysis = ...)
    # 4. Format output (return markdown response)
```

## 📊 Phase 2: Design Data Models

### 2.1 Use Modern Dataclasses
Transform domain concepts into type-safe models:
```python
@dataclass
class TradeIdea:
    ticker: str
    source: str  # Critical for domain rules
    score: ConvictionScore
    
@dataclass  
class Position:
    ticker: str
    source: str
    side: str
    qty: int
    entry: float
    current: float
    
    @property
    def pnl(self) -> float:
        # Domain logic embedded in model
        if self.side == "long":
            return (self.current - self.entry) * self.qty
        else:
            return (self.entry - self.current) * self.qty
```

### 2.2 Design Context Structure
Start with MVP context, add fields as needed:
```python
@dataclass
class TradingContext:
    # Workflow phase
    phase: str = "PLAN"
    
    # Analysis outputs (populated by handlers)
    dp_analysis: Dict = field(default_factory=dict)
    mancini_analysis: Dict = field(default_factory=dict)
    trade_plan: Dict = field(default_factory=dict)
    
    # Runtime state
    ideas: List[TradeIdea] = field(default_factory=list)
    positions: List[Position] = field(default_factory=list)
    
    # Tracking
    realized_pnl: float = 0.0
    journal: List[str] = field(default_factory=list)
```

## 🔧 Phase 3: Extend the IAA Template

### 3.1 Add New Intents
Update intent patterns with keywords per action:
```python
def _register_handlers(self):
    return {
        # Natural language patterns
        "analyze dp": self.handle_analyze_dp,
        "analyze mancini": self.handle_analyze_mancini,
        "create plan": self.handle_create_plan,
        "focus trades": self.handle_focus_trades,
        
        # Action words that work naturally
        "buy": self.handle_execute,
        "sell": self.handle_execute,
        "positions": self.handle_positions,
        
        # Keep it conversational
        "help": self.handle_help,
        "save": self.handle_save,
    }
```

### 3.2 Keep Input Natural
Your MVP should support natural conversation:
```python
# Good IAA design accepts multiple phrasings:
"analyze dp AAPL focus trade"  ✓
"analyze today's DP call"  ✓
"what did DP say about AAPL?"  ✓

# All route to same handler through keyword matching
```

### 3.3 Handler Implementation Pattern
Each handler follows the same structure:
```python
def handle_create_plan(self, message: str) -> str:
    """Create unified trading plan from analyses."""
    # 1. Check prerequisites
    if not self.context.dp_analysis:
        return "❌ Run DP analysis first"
    
    # 2. Apply domain logic
    dp_ideas = self._extract_dp_ideas()
    mancini_setups = self._extract_mancini_setups()
    
    # 3. Update context
    self.context.trade_plan = {
        "dp_focus": dp_ideas,
        "mancini_focus": mancini_setups,
        "created": datetime.now()
    }
    self.context.phase = "FOCUS"
    
    # 4. Format response
    response = "=== UNIFIED PLAN ===\n"
    response += f"DP Ideas: {len(dp_ideas)}\n"
    response += f"Mancini Setups: {len(mancini_setups)}\n"
    return response
```

## 🚀 Phase 4: Implementation Checklist

### Step-by-Step Process:
1. **Collect ALL required actions** from docs, domain model, workflow
2. **Design context dict** that stores all outputs/objects to persist
3. **Define keywords** for each intent (natural language)
4. **Write one handler per intent** (1:1 with command)
5. **Connect the dots**: intent matched → handler called → context updated
6. **Test typical workflow**: 
   - "analyze dp [call]"
   - "analyze mancini [newsletter]"  
   - "create plan"
   - "show focus trades"
   - "buy AAPL"
7. **Polish**: Add fallback handler, help system, examples

### 4.1 Start with MVP Commands
What we started with:
- `analyze dp [text]`
- `buy QTY TICKER @ PRICE`
- `positions`

### 4.2 Add Convenience Features
What we added based on testing:
- `buy TICKER` (no qty/price needed)
- `quick AAPL` (instant add)
- `update AAPL 227.50 TSLA 185.20` (batch updates)
- `note AAPL holding overnight` (quick notes)

### 4.3 Refine Language Processing
Evolution during development:
```python
# Version 1: Strict format
pattern = r'buy\s+(\d+)\s+([A-Z]+)\s+@\s+([\d.]+)'

# Version 2: Flexible formats
pattern = r'(?:buy|sell|long|short|add)\s+(?:(\d+)\s+)?([A-Z]+)(?:\s+@\s+|\s+at\s+)?([\d.]+)?'
```

## 📝 Phase 5: Polish for Production

### 5.1 Add Helper Methods
Extract common patterns:
```python
def _extract_symbols(self, text: str) -> List[str]:
    """Extract stock symbols from text."""
    symbols = re.findall(r'\b[A-Z]{2,5}\b', text)
    exclude = {'THE', 'AND', 'FOR', 'BUY', 'SELL'}
    return [s for s in symbols if s not in exclude]
```

### 5.2 Add Domain-Specific Features
Based on requirements:
```python
def _check_behavioral_patterns(self) -> Optional[str]:
    """Real-time behavioral detection."""
    if self.context.stops_hit >= 3:
        return "🚨 COACH ALERT: 3 stops hit - Maximum risk reached!"
```

### 5.3 Make It Chat-Native
Format all responses for conversation:
```python
response = f"""
=== EXECUTED ===
📊 {side.upper()} {qty} {ticker} @ {price}
✓ Source: {source.upper()}
✓ Phase → MANAGE

📈 Management Rules:
• {source_specific_rules}
"""
```

## 🎯 Final Notes

### No Framework or Command Bus
- Your IAA app is readable, hackable, and never needs more than one file
- Your "domain model" and "commands" are not code—they're just what you turn into intents and context variables!
- Scale up? Add more intents and context fields—never break the template

### The IAA Promise
1. **One file** = One owner can understand everything
2. **No magic** = Every transition is traceable
3. **Natural language** = No slash commands or syntax to learn
4. **<1ms response** = Pure in-memory operations
5. **AI-native** = Designed for conversational AI context

### Remember
The goal is a single file that a solo developer can understand, modify, and own completely. No magic, no frameworks, just pure domain logic in <1ms chat-native responses.

Your domain requirements become intents. Your workflow becomes phases. Your language becomes the interface. That's IAA.