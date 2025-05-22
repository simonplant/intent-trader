# Intent-Trader: Streamlined Application Specification

## Executive Summary

Intent-Trader is a conversational AI trading assistant for solo traders implementing PFEMRC methodology (Plan → Focus → Execute → Manage → Review → Coach). Transform natural language into actionable trading decisions using proven JSON schema and focused modules.

**Core Value**: Replace trading checklists with intelligent conversation that remembers your plans and provides contextual decision support.

**Build Target**: Working application in days using Claude-friendly architecture.

---

## System Architecture

```
Natural Language → Intent Classification → PFEMRC Module → Schema Objects → Response
```

**Technology Stack**:
- Python 3.9+ with JSON Schema validation
- OpenAI/Claude APIs for NLP
- Simple file-based storage
- CLI interface (web later)

**Schema Foundation**: All data uses proven runtime schema v0.5.2 with flat structure optimized for Claude context limits.

---

## Core Schema Objects

All modules emit and consume these validated schema objects:

### **tradePlan** - Daily Trading Blueprint
```python
{
  "schemaVersion": "0.5.2",
  "id": "plan-20250521",
  "source": "claude",
  "timestamp": "2025-05-21T08:30:00Z",
  "date": "2025-05-21",
  "tradeIdeas": [...],           # 1-5 primary opportunities
  "scenarioPlanning": [...],     # Bull/bear/neutral scenarios
  "riskManagement": {...},       # Daily risk parameters
  "levelFramework": {...},       # Key price levels
  "marketFramework": {...}       # Market bias and context
}
```

### **tradeIdea** - Individual Trading Opportunity
```python
{
  "schemaVersion": "0.5.2",
  "id": "idea-dp-20250521-AAPL-01",
  "source": "dp",
  "symbol": "AAPL",
  "direction": "long",
  "conviction": {
    "level": "high",             # focus-trade, high, medium, low, negative
    "phrases": ["strong setup", "favorite play"]
  },
  "entryParameters": {
    "zone": {"min": 210.50, "max": 212.75},
    "condition": "entry above 212.75 with volume"
  },
  "exitParameters": {
    "stopLoss": 205.80,
    "target": 225.00,
    "strategy": "Scale out approach"
  },
  "classifications": {
    "isBreakout": true,
    "isFlagPattern": true,
    "isTrendFollow": false
    // ... other boolean flags
  },
  "setup": "bull-flag-breakout",
  "status": "active"
}
```

### **tradePosition** - Active Trade Management
```python
{
  "schemaVersion": "0.5.2",
  "id": "pos-20250521-AAPL-01",
  "symbol": "AAPL",
  "direction": "long",
  "entry": {
    "price": 212.75,
    "date": "2025-05-21",
    "shares": 100
  },
  "stop": 205.80,
  "target": 225.00,
  "status": "open",
  "isRunner": false,
  "isCorePosition": true
}
```

---

## PFEMRC Modules

### **PLAN Module**
Transform morning inputs into structured trading plans.

#### **plan_processor.py**
```python
class PlanProcessor:
    def process_dp_transcript(self, text: str) -> tradePlan:
        """Extract DP morning call into structured plan"""
        
    def process_mancini_pdf(self, pdf_bytes: bytes) -> tradePlan:
        """Parse Mancini newsletter into plan structure"""
        
    def merge_sources(self, dp_plan: tradePlan, mancini_plan: tradePlan) -> tradePlan:
        """Consolidate multiple sources into unified plan"""
```

**Natural Language Interface:**
- "Process today's DP transcript"
- "Parse Mancini newsletter"
- "Create unified plan from morning sources"

### **FOCUS Module**
Prioritize opportunities and prepare for execution.

#### **focus_manager.py**
```python
class FocusManager:
    def rank_trade_ideas(self, plan: tradePlan) -> List[tradeIdea]:
        """Sort opportunities by conviction and setup quality"""
        
    def create_watchlist(self, ideas: List[tradeIdea]) -> List[tradeIdea]:
        """Generate monitored setups with alerts"""
        
    def allocate_risk(self, ideas: List[tradeIdea], daily_budget: float) -> Dict[str, float]:
        """Distribute risk across opportunities"""
```

**Natural Language Interface:**
- "What are my top opportunities today?"
- "Show me high conviction setups"
- "Create watchlist from my plan"

### **EXECUTE Module**
Real-time trade decision support.

#### **trade_evaluator.py**
```python
class TradeEvaluator:
    def evaluate_trade(self, query: str, context: dict) -> str:
        """Analyze 'Should I take this trade?' queries"""
        
    def calculate_position_size(self, idea: tradeIdea, account_size: float) -> int:
        """Determine shares/contracts for trade"""
        
    def validate_entry(self, idea: tradeIdea, current_price: float) -> bool:
        """Check if entry conditions are met"""
```

**Natural Language Interface:**
- "Should I take this AAPL breakout at 213?"
- "What size for this NVDA trade?"
- "Is my TSLA setup still valid?"

### **MANAGE Module**
Active position management using core/runner methodology.

#### **position_manager.py**
```python
class PositionManager:
    def trim_position(self, position: tradePosition, current_price: float) -> str:
        """Apply level-to-level profit taking"""
        
    def adjust_stop(self, position: tradePosition, new_stop: float) -> tradePosition:
        """Update stop loss for risk management"""
        
    def convert_to_runner(self, position: tradePosition, runner_size: int) -> tradePosition:
        """Isolate long-term holding from core position"""
```

**Natural Language Interface:**
- "Should I trim my AAPL position here?"
- "Move my TSLA stop to breakeven"
- "Convert part of NVDA to runner"

### **REVIEW Module**
Performance analysis and learning extraction.

#### **performance_analyzer.py**
```python
class PerformanceAnalyzer:
    def analyze_daily_performance(self, trades: List[tradePosition]) -> dict:
        """Generate end-of-day performance review"""
        
    def compare_plan_vs_execution(self, plan: tradePlan, actual_trades: List[tradePosition]) -> dict:
        """Measure plan adherence"""
        
    def extract_lessons(self, performance_data: dict) -> List[str]:
        """Identify key learning points"""
```

**Natural Language Interface:**
- "How did I do today?"
- "Did I follow my plan?"
- "What should I learn from today?"

### **COACH Module**
Continuous improvement and habit development.

#### **coaching_engine.py**
```python
class CoachingEngine:
    def provide_trade_feedback(self, position: tradePosition) -> str:
        """Immediate learning from completed trades"""
        
    def identify_patterns(self, recent_trades: List[tradePosition]) -> List[str]:
        """Detect behavioral and performance patterns"""
        
    def suggest_improvements(self, performance_history: dict) -> List[str]:
        """Actionable recommendations for skill development"""
```

**Natural Language Interface:**
- "Give feedback on my AAPL trade"
- "What patterns do you see in my trading?"
- "How can I improve my entries?"

---

## Application Core

### **Intent Classification**
```python
class IntentClassifier:
    def classify(self, user_input: str) -> dict:
        """Determine PFEMRC module and specific action"""
        return {
            "module": "execute",
            "action": "evaluate_trade",
            "entities": {"symbol": "AAPL", "price": 213.0}
        }
```

### **Context Manager**
```python
class ContextManager:
    def get_current_context(self) -> dict:
        """Retrieve active plan, positions, and session state"""
        
    def update_context(self, new_data: dict) -> None:
        """Persist state changes from user interactions"""
```

### **Response Generator**
```python
class ResponseGenerator:
    def format_response(self, data: dict, template: str) -> str:
        """Convert schema objects into conversational responses"""
```

---

## File Structure
```
intent-trader/
├── main.py                    # Application entry point
├── src/
│   ├── core/
│   │   ├── intent_classifier.py
│   │   ├── context_manager.py
│   │   └── response_generator.py
│   ├── modules/
│   │   ├── plan_processor.py
│   │   ├── focus_manager.py
│   │   ├── trade_evaluator.py
│   │   ├── position_manager.py
│   │   ├── performance_analyzer.py
│   │   └── coaching_engine.py
│   └── schema/
│       ├── validator.py
│       └── models.py
├── data/
│   ├── plans/
│   ├── positions/
│   └── analysis/
└── config/
    ├── settings.py
    └── prompts/
```

---

## Natural Language Patterns

### **Query Classification**
```python
INTENT_PATTERNS = {
    "plan": ["process DP", "analyze Mancini", "create plan"],
    "focus": ["top opportunities", "watchlist", "high conviction"],
    "execute": ["should I take", "what size", "is setup valid"],
    "manage": ["trim position", "adjust stop", "runner strategy"],
    "review": ["how did I do", "performance", "plan adherence"],
    "coach": ["feedback", "lessons", "improvement"]
}
```

### **Response Templates**
```python
# Trade Evaluation
"""
{symbol} Analysis:
Plan Alignment: ✅ Matches high conviction setup
Entry Quality: Strong - volume confirms breakout
Risk: ${risk} per share ({percentage}% daily budget)
Recommendation: TAKE with {size} shares
Stop: {stop_price} | Target: {target_price}
"""

# Daily Review
"""
Daily Performance: {date}
P&L: ${pnl} ({wins}W/{losses}L)
Plan Adherence: {adherence}%
Best Decision: {best}
Key Lesson: {lesson}
Tomorrow Focus: {focus}
"""
```

---

## Implementation Priority

### **Phase 1: Core Trading Loop (Week 1)**
1. Plan processor for DP/Mancini inputs
2. Trade evaluator for execution decisions
3. Basic position tracking
4. Simple CLI interface

### **Phase 2: Enhancement (Week 2)**
1. Performance analysis and review
2. Coaching feedback system
3. Advanced position management
4. Alert and notification system

### **Phase 3: Polish (Week 3)**
1. Web interface development
2. Integration with data providers
3. Advanced pattern recognition
4. Backup and export features

---

## Success Metrics

**Development Speed**: Working prototype in 3-5 days
**User Experience**: <2 second response time, >90% query accuracy
**Trading Support**: Measurable improvement in plan adherence and decision quality
**Technical Performance**: <200MB memory usage, >99% uptime

---

This streamlined specification focuses on rapid development using your proven schema foundation. The flat structure fits Claude's context limits while providing complete PFEMRC functionality for solo traders. Ready to build in days, not months!