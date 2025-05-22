# Intent-Trader: Complete Application Specification

## Executive Summary

Intent-Trader is a conversational AI-powered trading assistant for solo traders implementing the PFEMRC methodology (Plan → Focus → Execute → Manage → Review → Coach). The application transforms natural language queries into actionable trading insights, supporting decision-making from morning preparation through end-of-day analysis.

**Core Value Proposition**: Replace rigid trading checklists and manual analysis with intelligent conversation that understands your trading style, remembers your plans, and provides contextual decision support throughout the trading day.

---

## Application Overview

### **System Architecture**
```
Natural Language Input → Intent Classification → PFEMRC Module → Contextual Response
```

### **Primary User Workflow**
```
Pre-Market (4:00-9:30 AM):  Plan-DP + Plan-Mancini → Focus-Unified-Plan
Market Hours (9:30-4:00 PM): Execute-* + Manage-* modules for real-time decisions  
Post-Market (4:00+ PM):     Review-* + Coaching-* modules for analysis and learning
```

### **Technology Stack**
- **Core Language**: Python 3.9+
- **AI Integration**: OpenAI/Claude APIs for natural language processing
- **Data Storage**: JSON files for state, SQLite for historical data
- **Interface**: CLI with planned web interface
- **Integration**: PDF processing, audio transcription, market data APIs

---

## Detailed Module Specifications

## PLAN Module

### **Purpose**
Establish market framework and identify potential trading opportunities from analyst sources.

### **Components**

#### **plan-dp.py - DP Morning Call Processor**
```python
class DPProcessor:
    def process_transcript(self, transcript_text: str) -> DPPlan:
        """Process DP morning call transcript into structured trading plan"""
        
    def extract_focus_trades(self, transcript: str) -> List[TradeSetup]:
        """Identify high-conviction opportunities with specific parameters"""
        
    def extract_market_context(self, transcript: str) -> MarketContext:
        """Parse market regime, sentiment, and key catalysts"""
        
    def extract_technical_levels(self, transcript: str) -> List[PriceLevel]:
        """Identify key support/resistance levels mentioned"""
```

**Natural Language Interface:**
- "Process today's DP transcript"
- "What were DP's focus trades today?"
- "What's the market context from DP?"
- "Extract the technical levels from this morning's call"

**Data Output:**
```python
@dataclass
class DPPlan:
    date: datetime
    market_context: MarketContext
    focus_trades: List[TradeSetup]       # 1-4 high conviction ideas
    technical_levels: List[PriceLevel]   # Key S/R levels
    analyst_commentary: str              # Key insights and philosophy
    conviction_signals: Dict[str, str]   # Language indicating confidence
```

#### **plan-mancini.py - Mancini Newsletter Processor**
```python
class ManciniProcessor:
    def process_newsletter(self, pdf_content: bytes) -> ManciniPlan:
        """Process Mancini newsletter PDF into structured plan"""
        
    def extract_mode_classification(self, content: str) -> ModeClassification:
        """Determine Mode 1 (trend) vs Mode 2 (range/trap) assessment"""
        
    def extract_level_framework(self, content: str) -> List[PriceLevel]:
        """Parse precise support/resistance levels with classifications"""
        
    def extract_failed_breakdown_analysis(self, content: str) -> FailedBreakdownSetup:
        """Identify core Mancini setup opportunities"""
        
    def extract_runner_management(self, content: str) -> RunnerStatus:
        """Parse status of active runners and management protocol"""
```

**Natural Language Interface:**
- "Process today's Mancini newsletter"
- "What's Mancini's mode classification?"
- "Show me the key levels from Mancini"
- "What's the failed breakdown setup today?"
- "How should I manage my runners according to Mancini?"

**Data Output:**
```python
@dataclass
class ManciniPlan:
    date: datetime
    mode_classification: str             # "Mode 1" or "Mode 2"
    key_levels: List[PriceLevel]        # Precise S/R with classifications
    failed_breakdown_setup: TradeSetup  # Core Mancini opportunity
    bull_bear_scenarios: Dict[str, str] # Conditional projections
    runner_management: RunnerStatus      # Active position guidance
    level_to_level_plan: str            # Profit-taking methodology
```

#### **market-regime.py - Regime Classification Engine**
```python
class RegimeClassifier:
    def classify_current_regime(self, market_data: MarketData) -> RegimeClassification:
        """Determine 'buy dips' vs 'sell bounces' classification"""
        
    def assess_regime_strength(self, historical_data: List[MarketData]) -> float:
        """Measure conviction in current regime classification"""
        
    def identify_transition_signals(self, recent_data: List[MarketData]) -> List[str]:
        """Detect indicators of possible regime change"""
        
    def compare_historical_regimes(self, current: RegimeClassification) -> List[str]:
        """Find similar historical periods for context"""
```

**Natural Language Interface:**
- "What's the current market regime?"
- "How strong is this regime?"
- "Any signs of regime change?"
- "What historical periods are similar to now?"

#### **scenario-planning.py - Market Scenario Framework**
```python
class ScenarioPlanner:
    def create_market_scenarios(self, context: MarketContext) -> List[MarketScenario]:
        """Generate primary and alternative market scenarios"""
        
    def define_trigger_conditions(self, scenarios: List[MarketScenario]) -> Dict[str, List[str]]:
        """Identify what activates each scenario"""
        
    def create_response_framework(self, scenarios: List[MarketScenario]) -> Dict[str, str]:
        """Define how to adapt trading to each scenario"""
        
    def assess_scenario_probabilities(self, scenarios: List[MarketScenario]) -> Dict[str, float]:
        """Estimate likelihood of each scenario developing"""
```

**Natural Language Interface:**
- "What are today's market scenarios?"
- "What would trigger the bearish scenario?"
- "How should I trade if we get scenario 2?"
- "What's the probability of each scenario?"

---

## FOCUS Module

### **Purpose**
Consolidate multiple plan sources into prioritized, actionable trading opportunities.

### **Components**

#### **focus-unified-plan.py - Plan Consolidation Engine**
```python
class UnifiedPlanCreator:
    def consolidate_plans(self, dp_plan: DPPlan, mancini_plan: ManciniPlan, 
                         personal_analysis: Optional[str] = None) -> UnifiedPlan:
        """Merge multiple sources into single actionable plan"""
        
    def resolve_conflicts(self, conflicting_setups: List[TradeSetup]) -> List[TradeSetup]:
        """Handle contradictory trade ideas from different sources"""
        
    def prioritize_opportunities(self, all_setups: List[TradeSetup]) -> List[TradeSetup]:
        """Rank opportunities by conviction, risk/reward, and timing"""
        
    def allocate_risk_budget(self, prioritized_setups: List[TradeSetup]) -> Dict[str, float]:
        """Distribute daily risk across selected opportunities"""
```

**Natural Language Interface:**
- "Create my unified trading plan for today"
- "Consolidate DP and Mancini plans"
- "What are my top 3 opportunities?"
- "How should I allocate risk today?"
- "Any conflicts between my sources?"

**Data Output:**
```python
@dataclass
class UnifiedPlan:
    date: datetime
    primary_setups: List[TradeSetup]     # 1-3 highest conviction
    secondary_setups: List[TradeSetup]   # 3-5 backup opportunities
    key_levels: List[PriceLevel]         # All important S/R levels
    risk_allocation: Dict[str, float]    # Risk budget distribution
    market_bias: str                     # Overall directional bias
    focus_themes: List[str]              # Key themes to watch
    correlation_warnings: List[str]      # Related exposure risks
```

#### **conviction-ranking.py - Confidence Assessment System**
```python
class ConvictionRanker:
    def analyze_conviction_language(self, commentary: str) -> float:
        """Parse language patterns to determine confidence level"""
        
    def standardize_conviction_levels(self, raw_confidence: float) -> str:
        """Convert to High/Medium/Low framework"""
        
    def weight_by_source_accuracy(self, conviction: float, source: str) -> float:
        """Adjust confidence based on historical source performance"""
        
    def identify_confluence_factors(self, setup: TradeSetup) -> List[str]:
        """Find supporting elements that increase conviction"""
```

**Natural Language Interface:**
- "What's the conviction level on AAPL?"
- "Rank my setups by confidence"
- "What factors support this trade idea?"
- "How confident should I be in this setup?"

#### **watchlist-manager.py - Active Setup Tracking**
```python
class WatchlistManager:
    def create_active_watchlist(self, unified_plan: UnifiedPlan) -> Watchlist:
        """Convert plan into monitored opportunities"""
        
    def update_setup_status(self, symbol: str, new_status: str) -> None:
        """Track setup state changes (pending/triggered/invalidated)"""
        
    def filter_by_criteria(self, criteria: Dict[str, Any]) -> List[TradeSetup]:
        """Apply filters for time, conviction, risk, etc."""
        
    def organize_by_priority(self, setups: List[TradeSetup]) -> Dict[str, List[TradeSetup]]:
        """Group setups by execution priority"""
```

**Natural Language Interface:**
- "Show me my active watchlist"
- "What setups are ready to trigger?"
- "Filter for high conviction only"
- "Organize my watchlist by priority"

#### **alert-config.py - Notification Management**
```python
class AlertConfigurator:
    def create_price_alerts(self, watchlist: Watchlist) -> List[PriceAlert]:
        """Generate alerts for key levels and trigger points"""
        
    def create_condition_alerts(self, conditions: List[str]) -> List[ConditionalAlert]:
        """Set up complex trigger alerts (volume, momentum, etc.)"""
        
    def prioritize_alert_urgency(self, alerts: List[Alert]) -> Dict[str, List[Alert]]:
        """Classify alerts by urgency and importance"""
        
    def manage_alert_lifecycle(self, alerts: List[Alert]) -> List[Alert]:
        """Handle expiration and cleanup of old alerts"""
```

**Natural Language Interface:**
- "Set alerts for my watchlist"
- "Alert me when TSLA breaks 250"
- "Set up momentum alerts for my setups"
- "Show me my active alerts"

---

## EXECUTE Module

### **Purpose**
Support real-time trading decisions with entry timing, sizing, and trade evaluation.

### **Components**

#### **execute-evaluate-trade.py - Trade Decision Support**
```python
class TradeEvaluator:
    def evaluate_trade_decision(self, proposed_trade: str, current_context: TradingContext) -> TradeEvaluation:
        """Analyze 'Should I take this trade?' queries"""
        
    def check_plan_alignment(self, trade: TradeSetup, plan: UnifiedPlan) -> PlanAlignment:
        """Verify trade matches prepared opportunities"""
        
    def assess_current_risk(self, new_trade: TradeSetup, positions: List[Position]) -> RiskAssessment:
        """Evaluate risk impact of additional position"""
        
    def validate_entry_conditions(self, trade: TradeSetup, market_data: MarketData) -> EntryValidation:
        """Confirm technical and fundamental entry criteria"""
```

**Natural Language Interface:**
- "Should I take this AAPL breakout at 150?"
- "NVDA just broke resistance with volume, thoughts?"
- "Is this SPY setup still valid?"
- "Should I chase this TSLA move?"

**Response Format:**
```python
@dataclass
class TradeEvaluation:
    recommendation: str              # "TAKE", "PASS", "WAIT", "PARTIAL"
    reasoning: List[str]            # Bullet points supporting decision
    plan_alignment: str             # How well it matches prepared plan
    risk_assessment: RiskMetrics    # Position size and risk impact
    entry_timing: str               # Immediate, wait for pullback, etc.
    alternative_suggestions: List[str] # Other approaches to consider
```

#### **execute-sizing.py - Position Sizing Calculator**
```python
class PositionSizer:
    def calculate_position_size(self, trade: TradeSetup, account: AccountInfo, 
                               risk_budget: float) -> PositionSize:
        """Determine optimal position size based on risk parameters"""
        
    def adjust_for_conviction(self, base_size: int, conviction_level: str) -> int:
        """Scale position size based on confidence level"""
        
    def check_correlation_limits(self, new_position: Position, 
                                existing_positions: List[Position]) -> bool:
        """Ensure new position doesn't create excessive correlation"""
        
    def apply_volatility_adjustment(self, base_size: int, volatility: float) -> int:
        """Adapt size for current market volatility"""
```

**Natural Language Interface:**
- "What size should I use for this AMZN trade?"
- "How big can I go on this high conviction GOOGL setup?"
- "Check correlation limits for this AAPL position"
- "Adjust my size for current volatility"

#### **execute-orders.py - Order Management**
```python
class OrderManager:
    def recommend_order_type(self, trade: TradeSetup, market_conditions: MarketData) -> OrderRecommendation:
        """Suggest market vs limit order based on conditions"""
        
    def calculate_limit_price(self, trade: TradeSetup, current_price: float) -> float:
        """Determine optimal limit order placement"""
        
    def create_bracket_orders(self, entry: Order, stop: float, target: float) -> BracketOrder:
        """Set up stop and target orders with entry"""
        
    def handle_partial_fills(self, order: Order, fill_info: FillInfo) -> List[Order]:
        """Manage incomplete order execution"""
```

**Natural Language Interface:**
- "Should I use a market or limit order for TSLA?"
- "Where should I place my limit for this QQQ entry?"
- "Set up bracket orders for my MSFT trade"
- "My AAPL order only partially filled, what now?"

#### **execute-alerts.py - Real-Time Monitoring**
```python
class ExecutionAlerter:
    def monitor_trigger_conditions(self, watchlist: Watchlist, market_data: MarketData) -> List[TriggerAlert]:
        """Check for setup activation signals"""
        
    def detect_invalidation_signals(self, watchlist: Watchlist, market_data: MarketData) -> List[InvalidationAlert]:
        """Identify when setups are no longer valid"""
        
    def track_momentum_changes(self, positions: List[Position], market_data: MarketData) -> List[MomentumAlert]:
        """Monitor momentum shifts in active positions"""
        
    def generate_urgency_notifications(self, alerts: List[Alert]) -> List[UrgentAlert]:
        """Prioritize time-sensitive opportunities"""
```

**Natural Language Interface:**
- "Any setups triggering now?"
- "Check if my NVDA setup is still valid"
- "Monitor momentum on my active positions"
- "What needs immediate attention?"

#### **entry-timing.py - Timing Optimization**
```python
class EntryTimer:
    def identify_optimal_entry_window(self, setup: TradeSetup, market_data: MarketData) -> TimeWindow:
        """Determine best timing for trade entry"""
        
    def assess_signal_strength(self, trigger_conditions: List[str], market_data: MarketData) -> float:
        """Measure quality of entry signal"""
        
    def validate_volume_confirmation(self, required_volume: float, current_volume: float) -> bool:
        """Confirm adequate volume for reliable entry"""
        
    def recommend_entry_approach(self, setup: TradeSetup, market_conditions: str) -> str:
        """Suggest scaling, all-in, or test position approach"""
```

**Natural Language Interface:**
- "Is now a good time to enter AAPL?"
- "How strong is this entry signal?"
- "Does volume support this entry?"
- "Should I scale in or go all-in?"

---

## MANAGE Module

### **Purpose**
Handle active positions for optimal outcomes using core/runner methodology.

### **Components**

#### **manage-trading-around-core.py - Core Position Management**
```python
class CorePositionManager:
    def establish_core_position(self, entry: TradeEntry) -> CorePosition:
        """Set up initial position with core/buffer structure"""
        
    def track_profit_buffer(self, position: CorePosition, current_price: float) -> float:
        """Monitor accumulated gains beyond initial risk"""
        
    def assess_reentry_opportunities(self, position: CorePosition, market_data: MarketData) -> bool:
        """Evaluate adding to position at better prices"""
        
    def monitor_position_character(self, position: CorePosition, price_action: PriceAction) -> str:
        """Track behavioral changes in position performance"""
```

**Natural Language Interface:**
- "How's my TSLA core position doing?"
- "Should I add to my AMZN position here?"
- "What's my profit buffer on GOOGL?"
- "Has my NVDA position character changed?"

#### **manage-mancini.py - Level-to-Level Management**
```python
class ManciniManager:
    def implement_level_to_level(self, position: Position, key_levels: List[PriceLevel]) -> ManagementPlan:
        """Apply Mancini's level-to-level profit-taking methodology"""
        
    def identify_next_target_level(self, current_price: float, levels: List[PriceLevel]) -> PriceLevel:
        """Determine next resistance level for profit-taking"""
        
    def calculate_trim_percentage(self, position: Position, target_level: PriceLevel) -> float:
        """Determine how much to trim at each level"""
        
    def adjust_runner_strategy(self, runner: RunnerPosition, market_conditions: str) -> RunnerStrategy:
        """Adapt long-term position management to conditions"""
```

**Natural Language Interface:**
- "Apply Mancini management to my SPY position"
- "What's the next level to take profits on QQQ?"
- "How much should I trim at this resistance?"
- "Update my runner strategy for current conditions"

#### **trimming-protocol.py - Profit Taking System**
```python
class TrimmingProtocol:
    def apply_75_15_10_rule(self, position: Position, profit_target: float) -> TrimPlan:
        """Implement systematic profit-taking methodology"""
        
    def execute_target_based_reduction(self, position: Position, targets: List[float]) -> List[TrimOrder]:
        """Take profits at predetermined price objectives"""
        
    def scale_out_gradually(self, position: Position, price_action: PriceAction) -> ScaleOutPlan:
        """Gradual position reduction based on momentum"""
        
    def preserve_profit_buffer(self, position: Position, market_volatility: float) -> float:
        """Maintain gains while allowing for continued upside"""
```

**Natural Language Interface:**
- "Should I trim my AAPL position here?"
- "Apply the 75/15/10 rule to my MSFT trade"
- "Take some profits on this AMZN move"
- "Preserve my gains on this volatile day"

#### **adding-protocol.py - Position Building**
```python
class AddingProtocol:
    def validate_add_criteria(self, position: Position, market_data: MarketData) -> bool:
        """Confirm conditions are right for increasing position"""
        
    def calculate_add_size(self, existing_position: Position, account_risk: float) -> int:
        """Determine appropriate size for additional entry"""
        
    def update_average_price(self, position: Position, add_price: float, add_size: int) -> float:
        """Recalculate cost basis with new entry"""
        
    def enforce_maximum_limits(self, position: Position, proposed_add: int) -> int:
        """Ensure additions don't exceed position size limits"""
```

**Natural Language Interface:**
- "Can I add to my TSLA position here?"
- "What size should I add to NVDA?"
- "Update my average price with this addition"
- "Check my position size limits before adding"

#### **stop-adjustment.py - Dynamic Risk Management**
```python
class StopAdjuster:
    def move_to_breakeven(self, position: Position, profit_threshold: float) -> float:
        """Eliminate risk when position reaches profit threshold"""
        
    def implement_trailing_stop(self, position: Position, trailing_distance: float) -> float:
        """Dynamic stop that follows favorable price movement"""
        
    def adjust_for_volatility(self, stop_price: float, volatility_measure: float) -> float:
        """Adapt stop distance for current market volatility"""
        
    def respond_to_character_change(self, position: Position, behavior_change: str) -> float:
        """Tighten or loosen stops based on price behavior changes"""
```

**Natural Language Interface:**
- "Move my AAPL stop to breakeven"
- "Set a trailing stop on my GOOGL runner"
- "Adjust my stops for this volatility"
- "My AMZN position character changed, adjust stop"

#### **runner-management.py - Long-Term Position Handling**
```python
class RunnerManager:
    def isolate_runner_from_core(self, position: Position, isolation_criteria: str) -> RunnerPosition:
        """Separate long-term holdings from active trading position"""
        
    def set_runner_stop_protocol(self, runner: RunnerPosition, stop_type: str) -> float:
        """Establish appropriate stop methodology for runners"""
        
    def evaluate_runner_exit_criteria(self, runner: RunnerPosition, market_context: MarketContext) -> bool:
        """Assess when to close long-term positions"""
        
    def manage_runner_additions(self, runner: RunnerPosition, opportunity: TradeSetup) -> bool:
        """Determine if/when to add to existing runners"""
```

**Natural Language Interface:**
- "Convert part of my TSLA position to a runner"
- "How should I manage my NVDA runner?"
- "Should I exit my QQQ runner here?"
- "Add to my AMZN runner on this pullback?"

---

## REVIEW Module

### **Purpose**
Analyze trading performance for continuous improvement and learning.

### **Components**

#### **review-end-of-day.py - Daily Performance Analysis**
```python
class EndOfDayReviewer:
    def analyze_daily_performance(self, trades: List[CompletedTrade], plan: UnifiedPlan) -> DailyReview:
        """Comprehensive analysis of day's trading results"""
        
    def compare_plan_vs_execution(self, plan: UnifiedPlan, actual_trades: List[Trade]) -> PlanAdherence:
        """Measure how well execution matched prepared plan"""
        
    def calculate_performance_metrics(self, trades: List[CompletedTrade]) -> PerformanceMetrics:
        """Generate P&L, win rate, R-multiple statistics"""
        
    def identify_key_lessons(self, performance: PerformanceMetrics, plan_adherence: PlanAdherence) -> List[str]:
        """Extract main learning points from day's activity"""
```

**Natural Language Interface:**
- "How did I do today?"
- "Analyze my daily performance"
- "Did I follow my plan?"
- "What were today's key lessons?"

**Response Format:**
```python
@dataclass
class DailyReview:
    date: datetime
    total_pnl: float
    number_of_trades: int
    win_rate: float
    average_r_multiple: float
    plan_adherence_score: float     # 0-100%
    best_decision: str              # Highlight of the day
    worst_decision: str             # Biggest mistake
    key_lessons: List[str]          # Main takeaways
    tomorrow_focus: List[str]       # Areas for improvement
```

#### **review-vs-optimal-performance.py - Perfect Trader Analysis**
```python
class OptimalPerformanceAnalyzer:
    def calculate_optimal_trading(self, plan: UnifiedPlan, market_action: MarketData) -> OptimalTrading:
        """Determine perfect execution given plan and actual market movements"""
        
    def identify_missed_opportunities(self, plan: UnifiedPlan, market_movements: Dict[str, float]) -> List[MissedTrade]:
        """Find trades that should have been taken but weren't"""
        
    def analyze_suboptimal_decisions(self, actual_trades: List[Trade], optimal_trades: List[Trade]) -> List[Improvement]:
        """Compare actual vs optimal trade execution"""
        
    def calculate_opportunity_cost(self, optimal_pnl: float, actual_pnl: float) -> OpportunityCost:
        """Quantify the cost of suboptimal decisions"""
```

**Natural Language Interface:**
- "How should I have traded today optimally?"
- "What opportunities did I miss?"
- "Compare my trading to perfect execution"
- "What was the cost of my mistakes?"

**Response Format:**
```python
@dataclass
class OptimalTrading:
    theoretical_max_pnl: float
    actual_pnl: float
    efficiency_percentage: float     # Actual vs theoretical
    missed_opportunities: List[MissedTrade]
    suboptimal_decisions: List[Improvement]
    key_decision_points: List[str]   # Critical moments
    improvement_suggestions: List[str] # Specific recommendations
```

#### **trade-logging.py - Structured Record Keeping**
```python
class TradeLogger:
    def log_trade_entry(self, trade: Trade, context: TradingContext) -> TradeRecord:
        """Record trade entry with full context"""
        
    def log_trade_exit(self, trade: Trade, exit_reason: str, market_context: MarketContext) -> TradeRecord:
        """Record trade exit with analysis"""
        
    def capture_decision_rationale(self, decision: TradingDecision, reasoning: str) -> DecisionRecord:
        """Document why specific decisions were made"""
        
    def preserve_market_context(self, timestamp: datetime, market_state: MarketData) -> ContextRecord:
        """Save market conditions for future analysis"""
```

**Natural Language Interface:**
- "Log my AAPL trade entry"
- "Record why I exited TSLA early"
- "Document my decision to pass on NVDA"
- "Save current market context"

#### **performance-metrics.py - Statistical Analysis**
```python
class PerformanceMetricsCalculator:
    def calculate_r_multiples(self, trades: List[CompletedTrade]) -> List[float]:
        """Compute risk-adjusted returns for each trade"""
        
    def analyze_win_rate_by_setup(self, trades: List[CompletedTrade]) -> Dict[str, float]:
        """Break down success rates by trade type"""
        
    def compute_expectancy(self, trades: List[CompletedTrade]) -> float:
        """Calculate expected value per trade"""
        
    def track_consecutive_performance(self, trades: List[CompletedTrade]) -> Dict[str, int]:
        """Monitor winning/losing streaks"""
```

**Natural Language Interface:**
- "What's my R-multiple distribution?"
- "Show win rates by setup type"
- "Calculate my trading expectancy"
- "Track my recent streak performance"

#### **pattern-recognition.py - Behavioral Analysis**
```python
class PatternRecognizer:
    def identify_time_patterns(self, trades: List[CompletedTrade]) -> Dict[str, float]:
        """Find performance patterns by time of day/week"""
        
    def analyze_psychological_patterns(self, decisions: List[DecisionRecord]) -> List[PsychPattern]:
        """Detect behavioral tendencies and biases"""
        
    def find_market_condition_correlations(self, trades: List[CompletedTrade], 
                                          market_data: List[MarketData]) -> Dict[str, float]:
        """Correlate performance with market environments"""
        
    def detect_recurring_mistakes(self, poor_decisions: List[DecisionRecord]) -> List[RecurringError]:
        """Identify repeated errors for focused improvement"""
```

**Natural Language Interface:**
- "What are my time-of-day patterns?"
- "Show my psychological tendencies"
- "How do I perform in different market conditions?"
- "What mistakes do I keep repeating?"

---

## COACHING Module

### **Purpose**
Facilitate continuous learning and habit development for trading improvement.

### **Components**

#### **coaching-feedback.py - Immediate Learning Support**
```python
class CoachingFeedback:
    def provide_trade_feedback(self, completed_trade: CompletedTrade) -> TradeFeedback:
        """Immediate analysis and lessons from individual trades"""
        
    def identify_decision_quality(self, decision: TradingDecision, outcome: TradeOutcome) -> DecisionQuality:
        """Assess whether decisions were good regardless of outcome"""
        
    def suggest_immediate_improvements(self, recent_performance: List[Trade]) -> List[Improvement]:
        """Provide actionable suggestions based on recent activity"""
        
    def reinforce_positive_behaviors(self, successful_decisions: List[DecisionRecord]) -> List[Reinforcement]:
        """Highlight and strengthen good trading behaviors"""
```

**Natural Language Interface:**
- "Give me feedback on my AAPL trade"
- "Was my decision to exit TSLA good?"
- "What should I improve after today?"
- "What did I do well today?"

#### **coaching-habits.py - Behavioral Development**
```python
class HabitCoach:
    def design_habit_formation_plan(self, target_behavior: str) -> HabitPlan:
        """Create structured approach to developing new trading habits"""
        
    def track_habit_consistency(self, habit: TradingHabit, recent_activity: List[TradingSession]) -> HabitProgress:
        """Monitor progress on habit development"""
        
    def identify_habit_triggers(self, desired_habit: str, trading_context: TradingContext) -> List[Trigger]:
        """Find environmental cues that can prompt good habits"""
        
    def suggest_habit_stacking(self, existing_habits: List[TradingHabit], new_habit: str) -> HabitStack:
        """Link new habits to established routines"""
```

**Natural Language Interface:**
- "Help me develop better entry timing habits"
- "Track my progress on stop-loss discipline"
- "What triggers can help me stick to my plan?"
- "Stack a new habit with my morning routine"

#### **lesson-extraction.py - Knowledge Building**
```python
class LessonExtractor:
    def extract_strategic_lessons(self, performance_period: List[TradingSession]) -> List[StrategicLesson]:
        """Identify high-level insights about trading approach"""
        
    def compile_tactical_learnings(self, trade_data: List[CompletedTrade]) -> List[TacticalLesson]:
        """Document specific execution improvements"""
        
    def create_personal_trading_rules(self, lessons: List[Lesson]) -> List[PersonalRule]:
        """Convert insights into actionable trading rules"""
        
    def update_knowledge_base(self, new_lessons: List[Lesson]) -> KnowledgeBase:
        """Integrate learning into permanent knowledge system"""
```

**Natural Language Interface:**
- "What strategic lessons should I learn from this week?"
- "Extract tactical improvements from my recent trades"
- "Create new rules based on my lessons"
- "Update my trading knowledge base"

#### **skill-development.py - Capability Enhancement**
```python
class SkillDeveloper:
    def assess_current_skill_levels(self, performance_data: PerformanceHistory) -> SkillAssessment:
        """Evaluate current capabilities across trading skills"""
        
    def identify_skill_gaps(self, current_skills: SkillAssessment, target_performance: PerformanceTarget) -> List[SkillGap]:
        """Find areas needing development for improved performance"""
        
    def create_development_exercises(self, skill_gaps: List[SkillGap]) -> List[Exercise]:
        """Design practice activities for skill improvement"""
        
    def track_skill_progression(self, exercises: List[Exercise], recent_performance: PerformanceData) -> ProgressReport:
        """Monitor improvement in targeted skill areas"""
```

**Natural Language Interface:**
- "Assess my current trading skills"
- "What skills should I focus on developing?"
- "Create exercises to improve my entry timing"
- "Track my progress on risk management skills"

---

## Data Models and Schemas

### **Core Trading Entities**

#### **TradeSetup**
```python
@dataclass
class TradeSetup:
    symbol: str
    setup_type: str              # "Breakout", "Failed Breakdown", "Pullback"
    conviction: str              # "High", "Medium", "Low"
    entry_range: Tuple[float, float]  # (low, high)
    stop_loss: float
    target: float
    risk_reward_ratio: float
    notes: str
    source: str                  # "DP", "Mancini", "Personal"
    timestamp: datetime
    market_context: str          # Conditions when identified
```

#### **Position**
```python
@dataclass
class Position:
    symbol: str
    entry_price: float
    current_price: float
    quantity: int
    stop_loss: float
    target: float
    entry_time: datetime
    
    # Core/Runner methodology
    core_size: int               # Base position
    profit_buffer: float         # Gains beyond initial risk
    runner_size: int             # Long-term holding
    
    # Management tracking
    management_style: str        # "Mancini", "DP", "Personal"
    trim_history: List[Trim]     # Previous profit-taking
    add_history: List[Addition]  # Position building record
    
    # Performance
    unrealized_pnl: float
    max_favorable_excursion: float
    max_adverse_excursion: float
```

#### **TradingPlan**
```python
@dataclass
class UnifiedPlan:
    date: datetime
    created_at: datetime
    
    # Core opportunities
    primary_setups: List[TradeSetup]      # 1-3 highest conviction
    secondary_setups: List[TradeSetup]    # 3-5 backup opportunities
    
    # Market framework
    market_bias: str                      # "Bullish", "Bearish", "Neutral"
    key_levels: List[PriceLevel]          # Important S/R levels
    market_regime: str                    # "Buy dips" vs "Sell bounces"
    mode_classification: str              # "Mode 1" vs "Mode 2"
    
    # Risk management
    daily_risk_budget: float              # Total risk for day
    max_position_size: float              # Largest individual position
    correlation_limits: Dict[str, float]  # Sector/theme exposure limits
    
    # Sources and notes
    source_plans: Dict[str, Any]          # Original DP/Mancini plans
    focus_themes: List[str]               # Key themes to watch
    potential_catalysts: List[str]        # News/events to monitor
```

#### **TradingContext**
```python
@dataclass
class TradingContext:
    # Session information
    current_date: datetime
    market_phase: str            # "PREMARKET", "OPEN", "CLOSE", "AFTERHOURS"
    session_pnl: float
    
    # Active state
    open_positions: List[Position]
    watchlist: List[TradeSetup]
    active_alerts: List[Alert]
    daily_plan: UnifiedPlan
    
    # Risk status
    current_risk_usage: float    # Percentage of daily budget used
    correlation_exposure: Dict[str, float]  # Current sector exposure
    
    # Market conditions
    market_volatility: str       # "LOW", "NORMAL", "HIGH"
    overall_trend: str          # "UP", "DOWN", "SIDEWAYS"
    volume_profile: str         # "LOW", "AVERAGE", "HIGH"
```

### **Analysis and Performance Entities**

#### **TradeRecord**
```python
@dataclass
class CompletedTrade:
    # Basic information
    symbol: str
    entry_price: float
    exit_price: float
    quantity: int
    entry_time: datetime
    exit_time: datetime
    
    # Performance metrics
    gross_pnl: float
    net_pnl: float              # After commissions
    r_multiple: float           # Risk-adjusted return
    hold_duration: timedelta
    
    # Context and reasoning
    setup_type: str
    entry_reason: str
    exit_reason: str
    market_context_entry: MarketContext
    market_context_exit: MarketContext
    
    # Classification
    trade_category: str         # "Scalp", "Swing", "Runner"
    conviction_level: str       # "High", "Medium", "Low"
    plan_adherence: bool        # Was this planned?
```

#### **PerformanceMetrics**
```python
@dataclass
class PerformanceMetrics:
    # Time period
    start_date: datetime
    end_date: datetime
    
    # Basic metrics
    total_pnl: float
    number_of_trades: int
    win_rate: float
    average_win: float
    average_loss: float
    
    # Risk-adjusted metrics
    profit_factor: float        # Gross wins / Gross losses
    expectancy: float           # Expected value per trade
    sharpe_ratio: float         # Risk-adjusted returns
    
    # Advanced metrics
    consecutive_wins: int
    consecutive_losses: int
    largest_win: float
    largest_loss: float
    
    # Breakdown by category
    performance_by_setup: Dict[str, float]
    performance_by_time: Dict[str, float]
    performance_by_conviction: Dict[str, float]
```

---

## Natural Language Interface Patterns

### **Intent Classification System**
```python
INTENT_PATTERNS = {
    # Planning phase
    "process_morning_inputs": [
        "process morning inputs",
        "analyze today's plans",
        "what do we have for today"
    ],
    
    # Focus phase
    "create_unified_plan": [
        "create unified plan",
        "consolidate plans",
        "what should I focus on"
    ],
    
    # Execution phase
    "evaluate_trade": [
        "should I take this {symbol} trade",
        "what do you think about {symbol}",
        "is this {symbol} setup good"
    ],
    
    "position_sizing": [
        "what size for {symbol}",
        "how big should I go",
        "position size for this trade"
    ],
    
    # Management phase
    "position_management": [
        "should I trim {symbol}",
        "where should my stop be",
        "manage my {symbol} position"
    ],
    
    # Review phase
    "daily_review": [
        "how did I do today",
        "analyze my performance",
        "review today's trading"
    ],
    
    "optimal_analysis": [
        "how should I have traded",
        "what did I miss",
        "optimal trading analysis"
    ],
    
    # Coaching phase
    "learning_feedback": [
        "what should I learn",
        "feedback on my trading",
        "how can I improve"
    ]
}
```

### **Response Templates**

#### **Trade Evaluation Response**
```
Analyzing {symbol} trade at {price}...

Plan Alignment:
✅/⚠️/❌ {alignment_status}

Risk Assessment:
• Position size: {recommended_size} shares
• Risk per share: ${risk_amount}
• Portfolio impact: {percentage}% of daily budget

Entry Quality:
• Technical setup: {setup_evaluation}
• Volume confirmation: {volume_status}
• Timing: {timing_assessment}

Recommendation: {TAKE/PASS/WAIT/PARTIAL}

Reasoning:
• {reason_1}
• {reason_2}
• {reason_3}

Alternative: {alternative_suggestion}
```

#### **Daily Review Response**
```
Daily Performance Review - {date}

Overall: {performance_summary}
• P&L: ${total_pnl} ({win_count}W/{loss_count}L)
• Plan adherence: {adherence_percentage}%
• Risk usage: {risk_percentage}% of budget

Best Decision: {best_decision_description}
Worst Decision: {worst_decision_description}

Key Lessons:
• {lesson_1}
• {lesson_2}
• {lesson_3}

Tomorrow's Focus:
• {focus_area_1}
• {focus_area_2}
```

---

## Technical Implementation

### **Application Architecture**
```python
# Main application entry point
class IntentTrader:
    def __init__(self):
        # Initialize modules
        self.plan = PlanModule()
        self.focus = FocusModule()
        self.execute = ExecuteModule()
        self.manage = ManageModule()
        self.review = ReviewModule()
        self.coaching = CoachingModule()
        
        # Initialize support systems
        self.context = TradingContextManager()
        self.data = DataManager()
        self.nlp = NaturalLanguageProcessor()
        
    def process_query(self, user_input: str) -> str:
        """Main entry point for all user interactions"""
        
        # Parse intent and extract entities
        intent = self.nlp.classify_intent(user_input)
        entities = self.nlp.extract_entities(user_input)
        
        # Get current context
        context = self.context.get_current_context()
        
        # Route to appropriate module
        if intent.category == "PLAN":
            response = self.plan.handle_request(intent, entities, context)
        elif intent.category == "FOCUS":
            response = self.focus.handle_request(intent, entities, context)
        elif intent.category == "EXECUTE":
            response = self.execute.handle_request(intent, entities, context)
        elif intent.category == "MANAGE":
            response = self.manage.handle_request(intent, entities, context)
        elif intent.category == "REVIEW":
            response = self.review.handle_request(intent, entities, context)
        elif intent.category == "COACHING":
            response = self.coaching.handle_request(intent, entities, context)
        else:
            response = self.handle_general_query(user_input, context)
        
        # Update context with interaction
        self.context.update_from_interaction(user_input, response)
        
        return response
```

### **File Structure**
```
intent-trader/
├── main.py                     # Application entry point
├── config/
│   ├── settings.py            # Application configuration
│   └── prompts/               # AI prompt templates
├── src/
│   ├── core/
│   │   ├── intent_classifier.py
│   │   ├── context_manager.py
│   │   └── response_generator.py
│   ├── modules/
│   │   ├── plan/              # Planning module components
│   │   ├── focus/             # Focus module components
│   │   ├── execute/           # Execution module components
│   │   ├── manage/            # Management module components
│   │   ├── review/            # Review module components
│   │   └── coaching/          # Coaching module components
│   ├── data/
│   │   ├── models.py          # Data model definitions
│   │   ├── storage.py         # Data persistence
│   │   └── external.py        # External data sources
│   └── interfaces/
│       ├── cli.py             # Command line interface
│       └── web/               # Web interface (future)
├── data/
│   ├── plans/                 # Daily trading plans
│   ├── trades/                # Trade records
│   ├── positions/             # Position tracking
│   └── analysis/              # Performance data
├── tests/
│   ├── unit/                  # Unit tests
│   ├── integration/           # Integration tests
│   └── fixtures/              # Test data
└── docs/
    ├── user_guide.md          # User documentation
    ├── api_reference.md       # Technical documentation
    └── examples/              # Usage examples
```

### **Configuration and Deployment**
```python
# config/settings.py
@dataclass
class Settings:
    # AI Configuration
    ai_provider: str = "openai"  # or "anthropic"
    api_key: str = ""
    model_name: str = "gpt-4"
    
    # Trading Configuration
    default_account_size: float = 50000
    max_daily_risk: float = 0.02
    default_position_risk: float = 0.01
    
    # Data Sources
    market_data_provider: str = "alpaca"  # or "polygon", "yahoo"
    data_refresh_interval: int = 60  # seconds
    
    # Storage
    data_directory: str = "./data"
    backup_enabled: bool = True
    backup_interval: int = 3600  # seconds
    
    # Interface
    default_interface: str = "cli"
    log_level: str = "INFO"
```

---

## Future Enhancements

### **Phase 2 Features (3-6 months)**
- Web-based interface with real-time updates
- Integration with major brokerages for live position data
- Advanced pattern recognition using machine learning
- Mobile companion app for alerts and quick decisions
- Backtesting engine for strategy validation

### **Phase 3 Features (6-12 months)**
- Multi-timeframe analysis integration
- Social sentiment incorporation from trading communities
- Advanced risk management with portfolio-level optimization
- Automated paper trading for strategy testing
- Integration with popular trading platforms (TradingView, etc.)

### **Long-term Vision (1+ years)**
- Predictive analytics for market regime changes
- Personalized coaching using reinforcement learning
- Community features for sharing insights and strategies
- Advanced automation with human oversight
- Integration with tax and reporting systems

---

## Success Metrics

### **User Experience Metrics**
- Query response accuracy: >90%
- Average response time: <2 seconds
- User satisfaction rating: >8/10
- Daily active usage: >80% of trading days

### **Trading Performance Metrics**
- Plan adherence improvement: +20%
- Risk management compliance: >95%
- Decision quality scoring: >7/10 average
- Learning acceleration: Measurable improvement in identified areas

### **Technical Performance Metrics**
- System uptime: >99.5%
- Data accuracy: >99.9%
- Error rate: <1% of queries
- Memory usage: <500MB typical operation

---

This comprehensive specification defines Intent-Trader as a complete solo trading assistant implementing PFEMRC methodology through conversational AI, designed to enhance decision-making from morning preparation through continuous learning and improvement.