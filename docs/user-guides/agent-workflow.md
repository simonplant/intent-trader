# Agent Workflow Guide

Intent Trader's agent-based architecture follows the PFEMRC methodology (Plan, Focus, Execute, Manage, Review, Coach). This guide explains how to effectively use each agent and orchestrate complete trading workflows.

## PFEMRC Overview

The PFEMRC methodology provides a systematic approach to trading:

- **Plan**: Analyze market conditions and identify opportunities
- **Focus**: Prioritize setups and allocate resources
- **Execute**: Place trades with proper risk management
- **Manage**: Monitor and adjust positions
- **Review**: Analyze performance and outcomes
- **Coach**: Learn from results and improve

## Agent Architecture

Each agent is designed to be:
- **Autonomous**: Can operate independently
- **Composable**: Can be combined with other agents
- **Testable**: Isolated functionality for easy testing
- **Configurable**: Behavior can be customized via parameters

## Plan Agent

### Purpose
The Plan Agent analyzes market conditions, processes information, and creates trading plans.

### Usage

```python
from agents.plan_agent import PlanAgent

plan_agent = PlanAgent()

# Basic market analysis
result = plan_agent.execute(
    content="Market showing strong bullish momentum with high volume",
    symbols=["AAPL", "MSFT", "GOOGL"],
    timeframe="day_trading"
)

print(f"Plan status: {result['status']}")
print(f"Analysis: {result['data']['analysis']}")
```

### Advanced Usage

```python
# Analyze morning call transcript
morning_call_result = plan_agent.execute(
    content="""
    Good morning traders. Today we're seeing:
    - SPY gapping up 0.5% on strong overnight futures
    - AAPL showing strength above 150 resistance
    - Tech sector leading with NVDA up 2%
    - VIX down to 18, showing reduced fear
    - Key levels to watch: SPY 420 support, QQQ 350 resistance
    """,
    analysis_type="morning_call",
    symbols=["SPY", "QQQ", "AAPL", "NVDA"],
    market_conditions={
        "sentiment": "bullish",
        "volatility": "low",
        "volume": "high"
    }
)

# Process earnings report
earnings_result = plan_agent.execute(
    content="AAPL reported Q4 earnings: EPS $1.29 vs $1.27 expected, Revenue $89.5B vs $89.3B expected",
    analysis_type="earnings",
    symbols=["AAPL"],
    event_type="earnings_beat"
)
```

### Output Structure

```python
{
    "status": "success",
    "message": "Analysis completed",
    "data": {
        "analysis": {
            "market_bias": "bullish",
            "key_levels": [150.0, 155.0, 160.0],
            "catalysts": ["earnings_beat", "sector_rotation"],
            "risk_factors": ["fed_meeting", "geopolitical"]
        },
        "opportunities": [
            {
                "symbol": "AAPL",
                "setup": "breakout",
                "conviction": 0.8,
                "timeframe": "day"
            }
        ],
        "market_conditions": {
            "sentiment": "bullish",
            "volatility": "moderate",
            "trend": "uptrend"
        }
    }
}
```

## Focus Agent

### Purpose
The Focus Agent prioritizes trading opportunities and allocates attention/resources.

### Usage

```python
from agents.focus_agent import FocusAgent

focus_agent = FocusAgent()

# Basic focus identification
result = focus_agent.execute(
    market_conditions="bullish",
    timeframe="day_trading",
    risk_tolerance=0.7
)

print(f"Focus areas: {result['data']['focus_areas']}")
```

### Advanced Usage

```python
# Focus with specific criteria
focus_result = focus_agent.execute(
    market_conditions={
        "trend": "uptrend",
        "volatility": "moderate",
        "volume": "above_average"
    },
    timeframe="day_trading",
    capital_allocation=100000,
    max_positions=3,
    sector_preferences=["technology", "healthcare"],
    setup_types=["breakout", "pullback", "momentum"]
)

# Focus on specific symbols
symbol_focus = focus_agent.execute(
    symbols=["AAPL", "MSFT", "GOOGL", "NVDA", "TSLA"],
    screening_criteria={
        "min_volume": 1000000,
        "price_range": [50, 500],
        "rsi_range": [30, 70]
    }
)
```

### Output Structure

```python
{
    "status": "success",
    "message": "Focus areas identified",
    "data": {
        "focus_areas": [
            {
                "symbol": "AAPL",
                "priority": 1,
                "setup": "breakout_above_resistance",
                "conviction": 0.85,
                "allocation": 0.4
            },
            {
                "symbol": "MSFT",
                "priority": 2,
                "setup": "pullback_to_support",
                "conviction": 0.7,
                "allocation": 0.3
            }
        ],
        "market_focus": {
            "primary_sector": "technology",
            "key_themes": ["ai_momentum", "cloud_growth"],
            "risk_level": "moderate"
        }
    }
}
```

## Execute Agent

### Purpose
The Execute Agent handles order placement with comprehensive safety checks and risk management.

### Usage

```python
from agents.execute_agent import ExecuteAgent

execute_agent = ExecuteAgent()

# Basic order execution
result = execute_agent.execute(
    symbol="AAPL",
    side="buy",
    quantity=100,
    order_type="market"
)

print(f"Execution status: {result['status']}")
```

### Advanced Usage

```python
# Complex order with risk management
execution_result = execute_agent.execute(
    symbol="AAPL",
    side="buy",
    quantity=100,
    order_type="limit",
    price=150.0,
    stop_loss=145.0,
    take_profit=160.0,
    time_in_force="day",
    notes="Breakout trade above resistance"
)

# Bracket order
bracket_result = execute_agent.execute(
    symbol="MSFT",
    side="buy",
    quantity=50,
    order_type="market",
    stop_loss=280.0,
    take_profit=300.0,
    trailing_stop=True,
    trail_amount=2.0
)

# Scale-in execution
scale_result = execute_agent.execute(
    symbol="GOOGL",
    side="buy",
    total_quantity=200,
    execution_style="scale_in",
    scale_intervals=4,
    price_range=[2800, 2820]
)
```

### Safety Checks

The Execute Agent performs comprehensive safety checks:

```python
# Safety checks include:
# - Position size limits
# - Daily trade limits
# - Account balance verification
# - Risk-reward ratio validation
# - Correlation checks
# - Market hours verification
```

### Output Structure

```python
{
    "status": "success",
    "message": "Order executed successfully",
    "data": {
        "order": {
            "order_id": "order_123",
            "symbol": "AAPL",
            "side": "buy",
            "quantity": 100,
            "price": 150.0,
            "status": "filled"
        },
        "execution_quality": {
            "slippage": 0.02,
            "fill_time": 0.15,
            "market_impact": 0.01
        },
        "risk_metrics": {
            "position_size": 15000,
            "risk_amount": 500,
            "risk_percent": 0.5
        }
    }
}
```

## Manage Agent

### Purpose
The Manage Agent monitors positions and makes adjustments to optimize performance and manage risk.

### Usage

```python
from agents.manage_agent import ManageAgent

manage_agent = ManageAgent()

# Basic position adjustment
result = manage_agent.execute(
    action="adjust",
    position_id="pos_123",
    stop_loss=148.0,
    take_profit=165.0
)

print(f"Management status: {result['status']}")
```

### Advanced Usage

```python
# Scale position
scale_result = manage_agent.execute(
    action="scale",
    position_id="pos_123",
    quantity=1.5,  # Scale factor
    reason="strong_momentum"
)

# Trailing stop adjustment
trail_result = manage_agent.execute(
    action="adjust",
    position_id="pos_123",
    trailing_stop=True,
    trail_amount=3.0,
    trail_percent=2.0
)

# Close position
close_result = manage_agent.execute(
    action="close",
    position_id="pos_123",
    reason="target_reached",
    close_type="full"
)

# Partial close
partial_close = manage_agent.execute(
    action="close",
    position_id="pos_123",
    close_type="partial",
    close_percentage=0.5,
    reason="take_profits"
)
```

### Risk Monitoring

```python
# Continuous risk monitoring
risk_check = manage_agent.execute(
    action="monitor",
    check_type="risk_limits",
    positions=["pos_123", "pos_124", "pos_125"]
)

# Portfolio rebalancing
rebalance_result = manage_agent.execute(
    action="rebalance",
    target_allocation={
        "AAPL": 0.4,
        "MSFT": 0.3,
        "GOOGL": 0.3
    }
)
```

### Output Structure

```python
{
    "status": "success",
    "message": "Position adjusted successfully",
    "data": {
        "position": {
            "position_id": "pos_123",
            "symbol": "AAPL",
            "quantity": 100,
            "current_price": 152.0,
            "pnl": 200.0,
            "pnl_percent": 1.33
        },
        "adjustments": {
            "stop_loss": {"old": 145.0, "new": 148.0},
            "take_profit": {"old": 160.0, "new": 165.0}
        },
        "risk_metrics": {
            "current_risk": 400.0,
            "max_profit": 300.0,
            "unrealized_pnl": 200.0
        }
    }
}
```

## Review Agent

### Purpose
The Review Agent analyzes trading performance and generates insights for improvement.

### Usage

```python
from agents.review_agent import ReviewAgent
from datetime import datetime, timedelta

review_agent = ReviewAgent()

# Daily review
result = review_agent.execute(
    start_time=datetime.now() - timedelta(days=1),
    end_time=datetime.now()
)

print(f"Review metrics: {result['data']['metrics']}")
```

### Advanced Usage

```python
# Weekly performance review
weekly_review = review_agent.execute(
    start_time=datetime.now() - timedelta(days=7),
    end_time=datetime.now(),
    analysis_type="comprehensive",
    include_execution_analysis=True,
    include_risk_analysis=True
)

# Symbol-specific review
symbol_review = review_agent.execute(
    symbol="AAPL",
    start_time=datetime.now() - timedelta(days=30),
    end_time=datetime.now(),
    analysis_depth="detailed"
)

# Strategy performance review
strategy_review = review_agent.execute(
    strategy="momentum_breakout",
    start_time=datetime.now() - timedelta(days=90),
    end_time=datetime.now(),
    benchmark="SPY"
)
```

### Output Structure

```python
{
    "status": "success",
    "message": "Trade review completed",
    "data": {
        "metrics": {
            "total_trades": 25,
            "win_rate": 0.68,
            "profit_factor": 1.85,
            "sharpe_ratio": 1.42,
            "max_drawdown": 0.08,
            "total_pnl": 2500.0
        },
        "execution_analysis": {
            "average_slippage": 0.03,
            "fill_rate": 0.95,
            "execution_quality": 0.87
        },
        "management_analysis": {
            "average_hold_time": "2.5 hours",
            "stop_loss_hit_rate": 0.15,
            "take_profit_hit_rate": 0.45
        },
        "insights": [
            "Strong performance in momentum trades",
            "Consider tighter stops in volatile conditions",
            "Execution quality excellent during market hours"
        ]
    }
}
```

## Coach Agent

### Purpose
The Coach Agent provides learning insights and improvement recommendations based on performance analysis.

### Usage

```python
from agents.coach_agent import CoachAgent

coach_agent = CoachAgent()

# Basic coaching session
result = coach_agent.execute(
    start_time=datetime.now() - timedelta(days=30),
    end_time=datetime.now()
)

print(f"Improvement areas: {result['data']['improvement_areas']}")
```

### Advanced Usage

```python
# Comprehensive coaching analysis
coaching_result = coach_agent.execute(
    start_time=datetime.now() - timedelta(days=90),
    end_time=datetime.now(),
    focus_areas=["execution", "risk_management", "strategy_selection"],
    performance_benchmark="market_neutral",
    learning_style="detailed"
)

# Specific skill development
skill_coaching = coach_agent.execute(
    skill_focus="position_sizing",
    analysis_period=timedelta(days=60),
    improvement_target="risk_adjusted_returns"
)

# Trading psychology coaching
psychology_coaching = coach_agent.execute(
    focus_type="psychology",
    analysis_areas=["discipline", "patience", "risk_tolerance"],
    behavioral_patterns=True
)
```

### Output Structure

```python
{
    "status": "success",
    "message": "Coaching insights generated",
    "data": {
        "improvement_areas": [
            {
                "category": "Risk Management",
                "description": "Position sizing could be optimized",
                "impact": 0.8,
                "suggestions": [
                    "Consider Kelly criterion for position sizing",
                    "Implement dynamic stop losses",
                    "Review correlation limits"
                ]
            }
        ],
        "best_practices": [
            {
                "category": "Execution",
                "description": "Excellent timing on entries",
                "evidence": ["95% fill rate", "Low slippage"],
                "implementation": "Continue current approach"
            }
        ],
        "development_plan": {
            "short_term": ["Optimize position sizing algorithm"],
            "medium_term": ["Develop sector rotation strategy"],
            "long_term": ["Implement machine learning signals"]
        }
    }
}
```

## Complete Workflow Examples

### Daily Trading Workflow

```python
def complete_daily_workflow():
    """Complete PFEMRC workflow for daily trading"""
    
    # 1. PLAN - Morning market analysis
    plan_agent = PlanAgent()
    plan_result = plan_agent.execute(
        content="Daily market analysis and news review",
        symbols=["SPY", "QQQ", "IWM"],
        analysis_type="daily_prep"
    )
    
    if plan_result["status"] != "success":
        return {"error": "Planning phase failed"}
    
    # 2. FOCUS - Identify trading opportunities
    focus_agent = FocusAgent()
    focus_result = focus_agent.execute(
        market_conditions=plan_result["data"]["market_conditions"],
        timeframe="day_trading",
        max_positions=3
    )
    
    # 3. EXECUTE - Place trades
    execute_agent = ExecuteAgent()
    executions = []
    
    for opportunity in focus_result["data"]["focus_areas"]:
        execution_result = execute_agent.execute(
            symbol=opportunity["symbol"],
            side="buy",
            quantity=opportunity["suggested_quantity"],
            order_type="limit",
            price=opportunity["entry_price"],
            stop_loss=opportunity["stop_loss"],
            take_profit=opportunity["take_profit"]
        )
        executions.append(execution_result)
    
    # 4. MANAGE - Monitor throughout the day
    manage_agent = ManageAgent()
    
    # Set up monitoring loop (simplified)
    for execution in executions:
        if execution["status"] == "success":
            position_id = execution["data"]["order"]["order_id"]
            
            # Monitor and adjust as needed
            management_result = manage_agent.execute(
                action="monitor",
                position_id=position_id
            )
    
    # 5. REVIEW - End of day analysis
    review_agent = ReviewAgent()
    review_result = review_agent.execute(
        start_time=datetime.now().replace(hour=9, minute=30),
        end_time=datetime.now()
    )
    
    # 6. COACH - Learning insights
    coach_agent = CoachAgent()
    coaching_result = coach_agent.execute(
        start_time=datetime.now() - timedelta(days=7),
        end_time=datetime.now()
    )
    
    return {
        "plan": plan_result,
        "focus": focus_result,
        "executions": executions,
        "review": review_result,
        "coaching": coaching_result
    }

# Run the complete workflow
daily_results = complete_daily_workflow()
```

### Swing Trading Workflow

```python
def swing_trading_workflow():
    """PFEMRC workflow optimized for swing trading"""
    
    # Plan with longer timeframe
    plan_agent = PlanAgent()
    plan_result = plan_agent.execute(
        content="Weekly market outlook and sector analysis",
        timeframe="swing_trading",
        analysis_depth="comprehensive"
    )
    
    # Focus on swing setups
    focus_agent = FocusAgent()
    focus_result = focus_agent.execute(
        timeframe="swing_trading",
        setup_types=["pullback", "breakout", "reversal"],
        holding_period="3-10_days"
    )
    
    # Execute with wider stops
    execute_agent = ExecuteAgent()
    for opportunity in focus_result["data"]["focus_areas"]:
        execution_result = execute_agent.execute(
            symbol=opportunity["symbol"],
            side=opportunity["side"],
            quantity=opportunity["quantity"],
            order_type="limit",
            price=opportunity["entry_price"],
            stop_loss=opportunity["stop_loss"],
            take_profit=opportunity["take_profit"],
            time_in_force="gtc"  # Good till cancelled
        )
    
    # Manage with less frequent adjustments
    manage_agent = ManageAgent()
    # ... (similar pattern with swing-specific parameters)
    
    return results

# Run swing workflow
swing_results = swing_trading_workflow()
```

## Best Practices

### 1. Error Handling

```python
def robust_agent_execution():
    """Example of robust agent execution with error handling"""
    
    try:
        plan_agent = PlanAgent()
        result = plan_agent.execute(content="Market analysis")
        
        if result["status"] != "success":
            # Handle planning failure
            fallback_plan = plan_agent.execute(
                content="Simplified market analysis",
                analysis_type="basic"
            )
            result = fallback_plan
            
    except Exception as e:
        # Log error and use default plan
        logger.error(f"Planning failed: {e}")
        result = {"status": "error", "message": str(e)}
    
    return result
```

### 2. Configuration Management

```python
# Use configuration for agent parameters
from src.core.config import get_config_manager

config = get_config_manager()

# Configure agents based on trading style
trading_style = config.get("trading.style", "day_trading")

if trading_style == "day_trading":
    execute_params = {
        "time_in_force": "day",
        "max_position_size": 0.1,
        "stop_loss_percent": 0.02
    }
elif trading_style == "swing_trading":
    execute_params = {
        "time_in_force": "gtc",
        "max_position_size": 0.2,
        "stop_loss_percent": 0.05
    }
```

### 3. Logging and Monitoring

```python
import logging

logger = logging.getLogger(__name__)

def monitored_agent_execution():
    """Example with comprehensive logging"""
    
    logger.info("Starting PFEMRC workflow")
    
    # Plan phase
    logger.info("Executing Plan phase")
    plan_start = time.time()
    plan_result = plan_agent.execute(content="Market analysis")
    plan_duration = time.time() - plan_start
    
    logger.info(f"Plan phase completed in {plan_duration:.2f}s")
    logger.info(f"Plan result: {plan_result['status']}")
    
    # Continue with other phases...
    
    return results
```

## Troubleshooting

### Common Issues

1. **Agent Execution Failures**
   - Check input parameters
   - Verify agent initialization
   - Review error messages in logs

2. **Performance Issues**
   - Monitor agent execution times
   - Check resource usage
   - Optimize parameter sets

3. **Integration Problems**
   - Verify agent communication
   - Check data flow between agents
   - Validate output formats

### Debugging Tips

```python
# Enable debug logging
import logging
logging.basicConfig(level=logging.DEBUG)

# Test individual agents
def test_agent(agent_class, **kwargs):
    """Test individual agent execution"""
    agent = agent_class()
    result = agent.execute(**kwargs)
    print(f"Agent: {agent_class.__name__}")
    print(f"Status: {result['status']}")
    print(f"Message: {result['message']}")
    return result

# Test each agent individually
test_agent(PlanAgent, content="Test analysis")
test_agent(FocusAgent, market_conditions="bullish")
test_agent(ExecuteAgent, symbol="AAPL", side="buy", quantity=100)
```

This comprehensive agent workflow guide provides the foundation for building sophisticated trading systems using Intent Trader's agent-based architecture. Each agent can be used independently or combined to create powerful automated trading workflows. 