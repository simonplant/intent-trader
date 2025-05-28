# Getting Started with Intent Trader

Welcome to Intent Trader! This guide will help you get up and running with the system quickly and efficiently.

## Prerequisites

Before you begin, ensure you have the following installed:

- **Python 3.12+** (managed via pyenv)
- **Git** for version control
- **4GB RAM minimum** (8GB recommended)
- **1GB free disk space** (plus additional space for market data)

## Installation

### 1. Clone the Repository

```bash
git clone https://github.com/yourusername/intent-trader.git
cd intent-trader
```

### 2. Set Up Python Environment

```bash
# Install Python 3.12.10 via pyenv
pyenv install 3.12.10
pyenv local 3.12.10

# Verify Python version
python --version  # Should show Python 3.12.10
```

### 3. Create Virtual Environment

```bash
# Create virtual environment
python -m venv venv

# Activate virtual environment
source venv/bin/activate  # On macOS/Linux
# OR
venv\Scripts\activate     # On Windows
```

### 4. Install Dependencies

```bash
# Install core dependencies
pip install -r requirements.txt

# For development (optional)
pip install -r dev-requirements.txt
```

### 5. Initialize the System

```bash
# Initialize database
python -c "from src.core.database import DatabaseManager; DatabaseManager().init_database()"

# Verify installation
python -m pytest -v
```

## Configuration

### Basic Configuration

The system uses YAML configuration files located in the `config/` directory:

```yaml
# config/config.yaml
trading:
  default_symbol: AAPL
  position_size: 100
  max_positions: 5
  risk_per_trade: 0.02

strategy:
  sma_crossover:
    short_window: 20
    long_window: 50

logging:
  level: INFO
  file: logs/trading.log
```

### Environment Variables

You can override configuration using environment variables:

```bash
# Set environment variables
export TRADING_DEFAULT_SYMBOL=MSFT
export STRATEGY_SMA_CROSSOVER_SHORT_WINDOW=10
export LOGGING_LEVEL=DEBUG
```

## Your First Trade

Let's walk through executing your first trade using Intent Trader:

### 1. Basic Market Data

```python
from src.market_data.feed import MarketDataFeed

# Initialize market data feed
feed = MarketDataFeed()

# Get historical data
data = feed.get_historical_data(
    symbol="AAPL",
    period="30d",
    interval="1d"
)

print(f"Retrieved {len(data)} data points for AAPL")
```

### 2. Generate Trading Signals

```python
from src.strategy.engine import StrategyEngine
import pandas as pd

# Initialize strategy engine
engine = StrategyEngine()

# Convert data to DataFrame
df = pd.DataFrame(data)

# Generate signals using SMA crossover strategy
signals = engine.generate_signals(
    data=df,
    strategy="sma_crossover",
    params={"short_window": 20, "long_window": 50}
)

print(f"Latest signal: {signals['signal'].iloc[-1]}")
```

### 3. Execute Orders

```python
from src.order.manager import OrderManager, OrderSide, OrderType

# Initialize order manager
order_manager = OrderManager()

# Create a market order
if signals['signal'].iloc[-1] == 1:  # Buy signal
    order = order_manager.create_order(
        symbol="AAPL",
        side=OrderSide.BUY,
        order_type=OrderType.MARKET,
        quantity=100
    )
    print(f"Order created: {order.to_dict()}")
```

## Agent-Based Workflow

Intent Trader's power comes from its agent-based architecture. Here's how to use the PFEMRC workflow:

### 1. Plan Phase

```python
from agents.plan_agent import PlanAgent

plan_agent = PlanAgent()
plan_result = plan_agent.execute(
    content="Market showing bullish momentum with strong volume",
    symbols=["AAPL", "MSFT", "GOOGL"]
)

print(f"Plan status: {plan_result['status']}")
```

### 2. Focus Phase

```python
from agents.focus_agent import FocusAgent

focus_agent = FocusAgent()
focus_result = focus_agent.execute(
    market_conditions="bullish",
    timeframe="day_trading"
)

print(f"Focus areas: {focus_result['data']['focus_areas']}")
```

### 3. Execute Phase

```python
from agents.execute_agent import ExecuteAgent

execute_agent = ExecuteAgent()
execution_result = execute_agent.execute(
    symbol="AAPL",
    side="buy",
    quantity=100,
    order_type="market"
)

print(f"Execution status: {execution_result['status']}")
```

### 4. Manage Phase

```python
from agents.manage_agent import ManageAgent

manage_agent = ManageAgent()
management_result = manage_agent.execute(
    action="adjust",
    position_id="pos_123",
    stop_loss=150.0,
    take_profit=160.0
)

print(f"Management status: {management_result['status']}")
```

### 5. Review Phase

```python
from agents.review_agent import ReviewAgent
from datetime import datetime, timedelta

review_agent = ReviewAgent()
review_result = review_agent.execute(
    start_time=datetime.now() - timedelta(days=7),
    end_time=datetime.now()
)

print(f"Review metrics: {review_result['data']['metrics']}")
```

### 6. Coach Phase

```python
from agents.coach_agent import CoachAgent

coach_agent = CoachAgent()
coaching_result = coach_agent.execute(
    start_time=datetime.now() - timedelta(days=30),
    end_time=datetime.now()
)

print(f"Improvement areas: {coaching_result['data']['improvement_areas']}")
```

## Performance Analysis

### Basic Performance Metrics

```python
from analysis.performance_analyzer import PerformanceAnalyzer

analyzer = PerformanceAnalyzer()
metrics = analyzer.get_overall_metrics()

if metrics:
    print(f"Win Rate: {metrics.win_rate:.2%}")
    print(f"Profit Factor: {metrics.profit_factor:.2f}")
    print(f"Total P&L: ${metrics.total_pnl:.2f}")
    print(f"Sharpe Ratio: {metrics.sharpe_ratio:.2f}")
```

### Performance Visualization

```python
from analysis.performance_visualizer import PerformanceVisualizer

if metrics:
    visualizer = PerformanceVisualizer(metrics)
    
    # Create equity curve
    equity_fig = visualizer.plot_equity_curve()
    equity_fig.show()
    
    # Create performance dashboard
    dashboard = visualizer.create_performance_dashboard()
    dashboard.show()
```

## Risk Management

### Position Sizing

```python
from risk.risk_manager import RiskManager, RiskParameters

# Set up risk parameters
risk_params = RiskParameters(
    account_value=100000,
    max_daily_risk=0.02,
    max_position_risk=0.01,
    max_drawdown=0.05,
    min_risk_reward=2.0,
    max_positions=5,
    position_sizing_method="kelly"
)

# Initialize risk manager
risk_manager = RiskManager(risk_params)

# Calculate position size
position_size = risk_manager.calculate_position_size(
    symbol="AAPL",
    entry_price=150.0,
    stop_loss=145.0,
    take_profit=160.0
)

print(f"Recommended position size: {position_size:.0f} shares")
```

### Trade Validation

```python
# Validate a trade before execution
validation = risk_manager.validate_trade(
    symbol="AAPL",
    quantity=100,
    entry_price=150.0,
    stop_loss=145.0,
    take_profit=160.0
)

if validation["valid"]:
    print("Trade is valid and within risk limits")
else:
    print(f"Trade rejected: {validation['reason']}")
```

## Common Workflows

### Daily Trading Routine

```python
def daily_trading_routine():
    """Complete daily trading workflow"""
    
    # 1. Plan - Analyze market conditions
    plan_agent = PlanAgent()
    plan_result = plan_agent.execute(
        content="Daily market analysis",
        symbols=["AAPL", "MSFT", "GOOGL"]
    )
    
    # 2. Focus - Identify opportunities
    focus_agent = FocusAgent()
    focus_result = focus_agent.execute()
    
    # 3. Execute - Place trades
    if focus_result["status"] == "success":
        execute_agent = ExecuteAgent()
        for opportunity in focus_result["data"]["opportunities"]:
            execution_result = execute_agent.execute(**opportunity)
            print(f"Executed: {execution_result}")
    
    # 4. Manage - Monitor positions
    manage_agent = ManageAgent()
    management_result = manage_agent.execute(action="monitor")
    
    # 5. Review - End of day analysis
    review_agent = ReviewAgent()
    review_result = review_agent.execute()
    
    # 6. Coach - Learning insights
    coach_agent = CoachAgent()
    coaching_result = coach_agent.execute()
    
    return {
        "plan": plan_result,
        "focus": focus_result,
        "review": review_result,
        "coaching": coaching_result
    }

# Run daily routine
results = daily_trading_routine()
```

### Backtesting Strategy

```python
def backtest_strategy(symbol, start_date, end_date):
    """Backtest a trading strategy"""
    
    # Get historical data
    feed = MarketDataFeed()
    data = feed.get_historical_data(
        symbol=symbol,
        period="1y",
        interval="1d"
    )
    
    # Initialize components
    engine = StrategyEngine()
    analyzer = PerformanceAnalyzer()
    
    # Generate signals
    df = pd.DataFrame(data)
    signals = engine.generate_signals(
        data=df,
        strategy="sma_crossover",
        params={"short_window": 20, "long_window": 50}
    )
    
    # Simulate trades and analyze performance
    # (Implementation depends on your specific backtesting needs)
    
    return analyzer.get_overall_metrics()

# Run backtest
backtest_results = backtest_strategy("AAPL", "2023-01-01", "2023-12-31")
```

## Troubleshooting

### Common Issues

1. **Import Errors**
   ```bash
   # Ensure virtual environment is activated
   source venv/bin/activate
   
   # Reinstall dependencies
   pip install -r requirements.txt
   ```

2. **Database Issues**
   ```python
   # Reinitialize database
   from src.core.database import DatabaseManager
   db = DatabaseManager()
   db.init_database()
   ```

3. **Configuration Issues**
   ```bash
   # Check configuration
   python -c "from src.core.config import get_config_manager; print(get_config_manager().config)"
   ```

4. **Test Failures**
   ```bash
   # Run specific test
   python -m pytest tests/test_basic_flow.py -v
   
   # Run all tests
   python -m pytest -v
   ```

### Getting Help

- **Documentation**: Check the [docs/](../README.md) directory
- **Issues**: Report bugs on [GitHub Issues](https://github.com/yourusername/intent-trader/issues)
- **Discussions**: Join [GitHub Discussions](https://github.com/yourusername/intent-trader/discussions)

## Next Steps

Now that you have Intent Trader running:

1. **Explore the [Agent Workflow Guide](agent-workflow.md)** for advanced agent usage
2. **Read the [Configuration Guide](configuration.md)** for detailed configuration options
3. **Check out the [Strategy Development Guide](../developer-guides/strategy-development.md)** to create custom strategies
4. **Review the [API Reference](../api-reference/README.md)** for complete API documentation

Happy trading! 🚀 