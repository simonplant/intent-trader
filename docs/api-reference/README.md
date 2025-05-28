# Intent Trader API Reference

This document provides comprehensive API reference for all Intent Trader components.

## Table of Contents

- [Core Components](#core-components)
  - [Configuration Manager](#configuration-manager)
  - [Database Manager](#database-manager)
  - [Logging Manager](#logging-manager)
  - [Intent Parser](#intent-parser)
- [Trading Agents](#trading-agents)
  - [Plan Agent](#plan-agent)
  - [Focus Agent](#focus-agent)
  - [Execute Agent](#execute-agent)
  - [Manage Agent](#manage-agent)
  - [Review Agent](#review-agent)
  - [Coach Agent](#coach-agent)
- [Market Data](#market-data)
  - [Market Data Feed](#market-data-feed)
- [Order Management](#order-management)
  - [Order Manager](#order-manager)
- [Strategy Engine](#strategy-engine)
- [Risk Management](#risk-management)
  - [Risk Manager](#risk-manager)
- [Performance Analytics](#performance-analytics)
  - [Performance Analyzer](#performance-analyzer)
  - [Performance Visualizer](#performance-visualizer)
- [Data Schemas](#data-schemas)

## Core Components

### Configuration Manager

**Module**: `src.core.config`

#### ConfigManager

```python
class ConfigManager:
    """Centralized configuration management with environment variable support."""
    
    def __init__(self, config_path: str = "config/config.yaml") -> None:
        """Initialize configuration manager.
        
        Args:
            config_path: Path to YAML configuration file
            
        Raises:
            FileNotFoundError: If configuration file not found
            RuntimeError: If configuration file is invalid
        """
```

##### Methods

```python
def get(self, key: str, default: Any = None) -> Any:
    """Get configuration value using dot notation.
    
    Args:
        key: Configuration key (e.g., 'trading.default_symbol')
        default: Default value if key not found
        
    Returns:
        Configuration value or default
        
    Example:
        >>> config = ConfigManager()
        >>> symbol = config.get('trading.default_symbol', 'AAPL')
    """

def get_trading_config(self) -> Dict[str, Any]:
    """Get trading configuration section.
    
    Returns:
        Dictionary containing trading configuration
    """

def get_strategy_config(self, strategy_name: str) -> Dict[str, Any]:
    """Get configuration for specific strategy.
    
    Args:
        strategy_name: Name of the strategy
        
    Returns:
        Dictionary containing strategy configuration
    """

def set(self, key: str, value: Any) -> None:
    """Set configuration value.
    
    Args:
        key: Configuration key (dot notation supported)
        value: Value to set
    """

def save(self) -> None:
    """Save configuration to file."""
```

##### Usage Example

```python
from src.core.config import ConfigManager

config = ConfigManager()

# Get configuration values
symbol = config.get('trading.default_symbol')
position_size = config.get('trading.position_size', 100)

# Get section configuration
trading_config = config.get_trading_config()
strategy_config = config.get_strategy_config('sma_crossover')

# Set configuration
config.set('trading.max_positions', 10)
config.save()
```

### Database Manager

**Module**: `src.core.database`

#### DatabaseManager

```python
class DatabaseManager:
    """SQLite-based data persistence with schema management."""
    
    def __init__(self, db_path: str = "data/db/trading.db") -> None:
        """Initialize database manager.
        
        Args:
            db_path: Path to SQLite database file
        """
```

##### Methods

```python
def init_database(self) -> None:
    """Initialize database with required tables."""

def insert_order(self, order: OrderSchema) -> None:
    """Insert order into database.
    
    Args:
        order: Order schema object
    """

def get_orders(self, status: Optional[str] = None) -> List[OrderSchema]:
    """Get orders from database.
    
    Args:
        status: Optional status filter
        
    Returns:
        List of order schema objects
    """

def update_order_status(self, order_id: str, status: str) -> None:
    """Update order status.
    
    Args:
        order_id: Order identifier
        status: New status
    """

def insert_position(self, position: PositionSchema) -> None:
    """Insert position into database.
    
    Args:
        position: Position schema object
    """

def get_positions(self, symbol: Optional[str] = None) -> List[PositionSchema]:
    """Get positions from database.
    
    Args:
        symbol: Optional symbol filter
        
    Returns:
        List of position schema objects
    """
```

##### Usage Example

```python
from src.core.database import DatabaseManager
from data.schemas import OrderSchema

db = DatabaseManager()
db.init_database()

# Insert order
order = OrderSchema(
    order_id="order_123",
    symbol="AAPL",
    side="buy",
    order_type="market",
    quantity=100,
    status="pending",
    timestamp=datetime.now()
)
db.insert_order(order)

# Get orders
orders = db.get_orders(status="pending")
```

### Logging Manager

**Module**: `src.core.logging`

#### LogManager

```python
class LogManager:
    """Structured logging with rotation and multiple outputs."""
    
    def __init__(self, config: Optional[ConfigManager] = None) -> None:
        """Initialize logging manager.
        
        Args:
            config: Configuration manager instance
        """
```

##### Methods

```python
def get_logger(self, name: str) -> logging.Logger:
    """Get logger instance.
    
    Args:
        name: Logger name (typically __name__)
        
    Returns:
        Configured logger instance
    """

def setup_file_handler(self, log_file: str) -> None:
    """Set up file logging handler.
    
    Args:
        log_file: Path to log file
    """
```

##### Usage Example

```python
from src.core.logging import LogManager

log_manager = LogManager()
logger = log_manager.get_logger(__name__)

logger.info("Application started")
logger.error("An error occurred", extra={"order_id": "123"})
```

### Intent Parser

**Module**: `src.core.intent_parser`

#### IntentParser

```python
class IntentParser:
    """Natural language intent recognition and parameter extraction."""
    
    def __init__(self) -> None:
        """Initialize intent parser."""
```

##### Methods

```python
def parse(self, text: str) -> Intent:
    """Parse text and extract intent.
    
    Args:
        text: Input text to parse
        
    Returns:
        Intent object with action and parameters
        
    Raises:
        ValueError: If intent cannot be parsed
    """
```

##### Usage Example

```python
from src.core.intent_parser import IntentParser

parser = IntentParser()
intent = parser.parse("Buy 100 shares of AAPL at market price")

print(f"Action: {intent.action}")
print(f"Parameters: {intent.parameters}")
```

## Trading Agents

### Plan Agent

**Module**: `agents.plan_agent`

#### PlanAgent

```python
class PlanAgent:
    """Agent for market analysis and trading plan creation."""
    
    def __init__(self) -> None:
        """Initialize plan agent."""
```

##### Methods

```python
def execute(self, **kwargs) -> Dict[str, Any]:
    """Execute planning analysis.
    
    Args:
        content: Market analysis content
        symbols: List of symbols to analyze
        timeframe: Trading timeframe
        analysis_type: Type of analysis
        
    Returns:
        Dictionary with analysis results
        
    Example:
        >>> agent = PlanAgent()
        >>> result = agent.execute(
        ...     content="Market showing bullish momentum",
        ...     symbols=["AAPL", "MSFT"],
        ...     timeframe="day_trading"
        ... )
    """
```

### Focus Agent

**Module**: `agents.focus_agent`

#### FocusAgent

```python
class FocusAgent:
    """Agent for setup prioritization and focus area identification."""
    
    def __init__(self) -> None:
        """Initialize focus agent."""
```

##### Methods

```python
def execute(self, **kwargs) -> Dict[str, Any]:
    """Execute focus identification.
    
    Args:
        market_conditions: Current market conditions
        timeframe: Trading timeframe
        risk_tolerance: Risk tolerance level
        max_positions: Maximum number of positions
        
    Returns:
        Dictionary with focus areas and priorities
    """
```

### Execute Agent

**Module**: `agents.execute_agent`

#### ExecuteAgent

```python
class ExecuteAgent:
    """Agent for order execution with safety checks."""
    
    def __init__(self) -> None:
        """Initialize execute agent."""
```

##### Methods

```python
def execute(self, **kwargs) -> Dict[str, Any]:
    """Execute trade order.
    
    Args:
        symbol: Trading symbol
        side: Order side (buy/sell)
        quantity: Order quantity
        order_type: Order type (market/limit/stop)
        price: Order price (for limit orders)
        stop_loss: Stop loss price
        take_profit: Take profit price
        
    Returns:
        Dictionary with execution results
        
    Raises:
        ValueError: If order parameters are invalid
    """
```

### Manage Agent

**Module**: `agents.manage_agent`

#### ManageAgent

```python
class ManageAgent:
    """Agent for position management and adjustments."""
    
    def __init__(self) -> None:
        """Initialize manage agent."""
```

##### Methods

```python
def execute(self, **kwargs) -> Dict[str, Any]:
    """Execute position management action.
    
    Args:
        action: Management action (adjust/scale/close/monitor)
        position_id: Position identifier
        stop_loss: New stop loss price
        take_profit: New take profit price
        quantity: Scale quantity
        
    Returns:
        Dictionary with management results
    """
```

### Review Agent

**Module**: `agents.review_agent`

#### ReviewAgent

```python
class ReviewAgent:
    """Agent for performance analysis and trade review."""
    
    def __init__(self) -> None:
        """Initialize review agent."""
```

##### Methods

```python
def execute(self, **kwargs) -> Dict[str, Any]:
    """Execute performance review.
    
    Args:
        start_time: Review start time
        end_time: Review end time
        symbol: Optional symbol filter
        strategy: Optional strategy filter
        
    Returns:
        Dictionary with performance metrics and analysis
    """
```

### Coach Agent

**Module**: `agents.coach_agent`

#### CoachAgent

```python
class CoachAgent:
    """Agent for learning insights and improvement recommendations."""
    
    def __init__(self) -> None:
        """Initialize coach agent."""
```

##### Methods

```python
def execute(self, **kwargs) -> Dict[str, Any]:
    """Execute coaching analysis.
    
    Args:
        start_time: Analysis start time
        end_time: Analysis end time
        focus_areas: Areas to focus coaching on
        
    Returns:
        Dictionary with coaching insights and recommendations
    """
```

## Market Data

### Market Data Feed

**Module**: `src.market_data.feed`

#### MarketDataFeed

```python
class MarketDataFeed:
    """Real-time and historical market data provider."""
    
    def __init__(self) -> None:
        """Initialize market data feed."""
```

##### Methods

```python
def get_historical_data(
    self,
    symbol: str,
    period: str = "1y",
    interval: str = "1d"
) -> List[Dict[str, Any]]:
    """Get historical market data.
    
    Args:
        symbol: Trading symbol
        period: Data period (1d, 5d, 1mo, 3mo, 6mo, 1y, 2y, 5y, 10y, ytd, max)
        interval: Data interval (1m, 2m, 5m, 15m, 30m, 60m, 90m, 1h, 1d, 5d, 1wk, 1mo, 3mo)
        
    Returns:
        List of market data dictionaries
        
    Example:
        >>> feed = MarketDataFeed()
        >>> data = feed.get_historical_data("AAPL", period="30d", interval="1d")
    """

def get_real_time_data(self, symbols: List[str]) -> Dict[str, Dict[str, Any]]:
    """Get real-time market data.
    
    Args:
        symbols: List of trading symbols
        
    Returns:
        Dictionary mapping symbols to market data
    """

def validate_data(self, data: List[Dict[str, Any]]) -> bool:
    """Validate market data quality.
    
    Args:
        data: Market data to validate
        
    Returns:
        True if data is valid, False otherwise
    """
```

## Order Management

### Order Manager

**Module**: `src.order.manager`

#### OrderManager

```python
class OrderManager:
    """Comprehensive order lifecycle management."""
    
    def __init__(self) -> None:
        """Initialize order manager."""
```

##### Methods

```python
def create_order(
    self,
    symbol: str,
    side: Union[OrderSide, str],
    order_type: Union[OrderType, str],
    quantity: float,
    price: Optional[float] = None,
    stop_price: Optional[float] = None,
    client_order_id: Optional[str] = None
) -> Order:
    """Create a new order.
    
    Args:
        symbol: Trading symbol
        side: Order side (BUY/SELL)
        order_type: Order type (MARKET/LIMIT/STOP/STOP_LIMIT)
        quantity: Order quantity
        price: Order price (required for limit orders)
        stop_price: Stop price (required for stop orders)
        client_order_id: Optional client order ID
        
    Returns:
        Order object
        
    Raises:
        ValueError: If required parameters are missing
    """

def cancel_order(self, order_id: UUID) -> Optional[Order]:
    """Cancel an existing order.
    
    Args:
        order_id: Order ID to cancel
        
    Returns:
        Cancelled order or None if not found
        
    Raises:
        ValueError: If order cannot be cancelled
    """

def get_open_orders(self, symbol: Optional[str] = None) -> List[Order]:
    """Get all open orders.
    
    Args:
        symbol: Optional symbol filter
        
    Returns:
        List of open orders
    """

def update_order_status(
    self,
    order_id: UUID,
    status: Union[OrderStatus, str],
    filled_quantity: Optional[float] = None,
    average_fill_price: Optional[float] = None
) -> Optional[Order]:
    """Update order status.
    
    Args:
        order_id: Order ID
        status: New order status
        filled_quantity: Filled quantity
        average_fill_price: Average fill price
        
    Returns:
        Updated order or None if not found
    """
```

##### Enums

```python
class OrderStatus(Enum):
    """Order status enumeration."""
    PENDING = "pending"
    SUBMITTED = "submitted"
    PARTIALLY_FILLED = "partially_filled"
    FILLED = "filled"
    CANCELED = "canceled"
    REJECTED = "rejected"
    EXPIRED = "expired"

class OrderType(Enum):
    """Order type enumeration."""
    MARKET = "market"
    LIMIT = "limit"
    STOP = "stop"
    STOP_LIMIT = "stop_limit"

class OrderSide(Enum):
    """Order side enumeration."""
    BUY = "buy"
    SELL = "sell"
```

## Strategy Engine

**Module**: `src.strategy.engine`

#### StrategyEngine

```python
class StrategyEngine:
    """Pluggable strategy framework with technical indicators."""
    
    def __init__(self, config: Optional[ConfigManager] = None) -> None:
        """Initialize strategy engine.
        
        Args:
            config: Configuration manager instance
        """
```

##### Methods

```python
def generate_signals(
    self,
    data: pd.DataFrame,
    strategy: str,
    params: Optional[Dict] = None
) -> pd.DataFrame:
    """Generate trading signals using specified strategy.
    
    Args:
        data: Market data DataFrame
        strategy: Strategy name
        params: Optional strategy parameters
        
    Returns:
        DataFrame with added signal columns
        
    Raises:
        ValueError: If strategy is unknown
        
    Example:
        >>> engine = StrategyEngine()
        >>> signals = engine.generate_signals(
        ...     data=df,
        ...     strategy="sma_crossover",
        ...     params={"short_window": 20, "long_window": 50}
        ... )
    """

def add_strategy(self, name: str, strategy_func: Callable) -> None:
    """Add custom strategy.
    
    Args:
        name: Strategy name
        strategy_func: Strategy function
    """
```

## Risk Management

### Risk Manager

**Module**: `risk.risk_manager`

#### RiskManager

```python
class RiskManager:
    """Sophisticated risk management and position sizing."""
    
    def __init__(self, risk_parameters: RiskParameters) -> None:
        """Initialize risk manager.
        
        Args:
            risk_parameters: Risk management parameters
        """
```

##### Methods

```python
def calculate_position_size(
    self,
    symbol: str,
    entry_price: float,
    stop_loss: float,
    take_profit: float
) -> float:
    """Calculate optimal position size.
    
    Args:
        symbol: Trading symbol
        entry_price: Entry price
        stop_loss: Stop loss price
        take_profit: Take profit price
        
    Returns:
        Recommended position size
        
    Raises:
        ValueError: If no market data available
    """

def validate_trade(
    self,
    symbol: str,
    quantity: float,
    entry_price: float,
    stop_loss: float,
    take_profit: float
) -> Dict[str, Any]:
    """Validate trade against risk parameters.
    
    Args:
        symbol: Trading symbol
        quantity: Trade quantity
        entry_price: Entry price
        stop_loss: Stop loss price
        take_profit: Take profit price
        
    Returns:
        Validation result dictionary
    """

def calculate_portfolio_risk(self) -> PortfolioRisk:
    """Calculate portfolio-level risk metrics.
    
    Returns:
        Portfolio risk object
    """
```

##### Data Classes

```python
class RiskParameters(BaseModel):
    """Risk management parameters."""
    account_value: float
    max_daily_risk: float = Field(ge=0.0, le=1.0)
    max_position_risk: float = Field(ge=0.0, le=1.0)
    max_drawdown: float = Field(ge=0.0, le=1.0)
    max_correlation: float = Field(ge=0.0, le=1.0)
    min_risk_reward: float = Field(ge=1.0)
    max_positions: int = Field(ge=1)
    position_sizing_method: str = "kelly"
```

## Performance Analytics

### Performance Analyzer

**Module**: `analysis.performance_analyzer`

#### PerformanceAnalyzer

```python
class PerformanceAnalyzer:
    """Comprehensive trading performance analysis."""
    
    def __init__(self) -> None:
        """Initialize performance analyzer."""
```

##### Methods

```python
def add_trade(
    self,
    plan: TradePlanSchema,
    position: PositionSchema,
    orders: List[OrderSchema]
) -> None:
    """Add completed trade to analysis.
    
    Args:
        plan: Trade plan schema
        position: Position schema
        orders: List of order schemas
    """

def get_overall_metrics(self) -> Optional[PerformanceMetrics]:
    """Get overall performance metrics.
    
    Returns:
        Performance metrics object or None
    """

def get_daily_metrics(self, date: datetime) -> Optional[DailyMetrics]:
    """Get metrics for specific day.
    
    Args:
        date: Date to get metrics for
        
    Returns:
        Daily metrics object or None
    """

def get_symbol_metrics(self, symbol: str) -> Dict[str, float]:
    """Get metrics for specific symbol.
    
    Args:
        symbol: Trading symbol
        
    Returns:
        Dictionary of symbol metrics
    """
```

### Performance Visualizer

**Module**: `analysis.performance_visualizer`

#### PerformanceVisualizer

```python
class PerformanceVisualizer:
    """Interactive performance visualizations using Plotly."""
    
    def __init__(self, performance_metrics: PerformanceMetrics) -> None:
        """Initialize performance visualizer.
        
        Args:
            performance_metrics: Performance metrics to visualize
        """
```

##### Methods

```python
def plot_equity_curve(self) -> go.Figure:
    """Create equity curve plot.
    
    Returns:
        Plotly figure object
    """

def plot_daily_returns(self) -> go.Figure:
    """Create daily returns plot.
    
    Returns:
        Plotly figure object
    """

def create_performance_dashboard(self) -> go.Figure:
    """Create comprehensive performance dashboard.
    
    Returns:
        Plotly figure with multiple subplots
    """

def plot_win_rate_by_symbol(self) -> go.Figure:
    """Create win rate by symbol plot.
    
    Returns:
        Plotly figure object
    """
```

## Data Schemas

### Core Schemas

**Module**: `data.schemas`

#### OrderSchema

```python
class OrderSchema(BaseModel):
    """Order data schema."""
    order_id: str
    symbol: str
    side: str
    order_type: str
    quantity: float
    price: Optional[float] = None
    status: str
    timestamp: datetime
    metadata: Dict[str, Any] = Field(default_factory=dict)
```

#### PositionSchema

```python
class PositionSchema(BaseModel):
    """Position data schema."""
    position_id: str
    symbol: str
    quantity: float
    entry_price: float
    current_price: float
    stop_loss: float
    take_profit: float
    pnl: float
    pnl_percent: float
    timestamp: datetime
    status: str
```

#### MarketDataSchema

```python
class MarketDataSchema(BaseModel):
    """Market data schema."""
    symbol: str
    price: float
    change: float
    change_percent: float
    volume: int
    high: float
    low: float
    timestamp: datetime
    additional_data: Dict[str, Any] = Field(default_factory=dict)
```

#### TradePlanSchema

```python
class TradePlanSchema(BaseModel):
    """Trade plan schema."""
    plan_id: str
    symbol: str
    side: str
    entry_price: float
    stop_loss: float
    take_profit: float
    quantity: float
    risk_parameters: Dict[str, Any]
    timestamp: datetime
```

## Error Handling

### Exception Classes

```python
class TradingError(Exception):
    """Base exception for trading-related errors."""
    pass

class OrderValidationError(TradingError):
    """Raised when order validation fails."""
    pass

class InsufficientFundsError(TradingError):
    """Raised when account has insufficient funds."""
    pass

class MarketDataError(TradingError):
    """Raised when market data is unavailable or invalid."""
    pass

class RiskLimitError(TradingError):
    """Raised when risk limits are exceeded."""
    pass
```

## Response Formats

### Standard Response Format

All agents and major components return responses in this format:

```python
{
    "status": "success" | "error" | "warning",
    "message": "Human-readable message",
    "data": {
        # Component-specific data
    }
}
```

### Error Response Format

```python
{
    "status": "error",
    "message": "Error description",
    "error_code": "ERROR_CODE",
    "details": {
        # Additional error details
    }
}
```

## Usage Examples

### Complete Trading Workflow

```python
from src.market_data.feed import MarketDataFeed
from src.strategy.engine import StrategyEngine
from src.order.manager import OrderManager, OrderSide, OrderType
from agents.plan_agent import PlanAgent
from agents.execute_agent import ExecuteAgent

# Initialize components
feed = MarketDataFeed()
engine = StrategyEngine()
order_manager = OrderManager()
plan_agent = PlanAgent()
execute_agent = ExecuteAgent()

# Get market data
data = feed.get_historical_data("AAPL", period="30d", interval="1d")

# Generate signals
df = pd.DataFrame(data)
signals = engine.generate_signals(
    data=df,
    strategy="sma_crossover",
    params={"short_window": 20, "long_window": 50}
)

# Plan trade
plan_result = plan_agent.execute(
    content="AAPL showing bullish momentum",
    symbols=["AAPL"]
)

# Execute trade
if signals["signal"].iloc[-1] == 1:
    execution_result = execute_agent.execute(
        symbol="AAPL",
        side="buy",
        quantity=100,
        order_type="market"
    )
```

### Configuration and Setup

```python
from src.core.config import ConfigManager
from src.core.database import DatabaseManager
from src.core.logging import LogManager

# Initialize core components
config = ConfigManager()
db = DatabaseManager()
log_manager = LogManager(config)

# Set up logging
logger = log_manager.get_logger(__name__)

# Initialize database
db.init_database()

# Get configuration
trading_config = config.get_trading_config()
logger.info(f"Trading configuration loaded: {trading_config}")
```

---

This API reference provides comprehensive documentation for all Intent Trader components. For additional examples and tutorials, see the [User Guides](../user-guides/) and [Developer Guides](../developer-guides/). 