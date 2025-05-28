# Contributing to Intent Trader

Welcome to the Intent Trader development community! This guide will help you get started with contributing to the project.

## Development Environment Setup

### Prerequisites

- **Python 3.12+** (managed via pyenv)
- **Git** for version control
- **Make** for build automation
- **Docker** (optional, for containerized development)

### Initial Setup

```bash
# Clone the repository
git clone https://github.com/yourusername/intent-trader.git
cd intent-trader

# Set up Python environment
pyenv install 3.12.10
pyenv local 3.12.10

# Create virtual environment
python -m venv venv
source venv/bin/activate

# Install development dependencies
pip install -r dev-requirements.txt

# Install pre-commit hooks
pre-commit install

# Run initial tests
make test
```

## Project Structure

```
intent-trader/
├── src/                    # Core source code
│   ├── core/              # Core infrastructure
│   ├── market_data/       # Market data handling
│   ├── order/             # Order management
│   ├── strategy/          # Strategy engine
│   └── analysis/          # Performance analysis
├── agents/                # Trading agents (PFEMRC)
├── risk/                  # Risk management
├── tests/                 # Test suite
├── docs/                  # Documentation
├── config/                # Configuration files
├── scripts/               # Utility scripts
└── requirements*.txt      # Dependencies
```

## Code Quality Standards

### Type Safety

All code must include comprehensive type hints:

```python
from typing import Dict, List, Optional, Union
from datetime import datetime

def process_market_data(
    symbol: str,
    data: List[Dict[str, Union[float, str]]],
    start_time: Optional[datetime] = None
) -> Dict[str, float]:
    """Process market data and return metrics."""
    # Implementation here
    return {"price": 150.0, "volume": 1000000.0}
```

### Documentation

All public functions and classes must have comprehensive docstrings:

```python
class OrderManager:
    """Manages the creation, modification, and tracking of trading orders.
    
    The OrderManager provides a comprehensive interface for handling all
    aspects of order lifecycle management, including validation, execution,
    and tracking.
    
    Attributes:
        orders: Dictionary of active orders keyed by order ID
        positions: Dictionary of current positions keyed by symbol
        
    Example:
        >>> manager = OrderManager()
        >>> order = manager.create_order("AAPL", OrderSide.BUY, OrderType.MARKET, 100)
        >>> print(order.status)
        OrderStatus.PENDING
    """
    
    def create_order(
        self,
        symbol: str,
        side: OrderSide,
        order_type: OrderType,
        quantity: float,
        price: Optional[float] = None
    ) -> Order:
        """Create a new trading order.
        
        Args:
            symbol: Trading symbol (e.g., "AAPL")
            side: Order side (BUY or SELL)
            order_type: Type of order (MARKET, LIMIT, etc.)
            quantity: Number of shares to trade
            price: Limit price for limit orders
            
        Returns:
            Order object with unique ID and initial status
            
        Raises:
            ValueError: If required parameters are missing or invalid
            
        Example:
            >>> order = manager.create_order("AAPL", OrderSide.BUY, OrderType.LIMIT, 100, 150.0)
            >>> assert order.symbol == "AAPL"
        """
        # Implementation here
```

### Error Handling

Use structured error handling with appropriate exception types:

```python
from typing import Dict, Any

class TradingError(Exception):
    """Base exception for trading-related errors."""
    pass

class OrderValidationError(TradingError):
    """Raised when order validation fails."""
    pass

class InsufficientFundsError(TradingError):
    """Raised when account has insufficient funds."""
    pass

def validate_order(order_params: Dict[str, Any]) -> None:
    """Validate order parameters.
    
    Raises:
        OrderValidationError: If validation fails
    """
    if order_params.get("quantity", 0) <= 0:
        raise OrderValidationError("Quantity must be positive")
    
    if order_params.get("symbol") is None:
        raise OrderValidationError("Symbol is required")
```

## Testing Guidelines

### Test Structure

Tests are organized by component and type:

```
tests/
├── unit/                  # Unit tests
│   ├── test_order_manager.py
│   ├── test_strategy_engine.py
│   └── test_risk_manager.py
├── integration/           # Integration tests
│   ├── test_market_data.py
│   └── test_agent_workflow.py
├── performance/           # Performance tests
│   └── test_benchmarks.py
└── conftest.py           # Test configuration
```

### Writing Tests

Use pytest with comprehensive test coverage:

```python
import pytest
from unittest.mock import Mock, patch
from datetime import datetime

from src.order.manager import OrderManager, OrderSide, OrderType
from src.order.manager import OrderValidationError

class TestOrderManager:
    """Test suite for OrderManager."""
    
    @pytest.fixture
    def order_manager(self):
        """Create OrderManager instance for testing."""
        return OrderManager()
    
    @pytest.fixture
    def sample_market_data(self):
        """Sample market data for testing."""
        return {
            "symbol": "AAPL",
            "price": 150.0,
            "volume": 1000000,
            "timestamp": datetime.now()
        }
    
    def test_create_market_order_success(self, order_manager):
        """Test successful market order creation."""
        order = order_manager.create_order(
            symbol="AAPL",
            side=OrderSide.BUY,
            order_type=OrderType.MARKET,
            quantity=100
        )
        
        assert order.symbol == "AAPL"
        assert order.side == OrderSide.BUY
        assert order.quantity == 100
        assert order.id is not None
    
    def test_create_order_invalid_quantity(self, order_manager):
        """Test order creation with invalid quantity."""
        with pytest.raises(OrderValidationError, match="Quantity must be positive"):
            order_manager.create_order(
                symbol="AAPL",
                side=OrderSide.BUY,
                order_type=OrderType.MARKET,
                quantity=-100
            )
    
    @patch('src.order.manager.datetime')
    def test_order_timestamp(self, mock_datetime, order_manager):
        """Test order timestamp is set correctly."""
        mock_now = datetime(2023, 1, 1, 12, 0, 0)
        mock_datetime.now.return_value = mock_now
        
        order = order_manager.create_order(
            symbol="AAPL",
            side=OrderSide.BUY,
            order_type=OrderType.MARKET,
            quantity=100
        )
        
        assert order.created_at == mock_now
    
    def test_integration_with_market_data(self, order_manager, sample_market_data):
        """Test order manager integration with market data."""
        order_manager.update_market_data({"AAPL": sample_market_data})
        
        order = order_manager.create_order(
            symbol="AAPL",
            side=OrderSide.BUY,
            order_type=OrderType.MARKET,
            quantity=100
        )
        
        # Verify order was executed at market price
        assert order.status == OrderStatus.FILLED
        assert abs(order.execution_price - sample_market_data["price"]) < 0.01
```

### Test Coverage

Maintain 100% test coverage:

```bash
# Run tests with coverage
make test-coverage

# Generate HTML coverage report
make coverage-html

# View coverage report
open htmlcov/index.html
```

## Development Workflow

### Branch Strategy

We use GitFlow with the following branches:

- `main`: Production-ready code
- `develop`: Integration branch for features
- `feature/*`: Feature development branches
- `hotfix/*`: Critical bug fixes
- `release/*`: Release preparation

### Feature Development

1. **Create Feature Branch**
   ```bash
   git checkout develop
   git pull origin develop
   git checkout -b feature/your-feature-name
   ```

2. **Develop and Test**
   ```bash
   # Make your changes
   # Add tests
   make test
   make lint
   ```

3. **Commit Changes**
   ```bash
   git add .
   git commit -m "feat: add new feature description"
   ```

4. **Push and Create PR**
   ```bash
   git push origin feature/your-feature-name
   # Create pull request on GitHub
   ```

### Commit Message Format

Use conventional commits:

```
type(scope): description

[optional body]

[optional footer]
```

Types:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes
- `refactor`: Code refactoring
- `test`: Test additions/changes
- `chore`: Build/tooling changes

Examples:
```
feat(order): add support for bracket orders
fix(risk): correct position sizing calculation
docs(api): update order manager documentation
test(agents): add integration tests for PFEMRC workflow
```

## Code Review Process

### Pull Request Guidelines

1. **PR Description**
   - Clear description of changes
   - Link to related issues
   - Screenshots for UI changes
   - Breaking changes noted

2. **Code Quality Checks**
   - All tests pass
   - Code coverage maintained
   - Linting passes
   - Documentation updated

3. **Review Criteria**
   - Code follows style guidelines
   - Tests are comprehensive
   - Documentation is clear
   - Performance impact considered

### Review Checklist

- [ ] Code follows project style guidelines
- [ ] All tests pass and coverage is maintained
- [ ] Documentation is updated
- [ ] No breaking changes without version bump
- [ ] Error handling is appropriate
- [ ] Performance impact is acceptable
- [ ] Security considerations addressed

## Development Tools

### Make Commands

```bash
# Development
make install          # Install dependencies
make install-dev      # Install dev dependencies
make clean           # Clean build artifacts

# Code Quality
make format          # Format code with black/isort
make lint            # Run linting (flake8, pylint)
make type-check      # Run mypy type checking
make check-all       # Run all quality checks

# Testing
make test            # Run test suite
make test-unit       # Run unit tests only
make test-integration # Run integration tests only
make test-coverage   # Run tests with coverage
make coverage-html   # Generate HTML coverage report

# Documentation
make docs            # Build documentation
make docs-serve      # Serve docs locally
make docs-clean      # Clean docs build

# Release
make build           # Build distribution packages
make release         # Create release (maintainers only)
```

### Pre-commit Hooks

Pre-commit hooks ensure code quality:

```yaml
# .pre-commit-config.yaml
repos:
  - repo: https://github.com/psf/black
    rev: 23.3.0
    hooks:
      - id: black
        language_version: python3.12

  - repo: https://github.com/pycqa/isort
    rev: 5.12.0
    hooks:
      - id: isort

  - repo: https://github.com/pycqa/flake8
    rev: 6.0.0
    hooks:
      - id: flake8

  - repo: https://github.com/pre-commit/mirrors-mypy
    rev: v1.3.0
    hooks:
      - id: mypy
```

### IDE Configuration

#### VS Code Settings

```json
{
    "python.defaultInterpreterPath": "./venv/bin/python",
    "python.linting.enabled": true,
    "python.linting.flake8Enabled": true,
    "python.linting.mypyEnabled": true,
    "python.formatting.provider": "black",
    "python.sortImports.args": ["--profile", "black"],
    "editor.formatOnSave": true,
    "editor.codeActionsOnSave": {
        "source.organizeImports": true
    }
}
```

## Adding New Features

### Creating a New Agent

1. **Create Agent Class**
   ```python
   # agents/my_new_agent.py
   from typing import Any, Dict
   from agents.base_agent import BaseAgent
   
   class MyNewAgent(BaseAgent):
       """Agent for specific trading functionality."""
       
       def execute(self, **kwargs) -> Dict[str, Any]:
           """Execute agent functionality.
           
           Args:
               **kwargs: Agent-specific parameters
               
           Returns:
               Standardized response dictionary
           """
           try:
               # Agent implementation
               result = self._process_request(**kwargs)
               
               return {
                   "status": "success",
                   "message": "Agent executed successfully",
                   "data": result
               }
           except Exception as e:
               return {
                   "status": "error",
                   "message": f"Agent execution failed: {str(e)}"
               }
       
       def _process_request(self, **kwargs) -> Dict[str, Any]:
           """Process the agent request."""
           # Implementation here
           return {}
   ```

2. **Add Tests**
   ```python
   # tests/test_my_new_agent.py
   import pytest
   from agents.my_new_agent import MyNewAgent
   
   class TestMyNewAgent:
       @pytest.fixture
       def agent(self):
           return MyNewAgent()
       
       def test_execute_success(self, agent):
           result = agent.execute(param1="value1")
           assert result["status"] == "success"
       
       def test_execute_error_handling(self, agent):
           result = agent.execute(invalid_param=None)
           assert result["status"] == "error"
   ```

3. **Update Documentation**
   - Add agent to architecture documentation
   - Update API reference
   - Add usage examples

### Adding New Strategy

1. **Implement Strategy Function**
   ```python
   # src/strategy/strategies/my_strategy.py
   import pandas as pd
   from typing import Dict, Optional
   
   def my_custom_strategy(
       data: pd.DataFrame,
       params: Optional[Dict] = None
   ) -> pd.DataFrame:
       """Custom trading strategy implementation.
       
       Args:
           data: Market data DataFrame with OHLCV columns
           params: Strategy parameters
           
       Returns:
           DataFrame with added signal columns
       """
       if params is None:
           params = {"threshold": 0.02}
       
       # Strategy implementation
       data["signal"] = 0  # Initialize signals
       
       # Add your strategy logic here
       # Example: Simple momentum strategy
       data["returns"] = data["close"].pct_change()
       data.loc[data["returns"] > params["threshold"], "signal"] = 1
       data.loc[data["returns"] < -params["threshold"], "signal"] = -1
       
       return data
   ```

2. **Register Strategy**
   ```python
   # src/strategy/engine.py
   from .strategies.my_strategy import my_custom_strategy
   
   class StrategyEngine:
       def __init__(self):
           self.strategies = {
               "sma_crossover": self._sma_crossover_strategy,
               "rsi": self._rsi_strategy,
               "my_custom": my_custom_strategy,  # Add here
           }
   ```

3. **Add Tests and Documentation**

## Performance Guidelines

### Optimization Best Practices

1. **Use Efficient Data Structures**
   ```python
   # Good: Use pandas for numerical operations
   import pandas as pd
   import numpy as np
   
   def calculate_returns(prices: pd.Series) -> pd.Series:
       return prices.pct_change()
   
   # Avoid: Manual loops for large datasets
   def slow_calculate_returns(prices: list) -> list:
       returns = []
       for i in range(1, len(prices)):
           returns.append((prices[i] - prices[i-1]) / prices[i-1])
       return returns
   ```

2. **Profile Performance-Critical Code**
   ```python
   import cProfile
   import pstats
   
   def profile_function():
       """Profile a function for performance analysis."""
       profiler = cProfile.Profile()
       profiler.enable()
       
       # Your code here
       result = expensive_function()
       
       profiler.disable()
       stats = pstats.Stats(profiler)
       stats.sort_stats('cumulative')
       stats.print_stats(10)
       
       return result
   ```

3. **Use Caching for Expensive Operations**
   ```python
   from functools import lru_cache
   
   @lru_cache(maxsize=128)
   def expensive_calculation(symbol: str, period: str) -> float:
       """Cache expensive calculations."""
       # Expensive operation here
       return result
   ```

### Memory Management

1. **Use Generators for Large Datasets**
   ```python
   def process_large_dataset(file_path: str):
       """Process large dataset efficiently."""
       with open(file_path, 'r') as f:
           for line in f:
               yield process_line(line)
   ```

2. **Clean Up Resources**
   ```python
   from contextlib import contextmanager
   
   @contextmanager
   def database_connection():
       """Manage database connections properly."""
       conn = create_connection()
       try:
           yield conn
       finally:
           conn.close()
   ```

## Security Guidelines

### Input Validation

```python
from pydantic import BaseModel, validator
from typing import List

class TradingRequest(BaseModel):
    symbol: str
    quantity: float
    price: Optional[float] = None
    
    @validator('symbol')
    def validate_symbol(cls, v):
        if not v.isalpha() or len(v) > 10:
            raise ValueError('Invalid symbol format')
        return v.upper()
    
    @validator('quantity')
    def validate_quantity(cls, v):
        if v <= 0:
            raise ValueError('Quantity must be positive')
        return v
```

### Secure Configuration

```python
import os
from typing import Optional

class SecureConfig:
    """Secure configuration management."""
    
    @staticmethod
    def get_api_key() -> Optional[str]:
        """Get API key from environment."""
        return os.getenv('TRADING_API_KEY')
    
    @staticmethod
    def get_database_url() -> str:
        """Get database URL with fallback."""
        return os.getenv('DATABASE_URL', 'sqlite:///trading.db')
```

## Release Process

### Version Management

We use semantic versioning (SemVer):

- `MAJOR.MINOR.PATCH`
- `MAJOR`: Breaking changes
- `MINOR`: New features (backward compatible)
- `PATCH`: Bug fixes (backward compatible)

### Release Checklist

1. **Pre-release**
   - [ ] All tests pass
   - [ ] Documentation updated
   - [ ] CHANGELOG.md updated
   - [ ] Version bumped

2. **Release**
   - [ ] Create release branch
   - [ ] Final testing
   - [ ] Tag release
   - [ ] Build and publish

3. **Post-release**
   - [ ] Merge to main
   - [ ] Update develop branch
   - [ ] Announce release

## Getting Help

### Resources

- **Documentation**: [docs/](../README.md)
- **API Reference**: [docs/api-reference/](../api-reference/README.md)
- **Architecture Guide**: [ARCHITECTURE.md](../../ARCHITECTURE.md)

### Communication

- **Issues**: [GitHub Issues](https://github.com/yourusername/intent-trader/issues)
- **Discussions**: [GitHub Discussions](https://github.com/yourusername/intent-trader/discussions)
- **Discord**: [Development Discord Server](https://discord.gg/intent-trader)

### Mentorship

New contributors can request mentorship:

1. Comment on a "good first issue"
2. Join our Discord server
3. Attend weekly development calls

## Code of Conduct

We are committed to providing a welcoming and inclusive environment. Please read our [Code of Conduct](CODE_OF_CONDUCT.md) before contributing.

## License

By contributing to Intent Trader, you agree that your contributions will be licensed under the GNU Affero General Public License v3.0.

---

Thank you for contributing to Intent Trader! Your efforts help make algorithmic trading more accessible and reliable for everyone. 