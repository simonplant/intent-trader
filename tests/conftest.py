import pytest
import sys
from pathlib import Path
from datetime import datetime, timedelta

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

@pytest.fixture
def sample_market_data():
    """Fixture for sample market data"""
    return {
        "symbol": "AAPL",
        "timestamp": datetime.now(),
        "open": 225.0,
        "high": 227.0,
        "low": 224.0,
        "close": 226.0,
        "volume": 1000000
    }

@pytest.fixture
def sample_position():
    """Fixture for sample position"""
    return {
        "symbol": "AAPL",
        "side": "long",
        "quantity": 100,
        "entry_price": 225.0,
        "stop_loss": 223.0,
        "take_profit": 228.0,
        "timestamp": datetime.now()
    }

@pytest.fixture
def sample_trading_context():
    """Fixture for trading context"""
    return {
        "phase": "PLAN",
        "positions": [],
        "plan": {},
        "session_metrics": {"pnl": 0.0, "trades": 0}
    }

@pytest.fixture
def mock_config():
    """Mock configuration for testing"""
    return {
        "trading": {
            "default_symbol": "AAPL",
            "position_size": 100,
            "max_positions": 5,
            "risk_per_trade": 0.02
        },
        "market_data": {
            "cache_dir": "data/cache",
            "update_interval": 60
        }
    } 