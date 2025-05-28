import sys
from datetime import datetime
from pathlib import Path

import pytest
from data.schemas import TradePlanSchema, MarketDataSchema, PositionSchema

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))


@pytest.fixture
def sample_market_data():
    """Sample market data for testing."""
    return MarketDataSchema(
        symbol="AAPL",
        price=150.0,
        change=2.5,
        change_percent=1.69,
        volume=1000000,
        high=152.0,
        low=148.0,
        timestamp=datetime.now(),
    )


@pytest.fixture
def sample_position():
    """Sample position for testing."""
    return PositionSchema(
        position_id="pos_123",
        symbol="AAPL",
        quantity=100.0,
        entry_price=145.0,
        current_price=150.0,
        stop_loss=140.0,
        take_profit=160.0,
        pnl=500.0,
        pnl_percent=3.45,
        timestamp=datetime.now(),
        status="open",
    )


@pytest.fixture
def sample_trading_context():
    """Sample trading context for testing."""
    return {
        "market_regime": "trending",
        "volatility": 0.15,
        "account_balance": 100000.0,
        "buying_power": 50000.0,
        "open_positions": [],
    }


@pytest.fixture
def mock_config():
    """Mock configuration for testing."""
    return {
        "trading": {
            "default_symbol": "AAPL",
            "max_position_size": 1000,
            "risk_per_trade": 0.02,
        },
        "logging": {
            "level": "INFO",
            "format": "%(asctime)s - %(name)s - %(levelname)s - %(message)s",
        },
        "database": {
            "path": ":memory:",
        },
    }


@pytest.fixture
def sample_trade_plan():
    """Sample trade plan for testing."""
    return TradePlanSchema(
        plan_id="plan_123",
        symbol="AAPL",
        side="long",  # Changed from "buy" to "long"
        entry_price=150.0,
        stop_loss=145.0,
        take_profit=160.0,
        quantity=100.0,
        conviction_score=0.85,
        risk_parameters={"max_loss": 500.0, "risk_reward_ratio": 2.0},
        notes="Strong bullish momentum",
        timestamp=datetime.now(),
        status="active",
    )
