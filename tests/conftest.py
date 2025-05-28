import pytest
from datetime import datetime, timedelta
from data.storage import Storage
from core.agent_registry import AgentRegistry
from data.schemas import (
    MarketDataSchema,
    AnalysisSchema,
    TradePlanSchema,
    PositionSchema,
    OrderSchema,
    MarketContextSchema,
    RiskMetricsSchema,
    PerformanceMetricsSchema
)

@pytest.fixture
def storage():
    """Fixture for Storage instance"""
    return Storage()

@pytest.fixture
def agent_registry():
    """Fixture for AgentRegistry instance"""
    return AgentRegistry()

@pytest.fixture
def sample_market_data():
    """Fixture for sample market data"""
    return MarketDataSchema(
        symbol="ES",
        price=4500.0,
        change=10.0,
        change_percent=0.22,
        volume=1000000,
        high=4510.0,
        low=4490.0,
        timestamp=datetime.now(),
        additional_data={"atr": 20.0}
    )

@pytest.fixture
def sample_analysis():
    """Fixture for sample analysis data"""
    return AnalysisSchema(
        analysis_id="test_analysis_1",
        symbol="ES",
        content="Market showing strong bullish momentum",
        sentiment=0.8,
        confidence=0.9,
        timestamp=datetime.now(),
        source="analyst_agent",
        metadata={"key_levels": [4500, 4520]}
    )

@pytest.fixture
def sample_trade_plan():
    """Fixture for sample trade plan"""
    return TradePlanSchema(
        plan_id="test_plan_1",
        symbol="ES",
        side="long",
        entry_price=4500.0,
        stop_loss=4480.0,
        take_profit=4550.0,
        quantity=1.0,
        conviction_score=0.8,
        risk_parameters={
            "max_risk": 1000.0,
            "risk_per_trade": 0.02
        },
        notes="Strong bullish setup",
        timestamp=datetime.now(),
        status="active"
    )

@pytest.fixture
def sample_position():
    """Fixture for sample position"""
    return PositionSchema(
        position_id="test_position_1",
        symbol="ES",
        quantity=1.0,
        entry_price=4500.0,
        current_price=4510.0,
        stop_loss=4480.0,
        take_profit=4550.0,
        pnl=10.0,
        pnl_percent=0.22,
        timestamp=datetime.now(),
        status="open"
    )

@pytest.fixture
def sample_order():
    """Fixture for sample order"""
    return OrderSchema(
        order_id="test_order_1",
        symbol="ES",
        side="buy",
        order_type="limit",
        quantity=1.0,
        price=4500.0,
        status="filled",
        timestamp=datetime.now(),
        metadata={"execution_price": 4500.0}
    )

@pytest.fixture
def sample_market_context():
    """Fixture for sample market context"""
    return MarketContextSchema(
        timestamp=datetime.now(),
        market_regime="bullish",
        volatility=0.15,
        trend=0.02,
        volume_profile={
            "pre_market": 0.2,
            "regular": 0.6,
            "post_market": 0.2
        },
        key_levels={
            "support": [4480, 4450],
            "resistance": [4520, 4550]
        },
        metadata={"vix": 15.0}
    )

@pytest.fixture
def sample_risk_metrics():
    """Fixture for sample risk metrics"""
    return RiskMetricsSchema(
        timestamp=datetime.now(),
        portfolio_value=100000.0,
        total_risk=2000.0,
        position_risks={
            "ES": 1000.0,
            "SPX": 1000.0
        },
        correlation_matrix={
            "ES": {"SPX": 0.95, "QQQ": 0.85},
            "SPX": {"ES": 0.95, "QQQ": 0.80}
        },
        var_metrics={
            "daily_var": 1000.0,
            "weekly_var": 2500.0
        },
        metadata={"confidence_level": 0.95}
    )

@pytest.fixture
def sample_performance_metrics():
    """Fixture for sample performance metrics"""
    return PerformanceMetricsSchema(
        timestamp=datetime.now(),
        total_trades=100,
        winning_trades=60,
        losing_trades=40,
        win_rate=0.6,
        average_win=100.0,
        average_loss=50.0,
        profit_factor=3.0,
        sharpe_ratio=1.5,
        max_drawdown=0.1,
        metadata={"period": "1M"}
    ) 