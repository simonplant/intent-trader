from datetime import datetime

import pytest

from data.schemas import MarketDataSchema, PositionSchema
from risk.risk_manager import PortfolioRisk, PositionRisk, RiskManager, RiskParameters


@pytest.fixture
def risk_parameters():
    """Fixture for risk parameters"""
    return RiskParameters(
        account_value=100000.0,
        max_daily_risk=0.02,  # 2% max daily risk
        max_position_risk=0.01,  # 1% max position risk
        max_drawdown=0.05,  # 5% max drawdown
        max_correlation=0.7,  # 70% max correlation
        min_risk_reward=2.0,  # Minimum 2:1 risk-reward ratio
        max_positions=5,
        position_sizing_method="kelly",
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
        status="open",
    )


@pytest.fixture
def sample_market_data():
    """Fixture for sample market data"""
    return MarketDataSchema(
        symbol="ES",
        price=4510.0,
        change=10.0,
        change_percent=0.22,
        volume=1000000,
        high=4520.0,
        low=4500.0,
        timestamp=datetime.now(),
        additional_data={"atr": 20.0},
    )


@pytest.fixture
def risk_manager(risk_parameters):
    """Fixture for risk manager"""
    return RiskManager(risk_parameters)


def test_risk_parameters_validation():
    """Test risk parameters validation"""
    # Test valid parameters
    params = RiskParameters(
        account_value=100000.0,
        max_daily_risk=0.02,
        max_position_risk=0.01,
        max_drawdown=0.05,
        max_correlation=0.7,
        min_risk_reward=2.0,
        max_positions=5,
        position_sizing_method="kelly",
    )
    assert params.account_value == 100000.0

    # Test invalid position sizing method
    with pytest.raises(ValueError):
        RiskParameters(account_value=100000.0, position_sizing_method="invalid")

    # Test invalid risk parameters
    with pytest.raises(ValueError):
        RiskParameters(account_value=100000.0, max_daily_risk=1.5)  # Should be <= 1.0


def test_position_size_calculation(risk_manager, sample_market_data):
    """Test position size calculation"""
    # Test Kelly Criterion
    size = risk_manager.calculate_position_size(
        symbol="ES", entry_price=4500.0, stop_loss=4480.0, take_profit=4550.0
    )
    assert size > 0
    assert size <= 5.0  # Max position size should be reasonable

    # Test fixed sizing
    risk_manager.risk_parameters.position_sizing_method = "fixed"
    size = risk_manager.calculate_position_size(
        symbol="ES", entry_price=4500.0, stop_loss=4480.0, take_profit=4550.0
    )
    assert size > 0

    # Test adaptive sizing
    risk_manager.risk_parameters.position_sizing_method = "adaptive"
    size = risk_manager.calculate_position_size(
        symbol="ES", entry_price=4500.0, stop_loss=4480.0, take_profit=4550.0
    )
    assert size > 0


def test_position_risk_calculation(risk_manager, sample_position, sample_market_data):
    """Test position risk calculation"""
    # Update manager with position and market data
    risk_manager.update_positions({"ES": sample_position})
    risk_manager.update_market_data({"ES": sample_market_data})

    # Calculate position risk
    position_risk = risk_manager.calculate_position_risk("ES")
    assert position_risk is not None
    assert position_risk.symbol == "ES"
    assert position_risk.quantity == sample_position.quantity
    assert position_risk.entry_price == sample_position.entry_price
    assert position_risk.current_price == sample_market_data.price
    assert position_risk.stop_loss == sample_position.stop_loss
    assert position_risk.take_profit == sample_position.take_profit
    assert position_risk.risk_amount > 0
    assert position_risk.reward_amount > 0
    assert position_risk.risk_reward_ratio > 0
    assert position_risk.risk_percent > 0
    assert isinstance(position_risk.correlation, dict)


def test_portfolio_risk_calculation(risk_manager, sample_position, sample_market_data):
    """Test portfolio risk calculation"""
    # Update manager with position and market data
    risk_manager.update_positions({"ES": sample_position})
    risk_manager.update_market_data({"ES": sample_market_data})

    # Calculate portfolio risk
    portfolio_risk = risk_manager.calculate_portfolio_risk()
    assert portfolio_risk is not None
    assert portfolio_risk.total_value == risk_manager.risk_parameters.account_value
    assert portfolio_risk.total_risk > 0
    assert portfolio_risk.total_reward > 0
    assert portfolio_risk.net_risk_reward > 0
    assert "ES" in portfolio_risk.position_risks
    assert isinstance(portfolio_risk.correlation_matrix, dict)
    assert portfolio_risk.drawdown >= 0


def test_trade_validation(risk_manager, sample_market_data):
    """Test trade validation"""
    # Update manager with market data
    risk_manager.update_market_data({"ES": sample_market_data})

    # Test valid trade
    validation = risk_manager.validate_trade(
        symbol="ES",
        quantity=1.0,
        entry_price=4500.0,
        stop_loss=4480.0,
        take_profit=4550.0,
    )
    assert validation["valid"]

    # Test invalid position risk
    validation = risk_manager.validate_trade(
        symbol="ES",
        quantity=100.0,  # Too large position
        entry_price=4500.0,
        stop_loss=4480.0,
        take_profit=4550.0,
    )
    assert not validation["valid"]
    assert "Position risk" in validation["reason"]

    # Test invalid risk-reward ratio
    validation = risk_manager.validate_trade(
        symbol="ES",
        quantity=1.0,
        entry_price=4500.0,
        stop_loss=4480.0,
        take_profit=4510.0,  # Too close to entry
    )
    assert not validation["valid"]
    assert "Risk-reward ratio" in validation["reason"]

    # Test maximum positions
    for i in range(risk_manager.risk_parameters.max_positions):
        risk_manager.positions[f"SYMBOL_{i}"] = sample_position
    validation = risk_manager.validate_trade(
        symbol="ES",
        quantity=1.0,
        entry_price=4500.0,
        stop_loss=4480.0,
        take_profit=4550.0,
    )
    assert not validation["valid"]
    assert "Maximum number of positions" in validation["reason"]
