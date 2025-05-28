from datetime import datetime, timedelta

import pytest

from analysis.performance_analyzer import PerformanceAnalyzer, TradeMetrics
from data.schemas import OrderSchema, PositionSchema, TradePlanSchema


@pytest.fixture
def sample_trade_plan():
    return TradePlanSchema(
        plan_id="test_plan_1",
        symbol="AAPL",
        side="buy",
        entry_price=150.0,
        stop_loss=145.0,
        take_profit=160.0,
        quantity=100,
        strategy="momentum",
    )


@pytest.fixture
def sample_position():
    return PositionSchema(
        position_id="test_position_1",
        symbol="AAPL",
        quantity=100,
        entry_price=150.0,
        current_price=155.0,
        stop_loss=145.0,
        take_profit=160.0,
        pnl=500.0,
        pnl_percent=3.33,
        status="open",
    )


@pytest.fixture
def sample_orders():
    entry_time = datetime.now()
    exit_time = entry_time + timedelta(hours=2)

    return [
        OrderSchema(
            order_id="test_order_1",
            symbol="AAPL",
            side="buy",
            order_type="limit",
            price=150.0,
            quantity=100,
            status="filled",
            timestamp=entry_time,
            metadata={"execution_price": 150.0, "executed_at": entry_time},
        ),
        OrderSchema(
            order_id="test_order_2",
            symbol="AAPL",
            side="sell",
            order_type="limit",
            price=155.0,
            quantity=100,
            status="filled",
            timestamp=exit_time,
            metadata={"execution_price": 155.0, "executed_at": exit_time},
        ),
    ]


@pytest.fixture
def performance_analyzer():
    return PerformanceAnalyzer()


def test_add_trade(performance_analyzer, sample_trade_plan, sample_position, sample_orders):
    """Test adding a trade to the analyzer"""
    performance_analyzer.add_trade(sample_trade_plan, sample_position, sample_orders)

    # Check that trade was added
    assert len(performance_analyzer.trades) == 1

    # Check trade metrics
    trade = performance_analyzer.trades[0]
    assert trade.symbol == "AAPL"
    assert trade.entry_price == 150.0
    assert trade.exit_price == 155.0
    assert trade.quantity == 100
    assert trade.pnl == 500.0
    assert trade.pnl_percent == 3.33
    assert trade.win is True
    assert trade.risk_reward_ratio == 2.0  # (160-150)/(150-145)
    assert trade.execution_quality > 0
    assert trade.plan_adherence > 0


def test_daily_metrics(performance_analyzer, sample_trade_plan, sample_position, sample_orders):
    """Test daily metrics calculation"""
    performance_analyzer.add_trade(sample_trade_plan, sample_position, sample_orders)

    # Get daily metrics
    date = sample_orders[0].timestamp.date()
    daily_metrics = performance_analyzer.get_daily_metrics(date)

    assert daily_metrics is not None
    assert daily_metrics.total_trades == 1
    assert daily_metrics.winning_trades == 1
    assert daily_metrics.losing_trades == 0
    assert daily_metrics.win_rate == 1.0
    assert daily_metrics.total_pnl == 500.0
    assert daily_metrics.total_pnl_percent == 3.33
    assert daily_metrics.average_win == 500.0
    assert daily_metrics.average_loss == 0.0
    assert daily_metrics.profit_factor > 0


def test_overall_metrics(performance_analyzer, sample_trade_plan, sample_position, sample_orders):
    """Test overall metrics calculation"""
    performance_analyzer.add_trade(sample_trade_plan, sample_position, sample_orders)

    # Get overall metrics
    overall_metrics = performance_analyzer.get_overall_metrics()

    assert overall_metrics is not None
    assert overall_metrics.total_trades == 1
    assert overall_metrics.winning_trades == 1
    assert overall_metrics.losing_trades == 0
    assert overall_metrics.win_rate == 1.0
    assert overall_metrics.total_pnl == 500.0
    assert overall_metrics.total_pnl_percent == 3.33
    assert overall_metrics.average_win == 500.0
    assert overall_metrics.average_loss == 0.0
    assert overall_metrics.profit_factor > 0
    assert overall_metrics.max_drawdown >= 0
    assert overall_metrics.sharpe_ratio >= 0
    assert overall_metrics.sortino_ratio >= 0


def test_symbol_metrics(performance_analyzer, sample_trade_plan, sample_position, sample_orders):
    """Test symbol-specific metrics calculation"""
    performance_analyzer.add_trade(sample_trade_plan, sample_position, sample_orders)

    # Get symbol metrics
    symbol_metrics = performance_analyzer.get_symbol_metrics("AAPL")

    assert symbol_metrics is not None
    assert symbol_metrics["total_trades"] == 1
    assert symbol_metrics["winning_trades"] == 1
    assert symbol_metrics["win_rate"] == 1.0
    assert symbol_metrics["total_pnl"] == 500.0
    assert symbol_metrics["total_pnl_percent"] == 3.33
    assert symbol_metrics["average_win"] == 500.0
    assert symbol_metrics["average_loss"] == 0.0
    assert symbol_metrics["profit_factor"] > 0


def test_strategy_metrics(performance_analyzer, sample_trade_plan, sample_position, sample_orders):
    """Test strategy-specific metrics calculation"""
    performance_analyzer.add_trade(sample_trade_plan, sample_position, sample_orders)

    # Get strategy metrics
    strategy_metrics = performance_analyzer.get_strategy_metrics("momentum")

    assert strategy_metrics is not None
    assert strategy_metrics["total_trades"] == 1
    assert strategy_metrics["winning_trades"] == 1
    assert strategy_metrics["win_rate"] == 1.0
    assert strategy_metrics["total_pnl"] == 500.0
    assert strategy_metrics["total_pnl_percent"] == 3.33
    assert strategy_metrics["average_win"] == 500.0
    assert strategy_metrics["average_loss"] == 0.0
    assert strategy_metrics["profit_factor"] > 0


def test_multiple_trades(performance_analyzer):
    """Test adding multiple trades"""
    # Create multiple trades
    trades = []
    for i in range(3):
        entry_time = datetime.now() + timedelta(hours=i)
        exit_time = entry_time + timedelta(hours=1)

        plan = TradePlanSchema(
            plan_id=f"test_plan_{i}",
            symbol="AAPL",
            side="buy",
            entry_price=150.0,
            stop_loss=145.0,
            take_profit=160.0,
            quantity=100,
            strategy="momentum",
        )

        position = PositionSchema(
            position_id=f"test_position_{i}",
            symbol="AAPL",
            quantity=100,
            entry_price=150.0,
            current_price=155.0,
            stop_loss=145.0,
            take_profit=160.0,
            pnl=500.0,
            pnl_percent=3.33,
            status="open",
        )

        orders = [
            OrderSchema(
                order_id=f"test_order_{i}_1",
                symbol="AAPL",
                side="buy",
                order_type="limit",
                price=150.0,
                quantity=100,
                status="filled",
                timestamp=entry_time,
                metadata={"execution_price": 150.0, "executed_at": entry_time},
            ),
            OrderSchema(
                order_id=f"test_order_{i}_2",
                symbol="AAPL",
                side="sell",
                order_type="limit",
                price=155.0,
                quantity=100,
                status="filled",
                timestamp=exit_time,
                metadata={"execution_price": 155.0, "executed_at": exit_time},
            ),
        ]

        trades.append((plan, position, orders))

    # Add trades to analyzer
    for plan, position, orders in trades:
        performance_analyzer.add_trade(plan, position, orders)

    # Check overall metrics
    overall_metrics = performance_analyzer.get_overall_metrics()
    assert overall_metrics.total_trades == 3
    assert overall_metrics.winning_trades == 3
    assert overall_metrics.total_pnl == 1500.0
    assert overall_metrics.total_pnl_percent == 10.0


def test_losing_trade(performance_analyzer):
    """Test adding a losing trade"""
    entry_time = datetime.now()
    exit_time = entry_time + timedelta(hours=1)

    plan = TradePlanSchema(
        plan_id="test_plan_1",
        symbol="AAPL",
        side="buy",
        entry_price=150.0,
        stop_loss=145.0,
        take_profit=160.0,
        quantity=100,
        strategy="momentum",
    )

    position = PositionSchema(
        position_id="test_position_1",
        symbol="AAPL",
        quantity=100,
        entry_price=150.0,
        current_price=145.0,
        stop_loss=145.0,
        take_profit=160.0,
        pnl=-500.0,
        pnl_percent=-3.33,
        status="closed",
    )

    orders = [
        OrderSchema(
            order_id="test_order_1",
            symbol="AAPL",
            side="buy",
            order_type="limit",
            price=150.0,
            quantity=100,
            status="filled",
            timestamp=entry_time,
            metadata={"execution_price": 150.0, "executed_at": entry_time},
        ),
        OrderSchema(
            order_id="test_order_2",
            symbol="AAPL",
            side="sell",
            order_type="limit",
            price=145.0,
            quantity=100,
            status="filled",
            timestamp=exit_time,
            metadata={"execution_price": 145.0, "executed_at": exit_time},
        ),
    ]

    performance_analyzer.add_trade(plan, position, orders)

    # Check trade metrics
    trade = performance_analyzer.trades[0]
    assert trade.win is False
    assert trade.pnl == -500.0
    assert trade.pnl_percent == -3.33

    # Check overall metrics
    overall_metrics = performance_analyzer.get_overall_metrics()
    assert overall_metrics.winning_trades == 0
    assert overall_metrics.losing_trades == 1
    assert overall_metrics.win_rate == 0.0
    assert overall_metrics.total_pnl == -500.0
    assert overall_metrics.total_pnl_percent == -3.33
    assert overall_metrics.average_win == 0.0
    assert overall_metrics.average_loss == -500.0
