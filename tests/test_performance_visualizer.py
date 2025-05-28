from datetime import datetime, timedelta

import pytest

from analysis.performance_analyzer import DailyMetrics, PerformanceMetrics, TradeMetrics
from analysis.performance_visualizer import PerformanceVisualizer


@pytest.fixture
def sample_trade_metrics():
    return TradeMetrics(
        symbol="AAPL",
        entry_price=150.0,
        exit_price=155.0,
        quantity=100,
        pnl=500.0,
        pnl_percent=3.33,
        holding_time=timedelta(hours=2),
        entry_time=datetime.now(),
        exit_time=datetime.now() + timedelta(hours=2),
        win=True,
        risk_reward_ratio=2.0,
        max_drawdown=0.02,
        max_profit=0.05,
        execution_quality=0.9,
        plan_adherence=0.95,
    )


@pytest.fixture
def sample_daily_metrics(sample_trade_metrics):
    return DailyMetrics(
        date=datetime.now().date(),
        total_trades=1,
        winning_trades=1,
        losing_trades=0,
        win_rate=1.0,
        total_pnl=500.0,
        total_pnl_percent=3.33,
        average_win=500.0,
        average_loss=0.0,
        profit_factor=0.0,
        max_drawdown=0.02,
        sharpe_ratio=2.0,
        sortino_ratio=3.0,
        trades=[sample_trade_metrics],
    )


@pytest.fixture
def sample_performance_metrics(sample_daily_metrics):
    return PerformanceMetrics(
        start_date=datetime.now(),
        end_date=datetime.now() + timedelta(days=1),
        total_trades=1,
        winning_trades=1,
        losing_trades=0,
        win_rate=1.0,
        total_pnl=500.0,
        total_pnl_percent=3.33,
        average_win=500.0,
        average_loss=0.0,
        profit_factor=0.0,
        max_drawdown=0.02,
        sharpe_ratio=2.0,
        sortino_ratio=3.0,
        daily_metrics=[sample_daily_metrics],
        symbol_metrics={
            "AAPL": {
                "total_trades": 1,
                "winning_trades": 1,
                "win_rate": 1.0,
                "total_pnl": 500.0,
                "total_pnl_percent": 3.33,
                "average_win": 500.0,
                "average_loss": 0.0,
                "profit_factor": 0.0,
            }
        },
        strategy_metrics={
            "momentum": {
                "total_trades": 1,
                "winning_trades": 1,
                "win_rate": 1.0,
                "total_pnl": 500.0,
                "total_pnl_percent": 3.33,
                "average_win": 500.0,
                "average_loss": 0.0,
                "profit_factor": 0.0,
            }
        },
    )


@pytest.fixture
def performance_visualizer(sample_performance_metrics):
    return PerformanceVisualizer(sample_performance_metrics)


def test_plot_equity_curve(performance_visualizer):
    """Test equity curve plot"""
    fig = performance_visualizer.plot_equity_curve()

    # Check figure properties
    assert fig is not None
    assert len(fig.data) == 2  # Equity curve and drawdown
    assert fig.layout.title.text == "Equity Curve and Drawdown"
    assert fig.layout.xaxis.title.text == "Date"
    assert fig.layout.yaxis.title.text == "Cumulative P&L"
    assert fig.layout.yaxis2.title.text == "Drawdown %"


def test_plot_daily_returns(performance_visualizer):
    """Test daily returns plot"""
    fig = performance_visualizer.plot_daily_returns()

    # Check figure properties
    assert fig is not None
    assert len(fig.data) == 1
    assert fig.layout.title.text == "Daily Returns"
    assert fig.layout.xaxis.title.text == "Date"
    assert fig.layout.yaxis.title.text == "Return %"


def test_plot_win_rate_by_symbol(performance_visualizer):
    """Test win rate by symbol plot"""
    fig = performance_visualizer.plot_win_rate_by_symbol()

    # Check figure properties
    assert fig is not None
    assert len(fig.data) == 1
    assert fig.layout.title.text == "Win Rate by Symbol"
    assert fig.layout.xaxis.title.text == "Symbol"
    assert fig.layout.yaxis.title.text == "Win Rate %"


def test_plot_pnl_by_symbol(performance_visualizer):
    """Test P&L by symbol plot"""
    fig = performance_visualizer.plot_pnl_by_symbol()

    # Check figure properties
    assert fig is not None
    assert len(fig.data) == 1
    assert fig.layout.title.text == "Total P&L by Symbol"
    assert fig.layout.xaxis.title.text == "Symbol"
    assert fig.layout.yaxis.title.text == "P&L"


def test_plot_strategy_performance(performance_visualizer):
    """Test strategy performance plot"""
    fig = performance_visualizer.plot_strategy_performance()

    # Check figure properties
    assert fig is not None
    assert len(fig.data) == 2  # Win rate and P&L
    assert fig.layout.title.text == "Strategy Performance"
    assert fig.layout.xaxis.title.text == "Strategy"
    assert fig.layout.yaxis.title.text == "Win Rate %"
    assert fig.layout.yaxis2.title.text == "Total P&L"


def test_plot_trade_duration(performance_visualizer):
    """Test trade duration plot"""
    fig = performance_visualizer.plot_trade_duration()

    # Check figure properties
    assert fig is not None
    assert len(fig.data) == 1
    assert fig.layout.title.text == "Trade Duration Distribution"
    assert fig.layout.xaxis.title.text == "Duration (hours)"
    assert fig.layout.yaxis.title.text == "Count"


def test_plot_risk_reward(performance_visualizer):
    """Test risk-reward plot"""
    fig = performance_visualizer.plot_risk_reward()

    # Check figure properties
    assert fig is not None
    assert len(fig.data) == 1
    assert fig.layout.title.text == "Risk-Reward vs P&L"
    assert fig.layout.xaxis.title.text == "Risk-Reward Ratio"
    assert fig.layout.yaxis.title.text == "P&L"


def test_plot_execution_quality(performance_visualizer):
    """Test execution quality plot"""
    fig = performance_visualizer.plot_execution_quality()

    # Check figure properties
    assert fig is not None
    assert len(fig.data) == 2  # Execution quality and plan adherence
    assert fig.layout.title.text == "Execution Quality Metrics"
    assert fig.layout.yaxis.title.text == "Score %"


def test_create_performance_dashboard(performance_visualizer):
    """Test performance dashboard creation"""
    fig = performance_visualizer.create_performance_dashboard()

    # Check figure properties
    assert fig is not None
    assert len(fig.data) == 8  # All plots combined (equity curve has 2, strategy performance has 2)
    assert fig.layout.title.text == "Trading Performance Dashboard"
    assert fig.layout.height == 1200

    # Check subplot titles
    subplot_titles = [
        "Equity Curve",
        "Daily Returns",
        "Win Rate by Symbol",
        "P&L by Symbol",
        "Strategy Performance",
        "Trade Duration",
    ]
    assert fig.layout.annotations[0].text == subplot_titles[0]
    assert fig.layout.annotations[1].text == subplot_titles[1]
    assert fig.layout.annotations[2].text == subplot_titles[2]
    assert fig.layout.annotations[3].text == subplot_titles[3]
    assert fig.layout.annotations[4].text == subplot_titles[4]
    assert fig.layout.annotations[5].text == subplot_titles[5]
