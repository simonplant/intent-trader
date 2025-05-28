from datetime import datetime, UTC
from typing import Dict, List, Optional, Union

import numpy as np
import pandas as pd
from plotly.graph_objects import Figure

try:
    import plotly.graph_objects as go
    from plotly.subplots import make_subplots
    PLOTLY_AVAILABLE = True
except ImportError:
    PLOTLY_AVAILABLE = False
    print("Warning: plotly not available. Visualization features will be disabled.")


class TradeMetrics:
    """Metrics for individual trades"""

    def __init__(
        self,
        symbol: str,
        entry_price: float,
        exit_price: float,
        quantity: float,
        entry_time: datetime,
        exit_time: datetime,
        pnl: float,
        pnl_percent: float,
    ):
        self.symbol = symbol
        self.entry_price = entry_price
        self.exit_price = exit_price
        self.quantity = quantity
        self.entry_time = entry_time
        self.exit_time = exit_time
        self.pnl = pnl
        self.pnl_percent = pnl_percent
        self.duration = exit_time - entry_time


class DailyMetrics:
    """Metrics for daily performance"""

    def __init__(
        self,
        date: datetime,
        pnl: float,
        pnl_percent: float,
        num_trades: int,
        win_rate: float,
        avg_trade: float,
    ):
        self.date = date
        self.pnl = pnl
        self.pnl_percent = pnl_percent
        self.num_trades = num_trades
        self.win_rate = win_rate
        self.avg_trade = avg_trade


class PerformanceMetrics:
    """Overall performance metrics"""

    def __init__(
        self,
        trades: List[TradeMetrics],
        daily_metrics: List[DailyMetrics],
        start_date: datetime,
        end_date: datetime,
        initial_capital: float,
        final_capital: float,
    ):
        self.trades = trades
        self.daily_metrics = daily_metrics
        self.start_date = start_date
        self.end_date = end_date
        self.initial_capital = initial_capital
        self.final_capital = final_capital
        self.total_pnl = final_capital - initial_capital
        self.total_pnl_percent = (self.total_pnl / initial_capital) * 100
        self.num_trades = len(trades)
        self.win_rate = sum(1 for t in trades if t.pnl > 0) / self.num_trades if self.num_trades > 0 else 0
        self.avg_trade = sum(t.pnl for t in trades) / self.num_trades if self.num_trades > 0 else 0
        self.max_drawdown = self._calculate_max_drawdown()
        self.sharpe_ratio = self._calculate_sharpe_ratio()
        self.sortino_ratio = self._calculate_sortino_ratio()

    def _calculate_max_drawdown(self) -> float:
        """Calculate maximum drawdown"""
        if not self.daily_metrics:
            return 0.0

        cumulative_returns = np.cumsum([m.pnl for m in self.daily_metrics])
        max_dd = 0.0
        peak = cumulative_returns[0]

        for value in cumulative_returns:
            if value > peak:
                peak = value
            dd = (peak - value) / peak if peak > 0 else 0
            max_dd = max(max_dd, dd)

        return max_dd

    def _calculate_sharpe_ratio(self) -> float:
        """Calculate Sharpe ratio"""
        if not self.daily_metrics:
            return 0.0

        returns = [m.pnl_percent for m in self.daily_metrics]
        if not returns:
            return 0.0

        avg_return = np.mean(returns)
        std_return = np.std(returns)
        return avg_return / std_return if std_return > 0 else 0.0

    def _calculate_sortino_ratio(self) -> float:
        """Calculate Sortino ratio"""
        if not self.daily_metrics:
            return 0.0

        returns = [m.pnl_percent for m in self.daily_metrics]
        if not returns:
            return 0.0

        avg_return = np.mean(returns)
        downside_returns = [r for r in returns if r < 0]
        downside_std = np.std(downside_returns) if downside_returns else 0.0
        return avg_return / downside_std if downside_std > 0 else 0.0


class PerformanceVisualizer:
    """Visualize trading performance metrics"""

    def __init__(self, metrics: PerformanceMetrics):
        self.metrics = metrics
        if not PLOTLY_AVAILABLE:
            print("Warning: plotly not available. Visualization features will be disabled.")

    def plot_equity_curve(self) -> Optional[Figure]:
        """Plot equity curve"""
        if not PLOTLY_AVAILABLE:
            return None

        dates = [m.date for m in self.metrics.daily_metrics]
        equity = np.cumsum([m.pnl for m in self.metrics.daily_metrics]) + self.metrics.initial_capital

        fig = go.Figure()
        fig.add_trace(
            go.Scatter(
                x=dates,
                y=equity,
                mode="lines",
                name="Equity",
                line=dict(color="blue", width=2),
            )
        )

        fig.update_layout(
            title="Equity Curve",
            xaxis_title="Date",
            yaxis_title="Equity",
            showlegend=True,
            template="plotly_white",
        )

        return fig

    def plot_daily_returns(self) -> Optional[Figure]:
        """Plot daily returns"""
        if not PLOTLY_AVAILABLE:
            return None

        dates = [m.date for m in self.metrics.daily_metrics]
        returns = [m.pnl_percent for m in self.metrics.daily_metrics]

        fig = go.Figure()
        fig.add_trace(
            go.Bar(
                x=dates,
                y=returns,
                name="Daily Returns",
                marker_color=np.where(np.array(returns) >= 0, "green", "red"),
            )
        )

        fig.update_layout(
            title="Daily Returns",
            xaxis_title="Date",
            yaxis_title="Return (%)",
            showlegend=True,
            template="plotly_white",
        )

        return fig

    def plot_win_rate_by_symbol(self) -> Optional[Figure]:
        """Plot win rate by symbol"""
        if not PLOTLY_AVAILABLE:
            return None

        symbol_trades = {}
        for trade in self.metrics.trades:
            if trade.symbol not in symbol_trades:
                symbol_trades[trade.symbol] = []
            symbol_trades[trade.symbol].append(trade)

        symbols = list(symbol_trades.keys())
        win_rates = [
            sum(1 for t in trades if t.pnl > 0) / len(trades)
            for trades in symbol_trades.values()
        ]

        fig = go.Figure()
        fig.add_trace(
            go.Bar(
                x=symbols,
                y=win_rates,
                name="Win Rate",
                marker_color="blue",
            )
        )

        fig.update_layout(
            title="Win Rate by Symbol",
            xaxis_title="Symbol",
            yaxis_title="Win Rate",
            showlegend=True,
            template="plotly_white",
        )

        return fig

    def plot_pnl_by_symbol(self) -> Optional[Figure]:
        """Plot P&L by symbol"""
        if not PLOTLY_AVAILABLE:
            return None

        symbol_pnl = {}
        for trade in self.metrics.trades:
            if trade.symbol not in symbol_pnl:
                symbol_pnl[trade.symbol] = 0.0
            symbol_pnl[trade.symbol] += trade.pnl

        symbols = list(symbol_pnl.keys())
        pnl = list(symbol_pnl.values())

        fig = go.Figure()
        fig.add_trace(
            go.Bar(
                x=symbols,
                y=pnl,
                name="P&L",
                marker_color=np.where(np.array(pnl) >= 0, "green", "red"),
            )
        )

        fig.update_layout(
            title="P&L by Symbol",
            xaxis_title="Symbol",
            yaxis_title="P&L",
            showlegend=True,
            template="plotly_white",
        )

        return fig

    def plot_strategy_performance(self) -> Optional[Figure]:
        """Plot comprehensive strategy performance dashboard"""
        if not PLOTLY_AVAILABLE:
            return None

        fig = make_subplots(
            rows=2,
            cols=2,
            subplot_titles=(
                "Equity Curve",
                "Daily Returns",
                "Win Rate by Symbol",
                "P&L by Symbol",
            ),
        )

        # Equity Curve
        dates = [m.date for m in self.metrics.daily_metrics]
        equity = np.cumsum([m.pnl for m in self.metrics.daily_metrics]) + self.metrics.initial_capital
        fig.add_trace(
            go.Scatter(
                x=dates,
                y=equity,
                mode="lines",
                name="Equity",
                line=dict(color="blue", width=2),
            ),
            row=1,
            col=1,
        )

        # Daily Returns
        returns = [m.pnl_percent for m in self.metrics.daily_metrics]
        fig.add_trace(
            go.Bar(
                x=dates,
                y=returns,
                name="Daily Returns",
                marker_color=np.where(np.array(returns) >= 0, "green", "red"),
            ),
            row=1,
            col=2,
        )

        # Win Rate by Symbol
        symbol_trades = {}
        for trade in self.metrics.trades:
            if trade.symbol not in symbol_trades:
                symbol_trades[trade.symbol] = []
            symbol_trades[trade.symbol].append(trade)

        symbols = list(symbol_trades.keys())
        win_rates = [
            sum(1 for t in trades if t.pnl > 0) / len(trades)
            for trades in symbol_trades.values()
        ]
        fig.add_trace(
            go.Bar(
                x=symbols,
                y=win_rates,
                name="Win Rate",
                marker_color="blue",
            ),
            row=2,
            col=1,
        )

        # P&L by Symbol
        symbol_pnl = {}
        for trade in self.metrics.trades:
            if trade.symbol not in symbol_pnl:
                symbol_pnl[trade.symbol] = 0.0
            symbol_pnl[trade.symbol] += trade.pnl

        pnl = list(symbol_pnl.values())
        fig.add_trace(
            go.Bar(
                x=symbols,
                y=pnl,
                name="P&L",
                marker_color=np.where(np.array(pnl) >= 0, "green", "red"),
            ),
            row=2,
            col=2,
        )

        fig.update_layout(
            title="Strategy Performance Dashboard",
            showlegend=True,
            template="plotly_white",
            height=800,
        )

        return fig

    def plot_trade_duration(self) -> Optional[Figure]:
        """Plot trade duration distribution"""
        if not PLOTLY_AVAILABLE:
            return None

        durations = [t.duration.total_seconds() / 3600 for t in self.metrics.trades]  # Convert to hours

        fig = go.Figure()
        fig.add_trace(
            go.Histogram(
                x=durations,
                name="Trade Duration",
                marker_color="blue",
                nbinsx=20,
            )
        )

        fig.update_layout(
            title="Trade Duration Distribution",
            xaxis_title="Duration (hours)",
            yaxis_title="Frequency",
            showlegend=True,
            template="plotly_white",
        )

        return fig

    def plot_risk_reward(self) -> Optional[Figure]:
        """Plot risk-reward scatter plot"""
        if not PLOTLY_AVAILABLE:
            return None

        pnl = [t.pnl for t in self.metrics.trades]
        pnl_percent = [t.pnl_percent for t in self.metrics.trades]

        fig = go.Figure()
        fig.add_trace(
            go.Scatter(
                x=pnl_percent,
                y=pnl,
                mode="markers",
                name="Risk-Reward",
                marker=dict(
                    color=np.where(np.array(pnl) >= 0, "green", "red"),
                    size=10,
                ),
            )
        )

        fig.update_layout(
            title="Risk-Reward Scatter Plot",
            xaxis_title="Return (%)",
            yaxis_title="P&L",
            showlegend=True,
            template="plotly_white",
        )

        return fig

    def plot_execution_quality(self) -> Optional[Figure]:
        """Plot execution quality metrics"""
        if not PLOTLY_AVAILABLE:
            return None

        # Calculate execution quality metrics
        slippage = []
        for trade in self.metrics.trades:
            if trade.side == "buy":
                slippage.append((trade.entry_price - trade.expected_price) / trade.expected_price * 100)
            else:
                slippage.append((trade.expected_price - trade.entry_price) / trade.expected_price * 100)

        fig = go.Figure()
        fig.add_trace(
            go.Histogram(
                x=slippage,
                name="Slippage",
                marker_color="blue",
                nbinsx=20,
            )
        )

        fig.update_layout(
            title="Execution Slippage Distribution",
            xaxis_title="Slippage (%)",
            yaxis_title="Frequency",
            showlegend=True,
            template="plotly_white",
        )

        return fig 