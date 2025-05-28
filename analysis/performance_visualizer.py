# Make plotly optional
try:
    import plotly.graph_objects as go
    import plotly.express as px
    from plotly.subplots import make_subplots
    PLOTLY_AVAILABLE = True
except ImportError:
    PLOTLY_AVAILABLE = False
    go = None
    px = None
    make_subplots = None

import pandas as pd
from datetime import datetime, timedelta
from typing import List, Dict, Any, Optional, Union, TYPE_CHECKING

if TYPE_CHECKING:
    try:
        from plotly.graph_objects import Figure
    except ImportError:
        Figure = Any
else:
    Figure = Any

# Fix import path - create a simple performance metrics structure for now
from dataclasses import dataclass

@dataclass
class TradeMetrics:
    symbol: str
    pnl: float
    holding_time: timedelta
    risk_reward_ratio: float
    execution_quality: float
    plan_adherence: float

@dataclass  
class DailyMetrics:
    date: datetime
    total_pnl: float
    total_pnl_percent: float
    trades: List[TradeMetrics]

@dataclass
class PerformanceMetrics:
    daily_metrics: List[DailyMetrics]
    symbol_metrics: Dict[str, Dict]
    strategy_metrics: Dict[str, Dict]

class PerformanceVisualizer:
    """Visualizer for trading performance metrics"""
    
    def __init__(self, metrics: PerformanceMetrics):
        self.metrics = metrics
        if not PLOTLY_AVAILABLE:
            print("Warning: Plotly not available. Visualization features disabled.")
        
    def plot_equity_curve(self) -> Optional[Figure]:
        """Plot equity curve"""
        if not PLOTLY_AVAILABLE:
            print("Plotly not available. Cannot create equity curve plot.")
            return None
            
        # Create cumulative P&L data
        dates = []
        cumulative_pnl = []
        current_pnl = 0
        
        for daily in sorted(self.metrics.daily_metrics, key=lambda x: x.date):
            dates.append(daily.date)
            current_pnl += daily.total_pnl
            cumulative_pnl.append(current_pnl)
            
        # Create figure
        fig = go.Figure()
        
        # Add equity curve
        fig.add_trace(go.Scatter(
            x=dates,
            y=cumulative_pnl,
            mode='lines',
            name='Equity',
            line=dict(color='blue', width=2)
        ))
        
        # Add drawdown
        drawdown = []
        peak = 0
        for pnl in cumulative_pnl:
            peak = max(peak, pnl)
            drawdown.append((pnl - peak) / peak * 100 if peak > 0 else 0)
            
        fig.add_trace(go.Scatter(
            x=dates,
            y=drawdown,
            mode='lines',
            name='Drawdown %',
            line=dict(color='red', width=1),
            yaxis='y2'
        ))
        
        # Update layout
        fig.update_layout(
            title='Equity Curve and Drawdown',
            xaxis_title='Date',
            yaxis_title='Cumulative P&L',
            yaxis2=dict(
                title='Drawdown %',
                overlaying='y',
                side='right',
                range=[min(drawdown) * 1.1, 5]
            ),
            showlegend=True,
            hovermode='x unified'
        )
        
        return fig
        
    def plot_daily_returns(self) -> Optional[Figure]:
        """Plot daily returns"""
        if not PLOTLY_AVAILABLE:
            print("Plotly not available. Cannot create daily returns plot.")
            return None
            
        # Create daily returns data
        dates = []
        returns = []
        
        for daily in sorted(self.metrics.daily_metrics, key=lambda x: x.date):
            dates.append(daily.date)
            returns.append(daily.total_pnl_percent)
            
        # Create figure
        fig = go.Figure()
        
        # Add daily returns
        fig.add_trace(go.Bar(
            x=dates,
            y=returns,
            name='Daily Returns %',
            marker_color=['green' if r >= 0 else 'red' for r in returns]
        ))
        
        # Update layout
        fig.update_layout(
            title='Daily Returns',
            xaxis_title='Date',
            yaxis_title='Return %',
            showlegend=True,
            hovermode='x unified'
        )
        
        return fig
        
    def plot_win_rate_by_symbol(self) -> Optional[Figure]:
        """Plot win rate by symbol"""
        if not PLOTLY_AVAILABLE:
            print("Plotly not available. Cannot create win rate plot.")
            return None
            
        # Create win rate data
        symbols = []
        win_rates = []
        
        for symbol, metrics in self.metrics.symbol_metrics.items():
            symbols.append(symbol)
            win_rates.append(metrics['win_rate'] * 100)
            
        # Create figure
        fig = go.Figure()
        
        # Add win rates
        fig.add_trace(go.Bar(
            x=symbols,
            y=win_rates,
            name='Win Rate %',
            marker_color='blue'
        ))
        
        # Update layout
        fig.update_layout(
            title='Win Rate by Symbol',
            xaxis_title='Symbol',
            yaxis_title='Win Rate %',
            showlegend=True,
            hovermode='x unified'
        )
        
        return fig
        
    def plot_pnl_by_symbol(self) -> Optional[Figure]:
        """Plot P&L by symbol"""
        if not PLOTLY_AVAILABLE:
            print("Plotly not available. Cannot create P&L plot.")
            return None
            
        # Create P&L data
        symbols = []
        pnl = []
        
        for symbol, metrics in self.metrics.symbol_metrics.items():
            symbols.append(symbol)
            pnl.append(metrics['total_pnl'])
            
        # Create figure
        fig = go.Figure()
        
        # Add P&L
        fig.add_trace(go.Bar(
            x=symbols,
            y=pnl,
            name='Total P&L',
            marker_color=['green' if p >= 0 else 'red' for p in pnl]
        ))
        
        # Update layout
        fig.update_layout(
            title='Total P&L by Symbol',
            xaxis_title='Symbol',
            yaxis_title='P&L',
            showlegend=True,
            hovermode='x unified'
        )
        
        return fig
        
    def plot_strategy_performance(self) -> Optional[Figure]:
        """Plot strategy performance"""
        if not PLOTLY_AVAILABLE:
            print("Plotly not available. Cannot create strategy performance plot.")
            return None
        
        # Create strategy performance data
        strategies = []
        win_rates = []
        pnl = []
        
        for strategy, metrics in self.metrics.strategy_metrics.items():
            strategies.append(strategy)
            win_rates.append(metrics['win_rate'] * 100)
            pnl.append(metrics['total_pnl'])
            
        # Create figure with secondary y-axis
        fig = make_subplots(specs=[[{"secondary_y": True}]])
        
        # Add win rates
        fig.add_trace(
            go.Bar(
                x=strategies,
                y=win_rates,
                name='Win Rate %',
                marker_color='blue'
            ),
            secondary_y=False
        )
        
        # Add P&L
        fig.add_trace(
            go.Bar(
                x=strategies,
                y=pnl,
                name='Total P&L',
                marker_color=['green' if p >= 0 else 'red' for p in pnl]
            ),
            secondary_y=True
        )
        
        # Update layout
        fig.update_layout(
            title='Strategy Performance',
            xaxis_title='Strategy',
            showlegend=True,
            hovermode='x unified'
        )
        
        # Update y-axes labels
        fig.update_yaxes(title_text='Win Rate %', secondary_y=False)
        fig.update_yaxes(title_text='Total P&L', secondary_y=True)
        
        return fig
        
    def plot_trade_duration(self) -> Optional[Figure]:
        """Plot trade duration distribution"""
        if not PLOTLY_AVAILABLE:
            print("Plotly not available. Cannot create trade duration plot.")
            return None
            
        # Create duration data
        durations = []
        for daily in self.metrics.daily_metrics:
            for trade in daily.trades:
                durations.append(trade.holding_time.total_seconds() / 3600)  # Convert to hours
                
        # Create figure
        fig = go.Figure()
        
        # Add histogram
        fig.add_trace(go.Histogram(
            x=durations,
            name='Trade Duration',
            marker_color='blue',
            nbinsx=20
        ))
        
        # Update layout
        fig.update_layout(
            title='Trade Duration Distribution',
            xaxis_title='Duration (hours)',
            yaxis_title='Count',
            showlegend=True,
            hovermode='x unified'
        )
        
        return fig
        
    def plot_risk_reward(self) -> Optional[Figure]:
        """Plot risk-reward scatter plot"""
        if not PLOTLY_AVAILABLE:
            print("Plotly not available. Cannot create risk-reward plot.")
            return None
            
        # Create risk-reward data
        risk_reward = []
        pnl = []
        symbols = []
        
        for daily in self.metrics.daily_metrics:
            for trade in daily.trades:
                risk_reward.append(trade.risk_reward_ratio)
                pnl.append(trade.pnl)
                symbols.append(trade.symbol)
                
        # Create figure
        fig = go.Figure()
        
        # Add scatter plot
        fig.add_trace(go.Scatter(
            x=risk_reward,
            y=pnl,
            mode='markers',
            name='Trades',
            marker=dict(
                size=10,
                color=['green' if p >= 0 else 'red' for p in pnl],
                symbol='circle'
            ),
            text=symbols,
            hovertemplate='Symbol: %{text}<br>Risk-Reward: %{x:.2f}<br>P&L: %{y:.2f}<extra></extra>'
        ))
        
        # Update layout
        fig.update_layout(
            title='Risk-Reward vs P&L',
            xaxis_title='Risk-Reward Ratio',
            yaxis_title='P&L',
            showlegend=True,
            hovermode='closest'
        )
        
        return fig
        
    def plot_execution_quality(self) -> Optional[Figure]:
        """Plot execution quality metrics"""
        if not PLOTLY_AVAILABLE:
            print("Plotly not available. Cannot create execution quality plot.")
            return None
            
        # Create execution quality data
        execution_quality = []
        plan_adherence = []
        pnl = []
        
        for daily in self.metrics.daily_metrics:
            for trade in daily.trades:
                execution_quality.append(trade.execution_quality * 100)
                plan_adherence.append(trade.plan_adherence * 100)
                pnl.append(trade.pnl)
                
        # Create figure
        fig = go.Figure()
        
        # Add execution quality
        fig.add_trace(go.Box(
            y=execution_quality,
            name='Execution Quality',
            marker_color='blue'
        ))
        
        # Add plan adherence
        fig.add_trace(go.Box(
            y=plan_adherence,
            name='Plan Adherence',
            marker_color='green'
        ))
        
        # Update layout
        fig.update_layout(
            title='Execution Quality Metrics',
            yaxis_title='Score %',
            showlegend=True,
            hovermode='x unified'
        )
        
        return fig
        
    def create_performance_dashboard(self) -> Optional[Figure]:
        """Create a comprehensive performance dashboard"""
        if not PLOTLY_AVAILABLE:
            print("Plotly not available. Cannot create performance dashboard.")
            return None
            
        # Create subplots
        fig = make_subplots(
            rows=3, cols=2,
            subplot_titles=(
                'Equity Curve', 'Daily Returns',
                'Win Rate by Symbol', 'P&L by Symbol',
                'Strategy Performance', 'Trade Duration'
            )
        )
        
        # Add equity curve
        equity_fig = self.plot_equity_curve()
        if equity_fig is not None:
            for trace in equity_fig.data:
                fig.add_trace(trace, row=1, col=1)
            
        # Add daily returns
        returns_fig = self.plot_daily_returns()
        if returns_fig is not None:
            for trace in returns_fig.data:
                fig.add_trace(trace, row=1, col=2)
            
        # Add win rate by symbol
        win_rate_fig = self.plot_win_rate_by_symbol()
        if win_rate_fig is not None:
            for trace in win_rate_fig.data:
                fig.add_trace(trace, row=2, col=1)
            
        # Add P&L by symbol
        pnl_fig = self.plot_pnl_by_symbol()
        if pnl_fig is not None:
            for trace in pnl_fig.data:
                fig.add_trace(trace, row=2, col=2)
            
        # Add strategy performance
        strategy_fig = self.plot_strategy_performance()
        if strategy_fig is not None:
            for trace in strategy_fig.data:
                fig.add_trace(trace, row=3, col=1)
            
        # Add trade duration
        duration_fig = self.plot_trade_duration()
        if duration_fig is not None:
            for trace in duration_fig.data:
                fig.add_trace(trace, row=3, col=2)
            
        # Update layout
        fig.update_layout(
            title='Trading Performance Dashboard',
            height=1200,
            showlegend=True,
            hovermode='x unified'
        )
        
        return fig 