from datetime import datetime, timedelta
from typing import Dict, List, Optional

from pydantic import BaseModel

from data.schemas import OrderSchema, PositionSchema, TradePlanSchema


class TradeMetrics(BaseModel):
    """Metrics for a single trade"""

    symbol: str
    entry_price: float
    exit_price: float
    quantity: float
    pnl: float
    pnl_percent: float
    holding_time: timedelta
    entry_time: datetime
    exit_time: datetime
    win: bool
    risk_reward_ratio: float
    max_drawdown: float
    max_profit: float
    execution_quality: float  # 0-1 score for execution quality
    plan_adherence: float  # 0-1 score for following the plan


class DailyMetrics(BaseModel):
    """Metrics for a single trading day"""

    date: datetime
    total_trades: int
    winning_trades: int
    losing_trades: int
    win_rate: float
    total_pnl: float
    total_pnl_percent: float
    average_win: float
    average_loss: float
    profit_factor: float
    max_drawdown: float
    sharpe_ratio: float
    sortino_ratio: float
    trades: List[TradeMetrics]


class PerformanceMetrics(BaseModel):
    """Overall performance metrics"""

    start_date: datetime
    end_date: datetime
    total_trades: int
    winning_trades: int
    losing_trades: int
    win_rate: float
    total_pnl: float
    total_pnl_percent: float
    average_win: float
    average_loss: float
    profit_factor: float
    max_drawdown: float
    sharpe_ratio: float
    sortino_ratio: float
    daily_metrics: List[DailyMetrics]
    symbol_metrics: Dict[str, Dict[str, float]]  # Metrics by symbol
    strategy_metrics: Dict[str, Dict[str, float]]  # Metrics by strategy


class PerformanceAnalyzer:
    """Analyzer for trading performance"""

    def __init__(self):
        self.trades: List[TradeMetrics] = []
        self.daily_metrics: Dict[datetime, DailyMetrics] = {}
        self.overall_metrics: Optional[PerformanceMetrics] = None

    def add_trade(
        self, plan: TradePlanSchema, position: PositionSchema, orders: List[OrderSchema]
    ) -> None:
        """Add a completed trade to the analysis"""
        # Find entry and exit orders
        entry_order = next((o for o in orders if o.side == "buy"), None)
        exit_order = next((o for o in orders if o.side == "sell"), None)

        if not entry_order or not exit_order:
            return

        # Calculate trade metrics
        entry_price = entry_order.metadata.get("execution_price", entry_order.price)
        exit_price = exit_order.metadata.get("execution_price", exit_order.price)
        quantity = position.quantity
        pnl = position.pnl
        pnl_percent = position.pnl_percent
        holding_time = exit_order.timestamp - entry_order.timestamp
        win = pnl > 0

        # Calculate risk-reward ratio
        risk = abs(entry_price - plan.stop_loss)
        reward = abs(plan.take_profit - entry_price)
        risk_reward_ratio = reward / risk if risk > 0 else 0

        # Calculate execution quality
        execution_quality = self._calculate_execution_quality(entry_order, exit_order, plan)

        # Calculate plan adherence
        plan_adherence = self._calculate_plan_adherence(position, plan, entry_order, exit_order)

        # Create trade metrics
        trade = TradeMetrics(
            symbol=position.symbol,
            entry_price=entry_price,
            exit_price=exit_price,
            quantity=quantity,
            pnl=pnl,
            pnl_percent=pnl_percent,
            holding_time=holding_time,
            entry_time=entry_order.timestamp,
            exit_time=exit_order.timestamp,
            win=win,
            risk_reward_ratio=risk_reward_ratio,
            max_drawdown=self._calculate_max_drawdown(position, orders),
            max_profit=self._calculate_max_profit(position, orders),
            execution_quality=execution_quality,
            plan_adherence=plan_adherence,
        )

        # Add trade to list
        self.trades.append(trade)

        # Update daily metrics
        self._update_daily_metrics(trade)

        # Update overall metrics
        self._update_overall_metrics()

    def get_daily_metrics(self, date: datetime) -> Optional[DailyMetrics]:
        """Get metrics for a specific day"""
        return self.daily_metrics.get(date)

    def get_overall_metrics(self) -> Optional[PerformanceMetrics]:
        """Get overall performance metrics"""
        return self.overall_metrics

    def get_symbol_metrics(self, symbol: str) -> Dict[str, float]:
        """Get metrics for a specific symbol"""
        if not self.overall_metrics:
            return {}

        return self.overall_metrics.symbol_metrics.get(symbol, {})

    def get_strategy_metrics(self, strategy: str) -> Dict[str, float]:
        """Get metrics for a specific strategy"""
        if not self.overall_metrics:
            return {}

        return self.overall_metrics.strategy_metrics.get(strategy, {})

    def _calculate_execution_quality(
        self, entry_order: OrderSchema, exit_order: OrderSchema, plan: TradePlanSchema
    ) -> float:
        """Calculate execution quality score"""
        score = 1.0

        # Check entry execution
        if entry_order.order_type == "market":
            # Market orders should be executed quickly
            execution_time = entry_order.metadata.get("executed_at", entry_order.timestamp)
            if (execution_time - entry_order.timestamp).total_seconds() > 1:
                score *= 0.9
        else:
            # Limit orders should be executed at or better than limit price
            execution_price = entry_order.metadata.get("execution_price", entry_order.price)
            if entry_order.side == "buy" and execution_price > entry_order.price:
                score *= 0.8
            elif entry_order.side == "sell" and execution_price < entry_order.price:
                score *= 0.8

        # Check exit execution
        if exit_order.order_type == "market":
            execution_time = exit_order.metadata.get("executed_at", exit_order.timestamp)
            if (execution_time - exit_order.timestamp).total_seconds() > 1:
                score *= 0.9
        else:
            execution_price = exit_order.metadata.get("execution_price", exit_order.price)
            if exit_order.side == "buy" and execution_price > exit_order.price:
                score *= 0.8
            elif exit_order.side == "sell" and execution_price < exit_order.price:
                score *= 0.8

        return score

    def _calculate_plan_adherence(
        self,
        position: PositionSchema,
        plan: TradePlanSchema,
        entry_order: OrderSchema,
        exit_order: OrderSchema,
    ) -> float:
        """Calculate plan adherence score"""
        score = 1.0

        # Check entry price
        if plan.entry_price:
            entry_price = entry_order.metadata.get("execution_price", entry_order.price)
            price_diff = abs(entry_price - plan.entry_price) / plan.entry_price
            if price_diff > 0.01:  # More than 1% deviation
                score *= 0.9

        # Check stop loss
        if plan.stop_loss:
            if position.stop_loss != plan.stop_loss:
                score *= 0.8

        # Check take profit
        if plan.take_profit:
            if position.take_profit != plan.take_profit:
                score *= 0.8

        # Check position size
        if plan.quantity:
            size_diff = abs(position.quantity - plan.quantity) / plan.quantity
            if size_diff > 0.1:  # More than 10% deviation
                score *= 0.9

        return score

    def _calculate_max_drawdown(self, position: PositionSchema, orders: List[OrderSchema]) -> float:
        """Calculate maximum drawdown for a trade"""
        if not orders:
            return 0.0

        entry_price = orders[0].metadata.get("execution_price", orders[0].price)
        max_drawdown = 0.0

        for order in orders:
            price = order.metadata.get("execution_price", order.price)
            drawdown = (entry_price - price) / entry_price
            max_drawdown = max(max_drawdown, drawdown)

        return max_drawdown

    def _calculate_max_profit(self, position: PositionSchema, orders: List[OrderSchema]) -> float:
        """Calculate maximum profit for a trade"""
        if not orders:
            return 0.0

        entry_price = orders[0].metadata.get("execution_price", orders[0].price)
        max_profit = 0.0

        for order in orders:
            price = order.metadata.get("execution_price", order.price)
            profit = (price - entry_price) / entry_price
            max_profit = max(max_profit, profit)

        return max_profit

    def _update_daily_metrics(self, trade: TradeMetrics) -> None:
        """Update daily metrics with a new trade"""
        date = trade.entry_time.date()

        if date not in self.daily_metrics:
            self.daily_metrics[date] = DailyMetrics(
                date=date,
                total_trades=0,
                winning_trades=0,
                losing_trades=0,
                win_rate=0.0,
                total_pnl=0.0,
                total_pnl_percent=0.0,
                average_win=0.0,
                average_loss=0.0,
                profit_factor=0.0,
                max_drawdown=0.0,
                sharpe_ratio=0.0,
                sortino_ratio=0.0,
                trades=[],
            )

        metrics = self.daily_metrics[date]
        metrics.trades.append(trade)
        metrics.total_trades += 1

        if trade.win:
            metrics.winning_trades += 1
        else:
            metrics.losing_trades += 1

        metrics.total_pnl += trade.pnl
        metrics.total_pnl_percent += trade.pnl_percent
        metrics.win_rate = metrics.winning_trades / metrics.total_trades

        # Calculate averages
        winning_trades = [t for t in metrics.trades if t.win]
        losing_trades = [t for t in metrics.trades if not t.win]

        if winning_trades:
            metrics.average_win = sum(t.pnl for t in winning_trades) / len(winning_trades)
        if losing_trades:
            metrics.average_loss = sum(t.pnl for t in losing_trades) / len(losing_trades)

        # Calculate profit factor
        if metrics.average_loss != 0:
            metrics.profit_factor = abs(metrics.average_win / metrics.average_loss)

        # Calculate max drawdown
        metrics.max_drawdown = max(t.max_drawdown for t in metrics.trades)

        # Calculate Sharpe ratio (assuming risk-free rate of 0)
        if len(metrics.trades) > 1:
            returns = [t.pnl_percent for t in metrics.trades]
            metrics.sharpe_ratio = (
                sum(returns)
                / len(returns)
                / (sum((r - sum(returns) / len(returns)) ** 2 for r in returns) / len(returns))
                ** 0.5
            )

        # Calculate Sortino ratio
        if len(metrics.trades) > 1:
            returns = [t.pnl_percent for t in metrics.trades]
            downside_returns = [r for r in returns if r < 0]
            if downside_returns:
                metrics.sortino_ratio = (
                    sum(returns)
                    / len(returns)
                    / (sum(r**2 for r in downside_returns) / len(downside_returns)) ** 0.5
                )

    def _update_overall_metrics(self) -> None:
        """Update overall performance metrics"""
        if not self.trades:
            return

        # Calculate date range
        start_date = min(t.entry_time for t in self.trades)
        end_date = max(t.exit_time for t in self.trades)

        # Calculate basic metrics
        total_trades = len(self.trades)
        winning_trades = sum(1 for t in self.trades if t.win)
        losing_trades = total_trades - winning_trades
        win_rate = winning_trades / total_trades if total_trades > 0 else 0

        total_pnl = sum(t.pnl for t in self.trades)
        total_pnl_percent = sum(t.pnl_percent for t in self.trades)

        # Calculate averages
        winning_trades = [t for t in self.trades if t.win]
        losing_trades = [t for t in self.trades if not t.win]

        average_win = (
            sum(t.pnl for t in winning_trades) / len(winning_trades) if winning_trades else 0
        )
        average_loss = (
            sum(t.pnl for t in losing_trades) / len(losing_trades) if losing_trades else 0
        )

        # Calculate profit factor
        profit_factor = abs(average_win / average_loss) if average_loss != 0 else 0

        # Calculate max drawdown
        max_drawdown = max(t.max_drawdown for t in self.trades)

        # Calculate Sharpe ratio
        if len(self.trades) > 1:
            returns = [t.pnl_percent for t in self.trades]
            sharpe_ratio = (
                sum(returns)
                / len(returns)
                / (sum((r - sum(returns) / len(returns)) ** 2 for r in returns) / len(returns))
                ** 0.5
            )
        else:
            sharpe_ratio = 0

        # Calculate Sortino ratio
        if len(self.trades) > 1:
            returns = [t.pnl_percent for t in self.trades]
            downside_returns = [r for r in returns if r < 0]
            if downside_returns:
                sortino_ratio = (
                    sum(returns)
                    / len(returns)
                    / (sum(r**2 for r in downside_returns) / len(downside_returns)) ** 0.5
                )
            else:
                sortino_ratio = 0
        else:
            sortino_ratio = 0

        # Calculate metrics by symbol
        symbol_metrics = {}
        for symbol in set(t.symbol for t in self.trades):
            symbol_trades = [t for t in self.trades if t.symbol == symbol]
            symbol_metrics[symbol] = {
                "total_trades": len(symbol_trades),
                "winning_trades": sum(1 for t in symbol_trades if t.win),
                "win_rate": sum(1 for t in symbol_trades if t.win) / len(symbol_trades),
                "total_pnl": sum(t.pnl for t in symbol_trades),
                "total_pnl_percent": sum(t.pnl_percent for t in symbol_trades),
                "average_win": (
                    sum(t.pnl for t in symbol_trades if t.win)
                    / len([t for t in symbol_trades if t.win])
                    if any(t.win for t in symbol_trades)
                    else 0
                ),
                "average_loss": (
                    sum(t.pnl for t in symbol_trades if not t.win)
                    / len([t for t in symbol_trades if not t.win])
                    if any(not t.win for t in symbol_trades)
                    else 0
                ),
                "profit_factor": (
                    abs(
                        sum(t.pnl for t in symbol_trades if t.win)
                        / sum(t.pnl for t in symbol_trades if not t.win)
                    )
                    if sum(t.pnl for t in symbol_trades if not t.win) != 0
                    else 0
                ),
            }

        # Calculate metrics by strategy
        strategy_metrics = {}
        for strategy in set(t.plan.strategy for t in self.trades if hasattr(t, "plan")):
            strategy_trades = [
                t for t in self.trades if hasattr(t, "plan") and t.plan.strategy == strategy
            ]
            strategy_metrics[strategy] = {
                "total_trades": len(strategy_trades),
                "winning_trades": sum(1 for t in strategy_trades if t.win),
                "win_rate": sum(1 for t in strategy_trades if t.win) / len(strategy_trades),
                "total_pnl": sum(t.pnl for t in strategy_trades),
                "total_pnl_percent": sum(t.pnl_percent for t in strategy_trades),
                "average_win": (
                    sum(t.pnl for t in strategy_trades if t.win)
                    / len([t for t in strategy_trades if t.win])
                    if any(t.win for t in strategy_trades)
                    else 0
                ),
                "average_loss": (
                    sum(t.pnl for t in strategy_trades if not t.win)
                    / len([t for t in strategy_trades if not t.win])
                    if any(not t.win for t in strategy_trades)
                    else 0
                ),
                "profit_factor": (
                    abs(
                        sum(t.pnl for t in strategy_trades if t.win)
                        / sum(t.pnl for t in strategy_trades if not t.win)
                    )
                    if sum(t.pnl for t in strategy_trades if not t.win) != 0
                    else 0
                ),
            }

        # Create overall metrics
        self.overall_metrics = PerformanceMetrics(
            start_date=start_date,
            end_date=end_date,
            total_trades=total_trades,
            winning_trades=winning_trades,
            losing_trades=losing_trades,
            win_rate=win_rate,
            total_pnl=total_pnl,
            total_pnl_percent=total_pnl_percent,
            average_win=average_win,
            average_loss=average_loss,
            profit_factor=profit_factor,
            max_drawdown=max_drawdown,
            sharpe_ratio=sharpe_ratio,
            sortino_ratio=sortino_ratio,
            daily_metrics=list(self.daily_metrics.values()),
            symbol_metrics=symbol_metrics,
            strategy_metrics=strategy_metrics,
        )
