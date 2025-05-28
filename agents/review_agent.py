from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional

from pydantic import BaseModel

from brokers.test_broker import TestBroker
from data.storage import Storage


class TradeMetrics(BaseModel):
    total_trades: int
    winning_trades: int
    losing_trades: int
    win_rate: float
    average_win: float
    average_loss: float
    profit_factor: float
    largest_win: float
    largest_loss: float
    average_hold_time: timedelta
    total_pnl: float


class ReviewAgent:
    def __init__(self):
        self.storage = Storage()
        self.broker = TestBroker()

    def execute(self, **kwargs) -> Dict[str, Any]:
        """
        Analyzes completed trades and generates performance metrics.
        """
        try:
            # Get review parameters
            start_time = kwargs.get("start_time", datetime.now() - timedelta(days=30))
            end_time = kwargs.get("end_time", datetime.now())
            symbol = kwargs.get("symbol")

            # Get trade history
            trades = self._get_trade_history(start_time, end_time, symbol)
            if not trades:
                return {
                    "status": "error",
                    "message": "No trades found for the specified period",
                }

            # Calculate metrics
            metrics = self._calculate_metrics(trades)

            # Analyze execution
            execution_analysis = self._analyze_execution(trades)

            # Analyze management
            management_analysis = self._analyze_management(trades)

            return {
                "status": "success",
                "message": "Trade review completed",
                "data": {
                    "metrics": metrics.model_dump(),
                    "execution_analysis": execution_analysis,
                    "management_analysis": management_analysis,
                    "trades": trades,
                },
            }

        except Exception as e:
            return {"status": "error", "message": f"Review failed: {str(e)}"}

    def _get_trade_history(
        self, start_time: datetime, end_time: datetime, symbol: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Get trade history for the specified period.
        """
        trades = self.storage.get_history("trades", start_time, end_time)
        if symbol:
            trades = [t for t in trades if t["symbol"] == symbol]
        return trades

    def _calculate_metrics(self, trades: List[Dict[str, Any]]) -> TradeMetrics:
        """
        Calculate performance metrics from trades.
        """
        total_trades = len(trades)
        winning_trades = [t for t in trades if t["pnl"] > 0]
        losing_trades = [t for t in trades if t["pnl"] < 0]

        win_rate = len(winning_trades) / total_trades if total_trades > 0 else 0

        average_win = (
            sum(t["pnl"] for t in winning_trades) / len(winning_trades) if winning_trades else 0
        )
        average_loss = (
            sum(t["pnl"] for t in losing_trades) / len(losing_trades) if losing_trades else 0
        )

        total_profit = sum(t["pnl"] for t in winning_trades)
        total_loss = abs(sum(t["pnl"] for t in losing_trades))
        profit_factor = total_profit / total_loss if total_loss > 0 else float("inf")

        largest_win = max((t["pnl"] for t in winning_trades), default=0)
        largest_loss = min((t["pnl"] for t in losing_trades), default=0)

        # Calculate average hold time
        hold_times = []
        for trade in trades:
            entry_time = datetime.fromisoformat(trade["order"]["timestamp"])
            exit_time = datetime.fromisoformat(trade.get("exit_time", datetime.now().isoformat()))
            hold_times.append(exit_time - entry_time)
        average_hold_time = (
            sum(hold_times, timedelta()) / len(hold_times) if hold_times else timedelta()
        )

        total_pnl = sum(t["pnl"] for t in trades)

        return TradeMetrics(
            total_trades=total_trades,
            winning_trades=len(winning_trades),
            losing_trades=len(losing_trades),
            win_rate=win_rate,
            average_win=average_win,
            average_loss=average_loss,
            profit_factor=profit_factor,
            largest_win=largest_win,
            largest_loss=largest_loss,
            average_hold_time=average_hold_time,
            total_pnl=total_pnl,
        )

    def _analyze_execution(self, trades: List[Dict[str, Any]]) -> Dict[str, Any]:
        """
        Analyze trade execution quality.
        """
        execution_quality = {
            "slippage": [],
            "fill_prices": [],
            "order_types": {},
            "time_to_fill": [],
        }

        for trade in trades:
            order = trade["order"]

            # Calculate slippage
            if order["order_type"] == "market":
                expected_price = order.get("price", 0)
                actual_price = trade["exit_price"]
                slippage = (
                    abs(actual_price - expected_price) / expected_price if expected_price > 0 else 0
                )
                execution_quality["slippage"].append(slippage)

            # Track fill prices
            execution_quality["fill_prices"].append(trade["exit_price"])

            # Count order types
            order_type = order["order_type"]
            execution_quality["order_types"][order_type] = (
                execution_quality["order_types"].get(order_type, 0) + 1
            )

            # Calculate time to fill
            entry_time = datetime.fromisoformat(order["timestamp"])
            exit_time = datetime.fromisoformat(trade.get("exit_time", datetime.now().isoformat()))
            execution_quality["time_to_fill"].append((exit_time - entry_time).total_seconds())

        # Calculate averages
        if execution_quality["slippage"]:
            execution_quality["average_slippage"] = sum(execution_quality["slippage"]) / len(
                execution_quality["slippage"]
            )
        if execution_quality["time_to_fill"]:
            execution_quality["average_time_to_fill"] = sum(
                execution_quality["time_to_fill"]
            ) / len(execution_quality["time_to_fill"])

        return execution_quality

    def _analyze_management(self, trades: List[Dict[str, Any]]) -> Dict[str, Any]:
        """
        Analyze trade management effectiveness.
        """
        management_quality = {
            "stop_loss_hits": 0,
            "take_profit_hits": 0,
            "scaling_effectiveness": [],
            "adjustment_effectiveness": [],
        }

        for trade in trades:
            # Check if stop loss or take profit was hit
            if trade.get("exit_reason") == "stop_loss":
                management_quality["stop_loss_hits"] += 1
            elif trade.get("exit_reason") == "take_profit":
                management_quality["take_profit_hits"] += 1

            # Analyze scaling effectiveness
            scales = self.storage.get_history(
                "scales",
                start_time=datetime.fromisoformat(trade["order"]["timestamp"]),
                end_time=datetime.fromisoformat(trade.get("exit_time", datetime.now().isoformat())),
            )

            for scale in scales:
                if scale["position_id"] == trade["position_id"]:
                    pre_scale_pnl = scale.get("pre_scale_pnl", 0)
                    post_scale_pnl = scale.get("post_scale_pnl", 0)
                    if pre_scale_pnl and post_scale_pnl:
                        effectiveness = (post_scale_pnl - pre_scale_pnl) / pre_scale_pnl
                        management_quality["scaling_effectiveness"].append(effectiveness)

            # Analyze adjustment effectiveness
            adjustments = self.storage.get_history(
                "adjustments",
                start_time=datetime.fromisoformat(trade["order"]["timestamp"]),
                end_time=datetime.fromisoformat(trade.get("exit_time", datetime.now().isoformat())),
            )

            for adj in adjustments:
                if adj["position_id"] == trade["position_id"]:
                    pre_adj_pnl = adj.get("pre_adjustment_pnl", 0)
                    post_adj_pnl = adj.get("post_adjustment_pnl", 0)
                    if pre_adj_pnl and post_adj_pnl:
                        effectiveness = (post_adj_pnl - pre_adj_pnl) / pre_adj_pnl
                        management_quality["adjustment_effectiveness"].append(effectiveness)

        # Calculate averages
        if management_quality["scaling_effectiveness"]:
            management_quality["average_scaling_effectiveness"] = sum(
                management_quality["scaling_effectiveness"]
            ) / len(management_quality["scaling_effectiveness"])
        if management_quality["adjustment_effectiveness"]:
            management_quality["average_adjustment_effectiveness"] = sum(
                management_quality["adjustment_effectiveness"]
            ) / len(management_quality["adjustment_effectiveness"])

        return management_quality
