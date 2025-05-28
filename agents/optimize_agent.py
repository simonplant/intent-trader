from datetime import datetime

from typing import Any, Dict, List, Optional

from pydantic import BaseModel

from agents.coach_agent import CoachAgent
from agents.execute_agent import ExecuteAgent
from agents.focus_agent import FocusAgent
from agents.manage_agent import ManageAgent
from agents.market_data_agent import MarketDataAgent
from agents.plan_agent import PlanAgent
from agents.review_agent import ReviewAgent
from data.storage import Storage


class OptimizationParameters(BaseModel):
    capital: float
    max_daily_risk: float
    max_position_size: float
    max_drawdown: float
    target_daily_pnl: float
    market_conditions: Dict[str, Any]
    trading_style: str  # "day" or "swing"
    risk_tolerance: float  # 0-1 scale


class OptimizationResult(BaseModel):
    optimal_position_size: float
    optimal_entry_price: float
    optimal_stop_loss: float
    optimal_take_profit: float
    risk_reward_ratio: float
    expected_pnl: float
    confidence_score: float
    execution_priority: int
    notes: str = ""


class OptimizeAgent:
    def __init__(self):
        self.storage = Storage()
        self.market_data_agent = MarketDataAgent()
        self.plan_agent = PlanAgent()
        self.focus_agent = FocusAgent()
        self.execute_agent = ExecuteAgent()
        self.manage_agent = ManageAgent()
        self.review_agent = ReviewAgent()
        self.coach_agent = CoachAgent()

        # Default optimization parameters
        self.default_params = OptimizationParameters(
            capital=100000,  # $100K initial capital
            max_daily_risk=0.02,  # 2% max daily risk
            max_position_size=0.1,  # 10% max position size
            max_drawdown=0.05,  # 5% max drawdown
            target_daily_pnl=0.01,  # 1% target daily return
            market_conditions={},
            trading_style="day",
            risk_tolerance=0.5,
        )

    def execute(self, **kwargs) -> Dict[str, Any]:
        """
        Optimizes trading decisions to maximize daily P&L.
        """
        try:
            # Get optimization parameters
            params = self._get_optimization_parameters(kwargs)

            # Get current market data
            market_data = self._get_market_data()

            # Get active plans and focus areas
            plans = self._get_active_plans()
            focus_areas = self._get_focus_areas()

            # Get current positions and P&L
            positions = self._get_current_positions()
            daily_pnl = self._calculate_daily_pnl()

            # Check risk limits
            risk_check = self._check_risk_limits(params, positions, daily_pnl)
            if not risk_check["status"] == "success":
                return risk_check

            # Optimize each potential trade
            optimization_results = []
            for plan in plans:
                if self._is_plan_valid(plan, focus_areas, market_data):
                    result = self._optimize_trade(plan, params, market_data)
                    if result:
                        optimization_results.append(result)

            # Sort by execution priority
            optimization_results.sort(key=lambda x: x.execution_priority)

            return {
                "status": "success",
                "message": "Optimization completed",
                "data": {
                    "optimization_results": [result.model_dump() for result in optimization_results],
                    "market_conditions": market_data,
                    "daily_pnl": daily_pnl,
                    "risk_metrics": self._calculate_risk_metrics(params, positions),
                },
            }

        except Exception as e:
            return {"status": "error", "message": f"Optimization failed: {str(e)}"}

    def _get_optimization_parameters(self, kwargs: Dict[str, Any]) -> OptimizationParameters:
        """
        Get optimization parameters from kwargs or use defaults.
        """
        params = self.default_params.model_dump()
        params.update(kwargs)
        return OptimizationParameters(**params)

    def _get_market_data(self) -> Dict[str, Any]:
        """
        Get current market data and conditions.
        """
        result = self.market_data_agent.execute()
        if result["status"] != "success":
            raise Exception(f"Failed to get market data: {result['message']}")
        return result["data"]

    def _get_active_plans(self) -> List[Dict[str, Any]]:
        """
        Get active trading plans.
        """
        plans = self.storage.get_active_plans()
        return [plan for plan in plans if self._is_plan_current(plan)]

    def _get_focus_areas(self) -> List[Dict[str, Any]]:
        """
        Get current focus areas.
        """
        result = self.focus_agent.execute()
        if result["status"] != "success":
            raise Exception(f"Failed to get focus areas: {result['message']}")
        return result["data"]["focus_areas"]

    def _get_current_positions(self) -> List[Dict[str, Any]]:
        """
        Get current open positions.
        """
        return self.storage.list_positions()

    def _calculate_daily_pnl(self) -> float:
        """
        Calculate today's P&L.
        """
        today = datetime.now().date()
        trades = self.storage.get_history(
            "trades",
            start_time=datetime.combine(today, datetime.min.time()),
            end_time=datetime.now(),
        )
        return sum(trade["pnl"] for trade in trades)

    def _check_risk_limits(
        self,
        params: OptimizationParameters,
        positions: List[Dict[str, Any]],
        daily_pnl: float,
    ) -> Dict[str, Any]:
        """
        Check if current positions and P&L are within risk limits.
        """
        # Check daily drawdown
        if daily_pnl < -params.capital * params.max_drawdown:
            return {"status": "error", "message": "Daily drawdown limit reached"}

        # Check position concentration
        total_position_value = sum(pos["quantity"] * pos["current_price"] for pos in positions)
        if total_position_value > params.capital * params.max_position_size:
            return {"status": "error", "message": "Position size limit reached"}

        return {"status": "success"}

    def _is_plan_valid(
        self,
        plan: Dict[str, Any],
        focus_areas: List[Dict[str, Any]],
        market_data: Dict[str, Any],
    ) -> bool:
        """
        Check if a plan is valid given current market conditions and focus areas.
        """
        # Check if plan symbol is in focus areas
        if not any(area["symbol"] == plan["symbol"] for area in focus_areas):
            return False

        # Check if plan is still valid based on market conditions
        symbol_data = market_data.get(plan["symbol"])
        if not symbol_data:
            return False

        # Check if price is within plan's expected range
        current_price = symbol_data["price"]
        if not self._is_price_in_range(current_price, plan.get("price_range", {})):
            return False

        return True

    def _optimize_trade(
        self,
        plan: Dict[str, Any],
        params: OptimizationParameters,
        market_data: Dict[str, Any],
    ) -> Optional[OptimizationResult]:
        """
        Optimize a single trade based on the plan and current conditions.
        """
        symbol_data = market_data[plan["symbol"]]
        current_price = symbol_data["price"]

        # Calculate optimal position size
        position_size = self._calculate_position_size(plan, params, current_price)

        # Calculate optimal entry price
        entry_price = self._calculate_entry_price(plan, current_price, symbol_data)

        # Calculate optimal stop loss and take profit
        stop_loss, take_profit = self._calculate_exit_levels(plan, entry_price, symbol_data)

        # Calculate risk-reward ratio
        risk = abs(entry_price - stop_loss)
        reward = abs(take_profit - entry_price)
        risk_reward = reward / risk if risk > 0 else 0

        # Calculate expected P&L
        expected_pnl = self._calculate_expected_pnl(position_size, risk_reward, params)

        # Calculate confidence score
        confidence = self._calculate_confidence_score(plan, market_data, params)

        # Calculate execution priority
        priority = self._calculate_execution_priority(confidence, risk_reward, expected_pnl)

        # Generate optimization notes
        notes = self._generate_optimization_notes(plan, market_data)

        return OptimizationResult(
            optimal_position_size=position_size,
            optimal_entry_price=entry_price,
            optimal_stop_loss=stop_loss,
            optimal_take_profit=take_profit,
            risk_reward_ratio=risk_reward,
            expected_pnl=expected_pnl,
            confidence_score=confidence,
            execution_priority=priority,
            notes=notes,
        )

    def _calculate_position_size(
        self, plan: Dict[str, Any], params: OptimizationParameters, current_price: float
    ) -> float:
        """
        Calculate optimal position size based on risk parameters.
        """
        # Get risk per trade from plan or use default
        risk_per_trade = plan.get("risk_parameters", {}).get(
            "risk_per_trade", params.max_daily_risk * params.capital
        )

        # Calculate position size based on risk
        stop_loss = plan.get("risk_parameters", {}).get("stop_loss")
        if stop_loss:
            risk_per_share = abs(current_price - stop_loss)
            position_size = risk_per_trade / risk_per_share
        else:
            # Use default position sizing
            position_size = params.capital * params.max_position_size / current_price

        # Apply position size limits
        max_size = params.capital * params.max_position_size / current_price
        return min(position_size, max_size)

    def _calculate_entry_price(
        self, plan: Dict[str, Any], current_price: float, market_data: Dict[str, Any]
    ) -> float:
        """
        Calculate optimal entry price based on plan and market conditions.
        """
        # Use plan's entry price if specified
        if "entry_price" in plan:
            return plan["entry_price"]

        # Calculate based on market conditions
        if plan["side"] == "long":
            return min(current_price, market_data["high"])
        else:
            return max(current_price, market_data["low"])

    def _calculate_exit_levels(
        self, plan: Dict[str, Any], entry_price: float, market_data: Dict[str, Any]
    ) -> tuple[float, float]:
        """
        Calculate optimal stop loss and take profit levels.
        """
        # Use plan's levels if specified
        if "stop_loss" in plan and "take_profit" in plan:
            return plan["stop_loss"], plan["take_profit"]

        # Calculate based on market volatility
        atr = market_data.get("atr", 0)
        if plan["side"] == "long":
            stop_loss = entry_price - (2 * atr)
            take_profit = entry_price + (3 * atr)
        else:
            stop_loss = entry_price + (2 * atr)
            take_profit = entry_price - (3 * atr)

        return stop_loss, take_profit

    def _calculate_expected_pnl(
        self, position_size: float, risk_reward: float, params: OptimizationParameters
    ) -> float:
        """
        Calculate expected P&L for the trade.
        """
        return position_size * risk_reward * params.capital * params.max_daily_risk

    def _calculate_confidence_score(
        self,
        plan: Dict[str, Any],
        market_data: Dict[str, Any],
        params: OptimizationParameters,
    ) -> float:
        """
        Calculate confidence score for the trade.
        """
        # Start with plan's conviction score
        confidence = plan.get("conviction_score", 0.5)

        # Adjust based on market conditions
        if params.trading_style == "day" and market_data.get("volatility", 0) > 0.02:
            confidence *= 0.8  # Reduce confidence in high volatility

        # Adjust based on risk tolerance
        confidence *= params.risk_tolerance

        return min(confidence, 1.0)

    def _calculate_execution_priority(
        self, confidence: float, risk_reward: float, expected_pnl: float
    ) -> int:
        """
        Calculate execution priority for the trade.
        """
        # Higher priority for high confidence, high risk-reward trades
        priority = int((confidence * 0.4 + risk_reward * 0.3 + expected_pnl * 0.3) * 100)
        return max(1, min(priority, 100))

    def _generate_optimization_notes(
        self, plan: Dict[str, Any], market_data: Dict[str, Any]
    ) -> str:
        """
        Generate optimization notes for the trade.
        """
        notes = []

        # Add plan notes
        if "notes" in plan:
            notes.append(f"Plan notes: {plan['notes']}")

        # Add market condition notes
        if market_data.get("volatility", 0) > 0.02:
            notes.append("High volatility - consider tighter stops")
        if market_data.get("volume", 0) < 1000000:
            notes.append("Low volume - consider smaller position size")

        return " | ".join(notes)

    def _calculate_risk_metrics(
        self, params: OptimizationParameters, positions: List[Dict[str, Any]]
    ) -> Dict[str, Any]:
        """
        Calculate current risk metrics.
        """
        total_position_value = sum(pos["quantity"] * pos["current_price"] for pos in positions)
        total_risk = sum(
            pos["quantity"] * abs(pos["current_price"] - pos["stop_loss"])
            for pos in positions
            if pos["stop_loss"] is not None
        )

        return {
            "total_position_value": total_position_value,
            "total_risk": total_risk,
            "risk_percentage": total_risk / params.capital if params.capital > 0 else 0,
            "position_concentration": (
                total_position_value / params.capital if params.capital > 0 else 0
            ),
        }
