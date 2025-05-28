from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional

from pydantic import BaseModel


class PositionSide(str, Enum):
    LONG = "long"
    SHORT = "short"


class Position(BaseModel):
    position_id: str
    symbol: str
    side: PositionSide
    quantity: float
    entry_price: float
    current_price: float
    timestamp: datetime
    unrealized_pnl: float = 0.0
    realized_pnl: float = 0.0
    stop_loss: Optional[float] = None
    take_profit: Optional[float] = None
    notes: str = ""


class PositionAgent:
    def __init__(self):
        self.positions: Dict[str, Position] = {}
        self.position_history: List[Position] = []

    def execute(self, **kwargs) -> Dict[str, Any]:
        """
        Handles position management operations.
        """
        action = kwargs.get("action", "list")

        if action == "open":
            return self._open_position(**kwargs)
        elif action == "close":
            return self._close_position(**kwargs)
        elif action == "update":
            return self._update_position(**kwargs)
        elif action == "list":
            return self._list_positions(**kwargs)
        else:
            return {"status": "error", "message": f"Unknown action: {action}"}

    def _open_position(self, **kwargs) -> Dict[str, Any]:
        """
        Opens a new position.
        """
        # TODO: Implement position opening logic
        position = Position(
            position_id="dummy_id",
            symbol=kwargs.get("symbol", ""),
            side=PositionSide.LONG,
            quantity=kwargs.get("quantity", 0.0),
            entry_price=kwargs.get("price", 0.0),
            current_price=kwargs.get("price", 0.0),
            timestamp=datetime.now(),
        )

        self.positions[position.position_id] = position
        return {
            "status": "success",
            "message": "Position opened",
            "data": {"position": position.model_dump()},
        }

    def _close_position(self, **kwargs) -> Dict[str, Any]:
        """
        Closes an existing position.
        """
        position_id = kwargs.get("position_id")
        if position_id in self.positions:
            position = self.positions[position_id]
            # Calculate P&L
            position.unrealized_pnl = self._calculate_pnl(position)
            self.position_history.append(position)
            del self.positions[position_id]
            return {
                "status": "success",
                "message": "Position closed",
                "data": {"position": position.model_dump()},
            }
        return {"status": "error", "message": f"Position not found: {position_id}"}

    def _update_position(self, **kwargs) -> Dict[str, Any]:
        """
        Updates position details (e.g., stop loss, take profit).
        """
        position_id = kwargs.get("position_id")
        if position_id in self.positions:
            position = self.positions[position_id]
            # Update position fields
            if "stop_loss" in kwargs:
                position.stop_loss = kwargs["stop_loss"]
            if "take_profit" in kwargs:
                position.take_profit = kwargs["take_profit"]
            if "current_price" in kwargs:
                position.current_price = kwargs["current_price"]
                position.unrealized_pnl = self._calculate_pnl(position)
            return {
                "status": "success",
                "message": "Position updated",
                "data": {"position": position.model_dump()},
            }
        return {"status": "error", "message": f"Position not found: {position_id}"}

    def _list_positions(self, **kwargs) -> Dict[str, Any]:
        """
        Lists all positions, optionally filtered by symbol.
        """
        symbol = kwargs.get("symbol")
        positions = [
            position.model_dump()
            for position in self.positions.values()
            if not symbol or position.symbol == symbol
        ]
        return {
            "status": "success",
            "message": "Positions listed",
            "data": {"positions": positions},
        }

    def _calculate_pnl(self, position: Position) -> float:
        """
        Calculates the P&L for a position.
        """
        if position.side == PositionSide.LONG:
            return (position.current_price - position.entry_price) * position.quantity
        else:
            return (position.entry_price - position.current_price) * position.quantity

    def update_prices(self, market_data: Dict[str, float]) -> None:
        """
        Updates all position prices based on market data.
        """
        for position in self.positions.values():
            if position.symbol in market_data:
                position.current_price = market_data[position.symbol]
                position.unrealized_pnl = self._calculate_pnl(position)
