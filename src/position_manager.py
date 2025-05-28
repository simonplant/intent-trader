"""
Position Manager for IAA Trading Assistant

Handles position tracking, risk calculations, and P&L management
from natural language inputs in a stateless manner.
"""

import re
from dataclasses import dataclass
from datetime import datetime
from typing import Dict, List, Optional


@dataclass
class Position:
    """Represents a trading position."""

    symbol: str
    side: str  # "long" or "short"
    quantity: int
    entry_price: float
    current_price: float = None
    stop_loss: float = None
    take_profit: float = None
    entry_time: datetime = None

    def __post_init__(self):
        if self.entry_time is None:
            self.entry_time = datetime.now()
        if self.current_price is None:
            self.current_price = self.entry_price

    @property
    def market_value(self) -> float:
        """Current market value of position."""
        return self.quantity * self.current_price

    @property
    def unrealized_pnl(self) -> float:
        """Unrealized P&L of position."""
        if self.side == "long":
            return self.quantity * (self.current_price - self.entry_price)
        else:  # short
            return self.quantity * (self.entry_price - self.current_price)

    @property
    def unrealized_pnl_percent(self) -> float:
        """Unrealized P&L as percentage."""
        cost_basis = self.quantity * self.entry_price
        return (self.unrealized_pnl / cost_basis) * 100 if cost_basis > 0 else 0

    def to_compressed(self) -> str:
        """Convert to compressed string format."""
        return f"{self.symbol}:{self.side[0].upper()}{self.quantity}@{self.entry_price:.2f}"

    @classmethod
    def from_compressed(cls, compressed: str) -> "Position":
        """Create position from compressed string."""
        # Format: AAPL:L100@225.50
        match = re.match(r"([A-Z]+):([LS])(\d+)@([\d.]+)", compressed)
        if match:
            symbol, side_char, quantity, price = match.groups()
            side = "long" if side_char == "L" else "short"
            return cls(
                symbol=symbol,
                side=side,
                quantity=int(quantity),
                entry_price=float(price),
            )
        raise ValueError(f"Invalid compressed position format: {compressed}")


class PositionManager:
    """Manages trading positions for the IAA."""

    def __init__(self):
        self.positions: Dict[str, Position] = {}
        self.closed_positions: List[Position] = []
        self.total_realized_pnl = 0.0

    def add_position(
        self,
        symbol: str,
        side: str,
        quantity: int,
        entry_price: float,
        stop_loss: float = None,
        take_profit: float = None,
    ) -> Position:
        """Add a new position."""
        position = Position(
            symbol=symbol,
            side=side.lower(),
            quantity=quantity,
            entry_price=entry_price,
            stop_loss=stop_loss,
            take_profit=take_profit,
        )

        # If position already exists, average in or add to it
        if symbol in self.positions:
            existing = self.positions[symbol]
            if existing.side == position.side:
                # Average the positions
                total_quantity = existing.quantity + position.quantity
                total_cost = (
                    existing.quantity * existing.entry_price
                    + position.quantity * position.entry_price
                )
                avg_price = total_cost / total_quantity

                existing.quantity = total_quantity
                existing.entry_price = avg_price
                return existing
            else:
                # Opposite sides - close out the smaller position
                if existing.quantity > position.quantity:
                    existing.quantity -= position.quantity
                    return existing
                elif existing.quantity < position.quantity:
                    position.quantity -= existing.quantity
                    del self.positions[symbol]
                    self.positions[symbol] = position
                    return position
                else:
                    # Equal quantities - close both
                    del self.positions[symbol]
                    return None

        self.positions[symbol] = position
        return position

    def close_position(self, symbol: str, exit_price: float = None) -> Optional[float]:
        """Close a position and return realized P&L."""
        if symbol not in self.positions:
            return None

        position = self.positions[symbol]
        if exit_price:
            position.current_price = exit_price

        realized_pnl = position.unrealized_pnl
        self.total_realized_pnl += realized_pnl

        # Move to closed positions
        self.closed_positions.append(position)
        del self.positions[symbol]

        return realized_pnl

    def update_price(self, symbol: str, current_price: float):
        """Update current price for a position."""
        if symbol in self.positions:
            self.positions[symbol].current_price = current_price

    def get_position(self, symbol: str) -> Optional[Position]:
        """Get position for a symbol."""
        return self.positions.get(symbol)

    def get_all_positions(self) -> List[Position]:
        """Get all active positions."""
        return list(self.positions.values())

    def get_total_unrealized_pnl(self) -> float:
        """Get total unrealized P&L across all positions."""
        return sum(pos.unrealized_pnl for pos in self.positions.values())

    def get_total_market_value(self) -> float:
        """Get total market value of all positions."""
        return sum(pos.market_value for pos in self.positions.values())

    def get_portfolio_summary(self) -> Dict:
        """Get portfolio summary."""
        return {
            "active_positions": len(self.positions),
            "total_market_value": self.get_total_market_value(),
            "unrealized_pnl": self.get_total_unrealized_pnl(),
            "realized_pnl": self.total_realized_pnl,
            "total_pnl": self.get_total_unrealized_pnl() + self.total_realized_pnl,
        }

    def to_compressed(self) -> str:
        """Convert all positions to compressed format."""
        pos_strings = [pos.to_compressed() for pos in self.positions.values()]
        return "|".join(pos_strings)

    def from_compressed(self, compressed: str):
        """Load positions from compressed format."""
        if not compressed:
            return

        self.positions.clear()
        for pos_str in compressed.split("|"):
            if pos_str:
                position = Position.from_compressed(pos_str)
                self.positions[position.symbol] = position

    def parse_position_from_text(self, text: str) -> Optional[Dict]:
        """Parse position information from natural language text."""
        text_lower = text.lower()

        # Extract symbols
        symbols = re.findall(r"\b[A-Z]{1,5}\b", text)
        if not symbols:
            return None

        symbol = symbols[0]  # Take first symbol found

        # Determine side
        side = "long"
        if any(word in text_lower for word in ["short", "sell", "put", "bear"]):
            side = "short"
        elif any(word in text_lower for word in ["long", "buy", "call", "bull"]):
            side = "long"

        # Extract quantity
        quantity_match = re.search(r"(\d+)\s*shares?", text_lower)
        quantity = int(quantity_match.group(1)) if quantity_match else 100

        # Extract price
        price_matches = re.findall(r"\$?(\d+\.?\d*)", text)
        prices = [float(p) for p in price_matches if float(p) > 1]
        entry_price = prices[0] if prices else None

        # Extract stop loss and take profit
        stop_loss = None
        take_profit = None

        if "stop" in text_lower and len(prices) > 1:
            stop_loss = prices[1]
        if "target" in text_lower or "profit" in text_lower:
            if len(prices) > 2:
                take_profit = prices[2]
            elif len(prices) > 1 and stop_loss is None:
                take_profit = prices[1]

        return {
            "symbol": symbol,
            "side": side,
            "quantity": quantity,
            "entry_price": entry_price,
            "stop_loss": stop_loss,
            "take_profit": take_profit,
        }


class RiskManager:
    """Manages trading risk calculations."""

    def __init__(self, account_size: float = 100000):
        self.account_size = account_size
        self.max_risk_per_trade = 0.02  # 2%
        self.max_portfolio_risk = 0.06  # 6%
        self.max_position_size = 0.20  # 20%

    def calculate_position_size(
        self, entry_price: float, stop_loss: float, risk_amount: float = None
    ) -> int:
        """Calculate appropriate position size based on risk."""
        if risk_amount is None:
            risk_amount = self.account_size * self.max_risk_per_trade

        if stop_loss and entry_price != stop_loss:
            risk_per_share = abs(entry_price - stop_loss)
            max_shares = int(risk_amount / risk_per_share)

            # Apply position size limit
            max_position_value = self.account_size * self.max_position_size
            max_shares_by_value = int(max_position_value / entry_price)

            return min(max_shares, max_shares_by_value)

        # Default to 2% of account if no stop loss
        return int((self.account_size * self.max_risk_per_trade) / entry_price)

    def validate_trade(
        self,
        symbol: str,
        side: str,
        quantity: int,
        entry_price: float,
        current_positions: Dict,
    ) -> Dict:
        """Validate if a trade meets risk criteria."""
        position_value = quantity * entry_price
        position_risk = position_value / self.account_size

        # Check position size limit
        if position_risk > self.max_position_size:
            return {
                "valid": False,
                "reason": f"Position size ({position_risk:.1%}) exceeds limit ({self.max_position_size:.1%})",
            }

        # Check portfolio concentration
        total_exposure = sum(pos.market_value for pos in current_positions.values())
        new_total = total_exposure + position_value
        portfolio_risk = new_total / self.account_size

        if portfolio_risk > self.max_portfolio_risk:
            return {
                "valid": False,
                "reason": f"Portfolio risk ({portfolio_risk:.1%}) exceeds limit ({self.max_portfolio_risk:.1%})",
            }

        return {"valid": True, "reason": "Trade passes risk checks"}
