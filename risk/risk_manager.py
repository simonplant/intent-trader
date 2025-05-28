from datetime import datetime
from typing import Any, Dict, Optional

from pydantic import BaseModel, Field, validator

from data.schemas import MarketDataSchema, PositionSchema


class RiskParameters(BaseModel):
    """Parameters for risk management"""

    account_value: float
    max_daily_risk: float = Field(ge=0.0, le=1.0)  # As percentage of account
    max_position_risk: float = Field(ge=0.0, le=1.0)  # As percentage of account
    max_drawdown: float = Field(ge=0.0, le=1.0)  # As percentage of account
    max_correlation: float = Field(ge=0.0, le=1.0)  # Maximum correlation between positions
    min_risk_reward: float = Field(ge=1.0)  # Minimum risk-reward ratio
    max_positions: int = Field(ge=1)  # Maximum number of concurrent positions
    position_sizing_method: str = "kelly"  # kelly, fixed, or adaptive

    @validator("position_sizing_method")
    def validate_position_sizing_method(cls, v):
        if v not in ["kelly", "fixed", "adaptive"]:
            raise ValueError("Position sizing method must be one of: kelly, fixed, adaptive")
        return v


class PositionRisk(BaseModel):
    """Risk metrics for a single position"""

    symbol: str
    quantity: float
    entry_price: float
    current_price: float
    stop_loss: float
    take_profit: float
    risk_amount: float
    reward_amount: float
    risk_reward_ratio: float
    risk_percent: float  # As percentage of account
    correlation: Dict[str, float]  # Correlation with other positions
    timestamp: datetime


class PortfolioRisk(BaseModel):
    """Risk metrics for the entire portfolio"""

    total_value: float
    total_risk: float
    total_reward: float
    net_risk_reward: float
    position_risks: Dict[str, PositionRisk]
    correlation_matrix: Dict[str, Dict[str, float]]
    drawdown: float
    timestamp: datetime


class RiskManager:
    """Manager for risk calculations and portfolio management"""

    def __init__(self, risk_parameters: RiskParameters):
        self.risk_parameters = risk_parameters
        self.positions: Dict[str, PositionSchema] = {}
        self.market_data: Dict[str, MarketDataSchema] = {}

    def update_market_data(self, market_data: Dict[str, MarketDataSchema]) -> None:
        """Update market data for risk calculations"""
        self.market_data.update(market_data)

    def update_positions(self, positions: Dict[str, PositionSchema]) -> None:
        """Update positions for risk calculations"""
        self.positions.update(positions)

    def calculate_position_size(
        self, symbol: str, entry_price: float, stop_loss: float, take_profit: float
    ) -> float:
        """Calculate optimal position size based on risk parameters"""
        if symbol not in self.market_data:
            raise ValueError(f"No market data available for {symbol}")

        # Calculate risk per share
        risk_per_share = abs(entry_price - stop_loss)
        if risk_per_share == 0:
            return 0

        # Calculate maximum position size based on risk
        max_risk_amount = (
            self.risk_parameters.account_value * self.risk_parameters.max_position_risk
        )
        max_shares = max_risk_amount / risk_per_share

        # Apply position sizing method
        if self.risk_parameters.position_sizing_method == "kelly":
            # Kelly Criterion
            win_prob = self._calculate_win_probability(symbol)
            kelly_fraction = win_prob - (
                (1 - win_prob) / ((take_profit - entry_price) / (entry_price - stop_loss))
            )
            kelly_fraction = max(0, min(kelly_fraction, 0.5))  # Conservative Kelly
            return max_shares * kelly_fraction

        elif self.risk_parameters.position_sizing_method == "fixed":
            # Fixed percentage of max position size
            return max_shares * 0.5  # 50% of max position size

        else:  # adaptive
            # Adaptive sizing based on market conditions and portfolio state
            market_volatility = self._calculate_market_volatility(symbol)
            portfolio_correlation = self._calculate_portfolio_correlation(symbol)

            # Reduce size for high volatility or correlation
            size_multiplier = 1.0
            if market_volatility > 0.2:  # High volatility
                size_multiplier *= 0.7
            if portfolio_correlation > 0.7:  # High correlation
                size_multiplier *= 0.8

            return max_shares * size_multiplier

    def calculate_position_risk(self, symbol: str) -> Optional[PositionRisk]:
        """Calculate risk metrics for a position"""
        if symbol not in self.positions or symbol not in self.market_data:
            return None

        position = self.positions[symbol]
        market_data = self.market_data[symbol]

        # Calculate risk and reward amounts
        risk_amount = abs(position.entry_price - position.stop_loss) * position.quantity
        reward_amount = abs(position.take_profit - position.entry_price) * position.quantity

        # Calculate risk as percentage of account
        risk_percent = risk_amount / self.risk_parameters.account_value

        # Calculate correlation with other positions
        correlation = self._calculate_position_correlation(symbol)

        return PositionRisk(
            symbol=symbol,
            quantity=position.quantity,
            entry_price=position.entry_price,
            current_price=market_data.price,
            stop_loss=position.stop_loss,
            take_profit=position.take_profit,
            risk_amount=risk_amount,
            reward_amount=reward_amount,
            risk_reward_ratio=reward_amount / risk_amount if risk_amount > 0 else 0,
            risk_percent=risk_percent,
            correlation=correlation,
            timestamp=datetime.now(),
        )

    def calculate_portfolio_risk(self) -> PortfolioRisk:
        """Calculate risk metrics for the entire portfolio"""
        position_risks = {}
        total_risk = 0
        total_reward = 0

        # Calculate risk for each position
        for symbol in self.positions:
            position_risk = self.calculate_position_risk(symbol)
            if position_risk:
                position_risks[symbol] = position_risk
                total_risk += position_risk.risk_amount
                total_reward += position_risk.reward_amount

        # Calculate correlation matrix
        correlation_matrix = self._calculate_correlation_matrix()

        # Calculate drawdown
        drawdown = self._calculate_drawdown()

        return PortfolioRisk(
            total_value=self.risk_parameters.account_value,
            total_risk=total_risk,
            total_reward=total_reward,
            net_risk_reward=total_reward / total_risk if total_risk > 0 else 0,
            position_risks=position_risks,
            correlation_matrix=correlation_matrix,
            drawdown=drawdown,
            timestamp=datetime.now(),
        )

    def validate_trade(
        self,
        symbol: str,
        quantity: float,
        entry_price: float,
        stop_loss: float,
        take_profit: float,
    ) -> Dict[str, Any]:
        """Validate a trade against risk parameters"""
        # Calculate position risk
        risk_amount = abs(entry_price - stop_loss) * quantity
        risk_percent = risk_amount / self.risk_parameters.account_value

        # Check position risk limit
        if risk_percent > self.risk_parameters.max_position_risk:
            return {
                "valid": False,
                "reason": f"Position risk {risk_percent:.2%} exceeds maximum {self.risk_parameters.max_position_risk:.2%}",
            }

        # Check risk-reward ratio
        reward_amount = abs(take_profit - entry_price) * quantity
        risk_reward = reward_amount / risk_amount if risk_amount > 0 else 0
        if risk_reward < self.risk_parameters.min_risk_reward:
            return {
                "valid": False,
                "reason": f"Risk-reward ratio {risk_reward:.2f} below minimum {self.risk_parameters.min_risk_reward:.2f}",
            }

        # Check position count
        if len(self.positions) >= self.risk_parameters.max_positions:
            return {
                "valid": False,
                "reason": f"Maximum number of positions ({self.risk_parameters.max_positions}) reached",
            }

        # Check correlation
        correlation = self._calculate_position_correlation(symbol)
        if any(corr > self.risk_parameters.max_correlation for corr in correlation.values()):
            return {
                "valid": False,
                "reason": "Position correlation exceeds maximum allowed",
            }

        return {"valid": True}

    def _calculate_win_probability(self, symbol: str) -> float:
        """Calculate win probability for a symbol"""
        # This would typically use historical data and market analysis
        # For now, return a default value
        return 0.6

    def _calculate_market_volatility(self, symbol: str) -> float:
        """Calculate market volatility for a symbol"""
        if symbol not in self.market_data:
            return 0.0

        # This would typically use historical data
        # For now, use a simple calculation based on high/low
        market_data = self.market_data[symbol]
        return (market_data.high - market_data.low) / market_data.price

    def _calculate_position_correlation(self, symbol: str) -> Dict[str, float]:
        """Calculate correlation between a position and other positions"""
        correlations = {}
        for other_symbol in self.positions:
            if other_symbol != symbol:
                # This would typically use historical data
                # For now, return a default value
                correlations[other_symbol] = 0.5
        return correlations

    def _calculate_correlation_matrix(self) -> Dict[str, Dict[str, float]]:
        """Calculate correlation matrix for all positions"""
        matrix = {}
        for symbol1 in self.positions:
            matrix[symbol1] = {}
            for symbol2 in self.positions:
                if symbol1 != symbol2:
                    # This would typically use historical data
                    # For now, return a default value
                    matrix[symbol1][symbol2] = 0.5
        return matrix

    def _calculate_drawdown(self) -> float:
        """Calculate current drawdown"""
        # This would typically use historical data
        # For now, return a default value
        return 0.0
