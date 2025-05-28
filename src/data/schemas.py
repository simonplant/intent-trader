from datetime import UTC, datetime
from typing import Dict, List, Optional, Union

from pydantic import BaseModel, Field, field_validator


class MarketDataSchema(BaseModel):
    """Schema for market data"""

    symbol: str
    price: float
    volume: float
    timestamp: datetime = Field(default_factory=lambda: datetime.now(UTC))
    metadata: Dict[str, Union[str, float, int]] = Field(default_factory=dict)


class AnalysisSchema(BaseModel):
    """Schema for market analysis"""

    symbol: str
    analysis_type: str
    timestamp: datetime = Field(default_factory=lambda: datetime.now(UTC))
    metrics: Dict[str, float] = Field(default_factory=dict)
    signals: List[Dict[str, Union[str, float]]] = Field(default_factory=list)
    metadata: Dict[str, Union[str, float, int]] = Field(default_factory=dict)


class TradePlanSchema(BaseModel):
    """Schema for trade plan"""

    plan_id: str
    symbol: str
    side: str
    entry_price: float
    stop_loss: Optional[float] = None
    take_profit: Optional[float] = None
    quantity: float
    strategy: str
    conviction_score: float = Field(default=0.0)
    notes: str = Field(default="")
    timestamp: datetime = Field(default_factory=lambda: datetime.now(UTC))

    @field_validator("side")
    @classmethod
    def validate_side(cls, v):
        if v not in ["long", "short"]:
            raise ValueError("Side must be either 'long' or 'short'")
        return v


class PositionSchema(BaseModel):
    """Schema for position data"""

    position_id: str
    symbol: str
    quantity: float
    entry_price: float
    current_price: float
    pnl: float
    pnl_percent: float
    stop_loss: Optional[float] = None
    take_profit: Optional[float] = None
    timestamp: datetime = Field(default_factory=lambda: datetime.now(UTC))
    status: str = "open"
    metadata: Dict[str, Union[str, float, int]] = Field(default_factory=dict)

    @field_validator("status")
    @classmethod
    def validate_status(cls, v):
        if v not in ["open", "closed"]:
            raise ValueError("Status must be either 'open' or 'closed'")
        return v


class OrderSchema(BaseModel):
    """Schema for order"""

    order_id: str
    symbol: str
    side: str
    order_type: str
    quantity: float
    price: Optional[float] = None
    status: str = "pending"
    timestamp: datetime = Field(default_factory=lambda: datetime.now(UTC))
    metadata: Dict[str, Union[str, float, int]] = Field(default_factory=dict)


class AgentResponseSchema(BaseModel):
    """Schema for agent response"""

    response_id: str
    agent_type: str
    timestamp: datetime = Field(default_factory=lambda: datetime.now(UTC))
    content: Dict[str, Union[str, float, int, List[Dict[str, Union[str, float]]]]]
    metadata: Dict[str, Union[str, float, int]] = Field(default_factory=dict)


class MarketContextSchema(BaseModel):
    """Schema for market context"""

    timestamp: datetime = Field(default_factory=lambda: datetime.now(UTC))
    market_data: Dict[str, MarketDataSchema] = Field(default_factory=dict)
    analysis: Dict[str, AnalysisSchema] = Field(default_factory=dict)
    positions: Dict[str, PositionSchema] = Field(default_factory=dict)
    orders: Dict[str, OrderSchema] = Field(default_factory=dict)
    metadata: Dict[str, Union[str, float, int]] = Field(default_factory=dict)


class RiskMetricsSchema(BaseModel):
    """Schema for risk metrics"""

    timestamp: datetime = Field(default_factory=lambda: datetime.now(UTC))
    portfolio_value: float
    total_pnl: float
    total_pnl_percent: float
    sharpe_ratio: float
    sortino_ratio: float
    max_drawdown: float
    max_drawdown_percent: float
    win_rate: float
    profit_factor: float
    metadata: Dict[str, Union[str, float, int]] = Field(default_factory=dict)


class PerformanceMetricsSchema(BaseModel):
    """Schema for performance metrics"""

    timestamp: datetime = Field(default_factory=lambda: datetime.now(UTC))
    total_trades: int
    winning_trades: int
    losing_trades: int
    win_rate: float
    average_win: float
    average_loss: float
    profit_factor: float
    sharpe_ratio: float
    sortino_ratio: float
    max_drawdown: float
    max_drawdown_percent: float
    metadata: Dict[str, Union[str, float, int]] = Field(default_factory=dict)
