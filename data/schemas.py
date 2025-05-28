from typing import Dict, Any, List, Optional
from datetime import datetime
from pydantic import BaseModel, Field, validator

class MarketDataSchema(BaseModel):
    """Schema for market data exchange between agents"""
    symbol: str
    price: float
    change: float
    change_percent: float
    volume: int
    high: float
    low: float
    timestamp: datetime
    additional_data: Dict[str, Any] = Field(default_factory=dict)

    @validator('price', 'change', 'change_percent', 'high', 'low')
    def validate_price_fields(cls, v):
        if v < 0:
            raise ValueError("Price fields cannot be negative")
        return v

class AnalysisSchema(BaseModel):
    """Schema for analysis data exchange between agents"""
    analysis_id: str
    symbol: str
    content: str
    sentiment: float = Field(ge=-1.0, le=1.0)
    confidence: float = Field(ge=0.0, le=1.0)
    timestamp: datetime
    source: str
    metadata: Dict[str, Any] = Field(default_factory=dict)

class TradePlanSchema(BaseModel):
    """Schema for trade plan exchange between agents"""
    plan_id: str
    symbol: str
    side: str
    entry_price: Optional[float]
    stop_loss: Optional[float]
    take_profit: Optional[float]
    quantity: Optional[float]
    conviction_score: float = Field(ge=0.0, le=1.0)
    risk_parameters: Dict[str, Any] = Field(default_factory=dict)
    notes: Optional[str]
    timestamp: datetime
    status: str = "active"

    @validator('side')
    def validate_side(cls, v):
        if v not in ['long', 'short']:
            raise ValueError("Side must be either 'long' or 'short'")
        return v

class PositionSchema(BaseModel):
    """Schema for position data exchange between agents"""
    position_id: str
    symbol: str
    quantity: float
    entry_price: float
    current_price: float
    stop_loss: Optional[float]
    take_profit: Optional[float]
    pnl: float
    pnl_percent: float
    timestamp: datetime
    status: str = "open"

class OrderSchema(BaseModel):
    """Schema for order data exchange between agents"""
    order_id: str
    symbol: str
    side: str
    order_type: str
    quantity: float
    price: Optional[float]
    status: str
    timestamp: datetime
    metadata: Dict[str, Any] = Field(default_factory=dict)

class AgentResponseSchema(BaseModel):
    """Schema for standardized agent responses"""
    status: str
    message: str
    data: Optional[Dict[str, Any]]
    timestamp: datetime = Field(default_factory=datetime.now)

    @validator('status')
    def validate_status(cls, v):
        if v not in ['success', 'error', 'warning']:
            raise ValueError("Status must be one of: success, error, warning")
        return v

class MarketContextSchema(BaseModel):
    """Schema for market context data"""
    timestamp: datetime
    market_regime: str
    volatility: float
    trend: float
    volume_profile: Dict[str, float]
    key_levels: Dict[str, List[float]]
    metadata: Dict[str, Any] = Field(default_factory=dict)

class RiskMetricsSchema(BaseModel):
    """Schema for risk metrics data"""
    timestamp: datetime
    portfolio_value: float
    total_risk: float
    position_risks: Dict[str, float]
    correlation_matrix: Dict[str, Dict[str, float]]
    var_metrics: Dict[str, float]
    metadata: Dict[str, Any] = Field(default_factory=dict)

class PerformanceMetricsSchema(BaseModel):
    """Schema for performance metrics data"""
    timestamp: datetime
    total_trades: int
    winning_trades: int
    losing_trades: int
    win_rate: float
    average_win: float
    average_loss: float
    profit_factor: float
    sharpe_ratio: float
    max_drawdown: float
    metadata: Dict[str, Any] = Field(default_factory=dict) 