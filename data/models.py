from datetime import datetime
from typing import Any, Dict, List, Optional

from pydantic import BaseModel, validator


class MarketData(BaseModel):
    symbol: str
    price: float
    change: float
    change_percent: float
    volume: int
    high: float
    low: float
    timestamp: datetime


class Order(BaseModel):
    order_id: str
    symbol: str
    side: str
    order_type: str
    quantity: float
    price: Optional[float]
    stop_price: Optional[float]
    status: str
    timestamp: datetime
    notes: str = ""


class Position(BaseModel):
    position_id: str
    symbol: str
    side: str
    quantity: float
    entry_price: float
    current_price: float
    stop_loss: Optional[float]
    take_profit: Optional[float]
    pnl: float
    pnl_percent: float
    timestamp: datetime
    notes: str = ""


class Trade(BaseModel):
    trade_id: str
    position_id: str
    symbol: str
    side: str
    quantity: float
    entry_price: float
    exit_price: float
    pnl: float
    pnl_percent: float
    entry_time: datetime
    exit_time: datetime
    exit_reason: str
    notes: str = ""


class Analysis(BaseModel):
    analysis_id: str
    symbol: str
    content: str
    sentiment: float
    confidence: float
    key_points: List[str]
    timestamp: datetime
    source: str
    notes: str = ""


class Plan(BaseModel):
    plan_id: str
    symbol: str
    analysis_id: str
    conviction_score: float
    entry_rules: List[str]
    exit_rules: List[str]
    risk_parameters: Dict[str, Any]
    timestamp: datetime
    notes: str = ""


class FocusArea(BaseModel):
    focus_id: str
    symbol: str
    key_levels: List[float]
    setups: List[str]
    market_conditions: Dict[str, Any]
    timestamp: datetime
    notes: str = ""


class ExecutionRules(BaseModel):
    execution_id: str
    plan_id: str
    entry_rules: List[str]
    exit_rules: List[str]
    position_sizing: Dict[str, Any]
    timestamp: datetime
    notes: str = ""


class ManagementRules(BaseModel):
    management_id: str
    position_id: str
    adjustment_rules: List[str]
    scaling_rules: List[str]
    risk_management: Dict[str, Any]
    timestamp: datetime
    notes: str = ""


class TradeReview(BaseModel):
    review_id: str
    trade_id: str
    execution_analysis: Dict[str, Any]
    management_analysis: Dict[str, Any]
    performance_metrics: Dict[str, Any]
    timestamp: datetime
    notes: str = ""


class CoachingInsights(BaseModel):
    insights_id: str
    review_id: str
    improvement_areas: List[Dict[str, Any]]
    best_practices: List[Dict[str, Any]]
    development_plan: Dict[str, Any]
    timestamp: datetime
    notes: str = ""


class AgentResponse(BaseModel):
    status: str
    message: str
    data: Optional[Dict[str, Any]] = None

    @validator("status")
    def validate_status(cls, v):
        if v not in ["success", "error", "warning"]:
            raise ValueError("Status must be success, error, or warning")
        return v
