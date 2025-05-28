from pydantic import BaseModel
from typing import Dict, Any, List

class Transcript(BaseModel):
    content: str
    source: str

class Analysis(BaseModel):
    bias: str
    setups: List[str]
    confidence: float

class TradePlan(BaseModel):
    summary: str
    setups: List[Dict[str, Any]] 