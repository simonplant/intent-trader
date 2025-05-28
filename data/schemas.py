from pydantic import BaseModel
from typing import Dict, Any

class IntentSchema(BaseModel):
    action: str
    parameters: Dict[str, Any]

class AgentResponseSchema(BaseModel):
    status: str
    message: str
    data: Dict[str, Any] 