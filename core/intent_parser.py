from typing import Dict, Any, Optional
from pydantic import BaseModel

class Intent(BaseModel):
    action: str
    parameters: Dict[str, Any]

class IntentParser:
    def parse(self, user_input: str) -> Optional[Intent]:
        # TODO: Implement intent parsing logic (e.g., using LLM or rules)
        # For now, return a dummy intent
        return Intent(action="plan", parameters={"source": "dp_transcript"}) 