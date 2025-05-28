from typing import Any, Dict, Optional

from actions.analyze_transcript import AnalyzeTranscriptAction
from actions.clean_transcript import CleanTranscriptAction
from actions.generate_plan import GeneratePlanAction


class ActionDispatcher:
    def __init__(self):
        self.actions: Dict[str, Any] = {
            "clean_transcript": CleanTranscriptAction(),
            "analyze_transcript": AnalyzeTranscriptAction(),
            "generate_plan": GeneratePlanAction(),
        }

    def dispatch(self, action_name: str, **kwargs) -> Optional[Dict[str, Any]]:
        action = self.actions.get(action_name)
        if action:
            return action.execute(**kwargs)
        return None
