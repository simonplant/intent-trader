from typing import Dict, Any, List

class ConversationManager:
    def __init__(self):
        self.context: Dict[str, Any] = {}
        self.history: List[Dict[str, Any]] = []

    def add_to_history(self, message: Dict[str, Any]) -> None:
        self.history.append(message)

    def get_context(self) -> Dict[str, Any]:
        return self.context

    def update_context(self, new_context: Dict[str, Any]) -> None:
        self.context.update(new_context) 