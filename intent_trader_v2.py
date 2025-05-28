# New Intent Trader IAA Scaffold
# Following the first principles for building lightning-fast, best-in-class IAA apps

import re
from typing import Dict, Tuple

class IntentTrader:
    """Intent Trader IAA - Minimalist Chat-Native Trading Assistant"""
    
    def __init__(self):
        self.intent_patterns = {
            'PLAN': ['plan', 'levels', 'bias'],
            'FOCUS': ['focus', 'setup', 'target'],
            'EXECUTE': ['buy', 'sell', 'long', 'short'],
            'MANAGE': ['stop', 'exit', 'adjust'],
            'REVIEW': ['review', 'pnl', 'performance'],
            'COACH': ['improve', 'learn', 'feedback']
        }

    def process_message(self, message: str, context: str = "") -> Dict[str, str]:
        """Process a trading message and return response and new context"""
        intent = self.detect_intent(message)
        handler = getattr(self, f'handle_{intent.lower()}', self.handle_unknown)
        response, new_context = handler(message, context)
        return {
            'response': response,
            'context': new_context,
            'intent': intent
        }

    def detect_intent(self, message: str) -> str:
        """Detect intent from message using direct string match"""
        for intent, keywords in self.intent_patterns.items():
            if any(kw in message for kw in keywords):
                return intent
        return 'UNKNOWN'

    def handle_plan(self, message: str, context: str) -> Tuple[str, str]:
        """Handle PLAN intent"""
        return "Planning phase initiated.", context

    def handle_focus(self, message: str, context: str) -> Tuple[str, str]:
        """Handle FOCUS intent"""
        return "Focusing on setups.", context

    def handle_execute(self, message: str, context: str) -> Tuple[str, str]:
        """Handle EXECUTE intent"""
        return "Executing trades.", context

    def handle_manage(self, message: str, context: str) -> Tuple[str, str]:
        """Handle MANAGE intent"""
        return "Managing positions.", context

    def handle_review(self, message: str, context: str) -> Tuple[str, str]:
        """Handle REVIEW intent"""
        return "Reviewing performance.", context

    def handle_coach(self, message: str, context: str) -> Tuple[str, str]:
        """Handle COACH intent"""
        return "Coaching for improvement.", context

    def handle_unknown(self, message: str, context: str) -> Tuple[str, str]:
        """Handle unknown intent"""
        return "Unknown intent. Please try again.", context

# Example usage
if __name__ == "__main__":
    trader = IntentTrader()
    context = ""
    print(trader.process_message("plan the day", context))
    print(trader.process_message("focus on setups", context))
    print(trader.process_message("buy 100 AAPL", context))
    print(trader.process_message("review my trades", context)) 