"""Action dispatcher for executing trading commands."""

from typing import Any, Callable, Dict

from .intent_parser import Intent


class ActionDispatcher:
    """Dispatches parsed intents to appropriate handlers."""

    def __init__(self):
        self.handlers: Dict[str, Callable] = {}
        self._register_default_handlers()

    def _register_default_handlers(self):
        """Register default action handlers."""
        self.handlers.update(
            {
                "buy": self._handle_buy,
                "sell": self._handle_sell,
                "check": self._handle_check,
                "cancel": self._handle_cancel,
                "unknown": self._handle_unknown,
            }
        )

    def register_handler(self, action: str, handler: Callable):
        """Register a custom handler for an action."""
        self.handlers[action] = handler

    def dispatch(self, intent: Intent) -> Dict[str, Any]:
        """Dispatch an intent to the appropriate handler."""
        handler = self.handlers.get(intent.action, self._handle_unknown)
        return handler(intent)

    def _handle_buy(self, intent: Intent) -> Dict[str, Any]:
        """Handle buy intent."""
        return {
            "action": "buy",
            "symbol": intent.symbol,
            "quantity": intent.quantity or 100,
            "price": intent.price,
            "status": "pending",
        }

    def _handle_sell(self, intent: Intent) -> Dict[str, Any]:
        """Handle sell intent."""
        return {
            "action": "sell",
            "symbol": intent.symbol,
            "quantity": intent.quantity or 100,
            "price": intent.price,
            "status": "pending",
        }

    def _handle_check(self, intent: Intent) -> Dict[str, Any]:
        """Handle check intent."""
        return {"action": "check", "symbol": intent.symbol, "status": "checking"}

    def _handle_cancel(self, intent: Intent) -> Dict[str, Any]:
        """Handle cancel intent."""
        return {"action": "cancel", "symbol": intent.symbol, "status": "cancelling"}

    def _handle_unknown(self, intent: Intent) -> Dict[str, Any]:
        """Handle unknown intent."""
        return {
            "action": "unknown",
            "error": "Could not understand the command",
            "status": "error",
        }
