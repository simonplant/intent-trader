"""Intent parser for natural language trading commands."""

import re
from dataclasses import dataclass
from typing import Any, Dict, Optional


@dataclass
class Intent:
    """Represents a parsed trading intent."""

    action: str
    symbol: Optional[str] = None
    quantity: Optional[int] = None
    price: Optional[float] = None
    side: Optional[str] = None  # 'buy' or 'sell'
    order_type: Optional[str] = None  # 'market', 'limit', 'stop'
    confidence: float = 0.0
    parameters: Dict[str, Any] = None

    def __post_init__(self):
        if self.parameters is None:
            self.parameters = {}


class IntentParser:
    """Parses natural language into trading intents."""

    def __init__(self):
        self.patterns = {
            "buy": [r"buy", r"long", r"purchase"],
            "sell": [r"sell", r"short", r"exit"],
            "check": [r"check", r"status", r"position"],
            "cancel": [r"cancel", r"stop", r"abort"],
            "plan": [r"plan", r"strategy", r"analyze"],
        }

    def parse(self, text: str) -> Intent:
        """Parse text into a trading intent."""
        text_lower = text.lower()

        # Detect action
        action = "unknown"
        for intent_type, patterns in self.patterns.items():
            if any(re.search(pattern, text_lower) for pattern in patterns):
                action = intent_type
                break

        # Extract symbol
        symbols = re.findall(r"\b[A-Z]{1,5}\b", text)
        symbol = symbols[0] if symbols else None

        # Extract quantity
        quantities = re.findall(r"(\d+)\s*shares?", text_lower)
        quantity = int(quantities[0]) if quantities else None

        # Extract price
        prices = re.findall(r"\$?(\d+\.?\d*)", text)
        price = float(prices[0]) if prices and float(prices[0]) > 1 else None

        # Extract parameters
        parameters = {}

        # Check for specific sources
        if "dp transcript" in text_lower:
            parameters["source"] = "dp_transcript"
        elif "transcript" in text_lower:
            parameters["source"] = "transcript"

        return Intent(
            action=action,
            symbol=symbol,
            quantity=quantity,
            price=price,
            confidence=0.8 if action != "unknown" else 0.1,
            parameters=parameters,
        )
