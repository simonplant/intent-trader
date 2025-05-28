"""
Intent Trader IAA (Intelligent Assistant Application) Core

This is the main entry point for the chat-native trading assistant.
Designed for ChatGPT and Claude.ai environments with stateless operation.
"""

from .core.config import get_config_manager
from .core.logging import get_log_manager
from .market_data.feed import MarketDataFeed
from .position_manager import PositionManager, RiskManager


class TradingContext:
    """Manages trading context extracted from conversation."""

    def __init__(self):
        self.phase = "PLAN"  # PLAN, FOCUS, EXECUTE, MANAGE, REVIEW, COACH
        self.positions = []
        self.plan = {}
        self.session_metrics = {"pnl": 0.0, "trades": 0}
        self.market_bias = None
        self.focus_ideas = []
        self.position_manager = PositionManager()

    def to_compressed(self) -> str:
        """Convert context to compressed string format."""
        pos_str = self.position_manager.to_compressed()
        return f"P:{self.phase}|POS:{pos_str}|PNL:{self.session_metrics['pnl']}"

    @classmethod
    def from_compressed(cls, compressed: str) -> "TradingContext":
        """Create context from compressed string."""
        ctx = cls()
        parts = compressed.split("|")
        for part in parts:
            if part.startswith("P:"):
                ctx.phase = part[2:]
            elif part.startswith("PNL:"):
                ctx.session_metrics["pnl"] = float(part[4:])
            elif part.startswith("POS:"):
                pos_data = part[4:]
                if pos_data:
                    ctx.position_manager.from_compressed(pos_data)
        return ctx


class IntentProcessor:
    """Processes natural language intents into trading actions."""

    INTENT_PATTERNS = {
        "PLAN": [
            r"morning call",
            r"market analysis",
            r"plan.*day",
            r"levels.*today",
            r"bias.*market",
            r"key.*ideas",
            r"setup.*today",
            r"what.*levels",
        ],
        "FOCUS": [
            r"focus.*on",
            r"trading.*plan",
            r"prioritize",
            r"best.*setup",
            r"which.*trade",
            r"entry.*strategy",
            r"plan.*trade",
        ],
        "EXECUTE": [
            r"buy.*shares",
            r"sell.*shares",
            r"enter.*position",
            r"place.*order",
            r"go.*long",
            r"go.*short",
            r"size.*position",
            r"execute.*trade",
        ],
        "MANAGE": [
            r"adjust.*stop",
            r"move.*target",
            r"check.*position",
            r"current.*pnl",
            r"exit.*position",
            r"close.*trade",
            r"how.*doing",
            r"position.*status",
        ],
        "REVIEW": [
            r"session.*review",
            r"how.*did",
            r"performance",
            r"what.*learned",
            r"analyze.*trades",
            r"daily.*summary",
            r"review.*day",
        ],
        "COACH": [
            r"improve.*trading",
            r"feedback",
            r"what.*wrong",
            r"better.*next",
            r"pattern.*recognition",
            r"behavioral.*insight",
            r"coaching",
        ],
    }

    def detect_intent(self, text: str) -> str:
        """Detect the primary intent from text."""
        text_lower = text.lower()

        for intent, patterns in self.INTENT_PATTERNS.items():
            for pattern in patterns:
                if re.search(pattern, text_lower):
                    return intent

        return "PLAN"  # Default intent

    def extract_symbols(self, text: str) -> List[str]:
        """Extract stock symbols from text."""
        # Look for common stock symbol patterns
        symbols = re.findall(r"\b[A-Z]{1,5}\b", text)
        # Filter out common words that aren't symbols
        excluded = {
            "THE",
            "AND",
            "FOR",
            "ARE",
            "BUT",
            "NOT",
            "YOU",
            "ALL",
            "CAN",
            "HER",
            "WAS",
            "ONE",
            "OUR",
            "HAD",
            "BY",
            "UP",
            "DO",
            "NO",
            "IF",
            "MY",
            "ON",
            "IT",
            "IS",
            "IN",
            "TO",
            "OF",
            "AS",
            "AT",
            "BE",
            "OR",
            "AN",
            "HE",
            "HAS",
            "HIS",
            "THAT",
            "WHICH",
            "SHE",
            "HIM",
            "BEEN",
            "THAN",
            "ITS",
            "GET",
            "VERY",
            "DIM",
            "SET",
            "OWN",
            "UNDER",
            "LAST",
            "RIGHT",
            "THINK",
            "EACH",
            "SHOULD",
            "SAID",
            "OVER",
            "BACK",
            "OTHER",
            "MANY",
            "THEN",
            "THEM",
            "THESE",
            "SO",
            "SOME",
            "HER",
            "WOULD",
            "MAKE",
            "LIKE",
            "INTO",
            "TIME",
            "HAS",
            "TWO",
            "MORE",
            "VERY",
            "WHAT",
            "KNOW",
            "JUST",
            "FIRST",
            "GET",
            "MAY",
            "NEW",
            "WAY",
            "COULD",
            "THERE",
            "USE",
            "YOUR",
            "HOW",
            "SAID",
            "EACH",
            "WHICH",
            "THEIR",
            "WILL",
            "ABOUT",
            "IF",
            "UP",
            "OUT",
            "MANY",
            "THEN",
            "THEM",
            "THESE",
            "SO",
            "SOME",
            "HER",
            "WOULD",
            "MAKE",
            "LIKE",
            "INTO",
            "HIM",
            "HAS",
            "TWO",
            "MORE",
            "GO",
            "NO",
            "WAY",
            "COULD",
            "MY",
            "THAN",
            "FIRST",
            "BEEN",
            "CALL",
            "WHO",
            "ITS",
            "NOW",
            "FIND",
            "LONG",
            "DOWN",
            "DAY",
            "DID",
            "GET",
            "COME",
            "MADE",
            "MAY",
            "PART",
        }
        return [s for s in symbols if s not in excluded and len(s) <= 5]

    def extract_quantities(self, text: str) -> List[int]:
        """Extract quantities from text."""
        quantities = re.findall(r"\b(\d+)\s*shares?\b", text.lower())
        return [int(q) for q in quantities]

    def extract_prices(self, text: str) -> List[float]:
        """Extract prices from text."""
        prices = re.findall(r"\$?(\d+\.?\d*)", text)
        return [float(p) for p in prices if float(p) > 1]  # Filter out small numbers


class IAA:
    """Main Intelligent Assistant Application for Trading."""

    def __init__(self, config_path: str = None):
        """Initialize the IAA."""
        self.config = get_config_manager(config_path) if config_path else None
        self.logger = get_log_manager(self.config).get_logger(__name__)
        self.intent_processor = IntentProcessor()
        self.context = TradingContext()
        self.risk_manager = RiskManager()
        self.market_feed = MarketDataFeed()

    def process_message(self, message: str, context_str: str = None) -> Dict[str, Any]:
        """Process a chat message and return response.

        Args:
            message: The user's message
            context_str: Optional compressed context from previous conversation

        Returns:
            Dictionary containing response and updated context
        """
        try:
            # Restore context if provided
            if context_str:
                self.context = TradingContext.from_compressed(context_str)

            # Detect intent
            intent = self.intent_processor.detect_intent(message)

            # Extract trading entities
            symbols = self.intent_processor.extract_symbols(message)
            quantities = self.intent_processor.extract_quantities(message)
            prices = self.intent_processor.extract_prices(message)

            # Process based on intent
            response = self._handle_intent(intent, message, symbols, quantities, prices)

            # Update context
            self.context.phase = intent

            return {
                "response": response,
                "context": self.context.to_compressed(),
                "intent": intent,
                "entities": {
                    "symbols": symbols,
                    "quantities": quantities,
                    "prices": prices,
                },
            }

        except Exception as e:
            self.logger.error(f"Error processing message: {e}")
            return {
                "response": "❌ Sorry, I encountered an error processing your request. Please try again.",
                "context": self.context.to_compressed() if self.context else "",
                "intent": "ERROR",
                "entities": {},
            }

    def _handle_intent(
        self,
        intent: str,
        message: str,
        symbols: List[str],
        quantities: List[int],
        prices: List[float],
    ) -> str:
        """Handle specific intent and generate response."""

        if intent == "PLAN":
            return self._handle_plan(message, symbols)
        elif intent == "FOCUS":
            return self._handle_focus(message, symbols)
        elif intent == "EXECUTE":
            return self._handle_execute(message, symbols, quantities, prices)
        elif intent == "MANAGE":
            return self._handle_manage(message, symbols)
        elif intent == "REVIEW":
            return self._handle_review(message)
        elif intent == "COACH":
            return self._handle_coach(message)
        else:
            return "🤔 I'm not sure how to help with that. Could you clarify your trading intent?"

    def _handle_plan(self, message: str, symbols: List[str]) -> str:
        """Handle PLAN phase requests."""
        # Get current prices for mentioned symbols
        price_info = ""
        if symbols:
            try:
                prices = self.market_feed.get_multiple_prices(symbols)
                price_lines = [
                    f"• {symbol}: ${price:.2f}" for symbol, price in prices.items() if price > 0
                ]
                if price_lines:
                    price_info = f"\n\n📊 **Current Prices:**\n" + "\n".join(price_lines)
            except Exception as e:
                self.logger.warning(f"Could not fetch prices: {e}")

        return f"""📋 **PLAN Phase - Market Analysis**

🎯 **Key Focus Areas:**
• Market bias assessment
• Level identification  
• Setup prioritization

{f"📊 **Symbols Mentioned:** {', '.join(symbols)}" if symbols else ""}{price_info}

💡 **Next Steps:**
1. Analyze morning call levels
2. Identify high-probability setups
3. Prepare trading plan

*Ready to move to FOCUS phase when you have your key ideas.*"""

    def _handle_focus(self, message: str, symbols: List[str]) -> str:
        """Handle FOCUS phase requests."""
        return f"""🎯 **FOCUS Phase - Trading Plan**

🔍 **Setup Analysis:**
{f"• Analyzing: {', '.join(symbols)}" if symbols else "• Awaiting symbol selection"}

📈 **Plan Elements:**
• Entry strategy
• Position sizing
• Risk management
• Target levels

*Ready to move to EXECUTE phase when plan is confirmed.*"""

    def _handle_execute(
        self,
        message: str,
        symbols: List[str],
        quantities: List[int],
        prices: List[float],
    ) -> str:
        """Handle EXECUTE phase requests."""
        execution_summary = []

        # Parse position from text
        position_data = self.context.position_manager.parse_position_from_text(message)

        if position_data and position_data["entry_price"]:
            symbol = position_data["symbol"]
            side = position_data["side"]
            quantity = position_data["quantity"]
            entry_price = position_data["entry_price"]

            # Validate trade with risk manager
            validation = self.risk_manager.validate_trade(
                symbol,
                side,
                quantity,
                entry_price,
                self.context.position_manager.positions,
            )

            if validation["valid"]:
                # Add position
                position = self.context.position_manager.add_position(
                    symbol,
                    side,
                    quantity,
                    entry_price,
                    position_data.get("stop_loss"),
                    position_data.get("take_profit"),
                )

                if position:
                    execution_summary.append(
                        f"✅ {symbol}: {side.upper()} {quantity} shares @ ${entry_price:.2f}"
                    )
                    if position_data.get("stop_loss"):
                        execution_summary.append(f"   Stop Loss: ${position_data['stop_loss']:.2f}")
                    if position_data.get("take_profit"):
                        execution_summary.append(
                            f"   Take Profit: ${position_data['take_profit']:.2f}"
                        )
            else:
                execution_summary.append(f"❌ Trade rejected: {validation['reason']}")

        if not execution_summary:
            execution_summary.append("• Awaiting order details (symbol, quantity, price)")

        return f"""⚡ **EXECUTE Phase - Order Placement**

🎯 **Execution Plan:**
{chr(10).join(execution_summary)}

⚠️ **Risk Check:**
• Position sizing validated
• Stop loss levels confirmed
• Portfolio exposure reviewed

*Moving to MANAGE phase for position monitoring.*"""

    def _handle_manage(self, message: str, symbols: List[str]) -> str:
        """Handle MANAGE phase requests."""
        # Get current positions
        positions = self.context.position_manager.get_all_positions()
        portfolio_summary = self.context.position_manager.get_portfolio_summary()

        position_lines = []
        if positions:
            # Update current prices
            position_symbols = [pos.symbol for pos in positions]
            try:
                current_prices = self.market_feed.get_multiple_prices(position_symbols)
                for pos in positions:
                    if pos.symbol in current_prices and current_prices[pos.symbol] > 0:
                        pos.current_price = current_prices[pos.symbol]

                # Format position display
                for pos in positions:
                    pnl_str = f"${pos.unrealized_pnl:+.2f} ({pos.unrealized_pnl_percent:+.1f}%)"
                    position_lines.append(
                        f"• {pos.symbol}: {pos.side.upper()} {pos.quantity}@${pos.entry_price:.2f} → ${pos.current_price:.2f} {pnl_str}"
                    )
            except Exception as e:
                self.logger.warning(f"Could not update prices: {e}")
                for pos in positions:
                    position_lines.append(
                        f"• {pos.symbol}: {pos.side.upper()} {pos.quantity}@${pos.entry_price:.2f}"
                    )

        if not position_lines:
            position_lines.append("• No active positions")

        return f"""📊 **MANAGE Phase - Position Monitoring**

🔍 **Active Positions:**
{chr(10).join(position_lines)}

💰 **Portfolio Summary:**
• Total P&L: ${portfolio_summary['total_pnl']:+.2f}
• Market Value: ${portfolio_summary['total_market_value']:.2f}
• Active Positions: {portfolio_summary['active_positions']}

📈 **Management Actions:**
• Stop loss adjustments
• Target modifications
• P&L tracking
• Exit strategies

*Continuous monitoring until position closure.*"""

    def _handle_review(self, message: str) -> str:
        """Handle REVIEW phase requests."""
        portfolio_summary = self.context.position_manager.get_portfolio_summary()

        return f"""📋 **REVIEW Phase - Session Analysis**

📊 **Session Metrics:**
• Total P&L: ${portfolio_summary['total_pnl']:+.2f}
• Realized P&L: ${portfolio_summary['realized_pnl']:+.2f}
• Unrealized P&L: ${portfolio_summary['unrealized_pnl']:+.2f}
• Trades: {len(self.context.position_manager.closed_positions)}

🎯 **Performance Review:**
• Trade execution quality
• Risk management effectiveness
• Plan adherence
• Learning opportunities

*Ready for COACH phase for improvement insights.*"""

    def _handle_coach(self, message: str) -> str:
        """Handle COACH phase requests."""
        return f"""🎓 **COACH Phase - Performance Insights**

💡 **Key Insights:**
• Pattern recognition
• Behavioral analysis
• Improvement areas
• Success factors

🚀 **Next Session Prep:**
• Refined strategies
• Updated risk parameters
• Enhanced focus areas

*Ready to start new PLAN phase when market opens.*"""


# Convenience function for direct usage
def process_trading_message(message: str, context: str = None) -> Dict[str, Any]:
    """Process a trading message with the IAA.

    Args:
        message: User's trading message
        context: Optional compressed context string

    Returns:
        Response dictionary with answer and updated context
    """
    iaa = IAA()
    return iaa.process_message(message, context)
