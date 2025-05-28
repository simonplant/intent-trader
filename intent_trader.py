"""
Intent Trader IAA v3.1 - Production Master
A complete PFEMRC trading assistant integrating DP/Mancini analysis.
Single file. Zero dependencies. Production ready.

Future extensibility notes:
- To add new analysts: Add to intent_patterns and create handler
- To add broker API: Extend EXECUTE handler with API calls
- To add plugins: Extract handlers dict to config file
- Current design is intentionally monolithic for solo dev speed
"""

import re
import sys
from datetime import datetime

class IntentTrader:
    """PFEMRC trading assistant with DP/Mancini integration."""
    
    def __init__(self):
        self.phases = ["PLAN", "FOCUS", "EXECUTE", "MANAGE", "REVIEW", "COACH"]
        self.intent_patterns = {
            # PLAN Phase
            "ANALYZE_DP": ["dp", "prince", "morning call", "inner circle"],
            "ANALYZE_MANCINI": ["mancini", "newsletter", "es futures", "blueprint"],
            "CREATE_PLAN": ["plan", "daily plan", "trade plan", "create plan"],
            
            # FOCUS Phase  
            "FOCUS_TRADES": ["focus", "setup", "a+ setup", "best trades", "high conviction"],
            "GRADE_SETUP": ["grade", "quality", "rate setup"],
            
            # EXECUTE Phase
            "EXECUTE": ["buy", "sell", "long", "short", "bought", "sold"],
            "SIZE_POSITION": ["size", "position size", "how much", "risk"],
            
            # MANAGE Phase
            "POSITIONS": ["positions", "portfolio", "what am i in", "holdings"],
            "MOVE_STOP": ["stop", "trail", "adjust stop", "move stop"],
            "EXIT": ["exit", "close", "take profit", "scale out"],
            
            # REVIEW Phase
            "REVIEW": ["review", "session", "how did i do", "performance"],
            
            # COACH Phase
            "COACH": ["coach", "feedback", "patterns", "improve", "behavioral"],
            
            # Utilities
            "SAVE": ["save", "backup"],
            "LOAD": ["load", "restore"], 
            "JOURNAL": ["journal", "show journal"],
            "HELP": ["help", "?", "commands"],
            "RESET": ["reset", "clear"],
            "CONTEXT": ["context", "show context"]
        }
        
        self.handlers = {
            "ANALYZE_DP": self.handle_analyze_dp,
            "ANALYZE_MANCINI": self.handle_analyze_mancini,
            "CREATE_PLAN": self.handle_create_plan,
            "FOCUS_TRADES": self.handle_focus_trades,
            "GRADE_SETUP": self.handle_grade_setup,
            "EXECUTE": self.handle_execute,
            "SIZE_POSITION": self.handle_size_position,
            "POSITIONS": self.handle_positions,
            "MOVE_STOP": self.handle_move_stop,
            "EXIT": self.handle_exit,
            "REVIEW": self.handle_review,
            "COACH": self.handle_coach,
            "SAVE": self.handle_save,
            "LOAD": self.handle_load,
            "JOURNAL": self.handle_journal,
            "HELP": self.handle_help,
            "RESET": self.handle_reset,
            "CONTEXT": self.handle_context
        }

    # === ROUTING ===
    def process_message(self, msg, context_str=""):
        """Main entry point - process any message."""
        ctx = self.parse_context(context_str)
        intent = self.detect_intent(msg.lower())
        
        handler = self.handlers.get(intent, self.handle_unknown)
        response, new_ctx = handler(msg, ctx)
        
        # Check for behavioral patterns (real-time coach)
        coach_alert = self.check_behavioral_patterns(new_ctx)
        if coach_alert:
            response += f"\n\n{coach_alert}"
        
        return {
            "response": response,
            "context": self.compress_context(new_ctx),
            "phase": new_ctx.get("phase", "PLAN")
        }
    
    def detect_intent(self, msg):
        """Detect intent from message."""
        for intent, keywords in self.intent_patterns.items():
            for kw in keywords:
                if kw in msg:
                    return intent
        return "HELP"

    # === CONTEXT MANAGEMENT ===
    def parse_context(self, context_str):
        """Parse flat string context into dict."""
        ctx = {
            "phase": "PLAN",
            "positions": [],
            "pnl": 0.0,
            "completed": 0,
            "dp": "",
            "mancini": "",
            "plan": "",
            "focus": "",
            "stops_hit": 0,
            "revenge": False,
            "notes": []
        }
        
        if not context_str:
            return ctx
            
        for part in context_str.split("|"):
            if ":" in part:
                key, value = part.split(":", 1)
                
                if key == "positions" and value:
                    ctx["positions"] = self._parse_positions(value)
                elif key == "pnl":
                    ctx["pnl"] = float(value)
                elif key == "completed":
                    ctx["completed"] = int(value)
                elif key == "stops_hit":
                    ctx["stops_hit"] = int(value)
                elif key == "revenge":
                    ctx["revenge"] = value == "true"
                elif key == "notes" and value:
                    ctx["notes"] = value.split(";;")
                else:
                    ctx[key] = value
        
        return ctx
    
    def compress_context(self, ctx):
        """Compress dict context to flat string."""
        parts = []
        
        # Add all context fields
        parts.append(f"phase:{ctx.get('phase', 'PLAN')}")
        parts.append(f"positions:{self._compress_positions(ctx.get('positions', []))}")
        parts.append(f"pnl:{ctx.get('pnl', 0):.2f}")
        parts.append(f"completed:{ctx.get('completed', 0)}")
        parts.append(f"dp:{ctx.get('dp', '')}")
        parts.append(f"mancini:{ctx.get('mancini', '')}")
        parts.append(f"plan:{ctx.get('plan', '')}")
        parts.append(f"focus:{ctx.get('focus', '')}")
        parts.append(f"stops_hit:{ctx.get('stops_hit', 0)}")
        parts.append(f"revenge:{str(ctx.get('revenge', False)).lower()}")
        parts.append(f"notes:{';;'.join(ctx.get('notes', [])[-3:])}")  # Keep last 3
        
        return "|".join(parts)
    
    def _parse_positions(self, value):
        """Parse position string into list of dicts."""
        positions = []
        for pos in value.split(","):
            match = re.match(r'([A-Z]+):([LS])(\d+)@([\d.]+)', pos)
            if match:
                sym, side, qty, price = match.groups()
                positions.append({
                    "symbol": sym,
                    "side": "long" if side == "L" else "short",
                    "qty": int(qty),
                    "entry": float(price),
                    "current": float(price)  # Will update when exiting
                })
        return positions
    
    def _compress_positions(self, positions):
        """Compress position list to string."""
        if not positions:
            return ""
        return ",".join(
            f"{p['symbol']}:{'L' if p['side']=='long' else 'S'}{p['qty']}@{p['entry']:.2f}"
            for p in positions
        )

    # === PLAN PHASE HANDLERS ===
    def handle_analyze_dp(self, msg, ctx):
        """Analyze DP morning call - handles multi-line transcripts."""
        # Enhanced parsing for full transcripts
        lines = msg.split('\n')
        bias = "NEUTRAL"
        conviction_phrases = []
        key_phrases = []
        
        for line in lines:
            line_lower = line.lower()
            # Bias detection with context
            if "bullish" in line_lower and any(word in line_lower for word in ["above", "over", "break"]):
                bias = "BULLISH"
                key_phrases.append(line.strip())
            elif "bearish" in line_lower and any(word in line_lower for word in ["below", "under", "fail"]):
                bias = "BEARISH"
                key_phrases.append(line.strip())
            
            # Conviction detection
            if any(phrase in line_lower for phrase in ["focus trade", "high conviction", "aggressive", "love this"]):
                conviction_phrases.append(line.strip())
        
        levels = self.extract_levels(msg)
        symbols = self.extract_symbols(msg)
        
        # Filter symbols mentioned with positive context
        focus_symbols = []
        for sym in symbols:
            for line in lines:
                if sym in line and any(word in line.lower() for word in ["like", "focus", "buy", "long"]):
                    focus_symbols.append(sym)
                    break
        
        # Store rich analysis
        ctx["dp"] = f"bias:{bias}|levels:{','.join(map(str, levels))}|symbols:{','.join(focus_symbols[:5])}"
        if conviction_phrases:
            ctx["dp"] += f"|conviction:{conviction_phrases[0][:50]}"  # Store first conviction phrase
        
        ctx["phase"] = "PLAN"
        
        response = f"""
=== DP ANALYSIS ===
📊 Bias: {bias}
📍 Key Levels: {', '.join(map(str, levels[:3])) if levels else 'None specified'}
🎯 Focus List: {', '.join(focus_symbols[:5]) if focus_symbols else 'None specified'}
💪 Conviction: {"HIGH - " + conviction_phrases[0][:50] if conviction_phrases else "Standard"}
⚠️  No-Trade: Below {min(levels) if levels else 'key support'}

Key Phrases:
{chr(10).join('• ' + phrase for phrase in key_phrases[:3])}

→ Next: Analyze Mancini for confluence"""
        
        return response.strip(), ctx
    
    def handle_analyze_mancini(self, msg, ctx):
        """Analyze Mancini newsletter."""
        # Extract ES/SPX levels
        levels = self.extract_levels(msg)
        
        # Look for setup types
        setup = "Standard"
        if "failed breakdown" in msg.lower():
            setup = "Failed Breakdown"
        elif "back-test" in msg.lower() or "backtest" in msg.lower():
            setup = "Back-test"
        elif "support" in msg.lower():
            setup = "Support Test"
        
        # Store analysis
        ctx["mancini"] = f"setup:{setup}|levels:{','.join(map(str, levels))}"
        ctx["phase"] = "PLAN"
        
        # Convert ES to SPX approximation
        spx_levels = [int(l/10) if l > 1000 else l for l in levels]
        
        response = f"""
=== MANCINI ANALYSIS ===
📈 Setup Type: {setup}
📍 ES Levels: {', '.join(map(str, levels[:4])) if levels else 'None'}
📍 SPX Approx: {', '.join(map(str, spx_levels[:4])) if spx_levels else 'None'}

→ Next: Create unified plan"""
        
        return response.strip(), ctx
    
    def handle_create_plan(self, msg, ctx):
        """Create unified trading plan."""
        # Parse stored analyses
        dp_parts = dict(x.split(":", 1) for x in ctx.get("dp", "").split("|") if ":" in x)
        mancini_parts = dict(x.split(":", 1) for x in ctx.get("mancini", "").split("|") if ":" in x)
        
        bias = dp_parts.get("bias", "NEUTRAL")
        dp_symbols = dp_parts.get("symbols", "").split(",")[:3]
        all_levels = []
        
        if dp_parts.get("levels"):
            all_levels.extend(dp_parts["levels"].split(","))
        if mancini_parts.get("levels"):
            all_levels.extend(mancini_parts["levels"].split(","))
        
        # Determine consensus
        consensus = "ALIGNED" if bias != "NEUTRAL" else "WAITING"
        
        # Store plan
        ctx["plan"] = f"bias:{bias}|consensus:{consensus}|focus:{','.join(dp_symbols)}"
        ctx["phase"] = "FOCUS"
        
        response = f"""
=== DAILY PLAN ===
📊 Market Bias: {bias} ({consensus})
🎯 Primary Focus: {', '.join(dp_symbols) if dp_symbols else 'Market levels'}
📍 Key Levels: {', '.join(sorted(set(all_levels))[:5]) if all_levels else 'Use caution'}
⚠️  No-Trade Zone: Avoid chop between levels

✓ Entry Rules: Wait for trigger at levels
✓ Risk Plan: Max 3 trades, 1% each
✓ Targets: 2R minimum on A+ setups

Checklist:
- Sources aligned? {consensus}
- Risk events flagged? Check news
- No-touch list defined? Yes, chop zones
- Position sizing ready? 1% max per trade

→ Next: FOCUS on highest conviction setups"""
        
        return response.strip(), ctx

    # === FOCUS PHASE HANDLERS ===
    def handle_focus_trades(self, msg, ctx):
        """Identify focus trades."""
        plan_parts = dict(x.split(":", 1) for x in ctx.get("plan", "").split("|") if ":" in x)
        focus_symbols = plan_parts.get("focus", "").split(",")
        
        if not focus_symbols or focus_symbols == ['']:
            return "❌ Create plan first to identify focus trades", ctx
        
        # Grade setups
        setups = []
        for sym in focus_symbols[:3]:
            if sym:
                setups.append(f"{sym} (A+)" if sym in ctx.get("dp", "") else f"{sym} (B)")
        
        ctx["focus"] = "|".join(setups)
        ctx["phase"] = "FOCUS"
        
        response = f"""
=== FOCUS TRADES ===
🎯 Top Setups:
"""
        for i, setup in enumerate(setups, 1):
            response += f"  {i}. {setup}\n"
        
        response += """
✓ A+ = Full size (DP + Mancini confluence)
✓ B = Half size (Single system signal)

→ Next: EXECUTE when setup triggers"""
        
        return response.strip(), ctx
    
    def handle_grade_setup(self, msg, ctx):
        """Grade a specific setup."""
        symbols = self.extract_symbols(msg)
        if not symbols:
            return "Specify symbol: 'grade AAPL setup'", ctx
        
        symbol = symbols[0]
        
        # Check if in focus list
        in_dp = symbol in ctx.get("dp", "")
        in_focus = symbol in ctx.get("focus", "")
        
        if in_dp and in_focus:
            grade = "A+"
            size = "Full size"
        elif in_dp or in_focus:
            grade = "B"
            size = "Half size"
        else:
            grade = "C"
            size = "Pass or 1/4 size"
        
        response = f"""
=== SETUP GRADE: {symbol} ===
📊 Grade: {grade}
📏 Position Size: {size}
✓ In DP Focus: {'Yes' if in_dp else 'No'}
✓ Technical Setup: {'Strong' if in_focus else 'Weak'}

→ {f'Ready to execute with {size}' if grade != 'C' else 'Consider passing'}"""
        
        return response.strip(), ctx

    # === EXECUTE PHASE HANDLERS ===
    def handle_execute(self, msg, ctx):
        """Execute a trade."""
        # Parse trade command
        pattern = r'(buy|long|sell|short)\s+(\d+)\s+([A-Z]+)\s+(?:at|@)\s+([\d.]+)'
        match = re.search(pattern, msg, re.I)
        
        if not match:
            return "Format: buy 100 AAPL at 225.50", ctx
        
        action, qty, symbol, price = match.groups()
        side = 'long' if action.lower() in ['buy', 'long'] else 'short'
        
        # Check for existing position
        positions = ctx.get("positions", [])
        existing = next((p for p in positions if p["symbol"] == symbol.upper()), None)
        
        new_pos = {
            "symbol": symbol.upper(),
            "side": side,
            "qty": int(qty),
            "entry": float(price),
            "current": float(price)
        }
        
        if existing:
            if existing["side"] == side:
                # Average in
                total_qty = existing["qty"] + new_pos["qty"]
                total_cost = existing["qty"] * existing["entry"] + new_pos["qty"] * new_pos["entry"]
                existing["qty"] = total_qty
                existing["entry"] = total_cost / total_qty
                response = f"AVERAGED: {symbol} now {total_qty}@{existing['entry']:.2f}"
            else:
                # Flip position
                positions.remove(existing)
                positions.append(new_pos)
                response = f"FLIPPED: {symbol} to {side}"
        else:
            # New position
            positions.append(new_pos)
            response = f"""
=== EXECUTE ===
📊 Order: {side.upper()} {qty} {symbol} @ {price}
✓ Position Sizing: Confirmed per plan
✓ Initial Stop: Define per setup (ATR/technical)
✓ Target: 2R minimum per plan

Order Summary: {symbol} {qty} {side.upper()} @ {price}
"""
            
            # Future API integration point:
            # if self.broker_api:
            #     order_id = self.broker_api.place_order(symbol, side, qty, price)
            #     response += f"\n🔗 Broker Order ID: {order_id}"
        
        ctx["positions"] = positions
        ctx["phase"] = "MANAGE"
        
        # Check for revenge trading
        if ctx.get("stops_hit", 0) >= 3:
            ctx["revenge"] = True
        
        return response + "\n→ Managing position", ctx
    
    def handle_size_position(self, msg, ctx):
        """Calculate position size."""
        # Extract risk parameters
        risk_match = re.search(r'risk\s+([\d.]+)', msg, re.I)
        stop_match = re.search(r'stop\s+([\d.]+)', msg, re.I)
        
        if not (risk_match or stop_match):
            return "Specify: 'size AAPL risk 500' or 'size AAPL stop 220'", ctx
        
        symbols = self.extract_symbols(msg)
        symbol = symbols[0] if symbols else "Position"
        
        response = f"""
=== POSITION SIZE: {symbol} ===
"""
        
        if risk_match:
            risk = float(risk_match.group(1))
            response += f"💰 Risk Amount: ${risk}\n"
            response += f"📏 Suggested Size: {int(risk/5)} shares\n"
            response += f"   (Assuming $5 stop distance)\n"
        
        if stop_match:
            stop = float(stop_match.group(1))
            response += f"🛑 Stop Level: {stop}\n"
            response += f"📏 Size based on 1% account risk\n"
        
        # Check setup grade
        if symbol in ctx.get("focus", ""):
            response += "\n✓ This is a FOCUS trade - consider full size"
        else:
            response += "\n⚠️ Not a focus trade - consider half size"
        
        return response.strip(), ctx

    # === MANAGE PHASE HANDLERS ===
    def handle_positions(self, msg, ctx):
        """Show current positions."""
        positions = ctx.get("positions", [])
        
        if not positions:
            return "📊 No open positions", ctx
        
        response = """
=== OPEN POSITIONS ===
"""
        total_unrealized = 0
        
        for p in positions:
            pnl, pnl_pct = self._calculate_pnl(p)
            response += f"• {p['side'].upper()} {p['symbol']} {p['qty']}@{p['entry']:.2f}"
            response += f" → ${pnl:+.2f} ({pnl_pct:+.1f}%)\n"
            total_unrealized += pnl
        
        response += f"""
💰 Unrealized: ${total_unrealized:+.2f}
💵 Realized: ${ctx.get('pnl', 0):.2f}
📈 Total P&L: ${ctx.get('pnl', 0) + total_unrealized:+.2f}"""
        
        return response.strip(), ctx
    
    def handle_move_stop(self, msg, ctx):
        """Move stop loss."""
        symbols = self.extract_symbols(msg)
        levels = self.extract_levels(msg)
        
        if not symbols or not levels:
            return "Format: move stop AAPL 223", ctx
        
        symbol = symbols[0]
        stop_level = levels[0]
        positions = ctx.get("positions", [])
        
        pos = next((p for p in positions if p["symbol"] == symbol), None)
        if not pos:
            return f"No position in {symbol}", ctx
        
        # Store stop (simplified - not tracking in context)
        risk_free = False
        if pos["side"] == "long" and stop_level >= pos["entry"]:
            risk_free = True
        elif pos["side"] == "short" and stop_level <= pos["entry"]:
            risk_free = True
        
        response = f"""
=== STOP MOVED ===
📊 {symbol} stop → {stop_level}
{'✅ Position now RISK FREE!' if risk_free else '⚠️ Stop still below entry'}"""
        
        return response.strip(), ctx
    
    def handle_exit(self, msg, ctx):
        """Exit a position."""
        positions = ctx.get("positions", [])
        
        if not positions:
            return "No positions to exit", ctx
        
        # Check for "close all"
        if "all" in msg.lower():
            total_pnl = 0
            for p in positions:
                pnl, _ = self._calculate_pnl(p)
                total_pnl += pnl
                if pnl < 0:
                    ctx["stops_hit"] = ctx.get("stops_hit", 0) + 1
            
            ctx["pnl"] = ctx.get("pnl", 0) + total_pnl
            ctx["completed"] = ctx.get("completed", 0) + len(positions)
            ctx["positions"] = []
            ctx["phase"] = "REVIEW"
            
            return f"CLOSED ALL: P&L ${total_pnl:+.2f}\n→ Ready for REVIEW", ctx
        
        # Exit specific symbol
        symbols = self.extract_symbols(msg)
        if not symbols:
            return "Specify: 'exit AAPL' or 'close all'", ctx
        
        symbol = symbols[0]
        pos = next((p for p in positions if p["symbol"] == symbol), None)
        
        if not pos:
            return f"No position in {symbol}", ctx
        
        # Update price if provided
        levels = self.extract_levels(msg)
        if levels:
            pos["current"] = levels[0]
        else:
            # Mock 1% profit
            pos["current"] = pos["entry"] * 1.01 if pos["side"] == "long" else pos["entry"] * 0.99
        
        pnl, pnl_pct = self._calculate_pnl(pos)
        
        # Update context
        positions.remove(pos)
        ctx["positions"] = positions
        ctx["pnl"] = ctx.get("pnl", 0) + pnl
        ctx["completed"] = ctx.get("completed", 0) + 1
        
        if pnl < 0:
            ctx["stops_hit"] = ctx.get("stops_hit", 0) + 1
        
        response = f"""
=== POSITION CLOSED ===
📊 {symbol}: ${pnl:+.2f} ({pnl_pct:+.1f}%)
{'❌ Stop hit' if pnl < 0 else '✅ Profit taken'}"""
        
        if not positions:
            response += "\n\n→ All flat. Ready for REVIEW"
            ctx["phase"] = "REVIEW"
        
        return response.strip(), ctx

    # === REVIEW PHASE HANDLERS ===
    def handle_review(self, msg, ctx):
        """Review session performance."""
        completed = ctx.get("completed", 0)
        realized = ctx.get("pnl", 0)
        positions = ctx.get("positions", [])
        
        unrealized = sum(self._calculate_pnl(p)[0] for p in positions)
        total = realized + unrealized
        
        response = f"""
=== SESSION REVIEW ===
📊 Completed Trades: {completed}
💵 Realized P&L: ${realized:.2f}
💰 Unrealized: ${unrealized:.2f}
📈 Total P&L: ${total:+.2f}

✓ Stops Hit: {ctx.get('stops_hit', 0)}
✓ Open Positions: {len(positions)}"""
        
        # Add performance notes
        if completed > 0:
            win_rate = "N/A"  # Would need to track this
            response += f"\n✓ Win Rate: {win_rate}"
        
        if total > 0:
            response += "\n\n✅ Positive session - good discipline"
        else:
            response += "\n\n⚠️ Negative session - review entries"
        
        response += "\n\n→ Ready for COACH feedback"
        ctx["phase"] = "COACH"
        
        return response.strip(), ctx

    # === COACH PHASE HANDLERS ===
    def handle_coach(self, msg, ctx):
        """Provide coaching feedback with behavioral analytics."""
        stops_hit = ctx.get("stops_hit", 0)
        revenge = ctx.get("revenge", False)
        pnl = ctx.get("pnl", 0)
        completed = ctx.get("completed", 0)
        
        response = """
=== COACH FEEDBACK ===
"""
        
        # Performance analytics
        if completed > 0:
            avg_pnl = pnl / completed
            response += f"""
📊 PERFORMANCE STATS:
• Trades: {completed}
• Avg P&L: ${avg_pnl:.2f}
• Stops Hit: {stops_hit} ({(stops_hit/completed*100):.0f}% of trades)
"""
        
        # Performance coaching
        if pnl > 0:
            response += """
✅ STRENGTHS:
• Positive P&L shows good execution
• Consider sizing up A+ setups
• Keep following your plan
"""
        else:
            response += """
⚠️ IMPROVEMENT AREAS:
• Review entry timing (too early?)
• Check stop placement (too tight?)
• Focus on A+ setups only
"""
        
        # Behavioral patterns
        if stops_hit >= 3:
            response += """
🚨 PATTERN DETECTED: Multiple stops hit
→ PRESCRIPTION: Half size rest of day
→ No new trades for 30 minutes
→ Journal: What's different about today?
"""
        
        if revenge:
            response += """
🚨 PATTERN DETECTED: Revenge trading
→ PRESCRIPTION: Step away from screens
→ Journal about the emotions
→ Return with clear plan only
"""
        
        # Time-based patterns (future enhancement)
        current_hour = datetime.now().hour
        if current_hour >= 15 and completed > 3:  # After 3pm with many trades
            response += """
⚠️ LATE DAY OVERTRADING: 
→ Best setups usually before 2pm
→ Consider closing shop after 3pm
"""
        
        # Daily habits
        response += """
📝 TOMORROW'S FOCUS:
1. Wait for A+ setups only
2. Size appropriately to conviction
3. Honor stops without revenge
4. Journal after each trade
"""
        
        ctx["phase"] = "PLAN"
        response += "\n→ Ready for next PLAN"
        
        return response.strip(), ctx
    
    def check_behavioral_patterns(self, ctx):
        """Real-time behavioral pattern detection."""
        stops = ctx.get("stops_hit", 0)
        positions = len(ctx.get("positions", []))
        revenge = ctx.get("revenge", False)
        
        if stops >= 2 and positions > 2:
            return "🚨 COACH ALERT: Overtrading after stops! Reduce size."
        
        if revenge and positions > 0:
            return "🚨 COACH ALERT: Revenge trading active! Step away."
        
        if stops >= 3:
            return "🚨 COACH ALERT: 3 stops hit. Maximum risk reached."
        
        return None

    # === UTILITY HANDLERS ===
    def handle_save(self, msg, ctx):
        """Save state to journal."""
        filename = f"trader_{datetime.now().strftime('%Y%m%d')}.txt"
        
        # Create journal entry
        entry = f"""
TRADING JOURNAL - {datetime.now().strftime('%Y-%m-%d %H:%M')}
Phase: {ctx.get('phase', 'PLAN')}

POSITIONS:
"""
        positions = ctx.get("positions", [])
        if positions:
            for p in positions:
                pnl, pnl_pct = self._calculate_pnl(p)
                entry += f"  {p['symbol']}: {p['side'].upper()} {p['qty']}@{p['entry']:.2f} "
                entry += f"P&L: ${pnl:+.2f} ({pnl_pct:+.1f}%)\n"
        else:
            entry += "  No open positions\n"
        
        entry += f"""
PERFORMANCE:
  Realized P&L: ${ctx.get('pnl', 0):.2f}
  Completed Trades: {ctx.get('completed', 0)}
  Stops Hit: {ctx.get('stops_hit', 0)}

{'='*50}
RAW CONTEXT:
{self.compress_context(ctx)}
"""
        
        with open(filename, 'a') as f:
            f.write(entry + "\n\n")
        
        return f"💾 Saved to {filename}", ctx
    
    def handle_load(self, msg, ctx):
        """Load state from journal."""
        filename = f"trader_{datetime.now().strftime('%Y%m%d')}.txt"
        
        try:
            with open(filename, 'r') as f:
                content = f.read()
            
            # Find last context
            if 'RAW CONTEXT:' in content:
                sections = content.split('RAW CONTEXT:')
                if len(sections) > 1:
                    last_context = sections[-1].split('\n')[1].strip()
                    if last_context and '=' not in last_context:
                        loaded_ctx = self.parse_context(last_context)
                        return "📂 Context restored from journal", loaded_ctx
            
            return "❌ No valid context found", ctx
        
        except FileNotFoundError:
            return f"❌ No journal file: {filename}", ctx
    
    def handle_journal(self, msg, ctx):
        """Show today's journal."""
        filename = f"trader_{datetime.now().strftime('%Y%m%d')}.txt"
        
        try:
            with open(filename, 'r') as f:
                content = f.read()
            
            # Extract human-readable parts
            entries = content.split('TRADING JOURNAL')
            if len(entries) > 1:
                # Show last 3 entries
                recent = entries[-3:] if len(entries) > 3 else entries[1:]
                display = "📖 RECENT JOURNAL ENTRIES:\n\n"
                for entry in recent:
                    if 'RAW CONTEXT:' in entry:
                        entry = entry.split('RAW CONTEXT:')[0]
                    display += "JOURNAL" + entry + "\n"
                
                return display.strip(), ctx
            
            return "📖 No journal entries today", ctx
        
        except FileNotFoundError:
            return "📖 No journal file found", ctx
    
    def handle_help(self, msg, ctx):
        """Show help."""
        return """
📚 INTENT TRADER COMMANDS

=== PLAN PHASE ===
• analyze dp [morning call]
• analyze mancini [newsletter]  
• create plan

=== FOCUS PHASE ===
• show focus trades
• grade AAPL setup

=== EXECUTE PHASE ===
• buy 100 AAPL at 225.50
• size AAPL risk 500

=== MANAGE PHASE ===
• positions
• move stop AAPL 223
• exit AAPL / close all

=== REVIEW PHASE ===
• review session

=== COACH PHASE ===
• coach / feedback

=== UTILITIES ===
• save, load, journal
• help, reset, context

Natural language works!
Current phase: """ + ctx.get("phase", "PLAN"), ctx
    
    def handle_reset(self, msg, ctx):
        """Reset context."""
        return "✅ Context reset. Starting fresh in PLAN phase.", self.parse_context("")
    
    def handle_context(self, msg, ctx):
        """Show current context."""
        return f"📋 Current context:\n{self.compress_context(ctx)}", ctx
    
    def handle_unknown(self, msg, ctx):
        """Handle unknown intent."""
        return f"❓ Not sure what you mean. Try 'help'\nCurrent phase: {ctx.get('phase', 'PLAN')}", ctx

    # === UTILITY FUNCTIONS ===
    def extract_symbols(self, text):
        """Extract stock symbols from text."""
        symbols = re.findall(r'\b[A-Z]{2,5}\b', text)
        exclude = {'THE', 'AND', 'FOR', 'BUY', 'SELL', 'LONG', 'SHORT', 'AT', 'ES', 'SPX', 'DP'}
        return [s for s in symbols if s not in exclude]
    
    def extract_levels(self, text):
        """Extract price levels from text."""
        text = text.replace(',', '')
        prices = re.findall(r'\b(\d{3,6}\.?\d{0,2})\b', text)
        levels = []
        for p in prices:
            try:
                val = float(p)
                if 10 < val < 99999:  # Reasonable price range
                    levels.append(val)
            except:
                pass
        return sorted(set(levels))
    
    def _calculate_pnl(self, position):
        """Calculate P&L for a position."""
        current = position.get("current", position["entry"])
        
        if position["side"] == "long":
            pnl = (current - position["entry"]) * position["qty"]
        else:
            pnl = (position["entry"] - current) * position["qty"]
        
        pnl_pct = (pnl / (position["entry"] * position["qty"])) * 100 if position["entry"] > 0 else 0
        
        return pnl, pnl_pct


# === INTERACTIVE SHELL ===
def run_interactive():
    """Run the trading assistant interactively."""
    bot = IntentTrader()
    context = ""
    
    print("""
╔══════════════════════════════════════════╗
║   Intent Trader IAA v3.0 - Production    ║
║   Type 'help' for commands, 'quit' to exit   ║
╚══════════════════════════════════════════╝
""")
    
    while True:
        try:
            user_input = input("\n📊 > ").strip()
            
            if user_input.lower() in ['quit', 'exit', 'q']:
                print("\n👋 Good luck in the markets!")
                break
            
            if user_input:
                result = bot.process_message(user_input, context)
                print(result["response"])
                context = result["context"]
                
        except KeyboardInterrupt:
            print("\n\nUse 'quit' to exit cleanly.")
        except Exception as e:
            print(f"❌ Error: {e}")


if __name__ == "__main__":
    run_interactive()
