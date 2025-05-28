"""
Intent Trader v1.0.0
A minimalist chat-native trading assistant in ~300 lines.
No dependencies. No bloat. Just pure trading logic.
"""

import re
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass
from enum import Enum

# Core Types
class Phase(Enum):
    PLAN = "PLAN"
    FOCUS = "FOCUS" 
    EXECUTE = "EXECUTE"
    MANAGE = "MANAGE"
    REVIEW = "REVIEW"
    COACH = "COACH"

@dataclass
class Position:
    symbol: str
    side: str  # 'long' or 'short'
    qty: int
    entry: float
    current: float = None
    
    def __post_init__(self):
        if self.current is None:
            self.current = self.entry
    
    @property
    def pnl(self) -> float:
        if self.side == 'long':
            return (self.current - self.entry) * self.qty
        else:
            return (self.entry - self.current) * self.qty
    
    @property
    def pnl_pct(self) -> float:
        if self.entry > 0:
            return (self.pnl / (self.entry * self.qty)) * 100
        return 0.0

# Main Entry Point
def process(msg: str, ctx: str = "") -> Dict[str, str]:
    """Process trading message with context"""
    try:
        # Parse context
        context = parse_context(ctx)
        
        # Detect intent
        intent = detect_intent(msg.lower(), context['phase'])
        
        # Route to handler
        handlers = {
            Phase.PLAN: handle_plan,
            Phase.FOCUS: handle_focus,
            Phase.EXECUTE: handle_execute,
            Phase.MANAGE: handle_manage,
            Phase.REVIEW: handle_review,
            Phase.COACH: handle_coach
        }
        
        # Process
        response, new_context = handlers[intent](msg, context)
        
        # Return
        return {
            'response': response,
            'context': compress_context(new_context),
            'phase': intent.value
        }
    except Exception as e:
        return {
            'response': f"Error: {str(e)}\nTry: 'buy 100 AAPL at 225.50'",
            'context': ctx,
            'phase': 'ERROR'
        }

# Context Management
def parse_context(ctx_str: str) -> Dict:
    """Parse context string into working data"""
    if not ctx_str:
        return {'phase': Phase.PLAN, 'positions': [], 'pnl': 0.0}
    
    context = {'positions': [], 'pnl': 0.0}
    
    for part in ctx_str.split('|'):
        if part.startswith('P:'):
            try:
                context['phase'] = Phase(part[2:])
            except:
                context['phase'] = Phase.PLAN
                
        elif part.startswith('POS:') and len(part) > 4:
            # Format: AAPL:L100@225.50/230.00
            for pos_str in part[4:].split(','):
                match = re.match(r'([A-Z]+):([LS])(\d+)@([\d.]+)(?:/([\d.]+))?', pos_str)
                if match:
                    sym, side, qty, entry, current = match.groups()
                    context['positions'].append(Position(
                        symbol=sym,
                        side='long' if side == 'L' else 'short',
                        qty=int(qty),
                        entry=float(entry),
                        current=float(current) if current else float(entry)
                    ))
                    
        elif part.startswith('PNL:'):
            try:
                context['pnl'] = float(part[4:])
            except:
                context['pnl'] = 0.0
    
    return context

def compress_context(ctx: Dict) -> str:
    """Compress context to minimal string"""
    pos_strs = []
    for p in ctx['positions']:
        s = 'L' if p.side == 'long' else 'S'
        pos_str = f"{p.symbol}:{s}{p.qty}@{p.entry:.2f}"
        if p.current != p.entry:
            pos_str += f"/{p.current:.2f}"
        pos_strs.append(pos_str)
    
    pos = ','.join(pos_strs)
    return f"P:{ctx['phase'].value}|POS:{pos}|PNL:{ctx['pnl']:.2f}"

# Intent Detection
def detect_intent(msg: str, current_phase: Phase) -> Phase:
    """Detect trading intent from message"""
    patterns = {
        Phase.PLAN: ['morning', 'plan', 'levels', 'bias', 'market', 'analysis'],
        Phase.FOCUS: ['focus', 'setup', 'watch', 'looking', 'target'],
        Phase.EXECUTE: ['buy', 'sell', 'long', 'short', 'bought', 'sold', 'enter'],
        Phase.MANAGE: ['stop', 'target', 'exit', 'close', 'adjust', 'trail'],
        Phase.REVIEW: ['review', 'pnl', 'performance', 'how did', 'results'],
        Phase.COACH: ['improve', 'learn', 'coach', 'better', 'feedback']
    }
    
    # Score each phase
    scores = {}
    for phase, keywords in patterns.items():
        scores[phase] = sum(1 for kw in keywords if kw in msg)
    
    # Get best match
    best = max(scores.items(), key=lambda x: x[1])
    
    # Return best match or current phase if no clear winner
    return best[0] if best[1] > 0 else current_phase

# Phase Handlers
def handle_plan(msg: str, ctx: Dict) -> Tuple[str, Dict]:
    """PLAN: Market analysis and bias"""
    symbols = extract_symbols(msg)
    levels = extract_levels(msg)
    
    response = f"PLAN: {', '.join(symbols) if symbols else 'Market analysis'}"
    if levels:
        response += f"\nLevels: {', '.join(str(l) for l in levels)}"
    response += "\n→ Next: FOCUS on specific setups"
    
    ctx['phase'] = Phase.PLAN
    return response, ctx

def handle_focus(msg: str, ctx: Dict) -> Tuple[str, Dict]:
    """FOCUS: Setup identification"""
    symbols = extract_symbols(msg)
    setup = "breakout" if "break" in msg else "setup"
    
    response = f"FOCUS: {symbols[0] if symbols else 'Watching'} {setup}"
    response += "\n→ Next: EXECUTE when triggered"
    
    ctx['phase'] = Phase.FOCUS
    return response, ctx

def handle_execute(msg: str, ctx: Dict) -> Tuple[str, Dict]:
    """EXECUTE: Position entry"""
    pos_data = parse_position(msg)
    
    if pos_data:
        # Create position
        pos = Position(**pos_data)
        
        # Check for existing position in symbol
        existing = next((p for p in ctx['positions'] if p.symbol == pos.symbol), None)
        if existing:
            # Handle position already exists
            if existing.side == pos.side:
                # Average in
                total_qty = existing.qty + pos.qty
                total_cost = existing.qty * existing.entry + pos.qty * pos.entry
                existing.qty = total_qty
                existing.entry = total_cost / total_qty
                response = f"AVERAGED: {pos.symbol} now {existing.qty}@{existing.entry:.2f}"
            else:
                # Reduce or flip
                if existing.qty > pos.qty:
                    existing.qty -= pos.qty
                    response = f"REDUCED: {pos.symbol} to {existing.qty}"
                elif existing.qty < pos.qty:
                    ctx['positions'].remove(existing)
                    pos.qty -= existing.qty
                    ctx['positions'].append(pos)
                    response = f"FLIPPED: {pos.symbol} to {pos.side}"
                else:
                    ctx['positions'].remove(existing)
                    response = f"FLAT: {pos.symbol} position closed"
        else:
            # New position
            ctx['positions'].append(pos)
            response = f"EXECUTED: {pos.side.upper()} {pos.symbol} {pos.qty}@{pos.entry:.2f}"
        
        response += "\n→ Managing position"
        ctx['phase'] = Phase.MANAGE
    else:
        response = "Format: buy/sell [qty] [symbol] at [price]\nExample: buy 100 AAPL at 225.50"
    
    return response, ctx

def handle_manage(msg: str, ctx: Dict) -> Tuple[str, Dict]:
    """MANAGE: Position management"""
    if not ctx['positions']:
        return "No positions to manage", ctx
    
    # Check for exit command
    exit_match = re.search(r'(?:exit|close)\s+([A-Z]+)', msg, re.I)
    if exit_match:
        symbol = exit_match.group(1).upper()
        pos = next((p for p in ctx['positions'] if p.symbol == symbol), None)
        
        if pos:
            # Update price (mock 1% profit for demo)
            pos.current = pos.entry * 1.01 if pos.side == 'long' else pos.entry * 0.99
            pnl = pos.pnl
            
            # Remove position and update realized PNL
            ctx['positions'].remove(pos)
            ctx['pnl'] += pnl
            
            response = f"CLOSED: {symbol} P&L: ${pnl:.2f} ({pos.pnl_pct:+.1f}%)"
            
            if not ctx['positions']:
                response += "\n→ All flat. Ready for REVIEW"
                ctx['phase'] = Phase.REVIEW
        else:
            response = f"No position in {symbol}"
    else:
        # Display positions
        response = "POSITIONS:\n"
        total_unrealized = 0
        
        for p in ctx['positions']:
            response += f"• {p.side.upper()} {p.symbol} {p.qty}@{p.entry:.2f} "
            response += f"P&L: ${p.pnl:+.2f} ({p.pnl_pct:+.1f}%)\n"
            total_unrealized += p.pnl
        
        response += f"\nUnrealized: ${total_unrealized:+.2f}"
        response += "\nCommands: 'exit SYMBOL' or 'close all'"
    
    return response, ctx

def handle_review(msg: str, ctx: Dict) -> Tuple[str, Dict]:
    """REVIEW: Performance analysis"""
    unrealized = sum(p.pnl for p in ctx['positions'])
    total = ctx['pnl'] + unrealized
    
    response = "REVIEW: Session Performance\n"
    response += f"• Realized: ${ctx['pnl']:.2f}\n"
    response += f"• Unrealized: ${unrealized:.2f}\n"
    response += f"• Total: ${total:+.2f}\n"
    response += f"• Open: {len(ctx['positions'])} positions\n"
    response += "→ Ready for COACH insights"
    
    ctx['phase'] = Phase.REVIEW
    return response, ctx

def handle_coach(msg: str, ctx: Dict) -> Tuple[str, Dict]:
    """COACH: Trading insights"""
    total_pnl = ctx['pnl'] + sum(p.pnl for p in ctx['positions'])
    
    response = "COACH: Session Insights\n"
    
    if total_pnl > 0:
        response += "✓ Positive session - maintain discipline\n"
        response += "✓ Consider: Scaling winners, tighter stops"
    else:
        response += "→ Review: Entry timing, stop placement\n"
        response += "→ Focus: Higher quality setups, better R:R"
    
    response += "\n→ Ready for next PLAN"
    ctx['phase'] = Phase.PLAN
    
    return response, ctx

# Utility Functions
def extract_symbols(text: str) -> List[str]:
    """Extract stock symbols from text"""
    symbols = re.findall(r'\b[A-Z]{1,5}\b', text)
    exclude = {'THE', 'AND', 'FOR', 'BUY', 'SELL', 'LONG', 'SHORT', 'AT', 'TO', 'IS', 'OR'}
    return [s for s in symbols if s not in exclude and len(s) >= 2]

def extract_levels(text: str) -> List[float]:
    """Extract price levels from text"""
    prices = re.findall(r'\b(\d{1,6}\.?\d{0,2})\b', text)
    levels = []
    for p in prices:
        try:
            val = float(p)
            if 0.01 < val < 999999:
                levels.append(val)
        except:
            pass
    return sorted(set(levels))

def parse_position(text: str) -> Optional[Dict]:
    """Parse position entry from text"""
    # Remove commas from numbers
    text = text.replace(',', '')
    
    # Try multiple patterns
    patterns = [
        # buy 100 AAPL at 225.50
        r'(buy|sell|long|short)\s+(\d+)\s+([A-Z]{1,5})\s+(?:at|@)\s+([\d.]+)',
        # bought 100 shares of AAPL at 225
        r'(bought|sold)\s+(\d+)\s+(?:shares?\s+of\s+)?([A-Z]{1,5})\s+(?:at|@)\s+([\d.]+)',
        # AAPL 100 @ 225.50
        r'([A-Z]{1,5})\s+(\d+)\s*@\s*([\d.]+)'
    ]
    
    for pattern in patterns:
        match = re.search(pattern, text, re.I)
        if match:
            groups = match.groups()
            
            if len(groups) == 4:
                action, qty, symbol, price = groups
                side = 'long' if action.lower() in ['buy', 'bought', 'long'] else 'short'
            else:  # 3 groups - symbol first pattern
                symbol, qty, price = groups
                # Infer side from context words
                side = 'short' if any(w in text.lower() for w in ['sell', 'short', 'sold']) else 'long'
            
            return {
                'symbol': symbol.upper(),
                'side': side,
                'qty': int(qty),
                'entry': float(price)
            }
    
    return None

# Test Function
def test():
    """Run basic test flow"""
    print("=== Intent Trader v1.0.0 Test ===\n")
    
    context = ""
    test_flow = [
        "Morning - ES bullish above 5900, watching AAPL at 225",
        "Focus on ES breakout above 5905",
        "buy 2 ES at 5906",
        "How are my positions?",
        "exit ES",
        "review my session",
        "coach me on today's trading"
    ]
    
    for msg in test_flow:
        result = process(msg, context)
        print(f"You: {msg}")
        print(f"Bot: {result['response']}\n")
        context = result['context']
    
    print(f"Final context: {context}")

# Run test if called directly
if __name__ == "__main__":
    test()