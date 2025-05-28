# Enhanced handle_analyze_dp with conviction scoring
def handle_analyze_dp_enhanced(self, msg, ctx):
    """Analyze DP morning call with conviction scoring."""
    # Extract ideas with scores
    ideas = self.extract_dp_ideas_with_scores(msg)
    
    # Focus trades (0.90+)
    focus_trades = [i for i in ideas if i['conviction'] >= 0.90]
    high_conviction = [i for i in ideas if 0.70 <= i['conviction'] < 0.90]
    
    # Store enhanced analysis
    focus_symbols = [i['symbol'] for i in focus_trades]
    all_symbols = [i['symbol'] for i in ideas[:10]]  # Top 10
    
    ctx["dp"] = f"focus:{','.join(focus_symbols)}|all:{','.join(all_symbols)}"
    ctx["dp_scores"] = {i['symbol']: i['conviction'] for i in ideas}  # Store scores
    ctx["phase"] = "PLAN"
    
    response = f"""
=== DP ANALYSIS WITH CONVICTION SCORES ===

🎯 FOCUS TRADES (0.90+):
"""
    for idea in focus_trades[:3]:
        response += f"  • {idea['symbol']} ({idea['conviction']}) - {idea['context']}\n"
    
    if not focus_trades:
        response += "  None today - no exceptional conviction\n"
    
    response += f"""
💪 HIGH CONVICTION (0.70-0.89):
"""
    for idea in high_conviction[:3]:
        response += f"  • {idea['symbol']} ({idea['conviction']})\n"
    
    response += f"""
📊 Summary: {len(focus_trades)} focus trades, {len(high_conviction)} high conviction

→ Next: Analyze Mancini for technical confluence"""
    
    return response.strip(), ctx

# Enhanced handle_create_plan with unified scoring
def handle_create_plan_enhanced(self, msg, ctx):
    """Create unified plan with combined conviction scores."""
    # Get DP scores
    dp_scores = ctx.get('dp_scores', {})
    
    # Mock current prices and identify FB setups
    # In production, these would come from market data
    current_prices = {
        'QQQ': 495.20,
        'AAPL': 225.50,
        'TSLA': 180.30
    }
    
    # Get Mancini levels
    mancini_parts = dict(x.split(":", 1) for x in ctx.get("mancini", "").split("|") if ":" in x)
    levels = [float(l) for l in mancini_parts.get("levels", "").split(",") if l]
    
    # Find FB setups
    fb_setups = self.identify_fb_setups(levels, current_prices)
    
    # Calculate combined scores
    unified_setups = []
    for symbol in dp_scores:
        dp_score = dp_scores[symbol]
        
        # Check if has FB setup
        fb_setup = next((s for s in fb_setups if s['symbol'] == symbol), None)
        mancini_score = 0.85 if fb_setup else 0.50
        
        combined = self.calculate_combined_score(dp_score, mancini_score)
        
        unified_setups.append({
            'symbol': symbol,
            'dp_score': dp_score,
            'mancini_score': mancini_score,
            'combined': combined,
            'fb_level': fb_setup['level'] if fb_setup else None
        })
    
    # Sort by combined score
    unified_setups.sort(key=lambda x: x['combined'], reverse=True)
    
    # Detect market mode
    mode_info = self.classify_market_mode([])  # Would pass real price data
    
    response = f"""
=== UNIFIED TRADING PLAN ===

📊 Market Mode: Mode {mode_info['mode']} ({mode_info['confidence']:.0%} confidence)
Strategy: {mode_info['strategy']}

🎯 TOP UNIFIED SETUPS:
"""
    
    for i, setup in enumerate(unified_setups[:3], 1):
        response += f"""
{i}. {setup['symbol']} - Combined Score: {setup['combined']}
   • DP: {setup['dp_score']} | Mancini: {setup['mancini_score']}"""
        if setup['fb_level']:
            response += f"\n   • Failed Breakdown at {setup['fb_level']}"
        response += "\n"
    
    # Store plan
    focus = [s['symbol'] for s in unified_setups if s['combined'] >= 0.85]
    ctx["plan"] = f"mode:{mode_info['mode']}|focus:{','.join(focus)}"
    ctx["unified_scores"] = {s['symbol']: s['combined'] for s in unified_setups}
    ctx["phase"] = "FOCUS"
    
    response += f"""
✅ Execution Rules:
• Combined 0.85+ = Full size
• Combined 0.70-0.84 = Half size  
• Below 0.70 = Pass or minimal
• Mode {mode_info['mode']} sizing adjustments apply

→ Next: FOCUS on setups when market opens"""
    
    return response.strip(), ctx

# Enhanced handle_coach with pattern detection
def handle_coach_enhanced(self, msg, ctx):
    """Enhanced coaching with behavioral pattern detection."""
    # Detect patterns
    patterns = self.detect_trading_patterns(ctx)
    
    # Calculate behavioral score
    behav_score = self.calculate_behavioral_score(ctx)
    
    # Generate alerts
    alerts = self.generate_pattern_alerts(patterns)
    
    response = f"""
=== COACH FEEDBACK ===

📊 Behavioral Score: {behav_score}/100
"""
    
    # Add pattern alerts if any
    if alerts:
        response += alerts
    
    # Performance analysis
    pnl = ctx.get('pnl', 0)
    completed = ctx.get('completed', 0)
    
    if completed > 0:
        response += f"""
📈 PERFORMANCE METRICS:
• Trades: {completed}
• P&L: ${pnl:.2f}
• Avg per trade: ${pnl/completed:.2f}
"""
    
    # Prescriptive coaching based on score
    if behav_score >= 80:
        response += """
✅ EXCELLENT DISCIPLINE
• Maintaining trading plan
• Following risk rules
• Keep this consistency!
"""
    elif behav_score >= 60:
        response += """
⚠️ SOME CONCERNS
• Review detected patterns above
• Tighten discipline on rules
• Consider smaller size rest of day
"""
    else:
        response += """
🔴 DISCIPLINE BREAKDOWN
• Stop trading for today
• Journal about what happened
• Return tomorrow with fresh mindset
"""
    
    # Tomorrow's focus
    response += """
📝 TOMORROW'S PROCESS:
1. Review today's patterns
2. Set behavioral goals (not P&L goals)
3. Focus on process over profits
4. Maximum 3-4 A+ trades only
"""
    
    ctx["phase"] = "PLAN"
    return response.strip(), ctx