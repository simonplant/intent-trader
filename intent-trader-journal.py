"""
Intent Trader IAA - Save/Load Enhancement
Makes your daily files human-readable journals
"""

def save_state_journal(self, context: str, filename: str = None):
    """Save context AND human-readable journal entry"""
    if not filename:
        filename = f"trader_{datetime.now().strftime('%Y%m%d')}.txt"
    
    # Parse context for journal
    ctx = self.parse_context(context)
    positions = ctx.get('positions', {})
    phase = ctx.get('phase', 'PLAN')
    
    # Create journal entry
    journal = f"""TRADING JOURNAL - {datetime.now().strftime('%Y-%m-%d %H:%M')}
Phase: {phase}

POSITIONS:
"""
    for symbol, details in positions.items():
        journal += f"  {symbol}: {details}\n"
    
    if not positions:
        journal += "  No open positions\n"
    
    # Add completed trades if any
    completed = ctx.get('completed_trades', [])
    if completed:
        journal += "\nCOMPLETED TRADES:\n"
        total_pnl = 0
        for trade in completed:
            parts = trade.split(',')
            if len(parts) >= 5:
                journal += f"  {parts[0]}: {parts[1]} P&L: ${parts[4]}\n"
                total_pnl += float(parts[4])
        journal += f"  Total P&L: ${total_pnl:.2f}\n"
    
    # Add any coaching notes
    coach_notes = ctx.get('coach_feedback', [])
    if coach_notes:
        journal += "\nCOACH NOTES:\n"
        for note in coach_notes[-3:]:  # Last 3 notes
            journal += f"  - {note}\n"
    
    journal += f"\n{'='*50}\nRAW CONTEXT:\n{context}\n"
    
    # Write to file
    with open(filename, 'a') as f:  # Append mode!
        f.write(journal)
        f.write("\n\n")
    
    return f"💾 Saved to {filename}"

def load_state_latest(self, filename: str = None) -> tuple:
    """Load the most recent context from journal file"""
    if not filename:
        filename = f"trader_{datetime.now().strftime('%Y%m%d')}.txt"
    
    try:
        with open(filename, 'r') as f:
            content = f.read()
        
        # Find the last RAW CONTEXT section
        if 'RAW CONTEXT:' in content:
            # Split by RAW CONTEXT and get the last one
            sections = content.split('RAW CONTEXT:')
            if len(sections) > 1:
                last_context = sections[-1].split('\n')[1].strip()
                if last_context and last_context != '=' * 50:
                    # Find the timestamp of this entry
                    last_section = content.split('TRADING JOURNAL - ')[-1]
                    timestamp = last_section.split('\n')[0]
                    return last_context, f"📂 Loaded from {timestamp}"
        
        return "", "❌ No valid context found in journal"
    
    except FileNotFoundError:
        return "", f"❌ No journal found: {filename}"

# Quick add to show journal
def show_journal(self, filename: str = None) -> str:
    """Display today's journal entries"""
    if not filename:
        filename = f"trader_{datetime.now().strftime('%Y%m%d')}.txt"
    
    try:
        with open(filename, 'r') as f:
            content = f.read()
        
        # Just show the human-readable parts (skip raw context)
        clean_content = ""
        for line in content.split('\n'):
            if line.startswith('RAW CONTEXT:'):
                clean_content += "...\n"
                # Skip until next journal entry or end
                continue
            if not line.startswith('positions:') and not line.startswith('phase:'):
                clean_content += line + '\n'
        
        return f"📖 TODAY'S JOURNAL:\n{clean_content}"
    
    except FileNotFoundError:
        return "📖 No journal entries today"

# Usage in process_message:
if message.lower() == 'journal':
    return {
        'response': self.show_journal(),
        'context': context,
        'intent': 'show_journal'
    }
