"""
Trading Bot Launcher - Drop this at the bottom of your intent_trader.py file
Or save as a separate launcher.py that imports your bot
"""

# Add this to the bottom of your intent_trader.py file:

def run_interactive():
    """Interactive shell for the trading bot."""
    bot = IntentTraderAssistant()
    context = ""
    
    print("="*50)
    print("🚀 Intent Trader IAA - Interactive Mode")
    print("="*50)
    print("Type 'help' for commands, 'quit' to exit")
    print("Current phase: PLAN")
    print("-"*50)
    
    while True:
        try:
            # Get user input
            user_input = input("\n📊 > ").strip()
            
            # Check for special commands
            if user_input.lower() in ['quit', 'exit', 'q']:
                print("\n👋 Closing trading session. Good luck!")
                break
            
            elif user_input.lower() in ['help', 'h', '?']:
                print_help()
                continue
            
            elif user_input.lower() == 'clear':
                import os
                os.system('cls' if os.name == 'nt' else 'clear')
                continue
            
            elif user_input.lower() == 'context':
                print(f"\n📋 Current context:\n{context}")
                continue
            
            elif user_input.lower() == 'reset':
                context = ""
                print("✅ Context reset. Starting fresh in PLAN phase.")
                continue
            
            # Process trading command
            if user_input:
                response = bot.process_message(user_input, context)
                print(f"\n{response['response']}")
                context = response['context']
                
                # Show phase if changed
                ctx = bot.parse_context(context)
                print(f"\n[Phase: {ctx.get('phase', 'PLAN')}]")
        
        except KeyboardInterrupt:
            print("\n\n👋 Interrupted. Type 'quit' to exit cleanly.")
        except Exception as e:
            print(f"\n❌ Error: {e}")

def print_help():
    """Print help information."""
    help_text = """
📚 HELP - Intent Trader Commands

PLAN PHASE:
  • analyze dp: [paste morning call]
  • analyze mancini: [paste newsletter]  
  • create plan / make trading plan

FOCUS PHASE:
  • show levels / extract levels
  • focus ideas / best setups

EXECUTE PHASE:
  • buy 100 AAPL at 225.50
  • short 50 MSFT at 380
  • size AAPL stop 220

MANAGE PHASE:
  • show positions / list portfolio
  • move stop AAPL 223
  • close AAPL at 230

REVIEW PHASE:
  • session review / how did I do
  • log trade

UTILITY COMMANDS:
  • help - Show this help
  • context - Show raw context
  • reset - Start fresh
  • clear - Clear screen
  • quit - Exit

TIPS:
  • Natural language works: "what are my positions?"
  • No need for exact syntax
  • Context persists between commands
"""
    print(help_text)

def run_script_mode(commands_file: str):
    """Run commands from a file."""
    bot = IntentTraderAssistant()
    context = ""
    
    print(f"📄 Running commands from: {commands_file}")
    print("="*50)
    
    try:
        with open(commands_file, 'r') as f:
            commands = f.readlines()
        
        for i, cmd in enumerate(commands, 1):
            cmd = cmd.strip()
            if cmd and not cmd.startswith('#'):  # Skip empty lines and comments
                print(f"\n[{i}] > {cmd}")
                response = bot.process_message(cmd, context)
                print(response['response'])
                context = response['context']
                print("-"*30)
        
        print("\n✅ Script complete!")
        return context
        
    except FileNotFoundError:
        print(f"❌ File not found: {commands_file}")
        return ""

def run_quick_workflow():
    """Run a quick morning workflow."""
    bot = IntentTraderAssistant()
    context = ""
    
    print("🌅 Running Quick Morning Workflow...")
    print("="*50)
    
    # Sample morning workflow
    workflow = [
        ("Analyzing DP morning call...", 
         "analyze dp: Futures lower on CPI. Bullish above 5900. Like TEM above 60, HOOD above 56."),
        ("Creating trade plan...", 
         "create trading plan"),
        ("Checking focus ideas...", 
         "show focus ideas"),
        ("Sizing first position...", 
         "size TEM stop 59 entry 61"),
    ]
    
    for description, command in workflow:
        print(f"\n{description}")
        response = bot.process_message(command, context)
        print(response['response'])
        context = response['context']
        input("\nPress Enter to continue...")
    
    print("\n✅ Morning workflow complete! Entering interactive mode...\n")
    return context

# === Main Entry Point ===
if __name__ == "__main__":
    import sys
    
    if len(sys.argv) > 1:
        if sys.argv[1] == '--script' and len(sys.argv) > 2:
            # Run script mode: python intent_trader.py --script commands.txt
            run_script_mode(sys.argv[2])
        elif sys.argv[1] == '--quick':
            # Run quick workflow: python intent_trader.py --quick
            context = run_quick_workflow()
            # Continue in interactive mode with context
            bot = IntentTraderAssistant()
            print("\nEntering interactive mode with context...")
            run_interactive()
        elif sys.argv[1] == '--test':
            # Run built-in test
            print("Running built-in test...")
            # (Original test code here)
        else:
            print(f"Unknown option: {sys.argv[1]}")
            print("Usage:")
            print("  python intent_trader.py              # Interactive mode")
            print("  python intent_trader.py --quick      # Quick workflow")
            print("  python intent_trader.py --script file.txt  # Script mode")
    else:
        # Default: Interactive mode
        run_interactive()
