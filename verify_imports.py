#!/usr/bin/env python3
"""
Verification script to test all imports in the intent-trader project.
Run this to verify that all import issues have been resolved.
"""

import sys
import traceback

def test_import(module_name, description):
    """Test importing a module and report the result."""
    try:
        __import__(module_name)
        print(f"✅ {description}: WORKING")
        return True
    except Exception as e:
        print(f"❌ {description}: {e}")
        return False

def main():
    print("🔍 Intent Trader Import Verification")
    print("=" * 50)
    
    # Test core modules
    print("\n📦 Core Modules:")
    test_import("src.core.config", "Configuration Manager")
    test_import("src.core.logging", "Logging Manager") 
    test_import("src.core.database", "Database Manager")
    test_import("src.core.intent_parser", "Intent Parser")
    test_import("src.core.action_dispatcher", "Action Dispatcher")
    test_import("src.core.agent_registry", "Agent Registry")
    
    # Test trading modules
    print("\n💰 Trading Modules:")
    test_import("src.position_manager", "Position Manager")
    test_import("src.market_data.feed", "Market Data Feed")
    test_import("src.strategy.engine", "Strategy Engine")
    test_import("src.order.manager", "Order Manager")
    
    # Test IAA core
    print("\n🤖 IAA Core:")
    test_import("src.iaa_core", "IAA Core")
    
    # Test analysis modules
    print("\n📊 Analysis Modules:")
    test_import("analysis.performance_visualizer", "Performance Visualizer")
    
    # Test agents
    print("\n🤖 Agent Modules:")
    test_import("agents.optimize_agent", "Optimize Agent")
    test_import("agents.plan_agent", "Plan Agent")
    
    # Test data modules
    print("\n📋 Data Modules:")
    test_import("data.schemas", "Data Schemas")
    test_import("data.models", "Data Models")
    
    print("\n" + "=" * 50)
    
    # Test the main IAA functionality
    print("\n🚀 Testing IAA Functionality:")
    try:
        from src.iaa_core import process_trading_message
        result = process_trading_message("What are the current levels for AAPL?")
        print(f"✅ IAA Processing: WORKING")
        print(f"   Intent: {result['intent']}")
        print(f"   Entities: {result['entities']}")
        print(f"   Response: {result['response'][:100]}...")
    except Exception as e:
        print(f"❌ IAA Processing: {e}")
        traceback.print_exc()
    
    print("\n🎉 Verification Complete!")
    print("\nIf you see errors in your IDE but this script shows ✅ WORKING,")
    print("try restarting your IDE or refreshing the Python interpreter.")

if __name__ == "__main__":
    main() 