from datetime import datetime

import pytest

from agents.optimize_agent import OptimizationParameters, OptimizeAgent, OptimizationResult
from data.schemas import MarketDataSchema, TradePlanSchema


def test_optimize_agent_initialization():
    """Test OptimizeAgent initialization"""
    agent = OptimizeAgent()
    assert agent is not None
    assert agent.storage is not None
    assert agent.market_data_agent is not None
    assert agent.plan_agent is not None
    assert agent.focus_agent is not None
    assert agent.execute_agent is not None
    assert agent.manage_agent is not None
    assert agent.review_agent is not None
    assert agent.coach_agent is not None


def test_optimize_trade_with_valid_inputs(sample_trade_plan, sample_market_data):
    """Test trade optimization with valid inputs"""
    agent = OptimizeAgent()

    # Create optimization parameters
    params = OptimizationParameters(
        capital=100000,
        max_daily_risk=0.02,
        max_position_size=0.1,
        max_drawdown=0.05,
        target_daily_pnl=0.01,
        market_conditions={},
        trading_style="day",
        risk_tolerance=0.5,
    )

    # Convert sample data to dict format
    plan_dict = sample_trade_plan.dict()
    market_data_dict = {sample_market_data.symbol: sample_market_data.dict()}

    # Execute optimization with a single plan
    # Pass the plan directly since Storage doesn't have get_active_plans
    result = agent._optimize_trade(plan_dict, params, market_data_dict)

    # Verify result
    assert result is not None
    assert isinstance(result, OptimizationResult)
    
    # Verify optimization results
    assert result.optimal_position_size > 0
    assert result.optimal_entry_price > 0
    assert result.optimal_stop_loss > 0
    assert result.optimal_take_profit > 0
    assert result.risk_reward_ratio > 0
    assert result.expected_pnl != 0
    assert 0 <= result.confidence_score <= 1
    assert 1 <= result.execution_priority <= 100
    assert isinstance(result.notes, str)


def test_optimize_trade_with_invalid_inputs():
    """Test trade optimization with invalid inputs"""
    agent = OptimizeAgent()

    # Test with missing plan
    result = agent.execute(market_data={})
    assert result["status"] == "error"
    assert "message" in result

    # Test with invalid market data
    result = agent.execute(plan={}, market_data=None)
    assert result["status"] == "error"
    assert "message" in result


def test_position_size_calculation(sample_trade_plan, sample_market_data):
    """Test position size calculation"""
    agent = OptimizeAgent()

    params = OptimizationParameters(
        capital=100000,
        max_daily_risk=0.02,
        max_position_size=0.1,
        max_drawdown=0.05,
        target_daily_pnl=0.01,
        market_conditions={},
        trading_style="day",
        risk_tolerance=0.5,
    )

    position_size = agent._calculate_position_size(
        sample_trade_plan.dict(), params, sample_market_data.price
    )

    assert position_size > 0
    assert position_size <= params.capital * params.max_position_size / sample_market_data.price


def test_confidence_score_calculation(sample_trade_plan, sample_market_data):
    """Test confidence score calculation"""
    agent = OptimizeAgent()

    params = OptimizationParameters(
        capital=100000,
        max_daily_risk=0.02,
        max_position_size=0.1,
        max_drawdown=0.05,
        target_daily_pnl=0.01,
        market_conditions={},
        trading_style="day",
        risk_tolerance=0.5,
    )

    confidence = agent._calculate_confidence_score(
        sample_trade_plan.dict(), sample_market_data.dict(), params
    )

    assert 0 <= confidence <= 1


def test_execution_priority_calculation():
    """Test execution priority calculation"""
    agent = OptimizeAgent()

    priority = agent._calculate_execution_priority(
        confidence=0.8, risk_reward=2.0, expected_pnl=1000.0
    )

    assert 1 <= priority <= 100


def test_optimization_notes_generation(sample_trade_plan, sample_market_data):
    """Test optimization notes generation"""
    agent = OptimizeAgent()

    notes = agent._generate_optimization_notes(sample_trade_plan.dict(), sample_market_data.dict())

    assert isinstance(notes, str)
    assert len(notes) > 0
