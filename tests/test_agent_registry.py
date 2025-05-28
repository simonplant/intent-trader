import pytest

from core.agent_registry import AgentRegistry


def test_agent_registry():
    registry = AgentRegistry()
    planner = registry.get_agent("planner")
    assert planner is not None
