import pytest

from src.core.agent_registry import AgentRegistry, BaseAgent


class PlannerAgent(BaseAgent):
    """Test planner agent."""
    
    def execute(self, context):
        return {"status": "success", "plan": "test plan"}


def test_agent_registry():
    registry = AgentRegistry()
    
    # Register the planner agent type
    registry.register_agent_type("planner", PlannerAgent)
    
    # Create a planner agent instance
    registry.create_agent("planner", "planner")
    
    # Now get the agent
    planner = registry.get_agent("planner")
    assert planner is not None
    assert isinstance(planner, PlannerAgent)
    assert planner.name == "planner"
