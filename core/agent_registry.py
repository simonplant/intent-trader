from typing import Dict, Type, Any
from agents.planner_agent import PlannerAgent
from agents.analyst_agent import AnalystAgent
from agents.conviction_classifier_agent import ConvictionClassifierAgent

class AgentRegistry:
    def __init__(self):
        self.agents: Dict[str, Any] = {
            "planner": PlannerAgent(),
            "analyst": AnalystAgent(),
            "conviction_classifier": ConvictionClassifierAgent(),
        }

    def get_agent(self, agent_name: str) -> Any:
        return self.agents.get(agent_name) 