from typing import Any, Dict, List, Optional

from agents.analyst_agent import AnalystAgent
from agents.coach_agent import CoachAgent
from agents.conviction_classifier_agent import ConvictionClassifierAgent
from agents.execute_agent import ExecuteAgent
from agents.focus_agent import FocusAgent
from agents.manage_agent import ManageAgent
from agents.market_data_agent import MarketDataAgent
from agents.optimize_agent import OptimizeAgent
from agents.plan_agent import PlanAgent
from agents.review_agent import ReviewAgent


class AgentRegistry:
    def __init__(self):
        # Analysis Agents
        self.analyst_agent = AnalystAgent()
        self.conviction_classifier_agent = ConvictionClassifierAgent()
        self.market_data_agent = MarketDataAgent()

        # PFEMRC Agents
        self.plan_agent = PlanAgent()
        self.focus_agent = FocusAgent()
        self.execute_agent = ExecuteAgent()
        self.manage_agent = ManageAgent()
        self.review_agent = ReviewAgent()
        self.coach_agent = CoachAgent()
        self.optimize_agent = OptimizeAgent()

        # Register all agents
        self.agents = {
            "Analysis": {
                "analyst": self.analyst_agent,
                "conviction_classifier": self.conviction_classifier_agent,
                "market_data": self.market_data_agent,
            },
            "PFEMRC": {
                "plan": self.plan_agent,
                "focus": self.focus_agent,
                "execute": self.execute_agent,
                "manage": self.manage_agent,
                "review": self.review_agent,
                "coach": self.coach_agent,
                "optimize": self.optimize_agent,
            },
        }

    def get_agent(self, category: str, name: str) -> Any:
        """
        Get an agent by category and name.
        """
        if category not in self.agents:
            raise ValueError(f"Unknown agent category: {category}")
        if name not in self.agents[category]:
            raise ValueError(f"Unknown agent name: {name} in category {category}")
        return self.agents[category][name]

    def get_category_agents(self, category: str) -> Dict[str, Any]:
        """
        Get all agents in a category.
        """
        if category not in self.agents:
            raise ValueError(f"Unknown agent category: {category}")
        return self.agents[category]

    def list_categories(self) -> List[str]:
        """
        List all agent categories.
        """
        return list(self.agents.keys())

    def list_agents(self, category: Optional[str] = None) -> Dict[str, List[str]]:
        """
        List all agents, optionally filtered by category.
        """
        if category:
            if category not in self.agents:
                raise ValueError(f"Unknown agent category: {category}")
            return {category: list(self.agents[category].keys())}
        return {cat: list(agents.keys()) for cat, agents in self.agents.items()}
