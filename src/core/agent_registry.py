"""Agent registry for managing trading agents."""

from typing import Dict, Any, Type, Optional
from abc import ABC, abstractmethod

class BaseAgent(ABC):
    """Base class for all trading agents."""
    
    def __init__(self, name: str):
        self.name = name
        self.active = False
    
    @abstractmethod
    def execute(self, context: Dict[str, Any]) -> Dict[str, Any]:
        """Execute the agent's logic."""
        pass
    
    def activate(self):
        """Activate the agent."""
        self.active = True
    
    def deactivate(self):
        """Deactivate the agent."""
        self.active = False

class AgentRegistry:
    """Registry for managing trading agents."""
    
    def __init__(self):
        self.agents: Dict[str, BaseAgent] = {}
        self.agent_types: Dict[str, Type[BaseAgent]] = {}
    
    def register_agent_type(self, name: str, agent_class: Type[BaseAgent]):
        """Register an agent type."""
        self.agent_types[name] = agent_class
    
    def create_agent(self, agent_type: str, name: str, **kwargs) -> Optional[BaseAgent]:
        """Create an agent instance."""
        if agent_type not in self.agent_types:
            return None
        
        agent_class = self.agent_types[agent_type]
        agent = agent_class(name, **kwargs)
        self.agents[name] = agent
        return agent
    
    def get_agent(self, name: str) -> Optional[BaseAgent]:
        """Get an agent by name."""
        return self.agents.get(name)
    
    def remove_agent(self, name: str) -> bool:
        """Remove an agent."""
        if name in self.agents:
            del self.agents[name]
            return True
        return False
    
    def list_agents(self) -> Dict[str, BaseAgent]:
        """List all registered agents."""
        return self.agents.copy()
    
    def list_active_agents(self) -> Dict[str, BaseAgent]:
        """List all active agents."""
        return {name: agent for name, agent in self.agents.items() if agent.active} 