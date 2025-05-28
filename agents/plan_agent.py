from typing import Dict, Any
from data.models import TradePlan
from agents.analyst_agent import AnalystAgent
from agents.conviction_classifier_agent import ConvictionClassifierAgent

class PlanAgent:
    def __init__(self):
        self.analyst = AnalystAgent()
        self.conviction_classifier = ConvictionClassifierAgent()

    def execute(self, **kwargs) -> Dict[str, Any]:
        """
        Synthesizes a unified trade plan from analyzed transcripts and PDFs.
        Uses AnalystAgent for raw analysis and ConvictionClassifier for confidence scoring.
        """
        # Get raw analysis from AnalystAgent
        analysis_result = self.analyst.execute(**kwargs)
        
        # Get conviction classification
        conviction_result = self.conviction_classifier.execute(
            analysis=analysis_result["data"]
        )
        
        # Synthesize plan from analysis and conviction
        plan = self._synthesize_plan(
            analysis=analysis_result["data"],
            conviction=conviction_result["data"]
        )
        
        return {
            "status": "success",
            "message": "Plan synthesized from analysis",
            "data": {
                "plan": plan,
                "source_analysis": analysis_result["data"],
                "conviction": conviction_result["data"]
            }
        }
    
    def _synthesize_plan(self, analysis: Dict[str, Any], conviction: Dict[str, Any]) -> TradePlan:
        """
        Synthesizes a unified trade plan from analysis and conviction data.
        """
        # TODO: Implement plan synthesis logic
        return TradePlan(
            summary="Unified trade plan synthesized from analysis",
            setups=[]
        ) 