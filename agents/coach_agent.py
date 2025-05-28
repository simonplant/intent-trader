from datetime import datetime, timedelta
from typing import Any, Dict, List
from pydantic import BaseModel
from data.storage import Storage
from agents.review_agent import ReviewAgent


class ImprovementArea(BaseModel):
    category: str
    description: str
    impact: float  # 0-1 scale of potential impact
    suggestions: List[str]
    metrics: Dict[str, Any]


class BestPractice(BaseModel):
    category: str
    description: str
    evidence: List[str]
    implementation: str


class DevelopmentPlan(BaseModel):
    short_term: List[str]
    medium_term: List[str]
    long_term: List[str]
    priority_areas: List[str]


class CoachAgent:
    def __init__(self):
        self.storage = Storage()
        self.review_agent = ReviewAgent()
        self.improvement_thresholds = {
            "win_rate": 0.5,
            "profit_factor": 1.5,
            "average_win_loss_ratio": 1.5,
            "max_drawdown": 0.1,
            "scaling_effectiveness": 0.1,
            "adjustment_effectiveness": 0.05,
        }

    def execute(self, **kwargs) -> Dict[str, Any]:
        """
        Analyzes trading performance and provides coaching insights.
        """
        try:
            # Get review data
            start_time = kwargs.get("start_time", datetime.now() - timedelta(days=30))
            end_time = kwargs.get("end_time", datetime.now())
            symbol = kwargs.get("symbol")

            review_result = self.review_agent.execute(
                start_time=start_time, end_time=end_time, symbol=symbol
            )

            if review_result["status"] != "success":
                return review_result

            # Analyze performance and generate insights
            improvement_areas = self._identify_improvement_areas(review_result["data"])
            best_practices = self._identify_best_practices(review_result["data"])
            development_plan = self._create_development_plan(improvement_areas, best_practices)

            return {
                "status": "success",
                "message": "Coaching insights generated",
                "data": {
                    "improvement_areas": [area.dict() for area in improvement_areas],
                    "best_practices": [practice.dict() for practice in best_practices],
                    "development_plan": development_plan.dict(),
                },
            }

        except Exception as e:
            return {"status": "error", "message": f"Coaching analysis failed: {str(e)}"}

    def _identify_improvement_areas(self, review_data: Dict[str, Any]) -> List[ImprovementArea]:
        """
        Identify areas for improvement based on performance metrics.
        """
        improvement_areas = []
        metrics = review_data["metrics"]
        execution = review_data["execution_analysis"]
        management = review_data["management_analysis"]

        # Analyze win rate
        if metrics["win_rate"] < self.improvement_thresholds["win_rate"]:
            improvement_areas.append(
                ImprovementArea(
                    category="Entry Quality",
                    description="Low win rate indicates potential issues with entry timing or setup selection",
                    impact=0.8,
                    suggestions=[
                        "Review entry criteria and setup quality",
                        "Analyze market conditions at entry points",
                        "Consider tightening entry filters",
                    ],
                    metrics={"win_rate": metrics["win_rate"]},
                )
            )

        # Analyze profit factor
        if metrics["profit_factor"] < self.improvement_thresholds["profit_factor"]:
            improvement_areas.append(
                ImprovementArea(
                    category="Risk Management",
                    description="Low profit factor suggests risk management needs improvement",
                    impact=0.9,
                    suggestions=[
                        "Review stop loss placement",
                        "Analyze position sizing",
                        "Consider implementing trailing stops",
                    ],
                    metrics={"profit_factor": metrics["profit_factor"]},
                )
            )

        # Analyze win/loss ratio
        win_loss_ratio = (
            abs(metrics["average_win"] / metrics["average_loss"])
            if metrics["average_loss"] != 0
            else 0
        )
        if win_loss_ratio < self.improvement_thresholds["average_win_loss_ratio"]:
            improvement_areas.append(
                ImprovementArea(
                    category="Exit Management",
                    description="Low win/loss ratio indicates potential issues with exit management",
                    impact=0.7,
                    suggestions=[
                        "Review take profit levels",
                        "Analyze exit timing",
                        "Consider implementing partial exits",
                    ],
                    metrics={"win_loss_ratio": win_loss_ratio},
                )
            )

        # Analyze execution quality
        if execution.get("average_slippage", 0) > 0.001:  # 0.1% slippage threshold
            improvement_areas.append(
                ImprovementArea(
                    category="Execution",
                    description="High slippage indicates potential execution issues",
                    impact=0.6,
                    suggestions=[
                        "Review order types used",
                        "Analyze market liquidity",
                        "Consider using limit orders",
                    ],
                    metrics={"average_slippage": execution["average_slippage"]},
                )
            )

        # Analyze scaling effectiveness
        if (
            management.get("average_scaling_effectiveness", 0)
            < self.improvement_thresholds["scaling_effectiveness"]
        ):
            improvement_areas.append(
                ImprovementArea(
                    category="Position Scaling",
                    description="Low scaling effectiveness suggests issues with scaling decisions",
                    impact=0.5,
                    suggestions=[
                        "Review scaling criteria",
                        "Analyze scaling timing",
                        "Consider adjusting scaling size",
                    ],
                    metrics={"scaling_effectiveness": management["average_scaling_effectiveness"]},
                )
            )

        return improvement_areas

    def _identify_best_practices(self, review_data: Dict[str, Any]) -> List[BestPractice]:
        """
        Identify successful trading practices based on performance data.
        """
        best_practices = []
        metrics = review_data["metrics"]
        execution = review_data["execution_analysis"]
        management = review_data["management_analysis"]

        # Identify successful entry practices
        if metrics["win_rate"] > 0.6:  # High win rate threshold
            best_practices.append(
                BestPractice(
                    category="Entry",
                    description="Strong entry selection criteria",
                    evidence=[
                        f"Win rate: {metrics['win_rate']:.2%}",
                        f"Average win: ${metrics['average_win']:.2f}",
                    ],
                    implementation="Document and maintain current entry criteria",
                )
            )

        # Identify successful risk management
        if metrics["profit_factor"] > 2.0:  # High profit factor threshold
            best_practices.append(
                BestPractice(
                    category="Risk Management",
                    description="Effective risk management approach",
                    evidence=[
                        f"Profit factor: {metrics['profit_factor']:.2f}",
                        f"Average loss: ${metrics['average_loss']:.2f}",
                    ],
                    implementation="Maintain current risk management rules",
                )
            )

        # Identify successful scaling
        if (
            management.get("average_scaling_effectiveness", 0) > 0.2
        ):  # High scaling effectiveness threshold
            best_practices.append(
                BestPractice(
                    category="Position Management",
                    description="Effective position scaling strategy",
                    evidence=[
                        f"Scaling effectiveness: {management['average_scaling_effectiveness']:.2%}",
                        f"Number of successful scales: {len(management['scaling_effectiveness'])}",
                    ],
                    implementation="Continue current scaling approach",
                )
            )

        return best_practices

    def _create_development_plan(
        self,
        improvement_areas: List[ImprovementArea],
        best_practices: List[BestPractice],
    ) -> DevelopmentPlan:
        """
        Create a development plan based on improvement areas and best practices.
        """
        # Sort improvement areas by impact
        sorted_areas = sorted(improvement_areas, key=lambda x: x.impact, reverse=True)

        # Create short-term goals (highest impact areas)
        short_term = [
            f"Focus on improving {area.category}: {area.description}" for area in sorted_areas[:2]
        ]

        # Create medium-term goals (next set of areas)
        medium_term = [
            f"Develop {area.category} strategy: {area.description}" for area in sorted_areas[2:4]
        ]

        # Create long-term goals (remaining areas)
        long_term = [f"Master {area.category}: {area.description}" for area in sorted_areas[4:]]

        # Identify priority areas based on impact and current performance
        priority_areas = [area.category for area in sorted_areas[:3]]

        return DevelopmentPlan(
            short_term=short_term,
            medium_term=medium_term,
            long_term=long_term,
            priority_areas=priority_areas,
        )
