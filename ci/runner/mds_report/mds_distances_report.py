import datetime
import os
import re

import numpy as np
import plotly.graph_objects as go
from common.ignored_errors_list import IgnoredErrorsList
from common.logger import get_logger
from common.utils import print_pretty
from mds_report.unknown_error_entry import UnknownErrorEntry
from sklearn.manifold import MDS
from textdistance import LZMANCD


def compute_ncd_for_errors(nodes_text: list[str]) -> dict[tuple[int, int], float]:
    results: dict[tuple[int, int], float] = {}
    n = len(nodes_text)
    for i in range(n):
        for j in range(i + 1, n):
            try:
                results[(i, j)] = LZMANCD().distance(nodes_text[i], nodes_text[j])
            except Exception as e:
                get_logger().error(e)
                results[(i, j)] = None
    return results


class MDSDistancesReport:
    def __init__(
        self,
        new_errors: list[UnknownErrorEntry],
        ignored_errors: IgnoredErrorsList,
        tool_name: str,
        job_link: str,
    ):
        self.new_errors = new_errors
        self.ignored_errors = ignored_errors
        self.tool_name = tool_name
        self.job_link = job_link

    def save(self, output_path: str = "error_distances.html") -> None:
        nodes_text = [e.error_text for e in self.new_errors] + [ke.pattern for ke in self.ignored_errors.errors()]
        if len(nodes_text) < 2:
            print_pretty(["Not enough errors to plot MDS."])
            return
        distances = compute_ncd_for_errors(nodes_text)
        self._plot(distances, output_path)

    def _plot(self, distances: dict[tuple[int, int], float], output_path: str) -> None:
        n_found = len(self.new_errors)
        n_known = len(self.ignored_errors.errors())
        n_total = n_found + n_known

        dist_matrix = np.zeros((n_total, n_total))
        for (i, j), dist in distances.items():
            if dist is not None:
                dist_matrix[i, j] = dist
                dist_matrix[j, i] = dist

        mds = MDS(n_components=2, dissimilarity="precomputed", random_state=42)
        coords = mds.fit_transform(dist_matrix)
        if coords is None:
            get_logger().error("MDS failed.")
            return
        x, y = coords[:, 0], coords[:, 1]

        fig = go.Figure()
        if n_found > 0:
            fig.add_trace(self._found_trace(x, y, n_found))
        if n_known > 0:
            fig.add_trace(self._known_trace(x, y))

        fig.update_layout(
            title=(
                f"New vs Known errors (MDS) for <b>{self.tool_name}</b> "
                f"({datetime.datetime.now().strftime('%d-%m-%Y %H:%M')}) "
                f"<a href='{self.job_link}'>GitHub Job</a>"
            ),
            xaxis={"showgrid": False, "zeroline": False, "showticklabels": False},
            yaxis={"showgrid": False, "zeroline": False, "showticklabels": False},
            plot_bgcolor="white",
            showlegend=True,
            hoverlabel={"namelength": -1},
        )
        fig.write_html(output_path)
        print_pretty([f"Interactive MDS plot saved to {output_path}"])

    def _found_trace(self, x, y, n_found: int) -> go.Scatter:
        labels, hover = [], []
        for e in self.new_errors:
            base = os.path.basename(e.file_path)
            m = re.match(r"(\d+)-", base)
            label = m.group(1) if m else base
            labels.append(label)
            hover.append(f"File: {label}<br>Error: {e.error_text}")
        return go.Scatter(
            x=x[:n_found],
            y=y[:n_found],
            mode="markers+text",
            marker={"size": 20, "color": "lightcoral", "symbol": "circle", "line": {"width": 2, "color": "darkred"}},
            text=labels,
            textposition="top center",
            textfont={"size": 14, "color": "black"},
            hovertemplate="%{hovertext}<extra></extra>",
            hovertext=hover,
            name="New errors",
        )

    def _known_trace(self, x, y) -> go.Scatter:
        n_found = len(self.new_errors)
        known = self.ignored_errors.errors()
        labels = [ke.error_id for ke in known]
        hover = [f"Known: {ke.error_id}<br>Pattern: {ke.pattern}" for ke in known]
        return go.Scatter(
            x=x[n_found:],
            y=y[n_found:],
            mode="markers+text",
            marker={"size": 18, "color": "lightskyblue", "symbol": "circle", "line": {"width": 2, "color": "darkblue"}},
            text=labels,
            textposition="top center",
            textfont={"size": 14, "color": "black"},
            hovertemplate="%{hovertext}<extra></extra>",
            hovertext=hover,
            name="Known errors",
        )
