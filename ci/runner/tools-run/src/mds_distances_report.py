import datetime
import os
import re
from typing import Dict, List, Tuple

import numpy as np
import plotly.graph_objects as go
from sklearn.manifold import MDS
from src.ignored_errors_list import IgnoredErrorsList
from src.logger import get_logger
from src.unexpected_error import UnexpectedError
from src.utils import print_pretty
from textdistance import LZMANCD


def compute_ncd_for_errors(nodes_text: List[str]) -> Dict[Tuple[int, int], float]:
    """
    Compute NCD for each unique pair of text nodes using ncd-xz.sh.
    Returns a dictionary mapping (i, j) index pairs to the NCD value.
    """
    results: Dict[Tuple[int, int], float] = {}
    n = len(nodes_text)
    for i in range(n):
        for j in range(i + 1, n):
            # file1 = "error_1.txt"
            # file2 = "error_2.txt"
            # with open(file1, "w", encoding="utf-8") as f1:
            #     f1.write(nodes_text[i])
            #     f1.flush()
            # with open(file2, "w", encoding="utf-8") as f2:
            #     f2.write(nodes_text[j])
            #     f2.flush()
            # try:
            #     proc = subprocess.run(
            #         [f"{ncd_script_path} {file1} {file2}"], shell=True, capture_output=True, text=True, check=True
            #     )
            #     ncd_value = float(proc.stdout.strip())
            #     results[(i, j)] = ncd_value
            # except Exception as e:
            #     print(e)
            #     results[(i, j)] = None
            # finally:
            #     if os.path.exists(file1):
            #         os.remove(file1)
            #     if os.path.exists(file2):
            #         os.remove(file2)
            try:
                ncd_value = LZMANCD().distance(nodes_text[i], nodes_text[j])
                results[(i, j)] = ncd_value
            except Exception as e:
                get_logger().error(e)
                results[(i, j)] = None
    return results


class MDSDistancesReport:
    def __init__(
        self,
        new_errors: List[UnexpectedError],
        ignored_errors: IgnoredErrorsList,
        tool_name: str,
        job_link: str,
    ):
        self.new_errors = new_errors
        self.ignored_errors = ignored_errors
        self.tool_name = tool_name
        self.job_link = job_link

    def save(self, output_path: str = "error_distances.html"):
        # Compute NCD across found and known errors using a single compute function
        nodes_text = [err.tool_output_error_text for err in self.new_errors] + [ke.pattern for ke in self.ignored_errors.errors()]
        distances: Dict[Tuple[int, int], float] = compute_ncd_for_errors(
            nodes_text,
            # ".github/workflows/runner/tools-run/ncd-xz.sh",
        )
        # Make plot
        self.plot_error_distances_mds(
            distances,
            output_path,
        )

    def plot_error_distances_mds(
        self,
        distances: Dict[Tuple[int, int], float],
        output_path: str = "error_distances.html",
    ):
        """
        Plots error nodes in 2D using MDS (scikit-learn) and plotly, grouping similar errors close together.
        Each node is labeled with its test number.
        The interactive plot is saved to output_path
        """
        n_found = len(self.new_errors)
        n_known = len(self.ignored_errors.errors())
        n_total = n_found + n_known

        if n_total < 2:
            print_pretty(["Not enough errors to plot MDS."])
            return

        x_coords, y_coords = self._compute_mds_coordinates(n_total, distances)
        if x_coords is None or y_coords is None:
            return

        fig = go.Figure()

        if n_found > 0:
            fig.add_trace(self._create_found_errors_trace(x_coords, y_coords, n_found))

        if n_known > 0:
            fig.add_trace(self._create_known_errors_trace(x_coords, y_coords))

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

    def _compute_mds_coordinates(self, n_total: int, distances: Dict[Tuple[int, int], float]) -> Tuple[np.ndarray | None, np.ndarray | None]:
        dist_matrix = np.zeros((n_total, n_total))
        for (i, j), dist in distances.items():
            if dist is not None:
                dist_matrix[i, j] = dist
                dist_matrix[j, i] = dist

        mds = MDS(n_components=2, dissimilarity="precomputed", random_state=42)
        coords = mds.fit_transform(dist_matrix)
        if coords is None:
            get_logger().error("MDS failed to compute coordinates.")
            return None, None
        return coords[:, 0], coords[:, 1]

    def _create_found_errors_trace(self, x_coords: np.ndarray, y_coords: np.ndarray, n_found: int) -> go.Scatter:
        found_labels: List[str] = []
        found_hover: List[str] = []
        for idx in range(n_found):
            base_name = os.path.basename(self.new_errors[idx].test_file_path)
            match = re.match(r"(\d+)-", base_name)
            if match:
                label = match.group(1)
            else:
                label = base_name
            found_labels.append(label)
            found_hover.append(f"Test: {label}<br>Error: {self.new_errors[idx].test_file_path}")

        return go.Scatter(
            x=x_coords[:n_found],
            y=y_coords[:n_found],
            mode="markers+text",
            marker={
                "size": 20,
                "color": "lightcoral",
                "symbol": "circle",
                "line": {"width": 2, "color": "darkred"},
            },
            text=found_labels,
            textposition="top center",
            textfont={"size": 14, "color": "black"},
            hovertemplate="%{hovertext}<extra></extra>",
            hovertext=found_hover,
            name="New errors",
        )

    def _create_known_errors_trace(self, x_coords: np.ndarray, y_coords: np.ndarray) -> go.Scatter:
        known_labels: List[str] = []
        known_hover: List[str] = []
        n_found = len(self.new_errors)
        # known errors are after found errors in the list
        known_errors_list = self.ignored_errors.errors()

        for ke in known_errors_list:
            known_labels.append(ke.error_id)
            known_hover.append(f"Known: {ke.error_id}<br>Pattern: {ke.pattern}")

        return go.Scatter(
            x=x_coords[n_found:],
            y=y_coords[n_found:],
            mode="markers+text",
            marker={
                "size": 18,
                "color": "lightskyblue",
                "symbol": "circle",
                "line": {"width": 2, "color": "darkblue"},
            },
            text=known_labels,
            textposition="top center",
            textfont={"size": 14, "color": "black"},
            hovertemplate="%{hovertext}<extra></extra>",
            hovertext=known_hover,
            name="Known errors",
        )
