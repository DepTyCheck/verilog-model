import subprocess
import os
from typing import List, Tuple, Dict
import numpy as np
import plotly.graph_objects as go
import plotly.express as px
from ignored_errors_list import KnownError
from sklearn.manifold import MDS
import re
from utils import print_pretty
from textdistance import ZLIBNCD
import datetime


class FoundError:
    def __init__(self, text: str, file_name: str):
        self.text = text
        self.file_name = file_name


def compute_ncd_for_errors(nodes_text: List[str], ncd_script_path: str) -> Dict[Tuple[int, int], float]:
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
                ncd_value = ZLIBNCD().distance(nodes_text[i], nodes_text[j])
                results[(i, j)] = ncd_value
            except Exception as e:
                print(e)
                results[(i, j)] = None
    return results


def plot_error_distances_mds(
    errors: List[FoundError],
    distances: Dict[Tuple[int, int], float],
    known_errors: List[KnownError],
    tool_name: str,
    job_link: str,
    output_path: str = "error_distances.html",
):
    """
    Plots error nodes in 2D using MDS (scikit-learn) and plotly, grouping similar errors close together.
    Each node is labeled with its test number.
    The interactive plot is saved to output_path
    """
    n_found = len(errors)
    n_known = len(known_errors)
    n_total = n_found + n_known

    if n_total < 2:
        print_pretty(["Not enough errors to plot MDS."])
        return

    dist_matrix = np.zeros((n_total, n_total))
    for (i, j), dist in distances.items():
        if dist is not None:
            dist_matrix[i, j] = dist
            dist_matrix[j, i] = dist

    mds = MDS(n_components=2, dissimilarity="precomputed", random_state=42)
    coords = mds.fit_transform(dist_matrix)
    if coords is None:
        print("MDS failed to compute coordinates.")
        return

    x_coords = coords[:, 0]
    y_coords = coords[:, 1]

    found_labels: List[str] = []
    found_hover: List[str] = []
    for idx in range(n_found):
        base_name = os.path.basename(errors[idx].file_name)
        match = re.match(r"(\d+)-", base_name)
        if match:
            label = match.group(1)
        else:
            label = base_name
        found_labels.append(label)
        found_hover.append(f"Test: {label}<br>Error: {errors[idx].text}")

    known_labels: List[str] = []
    known_hover: List[str] = []
    for ke in known_errors:
        known_labels.append(ke.id)
        known_hover.append(f"Known: {ke.id}<br>Pattern: {ke.pattern}")

    fig = go.Figure()

    if n_found > 0:
        fig.add_trace(
            go.Scatter(
                x=x_coords[:n_found],
                y=y_coords[:n_found],
                mode="markers+text",
                marker=dict(size=20, color="lightcoral", symbol="circle", line=dict(width=2, color="darkred")),
                text=found_labels,
                textposition="top center",
                textfont=dict(size=14, color="black"),
                hovertemplate="%{hovertext}<extra></extra>",
                hovertext=found_hover,
                name="New errors",
            )
        )

    if n_known > 0:
        fig.add_trace(
            go.Scatter(
                x=x_coords[n_found:],
                y=y_coords[n_found:],
                mode="markers+text",
                marker=dict(size=18, color="lightskyblue", symbol="circle", line=dict(width=2, color="darkblue")),
                text=known_labels,
                textposition="top center",
                textfont=dict(size=14, color="black"),
                hovertemplate="%{hovertext}<extra></extra>",
                hovertext=known_hover,
                name="Known errors",
            )
        )

    fig.update_layout(
        title=(
            f"New vs Known errors (MDS) for <b>{tool_name}</b> "
            f"({datetime.datetime.now().strftime('%d-%m-%Y %H:%M')}) "
            f"<a href='{job_link}'>GitHub Job</a>"
        ),
        xaxis=dict(showgrid=False, zeroline=False, showticklabels=False),
        yaxis=dict(showgrid=False, zeroline=False, showticklabels=False),
        plot_bgcolor="white",
        showlegend=True,
        hoverlabel=dict(namelength=-1),
    )

    fig.write_html(output_path)
    print_pretty([f"Interactive MDS plot saved to {output_path}"])
