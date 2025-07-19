import subprocess
import os
from typing import List, Tuple, Dict
import numpy as np
import matplotlib.pyplot as plt
from sklearn.manifold import MDS
import re
from utils import print_pretty

class FoundError:
    def __init__(self, text: str, file_name: str):
        self.text = text
        self.file_name = file_name


def compute_ncd_for_errors(errors: List[FoundError], ncd_script_path: str) -> Dict[Tuple[int, int], float]:
    """
    For each unique pair of FoundError objects, compute the NCD using ncd-xz.sh.
    Returns a dictionary mapping (i, j) index pairs to the NCD value.
    """
    results = {}
    n = len(errors)
    for i in range(n):
        for j in range(i + 1, n):
            file1 = "error_1.txt"
            file2 = "error_2.txt"
            with open(file1, "w", encoding="utf-8") as f1:
                f1.write(errors[i].text)
                f1.flush()
            with open(file2, "w", encoding="utf-8") as f2:
                f2.write(errors[j].text)
                f2.flush()
            try:
                proc = subprocess.run(
                    [f"{ncd_script_path} {file1} {file2}"],
                    shell=True,
                    capture_output=True, text=True, check=True
                )
                ncd_value = float(proc.stdout.strip())
                results[(i, j)] = ncd_value
            except Exception as e:
                print(e)
                results[(i, j)] = None
            finally:
                if os.path.exists(file1):
                    os.remove(file1)
                if os.path.exists(file2):
                    os.remove(file2)
    # for (i, j), dist in results.items():
    #     print(f"Pair ({i}, {j}):\nError 1: {errors[i].text}\nError 2: {errors[j].text}\nDistance: {dist}\n{'-'*40}")
    return results


def plot_error_distances_mds(errors: List[FoundError], distances: Dict[Tuple[int, int], float], output_path: str = "error_distances.png"):
    """
    Plots error nodes in 2D using MDS (scikit-learn) and matplotlib, grouping similar errors close together.
    No edges are drawn. Each node is labeled with its test number.
    The image is saved to output_path.
    """
    n = len(errors)
    if n < 2:
        print_pretty(["Not enough errors to plot MDS."])
        return
    # Build the full distance matrix
    dist_matrix = np.zeros((n, n))
    for (i, j), dist in distances.items():
        if dist is not None:
            dist_matrix[i, j] = dist
            dist_matrix[j, i] = dist
    # MDS embedding
    mds = MDS(n_components=2, dissimilarity='precomputed', random_state=42)
    coords = mds.fit_transform(dist_matrix)
    if coords is None:
        print("MDS failed to compute coordinates.")
        return

    plt.figure(figsize=(8, 8))
    for idx, (x, y) in enumerate(coords):
        # Extract the number before '-' in the file name
        base_name = os.path.basename(errors[idx].file_name)
        match = re.match(r"(\d+)-", base_name)
        if match:
            label = match.group(1)
        else:
            label = base_name
        plt.scatter(x, y, s=500)
        plt.text(x, y, label, fontsize=14, ha='center', va='center', bbox=dict(facecolor='white', alpha=0.7, edgecolor='none'))
    plt.title("Found errors groups (MDS)")
    plt.axis('off')
    plt.tight_layout()
    plt.savefig(output_path)
    plt.close()
    print_pretty([f"MDS plot saved to {output_path}"])
