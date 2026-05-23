"""Format regression reproducibility rows as CSV text.

Output schema: `example_id,type,is_reproduced` with `is_reproduced` serialised
as lowercase `true`/`false`. Special characters in `example_id` are quoted
per `csv` module rules. Caller is responsible for ordering.
"""

import csv
import io

HEADER = ("example_id", "type", "is_reproduced")


def format_csv(rows: list[tuple[str, str, bool]]) -> str:
    buf = io.StringIO()
    writer = csv.writer(buf, lineterminator="\n")
    writer.writerow(HEADER)
    for example_id, ex_type, reproduced in rows:
        writer.writerow([example_id, ex_type, "true" if reproduced else "false"])
    return buf.getvalue()
