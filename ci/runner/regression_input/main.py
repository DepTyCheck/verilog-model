# ci/runner/regression_input/main.py
import logging
from pathlib import Path

from common.error_file_parser import parse_error_files
from common.language_config import get_file_extension, load_language_config
from common.logger import configure_logger, get_logger
from regression_input.parse_args import parse_args


def main() -> None:
    configure_logger(level=logging.INFO)
    args = parse_args()
    logger = get_logger()

    extensions = load_language_config(args.language_config)
    suffix = get_file_extension(args.language, extensions)

    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    base = Path(args.known_errors_dir)
    if not base.exists():
        logger.warning(f"known-errors-dir does not exist: {base.resolve()}")
        return

    written = 0
    seen: set[str] = set()
    for tool_dir in sorted(p for p in base.iterdir() if p.is_dir()):
        for ef in parse_error_files(str(tool_dir)):
            if ef.language != args.language:
                continue
            for ex in ef.examples:
                filename = f"{ex.name}-{ex.type}{suffix}"
                if filename in seen:
                    logger.warning(f"Duplicate example filename {filename!r} (error_id={ef.error_id}); skipping")
                    continue
                seen.add(filename)
                (out_dir / filename).write_text(ex.content, encoding="utf-8")
                written += 1
    logger.info(f"Materialised {written} regression example files into {out_dir}")


if __name__ == "__main__":
    main()
