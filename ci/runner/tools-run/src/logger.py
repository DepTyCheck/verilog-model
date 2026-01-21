import logging


def get_logger():
    return logging.getLogger("tools_run")


def configure_logger(
    format: str = "%(asctime)s - %(relativeCreated)d ms - %(levelname)s - %(funcName)s - %(message)s",
    level: int = logging.INFO,
):
    tools_run_logger = get_logger()

    tools_run_logger.setLevel(level)

    tools_run_logger.handlers.clear()

    handler = logging.StreamHandler()
    handler.setLevel(level)

    formatter = logging.Formatter(format)
    handler.setFormatter(formatter)

    tools_run_logger.addHandler(handler)

    tools_run_logger.propagate = False
