import logging


def get_logger(name: str = "verilog_model_ci"):
    return logging.getLogger(name)


def configure_logger(
    name: str = "verilog_model_ci",
    fmt: str = "%(asctime)s - %(relativeCreated)d ms - %(levelname)s - %(funcName)s - %(message)s",
    level: int = logging.INFO,
):
    logger = get_logger(name)

    logger.setLevel(level)
    logger.handlers.clear()

    handler = logging.StreamHandler()
    handler.setLevel(level)
    handler.setFormatter(logging.Formatter(fmt))

    logger.addHandler(handler)
    logger.propagate = False
