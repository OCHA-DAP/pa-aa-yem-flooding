import argparse
import logging

from src import constants  # noqa: F401

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--clobber", action="store_true")
    parser.add_argument("-d", "--debug", action="store_true")
    return parser.parse_args()


def run_pipeline(clobber: bool = False):
    pass


if __name__ == "__main__":
    args = parse_args()
    if args.debug:
        logging.getLogger().setLevel(logging.DEBUG)
    run_pipeline(clobber=args.clobber)
