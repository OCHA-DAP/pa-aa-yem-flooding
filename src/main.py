import argparse
import logging

from src import utils

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--clobber", action="store_true")
    parser.add_argument("-d", "--debug", action="store_true")
    return parser.parse_args()


def run_pipeline(clobber: bool = False):
    # Floodscan
    # utils.process_floodscan_admin(admin_level=0)
    # utils.process_floodscan_admin(admin_level=1)
    # utils.process_floodscan_admin(admin_level=2)
    # Chirps
    # utils.download_and_process_chirps(clobber=clobber)
    # Chirps-Gefs
    utils.download_chirps_gefs(days_ahead=5, clobber=clobber)
    # utils.download_chirps_gefs(days_ahead=10, clobber=clobber)
    # utils.download_chirps_gefs(days_ahead=15, clobber=clobber)
    return


if __name__ == "__main__":
    args = parse_args()
    if args.debug:
        logging.getLogger().setLevel(logging.DEBUG)
    run_pipeline(clobber=args.clobber)
