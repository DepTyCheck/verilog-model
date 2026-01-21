import shutil
from pathlib import Path


class Assets:
    def __init__(self, assets: list[str]):
        self.assets = assets

    def copy_to_tmp_dir(self, tmp_dir: str) -> None:
        tmp_dir_path = Path(tmp_dir)
        for asset in self.assets:
            asset_path = Path(asset)
            if asset_path.exists():
                destination = tmp_dir_path / asset_path
                destination.parent.mkdir(parents=True, exist_ok=True)
                if asset_path.is_dir():
                    shutil.copytree(asset_path, destination, dirs_exist_ok=True)
                else:
                    shutil.copy2(asset_path, destination)
