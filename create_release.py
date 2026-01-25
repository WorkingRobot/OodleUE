import zipfile

from glob import glob
from pathlib import Path

for file_path in glob("*/*.zip"):
    asset_name = Path(file_path).stem
    with zipfile.ZipFile(f"{asset_name}.zip", mode='w', compression=zipfile.ZIP_DEFLATED) as zip_file:
        with zipfile.ZipFile(file_path, mode='r') as sub_zip:
            for n in sub_zip.namelist():
                new_name = n.replace("/Release/", "/").replace("/Debug/", "/")
                zip_file.writestr(new_name, sub_zip.open(n).read())
