import zipfile

from glob import glob
from pathlib import Path

zip_files = glob("*/*.zip")
grouped_zips = {}
for name in zip_files:
	name_type = '-'.join(Path(name).stem.split('-')[:-1])
	if name_type not in grouped_zips:
		grouped_zips[name_type] = [name]
	else:
		grouped_zips[name_type].append(name)

for group, files in grouped_zips.items():
	with zipfile.ZipFile(group + '.zip', mode='w', compression=zipfile.ZIP_DEFLATED) as zip_file:
		for file in files:
			with zipfile.ZipFile(file, mode='r') as sub_zip:
				for n in sub_zip.namelist():
					zip_file.writestr(n, sub_zip.open(n).read())
