import os, sys, zipfile

dir_path = sys.argv[1]
zip_path = sys.argv[2]

with zipfile.ZipFile(zip_path, mode='w', compression=zipfile.ZIP_DEFLATED) as zip_file:
    for subdir in ['ar', 'bin']:
        subdir_path = "%s/%s" % (dir_path, subdir)
        for root, dirs, files in os.walk(subdir_path):
            for file in files:
                zip_file.write(os.path.join(root, file), os.path.join(subdir + root[len(subdir_path):], file))
