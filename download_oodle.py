import gzip, os, shutil, glob, zipfile
import xml.etree.ElementTree as ET

from concurrent.futures import as_completed
from requests_futures.sessions import FuturesSession

print("Moving gitdeps", flush=True)
shutil.copy("unreal/Engine/Build/Commit.gitdeps.xml", "Commit.gitdeps.xml")

print("Parsing gitdeps", flush=True)
manifest = ET.parse("Commit.gitdeps.xml").getroot()

print("Parsing files", flush=True)
files = []
for file in manifest.find("Files"):
    if "Oodle" in file.attrib["Name"] and "Sdk" in file.attrib["Name"]:
        files.append(file)

print("Parsing blobs", flush=True)
blobs = []
for blob in manifest.find("Blobs"):
    for file in files:
        if file.attrib["Hash"] == blob.attrib["Hash"]:
            blobs.append(blob)
            break

print("Parsing packs", flush=True)
packs = []
for pack in manifest.find("Packs"):
    for blob in blobs:
        if blob.attrib["PackHash"] == pack.attrib["Hash"]:
            packs.append(pack)
            break

print("::group::Grabbing files", flush=True)
session = FuturesSession(max_workers=32)
futures = []
for pack in packs:
    future = session.get("%s/%s/%s" % (manifest.attrib["BaseUrl"], pack.attrib["RemotePath"], pack.attrib["Hash"]))
    future.pack_hash = pack.attrib["Hash"]
    futures.append(future)

idx = 0
for future in as_completed(futures):
    pack_data = gzip.decompress(future.result().content)
    for blob in blobs:
        if blob.attrib["PackHash"] != future.pack_hash:
            continue

        file_name = None
        for file in files:
            if file.attrib["Hash"] != blob.attrib["Hash"]:
                continue

            file_name = file.attrib["Name"]
            break
        else:
            continue

        size = int(blob.attrib["Size"])
        offset = int(blob.attrib["PackOffset"])
        os.makedirs(os.path.dirname(file_name), exist_ok=True)
        with open(file_name, "wb") as f:
            f.write(pack_data[offset:offset + size])

        idx += 1
        print("%s (%d/%d)" % (file_name, idx, len(packs)), flush=True)
print("::endgroup::", flush=True)

print("Moving includes", flush=True)
shutil.copytree("unreal/Engine/Source/Runtime/OodleDataCompression/Sdks", "Engine/Source/Runtime/OodleDataCompression/Sdks", dirs_exist_ok=True)
shutil.copytree("unreal/Engine/Plugins/Developer/TextureFormatOodle/Sdks", "Engine/Plugins/Developer/TextureFormatOodle/Sdks", dirs_exist_ok=True)
shutil.copytree("unreal/Engine/Plugins/Compression/OodleNetwork/Sdks", "Engine/Plugins/Compression/OodleNetwork/Sdks", dirs_exist_ok=True)

print("::group::Extracting zips", flush=True)
for file in glob.glob("Engine/**/*.zip", recursive=True):
    print(file, flush=True)
    with zipfile.ZipFile(file, 'r') as zip_ref:
        zip_ref.extractall(os.path.dirname(file))
print("::endgroup::", flush=True)

print("Done", flush=True)