import gzip, os, shutil, glob, zipfile
import xml.etree.ElementTree as ET

from concurrent.futures import as_completed
from requests_futures.sessions import FuturesSession

print("Parsing Commit.gitdeps.xml", flush=True)
manifest = ET.parse("unreal/Engine/Build/Commit.gitdeps.xml").getroot()

print("Parsing files", flush=True)
files = []
for file in manifest.find("Files"):
    if "Oodle" in file.attrib["Name"] and "Sdk" in file.attrib["Name"]:
        files.append(file)
print(f"Found {len(files)} files")

print("Parsing blobs", flush=True)
blobs = []
for blob in manifest.find("Blobs"):
    in_blobs = False
    for file in files:
        if file.attrib["Hash"] == blob.attrib["Hash"]:
            if not in_blobs:
                blobs.append(blob)
                in_blobs = True
                blob.files = [file]
            else:
                blob.files.append(file)
print(f"Found {len(blobs)} blobs", flush=True)

print("Parsing packs", flush=True)
packs = []
for pack in manifest.find("Packs"):
    in_packs = False
    for blob in blobs:
        if blob.attrib["PackHash"] == pack.attrib["Hash"]:
            if not in_packs:
                packs.append(pack)
                in_packs = True
                pack.blobs = [blob]
            else:
                pack.blobs.append(blob)
print(f"Found {len(packs)} packs", flush=True)

print("::group::Grabbing files", flush=True)
session = FuturesSession(max_workers=64)
futures = []
for pack in packs:
    future = session.get(f"{manifest.attrib['BaseUrl']}/{pack.attrib['RemotePath']}/{pack.attrib['Hash']}")
    future.pack = pack
    futures.append(future)

idx = 0
for future in as_completed(futures):
    pack_data = gzip.decompress(future.result().content)
    for blob in future.pack.blobs:
        size = int(blob.attrib["Size"])
        offset = int(blob.attrib["PackOffset"])

        for file in blob.files:
            file_name = file.attrib["Name"]
            os.makedirs(os.path.dirname(file_name), exist_ok=True)
            with open(file_name, "wb") as f:
                f.write(pack_data[offset:offset + size])
            idx += 1
            print(f"{file_name} ({idx}/{len(files)})", flush=True)
print("::endgroup::", flush=True)

print("::group::Moving includes", flush=True)
def movetree(dst):
    src = f"unreal/{dst}"
    print(dst, flush=True)
    if os.path.exists(src):
        shutil.copytree(src, dst, dirs_exist_ok=True)
movetree("Engine/Source/Runtime/OodleDataCompression/Sdks")
movetree("Engine/Source/Developer/TextureFormatOodle/Sdks")
movetree("Engine/Plugins/Compression/OodleNetwork/Sdks")
print("::endgroup::", flush=True)

print("::group::Extracting zips", flush=True)
for file in glob.glob("Engine/**/*.zip", recursive=True):
    print(file, flush=True)
    with zipfile.ZipFile(file, 'r') as zip_ref:
        zip_ref.extractall(os.path.dirname(file))
print("::endgroup::", flush=True)

print("Done", flush=True)