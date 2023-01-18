import requests, gzip, os, shutil, glob, zipfile
import xml.etree.ElementTree as ET

print("Moving gitdeps", flush=True)
os.replace("unreal/Engine/Build/Commit.gitdeps.xml", "Commit.gitdeps.xml")

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
idx = 0
for pack in packs:
    idx += 1
    pack_data = gzip.decompress(requests.get("%s/%s/%s" % (manifest.attrib["BaseUrl"], pack.attrib["RemotePath"], pack.attrib["Hash"])).content)
    for blob in blobs:
        if blob.attrib["PackHash"] == pack.attrib["Hash"]:
            file_name = None
            for file in files:
                if file.attrib["Hash"] == blob.attrib["Hash"]:
                    file_name = file.attrib["Name"]
                    break
            if not file_name:
                continue
            size = int(blob.attrib["Size"])
            offset = int(blob.attrib["PackOffset"])
            os.makedirs(os.path.dirname(file_name), exist_ok=True)
            with open(file_name, "wb") as f:
                f.write(pack_data[offset:offset + size])
            print("%s (%d/%d)" % (file_name, idx, len(packs)), flush=True)
print("::endgroup::", flush=True)

print("Moving includes", flush=True)
shutil.copytree("unreal/Engine/Source/Runtime/OodleDataCompression/Sdks", "Engine/Source/Runtime/OodleDataCompression/Sdks", dirs_exist_ok=True)
shutil.copytree("unreal/Engine/Plugins/Developer/TextureFormatOodle/Sdks", "Engine/Plugins/Developer/TextureFormatOodle/Sdks", dirs_exist_ok=True)
shutil.copytree("unreal/Engine/Plugins/Compression/OodleNetwork/Sdks", "Engine/Plugins/Compression/OodleNetwork/Sdks", dirs_exist_ok=True)

print("::group::Extracting zips", flush=True)
for file in glob.glob("unreal/**/*.zip", recursive=True):
    print(file, flush=True)
    with zipfile.ZipFile(file, 'r') as zip_ref:
        zip_ref.extractall(os.path.dirname(file))
print("::endgroup::", flush=True)

print("Done", flush=True)