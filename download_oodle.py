import requests, gzip, os, shutil
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

print("Grabbing files", flush=True)
for pack in packs:
    packData = gzip.decompress(requests.get("%s/%s/%s" % (manifest.attrib["BaseUrl"], pack.attrib["RemotePath"], pack.attrib["Hash"])).content)
    for blob in blobs:
        if blob.attrib["PackHash"] == pack.attrib["Hash"]:
            Size = int(blob.attrib["Size"])
            Offset = int(blob.attrib["PackOffset"])
            FileName = None
            for file in files:
                if file.attrib["Hash"] == blob.attrib["Hash"]:
                    FileName = file.attrib["Name"]
                    break
            if not FileName:
                continue
            print(FileName, flush=True)
            os.makedirs(os.path.dirname(FileName), exist_ok=True)
            with open(FileName, "wb") as f:
                f.write(packData[Offset:Offset + Size])

print("Moving includes", flush=True)
os.replace("unreal/Engine/Build/Commit.gitdeps.xml", "Commit.gitdeps.xml")
shutil.copytree("unreal/Engine/Source/Runtime/OodleDataCompression/Sdks", "Engine/Source/Runtime/OodleDataCompression/Sdks", dirs_exist_ok=True)
shutil.copytree("unreal/Engine/Plugins/Developer/TextureFormatOodle/Sdks", "Engine/Plugins/Developer/TextureFormatOodle/Sdks", dirs_exist_ok=True)
shutil.copytree("unreal/Engine/Plugins/Compression/OodleNetwork/Sdks", "Engine/Plugins/Compression/OodleNetwork/Sdks", dirs_exist_ok=True)

print("Done", flush=True)