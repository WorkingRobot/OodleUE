import requests, gzip, os
import xml.etree.ElementTree as ET

print("Moving")
os.replace("unreal/Engine/Build/Commit.gitdeps.xml", "Commit.gitdeps.xml")

print("Parsing")
manifest = ET.parse("Commit.gitdeps.xml").getroot()

print("Parsing files")
files = []
for file in manifest.find("Files"):
    if "Oodle" in file.attrib["Name"] and "Sdk" in file.attrib["Name"]:
        files.append(file)

print("Parsing blobs")
blobs = []
for blob in manifest.find("Blobs"):
    for file in files:
        if file.attrib["Hash"] == blob.attrib["Hash"]:
            blobs.append(blob)
            break

print("Parsing packs")
packs = []
for pack in manifest.find("Packs"):
    for blob in blobs:
        if blob.attrib["PackHash"] == pack.attrib["Hash"]:
            packs.append(pack)
            break

print("Grabbing files")
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
            print(FileName)
            os.makedirs(os.path.dirname(FileName), exist_ok=True)
            with open(FileName, "wb") as f:
                f.write(packData[Offset:Offset + Size])

print("Done")