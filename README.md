# OodleUE

[![Build](https://github.com/WorkingRobot/OodleUE/actions/workflows/build.yml/badge.svg)](https://github.com/WorkingRobot/OodleUE/releases)

Despite being one of the best compression libraries out there, Oodle is notoriously difficult to obtain. This repository is updated daily with the latest Oodle builds for Unreal Engine 5.

---

### Latest Supported Oodle Version: 2.9.12
If this is out of date, please [let me know](https://camora.dev) so I can update the build system accordingly. You can see the latest changelog [here](https://www.radgametools.com/oodlehist.htm).

## Repository Layout

- [CMake Build Files](/build)
- [GitHub Actions Binaries](https://github.com/WorkingRobot/OodleUE/releases)
- Oodle Data
    - [Source Code](/Engine/Source/Runtime/OodleDataCompression/Sdks/2.9.12/src)
    - [Documentation](https://htmlpreview.github.io/?https://github.com/WorkingRobot/OodleUE/blob/main/Engine/Source/Runtime/OodleDataCompression/Sdks/2.9.12/help/oodle2.html)
    - [Includes](/Engine/Source/Runtime/OodleDataCompression/Sdks/2.9.12/include)
    - [Official Binaries](/Engine/Source/Runtime/OodleDataCompression/Sdks/2.9.12/lib)
        - [Distributable Binaries](/Engine/Source/Programs/Shared/EpicGames.Oodle/Sdk/2.9.10) (Only up to 2.9.10)
        - [VisionOS](/Engine/Platforms/VisionOS/Source/Runtime/OodleDataCompression/Sdks/2.9.12/lib)
        - [Hololens](/Engine/Platforms/Hololens/Source/Runtime/OodleDataCompression/Sdks/2.9.8/lib) (Only up to 2.9.8)
- Oodle Network
    - [Source Code](/Engine/Plugins/Compression/OodleNetwork/Sdks/2.9.12/src)
    - [Includes](/Engine/Plugins/Compression/OodleNetwork/Sdks/2.9.12/include)
    - [Official Binaries](/Engine/Plugins/Compression/OodleNetwork/Sdks/2.9.12/lib)
        - [VisionOS](/Engine/Platforms/VisionOS/Plugins/Compression/OodleNetwork/Sdks/2.9.12/lib)
- Oodle Texture
    - [Source Code](/Engine/Plugins/Developer/TextureFormatOodle/Sdks/2.9.12/src)
    - [Includes](/Engine/Plugins/Developer/TextureFormatOodle/Sdks/2.9.12/include)
    - [Official Binaries](/Engine/Plugins/Developer/TextureFormatOodle/Sdks/2.9.12/lib)
        - [Distributable Binaries](/Engine/Plugins/Developer/TextureFormatOodle/Sdks/2.9.12/redist)

## EULA Notice

**By using these assets, you agree to the [Unreal Engine EULA](https://www.unrealengine.com/eula/unreal). I take zero ownership of any code or build artifacts that are retrieved from Epic's servers and Git repository.**

This repository is available to allow for easy installation of Oodle for those who don't have Unreal Engine constantly kept up to date. It also theoretically allows package managers to be able to provide Oodle to its end users. However, considering that Oodle is not my IP, I believe that there is a non-zero chance that someone at Epic Games or RAD doesn't want this repository to be publicly available. If that is indeed the case, simply contact me and I can take down this repository and/or work with you to find a viable solution.