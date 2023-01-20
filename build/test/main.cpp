#include <oodle2.h>

#include <cstdio>
#include <memory>
#include <filesystem>
#include "Stopwatch.h"

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

static constexpr OodleLZ_Compressor Compressor = OodleLZ_Compressor_Hydra;
static constexpr OodleLZ_CompressionLevel CompressionLevel = OodleLZ_CompressionLevel_Optimal5;

std::pair<std::unique_ptr<std::byte[]>, size_t> ReadFile(const std::filesystem::path& Path)
{
    auto Size = std::filesystem::file_size(Path);
    auto Ret = std::make_pair(std::make_unique_for_overwrite<std::byte[]>(Size), Size);

    FILE* File = fopen(Path.string().c_str(), "rb");
    fread(Ret.first.get(), sizeof(std::byte), Size, File);
    fclose(File);

    return Ret;
}

void WriteFile(const std::filesystem::path& Path, const std::unique_ptr<std::byte[]>& Data, size_t DataSize)
{
    FILE* File = fopen(Path.string().c_str(), "wb");
    fwrite(Data.get(), sizeof(std::byte), DataSize, File);
    fclose(File);
}

void CreateData(const std::filesystem::path& Path)
{
    FILE* File = fopen(Path.string().c_str(), "wb");

    for (int i = 0; i < 1 << 20; ++i)
    {
        fwrite(&i, sizeof(i), 1, File);
    }

    fclose(File);
}

struct OodleCallbacks
{
    decltype(&OodleLZ_GetCompressedBufferSizeNeeded) GetCompressedBufferSizeNeeded;
    decltype(&OodleLZ_Compress) Compress;
    decltype(&OodleLZ_Decompress) Decompress;
};

class Tester
{
public:
    Tester(const OodleCallbacks& Callbacks) :
        Callbacks(Callbacks)
    {

    }

    size_t Compress(const std::filesystem::path& Path)
    {
        auto [Data, DataSize] = ReadFile(Path);

        auto CompData = std::make_unique_for_overwrite<std::byte[]>(Callbacks.GetCompressedBufferSizeNeeded(Compressor, DataSize));

        auto Timer = L4::Stopwatch::StartNew();
        auto CompSize = Callbacks.Compress(Compressor, Data.get(), DataSize, CompData.get(), CompressionLevel, NULL, NULL, NULL, NULL, 0);
        Timer.Stop();
        printf("Compression of %s took %.2fms\n", Path.string().c_str(), Timer.TimeElapsedMs());

        if (CompSize == OODLELZ_FAILED)
        {
            throw std::runtime_error("Could not compress file");
        }

        WriteFile(Path.string() + ".comp", CompData, CompSize);

        return DataSize;
    }

    void Decompress(const std::filesystem::path& Path, size_t ExpectedDecompSize)
    {
        auto [Data, DataSize] = ReadFile(Path.string() + ".comp");

        auto DecompData = std::make_unique_for_overwrite<std::byte[]>(ExpectedDecompSize);

        auto Timer = L4::Stopwatch::StartNew();
        auto DecompSize = Callbacks.Decompress(Data.get(), DataSize, DecompData.get(), ExpectedDecompSize, OodleLZ_FuzzSafe_Yes, OodleLZ_CheckCRC_No, OodleLZ_Verbosity_None, NULL, 0, NULL, NULL, NULL, 0, OodleLZ_Decode_Unthreaded);
        Timer.Stop();
        printf("Decompression of %s took %.2fms\n", Path.string().c_str(), Timer.TimeElapsedMs());

        if (DecompSize == OODLELZ_FAILED)
        {
            throw std::runtime_error("Could not decompress file");
        }

        if (DecompSize != ExpectedDecompSize)
        {
            throw std::runtime_error("Decompressed size is not the expected value");
        }

        WriteFile(Path.string() + ".decomp", DecompData, DecompSize);
    }

private:
    OodleCallbacks Callbacks;
};

int main()
{
    {
        CreateData("test.bin");

        Tester Test(OodleCallbacks{
            .GetCompressedBufferSizeNeeded = OodleLZ_GetCompressedBufferSizeNeeded,
            .Compress = OodleLZ_Compress,
            .Decompress = OodleLZ_Decompress,
            });

        auto DataSize = Test.Compress("test.bin");
        Test.Decompress("test.bin", DataSize);
    }

    {
        auto Lib = LoadLibraryA("C:/Users/Asriel/Downloads/oo2core_9_win64.dll");

        CreateData("test2.bin");

        Tester Test(OodleCallbacks{
            .GetCompressedBufferSizeNeeded = (decltype(&OodleLZ_GetCompressedBufferSizeNeeded))GetProcAddress(Lib, "OodleLZ_GetCompressedBufferSizeNeeded"),
            .Compress = (decltype(&OodleLZ_Compress))GetProcAddress(Lib, "OodleLZ_Compress"),
            .Decompress = (decltype(&OodleLZ_Decompress))GetProcAddress(Lib, "OodleLZ_Decompress"),
            });

        auto DataSize = Test.Compress("test2.bin");
        Test.Decompress("test2.bin", DataSize);

        FreeLibrary(Lib);
    }
}