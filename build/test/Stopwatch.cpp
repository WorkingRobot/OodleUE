#include "Stopwatch.h"

namespace L4
{
    Stopwatch::Stopwatch() noexcept :
        StartTime(StartTime.min()),
        StopTime(StopTime.min())
    {
    }

    void Stopwatch::Start() noexcept
    {
        if (IsRunning())
        {
            return;
        }

        if (StopTime != StopTime.min())
        {
            StartTime = std::chrono::steady_clock::now() - TimeElapsed();
            StopTime = StopTime.min();
        }
        else
        {
            StartTime = std::chrono::steady_clock::now();
        }
    }

    void Stopwatch::Stop() noexcept
    {
        StopTime = std::chrono::steady_clock::now();
    }

    void Stopwatch::Restart() noexcept
    {
        Reset();
        Start();
    }

    void Stopwatch::Reset() noexcept
    {
        StartTime = StopTime = StartTime.min();
    }

    std::chrono::steady_clock::duration Stopwatch::TimeElapsed() const noexcept
    {
        if (StartTime == StartTime.min())
        {
            return std::chrono::steady_clock::duration(0);
        }

        if (StopTime == StopTime.min())
        {
            return std::chrono::steady_clock::now() - StartTime;
        }

        return StopTime - StartTime;
    }

    float Stopwatch::TimeElapsedMs() const noexcept
    {
        return TimeElapsed().count() / 1000000.f;
    }

    bool Stopwatch::IsRunning() const noexcept
    {
        return StartTime != StartTime.min() && StopTime == StopTime.min();
    }

    Stopwatch Stopwatch::StartNew() noexcept
    {
        Stopwatch Ret;
        Ret.Start();
        return Ret;
    }
}