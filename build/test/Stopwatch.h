#pragma once

#include <chrono>

namespace L4
{
    struct Stopwatch
    {
        Stopwatch() noexcept;

        void Start() noexcept;

        void Stop() noexcept;

        void Restart() noexcept;

        void Reset() noexcept;

        std::chrono::steady_clock::duration TimeElapsed() const noexcept;

        float TimeElapsedMs() const noexcept;

        bool IsRunning() const noexcept;

        static Stopwatch StartNew() noexcept;

    private:
        std::chrono::steady_clock::time_point StartTime;
        std::chrono::steady_clock::time_point StopTime;
    };
}