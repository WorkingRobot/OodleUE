cmake_minimum_required(VERSION 3.20)
cmake_policy(SET CMP0091 NEW)

include(FetchContent)
FetchContent_Declare(
  smake
  GIT_REPOSITORY https://github.com/WorkingRobot/smake.git
  GIT_TAG        d8b2f8bac5df81035871c990a88e49a537d38660
)
FetchContent_MakeAvailable(smake)
include(${smake_SOURCE_DIR}/smake.cmake)

## Project Definition ##

s_get_version_info(DEFAULT_BRANCH "main")
project(${PROJECT_NAME} VERSION ${PROJECT_VERSION} LANGUAGES C CXX)

message(STATUS "Compiling ${PROJECT_NAME} ${PROJECT_VERSION} for ${CMAKE_SYSTEM_NAME}")

## Set output directories ##

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/ar)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

## Define content ##

FetchContent_Declare(
  oodle_data
  URL https://github.com/WorkingRobot/UECommitDep/raw/main/Engine/Source/Runtime/OodleDataCompression/Sdks/${PROJECT_VERSION}/src/oodle2_src_for_unreal_data_${PROJECT_VERSION}.zip
  DOWNLOAD_EXTRACT_TIMESTAMP FALSE
)

FetchContent_Declare(
  oodle_texture
  URL https://github.com/WorkingRobot/UECommitDep/raw/main/Engine/Plugins/Developer/TextureFormatOodle/Sdks/${PROJECT_VERSION}/src/oodle2_src_for_unreal_texture_${PROJECT_VERSION}.zip
  DOWNLOAD_EXTRACT_TIMESTAMP FALSE
)

FetchContent_Declare(
  oodle_network
  URL https://github.com/WorkingRobot/UECommitDep/raw/main/Engine/Plugins/Compression/OodleNetwork/Sdks/${PROJECT_VERSION}/src/oodle2_src_for_unreal_network_${PROJECT_VERSION}.zip
  DOWNLOAD_EXTRACT_TIMESTAMP FALSE
)

FetchContent_MakeAvailable(oodle_data oodle_texture oodle_network)

## Add projects ##

add_subdirectory(data)
add_subdirectory(texture)
add_subdirectory(network)
