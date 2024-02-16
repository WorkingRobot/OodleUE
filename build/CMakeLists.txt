cmake_minimum_required(VERSION 3.20)
cmake_policy(SET CMP0091 NEW)

include(FetchContent)
FetchContent_Declare(
  smake
  GIT_REPOSITORY https://github.com/WorkingRobot/smake.git
  GIT_TAG        ab947d3000e8f7d0b188118b4205db521449cdd0
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

function(FetchContent_DeclareWithPatch NAME PATH)
  if(EXISTS "${CMAKE_SOURCE_DIR}/${PRESET_NAME}-${NAME}.patch")
    FetchContent_Declare(
      oodle_${NAME}
      URL ${PATH}
      PATCH_COMMAND patch --verbose --binary -p1 < ${CMAKE_SOURCE_DIR}/${PRESET_NAME}-${NAME}.patch
      LOG_PATCH ON
      DOWNLOAD_EXTRACT_TIMESTAMP FALSE
    )
    message(STATUS "Applying patch ${PRESET_NAME}-${NAME}.patch")
  else()
    FetchContent_Declare(
      oodle_${NAME}
      URL ${PATH}
      DOWNLOAD_EXTRACT_TIMESTAMP FALSE
    )
  endif()

endfunction()

FetchContent_DeclareWithPatch(
  data
  ${CMAKE_SOURCE_DIR}/../Engine/Source/Runtime/OodleDataCompression/Sdks/${PROJECT_VERSION}/src/oodle2_src_for_unreal_data_${PROJECT_VERSION}.zip
)

FetchContent_DeclareWithPatch(
  texture
  ${CMAKE_SOURCE_DIR}/../Engine/Plugins/Developer/TextureFormatOodle/Sdks/${PROJECT_VERSION}/src/oodle2_src_for_unreal_texture_${PROJECT_VERSION}.zip
)

FetchContent_DeclareWithPatch(
  network
  ${CMAKE_SOURCE_DIR}/../Engine/Plugins/Compression/OodleNetwork/Sdks/${PROJECT_VERSION}/src/oodle2_src_for_unreal_network_${PROJECT_VERSION}.zip
)

FetchContent_MakeAvailable(oodle_data oodle_texture oodle_network)

file(COPY ${oodle_texture_SOURCE_DIR}/texture DESTINATION ${oodle_data_SOURCE_DIR})
file(COPY ${oodle_network_SOURCE_DIR}/network DESTINATION ${oodle_data_SOURCE_DIR})

## Add projects ##

add_subdirectory(data)
add_subdirectory(texture)
add_subdirectory(network)
add_subdirectory(test)
