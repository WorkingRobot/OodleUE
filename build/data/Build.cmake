s_project(oodle::data::${PROJ_NAME} ${PROJ_TYPE})
s_add_dir(${oodle_data_SOURCE_DIR}/base)
s_add_dir(${oodle_data_SOURCE_DIR}/core)
s_end_sources()

s_include_directories(PRIVATE ${oodle_data_SOURCE_DIR}/core)
s_include_directories(PRIVATE ${oodle_data_SOURCE_DIR}/core/public)
s_include_directories(INTERFACE ${CMAKE_SOURCE_DIR}/../Engine/Source/Runtime/OodleDataCompression/Sdks/${PROJECT_VERSION}/include)

if(CMAKE_SYSTEM_PROCESSOR MATCHES "^(x86_64|AMD64|i386|i686|x86)$")
    s_set_arch(AVX2)
endif()
s_set_cxx_standard(20)
s_compile_definitions(PRIVATE ${PROJ_DEF} OODLE_BUILDING_DATA)
