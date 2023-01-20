s_project(oodle::data::${PROJ_NAME} ${PROJ_TYPE})
s_add_dir(${oodle_data_SOURCE_DIR}/base)
s_add_dir(${oodle_data_SOURCE_DIR}/core)
s_end_sources()

s_include_directories(PRIVATE ${oodle_data_SOURCE_DIR}/core)
s_include_directories(PRIVATE ${oodle_data_SOURCE_DIR}/core/public)
s_include_directories(INTERFACE ${CMAKE_SOURCE_DIR}/../Engine/Source/Runtime/OodleDataCompression/Sdks/${PROJECT_VERSION}/include)

s_set_arch(AVX2)
s_compile_definitions(PRIVATE ${PROJ_DEF} OODLE_BUILDING_DATA)
