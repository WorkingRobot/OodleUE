s_project(oodle::data::${PROJ_NAME} ${PROJ_TYPE})
s_add_dir(${oodle_data_SOURCE_DIR}/base)
s_add_dir(${oodle_data_SOURCE_DIR}/core)
s_end_sources()

s_include_directories(PUBLIC ${oodle_data_SOURCE_DIR}/core)
s_include_directories(PUBLIC ${oodle_data_SOURCE_DIR}/core/public)

s_set_arch(AVX512)
s_compile_definitions(PUBLIC ${PROJ_DEF} OODLE_NNS)
#s_compile_definitions(PRIVATE OODLE_BUILDING_DATA)
