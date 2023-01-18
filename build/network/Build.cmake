s_project(oodle::network::${PROJ_NAME} ${PROJ_TYPE})
s_add_dir_recursive(${oodle_data_SOURCE_DIR})
s_add_dir_recursive(${oodle_network_SOURCE_DIR}/network)
s_del_file(${oodle_network_SOURCE_DIR}/network/testoo2netstub.cpp)
s_end_sources()

s_include_directories(PRIVATE ${oodle_data_SOURCE_DIR}/core)
s_include_directories(PRIVATE ${oodle_data_SOURCE_DIR}/core/public)

s_set_arch(AVX512)
s_compile_definitions(PRIVATE ${PROJ_DEF} OODLE_BUILDING_NETWORK)
