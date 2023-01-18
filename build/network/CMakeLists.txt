s_project(oodle_network STATIC_LIB)
s_add_dir_recursive(${oodle_network_SOURCE_DIR}/network)
s_del_file(${oodle_network_SOURCE_DIR}/network/testoo2netstub.cpp)
s_end_sources()

s_include_directories(PRIVATE ${oodle_data_SOURCE_DIR}/core)
s_include_directories(PRIVATE ${oodle_data_SOURCE_DIR}/core/public)

s_set_arch(AVX512)
s_compile_definitions(PRIVATE OODLE_BUILDING_LIB OODLE_BUILDING_NETWORK)
