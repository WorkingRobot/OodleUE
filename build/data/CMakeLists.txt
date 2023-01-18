s_project(oodle_data STATIC_LIB)
s_add_dir_recursive(${oodle_data_SOURCE_DIR})
s_end_sources()

s_include_directories(PRIVATE ${oodle_data_SOURCE_DIR}/core)
s_include_directories(PRIVATE ${oodle_data_SOURCE_DIR}/core/public)

s_set_arch(AVX2)
s_compile_definitions(PRIVATE OODLE_BUILDING_LIB OODLE_BUILDING_DATA)
