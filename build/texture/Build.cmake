s_project(oodle::texture::${PROJ_NAME} ${PROJ_TYPE})
s_add_dir_recursive(${oodle_data_SOURCE_DIR}/base)
s_add_dir_recursive(${oodle_data_SOURCE_DIR}/core)
s_add_dir_recursive(${oodle_data_SOURCE_DIR}/texture)
s_end_sources()

s_include_directories(PRIVATE ${oodle_data_SOURCE_DIR}/core)
s_include_directories(PRIVATE ${oodle_data_SOURCE_DIR}/core/public)
s_include_directories(PRIVATE ${CMAKE_CURRENT_LIST_DIR}/include)

s_set_arch(AVX512)
s_compile_definitions(PRIVATE ${PROJ_DEF} OODLE_BUILDING_TEXTURE)
