s_project(oodle::test EXEC IS_SIMPLE)

s_link_libraries(PRIVATE oodle::data::static)

s_set_properties(PROPERTIES OUTPUT_NAME "ozip")
