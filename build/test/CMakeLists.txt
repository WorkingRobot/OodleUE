s_project(oodle::test EXEC IS_SIMPLE)

s_link_libraries(PRIVATE oodle::data::static)

# clang-cl complains
s_compile_options(PRIVATE /EHsc)