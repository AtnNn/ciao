INCLUDE_DIRECTORIES(${MiniProlog_BINARY_DIR}
		    ${MiniProlog_SOURCE_DIR}/wam)

LINK_DIRECTORIES(${MiniProlog_BINARY_DIR}/engine)

add_executable(profview MainProfView.cpp)

target_link_libraries(profview engine)
