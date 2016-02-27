INCLUDE_DIRECTORIES(${MiniProlog_BINARY_DIR}
		    ${MiniProlog_SOURCE_DIR}/wam)

LINK_DIRECTORIES(${MiniProlog_BINARY_DIR}/engine)

add_executable(profreport MainProfReport.cpp)

target_link_libraries(profreport engine)
