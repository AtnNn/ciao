GET_TARGET_PROPERTY(CONFIGURE_EXE configure LOCATION)

INCLUDE_DIRECTORIES(${MiniProlog_BINARY_DIR})

ADD_CUSTOM_COMMAND(
	OUTPUT ${MiniProlog_BINARY_DIR}/Configure.cpp
	       ${MiniProlog_BINARY_DIR}/Configure.h
	       ${MiniProlog_BINARY_DIR}/EngineLoop.h
	       ${MiniProlog_BINARY_DIR}/InstructionBC.h
	       ${MiniProlog_BINARY_DIR}/../miniprolog_auto.pl
	COMMAND ${CONFIGURE_EXE}
	WORKING_DIRECTORY ${MiniProlog_BINARY_DIR}
	DEPENDS configure
	)

ADD_LIBRARY(engine Atoms.cpp Functions.cpp Program.cpp WAMUtils.cpp
	       Compiler.cpp InstructionSpec.cpp PrologCompiler.cpp
	       CompilerStructure.cpp QueryCompiler.cpp Engine.cpp
	       Profiler.cpp Variable.cpp hrtime.cpp
	       ${MiniProlog_BINARY_DIR}/Configure.cpp)

LINK_DIRECTORIES(${MiniProlog_BINARY_DIR}/engine)

add_executable(wam MainWAM.cpp)

target_link_libraries(wam engine)
