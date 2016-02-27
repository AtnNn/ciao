call "C:/Archivos de programa/Microsoft Visual Studio 9.0/Common7/Tools/vsvars32.bat"
"C:/Archivos de programa/CMake 2.4/bin/cmake.exe" -G "NMake Makefiles" -DCMAKE_C_COMPILER="C:/Archivos de programa/Microsoft Visual Studio 9.0/VC/bin/cl.exe" -DCMAKE_RC_COMPILER="C:/Archivos de programa/Microsoft SDKs/Windows/v6.0A/bin/rc.exe" ../../src

@REM Undefine MAKEFLAGS which has been set by gnu make
SET MAKEFLAGS=

"C:/Archivos de programa/Microsoft Visual Studio 9.0/VC/bin/nmake.exe"
