@echo off
echo Copying DLLs to the Windows System32 and SysWOW64 directories
echo Copying nVidia CG DLLs
rem win32
copy "%~dp0cg.dll" %SystemRoot%\SysWOW64\
copy "%~dp0cgGL.dll" %SystemRoot%\SysWOW64\
rem win64
copy "%~dp0cg.dll" %SystemRoot%\System32\
copy "%~dp0cgGL.dll" %SystemRoot%\System32\

echo.
echo Copying SDL2 DLLs
rem win32
copy "%~dp0sdl2_32.dll" %SystemRoot%\SysWOW64\sdl2_32.dll
rem win64
copy "%~dp0sdl2_64.dll" %SystemRoot%\System32\sdl2_64.dll

echo.
echo Copying Sound DLLs
rem win32
copy "%~dp0bass32.dll" %SystemRoot%\SysWOW64\
copy "%~dp0fmod32.dll" %SystemRoot%\SysWOW64\
copy "%~dp0OpenAL32.dll" %SystemRoot%\SysWOW64\
rem win64
copy "%~dp0bass64.dll" %SystemRoot%\System32\
copy "%~dp0fmod64.dll" %SystemRoot%\System32\
copy "%~dp0OpenAL64.dll" %SystemRoot%\System32\

echo.
echo Copying ODE DLLs
rem win32
copy "%~dp0ode32s.dll" %SystemRoot%\SysWOW64\
copy "%~dp0ode32d.dll" %SystemRoot%\SysWOW64\
rem win64
copy "%~dp0ode64s.dll" %SystemRoot%\System32\
copy "%~dp0ode64d.dll" %SystemRoot%\System32\

echo.
echo Copying Newton DLLs
rem win32
copy "%~dp0newton32.dll" %SystemRoot%\SysWOW64\
rem win64
copy "%~dp0newton64.dll" %SystemRoot%\System32\

echo.
echo Copying nVidia PhysX DLLs
rem win32
copy "%~dp0PhysXwrap32.dll" %SystemRoot%\SysWOW64\
rem win64
copy "%~dp0PhysXwrap64.dll" %SystemRoot%\System32\

echo.
echo Copying CUDA and zlib1 DLLs
rem win32
copy "%~dp0cutil32.dll" %SystemRoot%\SysWOW64\
copy "%~dp0zlib1.dll" %SystemRoot%\SysWOW64\
rem win64
copy "%~dp0cutil64.dll" %SystemRoot%\System32\
copy "%~dp0zlib1.dll" %SystemRoot%\System32\

pause