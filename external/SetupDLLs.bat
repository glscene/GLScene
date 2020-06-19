@echo off
echo Copying DLLs to the Windows System32 and SysWOW64 directories
echo Copying nVidia CG DLLs
rem win32
copy cg.dll %SystemRoot%\SysWOW64\
copy cgGL.dll %SystemRoot%\SysWOW64\
rem win64
copy cg.dll %SystemRoot%\System32\
copy cgGL.dll %SystemRoot%\System32\

echo.
echo Copying SDL2 DLLs
rem win32
copy sdl2.dll %SystemRoot%\SysWOW64\sdl2.dll
rem win64
copy sdl264.dll %SystemRoot%\System32\sdl264.dll

echo.
echo Copying Sound DLLs
rem win32
copy bass32.dll %SystemRoot%\SysWOW64\
copy fmod32.dll %SystemRoot%\SysWOW64\
copy OpenAL32.dll %SystemRoot%\SysWOW64\
rem win64
copy bass64.dll %SystemRoot%\System32\
copy fmod64.dll %SystemRoot%\System32\
copy OpenAL64.dll %SystemRoot%\System32\

echo.
echo Copying ODE DLLs
rem win32
copy ode32s.dll %SystemRoot%\SysWOW64\
copy ode32d.dll %SystemRoot%\SysWOW64\
rem win64
copy ode64s.dll %SystemRoot%\System32\
copy ode64d.dll %SystemRoot%\System32\

echo.
echo Copying Newton DLLs
rem win32
copy newton32.dll %SystemRoot%\SysWOW64\
copy dJointLibrary32.dll %SystemRoot%\SysWOW64\
rem win64
copy newton64.dll %SystemRoot%\System32\
copy dJointLibrary64.dll %SystemRoot%\System32\

echo.
echo Copying nVidia PhysX DLLs
rem win32
copy PhysXwrap32.dll %SystemRoot%\SysWOW64\
rem win64
copy PhysXwrap64.dll %SystemRoot%\System32\

echo.
echo Copying CUDA and zlib1 DLLs
rem win32
copy cutil32.dll %SystemRoot%\SysWOW64\
copy zlib1.dll %SystemRoot%\SysWOW64\
rem win64
copy cutil64.dll %SystemRoot%\System32\
copy zlib1.dll %SystemRoot%\System32\
pause