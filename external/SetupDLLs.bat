@echo off
echo Copying DLLs to the Windows System32 and SysWOW64 directories
echo Copying nVidia CG DLLs
rem win64
copy cg.dll %SystemRoot%\System32\
copy cgGL.dll %SystemRoot%\System32\
rem win32
copy cg.dll %SystemRoot%\SysWOW64\
copy cgGL.dll %SystemRoot%\SysWOW64\

echo Copying SDL2 DLLs
rem win32
copy sdl2.dll %SystemRoot%\SysWOW64\sdl2.dll
rem win64
copy sdl264.dll %SystemRoot%\System32\sdl264.dll

echo Copying Sound DLLs
rem win32
copy bass.dll %SystemRoot%\SysWOW64\
copy fmod.dll %SystemRoot%\SysWOW64\
copy OpenAL32.dll %SystemRoot%\SysWOW64\
rem win64
copy bass64.dll %SystemRoot%\System32\
copy fmod.dll %SystemRoot%\System32\
copy OpenAL64.dll %SystemRoot%\System32\

echo Copying ODE DLLs
rem win32
copy ode_single.dll %SystemRoot%\SysWOW64\
copy ode_double.dll %SystemRoot%\SysWOW64\
rem win64
copy ode_single64.dll %SystemRoot%\System32\
copy ode_double64.dll %SystemRoot%\System32\

echo Copying Newton DLLs
rem win32
copy newton.dll %SystemRoot%\SysWOW64\
copy dJointLibrary.dll %SystemRoot%\SysWOW64\
rem win64
copy newton64.dll %SystemRoot%\System32\
copy dJointLibrary64.dll %SystemRoot%\System32\

echo Copying nVidia PhysX DLLs
rem win32
copy PhysXwrap.dll %SystemRoot%\SysWOW64\
rem win64
copy PhysXwrap.dll %SystemRoot%\System32\

echo Copying CUDA and zlib1 DLLs
rem win32
copy cutil32.dll %SystemRoot%\SysWOW64\
copy zlib1.dll %SystemRoot%\SysWOW64\
rem win64
copy cutil64.dll %SystemRoot%\System32\
copy zlib1.dll %SystemRoot%\System32\
