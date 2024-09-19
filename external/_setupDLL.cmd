@echo off
echo Copying DLLs to the Windows System32 and SysWOW64 dirs

echo copying CG nVidia win32/win64
copy "%~dp0cg.dll" %SystemRoot%\System32\
copy "%~dp0cgGL.dll" %SystemRoot%\System32\
copy "%~dp0cg.dll" %SystemRoot%\SysWOW64\
copy "%~dp0cgGL.dll" %SystemRoot%\SysWOW64\

echo copying SDL2 win32/win64
copy "%~dp0sdl2.dll" %SystemRoot%\System32\sdl2.dll
copy "%~dp0sdl2.dll" %SystemRoot%\SysWOW64\sdl2.dll

echo copying Sound win32/win64
copy "%~dp0bass64.dll" %SystemRoot%\System32\
copy "%~dp0fmod64.dll" %SystemRoot%\System32\
copy "%~dp0OpenAL64.dll" %SystemRoot%\System32\
copy "%~dp0bass32.dll" %SystemRoot%\SysWOW64\
copy "%~dp0fmod32.dll" %SystemRoot%\SysWOW64\
copy "%~dp0OpenAL32.dll" %SystemRoot%\SysWOW64\

echo copying ODE DLLs win32/win64
copy "%~dp0ode64s.dll" %SystemRoot%\System32\
copy "%~dp0ode64d.dll" %SystemRoot%\System32\
copy "%~dp0ode32s.dll" %SystemRoot%\SysWOW64\
copy "%~dp0ode32d.dll" %SystemRoot%\SysWOW64\

echo copying Newton win32/win64
copy "%~dp0newton64.dll" %SystemRoot%\System32\
copy "%~dp0newton32.dll" %SystemRoot%\SysWOW64\

echo copying nVidia PhysX win32/win64
copy "%~dp0PhysXwrap64.dll" %SystemRoot%\System32\
copy "%~dp0PhysXwrap32.dll" %SystemRoot%\SysWOW64\

echo copying CUDA gpu win32/win64
copy "%~dp0cutil64.dll" %SystemRoot%\System32\
copy "%~dp0cutil32.dll" %SystemRoot%\SysWOW64\

echo copying GLUT DLLs win32/win64
copy "%~dp0glut32.dll" %SystemRoot%\System32\
copy "%~dp0glut32.dll" %SystemRoot%\SysWOW64\

pause