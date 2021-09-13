echo off
del *.exe /s  
del *.scr /s
del *.dcu /s 
del *.ddp /s
del *.ppu /s
del *.o /s
del *.~* /s
del *.log /s
del *.dsk /s
del *.dof /s
del *.bk? /s
del *.mps /s
del *.rst /s
del *.s /s
del *.a /s
del *.map /s
del *.rsm /s
del *.drc /s
del *.2007 /s
del *.local /s

rem delete more files

del *.bak /s
del *.xml /s
del *.cvsignore /s
del *.identcache /s
del *.otares /s
del *.tvsconfig /s
del *.stat /s
del *.db /s
del *.~dbg /s
del *.spider /s

rem delete cpp builder files

rem del *.hpp /s
del *.#00 /s
del *.pch /s
del *.tds /s
del *.ilc /s
del *.ild /s
del *.ilf /s
del *.ils /s
del *.pdi /s
del *.vlb /s


echo _
echo ************************************************
echo             Don't delete some files
echo ************************************************
echo _

attrib +R "AdvDemos/Q3Demo/Model/animation.cfg"
rem del *.cfg /s  - there are quake's animations
attrib -R "AdvDemos/Q3Demo/Model/animation.cfg"

del *.res /s
rem del *.ico /s  - some projects have own icos
rem del *.cur /s  - cursors
del *.obj /s 

echo---------------------------------------------------------
echo delete debug and Platform directories with all subdirectories and files 
for /r %1 %%R in (Win32) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (Win64) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (Debug) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (Release) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (Debug_Build) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (Release_Build) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (__history) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (__recovery) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (__astcache) do if exist "%%R" (rd /s /q "%%R")