The directory for PasDoc scripts
------------------------------------------------------------
To generate GLSysHelp.chm you need to follow the next steps:
1.Download PasDoc program from https://github.com/pasdoc/pasdoc and setup it.
2.Run the pasdoc_gui program and Open GLSysHelp.pds script 
3.Click on the button Generate and create GLScene's help system files in HTML folder
4.Download HTML Help Workshop 1.3 software from Microsoft site and setup it.
5.Run the hhw.exe program from C:\Program Files (x86)\HTML Help Workshop\
6.Open GLSysHelp.hhp project in ..\GLSysHelp\HTML\ folder and compile it.
7.Clean HTML folder using _CleanHTML.bat and fimd there the file GLSysHelp.chm
