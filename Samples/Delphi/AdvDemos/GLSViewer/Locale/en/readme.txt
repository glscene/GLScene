The directory for GLScene's help manuals and po files with translations of captions, text strings and hints 
from English to other languages using GNU Gettext tools for Delphi, C++ and Lazarus.

The po files could be created, edited and used in the next way: 
- Download dxgettext-1.2.2.exe from http://sourceforge.net/projects/dxgettext/ 
  and install it on your computer. For Windows users there will be corresponding items 
  inserted to Explorer's menu. 
- Extract captions, text strings and hints to be translated in your project's directory 
  from pas/cpp/dfm files to a template default.po file. Right click by mouse button on appropriate folder opended in Explorer.
- Merge your project's templete default.po file with main translated samples.po file, 
  earlier extracted from GLScene\Samples directory. Right click on default.po file to execute the command (the same could be done 
  to merge a template with glscene.po updated by GLScene's developers).
- Download poEdit editor from http://www.poedit/net/ or http://sourceforge.net/projects/poedit/files/
  and make necessary translations of your additional text strings. 
- Save your po file from poEdit and thus get compiled mo file. Usually you will have one mo file for each language.
- Download gnugettext.pas unit from svn page:
  https://dxgettext.svn.sourceforge.net/svnroot/dxgettext/trunk/dxgettext-pachages/dxgettext/dxgettext/freepascal/
- Include gnugettext.pas into your project's uses clause, set paths to mo files in your program 
  and make language code selections (note: you can find the last version of GNUgettext.pas for Delphi in GLScene\Samples\Delphi\AdvDemos\GLSViewer directory)
- Choose a necessary language to build your application and execute your software with localised interface,
  that should also include GLScene's text strings and messages translations.

12.12.13
--------
GLS Team





