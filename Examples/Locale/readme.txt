The directory for po files with translations 
of captions, text strings and hints from English to other languages 
using GNU Gettext tools for Delphi, C++ and Lazarus.

----------------------------------------------------------------------------------------

The po files could be created, edited and used in the next way: 
- Download dxgettext-1.2.2.exe from http://sourceforge.net/projects/dxgettext/ 
  and install it on your computer. For Windows users there will be corresponding items 
  inserted to Explorer's menu. 
- Extract captions, text strings and hints from pas/cpp/dfm files in your project's directory  
  to insert it to a template default.po file. Right click by mouse button on appropriate folder opened in PoEditor.
- Merge your project's templete default.po file with main translated samples.po file 
  if it was earlier extracted from GLScene\Samples directory. Right click on default.po file to execute the command 
  (the same could be done to merge a template with glscene.po updated by GLScene's developers).
- Download poEdit editor from https://poedit.net/ and make necessary translations in your additional text strings. 
- Save your po file from poEdit and then gain compiled mo file. Usually you will have one po/mo files for each language.
- Include gnugettext.pas into your project's uses clause, set paths to mo files in your program 
  and make language code selections (note: the last version of gnugettext.pas unit included in GLScene also)
- Choose the necessary language to build your application and execute your software with localized interface,
  that includes all source's text strings, hints and translated messages.

---------------------------------------------------------------------------

GLSTeam





