//
//  uGlobals for GLSViewer
//

unit uGlobals;

interface

uses
  System.SysUtils,
  System.IniFiles;


const
  //cRegistryKey = 'Software\GLSViewer';
  RegGLSViewer = PathDelim + 'SOFTWARE' + PathDelim + 'GLSViewer' + PathDelim;


var
  ExePath: TFileName;
  ModelPath:   TFileName;
  TexturePath: TFileName;
  IniFile: TIniFile;

  Language: integer;
  GeneralSection: string = RegGLSViewer + 'General';

  SplashStart : Boolean;
  TipOfTheDay : Boolean;


//==========================================================================
implementation
//==========================================================================

end.
