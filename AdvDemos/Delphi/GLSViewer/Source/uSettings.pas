//
//  uSettings for GLSViewer
//

unit uSettings;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Win.Registry,
  System.IniFiles,
  System.SysUtils,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.ActnList,
  //
  GnuGettext;

procedure InitGeneralRegistry;

//-------------------------------------------------------------------------
implementation
// ------------------------------------------------------------------------

uses
  uGlobals;

procedure InitGeneralRegistry;
var
  RegIni:      TRegistryIniFile;
  FileVersion: cardinal;
begin
  GeneralSection := RegGLSViewer + 'General';
  FileVersion := GetFileVersion(ParamStr(0));
  ExePath := ExtractFilePath(ParamStr(0));
  RegIni := TRegistryIniFile.Create(GeneralSection);
  try
    with RegIni do
    begin
      if not RegIni.SectionExists(GeneralSection) then
      begin
        WriteString(GeneralSection, 'ExePath', ExePath);  //Don't translate the strings
        WriteInteger(GeneralSection, 'FileVersion', FileVersion);
        WriteString(GeneralSection, 'License', 'MPL');
        WriteInteger(GeneralSection, 'Language', LANG_ENGLISH); //9, Default installation
        Language := LANG_ENGLISH;
      end
      else
      begin
        ExePath  := ReadString(GeneralSection, 'ExePath', ExePath);
        Language := ReadInteger(GeneralSection, 'Language', LANG_ENGLISH);
        //9, LANG_ENGLISH - Default
      end;
    end;
  finally
    RegIni.Free;
  end;
{
  if RegIni.ValueExists(GeneralSection,'SplashStart') then
    SplashStart := RegIni.ReadBool(GeneralSection,'SplashStart',False)
  else
    SplashStart := True;

  if RegIni.ValueExists(GeneralSection,'TipOfTheDay') then
    TipOfTheDay := RegIni.ReadBool(GeneralSection,'TipOfTheDay', True)
  else
    TipOfTheDay := False;
}
end;

initialization
  InitGeneralRegistry;
end.
