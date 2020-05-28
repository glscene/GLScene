// FDXPOptions
{
   DXP utility functions.

   Licensed under MPL (http://www.mozilla.org/MPL/)

   Copyright 2003 - Eric Grange
}
unit DXPUtils;

interface

uses Forms, Classes, Windows;

function ExecuteAndWait(cmdLine : String; visibility : Word;
                        timeout : Cardinal = MaxInt;
                        killAppOnTimeOut : Boolean = True) : Integer;

function GetTemporaryFilesPath : String;
function GetTemporaryFileName : String;

function FindFileInPaths(const fileName, paths : String) : String;

function PathsToString(const paths : TStrings) : String;
procedure StringToPaths(const pathsString : String; paths : TStrings);
function MacroExpandPath(const aPath : String) : String;

// -----------------------------------------------------------------
// -----------------------------------------------------------------
// -----------------------------------------------------------------
implementation
// -----------------------------------------------------------------
// -----------------------------------------------------------------
// -----------------------------------------------------------------

uses Dialogs, SysUtils, DXPGlobals;

// ExecuteAndWait
//
function ExecuteAndWait(cmdLine : String; visibility : Word;
                        timeout : Cardinal = MaxInt;
                        killAppOnTimeOut : Boolean = True) : Integer;
var
   waitResult : Cardinal;
   startupInfo: TStartupInfo;
   processInfo: TProcessInformation;
   app : String;
   exitCode : Cardinal;
begin
   FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
   with StartupInfo do begin
      cb:=SizeOf(TStartupInfo);
      dwFlags:=(STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK);
      wShowWindow:=visibility;
   end;
   app:=Copy(cmdLine, 1, Pos(' ', cmdLine)-1);
   if CreateProcess(PChar(app), PChar(cmdLine), nil, nil, False, NORMAL_PRIORITY_CLASS, nil, nil,
              		  startupInfo, processInfo) then begin
      try
         repeat
            waitResult:=WaitForSingleObject(ProcessInfo.hProcess, 500);
            if waitResult<>WAIT_TIMEOUT then Break;
            Application.ProcessMessages;
            Dec(timeOut, 500);
         until timeOut<=0;
         if waitResult<>WAIT_OBJECT_0 then begin
            Result:=GetLastError;
            if killAppOnTimeOut then begin
               TerminateProcess(ProcessInfo.hProcess, 0);
               WaitForSingleObject(ProcessInfo.hProcess, 1000);
            end;
         end else begin
            GetExitCodeProcess(ProcessInfo.hProcess, exitCode);
            Result:=exitCode;
         end;
      finally
         CloseHandle(ProcessInfo.hProcess);
         CloseHandle(ProcessInfo.hThread);
      end;
   end else begin
      RaiseLastOSError;
      Result:=-1;
   end;
end;

// GetTemporaryFilesPath
//
function GetTemporaryFilesPath : String;
begin
   SetLength(Result, 512);
   Setlength(Result, GetTempPath(510, PChar(Result)));
end;

// GetTemporaryFileName
//
function GetTemporaryFileName : String;
begin
   SetLength(Result, 512);
   GetTempFileName(PChar(GetTemporaryFilesPath), 'DXP-', 0, PChar(Result));
   Result:=StrPas(PChar(Result));
end;

// FindFileInPaths
//
function FindFileInPaths(const fileName, paths : String) : String;
var
   i : Integer;
   sl : TStringList;
begin
   sl:=TStringList.Create;
   try
      sl.Delimiter:=';';
      sl.CommaText:=paths;
      for i:=0 to sl.Count-1 do begin
         if FileExists(sl[i]+'\'+fileName) then begin
            Result:=sl[i]+'\'+fileName;
            Exit;
         end;
      end;
   finally
      sl.Free;
   end;
   Result:='';
end;

// PathsToString
//
function PathsToString(const paths : TStrings) : String;
var
   i : Integer;
begin
   Result:='';
   for i:=0 to paths.Count-1 do if paths[i]<>'' then
      Result:=Result+paths[i]+';';
   if Result<>'' then
      SetLength(Result, Length(Result)-1);
end;

// StringToPaths
//
procedure StringToPaths(const pathsString : String; paths : TStrings);
var
   i, p, n : Integer;
begin
   paths.BeginUpdate;
   paths.Clear;
   p:=1;
   for i:=1 to Length(pathsString) do begin
      if pathsString[i]=';' then begin
         n:=i-p;
         if n>0 then
            paths.Add(Copy(pathsString, p, n));
         p:=i+1;
      end;
   end;
   n:=Length(pathsString)-p+1;
   if n>0 then
      paths.Add(Copy(pathsString, p, n));
   paths.EndUpdate;
end;

// MacroExpandPath
//
function MacroExpandPath(const aPath : String) : String;
begin
   Result:=aPath;
   Result:=StringReplace(aPath, '$FreePascal', vFPC_RootPath, [rfReplaceAll, rfIgnoreCase]);
   Result:=StringReplace(aPath, '$FPC', vFPC_RootPath, [rfReplaceAll, rfIgnoreCase]);
end;

end.
