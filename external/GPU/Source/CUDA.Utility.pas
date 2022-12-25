//
// The graphics rendering engine GLScene http://glscene.org
//
unit CUDA.Utility;

(* CUDA Utility Wraper of cutil *)

interface

uses
  Winapi.Windows;

const
{$IFDEF WIN64}
  CUTILDLL = 'cutil64.dll';
{$ELSE}
  CUTILDLL = 'cutil32.dll';
{$ENDIF}

var
  cutFindFilePath: function(const filename: PAnsiChar; const executablePath: PAnsiChar): PAnsiChar;stdcall;
  cutLoadPGMf: function(const filename: PAnsiChar; var data: System.PSingle; var w: Integer; var h: Integer): Boolean;stdcall;
  cutSavePGMf: function(const filename: PAnsiChar; data: System.PSingle; w: Integer; h: Integer): Boolean;stdcall;
  cutLoadPGMub: function(const filename: PAnsiChar; var data: PByte; var w: Integer; var h: Integer): Boolean;stdcall;
  cutLoadPPMub: function(const filename: PAnsiChar; var data: PByte; var w: Integer; var h: Integer): Boolean;stdcall;
  cutLoadPPM4ub: function(const filename: PAnsiChar; var data: PByte; var w: Integer; var h: Integer): Boolean;stdcall;
  cutLoadPGMi: function(const filename: PAnsiChar; var data: PInteger; var w: Integer; var h: Integer): Boolean;stdcall;
  cutLoadPGMs: function(const filename: PAnsiChar; var data: PWord; var w: Integer; var h: Integer): Boolean;stdcall;
  cutSavePGMub: function(const filename: PAnsiChar; data: PByte; w: Integer; h: Integer): Boolean;stdcall;
  cutSavePPMub: function(const filename: PAnsiChar; data: PByte; w: Integer; h: Integer): Boolean;stdcall;
  cutSavePPM4ub: function(const filename: PAnsiChar; data: PByte; w: Integer; h: Integer): Boolean;stdcall;
  cutSavePGMi: function(const filename: PAnsiChar; data: PInteger; w: Integer; h: Integer): Boolean;stdcall;
  cutSavePGMs: function(const filename: PAnsiChar; data: PWord; w: Integer; h: Integer): Boolean;stdcall;
  cutComparef: function(const reference: PSingle; const data: PSingle; const len: Cardinal): Boolean;stdcall;
  cutComparei: function(const reference: PInteger; const data: PInteger; const len: Cardinal): Boolean;stdcall;
  cutCompareuit: function(const reference: PInteger; const data: PInteger; const len: Cardinal; const epsilon: Single;
    const threshold: Single): Boolean;stdcall;
  cutCompareub: function(const reference: PByte; const data: PByte; const len: Cardinal): Boolean;stdcall;
  cutCompareubt: function(const reference: PByte; const data: PByte; const len: Cardinal; const epsilon: Single;
    const threshold: Single): Boolean;stdcall;
  cutCompareube: function(const reference: PByte; const data: PByte; const len: Cardinal; const epsilon: Single): Boolean;stdcall;
  cutComparefe: function(const reference: PSingle; const data: PSingle; const len: Cardinal; const epsilon: Single): Boolean;stdcall;
  cutComparefet: function(const reference: PSingle; const data: PSingle; const len: Cardinal; const epsilon: Single;
    const threshold: Single): Boolean;stdcall;
  cutCompareL2fe: function(const reference: PSingle; const data: PSingle; const len: Cardinal; const epsilon: Single): Boolean;stdcall;
  cutCreateTimer: function(var name: Cardinal): Boolean;stdcall;
  cutStartTimer: function(const name: Cardinal): Boolean;stdcall;
  cutStopTimer: function(const name: Cardinal): Boolean;stdcall;
  cutResetTimer: function(const name: Cardinal): Boolean;stdcall;
  cutDeleteTimer: function(const name: Cardinal): Boolean;stdcall;
  cutGetTimerValue: function(const name: Cardinal): Single;stdcall;
  cutGetAverageTimerValue: function(const name: Cardinal): Single;stdcall;
  cutFree: procedure(ptr: Pointer);stdcall;

function InitCUTIL: Boolean;
procedure CloseCUTIL;
function InitCUTILFromLibrary(const LibName: WideString): Boolean;
function IsCUTILInitialized: Boolean;

// ------------------------------------------------------
implementation
// ------------------------------------------------------

const
  INVALID_MODULEHANDLE = 0;

{$IFDEF MSWINDOWS}
// ************** Windows specific ********************
var
  CUTILHandle: HINST = INVALID_MODULEHANDLE;
{$ELSE}
// ************** UNIX specific ********************
var
  CUTILHandle: TLibHandle = INVALID_MODULEHANDLE;
{$ENDIF}

function CUTILGetProcAddress(ProcName: PAnsiChar): Pointer;
begin
  result := GetProcAddress(Cardinal(CUTILHandle), ProcName);
end;

function InitCUTIL: Boolean;
begin
  if CUTILHandle = INVALID_MODULEHANDLE then
    result := InitCUTILFromLibrary(CUTILDLL)
  else
    result := True;
end;

procedure CloseCUTIL;
begin
  if CUTILHandle <> INVALID_MODULEHANDLE then
  begin
    FreeLibrary(Cardinal(CUTILHandle));
    CUTILHandle := INVALID_MODULEHANDLE;
  end;
end;

function InitCUTILFromLibrary(const LibName: WideString): Boolean;
begin
  result := False;
  CloseCUTIL;
  CUTILHandle := LoadLibraryW(PWideChar(LibName));
  if CUTILHandle = INVALID_MODULEHANDLE then
    Exit;
  cutFindFilePath := CUTILGetProcAddress('cutFindFilePath');
  cutLoadPGMf := CUTILGetProcAddress('cutLoadPGMf');
  cutSavePGMf := CUTILGetProcAddress('cutSavePGMf');
  cutLoadPGMub := CUTILGetProcAddress('cutLoadPGMub');
  cutLoadPPMub := CUTILGetProcAddress('cutLoadPPMub');
  cutLoadPPM4ub := CUTILGetProcAddress('cutLoadPPM4ub');
  cutLoadPGMi := CUTILGetProcAddress('cutLoadPGMi');
  cutLoadPGMs := CUTILGetProcAddress('cutLoadPGMs');
  cutSavePGMub := CUTILGetProcAddress('cutSavePGMub');
  cutSavePPMub := CUTILGetProcAddress('cutSavePPMub');
  cutSavePPM4ub := CUTILGetProcAddress('cutSavePPM4ub');
  cutSavePGMi := CUTILGetProcAddress('cutSavePGMi');
  cutSavePGMs := CUTILGetProcAddress('cutSavePGMs');
  cutComparef := CUTILGetProcAddress('cutComparef');
  cutComparei := CUTILGetProcAddress('cutComparei');
  cutCompareuit := CUTILGetProcAddress('cutCompareuit');
  cutCompareub := CUTILGetProcAddress('cutCompareub');
  cutCompareubt := CUTILGetProcAddress('cutCompareubt');
  cutCompareube := CUTILGetProcAddress('cutCompareube');
  cutComparefe := CUTILGetProcAddress('cutComparefe');
  cutComparefet := CUTILGetProcAddress('cutComparefet');
  cutCompareL2fe := CUTILGetProcAddress('cutCompareL2fe');
  cutCreateTimer := CUTILGetProcAddress('cutCreateTimer');
  cutStartTimer := CUTILGetProcAddress('cutStartTimer');
  cutStopTimer := CUTILGetProcAddress('cutStopTimer');
  cutResetTimer := CUTILGetProcAddress('cutResetTimer');
  cutDeleteTimer := CUTILGetProcAddress('cutDeleteTimer');
  cutGetTimerValue := CUTILGetProcAddress('cutGetTimerValue');
  cutGetAverageTimerValue := CUTILGetProcAddress('cutGetAverageTimerValue');
  cutFree := CUTILGetProcAddress('cutFree');
  result := True;
end;

function IsCUTILInitialized: Boolean;
begin
  result := (CUTILHandle <> INVALID_MODULEHANDLE);
end;

//-----------------------------------------------
initialization
//-----------------------------------------------

finalization

  CloseCUTIL;

end.
