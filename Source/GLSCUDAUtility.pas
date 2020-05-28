//
// This unit is part of the GLScene Engine, http://glscene.org
//
{
    GLScene CUDA Utility
    Wraper of cutil.
}
unit GLSCUDAUtility;

interface

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}


{$I cuda.inc}

const
{$IFDEF WIN64}
  CUTILDLL = 'cutil64.dll';
{$ELSE}
  CUTILDLL = 'cutil32.dll';
{$ENDIF}

var
  cutFindFilePath: function(const filename: PAnsiChar; const executablePath: PAnsiChar): PAnsiChar;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutLoadPGMf: function(const filename: PAnsiChar; var data: System.PSingle; var w: Integer; var h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutSavePGMf: function(const filename: PAnsiChar; data: System.PSingle; w: Integer; h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutLoadPGMub: function(const filename: PAnsiChar; var data: PByte; var w: Integer; var h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutLoadPPMub: function(const filename: PAnsiChar; var data: PByte; var w: Integer; var h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutLoadPPM4ub: function(const filename: PAnsiChar; var data: PByte; var w: Integer; var h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutLoadPGMi: function(const filename: PAnsiChar; var data: PInteger; var w: Integer; var h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutLoadPGMs: function(const filename: PAnsiChar; var data: PWord; var w: Integer; var h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutSavePGMub: function(const filename: PAnsiChar; data: PByte; w: Integer; h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutSavePPMub: function(const filename: PAnsiChar; data: PByte; w: Integer; h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutSavePPM4ub: function(const filename: PAnsiChar; data: PByte; w: Integer; h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutSavePGMi: function(const filename: PAnsiChar; data: PInteger; w: Integer; h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutSavePGMs: function(const filename: PAnsiChar; data: PWord; w: Integer; h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutComparef: function(const reference: PSingle; const data: PSingle; const len: Cardinal): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutComparei: function(const reference: PInteger; const data: PInteger; const len: Cardinal): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutCompareuit: function(const reference: PInteger; const data: PInteger; const len: Cardinal; const epsilon: Single;
    const threshold: Single): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutCompareub: function(const reference: PByte; const data: PByte; const len: Cardinal): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutCompareubt: function(const reference: PByte; const data: PByte; const len: Cardinal; const epsilon: Single;
    const threshold: Single): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutCompareube: function(const reference: PByte; const data: PByte; const len: Cardinal; const epsilon: Single): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutComparefe: function(const reference: PSingle; const data: PSingle; const len: Cardinal; const epsilon: Single): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutComparefet: function(const reference: PSingle; const data: PSingle; const len: Cardinal; const epsilon: Single;
    const threshold: Single): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutCompareL2fe: function(const reference: PSingle; const data: PSingle; const len: Cardinal; const epsilon: Single): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutCreateTimer: function(var name: Cardinal): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutStartTimer: function(const name: Cardinal): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutStopTimer: function(const name: Cardinal): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutResetTimer: function(const name: Cardinal): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutDeleteTimer: function(const name: Cardinal): Boolean;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutGetTimerValue: function(const name: Cardinal): Single;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutGetAverageTimerValue: function(const name: Cardinal): Single;
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
  cutFree: procedure(ptr: Pointer);
{$IFDEF CUDA_STDCALL}stdcall; {$ELSE}cdecl; {$ENDIF}
function InitCUTIL: Boolean;
procedure CloseCUTIL;
function InitCUTILFromLibrary(const LibName: WideString): Boolean;
function IsCUTILInitialized: Boolean;

// ------------------------------------------------------
implementation
// ------------------------------------------------------

const
  INVALID_MODULEHANDLE = 0;

  // ************** Windows specific ********************
{$IFDEF MSWINDOWS}

var
  CUTILHandle: HINST = INVALID_MODULEHANDLE;
{$ENDIF}
  // ************** UNIX specific ********************
{$IFDEF UNIX}

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
