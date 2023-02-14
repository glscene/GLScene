//
// The graphics platform GLXcene https://github.com/glscene
//
unit CUDAx.Utility;

(* Wraper of cutil.*)

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
  cutFindFilePath: function(const filename: PAnsiChar; const executablePath:
    PAnsiChar): PAnsiChar;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutLoadPGMf: function(const filename: PAnsiChar; var data: System.PSingle; var w:
    Integer; var h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutSavePGMf: function(const filename: PAnsiChar; data: System.PSingle; w: Integer;
    h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutLoadPGMub: function(const filename: PAnsiChar; var data: PByte; var w:
    Integer; var h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutLoadPPMub: function(const filename: PAnsiChar; var data: PByte; var w:
    Integer; var h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutLoadPPM4ub: function(const filename: PAnsiChar; var data: PByte; var w:
    Integer; var h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutLoadPGMi: function(const filename: PAnsiChar; var data: PInteger; var w:
    Integer; var h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutLoadPGMs: function(const filename: PAnsiChar; var data: PWord; var w:
    Integer; var h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutSavePGMub: function(const filename: PAnsiChar; data: PByte; w: Integer;
    h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutSavePPMub: function(const filename: PAnsiChar; data: PByte; w: Integer;
    h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutSavePPM4ub: function(const filename: PAnsiChar; data: PByte; w: Integer;
    h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutSavePGMi: function(const filename: PAnsiChar; data: PInteger; w: Integer;
    h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutSavePGMs: function(const filename: PAnsiChar; data: PWord; w: Integer;
    h: Integer): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}


  cutComparef: function(const reference: PSingle; const data: PSingle;
                 const len: Cardinal): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutComparei: function(const reference: PInteger; const data: PInteger;
                 const len: Cardinal): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutCompareuit: function( const reference: PInteger; const data: PInteger;
                const len: Cardinal; const epsilon: Single; const threshold: Single ): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutCompareub: function(const reference: PByte; const data: PByte;
                 const len: Cardinal): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutCompareubt: function( const reference: PByte; const data: PByte;
                const len: Cardinal; const epsilon: Single; const threshold: Single ): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutCompareube: function( const reference: PByte; const data: PByte;
                const len: Cardinal; const epsilon: Single): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutComparefe: function( const reference: PSingle; const data: PSingle;
                const len: Cardinal; const epsilon: Single): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutComparefet: function( const reference: PSingle; const data: PSingle;
                const len: Cardinal; const epsilon: Single; const threshold: Single ): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutCompareL2fe: function( const reference: PSingle; const data: PSingle;
                const len: Cardinal; const epsilon: Single): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cutCreateTimer: function(var name: Cardinal): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutStartTimer: function
  (const name: Cardinal): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutStopTimer: function(const name: Cardinal): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutResetTimer: function(const name: Cardinal): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutDeleteTimer: function(const name: Cardinal): Boolean;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutGetTimerValue: function(const name: Cardinal): Single;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutGetAverageTimerValue: function(const name: Cardinal): Single;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cutFree: procedure(ptr: Pointer);
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

function InitCUTIL: Boolean;
procedure CloseCUTIL;
function InitCUTILFromLibrary(const LibName: WideString): Boolean;
function IsCUTILInitialized: Boolean;

//==============================================
implementation
//==============================================

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
    Result := InitCUTILFromLibrary(CUTILDLL)
  else
    Result := True;
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
  Result := False;
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
  Result := True;
end;

function IsCUTILInitialized: Boolean;
begin
  Result := (CUTILHandle <> INVALID_MODULEHANDLE);
end;

initialization

finalization

  CloseCUTIL;

end.

