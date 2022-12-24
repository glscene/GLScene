//
// The graphics rendering engine GLScene http://glscene.org
//
unit CUDA.FourierTransform;

(* CUDA Fourier Transform *)

/// *
// * Copyright 1993-2009 NVIDIA Corporation.  All rights reserved.
// *
// * NOTICE TO USER:
// *
// * This source code is subject to NVIDIA ownership rights under U.S. and
// * international Copyright laws.  Users and possessors of this source code
// * are hereby granted a nonexclusive, royalty-free license to use this code
// * in individual and commercial software.
// *
// * NVIDIA MAKES NO REPRESENTATION ABOUT THE SUITABILITY OF THIS SOURCE
// * CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR
// * IMPLIED WARRANTY OF ANY KIND.  NVIDIA DISCLAIMS ALL WARRANTIES WITH
// * REGARD TO THIS SOURCE CODE, INCLUDING ALL IMPLIED WARRANTIES OF
// * MERCHANTABILITY, NONINFRINGEMENT, AND FITNESS FOR A PARTICULAR PURPOSE.
// * IN NO EVENT SHALL NVIDIA BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL,
// * OR CONSEQUENTIAL DAMAGES, OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
// * OF USE, DATA OR PROFITS,  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
// * OR OTHER TORTIOUS ACTION,  ARISING OUT OF OR IN CONNECTION WITH THE USE
// * OR PERFORMANCE OF THIS SOURCE CODE.
// *
// * U.S. Government End Users.   This source code is a "commercial item" as
// * that term is defined at  48 C.F.R. 2.101 (OCT 1995), consisting  of
// * "commercial computer  software"  and "commercial computer software
// * documentation" as such terms are  used in 48 C.F.R. 12.212 (SEPT 1995)
// * and is provided to the U.S. Government only as a commercial end item.
// * Consistent with 48 C.F.R.12.212 and 48 C.F.R. 227.7202-1 through
// * 227.7202-4 (JUNE 1995), all U.S. Government End Users acquire the
// * source code with only those rights set forth herein.
// *
// * Any use of this source code in individual and commercial software must
// * include, in the user documentation and internal comments to the code,
// * the above Disclaimer and U.S. Government End Users Notice.
// */

interface

uses
  Winapi.Windows,

  GLS.VectorTypes,
  GLS.Strings,

  CUDA.Import,
  CUDA.RunTime;


const
{$IFDEF WIN32}
  CUFFTDLLNAMES: array [0 .. 9] of string = (
    'cufft32_42_9', 'cufft32_41_28',
    'cufft32_40_10', 'cufft32_32_16', 'cufft32_31_4', 'cufft32_30_14',
    'cufft32_30_9', 'cufft32_30_8', 'cufft32', 'cufft');
{$ENDIF}

{$IFDEF WIN64}
  CUFFTDLLNAMES: array [0 .. 7] of string = (
    'cufft64_42_9', 'cufft64_41_28',
    'cufft64_40_10', 'cufft64_32_16', 'cufft64_31_4', 'cufft64_30_14',
    'cufft64_30_9', 'cufft64_30_8');
{$ENDIF}
  /// CUFFT API function return values

type
  /// CUFFT defines and supports the following data types

  /// cufftHandle is a handle type used to store and access CUFFT plans.
  TcufftHandle = type Cardinal;

  TcufftReal = Single;
  PcufftReal = ^TcufftReal;

  TcufftRealfloat = Single;
  PcufftDoubleReal = ^TcufftDoubleReal;
  TcufftDoubleReal = Double;

  PcufftDoubleComplex = ^TcufftDoubleComplex;
  TcufftDoubleComplex = TVector2d;

  PcufftComplex = ^TcufftComplex;
  TcufftComplex = TVector2f;

  TcufftResult = type Byte;

const
  INVALID_CUFFT_HANDLE = $FFFFFFFF;

  CUFFT_SUCCESS: TcufftResult = $00;
  CUFFT_INVALID_PLAN: TcufftResult = $01;
  CUFFT_ALLOC_FAILED: TcufftResult = $02;
  CUFFT_INVALID_TYPE: TcufftResult = $03;
  CUFFT_INVALID_VALUE: TcufftResult = $04;
  CUFFT_INTERNAL_ERROR: TcufftResult = $05;
  CUFFT_EXEC_FAILED: TcufftResult = $06;
  CUFFT_SETUP_FAILED: TcufftResult = $07;
  CUFFT_INVALID_SIZE: TcufftResult = $08;

type
  TcufftType = type Cardinal;

  TcudaRoundMode = (cudaRoundNearest, cudaRoundZero, cudaRoundPosInf,
    cudaRoundMinInf);

  /// CUFFT transform directions
const
  CUFFT_FORWARD = -1; // Forward FFT
  CUFFT_INVERSE = 1;  // Inverse FFT

  /// CUFFT supports the following transform types
  CUFFT_R2C: TcufftType = $2A; // Real to Complex (interleaved)
  CUFFT_C2R: TcufftType = $2C; // Complex (interleaved) to Real
  CUFFT_C2C: TcufftType = $29; // Complex to Complex, interleaved
  CUFFT_D2Z: TcufftType = $6A; // Double to Double-Complex
  CUFFT_Z2D: TcufftType = $6C; // Double-Complex to Double
  CUFFT_Z2Z: TcufftType = $69; // Double-Complex to Double-Complex

  (*
    Certain R2C and C2R transforms go much more slowly when FFTW memory
    layout and behaviour is required. The default is "best performance",
    which means not-compatible-with-fftw. Use the cufftSetCompatibilityMode
    API to enable exact FFTW-like behaviour.

    These flags can be ORed together to select precise FFTW compatibility
    behaviour. The two levels presently supported are:

    CUFFT_COMPATIBILITY_FFTW_PADDING
    Inserts extra padding between packed in-place transforms for
    batched transforms with power-of-2 size.

    CUFFT_COMPATIBILITY_FFTW_C2R_ASYMMETRIC
    Guarantees FFTW-compatible output for non-symmetric complex inputs
    for transforms with power-of-2 size. This is only useful for
    artificial (i.e. random) datasets as actual data will always be
    symmetric if it has come from the real plane. If you don't
    understand what this means, you probably don't have to use it.

    CUFFT_COMPATIBILITY_FFTW
    For convenience, enables all FFTW compatibility modes at once.
  *)

type

  TcufftCompatibility = type Cardinal;

const
  CUFFT_COMPATIBILITY_NORMAL: TcufftCompatibility = $00; // The default value
  CUFFT_COMPATIBILITY_FFTW_PADDING: TcufftCompatibility = $01;
  CUFFT_COMPATIBILITY_FFTW_C2R_ASYMMETRIC: TcufftCompatibility = $02;
  CUFFT_COMPATIBILITY_FFTW: TcufftCompatibility = $03;

type

  TcufftPlan1d = function(out plan: TcufftHandle; nx: Integer;
    atype: TcufftType; batch: Integer): TcufftResult;stdcall;
  TcufftPlan2d = function(out plan: TcufftHandle; nx: Integer; ny: Integer;
    atype: TcufftType): TcufftResult;stdcall;
  TcufftPlan3d = function(out plan: TcufftHandle; nx: Integer; ny: Integer;
    nz: Integer; atype: TcufftType): TcufftResult;stdcall;
  TcufftDestroy = function(plan: TcufftHandle): TcufftResult;stdcall;
  TcufftPlanMany = function(out plan: TcufftHandle; rank: Integer;
    var n: Integer; var inembed: Integer; istride, idist: Integer;
    var onembed: Integer; ostride, odist: Integer; ctype: TcufftType;
    batch: Integer): TcufftResult;stdcall;
  TcufftExecC2C = function(plan: TcufftHandle; idata: PcufftComplex;
    odata: PcufftComplex; direction: Integer): TcufftResult;stdcall;
  TcufftExecR2C = function(plan: TcufftHandle; idata: PcufftReal;
    odata: PcufftComplex): TcufftResult;stdcall;
  TcufftExecC2R = function(plan: TcufftHandle; idata: PcufftComplex;
    odata: PcufftReal): TcufftResult;stdcall;
  TcufftExecZ2Z = function(plan: TcufftHandle; idata: PcufftDoubleComplex;
    odata: PcufftDoubleComplex; direction: Integer): TcufftResult;stdcall;
  TcufftExecD2Z = function(plan: TcufftHandle; idata: PcufftDoubleReal;
    odata: PcufftDoubleComplex): TcufftResult;stdcall;
  TcufftExecZ2D = function(plan: TcufftHandle; idata: PcufftDoubleComplex;
    odata: PcufftDoubleReal): TcufftResult;stdcall;
  TcufftSetStream = function(p: TcufftHandle; stream: Integer): TcufftResult;stdcall;
  TcufftSetCompatibilityMode = function(plan: TcufftHandle;
    mode: TcufftCompatibility): TcufftResult;stdcall;

var
  cufftPlan1d: TcufftPlan1d;
  cufftPlan2d: TcufftPlan2d;
  cufftPlan3d: TcufftPlan3d;
  cufftDestroy: TcufftDestroy;
  cufftPlanMany: TcufftPlanMany;
  cufftExecC2C: TcufftExecC2C;
  cufftExecR2C: TcufftExecR2C;
  cufftExecC2R: TcufftExecC2R;
  cufftExecZ2Z: TcufftExecZ2Z;
  cufftExecD2Z: TcufftExecD2Z;
  cufftExecZ2D: TcufftExecZ2D;
  cufftSetStream: TcufftSetStream;
  cufftSetCompatibilityMode: TcufftSetCompatibilityMode;

function InitCUFFT: Boolean;
procedure CloseCUFFT;
function InitCUFFTFromLibrary(const LibName: WideString): Boolean;
function IsCUFFTInitialized: Boolean;
function Get_CUDA_FFT_Error_String(AError: TcufftResult): string;

//---------------------------------------------------------
implementation
//---------------------------------------------------------

const
  cufftPlan1dName = 'cufftPlan1d';
  cufftPlan2dName = 'cufftPlan2d';
  cufftPlan3dName = 'cufftPlan3d';
  cufftDestroyName = 'cufftDestroy';
  cufftPlanManyName = 'cufftPlanMany';
  cufftExecC2CName = 'cufftExecC2C';
  cufftExecR2CName = 'cufftExecR2C';
  cufftExecC2RName = 'cufftExecC2R';
  cufftExecZ2ZName = 'cufftExecZ2Z';
  cufftExecD2ZName = 'cufftExecD2Z';
  cufftExecZ2DName = 'cufftExecZ2D';
  cufftSetStreamName = 'cufftSetStream';
  cufftSetCompatibilityModeName = 'cufftSetCompatibilityMode';

const
  INVALID_MODULEHANDLE = 0;

var
{$IFDEF MSWINDOWS}
  CUFFTHandle: HINST = INVALID_MODULEHANDLE;
{$ENDIF}{$IFDEF LINUX}
  CUFFTHandle: TLibHandle = INVALID_MODULEHANDLE;
{$ENDIF}

{$IFDEF USE_CUDA_DEBUG_MODE}
var
  cufftPlan1d_: TcufftPlan1d;
  cufftPlan2d_: TcufftPlan2d;
  cufftPlan3d_: TcufftPlan3d;
  cufftDestroy_: TcufftDestroy;
  cufftPlanMany_: TcufftPlanMany;
  cufftExecC2C_: TcufftExecC2C;
  cufftExecR2C_: TcufftExecR2C;
  cufftExecC2R_: TcufftExecC2R;
  cufftExecZ2Z_: TcufftExecZ2Z;
  cufftExecD2Z_: TcufftExecD2Z;
  cufftExecZ2D_: TcufftExecZ2D;
  cufftSetStream_: TcufftSetStream;
  cufftSetCompatibilityMode_: TcufftSetCompatibilityMode;

function cufftPlan1dShell(out plan: TcufftHandle; nx: Integer;
  atype: TcufftType; batch: Integer): TcufftResult;stdcall;
begin
  Result := cufftPlan1d_(plan, nx, atype, batch);
  if Result <> CUFFT_SUCCESS then
    GLSLogger.LogErrorFmt(strFFTFuncRetErr, [cufftPlan1dName,
      Get_CUDA_FFT_Error_String(Result)]);
end;

function cufftPlan2dShell(out plan: TcufftHandle; nx: Integer; ny: Integer;
  atype: TcufftType): TcufftResult;stdcall;
begin
  Result := cufftPlan2d_(plan, nx, ny, atype);
  if Result <> CUFFT_SUCCESS then
    GLSLogger.LogErrorFmt(strFFTFuncRetErr, [cufftPlan2dName,
      Get_CUDA_FFT_Error_String(Result)]);
end;

function cufftPlan3dShell(out plan: TcufftHandle; nx: Integer; ny: Integer;
  nz: Integer; atype: TcufftType): TcufftResult;stdcall;
begin
  Result := cufftPlan3d_(plan, nx, ny, nz, atype);
  if Result <> CUFFT_SUCCESS then
    GLSLogger.LogErrorFmt(strFFTFuncRetErr, [cufftPlan3dName,
      Get_CUDA_FFT_Error_String(Result)]);
end;

function cufftDestroyShell(plan: TcufftHandle): TcufftResult;stdcall;
begin
  Result := cufftDestroy_(plan);
  if Result <> CUFFT_SUCCESS then
    GLSLogger.LogErrorFmt(strFFTFuncRetErr, [cufftDestroyName,
      Get_CUDA_FFT_Error_String(Result)]);
end;

function cufftPlanManyShell(out plan: TcufftHandle; rank: Integer;
  var n: Integer; var inembed: Integer; istride, idist: Integer;
  var onembed: Integer; ostride, odist: Integer; ctype: TcufftType;
  batch: Integer): TcufftResult;stdcall;
begin
  Result := cufftPlanMany_(plan, rank, n, inembed, istride, idist, onembed,
    ostride, odist, ctype, batch);
  if Result <> CUFFT_SUCCESS then
    GLSLogger.LogErrorFmt(strFFTFuncRetErr, [cufftPlanManyName,
      Get_CUDA_FFT_Error_String(Result)]);
end;

function cufftExecC2CShell(plan: TcufftHandle; idata: PcufftComplex;
  odata: PcufftComplex; direction: Integer): TcufftResult;stdcall;
begin
  Result := cufftExecC2C_(plan, idata, odata, direction);
  if Result <> CUFFT_SUCCESS then
    GLSLogger.LogErrorFmt(strFFTFuncRetErr, [cufftExecC2CName,
      Get_CUDA_FFT_Error_String(Result)]);
end;

function cufftExecR2CShell(plan: TcufftHandle; idata: PcufftReal;
  odata: PcufftComplex): TcufftResult;stdcall;
begin
  Result := cufftExecR2C_(plan, idata, odata);
  if Result <> CUFFT_SUCCESS then
    GLSLogger.LogErrorFmt(strFFTFuncRetErr, [cufftExecR2CName,
      Get_CUDA_FFT_Error_String(Result)]);
end;

function cufftExecC2RShell(plan: TcufftHandle; idata: PcufftComplex;
  odata: PcufftReal): TcufftResult;stdcall;
begin
  Result := cufftExecC2R_(plan, idata, odata);
  if Result <> CUFFT_SUCCESS then
    GLSLogger.LogErrorFmt(strFFTFuncRetErr, [cufftExecC2RName,
      Get_CUDA_FFT_Error_String(Result)]);
end;

function cufftExecZ2ZShell(plan: TcufftHandle; idata: PcufftDoubleComplex;
  odata: PcufftDoubleComplex; direction: Integer): TcufftResult;stdcall;
begin
  Result := cufftExecZ2Z_(plan, idata, odata, direction);
  if Result <> CUFFT_SUCCESS then
    GLSLogger.LogErrorFmt(strFFTFuncRetErr, [cufftExecZ2ZName,
      Get_CUDA_FFT_Error_String(Result)]);
end;

function cufftExecD2ZShell(plan: TcufftHandle; idata: PcufftDoubleReal;
  odata: PcufftDoubleComplex): TcufftResult;stdcall;
begin
  Result := cufftExecD2Z_(plan, idata, odata);
  if Result <> CUFFT_SUCCESS then
    GLSLogger.LogErrorFmt(strFFTFuncRetErr, [cufftExecD2ZName,
      Get_CUDA_FFT_Error_String(Result)]);
end;

function cufftExecZ2DShell(plan: TcufftHandle; idata: PcufftDoubleComplex;
  odata: PcufftDoubleReal): TcufftResult;stdcall;
begin
  Result := cufftExecZ2D_(plan, idata, odata);
  if Result <> CUFFT_SUCCESS then
    GLSLogger.LogErrorFmt(strFFTFuncRetErr, [cufftExecZ2DName,
      Get_CUDA_FFT_Error_String(Result)]);
end;

function cufftSetStreamShell(p: TcufftHandle; stream: Integer): TcufftResult;stdcall;
begin
  Result := cufftSetStream_(p, stream);
  if Result <> CUFFT_SUCCESS then
    GLSLogger.LogErrorFmt(strFFTFuncRetErr, [cufftSetStreamName,
      Get_CUDA_FFT_Error_String(Result)]);
end;

function cufftSetCompatibilityModeShell(plan: TcufftHandle;
  mode: TcufftCompatibility): TcufftResult;stdcall;
begin
  Result := cufftSetCompatibilityMode_(plan, mode);
  if Result <> CUFFT_SUCCESS then
    GLSLogger.LogErrorFmt(strFFTFuncRetErr, [cufftSetCompatibilityModeName,
      Get_CUDA_FFT_Error_String(Result)]);
end;
{$ENDIF GLS_CUDA_DEBUG_MODE}

function CUFFTGetProcAddress(ProcName: PAnsiChar): Pointer;
begin
  result := GetProcAddress(CUFFTHandle, ProcName);
end;

function InitCUFFT: Boolean;
var
  I: Integer;
begin
  Result := True;
  if CUFFTHandle = INVALID_MODULEHANDLE then
  begin
    for I := 0 to High(CUFFTDLLNAMES) do
    begin
      if InitCUFFTFromLibrary(CUFFTDLLNAMES[I] + '.dll') then
        Exit;
    end;
    Result := False;
  end;
end;

procedure CloseCUFFT;
begin
  if CUFFTHandle <> INVALID_MODULEHANDLE then
  begin
    FreeLibrary(Cardinal(CUFFTHandle));
    CUFFTHandle := INVALID_MODULEHANDLE;
  end;
end;

function InitCUFFTFromLibrary(const LibName: WideString): Boolean;
begin
  CloseCUFFT;
  CUFFTHandle := GetModuleHandleW(PWideChar(LibName));
  if CUFFTHandle = INVALID_MODULEHANDLE then
    CUFFTHandle := LoadLibraryW(PWideChar(LibName));
  if CUFFTHandle = INVALID_MODULEHANDLE then
    Exit(False);
{$IFNDEF USE_CUDA_DEBUG_MODE}
  cufftPlan1d := CUFFTGetProcAddress(cufftPlan1dName);
  cufftPlan2d := CUFFTGetProcAddress(cufftPlan2dName);
  cufftPlan3d := CUFFTGetProcAddress(cufftPlan3dName);
  cufftDestroy := CUFFTGetProcAddress(cufftDestroyName);
  cufftPlanMany := CUFFTGetProcAddress(cufftPlanManyName);
  cufftExecC2C := CUFFTGetProcAddress(cufftExecC2CName);
  cufftExecR2C := CUFFTGetProcAddress(cufftExecR2CName);
  cufftExecC2R := CUFFTGetProcAddress(cufftExecC2RName);
  cufftExecZ2Z := CUFFTGetProcAddress(cufftExecZ2ZName);
  cufftExecD2Z := CUFFTGetProcAddress(cufftExecD2ZName);
  cufftExecZ2D := CUFFTGetProcAddress(cufftExecZ2DName);
  cufftSetStream := CUFFTGetProcAddress(cufftSetStreamName);
  cufftSetCompatibilityMode := CUFFTGetProcAddress(cufftSetCompatibilityModeName);
{$ELSE}
  cufftPlan1d_ := CUFFTGetProcAddress(cufftPlan1dName);
  cufftPlan1d := cufftPlan1dShell;
  cufftPlan2d_ := CUFFTGetProcAddress(cufftPlan2dName);
  cufftPlan2d := cufftPlan2dShell;
  cufftPlan3d_ := CUFFTGetProcAddress(cufftPlan3dName);
  cufftPlan3d := cufftPlan3dShell;
  cufftDestroy_ := CUFFTGetProcAddress(cufftDestroyName);
  cufftDestroy := cufftDestroyShell;
  cufftPlanMany_ := CUFFTGetProcAddress(cufftPlanManyName);
  cufftPlanMany := cufftPlanManyShell;
  cufftExecC2C_ := CUFFTGetProcAddress(cufftExecC2CName);
  cufftExecC2C := cufftExecC2CShell;
  cufftExecR2C_ := CUFFTGetProcAddress(cufftExecR2CName);
  cufftExecR2C := cufftExecR2CShell;
  cufftExecC2R_ := CUFFTGetProcAddress(cufftExecC2RName);
  cufftExecC2R := cufftExecC2RShell;
  cufftExecZ2Z_ := CUFFTGetProcAddress(cufftExecZ2ZName);
  cufftExecZ2Z := cufftExecZ2ZShell;
  cufftExecD2Z_ := CUFFTGetProcAddress(cufftExecD2ZName);
  cufftExecD2Z := cufftExecD2ZShell;
  cufftExecZ2D_ := CUFFTGetProcAddress(cufftExecZ2DName);
  cufftExecZ2D := cufftExecZ2DShell;
  cufftSetStream_ := CUFFTGetProcAddress(cufftSetStreamName);
  cufftSetStream := cufftSetStreamShell;
  cufftSetCompatibilityMode_ := CUFFTGetProcAddress(cufftSetCompatibilityModeName);
  cufftSetCompatibilityMode := cufftSetCompatibilityModeShell;
{$ENDIF}

 {$IFDEF USE_LOGGING}
  LogInfoFmt('%s loaded...', [LibName]);
 {$ENDIF}
  Result := True;
end;

function IsCUFFTInitialized: Boolean;
begin
  result := (CUFFTHandle <> INVALID_MODULEHANDLE);
end;

function Get_CUDA_FFT_Error_String(AError: TcufftResult): string;
begin
  if AError = CUFFT_SUCCESS then
    result := 'CUFFT operation is successful.'
  else if AError = CUFFT_INVALID_PLAN then
    result := 'CUFFT is passed an invalid plan handle.'
  else if AError = CUFFT_ALLOC_FAILED then
    result := 'CUFFT failed to allocate GPU memory.'
  else if AError = CUFFT_INVALID_TYPE then
    result := 'The user requests an unsupported type.'
  else if AError = CUFFT_INVALID_VALUE then
    result := 'The user specifies a bad memory pointer.'
  else if AError = CUFFT_INTERNAL_ERROR then
    result := 'Used for all internal driver errors.'
  else if AError = CUFFT_EXEC_FAILED then
    result := 'CUFFT failed to execute an FFT on the GPU.'
  else if AError = CUFFT_SETUP_FAILED then
    result := 'The CUFFT library failed to initialize.'
  else if AError = CUFFT_INVALID_SIZE then
    result := 'The user specifies an unsupported FFT size.'
  else
    result := 'Unknown error.'
end;

end.
