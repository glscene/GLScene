//
// The graphics platform GLScene https://github.com/glscene
//
unit CUDA.FFTPlan;

(*  Fast Fourier Transform for CUDA *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,

  CUDA.Import,
  CUDA.Context,
  CUDA.APIComps,
  CUDA.FourierTransform,

  GLS.Strings,
  GLS.Logger;

type

  TCUDAFFTransform =
  (
    fftRealToComplex,
    fftComplexToReal,
    fftComplexToComplex,
    fftDoubleToDoubleComplex,
    fftDoubleComplexToDouble,
    fftDoubleComplexToDoubleComplex
  );

  TCUDAFFTdir = (fftdForward, fftdInverse);

  TCUDAFFTPlan = class(TCUDAComponent)
  private
    FHandle: TcufftHandle;
    FWidth: Integer;
    FHeight: Integer;
    FDepth: Integer;
    FBatch: Integer;
    FSize: Integer;
    FPaddedSize: Integer;
    FTransform: TCUDAFFTransform;
    FStatus: TcufftResult;
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
    procedure SetDepth(Value: Integer);
    procedure SetBatch(Value: Integer);
    procedure SetTransform(Value: TCUDAFFTransform);
  protected
    procedure AllocateHandles; override;
    procedure DestroyHandles; override;
    class procedure CheckLib;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Execute(ASrc: TCUDAMemData; ADst: TCUDAMemData;
      const ADir: TCUDAFFTdir = fftdForward);
  published
    property Width: Integer read fWidth write SetWidth default 256;
    property Height: Integer read FHeight write SetHeight default 0;
    property Depth: Integer read FDepth write SetDepth default 0;
    property Batch: Integer read FBatch write SetBatch default 1;
    property Transform: TCUDAFFTransform read FTransform write SetTransform
      default fftRealToComplex;
  end;

//---------------------------------------------------------------------  
implementation
//---------------------------------------------------------------------  

constructor TCUDAFFTPlan.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := INVALID_CUFFT_HANDLE;
  fWidth := 256;
  FHeight := 0;
  FDepth := 0;
  FBatch := 1;
  FTransform := fftRealToComplex;
end;

destructor TCUDAFFTPlan.Destroy;
begin
  DestroyHandles;
  inherited;
end;

class procedure TCUDAFFTPlan.CheckLib;
begin
  if not IsCUFFTInitialized then
    if not InitCUFFT then
    begin
      GLSLogger.LogError('Can not initialize CUFFT library');
      Abort;
    end;
end;

procedure TCUDAFFTPlan.Assign(Source: TPersistent);
var
  plan: TCUDAFFTPlan;
begin
  if Source is TCUDAFFTPlan then
  begin
    DestroyHandles;
    plan := TCUDAFFTPlan(Source);
    Width := plan.fWidth;
    Height := plan.FHeight;
    Depth := plan.FDepth;
    Transform := plan.FTransform;
  end;
  inherited Assign(Source);
end;

procedure TCUDAFFTPlan.AllocateHandles;
var
  LType: TcufftType;
begin
  DestroyHandles;

  case FTransform of
    fftRealToComplex:
      LType := CUFFT_R2C;
    fftComplexToReal:
      LType := CUFFT_C2R;
    fftComplexToComplex:
      LType := CUFFT_C2C;
    fftDoubleToDoubleComplex:
      LType := CUFFT_D2Z;
    fftDoubleComplexToDouble:
      LType := CUFFT_Z2D;
    fftDoubleComplexToDoubleComplex:
      LType := CUFFT_Z2Z;
  else
    begin
      Assert(False, 'Error: Unknown Type');
      LType := CUFFT_R2C;
    end;
  end;

  Context.Requires;

  if (FHeight = 0) and (FDepth = 0) then
  begin
    FStatus := cufftPlan1d(FHandle, fWidth, LType, FBatch);
    FSize := FWidth;
    FPaddedSize := FWidth div 2 + 1;
    if FBatch > 0 then
    begin
      FSize := FSize * FBatch;
      FPaddedSize := FPaddedSize * FBatch;
    end;
  end
  else if FDepth = 0 then
  begin
    FStatus := cufftPlan2d(FHandle, fWidth, FHeight, LType);
    FSize := FWidth * FHeight;
    FPaddedSize := FWidth * (FHeight div 2 + 1);
  end
  else
  begin
    FStatus := cufftPlan3d(FHandle, fWidth, FHeight, FDepth, LType);
    FSize := FWidth * FHeight * FDepth;
    FPaddedSize := FWidth * FHeight * (FDepth div 2 + 1);
  end;

  Context.Release;

  if FStatus <> CUFFT_SUCCESS then
  begin
    FHandle := INVALID_CUFFT_HANDLE;
    Abort;
  end;

  Context.Requires;
  FStatus := cufftSetCompatibilityMode(FHandle, CUFFT_COMPATIBILITY_FFTW_PADDING);
  Context.Release;

  fChanges := [];
  inherited;
end;

procedure TCUDAFFTPlan.DestroyHandles;
begin
  inherited;
  CheckLib;

  if FHandle <> INVALID_CUFFT_HANDLE then
  begin
    Context.Requires;
    FStatus := cufftDestroy(FHandle);
    Context.Release;
    if FStatus <> CUFFT_SUCCESS then
      Abort;
    FHandle := 0;
    FPaddedSize := 0;
  end;
end;

procedure TCUDAFFTPlan.SetWidth(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value <> fWidth then
  begin
    fWidth := Value;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAFFTPlan.SetHeight(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FHeight then
  begin
    FHeight := Value;
    if FHeight > 0 then
      FBatch := 1;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAFFTPlan.SetDepth(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FDepth then
  begin
    FDepth := Value;
    if FDepth > 0 then
      FBatch := 1;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAFFTPlan.SetBatch(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value <> FBatch then
  begin
    FBatch := Value;
    if FBatch > 1 then
    begin
      FHeight := 0;
      FDepth := 0;
    end;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAFFTPlan.SetTransform(Value: TCUDAFFTransform);
begin
  if Value <> FTransform then
  begin
    FTransform := Value;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAFFTPlan.Execute(ASrc: TCUDAMemData; ADst: TCUDAMemData;
  const ADir: TCUDAFFTdir);
const
  sFFTdir: array [TCUDAFFTdir] of Integer = (CUFFT_FORWARD, CUFFT_INVERSE);

  cSourceTypeSize: array[TCUDAFFTransform] of Byte = (
  SizeOf(TcufftReal),
  SizeOf(TcufftComplex),
  SizeOf(TcufftComplex),
  SizeOf(TcufftDoubleReal),
  SizeOf(TcufftDoubleComplex),
  SizeOf(TcufftDoubleComplex));

  cDestinationTypeSize: array[TCUDAFFTransform] of Byte = (
  SizeOf(TcufftComplex),
  SizeOf(TcufftReal),
  SizeOf(TcufftComplex),
  SizeOf(TcufftDoubleComplex),
  SizeOf(TcufftDoubleReal),
  SizeOf(TcufftDoubleComplex));
var
  SrcPtr, DstPtr: Pointer;
  LSrcSize, LDstSize: Integer;

  procedure ForwardCheck;
  begin
    if (LSrcSize * FSize > ASrc.DataSize)
      or (LDstSize * FPaddedSize > ADst.DataSize) then
    begin
      // Bad Plan Size);
      Abort;
    end;
  end;

  procedure InverseCheck;
  begin
    if (LSrcSize * FPaddedSize > ASrc.DataSize)
      or (LDstSize * FSize > ADst.DataSize) then
    begin
      // Bad Plan Size);
      Abort;
    end;
  end;

begin
  if (FHandle = INVALID_CUFFT_HANDLE) or (fChanges <> []) then
    AllocateHandles;

  if CUDAContextManager.GetCurrentThreadContext <> nil then
  begin
    GLSLogger.LogError(strRequireFreeThread);
    Abort;
  end;

  SrcPtr := ASrc.RawData;
  DstPtr := ADst.RawData;

  LSrcSize := cSourceTypeSize[FTransform];
  LDstSize := cDestinationTypeSize[FTransform];

  Context.Requires;
  try
    case FTransform of
      fftRealToComplex:
        begin
          ForwardCheck;
          FStatus := cufftExecR2C(FHandle, SrcPtr, DstPtr);
        end;

      fftComplexToReal:
        begin
          InverseCheck;
          FStatus := cufftExecC2R(FHandle, SrcPtr, DstPtr);
        end;

      fftComplexToComplex:
        begin
          case ADir of
            fftdForward: ForwardCheck;
            fftdInverse: InverseCheck;
          end;
          FStatus := cufftExecC2C(FHandle, SrcPtr, DstPtr, sFFTdir[ADir]);
        end;

      fftDoubleToDoubleComplex:
      begin
        ForwardCheck;
        FStatus := cufftExecD2Z(FHandle, SrcPtr, DstPtr);
      end;

      fftDoubleComplexToDouble:
      begin
        InverseCheck;
        FStatus := cufftExecZ2D(FHandle, SrcPtr, DstPtr);
      end;

      fftDoubleComplexToDoubleComplex:
      begin
        case ADir of
          fftdForward: ForwardCheck;
          fftdInverse: InverseCheck;
        end;
        FStatus := cufftExecZ2Z(FHandle, SrcPtr, DstPtr, sFFTdir[ADir]);
      end
    else
      FStatus := CUFFT_INVALID_VALUE;
    end;
  finally
    Context.Release;
  end;

  if FStatus <> CUFFT_SUCCESS then
    Abort;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterClasses([TCUDAFFTPlan]);

finalization

  CloseCUFFT;

end.
