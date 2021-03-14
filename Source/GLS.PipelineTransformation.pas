//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.PipelineTransformation;

(* Pipeline transformations *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,

  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.Logger;

const
  MAX_MATRIX_STACK_DEPTH = 128;

type

  TGLPipelineTransformationState =
  (
    trsModelViewChanged,
    trsInvModelViewChanged,
    trsInvModelChanged,
    trsNormalModelChanged,
    trsViewProjChanged,
    trsFrustum
  );

  TGLPipelineTransformationStates = set of TGLPipelineTransformationState;

const
  cAllStatesChanged = [trsModelViewChanged, trsInvModelViewChanged, trsInvModelChanged, trsViewProjChanged, trsNormalModelChanged, trsFrustum];

type
  PTransformationRec = ^TTransformationRec;
  TTransformationRec = record
    FStates: TGLPipelineTransformationStates;
    FModelMatrix: TGLMatrix;
    FViewMatrix: TGLMatrix;
    FProjectionMatrix: TGLMatrix;
    FInvModelMatrix: TGLMatrix;
    FNormalModelMatrix: TAffineMatrix;
    FModelViewMatrix: TGLMatrix;
    FInvModelViewMatrix: TGLMatrix;
    FViewProjectionMatrix: TGLMatrix;
    FFrustum: TFrustum;
  end;

type

  TOnMatricesPush = procedure() of object;

  TGLTransformation = class(TObject)
  private
    FStackPos: Integer;
    FStack: array of TTransformationRec;
    FLoadMatricesEnabled: Boolean;
    FOnPush: TOnMatricesPush;
    function GetModelMatrix: PGLMatrix; inline;
    function GetViewMatrix: PGLMatrix; inline;
    function GetProjectionMatrix: PGLMatrix; inline;
    function GetModelViewMatrix: PGLMatrix; inline;
    function GetInvModelViewMatrix: PGLMatrix; inline;
    function GetInvModelMatrix: PGLMatrix; inline;
    function GetNormalModelMatrix: PAffineMatrix; inline;
    function GetViewProjectionMatrix: PGLMatrix; inline;
    function GetFrustum: TFrustum; inline;
  protected
    procedure LoadModelViewMatrix; inline;
    procedure LoadProjectionMatrix; inline;
    procedure DoMatricesLoaded; inline;
    property OnPush: TOnMatricesPush read FOnPush write FOnPush;
  public
    constructor Create;
    procedure SetModelMatrix(const AMatrix: TGLMatrix); inline;
    procedure SetViewMatrix(const AMatrix: TGLMatrix); inline;
    procedure SetProjectionMatrix(const AMatrix: TGLMatrix); inline;
    procedure IdentityAll; inline;
    procedure Push(AValue: PTransformationRec); overload;
    procedure Push(); overload; inline;
    procedure Pop;
    procedure ReplaceFromStack;
    function StackTop: TTransformationRec; inline;
    property ModelMatrix: PGLMatrix read GetModelMatrix;
    property ViewMatrix: PGLMatrix read GetViewMatrix;
    property ProjectionMatrix: PGLMatrix read GetProjectionMatrix;
    property InvModelMatrix: PGLMatrix read GetInvModelMatrix;
    property ModelViewMatrix: PGLMatrix read GetModelViewMatrix;
    property NormalModelMatrix: PAffineMatrix read GetNormalModelMatrix;
    property InvModelViewMatrix: PGLMatrix read GetInvModelViewMatrix;
    property ViewProjectionMatrix: PGLMatrix read GetViewProjectionMatrix;
    property Frustum: TFrustum read GetFrustum;
    property LoadMatricesEnabled: Boolean read FLoadMatricesEnabled write FLoadMatricesEnabled;
  end;

//=====================================================================
implementation
//=====================================================================

constructor TGLTransformation.Create;
begin
  FStackPos := 0;
  SetLength(FStack, MAX_MATRIX_STACK_DEPTH);
  IdentityAll;
end;

procedure TGLTransformation.LoadProjectionMatrix;
begin
  glMatrixMode(GL_PROJECTION);
  glLoadMatrixf(@FStack[FStackPos].FProjectionMatrix);
  glMatrixMode(GL_MODELVIEW);
end;

function TGLTransformation.GetModelViewMatrix: PGLMatrix;
begin
  if trsModelViewChanged in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FModelViewMatrix :=
      MatrixMultiply(FStack[FStackPos].FModelMatrix, FStack[FStackPos].FViewMatrix);
    Exclude(FStack[FStackPos].FStates, trsModelViewChanged);
  end;
  Result := @FStack[FStackPos].FModelViewMatrix;
end;

procedure TGLTransformation.LoadModelViewMatrix;
begin
  glLoadMatrixf(PGLFloat(GetModelViewMatrix));
end;

procedure TGLTransformation.IdentityAll;
begin
  with FStack[FStackPos] do
  begin
    FModelMatrix := IdentityHmgMatrix;
    FViewMatrix := IdentityHmgMatrix;
    FProjectionMatrix := IdentityHmgMatrix;
    FStates := cAllStatesChanged;
  end;
  if LoadMatricesEnabled then
  begin
    LoadModelViewMatrix;
    LoadProjectionMatrix;
  end;
end;

procedure TGLTransformation.DoMatricesLoaded;
begin
  if Assigned(FOnPush) then
    FOnPush();
end;

procedure TGLTransformation.Push;
var
  prevPos: Integer;
begin
  prevPos := FStackPos;
  Inc(FStackPos);
  FStack[FStackPos] := FStack[prevPos];
end;

procedure TGLTransformation.Push(AValue: PTransformationRec);
var
  prevPos: Integer;
begin
  {$IFDEF USE_LOGGING}
  if FStackPos > MAX_MATRIX_STACK_DEPTH then
  begin
    GLSLogger.LogWarningFmt('Transformation stack overflow, more then %d values',
      [MAX_MATRIX_STACK_DEPTH]);
  end;
  {$ENDIF}
  prevPos := FStackPos;
  Inc(FStackPos);

  if Assigned(AValue) then
  begin
    FStack[FStackPos] := AValue^;
    if LoadMatricesEnabled then
    begin
      LoadModelViewMatrix;
      LoadProjectionMatrix;
    end;
    DoMatricesLoaded;
  end
  else
    FStack[FStackPos] := FStack[prevPos];
end;

procedure TGLTransformation.Pop;
begin
  {$IFDEF USE_LOGGING}
  if FStackPos = 0 then
  begin
    GLSLogger.LogError('Transformation stack underflow');
    exit;
  end;
  {$ENDIF}

  Dec(FStackPos);
  if LoadMatricesEnabled then
  begin
    LoadModelViewMatrix;
    LoadProjectionMatrix;
  end;
end;

procedure TGLTransformation.ReplaceFromStack;
var
  prevPos: Integer;
begin
  {$IFDEF USE_LOGGING}
  if FStackPos = 0 then
  begin
    GLSLogger.LogError('Transformation stack underflow');
    exit;
  end;
  {$ENDIF}
  prevPos := FStackPos - 1;
  FStack[FStackPos].FModelMatrix := FStack[prevPos].FModelMatrix;
  FStack[FStackPos].FViewMatrix:= FStack[prevPos].FViewMatrix;
  FStack[FStackPos].FProjectionMatrix:= FStack[prevPos].FProjectionMatrix;
  FStack[FStackPos].FStates := FStack[prevPos].FStates;
  if LoadMatricesEnabled then
  begin
    LoadModelViewMatrix;
    LoadProjectionMatrix;
  end;
end;

function TGLTransformation.GetModelMatrix: PGLMatrix;
begin
  Result := @FStack[FStackPos].FModelMatrix;
end;

function TGLTransformation.GetViewMatrix: PGLMatrix;
begin
  Result := @FStack[FStackPos].FViewMatrix;
end;

function TGLTransformation.GetProjectionMatrix: PGLMatrix;
begin
  Result := @FStack[FStackPos].FProjectionMatrix;
end;

procedure TGLTransformation.SetModelMatrix(const AMatrix: TGLMatrix);
begin
  FStack[FStackPos].FModelMatrix := AMatrix;
  FStack[FStackPos].FStates := FStack[FStackPos].FStates +
    [trsModelViewChanged, trsInvModelViewChanged, trsInvModelChanged, trsNormalModelChanged];
  if LoadMatricesEnabled then
    LoadModelViewMatrix;
end;

procedure TGLTransformation.SetViewMatrix(const AMatrix: TGLMatrix);
begin
  FStack[FStackPos].FViewMatrix:= AMatrix;
  FStack[FStackPos].FStates := FStack[FStackPos].FStates +
    [trsModelViewChanged, trsInvModelViewChanged, trsViewProjChanged, trsFrustum];
  if LoadMatricesEnabled then
    LoadModelViewMatrix;
end;

function TGLTransformation.StackTop: TTransformationRec;
begin
  Result := FStack[FStackPos];
end;

procedure TGLTransformation.SetProjectionMatrix(const AMatrix: TGLMatrix);
begin
  FStack[FStackPos].FProjectionMatrix := AMatrix;
  FStack[FStackPos].FStates := FStack[FStackPos].FStates +
    [trsViewProjChanged, trsFrustum];
  if LoadMatricesEnabled then
    LoadProjectionMatrix;
end;


function TGLTransformation.GetInvModelViewMatrix: PGLMatrix;
begin
  if trsInvModelViewChanged in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FInvModelViewMatrix := GetModelViewMatrix^;
    InvertMatrix(FStack[FStackPos].FInvModelViewMatrix);
    Exclude(FStack[FStackPos].FStates, trsInvModelViewChanged);
  end;
  Result := @FStack[FStackPos].FInvModelViewMatrix;
end;

function TGLTransformation.GetInvModelMatrix: PGLMatrix;
begin
  if trsInvModelChanged in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FInvModelMatrix := MatrixInvert(FStack[FStackPos].FModelMatrix);
    Exclude(FStack[FStackPos].FStates, trsInvModelChanged);
  end;
  Result := @FStack[FStackPos].FInvModelMatrix;
end;

function TGLTransformation.GetNormalModelMatrix: PAffineMatrix;
var
  M: TGLMatrix;
begin
  if trsNormalModelChanged in FStack[FStackPos].FStates then
  begin
    M := FStack[FStackPos].FModelMatrix;
    NormalizeMatrix(M);
    SetMatrix(FStack[FStackPos].FNormalModelMatrix, M);
    Exclude(FStack[FStackPos].FStates, trsNormalModelChanged);
  end;
  Result := @FStack[FStackPos].FNormalModelMatrix;
end;

function TGLTransformation.GetViewProjectionMatrix: PGLMatrix;
begin
  if trsViewProjChanged in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FViewProjectionMatrix :=
      MatrixMultiply(FStack[FStackPos].FViewMatrix, FStack[FStackPos].FProjectionMatrix);
    Exclude(FStack[FStackPos].FStates, trsViewProjChanged);
  end;
  Result := @FStack[FStackPos].FViewProjectionMatrix;
end;

function TGLTransformation.GetFrustum: TFrustum;
begin
  if trsFrustum in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FFrustum := ExtractFrustumFromModelViewProjection(GetViewProjectionMatrix^);
    Exclude(FStack[FStackPos].FStates, trsFrustum);
  end;
  Result := FStack[FStackPos].FFrustum;
end;

end.
