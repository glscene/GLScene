//
// The graphics engine GLXEngine
//
unit Stage.PipelineTransform;

(* Pipeline transformations *)

interface

{$I Stage.Defines.inc}

uses
  Winapi.OpenGL,

  Stage.VectorTypes,
  Stage.VectorGeometry,
  Stage.Logger;

const
  MAX_MATRIX_STACK_DEPTH = 128;

type
  TgPipelineTransformationState =
  (
    trsModelViewChanged,
    trsInvModelViewChanged,
    trsInvModelChanged,
    trsNormalModelChanged,
    trsViewProjChanged,
    trsFrustum
  );
  TgPipelineTransformationStates = set of TgPipelineTransformationState;

const
  cAllStatesChanged = [trsModelViewChanged, trsInvModelViewChanged, trsInvModelChanged, trsViewProjChanged, trsNormalModelChanged, trsFrustum];

type
  PgTransformationRec = ^TgTransformationRec;
  TgTransformationRec = record
    FStates: TgPipelineTransformationStates;
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
  TgOnMatricesPush = procedure() of object;

  TgTransformation = class(TObject)
  private
    FStackPos: Integer;
    FStack: array of TgTransformationRec;
    FLoadMatricesEnabled: Boolean;
    FOnPush: TGOnMatricesPush;
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
    property OnPush: TgOnMatricesPush read FOnPush write FOnPush;
  public
    constructor Create;
    procedure SetModelMatrix(const AMatrix: TGLMatrix); inline;
    procedure SetViewMatrix(const AMatrix: TGLMatrix); inline;
    procedure SetProjectionMatrix(const AMatrix: TGLMatrix); inline;
    procedure IdentityAll; inline;
    procedure Push(AValue: PGTransformationRec); overload;
    procedure Push(); overload; inline;
    procedure Pop;
    procedure ReplaceFromStack;
    function StackTop: TGTransformationRec; inline;
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

implementation //------------------------------------------------------------

constructor TGTransformation.Create;
begin
  FStackPos := 0;
  SetLength(FStack, MAX_MATRIX_STACK_DEPTH);
  IdentityAll;
end;

procedure TGTransformation.LoadProjectionMatrix;
begin
  glMatrixMode(GL_PROJECTION);
  glLoadMatrixf(@FStack[FStackPos].FProjectionMatrix);
  glMatrixMode(GL_MODELVIEW);
end;

function TGTransformation.GetModelViewMatrix: PGLMatrix;
begin
  if trsModelViewChanged in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FModelViewMatrix :=
      MatrixMultiply(FStack[FStackPos].FModelMatrix, FStack[FStackPos].FViewMatrix);
    Exclude(FStack[FStackPos].FStates, trsModelViewChanged);
  end;
  Result := @FStack[FStackPos].FModelViewMatrix;
end;

procedure TGTransformation.LoadModelViewMatrix;
begin
  glLoadMatrixf(PGLFloat(GetModelViewMatrix));
end;

procedure TGTransformation.IdentityAll;
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

procedure TGTransformation.DoMatricesLoaded;
begin
  if Assigned(FOnPush) then
    FOnPush();
end;

procedure TGTransformation.Push;
var
  prevPos: Integer;
begin
  prevPos := FStackPos;
  Inc(FStackPos);
  FStack[FStackPos] := FStack[prevPos];
end;

procedure TGTransformation.Push(AValue: PGTransformationRec);
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

procedure TGTransformation.Pop;
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

procedure TGTransformation.ReplaceFromStack;
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

function TGTransformation.GetModelMatrix: PGLMatrix;
begin
  Result := @FStack[FStackPos].FModelMatrix;
end;

function TGTransformation.GetViewMatrix: PGLMatrix;
begin
  Result := @FStack[FStackPos].FViewMatrix;
end;

function TGTransformation.GetProjectionMatrix: PGLMatrix;
begin
  Result := @FStack[FStackPos].FProjectionMatrix;
end;

procedure TGTransformation.SetModelMatrix(const AMatrix: TGLMatrix);
begin
  FStack[FStackPos].FModelMatrix := AMatrix;
  FStack[FStackPos].FStates := FStack[FStackPos].FStates +
    [trsModelViewChanged, trsInvModelViewChanged, trsInvModelChanged, trsNormalModelChanged];
  if LoadMatricesEnabled then
    LoadModelViewMatrix;
end;

procedure TGTransformation.SetViewMatrix(const AMatrix: TGLMatrix);
begin
  FStack[FStackPos].FViewMatrix:= AMatrix;
  FStack[FStackPos].FStates := FStack[FStackPos].FStates +
    [trsModelViewChanged, trsInvModelViewChanged, trsViewProjChanged, trsFrustum];
  if LoadMatricesEnabled then
    LoadModelViewMatrix;
end;

function TGTransformation.StackTop: TGTransformationRec;
begin
  Result := FStack[FStackPos];
end;

procedure TGTransformation.SetProjectionMatrix(const AMatrix: TGLMatrix);
begin
  FStack[FStackPos].FProjectionMatrix := AMatrix;
  FStack[FStackPos].FStates := FStack[FStackPos].FStates +
    [trsViewProjChanged, trsFrustum];
  if LoadMatricesEnabled then
    LoadProjectionMatrix;
end;


function TGTransformation.GetInvModelViewMatrix: PGLMatrix;
begin
  if trsInvModelViewChanged in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FInvModelViewMatrix := GetModelViewMatrix^;
    InvertMatrix(FStack[FStackPos].FInvModelViewMatrix);
    Exclude(FStack[FStackPos].FStates, trsInvModelViewChanged);
  end;
  Result := @FStack[FStackPos].FInvModelViewMatrix;
end;

function TGTransformation.GetInvModelMatrix: PGLMatrix;
begin
  if trsInvModelChanged in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FInvModelMatrix := MatrixInvert(FStack[FStackPos].FModelMatrix);
    Exclude(FStack[FStackPos].FStates, trsInvModelChanged);
  end;
  Result := @FStack[FStackPos].FInvModelMatrix;
end;

function TGTransformation.GetNormalModelMatrix: PAffineMatrix;
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

function TGTransformation.GetViewProjectionMatrix: PGLMatrix;
begin
  if trsViewProjChanged in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FViewProjectionMatrix :=
      MatrixMultiply(FStack[FStackPos].FViewMatrix, FStack[FStackPos].FProjectionMatrix);
    Exclude(FStack[FStackPos].FStates, trsViewProjChanged);
  end;
  Result := @FStack[FStackPos].FViewProjectionMatrix;
end;

function TGTransformation.GetFrustum: TFrustum;
begin
  if trsFrustum in FStack[FStackPos].FStates then
  begin
    FStack[FStackPos].FFrustum := ExtractFrustumFromModelViewProjection(GetViewProjectionMatrix^);
    Exclude(FStack[FStackPos].FStates, trsFrustum);
  end;
  Result := FStack[FStackPos].FFrustum;
end;

//-----------------------------------------------------------------------------

end.
