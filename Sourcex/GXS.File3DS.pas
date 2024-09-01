//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.File3DS;

(* 3DStudio 3DS vector file format implementation *)

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Math,
  
  GXS.Strings,
  GXS.OpenGL,
  GXS.Scene,
  GXS.Objects,
  GXS.VectorFileObjects,
  GXS.Texture,
  GXS.ApplicationFileIO,
  GXS.VectorGeometry,
  GXS.Context,
  GXS.PersistentClasses,
  GXS.File3DSSceneObjects,
  GXS.VectorTypes,
  GXS.VectorLists,
  GXS.RenderContextInfo,
  GXS.Material,

  Formatx.m3DS,
  Formatx.m3DSTypes;

type

  EGLFile3DS = class(Exception);

  // A record that holds all the information that is used during 3ds animation.
  TgxFile3DSAnimationData = packed record
    ModelMatrix: TMatrix4f;
    Color: TVector4f;            // Omni Light.
    TargetPos: TAffineVector;  // Spot Light.
    SpotLightCutOff: single;
    HotSpot: single;
    Roll: single;
  end;

  //  An abstract class that describes how to interpolate animation keys.
  TgxFile3DSAnimationKeys = class(TgxPersistentObject)
  private
    FNumKeys: integer;
    FKeys: array of TKeyHeader3DS;
    procedure InterpolateFrame(var I: integer; var w: real; const AFrame: real);
  protected
    function InterpolateValue(const AValues: array of single; const AFrame: real): single; overload;
    function InterpolateValue(const AValues: array of TAffineVector; const AFrame: real): TAffineVector; overload;
    function InterpolateValue(const AValues: array of TKFRotKey3DS; const AFrame: real): TMatrix4f; overload;
  public
    procedure LoadData(const ANumKeys: integer; const Keys: PKeyHeaderList; const AData: Pointer); virtual;
    procedure Apply(var DataTransf: TgxFile3DSAnimationData; const AFrame: real); virtual; abstract;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(Reader: TgxVirtualReader); override;
  end;

  TgxFile3DSScaleAnimationKeys = class(TgxFile3DSAnimationKeys)
  private
    FScale: array of TAffineVector;
  public
    procedure LoadData(const ANumKeys: integer; const Keys: PKeyHeaderList; const AData: Pointer); override;
    procedure Apply(var DataTransf: TgxFile3DSAnimationData; const AFrame: real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(Reader: TgxVirtualReader); override;
  end;

  TgxFile3DSRotationAnimationKeys = class(TgxFile3DSAnimationKeys)
  private
    FRot: array of TKFRotKey3DS;
  public
    procedure LoadData(const ANumKeys: integer; const Keys: PKeyHeaderList; const AData: Pointer); override;
    procedure Apply(var DataTransf: TgxFile3DSAnimationData; const AFrame: real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(Reader: TgxVirtualReader); override;
  end;

  TgxFile3DSPositionAnimationKeys = class(TgxFile3DSAnimationKeys)
  private
    FPos: array of TAffineVector;
  public
    procedure LoadData(const ANumKeys: integer; const Keys: PKeyHeaderList; const AData: Pointer); override;
    procedure Apply(var DataTransf: TgxFile3DSAnimationData; const AFrame: real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(Reader: TgxVirtualReader); override;
  end;

  TgxFile3DSColorAnimationKeys = class(TgxFile3DSAnimationKeys)
  private
    FCol: array of TAffineVector;
  public
    procedure LoadData(const ANumKeys: integer; const Keys: PKeyHeaderList; const AData: Pointer); override;
    procedure Apply(var DataTransf: TgxFile3DSAnimationData; const AFrame: real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(Reader: TgxVirtualReader); override;
  end;

  TTgxFile3DSPositionAnimationKeys = class(TgxFile3DSAnimationKeys)
  private
    FTPos: array of TAffineVector;
  public
    procedure LoadData(const ANumKeys: integer; const Keys: PKeyHeaderList; const AData: Pointer); override;
    procedure Apply(var DataTransf: TgxFile3DSAnimationData; const AFrame: real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(Reader: TgxVirtualReader); override;
  end;

  TgxFile3DSSpotLightCutOffAnimationKeys = class(TgxFile3DSAnimationKeys)
  private
    FFall: array of single;
  public
    procedure LoadData(const ANumKeys: integer; const Keys: PKeyHeaderList; const AData: Pointer); override;
    procedure Apply(var DataTransf: TgxFile3DSAnimationData; const AFrame: real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(Reader: TgxVirtualReader); override;
  end;

  TgxFile3DSLightHotSpotAnimationKeys = class(TgxFile3DSAnimationKeys)
  private
    FHot: array of single;
  public
    procedure LoadData(const ANumKeys: integer; const Keys: PKeyHeaderList; const AData: Pointer); override;
    procedure Apply(var DataTransf: TgxFile3DSAnimationData; const AFrame: real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(Reader: TgxVirtualReader); override;
  end;

  TgxFile3DSRollAnimationKeys = class(TgxFile3DSAnimationKeys)
  private
    FRoll: array of single;
  public
    procedure LoadData(const ANumKeys: integer; const Keys: PKeyHeaderList; const AData: Pointer); override;
    procedure Apply(var DataTransf: TgxFile3DSAnimationData; const AFrame: real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(Reader: TgxVirtualReader); override;
  end;

  TgxFile3DSAnimationKeyList = class(TgxPersistentObject)
  private
    FAnimKeysList: array of TgxFile3DSAnimationKeys;
  protected
    procedure ApplyAnimKeys(var DataTransf: TgxFile3DSAnimationData; const AFrame: real);
  public
    procedure AddKeys(const AItem: TgxFile3DSAnimationKeys);
    procedure ClearKeys;

    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(Reader: TgxVirtualReader); override;
    destructor Destroy; override;
  end;

  // Used only for serialization. There probably is a more efficient way to do it.
  TgxFile3DSAnimKeysClassType = (ctScale, ctRot, ctPos, ctCol, ctTPos, ctFall, ctHot, ctRoll);

  // A 3ds-specific TgxMorphableMeshObject.
  TgxFile3DSDummyObject = class(TgxMorphableMeshObject)
  private
    FAnimList: TgxFile3DSAnimationKeyList;
    FAnimData: Pointer;
    FRefTranf, FAnimTransf: TgxFile3DSAnimationData;
    FParent: TgxFile3DSDummyObject;
    FParentName: String64;
    FStatic : Boolean; // Static tag used in BuildList to not apply animation matrix
  public
    procedure LoadAnimation(const AData: Pointer); virtual;
    procedure SetFrame(const AFrame: real); virtual;
    procedure MorphTo(morphTargetIndex: integer); override;
    procedure Lerp(morphTargetIndex1, morphTargetIndex2: integer; lerpFactor: single); override;
    procedure GetExtents(out min, max: TAffineVector); override;
    function ExtractTriangles(texCoords: TgxAffineVectorList = nil; normals: TgxAffineVectorList = nil): TgxAffineVectorList;
      override;
    procedure WriteToFiler(Writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(Reader: TgxVirtualReader); override;
    procedure Assign(Source: TPersistent); override;
    constructor Create; override;
    destructor Destroy; override;

    property AnimList: TgxFile3DSAnimationKeyList read FAnimList;
    property Parent: TgxFile3DSDummyObject read FParent write FParent;
    property RefrenceTransf: TgxFile3DSAnimationData read FRefTranf write FRefTranf;
  end;

  // A 3ds-specific mesh object.
  TgxFile3DSMeshObject = class(TgxFile3DSDummyObject)
  public
    procedure LoadAnimation(const AData: Pointer); override;
    procedure BuildList(var ARci: TgxRenderContextInfo); override;
  end;

  { A 3ds-specific omni light. }
  TgxFile3DSOmniLightObject = class(TgxFile3DSDummyObject)
  private
    FLightSrc: TgxFile3DSLight;
    FLightSrcName: String64;
  public
    constructor Create; override;
    procedure LoadData(const AOwner: TgxBaseMesh; const AData: PLight3DS); virtual;
    procedure LoadAnimation(const AData: Pointer); override;
    procedure SetFrame(const AFrame: real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(Reader: TgxVirtualReader); override;
    destructor Destroy; override;
  end;

  //  A 3ds-specific spot light.
  TgxFile3DSSpotLightObject = class(TgxFile3DSOmniLightObject)
  public
    procedure LoadData(const AOwner: TgxBaseMesh; const AData: PLight3DS); override;
    procedure LoadAnimation(const AData: Pointer); override;
    procedure SetFrame(const AFrame: real); override;
  end;

  // A 3ds-specific camera.
  TgxFile3DSCameraObject = class(TgxFile3DSDummyObject)
  private
    FTargetObj: TgxDummyCube;
    FCameraSrc: TgxFile3DSCamera;
    FCameraSrcName: String64;
  public
    constructor Create; override;
    procedure LoadData(Owner: TgxBaseMesh; AData: PCamera3DS);
    procedure LoadAnimation(const AData: Pointer); override;
    procedure SetFrame(const AFrame: real); override;
    procedure WriteToFiler(Writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(Reader: TgxVirtualReader); override;
    destructor Destroy; override;
  end;

  (* The 3DStudio vector file.
     A 3DS file may contain material
     information and require textures when loading. *)
  Tgx3DSVectorFile = class(TgxVectorFile)
  public

    class function Capabilities: TDataFileCapabilities; override;
    procedure LoadFromStream(aStream: TStream); override;
  end;

var
  (* If enabled, advanced parameters will be loaded from a 3ds file
     (TextureScale, TextureOffset), but it might break backwards compatibility.
     If disabled, it won't break anything, but some parameters will not be
     loaded correctly from a 3ds file.
     Also there is a significant drop in FPS when this option is on
     (for unknown reasons), so it is off by default. *)
  vFile3DS_UseTextureEx: boolean = False;

  (* If enabled, allows 3ds animation and fixes loading of some 3ds models,
     but has a few bugs:
     - TgxFreeForm.AutoCentering does now work correctly.
     - TgxMeshObject.vertices return values different from
        TgxMeshObject.ExtractTriangles() *)
  vFile3DS_EnableAnimation: boolean = False;

  (* If enabled, a -90 degrees (-PI/2) rotation will occured on X Axis.
     By design 3dsmax has a Z Up-Axis, after the rotation the Up axis will
     be Y. (Note: you need vFile3DS_EnableAnimation = true) *)
  vFile3DS_FixDefaultUpAxisY: boolean = False;


  (* If >= 0, then the vertices list will be updated with selected frame
     animation data. (Note: you need vFile3DS_EnableAnimation = true).
     Be aware that in that case animation will not be usable, it is made
     to be used with a static mesh like GLFreeForm. *)
  vFile3DS_LoadedStaticFrame: integer = -1;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

const
  cGLFILE3DS_FIXDEFAULTUPAXISY_ROTATIONVALUE = PI/2;
  CGLFILE3DS_DEFAULT_FRAME = 0;

function AnimKeysClassTypeToClass(const AAnimKeysClassType: TgxFile3DSAnimKeysClassType): TClass;
begin
  case AAnimKeysClassType of
    ctScale: Result := TgxFile3DSScaleAnimationKeys;
    ctRot: Result := TgxFile3DSRotationAnimationKeys;
    ctPos: Result := TgxFile3DSPositionAnimationKeys;
    ctCol: Result := TgxFile3DSColorAnimationKeys;
    ctTPos: Result := TTgxFile3DSPositionAnimationKeys;
    ctFall: Result := TgxFile3DSSpotLightCutOffAnimationKeys;
    ctHot: Result := TgxFile3DSLightHotSpotAnimationKeys;
    ctRoll: Result := TgxFile3DSRollAnimationKeys;
    else
    begin
      Result := nil;
      Assert(False, strErrorEx + strUnknownType);
    end;
  end;
end;

function ClassToAnimKeysClassType(const AAnimKeysClass: TClass): TgxFile3DSAnimKeysClassType;
begin
  if AAnimKeysClass.InheritsFrom(TgxFile3DSScaleAnimationKeys) then
    Result := ctScale
  else if AAnimKeysClass.InheritsFrom(TgxFile3DSRotationAnimationKeys) then
    Result := ctRot
  else if AAnimKeysClass.InheritsFrom(TgxFile3DSPositionAnimationKeys) then
    Result := ctPos
  else if AAnimKeysClass.InheritsFrom(TgxFile3DSColorAnimationKeys) then
    Result := ctCol
  else if AAnimKeysClass.InheritsFrom(TTgxFile3DSPositionAnimationKeys) then
    Result := ctTPos
  else if AAnimKeysClass.InheritsFrom(TgxFile3DSSpotLightCutOffAnimationKeys) then
    Result := ctFall
  else if AAnimKeysClass.InheritsFrom(TgxFile3DSLightHotSpotAnimationKeys) then
    Result := ctHot
  else if AAnimKeysClass.InheritsFrom(TgxFile3DSRollAnimationKeys) then
    Result := ctRoll
  else
  begin
    Result := ctScale;
    Assert(False, strErrorEx + strUnknownType);
  end;
end;

function MakeRotationQuaternion(const axis: TAffineVector; angle: single): TQuaternion;
var
  v: TVector4f;
  halfAngle, invAxisLengthMult: single;
begin
  halfAngle := (angle) / 2;
  invAxisLengthMult := 1 / VectorLength(axis) * sin(halfAngle);

  v.X := axis.X * invAxisLengthMult;
  v.Y := axis.Y * invAxisLengthMult;
  v.Z := axis.Z * invAxisLengthMult;
  v.W := cos(halfAngle);

  Result.ImagPart := AffineVectorMake(v);
  Result.RealPart := v.W;
end;

function QuaternionToRotateMatrix(const Quaternion: TQuaternion): TMatrix4f;
var
  wx, wy, wz, xx, yy, yz, xy, xz, zz, x2, y2, z2: single;
  quat: TVector4f;
  m: TMatrix4f;
begin
  quat := VectorMake(Quaternion.ImagPart);
  quat.W := Quaternion.RealPart;

  x2 := quat.X + quat.X;
  y2 := quat.Y + quat.Y;
  z2 := quat.Z + quat.Z;
  xx := quat.X * x2;
  xy := quat.X * y2;
  xz := quat.X * z2;
  yy := quat.Y * y2;
  yz := quat.Y * z2;
  zz := quat.Z * z2;
  wx := quat.W * x2;
  wy := quat.W * y2;
  wz := quat.W * z2;

  m.X.X := 1.0 - (yy + zz);
  m.X.Y := xy - wz;
  m.X.Z := xz + wy;
  m.Y.X := xy + wz;
  m.Y.Y := 1.0 - (xx + zz);
  m.Y.Z := yz - wx;
  m.Z.X := xz - wy;
  m.Z.Y := yz + wx;
  m.Z.Z := 1.0 - (xx + yy);

  m.X.W := 0;
  m.Y.W := 0;
  m.Z.W := 0;
  m.W.X := 0;
  m.W.Y := 0;
  m.W.Z := 0;
  m.W.W := 1;

  Result := m;
end;

// ------------------
// ------------------ Support classes ------------------
// ------------------

procedure TgxFile3DSAnimationKeys.InterpolateFrame(var I: integer; var w: real; const AFrame: real);
begin
  w := 1;
  I := 0;
  if FNumKeys > 1 then
  begin
    while (FNumKeys > I) and ((FKeys[I].Time) <= AFrame) do
      Inc(I);
    if (FNumKeys > I) and ((FKeys[I - 1].Time) <= AFrame) then
      w := (AFrame - FKeys[I - 1].Time) / (FKeys[I].Time - FKeys[I - 1].Time);

    // Don't allow keys to go our of range.
    if I = FNumKeys then
      I := FNumKeys - 1;
  end;
end;

function TgxFile3DSAnimationKeys.InterpolateValue(const AValues: array of single; const AFrame: real): single;
var
  I: integer;
  w: real;
  start, stop: single;
begin
  InterpolateFrame(I, w, AFrame);

  if I > 0 then
    start := AValues[I - 1]
  else
    start := 0;
  if FNumKeys > I then
    stop := AValues[I]
  else
    stop := 0;

  Result := Lerp(start, stop, w);
end;

function TgxFile3DSAnimationKeys.InterpolateValue(const AValues: array of TAffineVector; const AFrame: real): TAffineVector;
var
  I: integer;
  w: real;
  start, stop: TAffineVector;
begin
  InterpolateFrame(I, w, AFrame);

  if I > 0 then
    start := AValues[I - 1]
  else
    start := NullVector;
  if FNumKeys > I then
    stop := AValues[I]
  else
    stop := NullVector;

  Result := VectorLerp(start, stop, w);
end;

function TgxFile3DSAnimationKeys.InterpolateValue(const AValues: array of TKFRotKey3DS; const AFrame: real): TMatrix4f;
var
  I: integer;
  w: real;
begin
  Result := IdentityHmgMatrix;

  // First find the final matrix for this frame.
  I := 0;
  while (FNumKeys > I) and ((FKeys[I].Time) <= AFrame) do
  begin
    with AValues[I] do
      Result := MatrixMultiply(Result, CreateRotationMatrix(AffineVectorMake(X, Y, Z), Angle));
    Inc(I);
  end;

  InterpolateFrame(I, w, AFrame);

  // Then interpolate this matrix
  if (FNumKeys > I) and ((FKeys[I].Time) > AFrame) and ((FKeys[I - 1].Time) < AFrame) then
  begin
    with AValues[I] do
      Result := MatrixMultiply(Result, CreateRotationMatrix(AffineVectorMake(X, Y, Z), AngleLerp(0, Angle, w)));
  end;
end;

procedure TgxFile3DSAnimationKeys.LoadData(const ANumKeys: integer; const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: integer;
begin
  FNumKeys := ANumKeys;
  SetLength(FKeys, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FKeys[I] := Keys[I];
end;

procedure TgxFile3DSAnimationKeys.Assign(Source: TPersistent);
var
  I: integer;
begin
  if Source is TgxFile3DSAnimationKeys then
  begin
    FNumKeys := TgxFile3DSAnimationKeys(Source).FNumKeys;
    SetLength(FKeys, FNumKeys);
    for I := 0 to FNumKeys - 1 do
      FKeys[I] := TgxFile3DSAnimationKeys(Source).FKeys[I];
  end
  else
    inherited Assign(Source);
end;

procedure TgxFile3DSAnimationKeys.WriteToFiler(Writer: TgxVirtualWriter);
begin
  Writer.WriteInteger(FNumKeys);
  if FNumKeys > 0 then
    Writer.Write(FKeys[0], FNumKeys * SizeOf(TKeyHeader3DS));
end;

procedure TgxFile3DSAnimationKeys.ReadFromFiler(Reader: TgxVirtualReader);
begin
  FNumKeys := Reader.ReadInteger;
  SetLength(FKeys, FNumKeys);
  if FNumKeys > 0 then
    Reader.Read(FKeys[0], FNumKeys * SizeOf(TKeyHeader3DS));
end;

procedure TgxFile3DSScaleAnimationKeys.LoadData(const ANumKeys: integer;
  const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: integer;
  AffVect : TAffineVector;
  Sign : ShortInt;
begin
  inherited;
  SetLength(FScale, FNumKeys);
  for I := 0 to FNumKeys - 1 do
  begin
    FScale[I] := TAffineVector(PPointList(AData)[I]);

    if vFile3DS_FixDefaultUpAxisY then
    begin
      AffVect := FScale[I];

      if (AffVect.X < 0) or (AffVect.Y < 0) or (AffVect.Z < 0) then
        Sign := -1
      else
        Sign:= 1;

      AffVect := VectorRotateAroundX(AffVect, cGLFILE3DS_FIXDEFAULTUPAXISY_ROTATIONVALUE);

      FScale[I].X := Sign * Abs(AffVect.X);
      FScale[I].Y := Sign * Abs(AffVect.Y);
      FScale[I].Z := Sign * Abs(AffVect.Z);
    end;

  end;
end;

procedure TgxFile3DSScaleAnimationKeys.Apply(var DataTransf: TgxFile3DSAnimationData;
  const AFrame: real);
begin
  if FNumKeys > 0 then
    DataTransf.ModelMatrix := MatrixMultiply(DataTransf.ModelMatrix,
      CreateScaleMatrix(InterpolateValue(FScale, AFrame)));
end;

procedure TgxFile3DSScaleAnimationKeys.Assign(Source: TPersistent);
var
  I: integer;
begin
  inherited;

  SetLength(FScale, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FScale[I] := (Source as TgxFile3DSScaleAnimationKeys).Fscale[I];
end;

procedure TgxFile3DSScaleAnimationKeys.WriteToFiler(Writer: TgxVirtualWriter);
begin
  inherited;

  if FNumKeys > 0 then
    Writer.Write(FScale[0], FNumKeys * SizeOf(TPoint3DS));
end;

procedure TgxFile3DSScaleAnimationKeys.ReadFromFiler(Reader: TgxVirtualReader);
begin
  inherited;

  SetLength(FScale, FNumKeys);
  if FNumKeys > 0 then
    Reader.Read(FScale[0], FNumKeys * SizeOf(TPoint3DS));
end;

procedure TgxFile3DSRotationAnimationKeys.LoadData(const ANumKeys: integer;
  const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: integer;
  Rot: PKFRotKeyList;
  AffVect : TAffineVector;
begin
  inherited;

  SetLength(FRot, FNumKeys);
  Rot := PKFRotKeyList(AData);
  for I := 0 to FNumKeys - 1 do
  begin
    // The initial values do not contain any turns, that's why we have to make one.
    if (Rot[I].X = 0) and (Rot[I].Y = 0) and (Rot[I].Z = 0) then
      Rot[I].X := 1;

    // One quartalion can't describe a big angle (>180), that's why we have to subtract it from 2*pi
    if Rot[I].Angle > pi then
    begin
      Rot[I].Angle := 2 * pi - Rot[I].Angle;
      Rot[I].X := -Rot[I].X;
      Rot[I].Y := -Rot[I].Y;
      Rot[I].Z := -Rot[I].Z;
    end;
    FRot[I] := Rot[I];

    if vFile3DS_FixDefaultUpAxisY then
    begin
      AffVect.X := FRot[I].X;
      AffVect.Y := FRot[I].Y;
      AffVect.Z := FRot[I].Z;

      AffVect := VectorRotateAroundX(AffVect, cGLFILE3DS_FIXDEFAULTUPAXISY_ROTATIONVALUE);

      FRot[I].X := AffVect.X;
      FRot[I].Y := AffVect.Y;
      FRot[I].Z := AffVect.Z;
    end;
  end;
end;

procedure TgxFile3DSRotationAnimationKeys.Apply(var DataTransf: TgxFile3DSAnimationData; const AFrame: real);
begin
  if FNumKeys > 0 then
    DataTransf.ModelMatrix := MatrixMultiply(DataTransf.ModelMatrix,
      InterpolateValue(FRot, AFrame));
end;

procedure TgxFile3DSRotationAnimationKeys.Assign(Source: TPersistent);
var
  I: integer;
begin
  inherited;
  SetLength(FRot, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FRot[I] := (Source as TgxFile3DSRotationAnimationKeys).FRot[I];
end;

procedure TgxFile3DSRotationAnimationKeys.WriteToFiler(Writer: TgxVirtualWriter);
begin
  inherited;
  if FNumKeys > 0 then
    Writer.Write(FRot[0], FNumKeys * SizeOf(TKFRotKey3DS));
end;

procedure TgxFile3DSRotationAnimationKeys.ReadFromFiler(Reader: TgxVirtualReader);
begin
  inherited;
  SetLength(FRot, FNumKeys);
  if FNumKeys > 0 then
    Reader.Read(FRot[0], FNumKeys * SizeOf(TKFRotKey3DS));
end;

procedure TgxFile3DSPositionAnimationKeys.LoadData(const ANumKeys: integer;
  const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: integer;
begin
  inherited;
  SetLength(FPos, FNumKeys);
  for I := 0 to FNumKeys - 1 do
  begin
    FPos[I] := TAffineVector(PPointList(AData)[I]);
    if vFile3DS_FixDefaultUpAxisY then
    begin
      FPos[I] := VectorRotateAroundX(FPos[I], cGLFILE3DS_FIXDEFAULTUPAXISY_ROTATIONVALUE);
    end;
  end;
end;

procedure TgxFile3DSPositionAnimationKeys.Apply(var DataTransf: TgxFile3DSAnimationData;
  const AFrame: real);
begin
  if FNumKeys > 0 then
    DataTransf.ModelMatrix.W :=
      VectorAdd(DataTransf.ModelMatrix.W, VectorMake(InterpolateValue(FPos, AFrame)));
end;

procedure TgxFile3DSPositionAnimationKeys.Assign(Source: TPersistent);
var
  I: integer;
begin
  inherited;
  SetLength(FPos, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FPos[I] := (Source as TgxFile3DSPositionAnimationKeys).FPos[I];
end;

procedure TgxFile3DSPositionAnimationKeys.WriteToFiler(Writer: TgxVirtualWriter);
begin
  inherited;
  if FNumKeys > 0 then
    Writer.Write(FPos[0], FNumKeys * SizeOf(TPoint3DS));
end;

procedure TgxFile3DSPositionAnimationKeys.ReadFromFiler(Reader: TgxVirtualReader);
begin
  inherited;

  SetLength(FPos, FNumKeys);
  if FNumKeys > 0 then
    Reader.Read(FPos[0], FNumKeys * SizeOf(TPoint3DS));
end;

procedure TgxFile3DSColorAnimationKeys.LoadData(const ANumKeys: integer;
  const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: integer;
begin
  inherited;
  SetLength(FCol, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FCol[I] := TaffineVector(PFColorList(AData)[I]);
end;

procedure TgxFile3DSColorAnimationKeys.Apply(var DataTransf: TgxFile3DSAnimationData;
  const AFrame: real);
begin
  if FNumKeys > 0 then
    DataTransf.Color := VectorAdd(DataTransf.Color,
      VectorMake(InterpolateValue(FCol, AFrame)));
end;

procedure TgxFile3DSColorAnimationKeys.Assign(Source: TPersistent);
var
  I: integer;
begin
  inherited;
  SetLength(FCol, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FCol[I] := (Source as TgxFile3DSColorAnimationKeys).FCol[I];
end;

procedure TgxFile3DSColorAnimationKeys.WriteToFiler(Writer: TgxVirtualWriter);
begin
  inherited;
  if FNumKeys > 0 then
    Writer.Write(FCol[0], FNumKeys * SizeOf(TFColor3DS));
end;

procedure TgxFile3DSColorAnimationKeys.ReadFromFiler(Reader: TgxVirtualReader);
begin
  inherited;

  SetLength(FCol, FNumKeys);
  if FNumKeys > 0 then
    Reader.Read(FCol[0], FNumKeys * SizeOf(TFColor3DS));
end;

procedure TTgxFile3DSPositionAnimationKeys.LoadData(const ANumKeys: integer;
  const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: integer;
begin
  inherited;
  SetLength(FTPos, FNumKeys);
  for I := 0 to FNumKeys - 1 do
  begin
    FTPos[I] := TaffineVector(PPointList(AData)[I]);
    if vFile3DS_FixDefaultUpAxisY then
    begin
      FTPos[I] := VectorRotateAroundX(FTPos[I], cGLFILE3DS_FIXDEFAULTUPAXISY_ROTATIONVALUE);
    end;
  end;
end;

procedure TTgxFile3DSPositionAnimationKeys.Apply(var DataTransf: TgxFile3DSAnimationData;
  const AFrame: real);
begin
  if FNumKeys > 0 then
    DataTransf.TargetPos := VectorAdd(DataTransf.TargetPos,
      InterpolateValue(FTPos, AFrame));
end;

procedure TTgxFile3DSPositionAnimationKeys.Assign(Source: TPersistent);
var
  I: integer;
begin
  inherited;
  SetLength(FTPos, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FTPos[I] := (Source as TTgxFile3DSPositionAnimationKeys).FTPos[I];
end;

procedure TTgxFile3DSPositionAnimationKeys.WriteToFiler(Writer: TgxVirtualWriter);
begin
  inherited;
  if FNumKeys > 0 then
    Writer.Write(FTPos[0], FNumKeys * SizeOf(TPoint3DS));
end;

procedure TTgxFile3DSPositionAnimationKeys.ReadFromFiler(Reader: TgxVirtualReader);
begin
  inherited;

  SetLength(FTPos, FNumKeys);
  if FNumKeys > 0 then
    Reader.Read(FTPos[0], FNumKeys * SizeOf(TPoint3DS));
end;

procedure TgxFile3DSSpotLightCutOffAnimationKeys.LoadData(const ANumKeys: integer;
  const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: integer;
begin
  inherited;
  SetLength(FFall, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FFall[I] := PSingleList(AData)[I];
end;

procedure TgxFile3DSSpotLightCutOffAnimationKeys.Apply(
  var DataTransf: TgxFile3DSAnimationData; const AFrame: real);
begin
  if FNumKeys > 0 then
    DataTransf.SpotLightCutOff :=
      DataTransf.SpotLightCutOff + InterpolateValue(FFall, AFrame);
end;

procedure TgxFile3DSSpotLightCutOffAnimationKeys.Assign(Source: TPersistent);
var
  I: integer;
begin
  inherited;
  SetLength(FFall, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FFall[I] := (Source as TgxFile3DSSpotLightCutOffAnimationKeys).FFall[I];
end;

procedure TgxFile3DSSpotLightCutOffAnimationKeys.WriteToFiler(Writer: TgxVirtualWriter);
begin
  inherited;
  if FNumKeys > 0 then
    Writer.Write(FFall[0], FNumKeys * SizeOf(single));
end;

procedure TgxFile3DSSpotLightCutOffAnimationKeys.ReadFromFiler(Reader: TgxVirtualReader);
begin
  inherited;

  SetLength(FFall, FNumKeys);
  if FNumKeys > 0 then
    Reader.Read(FFall[0], FNumKeys * SizeOf(single));
end;

procedure TgxFile3DSLightHotSpotAnimationKeys.LoadData(const ANumKeys: integer;
  const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: integer;
begin
  inherited;
  SetLength(FHot, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FHot[I] := PSingleList(AData)[I];
end;

procedure TgxFile3DSLightHotSpotAnimationKeys.Apply(
  var DataTransf: TgxFile3DSAnimationData; const AFrame: real);
begin
  if FNumKeys > 0 then
    DataTransf.HotSpot := DataTransf.HotSpot + InterpolateValue(FHot, AFrame);
end;

procedure TgxFile3DSLightHotSpotAnimationKeys.Assign(Source: TPersistent);
var
  I: integer;
begin
  inherited;
  SetLength(FHot, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FHot[I] := (Source as TgxFile3DSLightHotSpotAnimationKeys).FHot[I];
end;

procedure TgxFile3DSLightHotSpotAnimationKeys.WriteToFiler(Writer: TgxVirtualWriter);
begin
  inherited;
  if FNumKeys > 0 then
    Writer.Write(FHot[0], FNumKeys * SizeOf(single));
end;

procedure TgxFile3DSLightHotSpotAnimationKeys.ReadFromFiler(Reader: TgxVirtualReader);
begin
  inherited;
  SetLength(FHot, FNumKeys);
  if FNumKeys > 0 then
    Reader.Read(FHot[0], FNumKeys * SizeOf(single));
end;

procedure TgxFile3DSRollAnimationKeys.LoadData(const ANumKeys: integer;
  const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: integer;
begin
  inherited;
  SetLength(FRoll, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FRoll[I] := PSingleList(AData)[I];
end;

procedure TgxFile3DSRollAnimationKeys.Apply(var DataTransf: TgxFile3DSAnimationData;
  const AFrame: real);
begin
  if FNumKeys > 0 then
    DataTransf.Roll := DataTransf.Roll + InterpolateValue(FRoll, AFrame);
end;

procedure TgxFile3DSRollAnimationKeys.Assign(Source: TPersistent);
var
  I: integer;
begin
  inherited;
  SetLength(FRoll, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FRoll[I] := (Source as TgxFile3DSRollAnimationKeys).FRoll[I];
end;

procedure TgxFile3DSRollAnimationKeys.WriteToFiler(Writer: TgxVirtualWriter);
begin
  inherited;
  if FNumKeys > 0 then
    Writer.Write(FRoll[0], FNumKeys * SizeOf(single));
end;

procedure TgxFile3DSRollAnimationKeys.ReadFromFiler(Reader: TgxVirtualReader);
begin
  inherited;
  SetLength(FRoll, FNumKeys);
  if FNumKeys > 0 then
    Reader.Read(FRoll[0], FNumKeys * SizeOf(single));
end;

procedure TgxFile3DSAnimationKeyList.AddKeys(const AItem: TgxFile3DSAnimationKeys);
var
  ind: integer;
begin
  if AItem = nil then
    Exit;
  ind := Length(FAnimKeysList);
  SetLength(FAnimKeysList, ind + 1);
  FAnimKeysList[ind] := AItem;
end;

procedure TgxFile3DSAnimationKeyList.ApplyAnimKeys(
  var DataTransf: TgxFile3DSAnimationData; const AFrame: real);
var
  I: integer;
begin
  for I := 0 to Length(FAnimKeysList) - 1 do
    FAnimKeysList[I].Apply(DataTransf, AFrame);
end;

procedure TgxFile3DSAnimationKeyList.ClearKeys;
var
  I: integer;
begin
  for I := 0 to Length(FAnimKeysList) - 1 do
    FAnimKeysList[I].Free;
  SetLength(FAnimKeysList, 0);
end;

procedure TgxFile3DSAnimationKeyList.Assign(Source: TPersistent);
var
  I: integer;
  item: TgxFile3DSAnimationKeys;
begin
  if Source is TgxFile3DSAnimationKeyList then
  begin
    ClearKeys;
    for I := 0 to Length(TgxFile3DSAnimationKeyList(Source).FAnimKeysList) - 1 do
    begin
      item := (TgxFile3DSAnimationKeyList(Source).FAnimKeysList[I].ClassType.Create as
        TgxFile3DSAnimationKeys);
      item.Assign(TgxFile3DSAnimationKeyList(Source).FAnimKeysList[I]);
      AddKeys(item);
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TgxFile3DSAnimationKeyList.WriteToFiler(Writer: TgxVirtualWriter);
var
  I: integer;
  Val: TgxFile3DSAnimKeysClassType;
begin
  Writer.WriteInteger(Length(FAnimKeysList));
  for I := 0 to Length(FAnimKeysList) - 1 do
  begin
    Val := ClassToAnimKeysClassType(FAnimKeysList[I].ClassType);
    Writer.Write(Val, SizeOf(Val));
    FAnimKeysList[I].WriteToFiler(Writer);
  end;
end;

procedure TgxFile3DSAnimationKeyList.ReadFromFiler(Reader: TgxVirtualReader);
var
  I, cnt: integer;
  Val: TgxFile3DSAnimKeysClassType;
begin
  ClearKeys;
  cnt := Reader.ReadInteger;
  SetLength(FAnimKeysList, cnt);
  for I := 0 to Length(FAnimKeysList) - 1 do
  begin
    Reader.Read(Val, SizeOf(Val));
    FAnimKeysList[I] := AnimKeysClassTypeToClass(Val).Create as TgxFile3DSAnimationKeys;
    FAnimKeysList[I].ReadFromFiler(Reader);
  end;
end;

destructor TgxFile3DSAnimationKeyList.Destroy;
begin
  ClearKeys;
  inherited Destroy;
end;

constructor TgxFile3DSDummyObject.Create;
begin
  inherited;
  FAnimList := TgxFile3DSAnimationKeyList.Create;
  FRefTranf.ModelMatrix := IdentityHmgMatrix;
  FAnimTransf.ModelMatrix := IdentityHmgMatrix;
  FStatic := False;
end;

procedure TgxFile3DSDummyObject.LoadAnimation(const AData: Pointer);
begin
  FAnimList.ClearKeys;
  FAnimData := AData;
end;

procedure TgxFile3DSDummyObject.SetFrame(const AFrame: real);
var
  p: TgxFile3DSDummyObject;
  lAnimationData: TgxFile3DSAnimationData;
begin
  if not vFile3DS_EnableAnimation then
    Exit;

  if (FParentName <> '') then
  begin
    FParent := Owner.FindMeshByName(string(FParentName)) as TgxFile3DSDummyObject;
    FParentName := '';
  end;
  lAnimationData := FRefTranf;
  p := self;
  while p <> nil do
  begin
    p.FAnimList.ApplyAnimKeys(lAnimationData, AFrame);
    p := p.FParent;
  end;
  FAnimTransf := lAnimationData;
end;

procedure TgxFile3DSDummyObject.MorphTo(morphTargetIndex: integer);
begin
  SetFrame(morphTargetIndex);
end;

procedure TgxFile3DSDummyObject.Lerp(morphTargetIndex1, morphTargetIndex2: integer;
  lerpFactor: single);
begin
  if (Owner.Owner is TgxActor) and ((Owner.Owner as TgxActor).AnimationMode in
    [aamBounceBackward, aamLoopBackward]) then
    SetFrame(morphTargetIndex1 - lerpFactor)
  else
    SetFrame(morphTargetIndex1 + lerpFactor);
end;

procedure TgxFile3DSDummyObject.Assign(Source: TPersistent);
begin
  inherited; // Assign all published properties here.
  if Source is TgxFile3DSDummyObject then
  begin
    FRefTranf := (Source as TgxFile3DSDummyObject).FRefTranf;
    FParent := (Source as TgxFile3DSDummyObject).FParent;
    FAnimList.Assign((Source as TgxFile3DSDummyObject).FAnimList);
    SetFrame(0);
  end;
end;

procedure TgxFile3DSDummyObject.GetExtents(out min, max: TAffineVector);
begin
  inherited GetExtents(min, max);
  if not FStatic then
  begin
    if not IsInfinite(min.X) then
      min := VectorTransform(min, FAnimTransf.ModelMatrix);
    if not IsInfinite(max.X) then
      max := VectorTransform(max, FAnimTransf.ModelMatrix);
  end;
end;

function TgxFile3DSDummyObject.ExtractTriangles(texCoords, normals: TgxAffineVectorList):
TgxAffineVectorList;
var
  I: integer;
begin
  Result := inherited ExtractTriangles(texCoords, normals);

  if not FStatic then
  begin
    if (Result.Count <> 0) and not MatrixEquals(FAnimTransf.ModelMatrix,
      IdentityHmgMatrix) then
      for I := 0 to Result.Count - 1 do
        Result[I] := VectorTransform(Result[I], FAnimTransf.ModelMatrix);
  end;
end;

procedure TgxFile3DSDummyObject.WriteToFiler(Writer: TgxVirtualWriter);
var
  str: string;
begin
  inherited;

  Writer.Write(FRefTranf, SizeOf(FRefTranf));
  if FParent <> nil then
    str := Copy(FParent.Name, 1, 32)
  else
    str := 'nil';
  Writer.WriteString(str);
  FAnimList.WriteToFiler(Writer);
end;

procedure TgxFile3DSDummyObject.ReadFromFiler(Reader: TgxVirtualReader);
begin
  inherited;

  Reader.Read(FRefTranf, SizeOf(FRefTranf));
  FParentName := String64(Copy(Reader.ReadString, 1, 64));
  if FParentName = 'nil' then
    FParentName := '';
  FAnimList.ReadFromFiler(Reader);
end;

destructor TgxFile3DSDummyObject.Destroy;
begin
  FAnimList.Free;

  inherited;
end;

procedure TgxFile3DSMeshObject.LoadAnimation(const AData: Pointer);
var
  aScale: TgxFile3DSScaleAnimationKeys;
  aRot: TgxFile3DSRotationAnimationKeys;
  aPos: TgxFile3DSPositionAnimationKeys;
  Mat : TMatrix4f;
  RotMat : TMatrix4f;
  AffVect : TAffineVector;
begin
  inherited;

  with PKFMesh3DS(AData)^, FAnimList do
  begin
    aScale := TgxFile3DSScaleAnimationKeys.Create;
    aScale.LoadData(NSKeys, SKeys, Scale);
    AddKeys(aScale);

    aRot := TgxFile3DSRotationAnimationKeys.Create;
    aRot.LoadData(NRKeys, RKeys, Rot);
    AddKeys(aRot);

    aPos := TgxFile3DSPositionAnimationKeys.Create;
    aPos.LoadData(NPKeys, PKeys, Pos);
    AddKeys(aPos);

    if ParentStr <> '' then
      FParent := TgxFile3DSDummyObject(Owner.FindMeshByName(string(ParentStr)));

    with FRefTranf do
    begin
      if vFile3DS_FixDefaultUpAxisY then
      begin
        RotMat := CreateRotationMatrixX(cGLFILE3DS_FIXDEFAULTUPAXISY_ROTATIONVALUE);
        InvertMatrix(RotMat);

        Mat := ModelMatrix;
        ModelMatrix := MatrixMultiply(Mat, RotMat);

        AffVect.X := Pivot.X;
        AffVect.Y := Pivot.Y;
        AffVect.Z := Pivot.Z;

        AffVect := VectorRotateAroundX(AffVect, cGLFILE3DS_FIXDEFAULTUPAXISY_ROTATIONVALUE);

        Pivot.X := AffVect.X;
        Pivot.Y := AffVect.Y;
        Pivot.Z := AffVect.Z;
      end;

      ModelMatrix.W.X := ModelMatrix.W.X - Pivot.X;
      ModelMatrix.W.Y := ModelMatrix.W.Y - Pivot.Y;
      ModelMatrix.W.Z := ModelMatrix.W.Z - Pivot.Z;

    end;
  end;

  if vFile3DS_LoadedStaticFrame = -1 then
    SetFrame(CGLFILE3DS_DEFAULT_FRAME)
  else
    SetFrame(vFile3DS_LoadedStaticFrame);
end;

procedure TgxFile3DSMeshObject.BuildList(var ARci: TgxRenderContextInfo);
begin
  glPushMatrix;
  if not FStatic then
    glMultMatrixf(@FAnimTransf.ModelMatrix);
  inherited;
  glPopMatrix;
end;

constructor TgxFile3DSOmniLightObject.Create;
begin
  inherited;
  FLightSrc := TgxFile3DSLight.Create(nil);
end;

procedure TgxFile3DSOmniLightObject.LoadData(const AOwner: TgxBaseMesh;
  const AData: PLight3DS);
begin
  FLightSrc.Parent := AOwner;
  FLightSrc.LightStyle := lsOmni;
  FLightSrc.Name := string(AData.NameStr);
  Name := string(AData.NameStr);
  FLightSrc.Position.SetPoint(PAffineVector(@AData.Pos)^);
  FLightSrc.Diffuse.Color := VectorMake(AData.Color.R, AData.Color.G, AData.Color.B);
  FLightSrc.Specular.Color := VectorMake(AData.Color.R, AData.Color.G, AData.Color.B);
  FLightSrc.Diffuse.Color := VectorScale(FLightSrc.Diffuse.Color, AData.Multiplier);
  //надо потестить
  FLightSrc.Shining := not AData.DLOff;
  FLightSrc.Multipler := AData.Multiplier;
  FLightSrc.ConstAttenuation := 1;
  FLightSrc.LinearAttenuation := 0;
  FLightSrc.QuadraticAttenuation := 0;
end;

procedure TgxFile3DSOmniLightObject.LoadAnimation(const AData: Pointer);
var
  aPos: TgxFile3DSPositionAnimationKeys;
  aCol: TgxFile3DSColorAnimationKeys;
begin
  inherited;

  with PKFOmni3DS(AData)^, FAnimList do
  begin
    aPos := TgxFile3DSPositionAnimationKeys.Create;
    aPos.LoadData(NPKeys, PKeys, Pos);
    AddKeys(aPos);

    aCol := TgxFile3DSColorAnimationKeys.Create;
    aCol.LoadData(NCKeys, CKeys, Color);
    AddKeys(aCol);

    if Parent <> '' then
      FParent := TgxFile3DSDummyObject(Owner.FindMeshByName(string(Parent)));
  end;

  if vFile3DS_LoadedStaticFrame = -1 then
    SetFrame(CGLFILE3DS_DEFAULT_FRAME)
  else
    SetFrame(vFile3DS_LoadedStaticFrame);
end;

procedure TgxFile3DSOmniLightObject.SetFrame(const AFrame: real);
var
  obj: TComponent;
begin
  if FLightSrcName <> '' then
  begin
    obj := Owner.Owner.FindChild(string(FLightSrcName), True);
    if obj is TgxFile3DSLight then
    begin
      FLightSrc.Free;
      FLightSrc := obj as TgxFile3DSLight;
    end;
    FLightSrcName := '';
  end;

  inherited;
  FLightSrc.Position.SetPoint(FAnimTransf.ModelMatrix.W);
  FLightSrc.Diffuse.Color := FAnimTransf.Color;
end;

procedure TgxFile3DSOmniLightObject.Assign(Source: TPersistent);
begin
  inherited;

  if Source is TgxFile3DSOmniLightObject then
    FlightSrc.Assign((Source as TgxFile3DSOmniLightObject).FLightSrc);
end;

procedure TgxFile3DSOmniLightObject.WriteToFiler(Writer: TgxVirtualWriter);
var
  str: string;
begin
  inherited;

  if FLightSrc.Name = '' then
    str := 'nil'
  else
    str := Copy(FLightSrc.Name, 1, 64);
  Writer.WriteString(str);
end;

procedure TgxFile3DSOmniLightObject.ReadFromFiler(Reader: TgxVirtualReader);
begin
  inherited;

  FLightSrcName := String64(Copy(Reader.ReadString, 1, 64));
  if FLightSrcName = 'nil' then
    FLightSrcName := '';
end;

destructor TgxFile3DSOmniLightObject.Destroy;
begin
  FLightSrc.Free;

  inherited;
end;

procedure TgxFile3DSSpotLightObject.LoadData(const AOwner: TgxBaseMesh;
  const AData: PLight3DS);
begin
  inherited;

  FLightSrc.LightStyle := lsSpot;
  FLightSrc.SpotTargetPos.SetPoint(TAffineVector(AData.Spot.Target));
  FLightSrc.SpotCutOff := AData.Spot.FallOff / 2;
  FLightSrc.HotSpot := AData.Spot.Hotspot / 2;
end;

procedure TgxFile3DSSpotLightObject.LoadAnimation(const AData: Pointer);
var
  aTPos: TTgxFile3DSPositionAnimationKeys;
  aFall: TgxFile3DSSpotLightCutOffAnimationKeys;
  aHot: TgxFile3DSLightHotSpotAnimationKeys;
begin
  inherited;

  with PKFSpot3DS(AData)^, FAnimList do
  begin
    aTPos := TTgxFile3DSPositionAnimationKeys.Create;
    aTPos.LoadData(NTKeys, TKeys, TPos);
    AddKeys(aTPos);

    aFall := TgxFile3DSSpotLightCutOffAnimationKeys.Create;
    aFall.LoadData(NFKeys, FKeys, Fall);
    AddKeys(aFall);

    aHot := TgxFile3DSLightHotSpotAnimationKeys.Create;
    aHot.LoadData(NHKeys, HKeys, Hot);
    AddKeys(aHot);

    if Parent <> '' then
      FParent := TgxFile3DSDummyObject(Owner.FindMeshByName(string(Parent)));
  end;

  if vFile3DS_LoadedStaticFrame = -1 then
    SetFrame(CGLFILE3DS_DEFAULT_FRAME)
  else
    SetFrame(vFile3DS_LoadedStaticFrame);
end;

procedure TgxFile3DSSpotLightObject.SetFrame(const AFrame: real);
begin
  inherited;
  FLightSrc.SpotTargetPos.SetPoint(FAnimTransf.TargetPos);
  FLightSrc.SpotCutOff := FAnimTransf.SpotLightCutOff / 2;
  FLightSrc.HotSpot := FAnimTransf.HotSpot / 2;
end;

constructor TgxFile3DSCameraObject.Create;
begin
  inherited;
  FCameraSrc := TgxFile3DSCamera.Create(nil);
  FTargetObj := TgxDummyCube.Create(nil);
  FCameraSrc.TargetObject := FTargetObj;
end;

procedure TgxFile3DSCameraObject.LoadData(Owner: TgxBaseMesh; AData: PCamera3DS);
begin
  FCameraSrc.Parent := Owner;
  FTargetObj.Parent := Owner;
  FCameraSrc.Name := string(AData.NameStr);
  Name := string(AData.NameStr);
  FCameraSrc.Position.AsAffineVector := TAffineVector(AData.Position);
  FTargetObj.Position.SetPoint(TAffineVector(AData.Target));
  FCameraSrc.RollAngle := AData.Roll;

  FCameraSrc.FocalLength := AData.FOV;
end;

procedure TgxFile3DSCameraObject.LoadAnimation(const AData: Pointer);
var
  aPos: TgxFile3DSPositionAnimationKeys;
  aRoll: TgxFile3DSRollAnimationKeys;
  aTPos: TTgxFile3DSPositionAnimationKeys;
begin
  inherited;

  with PKFCamera3DS(AData)^, FAnimList do
  begin
    aPos := TgxFile3DSPositionAnimationKeys.Create;
    aPos.LoadData(NPKeys, PKeys, Pos);
    AddKeys(aPos);

    aRoll := TgxFile3DSRollAnimationKeys.Create;
    aRoll.LoadData(NRKeys, RKeys, Roll);
    AddKeys(aRoll);

    aTPos := TTgxFile3DSPositionAnimationKeys.Create;
    aTPos.LoadData(NTKeys, TKeys, TPos);
    AddKeys(aTPos);
  end;

  if vFile3DS_LoadedStaticFrame = -1 then
    SetFrame(CGLFILE3DS_DEFAULT_FRAME)
  else
    SetFrame(vFile3DS_LoadedStaticFrame);
end;

procedure TgxFile3DSCameraObject.SetFrame(const AFrame: real);
var
  obj: TComponent;
begin
  inherited;

  if FCameraSrcName <> '' then
  begin
    obj := Owner.Owner.FindChild(string(FCameraSrcName), True);
    if obj is TgxFile3DSCamera then
    begin
      FCameraSrc.Free;
      FCameraSrc := obj as TgxFile3DSCamera;
    end;
    FCameraSrcName := '';
  end;

  FCameraSrc.Position.SetPoint(FAnimTransf.ModelMatrix.W);
  FCameraSrc.RollAngle := FAnimTransf.Roll;
  FTargetObj.Position.SetPoint(FAnimTransf.TargetPos);
end;

procedure TgxFile3DSCameraObject.WriteToFiler(Writer: TgxVirtualWriter);
var
  str: string;
begin
  inherited;

  if FCameraSrc.Name = '' then
    str := 'nil'
  else
    str := Copy(FCameraSrc.Name, 1, 64);
  Writer.WriteString(str);
end;

procedure TgxFile3DSCameraObject.ReadFromFiler(Reader: TgxVirtualReader);
begin
  inherited;

  FCameraSrcName := String64(Copy(Reader.ReadString, 1, 64));
  if FCameraSrcName = 'nil' then
    FCameraSrcName := '';
end;

destructor TgxFile3DSCameraObject.Destroy;
begin
  FCameraSrc.Free;
  FTargetObj.Free;
  inherited;
end;



// ------------------
// ------------------ TGL3DSVectorFile ------------------
// ------------------
class function Tgx3DSVectorFile.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead];
end;

procedure Tgx3DSVectorFile.LoadFromStream(aStream: TStream);
type
  TSmoothIndexEntry = array[0..31] of cardinal;
  PSmoothIndexArray = ^TSmoothIndexArray;
  TSmoothIndexArray = array[0..MaxInt shr 8] of TSmoothIndexEntry;
var
  Marker: PByteArray;
  CurrentVertexCount: integer;
  SmoothIndices: PSmoothIndexArray;
  mesh: TgxFile3DSMeshObject;
  hasLightmap: boolean;


  //--------------- local functions -------------------------------------------

  function GetOrAllocateMaterial(materials: TMaterialList; const Name: string): string;
  var
    material: PMaterial3DS;
    specColor: TVector4f;
    matLib: TgxMaterialLibrary;
    libMat, SecondMaterial: TgxLibMaterial;
  begin
    material := Materials.MaterialByName[Name];
    Assert(Assigned(material));
    if GetOwner is TgxBaseMesh then
    begin
      matLib := TgxBaseMesh(GetOwner).MaterialLibrary;
      if Assigned(matLib) then
      begin
        Result := Name;
        libMat := matLib.Materials.GetLibMaterialByName(Name);
        if not Assigned(libMat) then
        begin
          libMat := matLib.Materials.Add;
          libMat.Name := Name;

          with libMat.Material.FrontProperties do
          begin
            Ambient.Color := VectorMake(material.Ambient.R, material.Ambient.G,
              material.Ambient.B, 1);
            // Material transparency can be stored as a positive or negative value.
            Diffuse.Color := VectorMake(material.Diffuse.R, material.Diffuse.G,
              material.Diffuse.B, 1 - Abs(material.Transparency));
            specColor := VectorMake(material.Specular.R, material.Specular.G,
              material.Specular.B, 1);
            Specular.Color := VectorScale(specColor, material.ShinStrength);
            Shininess := MaxInteger(0, integer(round((material.Shininess) * 128)));
            if material.Transparency <> 0 then
              libMat.Material.BlendingMode := bmTransparency;
          end;
          if Trim(string(material.Texture.Map.NameStr)) <> '' then
            try
              if vFile3DS_UseTextureEx then
                with libMat.Material.TextureEx.Add do
                begin
                  Texture.Image.LoadFromFile(string(material.Texture.Map.NameStr));
                  Texture.Disabled := False;
                  Texture.TextureMode := tmModulate;
                  TextureIndex := 0;
                  with material.Texture.Map do
                  begin
                    TextureScale.SetPoint(UScale, VScale, 0);
                    TextureOffset.SetPoint((1 - frac(UOffset)) *
                      UScale, (frac(VOffset)) * VScale, 0);
                  end;
                end
              else
                with libMat.Material.Texture do
                begin
                  Image.LoadFromFile(string(material.Texture.Map.NameStr));
                  Disabled := False;
                  TextureMode := tmModulate;
                end

            except
              on E: ETexture do
              begin
                if not Owner.IgnoreMissingTextures then
                  raise EGLFile3DS.CreateFmt(str3DSMapNotFound,['diffuse', e.Message, matLib.TexturePaths]);
              end
              else
                raise;
            end;

          if Trim(string(material.Opacity.Map.NameStr)) <> '' then
            try
              if vFile3DS_UseTextureEx then
                with libMat.Material.TextureEx.Add do
                begin
                  libMat.Material.BlendingMode := bmTransparency;
                  Texture.ImageAlpha := tiaAlphaFromIntensity;
                  Texture.TextureMode := tmModulate;
                  Texture.Image.LoadFromFile(string(material.Opacity.Map.NameStr));
                  Texture.Disabled := False;
                  TextureIndex := 1;
                  with material.Opacity.Map do
                  begin
                    TextureScale.SetPoint(UScale, VScale, 0);
                    TextureOffset.SetPoint((1 - frac(UOffset)) *
                      UScale, (frac(VOffset)) * VScale, 0);
                  end;
                end
              else
                with libMat.Material.Texture do
                begin
                  SecondMaterial := matLib.Materials.Add;
                  SecondMaterial.Material.Texture.Image.LoadFromFile(string(
                    material.Opacity.Map.NameStr));
                  SecondMaterial.Material.Texture.Disabled := False;
                  SecondMaterial.Material.Texture.ImageAlpha := tiaAlphaFromIntensity;
                  SecondMaterial.Material.Texture.TextureMode := tmModulate;
                  SecondMaterial.Name := string(material.Opacity.Map.NameStr);
                  LibMat.Texture2Name := SecondMaterial.Name;
                  Disabled := False;
                end;

            except
              on E: ETexture do
              begin
                if not Owner.IgnoreMissingTextures then
                  raise EGLFile3DS.CreateFmt(str3DSMapNotFound,['opacity', e.Message, matLib.TexturePaths]);
              end
              else
                raise;
            end;
          if Trim(string(material.Bump.Map.NameStr)) <> '' then
            try
              if vFile3DS_UseTextureEx then
                with libMat.Material.TextureEx.Add do
                begin
                  Texture.Image.LoadFromFile(string(material.Bump.Map.NameStr));
                  Texture.Disabled := False;
                  Texture.TextureMode := tmModulate;
                  // You need a hight map for this parameter (like in 3d Max).
                  // Texture.TextureFormat := tfNormalMap;
                  TextureIndex := 2;
                  with material.Bump.Map do
                  begin
                    TextureScale.SetPoint(UScale, VScale, 0);
                    TextureOffset.SetPoint((1 - frac(UOffset)) *
                      UScale, (frac(VOffset)) * VScale, 0);
                  end;
                end
              else
                with libMat.Material.Texture do
                begin
                  SecondMaterial := matLib.Materials.Add;
                  SecondMaterial.Material.Texture.Image.LoadFromFile(string(
                    material.Bump.Map.NameStr));
                  SecondMaterial.Material.Texture.Disabled := False;
                  SecondMaterial.Material.Texture.ImageAlpha := tiaAlphaFromIntensity;
                  SecondMaterial.Material.Texture.TextureMode := tmModulate;
                  SecondMaterial.Name := string(material.Opacity.Map.NameStr);
                  Disabled := False;
                end;

            except
              on E: ETexture do
              begin
                if not Owner.IgnoreMissingTextures then
                  raise EGLFile3DS.CreateFmt(str3DSMapNotFound,['bump', e.Message, matLib.TexturePaths]);
              end
              else
                raise;
            end;
        end;
      end
      else
        Result := '';
    end
    else
      Result := '';
  end;

  function GetOrAllocateLightMap(materials: TMaterialList; const Name: string): integer;
  var
    material: PMaterial3DS;
    matLib: TgxMaterialLibrary;
    libMat: TgxLibMaterial;
  begin
    Result := -1;
    material := Materials.MaterialByName[Name];
    Assert(Assigned(material));
    if GetOwner is TgxBaseMesh then
    begin
      matLib := TgxBaseMesh(GetOwner).LightmapLibrary;
      if Assigned(matLib) then
      begin
        if Trim(string(material.IllumMap.Map.NameStr)) <> '' then
        begin
          libMat := matLib.Materials.GetLibMaterialByName(string(
            material.IllumMap.Map.NameStr));
          if not Assigned(libMat) then
          begin
            libMat := matLib.Materials.Add;
            libMat.Name := string(material.IllumMap.Map.NameStr);
            try
              with libMat.Material.Texture do
              begin
                Image.LoadFromFile(string(material.IllumMap.Map.NameStr));
                Disabled := False;
                TextureMode := tmModulate;
              end;
            except
              on E: ETexture do
              begin
                if not Owner.IgnoreMissingTextures then
                  raise EGLFile3DS.CreateFmt(str3DSMapNotFound,['light', e.Message, matLib.TexturePaths]);
              end
              else
                raise;
            end;
          end;
          Result := libmat.Index;
          hasLightMap := True;
        end;
      end;
    end;
  end;

  //----------------------------------------------------------------------

  function InvertMeshMatrix(Objects: TObjectList; const Name: string): TMatrix4f;
    // constructs a 4x4 matrix from 3x4 local mesh matrix given by Name and
    // inverts it so it can be used for the keyframer stuff
  var
    I, Index: integer;
    boolY: boolean;
    m: TMatrix4f;
    v4: TVector4f;
    factor: single;
  begin
    with Objects do
    begin
      Index := -1;
      for I := 0 to MeshCount - 1 do
        if CompareText(string(Mesh[I].NameStr), Name) = 0 then
        begin
          Index := I;
          Break;
        end;

      if Index > -1 then
      begin
        with Mesh[Index]^ do
        begin
          Result.X.X := LocMatrix[0];
          Result.X.Y := LocMatrix[1];
          Result.X.Z := LocMatrix[2];
          Result.X.W := 0;
          Result.Y.X := LocMatrix[3];
          Result.Y.Y := LocMatrix[4];
          Result.Y.Z := LocMatrix[5];
          Result.Y.W := 0;
          Result.Z.X := LocMatrix[6];
          Result.Z.Y := LocMatrix[7];
          Result.Z.Z := LocMatrix[8];
          Result.Z.W := 0;
          Result.W.X := LocMatrix[9];
          Result.W.Y := LocMatrix[10];
          Result.W.Z := LocMatrix[11];
          Result.W.W := 1;
        end;
        InvertMatrix(Result);

        // If the matrix is not normalized, ie the third column is not equal to the vector product of the first two columns,
        // it means that it is necessary to turn to-pi around the axis Y.
        m := Result;
        v4 := m.W;
        factor := VectorLength(m.X);
        NormalizeMatrix(m);
        ScaleMatrix(m, factor);
        m.W := v4;

        v4 := VectorAbs(VectorSubtract(Result.Z, m.Z));
        boolY := (v4.X > abs(Result.Z.X)) and
                 (v4.Y > abs(Result.Z.Y)) and
                 (v4.Z > abs(Result.Z.Z));


        if boolY then
          Result := MatrixMultiply(Result, CreateRotationMatrix(AffineVectorMake(0, 1, 0), -pi));

      end
      else
        Result := IdentityHmgMatrix;
    end;
  end;

  //----------------------------------------------------------------------

{$IFDEF USE_NO_ASM}
  function IsVertexMarked(P: PByteArray; Index: word): boolean; inline;
    // tests the Index-th bit, returns True if set else False
  var
    mi: word;
  begin
    DivMod(index, 8, mi, index);
    Result := (((p^[mi] shr Index) and 1) = 1);
  end;

{$ELSE}
  function IsVertexMarked(P: Pointer; Index: integer): boolean; assembler;
           // tests the Index-th bit, returns True if set else False
  asm
           BT      [EAX], EDX
           SETC    AL
  end;
{$ENDIF}

  //---------------------------------------------------------------------------

{$IFDEF USE_NO_ASM}
  function MarkVertex(P: PByteArray; Index: word): boolean; inline;
    // sets the Index-th bit and return True if it was already set else False
  var
    mi: word;
  begin
    DivMod(index, 8, mi, index);
    Result := (((p^[mi] shr Index) and 1) = 1);
    if not (Result) then
      p^[mi] := p^[mi] or (1 shl index);
  end;
{$ELSE}
  function MarkVertex(P: Pointer; Index: integer): boolean; assembler;
           // sets the Index-th bit and return True if it was already set else False
  asm
           BTS     [EAX], EDX
           SETC    AL
  end;
{$ENDIF}

  //---------------------------------------------------------------------------
  // Stores new vertex index (NewIndex) into the smooth index array of vertex ThisIndex
  // using field SmoothingGroup, which must not be 0.
  // For each vertex in the vertex array (also for duplicated vertices) an array of 32 cardinals
  // is maintained (each for one possible smoothing group. If a vertex must be duplicated because
  // it has no smoothing group or a different one then the index of the newly created vertex is
  // stored in the SmoothIndices to avoid loosing the conjunction between not yet processed vertices
  // and duplicated vertices.
  // Note: Only one smoothing must be assigned per vertex. Some available models break this rule and
  //       have more than one group assigned to a face. To make the code fail safe the group ID
  //       is scanned for the lowest bit set.

{$IFDEF USE_ASM}
  procedure StoreSmoothIndex(ThisIndex, SmoothingGroup, NewIndex: cardinal; P: Pointer);
  asm
           PUSH    EBX
           BSF     EBX, EDX
           // determine smoothing group index (convert flag into an index)
           MOV     EDX, [P]                  // get address of index array
           SHL     EAX, 7                    // ThisIndex * SizeOf(TSmoothIndexEntry)
           ADD     EAX, EDX
           LEA     EDX, [4 * EBX + EAX]
           // Address of array + vertex index + smoothing group index
           MOV     [EDX], ECX
           POP     EBX
  end;
{$ELSE}
  procedure StoreSmoothIndex(ThisIndex, SmoothingGroup, NewIndex: cardinal;
    P: PSmoothIndexArray);
  var
    i: word;
  begin
    i := 0;
    while SmoothingGroup and (1 shl i) = 0 do
      Inc(i);
    p^[ThisIndex, i] := NewIndex;
  end;
{$ENDIF}

  //---------------------------------------------------------------------------

{$IFDEF USE_ASM}
  function GetSmoothIndex(ThisIndex, SmoothingGroup: cardinal; P: Pointer): integer;
           // Retrieves the vertex index for the given index and smoothing group.
           // This redirection is necessary because a vertex might have been duplicated.
  asm
           PUSH    EBX
           BSF     EBX, EDX                  // determine smoothing group index
           SHL     EAX, 7                    // ThisIndex * SizeOf(TSmoothIndexEntry)
           ADD     EAX, ECX
           LEA     ECX, [4 * EBX + EAX]
           // Address of array + vertex index + smoothing group index
           MOV     EAX, [ECX]
           POP     EBX
  end;
{$ELSE}
  function GetSmoothIndex(ThisIndex, SmoothingGroup: cardinal;
    P: PSmoothIndexArray): integer; inline;
    // Retrieves the vertex index for the given index and smoothing group.
    // This redirection is necessary because a vertex might have been duplicated.
  var
    i: word;
  begin
    i := 0;
    while SmoothingGroup and (1 shl i) = 0 do
      Inc(i);
    Result := integer(p^[ThisIndex, i]);
  end;
{$ENDIF}

  //---------------------------------------------------------------------------

  procedure DuplicateVertex(Index: integer);
  // extends the vector and normal array by one entry and duplicates the vertex AData given by Index
  // the marker and texture arrays will be extended too, if necessary
  begin
    // enhance vertex array
    with mesh.Vertices do
      Add(Items[index]);
    mesh.Normals.Add(NullVector);
    // enhance smooth index array
    ReallocMem(SmoothIndices, (CurrentVertexCount + 1) * SizeOf(TSmoothIndexEntry));
    FillChar(SmoothIndices[CurrentVertexCount], SizeOf(TSmoothIndexEntry), $FF);
    // enhance marker array
    if (CurrentVertexCount div 8) <> ((CurrentVertexCount + 1) div 8) then
    begin
      ReallocMem(Marker, ((CurrentVertexCount + 1) div 8) + 1);
      Marker[(CurrentVertexCount div 8) + 1] := 0;
    end;
    with mesh.TexCoords do
      if Count > 0 then
        Add(Items[index]);
    Inc(CurrentVertexCount);
  end;

  //---------------------------------------------------------------------------

  function FindMotionIndex(KeyFramer: TKeyFramer; const ObjectName: AnsiString): integer;
    // Looks through the motion list for the object "ObjectName" and returns its index
    // or -1 if the name is not it the list
  var
    I: integer;
  begin
    Result := -1;
    with KeyFramer do
      for I := 0 to MeshMotionCount - 1 do
        if CompareText(string(MeshMotion[I].NameStr), string(ObjectName)) = 0 then
        begin
          Result := I;
          Break;
        end;
  end;

var
  CurrentMotionIndex, iMaterial, i, j, x: integer;
  aFaceGroup: TfgxVertexIndexList;
  Face, Vertex, TargetVertex: integer;
  SmoothingGroup: cardinal;
  CurrentIndex: word;
  Vector1, Vector2, Normal: TAffineVector;
  standardNormalsOrientation: boolean;
  lights_mesh: TgxFile3DSOmniLightObject;
  camera_mesh: TgxFile3DSCameraObject;
  basemesh: TgxBaseMesh;
begin

  with TFile3DS.Create do
    try
      LoadFromStream(aStream);
      // determine front face winding
      { TODO : better face winding }
      standardNormalsOrientation := not (NormalsOrientation = mnoDefault);

      for i := 0 to Objects.MeshCount - 1 do
        with PMesh3DS(Objects.Mesh[I])^ do
        begin
          hasLightMap := False;
          mesh := TgxFile3DSMeshObject.CreateOwned(Owner.MeshObjects);
          mesh.Name := string(PMesh3DS(Objects.Mesh[I])^.NameStr);
          //dummy targets
          for x := KeyFramer.Settings.Seg.SegBegin to KeyFramer.Settings.Seg.SegEnd do
            TgxMeshMorphTarget.CreateOwned(mesh.MorphTargets);

          with mesh do
          begin
            Mode := momFaceGroups;
            // make a copy of the vertex data, this must always be available
            Vertices.Capacity := NVertices;
            Normals.AddNulls(NVertices);
            if NTextVerts > 0 then
            begin
              TexCoords.Capacity := NVertices;
              for j := 0 to NVertices - 1 do
              begin
                Vertices.Add(PAffineVector(@VertexArray[j])^);
                TexCoords.Add(PTexPoint(@TextArray[j])^);
              end;
            end
            else
            begin
              for j := 0 to NVertices - 1 do
                Vertices.Add(PAffineVector(@VertexArray[j])^);
            end;
          end;

          // allocate memory for the smoothindices and the marker array
          CurrentVertexCount := NVertices;
          Marker := AllocMem((NVertices div 8) + 1); // one bit for each vertex
          GetMem(SmoothIndices, NVertices * SizeOf(TSmoothIndexEntry));

          if SmoothArray = nil then
          begin
            // no smoothing groups to consider
            for face := 0 to NFaces - 1 do
              with FaceArray^[Face] do
              begin
                // normal vector for the face
                with mesh.Vertices do
                begin
                  VectorSubtract(Items[V1], Items[V2], vector1);
                  VectorSubtract(Items[V3], Items[V2], vector2);
                end;
                if standardNormalsOrientation then
                  Normal := VectorCrossProduct(Vector1, Vector2)
                else
                  Normal := VectorCrossProduct(Vector2, Vector1);
                // go for each vertex in the current face
                for Vertex := 0 to 2 do
                begin
                  // copy current index for faster access
                  CurrentIndex := FaceRec[Vertex];
                  // already been touched?
                  if IsVertexMarked(Marker, CurrentIndex) and (CurrentVertexCount < High(FaceRec[Vertex])) then
                  begin
                    // already touched vertex must be duplicated
                    DuplicateVertex(CurrentIndex);
                    FaceRec[Vertex] := CurrentVertexCount - 1;
                    mesh.Normals[CurrentVertexCount - 1] := Normal;
                  end
                  else
                  begin
                    // not yet touched, so just store the normal
                    mesh.Normals[CurrentIndex] := Normal;
                    MarkVertex(Marker, CurrentIndex);
                  end;
                end;
              end;
          end
          else
          begin
            // smoothing groups are to be considered
            for Face := 0 to NFaces - 1 do
              with FaceArray^[Face] do
              begin
                // normal vector for the face
                with mesh.Vertices do
                begin
                  VectorSubtract(Items[V1], Items[V2], vector1);
                  VectorSubtract(Items[V3], Items[V2], vector2);
                end;
                if standardNormalsOrientation then
                  Normal := VectorCrossProduct(Vector1, Vector2)
                else
                  Normal := VectorCrossProduct(Vector2, Vector1);
                SmoothingGroup := SmoothArray^[Face];
                // go for each vertex in the current face
                for Vertex := 0 to 2 do
                begin
                  // copy current index for faster access
                  currentIndex := FaceRec[Vertex];
                  // Has vertex already been touched?
                  if IsVertexMarked(Marker, currentIndex) then
                  begin
                    // check smoothing group
                    if (SmoothingGroup = 0) then
                    begin
                      if (CurrentVertexCount < High(FaceRec[Vertex])) then
                      begin
                        // no smoothing then just duplicate vertex
                        DuplicateVertex(CurrentIndex);
                        FaceRec[Vertex] := CurrentVertexCount - 1;
                        mesh.Normals[CurrentVertexCount - 1] := Normal;
                        // mark new vertex also as touched
                        MarkVertex(Marker, CurrentVertexCount - 1);
                      end;
                    end
                    else
                    begin
                      // this vertex must be smoothed, check if there's already
                      // a (duplicated) vertex for this smoothing group
                      TargetVertex := GetSmoothIndex(CurrentIndex, SmoothingGroup, SmoothIndices);
                      if (TargetVertex < 0) then
                      begin
                        if (CurrentVertexCount < High(FaceRec[Vertex])) then
                        begin
                          // vertex has not yet been duplicated for this smoothing
                          // group, so do it now
                          DuplicateVertex(CurrentIndex);
                          FaceRec[Vertex] := CurrentVertexCount - 1;
                          mesh.Normals[CurrentVertexCount - 1] := Normal;
                          StoreSmoothIndex(CurrentIndex, SmoothingGroup,
                            CurrentVertexCount - 1, SmoothIndices);
                          StoreSmoothIndex(CurrentVertexCount - 1,
                            SmoothingGroup, CurrentVertexCount - 1, SmoothIndices);
                          // mark new vertex also as touched
                          MarkVertex(Marker, CurrentVertexCount - 1);
                        end;  
                      end
                      else
                      begin
                        // vertex has already been duplicated,
                        // so just add normal vector to other vertex...
                        mesh.Normals[TargetVertex] :=
                          VectorAdd(mesh.Normals[TargetVertex], Normal);
                        // ...and tell which new vertex has to be used from now on
                        FaceRec[Vertex] := TargetVertex;
                      end;
                    end;
                  end
                  else
                  begin
                    // vertex not yet touched, so just store the normal
                    mesh.Normals[CurrentIndex] := Normal;
                    // initialize smooth indices for this vertex
                    FillChar(SmoothIndices[CurrentIndex],
                      SizeOf(TSmoothIndexEntry), $FF);
                    if SmoothingGroup <> 0 then
                      StoreSmoothIndex(CurrentIndex, SmoothingGroup,
                        CurrentIndex, SmoothIndices);
                    MarkVertex(Marker, CurrentIndex);
                  end;
                end;
              end;
          end;
          FreeMem(Marker);
          FreeMem(SmoothIndices);

          Assert(mesh.Vertices.Count = CurrentVertexCount);

          // and normalize the Normals array
          mesh.Normals.Normalize;

          // now go for each material group
          // if there's no face to material assignment then just copy the
          // face definitions and rely on the default texture of the scene object
          if (NMats = 0) or (not vVectorFileObjectsAllocateMaterials) then
          begin
            aFaceGroup := TfgxVertexIndexList.CreateOwned(mesh.FaceGroups);
            with aFaceGroup do
            begin
              basemesh := TgxBaseMesh(Self.GetOwner);
              if basemesh.MaterialLibrary <> nil then
                MaterialName := basemesh.MaterialLibrary.Materials.Add.Name;
              // copy the face list
              for j := 0 to NFaces - 1 do
              begin
                Add(FaceArray[J].V1);
                Add(FaceArray[J].V2);
                Add(FaceArray[J].V3);
              end;
            end;
          end
          else
          begin
            for iMaterial := 0 to NMats - 1 do
            begin
              aFaceGroup := TfgxVertexIndexList.CreateOwned(mesh.FaceGroups);
              with aFaceGroup do
              begin
                MaterialName :=
                  GetOrAllocateMaterial(Materials, string(MatArray[iMaterial].NameStr));
                LightMapIndex :=
                  GetOrAllocateLightMap(Materials, string(MatArray[iMaterial].NameStr));
                // copy all vertices belonging to the current face into our index array,
                // there won't be redundant vertices since this would mean a face has more than one
                // material
                // NFaces is the one from FaceGroup
                with MatArray[iMaterial] do
                  for j := 0 to NFaces - 1 do
                  begin
                    Add(FaceArray[FaceIndex[J]].V1);
                    Add(FaceArray[FaceIndex[J]].V2);
                    Add(FaceArray[FaceIndex[J]].V3);
                  end;
              end;
            end;
          end;
          if hasLightMap then
            for j := 0 to mesh.TexCoords.Count - 1 do
              mesh.LightMapTexCoords.Add(mesh.TexCoords[j].X, mesh.TexCoords[j].Y);
        end;

      // Adding non-mesh objects (for example, dummies).
      for I := 0 to KeyFramer.MeshMotionCount - 1 do
        if (Owner.MeshObjects.FindMeshByName(string(
          KeyFramer.MeshMotion[I].NameStr)) = nil) then
        begin
          mesh := TgxFile3DSMeshObject.CreateOwned(Owner.MeshObjects);
          mesh.Name := string(KeyFramer.MeshMotion[I].NameStr);
          //dummy targets
          for x := KeyFramer.Settings.Seg.SegBegin to KeyFramer.Settings.Seg.SegEnd do
            TgxMeshMorphTarget.CreateOwned(mesh.MorphTargets);

          mesh.LoadAnimation(KeyFramer.MeshMotion[I]);
        end;

      for I := 0 to Objects.MeshCount - 1 do
        with PMesh3DS(Objects.Mesh[I])^ do
        begin
          mesh := Owner.MeshObjects.FindMeshByName(string(NameStr)) as TgxFile3DSMeshObject;
          with mesh, KeyFramer do
          begin
            CurrentMotionIndex := FindMotionIndex(KeyFramer, NameStr);
            FRefTranf.ModelMatrix := InvertMeshMatrix(Objects, string(NameStr));

            if MeshMotionCount > 0 then
              LoadAnimation(MeshMotion[CurrentMotionIndex]);
          end;
        end;

      // Lights Omni.
      for I := 0 to Objects.OmniLightCount - 1 do
      begin
        lights_mesh := TgxFile3DSOmniLightObject.CreateOwned(Owner.MeshObjects);
        // Dummy targets for it.
        for x := KeyFramer.Settings.Seg.SegBegin to KeyFramer.Settings.Seg.SegEnd do
          TgxMeshMorphTarget.CreateOwned(lights_mesh.MorphTargets);
        lights_mesh.LoadData(Owner, Objects.OmniLight[I]);
        lights_mesh.LoadAnimation(KeyFramer.OmniLightMotion[I]);
      end;

      // Lights Spot.
      for I := 0 to Objects.SpotLightCount - 1 do
      begin
        lights_mesh := TgxFile3DSSpotLightObject.CreateOwned(Owner.MeshObjects);
        // Dummy targets for it.
        for x := KeyFramer.Settings.Seg.SegBegin to KeyFramer.Settings.Seg.SegEnd do
          TgxMeshMorphTarget.CreateOwned(lights_mesh.MorphTargets);
        lights_mesh.LoadData(Owner, Objects.SpotLight[I]);
        lights_mesh.LoadAnimation(KeyFramer.SpotLightMotion[I]);
      end;

      // Camera Objects.
      for I := 0 to Objects.CameraCount - 1 do
      begin
        camera_mesh := TgxFile3DSCameraObject.CreateOwned(Owner.MeshObjects);
        // Dummy targets for it.
        for x := KeyFramer.Settings.Seg.SegBegin to KeyFramer.Settings.Seg.SegEnd do
          TgxMeshMorphTarget.CreateOwned(camera_mesh.MorphTargets);
        camera_mesh.LoadData(Owner, Objects.Camera[I]);
        camera_mesh.LoadAnimation(KeyFramer.CameraMotion[I]);
      end;

      // Apply animation matrix to static data
      if vFile3DS_LoadedStaticFrame >= 0 then
      begin
        for i := 0 to Owner.MeshObjects.Count - 1 do
        begin
          if Owner.MeshObjects[i] is TgxFile3DSMeshObject then
          begin
            mesh := Owner.MeshObjects[i] as TgxFile3DSMeshObject;
            mesh.FStatic := True;
            for j := 0 to mesh.Vertices.Count - 1 do
              mesh.Vertices[j] := VectorTransform(mesh.Vertices[j], mesh.FAnimTransf.ModelMatrix);
          end;
        end;
      end;


    finally
      Free;
    end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterClasses([TgxFile3DSDummyObject, TgxFile3DSMeshObject,
    TgxFile3DSOmniLightObject, TgxFile3DSSpotLightObject,
    TgxFile3DSCameraObject]);

  RegisterVectorFileFormat('3ds', '3D Studio files', Tgx3DSVectorFile);
  RegisterVectorFileFormat('prj', '3D Studio project files', Tgx3DSVectorFile);
end.

