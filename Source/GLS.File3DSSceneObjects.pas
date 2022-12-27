//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLS.File3DSSceneObjects;

(* 3ds-specific scene objects. *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,
  System.Math,
  
  GLS.OpenGLTokens,
  GLS.OpenGLAdapter,
  GLS.VectorGeometry,
  GLS.Context,
  GLS.Scene,
  GLS.VectorFileObjects,
  GLS.VectorTypes,
  GLS.PersistentClasses,
  GLS.Coordinates,
  GLS.RenderContextInfo,
  GLS.State;

type
  TGLFile3DSLight = class(TGLLightSource)
  private
    FTargetPos: TGLCoordinates;
    FHotSpot: Single;
    FMultipler: Single;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoRender(var rci: TGLRenderContextInfo; renderSelf, renderChildren: Boolean); override;
    procedure CoordinateChanged(Sender: TGLCustomCoordinates); override;
    destructor Destroy; override;
  published
    property SpotTargetPos: TGLCoordinates read FTargetPos;
    property HotSpot: Single read FHotSpot write FHotSpot;
    property Multipler: Single read FMultipler write FMultipler;
  end;

  TGLFile3DSCamera = class(TGLCamera)
  private
    FTargetPos: TGLCoordinates;
    FQuadCyl: array[0..1] of PGLUquadric;
    FQuadDisk: array[0..1] of PGLUquadric;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoRender(var rci: TGLRenderContextInfo; renderSelf, renderChildren: Boolean); override;
    procedure CoordinateChanged(Sender: TGLCustomCoordinates); override;
    destructor Destroy; override;
  published
    property CameraTargetPos: TGLCoordinates read FTargetPos;
    property RollAngle;
  end;

  TGLFile3DSActor = class(TGLActor)
  private
    procedure ReadMesh(Stream: TStream);
    procedure WriteMesh(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  end;

  TGLFile3DSFreeForm = class(TGLFreeForm)
  private
    FTransfMat, FScaleMat, ParentMatrix: TGLMatrix;

    FS_Rot3DS: TGLCoordinates4;
    FRot3DS: TGLCoordinates4;
    FScale3DS: TGLCoordinates4;
    procedure ReadMesh(Stream: TStream);
    procedure WriteMesh(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    FRefMat: TGLMatrix;
    constructor Create(AOWner: TComponent); override;
    destructor Destroy; override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    procedure CoordinateChanged(Sender: TGLCustomCoordinates); override;
    function AxisAlignedDimensionsUnscaled: TGLVector; override;
    function BarycenterAbsolutePosition: TGLVector; override;
  published
    property S_Rot3DS: TGLCoordinates4 read FS_Rot3DS;
    property Rot3DS: TGLCoordinates4 read FRot3DS;
    property Scale3DS: TGLCoordinates4 read FScale3DS;
  end;

var
  vGLFile3DSSceneObjects_RenderCameraAndLights: Boolean = False;

//---------------------------------------------------------  
implementation
//---------------------------------------------------------  

function MakeRotationQuaternion(const axis: TAffineVector; angle: Single): TQuaternion;
var
  v: TGLVector;
  halfAngle, invAxisLengthMult: Single;
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

function QuaternionToRotateMatrix(const Quaternion: TQuaternion): TGLMatrix;
var
  wx, wy, wz, xx, yy, yz, xy, xz, zz, x2, y2, z2: Single;
  quat: TGLVector;
  m: TGLMatrix;
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

constructor TGLFile3DSLight.Create(AOwner: TComponent);
begin
  inherited;

  FTargetPos := TGLCoordinates.CreateInitialized(self, VectorMake(NullVector), csPoint);
  FHotSpot := 1;
  FMultipler := 1;
end;

procedure TGLFile3DSLight.DoRender(var rci: TGLRenderContextInfo; renderSelf, renderChildren: Boolean);

  procedure BuildFace;
  begin
    gl.Begin_(GL_TRIANGLES);
    gl.Vertex3f(0.03, 0, 0);
    gl.Vertex3f(0, 0.03, 0);
    gl.Vertex3f(0, 0, 0.07);
    gl.End_;
  end;

var
  dv: Single;

begin
  inherited;
  if not vGLFile3DSSceneObjects_RenderCameraAndLights then
    Exit;

  rci.GLStates.PolygonMode := pmLines;
  gl.PushMatrix;

  dv := VectorDistance(Position.AsVector, rci.cameraPosition);
  gl.Scalef(dv, dv, dv);

  // Up.
  BuildFace;
  gl.Rotatef(90, 0, 0, 1);
  BuildFace;
  gl.Rotatef(180, 0, 0, 1);
  BuildFace;
  gl.Rotatef(270, 0, 0, 1);
  BuildFace;

  // Down.
  gl.Rotatef(180, 0, 1, 0);
  BuildFace;
  gl.Rotatef(90, 0, 0, 1);
  BuildFace;
  gl.Rotatef(180, 0, 0, 1);
  BuildFace;
  gl.Rotatef(270, 0, 0, 1);
  BuildFace;

  gl.PopMatrix;
end;

procedure TGLFile3DSLight.CoordinateChanged(Sender: TGLCustomCoordinates);
begin
  inherited;

  if (Sender = FTargetPos) or (Sender = Position) then
    SpotDirection.SetVector(VectorNormalize(VectorSubtract(FTargetPos.AsAffineVector, Position.AsAffineVector)));
end;

destructor TGLFile3DSLight.Destroy;
begin
  FTargetPos.Free;
  inherited;
end;

constructor TGLFile3DSCamera.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;

  FTargetPos := TGLCoordinates.CreateInitialized(self, VectorMake(NullVector), csPoint);

  for I := 0 to 1 do
  begin
    //    FQuadCyl[I] := gluNewQuadric;
    //    FQuadDisk[I] := gluNewQuadric;
    //    gluQuadricNormals(FQuadCyl[I], GLU_SMOOTH);
    //    gluQuadricNormals(FQuadDisk[I], GLU_SMOOTH);
  end;
end;

procedure TGLFile3DSCamera.DoRender(var rci: TGLRenderContextInfo; renderSelf, renderChildren: Boolean);

  procedure BuildCyl;
  begin
    //    gluCylinder(FQuadCyl[0], 1, 1, 0.5, 6, 1);
    //    glTranslatef(0, 0, 0.5);
    //    gluDisk(FQuadDisk[0], 0, 1, 6, 1);
    gl.Translatef(0, 0, -0.5);
    rci.GLStates.InvertGLFrontFace;
    //    gluDisk(FQuadDisk[0], 0, 1, 6, 1);
    rci.GLStates.InvertGLFrontFace;
  end;

  procedure BuildFace;
  begin
    gl.Rotatef(-90, 0, 1, 0);
    gl.Rotatef(45, 0, 0, 1);
    gl.Translatef(0, -0.5, 1);
    //    gluCylinder(FQuadCyl[0], 0.5, 1.3, 2.4, 4, 1);
    gl.Translatef(0, 0, 2.4);
    //    gluDisk(FQuadDisk[0], 0, 1.3, 4, 1);
  end;

var
  dv, ang: Single;
  v, v1: TAffineVector;

begin
  inherited;
  if not vGLFile3DSSceneObjects_RenderCameraAndLights then
    Exit;

  v := VectorNormalize(VectorSubtract(FTargetPos.AsAffineVector, Position.AsAffineVector));

  v1 := AffineVectorMake(v.X, v.Y, 0);
  NormalizeVector(v1);
  ang := ArcCos(VectorDotProduct(v, v1));

  rci.GLStates.PolygonMode := pmLines;

  gl.PushMatrix;
  gl.Rotatef(ang * 180 / pi, 0, 0, 1);
  dv := VectorDistance(Position.AsVector, rci.cameraPosition);
  gl.Scalef(dv / 25, dv / 25, dv / 25);

  gl.RotateF(90, 0, 1, 0);
  gl.Translatef(0, 1, 0);
  BuildCyl;
  gl.Translatef(1, -1, 0);
  BuildCyl;
  BuildFace;
  gl.PopMatrix;

  rci.GLStates.PolygonMode := pmFill;
end;

procedure TGLFile3DSCamera.CoordinateChanged(Sender: TGLCustomCoordinates);
begin
  inherited;

  if (Sender = FTargetPos) or (Sender = Position) then
  begin
    //    Up.AsAffineVector := ZVector;
    //    Direction.SetVector(VectorNormalize(VectorSubtract(FTargetPos.AsAffineVector, Position.AsAffineVector)));
  end;
end;

destructor TGLFile3DSCamera.Destroy;
var
  I: Integer;
begin
  inherited;
  FTargetPos.Free;
  for I := 0 to 1 do
  begin
    gluDeleteQuadric(FQuadCyl[I]);
    gluDeleteQuadric(FQuadDisk[I]);
  end;
end;

procedure TGLFile3DSActor.ReadMesh(Stream: TStream);
var
  virt: TGLBinaryReader;
begin
  virt := TGLBinaryReader.Create(Stream);
  MeshOBjects.ReadFromFiler(virt);
  virt.Free;
end;

procedure TGLFile3DSActor.WriteMesh(Stream: TStream);
var
  virt: TGLBinaryWriter;
begin
  virt := TGLBinaryWriter.Create(Stream);
  MeshOBjects.WriteToFiler(virt);
  virt.Free;
end;

procedure TGLFile3DSActor.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('MeshObjectsData', ReadMesh, WriteMesh, True);
end;

constructor TGLFile3DSFreeForm.Create(AOWner: TComponent);
begin
  inherited;

  FRefMat := IdentityHmgMatrix;
  FTransfMat := IdentityHmgMatrix;
  FScaleMat := IdentityHmgMatrix;
  FS_Rot3DS := TGLCoordinates4.CreateInitialized(self, VectorMake(1, 0, 0), csVector);
  FRot3DS := TGLCoordinates4.CreateInitialized(self, VectorMake(1, 0, 0), csVector);
  FScale3DS := TGLCoordinates4.CreateInitialized(self, VectorMake(1, 1, 1), csVector);

  ObjectStyle := [osDirectDraw];
end;

destructor TGLFile3DSFreeForm.Destroy;
begin
  FS_Rot3DS.Free;
  FRot3DS.Free;
  FScale3DS.Free;

  inherited;
end;

procedure TGLFile3DSFreeForm.ReadMesh(Stream: TStream);
var
  v: TGLVector;
  virt: TGLBinaryReader;
begin
  virt := TGLBinaryReader.Create(Stream);

  virt.read(FRefMat, sizeof(FRefMat));
  virt.read(v, sizeof(v));
  S_Rot3DS.SetVector(v);
  virt.read(v, sizeof(v));
  Rot3DS.SetVector(v);
  virt.read(v, sizeof(v));
  Scale3DS.SetVector(v);

  MeshOBjects.ReadFromFiler(virt);
  virt.Free;
end;

procedure TGLFile3DSFreeForm.WriteMesh(Stream: TStream);
var
  virt: TGLBinaryWriter;
  v: TGLVector;
begin
  virt := TGLBinaryWriter.Create(Stream);

  virt.write(FRefMat, sizeof(FRefMat));
  v := S_Rot3DS.AsVector;
  virt.write(v, sizeof(v));
  v := Rot3DS.AsVector;
  virt.write(v, sizeof(v));
  v := Scale3DS.AsVector;
  virt.write(v, sizeof(v));

  MeshOBjects.WriteToFiler(virt);
  virt.Free;
end;

procedure TGLFile3DSFreeForm.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('MeshObjectsData', ReadMesh, WriteMesh, True);
end;

procedure TGLFile3DSFreeForm.BuildList(var rci: TGLRenderContextInfo);
begin
  gl.MultMatrixf(@FTransfMat);
  gl.MultMatrixf(@FScaleMat);

  gl.PushMatrix;
  gl.MultMatrixf(@FRefMat);
  inherited;
  gl.PopMatrix;

  if parent is TGLFile3DSFreeForm then
    ParentMatrix := (parent as TGLFile3DSFreeForm).ParentMatrix
  else
    ParentMatrix := IdentityHmgMatrix;

  ParentMatrix := MatrixMultiply(FScaleMat, ParentMatrix);
  ParentMatrix := MatrixMultiply(FTransfMat, ParentMatrix);
end;

procedure TGLFile3DSFreeForm.CoordinateChanged(Sender: TGLCustomCoordinates);
var
  quat, quat1, quat2: TQuaternion;
begin
  inherited;

  if Sender.ClassType = FRot3DS.ClassType then
  begin
    quat1 := MakeRotationQuaternion(FS_Rot3DS.AsAffineVector, FS_Rot3DS.W);
    quat2 := MakeRotationQuaternion(FRot3DS.AsAffineVector, FRot3DS.W);

    quat := QuaternionMultiply(quat1, quat2);
    NormalizeQuaternion(quat);
    FTransfMat := QuaternionToRotateMatrix(quat);
    NormalizeMatrix(FTransfMat);
  end;
  if Sender.ClassType = FScale3DS.ClassType then
  begin
    FScaleMat := CreateScaleMatrix(FScale3DS.AsAffineVector);
  end;
end;

function TGLFile3DSFreeForm.AxisAlignedDimensionsUnscaled: TGLVector;
var
  dMin, dMax: TAffineVector;
  mat: TGLMatrix;
begin
  MeshObjects.GetExtents(dMin, dMax);
  mat := ParentMatrix;
  mat := MatrixMultiply(FRefMat, mat);
  if not IsInfinite(dMin.X) then
    dMin := VectorTransform(dMin, mat);
  if not IsInfinite(dMax.X) then
    dMax := VectorTransform(dMax, mat);

  Result.X := (dMax.X - dMin.X) / 2;
  Result.Y := (dMax.Y - dMin.Y) / 2;
  Result.Z := (dMax.Z - dMin.Z) / 2;
  Result.W := 0;
end;

function TGLFile3DSFreeForm.BarycenterAbsolutePosition: TGLVector;
var
  dMin, dMax: TAffineVector;
  mat: TGLMatrix;
begin
  MeshObjects.GetExtents(dMin, dMax);
  mat := ParentMatrix;
  mat := MatrixMultiply(FRefMat, mat);
  if not IsInfinite(dMin.X) then
    dMin := VectorTransform(dMin, mat);
  if not IsInfinite(dMax.X) then
    dMax := VectorTransform(dMax, mat);

  Result.X := (dMax.X + dMin.X) / 2;
  Result.Y := (dMax.Y + dMin.Y) / 2;
  Result.Z := (dMax.Z + dMin.Z) / 2;
  Result.W := 1;

  Result := LocalToAbsolute(Result);
end;

initialization
  RegisterClasses([TGLFile3DSLight, TGLFile3DSCamera, TGLFile3DSActor, TGLFile3DSFreeForm]);

end.

