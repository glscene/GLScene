//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.ExplosionFx;

(*
  ExplosionFX Effect

  Description: this effect explodes a mesh object into triangles
  that fly over. You can define a default direction, in wich case
  the pieces of the mesh will follow that direction, only rotating,
  or if you define a null vector as the direction, a vector will be
  calculated for each triangle, based on the normal vector of that
  triangle, with a little random addition so things look better.
  Pretty neat :)

  Note: the owner of this behaviour should be any class that derives
  from TgxBaseMesh class or any other class derived from TgxBaseMesh.
  Also, the structure of the mesh is lost after the caching of information,
  so if you'll need the mesh after exploding it, you'll have to save the
  MeshObjects property of the mesh, OR load it again.
*)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,

  GXS.XCollection,
  GXS.VectorGeometry,
  GXS.VectorTypes,
  GXS.VectorLists,

  GXS.Scene,
  GXS.VectorFileObjects,
  GXS.Coordinates,
  GXS.RenderContextInfo,
  GXS.Context,
  GXS.State;

type
  TgxBExplosionFX = class(TgxObjectPreEffect)
  private
    FTriList: TgxAffineVectorList;
    FRotList: TgxAffineVectorList;
    FDirList: TgxAffineVectorList;
    FPosList: TgxAffineVectorList;
    FEnabled: boolean;
    FFaceCount: integer;
    FSpeed: single;
    FDirection: TgxCoordinates;
    FMaxSteps: integer;
    FStep: integer;
    procedure SetTriList(Value: TgxAffineVectorList);
    procedure SetRotList(Value: TgxAffineVectorList);
    procedure SetDirList(Value: TgxAffineVectorList);
    procedure SetPosList(Value: TgxAffineVectorList);
    procedure SetDirection(value: TgxCoordinates);
    procedure SetEnabled(value: boolean);
  protected
    property TriList: TgxAffineVectorList read FTriList write SetTriList;
    property RotList: TgxAffineVectorList read FRotList write SetRotList;
    property DirList: TgxAffineVectorList read FDirList write SetDirList;
    property PosList: TgxAffineVectorList read FPosList write SetPosList;
    property FaceCount: integer read FFAceCount write FFaceCount;
    procedure CacheInfo;
  public
    property Enabled: boolean read FEnabled write SetEnabled;
    property Step: integer read FStep;
    constructor Create(aOwner : TXCollection); override;
    destructor Destroy; override;
    procedure Render(var rci : TgxRenderContextInfo); override;
    (* Resets the behaviour, so the information can be re-cached and
      the mesh can be exploded again *)
    procedure Reset;
    class function FriendlyName : String; override;
    class function FriendlyDescription : String; override;
  published
    property MaxSteps: integer read FMaxSteps write FMaxSteps;
    property Speed: single read FSpeed write FSpeed;
    property Direction: TgxCoordinates read FDirection write SetDirection;
  end;

//--------------------------------------
implementation
//--------------------------------------


//-----------------------------
// TgxBExplosionFx
//-----------------------------

constructor TgxBExplosionFx.Create(aOwner: TXCollection);
begin
  inherited Create(AOwner);
  FTriList := TgxAffineVectorList.Create;
  FRotList := TgxAffineVectorList.Create;
  FDirList := TgxAffineVectorList.Create;
  FPosList := TgxAffineVectorList.Create;
  FDirection := TgxCoordinates.CreateInitialized(Self, NullHmgVector, csPoint);
end;

destructor TgxBExplosionFX.Destroy;
begin
  FEnabled := False;
  FTriList.Free;
  FRotList.Free;
  FDirList.Free;
  FPosList.Free;
  FDirection.Free;
  inherited Destroy;
end;

class function TgxBExplosionFX.FriendlyName: string;
begin
  Result := 'ExplosionFx';
end;

class function TgxBExplosionFX.FriendlyDescription: string;
begin
  Result := 'Explosion FX';
end;

procedure TgxBExplosionFx.SetTriList(Value: TgxAffineVectorList);
begin
  FTriList.Assign(Value);
end;

procedure TgxBExplosionFx.SetRotList(Value: TgxAffineVectorList);
begin
  FRotList.Assign(Value);
end;

procedure TgxBExplosionFx.SetDirList(Value: TgxAffineVectorList);
begin
  FDirList.Assign(Value);
end;

procedure TgxBExplosionFx.SetPosList(Value: TgxAffineVectorList);
begin
  FPosList.Assign(Value);
end;

procedure TgxBExplosionFx.SetDirection(Value: TgxCoordinates);
begin
  Value.Normalize;
  FDirection.Assign(Value);
end;

procedure TgxBExplosionFx.SetEnabled(Value: boolean);
begin
  FEnabled := Value;
end;

procedure TgxBExplosionFx.Reset;
begin
  FEnabled := False;
  FStep := 0;
  FTriList.Clear;
  FRotList.Clear;
  FDirList.Clear;
  FPosList.Clear;
  FFaceCount := 0;
end;

procedure TgxBExplosionFx.CacheInfo;
var
  Face: integer;
  p1, p2, p3, v1, v2, posi: TAffineVector;
  Normal: TVector4f;
begin
  // make sure we can explode this object
  if not OwnerBaseSceneObject.InheritsFrom(TgxBaseMesh) then begin
    FEnabled := False;
    Exit;
  end;
  FTriList.Free;
  // get all the triangles of all the meshObjects
  FTriList := TgxBaseMesh(OwnerBaseSceneObject).MeshObjects.ExtractTriangles;
  FaceCount := FTriList.Count div 3;
  // set initial direction, rotation and position
  for Face := 0 to Facecount - 1 do begin
  // get the vertices of the triangle
    SetVector(p1, FTriList.Items[Face * 3]);
    SetVector(p2, FTriList.Items[Face * 3 + 1]);
    SetVector(p3, FTriList.Items[Face * 3 + 2]);
  // if the direction property is a null vector, than the direction is
  // given by the normal of the face
    if VectorEquals(FDirection.AsVector, NullHmgVector) then begin
      v1 := VectorSubtract(p2, p1);
      v2 := VectorSubtract(p2, p3);
      NormalizeVector(v1); // use of procedure is faster: PhP
      NormalizeVector(v2); // use of procedure is faster: PhP
      SetVector(Normal, VectorCrossProduct(v1, v2)); // use of procedure is faster: PhP
  // randomly rotate the normal vector so the faces are somewhat scattered
      case Random(3) of
        0: RotateVector(Normal, XVector, DegToRadian(45.0*Random));
        1: RotateVector(Normal, YVector, DegToRadian(45.0*Random));
        2: RotateVector(Normal, ZVector, DegToRadian(45.0*Random));
      end;
      NormalizeVector(Normal);
      FDirList.Add(Normal);
    end
    else
      FDirList.Add(FDirection.AsVector);
  // calculate the center (position) of the triangle so it rotates around its center
    posi.X := (p1.X + p2.X + p3.X) / 3;
    posi.Y := (p1.Y + p2.Y + p3.Y) / 3;
    posi.Z := (p1.Z + p2.Z + p3.Z) / 3;
    FPosList.add(posi);
  // random rotation (in degrees)
    FRotList.Add(DegToRadian(3.0*Random), DegToRadian(3.0*Random), DegToRadian(3.0*Random));
  end;
  // Dispose the struture of the mesh
  TgxBaseMesh(OwnerBaseSceneObject).MeshObjects.Clear;
  TgxBaseMesh(OwnerBaseSceneObject).StructureChanged;
end;

procedure TgxBExplosionFX.Render(var rci : TgxRenderContextInfo);
var
  Face: integer;
  dir, p1, p2, p3: TAffineVector;
  mat: TMatrix4f;

begin
  if not FEnabled then
    Exit;
  // cache de list of vertices
  if FTriList.Count <= 0 then begin
    CacheInfo;
    if not FEnabled then
      Exit;
  end;
  // render explosion
  rci.gxStates.Disable(stCullFace);
  glBegin(GL_TRIANGLES);
  for Face := 0 to FaceCount - 1 do begin
    SetVector(p1, FTriList.Items[Face * 3]);
    SetVector(p2, FTriList.Items[Face * 3 + 1]);
    SetVector(p3, FTriList.Items[Face * 3 + 2]);
  // rotate the face
    mat := IdentityHmgMatrix;
    mat := MatrixMultiply(mat, CreateRotationMatrixX(FRotList.Items[face].X));
    mat := MatrixMultiply(mat, CreateRotationMatrixY(FRotList.Items[face].Y));
    mat := MatrixMultiply(mat, CreateRotationMatrixZ(FRotList.Items[face].Z));
    SubtractVector(p1, FPosList.Items[Face]);  // use of procedure is faster: PhP
    SubtractVector(p2, FPosList.Items[Face]);  // -''-
    SubtractVector(p3, FPosList.Items[Face]);  // -''-
    p1 := VectorTransform(p1, mat);
    p2 := VectorTransform(p2, mat);
    p3 := VectorTransform(p3, mat);
    AddVector(p1, FPosList.Items[Face]);  // use of procedure is faster: PhP
    AddVector(p2, FPosList.Items[Face]);  // -''-
    AddVector(p3, FPosList.Items[Face]);  // -''-
  // move the face in the direction it is heading
    SetVector(dir, FDirList.Items[Face]);
    glNormal3f(dir.X, dir.Y, dir.Z);
    ScaleVector(dir, Speed);
    AddVector(p1, dir);
    AddVector(p2, dir);
    AddVector(p3, dir);
  // also, move the center of the face
    FPosList.Items[Face] := VectorAdd(FPosList.Items[Face], dir);

  // save the changes
    FTrilist.Items[face * 3] := p1;
    FTrilist.Items[face * 3 +1] := p2;
    FTrilist.Items[face * 3 +2] := p3;

    glVertex3f(p1.X, p1.Y, p1.Z);
    glVertex3f(p2.X, p2.Y, p2.Z);
    glVertex3f(p3.X, p3.Y, p3.Z);
  end;
  glEnd;
  rci.gxStates.Enable(stCullFace);
  if FMaxSteps <> 0 then begin
    Inc(FStep);
    if FStep = FMaxSteps then
      FEnabled := False;
  end;
end;

//------------------------------------
initialization
//------------------------------------
	
	RegisterXCollectionItemClass(TgxBExplosionFX);

finalization

	UnregisterXCollectionItemClass(TgxBExplosionFX);

end.
