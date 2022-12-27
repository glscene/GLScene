//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.MeshBuilder;

(*
   Build mesh objects.
   This unit is intended to create and draw some mesh objects using flexible functions
   with lots of options, including applying materials and textures for different facets.
   Started by Joen Joensen, who contributed procedures: BuildMeshCube, BuildMeshCylinder.
*)

interface

uses
  System.SysUtils,
  System.Classes,

  GLS.Scene,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.PersistentClasses,
  GLS.VectorFileObjects,
  GLS.MeshUtils;


type
  (*
    Properties of a hexahedron with 6 quad parts
  *)
  TGLHexahedronProperties = record
    Normal3f: TVector3f;
    VertexCoords: array [0 .. 3] of TVector3f;
    TextureCoords: array [0 .. 3] of TVector2f;
    PartIndices: array [0 .. 5] of Integer;
    Material: string;  // for 6 colors or cubic map from MaterialLibrary
  end;

  (*
    Properties of a sphere with two hemisphere parts
    using 5 points to make each hemisphere
  *)
  TGLSphereProperties = record
    Normal3f: TVector3f;
    VertexCoords: array [0 .. 4] of TVector3f;
    TextureCoords: array [0 .. 4] of TVector2f;
    PartIndices: array [0 .. 1] of Integer;
    Material: string; // for 2 colors or textures
  end;

  (*
    Properties of a cylinder with 8 parts for 2 hemicylinders
    using 6 points to make each part for hemidisk caps and side surfaces
  *)
  TGLCylinderProperties = record
    Normal3f: TVector3f;
    VertexCoords: array [0 .. 5] of TVector3f;
    TextureCoords: array [0 .. 5] of TVector2f;
    PartIndices: array [0 .. 7] of Integer;
    Material: string;  // for 8 colors or textures
  end;


type
  TGLMeshOptimizerOption = (mooStandardize, mooVertexCache, mooSortByMaterials,
    mooMergeObjects);
  TGLMeshOptimizerOptions = set of TGLMeshOptimizerOption;

// Hexahedron arrays for 6 parts
const
  cMeshHexahedron: array [0 .. 5] of TGLHexahedronProperties = ((
    // Front Quad
    VertexCoords: ((X: - 1; Y: - 1; Z: 1), (X: 1; Y: - 1; Z: 1), (X: 1; Y: 1;
    Z: 1), (X: - 1; Y: 1; Z: 1)); TextureCoords: ((X: 0; Y: 0), (X: 1; Y: 0), (X: 1;
    Y: 1), (X: 0; Y: 1)); PartIndices: (0, 1, 2, 2, 3, 0);
    Material: 'Front';), (
    // Back Quad
    VertexCoords: ((X: - 1; Y: - 1; Z: - 1), (X: - 1; Y: 1; Z: - 1), (X: 1; Y: 1;
    Z: - 1), (X: 1; Y: - 1; Z: - 1)); TextureCoords: ((X: 1; Y: 0), (X: 1;
    Y: 1), (X: 0; Y: 1), (X: 0; Y: 0)); PartIndices: (4, 5, 6, 6, 7, 4);
    Material: 'Back';), (
    // Top Quad
    VertexCoords: ((X: - 1; Y: 1; Z: - 1), (X: - 1; Y: 1; Z: 1), (X: 1; Y: 1;
    Z: 1), (X: 1; Y: 1; Z: - 1)); TextureCoords: ((X: 0; Y: 1), (X: 0; Y: 0), (X: 1;
    Y: 0), (X: 1; Y: 1)); PartIndices: (8, 9, 10, 10, 11, 8);
    Material: 'Top';), (
    // Bottom Quad
    VertexCoords: ((X: - 1; Y: - 1; Z: - 1), (X: 1; Y: - 1; Z: - 1), (X: 1; Y: - 1;
    Z: 1), (X: - 1; Y: - 1; Z: 1)); TextureCoords: ((X: 1; Y: 1), (X: 0;
    Y: 1), (X: 0; Y: 0), (X: 1; Y: 0)); PartIndices: (12, 13, 14, 14, 15, 12);
    Material: 'Bottom';), (
    // Right Quad
    VertexCoords: ((X: 1; Y: - 1; Z: - 1), (X: 1; Y: 1; Z: - 1), (X: 1; Y: 1;
    Z: 1), (X: 1; Y: - 1; Z: 1)); TextureCoords: ((X: 1; Y: 0), (X: 1; Y: 1), (X: 0;
    Y: 1), (X: 0; Y: 0)); PartIndices: (16, 17, 18, 18, 19, 16);
    Material: 'Right';), (
    // Left Quad
    VertexCoords: ((X: - 1; Y: - 1; Z: - 1), (X: - 1; Y: - 1; Z: 1), (X: - 1; Y: 1;
    Z: 1), (X: - 1; Y: 1; Z: - 1)); TextureCoords: ((X: 0; Y: 0), (X: 1;
    Y: 0), (X: 1; Y: 1), (X: 0; Y: 1)); PartIndices: (20, 21, 22, 22, 23, 20);
    Material: 'Left';));

// Sphere arrays for parts with 2 hemispheres
const
  cMeshSphere: array [0 .. 1] of TGLSphereProperties = ((
    // Top HemiSphere
    VertexCoords: ((X: - 1; Y: 1; Z: - 1), (X: - 1; Y: 1; Z: 1), (X: 1; Y: 1;
    Z: 1), (X: 1; Y: 1; Z: - 1), (X: 0; Y: 1; Z: 0)); TextureCoords: ((X: 0; Y: 1), (X: 0; Y: 0), (X: 1;
    Y: 0), (X: 1; Y: 1), (X: 1; Y: 1)); PartIndices: (0, 1);
    Material: 'Top';), (
    // Bottom HemiSphere
    VertexCoords: ((X: - 1; Y: - 1; Z: - 1), (X: 1; Y: - 1; Z: - 1), (X: 1; Y: - 1;
    Z: 1), (X: - 1; Y: - 1; Z: 1), (X: - 1; Y: - 1; Z: 1)); TextureCoords: ((X: 1; Y: 1), (X: 0;
    Y: 1), (X: 0; Y: 0), (X: 1; Y: 0), (X: 0; Y: 0)); PartIndices: (1, 2);
    Material: 'Bottom';));

(*
// Cylinder arrays
const
  cMeshCylinder: array [0 .. 3] of TGLCylinderProperties = ((
    // Front Disk
    VertexCoords: ((X: - 1; Y: - 1; Z: 1), (X: 1; Y: - 1; Z: 1), (X: 1; Y: 1;
    Z: 1), (X: - 1; Y: 1; Z: 1)); TextureCoords: ((X: 0; Y: 0), (X: 1; Y: 0), (X: 1;
    Y: 1), (X: 0; Y: 1)); PartIndices: (0, 1, 2);
    Material: 'Front';), (
    // Back Disk
    VertexCoords: ((X: - 1; Y: - 1; Z: - 1), (X: - 1; Y: 1; Z: - 1), (X: 1; Y: 1;
    Z: - 1), (X: 1; Y: - 1; Z: - 1)); TextureCoords: ((X: 1; Y: 0), (X: 1;
    Y: 1), (X: 0; Y: 1), (X: 0; Y: 0)); PartIndices: (3, 4, 5);
    Material: 'Back';), (
    // Top Dome
    VertexCoords: ((X: - 1; Y: 1; Z: - 1), (X: - 1; Y: 1; Z: 1), (X: 1; Y: 1;
    Z: 1), (X: 1; Y: 1; Z: - 1)); TextureCoords: ((X: 0; Y: 1), (X: 0; Y: 0), (X: 1;
    Y: 0), (X: 1; Y: 1)); PartIndices: (6, 7, 8);
    Material: 'Top';), (
    // Bottom Dome
    VertexCoords: ((X: - 1; Y: - 1; Z: - 1), (X: 1; Y: - 1; Z: - 1), (X: 1; Y: - 1;
    Z: 1), (X: - 1; Y: - 1; Z: 1)); TextureCoords: ((X: 1; Y: 1), (X: 0;
    Y: 1), (X: 0; Y: 0), (X: 1; Y: 0)); PartIndices: (9, 10, 11);
    Material: 'Bottom';));

// Tetrahedron part arrays
const
  cMeshTetrahedron: array [0 .. 3] of TGLFacetProperties = ((
    // Front HemiDisk
    VertexCoords: ((X: - 1; Y: - 1; Z: 1), (X: 1; Y: - 1; Z: 1), (X: 1; Y: 1;
    Z: 1), (X: - 1; Y: 1; Z: 1)); TextureCoords: ((X: 0; Y: 0), (X: 1; Y: 0), (X: 1;
    Y: 1), (X: 0; Y: 1)); FacetIndex: (0, 1, 2, 2, 3, 0);
    Material: 'Front';), (
    // Back HemiDisk
    VertexCoords: ((X: - 1; Y: - 1; Z: - 1), (X: - 1; Y: 1; Z: - 1), (X: 1; Y: 1;
    Z: - 1), (X: 1; Y: - 1; Z: - 1)); TextureCoords: ((X: 1; Y: 0), (X: 1;
    Y: 1), (X: 0; Y: 1), (X: 0; Y: 0)); FacetIndex: (4, 5, 6, 6, 7, 4);
    Material: 'Back';), (
    // Top HemiDome
    VertexCoords: ((X: - 1; Y: 1; Z: - 1), (X: - 1; Y: 1; Z: 1), (X: 1; Y: 1;
    Z: 1), (X: 1; Y: 1; Z: - 1)); TextureCoords: ((X: 0; Y: 1), (X: 0; Y: 0), (X: 1;
    Y: 0), (X: 1; Y: 1)); FacetIndex: (8, 9, 10, 10, 11, 8);
    Material: 'Top';), (
    // Bottom Square
    VertexCoords: ((X: - 1; Y: - 1; Z: - 1), (X: 1; Y: - 1; Z: - 1), (X: 1; Y: - 1;
    Z: 1), (X: - 1; Y: - 1; Z: 1)); TextureCoords: ((X: 1; Y: 1), (X: 0;
    Y: 1), (X: 0; Y: 0), (X: 1; Y: 0)); FacetIndex: (12, 13, 14, 14, 15, 12);
    Material: 'Bottom';));

*)
(* ---------------- Build Meshes ------------------- *)

procedure BuildMeshCube(Mesh : TGLMeshObject; const Position, Scale : TAffineVector);
procedure BuildMeshCylinder(Mesh : TGLMeshObject; const Position, Scale : TAffineVector; Slices : Integer);
procedure BuildMeshCylinderAdv(Mesh: TGLMeshObject;
  const Position, Scale: TAffineVector; TopRadius, BottomRadius, Height: single;
  Slices: Integer);
// Not implemented
procedure BuildMeshHemiSphere(Mesh: TGLMeshObject; const Position, Scale: TAffineVector);
procedure BuildMeshHemiCylinder(Mesh: TGLMeshObject; const Position, Scale: TAffineVector;
  Slices: Integer);

(* ---------------- Make Meshes ------------------- *)

procedure MakeMeshHexahedron(MeshObject: TGLMeshObject);
// Not implemented
procedure MakeMeshTetrahedron(MeshObject: TGLMeshObject);
procedure MakeMeshSphere(MeshObject: TGLMeshObject);

(* ------  -------- Mesh optimization --------------- *)

// Optimize Mesh (list, default options)
procedure OptimizeMesh(aList: TGLMeshObjectList; options: TGLMeshOptimizerOptions); overload;
procedure OptimizeMesh(aList: TGLMeshObjectList); overload;
// OptimizeMesh (object, with options)
procedure OptimizeMesh(aMeshObject: TGLMeshObject; options: TGLMeshOptimizerOptions); overload;
// OptimizeMesh (object, default options)
procedure OptimizeMesh(aMeshObject: TGLMeshObject); overload;
procedure FacesSmooth(aMeshObj: TGLMeshObject;
  aWeldDistance: Single = 0.0000001; aThreshold: Single = 35.0;
  InvertNormals: boolean = false);

var
  vDefaultMeshOptimizerOptions: TGLMeshOptimizerOptions = [mooStandardize,
    mooVertexCache, mooSortByMaterials, mooMergeObjects];

//------------------------------------------------------------------
implementation
//------------------------------------------------------------------

function VectorCombineWeighted(const Position, Scale: TAffineVector; X, Y, Z: single)
  : TAffineVector;

begin
  Result.X := Position.X + Scale.X * X;
  Result.Y := Position.Y + Scale.Y * Y;
  Result.Z := Position.Z + Scale.Z * Z;
end;

procedure BuildMeshCube(Mesh: TGLMeshObject;  const Position, Scale: TAffineVector);
var
  FGR : TFGVertexNormalTexIndexList;
  VertexOffset : Integer;
  NormalOffset : Integer;
  TextureOffset : Integer;
begin
  // Vertexes
  VertexOffset :=
  Mesh.Vertices.Add(VectorCombineWeighted(Position,Scale,0.5,0.5,0.5));
  Mesh.Vertices.Add(VectorCombineWeighted(Position,Scale,-0.5,0.5,0.5));
  Mesh.Vertices.Add(VectorCombineWeighted(Position,Scale,0.5,-0.5,0.5));
  Mesh.Vertices.Add(VectorCombineWeighted(Position,Scale,-0.5,-0.5,0.5));

  Mesh.Vertices.Add(VectorCombineWeighted(Position,Scale,0.5,0.5,-0.5));
  Mesh.Vertices.Add(VectorCombineWeighted(Position,Scale,-0.5,0.5,-0.5));
  Mesh.Vertices.Add(VectorCombineWeighted(Position,Scale,0.5,-0.5,-0.5));
  Mesh.Vertices.Add(VectorCombineWeighted(Position,Scale,-0.5,-0.5,-0.5));

  // Normals
  NormalOffset :=
  Mesh.Normals.Add(AffineVectorMake(0,0,1));
  Mesh.Normals.Add(AffineVectorMake(0,0,-1));
  Mesh.Normals.Add(AffineVectorMake(1,0,0));
  Mesh.Normals.Add(AffineVectorMake(-1,0,0));
  Mesh.Normals.Add(AffineVectorMake(0,1,0));
  Mesh.Normals.Add(AffineVectorMake(0,-1,0));

  // Texture Coordinates
  TextureOffset :=
  Mesh.TexCoords.Add(AffineVectorMake(1,1,1));
  Mesh.TexCoords.Add(AffineVectorMake(0,1,1));
  Mesh.TexCoords.Add(AffineVectorMake(1,0,1));
  Mesh.TexCoords.Add(AffineVectorMake(0,0,1));
  Mesh.TexCoords.Add(AffineVectorMake(1,1,0));
  Mesh.TexCoords.Add(AffineVectorMake(0,1,0));
  Mesh.TexCoords.Add(AffineVectorMake(1,0,0));
  Mesh.TexCoords.Add(AffineVectorMake(0,0,0));

  FGR := TFGVertexNormalTexIndexList.CreateOwned(Mesh.FaceGroups);
  FGR.Mode := fgmmTriangles;

  // Front
  FGR.VertexIndices.Add(VertexOffset + 0, VertexOffset + 1, VertexOffset + 3);
  FGR.VertexIndices.Add(VertexOffset + 0, VertexOffset + 3, VertexOffset + 2);
  FGR.NormalIndices.Add(NormalOffset + 0, NormalOffset + 0, NormalOffset + 0);
  FGR.NormalIndices.Add(NormalOffset + 0, NormalOffset + 0, NormalOffset + 0);
  FGR.TexCoordIndices.Add(TextureOffset + 0, TextureOffset + 1, TextureOffset + 3);
  FGR.TexCoordIndices.Add(TextureOffset + 0, TextureOffset + 3, TextureOffset + 2);

  // Back
  FGR.VertexIndices.Add(VertexOffset + 4, VertexOffset + 6, VertexOffset + 7);
  FGR.VertexIndices.Add(VertexOffset + 4, VertexOffset + 7, VertexOffset + 5);
  FGR.NormalIndices.Add(NormalOffset + 1, NormalOffset + 1, NormalOffset + 1);
  FGR.NormalIndices.Add(NormalOffset + 1, NormalOffset + 1, NormalOffset + 1);
  FGR.TexCoordIndices.Add(TextureOffset + 4, TextureOffset + 6, TextureOffset + 7);
  FGR.TexCoordIndices.Add(TextureOffset + 4, TextureOffset + 7, TextureOffset + 5);

  // Right
  FGR.VertexIndices.Add(VertexOffset + 0, VertexOffset + 2, VertexOffset + 6);
  FGR.VertexIndices.Add(VertexOffset + 0, VertexOffset + 6, VertexOffset + 4);
  FGR.NormalIndices.Add(NormalOffset + 2, NormalOffset + 2, NormalOffset + 2);
  FGR.NormalIndices.Add(NormalOffset + 2, NormalOffset + 2, NormalOffset + 2);
  FGR.TexCoordIndices.Add(TextureOffset + 0, TextureOffset + 2, TextureOffset + 6);
  FGR.TexCoordIndices.Add(TextureOffset + 0, TextureOffset + 6, TextureOffset + 4);

  // Left
  FGR.VertexIndices.Add(VertexOffset + 1, VertexOffset + 5, VertexOffset + 7);
  FGR.VertexIndices.Add(VertexOffset + 1, VertexOffset + 7, VertexOffset + 3);
  FGR.NormalIndices.Add(NormalOffset + 3, NormalOffset + 3, NormalOffset + 3);
  FGR.NormalIndices.Add(NormalOffset + 3, NormalOffset + 3, NormalOffset + 3);
  FGR.TexCoordIndices.Add(TextureOffset + 1, TextureOffset + 5, TextureOffset + 7);
  FGR.TexCoordIndices.Add(TextureOffset + 1, TextureOffset + 7, TextureOffset + 3);

  // Top
  FGR.VertexIndices.Add(VertexOffset + 0, VertexOffset + 4, VertexOffset + 5);
  FGR.VertexIndices.Add(VertexOffset + 0, VertexOffset + 5, VertexOffset + 1);
  FGR.NormalIndices.Add(NormalOffset + 4, NormalOffset + 4, NormalOffset + 4);
  FGR.NormalIndices.Add(NormalOffset + 4, NormalOffset + 4, NormalOffset + 4);
  FGR.TexCoordIndices.Add(TextureOffset + 0, TextureOffset + 4, TextureOffset + 5);
  FGR.TexCoordIndices.Add(TextureOffset + 0, TextureOffset + 5, TextureOffset + 1);

  // Bottom
  FGR.VertexIndices.Add(VertexOffset + 2, VertexOffset + 3, VertexOffset + 7);
  FGR.VertexIndices.Add(VertexOffset + 2, VertexOffset + 7, VertexOffset + 6);
  FGR.NormalIndices.Add(NormalOffset + 5, NormalOffset + 5, NormalOffset + 5);
  FGR.NormalIndices.Add(NormalOffset + 5, NormalOffset + 5, NormalOffset + 5);
  FGR.TexCoordIndices.Add(TextureOffset + 2, TextureOffset + 3, TextureOffset + 7);
  FGR.TexCoordIndices.Add(TextureOffset + 2, TextureOffset + 7, TextureOffset + 6);
end;

// Mesh cylinder
//
procedure BuildMeshCylinder(Mesh : TGLMeshObject; const Position, Scale : TAffineVector; Slices : Integer);
var
  FGR : TFGVertexNormalTexIndexList;
  VertexOffset : Integer;
  NormalOffset : Integer;
  TextureOffset : Integer;
  Cosine,Sine : array of Single;
  xc,yc : Integer;

begin
  if Slices < 3 then Exit;

  SetLength(Sine,Slices+1);
  SetLength(Cosine,Slices+1);
  PrepareSinCosCache(Sine,Cosine,0,360);

  VertexOffset  := Mesh.Vertices.Count;
  NormalOffset  := Mesh.Normals.Count;
  TextureOffset := Mesh.TexCoords.Count;
  for xc := 0 to Slices-1 do
  begin
    Mesh.Vertices.Add(VectorCombineWeighted(Position,Scale,0.5*cosine[xc],0.5*sine[xc],0.5));
    Mesh.Vertices.Add(VectorCombineWeighted(Position,Scale,0.5*cosine[xc],0.5*sine[xc],-0.5));
    // Normals
    Mesh.Normals.add(AffineVectorMake(cosine[xc],sine[xc],0));
    // Texture Coordinates
    Mesh.TexCoords.add(VectorCombineWeighted(Position,XYZVector,0.5*cosine[xc],0.5*sine[xc],0.5));
    Mesh.TexCoords.add(VectorCombineWeighted(Position,XYZVector,0.5*cosine[xc],0.5*sine[xc],-0.5));
  end;

  Mesh.Normals.add(AffineVectorMake(0,0,1));
  Mesh.Normals.add(AffineVectorMake(0,0,-1));

  FGR := TFGVertexNormalTexIndexList.CreateOwned(Mesh.FaceGroups);
  FGR.Mode := fgmmTriangles;
  for xc := 0 to Slices - 1 do
  begin
    yc := xc + 1;
    if yc = slices then yc := 0;

    FGR.VertexIndices.Add(VertexOffset + xc * 2, VertexOffset + xc * 2 + 1, VertexOffset + yc * 2 + 1);
    FGR.VertexIndices.Add(VertexOffset + xc * 2, VertexOffset + yc * 2 + 1, VertexOffset + yc * 2);
    FGR.NormalIndices.Add(NormalOffset + xc, NormalOffset + xc, NormalOffset + yc);
    FGR.NormalIndices.Add(NormalOffset + xc, NormalOffset + yc, NormalOffset + yc);
    FGR.TexCoordIndices.Add(TextureOffset + xc * 2, TextureOffset + xc * 2 + 1, TextureOffset + yc * 2 + 1);
    FGR.TexCoordIndices.Add(TextureOffset + xc * 2, TextureOffset + yc * 2 + 1, TextureOffset + yc * 2);
  End;

  for xc := 1 to Slices - 2 do
  begin
    yc := xc + 1;
    FGR.VertexIndices.Add(VertexOffset, VertexOffset + xc * 2, VertexOffset + yc * 2);
    FGR.VertexIndices.Add(VertexOffset + 1, VertexOffset + yc * 2 + 1, VertexOffset + xc * 2 + 1);
    FGR.NormalIndices.Add(NormalOffset + Slices, NormalOffset + Slices, NormalOffset + Slices);
    FGR.NormalIndices.Add(NormalOffset + Slices + 1, NormalOffset + Slices + 1, NormalOffset + Slices + 1);
    FGR.TexCoordIndices.Add(TextureOffset, TextureOffset + xc * 2, TextureOffset + yc * 2);
    FGR.TexCoordIndices.Add(TextureOffset + 1, TextureOffset + yc * 2 + 1, TextureOffset + xc * 2 + 1);
  end;
end;

//
// Mesh Cylinder with advanced options
//
procedure BuildMeshCylinderAdv(Mesh: TGLMeshObject;
  const Position, Scale: TAffineVector; TopRadius, BottomRadius, Height: single; Slices: Integer);
var
  FGR: TFGVertexNormalTexIndexList;
  VertexOffset: Integer;
  NormalOffset: Integer;
  TextureOffset: Integer;
  Cosine, Sine: array of single;
  xc, yc: Integer;

begin
  if Slices < 3 then
    Exit;

  SetLength(Sine, Slices + 1);
  SetLength(Cosine, Slices + 1);
  PrepareSinCosCache(Sine, Cosine, 0, 360);

  VertexOffset := Mesh.Vertices.Count;
  NormalOffset := Mesh.Normals.Count;
  TextureOffset := Mesh.TexCoords.Count;
  for xc := 0 to Slices - 1 do
  begin
    Mesh.Vertices.Add(VectorCombineWeighted(Position, Scale, TopRadius * 0.5 * Cosine[xc],
      TopRadius * 0.5 * Sine[xc], Height / 2));
    Mesh.Vertices.Add(VectorCombineWeighted(Position, Scale, BottomRadius * 0.5 * Cosine[xc],
      BottomRadius * 0.5 * Sine[xc], -Height / 2));
    // Normals
    Mesh.Normals.Add(AffineVectorMake(Cosine[xc], Sine[xc], 0));
    // Texture Coordinates
    Mesh.TexCoords.Add(VectorCombineWeighted(Position, XYZVector, TopRadius * 0.5 * Cosine[xc],
      TopRadius * 0.5 * Sine[xc], Height / 2));
    Mesh.TexCoords.Add(VectorCombineWeighted(Position, XYZVector, BottomRadius * 0.5 * Cosine[xc],
      BottomRadius * 0.5 * Sine[xc], -Height / 2));
  end;

  Mesh.Normals.Add(AffineVectorMake(0, 0, 1));
  Mesh.Normals.Add(AffineVectorMake(0, 0, -1));

  FGR := TFGVertexNormalTexIndexList.CreateOwned(Mesh.FaceGroups);
  FGR.Mode := fgmmTriangles;
  for xc := 0 to Slices - 1 do
  begin
    yc := xc + 1;
    if yc = Slices then
      yc := 0;
    FGR.VertexIndices.Add(VertexOffset + xc * 2, VertexOffset + xc * 2 + 1,
      VertexOffset + yc * 2 + 1);
    FGR.VertexIndices.Add(VertexOffset + xc * 2, VertexOffset + yc * 2 + 1, VertexOffset + yc * 2);
    FGR.NormalIndices.Add(NormalOffset + xc, NormalOffset + xc, NormalOffset + yc);
    FGR.NormalIndices.Add(NormalOffset + xc, NormalOffset + yc, NormalOffset + yc);
    FGR.TexCoordIndices.Add(TextureOffset + xc * 2, TextureOffset + xc * 2 + 1,
      TextureOffset + yc * 2 + 1);
    FGR.TexCoordIndices.Add(TextureOffset + xc * 2, TextureOffset + yc * 2 + 1,
      TextureOffset + yc * 2);
  end;

  for xc := 1 to Slices - 2 do
  begin
    yc := xc + 1;
    FGR.VertexIndices.Add(VertexOffset, VertexOffset + xc * 2, VertexOffset + yc * 2);
    FGR.VertexIndices.Add(VertexOffset + 1, VertexOffset + yc * 2 + 1, VertexOffset + xc * 2 + 1);
    FGR.NormalIndices.Add(NormalOffset + Slices, NormalOffset + Slices, NormalOffset + Slices);
    FGR.NormalIndices.Add(NormalOffset + Slices + 1, NormalOffset + Slices + 1,
      NormalOffset + Slices + 1);
    FGR.TexCoordIndices.Add(TextureOffset, TextureOffset + xc * 2, TextureOffset + yc * 2);
    FGR.TexCoordIndices.Add(TextureOffset + 1, TextureOffset + yc * 2 + 1,
      TextureOffset + xc * 2 + 1);
  end;
end;

// -----------------------------------------------------------
// Make Mesh Hexahedron
// -----------------------------------------------------------
procedure MakeMeshHexahedron(MeshObject: TGLMeshObject);
var
  FaceGroupList: TFGVertexIndexList;
  i, j: Integer;
begin
  for i := Low(cMeshHexahedron) to High(cMeshHexahedron) do
  begin
    // Add vertex coordinates
    for j := Low(cMeshHexahedron[i].VertexCoords) to High(cMeshHexahedron[i].VertexCoords) do
      MeshObject.Vertices.Add(cMeshHexahedron[i].VertexCoords[j]);
    // Add texture coordinates
    for j := Low(cMeshHexahedron[i].TextureCoords) to High(cMeshHexahedron[i].TextureCoords) do
      MeshObject.TexCoords.Add(cMeshHexahedron[i].TextureCoords[j]);

    FaceGroupList := TFGVertexIndexList.CreateOwned(MeshObject.FaceGroups);
    for j := Low(cMeshHexahedron[i].PartIndices) to High(cMeshHexahedron[i].PartIndices) do
      FaceGroupList.Add(cMeshHexahedron[i].PartIndices[j]);

    FaceGroupList.MaterialName := cMeshHexahedron[i].Material;
  end;
end;

// --------------------------------------------------------------------------------------

procedure BuildMeshHemiSphere(Mesh : TGLMeshObject; const Position, Scale : TAffineVector);
begin
  //
end;

procedure BuildMeshHemiCylinder(Mesh : TGLMeshObject; const Position, Scale : TAffineVector; Slices : Integer);
begin
  //
end;


// --------------------------------------------------------------------------------------
procedure MakeMeshSphere(MeshObject : TGLMeshObject);
begin
  //
end;

procedure MakeMeshTetrahedron(MeshObject: TGLMeshObject);
begin
  //
end;


// --------------- Mesh Optimization ---------------

procedure OptimizeMesh(aList: TGLMeshObjectList);
begin
  OptimizeMesh(aList, vDefaultMeshOptimizerOptions);
end;

procedure OptimizeMesh(aList: TGLMeshObjectList;
  options: TGLMeshOptimizerOptions);
var
  i, k: Integer;
  mob, mo: TGLMeshObject;
  fg: TGLFaceGroup;
  fgvi: TFGVertexIndexList;
begin
  // optimize all mesh objects
  for i := 0 to aList.Count - 1 do
  begin
    OptimizeMesh(aList[i], options);
  end;
  if (mooStandardize in options) then
  begin
    // drop mesh objects that have become empty
    for i := aList.Count - 1 downto 0 do
    begin
      if (aList[i].Mode = momFaceGroups) and (aList[i].FaceGroups.Count = 0)
      then
        aList[i].Free;
    end;
  end;
  if (aList.Count > 0) and (mooMergeObjects in options) then
  begin
    mob := aList[0];
    Assert(mob.Mode = momFaceGroups);
    for i := 1 to aList.Count - 1 do
    begin
      mo := aList[i];
      Assert(mo.Mode = momFaceGroups);
      k := mob.Vertices.Count;
      mob.Vertices.Add(mo.Vertices);
      mob.Normals.Add(mo.Normals);
      mob.TexCoords.Add(mo.TexCoords);
      while mo.FaceGroups.Count > 0 do
      begin
        fg := mo.FaceGroups[0];
        fgvi := (fg as TFGVertexIndexList);
        fgvi.Owner := mob.FaceGroups;
        mob.FaceGroups.Add(fgvi);
        mo.FaceGroups.Delete(0);
        fgvi.VertexIndices.Offset(k);
      end;
    end;
    for i := aList.Count - 1 downto 1 do
      aList[i].Free;
  end;
end;

procedure OptimizeMesh(aMeshObject: TGLMeshObject);
begin
  OptimizeMesh(aMeshObject, vDefaultMeshOptimizerOptions);
end;

procedure OptimizeMesh(aMeshObject: TGLMeshObject;
  options: TGLMeshOptimizerOptions);
var
  i: Integer;
  fg: TGLFaceGroup;
  coords, TexCoords, Normals: TGLAffineVectorList;
  il: TGLIntegerList;
  materialName: String;
begin
  if (mooMergeObjects in options) then
  begin
    if aMeshObject.Mode = momFaceGroups then
    begin
      // remove empty facegroups
      for i := aMeshObject.FaceGroups.Count - 1 downto 0 do
      begin
        fg := aMeshObject.FaceGroups[i];
        if fg.TriangleCount = 0 then
          fg.Free;
      end;
    end;
  end;

  if (mooStandardize in options) then
  begin
    if (aMeshObject.Mode <> momFaceGroups) or (aMeshObject.FaceGroups.Count <= 1)
    then
    begin
      if aMeshObject.FaceGroups.Count = 1 then
        materialName := aMeshObject.FaceGroups[0].materialName;
      TexCoords := TGLAffineVectorList.Create;
      Normals := TGLAffineVectorList.Create;
      coords := aMeshObject.ExtractTriangles(TexCoords, Normals);
      try
        il := BuildVectorCountOptimizedIndices(coords, Normals, TexCoords);
        try
          aMeshObject.Clear;
          if il.Count > 0 then
          begin
            RemapReferences(Normals, il);
            RemapReferences(TexCoords, il);
            RemapAndCleanupReferences(coords, il);
            aMeshObject.Vertices := coords;
            aMeshObject.Normals := Normals;
            aMeshObject.TexCoords := TexCoords;
            fg := TFGVertexIndexList.CreateOwned(aMeshObject.FaceGroups);
            fg.materialName := materialName;
            TFGVertexIndexList(fg).VertexIndices := il;
          end;
        finally
          il.Free;
        end;
      finally
        coords.Free;
        Normals.Free;
        TexCoords.Free;
      end;
    end
    else
      Assert(false,
        'Standardization with multiple facegroups not supported... yet.');
  end;

  if (mooVertexCache in options) and (aMeshObject.Mode = momFaceGroups) then
  begin
    for i := 0 to aMeshObject.FaceGroups.Count - 1 do
    begin
      fg := aMeshObject.FaceGroups[i];
      if fg.ClassType = TFGVertexIndexList then
        with TFGVertexIndexList(fg) do
        begin
          if Mode in [fgmmTriangles, fgmmFlatTriangles] then
            IncreaseCoherency(VertexIndices, 12);
        end;
    end;
  end;

  if mooSortByMaterials in options then
    aMeshObject.FaceGroups.SortByMaterial;
end;

procedure FacesSmooth(aMeshObj: TGLMeshObject;
  aWeldDistance: Single = 0.0000001; aThreshold: Single = 35.0;
  InvertNormals: boolean = false);
Var
  i, J, k, L: Integer;
  WeldedVertex: TGLAffineVectorList;
  TmpIntegerList: TGLIntegerList;
  IndexMap: TStringList;
  n: TAffineVector;
  indicesMap: TGLIntegerList;
  Index: Integer;
  FaceList: TGLIntegerList;
  NormalList: TGLAffineVectorList;
  FaceNormalList: TGLAffineVectorList;
  FaceGroup: TGLFaceGroup;
  fg, FG1: TFGVertexIndexList;
  Threshold: Single;
  Angle: Single;
  ReferenceMap: TGLIntegerList;
  ID1, ID2: Integer;
  Index1, Index2, Index3: Integer;

  function FindReferenceIndex(aID: Integer): Integer;
  begin
    Result := ReferenceMap[aID];
  end;
  function iMin(a, b: Integer): Integer;
  begin
    if a < b then
      Result := a
    else
      Result := b;
  end;
  function iMax(a, b: Integer): Integer;
  begin
    if a > b then
      Result := a
    else
      Result := b;
  end;

begin
  Threshold := aThreshold * Pi / 180.0;
  // build the vectices reference map
  ReferenceMap := TGLIntegerList.Create;
  WeldedVertex := TGLAffineVectorList.Create;
  WeldedVertex.Assign(aMeshObj.Vertices);
  indicesMap := TGLIntegerList.Create;
  // first of all, weld the very closed vertices
  WeldVertices(WeldedVertex, indicesMap, aWeldDistance);
  // then, rebuild the map list
  IndexMap := TStringList.Create;
  for i := 0 to WeldedVertex.Count - 1 do
  begin
    ReferenceMap.Assign(indicesMap);
    TmpIntegerList := TGLIntegerList.Create;
    Index := ReferenceMap.IndexOf(i);
    while Index >= 0 do
    begin
      TmpIntegerList.Add(Index);
      ReferenceMap[Index] := -99999;
      Index := ReferenceMap.IndexOf(i);
    end;
    IndexMap.AddObject(IntToStr(i), TmpIntegerList);
  end;
  ReferenceMap.Assign(indicesMap);
  // never used these, free them all
  WeldedVertex.Free;
  indicesMap.Free;
  // creates a TexPoint list for save face infomation, where s=facegroup index, t=face index
  FaceList := TGLIntegerList.Create;
  NormalList := TGLAffineVectorList.Create;
  FaceNormalList := TGLAffineVectorList.Create;
  // NormalIndex := TGLIntegerList.Create;
  for i := 0 to aMeshObj.FaceGroups.Count - 1 do
  begin
    FaceGroup := aMeshObj.FaceGroups[i];
    TmpIntegerList := TFGVertexIndexList(FaceGroup).VertexIndices;
    for J := 0 to (TmpIntegerList.Count div 3) - 1 do
    begin
      FaceList.Add(i);
      FaceList.Add(J);
      CalcPlaneNormal(aMeshObj.Vertices[TmpIntegerList[J * 3 + 0]],
        aMeshObj.Vertices[TmpIntegerList[J * 3 + 1]],
        aMeshObj.Vertices[TmpIntegerList[J * 3 + 2]], n);
      // add three normals for one trangle
      FaceNormalList.Add(n);
      NormalList.Add(n);
      NormalList.Add(n);
      NormalList.Add(n);
    end;
  end;
  // do smooth
  for i := 0 to (FaceList.Count div 2) - 1 do
  begin
    Index := FaceList[i * 2 + 0];
    Index1 := FaceList[i * 2 + 1];
    fg := TFGVertexIndexList(aMeshObj.FaceGroups[Index]);
    for J := 0 to 2 do
    begin
      for k := 0 to (FaceList.Count div 2) - 1 do
      begin
        Index2 := FaceList[k * 2 + 0];
        Index3 := FaceList[k * 2 + 1];
        FG1 := TFGVertexIndexList(aMeshObj.FaceGroups[Index2]);
        if i <> k then
        begin
          for L := 0 to 2 do
          begin
            // two face contain the same vertex
            ID1 := FindReferenceIndex(fg.VertexIndices[Index1 * 3 + J]);
            ID2 := FindReferenceIndex(FG1.VertexIndices[Index3 * 3 + L]);
            if ID1 = ID2 then
            begin
              Angle := VectorDotProduct(FaceNormalList[i], FaceNormalList[k]);
              if Angle > Threshold then
                NormalList[i * 3 + J] := VectorAdd(NormalList[i * 3 + J],
                  FaceNormalList[k]);
            end;
          end;
        end;
      end;
      n := NormalList[i * 3 + J];
      NormalizeVector(n);
      NormalList[i * 3 + J] := n;
    end;
  end;
  for i := 0 to (FaceList.Count div 2) - 1 do
  begin
    Index := FaceList[i * 2 + 0];
    fg := TFGVertexIndexList(aMeshObj.FaceGroups[Index]);
    Index := FaceList[i * 2 + 1];
    aMeshObj.Normals[fg.VertexIndices[(Index * 3 + 0)]] :=
      NormalList[(i * 3 + 0)];
    aMeshObj.Normals[fg.VertexIndices[(Index * 3 + 1)]] :=
      NormalList[(i * 3 + 1)];
    aMeshObj.Normals[fg.VertexIndices[(Index * 3 + 2)]] :=
      NormalList[(i * 3 + 2)];
    if InvertNormals then
    begin
      aMeshObj.Normals[fg.VertexIndices[(Index * 3 + 0)]] :=
        VectorNegate(aMeshObj.Normals[fg.VertexIndices[(Index * 3 + 0)]]);
      aMeshObj.Normals[fg.VertexIndices[(Index * 3 + 1)]] :=
        VectorNegate(aMeshObj.Normals[fg.VertexIndices[(Index * 3 + 1)]]);
      aMeshObj.Normals[fg.VertexIndices[(Index * 3 + 2)]] :=
        VectorNegate(aMeshObj.Normals[fg.VertexIndices[(Index * 3 + 2)]]);
    end;
  end;
  FaceList.Free;
  NormalList.Free;
  FaceNormalList.Free;
  ReferenceMap.Free;
  for i := 0 to IndexMap.Count - 1 do
    IndexMap.Objects[i].Free;
  IndexMap.Free;
end;


end.
