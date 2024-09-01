//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.MeshBuilder;

(*
  Build mesh objects.
  How often do you miss a BuildSphereMesh function for testing or editors?
  Well this unit is intended to solve that problem. We want fast,
  flexible functions with lots of options...
*)

interface

uses
  System.SysUtils,
  System.Classes,
  GXS.Scene,
  GXS.VectorFileObjects,
  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.VectorLists;

procedure BuildCube(Mesh: TgxMeshObject; Position, Scale: TAffineVector);
procedure BuildCylinder(Mesh: TgxMeshObject; Position, Scale: TAffineVector;
  Slices: Integer);
procedure BuildCylinder2(Mesh: TgxMeshObject; Position, Scale: TAffineVector;
  TopRadius, BottomRadius, Height: single; Slices: Integer);

// ----------------------------------------------------------------------------
implementation

// ----------------------------------------------------------------------------
function VectorCombineWeighted(Position, Scale: TAffineVector; X, Y, Z: single)
  : TAffineVector;

begin
  Result.X := Position.X + Scale.X * X;
  Result.Y := Position.Y + Scale.Y * Y;
  Result.Z := Position.Z + Scale.Z * Z;
end;

procedure BuildCube(Mesh: TgxMeshObject; Position, Scale: TAffineVector);
var
  FGR: TFGVertexNormalTexIndexList;
  VertexOffset: Integer;
  NormalOffset: Integer;
  TextureOffset: Integer;
begin
  // Vertexes
  VertexOffset := Mesh.Vertices.Add(VectorCombineWeighted(Position, Scale, 0.5,
    0.5, 0.5));
  Mesh.Vertices.Add(VectorCombineWeighted(Position, Scale, -0.5, 0.5, 0.5));
  Mesh.Vertices.Add(VectorCombineWeighted(Position, Scale, 0.5, -0.5, 0.5));
  Mesh.Vertices.Add(VectorCombineWeighted(Position, Scale, -0.5, -0.5, 0.5));

  Mesh.Vertices.Add(VectorCombineWeighted(Position, Scale, 0.5, 0.5, -0.5));
  Mesh.Vertices.Add(VectorCombineWeighted(Position, Scale, -0.5, 0.5, -0.5));
  Mesh.Vertices.Add(VectorCombineWeighted(Position, Scale, 0.5, -0.5, -0.5));
  Mesh.Vertices.Add(VectorCombineWeighted(Position, Scale, -0.5, -0.5, -0.5));

  // Normals
  NormalOffset := Mesh.Normals.Add(AffineVectorMake(0, 0, 1));
  Mesh.Normals.Add(AffineVectorMake(0, 0, -1));
  Mesh.Normals.Add(AffineVectorMake(1, 0, 0));
  Mesh.Normals.Add(AffineVectorMake(-1, 0, 0));
  Mesh.Normals.Add(AffineVectorMake(0, 1, 0));
  Mesh.Normals.Add(AffineVectorMake(0, -1, 0));

  // Texture Coordinates
  TextureOffset := Mesh.TexCoords.Add(AffineVectorMake(1, 1, 1));
  Mesh.TexCoords.Add(AffineVectorMake(0, 1, 1));
  Mesh.TexCoords.Add(AffineVectorMake(1, 0, 1));
  Mesh.TexCoords.Add(AffineVectorMake(0, 0, 1));
  Mesh.TexCoords.Add(AffineVectorMake(1, 1, 0));
  Mesh.TexCoords.Add(AffineVectorMake(0, 1, 0));
  Mesh.TexCoords.Add(AffineVectorMake(1, 0, 0));
  Mesh.TexCoords.Add(AffineVectorMake(0, 0, 0));

  FGR := TFGVertexNormalTexIndexList.CreateOwned(Mesh.FaceGroups);
  FGR.Mode := fgmmTriangles;

  // front
  FGR.VertexIndices.Add(VertexOffset + 0, VertexOffset + 1, VertexOffset + 3);
  FGR.VertexIndices.Add(VertexOffset + 0, VertexOffset + 3, VertexOffset + 2);

  FGR.NormalIndices.Add(NormalOffset + 0, NormalOffset + 0, NormalOffset + 0);
  FGR.NormalIndices.Add(NormalOffset + 0, NormalOffset + 0, NormalOffset + 0);

  FGR.TexCoordIndices.Add(TextureOffset + 0, TextureOffset + 1,
    TextureOffset + 3);
  FGR.TexCoordIndices.Add(TextureOffset + 0, TextureOffset + 3,
    TextureOffset + 2);

  // back
  FGR.VertexIndices.Add(VertexOffset + 4, VertexOffset + 6, VertexOffset + 7);
  FGR.VertexIndices.Add(VertexOffset + 4, VertexOffset + 7, VertexOffset + 5);

  FGR.NormalIndices.Add(NormalOffset + 1, NormalOffset + 1, NormalOffset + 1);
  FGR.NormalIndices.Add(NormalOffset + 1, NormalOffset + 1, NormalOffset + 1);

  FGR.TexCoordIndices.Add(TextureOffset + 4, TextureOffset + 6,
    TextureOffset + 7);
  FGR.TexCoordIndices.Add(TextureOffset + 4, TextureOffset + 7,
    TextureOffset + 5);

  // right
  FGR.VertexIndices.Add(VertexOffset + 0, VertexOffset + 2, VertexOffset + 6);
  FGR.VertexIndices.Add(VertexOffset + 0, VertexOffset + 6, VertexOffset + 4);

  FGR.NormalIndices.Add(NormalOffset + 2, NormalOffset + 2, NormalOffset + 2);
  FGR.NormalIndices.Add(NormalOffset + 2, NormalOffset + 2, NormalOffset + 2);

  FGR.TexCoordIndices.Add(TextureOffset + 0, TextureOffset + 2,
    TextureOffset + 6);
  FGR.TexCoordIndices.Add(TextureOffset + 0, TextureOffset + 6,
    TextureOffset + 4);

  // left
  FGR.VertexIndices.Add(VertexOffset + 1, VertexOffset + 5, VertexOffset + 7);
  FGR.VertexIndices.Add(VertexOffset + 1, VertexOffset + 7, VertexOffset + 3);

  FGR.NormalIndices.Add(NormalOffset + 3, NormalOffset + 3, NormalOffset + 3);
  FGR.NormalIndices.Add(NormalOffset + 3, NormalOffset + 3, NormalOffset + 3);

  FGR.TexCoordIndices.Add(TextureOffset + 1, TextureOffset + 5,
    TextureOffset + 7);
  FGR.TexCoordIndices.Add(TextureOffset + 1, TextureOffset + 7,
    TextureOffset + 3);

  // top
  FGR.VertexIndices.Add(VertexOffset + 0, VertexOffset + 4, VertexOffset + 5);
  FGR.VertexIndices.Add(VertexOffset + 0, VertexOffset + 5, VertexOffset + 1);

  FGR.NormalIndices.Add(NormalOffset + 4, NormalOffset + 4, NormalOffset + 4);
  FGR.NormalIndices.Add(NormalOffset + 4, NormalOffset + 4, NormalOffset + 4);

  FGR.TexCoordIndices.Add(TextureOffset + 0, TextureOffset + 4,
    TextureOffset + 5);
  FGR.TexCoordIndices.Add(TextureOffset + 0, TextureOffset + 5,
    TextureOffset + 1);

  // bottom
  FGR.VertexIndices.Add(VertexOffset + 2, VertexOffset + 3, VertexOffset + 7);
  FGR.VertexIndices.Add(VertexOffset + 2, VertexOffset + 7, VertexOffset + 6);

  FGR.NormalIndices.Add(NormalOffset + 5, NormalOffset + 5, NormalOffset + 5);
  FGR.NormalIndices.Add(NormalOffset + 5, NormalOffset + 5, NormalOffset + 5);

  FGR.TexCoordIndices.Add(TextureOffset + 2, TextureOffset + 3,
    TextureOffset + 7);
  FGR.TexCoordIndices.Add(TextureOffset + 2, TextureOffset + 7,
    TextureOffset + 6);
end;

procedure BuildCylinder(Mesh: TgxMeshObject; Position, Scale: TAffineVector;
  Slices: Integer);
var
  FGR: TFGVertexNormalTexIndexList;
  VertexOffset: Integer;
  NormalOffset: Integer;
  TextureOffset: Integer;
  Cosine, Sine: Array of single;
  xc, yc: Integer;

begin
  If Slices < 3 then
    Exit;

  SetLength(Sine, Slices + 1);
  SetLength(Cosine, Slices + 1);
  PrepareSinCosCache(Sine, Cosine, 0, 360);

  VertexOffset := Mesh.Vertices.Count;
  NormalOffset := Mesh.Normals.Count;
  TextureOffset := Mesh.TexCoords.Count;
  For xc := 0 to Slices - 1 do
  begin
    Mesh.Vertices.Add(VectorCombineWeighted(Position, Scale, 0.5 * Cosine[xc],
      0.5 * Sine[xc], 0.5));
    Mesh.Vertices.Add(VectorCombineWeighted(Position, Scale, 0.5 * Cosine[xc],
      0.5 * Sine[xc], -0.5));

    // Normals
    Mesh.Normals.Add(AffineVectorMake(Cosine[xc], Sine[xc], 0));

    // Texture Coordinates
    Mesh.TexCoords.Add(VectorCombineWeighted(Position, XYZVector,
      0.5 * Cosine[xc], 0.5 * Sine[xc], 0.5));
    Mesh.TexCoords.Add(VectorCombineWeighted(Position, XYZVector,
      0.5 * Cosine[xc], 0.5 * Sine[xc], -0.5));
  end;

  Mesh.Normals.Add(AffineVectorMake(0, 0, 1));
  Mesh.Normals.Add(AffineVectorMake(0, 0, -1));

  FGR := TFGVertexNormalTexIndexList.CreateOwned(Mesh.FaceGroups);
  FGR.Mode := fgmmTriangles;
  for xc := 0 to Slices - 1 do
  begin
    yc := xc + 1;
    If yc = Slices then
      yc := 0;

    FGR.VertexIndices.Add(VertexOffset + xc * 2, VertexOffset + xc * 2 + 1,
      VertexOffset + yc * 2 + 1);
    FGR.VertexIndices.Add(VertexOffset + xc * 2, VertexOffset + yc * 2 + 1,
      VertexOffset + yc * 2);

    FGR.NormalIndices.Add(NormalOffset + xc, NormalOffset + xc,
      NormalOffset + yc);
    FGR.NormalIndices.Add(NormalOffset + xc, NormalOffset + yc,
      NormalOffset + yc);

    FGR.TexCoordIndices.Add(TextureOffset + xc * 2, TextureOffset + xc * 2 + 1,
      TextureOffset + yc * 2 + 1);
    FGR.TexCoordIndices.Add(TextureOffset + xc * 2, TextureOffset + yc * 2 + 1,
      TextureOffset + yc * 2);
  end;

  for xc := 1 to Slices - 2 do
  begin
    yc := xc + 1;
    FGR.VertexIndices.Add(VertexOffset, VertexOffset + xc * 2,
      VertexOffset + yc * 2);
    FGR.VertexIndices.Add(VertexOffset + 1, VertexOffset + yc * 2 + 1,
      VertexOffset + xc * 2 + 1);

    FGR.NormalIndices.Add(NormalOffset + Slices, NormalOffset + Slices,
      NormalOffset + Slices);
    FGR.NormalIndices.Add(NormalOffset + Slices + 1, NormalOffset + Slices + 1,
      NormalOffset + Slices + 1);

    FGR.TexCoordIndices.Add(TextureOffset, TextureOffset + xc * 2,
      TextureOffset + yc * 2);
    FGR.TexCoordIndices.Add(TextureOffset + 1, TextureOffset + yc * 2 + 1,
      TextureOffset + xc * 2 + 1);
  end;

end;

procedure BuildCylinder2(Mesh: TgxMeshObject; Position, Scale: TAffineVector;
  TopRadius, BottomRadius, Height: single; Slices: Integer);
var
  FGR: TFGVertexNormalTexIndexList;
  VertexOffset: Integer;
  NormalOffset: Integer;
  TextureOffset: Integer;
  Cosine, Sine: Array of single;
  xc, yc: Integer;

begin
  If Slices < 3 then
    Exit;

  SetLength(Sine, Slices + 1);
  SetLength(Cosine, Slices + 1);
  PrepareSinCosCache(Sine, Cosine, 0, 360);

  VertexOffset := Mesh.Vertices.Count;
  NormalOffset := Mesh.Normals.Count;
  TextureOffset := Mesh.TexCoords.Count;
  for xc := 0 to Slices - 1 do
  Begin
    Mesh.Vertices.Add(VectorCombineWeighted(Position, Scale,
      TopRadius * 0.5 * Cosine[xc], TopRadius * 0.5 * Sine[xc], Height / 2));
    Mesh.Vertices.Add(VectorCombineWeighted(Position, Scale,
      BottomRadius * 0.5 * Cosine[xc], BottomRadius * 0.5 * Sine[xc],
      -Height / 2));

    // Normals
    Mesh.Normals.Add(AffineVectorMake(Cosine[xc], Sine[xc], 0));

    // Texture Coordinates
    Mesh.TexCoords.Add(VectorCombineWeighted(Position, XYZVector,
      TopRadius * 0.5 * Cosine[xc], TopRadius * 0.5 * Sine[xc], Height / 2));
    Mesh.TexCoords.Add(VectorCombineWeighted(Position, XYZVector,
      BottomRadius * 0.5 * Cosine[xc], BottomRadius * 0.5 * Sine[xc],
      -Height / 2));
  end;

  Mesh.Normals.Add(AffineVectorMake(0, 0, 1));
  Mesh.Normals.Add(AffineVectorMake(0, 0, -1));

  FGR := TFGVertexNormalTexIndexList.CreateOwned(Mesh.FaceGroups);
  FGR.Mode := fgmmTriangles;
  for xc := 0 to Slices - 1 do
  begin
    yc := xc + 1;
    If yc = Slices then
      yc := 0;

    FGR.VertexIndices.Add(VertexOffset + xc * 2, VertexOffset + xc * 2 + 1,
      VertexOffset + yc * 2 + 1);
    FGR.VertexIndices.Add(VertexOffset + xc * 2, VertexOffset + yc * 2 + 1,
      VertexOffset + yc * 2);

    FGR.NormalIndices.Add(NormalOffset + xc, NormalOffset + xc,
      NormalOffset + yc);
    FGR.NormalIndices.Add(NormalOffset + xc, NormalOffset + yc,
      NormalOffset + yc);

    FGR.TexCoordIndices.Add(TextureOffset + xc * 2, TextureOffset + xc * 2 + 1,
      TextureOffset + yc * 2 + 1);
    FGR.TexCoordIndices.Add(TextureOffset + xc * 2, TextureOffset + yc * 2 + 1,
      TextureOffset + yc * 2);
  end;

  for xc := 1 to Slices - 2 do
  begin
    yc := xc + 1;
    FGR.VertexIndices.Add(VertexOffset, VertexOffset + xc * 2,
      VertexOffset + yc * 2);
    FGR.VertexIndices.Add(VertexOffset + 1, VertexOffset + yc * 2 + 1,
      VertexOffset + xc * 2 + 1);

    FGR.NormalIndices.Add(NormalOffset + Slices, NormalOffset + Slices,
      NormalOffset + Slices);
    FGR.NormalIndices.Add(NormalOffset + Slices + 1, NormalOffset + Slices + 1,
      NormalOffset + Slices + 1);

    FGR.TexCoordIndices.Add(TextureOffset, TextureOffset + xc * 2,
      TextureOffset + yc * 2);
    FGR.TexCoordIndices.Add(TextureOffset + 1, TextureOffset + yc * 2 + 1,
      TextureOffset + xc * 2 + 1);
  end;

end;

end.
