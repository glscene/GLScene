//
// This unit is part of the GLScene Engine, http://glscene.org
//
{
  Standard polyhedrons
}
unit GLPolyhedron;

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  System.Classes,

  GLScene,
  GLVectorGeometry,
  GLObjects,
  GLVectorFileObjects,
  GLMesh,
  GLRenderContextInfo;

type

  { The tetrahedron has no texture coordinates defined, ie. without using
    a texture generation mode, no texture will be mapped. }
  TGLTetrahedron = class(TGLBaseMesh)
  public
    procedure BuildList(var rci: TGLRenderContextInfo); override;
  end;

  {The octahedron has no texture coordinates defined, ie. without using
    a texture generation mode, no texture will be mapped. }
  TGLOctahedron = class(TGLBaseMesh)
  public
    procedure BuildList(var rci: TGLRenderContextInfo); override;
  end;

  {The hexahedron has no texture coordinates defined, ie. without using
    a texture generation mode, no texture will be mapped. }
  TGLHexahedron = class(TGLBaseMesh)
  public
    procedure BuildList(var rci: TGLRenderContextInfo); override;
  end;

  {The dodecahedron has no texture coordinates defined, ie. without using
   a texture generation mode, no texture will be mapped. }
  TGLDodecahedron = class(TGLBaseMesh)
  public
    procedure BuildList(var rci: TGLRenderContextInfo); override;
  end;

  {The icosahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TGLIcosahedron = class(TGLBaseMesh)
  public
    procedure BuildList(var rci: TGLRenderContextInfo); override;
  end;


//-------------------------------------------------------------
implementation
//-------------------------------------------------------------


//--------------------
//--------------------  TGLTetrahedron ------------------------
//--------------------

procedure TGLTetrahedron.BuildList(var rci: TGLRenderContextInfo);
const
  Vertices: packed array [0 .. 3] of TAffineVector =
       ((X: 1.0;  Y: 1.0;  Z: 1.0),
        (X: 1.0;  Y: -1.0; Z: -1.0),
        (X: -1.0; Y: 1.0;  Z: -1.0),
        (X: -1.0; Y: -1.0; Z: 1.0));
  Triangles: packed array [0 .. 3] of packed array [0 .. 2] of Byte =
    ((0, 1, 3),
     (2, 1, 0),
     (3, 2, 0),
     (1, 2, 3));
var
  i, j: Integer;
  n: TAffineVector;
  faceIndices: PByteArray;
begin
  for i := 0 to 3 do
  begin
    faceIndices := @triangles[i, 0];
    n := CalcPlaneNormal(vertices[faceIndices^[0]], vertices[faceIndices^[1]],
      vertices[faceIndices^[2]]);
    glNormal3fv(@n);
    glBegin(GL_TRIANGLES);
      for j := 0 to 2 do
        glVertex3fv(@vertices[faceIndices^[j]]);
    glEnd;
  end;
end;

//--------------------
//--------------------  TGLOctahedron ------------------------
//--------------------

procedure TGLOctahedron.BuildList(var rci: TGLRenderContextInfo);
const
  Vertices: packed array [0 .. 5] of TAffineVector =
      ((X: 1.0; Y: 0.0; Z: 0.0),
       (X:-1.0; Y: 0.0; Z: 0.0),
       (X: 0.0; Y: 1.0; Z: 0.0),
       (X: 0.0; Y: -1.0; Z: 0.0),
       (X: 0.0; Y: 0.0; Z: 1.0),
       (X: 0.0; Y: 0.0; Z: -1.0));
  Triangles: packed array [0 .. 7] of packed array [0 .. 2] of Byte =
    ((0, 4, 2),
     (1, 2, 4),
     (0, 3, 4),
     (1, 4, 3),
     (0, 2, 5),
     (1, 5, 2),
     (0, 5, 3),
     (1, 3, 5));

var
  i, j: Integer;
  n: TAffineVector;
  faceIndices: PByteArray;
begin
  for i := 0 to 7 do
  begin
    faceIndices := @triangles[i, 0];

    n := CalcPlaneNormal(vertices[faceIndices^[0]], vertices[faceIndices^[1]],
      vertices[faceIndices^[2]]);
    glNormal3fv(@n);
    glBegin(GL_TRIANGLES);
    for j := 0 to 2 do
      glVertex3fv(@vertices[faceIndices^[j]]);
    glEnd;
  end;
end;

// ------------------
// ------------------ TGLHexahedron ------------------
// ------------------

procedure TGLHexahedron.BuildList(var rci: TGLRenderContextInfo);
const
  Vertices: packed array [0 .. 7] of TAffineVector =
   ((X:-1; Y:-1; Z:-1),
    (X: 1; Y:-1; Z:-1),
    (X: 1; Y:-1; Z: 1),
    (X:-1; Y:-1; Z: 1),
    (X:-1; Y: 1; Z:-1),
    (X: 1; Y: 1; Z:-1),
    (X: 1; Y: 1; Z: 1),
    (X:-1; Y: 1; Z: 1));
  Quadrangles: packed array [0 .. 5] of packed array [0 .. 3] of Byte =
   ((0, 1, 2, 3),
    (3, 2, 6, 7),
    (7, 6, 5, 4),
    (4, 5, 1, 0),
    (0, 3, 7, 4),
    (1, 5, 6, 2));

var
  i, j: Integer;
  n: TAffineVector;
  faceIndices: PByteArray;
begin
  for i := 0 to 4 do
  begin
    faceIndices := @Quadrangles[i, 0];
    n := CalcPlaneNormal(vertices[faceIndices^[0]], vertices[faceIndices^[1]], vertices[faceIndices^[2]]);
    glNormal3fv(@n);
    glBegin(GL_QUADS);
      for j := 0 to 7 do
      glVertex3fv(@vertices[faceIndices^[j]]);
    glEnd;
  end;
end;

// ------------------
// ------------------ TGLDodecahedron ------------------
// ------------------

procedure TGLDodecahedron.BuildList(var rci: TGLRenderContextInfo);
const
  A = 1.61803398875 * 0.3; // (Sqrt(5)+1)/2
  B = 0.61803398875 * 0.3; // (Sqrt(5)-1)/2
  C = 1 * 0.3;
const
  Vertices: packed array [0 .. 19] of TAffineVector =
   ((X: - A; Y: 0; Z: B), (X: - A; Y: 0; Z: - B), (X: A; Y: 0; Z: - B),
    (X: A; Y: 0; Z: B), (X: B; Y: - A; Z: 0), (X: - B; Y: - A; Z: 0),
    (X: - B; Y: A; Z: 0), (X: B; Y: A; Z: 0), (X: 0; Y: B; Z: - A),
    (X: 0; Y: - B; Z: - A), (X: 0; Y: - B; Z: A), (X: 0; Y: B; Z: A),
    (X: - C; Y: - C; Z: C), (X: - C; Y: - C; Z: - C), (X: C; Y: - C; Z: - C),
    (X: C; Y: - C; Z: C), (X: - C; Y: C; Z: C), (X: - C; Y: C; Z: - C),
    (X: C; Y: C; Z: - C), (X: C; Y: C; Z: C));

  Polygons: packed array [0 .. 11] of packed array [0 .. 4] of Byte =
   ((0, 12, 10, 11, 16), (1, 17, 8, 9, 13), (2, 14, 9, 8, 18),
    (3, 19, 11, 10, 15), (4, 14, 2, 3, 15), (5, 12, 0, 1, 13),
    (6, 17, 1, 0, 16), (7, 19, 3, 2, 18), (8, 17, 6, 7, 18),
    (9, 14, 4, 5, 13), (10, 12, 5, 4, 15), (11, 19, 7, 6, 16));
var
  i, j: Integer;
  n: TAffineVector;
  faceIndices: PByteArray;
begin
  for i := 0 to 11 do
  begin
    faceIndices := @polygons[i, 0];
    n := CalcPlaneNormal(vertices[faceIndices^[0]], vertices[faceIndices^[1]],
      vertices[faceIndices^[2]]);
    glNormal3fv(@n);

//    glBegin(GL_TRIANGLE_FAN);
//    for j := 0 to 4 do
//      glVertex3fv(@vertices[faceIndices^[j]]);
//    glEnd;

    glBegin(GL_TRIANGLES);
    for j := 1 to 3 do
    begin
      glVertex3fv(@vertices[faceIndices^[0]]);
      glVertex3fv(@vertices[faceIndices^[j]]);
      glVertex3fv(@vertices[faceIndices^[j+1]]);
    end;
    glEnd;
  end;
end;

// ------------------
// ------------------ TGLIcosahedron ------------------
// ------------------

procedure TGLIcosahedron.BuildList(var rci: TGLRenderContextInfo);
const
  A = 0.5;
  B = 0.30901699437; // 1/(1+Sqrt(5))
const
  Vertices: packed array [0 .. 11] of TAffineVector =
   ((X: 0; Y: - B; Z: - A), (X: 0; Y: - B; Z: A), (X: 0; Y: B; Z: - A),
    (X: 0; Y: B; Z: A), (X: - A; Y: 0; Z: - B), (X: - A; Y: 0; Z: B),
    (X: A; Y: 0; Z: - B), (X: A; Y: 0; Z: B), (X: - B; Y: - A; Z: 0),
    (X: - B; Y: A; Z: 0), (X: B; Y: - A; Z: 0), (X: B; Y: A; Z: 0));
  Triangles: packed array [0 .. 19] of packed array [0 .. 2] of Byte =
   ((2, 9, 11), (3, 11, 9), (3, 5, 1), (3, 1, 7), (2, 6, 0),
    (2, 0, 4), (1, 8, 10), (0, 10, 8), (9, 4, 5), (8, 5, 4), (11, 7, 6),
    (10, 6, 7), (3, 9, 5), (3, 7, 11), (2, 4, 9), (2, 11, 6), (0, 8, 4),
    (0, 6, 10), (1, 5, 8), (1, 10, 7));

var
  i, j: Integer;
  n: TAffineVector;
  faceIndices: PByteArray;
begin
  for i := 0 to 19 do
  begin
    faceIndices := @triangles[i, 0];

    n := CalcPlaneNormal(vertices[faceIndices^[0]], vertices[faceIndices^[1]],
      vertices[faceIndices^[2]]);
    glNormal3fv(@n);

    glBegin(GL_TRIANGLES);
    for j := 0 to 2 do
      glVertex3fv(@vertices[faceIndices^[j]]);
    glEnd;
  end;
end;


//==================================================================
initialization
//==================================================================

  RegisterClasses([TGLDodecahedron, TGLIcosahedron, TGLHexahedron, TGLOctahedron, TGLTetrahedron]);

end.

