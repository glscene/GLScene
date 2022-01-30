//
// The graphics rendering engine GLScene http://glscene.org
//

unit Formats.OCT;

(* Loader for FSRad OCT files *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,

  GLS.VectorGeometry,
  GLS.VectorTypes,
  GLS.VectorLists;

type

  TOCTHeader = record
    numVerts: Integer;
    numFaces: Integer;
    numTextures: Integer;
    numLightmaps: Integer;
    numLights: Integer;
  end;

  TOCTVertex = record
    tv: TTexPoint; // texture coordinates
    lv: TTexPoint; // lightmap coordinates
    pos: TAffineVector; // vertex position
  end;

  TOCTFace = record
    start: Integer; // first face vert in vertex array
    num: Integer; // number of verts in the face
    id: Integer; // texture index into the texture array
    lid: Integer; // lightmap index into the lightmap array
    p: THmgPlane;
  end;

  POCTFace = ^TOCTFace;

  TOCTTexture = record
    id: Integer; // texture id
    Name: array [0 .. 63] of AnsiChar; // texture name
  end;

  TOCTLightmap = record
    id: Integer; // lightmaps id
    map: array [0 .. 49151] of Byte; // 128 x 128 raw RGB data
  end;

  POCTLightmap = ^TOCTLightmap;

  TOCTLight = record
    pos: TAffineVector; // Position
    color: TAffineVector; // Color (RGB)
    intensity: Integer; // Intensity
  end;

  TOCTFile = class(TObject)
  public
    Header: TOCTHeader;
    Vertices: array of TOCTVertex;
    Faces: array of TOCTFace;
    Textures: array of TOCTTexture;
    Lightmaps: array of TOCTLightmap;
    Lights: array of TOCTLight;
    PlayerPos: TAffineVector;
    constructor Create; overload;
    constructor Create(octStream: TStream); overload;
    (* Saves content to stream in OCT format.
      The Header is automatically prepared before streaming. *)
    procedure SaveToStream(aStream: TStream);
    procedure AddTriangles(vertexCoords: TGLAffineVectorList;
      texMapCoords: TGLAffineVectorList; const textureName: String);
    procedure AddLight(const lightPos: TAffineVector; const lightColor: TGLVector;
      lightIntensity: Integer);
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

uses
  GLS.MeshUtils;

// ------------------
// ------------------ TOCTFile ------------------
// ------------------

constructor TOCTFile.Create;
begin
  inherited Create;
end;

constructor TOCTFile.Create(octStream: TStream);
begin
  inherited Create;
  // Read in the header
  octStream.Read(Header, SizeOf(Header));
  // then the rest of the stuff
  SetLength(Vertices, Header.numVerts);
  octStream.Read(Vertices[0], Header.numVerts * SizeOf(TOCTVertex));
  SetLength(Faces, Header.numFaces);
  octStream.Read(Faces[0], Header.numFaces * SizeOf(TOCTFace));
  SetLength(Textures, Header.numTextures);
  octStream.Read(Textures[0], Header.numTextures * SizeOf(TOCTTexture));
  SetLength(Lightmaps, Header.numLightmaps);
  octStream.Read(Lightmaps[0], Header.numLightmaps * SizeOf(TOCTLightmap));
  SetLength(Lights, Header.numLights);
  octStream.Read(Lights[0], Header.numLights * SizeOf(TOCTLight));
  octStream.Read(PlayerPos, SizeOf(PlayerPos))
end;

procedure TOCTFile.SaveToStream(aStream: TStream);
begin
  with Header, aStream do
  begin
    numVerts := Length(Vertices);
    numFaces := Length(Faces);
    numTextures := Length(Textures);
    numLightmaps := Length(Lightmaps);
    numLights := Length(Lights);

    Write(Header, SizeOf(Header));
    Write(Vertices[0], numVerts * SizeOf(TOCTVertex));
    Write(Faces[0], numFaces * SizeOf(TOCTFace));
    Write(Textures[0], numTextures * SizeOf(TOCTTexture));
    Write(Lightmaps[0], numLightmaps * SizeOf(TOCTLightmap));
    Write(Lights[0], numLights * SizeOf(TOCTLight));
    Write(PlayerPos, SizeOf(PlayerPos))
  end;
end;

procedure TOCTFile.AddTriangles(vertexCoords: TGLAffineVectorList;
  texMapCoords: TGLAffineVectorList; const textureName: String);
var
  i: Integer;
  baseIdx, texIdx: Integer;
begin
  Assert((texMapCoords = nil) or (texMapCoords.Count = vertexCoords.Count));

  texIdx := Length(Textures);
  SetLength(Textures, texIdx + 1);
  Move(textureName[1], Textures[texIdx].Name[0], Length(textureName));
  SetLength(Lightmaps, 1);
  FillChar(Lightmaps[0].map[0], 128 * 3, 255);

  baseIdx := Length(Vertices);
  SetLength(Vertices, baseIdx + vertexCoords.Count);
  for i := 0 to vertexCoords.Count - 1 do
    with Vertices[baseIdx + i] do
    begin
      pos := vertexCoords.List[i];
      if Assigned(texMapCoords) then
        tv := PTexPoint(@texMapCoords.List[i])^;
    end;

  SetLength(Faces, vertexCoords.Count div 3);
  i := 0;
  while i < vertexCoords.Count do
  begin
    with Faces[i div 3] do
    begin
      start := baseIdx + i;
      num := 3;
      id := texIdx;
      p := PlaneMake(vertexCoords[i], CalcPlaneNormal(vertexCoords[i + 0],
        vertexCoords[i + 1], vertexCoords[i + 0]));
    end;
    Inc(i, 3);
  end;
end;

procedure TOCTFile.AddLight(const lightPos: TAffineVector;
  const lightColor: TGLVector; lightIntensity: Integer);
var
  n: Integer;
begin
  n := Length(Lights);
  SetLength(Lights, n + 1);
  with Lights[n] do
  begin
    pos := lightPos;
    color := PAffineVector(@lightColor)^;
    intensity := lightIntensity;
  end;
end;

end.
