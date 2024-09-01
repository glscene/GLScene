//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.FileOBJ;

(*
  Support-Code to load Wavefront OBJ Files into TgxFreeForm-Components
  in GLScene.
  Note that you must manually add this unit to one of your project's uses
  to enable support for OBJ & OBJF at run-time.
*)

{ .$DEFINE STATS } // Define to display statistics after loading.

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,

  System.Classes,
  System.SysUtils,

  GXS.XOpenGL,
  GXS.VectorTypes,
  GXS.Strings,
  GXS.Context,
  GXS.MeshUtils,
  GXS.Utils,
  GXS.ApplicationFileIO,
  GXS.PersistentClasses,
  GXS.VectorGeometry,
  GXS.Scene,
  GXS.VectorFileObjects,
  GXS.VectorLists,
  GXS.Texture,
  GXS.Color,
  GXS.RenderContextInfo,
  GXS.Material;

const
  BufSize = 10240; // Load input data in chunks of BufSize Bytes.
  LineLen = 100; // Allocate memory for the current line in chunks of LineLen Bytes

type

  TgxOBJVectorFile = class(TgxVectorFile)
  private
    FSourceStream: TStream; // Load from this stream
    FBuffer: AnsiString; // Buffer
    FLine: string; // current line
    FLineNo: Integer; // current Line number - for error messages
    FEof: Boolean; // Stream done?
    FBufPos: Integer; // Position in the buffer
  protected
    // Read a single line of text from the source stream, set FEof to true when done.
    procedure ReadLine;
    // Raise a class-specific exception
    procedure Error(const msg: string);
    procedure CalcMissingOBJNormals(mesh: TgxMeshObject);
  public
    class function Capabilities: TDataFileCapabilities; override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
  end;

  EGLOBJFileError = class(Exception)
  private
    FLineNo: Integer;
  public
    property LineNo: Integer read FLineNo;
  end;

  (* A simple class that know how to extract infos from a mtl file.
    mtl files are companion files of the obj, they store material
    information. Guessed content (imported ones denoted with a '*',
    please help if you know more):
    materials begin with a 'newmtl' command followed by material name
    *Kd command defines the diffuse color
    *map_Kd command defines the diffuse texture image
    *Ka/map_Ka define the ambient color and texmap
    *Ks/map_Ks define the specular color and texmap
    *Ke/map_Ke define the self-illumination/lightmap texmap
    map_Bump specifies the bump map
    *d specifies transparency (alpha-channel, range [0; 1])
    map_d specifies the opcaity texture map
    Ns defines the specular exponent or shininess or phong specular (?)
    Ni is the refraction index (greater than 1)
    *illum defines the illumination model (0 for no lighting, 1 for
    ambient and diffuse, 2 for full lighting) *)
  TgxMTLFile = class(TStringList)
  public
    procedure Prepare;
    function MaterialStringProperty(const materialName, propertyName: string): string;
    function MaterialVectorProperty(const materialName, propertyName: string; const defaultValue: TVector4f): TVector4f;
  end;

var
  // If enabled, main mesh will be splitted into multiple mesh from facegroup data.
  vFileOBJ_SplitMesh: Boolean = False;

// ===================================================================
implementation
// ===================================================================

function StreamEOF(S: TStream): Boolean;
begin
  // Is the stream at its end?
  Result := (S.Position >= S.Size);
end;

function Rest(const S: string; Count: Integer): string;
// Return the right part of s including s[Count].
begin
  Result := copy(S, Count, Length(S) - Count + 1);
end;

function NextToken(var S: string; delimiter: Char): string;
(* Return the next Delimiter-delimited Token from the string s and
  remove it from s *)
var
  p: Integer;
begin
  p := Pos(delimiter, S);
  if p = 0 then
  begin
    Result := S;
    S := '';
  end
  else
  begin
    Result := copy(S, 1, p - 1);
    S := TrimLeft(Rest(S, p + 1));
  end;
end;

(**** TOBJFGVertexNormalTexIndexList ****
  - based on TFGVertexNormalTexIndexList (GXS.VectorFileObjects.pas)
  - adds support for polygons and for "missing" normals and
  texture-coordinates. Pass -1 to Add for the index of a missing object.
  - Polygons are defined by counting off the number of vertices added to the
  PolygonVertices-property. So a PolygonVertices-List of
  [3,4,6]
  says "Vertex indices 0,1 and 2 make up a triangle, 3,4,5 and 6 a quad and
  7,8,9,10,11 and 12 a hexagon". *)

type
  TOBJFGMode = (objfgmmPolygons, objfgmmTriangleStrip);

  TOBJFGVertexNormalTexIndexList = class(TFGVertexNormalTexIndexList)
  private
    FMode: TOBJFGMode;
    FName: string;
    FPolygonVertices: TgxIntegerList;
    FCurrentVertexCount: Integer;
    FShowNormals: Boolean;
    procedure PolygonComplete; (* Current polygon completed. Adds FCurrentVertexCount
      to FPolygonVertices and sets the variable to 0 *)
    procedure SetMode(aMode: TOBJFGMode);
  public
    procedure Assign(Source: TPersistent); override;
    constructor CreateOwned(aOwner: TgxFaceGroups); override;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    procedure Add(VertexIdx, NormalIdx, TexCoordIdx: Integer);
    procedure BuildList(var mrci: TgxRenderContextInfo); override;
    procedure AddToTriangles(aList: TgxAffineVectorList; aTexCoords: TgxAffineVectorList = nil;
      aNormals: TgxAffineVectorList = nil); override;
    function TriangleCount: Integer; override;
    property Mode: TOBJFGMode read FMode write SetMode;
    property Name: string read FName write FName;
    property PolygonVertices: TgxIntegerList read FPolygonVertices;
    property ShowNormals: Boolean read FShowNormals write FShowNormals;
  end;

constructor TOBJFGVertexNormalTexIndexList.CreateOwned(aOwner: TgxFaceGroups);
begin
  inherited CreateOwned(aOwner);
  FMode := objfgmmTriangleStrip;
  // FShowNormals:=True;
end;

destructor TOBJFGVertexNormalTexIndexList.Destroy;
begin
  FPolygonVertices.Free;
  inherited Destroy;
end;

procedure TOBJFGVertexNormalTexIndexList.Add(VertexIdx, NormalIdx, TexCoordIdx: Integer);
begin
  inherited Add(VertexIdx, NormalIdx, TexCoordIdx);
  inc(FCurrentVertexCount);
end;

procedure TOBJFGVertexNormalTexIndexList.PolygonComplete;
begin
  Assert(FMode = objfgmmPolygons, 'PolygonComplete may only be called for Facegroups with Mode=objfgmmPolygons.');
  FPolygonVertices.Add(FCurrentVertexCount);
  FCurrentVertexCount := 0;
end;

procedure TOBJFGVertexNormalTexIndexList.SetMode(aMode: TOBJFGMode);
begin
  if aMode = FMode then
    exit;
  Assert(VertexIndices.Count = 0, 'Decide on the mode before adding vertices.');
  FMode := aMode;
  if FMode = objfgmmPolygons then
    FPolygonVertices := TgxIntegerList.Create
  else
  begin
    FPolygonVertices.Free;
    FPolygonVertices := nil;
  end;
end;

procedure TOBJFGVertexNormalTexIndexList.BuildList(var mrci: TgxRenderContextInfo);
var
  VertexPool: PAffineVectorArray;
  NormalPool: PAffineVectorArray;
  TexCoordPool: PAffineVectorArray;
  ColorPool: PVectorArray;
  GotColor: Boolean;

  procedure BuildPolygons;
  var
    Polygon, Index, j, idx: Integer;
    N: TAffineVector;
  begin
    // Build it. Ignoring texture-coordinates and normals that are missing.
    Index := 0; // Current index into the Index-Lists.
    // For every Polygon
    for Polygon := 0 to FPolygonVertices.Count - 1 do
    begin
      glBegin(GL_POLYGON);
      try
        // For every Vertex in the current Polygon
        for j := 0 to FPolygonVertices[Polygon] - 1 do
        begin
          Assert(NormalIndices.List <> nil);
          idx := NormalIndices.List^[Index];
          if idx >= 0 then
            glNormal3fv(@NormalPool[idx]);

          if GotColor then
            glColor4fv(@ColorPool[VertexIndices.List^[Index]]);

          if Assigned(TexCoordPool) then
          begin
            idx := TexCoordIndices.List^[Index];
            if idx >= 0 then
            begin
              glMultiTexCoord2fv(GL_TEXTURE0, @TexCoordPool[idx]);
              glMultiTexCoord2fv(GL_TEXTURE1, @TexCoordPool[idx]);
            end;
          end;
          glVertex3fv(@VertexPool[VertexIndices.List^[Index]]);
          inc(Index);
        end;
      finally
        glEnd;
      end;
    end;

    // Visible normals, rather moronic and mainly for debugging.
    if FShowNormals then
    begin
      Index := 0;
      for Polygon := 0 to FPolygonVertices.Count - 1 do
      begin
        // For every Vertex in the current Polygon
        for j := 0 to FPolygonVertices[Polygon] - 1 do
        begin
          idx := NormalIndices.List^[Index];
          if idx <> -1 then
          begin
            glBegin(GL_LINES);
            try
              glVertex3fv(@VertexPool^[VertexIndices.List^[Index]]);
              N := VectorAdd(VertexPool^[VertexIndices.List^[Index]], VectorScale(NormalPool^[idx], 0.1));
              glVertex3fv(@N);
            finally
              glEnd;
            end;
          end;
          inc(Index);
        end;
      end;
    end;
  end;

  procedure BuildTriangleStrip;
  (*
    begin
    Owner.Owner.DeclareArraysToOpenGL(False);
    glDrawElements(GL_TRIANGLE_STRIP,VertexIndices.Count,
    GL_UNSIGNED_INT,VertexIndices.List);
    end;
  *)
  var
    Index, idx: Integer;
  begin
    // Build it. Ignoring texture-coordinates and normals that are missing.
    glBegin(GL_TRIANGLE_STRIP);
    try
      for Index := 0 to VertexIndices.Count - 1 do
      begin
        idx := NormalIndices.List^[Index];
        if idx <> -1 then
          glNormal3fv(@NormalPool^[idx]);

        if Assigned(TexCoordPool) then
        begin
          idx := TexCoordIndices.List^[Index];
          if idx <> -1 then
            xglTexCoord2fv(@TexCoordPool^[idx]);
        end;

        glVertex3fv(@VertexPool^[VertexIndices.List^[Index]]);
      end;
    finally
      glEnd;
    end;
  end;

begin
  Assert(((TexCoordIndices.Count = 0) or (VertexIndices.Count <= TexCoordIndices.Count)) and
    (VertexIndices.Count <= NormalIndices.Count),
    'Number of Vertices does not match number of Normals or Texture coordinates.');

  // Shorthand notations.
  VertexPool := Owner.Owner.Vertices.List;
  NormalPool := Owner.Owner.Normals.List;
  ColorPool := Owner.Owner.Colors.List;

  if TexCoordIndices.Count = 0 then
    TexCoordPool := nil
  else
    TexCoordPool := Owner.Owner.TexCoords.List;

  GotColor := (Owner.Owner.Vertices.Count = Owner.Owner.Colors.Count);

  case FMode of
    objfgmmPolygons:
      BuildPolygons;
    objfgmmTriangleStrip:
      BuildTriangleStrip;
  end;
end;

procedure TOBJFGVertexNormalTexIndexList.AddToTriangles(aList: TgxAffineVectorList; aTexCoords: TgxAffineVectorList = nil;
  aNormals: TgxAffineVectorList = nil);
var
  i, j, N, n0: Integer;
  vertexList, texCoordList, normalsList: TgxAffineVectorList;
begin
  vertexList := Owner.Owner.Vertices;
  texCoordList := Owner.Owner.TexCoords;
  normalsList := Owner.Owner.Normals;
  case FMode of
    objfgmmPolygons:
      begin
        N := 0;
        for i := 0 to FPolygonVertices.Count - 1 do
        begin
          n0 := N;
          for j := 0 to FPolygonVertices[i] - 1 do
          begin
            if j > 1 then
            begin
              aList.Add(vertexList[VertexIndices[n0]], vertexList[VertexIndices[N - 1]], vertexList[VertexIndices[N]]);
              if Assigned(aTexCoords) then
              begin
                if texCoordList.Count > 0 then
                  aTexCoords.Add(texCoordList[VertexIndices[n0]], texCoordList[VertexIndices[N - 1]],
                    texCoordList[VertexIndices[N]])
                else
                  aTexCoords.AddNulls(3);
              end;
              if Assigned(aNormals) then
              begin
                if normalsList.Count > 0 then
                  aNormals.Add(normalsList[VertexIndices[n0]], normalsList[VertexIndices[N - 1]], normalsList[VertexIndices[N]])
                else
                  aNormals.AddNulls(3);
              end;
            end;
            inc(N);
          end;
        end;
      end;
    objfgmmTriangleStrip:
      begin
        ConvertStripToList(vertexList, VertexIndices, aList);
        N := (VertexIndices.Count - 2) * 3;
        if Assigned(aTexCoords) then
        begin
          if texCoordList.Count > 0 then
            ConvertStripToList(texCoordList, VertexIndices, aTexCoords)
          else
            aTexCoords.AddNulls(N);
        end;
        if Assigned(aNormals) then
        begin
          if normalsList.Count > 0 then
            ConvertStripToList(normalsList, VertexIndices, aNormals)
          else
            aNormals.AddNulls(N);
        end;
      end;
  else
    Assert(False);
  end;
end;

function TOBJFGVertexNormalTexIndexList.TriangleCount: Integer;
var
  i: Integer;
begin
  case FMode of
    objfgmmPolygons:
      begin
        Result := 0;
        for i := 0 to FPolygonVertices.Count - 1 do
        begin
          Result := Result + FPolygonVertices[i] - 2;
        end;
      end;
    objfgmmTriangleStrip:
      begin
        Result := VertexIndices.Count - 2;
        if Result < 0 then
          Result := 0;
      end;
  else
    Result := 0;
    Assert(False);
  end;
end;

// ------------------
// ------------------ TgxOBJVectorFile ------------------
// ------------------

procedure TgxOBJVectorFile.ReadLine;
var
  j: Integer;

  procedure FillBuffer;
  var
    l: Integer;
  begin
    l := FSourceStream.Size - FSourceStream.Position;
    if l > BufSize then
      l := BufSize;
    SetLength(FBuffer, l);
    FSourceStream.Read(FBuffer[1], l);
    FBufPos := 1;
  end;

begin
  inc(FLineNo);

  if FBufPos < 1 then
    FillBuffer;

  j := 1;
  while True do
  begin
    if FBufPos > Length(FBuffer) then
    begin
      if StreamEOF(FSourceStream) then
      begin
        FEof := True;
        break;
      end
      else
        FillBuffer
    end
    else
    begin
      case FBuffer[FBufPos] of
        #10, #13:
          begin
            inc(FBufPos);
            if FBufPos > Length(FBuffer) then
              if StreamEOF(FSourceStream) then
                break
              else
                FillBuffer;
            if (FBuffer[FBufPos] = #10) or (FBuffer[FBufPos] = #13) then
              inc(FBufPos);
            break;
          end;
      else
        if j > Length(FLine) then
          SetLength(FLine, Length(FLine) + LineLen);
        if FBuffer[FBufPos] = #9 then
          FLine[j] := #32
        else
          FLine[j] := Char(FBuffer[FBufPos]);
        inc(FBufPos);
        inc(j);
      end;
    end;
  end;

  SetLength(FLine, j - 1);
end;

procedure TgxOBJVectorFile.Error(const msg: string);
var
  E: EGLOBJFileError;
begin
  E := EGLOBJFileError.Create(msg);
  E.FLineNo := FLineNo;
  raise E;
end;

class function TgxOBJVectorFile.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

procedure TgxOBJVectorFile.CalcMissingOBJNormals(mesh: TgxMeshObject);
var
  VertexPool: PAffineVectorArray;
  N: TAffineVector;
  p: array [1 .. 3] of PAffineVector;
  face, Index: Integer;
  fg: TOBJFGVertexNormalTexIndexList;

  procedure DoCalcNormal;
  var
    idx: Integer;
  begin
    idx := TOBJFGVertexNormalTexIndexList(mesh.FaceGroups[face]).NormalIndices.List^[Index];
    if idx < 0 then
    begin
      N := CalcPlaneNormal(p[1]^, p[2]^, p[3]^);
      idx := mesh.Normals.Add(N);
      TOBJFGVertexNormalTexIndexList(mesh.FaceGroups[face]).NormalIndices.List^[Index] := idx;
    end;
  end;

  procedure CalcForPolygons;
  var
    Polygon, firstVertexIndex, j: Integer;
  begin
    with fg do
    begin
      (* Walk the polygons and calculate normals for those vertices that
        are missing. *)
      Index := 0; { Current index into the Index-List of this Facegroup. }

      // For every Polygon
      for Polygon := 0 to FPolygonVertices.Count - 1 do
      begin
        // Init
        firstVertexIndex := Index;
        FillChar(p, SizeOf(p), 0);
        // Last Vertex in this polygon
        p[2] := @VertexPool^[VertexIndices.List^[Index + FPolygonVertices[Polygon] - 1]];
        // First Vertex in this polygon
        p[3] := @VertexPool^[VertexIndices.List^[Index]];
        // For every Vertex in the current Polygon but the last.
        for j := 0 to FPolygonVertices[Polygon] - 2 do
        begin
          Move(p[2], p[1], 2 * SizeOf(PAffineVector));
          p[3] := @VertexPool^[VertexIndices.List^[Index + 1]];
          DoCalcNormal;
          inc(Index);
        end;

        // For the last vertex use the first as partner to span the plane.
        Move(p[2], p[1], 2 * SizeOf(PAffineVector));
        p[3] := @VertexPool^[VertexIndices.List^[firstVertexIndex]];
        DoCalcNormal;
        inc(Index);
      end; // of for FPolygonVertices
    end; // of with Facegroup
  end;

  procedure CalcForTriangleStrip;
  begin
  end;

begin
  // Shorthand notations.
  VertexPool := mesh.Vertices.List;

  for face := 0 to mesh.FaceGroups.Count - 1 do
  begin
    fg := TOBJFGVertexNormalTexIndexList(mesh.FaceGroups[face]);
    case fg.Mode of
      objfgmmPolygons:
        CalcForPolygons;
      objfgmmTriangleStrip:
        CalcForTriangleStrip;
    end;
  end;
end;

procedure TgxOBJVectorFile.LoadFromStream(aStream: TStream);
var
  hv: THomogeneousVector;
  av: TAffineVector;
  mesh: TgxMeshObject;
  faceGroup: TOBJFGVertexNormalTexIndexList;
  faceGroupNames: TStringList;

  procedure ReadHomogeneousVector;
  // Read a vector with a maximum of 4 elements from the current line.
  var
    i, c: Integer;
    f: string;
  begin
    FillChar(hv, SizeOf(hv), 0);
    i := 0;
    while (FLine <> '') and (i < 4) do
    begin
      f := NextToken(FLine, ' ');
      Val(f, hv.V[i], c);
      if c <> 0 then
        Error(Format('''%s'' is not a valid floating-point constant.', [f]));
      inc(i);
    end;
  end;

  procedure ReadAffineVector;
  // Read a vector with a maximum of 3 elements from the current line.
  var
    i, c: Integer;
    f: string;
  begin
    FillChar(av, SizeOf(av), 0);
    i := 0;
    while (FLine <> '') and (i < 3) do
    begin
      f := NextToken(FLine, ' ');
      Val(f, av.V[i], c);
      if c <> 0 then
        Error(Format('''%s'' is not a valid floating-point constant.', [f]));
      inc(i);
    end;
  end;

  procedure SetCurrentFaceGroup(aName: string; const matlName: string);
  var
    i: Integer;
  begin
    i := faceGroupNames.IndexOf(aName);
    if i >= 0 then
    begin
      faceGroup := TOBJFGVertexNormalTexIndexList(faceGroupNames.Objects[i]);

      if faceGroup.materialName <> matlName then
      begin
        i := faceGroupNames.IndexOf(aName);
        if i >= 0 then
        begin
          faceGroup := TOBJFGVertexNormalTexIndexList(faceGroupNames.Objects[i]);
          if faceGroup.materialName <> matlName then
            faceGroup := nil;
        end;
      end;
    end;

    if (faceGroup = nil) or (faceGroup.Name <> aName) or (faceGroup.PolygonVertices.Count > 0) or
      (faceGroup.materialName <> matlName) then
    begin
      faceGroup := TOBJFGVertexNormalTexIndexList.CreateOwned(mesh.FaceGroups);
      faceGroup.FName := aName;
      faceGroup.Mode := objfgmmPolygons;
      faceGroup.materialName := matlName;
      faceGroupNames.AddObject(aName, faceGroup);
    end;
  end;

  procedure AddFaceVertex(faceVertices: string);

    function GetIndex(Count: Integer): Integer;
    var
      S: string;
    begin
      S := NextToken(faceVertices, '/');
      Result := StrToIntDef(S, 0);
      if Result = 0 then
        Result := -1 // Missing
      else if Result < 0 then
      begin
        // Relative, make absolute. "-1" means last, "-2" second last.
        Result := Count + Result
      end
      else
      begin
        // Absolute, correct for zero-base.
        Dec(Result);
      end;
    end;

  var
    vIdx, tIdx, nIdx: Integer;
  begin
    vIdx := GetIndex(mesh.Vertices.Count);
    tIdx := GetIndex(mesh.TexCoords.Count);
    nIdx := GetIndex(mesh.Normals.Count);

    faceGroup.Add(vIdx, nIdx, tIdx);
  end;

  procedure ReadFace(const curMtlName: string);
  var
    faceVertices: string;
  begin
    if FLine <> '' then
    begin
      if not Assigned(faceGroup) then
        SetCurrentFaceGroup('', curMtlName);
      try
        while FLine <> '' do
        begin
          faceVertices := NextToken(FLine, ' ');
          AddFaceVertex(faceVertices);
        end;
      finally
        faceGroup.PolygonComplete;
      end;
    end;
  end;

  procedure ReadTriangleStripContinued;
  var
    faceVertices: string;
  begin
    if faceGroup = nil then
      Error('q-line without preceding t-line.');
    while FLine <> '' do
    begin
      faceVertices := NextToken(FLine, ' ');
      AddFaceVertex(faceVertices);
    end;
  end;

  procedure ReadTriangleStrip;
  begin
    // Start a new Facegroup, mode=triangle strip
    faceGroup := TOBJFGVertexNormalTexIndexList.CreateOwned(mesh.FaceGroups);
    faceGroup.Mode := objfgmmTriangleStrip;
    // The rest is the same as for continuation of a strip.
    ReadTriangleStripContinued;
  end;

  function GetOrAllocateMaterial(const libName, matName: string): string;
  var
    fs: TStream;
    objMtl: TgxMTLFile;
    matLib: TgxMaterialLibrary;
    libMat, libMat2: TgxLibMaterial;
    texName: string;
    libFilename: string;
  begin
    if GetOwner is TgxBaseMesh then
    begin
      // got a linked material library?
      matLib := TgxBaseMesh(GetOwner).MaterialLibrary;
      if Assigned(matLib) then
      begin
        Result := matName;
        libMat := matLib.Materials.GetLibMaterialByName(matName);
        if not Assigned(libMat) then
        begin
          // spawn a new material
          libMat := matLib.Materials.Add;
          libMat.Name := matName;

          // get full path for material file to be load
          if matLib.TexturePaths = EmptyStr then
            libFilename := libName
          else
            libFilename := IncludeTrailingPathDelimiter(matLib.TexturePaths) + libName;

          try
            fs := TFileStream.Create(libFilename, fmOpenReadWrite);
          except
            fs := nil;
          end;
          if Assigned(fs) then
          begin
            objMtl := TgxMTLFile.Create;
            try
              objMtl.LoadFromStream(fs);
              objMtl.Prepare;
              // setup material colors
              with libMat.Material.FrontProperties do
              begin
                Ambient.Color := objMtl.MaterialVectorProperty(matName, 'Ka', clrGray20);
                Diffuse.Color := objMtl.MaterialVectorProperty(matName, 'Kd', clrGray80);
                Diffuse.Alpha := StrToFloatDef(objMtl.MaterialStringProperty(matName, 'd'), 1);
                if Diffuse.Alpha < 1 then
                  libMat.Material.BlendingMode := bmTransparency;
                case StrToIntDef(objMtl.MaterialStringProperty(matName, 'illum'), 1) of
                  0:
                    begin // non-lit material
                      libMat.Material.MaterialOptions := [moNoLighting];
                    end;
                  1:
                    ; // flat, non-shiny material
                  2:
                    begin // specular material
                      Specular.Color := objMtl.MaterialVectorProperty(matName, 'Ks', clrTransparent);
                    end;
                else
                  // unknown, assume unlit
                  libMat.Material.MaterialOptions := [moNoLighting];
                  Diffuse.Color := clrGray80;
                end;
                Shininess := StrToIntDef(objMtl.MaterialStringProperty(matName, 'Ns'), 1);
              end;
              // setup texture
              texName := objMtl.MaterialStringProperty(matName, 'map_Kd');
              if texName <> '' then
              begin
                try
                  with libMat.Material.Texture do
                  begin
                    Image.LoadFromFile(texName);
                    Disabled := False;
                    TextureMode := tmModulate;
                  end;
                except
                  on E: ETexture do
                  begin
                    if not Owner.IgnoreMissingTextures then
                      raise;
                  end;
                end;
              end;
              // setup lightmap (self-illumination) texture
              texName := objMtl.MaterialStringProperty(matName, 'map_Ke');
              if texName <> '' then
              begin
                // spawn a new material
                libMat2 := matLib.Materials.Add;
                libMat2.Name := matName + '_lm';
                // Use the GLScene built-in second texture support (note: the mesh LightmapProperty MUST be empty)
                libMat.Texture2Name := libMat2.Name;
                try
                  with libMat2.Material.Texture do
                  begin
                    Image.LoadFromFile(texName);
                    Disabled := False;
                    minFilter := miLinear;
                    TextureWrap := twNone;
                    TextureFormat := tfRGB;
                    TextureMode := tmModulate;
                  end;
                except
                  on E: ETexture do
                  begin
                    if not Owner.IgnoreMissingTextures then
                      raise;
                  end;
                end;
              end;

            finally
              objMtl.Free;
              fs.Free;
            end;
          end;
        end
        else
          Result := matName;
      end
      else
        Result := '';
    end;
  end;

  procedure SplitMesh;
  var
    i, j, Count: Integer;
    newMesh: TgxMeshObject;
    newfaceGroup: TOBJFGVertexNormalTexIndexList;
    VertexIdx, NormalIdx, TexCoordIdx: Integer;
    AffineVector: TAffineVector;
  begin
    for i := 0 to mesh.FaceGroups.Count - 1 do
    begin
      faceGroup := mesh.FaceGroups[i] as TOBJFGVertexNormalTexIndexList;

      newMesh := TgxMeshObject.CreateOwned(Owner.MeshObjects);
      newMesh.Mode := momFaceGroups;
      newMesh.Name := faceGroup.Name;

      newfaceGroup := TOBJFGVertexNormalTexIndexList.CreateOwned(newMesh.FaceGroups);
      newfaceGroup.Assign(faceGroup);
      newfaceGroup.FName := faceGroup.Name;
      newfaceGroup.Mode := faceGroup.Mode;
      newfaceGroup.materialName := faceGroup.materialName;

      // SendInteger('VertexIndices', faceGroup.VertexIndices.Count);
      // SendInteger('TexCoords', faceGroup.TexCoordIndices.Count);
      // SendInteger('Normals', faceGroup.NormalIndices.Count);

      Count := faceGroup.VertexIndices.Count;
      for j := 0 to Count - 1 do
      begin
        VertexIdx := faceGroup.VertexIndices[j];
        AffineVector := mesh.Vertices[VertexIdx];
        VertexIdx := newMesh.Vertices.Add(AffineVector);

        TexCoordIdx := faceGroup.TexCoordIndices[j];
        AffineVector := mesh.TexCoords[TexCoordIdx];
        TexCoordIdx := newMesh.TexCoords.Add(AffineVector);

        NormalIdx := faceGroup.NormalIndices[j];
        AffineVector := mesh.Normals[NormalIdx];
        NormalIdx := newMesh.Normals.Add(AffineVector);

        newfaceGroup.Add(VertexIdx, NormalIdx, TexCoordIdx);
      end;

    end;

    Owner.MeshObjects.RemoveAndFree(mesh);
  end;

var
  command, objMtlFileName, curMtlName: string;
{$IFDEF STATS}
  t0, t1, t2: Integer;
{$ENDIF}
begin
{$IFDEF STATS}
  t0 := GLGetTickCount;
{$ENDIF}
  FEof := False;
  FSourceStream := aStream;
  FLineNo := 0;
  objMtlFileName := '';
  curMtlName := '';

  mesh := TgxMeshObject.CreateOwned(Owner.MeshObjects);
  mesh.Mode := momFaceGroups;

  faceGroupNames := TStringList.Create;
  faceGroupNames.Duplicates := dupAccept;
  faceGroupNames.Sorted := True;
  try

    faceGroup := nil;

    while not FEof do
    begin
      ReadLine;
      if FLine = '' then
        Continue; // Skip blank line
      if CharInSet(FLine[1], ['#', '$']) then
        Continue; // Skip comment and alternate comment

      command := AnsiUpperCase(NextToken(FLine, ' '));

      if command = 'V' then
      begin
        ReadHomogeneousVector;
        mesh.Vertices.Add(hv.X, hv.Y, hv.Z);
      end
      else if command = 'VT' then
      begin
        ReadAffineVector;
        mesh.TexCoords.Add(av.X, av.Y, 0);
      end
      else if command = 'VN' then
      begin
        ReadAffineVector;
        mesh.Normals.Add(av.X, av.Y, av.Z);
      end
      else if command = 'VP' then
      begin
        // Parameter Space Vertex: Ignore
      end
      else if command = 'G' then
      begin
        // Only the first name on the line, multiple groups not supported.
        SetCurrentFaceGroup(NextToken(FLine, ' '), curMtlName);
      end
      else if command = 'F' then
      begin
        ReadFace(curMtlName);
      end
      else if command = 'O' then
      begin
        // Object Name:  Ignore
      end
      else if command = 'MTLLIB' then
      begin
        objMtlFileName := NextToken(FLine, ' ');
      end
      else if command = 'USEMTL' then
      begin
        curMtlName := GetOrAllocateMaterial(objMtlFileName, NextToken(FLine, ' '));
        if faceGroup = nil then
          SetCurrentFaceGroup('', curMtlName)
        else
          SetCurrentFaceGroup(faceGroup.FName, curMtlName);
      end
      else if command = 'S' then
      begin
        // Smooth Group: Ignore
      end
      else if command = 'T' then
      begin
        ReadTriangleStrip;
      end
      else if command = 'Q' then
      begin
        ReadTriangleStripContinued;
      end
      else
        Error('Unsupported Command ''' + command + '''');
    end;

    mesh.FaceGroups.SortByMaterial;

{$IFDEF STATS}
    t1 := GLGetTickCount;
{$ENDIF}
    CalcMissingOBJNormals(mesh);

{$IFDEF STATS}
    t2 := GLGetTickCount;
    InformationDlg(Format('TgxOBJVectorFile Loaded in %dms'#13 + #13 + '    %dms spent reading'#13 +
      '    %dms spent calculating normals'#13 + '    %d Geometric Vertices'#13 + '    %d Texture Vertices'#13 +
      '    %d Normals'#13 + '    %d FaceGroups/Strips', [t2 - t0, t1 - t0, t2 - t1, mesh.Vertices.Count, mesh.TexCoords.Count,
      mesh.Normals.Count, mesh.FaceGroups.Count]));
{$ENDIF}
    if vFileOBJ_SplitMesh then
      SplitMesh;

  finally
    faceGroupNames.Free;
  end;
end;

procedure TgxOBJVectorFile.SaveToStream(aStream: TStream);
var
  OldDecimalSeparator: Char;

  procedure Write(const S: AnsiString);
  begin
    if S <> '' then
      aStream.Write(S[1], Length(S));
  end;

  procedure WriteLn(const S: string);
  begin
    Write(AnsiString(S));
    Write(#13#10);
  end;

  procedure WriteHeader;
  begin
    WriteLn('# OBJ-File exported by GLScene');
    WriteLn('');
  end;

  procedure WriteVertices;
  var
    S: string;
    j, i, N: Integer;
  begin
    N := 0;
    for j := 0 to Owner.MeshObjects.Count - 1 do
    begin
      WriteLn(Format('# Mesh %d', [j + 1]));
      with Owner.MeshObjects[j].Vertices do
      begin
        for i := 0 to Count - 1 do
        begin
          S := Format('v %g %g %g', [List^[i].X, List^[i].Y, List^[i].Z]);
          WriteLn(S);
        end;
        inc(N, Count);
      end;
    end;
    WriteLn(Format('# %d Vertices', [N]));
    WriteLn('');
  end;

  procedure WriteNormals;
  var
    S: string;
    j, i, N: Integer;
  begin
    N := 0;
    for j := 0 to Owner.MeshObjects.Count - 1 do
    begin
      WriteLn(Format('# Mesh %d', [j + 1]));
      with Owner.MeshObjects[j].Normals do
      begin
        for i := 0 to Count - 1 do
        begin
          S := Format('vn %g %g %g', [List^[i].X, List^[i].Y, List^[i].Z]);
          WriteLn(S);
        end;
        inc(N, Count);
      end;
    end;
    WriteLn(Format('# %d Normals', [N]));
    WriteLn('');
  end;

  procedure WriteTexCoords;
  var
    S: string;
    j, i, N: Integer;
  begin
    N := 0;
    for j := 0 to Owner.MeshObjects.Count - 1 do
    begin
      WriteLn(Format('# Mesh %d', [j + 1]));
      with Owner.MeshObjects[j].TexCoords do
      begin
        for i := 0 to Count - 1 do
        begin
          S := Format('vt %g %g', [List^[i].X, List^[i].Y]);
          WriteLn(S);
        end;
        inc(N, Count);
      end;
    end;
    WriteLn(Format('# %d Texture-Coordinates', [N]));
    WriteLn('');
  end;

  procedure WriteOBJFaceGroup(aFaceGroup: TOBJFGVertexNormalTexIndexList; o: Integer = 0);
  var
    vIdx, nIdx, tIdx: Integer;
    i, Index, Polygon: Integer;
    Line, t: string;
  begin
    with aFaceGroup do
    begin
      Index := 0;
      for Polygon := 0 to PolygonVertices.Count - 1 do
      begin
        Line := 'f ';
        for i := 1 to PolygonVertices[Polygon] do
        begin
          vIdx := VertexIndices[Index] + 1 + o;
          nIdx := NormalIndices[Index] + 1 + o;
          tIdx := TexCoordIndices[Index] + 1 + o;
          t := IntToStr(vIdx) + '/';
          if tIdx = -1 then
            t := t + '/'
          else
            t := t + IntToStr(tIdx) + '/';
          if nIdx = -1 then
            t := t + '/'
          else
            t := t + IntToStr(nIdx) + '/';
          Line := Line + copy(t, 1, Length(t) - 1) + ' ';
          inc(Index);
        end;
        WriteLn(Line);
      end;
    end;
    WriteLn('');
  end;

  procedure WriteVertexIndexList(fg: TfgxVertexIndexList; o: Integer = 0);
  var
    i, N: Integer;
  begin
    case fg.Mode of
      fgmmTriangles:
        begin
          N := fg.VertexIndices.Count - 3;
          i := 0;
          while i <= N do
          begin
            WriteLn(Format('f %d/%0:d %d/%1:d %d/%2:d', [fg.VertexIndices[i] + 1 + o, fg.VertexIndices[i + 1] + 1 + o,
              fg.VertexIndices[i + 2] + 1 + o]));
            inc(i, 3);
          end;
        end;
      fgmmTriangleFan:
        begin
          Write('f ');
          N := fg.VertexIndices.Count - 1;
          i := 0;
          while i <= N do
          begin
            if i < N then
              Write(AnsiString(Format('%d/%0:d ', [fg.VertexIndices[i] + 1 + o])))
            else
              WriteLn(Format('%d/%0:d', [fg.VertexIndices[i] + 1 + o]));
            inc(i);
          end;
        end;
      fgmmTriangleStrip:
        begin
          N := fg.VertexIndices.Count - 3;
          i := 0;
          while i <= N do
          begin
            WriteLn(Format('f %d/%0:d %d/%1:d %d/%2:d', [fg.VertexIndices[i] + 1 + o, fg.VertexIndices[i + 1] + 1 + o,
              fg.VertexIndices[i + 2] + 1 + o]));

            inc(i);
          end;
        end;
    end;
  end;

  procedure WriteFaceGroups;
  var
    j, i, k: Integer;
    fg: TgxFaceGroup;
    MoName: string;
  begin
    k := 0;
    for j := 0 to Owner.MeshObjects.Count - 1 do
    begin
      MoName := Owner.MeshObjects[j].Name;
      if MoName = '' then
        MoName := Format('Mesh%d', [j + 1]);
      WriteLn('g ' + MoName);
      for i := 0 to Owner.MeshObjects[j].FaceGroups.Count - 1 do
      begin
        WriteLn(Format('s %d', [i + 1]));
        fg := Owner.MeshObjects[j].FaceGroups[i];
        if fg is TOBJFGVertexNormalTexIndexList then
          WriteOBJFaceGroup(TOBJFGVertexNormalTexIndexList(fg), k)
        else if fg is TfgxVertexIndexList then
          WriteVertexIndexList(TfgxVertexIndexList(fg), k)
        else
          Assert(False); // unsupported face group
      end;
      // advance vertex index offset
      inc(k, Owner.MeshObjects[j].Vertices.Count);
    end;
  end;

begin
  Assert(Owner is TgxFreeForm, 'Can only save FreeForms.');

  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';

  // Better not call anything that wants the system-locale intact from this block
  try
    WriteHeader;
    WriteVertices;
    WriteNormals;
    WriteTexCoords;
    WriteFaceGroups;
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

// ------------------
// ------------------ TgxMTLFile ------------------
// ------------------

procedure TgxMTLFile.Prepare;
var
  i: Integer;
  buf: string;
begin
  // "standardize" the mtl file lines
  for i := Count - 1 downto 0 do
  begin
    buf := UpperCase(Trim(Strings[i]));
    if (buf = '') or CharInSet(buf[1], ['#', '$']) then
      Delete(i)
    else
    begin
      Strings[i] := StringReplace(buf, #9, #32, [rfIgnoreCase]);
    end;
  end;
end;

function TgxMTLFile.MaterialStringProperty(const materialName, propertyName: string): string;
var
  i, N: Integer;
  buf, Line: string;
begin
  buf := 'NEWMTL ' + UpperCase(materialName);
  i := IndexOf(buf);
  if i >= 0 then
  begin
    buf := UpperCase(propertyName) + ' ';
    N := Length(buf);
    inc(i);
    while i < Count do
    begin
      Line := Strings[i];
      if copy(Line, 1, 7) = 'NEWMTL ' then
        break;
      if copy(Line, 1, N) = buf then
      begin
        Result := copy(Strings[i], N + 1, MaxInt);
        exit;
      end;
      inc(i);
    end;
  end;
  Result := '';
end;

function TgxMTLFile.MaterialVectorProperty(const materialName, propertyName: string; const defaultValue: TVector4f): TVector4f;
var
  i: Integer;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.CommaText := MaterialStringProperty(materialName, propertyName);
    if sl.Count > 0 then
    begin
      Result := NullHmgVector;
      for i := 0 to 3 do
        if sl.Count > i then
          Result.V[i] := GXS.Utils.StrToFloatDef(sl[i], 0)
        else
          break;
    end
    else
      Result := defaultValue;
  finally
    sl.Free;
  end;
end;

procedure TOBJFGVertexNormalTexIndexList.Assign(Source: TPersistent);
begin
  if Source is TOBJFGVertexNormalTexIndexList then
  begin
    FMode := TOBJFGVertexNormalTexIndexList(Source).FMode;
    FName := TOBJFGVertexNormalTexIndexList(Source).FName;
    FCurrentVertexCount := TOBJFGVertexNormalTexIndexList(Source).FCurrentVertexCount;
    FShowNormals := TOBJFGVertexNormalTexIndexList(Source).FShowNormals;

    if TOBJFGVertexNormalTexIndexList(Source).FPolygonVertices = nil then
      FreeAndNil(FPolygonVertices)
    else
    begin
      if FPolygonVertices = nil then
        FPolygonVertices := TgxIntegerList.Create;
      FPolygonVertices.Assign(TOBJFGVertexNormalTexIndexList(Source).FPolygonVertices);
    end;
  end
  else
    inherited;
end;

procedure TOBJFGVertexNormalTexIndexList.ReadFromFiler(reader: TgxVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
  begin
    FMode := TOBJFGMode(reader.ReadInteger);
    FName := reader.ReadString;
    FCurrentVertexCount := reader.ReadInteger;
    FShowNormals := reader.ReadBoolean;

    if FMode = objfgmmPolygons then
    begin
      FPolygonVertices := TgxIntegerList.Create;
      FPolygonVertices.ReadFromFiler(reader);
    end;
  end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TOBJFGVertexNormalTexIndexList.WriteToFiler(writer: TgxVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0

    writer.WriteInteger(Ord(FMode));
    writer.WriteString(FName);
    writer.WriteInteger(FCurrentVertexCount);
    writer.WriteBoolean(FShowNormals);

    if FPolygonVertices <> nil then
      FPolygonVertices.WriteToFiler(writer);
  end;
end;

//------------------------------------------------------
initialization
//------------------------------------------------------

GXS.VectorFileObjects.RegisterVectorFileFormat('obj', 'WaveFront model file', TgxOBJVectorFile);
GXS.VectorFileObjects.RegisterVectorFileFormat('objf', 'Stripe model file', TgxOBJVectorFile);
RegisterClass(TOBJFGVertexNormalTexIndexList);

end.
