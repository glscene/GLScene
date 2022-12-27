//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit Formats.X;

(* Simple X format support for Microsoft's favorite format *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,

  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.PersistentClasses,
  GLS.Utils;

type
  TDXNode = class;

  TDXFileHeader = record
    Magic: array [0 .. 3] of AnsiChar;
    Major: array [0 .. 1] of AnsiChar;
    Minor: array [0 .. 1] of AnsiChar;
    FileType: array [0 .. 3] of AnsiChar;
    FloatType: array [0 .. 3] of AnsiChar;
  end;

  TDXNode = class(TList)
  private
    FName, FTypeName: String;
    FOwner: TDXNode;
    function GetItem(index: Integer): TDXNode;
  public
    constructor CreateOwned(AOwner: TDXNode);
    constructor Create; virtual;
    procedure Clear; override;
    property Name: String read FName write FName;
    property TypeName: String read FTypeName write FTypeName;
    property Owner: TDXNode read FOwner;
    property Items[index: Integer]: TDXNode read GetItem;
  end;

  TDXMaterialList = class;

  TDXMaterial = class(TGLPersistentObject)
  private
    FDiffuse: TVector4f;
    FSpecPower: Single;
    FSpecular, FEmissive: TVector3f;
    FTexture: String;
  public
    constructor CreateOwned(AOwner: TDXMaterialList);
    property Diffuse: TVector4f read FDiffuse write FDiffuse;
    property SpecPower: Single read FSpecPower write FSpecPower;
    property Specular: TVector3f read FSpecular write FSpecular;
    property Emissive: TVector3f read FEmissive write FEmissive;
    property Texture: String read FTexture write FTexture;

  end;

  TDXMaterialList = class(TDXNode)
  private
    function GetMaterial(index: Integer): TDXMaterial;
  public
    property Items[index: Integer]: TDXMaterial read GetMaterial;
  end;

  TDXFrame = class(TDXNode)
  private
    FMatrix: TGLMatrix;
  public
    constructor Create; override;
    function GlobalMatrix: TGLMatrix;
    property Matrix: TGLMatrix read FMatrix write FMatrix;

  end;

  TDXMesh = class(TDXNode)
  private
    FVertices, FNormals, FTexCoords: TGLAffineVectorList;
    FVertexIndices, FNormalIndices, FMaterialIndices, FVertCountIndices
      : TGLIntegerList;
    FMaterialList: TDXMaterialList;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Vertices: TGLAffineVectorList read FVertices;
    property Normals: TGLAffineVectorList read FNormals;
    property TexCoords: TGLAffineVectorList read FTexCoords;
    property VertexIndices: TGLIntegerList read FVertexIndices;
    property NormalIndices: TGLIntegerList read FNormalIndices;
    property MaterialIndices: TGLIntegerList read FMaterialIndices;
    property VertCountIndices: TGLIntegerList read FVertCountIndices;
    property MaterialList: TDXMaterialList read FMaterialList;
  end;

  TDXFile = class
  private
    FRootNode: TDXNode;
    FHeader: TDXFileHeader;
  protected
    procedure ParseText(Stream: TStream);
    procedure ParseBinary(Stream: TStream);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream);
    // procedure SaveToStream(Stream : TStream);
    property Header: TDXFileHeader read FHeader;
    property RootNode: TDXNode read FRootNode;

  end;

// ----------------------------------------------------------------------
implementation
// ----------------------------------------------------------------------

// ----------------------------------------------------------------------
// Text parsing functions
// ----------------------------------------------------------------------

procedure RemoveComments(Text: TStringList);
var
  i, comment: Integer;
begin
  for i := 0 to Text.Count - 1 do
  begin
    comment := Pos('//', Text[i]);
    if comment > 0 then
      Text[i] := Copy(Text[i], 0, comment - 1);
    comment := Pos('#', Text[i]);
    if comment > 0 then
      Text[i] := Copy(Text[i], 0, comment - 1);
  end;
end;


// ----------------------------------------------------------------------
// TDXFile
// ----------------------------------------------------------------------

constructor TDXFile.Create;
begin
  FRootNode := TDXNode.Create;
end;

destructor TDXFile.Destroy;
begin
  FRootNode.Free;

  inherited;
end;

procedure TDXFile.LoadFromStream(Stream: TStream);
begin
  Stream.Read(FHeader, SizeOf(TDXFileHeader));
  Assert(Header.Magic = 'xof ', 'Invalid DirectX file');

  if Header.FileType = 'txt ' then
    ParseText(Stream)
  else if Header.FileType = 'bin ' then
    raise Exception.Create('FileX error, "bin" filetype not supported')
  else if Header.FileType = 'tzip' then
    raise Exception.Create('FileX error, "tzip" filetype not supported')
  else if Header.FileType = 'bzip' then
    raise Exception.Create('FileX error, "bzip" filetype not supported');

end;

procedure TDXFile.ParseBinary(Stream: TStream);
begin
  // To-do
end;

procedure TDXFile.ParseText(Stream: TStream);
var
  XText, TempBuffer: TStringList;
  Cursor: Integer;
  Buffer: String;

  function ContainsColon(const Buffer: String): Boolean;
  begin
    Result := Pos(';', Buffer) > 0;
  end;

  function ContainsBegin(const Buffer: String): Boolean;
  begin
    Result := Pos('{', Buffer) > 0;
  end;

  function ContainsEnd(const Buffer: String): Boolean;
  begin
    Result := Pos('}', Buffer) > 0;
  end;

  function ReadString: String;
  begin
    if Cursor < XText.Count then
      Result := XText[Cursor]
    else
      Result := '';
    Inc(Cursor);
  end;

  function GetNodeData(var NodeType, NodeName: String): Boolean;
  begin
    NodeType := '';
    NodeName := '';
    Result := False;
    if Cursor < 3 then
      exit;

    NodeType := XText[Cursor - 3];
    NodeName := XText[Cursor - 2];

    if ContainsBegin(NodeType) or ContainsEnd(NodeType) or
      ContainsColon(NodeType) then
    begin
      NodeType := NodeName;
      NodeName := '';
    end;

    NodeType := LowerCase(NodeType);
  end;

  function ReadInteger: Integer;
  var
    str: String;
  begin
    str := ReadString;

    if ContainsColon(str) then
      str := StringReplace(str, ';', '', [rfReplaceAll]);
    if ContainsBegin(str) then
      str := StringReplace(str, '{', '', [rfReplaceAll]);
    if ContainsEnd(str) then
      str := StringReplace(str, '}', '', [rfReplaceAll]);

    Result := StrToInt(str);
  end;

  function ReadSingle: Single;
  var
    str: String;
  begin
    str := ReadString;

    if ContainsColon(str) then
      str := StringReplace(str, ';', '', [rfReplaceAll]);
    if ContainsBegin(str) then
      str := StringReplace(str, '{', '', [rfReplaceAll]);
    if ContainsEnd(str) then
      str := StringReplace(str, '}', '', [rfReplaceAll]);

    Result := GLStrToFloatDef(str, 0);
  end;

  function ReadMatrix: TGLMatrix;
  var
    i, j: Integer;
  begin
    try
      for j := 0 to 3 do
        for i := 0 to 3 do
          Result.V[i].V[j] := ReadSingle;
    except
      on E: Exception do
      begin
        Result := IdentityHMGMatrix;
      end;
    end;
  end;

  function ReadVector3f: TAffineVector;
  var
    str: String;
  begin
    str := ReadString;
    str := StringReplace(str, ';', ' ', [rfReplaceAll]);
    TempBuffer.CommaText := str;
    if TempBuffer.Count > 1 then
    begin
      Result.X := GLStrToFloatDef(TempBuffer[0], 0);
      Result.Y := GLStrToFloatDef(TempBuffer[1], 0);
      Result.Z := GLStrToFloatDef(TempBuffer[2], 0);
    end
    else
    begin
      Result.X := GLStrToFloatDef(TempBuffer[0], 0);
      Result.Y := ReadSingle;
      Result.Z := ReadSingle;
    end;
  end;

  function ReadVector4f: TGLVector;
  var
    str: String;
  begin
    str := ReadString;
    str := StringReplace(str, ';', ' ', [rfReplaceAll]);
    TempBuffer.CommaText := str;
    if TempBuffer.Count > 1 then
    begin
      Result.X := GLStrToFloatDef(TempBuffer[0], 0);
      Result.Y := GLStrToFloatDef(TempBuffer[1], 0);
      Result.Z := GLStrToFloatDef(TempBuffer[2], 0);
      Result.W := GLStrToFloatDef(TempBuffer[3], 0);
    end
    else
    begin
      Result.X := GLStrToFloatDef(TempBuffer[0], 0);
      Result.Y := ReadSingle;
      Result.Z := ReadSingle;
      Result.W := ReadSingle;
    end;
  end;

  function ReadTexCoord: TAffineVector;
  var
    str: String;
  begin
    str := ReadString;
    str := StringReplace(str, ';', ' ', [rfReplaceAll]);
    TempBuffer.CommaText := str;
    if TempBuffer.Count > 1 then
    begin
      Result.X := GLStrToFloatDef(TempBuffer[0], 0);
      Result.Y := GLStrToFloatDef(TempBuffer[1], 0);
    end
    else
    begin
      Result.X := GLStrToFloatDef(TempBuffer[0], 0);
      Result.Y := ReadSingle;
    end;
    Result.Z := 0;
  end;

  procedure ReadMeshVectors(VectorList: TGLAffineVectorList);
  var
    i, NumVectors: Integer;
  begin
    NumVectors := ReadInteger;
    for i := 0 to NumVectors - 1 do
      VectorList.Add(ReadVector3f);
  end;

  procedure ReadMeshIndices(IndexList: TGLIntegerList;
    VertCountIndices: TGLIntegerList = nil);
  var
    str: String;
    i, j, NumFaces, NumIndices, jStart: Integer;
    Indices: array of Integer;
  begin
    NumFaces := ReadInteger;
    for i := 0 to NumFaces - 1 do
    begin
      str := ReadString;
      str := StringReplace(str, ';', ' ', [rfReplaceAll]);
      TempBuffer.CommaText := str;
      NumIndices := StrToInt(TempBuffer[0]);
      SetLength(Indices, NumIndices);
      jStart := 0;
      if TempBuffer.Count > 1 then
      begin
        Indices[0] := StrToInt(TempBuffer[1]);
        jStart := 1;
      end;
      for j := jStart to NumIndices - 1 do
        Indices[j] := ReadInteger;
      case NumIndices of
        3:
          begin
            IndexList.Add(Indices[0], Indices[1], Indices[2]);
            if Assigned(VertCountIndices) then
              VertCountIndices.Add(3);
          end;
        4:
          begin
            IndexList.Add(Indices[0], Indices[1], Indices[2]);
            IndexList.Add(Indices[0], Indices[2], Indices[3]);
            if Assigned(VertCountIndices) then
              VertCountIndices.Add(6);
          end;
      end;
      SetLength(Indices, 0);
    end;
  end;

  procedure ReadTexCoords(VectorList: TGLAffineVectorList);
  var
    i, NumVectors: Integer;
  begin
    NumVectors := ReadInteger;
    for i := 0 to NumVectors - 1 do
      VectorList.Add(ReadTexCoord);
  end;

  procedure ReadMeshVertices(Mesh: TDXMesh);
  begin
    ReadMeshVectors(Mesh.Vertices);
    ReadMeshIndices(Mesh.VertexIndices, Mesh.VertCountIndices);
  end;

  procedure ReadMeshNormals(Mesh: TDXMesh);
  begin
    ReadMeshVectors(Mesh.Normals);
    ReadMeshIndices(Mesh.NormalIndices);
  end;

  procedure ReadMeshTexCoords(Mesh: TDXMesh);
  begin
    ReadTexCoords(Mesh.TexCoords);
  end;

  procedure ReadMeshMaterialList(Mesh: TDXMesh);
  var
    i, { NumMaterials, } NumIndices: Integer;
  begin
    { NumMaterials:= } ReadInteger;
    NumIndices := ReadInteger;
    for i := 0 to NumIndices - 1 do
      Mesh.MaterialIndices.Add(ReadInteger);
  end;

  procedure ReadMeshMaterial(Mesh: TDXMesh);
  begin
    with TDXMaterial.CreateOwned(Mesh.MaterialList) do
    begin
      Diffuse := ReadVector4f;
      SpecPower := ReadSingle;
      Specular := ReadVector3f;
      Emissive := ReadVector3f;
    end;
  end;

  procedure ReadTextureFilename(Mesh: TDXMesh);
  var
    str: String;
  begin
    if Mesh.MaterialList.Count > 0 then
    begin
      str := ReadString;
      str := StringReplace(str, '"', '', [rfReplaceAll]);
      str := StringReplace(str, ';', '', [rfReplaceAll]);
      str := Trim(str);
      Mesh.MaterialList.Items[Mesh.MaterialList.Count - 1].Texture := str;
    end;
  end;

  procedure ReadStruct(ParentNode: TDXNode);
  var
    Buffer, NodeType, NodeName: String;
    Loop: Boolean;
    NewNode: TDXNode;
  begin
    Loop := True;
    while Loop do
    begin
      Buffer := ReadString;
      if Cursor > XText.Count - 1 then
        break;
      if ContainsEnd(Buffer) then
        Loop := False
      else if ContainsBegin(Buffer) then
      begin
        GetNodeData(NodeType, NodeName);
        NewNode := nil;

        // Frame
        if NodeType = 'frame' then
        begin
          NewNode := TDXFrame.CreateOwned(ParentNode);
          ReadStruct(NewNode);

          // Frame transform matrix
        end
        else if NodeType = 'frametransformmatrix' then
        begin
          if ParentNode is TDXFrame then
            TDXFrame(ParentNode).Matrix := ReadMatrix;
          ReadStruct(ParentNode);

          // Mesh
        end
        else if NodeType = 'mesh' then
        begin
          NewNode := TDXMesh.CreateOwned(ParentNode);
          ReadMeshVertices(TDXMesh(NewNode));
          ReadStruct(NewNode);

          // Mesh normals
        end
        else if NodeType = 'meshnormals' then
        begin
          if ParentNode is TDXMesh then
            ReadMeshNormals(TDXMesh(ParentNode));
          ReadStruct(ParentNode);

          // Mesh texture coords
        end
        else if NodeType = 'meshtexturecoords' then
        begin
          if ParentNode is TDXMesh then
            ReadMeshTexCoords(TDXMesh(ParentNode));
          ReadStruct(ParentNode);

          // Mesh material list
        end
        else if NodeType = 'meshmateriallist' then
        begin
          if ParentNode is TDXMesh then
            ReadMeshMaterialList(TDXMesh(ParentNode));
          ReadStruct(ParentNode);

          // Mesh material
        end
        else if NodeType = 'material' then
        begin
          if ParentNode is TDXMesh then
            ReadMeshMaterial(TDXMesh(ParentNode));
          ReadStruct(ParentNode);

          // Material texture filename
        end
        else if NodeType = 'texturefilename' then
        begin
          if ParentNode is TDXMesh then
            ReadTextureFilename(TDXMesh(ParentNode));
          ReadStruct(ParentNode);

          // Unknown type
        end
        else
        begin
          // NewNode:=TDXNode.CreateOwned(ParentNode);
          // NodeType:='*'+NodeType;
          // ReadStruct(NewNode);
          ReadStruct(ParentNode);
        end;

        if Assigned(NewNode) then
        begin
          NewNode.TypeName := NodeType;
          NewNode.Name := NodeName;
        end;
      end;
    end;
  end;

begin
  XText := TStringList.Create;
  TempBuffer := TStringList.Create;
  XText.LoadFromStream(Stream);

  // Remove comments and white spaces
  RemoveComments(XText);
  XText.CommaText := XText.Text;

  // Fix embedded open braces
  Cursor := 0;
  while Cursor < XText.Count - 1 do
  begin
    Buffer := ReadString;
    if Pos('{', Buffer) > 1 then
    begin
      XText[Cursor - 1] := Copy(Buffer, 0, Pos('{', Buffer) - 1);
      XText.Insert(Cursor, '{');
    end;
  end;

  XText.SaveToFile('XText_dump.txt');

  // Start parsing
  Cursor := 0;
  while Cursor < XText.Count - 1 do
    ReadStruct(RootNode);

  TempBuffer.Free;
  XText.Free;
end;


// ----------------------------------------------------------------------
// TDXMaterialList
// ----------------------------------------------------------------------

function TDXMaterialList.GetMaterial(index: Integer): TDXMaterial;
begin
  Result := TDXMaterial(Get(index));
end;


// ----------------------------------------------------------------------
// TDXMesh
// ----------------------------------------------------------------------

constructor TDXMesh.Create;
begin
  inherited;

  FVertices := TGLAffineVectorList.Create;
  FNormals := TGLAffineVectorList.Create;
  FTexCoords := TGLAffineVectorList.Create;
  FVertexIndices := TGLIntegerList.Create;
  FNormalIndices := TGLIntegerList.Create;
  FMaterialIndices := TGLIntegerList.Create;
  FVertCountIndices := TGLIntegerList.Create;
  FMaterialList := TDXMaterialList.Create;
end;

destructor TDXMesh.Destroy;
begin
  FVertices.Free;
  FNormals.Free;
  FTexCoords.Free;
  FVertexIndices.Free;
  FNormalIndices.Free;
  FMaterialIndices.Free;
  FVertCountIndices.Free;
  FMaterialList.Free;

  inherited;
end;


// ----------------------------------------------------------------------
// TDXNode
// ----------------------------------------------------------------------

constructor TDXNode.Create;
begin
  // Virtual
end;

constructor TDXNode.CreateOwned(AOwner: TDXNode);
begin
  FOwner := AOwner;
  Create;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

function TDXNode.GetItem(index: Integer): TDXNode;
begin
  Result := TDXNode(Get(index));
end;

procedure TDXNode.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;

  inherited;
end;


// ----------------------------------------------------------------------
// TDXFrame
// ----------------------------------------------------------------------

constructor TDXFrame.Create;
begin
  inherited;
  FMatrix := IdentityHMGMatrix;
end;

function TDXFrame.GlobalMatrix: TGLMatrix;
begin
  if Owner is TDXFrame then
    Result := MatrixMultiply(TDXFrame(Owner).GlobalMatrix, FMatrix)
  else
    Result := FMatrix;
end;


// ----------------------------------------------------------------------
// TDXMaterial
// ----------------------------------------------------------------------

constructor TDXMaterial.CreateOwned(AOwner: TDXMaterialList);
begin
  Create;
  if Assigned(AOwner) then
    AOwner.Add(Self);
end;

end.
