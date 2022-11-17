//
// The unit is part of the GLScene Engine, http://glscene.org
//
unit Formats.VRML;

(*  VRML file format parser. *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Types,

  GLS.VectorGeometry,
  GLS.VectorTypes,
  GLS.VectorLists,
  GLS.Utils;

type
  TVRMLNode = class
  private
    FNodes: TList;
    FParent: TVRMLNode;
    FName, FDefName: String;
    function GetNode(index: Integer): TVRMLNode;
  public
    constructor Create; virtual;
    constructor CreateOwned(AParent: TVRMLNode);
    destructor Destroy; override;
    function Count: Integer;
    procedure Clear;
    procedure Add(node: TVRMLNode);
    procedure Remove(node: TVRMLNode);
    procedure Delete(index: Integer);
    property Nodes[index: Integer]: TVRMLNode read GetNode; default;
    property Parent: TVRMLNode read FParent;
    property Name: String read FName write FName;
    property DefName: String read FDefName write FDefName;
  end;

  TVRMLSingleArray = class(TVRMLNode)
  private
    FValues: TGLSingleList;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Values: TGLSingleList read FValues;
  end;

  TVRMLIntegerArray = class(TVRMLNode)
  private
    FValues: TGLIntegerList;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Values: TGLIntegerList read FValues;
  end;

  TVRMLMaterial = class(TVRMLNode)
  private
    FDiffuseColor, FAmbientColor, FSpecularColor, FEmissiveColor: TVector3f;
    FTransparency, FShininess: Single;
    FHasDiffuse, FHasAmbient, FHasSpecular, FHasEmissive, FHasTransparency,
      FHasShininess: Boolean;
  public
    constructor Create; override;
    property DiffuseColor: TVector3f read FDiffuseColor write FDiffuseColor;
    property AmbientColor: TVector3f read FAmbientColor write FAmbientColor;
    property SpecularColor: TVector3f read FSpecularColor write FSpecularColor;
    property EmissiveColor: TVector3f read FEmissiveColor write FEmissiveColor;
    property Transparency: Single read FTransparency write FTransparency;
    property Shininess: Single read FShininess write FShininess;
    property HasDiffuse: Boolean read FHasDiffuse write FHasDiffuse;
    property HasAmbient: Boolean read FHasAmbient write FHasAmbient;
    property HasSpecular: Boolean read FHasSpecular write FHasSpecular;
    property HasEmissive: Boolean read FHasEmissive write FHasEmissive;
    property HasTransparency: Boolean read FHasTransparency
      write FHasTransparency;
    property HasShininess: Boolean read FHasShininess write FHasShininess;
  end;

  TVRMLUse = class(TVRMLNode)
  private
    FValue: String;
  public
    property Value: String read FValue write FValue;
  end;

  TVRMLShapeHints = class(TVRMLNode)
  private
    FCreaseAngle: Single;
  public
    property CreaseAngle: Single read FCreaseAngle write FCreaseAngle;
  end;

  TVRMLTransform = class(TVRMLNode)
  private
    FCenter: TVector3f;
    FRotation: TVector4f;
    FScaleFactor: TVector3f;
  public
    constructor Create; override;
    property Center: TVector3f read FCenter write FCenter;
    property Rotation: TVector4f read FRotation write FRotation;
    property ScaleFactor: TVector3f read FScaleFactor write FScaleFactor;
  end;

  TVRMLParser = class
  private
    FCursor: Integer;
    FTokens: TStringList;
    FRootNode: TVRMLNode;
    FCurrentNode: TVRMLNode;
    FAllowUnknownNodes: Boolean;
    FDefines: TList;
  protected
    function ReadToken: String;
    function ReadSingle: Single;
    function ReadVector3f: TVector3f;
    function ReadVector4f: TVector4f;
    procedure ReadUnknownArray(DefName: String = '');
    procedure ReadUnknownHeirachy(DefName: String = '');
    procedure ReadUnknown(unknown_token: String; DefName: String = '');
    procedure ReadPointArray(DefName: String = '');
    procedure ReadCoordIndexArray(DefName: String = '');
    procedure ReadNormalIndexArray(DefName: String = '');
    procedure ReadTextureCoordIndexArray(DefName: String = '');
    procedure ReadCoordinate3(DefName: String = '');
    procedure ReadNormal(DefName: String = '');
    procedure ReadTextureCoordinate2(DefName: String = '');
    procedure ReadMaterial(DefName: String = '');
    procedure ReadIndexedFaceSet(DefName: String = '');
    procedure ReadTransform(DefName: String = '');
    procedure ReadShapeHints(DefName: String = '');
    procedure ReadSeparator(DefName: String = '');
    procedure ReadGroup(DefName: String = '');
    procedure ReadDef;
    procedure ReadUse;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(Text: String);
    property RootNode: TVRMLNode read FRootNode;
    property AllowUnknownNodes: Boolean read FAllowUnknownNodes
      write FAllowUnknownNodes;
  end;

// ---------------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------------

function CreateVRMLTokenList(Text: String): TStringList;
const
  cSymbols: array [0 .. 3] of char = ('{', '}', '[', ']');
var
  i, j, p: Integer;
  str, token: String;
begin
  Result := TStringList.Create;

  Result.Text := Text;
  for i := 0 to Result.Count - 1 do
  begin
    p := Pos('#', Result[i]);
    if p > 0 then
      Result[i] := Copy(Result[i], 1, p - 1);
  end;

  Result.CommaText := Result.Text;
  for j := 0 to Length(cSymbols) - 1 do
  begin
    i := 0;
    repeat
      token := Result[i];
      p := Pos(cSymbols[j], token);
      if (p > 0) and (token <> cSymbols[j]) then
      begin
        str := Copy(token, p + 1, Length(token) - p);

        if (p = 1) then
        begin
          Result.Delete(i);
          Result.Insert(i, trim(str));
          Result.Insert(i, cSymbols[j]);
        end
        else
        begin
          Result.Delete(i);
          if Length(str) > 0 then
            Result.Insert(i, trim(str));
          Result.Insert(i, cSymbols[j]);
          Result.Insert(i, trim(Copy(token, 1, p - 1)));
        end;
      end;
      Inc(i);
    until i >= Result.Count - 1;
  end;
end;

// ---------------
// --------------- TVRMLNode ---------------
// ---------------

constructor TVRMLNode.Create;
begin
  FNodes := TList.Create;
end;

constructor TVRMLNode.CreateOwned(AParent: TVRMLNode);
begin
  Create;
  if Assigned(AParent) then
    AParent.Add(Self);
end;

destructor TVRMLNode.Destroy;
begin
  Clear;
  FNodes.Free;
  inherited;
end;

function TVRMLNode.GetNode(index: Integer): TVRMLNode;
begin
  Result := TVRMLNode(FNodes[index]);
end;

function TVRMLNode.Count: Integer;
begin
  Result := FNodes.Count;
end;

procedure TVRMLNode.Clear;
begin
  while FNodes.Count > 0 do
    Delete(0);
end;

procedure TVRMLNode.Add(node: TVRMLNode);
begin
  if not Assigned(node) then
    exit;
  if Assigned(node.Parent) then
    node.Parent.FNodes.Remove(node);
  FNodes.Add(node);
  node.FParent := Self;
end;

procedure TVRMLNode.Remove(node: TVRMLNode);
begin
  if not Assigned(node) then
    exit;
  FNodes.Remove(node);
  node.Free;
end;

procedure TVRMLNode.Delete(index: Integer);
begin
  if (index < 0) or (index >= Count) then
    exit;
  Nodes[index].Free;
  FNodes.Delete(index);
end;


// ---------------
// --------------- TVRMLSingleArray ---------------
// ---------------

constructor TVRMLSingleArray.Create;
begin
  inherited;
  FValues := TGLSingleList.Create;
end;

destructor TVRMLSingleArray.Destroy;
begin
  FValues.Free;
  inherited;
end;


// ---------------
// --------------- TVRMLIntegerArray ---------------
// ---------------

constructor TVRMLIntegerArray.Create;
begin
  inherited;
  FValues := TGLIntegerList.Create;
end;

destructor TVRMLIntegerArray.Destroy;
begin
  FValues.Free;
  inherited;
end;


// ---------------
// --------------- TVRMLMaterial ---------------
// ---------------

constructor TVRMLMaterial.Create;
begin
  inherited;
  // Default shininess value
  FHasDiffuse := False;
  FHasAmbient := False;
  FHasSpecular := False;
  FHasEmissive := False;
  FHasTransparency := False;
  FHasShininess := False;
end;


// ---------------
// --------------- TVRMLTransform ---------------
// ---------------

constructor TVRMLTransform.Create;
begin
  inherited;
  FScaleFactor.X := 1;
  FScaleFactor.Y := 1;
  FScaleFactor.Z := 1;
end;


// ---------------
// --------------- TVRMLParser ---------------
// ---------------

constructor TVRMLParser.Create;
begin
  FDefines := TList.Create;
  FRootNode := TVRMLNode.Create;
  FRootNode.Name := 'Root';
  FAllowUnknownNodes := False;
end;

destructor TVRMLParser.Destroy;
begin
  FDefines.Free;
  FRootNode.Free;
  inherited;
end;

function TVRMLParser.ReadToken: String;
begin
  if FCursor < FTokens.Count then
  begin
    Result := LowerCase(FTokens[FCursor]);
    Inc(FCursor);
  end
  else
    Result := '';
end;

procedure TVRMLParser.ReadUnknownArray(DefName: String);
var
  token: String;
begin
  if AllowUnknownNodes then
  begin
    FCurrentNode := TVRMLNode.CreateOwned(FCurrentNode);
    FCurrentNode.Name := 'Unknown array';
  end;
  repeat
    token := ReadToken;
    if token = '' then
      exit;
  until token = ']';
  if AllowUnknownNodes then
    FCurrentNode := FCurrentNode.Parent;
end;

procedure TVRMLParser.ReadUnknownHeirachy(DefName: String);
var
  token: String;
begin
  if AllowUnknownNodes then
  begin
    FCurrentNode := TVRMLNode.CreateOwned(FCurrentNode);
    FCurrentNode.Name := 'Unknown hierarchy';
  end;
  repeat
    token := ReadToken;
    if token = '' then
      exit
    else
      ReadUnknown(token);
  until token = '}';
  if AllowUnknownNodes then
    FCurrentNode := FCurrentNode.Parent;
end;

procedure TVRMLParser.ReadUnknown(unknown_token: String; DefName: String);
begin
  if unknown_token = '{' then
    ReadUnknownHeirachy
  else if unknown_token = '[' then
    ReadUnknownArray
  else if (unknown_token <> '}') and (unknown_token <> ']') and AllowUnknownNodes
  then
  begin
    TVRMLNode.CreateOwned(FCurrentNode).Name := 'UNKNOWN[' +
      unknown_token + ']';
  end;
end;

function TVRMLParser.ReadSingle: Single;
begin
  Result := GLStrToFloatDef(ReadToken, 0);
end;

function TVRMLParser.ReadVector3f: TVector3f;
begin
  Result.X := ReadSingle;
  Result.Y := ReadSingle;
  Result.Z := ReadSingle;
end;

function TVRMLParser.ReadVector4f: TVector4f;
begin
  Result.X := ReadSingle;
  Result.Y := ReadSingle;
  Result.Z := ReadSingle;
  Result.W := ReadSingle;
end;

procedure TVRMLParser.ReadPointArray(DefName: String);
var
  token: String;
begin
  FCurrentNode := TVRMLSingleArray.CreateOwned(FCurrentNode);
  FCurrentNode.Name := 'PointArray';

  repeat
    token := ReadToken;
    if token = '' then
      exit;
  until token = '[';

  repeat
    token := ReadToken;
    if token = '' then
      exit
    else if token <> ']' then
      TVRMLSingleArray(FCurrentNode)
        .Values.Add(GLStrToFloatDef(token, 0));
  until token = ']';

  FCurrentNode := FCurrentNode.Parent;
end;

procedure TVRMLParser.ReadCoordIndexArray(DefName: String = '');
var
  token: String;
begin
  FCurrentNode := TVRMLIntegerArray.CreateOwned(FCurrentNode);
  FCurrentNode.Name := 'CoordIndexArray';
  FCurrentNode.DefName := DefName;

  repeat
    token := ReadToken;
    if token = '' then
      exit;
  until token = '[';

  repeat
    token := ReadToken;
    if token = '' then
      exit
    else if token <> ']' then
      TVRMLIntegerArray(FCurrentNode).Values.Add(StrToInt(token));
  until token = ']';

  FCurrentNode := FCurrentNode.Parent;
end;

procedure TVRMLParser.ReadNormalIndexArray(DefName: String = '');
var
  token: String;
begin
  FCurrentNode := TVRMLIntegerArray.CreateOwned(FCurrentNode);
  FCurrentNode.Name := 'NormalIndexArray';
  FCurrentNode.DefName := DefName;

  repeat
    token := ReadToken;
    if token = '' then
      exit;
  until token = '[';

  repeat
    token := ReadToken;
    if token = '' then
      exit
    else if token <> ']' then
      TVRMLIntegerArray(FCurrentNode).Values.Add(StrToInt(token));
  until token = ']';

  FCurrentNode := FCurrentNode.Parent;
end;

procedure TVRMLParser.ReadTextureCoordIndexArray(DefName: String = '');
var
  token: String;
begin
  FCurrentNode := TVRMLIntegerArray.CreateOwned(FCurrentNode);
  FCurrentNode.Name := 'TextureCoordIndexArray';
  FCurrentNode.DefName := DefName;

  repeat
    token := ReadToken;
    if token = '' then
      exit;
  until token = '[';

  repeat
    token := ReadToken;
    if token = '' then
      exit
    else if token <> ']' then
      TVRMLIntegerArray(FCurrentNode).Values.Add(StrToInt(token));
  until token = ']';

  FCurrentNode := FCurrentNode.Parent;
end;

procedure TVRMLParser.ReadMaterial(DefName: String);
var
  token: String;
begin
  FCurrentNode := TVRMLMaterial.CreateOwned(FCurrentNode);
  FCurrentNode.Name := 'Material';
  FCurrentNode.DefName := DefName;

  repeat
    token := ReadToken;
    if token = '' then
      exit;
  until token = '{';

  with TVRMLMaterial(FCurrentNode) do
  begin
    repeat
      token := ReadToken;
      if token = '' then
        exit
      else if token = 'diffusecolor' then
      begin
        DiffuseColor := ReadVector3f;
        HasDiffuse := True;
      end
      else if token = 'ambientcolor' then
      begin
        AmbientColor := ReadVector3f;
        HasAmbient := True;
      end
      else if token = 'specularcolor' then
      begin
        SpecularColor := ReadVector3f;
        HasSpecular := True;
      end
      else if token = 'emissivecolor' then
      begin
        EmissiveColor := ReadVector3f;
        HasEmissive := True;
      end
      else if token = 'transparency' then
      begin
        Transparency := ReadSingle;
        HasTransparency := True;
      end
      else if token = 'shininess' then
      begin
        Shininess := ReadSingle;
        HasShininess := True;
      end
      else if token <> '}' then
        ReadUnknown(token);
    until token = '}';
  end;

  FCurrentNode := FCurrentNode.Parent;
end;

procedure TVRMLParser.ReadCoordinate3(DefName: String = '');
var
  token: String;
begin
  FCurrentNode := TVRMLNode.CreateOwned(FCurrentNode);
  FCurrentNode.Name := 'Coordinate3';
  FCurrentNode.DefName := DefName;

  repeat
    token := ReadToken;
    if token = '' then
      exit;
  until token = '{';

  repeat
    token := ReadToken;
    if token = '' then
      exit
    else if token = 'point' then
      ReadPointArray
    else if token <> '}' then
      ReadUnknown(token);
  until token = '}';

  FCurrentNode := FCurrentNode.Parent;
end;

procedure TVRMLParser.ReadNormal(DefName: String = '');
var
  token: String;
begin
  FCurrentNode := TVRMLNode.CreateOwned(FCurrentNode);
  FCurrentNode.Name := 'Normal';
  FCurrentNode.DefName := DefName;

  repeat
    token := ReadToken;
    if token = '' then
      exit;
  until token = '{';

  repeat
    token := ReadToken;
    if token = '' then
      exit
    else if token = 'vector' then
      ReadPointArray
    else if token <> '}' then
      ReadUnknown(token);
  until token = '}';

  FCurrentNode := FCurrentNode.Parent;
end;

procedure TVRMLParser.ReadTextureCoordinate2(DefName: String = '');
var
  token: String;
begin
  FCurrentNode := TVRMLNode.CreateOwned(FCurrentNode);
  FCurrentNode.Name := 'TextureCoordinate2';
  FCurrentNode.DefName := DefName;

  repeat
    token := ReadToken;
    if token = '' then
      exit;
  until token = '{';

  repeat
    token := ReadToken;
    if token = '' then
      exit
    else if token = 'point' then
      ReadPointArray
    else if token <> '}' then
      ReadUnknown(token);
  until token = '}';

  FCurrentNode := FCurrentNode.Parent;
end;

procedure TVRMLParser.ReadIndexedFaceSet(DefName: String = '');
var
  token: String;
begin
  FCurrentNode := TVRMLNode.CreateOwned(FCurrentNode);
  FCurrentNode.Name := 'IndexedFaceSet';
  FCurrentNode.DefName := DefName;

  repeat
    token := ReadToken;
    if token = '' then
      exit;
  until token = '{';

  repeat
    token := ReadToken;
    if token = '' then
      exit
    else if token = 'coordindex' then
      ReadCoordIndexArray
    else if token = 'normalindex' then
      ReadNormalIndexArray
    else if token = 'texturecoordindex' then
      ReadTextureCoordIndexArray
    else if token <> '}' then
      ReadUnknown(token);
  until token = '}';

  FCurrentNode := FCurrentNode.Parent;
end;

procedure TVRMLParser.ReadTransform(DefName: String);
var
  token: String;
begin
  FCurrentNode := TVRMLTransform.CreateOwned(FCurrentNode);
  FCurrentNode.Name := 'Transform';
  FCurrentNode.DefName := DefName;

  repeat
    token := ReadToken;
    if token = '' then
      exit;
  until token = '{';

  with TVRMLTransform(FCurrentNode) do
  begin
    repeat
      token := ReadToken;
      if token = '' then
        exit
      else if token = 'rotation' then
        Rotation := ReadVector4f
      else if token = 'center' then
        Center := ReadVector3f
      else if token = 'scalefactor' then
        ScaleFactor := ReadVector3f
      else if token <> '}' then
        ReadUnknown(token);
    until token = '}';
  end;

  FCurrentNode := FCurrentNode.Parent;
end;

procedure TVRMLParser.ReadShapeHints(DefName: String = '');
var
  token: String;
begin
  FCurrentNode := TVRMLShapeHints.CreateOwned(FCurrentNode);
  FCurrentNode.Name := 'ShapeHints';
  FCurrentNode.DefName := DefName;

  repeat
    token := ReadToken;
    if token = '' then
      exit;
  until token = '{';

  repeat
    token := ReadToken;
    if token = '' then
      exit
    else if token = 'creaseangle' then
      TVRMLShapeHints(FCurrentNode).CreaseAngle := ReadSingle
    else if token <> '}' then
      ReadUnknown(token);
  until token = '}';

  FCurrentNode := FCurrentNode.Parent;
end;

procedure TVRMLParser.ReadSeparator(DefName: String = '');
var
  token: String;
begin
  FCurrentNode := TVRMLNode.CreateOwned(FCurrentNode);
  FCurrentNode.Name := 'Separator';
  FCurrentNode.DefName := DefName;

  repeat
    token := ReadToken;
    if token = '' then
      exit;
  until token = '{';

  repeat
    token := ReadToken;
    if token = '' then
      exit
    else if token = 'def' then
      ReadDef
    else if (token = 'group') or (token = 'switch') then
      ReadGroup
    else if token = 'separator' then
      ReadSeparator
    else if token = 'use' then
      ReadUse
    else if token = 'shapehints' then
      ReadShapeHints
    else if token = 'transform' then
      ReadTransform
    else if token = 'material' then
      ReadMaterial
    else if token = 'coordinate3' then
      ReadCoordinate3
    else if token = 'normal' then
      ReadNormal
    else if token = 'texturecoordinate2' then
      ReadTextureCoordinate2
    else if token = 'indexedfaceset' then
      ReadIndexedFaceSet
    else if token <> '}' then
      ReadUnknown(token);
  until token = '}';

  FCurrentNode := FCurrentNode.Parent;
end;

procedure TVRMLParser.ReadGroup(DefName: String = '');
var
  token: String;
begin
  FCurrentNode := TVRMLNode.CreateOwned(FCurrentNode);
  FCurrentNode.Name := 'Group';
  FCurrentNode.DefName := DefName;

  repeat
    token := ReadToken;
    if token = '' then
      exit;
  until token = '{';

  repeat
    token := ReadToken;
    if token = '' then
      exit
    else if token = 'def' then
      ReadDef
    else if (token = 'group') or (token = 'switch') then
      ReadGroup
    else if token = 'separator' then
      ReadSeparator
    else if token = 'use' then
      ReadUse
    else if token = 'shapehints' then
      ReadShapeHints
    else if token = 'transform' then
      ReadTransform
    else if token = 'material' then
      ReadMaterial
    else if token = 'coordinate3' then
      ReadCoordinate3
    else if token = 'indexedfaceset' then
      ReadIndexedFaceSet
    else if token <> '}' then
      ReadUnknown(token);
  until token = '}';

  FCurrentNode := FCurrentNode.Parent;
end;

procedure TVRMLParser.ReadDef;
var
  DefName, token: String;
begin
  DefName := ReadToken;
  token := ReadToken;
  if (token = 'group') or (token = 'switch') then
    ReadGroup(DefName)
  else if token = 'separator' then
    ReadSeparator(DefName)
  else if token = 'transform' then
    ReadTransform(DefName)
  else if token = 'material' then
    ReadMaterial(DefName)
  else if token = 'coordinate3' then
    ReadCoordinate3(DefName)
  else if token = 'indexedfaceset' then
    ReadIndexedFaceSet(DefName)
  else
    ReadUnknown(token);
end;

procedure TVRMLParser.ReadUse;
begin
  with TVRMLUse.CreateOwned(FCurrentNode) do
  begin
    name := 'Use';
    Value := ReadToken;
  end;
end;

procedure TVRMLParser.Parse(Text: String);
var
  token: String;
begin
  FTokens := CreateVRMLTokenList(Text);
  FCursor := 0;
  FCurrentNode := FRootNode;
  try
    repeat
      token := ReadToken;
      if token = 'def' then
        ReadDef
      else if (token = 'group') or (token = 'switch') then
        ReadGroup
      else if token = 'separator' then
        ReadSeparator
      else if token = 'use' then
        ReadUse
      else if token = 'shapehints' then
        ReadShapeHints
      else if token = 'transform' then
        ReadTransform
      else if token = 'material' then
        ReadMaterial
      else if token = 'coordinate3' then
        ReadCoordinate3
      else if token = 'indexedfaceset' then
        ReadIndexedFaceSet
      else
        ReadUnknown(token);
    until FCursor >= FTokens.Count;
  finally
    FTokens.Free;
  end;
end;

end.
