//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.MeshLines;

(* Line implementation by means of a Triangle strip. *)

interface

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,
  GLS.OpenGLTokens, 
   
  GLS.Scene, 
  GLS.Objects, 
  GLS.Texture,
  GLS.Coordinates,
  GLS.Context, 
  GLS.Material, 
  GLS.Color, 
  GLS.State, 
  GLS.Nodes, 
  GLS.VectorGeometry, 
  GLS.Spline,
  GLS.VectorTypes, 
  GLS.VectorLists,
  GLS.VectorFileObjects,
  GLS.RenderContextInfo;

type
   // Specialized Node for use in a TGLLines objects. Adds a Width property
   TLineNode = class(TGLNode)
   private
     FData: Pointer;
   protected
   public
     constructor Create(Collection : TCollection); override;
     destructor Destroy; override;
     procedure Assign(Source: TPersistent); override;
     property Data: Pointer read FData write FData;
   published
   end;

   // Specialized collection for Nodes in TGLMeshLines objects. Stores TLineNode items.
   TLineNodes = class(TGLNodes)
   public
     constructor Create(AOwner : TComponent); overload;
     destructor Destroy; override;
     procedure NotifyChange; override;
     function IndexOf(LineNode: TLineNode): Integer;
   end;

  TLineItem = class(TCollectionItem)
  private
    FName: String;
    FBreakAngle: Single;
    FDivision: Integer;
    FNodes: TLineNodes;
    FSplineMode: TGLLineSplineMode;
    FTextureLength: Single;
    FWidth: Single;
    FTextureCorrection: Boolean;
    FHide: Boolean;
    FData: Pointer;
    procedure SetHide(const Value: Boolean);
    procedure SetTextureCorrection(const Value: Boolean);
    procedure SetBreakAngle(const Value: Single);
    procedure SetDivision(const Value: Integer);
    procedure SetNodes(const Value: TLineNodes);
    procedure SetSplineMode(const Value:TGLLineSplineMode);
    procedure SetTextureLength(const Value: Single);
    procedure SetWidth(const Value: Single);
  protected
    procedure DoChanged; virtual;
  public
    property Data: Pointer read FData write FData;
  published
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Hide: Boolean read FHide write SetHide;
    property Name: String read FName write FName;
    property TextureCorrection: Boolean read FTextureCorrection write SetTextureCorrection;
    property BreakAngle: Single read FBreakAngle write SetBreakAngle;
    property Division: Integer read FDivision write SetDivision;
    property Nodes : TLineNodes read FNodes write SetNodes;
    property SplineMode: TGLLineSplineMode read FSplineMode write SetSplineMode;
    property TextureLength: Single read FTextureLength write SetTextureLength;
    property Width: Single read FWidth write SetWidth;
  end;

  TLineCollection = class(TOwnedCollection)
  private
    procedure SetItems(Index: Integer; const Val: TLineItem);
    function GetItems(Index: Integer): TLineItem;
  protected
  public
    function Add: TLineItem; overload;
    function Add(Name: String): TLineItem; overload;
    property Items[Index: Integer]: TLineItem read GetItems write SetItems; default;
  published
  end;

  TLightmapBounds = class(TGLCustomCoordinates)
  private
    function GetLeft: TGLFloat;
    function GetTop: TGLFloat;
    function GetRight: TGLFloat;
    function GetBottom: TGLFloat;
    function GetWidth: TGLFloat;
    function GetHeight: TGLFloat;
    procedure SetLeft(const value: TGLFloat);
    procedure SetTop(const value: TGLFloat);
    procedure SetRight(const value: TGLFloat);
    procedure SetBottom(const value: TGLFloat);
  published
    property Left: TGLFloat read GetLeft write SetLeft stored False;
    property Top: TGLFloat read GetTop write SetTop stored False;
    property Right: TGLFloat read GetRight write SetRight stored False;
    property Bottom: TGLFloat read GetBottom write SetBottom stored False;
    property Width: TGLFloat read GetWidth;
    property Height: TGLFloat read GetHeight;
  end;

  TGLMeshLines = class(TGLFreeForm)
  private
    FLines: TLineCollection;
    FMesh: TGLMeshObject;
    FLightmapBounds: TLightmapBounds;
    FLightmapIndex: Integer;
    FLightmapMaterialName: String;
    FFaceGroup: TFGVertexIndexList;
    FIndex: Integer;
    FNoZWrite: boolean;
    FShowNodes: Boolean;
    FUpdating: Integer;
    FSelectedLineItem: TLineItem;
    FSelectedNode: TLineNode;
    FNode1,FNode2: TLineNode;
    function GetUpdating: Boolean;
    function PointNearLine(const LineItem: TLineItem; const X,Z: Single; Tolerance: single = 1): boolean;
    function PointNearSegment(const StartNode, EndNode: TLineNode; const X,Z: Single; LineWidth: single; Tolerance: single = 1): boolean;
    procedure StitchStrips(idx: TGLIntegerList);
    procedure AddStitchMarker(idx: TGLIntegerList);
    procedure SetShowNodes(const Value: Boolean);
    procedure SetNoZWrite(const Value: Boolean);
    procedure SetLightmapIndex(const value: Integer);
    procedure SetLightmapMaterialName(const value: String);
    procedure SetLightmapBounds(const value: TLightmapBounds);
    procedure DoChanged;
    procedure AddIndex;
    procedure AddVertices(const Up, Inner, Outer: TAffineVector; S: Single; Correction: Single; UseDegenerate: Boolean; LineItem: TLineItem);
    procedure BuildLineItem(LineItem: TLineItem);
    procedure BuildGeometry;
    procedure DrawNode(var rci : TGLRenderContextInfo; Node: TLineNode; LineWidth: Single);
    procedure DrawCircle(Radius: Single);
    function SelectNode(LineItem: TLineItem; X,Z: Single):TLineNode;
  protected
    procedure Loaded; override;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    function SelectLineItem(const X,Z: Single; Tolerance: single = 1): TLineItem; overload;
    function SelectLineItem(LineItem: TLineItem): TLineItem; overload;
    function SelectLineItem(LineNode: TLineNode): TLineItem; overload;
    procedure DeselectLineItem;
    procedure DeselectLineNode;
    procedure BuildList(var rci : TGLRenderContextInfo); override;
    procedure DoRender(var rci : TGLRenderContextInfo; renderSelf, renderChildren : Boolean); override;
    procedure NotifyChange(Sender : TObject); override;
    property SelectedLineItem: TLineItem read FSelectedLineItem;
    property SelectedNode: TLineNode read FSelectedNode;
    property Node1: TLineNode read FNode1;
    property Node2: TLineNode read FNode2;
  published
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Updating: Boolean Read GetUpdating;
    property Lines: TLineCollection read FLines;
    property Material;
    property LightmapBounds: TLightmapBounds read FLightmapBounds write SetLightmapBounds;
    property LightmapIndex: Integer read FLightmapIndex write SetLightmapIndex;
    property LightmapMaterialName: String read FLightmapMaterialName write SetLightmapMaterialName;
    property NoZWrite: boolean read FNoZWrite write SetNoZWrite;
    property ShowNodes: Boolean read FSHowNodes write SetShowNodes;
  end;

//--------------------------------------------------------------------------
implementation
//--------------------------------------------------------------------------

const
  CIRCLESEGMENTS = 32;

constructor TLineNode.Create(Collection : TCollection);
begin
	inherited Create(Collection);
end;

destructor TLineNode.Destroy;
begin
	inherited Destroy;
end;

procedure TLineNode.Assign(Source: TPersistent);
begin
	if Source is TLineNode then
  begin
      FData := TLineNode(Source).FData;
  end;
	inherited;
end;

constructor TLineNodes.Create(AOwner : TComponent);
begin
   inherited Create(AOwner, TLineNode);
end;

destructor TLineNodes.destroy;
begin
  inherited;
end;

procedure TLineNodes.NotifyChange;
begin
   if (GetOwner<>nil) then
      TGLMeshLines((GetOwner as TLineItem).Collection.Owner).StructureChanged;
end;

function TLineNodes.IndexOf(LineNode: TLineNode): Integer;
var
  i: Integer;
begin
  result := -1;
  if assigned(LineNode) then
  begin
    for i := 0 to Count - 1 do
    begin
      if LineNode = Items[i] then
      begin
        Result := i;
        break;
      end;
    end;
  end;
end;


function TLineCollection.GetItems(index: Integer): TLineItem;
begin
   Result:=TLineItem(inherited Items[index]);
end;

procedure TLineCollection.SetItems(index: Integer; const val: TLineItem);
begin
   inherited Items[index]:=val;
end;

function TLineCollection.Add: TLineItem;
begin
  result := TLineItem.Create(self);
end;

function TLineCollection.Add(Name: String): TLineItem;
begin
  Result := Add;
  Result.Name := Name;
end;

constructor TLineItem.Create(Collection: TCollection);
begin
  inherited;
  FNodes:=TLineNodes.Create(Self, TLineNode);
  FBreakAngle := 30;
  FDivision := 10;
  FSplineMode := lsmLines;
  FTextureLength := 1;
  FWidth := 1;
  FTextureCorrection := False;
end;

destructor TLineItem.Destroy;
begin
  if TGLMeshLines(Collection.Owner).SelectedLineItem = self then
    TGLMeshLines(Collection.Owner).DeSelectLineItem;
  FNodes.Free;
  inherited;
end;

procedure TLineItem.SetHide(const Value: Boolean);
begin
  FHide := Value;
  DoChanged;
end;

procedure TLineItem.SetTextureCorrection(const Value: Boolean);
begin
  FTextureCorrection := Value;
  DoChanged;
end;

procedure TLineItem.SetBreakAngle(const Value: Single);
begin
  FBreakAngle := Value;
  DoChanged;
end;

procedure TLineItem.SetDivision(const Value: Integer);
begin
  FDivision := Value;
  DoChanged;
end;

procedure TLineItem.SetNodes(const Value: TLineNodes);
begin
  FNodes.Assign(Value);
  DoChanged;
end;

procedure TLineItem.SetSplineMode(const Value:TGLLineSplineMode);
begin
  FSplineMode := Value;
  DoChanged;
end;

procedure TLineItem.SetTextureLength(const Value: Single);
begin
  FTextureLength := Value;
  DoChanged;
end;

procedure TLineItem.SetWidth(const Value: Single);
begin
  FWidth := Value;
  DoChanged;
end;

procedure TLineItem.DoChanged;
begin
  //Notify parent of change because the mesh needs to be regenerated
  if (GetOwner<>nil) then
    TGLMeshLines(Collection.Owner).NotifyChange(Self);
end;

//---------------------------------
// TLightmapBounds
//---------------------------------

function TLightmapBounds.GetLeft: TGLFloat;
begin
  Result := X;
end;

function TLightmapBounds.GetTop: TGLFloat;
begin
  Result := Y;
end;

function TLightmapBounds.GetRight: TGLFloat;
begin
  Result := Z;
end;

function TLightmapBounds.GetBottom: TGLFloat;
begin
  Result := W;
end;

function TLightmapBounds.GetWidth: TGLFloat;
begin
  Result := Z - X;
end;

function TLightmapBounds.GetHeight: TGLFloat;
begin
  Result := W - Y;
end;

procedure TLightmapBounds.SetLeft(const value: TGLFloat);
begin
  X := Value;
end;

procedure TLightmapBounds.SetTop(const value: TGLFloat);
begin
  Y := Value;
end;

procedure TLightmapBounds.SetRight(const value: TGLFloat);
begin
  Z := Value;
end;

procedure TLightmapBounds.SetBottom(const value: TGLFloat);
begin
  W := Value;
end;

//--------------------------------
// TGLMeshLine
//--------------------------------
constructor TGLMeshLines.Create(AOwner: TComponent);
begin
  inherited;
  FLines := TLineCollection.Create(self,TLineItem);
  FLightmapBounds := TLightmapBounds.Create(Self);
end;

destructor TGLMeshLines.Destroy;
begin
  FLines.Free;
  FLightmapBounds.Free;
  inherited;
end;

procedure TGLMeshLines.Loaded;
begin
  DoChanged;
end;

procedure TGLMeshLines.BeginUpdate;
begin
  inc(FUpdating);
end;

procedure TGLMeshLines.EndUpdate;
begin
  Dec(FUpdating);
  if FUpdating < 1 then
  begin
    FUpdating := 0;
    DoChanged;
  end;
end;

procedure TGLMeshLines.Clear;
begin
  FSelectedLineItem := nil;
  FSelectedNode := nil;
  FLines.Clear;
  MeshObjects.Clear;
  StructureChanged;
end;

procedure TGLMeshLines.BuildList(var rci : TGLRenderContextInfo);
var
  i,j: Integer;
begin
  inherited;
  if FShowNodes then
  begin
    for i:= 0 to Lines.Count - 1 do
    begin
      if Lines[i] = FSelectedLineItem then
      begin
        for j := 0 to Lines[i].Nodes.Count-1 do
          DrawNode(rci, TLineNode(Lines[i].Nodes[j]),Lines[i].Width);
      end;
    end;
  end;
end;

procedure TGLMeshLines.DoRender(var rci : TGLRenderContextInfo; renderSelf, renderChildren : Boolean);
begin
  if FNoZWrite then
  begin
    gl.Disable(GL_Depth_Test);
    inherited;
    gl.Enable(GL_Depth_Test);
  end
  else
    inherited;
end;

procedure TGLMeshLines.SetShowNodes(const Value: Boolean);
begin
  FShowNodes := Value;
  DoChanged;
end;

procedure TGLMeshLines.SetNoZWrite(const Value: Boolean);
begin
  FNoZWrite := Value;
  DoChanged;
end;

procedure TGLMeshLines.SetLightmapIndex(const value: Integer);
begin
  FLightmapIndex := Value;
  DoChanged;
end;

procedure TGLMeshLines.SetLightmapMaterialName(const value: String);
var
  lLibMaterial: TGLLibMaterial;
begin
  if Value <> '' then
  begin
    if assigned(LightmapLibrary) then
    begin
      lLibMaterial := LightmapLibrary.Materials.GetLibMaterialByName(Value);
      if assigned(lLibMaterial) then
      begin
        FLightmapIndex := lLibMaterial.ID;
        FLightmapMaterialName := Value;
        DoChanged;
      end;
    end;
  end;
end;

procedure TGLMeshLines.SetLightmapBounds( const value: TLightmapBounds );
begin
  FLightmapBounds.SetVector(value.X, value.Y,value.Z,value.W);
  DoChanged;
end;

procedure TGLMeshLines.DoChanged;
begin
  if Updating then exit;
  BuildGeometry;
  StructureChanged;
end;

procedure TGLMeshLines.BuildGeometry;
var
  i: Integer;
  lFirstLineDone: Boolean;
  lVertex: TAffineVector;
  lTextPoint: TTexPoint;
begin
  if Updating then exit;
  //clear the mesh

  FMeshObjects.Clear;
  lFirstLineDone := False;
  FMesh := TGLMeshObject.CreateOwned(FMeshObjects);
  FMesh.Mode := momFaceGroups;
  FFaceGroup := TFGVertexIndexList.CreateOwned(FMesh.FaceGroups);
  FFaceGroup.Mode := fgmmTriangleStrip;
  FFaceGroup.LightMapIndex := FLightmapIndex;
  FIndex := 0;
  for i := 0 to Lines.Count - 1 do
  begin
    if not FLines.Items[i].Hide then
    begin
      if lFirstLineDone then
        AddStitchMarker(FFaceGroup.VertexIndices);
      if TLineItem(FLines.Items[i]).Nodes.Count > 0 then
      begin
        BuildLineItem(TLineItem(FLines.Items[i]));
        lFirstLineDone := True;
      end;
    end;
  end;
  StitchStrips(FFaceGroup.VertexIndices);
  //Calculate lightmapping
  if assigned(LightmapLibrary) and (LightmapIndex <> -1 ) then
    for i := 0 to FMesh.Vertices.Count - 1 do
    begin
      lVertex := FMesh.Vertices.Items[i];
      lTextPoint.s := (lVertex.X - FLightmapBounds.Left) / FLightmapBounds.Width;
      lTextPoint.t := (lVertex.Z - FLightmapBounds.Top) / FLightmapBounds.Height;
      FMesh.LightMapTexCoords.Add(lTextPoint);
    end;
end;

procedure TGLMeshLines.DrawNode(var rci : TGLRenderContextInfo; Node: TLineNode; LineWidth: Single);
var
  lNodeSize: Single;
begin
  lNodeSize := LineWidth* 0.7;
  gl.PushMatrix;
  gl.Translatef(Node.x,Node.y,Node.z);
  if lNodeSize <>1 then
  begin
    gl.PushMatrix;
    gl.Scalef(lNodeSize, lNodeSize, lNodeSize);
///    rci.GLStates.UnSetGLState(stTexture2D);
    rci.GLStates.Disable(stColorMaterial);
    rci.GLStates.Disable(stBlend);
    if Node = FSelectedNode then
      rci.GLStates.SetGLMaterialColors(cmFRONT, clrBlack, clrGray20, clrYellow, clrBlack, 0)
    else
      rci.GLStates.SetGLMaterialColors(cmFRONT, clrBlack, clrGray20, clrGreen, clrBlack, 0);
    DrawCircle(lNodeSize);
    gl.PopMatrix;
  end
  else
  begin
    if Node = FSelectedNode then
      rci.GLStates.SetGLMaterialColors(cmFRONT, clrBlack, clrGray20, clrYellow, clrBlack, 0)
    else
      rci.GLStates.SetGLMaterialColors(cmFRONT, clrBlack, clrGray20, clrGreen, clrBlack, 0);
    DrawCircle(lNodeSize);
  end;
  gl.PopMatrix;
end;

procedure TGLMeshLines.DrawCircle(Radius: Single);
var
  inner,outer,p1,p2: TGLVector;
  i: Integer;
  a: Single;
  lUp: TAffineVector;
begin
  inner := VectorMake(1, 0, 0);
  outer := VectorMake(1.3, 0, 0);
  gl.Begin_(GL_TRIANGLE_STRIP);
  for i:= 0 to CIRCLESEGMENTS do
  begin
    a := i * 2 * pi / CIRCLESEGMENTS;
    p1 := outer;
    p2 := inner;
    lUp := Up.AsAffineVector;
    RotateVector(p1,lUp, a);
    RotateVector(p2,lUp, a);
    gl.Vertex3fv(@p1.X);
    gl.Vertex3fv(@p2.X);
  end;
  gl.End_();
end;

function TGLMeshLines.SelectNode(LineItem: TLineItem; X,Z: Single): TLineNode;
var
  i: Integer;
  lRange: Single;
  length: single;
begin
  Result := nil;
  lRange := LineItem.Width * 0.88;
  for i := 0 to LineItem.Nodes.count - 1 do
  begin
    length := 1/RLength((X - LineItem.Nodes[i].X),(Z - LineItem.Nodes[i].Z));
    if length < lRange then
    begin
      Result := TLineNode(LineItem.Nodes[i]);
      Break;
    end;
  end;
end;

function TGLMeshLines.SelectLineItem(LineItem: TLineItem): TLineItem;
begin
  Result := nil;
  FSelectedLineItem := LineItem;
  FSelectedNode := nil;
  DoChanged;
end;

function TGLMeshLines.SelectLineItem(LineNode: TLineNode): TLineItem;
begin
  FSelectedLineItem := TLineItem(LineNode.Collection.Owner);
  FSelectedNode := LineNode;
  Result := FSelectedLineItem;
  DoChanged;
end;

procedure TGLMeshLines.DeselectLineItem;
begin
  FSelectedLineItem := nil;
  FSelectedNode := nil;
  DoChanged;
end;

procedure TGLMeshLines.DeselectLineNode;
begin
  FSelectedNode := nil;
  DoChanged;
end;

function TGLMeshLines.SelectLineItem(const X,Z: Single; Tolerance: single = 1): TLineItem;
var
  i: Integer;
  lStartPoint: Integer;
  lNode: TLineNode;
  lNodeWasSelected: Boolean;
begin
  Result := nil;
  lNodeWasSelected := False;

  if Assigned(FSelectedLineItem) and not lNodeWasSelected then
    lStartPoint := FSelectedLineItem.ID + 1
  else
    lStartPoint := 0;

  for i := lStartPoint to FLines.Count - 1 do
  begin
    if (FLines[i] <> FSelectedLineItem) or lNodeWasSelected then
    begin
      if PointNearLine(FLines[i],X,Z,Tolerance) then
      begin
        Result := FLines[i];
        lNode := SelectNode(FLines[i], X,Z);
        if lNode <> FSelectedNode then
        begin
          FSelectedNode := lNode;
        end;
        break;
      end;
    end;
  end;

  if not Assigned(Result) then
  begin
    for i := 0 to lStartPoint - 1 do
    begin
      if FLines[i] <> FSelectedLineItem then
      begin
        if PointNearLine(FLines[i],X,Z,Tolerance) then
        begin
          Result := FLines[i];
          break;
        end;
      end;
    end;
  end;

  FSelectedLineItem := Result;
  if not assigned(FSelectedLineItem) then
  begin
    FSelectedNode := nil;
    FNode1 := nil;
    FNode2 := nil;
  end;
  DoChanged;
end;

function TGLMeshLines.GetUpdating: Boolean;
begin
  Result := FUpdating > 0;
end;

function TGLMeshLines.PointNearLine(const LineItem: TLineItem; const X,Z: Single; Tolerance: single = 1): boolean;
var
  i: Integer;
  lStartNode,lEndNode: TLineNode;
begin
  Result := False;
  for i := 0 to LineItem.Nodes.Count - 2 do
  begin
    lStartNode := TLineNode(LineItem.Nodes[i]);
    lEndNode := TLineNode(LineItem.Nodes[i+1]);
    if PointNearSegment(lStartNode,lEndNode,X,Z,LineItem.Width,Tolerance) then
    begin
      Result := True;
      FNode1 := lStartNode;
      FNode2 := lEndNode;
      break;
    end;
  end;
end;

function TGLMeshLines.PointNearSegment(const StartNode, EndNode: TLineNode; const X,Z: Single; LineWidth: single; Tolerance: single = 1): boolean;
var
  xt, yt, u, len: single;
  xp, yp: single;
  lDist: Single;
begin
  Result:= false;
  lDist := (LineWidth/2) * Tolerance;
  xt:= EndNode.X - StartNode.X;
  yt:= EndNode.Z - StartNode.Z;
  len:= sqrt(xt*xt + yt*yt);
  xp:= (X - StartNode.X);
  yp:= (Z - StartNode.Z);
  u:= (xp * xt + yp * yt) / len;
  // point beyond line
  if (u < -lDist) or (u > len+lDist) then
    exit;
  u:= u / len;
  // get the point on the line that's pependicular to the point in question
  xt:= StartNode.X + xt * u;
  yt:= StartNode.Z + yt * u;
  // find the distance to the line, and see if it's closer than the specified distance
  Result:= sqrt(sqr(xt - X) + sqr(yt - Z)) <= lDist;
end;

procedure TGLMeshLines.StitchStrips(idx: TGLIntegerList);
var
  i: integer;
  i0, i1, i2: integer;
begin
  for i := idx.Count - 1 downto 0 do
  begin
    if idx[i] = -1 then
    begin
      i0:= idx[i-1];
      i1:= idx[i+4];
      i2:= idx[i+5];
      idx[i]:= i0;
      idx[i+1]:= i1;
      idx[i+2]:= i1;
      idx[i+3]:= i2;
    end;
  end;
end;

procedure TGLMeshLines.AddStitchMarker(idx: TGLIntegerList);
begin
  idx.Add(-1);
  idx.Add(-2);
  idx.Add(-2);
  idx.Add(-2);
end;

procedure TGLMeshLines.NotifyChange(Sender : TObject);
begin
  inherited;
  DoChanged;
end;

procedure TGLMeshLines.AddIndex;
begin
  FFaceGroup.Add(FIndex);
  inc(FIndex);
end;

procedure TGLMeshLines.AddVertices(const Up,Inner,Outer: TAffineVector; S: Single; Correction: Single; UseDegenerate: Boolean; LineItem: TLineItem);
begin
  if not LineItem.TextureCorrection then
    Correction := 0
  else
    Correction := Correction / (LineItem.TextureLength / LineItem.width);
  FMesh.Normals.Add(Up);
  FMesh.Vertices.Add(Outer);
  FMesh.TexCoords.Add(S-Correction,1);
  AddIndex;
  FMesh.Normals.Add(Up);
  FMesh.TexCoords.Add(S+Correction,0);
  FMesh.Vertices.Add(Inner);
  AddIndex;
  if LineItem.TextureCorrection then
  begin
    FMesh.Normals.Add(Up);
    FMesh.Vertices.Add(Outer);
    FMesh.TexCoords.Add(S+Correction,1);
    AddIndex;
    FMesh.Normals.Add(Up);
    FMesh.TexCoords.Add(S-Correction,0);
    FMesh.Vertices.Add(Inner);
    AddIndex;
  end;
end;

procedure TGLMeshLines.BuildLineItem(LineItem: TLineItem);
var
  Seg1: TAffineVector;
  Seg2: TAffineVector;
  NSeg1: TAffineVector;
  NSeg2: TAffineVector;
  N1,N2,N3: TAffineVector;
  Inner: TAffineVector;
  Outer: TAffineVector;
  lUp: TAffineVector;
  lAngle: Single;
  lAngleOffset: Single;
  lTotalAngleChange: Single;
  lBreakAngle: Single;
  i: Integer;
  Flip: Boolean;
  s: single;
  lSpline: TCubicSpline;
  lCount: Integer;
  f : Single;
  a, b, c : Single;
  lHalfLineWidth: single;
begin
  inherited;
  lTotalAngleChange := 0;
  lHalfLineWidth := LineItem.Width/2;
  lBreakAngle := DegToRadian(LineItem.BreakAngle);
  try
    N1 := AffineVectorMake(0,0,0);
    N2 := AffineVectorMake(0,0,0);
    N3 := AffineVectorMake(0,0,0);
    s:= 0;
    f := 0;
    lSpline := nil;
    lUp := Up.AsAffineVector;
    lCount := 0;
    if LineItem.SplineMode = lsmLines then
      lCount := LineItem.Nodes.Count - 1
    else
    if LineItem.Nodes.Count > 1 then
    begin
      lCount := (LineItem.Nodes.Count-1) * LineItem.Division;
      lSpline := LineItem.Nodes.CreateNewCubicSpline;
      f:=1/LineItem.Division;
    end;
    for i := 0 to lCount do
    begin
      if LineItem.SplineMode = lsmLines then
      begin
        N3 := LineItem.Nodes.Items[i].AsAffineVector
      end
      else
      begin
        if lCount > 1 then
        begin
          lSpline.SplineXYZ(i*f, a, b, c);
          N3 := AffineVectorMake(a,b,c);
        end;
      end;
      if i > 0 then
      begin
        Seg1 := Seg2;
        Seg2 := VectorSubtract(N3,N2);
      end;
      if (i = 1) and not VectorEQuals(Seg2,NullVector)then
      begin
        //Create start vertices
        //this makes the assumption that these vectors are different which not always true
        Inner := VectorCrossProduct(Seg2, lUp);
        NormalizeVector(Inner);
        ScaleVector(Inner,lHalfLineWidth);
        Outer := VectorNegate(Inner);
        AddVector(Inner,N2);
        AddVector(Outer,N2);
        AddVertices(lUp,Inner, Outer,S,0,False,LineItem);
        s := s + VectorLength(Seg2)/LineItem.TextureLength;
      end;
      if i > 1 then
      begin
        lUp := VectorCrossProduct(Seg2,Seg1);
        if VectorEquals(lUp, NullVector) then
          lUp := Up.AsAffineVector;
        Flip := VectorAngleCosine(lUp,Self.up.AsAffineVector) < 0;
        if Flip then
          NegateVector(lUp);
        NSeg1 := VectorNormalize(Seg1);
        NSeg2 := VectorNormalize(Seg2);
        if VectorEquals(NSeg1,NSeg2) then
        begin
          Inner := VectorCrossProduct(Seg2, lUp);
          lAngle := 0;
        end
        else
        begin
          Inner := VectorSubtract(NSeg2,NSeg1);
          lAngle := (1.5707963 - ArcCosine(VectorLength(Inner)/2));
        end;
        lTotalAngleChange := lTotalAngleChange + (lAngle * 2);
        //Create intermediate vertices
        if (lTotalAngleChange > lBreakAngle) or (LineItem.BreakAngle = -1 )then
        begin
          lTotalAngleChange := 0;
          //Correct width for angles less than 170
          if lAngle < 1.52 then
            lAngleOffset := lHalfLineWidth * sqrt(sqr(Tangent(lAngle))+1)
          else
            lAngleOffset := lHalfLineWidth;
          NormalizeVector(Inner);
          ScaleVector(Inner,lAngleOffset);
          Outer := VectorNegate(Inner);
          AddVector(Inner,N2);
          AddVector(Outer,N2);
          if not Flip then
            AddVertices(lUp,Inner, Outer,S,-Tangent(lAngle)/2,True, LineItem)
          else
            AddVertices(lUp,Outer, Inner,S,Tangent(lAngle)/2,True, LineItem);
        end;
        s:= s + VectorLength(seg2)/LineItem.TextureLength;
      end;

      //Create last vertices
      if (lCount > 0) and (i =  lCount) and not VectorEQuals(Seg2,NullVector) then
      begin
        lUp := Up.AsAffineVector;
        Inner := VectorCrossProduct(Seg2, lUp);
        NormalizeVector(Inner);
        ScaleVector(Inner,lHalfLineWidth);
        Outer := VectorNegate(Inner);
        AddVector(Inner,N3);
        AddVector(Outer,N3);
        AddVertices(lUp,Inner, Outer,S,0,False, LineItem);
      end;
      N1 := N2;
      N2 := N3;
    end;
  except
    on e: Exception do
      raise exception.Create(e.Message);
  end;
  if assigned(lSpline) then
    lSpline.Free;
end;

//---------------------------------
initialization
//---------------------------------

   RegisterClasses([TGLMeshLines]);

end.
