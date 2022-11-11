//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.TilePlane;

(* Implements a tiled texture plane. *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  System.Classes,

  GLS.OpenGLTokens,
  GLS.Scene,
  GLS.State,
  GLS.VectorGeometry,
  GLS.Context,
  GLS.Material,
  GLS.Objects,
  GLS.PersistentClasses,
  GLS.VectorLists,
  GLS.RenderContextInfo,
  GLS.XOpenGL;

type
  // Stores row information for a tiled area.
  TGLTiledAreaRow = class(TGLPersistentObject)
  private
    FColMin, FColMax: Integer;
    FData: TGLIntegerList;
  protected
    procedure SetColMin(const val: Integer);
    procedure SetColMax(const val: Integer);
    function GetCell(col: Integer): Integer;
    procedure SetCell(col, val: Integer);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TGLVirtualWriter); override;
    procedure ReadFromFiler(reader: TGLVirtualReader); override;
    property Cell[col: Integer]: Integer read GetCell write SetCell; default;
    property ColMin: Integer read FColMin write SetColMin;
    property ColMax: Integer read FColMax write SetColMax;
    property Data: TGLIntegerList read FData;
    procedure Pack;
    function Empty: Boolean;
    procedure RemapTiles(remapList: TGLIntegerList);
  end;

  (* Stores tile information in a tiled area.
    Each tile stores an integer value with zero the default value,
    assumed as "empty". *)
  TGLTiledArea = class(TGLPersistentObject)
  private
    FRowMin, FRowMax: Integer;
    FRows: TGLPersistentObjectList;
  protected
    procedure SetRowMin(const val: Integer);
    procedure SetRowMax(const val: Integer);
    function GetTile(col, row: Integer): Integer;
    procedure SetTile(col, row, val: Integer);
    function GetRow(index: Integer): TGLTiledAreaRow;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TGLVirtualWriter); override;
    procedure ReadFromFiler(reader: TGLVirtualReader); override;
    property Tile[col, row: Integer]: Integer read GetTile
      write SetTile; default;
    property Row[index: Integer]: TGLTiledAreaRow read GetRow;
    property RowMin: Integer read FRowMin write SetRowMin;
    property RowMax: Integer read FRowMax write SetRowMax;
    procedure Pack;
    procedure Clear;
    function Empty: Boolean;
    procedure RemapTiles(remapList: TGLIntegerList);
  end;

  (* A tiled textured plane.
    This plane object stores and displays texture tiles that composes it,
    and is optimized to minimize texture switches when rendering.
    Its bounding dimensions are determined by its painted tile. *)
  TGLTilePlane = class(TGLImmaterialSceneObject)
  private
    FNoZWrite: Boolean;
    FTiles: TGLTiledArea;
    FMaterialLibrary: TGLMaterialLibrary;
    FSortByMaterials: Boolean;
  protected
    procedure SetNoZWrite(const val: Boolean);
    procedure SetTiles(const val: TGLTiledArea);
    procedure SetMaterialLibrary(const val: TGLMaterialLibrary);
    procedure SetSortByMaterials(const val: Boolean);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    // Access to the TiledArea data
    property Tiles: TGLTiledArea read FTiles write SetTiles;
    (* Controls the sorting of tiles by material.
      This property should ideally be left always at its default, True,
      except for debugging and performance measurement, which is why
      it's only public and not published. *)
    property SortByMaterials: Boolean read FSortByMaterials
      write SetSortByMaterials;
  published
    // If True the tiles are rendered without writing to the ZBuffer.
    property NoZWrite: Boolean read FNoZWrite write SetNoZWrite;
    (* Material library where tiles materials will be stored/retrieved.
      The lower 16 bits of the tile integer value is understood as being
      the index of the tile's material in the library (material of
      index zero is thus unused). *)
    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary
      write SetMaterialLibrary;
  end;

// -------------------------------------------------------------
implementation
// -------------------------------------------------------------

// ------------------
// ------------------ TGLTiledAreaRow ------------------
// ------------------

constructor TGLTiledAreaRow.Create;
begin
  inherited;
  FData := TGLIntegerList.Create;
  FColMin := 0;
  FColMax := -1;
end;

destructor TGLTiledAreaRow.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TGLTiledAreaRow.WriteToFiler(writer: TGLVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteInteger(FColMin);
    FData.WriteToFiler(writer);
  end;
end;

procedure TGLTiledAreaRow.ReadFromFiler(reader: TGLVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FColMin := ReadInteger;
      FData.ReadFromFiler(reader);
      FColMax := FColMin + FData.Count - 1;
    end;
end;

procedure TGLTiledAreaRow.Pack;
var
  i, startSkip: Integer;
begin
  startSkip := MaxInt;
  for i := 0 to FData.Count - 1 do
  begin
    if FData.List^[i] <> 0 then
    begin
      startSkip := i;
      Break;
    end;
  end;
  if startSkip = MaxInt then
  begin
    FData.Clear;
    FColMax := ColMin - 1;
  end
  else
  begin
    for i := FData.Count - 1 downto 0 do
    begin
      if FData.List^[i] <> 0 then
      begin
        FData.Count := i + 1;
        FColMax := FColMin + FData.Count - 1;
        Break;
      end;
    end;
    if startSkip > 0 then
    begin
      FData.DeleteItems(0, startSkip);
      FColMin := FColMin + startSkip;
    end;
  end;
end;

function TGLTiledAreaRow.Empty: Boolean;
begin
  Result := (FData.Count = 0);
end;

procedure TGLTiledAreaRow.RemapTiles(remapList: TGLIntegerList);
var
  i, k: Integer;
begin
  for i := 0 to FData.Count - 1 do
  begin
    k := FData[i];
    if Cardinal(k) < Cardinal(remapList.Count) then
      FData[i] := remapList[k]
    else
      FData[i] := 0;
  end;
end;

procedure TGLTiledAreaRow.SetColMax(const val: Integer);
begin
  if val >= ColMin then
    FData.Count := val - ColMin + 1
  else
    FData.Clear;
  FColMax := val;
end;

procedure TGLTiledAreaRow.SetColMin(const val: Integer);
begin
  if ColMax >= val then
  begin
    if val < ColMin then
      FData.InsertNulls(0, ColMin - val)
    else
      FData.DeleteItems(0, val - ColMin);
  end
  else
    FData.Clear;
  FColMin := val;
end;

function TGLTiledAreaRow.GetCell(col: Integer): Integer;
begin
  if (col >= ColMin) and (col <= ColMax) then
    Result := FData[col - ColMin]
  else
    Result := 0;
end;

procedure TGLTiledAreaRow.SetCell(col, val: Integer);
var
  i: Integer;
begin
  i := col - ColMin;
  if Cardinal(i) >= Cardinal(FData.Count) then
  begin
    if ColMin <= ColMax then
    begin
      if col < ColMin then
        ColMin := col;
      if col > ColMax then
        ColMax := col;
    end
    else
    begin
      FColMin := col;
      FColMax := col;
      FData.Add(val);
      Exit;
    end;
  end;
  FData[col - ColMin] := val;
end;

// ------------------
// ------------------ TGLTiledArea ------------------
// ------------------

constructor TGLTiledArea.Create;
begin
  inherited;
  FRows := TGLPersistentObjectList.Create;
  FRowMax := -1;
end;

destructor TGLTiledArea.Destroy;
begin
  FRows.CleanFree;
  inherited;
end;

procedure TGLTiledArea.WriteToFiler(writer: TGLVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteInteger(FRowMin);
    FRows.WriteToFiler(writer);
  end;
end;

procedure TGLTiledArea.ReadFromFiler(reader: TGLVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FRowMin := ReadInteger;
      FRows.ReadFromFiler(reader);
      FRowMax := FRowMin + FRows.Count - 1;
    end;
end;

procedure TGLTiledArea.Pack;
var
  i, firstNonNil, lastNonNil: Integer;
  r: TGLTiledAreaRow;
begin
  // pack all rows, free empty ones, determine 1st and last non-nil
  lastNonNil := -1;
  firstNonNil := FRows.Count;
  for i := 0 to FRows.Count - 1 do
  begin
    r := TGLTiledAreaRow(FRows.List^[i]);
    if Assigned(r) then
    begin
      r.Pack;
      if r.FData.Count = 0 then
      begin
        r.Free;
        FRows.List^[i] := nil;
      end;
    end;
    if Assigned(r) then
    begin
      lastNonNil := i;
      if i < firstNonNil then
        firstNonNil := i;
    end;
  end;
  if lastNonNil >= 0 then
  begin
    FRows.Count := lastNonNil + 1;
    FRowMax := FRowMin + FRows.Count - 1;
    if firstNonNil > 0 then
    begin
      FRowMin := FRowMin + firstNonNil;
      FRows.DeleteItems(0, firstNonNil);
    end;
  end
  else
    FRows.Clear;
end;

procedure TGLTiledArea.Clear;
begin
  FRows.Clean;
  FRowMin := 0;
  FRowMax := -1;
end;

function TGLTiledArea.Empty: Boolean;
begin
  Result := (FRows.Count = 0);
end;

procedure TGLTiledArea.RemapTiles(remapList: TGLIntegerList);
var
  i: Integer;
  r: TGLTiledAreaRow;
begin
  for i := 0 to FRows.Count - 1 do
  begin
    r := TGLTiledAreaRow(FRows[i]);
    if Assigned(r) then
      r.RemapTiles(remapList);
  end;
end;

function TGLTiledArea.GetTile(col, row: Integer): Integer;
var
  i: Integer;
  r: TGLTiledAreaRow;
begin
  i := row - RowMin;
  if Cardinal(i) < Cardinal(FRows.Count) then
  begin
    r := TGLTiledAreaRow(FRows[row - RowMin]);
    if Assigned(r) then
      Result := r.Cell[col]
    else
      Result := 0;
  end
  else
    Result := 0;
end;

procedure TGLTiledArea.SetTile(col, row, val: Integer);
var
  r: TGLTiledAreaRow;
begin
  if row < RowMin then
    RowMin := row;
  if row > RowMax then
    RowMax := row;
  r := TGLTiledAreaRow(FRows[row - RowMin]);
  if not Assigned(r) then
  begin
    r := TGLTiledAreaRow.Create;
    FRows[row - RowMin] := r;
  end;
  r.Cell[col] := val;
end;

function TGLTiledArea.GetRow(index: Integer): TGLTiledAreaRow;
begin
  index := index - RowMin;
  if Cardinal(index) < Cardinal(FRows.Count) then
    Result := TGLTiledAreaRow(FRows[index])
  else
    Result := nil;
end;

procedure TGLTiledArea.SetRowMax(const val: Integer);
begin
  if val >= RowMin then
  begin
    if val > RowMax then
      FRows.AddNils(val - RowMax)
    else
      FRows.DeleteAndFreeItems(val - RowMin + 1, FRows.Count);
  end
  else
    FRows.Clean;
  FRowMax := val;
end;

procedure TGLTiledArea.SetRowMin(const val: Integer);
begin
  if val <= RowMax then
  begin
    if val < RowMin then
      FRows.InsertNils(0, RowMin - val)
    else
      FRows.DeleteAndFreeItems(0, val - RowMin);
  end
  else
    FRows.Clean;
  FRowMin := val;
end;

// ------------------
// ------------------ TGLTilePlane ------------------
// ------------------

constructor TGLTilePlane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTiles := TGLTiledArea.Create;
  FSortByMaterials := True;
end;

destructor TGLTilePlane.Destroy;
begin
  MaterialLibrary := nil;
  FTiles.Free;
  inherited;
end;

procedure TGLTilePlane.SetNoZWrite(const val: Boolean);
begin
  if FNoZWrite <> val then
  begin
    FNoZWrite := val;
    StructureChanged;
  end;
end;

procedure TGLTilePlane.SetTiles(const val: TGLTiledArea);
begin
  if val <> FTiles then
  begin
    FTiles.Assign(val);
    StructureChanged;
  end;
end;

procedure TGLTilePlane.SetMaterialLibrary(const val: TGLMaterialLibrary);
begin
  if FMaterialLibrary <> val then
  begin
    if Assigned(FMaterialLibrary) then
    begin
      DestroyHandle;
      FMaterialLibrary.RemoveFreeNotification(Self);
    end;
    FMaterialLibrary := val;
    if Assigned(FMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
    StructureChanged;
  end;
end;

procedure TGLTilePlane.SetSortByMaterials(const val: Boolean);
begin
  FSortByMaterials := val;
  StructureChanged;
end;

procedure TGLTilePlane.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FMaterialLibrary then
      MaterialLibrary := nil;
  end;
  inherited;
end;

procedure TGLTilePlane.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  i: Integer;
begin
  if (not ListHandleAllocated) and Assigned(FMaterialLibrary) then
  begin
    for i := 0 to MaterialLibrary.Materials.Count - 1 do
      MaterialLibrary.Materials[i].PrepareBuildList;
  end;
  inherited;
end;

procedure TGLTilePlane.BuildList(var rci: TGLRenderContextInfo);
type
  TQuadListInfo = packed record
    x, y: TGLIntegerList;
  end;

  procedure IssueQuad(col, row: Integer);
  begin
    xgl.TexCoord2f(col, row);
    gl.Vertex2f(col, row);
    xgl.TexCoord2f(col + 1, row);
    gl.Vertex2f(col + 1, row);
    xgl.TexCoord2f(col + 1, row + 1);
    gl.Vertex2f(col + 1, row + 1);
    xgl.TexCoord2f(col, row + 1);
    gl.Vertex2f(col, row + 1);
  end;

var
  i, j, row, col, t: Integer;
  r: TGLTiledAreaRow;
  libMat: TGLLibMaterial;
  quadInfos: array of TQuadListInfo;
begin
  if MaterialLibrary = nil then
    Exit;
  // initialize infos
  gl.Normal3fv(@ZVector);
  if FNoZWrite then
    rci.GLStates.DepthWriteMask := False;
  if SortByMaterials then
  begin
    SetLength(quadInfos, MaterialLibrary.Materials.Count);
    for i := 0 to High(quadInfos) do
    begin // correction in (i:=0) from (i:=1)
      quadInfos[i].x := TGLIntegerList.Create;
      quadInfos[i].y := TGLIntegerList.Create;
    end;
    // collect quads into quadInfos, sorted by material
    for row := Tiles.RowMin to Tiles.RowMax do
    begin
      r := Tiles.row[row];
      if Assigned(r) then
      begin
        for col := r.ColMin to r.ColMax do
        begin
          t := r.Cell[col] and $FFFF;
          if (t > -1) and (t < MaterialLibrary.Materials.Count) then
          begin // correction in (t>-1) from (t>0)
            quadInfos[t].x.Add(col);
            quadInfos[t].y.Add(row);
          end;
        end;
      end;
    end;
    // render and cleanup
    for i := 0 to High(quadInfos) do
    begin // correction in (i:=0) from (i:=1)
      if quadInfos[i].x.Count > 0 then
      begin
        libMat := MaterialLibrary.Materials[i];
        libMat.Apply(rci);
        repeat
          gl.Begin_(GL_QUADS);
          with quadInfos[i] do
            for j := 0 to x.Count - 1 do
              IssueQuad(x[j], y[j]);
          gl.End_;
        until not libMat.UnApply(rci);
      end;
      quadInfos[i].x.Free;
      quadInfos[i].y.Free;
    end;
  end
  else
  begin
    // process all quads in order
    for row := Tiles.RowMin to Tiles.RowMax do
    begin
      r := Tiles.row[row];
      if Assigned(r) then
      begin
        for col := r.ColMin to r.ColMax do
        begin
          t := r.Cell[col] and $FFFF;
          if (t > -1) and (t < MaterialLibrary.Materials.Count) then
          begin // correction in (t>-1) from (t>0)
            libMat := MaterialLibrary.Materials[t];
            libMat.Apply(rci);
            repeat
              gl.Begin_(GL_QUADS);
              IssueQuad(col, row);
              gl.End_;
            until not libMat.UnApply(rci);
          end;
        end;
      end;
    end;
  end;
  if FNoZWrite then
    rci.GLStates.DepthWriteMask := True;
end;

// -------------------------------------------------------------
initialization
// -------------------------------------------------------------

RegisterClasses([TGLTilePlane, TGLTiledAreaRow, TGLTiledArea]);

end.
