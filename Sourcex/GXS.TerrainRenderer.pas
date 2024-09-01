//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.TerrainRenderer;

(*
  The Brute-force terrain renderer.

  NOTA : multi-materials terrain support is not yet optimized to minimize
  texture switches (in case of resued tile textures).
*)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,

  GXS.XOpenGL,
  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.VectorLists,
  GXS.Scene,
  GXS.HeightData,
  GXS.Material,
  GXS.Coordinates,
  GXS.Context,
  GXS.ROAMPatch,
  GXS.RenderContextInfo,
  GXS.Utils;

const
  cTilesHashSize = 255;

type

  TGetTerrainBoundsEvent = procedure(var l, t, r, b: single) of object;
  TPatchPostRenderEvent = procedure(var rci: TgxRenderContextInfo; const patches: TList) of object;
  TgxHeightDataPostRenderEvent = procedure(var rci: TgxRenderContextInfo; var HeightDatas: TList) of object;
  TMaxCLODTrianglesReachedEvent = procedure(var rci: TgxRenderContextInfo) of object;

  TgxTerrainHighResStyle = (hrsFullGeometry, hrsTesselated);
  TgxTerrainOcclusionTesselate = (totTesselateAlways, totTesselateIfVisible);

  TgxTileManagementFlag = (tmClearUsedFlags, tmMarkUsedTiles, tmReleaseUnusedTiles, tmAllocateNewTiles, tmWaitForPreparing);
  TgxTileManagementFlags = set of TgxTileManagementFlag;

  (* Basic terrain renderer.
    This renderer uses no sophisticated meshing, it just builds and maintains
    a set of terrain tiles, performs basic visibility culling and renders its
    stuff. You can use it has a base class/sample for more specialized
    terrain renderers.
    The Terrain heightdata is retrieved directly from a TgxHeightDataSource, and
    expressed as z=f(x, y) data. *)
  TgxTerrainRenderer = class(TgxSceneObject)
  private
    FHeightDataSource: TgxHeightDataSource;
    FTileSize: Integer;
    FQualityDistance, FinvTileSize: single;
    FLastTriangleCount: Integer;
    FTilesPerTexture: single;
    FMaxCLODTriangles, FCLODPrecision: Integer;
    FBufferVertices: TgxAffineVectorList;
    FBufferTexPoints: TgxTexPointList;
    FBufferVertexIndices: TgxIntegerList;
    FMaterialLibrary: TgxMaterialLibrary;
    FOnGetTerrainBounds: TGetTerrainBoundsEvent;
    FOnPatchPostRender: TPatchPostRenderEvent;
    FOnHeightDataPostRender: TgxHeightDataPostRenderEvent;
    FOnMaxCLODTrianglesReached: TMaxCLODTrianglesReachedEvent;
    FQualityStyle: TgxTerrainHighResStyle;
    FOcclusionFrameSkip: Integer;
    FOcclusionTesselate: TgxTerrainOcclusionTesselate;
    FContourInterval: Integer;
    FContourWidth: Integer;
  protected
    FTilesHash: packed array [0 .. cTilesHashSize] of TList;
    procedure MarkAllTilesAsUnused;
    procedure ReleaseAllUnusedTiles;
    procedure MarkHashedTileAsUsed(const tilePos: TAffineVector);
    function HashedTile(const tilePos: TAffineVector; canAllocate: Boolean = True): TgxHeightData; overload;
    function HashedTile(const xLeft, yTop: Integer; canAllocate: Boolean = True): TgxHeightData; overload;
    procedure SetHeightDataSource(const val: TgxHeightDataSource);
    procedure SetTileSize(const val: Integer);
    procedure SetTilesPerTexture(const val: single);
    procedure SetCLODPrecision(const val: Integer);
    procedure SetMaterialLibrary(const val: TgxMaterialLibrary);
    procedure SetQualityStyle(const val: TgxTerrainHighResStyle);
    procedure SetOcclusionFrameSkip(val: Integer);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DestroyHandle; override;
    procedure ReleaseAllTiles; virtual;
    procedure OnTileDestroyed(Sender: TObject); virtual;
    function GetPreparedPatch(const tilePos, EyePos: TAffineVector; TexFactor: single; HDList: TList): TgxROAMPatch;
  public
    (* TileManagement flags can be used to turn off various Tile cache management features.
      This helps to prevent unnecessary tile cache flushes, when rendering from multiple cameras. *)
    TileManagement: TgxTileManagementFlags;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
    function RayCastIntersect(const rayStart, rayVector: TVector4f; intersectPoint: PVector4f = nil; intersectNormal: PVector4f = nil)
      : Boolean; override;
    (* Interpolates height for the given point.
      Expects a point expressed in absolute coordinates. *)
    function InterpolatedHeight(const p: TVector4f): single; overload; virtual;
    function InterpolatedHeight(const p: TAffineVector): single; overload;
    // Triangle count for the last render.
    property LastTriangleCount: Integer read FLastTriangleCount;
    function HashedTileCount: Integer;
  published
    // Specifies the HeightData provider component.
    property HeightDataSource: TgxHeightDataSource read FHeightDataSource write SetHeightDataSource;
    // Size of the terrain tiles. Must be a power of two.
    property TileSize: Integer read FTileSize write SetTileSize default 16;
    // Number of tiles required for a full texture map.
    property TilesPerTexture: single read FTilesPerTexture write SetTilesPerTexture;
    (* Link to the material library holding terrain materials.
      If unspecified, and for all terrain tiles with unspecified material,
      the terrain renderer's material is used. *)
    property MaterialLibrary: TgxMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    (* Quality distance hint.
      This parameter gives an hint to the terrain renderer at which distance
      the terrain quality can be degraded to favor speed. The distance is
      expressed in absolute coordinates units.
      All tiles closer than this distance are rendered according to
      QualityStyle and with a static resolution. *)
    property QualityDistance: single read FQualityDistance write FQualityDistance;
    (* Determines how high-res tiles (closer than QualityDistance) are rendered.
      hrsFullGeometry (default value) means that the high-res tiles are rendered
      with full-geometry, and no LOD of any kind, while hrsTesselated means
      the tiles will be tesselated once, with the best output for the
      CLODPrecision, and the result of that tesselation will be reused
      in further frames without any adpative tesselation. *)
    property QualityStyle: TgxTerrainHighResStyle read FQualityStyle write SetQualityStyle default hrsFullGeometry;
    (* Maximum number of CLOD triangles per scene.
      Triangles in high-resolution tiles (closer than QualityDistance) do
      not count toward this limit. *)
    property MaxCLODTriangles: Integer read FMaxCLODTriangles write FMaxCLODTriangles default 65536;
    (* Precision of CLOD tiles.
      The lower the value, the higher the precision and triangle count.
      Large values will result in coarse terrain.
      high-resolution tiles (closer than QualityDistance) ignore this setting. *)
    property CLODPrecision: Integer read FCLODPrecision write SetCLODPrecision default 100;
    (* Numbers of frames to skip for a tile when occlusion testing found it invisible.
      Occlusion testing can help reduce CPU, T&L and fillrate requirements
      when tiles are occluded, either by the terrain itself (tiles behind
      a mountain or a cliff) or by geometry that was rendered before the
      terrain (large buildings). If there is little occlusion in your scene
      (such as in top down or high-altitude view), turning occlusion on
      may have a slightly negative effect on framerate.
      It works by turning off rendering of tiles for the specified number
      of frames if it has been found invisible, after FrameSkip number
      of frames have been skipped, it will be rendered again, and a new
      occlusion testing made. This makes occlusion-testing a frame-to-frame
      coherency optimization, and as such, shouldn't be used for static
      rendering (ie. leave value to its default of zero).
      This optimization requires the hardware to support GL_NV_occlusion_query. *)
    property OcclusionFrameSkip: Integer read FOcclusionFrameSkip write SetOcclusionFrameSkip default 0;
    (* Determines if and how occlusion testing affects tesselation.
      Turning off tesselation of tiles determined invisible can improve
      performance, however, it may result in glitches since the tesselation
      of an ivisible tile can have a slight effect on the tesselation
      of its adjacent tiles (by forcing higher resolution at the border
      for instance). This negative effect can be lessened by increasing
      the QualityDistance, so that glitches will apear farther away
      (this will mean increasing your triangle count though, so you'll
      trade CPU power against T&L power). *)
    property OcclusionTesselate: TgxTerrainOcclusionTesselate read FOcclusionTesselate write FOcclusionTesselate
      default totTesselateIfVisible;
    (* Allows to specify terrain bounds.
      Default rendering bounds will reach depth of view in all direction,
      with this event you can chose to specify a smaller rendered terrain area. *)
    property OnGetTerrainBounds: TGetTerrainBoundsEvent read FOnGetTerrainBounds write FOnGetTerrainBounds;
    (* Invoked for each rendered patch after terrain render has completed.
      The list holds TgxROAMPatch objects and allows per-patch
      post-processings, like waters, trees... It is invoked *before* OnHeightDataPostRender *)
    property OnPatchPostRender: TPatchPostRenderEvent read FOnPatchPostRender write FOnPatchPostRender;
    (* Invoked for each heightData not culled out by the terrain renderer.
      The list holds TgxHeightData objects and allows per-patch
      post-processings, like waters, trees... It is invoked *after* OnPatchPostRender. *)
    property OnHeightDataPostRender: TgxHeightDataPostRenderEvent read FOnHeightDataPostRender write FOnHeightDataPostRender;
    (* Invoked whenever the MaxCLODTriangles limit was reached during last rendering.
      This forced the terrain renderer to resize the buffer, which affects performance.
      If this event is fired frequently, one should increase MaxCLODTriangles. *)
    property OnMaxCLODTrianglesReached: TMaxCLODTrianglesReachedEvent read FOnMaxCLODTrianglesReached
      write FOnMaxCLODTrianglesReached;
    // Distance between contours - zero (default) for no contours  PGS
    property ContourInterval: Integer read FContourInterval write FContourInterval default 0;
    // Width of contour lines
    property ContourWidth: Integer read FContourWidth write FContourWidth default 1;
  end;

// ===================================================================
implementation
// ===================================================================

function HashKey(const xLeft, yTop: Integer): Integer;
begin
  Result := (xLeft + (xLeft shr 8) + (xLeft shr 16) + (yTop shl 1) + (yTop shr 9) + (yTop shr 17)) and cTilesHashSize;
end;

// ------------------
// ------------------ TgxTerrainRenderer ------------------
// ------------------

constructor TgxTerrainRenderer.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  for i := 0 to cTilesHashSize do
    FTilesHash[i] := TList.Create;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FTileSize := 16;
  FinvTileSize := 1 / 16;
  FTilesPerTexture := 1;
  FMaxCLODTriangles := 65536;
  FCLODPrecision := 100;
  FOcclusionTesselate := totTesselateIfVisible;
  FBufferVertices := TgxAffineVectorList.Create;
  FBufferTexPoints := TgxTexPointList.Create;
  FBufferVertexIndices := TgxIntegerList.Create;
  TileManagement := [tmClearUsedFlags, tmMarkUsedTiles, tmReleaseUnusedTiles, tmAllocateNewTiles];
end;

destructor TgxTerrainRenderer.Destroy;
var
  i: Integer;
begin
  FBufferVertices.Free;
  FBufferTexPoints.Free;
  FBufferVertexIndices.Free;
  ReleaseAllTiles;
  for i := 0 to cTilesHashSize do
  begin
    FTilesHash[i].Free;
    FTilesHash[i] := nil;
  end;
  inherited Destroy;
end;

procedure TgxTerrainRenderer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FHeightDataSource then
      HeightDataSource := nil
    else if AComponent = FMaterialLibrary then
      MaterialLibrary := nil;
  end;
  inherited;
end;

procedure TgxTerrainRenderer.DestroyHandle;
begin
  inherited;
  ReleaseAllTiles;
  if Assigned(HeightDataSource) then
    HeightDataSource.Clear;
end;

function TgxTerrainRenderer.RayCastIntersect(const rayStart, rayVector: TVector4f; intersectPoint: PVector4f = nil;
  intersectNormal: PVector4f = nil): Boolean;
var
  p1, d, p2, p3: TVector4f;
  step, i, h, minH, maxH, p1height: single;
  startedAbove: Boolean;
  failSafe: Integer;
  AbsX, AbsY, AbsZ: TVector4f;
begin
  Result := False;
  if Assigned(HeightDataSource) then
  begin
    step := (Scale.X + Scale.Y); // Initial step size guess
    i := step;
    d := VectorNormalize(rayVector);
    AbsZ := VectorNormalize(LocalToAbsolute(ZHMGVector));
    startedAbove := ((InterpolatedHeight(rayStart) - VectorDotProduct(rayStart, AbsZ)) < 0);
    maxH := Scale.Z * 256;
    minH := -Scale.Z * 256;
    failSafe := 0;
    while True do
    begin
      p1 := VectorCombine(rayStart, d, 1, i);
      h := InterpolatedHeight(p1);
      p1height := VectorDotProduct(AbsZ, p1);
      if Abs(h - p1height) < 0.1 then
      begin // Need a tolerance variable here (how close is good enough?)
        Result := True;
        Break;
      end
      else
      begin
        if startedAbove then
        begin
          if h < p1height then
            i := i + step;
          if (h - p1height) > 0 then
          begin
            step := step * 0.5;
            i := i - step;
          end;
        end
        else
        begin
          if h > p1height then
            i := i + step;
        end;
      end;
      Inc(failSafe);
      if failSafe > 1024 then
        Break;
      if VectorDotProduct(AbsZ, d) < 0 then
      begin
        if h < minH then
          Exit;
      end
      else if h > maxH then
        Exit;
    end;

    if Result then
    begin
      p1 := VectorAdd(p1, VectorScale(AbsZ, InterpolatedHeight(p1) - VectorDotProduct(p1, AbsZ)));
      if Assigned(intersectPoint) then
        intersectPoint^ := p1;

      // Calc Normal
      if Assigned(intersectNormal) then
      begin
        // Get 2 nearby points for cross-product
        AbsX := VectorNormalize(LocalToAbsolute(XHMGVector));
        AbsY := VectorNormalize(LocalToAbsolute(YHMGVector));
        p2 := VectorAdd(p1, VectorScale(AbsX, 0.1));
        p2 := VectorAdd(p2, VectorScale(AbsZ, InterpolatedHeight(p2) - VectorDotProduct(p2, AbsZ)));
        p3 := VectorAdd(p1, VectorScale(AbsY, 0.1));
        p3 := VectorAdd(p3, VectorScale(AbsZ, InterpolatedHeight(p3) - VectorDotProduct(p3, AbsZ)));

        intersectNormal^ := VectorNormalize(VectorCrossProduct(VectorSubtract(p1, p2), VectorSubtract(p3, p1)));
      end;
    end;
  end;
end;

procedure TgxTerrainRenderer.ReleaseAllTiles;
var
  i, k: Integer;
  hd: TgxHeightData;
begin
  for i := 0 to cTilesHashSize do
    with FTilesHash[i] do
    begin
      for k := Count - 1 downto 0 do
      begin
        hd := TgxHeightData(Items[k]);
        OnTileDestroyed(hd);
        hd.OnDestroy := nil;
        hd.Release;
      end;
      Clear;
    end;
end;

procedure TgxTerrainRenderer.OnTileDestroyed(Sender: TObject);
var
  list: TList;
begin
  with Sender as TgxHeightData do
  begin
    if ObjectTag <> nil then
    begin
      ObjectTag.Free;
      ObjectTag := nil;
    end;
    list := FTilesHash[HashKey(xLeft, yTop)];
    Assert(Assigned(list));
    list.Remove(Sender);
  end;
end;

function TgxTerrainRenderer.InterpolatedHeight(const p: TVector4f): single;
var
  pLocal: TVector4f;
begin
  if Assigned(HeightDataSource) then
  begin
    pLocal := AbsoluteToLocal(p);
    Result := HeightDataSource.InterpolatedHeight(pLocal.X, pLocal.Y, TileSize + 1) * Scale.Z * (1 / 128);
  end
  else
    Result := 0;
end;

function TgxTerrainRenderer.InterpolatedHeight(const p: TAffineVector): single;
begin
  Result := InterpolatedHeight(PointMake(p));
end;

procedure TgxTerrainRenderer.BuildList(var rci: TgxRenderContextInfo);
var
  vEye, vEyeDirection: TVector4f;
  tilePos, AbsTilePos, Observer: TAffineVector;
  DeltaX, nbX, iX: Integer;
  DeltaY, nbY, iY: Integer;
  n, rpIdxDelta, AccumCount: Integer;
  f, TileRadius, TileGroundRadius, TexFactor, TileDist, qDist: single;
  Patch, PrevPatch: TgxROAMPatch;
  PatchList, RowList, prevRow, buf: TList;
  PostRenderPatchList, postRenderHeightDataList: TList;
  rcci: TgxRenderContextClippingInfo;
  CurrentMaterialName: String;
  MaxTilePosX, MaxTilePosY, MinTilePosX, MinTilePosY: single;
  t_l, t_t, t_r, t_b: single;

  procedure ApplyMaterial(const materialName: String);
  begin
    if (MaterialLibrary = nil) or (CurrentMaterialName = materialName) then
      Exit;
    // flush whatever is in progress
    TgxROAMPatch.FlushAccum(FBufferVertices, FBufferVertexIndices, FBufferTexPoints);
    // unapply current
    if CurrentMaterialName = '' then
    begin
      repeat
        // ... proper multipass support will be implemented later
      until not Material.UnApply(rci);
    end
    else
    begin
      repeat
        // ... proper multipass support will be implemented later
      until not MaterialLibrary.UnApplyMaterial(rci);
    end;
    // apply new
    if materialName = '' then
      Material.Apply(rci)
    else
      MaterialLibrary.ApplyMaterial(materialName, rci);
    CurrentMaterialName := materialName;
  end;

begin
  if csDesigning in ComponentState then
    Exit;
  if HeightDataSource = nil then
    Exit;

  CurrentMaterialName := '';
  // first project eye position into heightdata coordinates
  vEye := VectorTransform(rci.cameraPosition, InvAbsoluteMatrix);
  vEyeDirection := VectorTransform(rci.cameraDirection, InvAbsoluteMatrix);
  SetVector(Observer, vEye);
  vEye.X := Round(vEye.X * FinvTileSize - 0.5) * TileSize + TileSize * 0.5;
  vEye.Y := Round(vEye.Y * FinvTileSize - 0.5) * TileSize + TileSize * 0.5;
  TileGroundRadius := Sqr(TileSize * 0.5 * Scale.X) + Sqr(TileSize * 0.5 * Scale.Y);
  TileRadius := Sqrt(TileGroundRadius + Sqr(256 * Scale.Z));
  TileGroundRadius := Sqrt(TileGroundRadius);
  // now, we render a quad grid centered on eye position
  SetVector(tilePos, vEye);
  tilePos.Z := 0;
  f := (rci.rcci.farClippingDistance + TileGroundRadius) / Scale.X;
  f := Round(f * FinvTileSize + 1.0) * TileSize;
  MaxTilePosX := vEye.X + f;
  MaxTilePosY := vEye.Y + f;
  MinTilePosX := vEye.X - f;
  MinTilePosY := vEye.Y - f;

  if Assigned(FOnGetTerrainBounds) then
  begin
    // User-specified terrain bounds, may override ours
    t_l := MinTilePosX;
    t_t := MaxTilePosY;
    t_r := MaxTilePosX;
    t_b := MinTilePosY;

    FOnGetTerrainBounds(t_l, t_t, t_r, t_b);

    t_l := Round(t_l / TileSize - 0.5) * TileSize + TileSize * 0.5;
    t_t := Round(t_t / TileSize - 0.5) * TileSize - TileSize * 0.5;
    t_r := Round(t_r / TileSize - 0.5) * TileSize - TileSize * 0.5;
    t_b := Round(t_b / TileSize - 0.5) * TileSize + TileSize * 0.5;

    if MaxTilePosX > t_r then
      MaxTilePosX := t_r;
    if MaxTilePosY > t_t then
      MaxTilePosY := t_t;
    if MinTilePosX < t_l then
      MinTilePosX := t_l;
    if MinTilePosY < t_b then
      MinTilePosY := t_b;
  end;
  // if max is less than min, we have nothing to render
  if (MaxTilePosX < MinTilePosX) or (MaxTilePosY < MinTilePosY) then
    Exit;

  nbX := Round((MaxTilePosX - MinTilePosX) / TileSize);
  nbY := Round((MaxTilePosY - MinTilePosY) / TileSize);

  TexFactor := 1 / (TilesPerTexture * TileSize);
  rcci := rci.rcci;
  if QualityDistance > 0 then
    qDist := QualityDistance + TileRadius * 0.5
  else
    qDist := -1;

  SetROAMTrianglesCapacity(MaxCLODTriangles);
  n := MaxInteger(MaxCLODTriangles * 2, Integer(Sqr(TileSize + 1) * 2));
  FBufferVertices.Capacity := n;
  FBufferTexPoints.Capacity := n;

  xglPushState;
  try
    (*
      if GL_ARB_multitexture then
      xgl.MapTexCoordToDual
      else
      xgl.MapTexCoordToMain;
    *)
    xglMapTexCoordToDual;
    glPushMatrix;
    glScalef(1, 1, 1 / 128);
    glTranslatef(-0.5 * TileSize, -0.5 * TileSize, 0);
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_NORMAL_ARRAY);

    glVertexPointer(3, GL_FLOAT, 0, FBufferVertices.list);
    glTexCoordPointer(2, GL_FLOAT, 0, FBufferTexPoints.list);
  finally
    xglPopState;
  end;

  HeightDataSource.Data.LockList; // Lock out the HDS thread while rendering

  FLastTriangleCount := 0;
  PatchList := TList.Create;
  PatchList.Capacity := (nbX + 1) * (nbY + 1);
  RowList := TList.Create;
  prevRow := TList.Create;
  if Assigned(FOnPatchPostRender) then
    PostRenderPatchList := TList.Create
  else
    PostRenderPatchList := nil;
  if Assigned(FOnHeightDataPostRender) then
    postRenderHeightDataList := TList.Create
  else
    postRenderHeightDataList := nil;

  MarkAllTilesAsUnused;
  AbsoluteMatrix; // makes sure it is available

  // determine orientation (to render front-to-back)
  if vEyeDirection.X >= 0 then
    DeltaX := TileSize
  else
  begin
    DeltaX := -TileSize;
    MinTilePosX := MaxTilePosX;
  end;
  if vEyeDirection.Y >= 0 then
    DeltaY := TileSize
  else
  begin
    DeltaY := -TileSize;
    MinTilePosY := MaxTilePosY;
  end;

  TileRadius := TileRadius;

  tilePos.Y := MinTilePosY;
  for iY := 0 to nbY - 1 do
  begin
    tilePos.X := MinTilePosX;
    PrevPatch := nil;
    n := 0;
    for iX := 0 to nbX do
    begin
      AbsTilePos := VectorTransform(tilePos, DirectAbsoluteMatrix^);
      if not IsVolumeClipped(AbsTilePos, TileRadius, rcci.frustum) then
      begin
        Patch := GetPreparedPatch(tilePos, Observer, TexFactor, postRenderHeightDataList);

        if Patch <> nil then
        begin

          TileDist := VectorDistance(PAffineVector(@rcci.origin)^, AbsTilePos);
          Patch.HighRes := (TileDist < qDist);

          if not Patch.HighRes then
            Patch.ResetTessellation;
          if Assigned(PrevPatch) then
          begin
            if DeltaX > 0 then
              Patch.ConnectToTheWest(PrevPatch)
            else
              PrevPatch.ConnectToTheWest(Patch);
          end;
          if (prevRow.Count > n) and (prevRow.Items[n] <> nil) then
          begin
            if DeltaY > 0 then
              Patch.ConnectToTheNorth(TgxROAMPatch(prevRow.Items[n]))
            else
              TgxROAMPatch(prevRow.Items[n]).ConnectToTheNorth(Patch);
          end;

          if Patch.HighRes then
          begin
            // high-res patches are issued immediately
            ApplyMaterial(Patch.HeightData.materialName);
            Patch.RenderHighRes(FBufferVertices, FBufferVertexIndices, FBufferTexPoints, (QualityStyle = hrsTesselated));
            FLastTriangleCount := FLastTriangleCount + Patch.TriangleCount;
          end
          else
          begin
            // CLOD patches are issued after tesselation
            PatchList.Add(Patch);
          end;

          PrevPatch := Patch;
          RowList.Add(Patch);

          if Assigned(PostRenderPatchList) then
            PostRenderPatchList.Add(Patch);
        end
        else
        begin
          PrevPatch := nil;
          RowList.Add(nil);
        end;
      end
      else
      begin
        MarkHashedTileAsUsed(tilePos);
        PrevPatch := nil;
        RowList.Add(nil);
      end;
      tilePos.X := tilePos.X + DeltaX;
      Inc(n);
    end;
    tilePos.Y := tilePos.Y + DeltaY;
    buf := prevRow;
    prevRow := RowList;
    RowList := buf;
    RowList.Count := 0;
  end;

  AccumCount := FBufferVertexIndices.Capacity shr 3;

  // Interleave Tesselate and Render so we can send some work to the hardware
  // while the CPU keeps working
  rpIdxDelta := Round(2 * f / TileSize) + 2;
  for n := 0 to PatchList.Count - 1 + rpIdxDelta do
  begin
    if n < PatchList.Count then
    begin
      Patch := TgxROAMPatch(PatchList[n]);
      if Assigned(Patch) then
      begin
        if (Patch.LastOcclusionTestPassed) or (Patch.OcclusionCounter <= 0) or (OcclusionTesselate = totTesselateAlways) then
          Patch.SafeTesselate;
      end;
    end;
    if n >= rpIdxDelta then
    begin
      Patch := TgxROAMPatch(PatchList[n - rpIdxDelta]);
      if Assigned(Patch) then
      begin
        ApplyMaterial(Patch.HeightData.materialName);
        Patch.RenderAccum(FBufferVertices, FBufferVertexIndices, FBufferTexPoints, AccumCount);
        Inc(FLastTriangleCount, Patch.TriangleCount);
      end;
    end;
  end;

  if (GetROAMTrianglesCapacity > MaxCLODTriangles) and Assigned(FOnMaxCLODTrianglesReached) then
  begin
    FOnMaxCLODTrianglesReached(rci);
    // Fire an event if the MaxCLODTriangles limit was reached
  end;

  TgxROAMPatch.FlushAccum(FBufferVertices, FBufferVertexIndices, FBufferTexPoints);

  xglPushState;
  try
    (*
      if GL_ARB_multitexture then
      xgl.MapTexCoordToDual
      else
      xgl.MapTexCoordToMain;
    *)
    xglMapTexCoordToDual;
    glDisableClientState(GL_VERTEX_ARRAY);
    xglDisableClientState(GL_TEXTURE_COORD_ARRAY);
  finally
    xglPopState;
  end;

  ApplyMaterial('');
  if Assigned(PostRenderPatchList) then
  begin
    FOnPatchPostRender(rci, PostRenderPatchList);
    PostRenderPatchList.Free;
  end;
  if Assigned(postRenderHeightDataList) then
  begin
    FOnHeightDataPostRender(rci, postRenderHeightDataList);
    postRenderHeightDataList.Free;
  end;

  glPopMatrix;

  if (tmReleaseUnusedTiles in TileManagement) then
  begin // Tile cache management option
    ReleaseAllUnusedTiles;
    HeightDataSource.CleanUp;
  end;

  RowList.Free;
  prevRow.Free;
  PatchList.Free;

  HeightDataSource.Data.UnLockList;
end;

procedure TgxTerrainRenderer.MarkAllTilesAsUnused;
var
  i, j, zero: Integer;
begin
  if not(tmClearUsedFlags in TileManagement) then
    Exit; // Tile cache management option
  for i := 0 to cTilesHashSize do
    with FTilesHash[i] do
    begin
      zero := 0;
      for j := Count - 1 downto 0 do
        TgxHeightData(Items[j]).Tag := zero;
    end;
end;

procedure TgxTerrainRenderer.ReleaseAllUnusedTiles;
var
  i, j: Integer;
  hashList: TList;
  hd: TgxHeightData;
begin
  for i := 0 to cTilesHashSize do
  begin
    hashList := FTilesHash[i];
    for j := hashList.Count - 1 downto 0 do
    begin
      hd := TgxHeightData(hashList.Items[j]);
      if hd.Tag = 0 then
      begin
        hashList.Delete(j);
        OnTileDestroyed(hd);
        hd.OnDestroy := nil;
        hd.Release;
      end;
    end;
  end;
end;

function TgxTerrainRenderer.HashedTileCount: Integer;
var
  i: Integer;
  hashList: TList;
  cnt: Integer;
begin
  cnt := 0;
  for i := 0 to cTilesHashSize do
  begin
    hashList := FTilesHash[i]; // get the number of tiles in each list
    cnt := cnt + hashList.Count; // Add the current list's count to the total
  end;
  Result := cnt;
end;

procedure TgxTerrainRenderer.MarkHashedTileAsUsed(const tilePos: TAffineVector);
var
  hd: TgxHeightData;
  canAllocate: Boolean;
begin
  if not(tmMarkUsedTiles in TileManagement) then
    Exit; // Mark used tiles option
  canAllocate := tmAllocateNewTiles in TileManagement;
  // Allocate tile if not in the list
  hd := HashedTile(tilePos, canAllocate);
  if Assigned(hd) then
    hd.Tag := 1;
end;

function TgxTerrainRenderer.HashedTile(const tilePos: TAffineVector; canAllocate: Boolean = True): TgxHeightData;
var
  xLeft, yTop: Integer;
begin
  xLeft := Round(tilePos.X * FinvTileSize - 0.5) * (TileSize);
  yTop := Round(tilePos.Y * FinvTileSize - 0.5) * (TileSize);
  Result := HashedTile(xLeft, yTop, canAllocate);
end;

function TgxTerrainRenderer.HashedTile(const xLeft, yTop: Integer; canAllocate: Boolean = True): TgxHeightData;
var
  i: Integer;
  hd: TgxHeightData;
  hashList: TList;
begin
  // is the tile already in our list?
  hashList := FTilesHash[HashKey(xLeft, yTop)];
  for i := hashList.Count - 1 downto 0 do
  begin
    hd := TgxHeightData(hashList.Items[i]);
    if (hd.xLeft = xLeft) and (hd.yTop = yTop) then
    begin
      if hd.DontUse then
      begin
        // This tile has now been replaced. Remove it from the hash-table.
        hashList.Remove(hd);
      end
      else
      begin
        Result := hd;
        Exit;
      end;
    end;
  end;
  // if not, request it
  if canAllocate then
  begin
    Result := HeightDataSource.GetData(xLeft, yTop, TileSize + 1, hdtSmallInt);
    Result.RegisterUse;
    Result.OnDestroy := OnTileDestroyed;
    if Result.DataState <> hdsNone then
      Result.DataType := hdtSmallInt;
    FTilesHash[HashKey(xLeft, yTop)].Add(Result);
  end
  else
    Result := nil;
end;

function TgxTerrainRenderer.GetPreparedPatch(const tilePos, EyePos: TAffineVector; TexFactor: single; HDList: TList)
  : TgxROAMPatch;
var
  Tile: TgxHeightData;
  Patch: TgxROAMPatch;
  xLeft, yTop: Integer;
  canAllocate: Boolean;
begin
  canAllocate := tmAllocateNewTiles in TileManagement;
  xLeft := Round(tilePos.X * FinvTileSize - 0.5) * TileSize;
  yTop := Round(tilePos.Y * FinvTileSize - 0.5) * TileSize;
  Tile := HashedTile(xLeft, yTop, canAllocate);
  Result := nil;
  if not Assigned(Tile) then
    Exit;

  if (tmClearUsedFlags in TileManagement) // Tile cache management option
  then
    Tile.Tag := 1; // mark tile as used
  if Assigned(HDList) then
    HDList.Add(Tile);

  // if tile.DataState=hdsNone then begin
  if Tile.DataState <> hdsReady then
  begin
    Result := nil; // if the tile is still not hdsReady, then skip it
  end
  else
  begin
    Patch := TgxROAMPatch(Tile.ObjectTag);
    if not Assigned(Patch) then
    begin
      // spawn ROAM patch
      Patch := TgxROAMPatch.Create;
      Patch.ContourInterval := ContourInterval;
      Patch.ContourWidth := ContourWidth;
      Tile.ObjectTag := Patch;
      Patch.HeightData := Tile;
      Patch.VertexScale := XYZVector;
      Patch.VertexOffset := tilePos;
      Patch.OcclusionSkip := OcclusionFrameSkip;
      case Tile.TextureCoordinatesMode of
        tcmWorld:
          begin
            Patch.TextureScale := AffineVectorMake(TexFactor, -TexFactor, TexFactor);
            Patch.TextureOffset := AffineVectorMake(xLeft * TexFactor, 1 - yTop * TexFactor, 0);
          end;
        tcmLocal:
          begin
            with Tile.TextureCoordinatesScale do
              Patch.TextureScale := AffineVectorMake(TexFactor * S, -TexFactor * t, TexFactor);
            with Tile.TextureCoordinatesOffset do
              Patch.TextureOffset := AffineVectorMake(0 + S, 1 + t, 0);
          end;
      else
        Assert(False);
      end;
      Patch.ComputeVariance(FCLODPrecision);
    end;
    Patch.ObserverPosition := VectorSubtract(EyePos, tilePos);
    Result := Patch;
  end;
end;

procedure TgxTerrainRenderer.SetHeightDataSource(const val: TgxHeightDataSource);
begin
  if FHeightDataSource <> val then
  begin
    if Assigned(FHeightDataSource) then
    begin
      FHeightDataSource.RemoveFreeNotification(Self);
      ReleaseAllTiles;
      FHeightDataSource.Clear;
    end;
    FHeightDataSource := val;
    if Assigned(FHeightDataSource) then
      FHeightDataSource.FreeNotification(Self);
    StructureChanged;
  end;
end;

procedure TgxTerrainRenderer.SetTileSize(const val: Integer);
begin
  if val <> FTileSize then
  begin
    if val < 8 then
      FTileSize := 8
    else
      FTileSize := RoundUpToPowerOf2(val);
    FinvTileSize := 1 / FTileSize;
    ReleaseAllTiles;
    StructureChanged;
  end;
end;

procedure TgxTerrainRenderer.SetTilesPerTexture(const val: single);
begin
  if val <> FTilesPerTexture then
  begin
    FTilesPerTexture := val;
    StructureChanged;
  end;
end;

procedure TgxTerrainRenderer.SetCLODPrecision(const val: Integer);
var
  i, k: Integer;
  hd: TgxHeightData;
begin
  if val <> FCLODPrecision then
  begin
    FCLODPrecision := val;
    if FCLODPrecision < 1 then
      FCLODPrecision := 1;
    // drop all ROAM data (CLOD has changed, rebuild required)
    for i := 0 to cTilesHashSize do
      with FTilesHash[i] do
      begin
        for k := Count - 1 downto 0 do
        begin
          hd := TgxHeightData(Items[k]);
          if Assigned(hd.ObjectTag) then
          begin
            (hd.ObjectTag as TgxROAMPatch).Free;
            hd.ObjectTag := nil;
          end;
        end;
        Clear;
      end;
  end;
end;

procedure TgxTerrainRenderer.SetMaterialLibrary(const val: TgxMaterialLibrary);
begin
  if val <> FMaterialLibrary then
  begin
    FMaterialLibrary := val;
    StructureChanged;
  end;
end;

procedure TgxTerrainRenderer.SetQualityStyle(const val: TgxTerrainHighResStyle);
begin
  if val <> FQualityStyle then
  begin
    FQualityStyle := val;
    StructureChanged;
  end;
end;

procedure TgxTerrainRenderer.SetOcclusionFrameSkip(val: Integer);
var
  i, k: Integer;
  hd: TgxHeightData;
begin
  if val < 0 then
    val := 0;
  if FOcclusionFrameSkip <> val then
  begin
    FOcclusionFrameSkip := val;
    for i := 0 to cTilesHashSize do
      with FTilesHash[i] do
      begin
        for k := Count - 1 downto 0 do
        begin
          hd := TgxHeightData(Items[k]);
          if hd.ObjectTag <> nil then
            TgxROAMPatch(hd.ObjectTag).OcclusionSkip := OcclusionFrameSkip;
        end;
      end;
    NotifyChange(Self);
  end;
end;

// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------


RegisterClass(TgxTerrainRenderer);

end.
