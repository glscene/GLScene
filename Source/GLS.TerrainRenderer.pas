//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.TerrainRenderer;

(*
  Brute-force terrain renderer.
  NOTA : multi-materials terrain support is not yet optimized to minimize
  texture switches (in case of resued tile textures).
*)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,
  System.Types,

  GLS.OpenGLTokens,
  GLS.Scene,
  GLS.Coordinates,
  GLS.HeightData,
  GLS.Material,
  GLS.VectorGeometry,
  GLS.Context,
  GLS.ROAMPatch,
  GLS.VectorLists,
  GLS.RenderContextInfo,
  GLS.XOpenGL,
  GLS.Utils,
  GLS.VectorTypes,
  GLS.Texture;

const
  cTilesHashSize = 255;

type
  TGetTerrainBoundsEvent = procedure(var l, t, r, b: Single) of object;
  TPatchPostRenderEvent = procedure(var rci: TGLRenderContextInfo; const patches: TList) of object;
  TGLHeightDataPostRenderEvent = procedure(var rci: TGLRenderContextInfo;
    var HeightDatas: TList) of object;
  TMaxCLODTrianglesReachedEvent = procedure(var rci: TGLRenderContextInfo)
    of object;

  TTerrainHighResStyle = (hrsFullGeometry, hrsTesselated);
  TTerrainOcclusionTesselate = (totTesselateAlways, totTesselateIfVisible);

  TTileManagementFlag = (tmClearUsedFlags, tmMarkUsedTiles,
    tmReleaseUnusedTiles, tmAllocateNewTiles, tmWaitForPreparing);
  TTileManagementFlags = set of TTileManagementFlag;

  (*  Basic terrain renderer. 
    This renderer uses no sophisticated meshing, it just builds and maintains
    a set of terrain tiles, performs basic visibility culling and renders its
    stuff. You can use it has a base class/sample for more specialized
    terrain renderers. 
    The Terrain heightdata is retrieved directly from a TGLHeightDataSource, and
    expressed as z=f(x, y) data. *)
  TGLTerrainRenderer = class(TGLSceneObject)
  private
    FHeightDataSource: TGLHeightDataSource;
    FTileSize: Integer;
    FQualityDistance, FinvTileSize: Single;
    FLastTriangleCount: Integer;
    FTilesPerTexture: Single;
    FMaxCLODTriangles, FCLODPrecision: Integer;
    FBufferVertices: TGLAffineVectorList;
    FBufferTexPoints: TGLTexPointList;
    FBufferVertexIndices: TGLIntegerList;
    FMaterialLibrary: TGLMaterialLibrary;
    FOnGetTerrainBounds: TGetTerrainBoundsEvent;
    FOnPatchPostRender: TPatchPostRenderEvent;
    FOnHeightDataPostRender: TGLHeightDataPostRenderEvent;
    FOnMaxCLODTrianglesReached: TMaxCLODTrianglesReachedEvent;
    FQualityStyle: TTerrainHighResStyle;
    FOcclusionFrameSkip: Integer;
    FOcclusionTesselate: TTerrainOcclusionTesselate;
    FContourInterval: Integer;
    FContourWidth: Integer;
  protected
    FTilesHash: packed array [0 .. cTilesHashSize] of TList;
    procedure MarkAllTilesAsUnused;
    procedure ReleaseAllUnusedTiles;
    procedure MarkHashedTileAsUsed(const tilePos: TAffineVector);
    function HashedTile(const tilePos: TAffineVector; canAllocate: Boolean = True)
	  : TGLHeightData; overload;
    function HashedTile(const xLeft, yTop: Integer; canAllocate: Boolean = True)
      : TGLHeightData; overload;
    procedure SetHeightDataSource(const val: TGLHeightDataSource);
    procedure SetTileSize(const val: Integer);
    procedure SetTilesPerTexture(const val: Single);
    procedure SetCLODPrecision(const val: Integer);
    procedure SetMaterialLibrary(const Val: TGLMaterialLibrary);
    procedure SetQualityStyle(const Val: TTerrainHighResStyle);
    procedure SetOcclusionFrameSkip(Val: Integer);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DestroyHandle; override;
    procedure ReleaseAllTiles;
    procedure OnTileDestroyed(Sender: TObject);
    function GetPreparedPatch(const TilePos, EyePos: TAffineVector;
      TexFactor: Single; HDList: TList): TGLROAMPatch;
  public
   (* TileManagement flags can be used to turn off various Tile cache management features.
      This helps to prevent unnecessary tile cache flushes, when rendering from multiple cameras *)
    TileManagement: TTileManagementFlags;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    function RayCastIntersect(const rayStart, rayVector: TGLVector;
      intersectPoint: PGLVector = nil; intersectNormal: PGLVector = nil): Boolean; override;
    (* Interpolates height for the given point.
       Expects a point expressed in absolute coordinates. *)
    function InterpolatedHeight(const p: TGLVector): Single; overload;
    function InterpolatedHeight(const p: TAffineVector): Single; overload;
    // Triangle count for the last render
    property LastTriangleCount: Integer read FLastTriangleCount;
    function HashedTileCount: Integer;
  published
    // Specifies the HeightData provider component.
    property HeightDataSource: TGLHeightDataSource read FHeightDataSource
      write SetHeightDataSource;
    //  Size of the terrain tiles. Must be a power of two.
    property TileSize: Integer read FTileSize write SetTileSize default 16;
    // Number of tiles required for a full texture map.
    property TilesPerTexture: Single read FTilesPerTexture write SetTilesPerTexture;
    (* Link to the material library holding terrain materials.
      If unspecified, and for all terrain tiles with unspecified material,
      the terrain renderer's material is used. *)
    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    (* Quality distance hint.
      This parameter gives an hint to the terrain renderer at which distance
      the terrain quality can be degraded to favor speed. The distance is
      expressed in absolute coordinates units.
      All tiles closer than this distance are rendered according to
      QualityStyle and with a static resolution. *)
    property QualityDistance: Single read FQualityDistance write FQualityDistance;
    (* Determines how high-res tiles (closer than QualityDistance) are rendered.
      hrsFullGeometry (default value) means that the high-res tiles are rendered
      with full-geometry, and no LOD of any kind, while hrsTesselated means
      the tiles will be tesselated once, with the best output for the
      CLODPrecision, and the result of that tesselation will be reused
      in further frames without any adpative tesselation. *)
    property QualityStyle: TTerrainHighResStyle read FQualityStyle
      write SetQualityStyle default hrsFullGeometry;
    (* Maximum number of CLOD triangles per scene.
       Triangles in high-resolution tiles (closer than QualityDistance) do
       not count toward this limit. *)
    property MaxCLODTriangles: Integer read FMaxCLODTriangles
      write FMaxCLODTriangles default 65536;
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
      This optimization requires the hardware to support GL_NV_occlusion_query *)
    property OcclusionFrameSkip: Integer read FOcclusionFrameSkip
      write SetOcclusionFrameSkip default 0;
    (* Determines if and how occlusion testing affects tesselation.
      Turning off tesselation of tiles determined invisible can improve
      performance, however, it may result in glitches since the tesselation
      of an invisible tile can have a slight effect on the tesselation
      of its adjacent tiles (by forcing higher resolution at the border
      for instance). This negative effect can be lessened by increasing
      the QualityDistance, so that glitches will appear farther away
      (this will mean increasing your triangle count though, so you'll
      trade CPU power against T&L power). *)
    property OcclusionTesselate: TTerrainOcclusionTesselate
      read FOcclusionTesselate write FOcclusionTesselate default totTesselateIfVisible;
    (* Allows to specify terrain bounds.
      Default rendering bounds will reach depth of view in all direction,
      with this event you can chose to specify a smaller rendered terrain area *)
    property OnGetTerrainBounds: TGetTerrainBoundsEvent read FOnGetTerrainBounds
      write FOnGetTerrainBounds;
    (* Invoked for each rendered patch after terrain render has completed.
      The list holds TGLROAMPatch objects and allows per-patch
      post-processings, like waters, trees... It is invoked *before*
      OnHeightDataPostRender. *)
    property OnPatchPostRender: TPatchPostRenderEvent read FOnPatchPostRender
      write FOnPatchPostRender;
    (* Invoked for each heightData not culled out by the terrain renderer.
      The list holds TGLHeightData objects and allows per-patch
      post-processings, like waters, trees... It is invoked *after*
      OnPatchPostRender. *)
    property OnHeightDataPostRender: TGLHeightDataPostRenderEvent
      read FOnHeightDataPostRender write FOnHeightDataPostRender;
    (* Invoked whenever the MaxCLODTriangles limit was reached during last rendering.
      This forced the terrain renderer to resize the buffer, which affects performance.
      If this event is fired frequently, one should increase MaxCLODTriangles. *)
    property OnMaxCLODTrianglesReached: TMaxCLODTrianglesReachedEvent
      read FOnMaxCLODTrianglesReached write FOnMaxCLODTrianglesReached;
    //  Distance between contours - zero (default) for no contours  PGS
    property ContourInterval: Integer read FContourInterval
      write FContourInterval default 0;
    // Width of Isolines
    property ContourWidth: Integer read FContourWidth
      write FContourWidth default 1;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

function HashKey(const XLeft, YTop: Integer): Integer;
begin
  Result := (XLeft + (XLeft shr 8) + (XLeft shr 16) + (YTop shl 1) +
    (YTop shr 9) + (YTop shr 17)) and cTilesHashSize;
end;


// ------------------
// ------------------ TGLTerrainRenderer ------------------
// ------------------

constructor TGLTerrainRenderer.Create(AOwner: TComponent);
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
  FBufferVertices := TGLAffineVectorList.Create;
  FBufferTexPoints := TGLTexPointList.Create;
  FBufferVertexIndices := TGLIntegerList.Create;
  TileManagement := [tmClearUsedFlags, tmMarkUsedTiles, tmReleaseUnusedTiles,
    tmAllocateNewTiles];
end;

destructor TGLTerrainRenderer.Destroy;
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

procedure TGLTerrainRenderer.Notification(AComponent: TComponent;
  Operation: TOperation);
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

procedure TGLTerrainRenderer.DestroyHandle;
begin
  inherited;
  ReleaseAllTiles;
  if Assigned(HeightDataSource) then
    HeightDataSource.Clear;
end;

function TGLTerrainRenderer.RayCastIntersect(const rayStart, rayVector: TGLVector;
  intersectPoint: PGLVector = nil; intersectNormal: PGLVector = nil): Boolean;
var
  p1, d, p2, p3: TGLVector;
  step, i, h, minH, maxH, p1height: Single;
  startedAbove: Boolean;
  failSafe: Integer;
  AbsX, AbsY, AbsZ: TGLVector;
begin
  Result := False;
  if Assigned(HeightDataSource) then
  begin
    step := (Scale.X + Scale.Y); // Initial step size guess
    i := step;
    d := VectorNormalize(rayVector);
    AbsZ := VectorNormalize(LocalToAbsolute(ZHMGVector));
    startedAbove := ((InterpolatedHeight(rayStart) - VectorDotProduct(rayStart,
      AbsZ)) < 0);
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
      p1 := VectorAdd(p1, VectorScale(AbsZ, InterpolatedHeight(p1) -
        VectorDotProduct(p1, AbsZ)));
      if Assigned(intersectPoint) then
        intersectPoint^ := p1;

      // Calc Normal
      if Assigned(intersectNormal) then
      begin
        // Get 2 nearby points for cross-product
        AbsX := VectorNormalize(LocalToAbsolute(XHMGVector));
        AbsY := VectorNormalize(LocalToAbsolute(YHMGVector));
        p2 := VectorAdd(p1, VectorScale(AbsX, 0.1));
        p2 := VectorAdd(p2, VectorScale(AbsZ, InterpolatedHeight(p2) -
          VectorDotProduct(p2, AbsZ)));
        p3 := VectorAdd(p1, VectorScale(AbsY, 0.1));
        p3 := VectorAdd(p3, VectorScale(AbsZ, InterpolatedHeight(p3) -
          VectorDotProduct(p3, AbsZ)));

        intersectNormal^ :=
          VectorNormalize(VectorCrossProduct(VectorSubtract(p1, p2),
          VectorSubtract(p3, p1)));
      end;
    end;
  end;
end;

procedure TGLTerrainRenderer.ReleaseAllTiles;
var
  i, k: Integer;
  hd: TGLHeightData;
begin
  for i := 0 to cTilesHashSize do
    with FTilesHash[i] do
    begin
      for k := Count - 1 downto 0 do
      begin
        hd := TGLHeightData(Items[k]);
        OnTileDestroyed(hd);
        hd.OnDestroy := nil;
        hd.Release;
      end;
      Clear;
    end;
end;

procedure TGLTerrainRenderer.OnTileDestroyed(Sender: TObject);
var
  List: TList;
begin
  with Sender as TGLHeightData do
  begin
    if ObjectTag <> nil then
    begin
      ObjectTag.Free;
      ObjectTag := nil;
    end;
    List := FTilesHash[HashKey(xLeft, yTop)];
    Assert(Assigned(List));
    List.Remove(Sender);
  end;
end;

function TGLTerrainRenderer.InterpolatedHeight(const p: TGLVector): Single;
var
  pLocal: TGLVector;
begin
  if Assigned(HeightDataSource) then
  begin
    pLocal := AbsoluteToLocal(p);
    Result := HeightDataSource.InterpolatedHeight(pLocal.X, pLocal.Y,
      TileSize + 1) * Scale.Z * (1 / 128);
  end
  else
    Result := 0;
end;

function TGLTerrainRenderer.InterpolatedHeight(const p: TAffineVector): Single;
begin
  Result := InterpolatedHeight(PointMake(p));
end;

procedure TGLTerrainRenderer.BuildList(var rci: TGLRenderContextInfo);
var
  vEye, vEyeDirection: TGLVector;
  TilePos, AbsTilePos, Observer: TAffineVector;
  DeltaX, nbX, iX: Integer;
  DeltaY, nbY, iY: Integer;
  n, rpIdxDelta, AccumCount: Integer;
  f, TileRadius, TileGroundRadius, TexFactor, TileDist, qDist: Single;
  Patch, PrevPatch: TGLROAMPatch;
  PatchList, RowList, prevRow, buf: TList;
  PostRenderPatchList, postRenderHeightDataList: TList;
  rcci: TRenderContextClippingInfo;
  CurrentMaterialName: String;
  MaxTilePosX, MaxTilePosY, MinTilePosX, MinTilePosY: Single;
  t_l, t_t, t_r, t_b: Single;

  procedure ApplyMaterial(const materialName: String);
  begin
    if (MaterialLibrary = nil) or (currentMaterialName = materialName) then
      Exit;
    // flush whatever is in progress
    TGLROAMPatch.FlushAccum(FBufferVertices, FBufferVertexIndices,
      FBufferTexPoints);
    // unapply current
    if currentMaterialName = '' then
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
    CurrentMaterialName := MaterialName;
  end;

begin
  if csDesigning in ComponentState then
    Exit;
  if HeightDataSource = nil then
    Exit;
  currentMaterialName := '';
  // first project eye position into heightdata coordinates
  vEye := VectorTransform(rci.cameraPosition, InvAbsoluteMatrix);
  vEyeDirection := VectorTransform(rci.cameraDirection, InvAbsoluteMatrix);
  SetVector(observer, vEye);
  vEye.X := Round(vEye.X * FinvTileSize - 0.5) * TileSize +
    TileSize * 0.5;
  vEye.Y := Round(vEye.Y * FinvTileSize - 0.5) * TileSize +
    TileSize * 0.5;
  tileGroundRadius := Sqr(TileSize * 0.5 * Scale.X) +
    Sqr(TileSize * 0.5 * Scale.Y);
  tileRadius := Sqrt(tileGroundRadius + Sqr(256 * Scale.Z));
  tileGroundRadius := Sqrt(tileGroundRadius);
  // now, we render a quad grid centered on eye position
  SetVector(tilePos, vEye);
  tilePos.Z := 0;
  f := (rci.rcci.farClippingDistance + tileGroundRadius) / Scale.X;
  f := Round(f * FinvTileSize + 1.0) * TileSize;
  maxTilePosX := vEye.X + f;
  maxTilePosY := vEye.Y + f;
  minTilePosX := vEye.X - f;
  minTilePosY := vEye.Y - f;

  if Assigned(FOnGetTerrainBounds) then
  begin
    // User-specified terrain bounds, may override ours
    t_l := minTilePosX;
    t_t := maxTilePosY;
    t_r := maxTilePosX;
    t_b := minTilePosY;

    FOnGetTerrainBounds(t_l, t_t, t_r, t_b);

    t_l := Round(t_l / TileSize - 0.5) * TileSize + TileSize * 0.5;
    t_t := Round(t_t / TileSize - 0.5) * TileSize - TileSize * 0.5;
    t_r := Round(t_r / TileSize - 0.5) * TileSize - TileSize * 0.5;
    t_b := Round(t_b / TileSize - 0.5) * TileSize + TileSize * 0.5;

    if maxTilePosX > t_r then
      maxTilePosX := t_r;
    if maxTilePosY > t_t then
      maxTilePosY := t_t;
    if minTilePosX < t_l then
      minTilePosX := t_l;
    if minTilePosY < t_b then
      minTilePosY := t_b;
  end;
  // if max is less than min, we have nothing to render
  if (maxTilePosX < minTilePosX) or (maxTilePosY < minTilePosY) then
    Exit;

  nbX := Round((maxTilePosX - minTilePosX) / TileSize);
  nbY := Round((maxTilePosY - minTilePosY) / TileSize);

  texFactor := 1 / (TilesPerTexture * TileSize);
  rcci := rci.rcci;
  if QualityDistance > 0 then
    qDist := QualityDistance + tileRadius * 0.5
  else
    qDist := -1;

  SetROAMTrianglesCapacity(MaxCLODTriangles);
  n := MaxInteger(MaxCLODTriangles * 2, Integer(Sqr(TileSize + 1) * 2));
  FBufferVertices.Capacity := n;
  FBufferTexPoints.Capacity := n;

  xgl.PushState;
  try
    if GL.ARB_multitexture then
      xgl.MapTexCoordToDual
    else
      xgl.MapTexCoordToMain;

    gl.PushMatrix;
    gl.Scalef(1, 1, 1 / 128);
    gl.Translatef(-0.5 * TileSize, -0.5 * TileSize, 0);
    gl.EnableClientState(GL_VERTEX_ARRAY);
    xgl.EnableClientState(GL_TEXTURE_COORD_ARRAY);
    gl.DisableClientState(GL_COLOR_ARRAY);
    gl.DisableClientState(GL_NORMAL_ARRAY);

    gl.VertexPointer(3, GL_FLOAT, 0, FBufferVertices.list);
    xgl.TexCoordPointer(2, GL_FLOAT, 0, FBufferTexPoints.list);
  finally
    xgl.PopState;
  end;

  HeightDataSource.Data.LockList; // Lock out the HDS thread while rendering

  FLastTriangleCount := 0;
  patchList := TList.Create;
  patchList.Capacity := (nbX + 1) * (nbY + 1);
  rowList := TList.Create;
  prevRow := TList.Create;
  if Assigned(FOnPatchPostRender) then
    postRenderPatchList := TList.Create
  else
    postRenderPatchList := nil;
  if Assigned(FOnHeightDataPostRender) then
    postRenderHeightDataList := TList.Create
  else
    postRenderHeightDataList := nil;

  MarkAllTilesAsUnused;
  AbsoluteMatrix; // makes sure it is available

  // determine orientation (to render front-to-back)
  if vEyeDirection.X >= 0 then
    deltaX := TileSize
  else
  begin
    deltaX := -TileSize;
    minTilePosX := maxTilePosX;
  end;
  if vEyeDirection.Y >= 0 then
    deltaY := TileSize
  else
  begin
    deltaY := -TileSize;
    minTilePosY := maxTilePosY;
  end;

  TileRadius := TileRadius;

  TilePos.Y := minTilePosY;
  for iY := 0 to nbY - 1 do
  begin
    tilePos.X := minTilePosX;
    prevPatch := nil;
    n := 0;
    for iX := 0 to nbX do
    begin
      absTilePos := VectorTransform(tilePos, DirectAbsoluteMatrix^);
      if not IsVolumeClipped(absTilePos, tileRadius, rcci.frustum) then
      begin
        patch := GetPreparedPatch(tilePos, observer, texFactor,
          postRenderHeightDataList);

        if patch <> nil then
        begin

          tileDist := VectorDistance(PAffineVector(@rcci.origin)^, absTilePos);
          patch.HighRes := (tileDist < qDist);

          if not patch.HighRes then
            patch.ResetTessellation;
          if Assigned(prevPatch) then
          begin
            if deltaX > 0 then
              patch.ConnectToTheWest(prevPatch)
            else
              prevPatch.ConnectToTheWest(patch);
          end;
          if (prevRow.Count > n) and (prevRow.Items[n] <> nil) then
          begin
            if deltaY > 0 then
              patch.ConnectToTheNorth(TGLROAMPatch(prevRow.Items[n]))
            else
              TGLROAMPatch(prevRow.Items[n]).ConnectToTheNorth(patch);
          end;

          if patch.HighRes then
          begin
            // high-res patches are issued immediately
            ApplyMaterial(patch.HeightData.materialName);
            patch.RenderHighRes(FBufferVertices, FBufferVertexIndices,
              FBufferTexPoints, (QualityStyle = hrsTesselated));
            FLastTriangleCount := FLastTriangleCount + patch.TriangleCount;
          end
          else
          begin
            // CLOD patches are issued after tesselation
            patchList.Add(patch);
          end;

          prevPatch := patch;
          rowList.Add(patch);

          if Assigned(postRenderPatchList) then
            postRenderPatchList.Add(patch);
        end
        else
        begin
          prevPatch := nil;
          rowList.Add(nil);
        end;
      end
      else
      begin
        MarkHashedTileAsUsed(tilePos);
        prevPatch := nil;
        rowList.Add(nil);
      end;
      tilePos.X := tilePos.X + deltaX;
      Inc(n);
    end;
    tilePos.Y := tilePos.Y + deltaY;
    buf := prevRow;
    prevRow := rowList;
    rowList := buf;
    rowList.Count := 0;
  end;

  accumCount := FBufferVertexIndices.Capacity shr 3;

  // Interleave Tesselate and Render so we can send some work to the hardware
  // while the CPU keeps working
  rpIdxDelta := Round(2 * f / TileSize) + 2;
  for n := 0 to patchList.Count - 1 + rpIdxDelta do
  begin
    if n < patchList.Count then
    begin
      patch := TGLROAMPatch(patchList[n]);
      if Assigned(patch) then
      begin
        if (patch.LastOcclusionTestPassed) or (patch.OcclusionCounter <= 0) or
          (OcclusionTesselate = totTesselateAlways) then
          patch.SafeTesselate;
      end;
    end;
    if n >= rpIdxDelta then
    begin
      patch := TGLROAMPatch(patchList[n - rpIdxDelta]);
      if Assigned(patch) then
      begin
        ApplyMaterial(patch.HeightData.materialName);
        patch.RenderAccum(FBufferVertices, FBufferVertexIndices,
          FBufferTexPoints, accumCount);
        Inc(FLastTriangleCount, patch.TriangleCount);
      end;
    end;
  end;
  if (GetROAMTrianglesCapacity > MaxCLODTriangles) and
    Assigned(FOnMaxCLODTrianglesReached) then
  begin
    FOnMaxCLODTrianglesReached(rci);
    // Fire an event if the MaxCLODTriangles limit was reached
  end;
  TGLROAMPatch.FlushAccum(FBufferVertices, FBufferVertexIndices,
    FBufferTexPoints);

  xgl.PushState;
  try
    if gl.ARB_multitexture then
      xgl.MapTexCoordToDual
    else
      xgl.MapTexCoordToMain;

    gl.DisableClientState(GL_VERTEX_ARRAY);
    xgl.DisableClientState(GL_TEXTURE_COORD_ARRAY);
  finally
    xgl.PopState;
  end;

  ApplyMaterial('');
  if Assigned(postRenderPatchList) then
  begin
    FOnPatchPostRender(rci, postRenderPatchList);
    postRenderPatchList.Free;
  end;
  if Assigned(postRenderHeightDataList) then
  begin
    FOnHeightDataPostRender(rci, postRenderHeightDataList);
    postRenderHeightDataList.Free;
  end;

  gl.PopMatrix;

  if (tmReleaseUnusedTiles in TileManagement) then
  begin // Tile cache management option
    ReleaseAllUnusedTiles;
    HeightDataSource.CleanUp;
  end;

  rowList.Free;
  prevRow.Free;
  patchList.Free;

  HeightDataSource.Data.UnLockList;
end;

procedure TGLTerrainRenderer.MarkAllTilesAsUnused;
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
        TGLHeightData(Items[j]).Tag := zero;
    end;
end;

procedure TGLTerrainRenderer.ReleaseAllUnusedTiles;
var
  i, j: Integer;
  hashList: TList;
  hd: TGLHeightData;
begin
  for i := 0 to cTilesHashSize do
  begin
    hashList := FTilesHash[i];
    for j := hashList.Count - 1 downto 0 do
    begin
      hd := TGLHeightData(hashList.Items[j]);
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

function TGLTerrainRenderer.HashedTileCount: Integer;
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

procedure TGLTerrainRenderer.MarkHashedTileAsUsed(const tilePos: TAffineVector);
var
  hd: TGLHeightData;
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

function TGLTerrainRenderer.HashedTile(const tilePos: TAffineVector;
  canAllocate: Boolean = True): TGLHeightData;
var
  XLeft, YTop: Integer;
begin
  XLeft := Round(tilePos.X * FinvTileSize - 0.5) * (TileSize);
  YTop := Round(tilePos.Y * FinvTileSize - 0.5) * (TileSize);
  Result := HashedTile(xLeft, yTop, canAllocate);
end;

function TGLTerrainRenderer.HashedTile(const xLeft, yTop: Integer;
  canAllocate: Boolean = True): TGLHeightData;
var
  i: Integer;
  hd: TGLHeightData;
  hashList: TList;
begin
  // is the tile already in our list?
  hashList := FTilesHash[HashKey(xLeft, yTop)];
  for i := hashList.Count - 1 downto 0 do
  begin
    hd := TGLHeightData(hashList.Items[i]);
    if (hd.xLeft = xLeft) and (hd.yTop = yTop) then
    begin
      if hd.DontUse then
      begin
        // This tile has now been replaced. Remove it from the hash-table.
        HashList.Remove(hd);
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

function TGLTerrainRenderer.GetPreparedPatch(const TilePos,
  EyePos: TAffineVector; TexFactor: Single; HDList: TList): TGLROAMPatch;
var
  Tile: TGLHeightData;
  Patch: TGLROAMPatch;
  XLeft, YTop: Integer;
  CanAllocate: Boolean;
begin
  CanAllocate := tmAllocateNewTiles in TileManagement;
  XLeft := Round(TilePos.X * FinvTileSize - 0.5) * TileSize;
  YTop := Round(TilePos.Y * FinvTileSize - 0.5) * TileSize;
  Tile := HashedTile(xLeft, yTop, canAllocate);
  Result := nil;
  if not Assigned(tile) then
    Exit;

  if (tmClearUsedFlags in TileManagement) // Tile cache management option
  then
    Tile.Tag := 1; // mark tile as used
  if Assigned(hdList) then
    hdList.Add(tile);

  // if tile.DataState=hdsNone then begin
  if Tile.DataState <> hdsReady then
  begin
    Result := nil; // if the tile is still not hdsReady, then skip it
  end
  else
  begin
    Patch := TGLROAMPatch(Tile.ObjectTag);
    if not Assigned(patch) then
    begin
      // spawn ROAM patch
      Patch := TGLROAMPatch.Create;
      Patch.ContourInterval := ContourInterval;
      Patch.ContourWidth := ContourWidth;
      Tile.ObjectTag := patch;
      Patch.HeightData := tile;
      Patch.VertexScale := XYZVector;
      Patch.VertexOffset := tilePos;
      Patch.OcclusionSkip := OcclusionFrameSkip;
      case Tile.TextureCoordinatesMode of
        tcmWorld:
          begin
            Patch.TextureScale := AffineVectorMake(TexFactor, -TexFactor,
              texFactor);
            Patch.TextureOffset := AffineVectorMake(XLeft * TexFactor,
              1 - YTop * texFactor, 0);
          end;
        tcmLocal:
          begin
            with Tile.TextureCoordinatesScale do
              Patch.TextureScale := AffineVectorMake(texFactor * S,
                -texFactor * t, texFactor);
            with Tile.TextureCoordinatesOffset do
              Patch.TextureOffset := AffineVectorMake(0 + S, 1 + t, 0);
          end;
      else
        Assert(False);
      end;
      Patch.ComputeVariance(FCLODPrecision);
    end;
    Patch.ObserverPosition := VectorSubtract(eyePos, tilePos);
    Result := patch;
  end;
end;

procedure TGLTerrainRenderer.SetHeightDataSource(const val: TGLHeightDataSource);
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

procedure TGLTerrainRenderer.SetTileSize(const val: Integer);
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

procedure TGLTerrainRenderer.SetTilesPerTexture(const val: Single);
begin
  if val <> FTilesPerTexture then
  begin
    FTilesPerTexture := val;
    StructureChanged;
  end;
end;

procedure TGLTerrainRenderer.SetCLODPrecision(const val: Integer);
var
  i, k: Integer;
  hd: TGLHeightData;
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
          hd := TGLHeightData(Items[k]);
          if Assigned(hd.ObjectTag) then
          begin
            (hd.ObjectTag as TGLROAMPatch).Free;
            hd.ObjectTag := nil;
          end;
        end;
        Clear;
      end;
  end;
end;

procedure TGLTerrainRenderer.SetMaterialLibrary(const val: TGLMaterialLibrary);
begin
  if val <> FMaterialLibrary then
  begin
    FMaterialLibrary := val;
    StructureChanged;
  end;
end;

procedure TGLTerrainRenderer.SetQualityStyle(const val: TTerrainHighResStyle);
begin
  if val <> FQualityStyle then
  begin
    FQualityStyle := val;
    StructureChanged;
  end;
end;

procedure TGLTerrainRenderer.SetOcclusionFrameSkip(val: Integer);
var
  i, k: Integer;
  hd: TGLHeightData;
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
          hd := TGLHeightData(Items[k]);
          if hd.ObjectTag <> nil then
            TGLROAMPatch(hd.ObjectTag).OcclusionSkip := OcclusionFrameSkip;
        end;
      end;
    NotifyChange(Self);
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

RegisterClass(TGLTerrainRenderer);

end.
