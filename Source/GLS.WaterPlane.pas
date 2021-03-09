//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.WaterPlane;

(*
   A plane simulating animated water
   The Original Code is part of Cosmos4D
   http://users.hol.gr/~sternas/
*)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  System.Classes,
  Vcl.Graphics,

  GLS.OpenGLTokens,
  GLS.VectorGeometry,
  GLS.Scene,
  GLS.VectorLists,
  GLS.PersistentClasses,
  GLS.BaseClasses,
  GLS.Context,
  GLS.RenderContextInfo,
  GLS.VectorTypes,
  GLS.Utils;

type
   TGLWaterPlaneOption = (wpoTextured);
   TGLWaterPlaneOptions = set of TGLWaterPlaneOption;

const
   cDefaultWaterPlaneOptions = [wpoTextured];

 type
   TGLWaterPlane = class(TGLSceneObject)
   private
     FLocks: packed array of ByteBool;
     FPositions, FVelocity: packed array of Single;
     FPlaneQuadIndices: TPersistentObjectList;
     FPlaneQuadTexCoords: TTexPointList;
     FPlaneQuadVertices: TAffineVectorList;
     FPlaneQuadNormals: TAffineVectorList;
     FActive: Boolean;
     FRainTimeInterval: Integer;
     FRainForce: Single;
     FViscosity: Single;
     FElastic: Single;
     FResolution: Integer;
     FSimulationFrequency, FTimeToNextUpdate: Single;
     FTimeToNextRainDrop: Single;
     FMaximumCatchupIterations: Integer;
     FLastIterationStepTime: Single;
     FMask: TPicture;
     FOptions: TGLWaterPlaneOptions;
   protected
     procedure SetElastic(const value: Single);
     procedure SetResolution(const value: Integer);
     procedure SetRainTimeInterval(const val: Integer);
     procedure SetViscosity(const val: Single);
     procedure SetRainForce(const val: Single);
     procedure SetSimulationFrequency(const val: Single);
     procedure SetMask(val: TPicture);
     procedure SetOptions(const val: TGLWaterPlaneOptions);
     procedure DoMaskChanged(Sender: TObject);
     procedure InitResolution;
     procedure IterComputeVelocity;
     procedure IterComputePositions;
     procedure IterComputeNormals;
     procedure Iterate;
   public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     procedure DoProgress(const progressTime: TGLProgressTimes); override;
     procedure BuildList(var rci: TGLRenderContextInfo); override;
     procedure Assign(Source: TPersistent); override;
     function AxisAlignedDimensionsUnscaled: TGLVector; override;
     procedure CreateRippleAtGridPos(X, Y: Integer);
     procedure CreateRippleAtWorldPos(const X, Y, z: Single); overload;
     procedure CreateRippleAtWorldPos(const pos: TGLVector); overload;
     procedure CreateRippleRandom;
     procedure Reset;
     // CPU time (in seconds) taken by the last iteration step.
     property LastIterationStepTime: Single read FLastIterationStepTime;
   published
     property Active: Boolean read FActive write FActive default True;
     // Delay between raindrops in milliseconds (0 = no rain)
     property RainTimeInterval: Integer read FRainTimeInterval
       write SetRainTimeInterval default 500;
     property RainForce: Single read FRainForce write SetRainForce;
     property Viscosity: Single read FViscosity write SetViscosity;
     property Elastic: Single read FElastic write SetElastic;
     property Resolution: Integer read FResolution write SetResolution
       default 64;
     property Options: TGLWaterPlaneOptions read FOptions write SetOptions
       default cDefaultWaterPlaneOptions;
     (* A picture whose pixels determine what part of the waterplane is active.
       Pixels with a green/gray component beyond 128 are active, the others
       are not (in short, white = active, black = inactive).
       The picture will automatically be stretched to match the resolution. *)
     property Mask: TPicture read FMask write SetMask;
     // Maximum frequency (in Hz) at which simulation iterations happen.
     property SimulationFrequency: Single read FSimulationFrequency
       write SetSimulationFrequency;
     (* Maximum number of simulation iterations during catchups.
       Catchups happen when for a reason or another, the DoProgress doesn't
       happen as fast SimulationFrequency. *)
     property MaximumCatchupIterations: Integer read FMaximumCatchupIterations
       write FMaximumCatchupIterations default 1;
   end;

//-------------------------------------------------------------
implementation
//-------------------------------------------------------------

constructor TGLWaterPlane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw];

  FElastic := 10;
  FActive := True;
  FRainTimeInterval := 500;
  FRainForce := 5000;
  FViscosity := 0.99;
  FSimulationFrequency := 100; // 100 Hz
  FMaximumCatchupIterations := 1;
  FOptions := cDefaultWaterPlaneOptions;

  FPlaneQuadIndices := TPersistentObjectList.Create;
  FPlaneQuadTexCoords := TTexPointList.Create;
  FPlaneQuadVertices := TAffineVectorList.Create;
  FPlaneQuadNormals := TAffineVectorList.Create;
  FMask := TPicture.Create;
  FMask.OnChange := DoMaskChanged;
  SetResolution(64);
end;

destructor TGLWaterPlane.Destroy;
begin
  FMask.Free;
  FPlaneQuadNormals.Free;
  FPlaneQuadVertices.Free;
  FPlaneQuadTexCoords.Free;
  FPlaneQuadIndices.CleanFree;
  inherited;
end;

procedure TGLWaterPlane.DoProgress(const progressTime: TGLProgressTimes);
var
  i: Integer;
begin
  inherited;
  if Active and Visible then
  begin
    // new raindrops
    if FRainTimeInterval > 0 then
    begin
      FTimeToNextRainDrop := FTimeToNextRainDrop - progressTime.deltaTime;
      i := FMaximumCatchupIterations;
      while FTimeToNextRainDrop <= 0 do
      begin
        CreateRippleRandom;
        FTimeToNextRainDrop := FTimeToNextRainDrop + FRainTimeInterval * 0.001;
        Dec(i);
        if i < 0 then
        begin
          if FTimeToNextRainDrop < 0 then
            FTimeToNextRainDrop := FRainTimeInterval * 0.001;
          Break;
        end;
      end;
    end;
    // iterate simulation
    FTimeToNextUpdate := FTimeToNextUpdate - progressTime.deltaTime;
    if FTimeToNextUpdate <= 0 then
    begin
      i := FMaximumCatchupIterations;
      while FTimeToNextUpdate <= 0 do
      begin
        Iterate;
        FTimeToNextUpdate := FTimeToNextUpdate + 1 / FSimulationFrequency;
        Dec(i);
        if i < 0 then
        begin
          if FTimeToNextUpdate < 0 then
            FTimeToNextUpdate := 1 / FSimulationFrequency;
          Break;
        end;
      end;
      StructureChanged;
    end;
  end;
end;

procedure TGLWaterPlane.CreateRippleAtGridPos(X, Y: Integer);
begin
  if (X > 0) and (Y > 0) and (X < Resolution - 1) and (Y < Resolution - 1) then
    FVelocity[X + Y * Resolution] := FRainForce;
end;

procedure TGLWaterPlane.CreateRippleAtWorldPos(const X, Y, z: Single);
var
  vv: TGLVector;
begin
  vv := AbsoluteToLocal(PointMake(X, Y, z));
  CreateRippleAtGridPos(Round((vv.X + 0.5) * Resolution),
    Round((vv.z + 0.5) * Resolution));
end;

procedure TGLWaterPlane.CreateRippleAtWorldPos(const pos: TGLVector);
var
  vv: TGLVector;
begin
  vv := AbsoluteToLocal(PointMake(pos));
  CreateRippleAtGridPos(Round((vv.X + 0.5) * Resolution),
    Round((vv.z + 0.5) * Resolution));
end;

procedure TGLWaterPlane.CreateRippleRandom;
begin
  CreateRippleAtGridPos(Random(Resolution - 3) + 2, Random(Resolution - 3) + 2);
end;

procedure TGLWaterPlane.InitResolution;
var
  i, j: Integer;
  v: TAffineVector;
  resSqr: Integer;
  invResol: Single;
begin
  resSqr := FResolution * FResolution;
  FPlaneQuadIndices.Capacity := resSqr * 2;
  FPlaneQuadTexCoords.Clear;
  FPlaneQuadTexCoords.Capacity := resSqr;
  FPlaneQuadVertices.Clear;
  FPlaneQuadVertices.Capacity := resSqr;

  invResol := 1 / Resolution;
  for j := 0 to Resolution - 1 do
  begin
    for i := 0 to Resolution - 1 do
    begin
      FPlaneQuadTexCoords.Add(i * invResol, j * invResol);
      FPlaneQuadVertices.Add((i - Resolution * 0.5) * invResol, 0,
        (j - Resolution * 0.5) * invResol);
    end;
  end;

  FPlaneQuadNormals.Count := resSqr;
  v.X := 0;
  v.Y := 2048;
  v.z := 0;
  for i := 0 to FPlaneQuadNormals.Count - 1 do
    FPlaneQuadNormals.List[i] := v;

  SetLength(FPositions, resSqr);
  SetLength(FVelocity, resSqr);
  SetLength(FLocks, resSqr);

  Reset;
  Iterate;

  StructureChanged;
end;

procedure TGLWaterPlane.Reset;
var
  i, j, ij, resSqr: Integer;
  maskBmp: TBitmap;
  scanLine: PIntegerArray;
  il: TIntegerList;
  locked: Boolean;
begin
  resSqr := FResolution * FResolution;
  for i := 0 to resSqr - 1 do
  begin
    FPositions[i] := 0;
    FVelocity[i] := 0;
    FLocks[i] := False;
  end;
  if FMask.Width > 0 then
  begin
    maskBmp := TBitmap.Create;
    try
      maskBmp.PixelFormat := pf32bit;
      maskBmp.Width := Resolution;
      maskBmp.Height := Resolution;
      maskBmp.Canvas.StretchDraw(Rect(0, 0, Resolution, Resolution),
        FMask.Graphic);
      for j := 0 to Resolution - 1 do
      begin
        scanLine := maskBmp.scanLine[Resolution - 1 - j];
        for i := 0 to Resolution - 1 do
          FLocks[i + j * Resolution] := (((scanLine[i] shr 8) and $FF) < 128);
      end;
    finally
      maskBmp.Free;
    end;
  end;

  FPlaneQuadIndices.Clean;
  for j := 0 to Resolution - 2 do
  begin
    il := TIntegerList.Create;
    for i := 0 to Resolution - 1 do
    begin
      ij := i + j * Resolution;
      if (il.Count and 2) <> 0 then
        locked := False
      else
      begin
        locked := FLocks[ij] and FLocks[ij + Resolution];
        if locked and (i < Resolution - 1) then
          locked := FLocks[ij + 1] and FLocks[ij + Resolution + 1];
      end;
      if not locked then
        il.Add(ij, ij + Resolution)
      else if il.Count > 0 then
      begin
        FPlaneQuadIndices.Add(il);
        il := TIntegerList.Create;
      end;
    end;
    if il.Count > 0 then
      FPlaneQuadIndices.Add(il)
    else
      il.Free;
  end;
end;

procedure TGLWaterPlane.IterComputeVelocity;
var
  i, j, ij: Integer;
  f1, f2: Single;
  posList, velList: PSingleArray;
  lockList: PByteArray;
begin
  f1 := 0.05;
  f2 := 0.01 * FElastic;

  posList := @FPositions[0];
  velList := @FVelocity[0];
  lockList := @FLocks[0];
  for i := 1 to Resolution - 2 do
  begin
    ij := i * Resolution;
    for j := 1 to Resolution - 2 do
    begin
      Inc(ij);
      if lockList[ij] <> 0 then
        continue;
      velList[ij] := velList[ij] + f2 *
        (posList[ij] - f1 * (4 * (posList[ij - 1] + posList[ij + 1] +
        posList[ij - Resolution] + posList[ij + Resolution]) +
        posList[ij - 1 - Resolution] + posList[ij + 1 - Resolution] +
        posList[ij - 1 + Resolution] + posList[ij + 1 + Resolution]));
    end;
  end;
end;

procedure TGLWaterPlane.IterComputePositions;
const
  cVelocityIntegrationCoeff: Single = 0.02;
  cHeightFactor: Single = 1E-4;
var
  ij: Integer;
  f: Single;
  coeff: Single;
  posList, velList: PSingleArray;
  lockList: PByteArray;
begin
  // Calculate the new ripple positions and update vertex coordinates
  coeff := cVelocityIntegrationCoeff * Resolution;
  f := cHeightFactor / Resolution;
  posList := @FPositions[0];
  velList := @FVelocity[0];
  lockList := @FLocks[0];
  for ij := 0 to Resolution * Resolution - 1 do
  begin
    if lockList[ij] = 0 then
    begin
      posList[ij] := posList[ij] - coeff * velList[ij];
      velList[ij] := velList[ij] * FViscosity;
      FPlaneQuadVertices.List[ij].Y := posList[ij] * f;
    end;
  end;
end;

procedure TGLWaterPlane.IterComputeNormals;
var
  i, j, ij: Integer;
  pv: PAffineVector;
  posList: PSingleArray;
  normList: PAffineVectorArray;
begin
  // Calculate the new vertex normals (not normalized, the hardware will handle that)
  posList := @FPositions[0];
  normList := FPlaneQuadNormals.List;
  for i := 1 to Resolution - 2 do
  begin
    ij := i * Resolution;
    for j := 1 to Resolution - 2 do
    begin
      Inc(ij);
      pv := @normList[ij];
      pv.X := posList[ij + 1] - posList[ij - 1];
      pv.z := posList[ij + Resolution] - posList[ij - Resolution];
    end;
  end;
end;

procedure TGLWaterPlane.Iterate;
var
  t: Int64;
begin
  if Visible then
  begin
    t := StartPrecisionTimer;

    IterComputeVelocity;
    IterComputePositions;
    IterComputeNormals;

    FLastIterationStepTime := StopPrecisionTimer(t);
  end;
end;

procedure TGLWaterPlane.BuildList(var rci: TGLRenderContextInfo);
var
  i: Integer;
  il: TIntegerList;
begin
  gl.PushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT);

  gl.EnableClientState(GL_VERTEX_ARRAY);
  gl.VertexPointer(3, GL_FLOAT, 0, FPlaneQuadVertices.List);
  gl.EnableClientState(GL_NORMAL_ARRAY);
  gl.NormalPointer(GL_FLOAT, 0, FPlaneQuadNormals.List);
  if wpoTextured in Options then
  begin
    gl.EnableClientState(GL_TEXTURE_COORD_ARRAY);
    gl.TexCoordPointer(2, GL_FLOAT, 0, FPlaneQuadTexCoords.List);
  end
  else
    gl.DisableClientState(GL_TEXTURE_COORD_ARRAY);

  if gl.EXT_compiled_vertex_array then
    gl.LockArrays(0, FPlaneQuadVertices.Count);

  for i := 0 to FPlaneQuadIndices.Count - 1 do
  begin
    il := TIntegerList(FPlaneQuadIndices[i]);
    gl.DrawElements(GL_QUAD_STRIP, il.Count, GL_UNSIGNED_INT, il.List);
  end;

  if gl.EXT_compiled_vertex_array then
    gl.UnLockArrays;

  gl.PopClientAttrib;
end;

procedure TGLWaterPlane.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLWaterPlane) then
  begin
    Active := TGLWaterPlane(Source).Active;
    RainTimeInterval := TGLWaterPlane(Source).RainTimeInterval;
    RainForce := TGLWaterPlane(Source).RainForce;
    Viscosity := TGLWaterPlane(Source).Viscosity;
  end;
  inherited Assign(Source);
end;

function TGLWaterPlane.AxisAlignedDimensionsUnscaled: TGLVector;
begin
  Result.X := 0.5 * Abs(Resolution);
  Result.Y := 0;
  Result.z := 0.5 * Abs(FResolution);
end;

procedure TGLWaterPlane.SetElastic(const value: Single);
begin
  FElastic := value;
end;

procedure TGLWaterPlane.SetResolution(const value: Integer);
begin
  if value <> FResolution then
  begin
    FResolution := value;
    if FResolution < 16 then
      FResolution := 16;
    InitResolution;
  end;
end;

procedure TGLWaterPlane.SetRainTimeInterval(Const val: Integer);
begin
  if (val >= 0) and (val <= 1000000) then
    FRainTimeInterval := val;
end;

Procedure TGLWaterPlane.SetViscosity(const val: Single);
begin
  if (val >= 0) and (val <= 1) then
    FViscosity := val;
end;

procedure TGLWaterPlane.SetRainForce(const val: Single);
begin
  if (val >= 0) and (val <= 1000000) then
    FRainForce := val;
end;

procedure TGLWaterPlane.SetSimulationFrequency(const val: Single);
begin
  if FSimulationFrequency <> val then
  begin
    FSimulationFrequency := val;
    if FSimulationFrequency < 1 then
      FSimulationFrequency := 1;
    FTimeToNextUpdate := 0;
  end;
end;

procedure TGLWaterPlane.SetMask(val: TPicture);
begin
  FMask.Assign(val);
end;

procedure TGLWaterPlane.DoMaskChanged(Sender: TObject);
begin
  Reset;
  StructureChanged;
end;

procedure TGLWaterPlane.SetOptions(const val: TGLWaterPlaneOptions);
begin
  if FOptions <> val then
  begin
    FOptions := val;
    StructureChanged;
  end;
end;

//-------------------------------------------------------------
initialization
//-------------------------------------------------------------

   RegisterClasses([TGLWaterPlane]);

end.
