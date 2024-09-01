//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Trail;

(*
  Creates a trail-like mesh.
  Based on Jason Lanford's demo.
*)

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,

  GXS.Scene,
  GXS.VectorTypes,
  GXS.MeshUtils,
  GXS.VectorGeometry,
  GXS.VectorFileObjects,
  GXS.Mesh,
  GXS.Objects,
  GXS.Material,
  GXS.Strings,
  GXS.BaseClasses;

const
  cMaxVerts = 2000;

type

  TMarkStyle = (msUp, msDirection, msFaceCamera, msRight);

  TgxTrail = class(TgxMesh)
  private
    fVertLimit: integer;
    fTimeLimit: single;
    fMinDistance: single;
    fAlpha: single;
    fAlphaFade: Boolean;
    fUVScale: single;
    fVerts: array [1 .. cMaxVerts] of TVector3f;
    fUVs: array [1 .. cMaxVerts] of TTexpoint;
    fTimeStamps: array [1 .. cMaxVerts] of Double;
    fVertStart, fVertEnd, fVertCount: integer;
    fLastV0Pos, fLastPos, fLastDir, fLastUp: TVector3f;
    FLastUVs: single;
    // used for UV scaling
    fLastP1, fLastP2: TVector3f;
    FTrailObject: TgxBaseSceneObject;
    FMarkStyle: TMarkStyle;
    FMarkWidth: single;
    FEnabled: Boolean;
    FAntiZFightOffset: single;
    procedure SetTrailObject(const Value: TgxBaseSceneObject);
    procedure SetMarkStyle(const Value: TMarkStyle);
    procedure SetAlpha(const Value: single);
    procedure SetAlphaFade(const Value: Boolean);
    procedure SetMinDistance(const Value: single);
    procedure SetTimeLimit(const Value: single);
    procedure SetUVScale(const Value: single);
    procedure SetVertLimit(const Value: integer);
    procedure SetMarkWidth(const Value: single);
    procedure SetEnabled(const Value: Boolean);
    function StoreAntiZFightOffset: Boolean;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    // EnableUVmapping: boolean; // generate UV's or not
    procedure DoProgress(const progressTime: TgxProgressTimes); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateMark(obj: TgxBaseSceneObject; width: single;
      CurrentTime: Double); overload;
    procedure CreateMark(APos, ADir, AUp: TVector3f; AWidth: single;
      ACurrentTime: Double); overload;
    function CreateMark(p1, p2: TVector3f; CurrentTime: Double)
      : Boolean; overload;
    procedure ClearMarks;
  published
    (* Add a tiny bit of offset to help prevent z-fighting..
      Need a better solution here as this will get out of whack on really
      long trails and is dependant on scene scale. *)
    property AntiZFightOffset: single read FAntiZFightOffset
      write FAntiZFightOffset stored StoreAntiZFightOffset;
    property VertLimit: integer read fVertLimit write SetVertLimit default 150;
    property TimeLimit: single read fTimeLimit write SetTimeLimit;
    { Don't create mark unless moved at least this distance. }
    property MinDistance: single read fMinDistance write SetMinDistance;
    property Alpha: single read fAlpha write SetAlpha;
    property AlphaFade: Boolean read fAlphaFade write SetAlphaFade default True;
    property UVScale: single read fUVScale write SetUVScale;
    property MarkStyle: TMarkStyle read FMarkStyle write SetMarkStyle
      default msFaceCamera;
    property TrailObject: TgxBaseSceneObject read FTrailObject
      write SetTrailObject default nil;
    property MarkWidth: single read FMarkWidth write SetMarkWidth;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  // -----------------------------------------------------------------------------
implementation

// -----------------------------------------------------------------------------

constructor TgxTrail.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  vertices.Clear; // inherited Tgxmesh makes a triangle... remove it.
  Mode := mmTriangleStrip;
  FAntiZFightOffset := 0.0000266;
  VertexMode := vmVNCT;
  fVertStart := 1;
  fVertEnd := 0;
  fVertCount := 0;
  fVertLimit := 150;
  fTimeLimit := 0.5;
  fMinDistance := 0.05;
  fAlphaFade := True;
  fAlpha := 1.0;
  FLastUVs := 0;
  fUVScale := 1.0;
  FMarkWidth := 0.5;
  FEnabled := True;
  FMarkStyle := msFaceCamera;
  Material.BlendingMode := bmAdditive;
  Material.FaceCulling := fcNoCull;
end;

destructor TgxTrail.Destroy;
begin
  // notta?
  inherited Destroy;
end;

procedure TgxTrail.DoProgress(const progressTime: TgxProgressTimes);
begin
  inherited;
  if Enabled and Assigned(TrailObject) then
  begin
    CreateMark(TrailObject, MarkWidth, progressTime.NewTime);
  end;
end;

procedure TgxTrail.ClearMarks;
begin
  vertices.Clear;
  fVertCount := 0;
  fVertEnd := 0;
  fVertStart := 1;
end;

procedure TgxTrail.CreateMark(obj: TgxBaseSceneObject; width: single;
  CurrentTime: Double);
var
  v0, dv, p1, p2: TVector3f;
  v: TVector3f;
  c: TgxCamera;
begin
  case MarkStyle of
    msUp:
      begin
        v := AffinevectorMake(obj.AbsoluteUp);
      end;
    msDirection:
      begin
        v := AffinevectorMake(obj.AbsoluteDirection);

      end;
    msRight:
      begin
        v := AffinevectorMake(obj.AbsoluteRight);
      end;
    msFaceCamera:
      begin
        c := Scene.CurrentCamera;
        if c <> nil then
        begin
          dv := VectorSubtract(fLastV0Pos,
            AffinevectorMake(obj.AbsolutePosition));
          v := VectorCrossProduct
            (AffinevectorMake(VectorSubtract(c.AbsolutePosition,
            obj.AbsolutePosition)), dv);
          NormalizeVector(v);

        end;
      end;
  else
    Assert(False, strErrorEx + strUnknownType);
  end;
  v0 := AffinevectorMake(obj.AbsolutePosition);
  VectorScale(v, width, v);
  p1 := VectorSubtract(v0, v);
  p2 := VectorAdd(v0, v);

  // PREVENT REFLAT
  if not PointIsInHalfSpace(p1, fLastV0Pos, VectorSubtract(v0, fLastV0Pos)) then
    p1 := fLastP1;
  if not PointIsInHalfSpace(p2, fLastV0Pos, VectorSubtract(v0, fLastV0Pos)) then
    p2 := fLastP2;

  if CreateMark(p1, p2, CurrentTime) then
  begin
    fLastV0Pos := v0;
  end;
end;

function TgxTrail.CreateMark(p1, p2: TVector3f; CurrentTime: Double): Boolean;
var
  diff: integer;
  uv1, uv2: TTexpoint;
  apoint1, apoint2: TVector3f;
  currentvert: integer;
  i: integer;
  color: tVector4f;
  ramp: single;
  distance: single;
  uvsize: single;
  tinyoffset: TVector3f;
  MustRebuild: Boolean;
begin
  Result := False;
  apoint1 := p1;
  apoint2 := p2;

  // get distance moved, based on average of 2 point movement;
  distance := (VectorDistance(fLastP1, p1) + VectorDistance(fLastP2, p2)) / 2;

  if distance = 0 then
  begin
    apoint1 := AffinevectorMake(fLastP1.X, fLastP1.Y, fLastP1.Z);
    apoint2 := AffinevectorMake(fLastP2.X, fLastP2.Y, fLastP2.Z);
  end;

  uvsize := distance / fUVScale; // scale UV's
  uv2.S := 0 + FLastUVs + uvsize;
  uv2.T := 0;
  uv1.S := 0 + FLastUVs + uvsize;
  uv1.T := 1;

  // process verts, then send them to .vertices for rendering
  if fVertEnd >= cMaxVerts then
    fVertEnd := 0;

  fVerts[fVertEnd + 1] := apoint2;
  fVerts[fVertEnd + 2] := apoint1;
  fUVs[fVertEnd + 1] := uv2;
  fUVs[fVertEnd + 2] := uv1;
  // tstamp := GetTickCount; // win api
  fTimeStamps[fVertEnd + 1] := CurrentTime;
  fTimeStamps[fVertEnd + 2] := CurrentTime;

  MustRebuild := False;
  if distance >= fMinDistance then
  begin
    inc(fVertCount, 2);
    inc(fVertEnd, 2);

    // remember stuff
    FLastUVs := FLastUVs + uvsize;
    fLastP1 := p1;
    fLastP2 := p2;
    MustRebuild := True;
    Result := True;
  end;

  // remove expired verts over VertLimit
  if fVertCount > fVertLimit then
  begin
    diff := fVertCount - fVertLimit;
    inc(fVertStart, diff);
    // inc start, reducing count to fit in limit - rollover handled later
    dec(fVertCount, diff);
  end;

  // remove time expired verts over TimeLimit
  // currentvert := fVertStart;
  for i := 0 to fVertCount - 1 do
  begin
    if (i + fVertStart) > cMaxVerts then
      currentvert := (i + fVertStart) - cMaxVerts // rollover
    else
      currentvert := (i + fVertStart);

    if fTimeLimit > 0 then
      if CurrentTime - fTimeStamps[currentvert] > fTimeLimit then
      begin
        inc(fVertStart, 1);
        // inc start, reducing count to fit in limit - rollover handled later
        dec(fVertCount, 1);
        MustRebuild := True;
      end;
  end;

  // handle rollover
  if fVertStart > cMaxVerts then
    fVertStart := 0 + (fVertStart - cMaxVerts); // adjust if rollover

  if MustRebuild then
  begin
    // give to .vertices, from start to count
    // currentvert := fVertStart;
    ramp := fAlpha / (fVertCount);
    color := Material.FrontProperties.Diffuse.color;
    vertices.Clear;
    for i := 0 to fVertCount - 1 do
    begin
      if (i + fVertStart) > cMaxVerts then
        currentvert := (i + fVertStart) - cMaxVerts // rollover
      else
        currentvert := (i + fVertStart);

      if fAlphaFade then
        color.W := (ramp * i)
      else
        color.W := fAlpha;
      // add a tiny bit of offset to help prevent z-fighting..
      // need a better solution here
      // as this will get out of whack on really long trails
      // and is dependant on scene scale
      tinyoffset.X := FAntiZFightOffset * i;
      tinyoffset.Y := FAntiZFightOffset * i;
      tinyoffset.Z := FAntiZFightOffset * i;
      tinyoffset := VectorAdd(fVerts[currentvert], tinyoffset);
      // TinyOffset := fVerts[ currentvert]; // bypass
      vertices.AddVertex(tinyoffset, NullVector, color, fUVs[currentvert]);
    end;
  end;

end;

procedure TgxTrail.CreateMark(APos, ADir, AUp: TVector3f; AWidth: single;
  ACurrentTime: Double);
var
  apoint1, apoint2, crossp: TVector3f;
begin
  if fMinDistance > 0 then
    if VectorDistance(APos, fLastPos) < fMinDistance then
      exit;
  fLastPos := APos;
  fLastDir := ADir;
  fLastUp := AUp;
  apoint1 := APos;
  apoint2 := APos;
  crossp := VectorCrossProduct(ADir, AUp);
  CombineVector(apoint1, vectornormalize(crossp), AWidth);
  CombineVector(apoint2, vectornormalize(VectorNegate(crossp)), AWidth);
  CreateMark(apoint1, apoint2, ACurrentTime);
end;

// NOTES and stuff:

{ // UV mapped 4x4 square for refrence /debug
  uv.S := 0; uv.T := 0;
  Vertices.AddVertex( AffineVectorMake(1, 1, 1), NullVector, NullHmgVector, UV  );
  uv.S := 0; uv.T := 1;
  Vertices.AddVertex( AffineVectorMake(1, 1, 4), NullVector, NullHmgVector, UV  );
  uv.S := 1; uv.T := 0;
  Vertices.AddVertex( AffineVectorMake(4, 1, 1), NullVector, NullHmgVector, UV  );
  uv.S := 1; uv.T := 1;
  Vertices.AddVertex( AffineVectorMake(4, 1, 4), NullVector, NullHmgVector, UV  );


  // Directmode: append .vertices only, no way to process/delete except .clear;
  // else we manage vertices/UV in our own arrays then dump them all to .vertices
  // I don't know if directmode is that much faster, but could be considerably?

  if directmode then
  begin
  if fUVTop then // start a new UV tile
  begin
  uv2.S := 0; uv2.T := 0;
  Vertices.AddVertex( AffineVectorMake(apoint2[0], apoint2[1],apoint2[2]), NullVector, NullHmgVector, UV2  );
  uv1.S := 0; uv1.T := 1;
  Vertices.AddVertex( AffineVectorMake(apoint1[0],apoint1[1],apoint1[2]), NullVector, NullHmgVector, UV1  );
  end
  else // finish a UV tile
  begin
  uv2.S := 1; uv2.T := 0;
  Vertices.AddVertex( AffineVectorMake(apoint2[0], apoint2[1],apoint2[2]), NullVector, NullHmgVector, UV2  );
  uv1.S := 1; uv1.T := 1;
  Vertices.AddVertex( AffineVectorMake(apoint1[0],apoint1[1],apoint1[2]), NullVector, NullHmgVector, UV1  );
  end;
  end
}

procedure TgxTrail.SetTrailObject(const Value: TgxBaseSceneObject);
begin
  if FTrailObject <> nil then
    FTrailObject.RemoveFreeNotification(Self);
  FTrailObject := Value;
  if FTrailObject <> nil then
    FTrailObject.FreeNotification(Self);
end;

procedure TgxTrail.SetMarkStyle(const Value: TMarkStyle);
begin
  FMarkStyle := Value;
end;

procedure TgxTrail.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FTrailObject) then
    TrailObject := nil;
  inherited;
end;

procedure TgxTrail.SetAlpha(const Value: single);
begin
  fAlpha := Value;
end;

procedure TgxTrail.SetAlphaFade(const Value: Boolean);
begin
  fAlphaFade := Value;
end;

procedure TgxTrail.SetMinDistance(const Value: single);
begin
  fMinDistance := Value;
end;

procedure TgxTrail.SetTimeLimit(const Value: single);
begin
  fTimeLimit := Value;
end;

procedure TgxTrail.SetUVScale(const Value: single);
begin
  fUVScale := Value;
end;

procedure TgxTrail.SetVertLimit(const Value: integer);
begin
  fVertLimit := Value;
end;

procedure TgxTrail.SetMarkWidth(const Value: single);
begin
  FMarkWidth := Value;
end;

procedure TgxTrail.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

function TgxTrail.StoreAntiZFightOffset: Boolean;
begin
  Result := FAntiZFightOffset <> 0.0000266;
end;

// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------

RegisterClasses([TgxTrail]);

end.
