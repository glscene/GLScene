unit uGBEUtils3D;

interface

uses
  System.Math.Vectors,
  System.Types,
  System.Classes,
  FMX.Objects3D,
  System.Math,
  FMX.Controls3D,
  FMX.Graphics,
  FMX.Types3D,
  System.UITypes,
  FMX.Effects,
  System.UIConsts,
  System.SysUtils,
  System.RTLConsts,
  FMX.Types,
  FMX.Ani,
  FMX.Viewport3D;

type
  TCustomMeshHelper = class(TCustomMesh);

  TGBECollisionRetour = record
    bool: boolean;
    objet: TControl3D;
  end;

function Barycentre(p1, p2, p3: TPoint3D; p4: TPointF): single;
function CalculateHeight(Mesh: TMesh; P: TPoint3D; miseAEchelle: single;
  subDivX, subDivZ: integer): single;
function SizeOf3D(const unObjet3D: TControl3D): TPoint3D;
function DetectionCollisionObstacle(Mesh: TMesh; objet: TControl3D)
  : TGBECollisionRetour;
procedure InteractionIHM(viewport: TViewport3D);
function CollisionDummyChilds(aDummy: TDummy; objet3D: TControl3D)
  : TGBECollisionRetour;
function CollisionEntre2Objets(objet1, objet2: TControl3D): TGBECollisionRetour;

implementation // -------------------------------------------------------------

function Barycentre(p1, p2, p3: TPoint3D; p4: TPointF): single;
var
  det, l1, l2, l3, d1, d2, d3, t1, t2: single;
begin
  d1 := (p2.z - p3.z);
  // Small optimizations to only do intermediate calculations once in each iteration
  d2 := (p3.x - p2.x);
  d3 := (p1.x - p3.x);
  det := 1 / ((d1 * d3) + (d2 * (p1.z - p3.z)));
  // Inverse, allows to replace greedy divisions by a multiplication
  // (thus, we only do the division once instead of twice at each iteration)
  t1 := (p4.x - p3.x);
  t2 := (p4.y - p3.z);
  l1 := ((d1 * t1) + (d2 * t2)) * det;
  l2 := ((p3.z - p1.z) * (t1 + (d3 * t2))) * det;
  l3 := 1 - l1 - l2;
  Result := l1 * p1.y + l2 * p2.y + l3 * p3.y;
end;

// ------------------------------------------------------------------------------------------
function CalculateHeight(Mesh: TMesh; P: TPoint3D; miseAEchelle: single;
  subDivX, subDivZ: integer): single;
var
  grilleX, grilleZ, indiceMaille: integer;
  xCoord, zCoord, hauteurCalculee, demiDepth, demiWidth, subWidth,
    subDepth: single;
begin
  if (subDivX = 0) or (subDivZ = 0) then
  begin
    Result := 0;
    Exit;
  end;

  demiWidth := Mesh.width * 0.5;
  demiDepth := Mesh.Depth * 0.5;
  subWidth := Mesh.width / subDivX;
  subDepth := Mesh.Depth / subDivZ;

  // Determination of the indices allowing access to the mesh according to the position of the player
  grilleX := trunc((P.x + demiWidth) / subWidth);
  grilleZ := trunc((P.z + demiDepth) / subDepth);

  // If we are outside the mesh, we force (arbitrarily) the height
  if (grilleX >= subDivX) or (grilleZ >= subDivZ) or (grilleX < 0) or
    (grilleZ < 0) then
    Result := 0
  else
  begin
    xCoord := Frac((P.x + demiWidth) / subWidth);
    // X position in the current mesh
    zCoord := Frac((P.z + demiDepth) / subDepth);
    // Y position in the current mesh

    // The height is calculated based on the 3 vertices of the triangle in which the player is located.
    // We determine which triangle we are in
    indiceMaille := (grilleZ * subDivZ * 4) + grilleX * 4;
    if xCoord <= (1 - zCoord) then
    begin
      hauteurCalculee :=
        Barycentre(TPoint3D.Create(0, Mesh.data.VertexBuffer.Vertices
        [indiceMaille].y, 0), TPoint3D.Create(1, Mesh.data.VertexBuffer.Vertices
        [indiceMaille + 1].y, 0), TPoint3D.Create(0,
        Mesh.data.VertexBuffer.Vertices[indiceMaille + 3].y, 1),
        TPointF.Create(xCoord, zCoord));
    end
    else
    begin
      hauteurCalculee :=
        Barycentre(TPoint3D.Create(1, Mesh.data.VertexBuffer.Vertices
        [indiceMaille + 1].y, 0), TPoint3D.Create(1,
        Mesh.data.VertexBuffer.Vertices[indiceMaille + 2].y, 1),
        TPoint3D.Create(0, Mesh.data.VertexBuffer.Vertices[indiceMaille + 3].y,
        1), TPointF.Create(xCoord, zCoord));
    end;
    Result := hauteurCalculee * miseAEchelle; // - demiHeight ; //- mesh.Height;
  end;
end;

// Returns the dimensions of the 3D object
function SizeOf3D(const unObjet3D: TControl3D): TPoint3D;
begin
  Result := NullPoint3D;
  if unObjet3D <> nil then
    Result := Point3D(unObjet3D.width, unObjet3D.Height, unObjet3D.Depth);
end;

// ------------------------------------------------------------------------------------------
// "Bounding Box" collision detection between the mesh
// (TGBEHeightmap and its child objects which have their tag at 1) and an object
function DetectionCollisionObstacle(Mesh: TMesh; objet: TControl3D)
  : TGBECollisionRetour;
var
  unObjet3D: TControl3D; // the object being rendered
  i, j: integer;
  resultat: TGBECollisionRetour;
begin
  resultat.bool := false;
  resultat.objet := nil;
  // Collision test with direct children of mSol
  for i := 0 to Mesh.ChildrenCount - 1 do
  begin
    if Mesh.Children[i].Tag = 1 then
    // The TMesh child must have his tag at 1
    begin
      for j := 0 to Mesh.Children[i].ChildrenCount - 1 do
      begin
        // We are working on the object that is being calculated
        unObjet3D := TControl3D(Mesh.Children[i].Children[j]);

        if collisionEntre2Objets(unObjet3D, objet).bool then
        begin
          resultat.bool := true;
          resultat.objet := unObjet3D;
          Break;
        end;
      end;
    end;
  end;
  Result := resultat;
end;

// ------------------------------------------------------------------------------------------

procedure InteractionIHM(viewport: TViewport3D);
var
  obj: TFmxObject;
begin
  for obj in Viewport.Children do
  begin
    if obj is TAnimation then
      TAnimation(obj).ProcessTick(0, 0);
  end;
end;

// ------------------------------------------------------------------------------------------
function CollisionDummyChilds(aDummy: TDummy; objet3D: TControl3D)
  : TGBECollisionRetour;
var
  obj: TFmxObject;
  resultat: TGBECollisionRetour;
begin
  resultat.bool := false;
  resultat.objet := nil;
  for obj in aDummy.Children do
  begin
    if (obj as TControl3D).visible then
    begin
      resultat := collisionEntre2Objets(objet3D, (obj as TControl3D));
      if resultat.bool then
        Break;
    end;
  end;
  Result := resultat;
end;

function CollisionEntre2Objets(objet1, objet2: TControl3D): TGBECollisionRetour;
var
  DistanceEntreObjets, distanceMinimum: TPoint3D;
begin
  result.objet := nil;
  result.bool := false;

  DistanceEntreObjets := objet1.Position.Point - objet2.Position.Point;
  distanceMinimum := (SizeOf3D(objet1) + SizeOf3D(objet2)) * 0.5;

  if ((Abs(DistanceEntreObjets.x) < distanceMinimum.x) and
    (Abs(DistanceEntreObjets.y) < distanceMinimum.y) and
    (Abs(DistanceEntreObjets.z) < distanceMinimum.z)) then
  begin
    result.bool := True;
    result.objet := objet2;
  end;
end;

end.
