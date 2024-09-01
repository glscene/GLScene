//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.SpatialPartitioning;

(* Spatial partitioning related code that also uses scene objects *)

interface

uses
  Winapi.OpenGL,
  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.GeometryBB,
  GXS.SpacePartition,

  GXS.Scene,
  GXS.Coordinates,
  GXS.SceneViewer,

  GXS.RenderContextInfo,
  GXS.State;

type
  // Object for holding scene objects in a spatial partitioning
  TgxSceneObj = class(TSpacePartitionLeaf)
  public
    Obj: TgxBaseSceneObject;
    procedure UpdateCachedAABBAndBSphere; override;
    constructor CreateObj(Owner: TSectoredSpacePartition; aObj: TgxBaseSceneObject);
    destructor Destroy; override;
  end;

(* Render a spacial partitioning descending from TSectoredSpacePartition
   (octree and quadtree) as a grid - great for debugging and visualisation *)
procedure RenderSpatialPartitioning(var rci: TgxRenderContextInfo; const Space: TSectoredSpacePartition);

(* Create an extended frustum from a SceneViewer - this makes the unit
  specific to the windows platform! *)
function ExtendedFrustumMakeFromSceneViewer(const AFrustum: TFrustum; const vWidth, vHeight: integer; AVKCamera: TgxCamera)
  : TExtendedFrustum; overload;

function ExtendedFrustumMakeFromSceneViewer(const AFrustum: TFrustum; const AGLXceneViewer: TgxSceneViewer)
  : TExtendedFrustum; overload;

// Renders an AABB as a line
procedure RenderAABB(var rci: TgxRenderContextInfo; AABB: TAABB; w, r, g, b: single); overload;
procedure RenderAABB(var rci: TgxRenderContextInfo; AABB: TAABB); overload;

// -------------------------------------------------------------------
implementation
// -------------------------------------------------------------------

uses
  GXS.Context;

procedure RenderAABB(var rci: TgxRenderContextInfo; AABB: TAABB);
begin
  RenderAABB(rci, AABB, 1, 0.8, 0.8, 0.8);
end;

procedure RenderAABB(var rci: TgxRenderContextInfo; AABB: TAABB; w, r, g, b: single);
begin
  glColor3f(r, g, b);
  rci.gxStates.LineWidth := w;

  glBegin(GL_LINE_STRIP);
  glVertex3f(AABB.min.X, AABB.min.Y, AABB.min.Z);
  glVertex3f(AABB.min.X, AABB.max.Y, AABB.min.Z);
  glVertex3f(AABB.max.X, AABB.max.Y, AABB.min.Z);
  glVertex3f(AABB.max.X, AABB.min.Y, AABB.min.Z);
  glVertex3f(AABB.min.X, AABB.min.Y, AABB.min.Z);

  glVertex3f(AABB.min.X, AABB.min.Y, AABB.max.Z);
  glVertex3f(AABB.min.X, AABB.max.Y, AABB.max.Z);
  glVertex3f(AABB.max.X, AABB.max.Y, AABB.max.Z);
  glVertex3f(AABB.max.X, AABB.min.Y, AABB.max.Z);
  glVertex3f(AABB.min.X, AABB.min.Y, AABB.max.Z);
  glEnd;

  glBegin(GL_LINES);
  glVertex3f(AABB.min.X, AABB.max.Y, AABB.min.Z);
  glVertex3f(AABB.min.X, AABB.max.Y, AABB.max.Z);

  glVertex3f(AABB.max.X, AABB.max.Y, AABB.min.Z);
  glVertex3f(AABB.max.X, AABB.max.Y, AABB.max.Z);

  glVertex3f(AABB.max.X, AABB.min.Y, AABB.min.Z);
  glVertex3f(AABB.max.X, AABB.min.Y, AABB.max.Z);
  glEnd;
end;

procedure RenderSpatialPartitioning(var rci: TgxRenderContextInfo; const Space: TSectoredSpacePartition);

  procedure RenderSectorNode(Node: TSectorNode);
  var
    i: integer;
    AABB: TAABB;
  begin
    if Node.NoChildren then
    begin
      AABB := Node.AABB;
      if Node.RecursiveLeafCount > 0 then
        RenderAABB(rci, AABB, 1, 0, 0, 0)
      else
        RenderAABB(rci, AABB, 1, 0.8, 0.8, 0.8) // }
    end
    else
    begin
      for i := 0 to Node.ChildCount - 1 do
        RenderSectorNode(Node.Children[i]);
    end;
  end;

begin
  rci.gxStates.Disable(stLighting);
  RenderSectorNode(Space.RootNode);
end;

function ExtendedFrustumMakeFromSceneViewer(const AFrustum: TFrustum; const AGLXceneViewer: TgxSceneViewer): TExtendedFrustum;
// old version
begin
  Assert(Assigned(AGLXceneViewer.Camera), 'GLXceneViewer must have camera specified!');
  result := ExtendedFrustumMake(AFrustum, AGLXceneViewer.Camera.NearPlane, AGLXceneViewer.Camera.DepthOfView,
    AGLXceneViewer.FieldOfView, AGLXceneViewer.Camera.Position.AsAffineVector, AGLXceneViewer.Camera.Direction.AsAffineVector);
end;

function ExtendedFrustumMakeFromSceneViewer(const AFrustum: TFrustum; const vWidth, vHeight: integer; AVKCamera: TgxCamera)
  : TExtendedFrustum; // changed version
var
  buffov: single;
begin
  if vWidth < vHeight then
    buffov := AVKCamera.GetFieldOfView(vWidth)
  else
    buffov := AVKCamera.GetFieldOfView(vHeight);
  result := ExtendedFrustumMake(AFrustum, AVKCamera.NearPlane, AVKCamera.DepthOfView, buffov, AVKCamera.Position.AsAffineVector,
    AVKCamera.Direction.AsAffineVector);
end;

// --------- TgxSceneObj ------------
constructor TgxSceneObj.CreateObj(Owner: TSectoredSpacePartition; aObj: TgxBaseSceneObject);
begin
  Obj := aObj;
  inherited CreateOwned(Owner);
end;

destructor TgxSceneObj.Destroy;
begin
  inherited;
end;

procedure TgxSceneObj.UpdateCachedAABBAndBSphere;
begin
  FCachedAABB := Obj.AxisAlignedBoundingBox;
  FCachedAABB.min := Obj.LocalToAbsolute(FCachedAABB.min);
  FCachedAABB.max := Obj.LocalToAbsolute(FCachedAABB.max);
  FCachedBSphere.Radius := Obj.BoundingSphereRadius;
  FCachedBSphere.Center := AffineVectorMake(Obj.AbsolutePosition);
end;

end.
