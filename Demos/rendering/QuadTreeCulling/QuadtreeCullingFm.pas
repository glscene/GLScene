unit QuadtreeCullingFm;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Imaging.Jpeg,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,

  GLS.Scene,
  GLS.VectorTypes,
  GLS.PersistentClasses,
  GLS.SceneViewer,
  GLS.SkyDome,
  GLS.Objects,
  GLS.Keyboard,
  GLS.HeightData,
  GLS.TerrainRenderer,
  GLS.Texture,
  GLS.Cadencer,
  GLS.Navigator,
  GLS.SpacePartition,
  GLS.VectorGeometry,
  GLS.BitmapFont,
  GLS.GeometryBB,
  GLS.WindowsFont,
  GLS.HUDObjects,
 
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.RenderContextInfo,
  GLS.Utils;

type
  TfrmQuadtreeVisCulling = class(TForm)
    GLScene1: TGLScene;
    trees: TGLDummyCube;
    GLSkyDome1: TGLSkyDome;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLTerrainRenderer1: TGLTerrainRenderer;
    GLBitmapHDS1: TGLBitmapHDS;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLNavigator1: TGLNavigator;
    GLUserInterface1: TGLUserInterface;
    queryVisible: TGLDirectOpenGL;
    Timer1: TTimer;
    GLHUDText1: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    GLDirectOpenGL1: TGLDirectOpenGL;
    Panel1: TPanel;
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    GLDirectOpenGL2: TGLDirectOpenGL;
    tree: TGLSprite;
    GLSphere1: TGLSphere;
    Panel2: TPanel;
    cbUseQuadtree: TCheckBox;
    cbUseExtendedFrustum: TCheckBox;
    cbShowQuadtree: TCheckBox;
    Label2: TLabel;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure queryVisibleRender(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure cbShowQuadtreeClick(Sender: TObject);
    procedure GLDirectOpenGL2Render(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure Button1Click(Sender: TObject);
  private
     
    cullingMode: string;
    visiblecount,treecount: integer;
    SpacePartition: TSectoredSpacePartition;
    FCamHeight : single;
    procedure CreateTrees;
  public

  end;

var
  frmQuadtreeVisCulling: TfrmQuadtreeVisCulling;

implementation

{$R *.dfm}

procedure TfrmQuadtreeVisCulling.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
  var
   speed : Single;
begin
  GLUserInterface1.MouseLook;
  GLUserInterface1.MouseUpdate;
   if IsKeyDown(VK_SHIFT) then
      speed:=6000*deltaTime
   else speed:=1000*deltaTime;
   with GLCamera1.Position do begin
      if IsKeyDown(87) then
         GLNavigator1.MoveForward(speed);
      if IsKeyDown(83) then
         GLNavigator1.MoveForward(-speed);
      if IsKeyDown(65) then
         GLNavigator1.StrafeHorizontal(-speed);
      if IsKeyDown(68) then
         GLNavigator1.StrafeHorizontal(speed);
      if IsKeyDown('e') then
         FCamHeight := FCamHeight + 5;
      if IsKeyDown('c') then
         FCamHeight := FCamHeight - 5;
      if IsKeyDown(VK_ESCAPE) then Close;
   end;
   with GLCamera1.Position do
      Y:=glTerrainRenderer1.InterpolatedHeight(AsVector)+80+FCamHeight;
   GLHUDText1.Text := CullingMode+ 'visible tree count: '+inttostr(visiblecount)+
     ' / Total:'+inttostr(treecount)+
     #13#10+ ' Press ''W A S D'' to navigate, ''E'' - up, ''C'' - down'+
     #13#10+ ' Press ''Q'' to Show Quadtree, ''X'' - Advanced frustum'+
     #13#10+ ' Press ''V'' to Change quadtree query visible or visiblity culling'+
     #13#10+ ' Press ''Esc'' to quit';
end;

procedure TfrmQuadtreeVisCulling.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir;
  SpacePartition := TQuadtreeSpacePartition.Create;
  SpacePartition.LeafThreshold := 50;
  SpacePartition.MaxTreeDepth := 10;//}
  SpacePartition.GrowGravy := 0.01;

  tree.visible := false;
  trees.ObjectsSorting := osRenderFarthestFirst;

  GLBitmapHDS1.Picture.LoadFromFile('terrain.bmp');
  GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile('snow512.jpg');
  GLMaterialLibrary1.Materials[1].Material.Texture.Image.LoadFromFile('detailmap.jpg');
  tree.Material.Texture.Image.LoadFromFile('tree1.bmp');
  Show;
  CreateTrees;
  cullingMode := 'Quadtree ';
  GLUserInterface1.MouseLookActivate;
end;

procedure TfrmQuadtreeVisCulling.CreateTrees;
const
  cRange = 40;//40
var
  i,j: integer;
  obj: TGLProxyObject;
begin
  glscene1.BeginUpdate;

  ProgressBar1.Max := (cRange*2)*(cRange*2);
  Label1.Refresh;

  for i := -cRange to cRange do
    for j := -cRange to cRange do
    begin
      inc(treecount);
      ProgressBar1.Position := TreeCount;
      obj := TGLProxyObject(trees.AddNewChild(TGLProxyObject));
      obj.MasterObject := tree;
      obj.Position.AsAffineVector := AffineVectorMake(i*500+random(200),0,j*500+random(200));
      with obj.Position do
        y := GLTerrainRenderer1.InterpolatedHeight(obj.AbsolutePosition)+150;
      TSceneObj.CreateObj(SpacePartition,obj);

      Label2.Caption := Format('Leaves = %d, Nodes = %d, NodesInRoot = %d  ',[
        SpacePartition.Leaves.Count,
        SpacePartition.GetNodeCount,
        SpacePartition.RootNode.Leaves.Count]);

      Label2.Refresh;
    end;

  Panel1.Free;
  glscene1.EndUpdate;
end;

procedure TfrmQuadtreeVisCulling.FormDestroy(Sender: TObject);
begin
  SpacePartition.Free;
end;

procedure TfrmQuadtreeVisCulling.queryVisibleRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
  function PlaneToStr(const APlane : THmgPlane) : string;
  begin
    result := Format('(%2.1f, %2.1f, %2.1f, %2.1f)',[
      APlane.X,
      APlane.Y,
      APlane.Z,
      APlane.W]);
  end;
var
  i: integer;

begin
  if not cbUseQuadtree.Checked then exit;

  glscene1.BeginUpdate;
  for i := 0 to trees.Count - 1 do
    trees.Children[i].Visible := false;

  // Query the Quadtree for objects that intersect the frustum
  if cbUseExtendedFrustum.Checked then
    SpacePartition.QueryFrustumEx(
      ExtendedFrustumMakeFromSceneViewer(rci.rcci.frustum, GLSceneViewer1))
  else
    SpacePartition.QueryFrustum(rci.rcci.frustum);

  visiblecount := SpacePartition.QueryResult.Count;

  Label2.Caption := Format('NodeTests = %d (of %d), ObjTests = %d (of %d), Visible = %d',[
    SpacePartition.QueryNodeTests,
    SpacePartition.GetNodeCount,
    SpacePartition.QueryInterObjectTests,
    SpacePartition.Leaves.Count,
    SpacePartition.QueryResult.Count]);//}

  {if rci.rcci.frustum.pNear[3]>=0 then
    Label3.Caption := 'OK'
  else
    Label3.Caption := 'BAD';//}

  {Label3.Caption :=
    Format('%s, %s, %s, %s, %s, %s',[
      PlaneToStr(rci.rcci.frustum.pNear),
      PlaneToStr(rci.rcci.frustum.pFar),
      PlaneToStr(rci.rcci.frustum.pTop),
      PlaneToStr(rci.rcci.frustum.pBottom),
      PlaneToStr(rci.rcci.frustum.pLeft),
      PlaneToStr(rci.rcci.frustum.pRight)]);//}

  for i := 0 to SpacePartition.QueryResult.Count - 1 do begin
    TSceneObj(SpacePartition.QueryResult[i]).Obj.Visible := true;
    if cbShowQuadtree.Checked then
      RenderAABB(rci, TSceneObj(SpacePartition.QueryResult[i]).FCachedAABB);
  end;
  glscene1.EndUpdate;
end;

procedure TfrmQuadtreeVisCulling.Timer1Timer(Sender: TObject);
begin
  Caption := 'Quardtree Visibility Culling - '+GLSceneViewer1.FramesPerSecondText;
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TfrmQuadtreeVisCulling.FormKeyPress(Sender: TObject; var Key: Char);
var
  i: integer;
begin
  if Key = 'v' then
  begin
    cbUseQuadtree.Checked := not cbUseQuadtree.Checked;

    if cbUseQuadtree.Checked then
    begin
      cullingMode := ' Quadtree ';
      for i := 0 to trees.Count - 1 do
        trees.Children[i].Visible := true;
      trees.VisibilityCulling := vcNone;
    end else
    begin
      cullingMode := 'visibility culling ';
      for i := 0 to trees.Count - 1 do
        trees.Children[i].Visible := true;
      trees.VisibilityCulling := vcObjectBased;
    end;
  end;
end;

procedure TfrmQuadtreeVisCulling.cbShowQuadtreeClick(Sender: TObject);
begin
  GLDirectOpenGL2.Visible := cbShowQuadtree.Checked;
end;

procedure TfrmQuadtreeVisCulling.GLDirectOpenGL2Render(
  Sender: TObject; var rci: TGLRenderContextInfo);
{var
  ExtendendFrustum : TExtendedFrustum;//}
begin
  RenderSpatialPartitioning(rci, SpacePartition);

  {ExtendendFrustum := ExtendedFrustumMake(rci.rcci.frustum,
    GLCamera1.NearPlane,
    GLCamera1.DepthOfView,
    GLSceneViewer1.FieldOfView,
    GLCamera1.Position.AsAffineVector,
    GLCamera1.Direction.AsAffineVector);//}

  {ExtendendFrustum := ExtendedFrustumMakeFromSceneViewer(
    rci.rcci.frustum, GLSceneViewer1);

  GLSphere1.Position.AsAffineVector :=
    VectorCombine(ExtendendFrustum.SPCone.Base, ExtendendFrustum.SPCone.Axis, 1, GLCamera1.DepthOfView * 0.05);

  GLSphere1.Radius := sin(ExtendendFrustum.SPCone.Angle) * GLCamera1.DepthOfView  * 0.05;

  {GLSphere1.Position.AsAffineVector := ExtendendFrustum.BSphere.Center;
  GLSphere1.Radius := ExtendendFrustum.BSphere.Radius / 1.42;//}
end;

procedure TfrmQuadtreeVisCulling.Button1Click(Sender: TObject);
begin
  GLSphere1.Position.AsVector :=
    VectorCombine(GLCamera1.Position.AsVector, GLCamera1.Direction.AsVector, 1, GLCamera1.NearPlane)
end;

end.
