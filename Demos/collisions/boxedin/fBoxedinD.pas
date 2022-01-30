unit fBoxedinD;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,

  GLS.Keyboard,
  GLS.VectorGeometry,
  GLS.Scene,
  GLS.VectorFileObjects,
  GLS.VectorTypes,
  GLS.Objects,
  GLS.SceneViewer,
  GLS.Cadencer,
  GLS.Navigator,
  GLS.GeomObjects,

  GLS.Coordinates,
  GLS.Utils,
  GLS.BaseClasses,
  GLS.File3DS;

type
  TFormBoxedin = class(TForm)
    GLScene1: TGLScene;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    FreeForm1: TGLFreeForm;
    Sphere1: TGLSphere;
    ArrowLine1: TGLArrowLine;
    GLSceneViewer2: TGLSceneViewer;
    GLCamera2: TGLCamera;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    DummyCube2: TGLDummyCube;
    Sphere2: TGLSphere;
    GLLightSource2: TGLLightSource;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TrackBar1: TTrackBar;
    Button1: TButton;
    Lines1: TGLLines;
    LabelFPS: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private

    colTotalTime: Single; // for timing collision detection
    colCount: Integer;
    procedure AddToTrail(const p: TGLVector);
  public

    mousex, mousey: Integer;
  end;

var
  FormBoxedin: TFormBoxedin;

implementation

{$R *.dfm}

procedure TFormBoxedin.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  FreeForm1.LoadFromFile('BoxedIn.3ds');

  FreeForm1.BuildOctree;
  Label1.Caption := 'Octree Nodes    : ' + inttostr(FreeForm1.Octree.NodeCount);
  Label2.Caption := 'Tri Count Octree: ' +
    inttostr(FreeForm1.Octree.TriCountOctree);
  Label3.Caption := 'Tri Count Mesh  : ' +
    inttostr(FreeForm1.Octree.TriCountMesh);

  Lines1.AddNode(0, 0, 0);
  Lines1.ObjectStyle := Lines1.ObjectStyle + [osDirectDraw];
end;

procedure TFormBoxedin.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  rayStart, rayVector: TGLVector;
  velocity: Single;
  pPoint: TGLVector;
  pNormal: TGLVector;
  t: Int64;
begin
  if IsKeyDown(VK_ESCAPE) then
    close;

  velocity := TrackBar1.Position * deltaTime * 50;

  t := StartPrecisionTimer;

  with FreeForm1 do
  begin
    SetVector(rayStart, Sphere2.AbsolutePosition);
    SetVector(rayVector, Sphere2.AbsoluteDirection);
    NormalizeVector(rayVector);
    // Note: since collision may be performed on multiple meshes, we might need to know which hit
    // is closest (ie: d:=raystart - pPoint).
    if OctreeSphereSweepIntersect(rayStart, rayVector, velocity, Sphere2.Radius,
      @pPoint, @pNormal) then
    begin
      // Show the polygon intersection point
      NormalizeVector(pNormal);
      Sphere1.Position.AsVector := pPoint;
      Sphere1.Direction.AsVector := pNormal;

      // Make it rebound...
      with Sphere2.Direction do
        AsAffineVector := VectorReflect(AsAffineVector,
          AffineVectorMake(pNormal));
      // Add some "english"...
      with Sphere2.Direction do
      begin
        X := X + random / 10;
        Y := Y + random / 10;
        Z := Z + random / 10;
      end;
      // Add intersect point to trail
      AddToTrail(pPoint);
    end
    else
    begin
      Sphere2.Move(velocity); // No collision, so just move the ball.
    end;
  end;
  // Last trail point is always the sphere's current position
  Lines1.Nodes.Last.AsVector := Sphere2.Position.AsVector;

  colTotalTime := colTotalTime + StopPrecisionTimer(t);
  Inc(colCount);
end;

procedure TFormBoxedin.AddToTrail(const p: TGLVector);
var
  i, k: Integer;
begin
  Lines1.Nodes.Last.AsVector := p;
  Lines1.AddNode(0, 0, 0);
  if Lines1.Nodes.Count > 5 then // limit trail to 20 points
    Lines1.Nodes[0].Free;

  for i := 0 to 4 do // count to 19
  begin
    k := Lines1.Nodes.Count - i - 1;
    if k >= 0 then
      TGLLinesNode(Lines1.Nodes[k]).Color.Alpha := 0.95 - i * 0.05;
  end;
end;

procedure TFormBoxedin.Timer1Timer(Sender: TObject);
var
  t: Single;
begin
  if colCount > 0 then
    t := colTotalTime * 1000 / colCount
  else
    t := 0;
  LabelFPS.Caption := Format('%.2f FPS - %.3f ms for collisions/frame',
    [GLSceneViewer2.FramesPerSecond, t]);
  GLSceneViewer2.ResetPerformanceMonitor;
  colTotalTime := 0;
  colCount := 0;
end;

procedure TFormBoxedin.Button1Click(Sender: TObject);
begin
  // If the ball gets stuck in a pattern, hit the reset button.
  with Sphere2.Position do
  begin
    X := random;
    Y := random;
    Z := random;
  end;

  with Sphere2.Direction do
  begin
    X := random;
    if random > 0.5 then
      X := -X;
    Y := random;
    if random > 0.5 then
      Y := -Y;
    Z := random;
    if random > 0.5 then
      Z := -Z;
  end;
end;

end.
