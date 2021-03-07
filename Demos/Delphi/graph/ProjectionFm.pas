unit ProjectionFm;

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,

  System.SysUtils,
  System.Classes,
  System.Types,
  Vcl.Controls,
  Vcl.Forms,
  
  GLS.Scene,
  GLS.Objects,
  GLS.SceneViewer,
  GLS.Texture,
  GLS.VectorGeometry,
  GLS.GeomObjects,
  GLS.RenderContextInfo,
  GLS.State,
  GLS.VectorTypes,
  GLS.Graph,
  GLS.Coordinates,
 
  GLS.BaseClasses;

type
  TFormProjection = class(TForm)
    GLScene1: TGLScene;
    SceneViewer: TGLSceneViewer;
    GLCamera: TGLCamera;
    GLDummyCube: TGLDummyCube;
    GLPlane: TGLPlane;
    GLPoints: TGLPoints;
    DirectOpenGL: TGLDirectOpenGL;
    GLArrowLine1: TGLArrowLine;
    GLLightSource1: TGLLightSource;
    GLXYZGrid1: TGLXYZGrid;
    procedure FormCreate(Sender: TObject);
    procedure DirectOpenGLRender(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure SceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
     
  public
     
    mx, my: Integer;
  end;

var
  FormProjection: TFormProjection;

implementation

{$R *.dfm}

procedure TFormProjection.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  // generate a bunch of random points
  for i := 1 to 1000 do
    GLPoints.Positions.Add((Random - 0.5) * 5, (Random - 0.5) * 5, (Random - 0.5) * 5);
end;

procedure TFormProjection.DirectOpenGLRender(Sender: TObject; var rci: TGLRenderContextInfo);
var
  i: Integer;
  mat: TMatrix;
  p, pProj: TGLVector;
  planePoint, planeNormal: TGLVector;
  plane: THmgPlane;
begin
  // Here we recover our plane point and normal...
  planePoint := GLPlane.Position.AsVector;
  planeNormal := GLPlane.Direction.AsVector;
  // ...which we use to create a plane (equation)
  plane := PlaneMake(planePoint, planeNormal);
  // from that plane equation and our pojection direction
  // (which is here the plane normal)
  mat := MakeParallelProjectionMatrix(plane, planeNormal);

  // save state, turn off lighting and specify the lines color
  rci.GLStates.Disable(stLighting);
  glColor3f(1, 1, 0);

  // we'll be drawing a bunch of lines, to specify a line in OpenGL,
  // you only need to specify the line start and end vertices
  glBegin(GL_LINES);
  for i := 0 to GLPoints.Positions.Count - 1 do
  begin
    // read the point coordinates, directly from the TGLPoints list
    MakePoint(p, GLPoints.Positions.List[i]);
    // project this point on the plane with the matrix
    pProj := VectorTransform(p, mat);
    // specify the two vertices
    glVertex3fv(@p);
    glVertex3fv(@pProj);
  end;
  glEnd;
end;

procedure TFormProjection.SceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormProjection.SceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift = [ssLeft] then
    GLCamera.MoveAroundTarget(my - Y, mx - X)
  else if Shift = [ssRight] then
    GLCamera.RotateObject(GLPlane, my - Y, mx - X);
  mx := X;
  my := Y;
end;

procedure TFormProjection.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLPlane.Position.Y := GLPlane.Position.Y + WheelDelta * 0.001;
end;

end.
