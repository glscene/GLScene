unit fFormulaD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,

  
  GLS.Scene,
  GLS.Objects,
  Stage.VectorTypes,
  Stage.VectorGeometry,
  GLS.Texture,
  GLS.Cadencer,
  GLS.Mesh,
  GLS.SceneViewer,
  GLS.State,
  GLS.Color,
 
  GLS.Coordinates,
  GLS.BaseClasses;

type
  TFormFormula = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    Mesh1: TGLMesh;
    DummyCube1: TGLDummyCube;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Timer1: TTimer;
    GLSceneViewer2: TGLSceneViewer;
    Panel1: TPanel;
    Label1: TLabel;
    GLScene2: TGLScene;
    DummyCube2: TGLDummyCube;
    Mesh2: TGLMesh;
    GLLightSource2: TGLLightSource;
    GLCamera2: TGLCamera;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
     
    mx, my : Integer;
    invRes1, invRes2 : Single;
    function MakeVect(const aX, aY : Single) : TAffineVector;
    procedure AddTriangle(const p1, p2, p3 : TAffineVector;
                          const color : TGLColorVector);
  public
     
  end;

var
  FormFormula: TFormFormula;

implementation

{$R *.DFM}

const
   // half-grid resolution, grid width is actually cResolution*2 of "quads"
   cResolution = 50;

function TFormFormula.MakeVect(const aX, aY : Single) : TAffineVector;
begin
  SetVector(Result, aX*invRes1, sin((aX*aX+aY*aY)*invRes2), aY*invRes1);
end;

procedure TFormFormula.AddTriangle(const p1, p2, p3 : TAffineVector;
                         const color : TGLColorVector);
begin
  with Mesh1.Vertices do begin
     AddVertex(p1, NullVector, color);
     AddVertex(p2, NullVector, color);
     AddVertex(p3, NullVector, color);
  end;
end;


procedure TFormFormula.FormCreate(Sender: TObject);
var
   x, y : Integer;
   pTopLeft, pTopRight, pBottomRight, pBottomLeft : TAffineVector;
begin
   // scaling precalcs for our math func
   invRes1:=10/cResolution;
   invRes2:=0.1*Sqr(invRes1);
   //
   // Triangles
   //
   // this one is basic : we calculate the corner points for each grid quad and
   // add the two triangles that make it
   with Mesh1 do begin
      Mode:=mmTriangles;
      Vertices.Clear;
      for y:=-cResolution to cResolution do begin
         for x:=-cResolution to cResolution do begin
            pTopLeft:=MakeVect(x, y+1);
            pTopRight:=MakeVect(x+1, y+1);
            pBottomRight:=MakeVect(x+1, y);
            pBottomLeft:=MakeVect(x, y);
            // top left triangle
            AddTriangle(pBottomLeft, pTopLeft, pTopRight, clrBlue);
            // bottom right triangle
            AddTriangle(pTopRight, pBottomRight, pBottomLeft, clrBlue);
         end;
      end;
      CalcNormals(fwCounterClockWise);
//      Vertices.Locked:=True;
   end;
   //
   // TriangleStrip
   //
   // Same as triangle, however trianglestrips are continuous, and to cover
   // the grid, "null" segments are used at both ends of a strip (to avoid a
   // visible triangle that would stretch for the full width of the grid).
   // Note : this can be avoided by reversing grid traversing direction (one line
   // from left to right, one from right to left, etc.)
   with Mesh2 do begin
      Mode:=mmTriangleStrip;
      Vertices.Clear;
      for y:=-cResolution to cResolution do begin
         pTopLeft:=MakeVect(-cResolution, y+1);
         Vertices.AddVertex(pTopLeft, NullVector, clrBlue);
         Vertices.AddVertex(pTopLeft, NullVector, clrBlue);
         for x:=-cResolution to cResolution do begin
            pTopRight:=MakeVect(x+1, y+1);
            pBottomLeft:=MakeVect(x, y);
            with Vertices do begin
               AddVertex(pBottomLeft, NullVector, clrBlue);
               AddVertex(pTopRight, NullVector, clrBlue);
            end;
         end;
         pBottomRight:=MakeVect(cResolution+1, y);
         Vertices.AddVertex(pBottomRight, NullVector, clrBlue);
         Vertices.AddVertex(pBottomRight, NullVector, clrBlue);
      end;
      CalcNormals(fwClockWise);
//      Vertices.Locked:=True;
   end;
end;

procedure TFormFormula.Timer1Timer(Sender: TObject);
begin
   // nb of triangles in scene
   Caption:= 'Formula ' + Format('%d Triangles', [2*(cResolution*2)*(cResolution*2)]);
   // calculate & display triangles framerate
   with GLSceneViewer1 do begin
      // we render twice to get a fair FPS rating
      ResetPerformanceMonitor;
      Buffer.Render;
      Buffer.Render;
      Label1.Caption:=Format('%.2f FPS (mmTriangles)', [FramesPerSecond]);
   end;
   // calculate & display trianglestrip framerate
   with GLSceneViewer2 do begin
      // we render twice to get a fair FPS rating
      ResetPerformanceMonitor;
      Buffer.Render;
      Buffer.Render;
      Label2.Caption:=Format('%.2f FPS (mmTriangleStrip)', [FramesPerSecond]);
   end;
end;

procedure TFormFormula.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=X; my:=Y;
end;

procedure TFormFormula.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then begin
      TGLSceneViewer(Sender).Camera.MoveAroundTarget(my-Y, mx-X);
      my:=Y; mx:=X;
   end;
end;

end.
