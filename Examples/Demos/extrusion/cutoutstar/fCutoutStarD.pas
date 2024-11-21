unit fCutoutStarD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,


  GLS.Cadencer,
  GLS.Scene,
  GLS.Extrusion,
  Stage.VectorGeometry,
  GLS.MultiPolygon,
  GLS.SceneViewer,
 
  GLS.Coordinates,
  GLS.BaseClasses;

type
  TFormCutoutStar = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    ExtrusionSolid: TGLExtrusionSolid;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    PanelFPS: TPanel;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  public
  end;

var
  FormCutoutStar: TFormCutoutStar;

implementation

{$R *.DFM}

procedure TFormCutoutStar.FormCreate(Sender: TObject);
var
   i : Integer;
   r, x, y : Single;
const
   cSteps = 16;
 begin
   // a small star contour
   with ExtrusionSolid.Contours.Add.Nodes do
   for i := 0 to cSteps do
   begin
     r := 2 + (i and 1) * 2;
     SinCosine(i * c2PI / cSteps, y, x);
     AddNode(x * r, y * r, 0);
   end;
   // add an empty contour for the square cutout (see progress event)
   ExtrusionSolid.Contours.Add;
 end;

procedure TFormCutoutStar.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   x, y : Single;
begin
   // Make our Extrusion roll
   ExtrusionSolid.Roll(deltaTime*10);

   // At each frame, we drop the cutout and make a new.
   // Note that we could also have defined it once in the FormCreate and then moved
   // it around with the TGLNodes methods.
   SinCosine(newTime, 2, y, x);
   with ExtrusionSolid.Contours do
   begin
      Items[1].Free;
      with Add.Nodes do
      begin
         AddNode(x-1, y-1, 0);
         AddNode(x+1, y-1, 0);
         AddNode(x+1, y+1, 0);
         AddNode(x-1, y+1, 0);
      end;
   end;
end;

procedure TFormCutoutStar.Timer1Timer(Sender: TObject);
begin
   // Standard FPS counter
   PanelFPS.Caption:=Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
