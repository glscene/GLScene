unit fPointtoD;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,

  
  GLS.Objects,
  GLS.Scene,
  Stage.VectorTypes,
  Stage.VectorGeometry,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.GeomObjects,
 
  GLS.Coordinates,
  GLS.BaseClasses, GLS.SimpleNavigation;

type
  TFormPointto = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DCSphere: TGLDummyCube;
    ArrowLine: TGLArrowLine;
    GLLightSource1: TGLLightSource;
    Sphere: TGLSphere;
    DCArrow: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    Disk1: TGLDisk;
    Disk2: TGLDisk;
    Lines1: TGLLines;
    Plane1: TGLPlane;
    Lines2: TGLLines;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
  end;

var
  FormPointto: TFormPointto;

implementation

{$R *.DFM}

procedure TFormPointto.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   // Make the blue sphere turn and ride a sin
   DCSphere.Turn(deltaTime*30);
   Sphere.Position.Y := Sin(DegToRad(newTime*50))*3;

   // Make the arrow turn
   DCArrow.Turn(-deltaTime*15);

   // Make the arrow point toward the sphere, using Y as up reference
   ArrowLine.PointTo(Sphere, YHmgVector);
end;

end.
