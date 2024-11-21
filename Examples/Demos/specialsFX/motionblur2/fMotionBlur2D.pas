unit fMotionBlur2D;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Math,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Controls,
  
  GLS.Objects,
  GLS.Scene,
  Stage.VectorTypes,
  GLS.Blur,
  GLS.SimpleNavigation,
  GLS.Texture,
  GLS.Cadencer,
  Stage.VectorGeometry,
  GLS.GeomObjects,
  GLS.SceneViewer,
 
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses, GLS.VectorFileObjects;

type
  TFormMotionBlur2 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    Cam: TGLCamera;
    Light: TGLLightSource;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCube1: TGLCube;
    GLSphere1: TGLSphere;
    GLTorus1: TGLTorus;
    GLIcosahedron1: TGLIcosahedron;
    GLTeapot1: TGLTeapot;
    GLCube2: TGLCube;
    GLCube3: TGLCube;
    GLMotionBlur1: TGLMotionBlur;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
  public
    MotionBlur: TGLMotionBlur;
  end;

var
  FormMotionBlur2: TFormMotionBlur2;

implementation

{$R *.dfm}

procedure TFormMotionBlur2.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLCube1.RollAngle:= GLCube1.RollAngle - 175*deltaTime;
  GLCube1.TurnAngle:= GLCube1.TurnAngle + 175*deltaTime;
  GLTorus1.RollAngle:= GLTorus1.RollAngle - 5*deltaTime;
  GLTorus1.TurnAngle:= GLTorus1.TurnAngle + 5*deltaTime;
  GLIcosahedron1.RollAngle:= GLIcosahedron1.RollAngle - 50*deltaTime;
  GLIcosahedron1.TurnAngle:= GLIcosahedron1.TurnAngle + 50*deltaTime;
  GLCube1.Position.X:=Sin(newTime+2)*8-1;
  GLCube1.Position.Y:=Cos(newTime+2)*2;
  GLCube1.Position.Z:=Cos(newTime+2)*3;
  GLCube2.Position.X:=Sin(newTime+2)*8-1;
  GLSceneViewer1.Invalidate;
end;

end.
