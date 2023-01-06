unit fObjectMatsD;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  GLS.Scene,
  GLS.VectorFileObjects,
  GLS.Objects,
  GLS.Coordinates,
  GLS.SceneViewer,
  GLS.BaseClasses,
  GLS.Material,
  GLS.Cadencer,
  GLS.GeomObjects,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  GLS.SimpleNavigation,
  GLS.Utils, GLS.Mesh;

type
  TFormMO = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLActor1: TGLActor;
    GLCadencer1: TGLCadencer;
    GLMatLibCube: TGLMaterialLibrary;
    GLPolygon1: TGLPolygon;
    dcCubes: TGLDummyCube;
    GLCube1: TGLCube;
    GLHexahedron1: TGLHexahedron;
    Panel1: TPanel;
    RadioGroup1: TRadioGroup;
    GLSimpleNavigation1: TGLSimpleNavigation;
    Sphere1: TGLSphere;
    dcSpheres: TGLDummyCube;
    ffSphere: TGLFreeForm;
    dcPlaneCube: TGLDummyCube;
    GLPlaneFront: TGLPlane;
    GLPlaneBack: TGLPlane;
    GLPlaneBottom: TGLPlane;
    GLPlaneTop: TGLPlane;
    GLPlaneRight: TGLPlane;
    GLPlaneLeft: TGLPlane;
    GLMesh1: TGLMesh;
    chbRotate: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure dcPlaneCubeProgress(Sender: TObject; const DeltaTime, NewTime: Double);
    procedure GLCadencer1Progress(Sender: TObject; const DeltaTime, NewTime: Double);
  private

  public
    Path: TFileName;
  end;

var
  FormMO: TFormMO;

implementation

{$R *.dfm}

procedure TFormMO.FormCreate(Sender: TObject);
begin
  Path := GetCurrentAssetPath();
  SetCurrentDir(Path + '\map');
  Sphere1.Material.Texture.Disabled := False;
  Sphere1.Material.Texture.Image.LoadFromFile('earth.jpg');
end;

procedure TFormMO.GLCadencer1Progress(Sender: TObject; const DeltaTime, NewTime: Double);
begin
  GLSceneViewer1.Invalidate;
  if chbRotate.Checked then
  begin
    GLHexahedron1.Turn(-0.1);
    GLCube1.Turn(0.1);
    dcPlaneCube.Turnangle := 90 * newTime;
    dcPlaneCube.PitchAngle := 90 * newTime;
    dcPlaneCube.RollAngle := -90 * newTime;
    Sphere1.Turnangle := 90 * newTime;
  end;
end;

procedure TFormMO.dcPlaneCubeProgress(Sender: TObject; const DeltaTime, NewTime: Double);
begin
 // more movement
 // dcPlaneCube.MoveObjectAround(GLCamera1.TargetObject, sin(NewTime) * DeltaTime * 10, DeltaTime * 20);
end;


procedure TFormMO.RadioGroup1Click(Sender: TObject);
begin
 case RadioGroup1.ItemIndex of
   0: ;
   1:;
   2:;
   3:;
 end;
end;

end.
