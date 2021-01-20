unit ShadowVolumesFm;

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
  GLS.PersistentClasses,
  GLS.Objects,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.ShadowVolume,
  GLS.VectorFileObjects,
  GLS.VectorLists,
  GLS.FileSMD,
  GLS.Texture,
  GLS.VectorTypes,
  GLS.GeomObjects,
  GLS.Silhouette,
  GLS.VectorGeometry,
  GLS.Material,
  GLS.Coordinates,

  GLS.SimpleNavigation,
  GLS.BaseClasses,
  GLS.Utils;

type
  TFormShadowVolumes = class(TForm)
    GLSceneViewer: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCadencer1: TGLCadencer;
    GLCamera: TGLCamera;
    DCCamera: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLShadowVolume: TGLShadowVolume;
    GLSphere1: TGLSphere;
    DCLight1Turn: TGLDummyCube;
    DCLight1Pitch: TGLDummyCube;
    GLPlane1: TGLPlane;
    GLPlane2: TGLPlane;
    GLPlane3: TGLPlane;
    Panel1: TPanel;
    CBShowVolumes: TCheckBox;
    Label1: TLabel;
    RBZFail: TRadioButton;
    RBZPass: TRadioButton;
    RBNoShadows: TRadioButton;
    RBDarkening: TRadioButton;
    DCLight2: TGLDummyCube;
    GLLightSource2: TGLLightSource;
    GLSphere2: TGLSphere;
    CBMainLight: TCheckBox;
    CBBlueLight: TCheckBox;
    DCLight3: TGLDummyCube;
    GLLightSource3: TGLLightSource;
    GLSphere3: TGLSphere;
    CBRedLight: TCheckBox;
    DCSpheres: TGLDummyCube;
    GLFreeForm: TGLFreeForm;
    GLCube1: TGLCube;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCylinder1: TGLCylinder;
    GLSphere4: TGLSphere;
    GLSphere_Shadow: TGLSphere;
    Label2: TLabel;
    ScrollBar_ShadowResolution: TScrollBar;
    Button_GenerateSilhouette: TButton;
    GLLines1: TGLLines;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure CBShowVolumesClick(Sender: TObject);
    procedure RBZFailClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CBMainLightClick(Sender: TObject);
    procedure CBBlueLightClick(Sender: TObject);
    procedure CBRedLightClick(Sender: TObject);
    procedure ScrollBar_ShadowResolutionChange(Sender: TObject);
    procedure Button_GenerateSilhouetteClick(Sender: TObject);
  public
    mx, my: Integer;
  end;

var
  FormShadowVolumes: TFormShadowVolumes;

implementation

{$R *.dfm}

procedure TFormShadowVolumes.FormCreate(Sender: TObject);
const
  cSpacing = 1;
  cRadius = 0.3;
  cNb = 1;
var
  X, Y, z: Integer;
  sphere: TGLSphere;
begin
  SetGLSceneMediaDir;
  // Dynamically construct an array of spheres, and make them shadow casters
  // Note that as the spheres are children of the shadowvolume component,
  // they are thus also shadow receivers. If they were created as child of
  // another object (not under the shadow volume), they would not receive
  // shadows (which can sometimes be interesting).
  for X := -cNb to cNb do
    for Y := -cNb to cNb do
      for z := -cNb to cNb do
        if (X and Y and z) <> 0 then
        begin
          sphere := TGLSphere(DCSpheres.AddNewChild(TGLSphere));
          sphere.Position.SetPoint(X * cSpacing, Y * cSpacing, z * cSpacing);
          sphere.Radius := cRadius;
          GLShadowVolume.Occluders.AddCaster(sphere, 0, scmParentVisible);
        end;
  DCSpheres.MoveTo(GLShadowVolume);
  GLFreeForm.LoadFromFile('trinityrage.smd');
  GLFreeForm.BuildSilhouetteConnectivityData;
  GLShadowVolume.Occluders.AddCaster(GLFreeForm);
  CBBlueLightClick(Self);
  CBRedLightClick(Self);
  ScrollBar_ShadowResolutionChange(Self);
end;

procedure TFormShadowVolumes.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  DCLight2.TurnAngle := newTime * 45;
  DCLight3.RollAngle := newTime * 50;
  GLSceneViewer.Invalidate;
end;

procedure TFormShadowVolumes.CBShowVolumesClick(Sender: TObject);
begin
  if CBShowVolumes.Checked then
    GLShadowVolume.Options := GLShadowVolume.Options + [svoShowVolumes]
  else
    GLShadowVolume.Options := GLShadowVolume.Options - [svoShowVolumes];
end;

procedure TFormShadowVolumes.RBZFailClick(Sender: TObject);
begin
  // this event handles all the radio buttons
  if RBDarkening.Checked then
    GLShadowVolume.Mode := svmDarkening
  else if RBNoShadows.Checked then
    GLShadowVolume.Mode := svmOff
  else
  begin
    GLShadowVolume.Mode := svmAccurate;
    if RBZFail.Checked then
      GLShadowVolume.Capping := svcAlways
    else
      GLShadowVolume.Capping := svcNever;
  end;
end;

procedure TFormShadowVolumes.GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormShadowVolumes.GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift = [ssLeft] then
  begin
    GLCamera.MoveAroundTarget((my - Y) * 0.5, (mx - X) * 0.5);
    GLCadencer1.Progress;
  end
  else if Shift = [ssRight] then
  begin
    DCLight1Turn.Turn((mx - X) * 0.5);
    DCLight1Pitch.Pitch((my - Y) * 0.5);
    GLLightSource1.TransformationChanged;
    GLCadencer1.Progress;
  end;
  mx := X;
  my := Y;
end;

procedure TFormShadowVolumes.FormResize(Sender: TObject);
begin
  GLCamera.SceneScale := GLSceneViewer.Width / 450;
end;

procedure TFormShadowVolumes.CBMainLightClick(Sender: TObject);
begin
  GLLightSource1.Shining := CBMainLight.Checked;
  GLSphere1.Visible := CBMainLight.Checked;
end;

procedure TFormShadowVolumes.CBBlueLightClick(Sender: TObject);
begin
  GLLightSource2.Shining := CBBlueLight.Checked;
  GLSphere2.Visible := CBBlueLight.Checked;
end;

procedure TFormShadowVolumes.CBRedLightClick(Sender: TObject);
begin
  GLLightSource3.Shining := CBRedLight.Checked;
  GLSphere3.Visible := CBRedLight.Checked;
end;

procedure TFormShadowVolumes.ScrollBar_ShadowResolutionChange(Sender: TObject);
begin
  GLSphere_Shadow.Stacks := ScrollBar_ShadowResolution.Position;
  GLSphere_Shadow.Slices := ScrollBar_ShadowResolution.Position;
  GLShadowVolume.FlushSilhouetteCache;
end;

procedure TFormShadowVolumes.Button_GenerateSilhouetteClick(Sender: TObject);
var
  SilhouetteParameters: TGLSilhouetteParameters;
  Silhouette: TGLSilhouette;
  i: Integer;
  Target: TGLSceneObject;
begin
  Target := GLSphere4;
  SilhouetteParameters.CappingRequired := false;
  SetVector(SilhouetteParameters.SeenFrom, GLLines1.AbsoluteToLocal(GLCamera.AbsolutePosition));
  SilhouetteParameters.Style := ssOmni;
  Silhouette := Target.GenerateSilhouette(SilhouetteParameters);
  GLLines1.Nodes.Clear;
  for i := 0 to Silhouette.Indices.Count - 1 do
    GLLines1.Nodes.AddNode
      (GLLines1.AbsoluteToLocal(Target.LocalToAbsolute(Silhouette.Vertices
      [Silhouette.Indices[i]])));
  FreeAndNil(Silhouette);
end;

end.
