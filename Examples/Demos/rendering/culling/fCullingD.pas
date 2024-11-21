unit fCullingD;

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
  Vcl.Imaging.Jpeg,

  GLS.Scene,
  GLS.Objects,
  GLS.Cadencer,
  GLS.VectorFileObjects,
  GLS.SceneViewer,
  GLS.Texture,

  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.RenderContextInfo,
  GLS.FileMD2,
  Stage.Utils, GLS.SimpleNavigation;

type
  TFormCulling = class(TForm)
    Viewer: TGLSceneViewer;
    GLScene: TGLScene;
    GLCadencer: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DCTarget: TGLDummyCube;
    Timer1: TTimer;
    DCSpheres: TGLDummyCube;
    DCActors: TGLDummyCube;
    ACReference: TGLActor;
    GLMaterialLibrary: TGLMaterialLibrary;
    Panel2: TPanel;
    Label1: TLabel;
    RBNone: TRadioButton;
    RBObject: TRadioButton;
    RBHierarchical: TRadioButton;
    Label2: TLabel;
    RBActors: TRadioButton;
    RBSpheres: TRadioButton;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure GLCadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RBNoneClick(Sender: TObject);
    procedure RBSpheresClick(Sender: TObject);
  private

  public

  end;

var
  FormCulling: TFormCulling;

implementation

{$R *.DFM}

procedure TFormCulling.FormCreate(Sender: TObject);
var
  i, j: Integer;
  newSphere: TGLSphere;
  newActor: TGLActor;
begin
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path + '\modelext');
  // Actors are used as standalone, med-polycount objects
  // that aren't T&L friendly (all geometry must be sent to
  // the hardware at each frame)
  ACReference.LoadFromFile('waste.md2');
  for i := -3 to 3 do
    for j := -3 to 3 do
    begin
      newActor := (DCActors.AddNewChild(TGLActor) as TGLActor);
      newActor.Assign(ACReference);
      newActor.Position.SetPoint(i * 10, 0, j * 10);
      newActor.CurrentFrame := (i + 2) + (j + 2) * 5;
    end;

  GLMaterialLibrary.Materials[0].Material.Texture.Image.LoadFromFile('waste.jpg');
  // Spheres are used as standalone, high-polycount objects
  // that are highly T&L friendly
  for i := -4 to 4 do
    for j := -4 to 4 do
    begin
      newSphere := (DCSpheres.AddNewChild(TGLSphere) as TGLSphere);
      newSphere.Position.SetPoint(i * 5, 0, j * 5);
      newSphere.Slices := 32;
      newSphere.Stacks := 32;
    end;

  ACReference.Visible := False;
end;

procedure TFormCulling.RBSpheresClick(Sender: TObject);
begin
  DCActors.Visible := RBActors.Checked;
  DCSpheres.Visible := RBSpheres.Checked;
end;

procedure TFormCulling.GLCadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  Viewer.Invalidate;
end;

procedure TFormCulling.Timer1Timer(Sender: TObject);
begin
  Viewer.ResetPerformanceMonitor;
end;

procedure TFormCulling.RBNoneClick(Sender: TObject);
begin
  if RBObject.Checked then
    GLScene.VisibilityCulling := vcObjectBased
  else if RBHierarchical.Checked then
    GLScene.VisibilityCulling := vcHierarchical
  else
    GLScene.VisibilityCulling := vcNone;
end;

end.
