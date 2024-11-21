unit fBumpShaderD;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Types,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Imaging.Jpeg,

  
  GLS.Texture,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.Scene,
  GLS.Objects,
  GLS.VectorFileObjects,
  GLS.Graph,
  GLS.GeomObjects,
  Stage.VectorGeometry,
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  Stage.Utils,
  GLS.FileMS3D,
  GLS.File3DS,
  GLS.FileMD2,
  GLS.FileSMD,
  GLSL.Shader,
  GLSL.CustomShader,
  GLSL.BumpShaders,

  Formats.DDSImage;

type
  TFormBumpShader = class(TForm)
    Scene: TGLScene;
    Viewer: TGLSceneViewer;
    Cadencer: TGLCadencer;
    Camera: TGLCamera;
    Timer1: TTimer;
    Light: TGLLightSource;
    LightCube: TGLDummyCube;
    GLSphere1: TGLSphere;
    GLXYZGrid1: TGLXYZGrid;
    GLArrowLine1: TGLArrowLine;
    Panel1: TPanel;
    LightMovingCheckBox: TCheckBox;
    GUICube: TGLDummyCube;
    WorldCube: TGLDummyCube;
    actFighter: TGLActor;
    actTeapot: TGLActor;
    actSphere_big: TGLActor;
    actSphere_lit: TGLActor;
    MaterialLibrary: TGLMaterialLibrary;
    RollPitchTurnCheckBox: TCheckBox;
    ShaderEnabledCheckBox: TCheckBox;
    GLSphere2: TGLSphere;
    Light2: TGLLightSource;
    LightCube2: TGLDummyCube;
    MultiLightShaderCheckBox: TCheckBox;
    UseSpecularTextureCheckBox: TCheckBox;
    UseNormalTextureCheckBox: TCheckBox;
    MyBumpShader: TGLSLBumpShader;
    TrinityMatlib: TGLMaterialLibrary;
    Cube: TGLCube;
    Dodecahedron: TGLDodecahedron;
    ShowNotGLSceneObjectsCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CadencerProgress(Sender: TObject; const DeltaTime, newTime: Double);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure LightCubeProgress(Sender: TObject; const DeltaTime, newTime: Double);
    procedure ShaderEnabledCheckBoxClick(Sender: TObject);
    procedure MultiLightShaderCheckBoxClick(Sender: TObject);
    procedure UseSpecularTextureCheckBoxClick(Sender: TObject);
    procedure UseNormalTextureCheckBoxClick(Sender: TObject);
    procedure ShowNotGLSceneObjectsCheckBoxClick(Sender: TObject);
  end;

var
  FormBumpShader: TFormBumpShader;
  mx, my:    Integer;
  MultiLightShader: TGLSLMLBumpShader;

implementation

{$R *.dfm}

procedure TFormBumpShader.FormCreate(Sender: TObject);
var
  I : Integer;
begin
  //First loading modelexts
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path  + '\modelext');
  actFighter.LoadFromFile('TRINITYrage.smd'); //Fighter
  actFighter.AddDataFromFile('walk.smd');
  actFighter.Animations[1].MakeSkeletalTranslationStatic;
  actFighter.AddDataFromFile('run.smd');
  actFighter.Animations[2].MakeSkeletalTranslationStatic;
  actFighter.AddDataFromFile('long_jump.smd');
  actFighter.AddDataFromFile('jump.smd');
  actFighter.AddDataFromFile('look_left_right.smd');
  actFighter.Animations[5].MakeSkeletalRotationDelta;
  actFighter.SwitchToAnimation(1);
(*
  // or use a quake md2 model
  actFighter.LoadFromFile('waste.md2'); //Fighter
  actFighter.SwitchToAnimation(0, True);
  actFighter.AnimationMode := aamLoop;
  actFighter.Scale.Scale(3);
*)
  actFighter.AnimationMode := aamLoop;
  actFighter.Scale.Scale(3);
//  actFighter.MeshObjects.BuildTangentSpace;

  // Loading static models
  SetCurrentDir(Path  + '\model');
  actTeapot.LoadFromFile('Teapot.3ds'); //Teapot
  actTeapot.Scale.Scale(0.8);
  // actTeapotTeapot.MeshObjects.BuildTangentSpace; does not have texture coordinates...
  actSphere_big.LoadFromFile('Sphere_big.3DS'); //Sphere_big
  actSphere_big.Scale.Scale(70);
  actSphere_big.MeshObjects.BuildTangentSpace;
  actSphere_lit.LoadFromFile('Sphere.3ds'); //Sphere_little
  actSphere_lit.Scale.Scale(4);
  actSphere_lit.MeshObjects.BuildTangentSpace;

  // Then load textures
  SetCurrentDir(Path + '\map');
  MaterialLibrary.LibMaterialByName('Earth').Material.Texture.Image.LoadFromFile('earth.jpg');
  SetCurrentDir(Path  + '\texture');
  MaterialLibrary.LibMaterialByName('EarthNormals').Material.Texture.Image.LoadFromFile('EarthNormals.jpg');
  SetCurrentDir(Path  + '\cubemap');
  MaterialLibrary.LibMaterialByName('EarthGross').Material.Texture.Image.LoadFromFile('EarthSpec.dds');

  // Create Shader
  MultiLightShader := TGLSLMLBumpShader.Create(Self);
  MultiLightShader.LightSources := [1, 2];
  MultiLightShader.LightCompensation := 0.7;
  MultiLightShader.NormalTexture := MaterialLibrary.LibMaterialByName('EarthNormals').Material.Texture;
  MultiLightShader.SpecularTexture := MaterialLibrary.LibMaterialByName('EarthGross').Material.Texture;

  // Attach shader to the material
  MaterialLibrary.LibMaterialByName('Earth').Shader := MyBumpShader;
  for I := 0 to TrinityMatlib.Materials.Count - 1 do
    TrinityMatlib.Materials[I].Shader := MyBumpShader;

  ShowNotGLSceneObjectsCheckBoxClick(Sender);
  MultiLightShaderCheckBoxClick(Sender);
end;


procedure TFormBumpShader.ShaderEnabledCheckBoxClick(Sender: TObject);
begin
  MyBumpShader.Enabled := ShaderEnabledCheckBox.Checked;
  MultiLightShader.Enabled := ShaderEnabledCheckBox.Checked;
end;

procedure TFormBumpShader.MultiLightShaderCheckBoxClick(Sender: TObject);
var
  I: Integer;
begin
  if MultiLightShaderCheckBox.Checked then
  begin
    MaterialLibrary.LibMaterialByName('Earth').Shader := MultiLightShader;
    for I := 0 to TrinityMatlib.Materials.Count - 1 do
      TrinityMatlib.Materials[I].Shader := MultiLightShader;
  end
  else
  begin
    MaterialLibrary.LibMaterialByName('Earth').Shader := MyBumpShader;
    for I := 0 to TrinityMatlib.Materials.Count - 1 do
      TrinityMatlib.Materials[I].Shader := MyBumpShader;
  end;

  Light2.Shining := MultiLightShaderCheckBox.Checked;
  LightCube2.Visible := MultiLightShaderCheckBox.Checked;
end;

procedure TFormBumpShader.UseSpecularTextureCheckBoxClick(Sender: TObject);
begin
  if UseSpecularTextureCheckBox.Checked then
  begin
    MyBumpShader.SpecularTexture := MaterialLibrary.LibMaterialByName('EarthGross').Material.Texture;
    MultiLightShader.SpecularTexture := MaterialLibrary.LibMaterialByName('EarthGross').Material.Texture;
  end
  else
  begin
    MyBumpShader.SpecularTexture := nil;
    MultiLightShader.SpecularTexture := nil;
  end;
end;

procedure TFormBumpShader.UseNormalTextureCheckBoxClick(Sender: TObject);
begin
  if UseNormalTextureCheckBox.Checked then
  begin
    MyBumpShader.NormalTexture := MaterialLibrary.LibMaterialByName('EarthNormals').Material.Texture;
    MultiLightShader.NormalTexture := MaterialLibrary.LibMaterialByName('EarthNormals').Material.Texture;
  end
  else
  begin
    MyBumpShader.NormalTexture := nil;
    MultiLightShader.NormalTexture := nil;
  end;
end;

procedure TFormBumpShader.ShowNotGLSceneObjectsCheckBoxClick(
  Sender: TObject);
begin
  actTeapot.Visible := ShowNotGLSceneObjectsCheckBox.Checked;
  actFighter.Visible := ShowNotGLSceneObjectsCheckBox.Checked;
  Cube.Visible := ShowNotGLSceneObjectsCheckBox.Checked;
  Dodecahedron.Visible := ShowNotGLSceneObjectsCheckBox.Checked;
end;

procedure TFormBumpShader.CadencerProgress(Sender: TObject; const DeltaTime, newTime: Double);
begin
  Viewer.Invalidate;

  if RollPitchTurnCheckBox.Checked then
  begin
    actSphere_big.Turn(DeltaTime * 40);
    actSphere_big.Roll(DeltaTime * 40);
    actSphere_lit.Pitch(DeltaTime * 20);
    actFighter.Roll(DeltaTime * 20);
    actTeapot.Roll(-DeltaTime * 10);
    Cube.Pitch(-DeltaTime * 10);
    Dodecahedron.Pitch(DeltaTime * 10);
  end;
end;


procedure TFormBumpShader.ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;


procedure TFormBumpShader.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (ssRight in Shift) and (ssLeft in Shift) then
    Camera.AdjustDistanceToTarget(Power(1.01, Y - my))
  else
  if (ssRight in Shift) or (ssLeft in Shift) then
    Camera.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;


procedure TFormBumpShader.Timer1Timer(Sender: TObject);
begin
  Caption := 'GLSL Bump Shader - ' + Viewer.FramesPerSecondText;
  Viewer.ResetPerformanceMonitor;
end;


procedure TFormBumpShader.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Camera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TFormBumpShader.LightCubeProgress(Sender: TObject; const DeltaTime, newTime: Double);
begin
  if LightMovingCheckBox.Checked then
    LightCube.MoveObjectAround(Camera.TargetObject, sin(NewTime) * DeltaTime * 10, DeltaTime * 20);
end;

procedure TFormBumpShader.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Cadencer.Enabled := False;
end;

end.

