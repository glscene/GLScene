unit fDiffuseShaderD;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,

  GLS.Texture,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.Scene,
  GLS.Objects,
  GLS.Graph,
  GLS.VectorTypes,
  GLS.Context,
  GLS.VectorGeometry,
  GLS.GeomObjects,
  GLS.VectorFileObjects,

  GLSL.Shader,
  GLSL.DiffuseSpecularShader,
  GLSL.CustomShader,
  GLSL.UserShader,
  GLS.SimpleNavigation,
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.Utils,
  GLS.FileMD2,
  GLS.FileMS3D,
  GLS.File3DS,
  Formats.DDSImage;

type
  TFormDiffuseShader = class(TForm)
    Scene: TGLScene;
    Viewer: TGLSceneViewer;
    Cadencer: TGLCadencer;
    Camera: TGLCamera;
    Light: TGLLightSource;
    LightCube: TGLDummyCube;
    GLSphere1: TGLSphere;
    GLXYZGrid1: TGLXYZGrid;
    GLArrowLine1: TGLArrowLine;
    Panel1: TPanel;
    LightMovingCheckBox: TCheckBox;
    GUICube: TGLDummyCube;
    WorldCube: TGLDummyCube;
    Fighter: TGLActor;
    Teapot: TGLActor;
    Sphere_big: TGLActor;
    Sphere_little: TGLActor;
    MaterialLibrary: TGLMaterialLibrary;
    ShaderEnabledCheckBox: TCheckBox;
    TurnPitchrollCheckBox: TCheckBox;
    RealisticSpecularCheckBox: TCheckBox;
    LightCube2: TGLDummyCube;
    Light2: TGLLightSource;
    MultiLightShaderCheckBox: TCheckBox;
    DiffuseSpecularShader: TGLSLDiffuseSpecularShader;
    GLSimpleNavigation1: TGLSimpleNavigation;
    EnableFogCheckBox: TCheckBox;
    GLArrowLine2: TGLArrowLine;
    procedure FormCreate(Sender: TObject);
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: double);
    procedure LightCubeProgress(Sender: TObject; const deltaTime, newTime: double);
    procedure ShaderEnabledCheckBoxClick(Sender: TObject);
    procedure RealisticSpecularCheckBoxClick(Sender: TObject);
    procedure MultiLightShaderCheckBoxClick(Sender: TObject);
    procedure EnableFogCheckBoxClick(Sender: TObject);
  end;

var
  FormDiffuseShader: TFormDiffuseShader;
  MultiLightShader: TGLSLMLDiffuseSpecularShader;

implementation

{$R *.dfm}

procedure TFormDiffuseShader.FormCreate(Sender: TObject);
begin
  var Path: TFileName := GetCurrentAssetPath();
  // First load modelexts with animation and textures
  SetCurrentDir(Path + '\modelext');
  Fighter.LoadFromFile('waste.md2'); // Fighter
  Fighter.SwitchToAnimation(0, True);
  Fighter.AnimationMode := aamLoop;
  Fighter.Scale.Scale(3);
  MaterialLibrary.LibMaterialByName('Fighter').Material.Texture.Image.LoadFromFile('Waste.jpg');
  MaterialLibrary.LibMaterialByName('Fighter').Shader := DiffuseSpecularShader;

  // Second loading static models.
  SetCurrentDir(Path + '\model');
  Teapot.LoadFromFile('Teapot.3ds'); // Teapot (no texture coordinates)
  Teapot.Scale.Scale(0.8);
  Sphere_big.LoadFromFile('Sphere_big.3DS'); // Sphere_big
  Sphere_big.Scale.Scale(70);
  Sphere_little.LoadFromFile('Sphere_little.3ds'); // Sphere_little
  Sphere_little.Scale.Scale(4);

  // Loading textures
  SetCurrentDir(Path + '\texture');
  MaterialLibrary.LibMaterialByName('Earth').Material.Texture.Image.LoadFromFile('Earth.jpg');
  MaterialLibrary.LibMaterialByName('Earth').Shader := DiffuseSpecularShader;

  // This is how a shader is created in runtime.
  MultiLightShader := TGLSLMLDiffuseSpecularShader.Create(Self);

  // Disable fog.
  EnableFogCheckBoxClick(nil);

  MultiLightShaderCheckBoxClick(nil);

end;

procedure TFormDiffuseShader.CadencerProgress(Sender: TObject; const deltaTime, newTime: double);
begin
  Viewer.Invalidate;

  if TurnPitchrollCheckBox.Checked then
  begin
    Sphere_big.Pitch(40 * deltaTime);
    Fighter.Turn(40 * deltaTime);
    Sphere_little.Roll(40 * deltaTime);
    Teapot.Roll(-20 * deltaTime);
  end;
end;

procedure TFormDiffuseShader.LightCubeProgress(Sender: TObject; const deltaTime, newTime: double);
begin
  if LightMovingCheckBox.Checked then
    LightCube.MoveObjectAround(Camera.TargetObject, sin(newTime) * deltaTime * 10, deltaTime * 20);
end;

procedure TFormDiffuseShader.ShaderEnabledCheckBoxClick(Sender: TObject);
begin
  DiffuseSpecularShader.Enabled := ShaderEnabledCheckBox.Checked;
  MultiLightShader.Enabled := ShaderEnabledCheckBox.Checked;
end;

procedure TFormDiffuseShader.RealisticSpecularCheckBoxClick(Sender: TObject);
begin
  DiffuseSpecularShader.RealisticSpecular := RealisticSpecularCheckBox.Checked;
  MultiLightShader.RealisticSpecular := RealisticSpecularCheckBox.Checked;
  if DiffuseSpecularShader.RealisticSpecular then
  begin
    MaterialLibrary.Materials[0].Material.FrontProperties.Shininess := 20;
    MaterialLibrary.Materials[1].Material.FrontProperties.Shininess := 20;
  end
  else
  begin
    MaterialLibrary.Materials[0].Material.FrontProperties.Shininess := 8;
    MaterialLibrary.Materials[1].Material.FrontProperties.Shininess := 8;
  end;
end;

procedure TFormDiffuseShader.MultiLightShaderCheckBoxClick(Sender: TObject);
begin
  if MultiLightShaderCheckBox.Checked then
  begin
    MaterialLibrary.LibMaterialByName('Earth').Shader := MultiLightShader;
    MaterialLibrary.LibMaterialByName('Fighter').Shader := MultiLightShader;
  end
  else
  begin
    MaterialLibrary.LibMaterialByName('Earth').Shader := DiffuseSpecularShader;
    MaterialLibrary.LibMaterialByName('Fighter').Shader := DiffuseSpecularShader;
  end;

  Light2.Shining := MultiLightShaderCheckBox.Checked;
  LightCube2.Visible := MultiLightShaderCheckBox.Checked;
end;

procedure TFormDiffuseShader.EnableFogCheckBoxClick(Sender: TObject);
begin
  if EnableFogCheckBox.Checked then
  begin
    Viewer.Buffer.FogEnable := True;

    DiffuseSpecularShader.NotifyChange(Self);
    MultiLightShader.NotifyChange(Self);
  end
  else
  begin
    Viewer.Buffer.FogEnable := False;

    DiffuseSpecularShader.NotifyChange(Self);
    MultiLightShader.NotifyChange(Self);
  end;
end;

end.
