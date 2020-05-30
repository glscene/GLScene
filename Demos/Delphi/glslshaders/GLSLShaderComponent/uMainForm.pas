unit uMainForm;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,

  GLTexture,
  GLCadencer,
  GLWin32Viewer,
  GLScene,
  GLObjects,
  GLGraph,
  GLVectorLists,
  GLVectorTypes,
  GLVectorGeometry,
  GLSLShader,
  GLGeomObjects,
  GLVectorFileObjects,
  GLSimpleNavigation,
  GLCustomShader,
  GLCrossPlatform,
  GLMaterial,
  GLCoordinates,
  GLBaseClasses,
  GLUtils,

  GLFileMD2,
  GLFileMS3D,
  GLFile3DS,
  DDSImage;

type
  TGLSLTestForm = class(TForm)
    Scene: TGLScene;
    Viewer: TGLSceneViewer;
    Cadencer: TGLCadencer;
    Camera: TGLCamera;
    Light:  TGLLightSource;
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
    ShadeEnabledCheckBox: TCheckBox;
    TurnPitchrollCheckBox: TCheckBox;
    GLSLShader: TGLSLShader;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure FormCreate(Sender: TObject);
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: double);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LightCubeProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure ShadeEnabledCheckBoxClick(Sender: TObject);
    procedure GLSLShaderApply(Shader: TGLCustomGLSLShader);
    procedure GLSLShaderInitialize(Shader: TGLCustomGLSLShader);
    procedure GLSLShaderUnApply(Shader: TGLCustomGLSLShader;
      var ThereAreMorePasses: Boolean);
    procedure GLSLShaderApplyEx(Shader: TGLCustomGLSLShader;
      Sender: TObject);
    procedure GLSLShaderInitializeEx(Shader: TGLCustomGLSLShader;
      Sender: TObject);
  end;

var
  GLSLTestForm:  TGLSLTestForm;

implementation

{$R *.dfm}

procedure TGLSLTestForm.FormCreate(Sender: TObject);
begin
  //First load scripts from shader directory
  SetGLSceneMediaDir();
  GLSLShader.LoadShaderPrograms('Shaders\Shader.Vert','Shaders\Shader.Frag');
  GLSLShader.Enabled := True;


  //Second load models from media directory
  SetGLSceneMediaDir();

  Fighter.LoadFromFile('waste.md2'); //Fighter
  Fighter.SwitchToAnimation(0, True);
  Fighter.AnimationMode := aamLoop;
  Fighter.Scale.Scale(3);

  Teapot.LoadFromFile('Teapot.3ds'); //Teapot (no texture coordinates)
  Teapot.Scale.Scale(0.8);

  Sphere_big.LoadFromFile('Sphere_big.3DS'); //Sphere_big
  Sphere_big.Scale.Scale(70);

  Sphere_little.LoadFromFile('Sphere_little.3ds'); //Sphere_little
  Sphere_little.Scale.Scale(4);

  // Then load textures.
  MaterialLibrary.LibMaterialByName('Earth').Material.Texture.Image.LoadFromFile('Earth.jpg');
  MaterialLibrary.LibMaterialByName('WasteSkin').Material.Texture.Image.LoadFromFile('waste.jpg');

end;

procedure TGLSLTestForm.ShadeEnabledCheckBoxClick(Sender: TObject);
begin
  GLSLShader.Enabled := ShadeEnabledCheckBox.Checked;
end;

procedure TGLSLTestForm.GLSLShaderApply(Shader: TGLCustomGLSLShader);
begin
{*  Old variant of Apply
  with Shader do
  begin
    Param['DiffuseColor'].AsVector4f := VectorMake(1, 1, 1, 1);
    Param['AmbientColor'].AsVector4f := VectorMake(0.2, 0.2, 0.2, 1);
    Param['LightIntensity'].AsVector1f := 1;
    Param['MainTexture'].AsTexture2D[0] :=
      MaterialLibrary.LibMaterialByName('Earth').Material.Texture;
  end;
*}
end;

procedure TGLSLTestForm.GLSLShaderApplyEx(Shader: TGLCustomGLSLShader;
  Sender: TObject);
begin
  with Shader do
  begin
    Param['DiffuseColor'].AsVector4f := VectorMake(1, 1, 1, 1);
    Param['AmbientColor'].AsVector4f := VectorMake(0.2, 0.2, 0.2, 1);
    Param['LightIntensity'].AsVector1f := 1;
    SetTex('MainTexture', TGLLibMaterial(Sender).Material.Texture);
  end;
end;

procedure TGLSLTestForm.GLSLShaderInitialize(Shader: TGLCustomGLSLShader);
begin
  with Shader do
  begin
    // Nothing.
  end;
end;

procedure TGLSLTestForm.GLSLShaderInitializeEx(Shader: TGLCustomGLSLShader;
  Sender: TObject);
begin
  with Shader do
  begin
    // For AMD's shaders validation
    SetTex('MainTexture', TGLLibMaterial(Sender).Material.Texture);
  end;
end;

procedure TGLSLTestForm.GLSLShaderUnApply(Shader: TGLCustomGLSLShader;
  var ThereAreMorePasses: Boolean);
begin
  with Shader do
  begin
    // Nothing.
  end;
end;

procedure TGLSLTestForm.CadencerProgress(Sender: TObject; const deltaTime, newTime: double);
begin
  Viewer.Invalidate;

  if TurnPitchrollCheckBox.Checked then
  begin
    Sphere_big.Pitch(40 * deltaTime);
    Sphere_big.Turn(40 * deltaTime);
    Sphere_little.Roll(40 * deltaTime);
  end;
end;


procedure TGLSLTestForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Cadencer.Enabled := False;
end;


procedure TGLSLTestForm.LightCubeProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  if LightMovingCheckBox.Checked then
    LightCube.MoveObjectAround(Camera.TargetObject, sin(NewTime) * deltaTime * 10, deltaTime * 20);
end;


end.


