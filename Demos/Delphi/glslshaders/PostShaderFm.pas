unit PostShaderFm;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.CheckLst,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,

  GLS.Texture,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.Scene,
  GLS.Graph,
  GLS.Utils,
  GLS.Context,
  GLS.VectorGeometry,
  GLS.GeomObjects,
  GLS.Coordinates,
  GLS.Objects,
  GLS.VectorFileObjects,
  GLS.SimpleNavigation,

  GLS.Material,
  GLS.BaseClasses,
  GLSL.PostShaders,
  GLSL.PostEffects,

  CG.PostTransformationShader,
  GLS.FileMD2,
  GLS.FileMS3D,
  GLS.File3DS;

type
  TPostShaderDemoForm = class(TForm)
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
    TurnPitchrollCheckBox: TCheckBox;
    Panel2: TPanel;
    ShaderCheckListBox: TCheckListBox;
    Label1: TLabel;
    GLSimpleNavigation1: TGLSimpleNavigation;
    PostShaderHolder: TGLPostShaderHolder;
    Label2: TLabel;
    tbBlurValue: TTrackBar;
    lblBlurValue: TLabel;
    tbThermalThreshold: TTrackBar;
    Label3: TLabel;
    lblThermalThreshold: TLabel;
    tbThermalIntensity: TTrackBar;
    Label5: TLabel;
    lblThermalIntensity: TLabel;
    Label4: TLabel;
    tblNightThreshold: TTrackBar;
    lblNight: TLabel;
    Label6: TLabel;
    lblNightAmplification: TLabel;
    tbNightAmplification: TTrackBar;
    Label7: TLabel;
    lblDreamThreshold: TLabel;
    tbDreamThreshold: TTrackBar;
    Label8: TLabel;
    lblPixelateWidth: TLabel;
    tbPixelateWidth: TTrackBar;
    Label9: TLabel;
    lblPixelateHeight: TLabel;
    tbPixelateHeight: TTrackBar;
    Label10: TLabel;
    tbPosterizeGamma: TTrackBar;
    lblPosterizeGamma: TLabel;
    Label12: TLabel;
    tbPosterizeColors: TTrackBar;
    lblPosterizeColors: TLabel;
    tbFrostRand: TTrackBar;
    Label11: TLabel;
    lblFrostRand: TLabel;
    tbFrostFactor: TTrackBar;
    Label14: TLabel;
    lblFrostFactor: TLabel;
    Label13: TLabel;
    lblTroubleWidth: TLabel;
    tbTroubleWidth: TTrackBar;
    Label16: TLabel;
    lblTroubleHeight: TLabel;
    tbTroubleHeight: TTrackBar;
    Label18: TLabel;
    lblTroubleFreq: TLabel;
    tbTroubleFreq: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure CadencerProgress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LightCubeProgress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure ShaderCheckListBoxClick(Sender: TObject);
    procedure tbBlurValueChange(Sender: TObject);
    procedure tbThermalThresholdChange(Sender: TObject);
    procedure tbThermalIntensityChange(Sender: TObject);
    procedure tblNightThresholdChange(Sender: TObject);
    procedure tbNightAmplificationChange(Sender: TObject);
    procedure tbDreamThresholdChange(Sender: TObject);
    procedure tbPixelateWidthChange(Sender: TObject);
    procedure tbPixelateHeightChange(Sender: TObject);
    procedure tbPosterizeGammaChange(Sender: TObject);
    procedure tbPosterizeColorsChange(Sender: TObject);
    procedure tbFrostRandChange(Sender: TObject);
    procedure tbFrostFactorChange(Sender: TObject);
    procedure tbTroubleWidthChange(Sender: TObject);
    procedure tbTroubleHeightChange(Sender: TObject);
    procedure tbTroubleFreqChange(Sender: TObject);
  end;

var
  PostShaderDemoForm: TPostShaderDemoForm;
  //Shaders
  BlurShader: TGLSLPostBlurShader;
  ThermalVisionShader: TGLSLPostThermalVisionShader;
  TransformationShader: TGLCGPostTransformationShader;
  DreamVisionShader: TGLSLPostDreamVisionShader;
  NightVisionShader: TGLSLPostNightVisionShader;
  PosterizeShader: TGLSLPostPosterizeShader;
  FrostShader: TGLSLPostFrostShader;
  PixelateShader: TGLSLPostPixelateShader;
  TroubleShader: TGLSLPostTroubleShader;

implementation

{$R *.dfm}

procedure TPostShaderDemoForm.FormCreate(Sender: TObject);
begin
  // First load models.
  SetGLSceneMediaDir();
  Fighter.LoadFromFile('waste.md2'); // Fighter
  Fighter.SwitchToAnimation(0, True);
  Fighter.AnimationMode := aamLoop;
  Fighter.Scale.Scale(2);

  Teapot.LoadFromFile('Teapot.3ds'); // Teapot (no texture coordinates)
  Teapot.Scale.Scale(0.8);

  Sphere_big.LoadFromFile('Sphere_big.3DS');
  Sphere_big.Scale.Scale(70);

  Sphere_little.LoadFromFile('Sphere_little.3ds');
  Sphere_little.Scale.Scale(4);
  // Then load textures.
  MaterialLibrary.LibMaterialByName('Earth').Material.Texture.Image.LoadFromFile
    ('Earth.jpg');
  MaterialLibrary.LibMaterialByName('Fighter')
    .Material.Texture.Image.LoadFromFile('Waste.jpg');
  MaterialLibrary.LibMaterialByName('Noise').Material.Texture.Image.LoadFromFile
    ('Flare1.bmp');
  // MaterialLibrary.LibMaterialByName('Noise').Material.Texture.Image.LoadFromFile('wikiNoise.jpg');
  MaterialLibrary.LibMaterialByName('Mask').Material.Texture.Image.LoadFromFile
    ('wikiMask.jpg');

  // Blur Shader
  BlurShader := TGLSLPostBlurShader.Create(Self);
  BlurShader.Enabled := false;
  BlurShader.Threshold := 0.001;
  PostShaderHolder.Shaders.Add.Shader := BlurShader;
  ShaderCheckListBox.Items.AddObject('Blur Shader', BlurShader);
  ShaderCheckListBox.Checked[0] := false;

  // ThermalVision Shader
  ThermalVisionShader := TGLSLPostThermalVisionShader.Create(Self);
  ThermalVisionShader.Enabled := false;
  PostShaderHolder.Shaders.Add.Shader := ThermalVisionShader;
  ShaderCheckListBox.Items.AddObject('Thermal Vision Shader',
    ThermalVisionShader);
  ShaderCheckListBox.Checked[1] := false;

  // DreamVision Shader
  DreamVisionShader := TGLSLPostDreamVisionShader.Create(Self);
  DreamVisionShader.Enabled := false;
  PostShaderHolder.Shaders.Add.Shader := DreamVisionShader;
  ShaderCheckListBox.Items.AddObject('Dream Vision Shader', DreamVisionShader);
  ShaderCheckListBox.Checked[2] := false;

  // NightVision Shader
  NightVisionShader := TGLSLPostNightVisionShader.Create(Self);
  NightVisionShader.Enabled := false;
  NightVisionShader.MaterialLibrary := MaterialLibrary;
  NightVisionShader.NoiseTexName := 'Noise';
  NightVisionShader.MaskTexName := 'Mask';
  NightVisionShader.UseMask := 1;
  PostShaderHolder.Shaders.Add.Shader := NightVisionShader;
  ShaderCheckListBox.Items.AddObject('Night Vision Shader', NightVisionShader);
  ShaderCheckListBox.Checked[3] := false;

  // Pixelate Shader
  PixelateShader := TGLSLPostPixelateShader.Create(Self);
  PixelateShader.Enabled := false;
  PostShaderHolder.Shaders.Add.Shader := PixelateShader;
  ShaderCheckListBox.Items.AddObject('Pixelate Shader', PixelateShader);
  ShaderCheckListBox.Checked[4] := false;

  // Posterize Shader
  PosterizeShader := TGLSLPostPosterizeShader.Create(Self);
  PosterizeShader.Enabled := false;
  PostShaderHolder.Shaders.Add.Shader := PosterizeShader;
  ShaderCheckListBox.Items.AddObject('Posterize Shader', PosterizeShader);
  ShaderCheckListBox.Checked[5] := false;

  // Frost Shader
  FrostShader := TGLSLPostFrostShader.Create(Self);
  FrostShader.Enabled := false;
  PostShaderHolder.Shaders.Add.Shader := FrostShader;
  ShaderCheckListBox.Items.AddObject('Frost Shader', FrostShader);
  ShaderCheckListBox.Checked[6] := false;

  // Trouble Shader
  TroubleShader := TGLSLPostTroubleShader.Create(Self);
  TroubleShader.Enabled := false;
  TroubleShader.MaterialLibrary := MaterialLibrary;
  TroubleShader.NoiseTexName := 'Noise';
  PostShaderHolder.Shaders.Add.Shader := TroubleShader;
  ShaderCheckListBox.Items.AddObject('Trouble Shader', TroubleShader);
  ShaderCheckListBox.Checked[7] := false;

  // Transformation Shader
  TransformationShader := TGLCGPostTransformationShader.Create(Self);
  TransformationShader.TransformationTexture :=
    MaterialLibrary.LibMaterialByName('Noise').Material.Texture;
  PostShaderHolder.Shaders.Add.Shader := TransformationShader;
  ShaderCheckListBox.Items.AddObject('Transformation Shader',
    TransformationShader);
  ShaderCheckListBox.Checked[8] := True;
end;

//-------------------------------------------------------

procedure TPostShaderDemoForm.CadencerProgress(Sender: TObject;
  const deltaTime, newTime: double);
begin
  Viewer.Invalidate;
  if TurnPitchrollCheckBox.Checked then
  begin
    Fighter.Roll(20 * deltaTime);
    Sphere_big.Pitch(40 * deltaTime);
    Sphere_big.Turn(40 * deltaTime);
    Sphere_little.Roll(40 * deltaTime);
    Teapot.Roll(-20 * deltaTime);
  end;
  if NightVisionShader.Enabled then
    NightVisionShader.ElapsedTime := newTime; // 20*deltaTime;
end;

//-------------------------------------------------------

procedure TPostShaderDemoForm.LightCubeProgress(Sender: TObject;
  const deltaTime, newTime: double);
begin
  if LightMovingCheckBox.Checked then
    LightCube.MoveObjectAround(Camera.TargetObject, sin(newTime) * deltaTime *
      10, deltaTime * 20);
end;

//-------------------------------------------------------

procedure TPostShaderDemoForm.ShaderCheckListBoxClick(Sender: TObject);
var
  I: Integer;
begin
  if ShaderCheckListBox.Items.Count <> 0 then
    for I := 0 to ShaderCheckListBox.Items.Count - 1 do
      TGLShader(ShaderCheckListBox.Items.Objects[I]).Enabled :=
        ShaderCheckListBox.Checked[I];
end;

//-------------------------------------------------------

procedure TPostShaderDemoForm.tbBlurValueChange(Sender: TObject);
begin
  BlurShader.Threshold := tbBlurValue.Position / 100;
  lblBlurValue.Caption := FloatToStrF(BlurShader.Threshold, ffFixed, 5, 2);
end;

//-------------------------------------------------------

procedure TPostShaderDemoForm.tbDreamThresholdChange(Sender: TObject);
begin
  DreamVisionShader.Threshold := tbDreamThreshold.Position / 100;
  lblDreamThreshold.Caption := FloatToStrF(DreamVisionShader.Threshold,
    ffFixed, 5, 2);
end;

//-------------------------------------------------------

procedure TPostShaderDemoForm.tbFrostFactorChange(Sender: TObject);
begin
  FrostShader.RandFactor := tbFrostFactor.Position;
  lblFrostFactor.Caption := FloatToStrF(FrostShader.RandFactor, ffFixed, 5, 2);
end;

//-------------------------------------------------------

procedure TPostShaderDemoForm.tbFrostRandChange(Sender: TObject);
begin
  FrostShader.RandScale := tbFrostRand.Position;
  lblFrostRand.Caption := FloatToStrF(FrostShader.RandScale, ffFixed, 5, 2);
end;

//-------------------------------------------------------

procedure TPostShaderDemoForm.tblNightThresholdChange(Sender: TObject);
begin
  NightVisionShader.LuminanceThreshold := tblNightThreshold.Position / 100;
  lblNight.Caption := FloatToStrF(NightVisionShader.LuminanceThreshold,
    ffFixed, 5, 2);
end;

//-------------------------------------------------------

procedure TPostShaderDemoForm.tbNightAmplificationChange(Sender: TObject);
begin
  NightVisionShader.ColorAmplification := tbNightAmplification.Position / 100;
  lblNightAmplification.Caption :=
    FloatToStrF(NightVisionShader.ColorAmplification, ffFixed, 5, 2);
end;

//-------------------------------------------------------

procedure TPostShaderDemoForm.tbPixelateHeightChange(Sender: TObject);
begin
  PixelateShader.PixelHeight := tbPixelateHeight.Position;
  lblPixelateHeight.Caption := FloatToStrF(PixelateShader.PixelHeight,
    ffFixed, 5, 0);
end;

//-------------------------------------------------------

procedure TPostShaderDemoForm.tbPixelateWidthChange(Sender: TObject);
begin
  PixelateShader.PixelWidth := tbPixelateWidth.Position;
  lblPixelateWidth.Caption := FloatToStrF(PixelateShader.PixelWidth,
    ffFixed, 5, 0);
end;

//-------------------------------------------------------

procedure TPostShaderDemoForm.tbPosterizeColorsChange(Sender: TObject);
begin
  PosterizeShader.NumColors := tbPosterizeColors.Position;
  lblPosterizeColors.Caption := FloatToStrF(PosterizeShader.NumColors,
    ffFixed, 5, 2);
end;

//-------------------------------------------------------

procedure TPostShaderDemoForm.tbPosterizeGammaChange(Sender: TObject);
begin
  PosterizeShader.Gamma := tbPosterizeGamma.Position / 100;
  lblPosterizeGamma.Caption := FloatToStrF(PosterizeShader.Gamma,
    ffFixed, 5, 2);
end;

//-------------------------------------------------------

procedure TPostShaderDemoForm.tbThermalIntensityChange(Sender: TObject);
begin
  ThermalVisionShader.Intensity := tbThermalIntensity.Position / 100;
  lblThermalIntensity.Caption := FloatToStrF(ThermalVisionShader.Intensity,
    ffFixed, 5, 2);
end;

//-------------------------------------------------------

procedure TPostShaderDemoForm.tbThermalThresholdChange(Sender: TObject);
begin
  ThermalVisionShader.Threshold := tbThermalThreshold.Position / 100;
  lblThermalThreshold.Caption := FloatToStrF(ThermalVisionShader.Threshold,
    ffFixed, 5, 2);
end;

//-------------------------------------------------------

procedure TPostShaderDemoForm.tbTroubleFreqChange(Sender: TObject);
begin
  TroubleShader.Freq := tbTroubleFreq.Position / 100;
  lblTroubleFreq.Caption := FloatToStrF(TroubleShader.Freq, ffFixed, 5, 2);
end;

//-------------------------------------------------------

procedure TPostShaderDemoForm.tbTroubleHeightChange(Sender: TObject);
begin
  TroubleShader.PixelY := tbTroubleHeight.Position;
  lblTroubleHeight.Caption := FloatToStrF(TroubleShader.PixelY, ffFixed, 5, 2);
end;

//-------------------------------------------------------

procedure TPostShaderDemoForm.tbTroubleWidthChange(Sender: TObject);
begin
  TroubleShader.PixelX := tbTroubleWidth.Position;
  lblTroubleWidth.Caption := FloatToStrF(TroubleShader.PixelX, ffFixed, 5, 2);
end;

//-------------------------------------------------------

procedure TPostShaderDemoForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Cadencer.Enabled := false;
end;

end.
