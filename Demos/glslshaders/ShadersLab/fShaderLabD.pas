unit fShaderLabD;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.OpenGL,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.ExtDlgs,
  // Picture FileFormats
  Vcl.Imaging.Jpeg,
  Vcl.Imaging.PngImage,

  GLS.VectorTypes,
  GLS.VectorLists,
  GLS.VectorGeometry,
  GLS.BaseClasses,
  GLS.PersistentClasses,
  GLS.Keyboard,

  GLS.Material,
  GLS.Scene,
  GLS.SceneViewer,
  GLS.VectorFileObjects,
  GLS.Objects,
  GLS.Texture,
  GLS.Context,
  GLS.Cadencer,
  GLS.Coordinates,

  GLS.State,
  GLS.RenderContextInfo,
  GLS.TextureFormat,
  GLS.Color,
  GLS.Graphics,
  GLS.MeshUtils,
  GLS.Utils,
  GLS.GeomObjects,
  GLS.SimpleNavigation, 
  GLS.HUDObjects,

  GLSL.CustomShader,
  GLSL.ShapeShaders;

type
  TFormShaderLab = class(TForm)
    Panel1: TPanel;
    Viewer: TGLSceneViewer;
    MaterialLibrary: TGLMaterialLibrary;
    GLScene1: TGLScene;
    Cadencer: TGLCadencer;
    World: TGLDummyCube;
    Camera: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLSphere1: TGLSphere;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    chkAnimScene: TCheckBox;
    chkLightmoving: TCheckBox;
    chkFurShader: TCheckBox;
    LightCube: TGLDummyCube;
    lblFurDistance: TLabel;
    tbFurLength: TTrackBar;
    TabSheet2: TTabSheet;
    lblLatticeScaleX: TLabel;
    tbLatticeScaleX: TTrackBar;
    lblLatticeThresholdX: TLabel;
    tbLatticeThresholdX: TTrackBar;
    chkLatticeShader: TCheckBox;
    lblLatticeScaleY: TLabel;
    tbLatticeScaleY: TTrackBar;
    tbLatticeThresholdY: TTrackBar;
    TabSheet3: TTabSheet;
    chkErosionShader: TCheckBox;
    FreeForm: TGLFreeForm;
    TabSheet4: TTabSheet;
    chkIvoryShader: TCheckBox;
    LightCube2: TGLDummyCube;
    GLLightSource2: TGLLightSource;
    Label10: TLabel;
    Label11: TLabel;
    cbxFurBlendSrc: TComboBox;
    cbxFurBlendDest: TComboBox;
    Objects: TGLDummyCube;
    lblFurPassCount1: TLabel;
    tbFurPassCount: TTrackBar;
    Label4: TLabel;
    tbLatticeSpecularPower: TTrackBar;
    Label5: TLabel;
    tbLatticeLightPower: TTrackBar;
    TabSheet5: TTabSheet;
    chkGoochShader: TCheckBox;
    GLTorus1: TGLTorus;
    lblFurLength: TLabel;
    Label7: TLabel;
    tbFurMaxLength: TTrackBar;
    lblFurMaxLength: TLabel;
    lblFurPassCount: TLabel;
    chkFurRandomLength: TCheckBox;
    Label12: TLabel;
    tbFurDensity: TTrackBar;
    lblFurDensity: TLabel;
    Label6: TLabel;
    tbFurLightPower: TTrackBar;
    lblFurLightPower: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    ColorDialog: TColorDialog;
    Label13: TLabel;
    Shape3: TShape;
    Label14: TLabel;
    Shape4: TShape;
    Label15: TLabel;
    Shape5: TShape;
    Label16: TLabel;
    Shape6: TShape;
    Label17: TLabel;
    Shape7: TShape;
    Label18: TLabel;
    tbGoochDFactor: TTrackBar;
    lblGoochDFactor: TLabel;
    Label20: TLabel;
    tbGoochWFactor: TTrackBar;
    lblGoochWFactor: TLabel;
    Label22: TLabel;
    tbGoochCFactor: TTrackBar;
    lblGoochCFactor: TLabel;
    Label24: TLabel;
    tbGoochAFactor: TTrackBar;
    lblGoochAFactor: TLabel;
    Label26: TLabel;
    tbGoochSFactor: TTrackBar;
    lblGoochSFactor: TLabel;
    Label1: TLabel;
    tbErosionFactor: TTrackBar;
    lblErosionFactor: TLabel;
    Label3: TLabel;
    tberosionScale: TTrackBar;
    lblErosionScale: TLabel;
    Label25: TLabel;
    tbErosionIFactor1: TTrackBar;
    lblErosionIFactor1: TLabel;
    Label28: TLabel;
    tbErosionIFactor2: TTrackBar;
    lblerosionIFactor2: TLabel;
    Label2: TLabel;
    tbErosionAmbientF: TTrackBar;
    lblErosionAmbientF: TLabel;
    Label27: TLabel;
    tbErosionDiffuseF: TTrackBar;
    lblErosionDiffuseF: TLabel;
    Label30: TLabel;
    tbErosionSpecularF: TTrackBar;
    lblErosionSpecularF: TLabel;
    Label32: TLabel;
    tbErosionSpecularR: TTrackBar;
    lblErosionSpecularR: TLabel;
    Label34: TLabel;
    tbErosionAnisoR: TTrackBar;
    lblErosionAnisoR: TLabel;
    Label36: TLabel;
    Shape8: TShape;
    Shape9: TShape;
    Label37: TLabel;
    lblLatticeThresholdY: TLabel;
    lblLatticeSpecularPower: TLabel;
    lblLatticeLightPower: TLabel;
    Label23: TLabel;
    Label29: TLabel;
    Label31: TLabel;
    Label33: TLabel;
    Label35: TLabel;
    Shape10: TShape;
    Label38: TLabel;
    Shape11: TShape;
    Label39: TLabel;
    Shape12: TShape;
    cbxGootchBlendMode: TComboBox;
    Label40: TLabel;
    tbGoochAlpha: TTrackBar;
    Label41: TLabel;
    lblGoochAlpha: TLabel;
    TabSheet6: TTabSheet;
    Label19: TLabel;
    tbSemDiffuseF: TTrackBar;
    lblSemDiffuseF: TLabel;
    Label42: TLabel;
    tbSemAmbientF: TTrackBar;
    lblSemAmbientF: TLabel;
    Label44: TLabel;
    tbSemSpecularF: TTrackBar;
    lblSemSpecularF: TLabel;
    Label46: TLabel;
    Shape13: TShape;
    Label47: TLabel;
    Shape14: TShape;
    chkSEMShader: TCheckBox;
    Displacement: TTabSheet;
    chkVDShader: TCheckBox;
    Label21: TLabel;
    tbVDDiffuseF: TTrackBar;
    Label43: TLabel;
    tbVDAmbientF: TTrackBar;
    Label45: TLabel;
    tbVDSpecularF: TTrackBar;
    lblVDSpecularF: TLabel;
    lblVDAmbientF: TLabel;
    lblVDDiffuseF: TLabel;
    Label51: TLabel;
    Shape15: TShape;
    Label52: TLabel;
    Shape16: TShape;
    chkVDAnimate: TCheckBox;
    Label48: TLabel;
    tbVDNoise: TTrackBar;
    lblVDNoise: TLabel;
    Label49: TLabel;
    tbVDPeriod: TTrackBar;
    lblVDPeriod: TLabel;
    Label53: TLabel;
    tbVDNScale: TTrackBar;
    lblVDNScale: TLabel;
    Label55: TLabel;
    tbVDTurb: TTrackBar;
    lblVDTurb: TLabel;
    Label57: TLabel;
    tbVDDispScale: TTrackBar;
    lblVDDispScale: TLabel;
    Label50: TLabel;
    tbVDTimeF: TTrackBar;
    lblVDTimeF: TLabel;
    cbxObjects: TComboBox;
    Label54: TLabel;
    Button2: TButton;
    Button3: TButton;
    GLSimpleNavigation1: TGLSimpleNavigation;
    Button1: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    TabSheet7: TTabSheet;
    Label56: TLabel;
    Label58: TLabel;
    tbGlassDepth: TTrackBar;
    tbGlassMix: TTrackBar;
    Label59: TLabel;
    Shape17: TShape;
    lblGlassDepth: TLabel;
    lblGlassMix: TLabel;
    Button10: TButton;
    chkGlassShader: TCheckBox;
    Label60: TLabel;
    tbGlassAlpha: TTrackBar;
    lblGlassAlpha: TLabel;
    Label61: TLabel;
    cbxGlassBlendSrc: TComboBox;
    Label62: TLabel;
    cbxGlassBlendDst: TComboBox;
    ScreenBackGround: TGLHUDSprite;
    chkBackgroundImg: TCheckBox;
    Button11: TButton;
    Label63: TLabel;
    edtFurGravityX: TEdit;
    edtFurGravityY: TEdit;
    edtFurGravityZ: TEdit;
    TabSheet8: TTabSheet;
    chkToonShader: TCheckBox;
    Label64: TLabel;
    tbToonHighlightSize: TTrackBar;
    lblToonHighlightSize: TLabel;
    Label66: TLabel;
    tbToonMidSize: TTrackBar;
    lblToonMidSize: TLabel;
    Label68: TLabel;
    tbToonShadowSize: TTrackBar;
    lblToonShadowSize: TLabel;
    Label70: TLabel;
    tbToonOutlineWidth: TTrackBar;
    lblToonOutlineWidth: TLabel;
    Label72: TLabel;
    Shape18: TShape;
    Label73: TLabel;
    Shape19: TShape;
    Label74: TLabel;
    Shape20: TShape;
    Label75: TLabel;
    Shape21: TShape;
    Label76: TLabel;
    Shape22: TShape;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure chkFurShaderClick(Sender: TObject);
    procedure CadencerProgress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure LightCubeProgress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure tbFurLengthChange(Sender: TObject);
    procedure tbLatticeScaleXChange(Sender: TObject);
    procedure chkLatticeShaderClick(Sender: TObject);
    procedure tbLatticeScaleYChange(Sender: TObject);
    procedure tbLatticeThresholdXChange(Sender: TObject);
    procedure tbLatticeThresholdYChange(Sender: TObject);
    procedure chkIvoryShaderClick(Sender: TObject);
    procedure LightCube2Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure cbxFurBlendDestChange(Sender: TObject);
    procedure cbxFurBlendSrcChange(Sender: TObject);
    procedure tbFurPassCountChange(Sender: TObject);
    procedure chkGoochShaderClick(Sender: TObject);
    procedure tbFurMaxLengthChange(Sender: TObject);
    procedure tbFurDensityChange(Sender: TObject);
    procedure chkFurRandomLengthClick(Sender: TObject);
    procedure tbFurLightPowerChange(Sender: TObject);
    procedure Shape1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape3MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape4MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape5MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape6MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape7MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tbGoochDFactorChange(Sender: TObject);
    procedure tbGoochWFactorChange(Sender: TObject);
    procedure tbGoochCFactorChange(Sender: TObject);
    procedure tbGoochAFactorChange(Sender: TObject);
    procedure tbGoochSFactorChange(Sender: TObject);
    procedure cbxGoochBlendSrcChange(Sender: TObject);
    procedure cbxGoochBlendDstChange(Sender: TObject);
    procedure chkErosionShaderClick(Sender: TObject);
    procedure tbErosionFactorChange(Sender: TObject);
    procedure tberosionScaleChange(Sender: TObject);
    procedure tbErosionIFactor1Change(Sender: TObject);
    procedure tbErosionIFactor2Change(Sender: TObject);
    procedure tbErosionDiffuseFChange(Sender: TObject);
    procedure tbErosionAmbientFChange(Sender: TObject);
    procedure tbErosionSpecularFChange(Sender: TObject);
    procedure tbErosionSpecularRChange(Sender: TObject);
    procedure tbErosionAnisoRChange(Sender: TObject);
    procedure Shape8MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape9MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tbLatticeSpecularPowerChange(Sender: TObject);
    procedure tbLatticeLightPowerChange(Sender: TObject);
    procedure Shape10MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape11MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape12MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbxGootchBlendModeChange(Sender: TObject);
    procedure tbGoochAlphaChange(Sender: TObject);
    procedure chkSEMShaderClick(Sender: TObject);
    procedure tbSemDiffuseFChange(Sender: TObject);
    procedure tbSemAmbientFChange(Sender: TObject);
    procedure tbSemSpecularFChange(Sender: TObject);
    procedure Shape13MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape14MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chkVDShaderClick(Sender: TObject);
    procedure tbVDDiffuseFChange(Sender: TObject);
    procedure tbVDAmbientFChange(Sender: TObject);
    procedure tbVDSpecularFChange(Sender: TObject);
    procedure tbVDNoiseChange(Sender: TObject);
    procedure tbVDPeriodChange(Sender: TObject);
    procedure tbVDNScaleChange(Sender: TObject);
    procedure tbVDTurbChange(Sender: TObject);
    procedure tbVDDispScaleChange(Sender: TObject);
    procedure tbVDTimeFChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cbxObjectsChange(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure chkGlassShaderClick(Sender: TObject);
    procedure Shape17MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tbGlassDepthChange(Sender: TObject);
    procedure tbGlassMixChange(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure tbGlassAlphaChange(Sender: TObject);
    procedure cbxGlassBlendSrcChange(Sender: TObject);
    procedure cbxGlassBlendDstChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure chkBackgroundImgClick(Sender: TObject);
    procedure EditFloatKeyPress(Sender: TObject; var Key: Char);
    procedure edtFurGravityXChange(Sender: TObject);
    procedure edtFurGravityYChange(Sender: TObject);
    procedure edtFurGravityZChange(Sender: TObject);
    procedure chkToonShaderClick(Sender: TObject);
    procedure tbToonHighlightSizeChange(Sender: TObject);
    procedure tbToonMidSizeChange(Sender: TObject);
    procedure tbToonShadowSizeChange(Sender: TObject);
    procedure tbToonOutlineWidthChange(Sender: TObject);
    procedure Shape18MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape19MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape20MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape21MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape22MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  end;

var
  FormShaderLab: TFormShaderLab;
  mx, my: Integer;

  FurShader: TGLSLFurShader;
  LatticeShader: TGLSLLatticeShader;
  IvoryShader: TGLSLIvoryShader;
  GoochShader: TGLSLSimpleGoochShader;
  ErosionShader: TGLSLSimpleErosionShader;
  SEMShader: TGLSLSemShader;
  VertexDisplacementShader: TGLSLVertexDisplacementShader;
  GlassShader: TGLSLGlassShader;
  ToonShader: TGLSLToonShader;

  // DimpleShader
  // WoodShader
  // PhongShader
  // CookTorrenceShader
  // OreilNayar

  i, j: Integer;
  v: Single;

//----------------------------------------
implementation
//----------------------------------------

{$R *.dfm}

uses
  GLS.FileTGA,
  GLS.FileOBJ,
  GLS.FileSTL,
  GLS.FileLWO,
  GLS.FileQ3BSP,
  GLS.FileOCT,
  GLS.FileMS3D,
  GLS.FileNMF,
  GLS.FileMD3,
  GLS.File3DS,
  GLS.FileMD2,
  GLS.FileSMD,
  GLS.FilePLY,
  GLS.FileGTS,
  GLS.FileVRML,
  GLS.FileMD5,
  GLS.FileTIN,
  GLS.FileDXF,
  GLS.FileGRD;

procedure TFormShaderLab.Button10Click(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    MaterialLibrary.LibMaterialByName('RefractMap')
      .Material.Texture.Image.LoadFromFile(OpenPictureDialog.FileName);
  end;
end;

procedure TFormShaderLab.Button11Click(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    MaterialLibrary.LibMaterialByName('BackgroundTex')
      .Material.Texture.Image.LoadFromFile(OpenPictureDialog.FileName);
  end;
end;

procedure TFormShaderLab.Button1Click(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    MaterialLibrary.LibMaterialByName('ExplosionTexture')
      .Material.Texture.Image.LoadFromFile(OpenPictureDialog.FileName);
  end;
end;

procedure TFormShaderLab.Button2Click(Sender: TObject);
begin
  if ColorDialog.Execute then
  begin
    Viewer.Buffer.BackgroundColor := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Button3Click(Sender: TObject);
begin
  VertexDisplacementShader.ElapsedTime := 1.0;
end;

procedure TFormShaderLab.Button4Click(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    MaterialLibrary.LibMaterialByName('MatCapTexture')
      .Material.Texture.Image.LoadFromFile(OpenPictureDialog.FileName);
  end;
end;

procedure TFormShaderLab.Button5Click(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    MaterialLibrary.LibMaterialByName('ErosionMainTexture')
      .Material.Texture.Image.LoadFromFile(OpenPictureDialog.FileName);
  end;
end;

procedure TFormShaderLab.Button6Click(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    MaterialLibrary.LibMaterialByName('ErosionTexture')
      .Material.Texture.Image.LoadFromFile(OpenPictureDialog.FileName);
  end;
end;

procedure TFormShaderLab.Button8Click(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    MaterialLibrary.LibMaterialByName('MainTexture')
      .Material.Texture.Image.LoadFromFile(OpenPictureDialog.FileName);
  end;
end;

procedure TFormShaderLab.Button9Click(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    MaterialLibrary.LibMaterialByName('NoiseTexture')
      .Material.Texture.Image.LoadFromFile(OpenPictureDialog.FileName);
  end;
end;

procedure TFormShaderLab.CadencerProgress(Sender: TObject;
  const deltaTime, newTime: Double);

begin

  if chkAnimScene.Checked then
  begin
    // GLSphere1.Pitch(40 * deltaTime);
    Objects.Pitch(40 * deltaTime);
    Objects.Turn(20 * deltaTime);
    Objects.Roll(40 * deltaTime);
  end;

  if TabSheet1.Enabled then //Fur
  begin
    if (i <= 10) and (j >= 0) then
    begin
      v := v + 0.2;
      i := i + 1;
      if i = 10 then
        j := 0;
    end;

    if (j <= 10) and (i >= 0) then
    begin
      v := v - 0.2;
      j := j + 1;
      if j = 10 then
        i := 0;
    end;
    FurShader.Gravity.Y := v;
  end;

  if chkVDAnimate.Checked then
  begin
    VertexDisplacementShader.ElapsedTime := newTime;
  end;

  Viewer.Invalidate;
end;

procedure TFormShaderLab.cbxFurBlendDestChange(Sender: TObject);
begin
  FurShader.BlendDst := TGLBlendFunction(cbxFurBlendDest.ItemIndex);
end;

procedure TFormShaderLab.cbxFurBlendSrcChange(Sender: TObject);
begin
  FurShader.BlendSrc := TGLBlendFunction(cbxFurBlendSrc.ItemIndex);
end;

procedure TFormShaderLab.cbxGlassBlendDstChange(Sender: TObject);
begin
  GlassShader.BlendDst := TGLBlendFunction(cbxGlassBlendDst.ItemIndex);
end;

procedure TFormShaderLab.cbxGlassBlendSrcChange(Sender: TObject);
begin
  GlassShader.BlendSrc := TGLBlendFunction(cbxGlassBlendSrc.ItemIndex);
end;

procedure TFormShaderLab.cbxGoochBlendDstChange(Sender: TObject);
begin
  //GoochShader.BlendDst := TGLBlendFunction(cbxGoochBlendDst.ItemIndex);
end;

procedure TFormShaderLab.cbxGoochBlendSrcChange(Sender: TObject);
begin
  //GoochShader.BlendSrc := TGLBlendFunction(cbxGoochBlendSrc.ItemIndex);
end;

procedure TFormShaderLab.cbxGootchBlendModeChange(Sender: TObject);
begin
  case cbxGootchBlendMode.ItemIndex of
    0: GoochShader.BlendingMode := bmxOpaque;
    1: GoochShader.BlendingMode := bmxTransparency;
    2: GoochShader.BlendingMode := bmxAdditive;
    3: GoochShader.BlendingMode := bmxAlphaTest50;
    4: GoochShader.BlendingMode := bmxAlphaTest100;
    5: GoochShader.BlendingMode := bmxModulate;
    6: GoochShader.BlendingMode := bmxDestColorOne;
    7: GoochShader.BlendingMode := bmxDestAlphaOne;
  end;
end;

procedure TFormShaderLab.cbxObjectsChange(Sender: TObject);
begin
  case cbxObjects.ItemIndex of
    0:
      begin
        GLSphere1.Visible := False;
        GLTorus1.Visible := False;
        FreeForm.LoadFromFile('models\suzanne-blender.obj');
        GlassShader.OwnerObject := FreeForm;
        FreeForm.Visible := true;

      end;
    1:
      begin
        GLSphere1.Visible := False;
        GLTorus1.Visible := False;
        FreeForm.LoadFromFile('models\torus-knot2.obj');
        GlassShader.OwnerObject := FreeForm;
        FreeForm.Visible := true;
      end;
    2:
      begin
        GLSphere1.Visible := False;
        GLTorus1.Visible := False;
        FreeForm.LoadFromFile('models\sputnik.obj');
        GlassShader.OwnerObject := FreeForm;
        FreeForm.Visible := true;
      end;
    3:
      begin
        GLSphere1.Visible := False;
        GLTorus1.Visible := False;
        FreeForm.LoadFromFile('models\rectangle_spiral.obj');
        GlassShader.OwnerObject := FreeForm;
        FreeForm.Visible := true;
      end;
    4:
      begin
        GLSphere1.Visible := False;
        GLTorus1.Visible := False;
        FreeForm.LoadFromFile('models\geode.obj');
        GlassShader.OwnerObject := FreeForm;
        FreeForm.Visible := true;
      end;
    5:
      begin
        GLSphere1.Visible := False;
        GLTorus1.Visible := False;
        FreeForm.LoadFromFile('models\syamil_19.obj');
        GlassShader.OwnerObject := FreeForm;
        FreeForm.Visible := true;
      end;
    6:
      begin
        GLSphere1.Visible := False;
        FreeForm.Visible := False;
        GLTorus1.Visible := true;
        GlassShader.OwnerObject := GLTorus1;
      end;
    7:
      begin
        GLTorus1.Visible := False;
        FreeForm.Visible := False;
        GLSphere1.Visible := true;
        GlassShader.OwnerObject := GLSphere1;

      end;
  end;
end;

procedure TFormShaderLab.chkBackgroundImgClick(Sender: TObject);
begin
  Button11.Enabled := chkBackgroundImg.Checked;
  ScreenBackGround.Visible := chkBackgroundImg.Checked;
  if ScreenBackGround.Visible then
    Viewer.Buffer.BackgroundColor := clBlack;

end;

procedure TFormShaderLab.chkErosionShaderClick(Sender: TObject);
begin
  ErosionShader.Enabled := chkErosionShader.Checked;
  if ErosionShader.Enabled then
  begin
    // MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader.Enabled := False;
    MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader := ErosionShader
  end
  else
    MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader := nil;
end;

procedure TFormShaderLab.chkFurRandomLengthClick(Sender: TObject);
begin
  FurShader.RandomFurLength := chkFurRandomLength.Checked;
end;

procedure TFormShaderLab.chkFurShaderClick(Sender: TObject);
begin
  FurShader.Enabled := chkFurShader.Checked;
  if FurShader.Enabled then
  begin
    // MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader.Enabled := False;
    MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader := FurShader
  end
  else
    MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader := nil;
  // GLsphere1.Material.LibMaterialName := 'ShaderMaterial'
  // else
  // GLsphere1.Material.LibMaterialName := 'MainTexture';
end;

procedure TFormShaderLab.chkLatticeShaderClick(Sender: TObject);
begin
  LatticeShader.Enabled := chkLatticeShader.Checked;
  if LatticeShader.Enabled then
  begin
    // MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader.Enabled := False;
    MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader := LatticeShader
  end
  else
    MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader := nil;
end;

procedure TFormShaderLab.chkSEMShaderClick(Sender: TObject);
begin
  SEMShader.Enabled := chkSEMShader.Checked;
  if SEMShader.Enabled then
  begin
    // MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader.Enabled := False;
    MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader := SEMShader
  end
  else
    MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader := nil;
end;

procedure TFormShaderLab.chkToonShaderClick(Sender: TObject);
begin
  ToonShader.Enabled := chkToonShader.Checked;
  if ToonShader.Enabled then
  begin
    // MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader.Enabled := False;
    MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader := ToonShader
  end
  else
    MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader := nil;
end;

procedure TFormShaderLab.chkVDShaderClick(Sender: TObject);
begin
  VertexDisplacementShader.Enabled := chkVDShader.Checked;
  if VertexDisplacementShader.Enabled then
  begin
    // MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader.Enabled := False;
    MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader :=
      VertexDisplacementShader
  end
  else
    MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader := nil;
end;

procedure TFormShaderLab.EditFloatKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key,['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', ',', '-']) then
    Key := #0;
end;

procedure TFormShaderLab.edtFurGravityXChange(Sender: TObject);
begin
  if (edtFurGravityX.Text <> '') and (edtFurGravityX.Text <> '-') and
    (edtFurGravityX.Text <> ',') and (edtFurGravityX.Text <> '.') then
    FurShader.Gravity.X := StrToFloat(edtFurGravityX.Text);
end;

procedure TFormShaderLab.edtFurGravityYChange(Sender: TObject);
begin
  if (edtFurGravityY.Text <> '') and (edtFurGravityY.Text <> '-') and
    (edtFurGravityY.Text <> ',') and (edtFurGravityY.Text <> '.') then
    FurShader.Gravity.Y := StrToFloat(edtFurGravityY.Text);
end;

procedure TFormShaderLab.edtFurGravityZChange(Sender: TObject);
begin
  if (edtFurGravityZ.Text <> '') and (edtFurGravityZ.Text <> '-') and
    (edtFurGravityZ.Text <> ',') and (edtFurGravityZ.Text <> '.') then
    FurShader.Gravity.Z := StrToFloat(edtFurGravityZ.Text);
end;

procedure TFormShaderLab.chkGlassShaderClick(Sender: TObject);
begin
  GlassShader.Enabled := chkGlassShader.Checked;
  if GlassShader.Enabled then
  begin
    // MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader.Enabled := False;
    MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader := GlassShader
  end
  else
    MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader := nil;
end;

procedure TFormShaderLab.chkGoochShaderClick(Sender: TObject);
begin
  GoochShader.Enabled := chkGoochShader.Checked;
  if GoochShader.Enabled then
  begin
    // MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader.Enabled := False;
    MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader := GoochShader
  end
  else
    MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader := nil;
end;

procedure TFormShaderLab.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  Screen.Cursor := crHourGlass;
  // FreeForm.IgnoreMissingTextures := True;

  FreeForm.LoadFromFile('models\suzanne-blender.obj');
  FreeForm.Visible := False;

  // Then load textures

  MaterialLibrary.LibMaterialByName('ShaderMaterial')
    .Material.Texture.Image.LoadFromFile('textures\bigtiger.jpg');
  MaterialLibrary.LibMaterialByName('BackgroundTex')
    .Material.Texture.Image.LoadFromFile('textures\randal.jpg');

  FreeForm.Material.MaterialLibrary := MaterialLibrary;
  FreeForm.Material.LibMaterialName := 'ShaderMaterial';
  GLTorus1.Material.MaterialLibrary := MaterialLibrary;
  GLTorus1.Material.LibMaterialName := 'ShaderMaterial';
  GLTorus1.Visible := False;
  GLSphere1.Material.MaterialLibrary := MaterialLibrary;
  GLSphere1.Material.LibMaterialName := 'ShaderMaterial';
  GLSphere1.Visible := False;

  MaterialLibrary.LibMaterialByName('MainTexture')
    .Material.Texture.Image.LoadFromFile('textures\bigtiger.jpg');

  // My Shader

  FurShader := TGLSLFurShader.Create(self);
  MaterialLibrary.LibMaterialByName('NoiseTexture')
    .Material.Texture.Image.LoadFromFile('textures\fur.tga');
  FurShader.MaterialLibrary := MaterialLibrary;
  FurShader.MainTextureName := 'MainTexture';
  FurShader.NoiseTextureName := 'NoiseTexture';
  FurShader.Gravity.X := 0;
  FurShader.Gravity.Y := -2.0;
  FurShader.Gravity.Z := 0;
  FurShader.Enabled := False;

  LatticeShader := TGLSLLatticeShader.Create(self);
  LatticeShader.MaterialLibrary := MaterialLibrary;
  LatticeShader.MainTextureName := 'MainTexture';
  LatticeShader.Enabled := False;

  ErosionShader := TGLSLSimpleErosionShader.Create(self);
  MaterialLibrary.LibMaterialByName('ErosionNoiseTexture')
    .Material.Texture.Image.LoadFromFile('textures\ErosionNoise.tga');
  MaterialLibrary.LibMaterialByName('ErosionMainTexture')
    .Material.Texture.Image.LoadFromFile
    ('textures\eroded_scratch_metal_texture.jpg');
  MaterialLibrary.LibMaterialByName('ErosionTexture')
    .Material.Texture.Image.LoadFromFile
    ('textures\rust_eroded_scratch_metal_texture.jpg');
  ErosionShader.MaterialLibrary := MaterialLibrary;
  ErosionShader.MainTextureName := 'ErosionMainTexture';
  ErosionShader.ErosionTextureName := 'ErosionTexture';
  ErosionShader.NoiseTextureName := 'ErosionNoiseTexture';
  ErosionShader.Enabled := False;

  IvoryShader := TGLSLIvoryShader.Create(self);
  IvoryShader.Enabled := False;

  GoochShader := TGLSLSimpleGoochShader.Create(self);
  GoochShader.Enabled := False;

  SEMShader := TGLSLSemShader.Create(self);
  MaterialLibrary.LibMaterialByName('MatCapTexture')
    .Material.Texture.Image.LoadFromFile('textures\metal_matcap.jpg');
  SEMShader.MaterialLibrary := MaterialLibrary;
  SEMShader.MainTextureName := 'MatCapTexture';
  SEMShader.Enabled := False;

  VertexDisplacementShader := TGLSLVertexDisplacementShader.Create(self);
  MaterialLibrary.LibMaterialByName('ExplosionTexture')
    .Material.Texture.Image.LoadFromFile('textures\FireGrade2.png');
  VertexDisplacementShader.MaterialLibrary := MaterialLibrary;
  VertexDisplacementShader.MainTextureName := 'ExplosionTexture';
  VertexDisplacementShader.Enabled := False;

  GlassShader := TGLSLGlassShader.Create(self);
  MaterialLibrary.LibMaterialByName('RefractMap')
    .Material.Texture.Image.LoadFromFile('textures\barts.jpg');

  // Don't load any texture in EnvMap let it empty. The EnvMap is auto generated
  // MaterialLibrary.LibMaterialByName('EnvMap').Material.Texture.Image.LoadFromFile('textures\metal_matcap.jpg');
  // But we need to make a ScreenShoot of the Scene Once before Enbale the GlassShader; otherwise an exception is raised
  // I don't say at this time how to correct this
  // capture and create material from framebuffer
  Viewer.Buffer.CopyToTexture(MaterialLibrary.LibMaterialByName('EnvMap')
    .Material.Texture);

  GlassShader.MaterialLibrary := MaterialLibrary;
  GlassShader.MainTextureName := 'EnvMap';
  GlassShader.RefractionTextureName := 'RefractMap';

  GlassShader.OwnerObject := FreeForm;
  // GlassShader.Viewer := Viewer;
  GlassShader.Enabled := False;

  ToonShader := TGLSLToonShader.Create(self);

  FreeForm.Visible := true;

  Screen.Cursor := crDefault;
  i := 0;
  j := 0;
  v := 0.0;
end;

procedure TFormShaderLab.FormDestroy(Sender: TObject);
begin
  Cadencer.Enabled := False;
  FurShader.Free;
  LatticeShader.Free;
  IvoryShader.Free;
  GoochShader.Free;
  ErosionShader.Free;
  SEMShader.Free;
  VertexDisplacementShader.Free;
  GlassShader.Free;
end;

procedure TFormShaderLab.FormResize(Sender: TObject);
begin
  ScreenBackGround.Width := Viewer.Width;
  ScreenBackGround.Height := Viewer.Height;

  ScreenBackGround.Position.X := Viewer.Width / 2;
  ScreenBackGround.Position.Y := Viewer.Height / 2;
end;

procedure TFormShaderLab.chkIvoryShaderClick(Sender: TObject);
begin
  IvoryShader.Enabled := chkIvoryShader.Checked;
  if IvoryShader.Enabled then
    MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader := IvoryShader
  else
    MaterialLibrary.LibMaterialByName('ShaderMaterial').Shader := nil;
end;

procedure TFormShaderLab.LightCube2Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  if chkLightmoving.Checked then
    LightCube2.MoveObjectAround(Camera.TargetObject, sin(newTime) * deltaTime *
      20, deltaTime * 10);
end;

procedure TFormShaderLab.LightCubeProgress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  if chkLightmoving.Checked then
    LightCube.MoveObjectAround(Camera.TargetObject, sin(newTime) * deltaTime *
      10, deltaTime * 20);
end;

procedure TFormShaderLab.Shape10MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    LatticeShader.DiffuseColor.Color := ConvertWinColor(ColorDialog.Color);
    Shape10.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Shape11MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    LatticeShader.AmbientColor.Color := ConvertWinColor(ColorDialog.Color);
    Shape11.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Shape12MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    LatticeShader.SpecularColor.Color := ConvertWinColor(ColorDialog.Color);
    Shape12.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Shape13MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    SEMShader.AmbientColor.Color := ConvertWinColor(ColorDialog.Color);
    Shape13.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Shape14MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    SEMShader.SpecularColor.Color := ConvertWinColor(ColorDialog.Color);
    Shape14.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Shape17MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    GlassShader.DiffuseColor.Color := ConvertWinColor(ColorDialog.Color);
    Shape17.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Shape18MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    ToonShader.HighlightColor.Color := ConvertWinColor(ColorDialog.Color);
    Shape18.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Shape19MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    ToonShader.MidColor.Color := ConvertWinColor(ColorDialog.Color);
    Shape19.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Shape1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    FurShader.ColorScale.Color := ConvertWinColor(ColorDialog.Color);
    Shape1.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Shape20MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    ToonShader.LightenShadowColor.Color := ConvertWinColor(ColorDialog.Color);
    Shape20.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Shape21MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    ToonShader.DarkenShadowrColor.Color := ConvertWinColor(ColorDialog.Color);
    Shape21.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Shape22MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    ToonShader.OutlinetColor.Color := ConvertWinColor(ColorDialog.Color);
    Shape22.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Shape2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    FurShader.Ambient.Color := ConvertWinColor(ColorDialog.Color);
    Shape2.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Shape3MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    GoochShader.DiffuseColor.Color := ConvertWinColor(ColorDialog.Color);
    Shape3.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Shape4MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    GoochShader.WarmColor.Color := ConvertWinColor(ColorDialog.Color);
    Shape4.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Shape5MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    GoochShader.CoolColor.Color := ConvertWinColor(ColorDialog.Color);
    Shape5.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Shape6MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    GoochShader.AmbientColor.Color := ConvertWinColor(ColorDialog.Color);
    Shape6.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Shape7MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    GoochShader.SpecularColor.Color := ConvertWinColor(ColorDialog.Color);
    Shape7.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Shape8MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    ErosionShader.AmbientColor.Color := ConvertWinColor(ColorDialog.Color);
    Shape8.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.Shape9MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog.Execute then
  begin
    ErosionShader.SpecularColor.Color := ConvertWinColor(ColorDialog.Color);
    Shape9.Brush.Color := ColorDialog.Color;
  end;
end;

procedure TFormShaderLab.tbErosionAmbientFChange(Sender: TObject);
begin
  ErosionShader.AmbientFactor := tbErosionAmbientF.Position / 100;
  lblErosionAmbientF.Caption := FloatToStrF(ErosionShader.AmbientFactor,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbErosionAnisoRChange(Sender: TObject);
begin
  ErosionShader.AnisotropicRoughness := tbErosionAnisoR.Position / 100;
  lblErosionAnisoR.Caption := FloatToStrF(ErosionShader.AnisotropicRoughness,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbErosionDiffuseFChange(Sender: TObject);
begin
  ErosionShader.DiffuseFactor := tbErosionDiffuseF.Position / 100;
  lblErosionDiffuseF.Caption := FloatToStrF(ErosionShader.DiffuseFactor,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbErosionFactorChange(Sender: TObject);
begin
  ErosionShader.ErosionFactor := tbErosionFactor.Position / 100;
  lblErosionFactor.Caption := FloatToStrF(ErosionShader.ErosionFactor,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbErosionIFactor1Change(Sender: TObject);
begin
  ErosionShader.IntensityFactor1 := tbErosionIFactor1.Position / 100;
  lblErosionIFactor1.Caption := FloatToStrF(ErosionShader.IntensityFactor1,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbErosionIFactor2Change(Sender: TObject);
begin
  ErosionShader.IntensityFactor2 := tbErosionIFactor2.Position / 100;
  lblerosionIFactor2.Caption := FloatToStrF(ErosionShader.IntensityFactor2,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tberosionScaleChange(Sender: TObject);
begin
  ErosionShader.ErosionScale := tberosionScale.Position / 100;
  lblErosionScale.Caption := FloatToStrF(ErosionShader.ErosionScale,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbErosionSpecularFChange(Sender: TObject);
begin
  ErosionShader.SpecularFactor := tbErosionSpecularF.Position / 100;
  lblErosionSpecularF.Caption := FloatToStrF(ErosionShader.SpecularFactor,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbErosionSpecularRChange(Sender: TObject);
begin
  ErosionShader.SpecularRoughness := tbErosionSpecularR.Position / 100;
  lblErosionSpecularR.Caption := FloatToStrF(ErosionShader.SpecularRoughness,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbFurDensityChange(Sender: TObject);
begin
  FurShader.FurDensity := tbFurDensity.Position / 100;
  lblFurDensity.Caption := FloatToStrF(FurShader.FurDensity, ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbFurLengthChange(Sender: TObject);
begin
  FurShader.FurLength := tbFurLength.Position / 100;
  lblFurLength.Caption := FloatToStrF(FurShader.FurLength, ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbFurLightPowerChange(Sender: TObject);
begin
  FurShader.LightIntensity := tbFurLightPower.Position / 100;
  lblFurLightPower.Caption := FloatToStrF(FurShader.LightIntensity,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbFurMaxLengthChange(Sender: TObject);
begin
  FurShader.MaxFurLength := tbFurMaxLength.Position / 100;
  lblFurMaxLength.Caption := FloatToStrF(FurShader.MaxFurLength, ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbFurPassCountChange(Sender: TObject);
begin
  FurShader.PassCount := tbFurPassCount.Position;
  lblFurPassCount.Caption := FloatToStrF(FurShader.PassCount, ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbGlassAlphaChange(Sender: TObject);
begin
  GlassShader.Alpha := tbGlassAlpha.Position / 100;
  lblGlassAlpha.Caption := FloatToStrF(GlassShader.Alpha, ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbGlassDepthChange(Sender: TObject);
begin
  GlassShader.Depth := tbGlassDepth.Position / 100;
  lblGlassDepth.Caption := FloatToStrF(GlassShader.Depth, ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbGlassMixChange(Sender: TObject);
begin
  GlassShader.Mix := tbGlassMix.Position / 100;
  lblGlassMix.Caption := FloatToStrF(GlassShader.Mix, ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbGoochAFactorChange(Sender: TObject);
begin
  GoochShader.AmbientFactor := tbGoochAFactor.Position / 100;
  lblGoochAFactor.Caption := FloatToStrF(GoochShader.AmbientFactor,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbGoochAlphaChange(Sender: TObject);
begin
  GoochShader.DiffuseColor.Alpha := tbGoochAlpha.Position / 100;
  lblGoochAlpha.Caption := FloatToStrF(GoochShader.DiffuseColor.Alpha,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbGoochCFactorChange(Sender: TObject);
begin
  GoochShader.CoolFactor := tbGoochCFactor.Position / 100;
  lblGoochCFactor.Caption := FloatToStrF(GoochShader.CoolFactor, ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbGoochDFactorChange(Sender: TObject);
begin
  GoochShader.DiffuseFactor := tbGoochDFactor.Position / 100;
  lblGoochDFactor.Caption := FloatToStrF(GoochShader.DiffuseFactor,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbGoochSFactorChange(Sender: TObject);
begin
  GoochShader.SpecularFactor := tbGoochSFactor.Position / 100;
  lblGoochSFactor.Caption := FloatToStrF(GoochShader.SpecularFactor,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbGoochWFactorChange(Sender: TObject);
begin
  GoochShader.WarmFactor := tbGoochWFactor.Position / 100;
  lblGoochWFactor.Caption := FloatToStrF(GoochShader.WarmFactor, ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbLatticeLightPowerChange(Sender: TObject);
begin
  LatticeShader.LightPower := tbLatticeLightPower.Position / 100;
  lblLatticeLightPower.Caption := FloatToStrF(LatticeShader.LightPower,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbLatticeScaleXChange(Sender: TObject);
begin
  LatticeShader.LatticeScale.X := tbLatticeScaleX.Position;
  lblLatticeScaleX.Caption := FloatToStrF(LatticeShader.LatticeScale.X,
    ffFixed, 5, 0);
end;

procedure TFormShaderLab.tbLatticeScaleYChange(Sender: TObject);
begin
  LatticeShader.LatticeScale.Y := tbLatticeScaleY.Position;
  lblLatticeScaleY.Caption := FloatToStrF(LatticeShader.LatticeScale.Y,
    ffFixed, 5, 0);
end;

procedure TFormShaderLab.tbLatticeSpecularPowerChange(Sender: TObject);
begin
  LatticeShader.SpecularPower := tbLatticeSpecularPower.Position / 100;
  lblLatticeSpecularPower.Caption := FloatToStrF(LatticeShader.SpecularPower,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbLatticeThresholdXChange(Sender: TObject);
begin
  LatticeShader.LatticeThreshold.X := tbLatticeThresholdX.Position / 100;
  lblLatticeThresholdX.Caption := FloatToStrF(LatticeShader.LatticeThreshold.X,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbLatticeThresholdYChange(Sender: TObject);
begin
  LatticeShader.LatticeThreshold.Y := tbLatticeThresholdY.Position / 100;
  lblLatticeThresholdY.Caption := FloatToStrF(LatticeShader.LatticeThreshold.Y,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbSemAmbientFChange(Sender: TObject);
begin
  SEMShader.AmbientFactor := tbSemAmbientF.Position / 100;
  lblSemAmbientF.Caption := FloatToStrF(SEMShader.AmbientFactor, ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbSemDiffuseFChange(Sender: TObject);
begin
  SEMShader.DiffuseFactor := tbSemDiffuseF.Position / 100;
  lblSemDiffuseF.Caption := FloatToStrF(SEMShader.DiffuseFactor, ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbSemSpecularFChange(Sender: TObject);
begin
  SEMShader.SpecularFactor := tbSemSpecularF.Position / 100;
  lblSemSpecularF.Caption := FloatToStrF(SEMShader.SpecularFactor,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbToonHighlightSizeChange(Sender: TObject);
begin
  ToonShader.HighlightSize := tbToonHighlightSize.Position / 100;
  lblToonHighlightSize.Caption := FloatToStrF(ToonShader.HighlightSize,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbToonMidSizeChange(Sender: TObject);
begin
  ToonShader.MidSize := tbToonMidSize.Position / 100;
  lblToonMidSize.Caption := FloatToStrF(ToonShader.MidSize, ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbToonOutlineWidthChange(Sender: TObject);
begin
  ToonShader.OutlineWidth := tbToonOutlineWidth.Position / 100;
  lblToonOutlineWidth.Caption := FloatToStrF(ToonShader.OutlineWidth,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbToonShadowSizeChange(Sender: TObject);
begin
  ToonShader.ShadowSize := tbToonShadowSize.Position / 100;
  lblToonShadowSize.Caption := FloatToStrF(ToonShader.ShadowSize,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbVDAmbientFChange(Sender: TObject);
begin
  VertexDisplacementShader.AmbientFactor := tbVDAmbientF.Position / 100;
  lblVDAmbientF.Caption := FloatToStrF(VertexDisplacementShader.AmbientFactor,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbVDDiffuseFChange(Sender: TObject);
begin
  VertexDisplacementShader.DiffuseFactor := tbVDDiffuseF.Position / 100;
  lblVDDiffuseF.Caption := FloatToStrF(VertexDisplacementShader.DiffuseFactor,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbVDDispScaleChange(Sender: TObject);
begin
  VertexDisplacementShader.DisplacementScale := tbVDDispScale.Position / 100;
  lblVDDispScale.Caption :=
    FloatToStrF(VertexDisplacementShader.DisplacementScale, ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbVDNoiseChange(Sender: TObject);
begin
  VertexDisplacementShader.NoiseFactor := tbVDNoise.Position / 100;
  lblVDNoise.Caption := FloatToStrF(VertexDisplacementShader.NoiseFactor,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbVDNScaleChange(Sender: TObject);
begin
  VertexDisplacementShader.NoiseScale := tbVDNScale.Position / 100;
  lblVDNScale.Caption := FloatToStrF(VertexDisplacementShader.NoiseScale,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbVDPeriodChange(Sender: TObject);
begin
  VertexDisplacementShader.NoisePeriod := tbVDPeriod.Position / 100;
  lblVDPeriod.Caption := FloatToStrF(VertexDisplacementShader.NoisePeriod,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbVDSpecularFChange(Sender: TObject);
begin
  VertexDisplacementShader.SpecularFactor := tbVDSpecularF.Position / 100;
  lblVDSpecularF.Caption := FloatToStrF(VertexDisplacementShader.SpecularFactor,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbVDTimeFChange(Sender: TObject);
begin
  VertexDisplacementShader.TimeFactor := tbVDTimeF.Position / 100;
  lblVDTimeF.Caption := FloatToStrF(VertexDisplacementShader.TimeFactor,
    ffFixed, 5, 3);
end;

procedure TFormShaderLab.tbVDTurbChange(Sender: TObject);
begin
  VertexDisplacementShader.TurbulenceFactor := tbVDTurb.Position / 100;
  lblVDTurb.Caption := FloatToStrF(VertexDisplacementShader.TurbulenceFactor,
    ffFixed, 5, 3);
end;

end.
