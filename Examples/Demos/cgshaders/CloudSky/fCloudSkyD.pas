unit fCloudSkyD;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
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

  Stage.VectorGeometry,
  Stage.Keyboard,
  Stage.Utils,

  GLS.Scene,
  GLS.Objects,
  GLS.SceneViewer,
  GLS.AsyncTimer,
  GLS.Cadencer,
  GLS.SkyDome,
  GLS.Texture,
  GLS.FileTGA,
  GLS.XCollection,
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.SimpleNavigation,

  GLS.CgShader;

type

  TMainForm = class(TForm)
    GLScene: TGLScene;
    GLSV: TGLSceneViewer;
    dc_cam: TGLDummyCube;
    cam: TGLCamera;
    Cadencer: TGLCadencer;
    Timer: TGLAsyncTimer;
    SbBackground: TGLSkyBox;
    MatLib: TGLMaterialLibrary;
    CgBackground: TCgShader;
    CgClouds: TCgShader;
    SbClouds: TGLSkyBox;
    Moons: TGLDummyCube;
    sprSecunda: TGLSprite;
    sprMasser: TGLSprite;
    CgMasser: TCgShader;
    CgSecunda: TCgShader;
    CgSun: TCgShader;
    sprSun: TGLSprite;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    GLSimpleNavigation1: TGLSimpleNavigation;
    PanelFPS: TPanel;
    procedure CgCloudsApplyVP(CgProgram: TCgProgram; Sender: TObject);
    procedure CgSunUnApplyFP(CgProgram: TCgProgram);
    procedure CgSunApplyFP(CgProgram: TCgProgram; Sender: TObject);
    procedure CgSecundaUnApplyFP(CgProgram: TCgProgram);
    procedure CgSecundaApplyFP(CgProgram: TCgProgram; Sender: TObject);
    procedure CgMasserUnApplyFP(CgProgram: TCgProgram);
    procedure CgMasserApplyFP(CgProgram: TCgProgram; Sender: TObject);
    procedure CgCloudsUnApplyFP(CgProgram: TCgProgram);
    procedure CgCloudsApplyFP(CgProgram: TCgProgram; Sender: TObject);
    procedure CgBackgroundUnApplyFP(CgProgram: TCgProgram);
    procedure CgBackgroundApplyFP(CgProgram: TCgProgram; Sender: TObject);
    procedure CadencerProgress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure GLSVMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSVMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TimerTimer(Sender: TObject);
  private
    mx, my: Integer;
    bg_w1, bg_w2, c_w1, c_w2, m_w, a_w: single;
    DayMode, WeatherMode: byte;
    procedure HandleKeys;
    procedure AssignMaterials;
    procedure CreateMaterials;
  public
  end;

var
  MainForm: TMainForm;
  dirSelf, dirTextures, dirShaders: TFileName;

const
  Coeff = 0.1;

// ---------------------------------------------------------
implementation

{$R *.dfm}

// ---------------------------FormCreate--------------------
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // GetDir(0, dirSelf);
  dirSelf := GetCurrentAssetPath();  // current assets
  dirTextures := dirSelf + '\texture\';
  dirShaders := dirSelf + '\shader\';

  CreateMaterials;
  AssignMaterials;
  bg_w1 := 1;
  bg_w2 := 0;
  c_w1 := 1;
  c_w2 := 0;
  m_w := 0; // moons
  a_w := 0; // ambient

  WeatherMode := 0;
  ClientWidth := 1024;
  ClientHeight := 712;
  Position := poScreenCenter;
  GLSV.Align := alClient;
  Timer.Enabled := True;
end;

// ---------------------------CreateMaterials---------------
procedure TMainForm.CreateMaterials;
begin
  CgBackground.FragmentProgram.LoadFromFile
    (dirShaders + 'fragment_bkground.cg');
  CgClouds.FragmentProgram.LoadFromFile(dirShaders + 'fragment_clouds.cg');
  CgMasser.FragmentProgram.LoadFromFile(dirShaders + 'fragment_moon.cg');
  CgSecunda.FragmentProgram.LoadFromFile(dirShaders + 'fragment_moon.cg');
  CgSun.FragmentProgram.LoadFromFile(dirShaders + 'fragment_moon.cg');

  // day background
  MatLib.AddTextureMaterial('day', dirTextures + 'tx_day.tga');

  // night background
  MatLib.AddTextureMaterial('night', dirTextures + 'tx_night.tga');

  // main skybox material
  with MatLib.Materials.Add do
  begin
    Name := 'background';
    Shader := CgBackground;
  end;

  // clouds
  MatLib.AddTextureMaterial('clouds_clear', dirTextures + 'tx_sky_clear.tga');
  MatLib.AddTextureMaterial('clouds_cloudy', dirTextures + 'tx_sky_cloudy.tga');

  // main clouds material
  with MatLib.Materials.Add do
  begin
    Name := 'clouds';
    Material.BlendingMode := bmTransparency;
    Shader := CgClouds;
  end;

  // moons
  MatLib.AddTextureMaterial('masser', dirTextures + 'tx_masser_three_wan.tga');
  MatLib.AddTextureMaterial('secunda',
    dirTextures + 'tx_secunda_three_wan.tga');
  MatLib.AddTextureMaterial('sun', dirTextures + 'tx_sun.tga');

  with MatLib.Materials.Add do
  begin
    Name := 'moon_masser';
    Material.BlendingMode := bmTransparency;
    Shader := CgMasser;
  end;

  with MatLib.Materials.Add do
  begin
    Name := 'moon_secunda';
    Material.BlendingMode := bmTransparency;
    Shader := CgSecunda;
  end;

  with MatLib.Materials.Add do
  begin
    Name := 'moon_sun';
    Material.BlendingMode := bmTransparency;
    Shader := CgSun;
  end;
end;

// ---------------------------AssignMaterials---------------
procedure TMainForm.AssignMaterials;
begin
  with SbBackground do
  begin
    MaterialLibrary := MatLib;
    MatNameTop := 'background';
    MatNameRight := 'background';
    MatNameFront := 'background';
    MatNameLeft := 'background';
    MatNameBack := 'background';
    MatNameBottom := 'background';
  end;

  with SbClouds do
  begin
    MaterialLibrary := MatLib;
    MatNameClouds := 'clouds';
  end;

  with sprMasser.Material do
  begin
    MaterialLibrary := MatLib;
    LibMaterialName := 'moon_masser';
  end;

  with sprSecunda.Material do
  begin
    MaterialLibrary := MatLib;
    LibMaterialName := 'moon_secunda';
  end;

  with sprSun.Material do
  begin
    MaterialLibrary := MatLib;
    LibMaterialName := 'moon_sun';
  end;
end;

// ---------------------------GLSVMouseDown-----------------
procedure TMainForm.GLSVMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

// ---------------------------GLSVMouseMove-----------------
procedure TMainForm.GLSVMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift = [ssRight] then
    cam.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;

// ---------------------------FormMouseWheel----------------
procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  cam.AdjustDistanceToTarget(Power(1.1, WheelDelta / 100));
end;

// ---------------------------TimerTimer--------------------
procedure TMainForm.TimerTimer(Sender: TObject);
begin
  PanelFPS.Caption := GLSV.FramesPerSecondText(2);
  GLSV.ResetPerformanceMonitor;
end;

// ---------------------------CadencerProgress--------------
procedure TMainForm.CadencerProgress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  HandleKeys;
  case DayMode of
    1: // day
      begin
        bg_w1 := bg_w1 + deltaTime * Coeff;
        if bg_w1 > 1 then
          bg_w1 := 1;
        bg_w2 := bg_w2 - deltaTime * Coeff;
        if bg_w2 < 0 then
          bg_w2 := 0;
        m_w := m_w - deltaTime * Coeff;
        if m_w < 0 then
          m_w := 0;
      end;
    2: // night
      begin
        bg_w1 := bg_w1 - deltaTime * Coeff;
        if bg_w1 < 0 then
          bg_w1 := 0;
        bg_w2 := bg_w2 + deltaTime * Coeff;
        if bg_w2 > 1 then
          bg_w2 := 1;
        m_w := m_w + deltaTime * Coeff;
        if m_w > 1 then
          m_w := 1;

        if bg_w2 > 0.3 then
          a_w := a_w - deltaTime * Coeff
        else
          a_w := a_w + deltaTime * Coeff;

        if a_w < 0 then
          a_w := 0.01;
      end;
  end;

  case WeatherMode of
    1:
      begin
        c_w1 := c_w1 - deltaTime * Coeff;
        if c_w1 < 0 then
          c_w1 := 0;
        c_w2 := c_w2 + deltaTime * Coeff;
        if c_w2 > 1 then
          c_w2 := 1;
      end;
    0:
      begin
        c_w1 := c_w1 + deltaTime * Coeff;
        if c_w1 > 1 then
          c_w1 := 1;
        c_w2 := c_w2 - deltaTime * Coeff;
        if c_w2 < 0 then
          c_w2 := 0;
      end;

  end;
  GLSV.Invalidate;
end;

// ---------------------------HandleKeys--------------------
procedure TMainForm.HandleKeys;
begin
  if IsKeyDown('c') then           // weather1
    WeatherMode := 0
  else if IsKeyDown('s') then      // weather2
    WeatherMode := 1
  else if IsKeyDown('n') then      // night
    DayMode := 2
  else if IsKeyDown('d') then      // day
    DayMode := 1
  else

end;

// ---------------------------CgShaderApplyFP---------------
procedure TMainForm.CgBackgroundApplyFP(CgProgram: TCgProgram; Sender: TObject);
var
  w1, w2: single;
begin
  with CgProgram.ParamByName('channel1') do
  begin
    SetAsTexture2d(MatLib.LibMaterialByName('day').Material.Texture.Handle);
    EnableTexture;
  end;
  with CgProgram.ParamByName('channel2') do
  begin
    SetAsTexture2d(MatLib.LibMaterialByName('night')
      .Material.Texture.Handle);
    EnableTexture;
  end;
  CgProgram.ParamByName('w1').SetAsScalar(bg_w1);
  CgProgram.ParamByName('w2').SetAsScalar(bg_w2);
end;

// ---------------------------CgShaderUnApplyFP-------------
procedure TMainForm.CgBackgroundUnApplyFP(CgProgram: TCgProgram);
begin
  CgProgram.ParamByName('channel1').DisableTexture;
  CgProgram.ParamByName('channel2').DisableTexture;
end;

// ---------------------------CgShaderCloudsApplyFP---------
procedure TMainForm.CgCloudsApplyFP(CgProgram: TCgProgram; Sender: TObject);
begin
  with CgProgram.ParamByName('channel1') do
  begin
    SetAsTexture2d(MatLib.LibMaterialByName('clouds_clear')
      .Material.Texture.Handle);
    EnableTexture;
  end;
  with CgProgram.ParamByName('channel2') do
  begin
    SetAsTexture2d(MatLib.LibMaterialByName('clouds_cloudy')
      .Material.Texture.Handle);
    EnableTexture;
  end;
  // if a_w < 0.1 then
  // a:= 0.5;
  CgProgram.ParamByName('w1').SetAsScalar(c_w1);
  CgProgram.ParamByName('w2').SetAsScalar(c_w2 - bg_w2 / 2);
  if a_w > 0 then
  begin
    CgProgram.ParamByName('redoffset1').SetAsScalar(a_w);
    CgProgram.ParamByName('redoffset2').SetAsScalar(a_w);
  end;
end;

// ---------------------------CgCloudsApplyVP---------------
procedure TMainForm.CgCloudsApplyVP(CgProgram: TCgProgram; Sender: TObject);
begin
  //
end;

// ---------------------------CgShaderCloudsUnApplyFP-------
procedure TMainForm.CgCloudsUnApplyFP(CgProgram: TCgProgram);
begin
  CgProgram.ParamByName('channel1').DisableTexture;
  CgProgram.ParamByName('channel2').DisableTexture;
end;

// ---------------------------CgMoonApplyFP-----------------
procedure TMainForm.CgMasserApplyFP(CgProgram: TCgProgram; Sender: TObject);
begin
  with CgProgram.ParamByName('channel1') do
  begin
    SetAsTexture2d(MatLib.LibMaterialByName('masser').Material.Texture.Handle);
    EnableTexture;
  end;
  CgProgram.ParamByName('w').SetAsScalar(m_w);
end;

// ---------------------------CgMoonUnApplyFP---------------
procedure TMainForm.CgMasserUnApplyFP(CgProgram: TCgProgram);
begin
  CgProgram.ParamByName('channel1').DisableTexture;
end;

// ---------------------------CgSecundaApplyFP--------------
procedure TMainForm.CgSecundaApplyFP(CgProgram: TCgProgram; Sender: TObject);
begin
  with CgProgram.ParamByName('channel1') do
  begin
    SetAsTexture2d(MatLib.LibMaterialByName('secunda').Material.Texture.Handle);
    EnableTexture;
  end;
  CgProgram.ParamByName('w').SetAsScalar(m_w);
end;

// ---------------------------CgSecundaUnApplyFP------------
procedure TMainForm.CgSecundaUnApplyFP(CgProgram: TCgProgram);
begin
  CgProgram.ParamByName('channel1').DisableTexture;
end;

// ---------------------------CgSunApplyFP------------------
procedure TMainForm.CgSunApplyFP(CgProgram: TCgProgram; Sender: TObject);
begin
  with CgProgram.ParamByName('channel1') do
  begin
    SetAsTexture2d(MatLib.LibMaterialByName('sun').Material.Texture.Handle);
    EnableTexture;
  end;
  CgProgram.ParamByName('w').SetAsScalar(1.2 * (1 - m_w));
end;

// ---------------------------CgSunUnApplyFP----------------
procedure TMainForm.CgSunUnApplyFP(CgProgram: TCgProgram);
begin
  CgProgram.ParamByName('channel1').DisableTexture;
end;

end.
