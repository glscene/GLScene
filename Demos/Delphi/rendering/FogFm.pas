unit FogFm;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.UITypes,
  System.Types,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,

  GLS.Scene,
  GLS.VectorTypes,
  GLS.Objects,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.Texture,

  GLS.Material,
  GLS.Coordinates,
  GLS.Utils,
  GLS.BaseClasses;

type
  TFormFog = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    ColorDialog1: TColorDialog;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLLightSource1: TGLLightSource;
    Panel1: TPanel;
    CBFogEnable: TCheckBox;
    LFogStart: TLabel;
    LFogEnd: TLabel;
    EFogStart: TEdit;
    EFogEnd: TEdit;
    RGFogDistance: TRadioGroup;
    RGFogMode: TRadioGroup;
    GBTexture: TGroupBox;
    CBTextureEnabled: TCheckBox;
    CBTextureIgnoreFog: TCheckBox;
    LFogColor: TLabel;
    SFogColor: TShape;
    CBApplyToBackground: TCheckBox;
    LFogDensity: TLabel;
    EFogDensity: TEdit;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure CBFogEnableClick(Sender: TObject);
    procedure SEFogStartChange(Sender: TObject);
    procedure SFogColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RGFogModeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBApplyToBackgroundClick(Sender: TObject);
    procedure CBTextureEnabledClick(Sender: TObject);
    procedure CBTextureIgnoreFogClick(Sender: TObject);
    procedure EFogStartChange(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    MX: Integer;
    MY: Integer;
    procedure ApplyFogSettings;
  public
  end;

var
  FormFog: TFormFog;

implementation

{$R *.dfm}

// applyfogsettings
//
procedure TFormFog.ApplyFogSettings;
begin
  with GLSceneViewer1.Buffer.FogEnvironment do
  begin
    FogMode := TFogMode(RGFogMode.ItemIndex);
    FogDistance := TFogDistance(RGFogDistance.ItemIndex);
    FogColor.AsWinColor := SFogColor.Brush.Color;
    FogColor.Alpha := StrToInt(EFogDensity.Text) / 1000;
    if CBApplyToBackground.Checked then
      GLSceneViewer1.Buffer.BackgroundColor := SFogColor.Brush.Color;
    FogStart := StrToInt(EFogStart.Text);
    FogEnd := StrToInt(EFogEnd.Text);
  end;
  GLSceneViewer1.Buffer.FogEnable := CBFogEnable.Checked;
end;

// glsceneviewer1mousedown
//
procedure TFormFog.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MX := X;
  MY := Y;
end;

// glsceneviewer1mousemove
//
procedure TFormFog.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift <> [] then
    GLSceneViewer1.Camera.MoveAroundTarget(MY - Y, MX - X);
  MX := X;
  MY := Y;
end;

// cbfogenableclick
//
procedure TFormFog.CBFogEnableClick(Sender: TObject);
begin
  ApplyFogSettings;
end;

// sestartfogchange
//
procedure TFormFog.SEFogStartChange(Sender: TObject);
begin
  try
    ApplyFogSettings;
  except
  end;
end;

// sgfogcolormousedown
//
procedure TFormFog.SFogColorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog1.Execute then
  begin
    SFogColor.Brush.Color := ColorDialog1.Color;
    ApplyFogSettings;
  end;
end;

// rgfogmodeclick
//
procedure TFormFog.RGFogModeClick(Sender: TObject);
begin
  ApplyFogSettings;
end;

// formcreate
//
procedure TFormFog.FormCreate(Sender: TObject);
const
  cSpacing = 2;
  cEdgeLength = 0.7;
  cNb = 5;

var
  X: Integer;
  Y: Integer;
  Z: Integer;
  Cube: TGLCube;

begin
  SetGLSceneMediaDir();
  GLMaterialLibrary1.AddTextureMaterial('glscene', 'glscene.bmp');
  for X := -cNb to cNb do
    for Y := -cNb to cNb do
      for Z := -cNb to cNb do
        if (X and Y and Z) <> 0 then
        begin
          Cube := TGLCube(GLDummyCube1.AddNewChild(TGLCube));
          Cube.Material.MaterialLibrary := GLMaterialLibrary1;
          Cube.Material.LibMaterialName := 'glscene';
          Cube.Position.SetPoint(X * cSpacing, Y * cSpacing, Z * cSpacing);
          Cube.CubeWidth := cEdgeLength;
          Cube.CubeHeight := cEdgeLength;
          Cube.CubeDepth := cEdgeLength;
        end;
  RGFogModeClick(Self);
end;

procedure TFormFog.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
  Handled := true
end;

// cbapplytobackgroundclick
//
procedure TFormFog.CBApplyToBackgroundClick(Sender: TObject);
begin
  ApplyFogSettings;
end;

// cbtextureenabledclick
//
procedure TFormFog.CBTextureEnabledClick(Sender: TObject);
begin
  GLMaterialLibrary1.Materials[0].Material.Texture.Enabled :=
    CBTextureEnabled.Checked;
end;

// cbtextureignorefogclick
//
procedure TFormFog.CBTextureIgnoreFogClick(Sender: TObject);
begin
  if CBTextureIgnoreFog.Checked then
    GLMaterialLibrary1.Materials[0].Material.MaterialOptions :=
      GLMaterialLibrary1.Materials[0].Material.MaterialOptions + [moIgnoreFog]
  else
    GLMaterialLibrary1.Materials[0].Material.MaterialOptions :=
      GLMaterialLibrary1.Materials[0].Material.MaterialOptions - [moIgnoreFog];
end;

procedure TFormFog.EFogStartChange(Sender: TObject);
begin
  if TEdit(Sender).Text <> '' then
    ApplyFogSettings;
end;

end.
