unit fBlurD;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Imaging.Jpeg,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  GLS.SceneViewer,
  GLS.Scene,
  GLS.Objects,
  GLS.Texture,
  GLS.HUDObjects,
  GLS.CompositeImage,
  GLS.Cadencer,
  GLS.Blur,

  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  Stage.Utils;

type
  TFormBlur = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCube1: TGLCube;
    GLLightSource1: TGLLightSource;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLSphere1: TGLSphere;
    Panel1: TPanel;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    ComboBox2: TComboBox;
    Timer1: TTimer;
    GLDummyCube1: TGLDummyCube;
    LabelFPS: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure ComboBox1Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    oldx, oldy: integer;
  public
    B: TGLBlur;
  end;

var
  FormBlur: TFormBlur;

implementation

{$R *.dfm}

procedure TFormBlur.FormCreate(Sender: TObject);
var
  I : Integer;

begin
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path  + '\texture');
  GLMaterialLibrary1.TexturePaths := GetCurrentDir();
  // Add GLS.Blur to scene
  B := TGLBlur.Create(self);
  GLCube1.AddChild(B);
  B.TargetObject := GLCube1;
  B.RenderWidth := 256;
  B.RenderHeight := 256;
  // Load texture for objects
  GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile('marbletiles.jpg');
  ComboBox1.ItemIndex := 2;
  ComboBox1Click(self);

end;

procedure TFormBlur.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLCube1.Turn(deltatime * 10);
  GLSphere1.Turn(deltatime * 50);
end;

procedure TFormBlur.ComboBox1Click(Sender: TObject);
begin
  B.Preset := TGLBlurPreset(ComboBox1.itemIndex);
end;

procedure TFormBlur.ComboBox2Change(Sender: TObject);
begin
  B.RenderWidth := StrToInt(ComboBox2.Items[ComboBox2.ItemIndex]);
  B.RenderHeight := B.RenderWidth;
end;

procedure TFormBlur.Timer1Timer(Sender: TObject);
begin
  LabelFPS.Caption := FloatToStr(Trunc(GLSceneViewer1.FramesPerSecond))+ ' FPS';
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormBlur.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget(0.2 * (oldy - y), 0.2 * (oldx - x));
  end;
  oldx := x;
  oldy := y;
end;

end.

