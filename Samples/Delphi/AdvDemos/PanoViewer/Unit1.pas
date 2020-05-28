unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ExtDlgs,
  Vcl.Imaging.Jpeg,
   
  GLScene, GLObjects, GLTexture, GLKeyBoard, GLCadencer, GLWin32Viewer,
  GLMaterial, GLCoordinates, GLCrossPlatform, GLBaseClasses, GLUtils,
  GLVectorGeometry;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    Panel1: TPanel;
    GLCamera1: TGLCamera;
    BtnLoad: TButton;
    TrackBar1: TTrackBar;
    LabelYaw: TLabel;
    LabelPitch: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    Label1: TLabel;
    Sphere1: TGLSphere;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Label2: TLabel;
    GLCadencer1: TGLCadencer;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure BtnLoadClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
  private
     
    mx, my: Integer;
    pitch, yaw: single; // in degree
    procedure PanCameraAround(dx, dy: single);
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  dx, dy, f: single;
begin
  if Shift = [ssLeft] then
  begin
    f := 0.2 * 40 / GLCamera1.FocalLength;
    dx := (X - mx) * f;
    dy := (Y - my) * f;
    PanCameraAround(dx, dy);
  end;
  mx := X;
  my := Y;
end;

procedure TForm1.BtnLoadClick(Sender: TObject);
begin
  with OpenPictureDialog1 do
    if Execute then
      GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile
        (FileName);
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  GLCamera1.FocalLength := TrackBar1.Position;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  TrackBar1.Position := TrackBar1.Position + Round(2 * WheelDelta / 120);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
const
  step_size = 20;
var
  delta: single;
  dx, dy: single;
begin
  delta := step_size * 40 / GLCamera1.FocalLength * deltaTime;
  dx := 0;
  dy := 0;
  if IsKeyDown(VK_LEFT) then
    dx := dx + delta;
  if IsKeyDown(VK_UP) then
    dy := dy + delta;
  if IsKeyDown(VK_RIGHT) then
    dx := dx - delta;
  if IsKeyDown(VK_DOWN) then
    dy := dy - delta;
  PanCameraAround(dx, dy);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenPictureDialog1.InitialDir := ExtractFilePath(ParamStr(0));
  OpenPictureDialog1.FileName := 'sejourstmathieu2048.jpg';
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Key := 0; // all keys handled by Form1
end;

procedure TForm1.PanCameraAround(dx, dy: single);
begin
  pitch := pitch + dy;
  yaw := yaw - dx;

  if pitch > 90 then
    pitch := 90;
  if pitch < -90 then
    pitch := -90;
  if yaw > 360 then
    yaw := yaw - 360;
  if yaw < 0 then
    yaw := yaw + 360;

  GLCamera1.Up.SetVector(0, 1, 0);
  GLCamera1.Direction.SetVector(sin(DegToRad(yaw)), sin(DegToRad(pitch)),
    -cos(DegToRad(yaw)));

  LabelPitch.caption := format('Pitch: %3f', [pitch]);
  LabelYaw.caption := format('Yaw: %3f', [yaw]);
end;

end.
