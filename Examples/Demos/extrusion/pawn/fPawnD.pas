unit fPawnD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.JPeg,

  GLS.Scene,
  GLS.Objects,
  GLS.Texture,
  GLS.Extrusion,
  GLS.SceneViewer,
  GLS.Coordinates,
  GLS.BaseClasses,
  Stage.Utils;

type
  TFormPawn = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    RotationSolid1: TGLRevolutionSolid;
    Timer1: TTimer;
    Panel1: TPanel;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Label3: TLabel;
    TrackBar2: TTrackBar;
    Label4: TLabel;
    TrackBar3: TTrackBar;
    Label2: TLabel;
    TrackBar1: TTrackBar;
    LabelTri: TLabel;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
  private
    PathToData: TFileName;
  public
    mx, my: Integer;
  end;

var
  FormPawn: TFormPawn;

implementation

{$R *.DFM}

procedure TFormPawn.FormCreate(Sender: TObject);
begin
  PathToData := GetCurrentAssetPath();
  SetCurrentDir(PathToData + '\texture');
  RotationSolid1.Material.Texture.Image.LoadFromFile('ashwood.jpg');
end;

procedure TFormPawn.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    RotationSolid1.SplineMode := lsmCubicSpline
  else
    RotationSolid1.SplineMode := lsmLines;
end;

procedure TFormPawn.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then
    RotationSolid1.Normals := nsSmooth
  else
    RotationSolid1.Normals := nsFlat;
end;

procedure TFormPawn.CheckBox3Click(Sender: TObject);
begin
  RotationSolid1.Material.Texture.Disabled := not CheckBox3.Checked;
end;

procedure TFormPawn.CheckBox4Click(Sender: TObject);
begin
  if CheckBox4.Checked then
    RotationSolid1.Material.Texture.TextureMode := tmModulate
  else
    RotationSolid1.Material.Texture.TextureMode := tmDecal;
end;

procedure TFormPawn.TrackBar1Change(Sender: TObject);
begin
  RotationSolid1.StopAngle := TrackBar1.Position;
  if TrackBar1.Position = 360 then
    RotationSolid1.Parts := RotationSolid1.Parts - [rspStartPolygon, rspStopPolygon]
  else
    RotationSolid1.Parts := RotationSolid1.Parts + [rspStartPolygon, rspStopPolygon];
end;

procedure TFormPawn.TrackBar2Change(Sender: TObject);
begin
  RotationSolid1.Slices := TrackBar2.Position;
end;

procedure TFormPawn.TrackBar3Change(Sender: TObject);
begin
  RotationSolid1.Division := TrackBar3.Position;
end;

procedure TFormPawn.Timer1Timer(Sender: TObject);
begin
  LabelTri.Caption := Format('%d Triangles', [RotationSolid1.TriangleCount]);
end;

procedure TFormPawn.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormPawn.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Shift <> [] then
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;

end.
