unit Unit1;

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

  
  GLScene, GLObjects, GLTexture, GLExtrusion, GLWin32Viewer,
  GLCrossPlatform, GLCoordinates, GLBaseClasses, GLUtils;

type
  TForm1 = class(TForm)
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
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
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
     
  public
     
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  RotationSolid1.Material.Texture.Image.LoadFromFile('ashwood.jpg');
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
   if CheckBox1.Checked then
      RotationSolid1.SplineMode:=lsmCubicSpline
   else RotationSolid1.SplineMode:=lsmLines;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
   if CheckBox2.Checked then
      RotationSolid1.Normals:=nsSmooth
   else RotationSolid1.Normals:=nsFlat;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
   RotationSolid1.Material.Texture.Disabled:=not CheckBox3.Checked;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
   if CheckBox4.Checked then
      RotationSolid1.Material.Texture.TextureMode:=tmModulate
   else RotationSolid1.Material.Texture.TextureMode:=tmDecal;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
   RotationSolid1.StopAngle:=TrackBar1.Position;
   if TrackBar1.Position=360 then
      RotationSolid1.Parts:=RotationSolid1.Parts-[rspStartPolygon, rspStopPolygon]
   else RotationSolid1.Parts:=RotationSolid1.Parts+[rspStartPolygon, rspStopPolygon];
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
   RotationSolid1.Slices:=TrackBar2.Position;
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
begin
   RotationSolid1.Division:=TrackBar3.Position;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   LabelTri.Caption:=Format('%d Triangles', [RotationSolid1.TriangleCount]);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then
      GLCamera1.MoveAroundTarget(my-y, mx-x);
   mx:=x; my:=y;
end;

end.
