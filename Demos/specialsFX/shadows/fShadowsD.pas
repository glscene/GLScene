unit fShadowsD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Imaging.JPeg,

  
  GLS.VectorTypes, 
  GLS.VectorGeometry, 
  GLS.BaseClasses,
  GLS.Scene, 
  GLS.Graph, 
  GLS.Objects, 
  GLS.Texture,
  GLS.Graphics, 
  GLS.HUDObjects,
  GLS.zBuffer, 
  GLS.Cadencer, 
  GLS.AsyncTimer, 
  GLS.SceneViewer, 
  GLS.GeomObjects,
  GLS.Material, 
  GLS.Coordinates, 
  GLS.Behaviours, 
  GLS.Utils;

type
  TFormShadows = class(TForm)
    Panel2: TPanel;
    Panel1: TPanel;
    Panel3: TPanel;
    GLScene1: TGLScene;
    Label1: TLabel;
    Label2: TLabel;
    GLCamera1: TGLCamera;
    GLCamera2: TGLCamera;
    Objects: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    Viewer: TGLSceneViewer;
    Caster: TGLSceneViewer;
    Panel4: TPanel;
    Label4: TLabel;
    DistanceBar: TTrackBar;
    Label3: TLabel;
    DistanceBar2: TTrackBar;
    Panel5: TPanel;
    GLMaterialLibrary1: TGLMaterialLibrary;
    MemView: TGLMemoryViewer;
    Shadows1: TGLZShadows;
    Cube1: TGLCube;
    FrustBox: TCheckBox;
    AsyncTimer1: TGLAsyncTimer;
    Torus1: TGLTorus;
    RotateBox: TCheckBox;
    ShadowOnBox: TCheckBox;
    GLCadencer1: TGLCadencer;
    HeightField1: TGLHeightField;
    Teapot1: TGLTeapot;
    SoftBox: TCheckBox;
    SkyShadBox: TCheckBox;
    Focal: TTrackBar;
    Label5: TLabel;
    CastBtn: TButton;
    TimeLbl: TLabel;
    Panel6: TPanel;
    FadeBox: TCheckBox;
    dovBar: TTrackBar;
    Memo1: TMemo;
    AlphaBar: TTrackBar;
    Label9: TLabel;
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure CasterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CasterMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure DistanceBarChange(Sender: TObject);
    procedure DistanceBar2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CastBtnClick(Sender: TObject);
    procedure ViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CasterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FadeBoxClick(Sender: TObject);
    procedure HeightField1GetHeight(const X, Y: Single; var z: Single;
      var color: TVector4f; var texPoint: TTexPoint);
    procedure FrustBoxClick(Sender: TObject);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure RotateBoxClick(Sender: TObject);
    procedure ShadowOnBoxClick(Sender: TObject);
    procedure SoftBoxClick(Sender: TObject);
    procedure SkyShadBoxClick(Sender: TObject);
    procedure FocalChange(Sender: TObject);
    procedure dovBarChange(Sender: TObject);
    procedure AlphaBarChange(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
  private
     
  public
    mx, my: Integer;
    mx2, my2: Integer;
    zViewer, zCaster: TGLzBuffer;
  end;

var
  FormShadows: TFormShadows;

implementation

{$R *.DFM}

procedure TFormShadows.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  GLMaterialLibrary1.Materials[2].Material.texture.Image.loadFromFile
    ('marbletiles.jpg');
  GLMaterialLibrary1.Materials[2].Material.texture.disabled := false;

  GLMaterialLibrary1.Materials[3].Material.texture.Image.loadFromFile
    ('beigemarble.jpg');
  GLMaterialLibrary1.Materials[3].Material.texture.disabled := false;

  RotateBoxClick(Sender);
end;

procedure TFormShadows.ViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
  ActiveControl := DistanceBar;
end;

procedure TFormShadows.ViewerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift <> [] then
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
  GLCadencer1.Progress;
  Viewer.Refresh;
  Caster.Refresh;
end;

procedure TFormShadows.CasterMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx2 := X;
  my2 := Y;
  ActiveControl := DistanceBar2;
end;

procedure TFormShadows.CasterMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift <> [] then
    GLCamera2.MoveAroundTarget(my2 - Y, mx2 - X);
  mx2 := X;
  my2 := Y;
  if Shift <> [] then
  begin
    Shadows1.CastShadow;
    GLCadencer1.Progress;
    Viewer.Refresh;
    Caster.Refresh;
  end;
end;

procedure TFormShadows.DistanceBarChange(Sender: TObject);
var
  Dist, NewDist: Single;
begin
  with GLCamera1 do
  begin
    Dist := DistanceToTarget;
    NewDist := Sqr(DistanceBar.Position / 4) + 1;
    Position.AsAffineVector := VectorScale(Position.AsAffineVector,
      NewDist / Dist);
  end;
end;

procedure TFormShadows.DistanceBar2Change(Sender: TObject);
var
  Dist, NewDist: Single;
begin
  with GLCamera2 do
  begin
    Dist := DistanceToTarget;
    NewDist := Sqr(DistanceBar2.Position / 4) + 1;
    Position.AsAffineVector := VectorScale(Position.AsAffineVector,
      NewDist / Dist);
  end;
  Shadows1.CastShadow;
  Caster.Refresh;
end;

procedure TFormShadows.CastBtnClick(Sender: TObject);
var
  RefTime: Double;
begin
  RefTime := GLCadencer1.GetcurrentTime;
  Shadows1.CastShadow;
  Viewer.Refresh;
  TimeLbl.Caption := IntToStr(Round((GLCadencer1.GetcurrentTime - RefTime) *
    1000.00));
end;

procedure TFormShadows.ViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Viewer.Visible := True;
end;

procedure TFormShadows.CasterMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Shadows1.CastShadow;
end;

procedure TFormShadows.FadeBoxClick(Sender: TObject);
begin
  Shadows1.DepthFade := FadeBox.Checked;
end;

procedure TFormShadows.HeightField1GetHeight(const X, Y: Single; var z: Single;
  var color: TVector4f; var texPoint: TTexPoint);
begin
  z := 0;
end;

procedure TFormShadows.FrustBoxClick(Sender: TObject);
begin
  Shadows1.FrustShadow := FrustBox.Checked;
end;

procedure TFormShadows.AsyncTimer1Timer(Sender: TObject);
begin
  Caption := 'Shadows ' + Format('%.2f FPS', [Viewer.FramesPerSecond]);
  Viewer.ResetPerformanceMonitor;
end;

procedure TFormShadows.RotateBoxClick(Sender: TObject);
begin
  // AsyncTimer1.Enabled:=RotateBox.checked;
  GLCadencer1.Enabled := RotateBox.Checked;
end;

procedure TFormShadows.ShadowOnBoxClick(Sender: TObject);
begin
  Shadows1.Visible := ShadowOnBox.Checked;
end;

procedure TFormShadows.SoftBoxClick(Sender: TObject);
begin
  Shadows1.Soft := SoftBox.Checked;
end;

procedure TFormShadows.SkyShadBoxClick(Sender: TObject);
begin
  Shadows1.SkyShadow := SkyShadBox.Checked;
end;

procedure TFormShadows.FocalChange(Sender: TObject);
begin
  GLCamera2.FocalLength := Focal.Position;
  MemView.Render;
  Caster.Refresh;
  Shadows1.CastShadow;
  Viewer.Refresh;
end;

procedure TFormShadows.dovBarChange(Sender: TObject);
begin
  GLCamera2.DepthOfView := dovBar.Position;
  MemView.Render;
  Caster.Refresh;
  Shadows1.CastShadow;
  Viewer.Refresh;
end;

procedure TFormShadows.AlphaBarChange(Sender: TObject);
begin
  Shadows1.color.Alpha := AlphaBar.Position / 256;
end;

procedure TFormShadows.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  Shadows1.CastShadow;
end;

end.
