unit fLiningShaderD;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  GLS.Scene,
  GLS.Objects,
  GLS.SceneViewer,
  GLS.Texture,
  GLS.GeomObjects,
  GLSL.LineShaders,
 
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses;

type
  TFormLiningShader = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    Camera: TGLCamera;
    LightSource: TGLLightSource;
    Torus: TGLTorus;
    Sphere: TGLSphere;
    GLMaterialLibrary1: TGLMaterialLibrary;
    AnnulusOutlined: TGLAnnulus;
    AnnulusPink: TGLAnnulus;
    CubeGreen: TGLCube;
    CubeTransparent: TGLCube;
    GLOutlineShader1: TGLOutlineShader;
    GLHiddenLineShader1: TGLHiddenLineShader;
    GLHiddenLineShader2: TGLHiddenLineShader;
    GLHiddenLineShader3: TGLHiddenLineShader;
    GLHiddenLineShader4: TGLHiddenLineShader;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    GroupBox1: TGroupBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Label1: TLabel;
    CheckBox4: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    GLHiddenLineShader5: TGLHiddenLineShader;
    AnnulusDotted: TGLAnnulus;
    Bevel1: TBevel;
    Panel2: TPanel;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    Bevel2: TBevel;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
  public
    mx, my: Integer;
  end;

var
  FormLiningShader: TFormLiningShader;

// --------------------------------------
implementation
// --------------------------------------

{$R *.dfm}

procedure TFormLiningShader.CheckBox1Click(Sender: TObject);
begin
  GLOutlineShader1.Enabled := CheckBox1.Checked;
end;

procedure TFormLiningShader.CheckBox2Click(Sender: TObject);
begin
  GLHiddenLineShader2.Enabled := CheckBox2.Checked;
end;

procedure TFormLiningShader.CheckBox3Click(Sender: TObject);
begin
  GLHiddenLineShader2.Solid := CheckBox3.Checked;
end;

procedure TFormLiningShader.CheckBox4Click(Sender: TObject);
begin
  with GLHiddenLineShader2.BackLine do
    if CheckBox4.Checked then
      Pattern := $FF00 // bit pattern
    else
      Pattern := $FFFF;
end;

procedure TFormLiningShader.CheckBox5Click(Sender: TObject);
begin
  GLHiddenLineShader2.SurfaceLit := CheckBox5.Checked;
end;

procedure TFormLiningShader.CheckBox6Click(Sender: TObject);
begin
  if CheckBox6.Checked then
    GLHiddenLineShader2.ShadeModel := smFlat
  else
    GLHiddenLineShader2.ShadeModel := smSmooth;
end;

//
// Classic mouse movement bits
//

procedure TFormLiningShader.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormLiningShader.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift = [ssLeft] then
    Camera.MoveAroundTarget(my - Y, mx - X)
  else if Shift = [ssRight] then
    Camera.RotateTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;

end.
