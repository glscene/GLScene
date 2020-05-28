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
  Vcl.ExtCtrls,
  
  GLScene,
  GLObjects,
  GLWin32Viewer,
  GLTexture,
  GLGeomObjects,
  GLHiddenLineShader,
  GLOutlineShader,
  GLCrossPlatform,
  GLMaterial,
  GLCoordinates,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Torus1: TGLTorus;
    Sphere1: TGLSphere;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLAnnulusOutlined: TGLAnnulus;
    GLAnnulusPink: TGLAnnulus;
    GLCubeGreen: TGLCube;
    GLCubeTransparent: TGLCube;
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
    GLAnnulusDotted: TGLAnnulus;
    Bevel1: TBevel;
    Panel2: TPanel;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    Bevel2: TBevel;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
  private
     
  public
     
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  GLOutlineShader1.Enabled:=CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  GLHiddenLineShader2.Enabled:=CheckBox2.Checked;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  GLHiddenLineShader2.Solid:=CheckBox3.Checked;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  with GLHiddenLineShader2.BackLine do
    if CheckBox4.Checked then
      Pattern:=$FF00  // bit pattern
    else
      Pattern:=$FFFF;
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
  GLHiddenLineShader2.SurfaceLit:=CheckBox5.Checked;
end;

procedure TForm1.CheckBox6Click(Sender: TObject);
begin
  if CheckBox6.Checked then
    GLHiddenLineShader2.ShadeModel:=smFlat
  else
    GLHiddenLineShader2.ShadeModel:=smSmooth;
end;

//
// Classic mouse movement bits
//

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift=[ssLeft] then
      GLCamera1.MoveAroundTarget(my-y, mx-x)
   else if Shift=[ssRight] then
      GLCamera1.RotateTarget(my-y, mx-x);
   mx:=x; my:=y;
end;

end.
