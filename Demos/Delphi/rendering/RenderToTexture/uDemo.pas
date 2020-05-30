unit uDemo;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  
  GLScene, GLWin32Viewer, GLFBORenderer, GLObjects, GLCoordinates, GLSimpleNavigation,
  GLMaterial, GLCadencer,  GLCrossPlatform, GLBaseClasses,
  GLRenderContextInfo, GLContext, GLAsyncTimer;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCamera1: TGLCamera;
    GLCube1: TGLCube;
    GLFBORenderer1: TGLFBORenderer;
    GLLightSource1: TGLLightSource;
    GLFBORenderer2: TGLFBORenderer;
    GLDirectOpenGL1: TGLDirectOpenGL;
    Panel1: TPanel;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    Timer1: TTimer;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure FormCreate(Sender: TObject);
    procedure GLDirectOpenGL1Render(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure GLFBORenderer2AfterRender(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure GLFBORenderer1AfterRender(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure CheckBox1Click(Sender: TObject);
    procedure RBClick(Sender: TObject);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure SBClick(Sender: TObject);
  private
     
    Triger: Boolean;
    FramerateRatio, N: Integer;
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Triger := False;
  FramerateRatio := 1;
  N := 0;
  GLCube1.Material.LibMaterialName := GLFBORenderer2.ColorTextureName;
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    GLSceneViewer1.VSync := vsmSync
  else
    GLSceneViewer1.VSync := vsmNoSync;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
  GLCube1.Turn(60*deltaTime);
end;

procedure TForm1.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  Inc(N);
  if N >= FramerateRatio then
  begin
    GLFBORenderer2.Active := Triger;
    Triger := not Triger;
    GLFBORenderer1.Active := Triger;
    N := 0;
  end;
end;

procedure TForm1.GLFBORenderer1AfterRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  GLCube1.Material.LibMaterialName := GLFBORenderer1.ColorTextureName;
  GLFBORenderer1.Active := False;
end;

procedure TForm1.GLFBORenderer2AfterRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  GLCube1.Material.LibMaterialName := GLFBORenderer2.ColorTextureName;
  GLFBORenderer2.Active := False;
end;

procedure TForm1.SBClick(Sender: TObject);
var
  Size: Integer;
begin
  case RadioGroup1.ItemIndex of
    0: Size := 256;
    1: Size := 512;
    2: Size := 2048;
  end;
  GLFBORenderer1.Width := Size;
  GLFBORenderer1.Height := Size;
  GLFBORenderer2.Width := Size;
  GLFBORenderer2.Height := Size;
end;

procedure TForm1.RBClick(Sender: TObject);
begin
  case RadioGroup2.ItemIndex of
    0: FramerateRatio := 1;
    1: FramerateRatio := 2;
    2: FramerateRatio := 10;
  end;
end;

end.
