unit fMemViewerD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Imaging.JPeg,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  
  GLS.Scene,
  GLS.Objects,
  GLS.Cadencer,
  GLS.Texture,
  GLS.SceneViewer,
 
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.Context,
  GLS.OpenGLAdapter;


type
  TFormMemViewer = class(TForm)
    Timer1: TTimer;
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    Cube1: TGLCube;
    GLLightSource1: TGLLightSource;
    GLMemoryViewer1: TGLMemoryViewer;
    GLCadencer1: TGLCadencer;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    RB1to1: TRadioButton;
    RB1to2: TRadioButton;
    RB1to10: TRadioButton;
    CheckBox1: TCheckBox;
    LabelFPS: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure CheckBox1Click(Sender: TObject);
    procedure GLSceneViewer1AfterRender(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RB1to1Click(Sender: TObject);
  private
     
    textureFramerateRatio, n: Integer;
  public
     
  end;

var
  FormMemViewer: TFormMemViewer;

implementation

{$R *.dfm}

procedure TFormMemViewer.FormCreate(Sender: TObject);
begin
  textureFramerateRatio := 1;
  n := 0;
end;

procedure TFormMemViewer.RB1to1Click(Sender: TObject);
begin
  textureFramerateRatio := (Sender as TRadioButton).Tag;
end;

procedure TFormMemViewer.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    GLSceneViewer1.VSync := vsmSync
  else
    GLSceneViewer1.VSync := vsmNoSync;
end;

procedure TFormMemViewer.GLSceneViewer1AfterRender(Sender: TObject);
begin
  if not GLSceneViewer1.Buffer.RenderingContext.gl.W_ARB_pbuffer then
  begin
    ShowMessage('WGL_ARB_pbuffer not supported...'#13#10#13#10
      + 'Get newer graphics hardware or try updating your drivers!');
    GLSceneViewer1.AfterRender := nil;
    Exit;
  end;
  Inc(n);
  try
    if n >= textureFramerateRatio then
    begin
      // render to the viewer
      GLMemoryViewer1.Render;
      // copy result to the textures
      GLMemoryViewer1.CopyToTexture(Cube1.Material.Texture);
      n := 0;
    end;
  except
    // pbuffer not supported... catchall for exotic ICDs...
    GLSceneViewer1.AfterRender := nil;
    raise;
  end;
end;

procedure TFormMemViewer.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  DummyCube1.TurnAngle := newTime * 60;
end;

procedure TFormMemViewer.Timer1Timer(Sender: TObject);
begin
  LabelFPS.Caption := Format('GLScene Memory Viewer'+' - %.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.

