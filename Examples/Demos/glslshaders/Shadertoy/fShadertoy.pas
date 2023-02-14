unit fShadertoy;

interface

uses
  Winapi.Windows,
  Winapi.ShellAPI,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,

  GLS.Scene,
  GLS.VectorTypes,
  GLS.BaseClasses,
  GLS.Objects,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.Keyboard,
  GLS.RenderContextInfo,
  GLS.OpenGLAdapter,
  GLS.OpenGLTokens,
  GLS.VectorGeometry,

  GLS.AsyncTimer,
  GLS.Context,
  GLS.Coordinates,
  GLS.HUDObjects,
  GLS.Utils;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    VP: TGLSceneViewer;
    Cad: TGLCadencer;
    Cam: TGLCamera;
    dc_cam: TGLDummyCube;
    Hud: TGLHUDSprite;
    AT: TGLAsyncTimer;
    DoGL: TGLDirectOpenGL;
    procedure FormCreate(Sender: TObject);
    procedure doglRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure FormResize(Sender: TObject);
    procedure atTimer(Sender: TObject);
    procedure cadProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure vpClick(Sender: TObject);
  private
    PathToAsset: TFileName;
  end;

var
  Form1: TForm1;
  PrHnd: TGLProgramHandle;
  initDGL: boolean;

implementation

{$R *.dfm}


//
// FormCreate
//
procedure TForm1.FormCreate;
begin
  PathToAsset := GetCurrentAssetPath();
  SetCurrentDir(PathToAsset  + '\shader');

  vp.Buffer.RenderingContext.Activate;
end;


//
// CadencerProgress
//
procedure TForm1.cadProgress;
begin
  vp.Invalidate;
end;

//
// DirectOpenGLRender
//
procedure TForm1.doglRender;
begin
  if not initDGL then
  begin
    if not(GL.ARB_shader_objects and GL.ARB_fragment_shader) then
    begin
      ShowMessage('Your videocard don''t support necessary shaders');
      Halt;
    end;
    PrHnd := TGLProgramHandle.CreateAndAllocate;
    PrHnd.AddShader(TGLFragmentShaderHandle,
      LoadAnsiStringFromFile('eiffie_too-early.fp'));
    if not PrHnd.LinkProgram then
      raise Exception.Create(PrHnd.InfoLog);
    if not PrHnd.ValidateProgram then
      raise Exception.Create(PrHnd.InfoLog);
    initDGL := True;
  end;

  if initDGL then
  begin
    PrHnd.UseProgramObject;
    PrHnd.Uniform3f['iResolution'] := AffineVectorMake(vp.Width, vp.Height, 0);
    PrHnd.Uniform1f['iGlobalTime'] := cad.CurrentTime;
    Hud.Render(rci);
    PrHnd.EndUseProgramObject;
  end;
end;

//
// FormResize
//
procedure TForm1.FormResize;
begin
  Hud.Width := vp.Width;
  Hud.Height := vp.Height;
  Hud.Position.SetPoint(vp.Width div 2, vp.Height div 2, 0);
end;

//
// AtTimer
//
procedure TForm1.atTimer;
begin
  Form1.Caption :=
    Format('Eiffie from shadertoy.com / FPS: %.3f  time: %.3f',
    [vp.FramesPerSecond, cad.CurrentTime]);
  vp.ResetPerformanceMonitor;
end;

//
// Open URL
//
procedure TForm1.vpClick;
begin
  ShellExecute(Handle, 'open', 'https://www.shadertoy.com', nil,
    nil, SW_Normal);
  close;
end;

end.
