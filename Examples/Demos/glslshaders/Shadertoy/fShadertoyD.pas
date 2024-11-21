unit fShadertoyD;

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
  Stage.VectorTypes,
  GLS.BaseClasses,
  GLS.Objects,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.XCollection,
  GLS.RenderContextInfo,
  GLS.OpenGLAdapter,
  Stage.TextureFormat,
  Stage.VectorGeometry,

  GLS.AsyncTimer,
  GLS.Context,
  GLS.Coordinates,
  GLS.HUDObjects,
  Stage.Utils;

type
  TFormEiffie = class(TForm)
    GLScene: TGLScene;
    SceneViewer: TGLSceneViewer;
    GLCadencer: TGLCadencer;
    Cam: TGLCamera;
    dcCamera: TGLDummyCube;
    Hud: TGLHUDSprite;
    GLAsyncTimer: TGLAsyncTimer;
    DoGL: TGLDirectOpenGL;
    procedure FormCreate(Sender: TObject);
    procedure doglRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure FormResize(Sender: TObject);
    procedure GLAsyncTimerTimer(Sender: TObject);
    procedure GLCadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure SceneViewerClick(Sender: TObject);
  private
    PathToAsset: TFileName;
  end;

var
  FormEiffie: TFormEiffie;
  PrHnd: TGLProgramHandle;
  initDGL: boolean;

implementation

{$R *.dfm}


//
// FormCreate
//
procedure TFormEiffie.FormCreate;
begin
  PathToAsset := GetCurrentAssetPath();
  SetCurrentDir(PathToAsset  + '\shader');

  SceneViewer.Buffer.RenderingContext.Activate;
end;


//
// CadencerProgress
//
procedure TFormEiffie.GLCadencerProgress;
begin
  SceneViewer.Invalidate;
end;

//
// DirectOpenGLRender
//
procedure TFormEiffie.doglRender;
begin
  if not initDGL then
  begin
    (*
    if not(GL.ARB_shader_objects and GL.ARB_fragment_shader) then
    begin
      ShowMessage('Your videocard don''t support necessary shaders');
      Halt;
    end;
    *)
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
    PrHnd.Uniform3f['iResolution'] := AffineVectorMake(SceneViewer.Width, SceneViewer.Height, 0);
    PrHnd.Uniform1f['iGlobalTime'] := GLCadencer.CurrentTime;
    Hud.Render(rci);
    PrHnd.EndUseProgramObject;
  end;
end;

//
// FormResize
//
procedure TFormEiffie.FormResize;
begin
  Hud.Width := SceneViewer.Width;
  Hud.Height := SceneViewer.Height;
  Hud.Position.SetPoint(SceneViewer.Width div 2, SceneViewer.Height div 2, 0);
end;

//
// AtTimer
//
procedure TFormEiffie.GLAsyncTimerTimer;
begin
  FormEiffie.Caption :=
    Format('Eiffie from shadertoy.com / FPS: %.3f  time: %.3f',
    [SceneViewer.FramesPerSecond, GLCadencer.CurrentTime]);
  SceneViewer.ResetPerformanceMonitor;
end;

//
// Open URL
//
procedure TFormEiffie.SceneViewerClick;
begin
  ShellExecute(Handle, 'open', 'https://www.shadertoy.com', nil,
    nil, SW_Normal);
  close;
end;

end.
