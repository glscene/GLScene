//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.ViewerOpenGL;

(* Viewer OpenGL for FMX *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.Windows,
  GXS.OpenGL, // GL_ARB_framebuffer_object or GL_EXT_framebuffer_blit

  System.Types,
  System.Classes,
  System.UITypes,
  System.SysUtils,
  FMX.Forms,
  FMX.Platform.Win,
  FMX.Types,
  FMX.Types3D,
  FMX.Controls,
  FMX.Graphics,

  GXS.Scene,
  GXS.Context,
  GXS.TextureFormat,
  GXS.WinContext;

type

  TgxSceneViewport = class(TControl)
  private
    FGLSBuffer: TgxSceneBuffer;
    FFMXBuffer: TBitmap;
    FFMXContext: TContext3D;
    FMultisample: FMX.Types3D.TMultisample;
    FParentHandle: HWND;
    FOwnDC: HDC;
    FDrawing: Boolean;
    FPostRender: TNotifyEvent;
    procedure SetBuffer(const Value: TgxSceneBuffer);
    function GetSceneCamera: TgxCamera;
    procedure SetSceneCamera(const Value: TgxCamera);
    procedure CopyBuffer(Sender: TObject);
    procedure SetBeforeRender(const Value: TNotifyEvent);
    function GetBeforeRender: TNotifyEvent;
    procedure SetAfterRender(const Value: TNotifyEvent);
    function GetAfterRender: TNotifyEvent;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure Realign; override; - E2179, removed override;
    procedure Realign;
  published
    (* Triggered before the scene's objects get rendered.
       You may use this event to execute your own OpenGL rendering. *)
    property BeforeRender: TNotifyEvent read GetBeforeRender write SetBeforeRender;
    (* Triggered just after all the scene's objects have been rendered.
       The OpenGL context is still active in this event, and you may use it
       to execute your own OpenGL rendering. *)
    property PostRender: TNotifyEvent read FPostRender write FPostRender;
    (* Called after rendering. You cannot issue OpenGL calls in this event, if you want
      to do your own OpenGL stuff, use the PostRender event. *)
    property AfterRender: TNotifyEvent read GetAfterRender write SetAfterRender;
    // Access to buffer properties.
    property Buffer: TgxSceneBuffer read FGLSBuffer write SetBuffer;
    // Camera from which the scene is rendered.
    property SceneCamera: TgxCamera read GetSceneCamera write SetSceneCamera;
  end;

//--------------------------------------------------------
implementation
//--------------------------------------------------------

// TgxSceneViewport

constructor TgxSceneViewport.Create(AOwner: TComponent);
var
  FMXH: TFmxHandle;
begin
  inherited Create(AOwner);
  FGLSBuffer := TgxSceneBuffer.Create(Self);
  FGLSBuffer.ContextOptions := FGLSBuffer.ContextOptions +
    [roDestinationAlpha] - [roDoubleBuffer] - [roNoSwapBuffers] + [roDebugContext];
  FGLSBuffer.BackgroundAlpha := 1.0;
  FGLSBuffer.AccumBufferBits := 32;
  FGLSBuffer.PostRender := CopyBuffer;
  if Owner is TCommonCustomForm then
  begin
    FMXH := THandle(Owner); /// TCommonCustomForm(Owner).Handle;
    FParentHandle := FMXH; /// FmxHandleToHWND(FMXH);
  end;
  Width := 100;
  Height := 100;
  FFMXBuffer := TBitmap.Create(100, 100);
  FMultisample := TMultisample.None;
  FFMXContext := TContextManager.DefaultContextClass.Create;

end;

destructor TgxSceneViewport.Destroy;
begin
  FreeAndNil(FGLSBuffer);
  if FOwnDC <> 0 then
  begin
    ReleaseDC(FParentHandle, FOwnDC);
    FOwnDC := 0;
  end;
  FreeAndNil(FFMXBuffer);
  FreeAndNil(FFMXContext);
  inherited;
end;

procedure TgxSceneViewport.Realign;
begin
  inherited Realign;

  if FFMXContext <> nil then
  begin
    FGLSBuffer.DestroyRC; // Service Context is shared handles, will be no so much
    FFMXBuffer.SetSize(Trunc(Width), Trunc(Height));
    FFMXContext.SetSize(Trunc(Width), Trunc(Height));
    AlignObjects(Self, Margins, FFMXBuffer.Width, FFMXBuffer.Height, FLastWidth, FLastHeight, FDisableAlign);
  end;
end;

procedure TgxSceneViewport.CopyBuffer(Sender: TObject);
var
  tempBuffer: Cardinal;
begin
  // Flip GL framebuffer
  if GL_ARB_framebuffer_object or GL_EXT_framebuffer_blit  then
  begin
    if Buffer.RenderingContext.AntiAliasing in [aaDefault, aaNone] then
      tempBuffer := GL_AUX0
    else
      tempBuffer := GL_LEFT;
    glReadBuffer(GL_FRONT);
    glDrawBuffer(tempBuffer);
    FGLSBuffer.RenderingContext.gxStates.ReadFrameBuffer := 0;
    FGLSBuffer.RenderingContext.gxStates.DrawFrameBuffer := 0;
    glBlitFramebuffer(
      0, FGLSBuffer.Height-1, FGLSBuffer.Width-1, 0,
      0, 0,                   FGLSBuffer.Width-1, FGLSBuffer.Height-1,
      GL_COLOR_BUFFER_BIT, GL_NEAREST);
    glReadBuffer(tempBuffer);
    glDrawBuffer(GL_FRONT);
  end
  else
  begin
    FFMXBuffer.FlipHorizontal;
    glReadBuffer(GL_FRONT);
  end;

  // Read framebuffer to operative memory
  // FFMXBuffer.Startline - E2003 Undeclared identifier: 'StartLine', changed to
  glReadPixels(0, 0, FGLSBuffer.Width, FGLSBuffer.Height,
      GL_BGRA, GL_UNSIGNED_BYTE, FFMXBuffer.ClassInfo);
  glFinish;

  inherited Canvas.DrawBitmap(
    FFMXBuffer, RectF(0, 0, FFMXBuffer.Width, FFMXBuffer.Height),
    RectF(0, 0, FFMXBuffer.Width, FFMXBuffer.Height), AbsoluteOpacity, True);

  if Assigned(FPostRender) then
    FPostRender(Self);
end;

procedure TgxSceneViewport.Paint;
var
  R: TRectF;
begin
  if (csDesigning in ComponentState) then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.Stroke.Thickness := 1;
    Canvas.Stroke.Dash := TStrokeDash.Dash;
    Canvas.Stroke.Kind := TBrushKind.Solid;
    Canvas.Stroke.Color := $A0909090;
    Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);
    Canvas.Stroke.Dash := TStrokeDash.Solid;
  end;

  if FDrawing then Exit;

  if (FGLSBuffer.Width <> FFMXBuffer.Width)
    or (FGLSBuffer.Height <> FFMXBuffer.Height) then
    Realign;

  if FGLSBuffer.RenderingContext = nil then
  begin
    if FParentHandle <> 0 then
    begin
      FGLSBuffer.Resize(0, 0, Trunc(Width), Trunc(Height));
      FOwnDC := GetDC(FParentHandle);
      FGLSBuffer.CreateRC(FOwnDC, True, 1);
      FFMXContext.BeginScene;
      FFMXContext.Clear([TClearTarget.Color], TAlphaColor($FF000000), 1.0, 0);
      FFMXContext.EndScene;
      FDrawing := True;
      try
        FGLSBuffer.Render;
      finally
        FDrawing := False;
      end;
    end;
  end
  else
  begin
    FDrawing := True;
    try
      if FFMXContext.BeginScene then
      begin
        FGLSBuffer.Render;
        FFMXContext.EndScene;
      end;
    finally
      FDrawing := False;
    end;
  end;
end;

procedure TgxSceneViewport.SetBeforeRender(const Value: TNotifyEvent);
begin
  FGLSBuffer.BeforeRender := Value;
end;

function TgxSceneViewport.GetBeforeRender: TNotifyEvent;
begin
  Result := FGLSBuffer.BeforeRender;
end;

procedure TgxSceneViewport.SetAfterRender(const Value: TNotifyEvent);
begin
  FGLSBuffer.AfterRender := Value;
end;

function TgxSceneViewport.GetAfterRender: TNotifyEvent;
begin
 Result := FGLSBuffer.AfterRender;
end;

procedure TgxSceneViewport.SetBuffer(const Value: TgxSceneBuffer);
begin
  FGLSBuffer.Assign(Value);
end;

function TgxSceneViewport.GetSceneCamera: TgxCamera;
begin
  Result := FGLSBuffer.Camera;
end;

procedure TgxSceneViewport.SetSceneCamera(const Value: TgxCamera);
begin
  FGLSBuffer.Camera := Value;
end;

//----------------------------------------------
initialization
//----------------------------------------------

RegisterFmxClasses([TgxSceneViewport]);

end.
