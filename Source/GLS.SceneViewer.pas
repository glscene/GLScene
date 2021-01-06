//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.SceneViewer;

(* Windows specific Scene viewer *)

interface

{$I GLScene.inc}

uses
  WinApi.Windows,
  WinApi.Messages,
  System.Classes,
  System.SysUtils,
  System.Types,
  VCL.Graphics,
  VCL.Forms,
  VCL.Controls,
  GLS.Scene,
  GLS.WindowsContext,
  GLS.Context;



type
  TTouchEvent = procedure(X, Y, TouchWidth, TouchHeight : integer; TouchID : Cardinal;
                          TouchCount : integer; FromPen : boolean) of object;

  (* Component where the GLScene objects get rendered.
     This component delimits the area where OpenGL renders the scene,
     it represents the 3D scene viewed from a camera (specified in the
     camera property). This component can also render to a file or to a bitmap.
     It is primarily a windowed component, but it can handle full-screen
     operations : simply make this component fit the whole screen (use a
     borderless form).
     This viewer also allows to define rendering options such a fog, face culling,
     depth testing, etc. and can take care of framerate calculation.  *)
  TGLSceneViewer = class(TWinControl)
  private
    FBuffer: TGLSceneBuffer;
    FVSync: TGLVSyncMode;
    FOwnDC: HDC;
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
    FMouseInControl: Boolean;
    FLastScreenPos: TPoint;
    FPenAsTouch: boolean;
    FOnTouchMove: TTouchEvent;
    FOnTouchUp: TTouchEvent;
    FOnTouchDown: TTouchEvent;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMGetDglCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMTouch(var Message: TMessage); message WM_TOUCH;
    procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var msg: TMessage); message CM_MOUSELEAVE;
    function GetFieldOfView: single;
    procedure SetFieldOfView(const Value: single);
    function GetIsRenderingContextAvailable: Boolean;
  protected
    procedure SetBeforeRender(const val: TNotifyEvent);
    function GetBeforeRender: TNotifyEvent;
    procedure SetPostRender(const val: TNotifyEvent);
    function GetPostRender: TNotifyEvent;
    procedure SetAfterRender(const val: TNotifyEvent);
    function GetAfterRender: TNotifyEvent;
    procedure SetCamera(const val: TGLCamera);
    function GetCamera: TGLCamera;
    procedure SetBuffer(const val: TGLSceneBuffer);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Loaded; override;
    procedure DoBeforeRender(Sender: TObject); virtual;
    procedure DoBufferChange(Sender: TObject);
    procedure DoBufferStructuralChange(Sender: TObject); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    (* Makes TWinControl's RecreateWnd public.
       This procedure allows to work around limitations in some OpenGL
       drivers (like MS Software OpenGL) that are not able to share lists
       between RCs that already have display lists. *)
    procedure RecreateWnd;
    property IsRenderingContextAvailable: Boolean read GetIsRenderingContextAvailable;
    function LastFrameTime: Single;
    function FramesPerSecond: Single;
    function FramesPerSecondText(decimals: Integer = 1): string;
    procedure ResetPerformanceMonitor;
    function CreateSnapShotBitmap: TBitmap;
    procedure RegisterTouch;
    procedure UnregisterTouch;
    property RenderDC: HDC read FOwnDC;
    property MouseInControl: Boolean read FMouseInControl;
  published
    // Camera from which the scene is rendered.
    property Camera: TGLCamera read GetCamera write SetCamera;
    (* Specifies if the refresh should be synchronized with the VSync signal.
       If the underlying OpenGL ICD does not support the WGL_EXT_swap_control
       extension, this property is ignored.  *)
    property VSync: TGLVSyncMode read FVSync write FVSync default vsmNoSync;
    (* Triggered before the scene's objects get rendered.
       You may use this event to execute your own OpenGL rendering. *)
    property BeforeRender: TNotifyEvent read GetBeforeRender write SetBeforeRender;
    (* Triggered just after all the scene's objects have been rendered.
       The OpenGL context is still active in this event, and you may use it
       to execute your own OpenGL rendering.  *)
    property PostRender: TNotifyEvent read GetPostRender write SetPostRender;
    (* Called after rendering.
       You cannot issue OpenGL calls in this event, if you want to do your own
       OpenGL stuff, use the PostRender event. *)
    property AfterRender: TNotifyEvent read GetAfterRender write SetAfterRender;
    // Access to buffer properties.
    property Buffer: TGLSceneBuffer read FBuffer write SetBuffer;
    (* Returns or sets the field of view for the viewer, in degrees.
    This value depends on the camera and the width and height of the scene.
    The value isn't persisted, if the width/height or camera.focallength is
    changed, FieldOfView is changed also. *)
    property FieldOfView: single read GetFieldOfView write SetFieldOfView;
    (* Since Windows 10 Fall Creators Update in 2017, pen also triggers
      touch input. You can set PenAsTouch = false to filter out such input. *)
    property PenAsTouch: boolean read FPenAsTouch write FPenAsTouch;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnTouchMove: TTouchEvent read FOnTouchMove write FOnTouchMove;
    property OnTouchUp: TTouchEvent read FOnTouchUp write FOnTouchUp;
    property OnTouchDown: TTouchEvent read FOnTouchDown write FOnTouchDown;
    property Align;
    property Anchors;
    property DragCursor;
    property DragMode;
    property Enabled;
    property HelpContext;
    property Hint;
    property PopupMenu;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnContextPopup;
    property TabStop;
    property TabOrder;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property Touch;
  end;

procedure SetupVSync(const AVSyncMode : TGLVSyncMode);

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

procedure SetupVSync(const AVSyncMode : TGLVSyncMode);
var
  I: Integer;
begin
  if gl.W_EXT_swap_control then
  begin
    I := gl.WGetSwapIntervalEXT;
    case AVSyncMode of
      vsmSync  : if I <> 1 then gl.WSwapIntervalEXT(1);
      vsmNoSync: if I <> 0 then gl.WSwapIntervalEXT(0);
    else
       Assert(False);
    end;
  end;
end;

// ------------------
// ------------------ TGLSceneViewer ------------------
// ------------------
constructor TGLSceneViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csClickEvents, csDoubleClicks, csOpaque, csCaptureMouse];
  if csDesigning in ComponentState then
    ControlStyle := ControlStyle + [csFramed];
  Width := 100;
  Height := 100;
  FVSync := vsmNoSync;
  FBuffer := TGLSceneBuffer.Create(Self);
  FBuffer.ViewerBeforeRender := DoBeforeRender;
  FBuffer.OnChange := DoBufferChange;
  FBuffer.OnStructuralChange := DoBufferStructuralChange;
end;

destructor TGLSceneViewer.Destroy;
begin
  FBuffer.Free;
  FBuffer := nil;
  inherited Destroy;
end;

procedure TGLSceneViewer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (FBuffer <> nil) then
  begin
    if (AComponent = FBuffer.Camera) then
      FBuffer.Camera := nil;
  end;
  inherited;
end;

procedure TGLSceneViewer.RecreateWnd;
begin
  inherited;
end;

procedure TGLSceneViewer.RegisterTouch;
begin
  RegisterTouchWindow(Handle, 0);
end;

procedure TGLSceneViewer.SetBeforeRender(const val: TNotifyEvent);
begin
  FBuffer.BeforeRender := val;
end;

function TGLSceneViewer.GetBeforeRender: TNotifyEvent;
begin
  Result := FBuffer.BeforeRender;
end;

procedure TGLSceneViewer.SetPostRender(const val: TNotifyEvent);
begin
  FBuffer.PostRender := val;
end;

procedure TGLSceneViewer.UnregisterTouch;
begin
  UnregisterTouchWindow(Handle);
end;

function TGLSceneViewer.GetPostRender: TNotifyEvent;
begin
  Result := FBuffer.PostRender;
end;

procedure TGLSceneViewer.SetAfterRender(const val: TNotifyEvent);
begin
  FBuffer.AfterRender := val;
end;

function TGLSceneViewer.GetAfterRender: TNotifyEvent;
begin
  Result := FBuffer.AfterRender;
end;

procedure TGLSceneViewer.SetCamera(const val: TGLCamera);
begin
  FBuffer.Camera := val;
end;

function TGLSceneViewer.GetCamera: TGLCamera;
begin
  Result := FBuffer.Camera;
end;

procedure TGLSceneViewer.SetBuffer(const val: TGLSceneBuffer);
begin
  FBuffer.Assign(val);
end;

procedure TGLSceneViewer.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
    WindowClass.Style := WindowClass.Style or CS_OWNDC;
  end;
end;

procedure TGLSceneViewer.CreateWnd;
begin
  inherited CreateWnd;
  // initialize and activate the OpenGL rendering context
  // need to do this only once per window creation as we have a private DC
  FBuffer.Resize(0, 0, Self.Width, Self.Height);
  FOwnDC := GetDC(Handle);
  FBuffer.CreateRC(FOwnDC, False);
end;

procedure TGLSceneViewer.DestroyWnd;
begin
  FBuffer.DestroyRC;
  if FOwnDC <> 0 then
  begin
    ReleaseDC(Handle, FOwnDC);
    FOwnDC := 0;
  end;
  inherited;
end;

procedure TGLSceneViewer.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if IsRenderingContextAvailable then
    Message.Result := 1
  else
    inherited;
end;

procedure TGLSceneViewer.WMSize(var Message: TWMSize);
begin
  inherited;
  FBuffer.Resize(0, 0, Message.Width, Message.Height);
end;

procedure TGLSceneViewer.WMTouch(var Message: TMessage);

  function TOUCH_COORD_TO_PIXEL(l : integer) : integer; // Delphi XE needs this
  begin
    result := l div 100;
  end;

  function TouchPointToPoint(const TouchPoint: TTouchInput): TPoint;
  begin
    Result := Point(TOUCH_COORD_TO_PIXEL(TouchPoint.X), TOUCH_COORD_TO_PIXEL(TouchPoint.Y));
    PhysicalToLogicalPoint(Handle, Result);
    Result:=ScreenToClient(Result);
  end;

var
  TouchInputs: array of TTouchInput;
  TouchInput: TTouchInput;
  Handled: Boolean;
  Point: TPoint;
  TouchCount : integer;
  LFromPen : boolean;
begin
  Handled := False;
  SetLength(TouchInputs, Message.WParam);
  TouchCount := Message.WParam;
  GetTouchInputInfo(Message.LParam, Message.WParam, @TouchInputs[0],
    SizeOf(TTouchInput));
  try
    for TouchInput in TouchInputs do
    begin
      LFromPen := (TouchInput.dwFlags AND TOUCHEVENTF_PEN) > 0; // For Windows 10 Fall Creators Update or later
      if FPenAsTouch or not LFromPen then // skip if pen input?
      begin
        Point := TouchPointToPoint(TouchInput);

        if (TouchInput.dwFlags AND TOUCHEVENTF_MOVE) > 0 then
        if Assigned(OnTouchMove) then
        begin
          OnTouchMove(Point.X, Point.Y, TouchInput.cxContact, TouchInput.cyContact, TouchInput.dwID, TouchCount, LFromPen);
        end;

        if (TouchInput.dwFlags AND TOUCHEVENTF_DOWN) > 0 then
        if Assigned(OnTouchDown) then
        begin
          OnTouchDown(Point.X, Point.Y, TouchInput.cxContact, TouchInput.cyContact, TouchInput.dwID, TouchCount, LFromPen);
        end;

        if (TouchInput.dwFlags AND TOUCHEVENTF_UP) > 0 then
        if Assigned(OnTouchUp) then
        begin
          OnTouchUp(Point.X, Point.Y, TouchInput.cxContact, TouchInput.cyContact, TouchInput.dwID, TouchCount, LFromPen);
        end;
      end;
    end;

    Handled := True;
  finally
    if Handled then
      CloseTouchInputHandle(Message.LParam)
    else
      inherited;
  end;
end;

procedure TGLSceneViewer.WMPaint(var Message: TWMPaint);
var
  PS: TPaintStruct;
  p: TPoint;
begin
  p := ClientToScreen(Point(0, 0));
  if (FLastScreenPos.X <> p.X) or (FLastScreenPos.Y <> p.Y) then
  begin
    // Workaround for MS OpenGL "black borders" bug
    if FBuffer.RCInstantiated then
      PostMessage(Handle, WM_SIZE, SIZE_RESTORED,
        Width + (Height shl 16));
    FLastScreenPos := p;
  end;
  BeginPaint(Handle, PS);
  try
    if IsRenderingContextAvailable and (Width > 0) and (Height > 0) then
      FBuffer.Render;
  finally
    EndPaint(Handle, PS);
    Message.Result := 0;
  end;
end;


procedure TGLSceneViewer.WMGetDglCode(var Message: TMessage);
begin
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

procedure TGLSceneViewer.WMDestroy(var Message: TWMDestroy);
begin
  if Assigned(FBuffer) then
  begin
    FBuffer.DestroyRC;
    if FOwnDC <> 0 then
    begin
      ReleaseDC(Handle, FOwnDC);
      FOwnDC := 0;
    end;
  end;
  inherited;
end;

procedure TGLSceneViewer.CMMouseEnter(var msg: TMessage);
begin
  inherited;
  FMouseInControl := True;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TGLSceneViewer.CMMouseLeave(var msg: TMessage);
begin
  inherited;
  FMouseInControl := False;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TGLSceneViewer.Loaded;
begin
  inherited Loaded;
  // initiate window creation
  HandleNeeded;
end;

procedure TGLSceneViewer.DoBeforeRender(Sender: TObject);
begin
  SetupVSync(VSync);
end;

procedure TGLSceneViewer.DoBufferChange(Sender: TObject);
begin
  if (not Buffer.Rendering) and (not Buffer.Freezed) then
    Invalidate;
end;

procedure TGLSceneViewer.DoBufferStructuralChange(Sender: TObject);
begin
  RecreateWnd;
end;

procedure TGLSceneViewer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if csDesignInteractive in ControlStyle then
    FBuffer.NotifyMouseMove(Shift, X, Y);
end;

function TGLSceneViewer.LastFrameTime: Single;
begin
  Result := FBuffer.LastFrameTime;
end;

function TGLSceneViewer.FramesPerSecond: Single;
begin
  Result := FBuffer.FramesPerSecond;
end;

function TGLSceneViewer.FramesPerSecondText(decimals: Integer = 1): string;
begin
  Result := Format('%.*f FPS', [decimals, FBuffer.FramesPerSecond]);
end;

procedure TGLSceneViewer.ResetPerformanceMonitor;
begin
  FBuffer.ResetPerformanceMonitor;
end;

function TGLSceneViewer.CreateSnapShotBitmap: TBitmap;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Width;
  Result.Height := Height;

  BitBlt(Result.Canvas.Handle, 0, 0, Width, Height,
    RenderDC, 0, 0, SRCCOPY);
end;

function TGLSceneViewer.GetFieldOfView: single;
begin
  if not Assigned(Camera) then
    result := 0
  else 
  if Width < Height then
    result := Camera.GetFieldOfView(Width)
  else
    result := Camera.GetFieldOfView(Height);
end;

function TGLSceneViewer.GetIsRenderingContextAvailable: Boolean;
begin
  Result := FBuffer.RCInstantiated and FBuffer.RenderingContext.IsValid;
end;

procedure TGLSceneViewer.SetFieldOfView(const Value: single);
begin
  if Assigned(Camera) then
  begin
    if Width < Height then
      Camera.SetFieldOfView(Value, Width)

    else
      Camera.SetFieldOfView(Value, Height);
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterClass(TGLSceneViewer);
end.

