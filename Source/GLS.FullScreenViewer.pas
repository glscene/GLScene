//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.FullScreenViewer;

(* A Platform specific full-screen viewer *)

interface

{$I GLScene.inc}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  System.SysUtils,
  VCL.Forms,
  VCL.Controls,
  VCL.Menus,

  GLS.OpenGLTokens,
  GLS.Utils,
  GLS.Context,
  GLS.Scene,
  GLS.SceneViewer;

type

  TGLScreenDepth = (sd8bits, sd16bits, sd24bits, sd32bits);

  (*  A FullScreen viewer. 
    This non visual viewer will, when activated, use the full screen as rendering
    surface. It will also switch/restore videomode depending on the required
    width/height.
    This is performed by creating an underlying TForm and using its surface
    for rendering OpenGL, "decent" ICDs will automatically use PageFlipping
    instead of BlockTransfer (slower buffer flipping mode used for windowed OpenGL).
    Note: if you terminate the application either via a kill or in the IDE,
    the original resolution isn't restored. *)
  TGLFullScreenViewer = class(TGLNonVisualViewer)
  private
    FFormIsOwned: Boolean;
    FForm: TForm;
    FOwnDC: HWND;
    FScreenDepth: TGLScreenDepth;
    FActive: Boolean;
    FSwitchedResolution: Boolean;
    FManualRendering: Boolean;
    FUpdateCount: Integer;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FOnMouseWheelDown: TMouseWheelUpDownEvent;
    FOnMouseWheelUp: TMouseWheelUpDownEvent;
    FOnClick, FOnDblClick: TNotifyEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyUp: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnClose: TCloseEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FStayOnTop: Boolean;
    FVSync: TGLVSyncMode;
    FRefreshRate: Integer;
    FCursor: TCursor;
    FPopupMenu: TPopupMenu;
    procedure SetScreenDepth(const val: TGLScreenDepth);
    procedure SetActive(const val: Boolean);
    procedure SetOnMouseDown(const val: TMouseEvent);
    procedure SetOnMouseUp(const val: TMouseEvent);
    procedure SetOnMouseMove(const val: TMouseMoveEvent);
    procedure SetOnMouseWheel(const val: TMouseWheelEvent);
    procedure SetOnMouseWheelDown(const val: TMouseWheelUpDownEvent);
    procedure SetOnMouseWheelUp(const val: TMouseWheelUpDownEvent);
    procedure SetOnClick(const val: TNotifyEvent);
    procedure SetOnDblClick(const val: TNotifyEvent);
    procedure SetOnCloseQuery(const val: TCloseQueryEvent);
    procedure SetOnClose(const val: TCloseEvent);
    procedure SetOnKeyUp(const val: TKeyEvent);
    procedure SetOnKeyDown(const val: TKeyEvent);
    procedure SetOnKeyPress(const val: TKeyPressEvent);
    procedure SetStayOnTop(const val: Boolean);
    procedure SetCursor(const val: TCursor);
    procedure SetPopupMenu(const val: TPopupMenu);
    procedure SetForm(aVal: TForm);
    procedure SetManualRendering(const val: Boolean);
  protected
    function GetHandle: HWND;
    procedure DoBeforeRender(Sender: TObject);
    procedure DoBufferChange(Sender: TObject); override;
    procedure DoBufferStructuralChange(Sender: TObject); override;
    procedure Startup;
    procedure Shutdown;
    procedure BindFormEvents;
    procedure DoCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure DoPaint(Sender: TObject);
    procedure DoActivate(Sender: TObject);
    procedure DoDeactivate(Sender: TObject);
    procedure DoFormDestroy(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Render(baseObject: TGLBaseSceneObject = nil); override;

    (*  Adjusts property so that current resolution will be used. 
      Call this method if you want to make sure video mode isn't switched. *)
    procedure UseCurrentResolution;
    procedure BeginUpdate;
    procedure EndUpdate;
    //  Activates/deactivates full screen mode.  
    property Active: Boolean read FActive write SetActive;
    procedure ReActivate;
    (*  Read access to the underlying form handle. 
      Returns 0 (zero) if the viewer is not active or has not yet
      instantiated its form. *)
    property Handle: HWND read GetHandle;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function LastFrameTime: Single;
    function FramesPerSecond: Single;
    function FramesPerSecondText(decimals: Integer = 1): String;
    procedure ResetPerformanceMonitor;
    property RenderDC: HWND read FOwnDC;
  published
    property Form: TForm read FForm write SetForm;
    property ManualRendering: Boolean read FManualRendering write SetManualRendering;
    // It is not used in UNIX.  Requested ScreenDepth. 
    property ScreenDepth: TGLScreenDepth read FScreenDepth write SetScreenDepth default sd32bits;
    (*  Specifies if the underlying form is "fsStayOnTop". 
      The benefit of StayOnTop is that it hides the windows bar and
      other background windows. The "fsStayOnTop" is automatically
      switched off/on when the underlying form loses/gains focus.
      It is recommended not to use StayOnTop while running in the IDE
      or during the debugging phase.  *)
    property StayOnTop: Boolean read FStayOnTop write SetStayOnTop default False;
    (*  Specifies if the refresh should be synchronized with the VSync signal. 
      If the underlying OpenGL ICD does not support the WGL_EXT_swap_control
      extension, this property is ignored. *)
    property VSync: TGLVSyncMode read FVSync write FVSync default vsmSync;
    (*  Screen refresh rate. 
      Use zero for system default. This property allows you to work around
      the winxp bug that limits uses a refresh rate of 60hz when changeing
      resolution. it is however suggested to give the user the opportunity
      to adjust it instead of having a fixed value (expecially beyond
      75hz or for resolutions beyond 1024x768). 
      the value will be automatically clamped to the highest value
      *reported* compatible with the monitor. *)
    property RefreshRate: Integer read FRefreshRate write FRefreshRate;
    property Cursor: TCursor read FCursor write SetCursor default crDefault;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property OnClose: TCloseEvent read FOnClose write SetOnClose;
    property OnKeyUp: TKeyEvent read FOnKeyUp write SetOnKeyUp;
    property OnKeyDown: TKeyEvent read FOnKeyDown write SetOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write SetOnKeyPress;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write SetOnCloseQuery;
    property OnClick: TNotifyEvent read FOnClick write SetOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write SetOnDblClick;
    property OnMouseDown: TMouseEvent read FOnMouseDown write SetOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write SetOnMouseUp;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write SetOnMouseMove;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write SetOnMouseWheel;
    property OnMouseWheelDown: TMouseWheelUpDownEvent read FOnMouseWheelDown
      write SetOnMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read FOnMouseWheelUp
      write SetOnMouseWheelUp;
  end;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

uses
  GLS.Screen, 
  GLS.WindowsContext;

const
  cScreenDepthToBPP: array [sd8bits .. sd32bits] of Integer = (8, 16, 24, 32);

procedure Register;
begin
  RegisterComponents('GLScene', [TGLFullScreenViewer]);
end;

// ------------------
// ------------------ TGLFullScreenViewer ------------------
// ------------------

constructor TGLFullScreenViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 800;
  Height := 600;
  FScreenDepth := sd32bits;
  FVSync := vsmSync;
  FCursor := crDefault;
  Buffer.ViewerBeforeRender := DoBeforeRender;
end;

destructor TGLFullScreenViewer.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

procedure TGLFullScreenViewer.DoBeforeRender(Sender: TObject);
begin
  SetupVSync(VSync);
end;

procedure TGLFullScreenViewer.DoBufferChange(Sender: TObject);
begin
  if Assigned(FForm) and (not Buffer.Rendering) then
  begin
    Buffer.Render;
  end;
end;

procedure TGLFullScreenViewer.DoBufferStructuralChange(Sender: TObject);
begin
  if Active and (FUpdateCount = 0) then
    ReActivate
end;

procedure TGLFullScreenViewer.Render(baseObject: TGLBaseSceneObject = nil);
begin
  Buffer.Render(baseObject);
end;

procedure TGLFullScreenViewer.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TGLFullScreenViewer.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    if Active then
      DoBufferStructuralChange(Self)
  end
  else if FUpdateCount < 0 then
  begin
    FUpdateCount := 0;
    Assert(False, 'Unbalanced Begin/EndUpdate');
  end;
end;

procedure TGLFullScreenViewer.ReActivate;
begin
  Shutdown;
  Startup;
end;

procedure TGLFullScreenViewer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (Buffer <> nil) then
  begin
    if (AComponent = Buffer.Camera) then
      Buffer.Camera := nil;
    Active := False;
    if (AComponent = FForm) then
    begin
      Active := False;
      Form := nil;
    end;
  end;
  inherited Notification(AComponent, Operation);
end;

function TGLFullScreenViewer.LastFrameTime: Single;
begin
  Result := Buffer.LastFrameTime;
end;

function TGLFullScreenViewer.FramesPerSecond: Single;
begin
  Result := Buffer.FramesPerSecond;
end;

function TGLFullScreenViewer.FramesPerSecondText(decimals: Integer): String;
begin
  Result := Format('%.*f FPS', [decimals, Buffer.FramesPerSecond]);
end;

procedure TGLFullScreenViewer.ResetPerformanceMonitor;
begin
  Buffer.ResetPerformanceMonitor;
end;

procedure TGLFullScreenViewer.UseCurrentResolution;
begin
  BeginUpdate;
  try
    Width := Screen.Width;
    Height := Screen.Height;
    case GetCurrentColorDepth of
      24: ScreenDepth := sd24bits;
      16: ScreenDepth := sd16bits;
      8:  ScreenDepth := sd8bits;
    else
      // highest depth possible otherwise
      ScreenDepth := sd32bits;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TGLFullScreenViewer.SetActive(const val: Boolean);
begin
  if val <> FActive then
  begin

    // Alt+Tab delayed until better times
    // {$IFDEF MSWindows}
    // Application.OnDeactivate:=DoDeActivate;
    // Application.OnActivate:=DoActivate;
    // {$ENDIF}

    if val then
      Startup
    else
      Shutdown;
  end;
end;

procedure TGLFullScreenViewer.Startup;
var
  res: TResolution;
begin
  if FActive then
    Exit;

  if FForm = nil then
  begin
    FFormIsOwned := True;
    FForm := TForm.Create(nil);
    FForm.Show();
  end
  else
    FFormIsOwned := False;

  with FForm do
  begin
    If BorderStyle <> bsNone then
      BorderStyle := bsNone;
    Cursor := Self.Cursor;
    PopupMenu := Self.PopupMenu;
    Left := 0;
    Top := 0;
    ClientWidth := Self.Width;
    ClientHeight := Self.Height;
    BindFormEvents;
    res := GetIndexFromResolution(Width, Height,
      cScreenDepthToBPP[ScreenDepth]);
    if res = 0 then
      raise Exception.Create('Unsupported video mode');
    if StayOnTop then
      FormStyle := fsStayOnTop
    else
      FormStyle := fsNormal;
{$IFDEF MSWINDOWS}
    SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and
      not WS_CAPTION);
{$ENDIF}
    // WindowState:=wsMaximized;
    // Switch video mode
    if (Screen.Width <> Width) or (Screen.Height <> Height) or
      (GetCurrentColorDepth <> cScreenDepthToBPP[ScreenDepth]) then
    begin
      SetFullscreenMode(res, FRefreshRate);
      FSwitchedResolution := True;
    end;
{$IFDEF MSWINDOWS}
    // Hides Taskbar + Windows 7 Button
    ShowWindow(FindWindow('Shell_TrayWnd', nil), SW_HIDE);
    ShowWindow(FindWindow('BUTTON', nil), SW_HIDE);
{$ENDIF}
    // Show;
  end;

  Buffer.Resize(0, 0, Width, Height);
  FOwnDC := GetDC(FForm.Handle);
  Buffer.CreateRC(FOwnDC, False);
  // Linux Unicode
{$IFDEF Linux}
  GrabMouseToForm(FForm);
{$ENDIF}
  // todo
  FActive := True;
end;

procedure TGLFullScreenViewer.Shutdown;
begin
  if not FActive then
    Exit;
  Assert(FForm <> nil);

  Buffer.DestroyRC;
  with FForm do
  begin
    Cursor := crDefault;
    PopupMenu := nil;
  end;
{$IFDEF Linux}
  ReleaseMouseFromForm(FForm);
{$ENDIF}
{$IFDEF MSWINDOWS}
  // Restore Taskbar + Windows 7 Button
  ShowWindow(FindWindow('Shell_TrayWnd', nil), SW_SHOWNA);
  ShowWindow(FindWindow('BUTTON', nil), SW_SHOWNA);
{$ENDIF}
  // attempt that, at the very least...
  if FSwitchedResolution then
    RestoreDefaultMode;
  FActive := False;

  if FFormIsOwned then
    FreeAndNil(FForm);
end;

procedure TGLFullScreenViewer.BindFormEvents;
begin
  if Assigned(FForm) then
    with FForm do
    begin
      OnMouseDown := FOnMouseDown;
      OnMouseUp := FOnMouseUp;
      OnMouseMove := FOnMouseMove;
      OnMouseWheel := FOnMouseWheel;
      OnMouseWheelDown := FOnMouseWheelDown;
      OnMouseWheelUp := FOnMouseWheelUp;
      OnClick := FOnClick;
      OnDblClick := FOnDblClick;
      OnCloseQuery := DoCloseQuery;
      OnClose := FOnClose;
      OnKeyUp := FOnKeyUp;
      OnKeyDown := FOnKeyDown;
      OnKeyPress := FOnKeyPress;
      OnPaint := DoPaint;
      OnDestroy := DoFormDestroy;
    end;
end;

procedure TGLFullScreenViewer.DoCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if Assigned(FOnCloseQuery) then
    FOnCloseQuery(Sender, CanClose);
  CanClose := True;
  // if CanClose then Shutdown;
end;

procedure TGLFullScreenViewer.DoPaint(Sender: TObject);
begin
  If not ManualRendering then
    if Form <> nil then
      Render;
end;

procedure TGLFullScreenViewer.DoActivate(Sender: TObject);
begin
  (* If not Active and (Form <> nil) then begin
    Startup;
    end; *)
end;

procedure TGLFullScreenViewer.DoDeactivate(Sender: TObject);
begin
  (* If Active and (Form <> nil) then begin
    Shutdown;
    Form.Height:=0;
    Form.Width:=0;
    end; *)
end;

procedure TGLFullScreenViewer.DoFormDestroy(Sender: TObject);
begin
  Active := False;
end;

procedure TGLFullScreenViewer.SetScreenDepth(const val: TGLScreenDepth);
begin
  if FScreenDepth <> val then
  begin
    FScreenDepth := val;
    DoBufferStructuralChange(Self);
  end;
end;

procedure TGLFullScreenViewer.SetStayOnTop(const val: Boolean);
begin
  if val <> FStayOnTop then
  begin
    FStayOnTop := val;
    DoBufferStructuralChange(Self);
  end;
end;

procedure TGLFullScreenViewer.SetOnCloseQuery(const val: TCloseQueryEvent);
begin
  FOnCloseQuery := val; // this one uses a special binding
end;

procedure TGLFullScreenViewer.SetOnClose(const val: TCloseEvent);
begin
  If Form <> nil then
    Form.OnClose := val;
  FOnClose := val;
end;

procedure TGLFullScreenViewer.SetOnKeyPress(const val: TKeyPressEvent);
begin
  If Form <> nil then
    Form.OnKeyPress := val;
  FOnKeyPress := val;
end;

procedure TGLFullScreenViewer.SetOnKeyUp(const val: TKeyEvent);
begin
  If Form <> nil then
    Form.OnKeyUp := val;
  FOnKeyUp := val;
end;

procedure TGLFullScreenViewer.SetOnKeyDown(const val: TKeyEvent);
begin
  If Form <> nil then
    Form.OnKeyDown := val;
  FOnKeyDown := val;
end;

procedure TGLFullScreenViewer.SetOnMouseWheel(const val: TMouseWheelEvent);
begin
  If Form <> nil then
    Form.OnMouseWheel := val;
  FOnMouseWheel := val;
end;

procedure TGLFullScreenViewer.SetOnMouseWheelDown
  (const val: TMouseWheelUpDownEvent);
begin
  If Form <> nil then
    Form.OnMouseWheelDown := val;
  FOnMouseWheelDown := val;
end;

procedure TGLFullScreenViewer.SetOnMouseWheelUp
  (const val: TMouseWheelUpDownEvent);
begin
  If Form <> nil then
    Form.OnMouseWheelUp := val;
  FOnMouseWheelUp := val;
end;

procedure TGLFullScreenViewer.SetOnClick(const val: TNotifyEvent);
begin
  If Form <> nil then
    Form.OnClick := val;
  FOnClick := val;
end;

procedure TGLFullScreenViewer.SetOnDblClick(const val: TNotifyEvent);
begin
  If Form <> nil then
    Form.OnDblClick := val;
  FOnDblClick := val;
end;

procedure TGLFullScreenViewer.SetOnMouseMove(const val: TMouseMoveEvent);
begin
  If Form <> nil then
    Form.OnMouseMove := val;
  FOnMouseMove := val;
end;

procedure TGLFullScreenViewer.SetOnMouseDown(const val: TMouseEvent);
begin
  If Form <> nil then
    Form.OnMouseDown := val;
  FOnMouseDown := val;
end;

procedure TGLFullScreenViewer.SetOnMouseUp(const val: TMouseEvent);
begin
  If Form <> nil then
    Form.OnMouseUp := val;
  FOnMouseUp := val;
end;

procedure TGLFullScreenViewer.SetCursor(const val: TCursor);
begin
  if val <> FCursor then
  begin
    FCursor := val;
    if Form <> nil then
      FForm.Cursor := val;
  end;
end;

procedure TGLFullScreenViewer.SetPopupMenu(const val: TPopupMenu);
begin
  if val <> FPopupMenu then
  begin
    FPopupMenu := val;
    if Assigned(FForm) then
      FForm.PopupMenu := val;
  end;
end;

procedure TGLFullScreenViewer.SetForm(aVal: TForm);
begin
  FForm := aVal;
end;

procedure TGLFullScreenViewer.SetManualRendering(const val: Boolean);
begin
  if FManualRendering <> val then
    FManualRendering := val;
end;

function TGLFullScreenViewer.GetHandle: HWND;
begin
  if Form <> nil then
    Result := FForm.Handle
  else
    Result := 0;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

RegisterClasses([TGLFullScreenViewer]);

finalization

{$IFDEF MSWINDOWS}
// Restore Taskbar + Windows 7 Button
ShowWindow(FindWindow('Shell_TrayWnd', nil), SW_SHOWNA);
ShowWindow(FindWindow('BUTTON', nil), SW_SHOWNA);
{$ENDIF}

end.

