//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.FullScreenViewer;

(* A cross-platform full-screen viewer *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.Messages,
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  FMX.Types,
  FMX.Forms,
  FMX.Controls,
  FMX.Menus,

  GXS.Scene,
  GXS.Context,
  GXS.SceneViewer,
  GXS.WinContext,
  GXS.Utils,
  GXS.Screen;

type

  TgxScreenDepth = (sd8bits, sd16bits, sd24bits, sd32bits);

  { A FullScreen viewer.
    This non visual viewer will, when activated, use the full screen as rendering
    surface. It will also switch/restore videomode depending on the required
    width/height.
    This is performed by creating an underlying TForm and using its surface
    for rendering OpenGL, "decent" ICDs will automatically use PageFlipping
    instead of BlockTransfer (slower buffer flipping mode used for windowed
    OpenGL).
    Note: if you terminate the application either via a kill or in the IDE,
    the original resolution isn't restored. }
  TgxFullScreenViewer = class(TgxNonVisualViewer)
  private
    FFormIsOwned: Boolean;
    FForm: TForm;
    FOwnDC: THandle; // in VCL HWND;
    FScreenDepth: TgxScreenDepth;
    FActive: Boolean;
    FSwitchedResolution: Boolean;
    FManualRendering: Boolean;
    FUpdateCount: Integer;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseWheel: TMouseWheelEvent; // in VCL TMouseWheelEvent;
    FOnMouseWheelDown: TMouseWheelEvent; // in VCL TMouseWheelUpDownEvent;
    FOnMouseWheelUp: TMouseWheelEvent; // in VCL TMouseWheelUpDownEvent;
    FOnClick, FOnDblClick: TNotifyEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyUp: TKeyEvent;
    FOnKeyPress: TKeyEvent; // In VCL TKeyPressEvent;
    FOnClose: TCloseEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FStayOnTop: Boolean;
    FVSync: TgxSyncMode;
    FRefreshRate: Integer;
    { TODO : E2003 Undeclared identifier: 'TCursor' }
    (*FCursor: TCursor;*)
    FPopupMenu: TPopupMenu;
    procedure SetScreenDepth(const val: TgxScreenDepth);
    procedure SetActive(const val: Boolean);
    procedure SetOnMouseDown(const val: TMouseEvent);
    procedure SetOnMouseUp(const val: TMouseEvent);
    procedure SetOnMouseMove(const val: TMouseMoveEvent);
    procedure SetOnMouseWheel(const val: TMouseWheelEvent);
    procedure SetOnMouseWheelDown(const val: TMouseWheelEvent); //in VCL TMouseWheelUpDownEvent
    procedure SetOnMouseWheelUp(const val: TMouseWheelEvent); //in VCL TMouseWheelUpDownEvent
    procedure SetOnClick(const val: TNotifyEvent);
    procedure SetOnDblClick(const val: TNotifyEvent);
    procedure SetOnCloseQuery(const val: TCloseQueryEvent);
    procedure SetOnClose(const val: TCloseEvent);
    procedure SetOnKeyUp(const val: TKeyEvent);
    procedure SetOnKeyDown(const val: TKeyEvent);
    procedure SetOnKeyPress(const val: TKeyEvent); // in VCL TKeyPressEvent
    procedure SetStayOnTop(const val: Boolean);
    { TODO : E2003 Undeclared identifier: 'TCursor' }
    (*procedure SetCursor(const val: TCursor);*)
    procedure SetPopupMenu(const val: TPopupMenu);
    procedure SetForm(aVal: TForm);
    procedure SetManualRendering(const val: Boolean);
  protected
    function GetHandle: TWindowHandle;
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
    procedure Render(baseObject: TgxBaseSceneObject = nil); override;
    { Adjusts property so that current resolution will be used.
      Call this method if you want to make sure video mode isn't switched. }
    procedure UseCurrentResolution;
    procedure BeginUpdate;
    procedure EndUpdate;
    { Activates/deactivates full screen mode.  }
    property Active: Boolean read FActive write SetActive;
    procedure ReActivate;
    { Read access to the underlying form handle.
      Returns 0 (zero) if the viewer is not active or has not yet
      instantiated its form. }
    property Handle: TWindowHandle read GetHandle;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function LastFrameTime: Single;
    function FramesPerSecond: Single;
    function FramesPerSecondText(decimals: Integer = 1): String;
    procedure ResetPerformanceMonitor;
    property RenderDC: THandle read FOwnDC; //HWND
  published
    property Form: TForm read FForm write SetForm;
    property ManualRendering: Boolean read FManualRendering
      write SetManualRendering;
    // It is not used in UNIX
    { Requested ScreenDepth. }
    property ScreenDepth: TgxScreenDepth read FScreenDepth write SetScreenDepth
      default sd32bits;
    { Specifies if the underlying form is "fsStayOnTop".
      The benefit of StayOnTop is that it hides the windows bar and
      other background windows. The "fsStayOnTop" is automatically
      switched off/on when the underlying form loses/gains focus.
      It is recommended not to use StayOnTop while running in the IDE
      or during the debugging phase.  }
    property StayOnTop: Boolean read FStayOnTop write SetStayOnTop
      default False;
    { Specifies if the refresh should be synchronized with the VSync signal.
      If the underlying OpenGL ICD does not support the WGL_EXT_swap_control
      extension, this property is ignored. }
    property VSync: TgxSyncMode read FVSync write FVSync default vsmSync;
    { Screen refresh rate.
      Use zero for system default. This property allows you to work around
      the winxp bug that limits uses a refresh rate of 60hz when changeing
      resolution. it is however suggested to give the user the opportunity
      to adjust it instead of having a fixed value (expecially beyond
      75hz or for resolutions beyond 1024x768).
      the value will be automatically clamped to the highest value
      *reported* compatible with the monitor. }
    property RefreshRate: Integer read FRefreshRate write FRefreshRate;
    { TODO : E2003 Undeclared identifier: 'TCursor' }
    (*property Cursor: TCursor read FCursor write SetCursor default crDefault;*)
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property OnClose: TCloseEvent read FOnClose write SetOnClose;
    property OnKeyUp: TKeyEvent read FOnKeyUp write SetOnKeyUp;
    property OnKeyDown: TKeyEvent read FOnKeyDown write SetOnKeyDown;
    property OnKeyPress: TKeyEvent read FOnKeyPress write SetOnKeyPress;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery
      write SetOnCloseQuery;
    property OnClick: TNotifyEvent read FOnClick write SetOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write SetOnDblClick;
    property OnMouseDown: TMouseEvent read FOnMouseDown write SetOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write SetOnMouseUp;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove
      write SetOnMouseMove;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel
      write SetOnMouseWheel;
    property OnMouseWheelDown: TMouseWheelEvent read FOnMouseWheelDown
      write SetOnMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelEvent read FOnMouseWheelUp
      write SetOnMouseWheelUp;
  end;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

const
  cScreenDepthToBPP: array [sd8bits .. sd32bits] of Integer = (8, 16, 24, 32);

procedure Register;
begin
  RegisterComponents('GXScene', [TgxFullScreenViewer]);
end;

// ------------------
// ------------------ TgxFullScreenViewer ------------------
// ------------------

constructor TgxFullScreenViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 800;
  Height := 600;
  FScreenDepth := sd32bits;
  FVSync := vsmSync;
  { TODO : E2003 Undeclared identifier: 'TCursor' }
  (*FCursor := crDefault;*)
  Buffer.ViewerBeforeRender := DoBeforeRender;
end;

destructor TgxFullScreenViewer.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

procedure TgxFullScreenViewer.DoBeforeRender(Sender: TObject);
begin
  SetupVSync(VSync);
end;

procedure TgxFullScreenViewer.DoBufferChange(Sender: TObject);
begin
  if Assigned(FForm) and (not Buffer.Rendering) then
  begin
    Buffer.Render;
  end;
end;

procedure TgxFullScreenViewer.DoBufferStructuralChange(Sender: TObject);
begin
  if Active and (FUpdateCount = 0) then
    ReActivate
end;

procedure TgxFullScreenViewer.Render(baseObject: TgxBaseSceneObject = nil);
begin
  Buffer.Render(baseObject);
end;

procedure TgxFullScreenViewer.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TgxFullScreenViewer.EndUpdate;
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

procedure TgxFullScreenViewer.ReActivate;
begin
  Shutdown;
  Startup;
end;

procedure TgxFullScreenViewer.Notification(AComponent: TComponent;
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

function TgxFullScreenViewer.LastFrameTime: Single;
begin
  Result := Buffer.LastFrameTime;
end;

function TgxFullScreenViewer.FramesPerSecond: Single;
begin
  Result := Buffer.FramesPerSecond;
end;

function TgxFullScreenViewer.FramesPerSecondText(decimals: Integer): String;
begin
  Result := Format('%.*f FPS', [decimals, Buffer.FramesPerSecond]);
end;

procedure TgxFullScreenViewer.ResetPerformanceMonitor;
begin
  Buffer.ResetPerformanceMonitor;
end;

procedure TgxFullScreenViewer.UseCurrentResolution;
begin
  BeginUpdate;
  try
    Width := Trunc(Screen.Width);
    Height := Trunc(Screen.Height);
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

procedure TgxFullScreenViewer.SetActive(const val: Boolean);
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

procedure TgxFullScreenViewer.Startup;
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
    If BorderStyle <> TFmxFormBorderStyle.None then
      BorderStyle := TFmxFormBorderStyle.None;
    { TODO : E2003 Undeclared identifier: 'Cursor' }
    (*Cursor := Self.Cursor;*)
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
      FormStyle := TFormStyle.StayOnTop
    else
      FormStyle := TFormStyle.Normal;
     { TODO : E2010 Incompatible types: 'HWND' and 'TWindowHandle' }
     (*
    SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and
      not WS_CAPTION);
     *)
    // WindowState:=wsMaximized;
    // Switch video mode
    if (Screen.Width <> Width) or (Screen.Height <> Height) or
      (GetCurrentColorDepth <> cScreenDepthToBPP[ScreenDepth]) then
    begin
      SetFullscreenMode(res, FRefreshRate);
      FSwitchedResolution := True;
    end;
    // Hides Taskbar + Windows 7 Button
    ShowWindow(FindWindow('Shell_TrayWnd', nil), SW_HIDE);
    ShowWindow(FindWindow('BUTTON', nil), SW_HIDE);
    // Show;
  end;

  Buffer.Resize(0, 0, Width, Height);
  { TODO : E2010 Incompatible types: 'HWND' and 'TWindowHandle' }
  (*FOwnDC := GetDC(FForm.Handle);*)
  Buffer.CreateRC(FOwnDC, False);
  // Linux Unicode
{$IFDEF Linux}
  GrabMouseToForm(FForm);
{$ENDIF}
  // todo
  FActive := True;
end;

procedure TgxFullScreenViewer.Shutdown;
begin
  if not FActive then
    Exit;
  Assert(FForm <> nil);

  Buffer.DestroyRC;
  with FForm do
  begin
    { TODO : E2003 Undeclared identifier: 'crDefault' }
    (*Cursor := crDefault;*)
    PopupMenu := nil;
  end;
  // Restore Taskbar + Windows 7 Button
  ShowWindow(FindWindow('Shell_TrayWnd', nil), SW_SHOWNA);
  ShowWindow(FindWindow('BUTTON', nil), SW_SHOWNA);
  // attempt that, at the very least...
  if FSwitchedResolution then
    RestoreDefaultMode;
  FActive := False;

  if FFormIsOwned then
    FreeAndNil(FForm);
end;

procedure TgxFullScreenViewer.BindFormEvents;
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
      { TODO : E2009 Incompatible types: 'Parameter lists differ' }
      (*OnPaint := DoPaint;*)
      OnDestroy := DoFormDestroy;
    end;
end;

procedure TgxFullScreenViewer.DoCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if Assigned(FOnCloseQuery) then
    FOnCloseQuery(Sender, CanClose);
  CanClose := True;
  // if CanClose then Shutdown;
end;

procedure TgxFullScreenViewer.DoPaint(Sender: TObject);
begin
  If not ManualRendering then
    if Form <> nil then
      Render;
end;

procedure TgxFullScreenViewer.DoActivate(Sender: TObject);
begin
  (* If not Active and (Form <> nil) then begin
    Startup;
    end; *)
end;

procedure TgxFullScreenViewer.DoDeactivate(Sender: TObject);
begin
  (* If Active and (Form <> nil) then begin
    Shutdown;
    Form.Height:=0;
    Form.Width:=0;
    end; *)
end;

procedure TgxFullScreenViewer.DoFormDestroy(Sender: TObject);
begin
  Active := False;
end;

procedure TgxFullScreenViewer.SetScreenDepth(const val: TgxScreenDepth);
begin
  if FScreenDepth <> val then
  begin
    FScreenDepth := val;
    DoBufferStructuralChange(Self);
  end;
end;

procedure TgxFullScreenViewer.SetStayOnTop(const val: Boolean);
begin
  if val <> FStayOnTop then
  begin
    FStayOnTop := val;
    DoBufferStructuralChange(Self);
  end;
end;

procedure TgxFullScreenViewer.SetOnCloseQuery(const val: TCloseQueryEvent);
begin
  FOnCloseQuery := val; // this one uses a special binding
end;

procedure TgxFullScreenViewer.SetOnClose(const val: TCloseEvent);
begin
  If Form <> nil then
    Form.OnClose := val;
  FOnClose := val;
end;

procedure TgxFullScreenViewer.SetOnKeyPress(const val: TKeyEvent); //VCL - TKeyPressEvent
begin
  If Form <> nil then
    Form.OnKeyDown := val;
  FOnKeyPress := val;
end;

procedure TgxFullScreenViewer.SetOnKeyUp(const val: TKeyEvent);
begin
  If Form <> nil then
    Form.OnKeyUp := val;
  FOnKeyUp := val;
end;

procedure TgxFullScreenViewer.SetOnKeyDown(const val: TKeyEvent);
begin
  If Form <> nil then
    Form.OnKeyDown := val;
  FOnKeyDown := val;
end;

procedure TgxFullScreenViewer.SetOnMouseWheel(const val: TMouseWheelEvent);
begin
  If Form <> nil then
    Form.OnMouseWheel := val;
  FOnMouseWheel := val;
end;

procedure TgxFullScreenViewer.SetOnMouseWheelDown
  (const val: TMouseWheelEvent);
begin
  If Form <> nil then
    Form.OnMouseWheel := val;
  FOnMouseWheelDown := val;
end;

procedure TgxFullScreenViewer.SetOnMouseWheelUp(const val: TMouseWheelEvent);
begin
  If Form <> nil then
    Form.OnMouseWheel := val;
  FOnMouseWheelUp := val;
end;

procedure TgxFullScreenViewer.SetOnClick(const val: TNotifyEvent);
begin
  If Form <> nil then
    { TODO : E2003 Undeclared identifier: 'OnClick' }
    (*Form.OnClick := val;*)
  FOnClick := val;
end;

procedure TgxFullScreenViewer.SetOnDblClick(const val: TNotifyEvent);
begin
  If Form <> nil then
    { TODO : E2003 Undeclared identifier: 'OnDblClick' }
    (*Form.OnDblClick := val;*)
  FOnDblClick := val;
end;

procedure TgxFullScreenViewer.SetOnMouseMove(const val: TMouseMoveEvent);
begin
  If Form <> nil then
    Form.OnMouseMove := val;
  FOnMouseMove := val;
end;

procedure TgxFullScreenViewer.SetOnMouseDown(const val: TMouseEvent);
begin
  If Form <> nil then
    Form.OnMouseDown := val;
  FOnMouseDown := val;
end;

procedure TgxFullScreenViewer.SetOnMouseUp(const val: TMouseEvent);
begin
  If Form <> nil then
    Form.OnMouseUp := val;
  FOnMouseUp := val;
end;

(*
procedure TgxFullScreenViewer.SetCursor(const val: TCursor);
begin
  if val <> FCursor then
  begin
    FCursor := val;
    if Form <> nil then
      FForm.Cursor := val;
  end;
end;
*)

procedure TgxFullScreenViewer.SetPopupMenu(const val: TPopupMenu);
begin
  if val <> FPopupMenu then
  begin
    FPopupMenu := val;
    if Assigned(FForm) then
      { TODO : E2003 Undeclared identifier: 'PopupMenu' }
      (*FForm.PopupMenu := val;*)
  end;
end;

procedure TgxFullScreenViewer.SetForm(aVal: TForm);
begin
  FForm := aVal;
end;

procedure TgxFullScreenViewer.SetManualRendering(const val: Boolean);
begin
  if FManualRendering <> val then
    FManualRendering := val;
end;

function TgxFullScreenViewer.GetHandle: TWindowHandle;
begin
  if Form <> nil then
    Result := FForm.Handle
  else
    Result := nil;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

RegisterClasses([TgxFullScreenViewer]);

finalization

// Restore Taskbar + Windows 7 Button
ShowWindow(FindWindow('Shell_TrayWnd', nil), SW_SHOWNA);
ShowWindow(FindWindow('BUTTON', nil), SW_SHOWNA);

end.
