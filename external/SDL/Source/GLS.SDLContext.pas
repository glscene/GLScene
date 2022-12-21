//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.SDLContext;

(*
   SDL specific Context and Viewer.
   NOTA: SDL notifies use of context destruction *after* it happened, this prevents
         clean release of allocated stuff and requires a temporary switch to
         "ignore OpenGL errors" mode during destruction, thus potentially
         leaking memory (depending on hardware drivers willingness to perform
         automatic releases)
*)

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,

  GLS.OpenGLTokens,
  GLS.OpenGLAdapter,
  GLS.XOpenGL,
  GLS.Context,
  GLS.Scene,

  SDL2.Import,
  GLS.SDLWindow;

type
  (* A viewer using SDL.
     Beware: only one at a time, no other viewers allowed!
     Will also close the application when the window is closed! *)
  TSDLViewer = class(TGLNonVisualViewer)
  private
    FCaption: string;
    FOnSDLEvent: TSDLEvent;
    FOnEventPollDone: TNotifyEvent;
    FOnResize: TNotifyEvent;
  protected
    procedure SetCaption(const val: string);
    procedure DoOnOpen(sender: TObject);
    procedure DoOnClose(sender: TObject);
    procedure DoOnResize(sender: TObject);
    procedure DoOnSDLEvent(sender: TObject; const event: TSDL_Event);
    procedure DoOnEventPollDone(sender: TObject);
    procedure DoBufferStructuralChange(Sender: TObject); override;
    procedure PrepareGLContext; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render(baseObject: TGLBaseSceneObject = nil); override;
    function Active: Boolean;
  published
    property Caption: string read FCaption write SetCaption;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    (* Fired whenever an SDL Event is polled.
     SDL_QUITEV and SDL_VIDEORESIZE are not passed to this event handler,
     they are passed via OnClose and OnResize respectively. *)
    property OnSDLEvent: TSDLEvent read FOnSDLEvent write FOnSDLEvent;
    // Fired whenever an event polling completes with no events left to poll.
    property OnEventPollDone: TNotifyEvent read FOnEventPollDone write FOnEventPollDone;
  end;

  (* A context driver for OpenGL via SDL (libsdl.org).
     Due to limitations of SDL:
     you may have only one SDL window opened at any time (you cannot have memory viewers)
     closing the SDL window will terminate the application   *)
  TSDLContext = class(TGLScreenControlingContext)
  private
    FSDLWin: TSDLWindow;
    FSimulatedValidity: Boolean; // Hack around SDL's post-notified destruction of context
  protected
    procedure DoCreateContext(outputDevice: HDC); override;
    procedure DoCreateMemoryContext(outputDevice: HWND; width, height: Integer; BufferCount: integer); override;
    function DoShareLists(aContext: TGLContext): Boolean; override;
    procedure DoDestroyContext; override;
    procedure DoActivate; override;
    procedure DoDeactivate; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function IsValid: Boolean; override;
    procedure SwapBuffers; override;
    function RenderOutputDevice: Pointer; override;
    property SDLWindow: TSDLWindow read FSDLWin;
  end;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TSDLViewer ------------------
// ------------------

constructor TSDLViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 640;
  Height := 480;
end;

destructor TSDLViewer.Destroy;
begin
  inherited Destroy;
end;

procedure TSDLViewer.DoBufferStructuralChange(Sender: TObject);
begin
  // ignore that, supporting it with SDL is not very praticable as of now...
end;

procedure TSDLViewer.PrepareGLContext;
begin
  with Buffer.RenderingContext as TSDLContext do
  begin
    Width := Self.Width;
    Height := Self.Height;
    with FSDLWin do
    begin
      Caption := Self.Caption;
      OnOpen := DoOnOpen;
      OnClose := DoOnClose;
      OnResize := DoOnResize;
      OnSDLEvent := DoOnSDLEvent;
      OnEventPollDone := DoOnEventPollDone;
    end;
  end;
end;

procedure TSDLViewer.Render(baseObject: TGLBaseSceneObject = nil);
begin
  LoadOpenGL;
  if Buffer.RenderingContext = nil then
  begin
    Buffer.CreateRC(0, False);
  end;
  Buffer.Render(baseObject);
end;

function TSDLViewer.Active: Boolean;
begin
  Result := Assigned(Buffer.RenderingContext) and Buffer.RenderingContext.IsValid;
end;

procedure TSDLViewer.SetCaption(const val: string);
begin
  if val <> FCaption then
  begin
    FCaption := val;
    if Buffer.RenderingContext <> nil then
      with Buffer.RenderingContext as TSDLContext do
        if Assigned(FSDLWin) then
          FSDLWin.Caption := FCaption;
  end;
end;

procedure TSDLViewer.DoOnOpen(sender: TObject);
begin
  // nothing yet
end;

procedure TSDLViewer.DoOnClose(sender: TObject);
begin
  // nothing yet
end;

procedure TSDLViewer.DoOnResize(sender: TObject);
begin
  with Buffer.RenderingContext as TSDLContext do
  begin
    Self.Width := FSDLWin.Width;
    Self.Height := FSDLWin.Height;
    Buffer.Resize(0, 0, FSDLWin.Width, FSDLWin.Height);
  end;
  if Assigned(FOnResize) then
    FOnResize(Self);
end;

procedure TSDLViewer.DoOnSDLEvent(sender: TObject; const event: TSDL_Event);
begin
  if Assigned(FOnSDLEvent) then
    FOnSDLEvent(sender, event);
end;

procedure TSDLViewer.DoOnEventPollDone(sender: TObject);
begin
  if Assigned(FOnEventPollDone) then
    FOnEventPollDone(sender);
end;

// ------------------
// ------------------ TSDLContext ------------------
// ------------------

constructor TSDLContext.Create;
begin
  inherited Create;
  FSDLWin := TSDLWindow.Create(nil);
end;

destructor TSDLContext.Destroy;
var
  oldIgnore: Boolean;
begin
  oldIgnore := vIgnoreOpenGLErrors;
  FSimulatedValidity := True;
  vIgnoreOpenGLErrors := True;
  try
    inherited Destroy;
  finally
    vIgnoreOpenGLErrors := oldIgnore;
    FSimulatedValidity := False;
  end;
  FreeAndNil(FSDLWin);
end;

procedure TSDLContext.DoCreateContext(outputDevice: HDC);
var
  sdlOpt: TSDLWindowOptions;
begin
  // Just in case it didn't happen already.
  if not InitOpenGL then
    RaiseLastOSError;

  FSDLWin.Width := Width;
  FSDLWin.Height := Height;
  if ColorBits > 16 then
    FSDLWin.PixelDepth := vpd24bits
  else
    FSDLWin.PixelDepth := vpd16bits;

  sdlOpt := [voOpenGL, voHardwareAccel];
  if FullScreen then
    sdlOpt := sdlOpt + [voFullScreen]
  else
    sdlOpt := sdlOpt + [voResizable];
  if rcoDoubleBuffered in Options then
    sdlOpt := sdlOpt + [voDoubleBuffer];
  if StencilBits > 0 then
    sdlOpt := sdlOpt + [voStencilBuffer];

  FSDLWin.Open;
  if not FSDLWin.Active then
    raise Exception.Create('SDLWindow open failed.');

  FGL.Initialize;
  MakeGLCurrent;
end;

procedure TSDLContext.DoCreateMemoryContext(outputDevice: HWND; width, height: Integer; BufferCount: integer);
begin
  raise Exception.Create(ClassName + ': Memory contexts not supported');
end;

function TSDLContext.DoShareLists(aContext: TGLContext): Boolean;
begin
  // nothing (only one context at all times... no need to share)
  Result := False;
end;

procedure TSDLContext.DoDestroyContext;
begin
  // Beware, SDL will also terminate the application
  FGL.Close;
  FSDLWin.Close;
end;

procedure TSDLContext.DoActivate;
begin
  if not FGL.IsInitialized then
    FGL.Initialize;
end;

procedure TSDLContext.DoDeactivate;
begin
  // nothing particular (only one context, always active)
end;

function TSDLContext.IsValid: Boolean;
begin
  Result := (Assigned(FSDLWin) and (FSDLWin.Active)) or FSimulatedValidity;
end;

procedure TSDLContext.SwapBuffers;
begin
  FSDLWin.SwapBuffers;
end;

function TSDLContext.RenderOutputDevice: Pointer;
begin
  // unsupported
  Result := nil;
end;


procedure Register;
begin
  RegisterComponents('GLScene', [TSDLViewer]);
end;


// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterClass(TSDLViewer);
  RegisterGLContextClass(TSDLContext);

end.

