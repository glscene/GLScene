//
// The graphics engine GXScene https://github.com/glscene
//
unit SDLx.Context;

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

  SDL.Import,
  GXS.OpenGL,
  GXS.XOpenGL,
  GXS.Scene,
  GXS.Context,
  SDLx.Window;

type

  (* A viewer using SDL.

  Beware: only one at a time, no other viewers allowed!
  Will also close the application when the window is closed! *)
  TgxSDLViewer = class(TgxNonVisualViewer)
  private
    FCaption: string;
    FOnSDLEvent: TgxSDLEvent;
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
    procedure PrepareVXContext; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render(baseObject: TgxBaseSceneObject = nil); override;
    function Active: Boolean;
  published
    property Caption: string read FCaption write SetCaption;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    { Fired whenever an SDL Event is polled.
       SDL_QUITEV and SDL_VIDEORESIZE are not passed to this event handler,
       they are passed via OnClose and OnResize respectively. }
    property OnSDLEvent: TgxSDLEvent read FOnSDLEvent write FOnSDLEvent;
    { Fired whenever an event polling completes with no events left to poll. }
    property OnEventPollDone: TNotifyEvent read FOnEventPollDone write FOnEventPollDone;
  end;

  { A context driver for OpenGL via SDL (libsdl.org).
     Due to limitations of SDL:
      you may have only one SDL window opened at any time (you cannot
        have memory viewers)
      closing the SDL window will terminate the application }
  TgxSDLContext = class(TgxScreenControlingContext)
  private
    FSDLWin: TgxSDLWindow;
    FSimulatedValidity: Boolean; // Hack around SDL's post-notified destruction of context
  protected
    procedure DoCreateContext(outputDevice : THandle); override;
    procedure DoCreateMemoryContext(OutputDevice:THandle; Width, Height: Integer; BufferCount: Integer = 1); override;
    function DoShareLists(aContext: TgxContext): Boolean; override;
    procedure DoDestroyContext; override;
    procedure DoActivate; override;
    procedure DoDeactivate; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function IsValid: Boolean; override;
    procedure SwapBuffers; override;
    function RenderOutputDevice: Pointer; override;
    property SDLWindow: TgxSDLWindow read FSDLWin;
  end;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GXScene', [TgxSDLViewer]);
end;

// ------------------
// ------------------ TgxSDLViewer ------------------
// ------------------

constructor TgxSDLViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 640;
  Height := 480;
end;

destructor TgxSDLViewer.Destroy;
begin
  inherited Destroy;
end;

procedure TgxSDLViewer.DoBufferStructuralChange(Sender: TObject);
begin
  // ignore that, supporting it with SDL is not very praticable as of now...
end;

procedure TgxSDLViewer.PrepareVXContext;
begin
  with Buffer.RenderingContext as TgxSDLContext do
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

procedure TgxSDLViewer.Render(baseObject: TgxBaseSceneObject = nil);
begin
  InitOpenGL;
  if Buffer.RenderingContext = nil then
  begin
    Buffer.CreateRC(0, False);
  end;
  Buffer.Render(baseObject);
end;

function TgxSDLViewer.Active: Boolean;
begin
  Result := Assigned(Buffer.RenderingContext) and Buffer.RenderingContext.IsValid;
end;

procedure TgxSDLViewer.SetCaption(const val: string);
begin
  if val <> FCaption then
  begin
    FCaption := val;
    if Buffer.RenderingContext <> nil then
      with Buffer.RenderingContext as TgxSDLContext do
        if Assigned(FSDLWin) then
          FSDLWin.Caption := FCaption;
  end;
end;

procedure TgxSDLViewer.DoOnOpen(sender: TObject);
begin
  // nothing yet
end;

procedure TgxSDLViewer.DoOnClose(sender: TObject);
begin
  // nothing yet
end;

procedure TgxSDLViewer.DoOnResize(sender: TObject);
begin
  with Buffer.RenderingContext as TgxSDLContext do
  begin
    Self.Width := FSDLWin.Width;
    Self.Height := FSDLWin.Height;
    Buffer.Resize(0, 0, FSDLWin.Width, FSDLWin.Height);
  end;
  if Assigned(FOnResize) then
    FOnResize(Self);
end;

procedure TgxSDLViewer.DoOnSDLEvent(sender: TObject; const event: TSDL_Event);
begin
   if Assigned(FOnSDLEvent) then
      FOnSDLEvent(sender, event);
end;

procedure TgxSDLViewer.DoOnEventPollDone(sender: TObject);
begin
  if Assigned(FOnEventPollDone) then
    FOnEventPollDone(sender);
end;

// ------------------
// ------------------ TgxSDLContext ------------------
// ------------------

constructor TgxSDLContext.Create;
begin
  inherited Create;
  FSDLWin := TgxSDLWindow.Create(nil);
end;

destructor TgxSDLContext.Destroy;
var
  oldIgnore: Boolean;
begin
  oldIgnore := vIgnoreOpenGXErrors;
  FSimulatedValidity := True;
  vIgnoreOpenGXErrors := True;
  try
    inherited Destroy;
  finally
    vIgnoreOpenGXErrors := oldIgnore;
    FSimulatedValidity := False;
  end;
  FreeAndNil(FSDLWin);
end;

procedure TgxSDLContext.DoCreateContext(outputDevice: THandle);
var
  sdlOpt: TgxSDLWindowOptions;
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

  sdlOpt := [voOpenGL];
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

   xglMapTexCoordToNull;
   ReadExtensions;
   ReadImplementationProperties;
   xglMapTexCoordToMain;
end;

procedure TgxSDLContext.DoCreateMemoryContext(OutputDevice: THandle; Width, Height: // VCL ->HWND
      Integer; BufferCount: Integer = 1);
begin
  raise Exception.Create(ClassName + ': Memory contexts not supported');
end;

function TgxSDLContext.DoShareLists(aContext: TgxContext): Boolean;
begin
  // nothing (only one context at all times... no need to share)
  Result := False;
end;

procedure TgxSDLContext.DoDestroyContext;
begin
   // Beware, SDL will also terminate the application
   FSDLWin.Close;
end;

procedure TgxSDLContext.DoActivate;
begin
   // nothing particular (only one context, always active)
end;

procedure TgxSDLContext.DoDeactivate;
begin
  // nothing particular (only one context, always active)
end;

function TgxSDLContext.IsValid: Boolean;
begin
  Result := (Assigned(FSDLWin) and (FSDLWin.Active)) or FSimulatedValidity;
end;

procedure TgxSDLContext.SwapBuffers;
begin
  FSDLWin.SwapBuffers;
end;

function TgxSDLContext.RenderOutputDevice: Pointer;
begin
  // unsupported
  Result := nil;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterClass(TgxSDLViewer);
  RegisterVXContextClass(TgxSDLContext);

end.

