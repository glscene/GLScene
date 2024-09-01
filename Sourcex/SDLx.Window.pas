//
// The graphics engine GXScene https://github.com/glscene
//
unit SDLx.Window;

(*
  Non visual wrapper around basic SDL window features.
  Notes to Self:
  Unit must ultimately *NOT* make use of any platform specific stuff,
  *EVEN* through the use of conditionnals.
  SDL-specifics should also be avoided in the "interface" section.
  This component uses a Delphi header conversion for SDL from http://libsdl.org
*)

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,

  GXS.OpenGL,
  GXS.VectorTypes,
  GXS.Context,
  GXS.VectorGeometry,
  SDL.Import;

type
  (* Pixel Depth options.
    vpd16bits: 16bpp graphics (565) (and 16 bits depth buffer for OpenGL)
    vpd24bits: 24bpp graphics (565) (and 24 bits depth buffer for OpenGL) *)
  TgxSDLWindowPixelDepth = (vpd16bits, vpd24bits);

  (*  Specifies optional settings for the SDL window.
    Those options are a simplified subset of the SDL options:
     voDoubleBuffer: create a double-buffered window
     voOpenGL: requires OpenGL capability for the window
     voResizable: window should be resizable
     voFullScreen: requires a full screen "window" (screen resolution may be changed)
     voStencilBuffer: requires a stencil buffer (8bits, use along voOpenGL)  *)
  TgxSDLWindowOption = (voDoubleBuffer, voOpenGL, voResizable, voFullScreen, voStencilBuffer);
  TgxSDLWindowOptions = set of TgxSDLWindowOption;
  TgxSDLEvent = procedure(sender: TObject; const event: TSDL_Event) of object;

const
  cDefaultSDLWindowOptions = [voDoubleBuffer, voOpenGL, voResizable];

type
  (*  A basic SDL-based window (non-visual component).
    Only a limited subset of SDL's features are available, and this window
    is heavily oriented toward using it for OpenGL rendering.
    Be aware SDL is currently limited to a single window at any time...
    so you may have multiple components, but only one can be used. *)
  TgxSDLWindow = class(TComponent)
  private
    FWidth: Integer;
    FHeight: Integer;
    FPixelDepth: TgxSDLWindowPixelDepth;
    FOptions: TgxSDLWindowOptions;
    FActive: Boolean;
    FOnOpen: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnResize: TNotifyEvent;
    FOnSDLEvent: TgxSDLEvent;
    FOnEventPollDone: TNotifyEvent;
    FCaption: String;
    FThreadSleepLength: Integer;
    FThreadPriority: TThreadPriority;
    FThreadedEventPolling: Boolean;
    FThread: TThread;
    FSDLSurface: PSDL_Surface;
    FWindowHandle: Longword;
    FSDLWindow: PSDL_Window;
  protected
    procedure SetWidth(const val: Integer);
    procedure SetHeight(const val: Integer);
    procedure SetPixelDepth(const val: TgxSDLWindowPixelDepth);
    procedure SetOptions(const val: TgxSDLWindowOptions);
    procedure SetActive(const val: Boolean);
    procedure SetCaption(const val: String);
    procedure SetThreadSleepLength(const val: Integer);
    procedure SetThreadPriority(const val: TThreadPriority);
    procedure SetThreadedEventPolling(const val: Boolean);
    function BuildSDLVideoFlags: Cardinal;
    procedure SetSDLGLAttributes;
    procedure CreateOrRecreateSDLSurface;
    procedure ResizeGLWindow;
    procedure SetupSDLEnvironmentValues;
    procedure StartThread;
    procedure StopThread;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //  Initializes and Opens an SDL window
    procedure Open;
    (*  Closes an already opened SDL Window.
      NOTE: will also kill the app due to an SDL limitation... *)
    procedure Close;
    //  Applies changes (size, pixeldepth...) to the opened window.
    procedure UpdateWindow;
    //  Swap front and back buffer.
    procedure SwapBuffers;
    (* Polls SDL events.
      SDL events can be either polled "manually", through a call to this
      method, or automatically via ThreadEventPolling. *)
    procedure PollEvents;
    (*  Is the SDL window active (opened)?
      Adjusting this value as the same effect as invoking Open/Close. *)
    property Active: Boolean read FActive write SetActive;
    (*  Presents the SDL surface of the window.
      If Active is False, this value is undefined. *)
    property SDLSurface: PSDL_Surface read FSDLSurface;
    //  Experimental: ask SDL to reuse and existing WindowHandle
    property WindowHandle: Cardinal read FWindowHandle write FWindowHandle;
    //  Presents the SDL window. If Active is False, this value is undefined.
    property SDLWindow: PSDL_Window read FSDLWindow;
  published
    // Width of the SDL window.To apply changes to an active window, call UpdateWindow
    property Width: Integer read FWidth write SetWidth default 640;
    //  Height of the SDL window. To apply changes to an active window, call UpdateWindow.
    property Height: Integer read FHeight write SetHeight default 480;
    //  PixelDepth of the SDL window. To apply changes to an active window, call UpdateWindow.
    property PixelDepth: TgxSDLWindowPixelDepth read FPixelDepth write SetPixelDepth default vpd24bits;
    // Options for the SDL window. To apply changes to an active window, call UpdateWindow.
    property Options: TgxSDLWindowOptions read FOptions write SetOptions default cDefaultSDLWindowOptions;
    // Caption of the SDL window
    property Caption: String read FCaption write SetCaption;
    // Controls automatic threaded event polling.
    property ThreadedEventPolling: Boolean read FThreadedEventPolling write SetThreadedEventPolling default True;
    // Sleep length between pollings in the polling thread.
    property ThreadSleepLength: Integer read FThreadSleepLength write SetThreadSleepLength default 1;
    //  Priority of the event polling thread.
    property ThreadPriority: TThreadPriority read FThreadPriority write SetThreadPriority default tpLower;
    // Fired whenever Open succeeds. The SDL surface is defined and usable when the event happens.
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    // Fired whenever closing the window. The SDL surface is still defined and usable when the event happens.
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    // Fired whenever the window is resized. Note: glViewPort call is handled automatically for OpenGL windows
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    (*  Fired whenever an SDL Event is polled.
      SDL_QUITEV and SDL_VIDEORESIZE are not passed to this event handler,
      they are passed via OnClose and OnResize respectively. *)
    property OnSDLEvent: TgxSDLEvent read FOnSDLEvent write FOnSDLEvent;
    // Fired whenever an event polling completes with no events left to poll.
    property OnEventPollDone: TNotifyEvent read FOnEventPollDone write FOnEventPollDone;
  end;

  // Generic SDL or SDLWindow exception.
  ESDLError = class(Exception);

{------------------------------------------------------------------------------}
{ Get Environment Routines                                                     }
{------------------------------------------------------------------------------}
{$IFDEF WINDOWS}
function _putenv( const variable : PAnsiChar ): integer; cdecl;
{$ENDIF}

{ Put a variable of the form "name=value" into the environment }
//function SDL_putenv(const variable: PAnsiChar): integer; cdecl; external LibName;
function SDL_putenv(const variable: PAnsiChar): integer;

// The following function has been commented out to encourage developers to use
// SDL_putenv as it it more portable
//function putenv(const variable: PAnsiChar): integer;

{$IFDEF WINDOWS}
function getenv( const name : PAnsiChar ): PAnsiChar; cdecl;
{$ENDIF}

{* Retrieve a variable named "name" from the environment }
//function SDL_getenv(const name: PAnsiChar): PAnsiChar; cdecl; external LibName;
function SDL_getenv(const name: PAnsiChar): PAnsiChar;

// The following function has been commented out to encourage developers to use
// SDL_getenv as it it more portable
//function getenv(const name: PAnsiChar): PAnsiChar;

// ---------------------------------------------------------------------
procedure Register;
// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------

var
  vSDLCS: TCriticalSection;
  vSDLActive: Boolean; // will be removed once SDL supports multiple windows

type
  TSDLEventThread = class(TThread)
    Owner: TgxSDLWindow;
    procedure Execute; override;
    procedure DoPollEvents;
  end;

procedure RaiseSDLError(const msg: String = '');
begin
  if msg <> '' then
    raise ESDLError.Create(msg + #13#10 + SDL_GetError)
  else
    raise ESDLError.Create(SDL_GetError);
end;

{$IFDEF WINDOWS}
function _putenv( const variable : PAnsiChar ): Integer; cdecl; external 'MSVCRT.DLL';
{$ENDIF}

function SDL_putenv(const variable: PAnsiChar): Integer;
begin
  {$IFDEF WINDOWS}
  Result := _putenv(variable);
  {$ENDIF}

  {$IFDEF UNIX}
  Result := libc.putenv(variable);
  {$ENDIF}
end;

{$IFDEF WINDOWS}
function getenv( const name : PAnsiChar ): PAnsiChar; cdecl; external 'MSVCRT.DLL';
{$ENDIF}

function SDL_getenv(const name: PAnsiChar): PAnsiChar;
begin
  {$IFDEF WINDOWS}
  Result := getenv(name);
  {$ENDIF}

  {$IFDEF UNIX}
  Result := libc.getenv(name);
  {$ENDIF}
end;


// ------------------
// ------------------ TSDLEventThread ------------------
// ------------------

procedure TSDLEventThread.Execute;
begin
  try
    while not Terminated do
    begin
      vSDLCS.Enter;
      try
        SDL_Delay(Owner.ThreadSleepLength);
      finally
        vSDLCS.Leave;
      end;
      Synchronize(DoPollEvents);
    end;
  except
    // bail out asap, problem wasn't here anyway
  end;
  vSDLCS.Enter;
  try
    if Assigned(Owner) then
      Owner.FThread := nil;
  finally
    vSDLCS.Leave;
  end;
end;

procedure TSDLEventThread.DoPollEvents;
begin
  // no need for a CS here, we're in the main thread
  if Assigned(Owner) then
    Owner.PollEvents;
end;

// ------------------
// ------------------ TSDLWindow ------------------
// ------------------

constructor TgxSDLWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidth := 640;
  FHeight := 480;
  FPixelDepth := vpd24bits;
  FThreadedEventPolling := True;
  FThreadSleepLength := 1;
  FThreadPriority := tpLower;
  FOptions := cDefaultSDLWindowOptions;
end;

destructor TgxSDLWindow.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TgxSDLWindow.SetWidth(const val: Integer);
begin
  if FWidth <> val then
    if val > 0 then
      FWidth := val;
end;

procedure TgxSDLWindow.SetHeight(const val: Integer);
begin
  if FHeight <> val then
    if val > 0 then
      FHeight := val;
end;

procedure TgxSDLWindow.SetPixelDepth(const val: TgxSDLWindowPixelDepth);
begin
  FPixelDepth := val;
end;

procedure TgxSDLWindow.SetOptions(const val: TgxSDLWindowOptions);
begin
  FOptions := val;
end;

function TgxSDLWindow.BuildSDLVideoFlags: Cardinal;
var
  videoInfo: PSDL_RendererInfo;
begin
  SDL_GetRendererInfo(Self, videoInfo);

  if not Assigned(videoInfo) then
    raise ESDLError.Create('Video query failed.');

  Result := 0;
  if voOpenGL in Options then
    Result := Result + SDL_WINDOW_OPENGL;
  if voDoubleBuffer in Options then
    Result := Result + SDL_GL_DOUBLEBUFFER;
  if voResizable in Options then
    Result := Result + SDL_WINDOW_RESIZABLE;
  if voFullScreen in Options then
    Result := Result + SDL_WINDOW_FULLSCREEN;
  if voStencilBuffer in Options then
    Result := Result + SDL_SWSURFACE;  //for compatibility with SDL 1.2 only!
end;

procedure TgxSDLWindow.SetSDLGLAttributes;
begin
  case PixelDepth of
    vpd16bits:
      begin
        SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 5);
        SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 6);
        SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 5);
        SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 16);
      end;
    vpd24bits:
      begin
        SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
        SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
        SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
        SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);
      end;
  else
    Assert(False);
  end;
  if voStencilBuffer in Options then
    SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8)
  else
    SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 0);
  if voDoubleBuffer in Options then
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1)
  else
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 0)
end;

procedure TgxSDLWindow.CreateOrRecreateSDLSurface;
const
  cPixelDepthToBpp: array [Low(TgxSDLWindowPixelDepth) .. High(TgxSDLWindowPixelDepth)] of Integer = (16, 24);
var
  videoFlags: Integer;
begin
  videoFlags := BuildSDLVideoFlags;
  if voOpenGL in Options then
    SetSDLGLAttributes;
  {
  SDL_WM_SetCaption(PAnsiChar(AnsiString(FCaption)), nil);
  FSDLSurface := SDL_SetVideoMode(Width, Height, cPixelDepthToBpp[PixelDepth], videoFlags);
  }
  FSDLWindow := SDL_CreateWindow(PChar(AnsiString(FCaption)),
                          SDL_WINDOWPOS_UNDEFINED,
                          SDL_WINDOWPOS_UNDEFINED,
                          Width, Height,
                          videoFlags);
  if not Assigned(FSDLSurface) then
    RaiseSDLError('Unable to create surface.');

  if voOpenGL in Options then
    ResizeGLWindow;
end;

procedure TgxSDLWindow.SetupSDLEnvironmentValues;
var
  envVal: String;
begin
  if FWindowHandle <> 0 then
  begin
    envVal := '';
	
    SDL_putenv('SDL_VIDEODRIVER=windib');
    envVal := 'SDL_WINDOWID=' + IntToStr(Integer(FWindowHandle));

    SDL_putenv(PAnsiChar(AnsiString(envVal)));
  end;
end;

procedure TgxSDLWindow.Open;
begin
  if Active then
    Exit;
  if vSDLActive then
    raise ESDLError.Create('Only one SDL window can be opened at a time...')
  else
    vSDLActive := True;

  if SDL_Init(SDL_INIT_VIDEO) < 0 then
    raise ESDLError.Create('Could not initialize SDL.');
  if voOpenGL in Options then
    InitOpenGL;
  SetupSDLEnvironmentValues;
  CreateOrRecreateSDLSurface;
  FActive := True;
  if Assigned(FOnOpen) then
    FOnOpen(Self);
  if Assigned(FOnResize) then
    FOnResize(Self);
  if ThreadedEventPolling then
    StartThread;
end;

procedure TgxSDLWindow.Close;
begin
  if not Active then
    Exit;
  if Assigned(FOnClose) then
    FOnClose(Self);
  FActive := False;
  StopThread;
  SDL_Quit; // SubSystem(SDL_INIT_VIDEO);
  FSDLSurface := nil;
  vSDLActive := False;
end;

procedure TgxSDLWindow.UpdateWindow;
begin
  if Active then
    CreateOrRecreateSDLSurface;
end;

procedure TgxSDLWindow.SwapBuffers;
begin
  if Active then
    if voOpenGL in Options then
      SDL_GL_SwapWindow(SDLWindow)
    else
      SDL_RenderPresent(SDLWindow);
end;

procedure TgxSDLWindow.ResizeGLWindow;
var
  RC: TgxContext;
begin
  RC := CurrentContext;
  if Assigned(RC) then
    RC.gxStates.ViewPort := Vector4iMake(0, 0, Width, Height);
end;

procedure TgxSDLWindow.SetActive(const val: Boolean);
begin
  if val <> FActive then
    if val then
      Open
    else
      Close;
end;

procedure TgxSDLWindow.SetCaption(const val: String);
begin
  if FCaption <> val then
  begin
    FCaption := val;
    if Active then
      SDL_SetWindowTitle(nil, PChar(AnsiString(FCaption)));
  end;
end;

procedure TgxSDLWindow.SetThreadSleepLength(const val: Integer);
begin
  if val >= 0 then
    FThreadSleepLength := val;
end;

procedure TgxSDLWindow.SetThreadPriority(const val: TThreadPriority);
begin
  FThreadPriority := val;
  if Assigned(FThread) then
    FThread.Priority := val;
end;

procedure TgxSDLWindow.SetThreadedEventPolling(const val: Boolean);
begin
  if FThreadedEventPolling <> val then
  begin
    FThreadedEventPolling := val;
    if ThreadedEventPolling then
    begin
      if Active and (not Assigned(FThread)) then
        StartThread;
    end
    else if Assigned(FThread) then
      StopThread;
  end;
end;

procedure TgxSDLWindow.StartThread;
begin
  if Active and ThreadedEventPolling and (not Assigned(FThread)) then
  begin
    FThread := TSDLEventThread.Create(True);
    TSDLEventThread(FThread).Owner := Self;
    FThread.Priority := ThreadPriority;
    FThread.FreeOnTerminate := True;
    FThread.Resume;
  end;
end;

procedure TgxSDLWindow.StopThread;
begin
  if Assigned(FThread) then
  begin
    vSDLCS.Enter;
    try
      TSDLEventThread(FThread).Owner := nil;
      FThread.Terminate;
    finally
      vSDLCS.Leave;
    end;
  end;
end;

procedure TgxSDLWindow.PollEvents;
var
  event: TSDL_Event;
begin
  if Active then
  begin
    while SDL_PollEvent(@event) > 0 do
    begin
      case event.type_ of
        SDL_QUITEV:
          begin
            Close;
            Break;
          end;
        SDL_WINDOWEVENT_RESIZED:
          begin
            FWidth := event.window.data1; //resize.w
            FHeight := event.window.data2; //resize.h
            if voOpenGL in Options then
              ResizeGLWindow
            else
            begin
              CreateOrRecreateSDLSurface;
              if not Assigned(FSDLSurface) then
                RaiseSDLError('Could not get a surface after resize.');
            end;
            if Assigned(FOnResize) then
              FOnResize(Self);
          end;
      else
        if Assigned(FOnSDLEvent) then
          FOnSDLEvent(Self, event);
      end;
    end;
    if Active then
      if Assigned(FOnEventPollDone) then
        FOnEventPollDone(Self);
  end;
end;

procedure Register;
begin
  RegisterComponents('GLScene Utils', [TgxSDLWindow]);
end;

// ---------------------------------------------------------------------
initialization
// ---------------------------------------------------------------------

// We DON'T free this stuff manually,
// automatic release will take care of this
vSDLCS := TCriticalSection.Create;

end.
