//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.Screen;

(* Routines to interact with the screen/desktop *)

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  VCL.Forms,

  GLS.VectorGeometry;

const
  MaxVideoModes = 200;
  lcl_release = 0;

type
  TResolution = 0 .. MaxVideoModes;

  // window attributes
  TWindowAttribute = (woDesktop, woStayOnTop, woTransparent);
  TWindowAttributes = set of TWindowAttribute;

  // window-to-screen fitting
  TWindowFitting = (wfDefault, wfFitWindowToScreen, wfFitScreenToWindow);

  TGLDisplayOptions = class(TPersistent)
  private
    FFullScreen: Boolean;
    FScreenResolution: TResolution;
    FWindowAttributes: TWindowAttributes;
    FWindowFitting: TWindowFitting;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property FullScreen: Boolean read FFullScreen write FFullScreen
      default False;
    property ScreenResolution: TResolution read FScreenResolution
      write FScreenResolution default 0;
    property WindowAttributes: TWindowAttributes read FWindowAttributes
      write FWindowAttributes default [];
    property WindowFitting: TWindowFitting read FWindowFitting
      write FWindowFitting default wfDefault;
  end;

  TVideoMode = packed record
    Width: Word;
    Height: Word;
    ColorDepth: Byte;
    MaxFrequency: Byte;
    Description: String;
  end;

  PVideoMode = ^TVideoMode;

function GetIndexFromResolution(XRes, YRes, BPP: Integer): TResolution;
procedure ReadVideoModes;

// Changes to the video mode given by 'Index'
function SetFullscreenMode(modeIndex: TResolution;
  displayFrequency: Integer = 0): Boolean;

procedure ReadScreenImage(Dest: HDC; DestLeft, DestTop: Integer;
  const SrcRect: TRectangle);
procedure RestoreDefaultMode;

procedure GLShowCursor(AShow: Boolean);
procedure GLSetCursorPos(AScreenX, AScreenY: Integer);
procedure GLGetCursorPos(var point: TPoint);
function GLGetScreenWidth: Integer;
function GLGetScreenHeight: Integer;

var
  vNumberVideoModes: Integer = 0;
  vCurrentVideoMode: Integer = 0;
  vVideoModes: array of TVideoMode;

// ------------------------------------------------------------------------------
implementation
// ------------------------------------------------------------------------------


type
  TLowResMode = packed record
    Width: Word;
    Height: Word;
    ColorDepth: Byte;
  end;

const
  NumberLowResModes = 15;
  LowResModes: array [0 .. NumberLowResModes - 1] of TLowResMode = ((Width: 320;
    Height: 200; ColorDepth: 8), (Width: 320; Height: 200; ColorDepth: 15),
    (Width: 320; Height: 200; ColorDepth: 16), (Width: 320; Height: 200;
    ColorDepth: 24), (Width: 320; Height: 200; ColorDepth: 32), (Width: 400;
    Height: 300; ColorDepth: 8), (Width: 400; Height: 300; ColorDepth: 15),
    (Width: 400; Height: 300; ColorDepth: 16), (Width: 400; Height: 300;
    ColorDepth: 24), (Width: 400; Height: 300; ColorDepth: 32), (Width: 512;
    Height: 384; ColorDepth: 8), (Width: 512; Height: 384; ColorDepth: 15),
    (Width: 512; Height: 384; ColorDepth: 16), (Width: 512; Height: 384;
    ColorDepth: 24), (Width: 512; Height: 384; ColorDepth: 32));

procedure TGLDisplayOptions.Assign(Source: TPersistent);
begin
  if Source is TGLDisplayOptions then
  begin
    FFullScreen := TGLDisplayOptions(Source).FFullScreen;
    FScreenResolution := TGLDisplayOptions(Source).FScreenResolution;
    FWindowAttributes := TGLDisplayOptions(Source).FWindowAttributes;
    FWindowFitting := TGLDisplayOptions(Source).FWindowFitting;
  end
  else
    inherited Assign(Source);
end;

function GetIndexFromResolution(XRes, YRes, BPP: Integer): TResolution;

// Determines the index of a screen resolution nearest to the
// given values. The returned screen resolution is always greater
// or equal than XRes and YRes or, in case the resolution isn't
// supported, the value 0, which indicates the default mode.

var
  I: Integer;
  XDiff, YDiff: Integer;
  CDiff: Integer;

begin
  ReadVideoModes;
  // prepare result in case we don't find a valid mode
  Result := 0;
  // set differences to maximum
  XDiff := 9999;
  YDiff := 9999;
  CDiff := 99;
  for I := 1 to vNumberVideoModes - 1 do
    with vVideoModes[I] do
    begin
      if (Width >= XRes) and ((Width - XRes) <= XDiff) and (Height >= YRes) and
        ((Height - YRes) <= YDiff) and (ColorDepth >= BPP) and
        ((ColorDepth - BPP) <= CDiff) then
      begin
        XDiff := Width - XRes;
        YDiff := Height - YRes;
        CDiff := ColorDepth - BPP;
        Result := I;
      end;
    end;
end;

procedure TryToAddToList(deviceMode: TDevMode);
// Adds a video mode to the list if it's not a duplicate and can actually be set.
var
  I: Integer;
  vm: PVideoMode;
begin
  // See if this is a duplicate mode (can happen because of refresh
  // rates, or because we explicitly try all the low-res modes)
  for I := 1 to vNumberVideoModes - 1 do
    with deviceMode do
    begin
      vm := @vVideoModes[I];
      if ((dmBitsPerPel = vm^.ColorDepth) and (dmPelsWidth = vm^.Width) and
        (dmPelsHeight = vm^.Height)) then
      begin
        // it's a duplicate mode, higher frequency?
        if dmDisplayFrequency > vm^.MaxFrequency then
          vm^.MaxFrequency := dmDisplayFrequency;
        Exit;
      end;
    end;

  // do a mode set test (doesn't actually do the mode set, but reports whether it would have succeeded).
  if ChangeDisplaySettings(deviceMode, CDS_TEST or CDS_FULLSCREEN) <> DISP_CHANGE_SUCCESSFUL
  then
    Exit;

  // it's a new, valid mode, so add this to the list
  vm := @vVideoModes[vNumberVideoModes];
  with deviceMode do
  begin
    vm^.ColorDepth := dmBitsPerPel;
    vm^.Width := dmPelsWidth;
    vm^.Height := dmPelsHeight;
    vm^.MaxFrequency := dmDisplayFrequency;
    vm^.Description := Format('%d x %d, %d bpp', [dmPelsWidth, dmPelsHeight,
      dmBitsPerPel]);
  end;
  Inc(vNumberVideoModes);
end;

procedure ReadVideoModes;
var
  I, ModeNumber: Integer;
  done: Boolean;
  deviceMode: TDevMode;
  DeskDC: HDC;
begin
  if vNumberVideoModes > 0 then
    Exit;

  SetLength(vVideoModes, MaxVideoModes);
  vNumberVideoModes := 1;

  // prepare 'default' entry
  DeskDC := GetDC(0);
  with vVideoModes[0] do
    try
      ColorDepth := GetDeviceCaps(DeskDC, BITSPIXEL) *
        GetDeviceCaps(DeskDC, PLANES);
      Width := Screen.Width;
      Height := Screen.Height;
      Description := 'default';
    finally
      ReleaseDC(0, DeskDC);
    end;

  // enumerate all available video modes
  ModeNumber := 0;
  repeat
    done := not EnumDisplaySettings(nil, ModeNumber, deviceMode);
    TryToAddToList(deviceMode);
    Inc(ModeNumber);
  until (done or (vNumberVideoModes >= MaxVideoModes));

  // low-res modes don't always enumerate, ask about them explicitly
  with deviceMode do
  begin
    dmBitsPerPel := 8;
    dmPelsWidth := 42;
    dmPelsHeight := 37;
    dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
    // make sure the driver doesn't just answer yes to all tests
    if ChangeDisplaySettings(deviceMode, CDS_TEST or CDS_FULLSCREEN) <> DISP_CHANGE_SUCCESSFUL
    then
    begin
      I := 0;
      while (I < NumberLowResModes - 1) and
        (vNumberVideoModes < MaxVideoModes) do
      begin
        dmSize := Sizeof(deviceMode);
        with LowResModes[I] do
        begin
          dmBitsPerPel := ColorDepth;
          dmPelsWidth := Width;
          dmPelsHeight := Height;
        end;
        dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
        TryToAddToList(deviceMode);
        Inc(I);
      end;
    end;
  end;
end;

function SetFullscreenMode(modeIndex: TResolution;
  displayFrequency: Integer = 0): Boolean;
var
  deviceMode: TDevMode;
begin
  ReadVideoModes;
  FillChar(deviceMode, Sizeof(deviceMode), 0);
  with deviceMode do
  begin
    dmSize := Sizeof(deviceMode);
    dmBitsPerPel := vVideoModes[modeIndex].ColorDepth;
    dmPelsWidth := vVideoModes[modeIndex].Width;
    dmPelsHeight := vVideoModes[modeIndex].Height;
    dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
    if displayFrequency > 0 then
    begin
      dmFields := dmFields or DM_DISPLAYFREQUENCY;
      if displayFrequency > vVideoModes[modeIndex].MaxFrequency then
        displayFrequency := vVideoModes[modeIndex].MaxFrequency;
      dmDisplayFrequency := displayFrequency;
    end;
  end;
  Result := ChangeDisplaySettings(deviceMode, CDS_FULLSCREEN)
    = DISP_CHANGE_SUCCESSFUL;
  if Result then
    vCurrentVideoMode := modeIndex;
end;

procedure ReadScreenImage(Dest: HDC; DestLeft, DestTop: Integer;
  const SrcRect: TRectangle);
var
  screenDC: HDC;
begin
  screenDC := GetDC(0);
  try
    GDIFlush;
    BitBlt(Dest, DestLeft, DestTop, SrcRect.Width, SrcRect.Height, screenDC,
      SrcRect.Left, SrcRect.Top, SRCCOPY);
  finally
    ReleaseDC(0, screenDC);
  end;
end;

procedure RestoreDefaultMode;
var
  t: PDevMode;
begin
  t := nil;
  ChangeDisplaySettings(t^, CDS_FULLSCREEN);
end;

procedure GLShowCursor(AShow: Boolean);
begin
  ShowCursor(AShow);
end;

procedure GLSetCursorPos(AScreenX, AScreenY: Integer);
begin
  SetCursorPos(AScreenX, AScreenY);
end;

procedure GLGetCursorPos(var point: TPoint);
begin
  GetCursorPos(point);
end;

function GLGetScreenWidth: Integer;
begin
  Result := Screen.Width;
end;

function GLGetScreenHeight: Integer;
begin
  Result := Screen.Height;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

finalization

if vCurrentVideoMode <> 0 then
  RestoreDefaultMode; // set default video mode

end.
