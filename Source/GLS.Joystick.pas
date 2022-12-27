//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.Joystick;

(* Component for handling joystick messages *)

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.MMSystem,
  System.SysUtils,
  System.Classes;

type

  TJoystickButton = (jbButton1, jbButton2, jbButton3, jbButton4);
  TJoystickButtons = set of TJoystickButton;

  TJoystickID = (jidNoJoystick, jidJoystick1, jidJoystick2);
  TJoystickDesignMode = (jdmInactive, jdmActive);
  TJoyPos = (jpMin, jpCenter, jpMax);
  TJoyAxis = (jaX, jaY, jaZ, jaR, jaU, jaV);

  TJoystickEvent = procedure(Sender: TObject; JoyID: TJoystickID;
    Buttons: TJoystickButtons; XDeflection, YDeflection: Integer) of Object;

  // A component interfacing the Joystick via the (regular) windows API.
  TGLJoystick = class(TComponent)
  private
    FWindowHandle: HWND;
    FNumButtons, FLastX, FLastY, FLastZ: Integer;
    FThreshold, FInterval: Cardinal;
    FCapture, FNoCaptureErrors: Boolean;
    FJoystickID: TJoystickID;
    FMinMaxInfo: array [TJoyAxis, TJoyPos] of Integer;
    FXPosInfo, FYPosInfo: array [0 .. 4] of Integer;
    FOnJoystickButtonChange, FOnJoystickMove: TJoystickEvent;
    FXPosition, FYPosition: Integer;
    FJoyButtons: TJoystickButtons;
    procedure SetCapture(AValue: Boolean);
    procedure SetInterval(AValue: Cardinal);
    procedure SetJoystickID(AValue: TJoystickID);
    procedure SetThreshold(AValue: Cardinal);
  protected
    function MakeJoyButtons(Param: UINT): TJoystickButtons;
    procedure DoJoystickCapture(AHandle: HWND; AJoystick: TJoystickID);
    procedure DoJoystickRelease(AJoystick: TJoystickID);
    procedure DoXYMove(Buttons: Word; XPos, YPos: Integer);
    procedure DoZMove(Buttons: Word; ZPos: Integer);
    procedure ReapplyCapture(AJoystick: TJoystickID);
    procedure WndProc(var Msg: TMessage);
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property JoyButtons: TJoystickButtons read FJoyButtons;
    property XPosition: Integer read FXPosition;
    property YPosition: Integer read FYPosition;
  published
    (* When set to True, the component attempts to capture the joystick.
      If capture is successfull, retrieving joystick status is possible,
      if not, an error message is triggered. *)
    property Capture: Boolean read FCapture write SetCapture default False;
    // If true joystick capture errors do not result in exceptions.
    property NoCaptureErrors: Boolean read FNoCaptureErrors
      write FNoCaptureErrors default True;
    // Polling frequency (milliseconds)
    property Interval: Cardinal read FInterval write SetInterval default 100;
    property JoystickID: TJoystickID read FJoystickID write SetJoystickID
      default jidNoJoystick;
    property Threshold: Cardinal read FThreshold write SetThreshold
      default 1000;
    property OnJoystickButtonChange: TJoystickEvent read FOnJoystickButtonChange
      write FOnJoystickButtonChange;
    property OnJoystickMove: TJoystickEvent read FOnJoystickMove
      write FOnJoystickMove;
  end;

// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------

const
  cJoystickIDToNative: array [jidNoJoystick .. jidJoystick2] of Byte = (9,
    JOYSTICKID1, JOYSTICKID2);

// ------------------
// ------------------ TJoystick ------------------
// ------------------

constructor TGLJoystick.Create(AOwner: TComponent);
begin
  inherited;
  FWindowHandle := AllocateHWnd(WndProc);
  FInterval := 100;
  FThreshold := 1000;
  FJoystickID := jidNoJoystick;
  FLastX := -1;
  FLastY := -1;
  FLastZ := -1;
  FNoCaptureErrors := True;
end;

destructor TGLJoystick.Destroy;
begin
  DeallocateHWnd(FWindowHandle);
  inherited;
end;

procedure TGLJoystick.WndProc(var Msg: TMessage);
begin
  with Msg do
  begin
    case FJoystickID of
      jidJoystick1: // check only 1st stick
        case Msg of
          MM_JOY1MOVE:
            DoXYMove(wParam, lParamLo, lParamHi);
          MM_JOY1ZMOVE:
            DoZMove(wParam, lParamLo);
          MM_JOY1BUTTONDOWN:
            if Assigned(FOnJoystickButtonChange) then
              FOnJoystickButtonChange(Self, FJoystickID, MakeJoyButtons(wParam),
                FLastX, FLastY);
          MM_JOY1BUTTONUP:
            if Assigned(FOnJoystickButtonChange) then
              FOnJoystickButtonChange(Self, FJoystickID, MakeJoyButtons(wParam),
                FLastX, FLastY);
        end;
      jidJoystick2: // check only 2nd stick
        case Msg of
          MM_JOY2MOVE:
            DoXYMove(wParam, lParamLo, lParamHi);
          MM_JOY2ZMOVE:
            DoZMove(wParam, lParamLo);
          MM_JOY2BUTTONDOWN:
            if Assigned(FOnJoystickButtonChange) then
              FOnJoystickButtonChange(Self, FJoystickID, MakeJoyButtons(wParam),
                FLastX, FLastY);
          MM_JOY2BUTTONUP:
            if Assigned(FOnJoystickButtonChange) then
              FOnJoystickButtonChange(Self, FJoystickID, MakeJoyButtons(wParam),
                FLastX, FLastY);
        end;
      jidNoJoystick:
        ; // ignore
    else
      Assert(False);
    end;
    Result := 0;
  end;
end;

procedure TGLJoystick.Loaded;
begin
  inherited;
  ReapplyCapture(FJoystickID);
end;

procedure TGLJoystick.Assign(Source: TPersistent);
begin
  if Source is TGLJoystick then
  begin
    FInterval := TGLJoystick(Source).FInterval;
    FThreshold := TGLJoystick(Source).FThreshold;
    FCapture := TGLJoystick(Source).FCapture;
    FJoystickID := TGLJoystick(Source).FJoystickID;
    try
      ReapplyCapture(FJoystickID);
    except
      FJoystickID := jidNoJoystick;
      FCapture := False;
      raise;
    end;
  end
  else
    inherited Assign(Source);
end;

function TGLJoystick.MakeJoyButtons(Param: UINT): TJoystickButtons;
begin
  Result := [];
  if (Param and JOY_BUTTON1) > 0 then
    Include(Result, jbButton1);
  if (Param and JOY_BUTTON2) > 0 then
    Include(Result, jbButton2);
  if (Param and JOY_BUTTON3) > 0 then
    Include(Result, jbButton3);
  if (Param and JOY_BUTTON4) > 0 then
    Include(Result, jbButton4);
  FJoyButtons := Result;
end;

function DoScale(AValue: Integer): Integer;
begin
  Result := Round(AValue / 1);
end;

procedure TGLJoystick.ReapplyCapture(AJoystick: TJoystickID);
var
  jc: TJoyCaps;
begin
  DoJoystickRelease(AJoystick);
  if FCapture and (not(csDesigning in ComponentState)) then
    with jc do
    begin
      joyGetDevCaps(cJoystickIDToNative[FJoystickID], @jc, SizeOf(jc));
      FNumButtons := wNumButtons;
      FMinMaxInfo[jaX, jpMin] := DoScale(wXMin);
      FMinMaxInfo[jaX, jpCenter] := DoScale((wXMin + wXMax) div 2);
      FMinMaxInfo[jaX, jpMax] := DoScale(wXMax);
      FMinMaxInfo[jaY, jpMin] := DoScale(wYMin);
      FMinMaxInfo[jaY, jpCenter] := DoScale((wYMin + wYMax) div 2);
      FMinMaxInfo[jaY, jpMax] := DoScale(wYMax);
      FMinMaxInfo[jaZ, jpMin] := DoScale(wZMin);
      FMinMaxInfo[jaZ, jpCenter] := DoScale((wZMin + wZMax) div 2);
      FMinMaxInfo[jaZ, jpMax] := DoScale(wZMax);
      FMinMaxInfo[jaR, jpMin] := DoScale(wRMin);
      FMinMaxInfo[jaR, jpCenter] := DoScale((wRMin + wRMax) div 2);
      FMinMaxInfo[jaR, jpMax] := DoScale(wRMax);
      FMinMaxInfo[jaU, jpMin] := DoScale(wUMin);
      FMinMaxInfo[jaU, jpCenter] := DoScale((wUMin + wUMax) div 2);
      FMinMaxInfo[jaU, jpMax] := DoScale(wUMax);
      FMinMaxInfo[jaV, jpMin] := DoScale(wVMin);
      FMinMaxInfo[jaV, jpCenter] := DoScale((wVMin + wVMax) div 2);
      FMinMaxInfo[jaV, jpMax] := DoScale(wVMax);
      DoJoystickCapture(FWindowHandle, AJoystick)
    end;
end;

procedure TGLJoystick.DoJoystickCapture(AHandle: HWND; AJoystick: TJoystickID);
var
  res: Cardinal;
begin
  res := joySetCapture(AHandle, cJoystickIDToNative[AJoystick],
    FInterval, True);
  if res <> JOYERR_NOERROR then
  begin
    FCapture := False;
    if not NoCaptureErrors then
    begin
      case res of
        MMSYSERR_NODRIVER:
          raise Exception.Create('No Joystick Driver');
        JOYERR_UNPLUGGED:
          raise Exception.Create('Connect Joystick');
        JOYERR_NOCANDO:
          raise Exception.Create('Joystick Error');
      else
        raise Exception.Create('Joystick Error');
      end;
    end;
  end
  else
    joySetThreshold(cJoystickIDToNative[AJoystick], FThreshold);
end;

procedure TGLJoystick.DoJoystickRelease(AJoystick: TJoystickID);
begin
  if AJoystick <> jidNoJoystick then
    joyReleaseCapture(cJoystickIDToNative[AJoystick]);
end;

procedure TGLJoystick.SetCapture(AValue: Boolean);
begin
  if FCapture <> AValue then
  begin
    FCapture := AValue;
    if not(csReading in ComponentState) then
    begin
      try
        ReapplyCapture(FJoystickID);
      except
        FCapture := False;
        raise;
      end;
    end;
  end;
end;

procedure TGLJoystick.SetInterval(AValue: Cardinal);
begin
  if FInterval <> AValue then
  begin
    FInterval := AValue;
    if not(csReading in ComponentState) then
      ReapplyCapture(FJoystickID);
  end;
end;

procedure TGLJoystick.SetJoystickID(AValue: TJoystickID);
begin
  if FJoystickID <> AValue then
  begin
    try
      if not(csReading in ComponentState) then
        ReapplyCapture(AValue);
      FJoystickID := AValue;
    except
      on E: Exception do
      begin
        ReapplyCapture(FJoystickID);
        ShowException(E, nil);
      end;
    end;
  end;
end;

// ------------------------------------------------------------------------------

procedure TGLJoystick.SetThreshold(AValue: Cardinal);

begin
  if FThreshold <> AValue then
  begin
    FThreshold := AValue;
    if not(csReading in ComponentState) then
      ReapplyCapture(FJoystickID);
  end;
end;

// ------------------------------------------------------------------------------

function Approximation(const Data: array of Integer): Integer;

// calculate a better estimation of the last value in the given data, depending
// on the other values (used to approximate a smoother joystick movement)
//
// based on Gauss' principle of smallest squares in Maximum-Likelihood and
// linear normal equations

var
  SumX, SumY, SumXX, SumYX: Double;
  I, Comps: Integer;
  a0, a1: Double;

begin
  SumX := 0;
  SumY := 0;
  SumXX := 0;
  SumYX := 0;
  Comps := High(Data) + 1;
  for I := 0 to High(Data) do
  begin
    SumX := SumX + I;
    SumY := SumY + Data[I];
    SumXX := SumXX + I * I;
    SumYX := SumYX + I * Data[I];
  end;
  a0 := (SumY * SumXX - SumX * SumYX) / (Comps * SumXX - SumX * SumX);
  a1 := (Comps * SumYX - SumY * SumX) / (Comps * SumXX - SumX * SumX);
  Result := Round(a0 + a1 * High(Data));
end;

procedure TGLJoystick.DoXYMove(Buttons: Word; XPos, YPos: Integer);
var
  I: Integer;
  dX, dY: Integer;
begin
  XPos := DoScale(XPos);
  YPos := DoScale(YPos);
  if (FLastX = -1) or (FLastY = -1) then
  begin
    FLastX := XPos;
    FLastY := YPos;
    for I := 0 to 4 do
    begin
      FXPosInfo[I] := XPos;
      FYPosInfo[I] := YPos;
    end;
  end
  else
  begin
    Move(FXPosInfo[1], FXPosInfo[0], 16);
    FXPosInfo[4] := XPos;
    XPos := Approximation(FXPosInfo);
    Move(FYPosInfo[1], FYPosInfo[0], 16);
    FYPosInfo[4] := YPos;
    YPos := Approximation(FYPosInfo);
    MakeJoyButtons(Buttons);
    dX := Round((XPos - FMinMaxInfo[jaX, jpCenter]) * 100 / FMinMaxInfo[jaX,
      jpCenter]);
    dY := Round((YPos - FMinMaxInfo[jaY, jpCenter]) * 100 / FMinMaxInfo[jaY,
      jpCenter]);
    if Assigned(FOnJoystickMove) then
      FOnJoystickMove(Self, FJoystickID, FJoyButtons, dX, dY);
    FXPosition := dX;
    FYPosition := dY;
    FLastX := XPos;
    FLastY := YPos;
  end;
end;

procedure TGLJoystick.DoZMove(Buttons: Word; ZPos: Integer);
begin
  if FLastZ = -1 then
    FLastZ := Round(ZPos * 100 / 65536);
  MakeJoyButtons(Buttons);
end;

//------------------------------------
initialization
//------------------------------------

RegisterClasses([TGLJoystick]);

end.
