//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Cadencer;

(* Cadencing composant for ease Progress processing *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.Windows,
  System.Messaging,
  Winapi.Messages,
  System.Classes,
  System.Types,
  System.SysUtils,
  FMX.Forms,

  GXS.Scene,
  GXS.BaseClasses;

type

  (* Determines how the TgxCadencer operates.
   - cmManual : you must trigger progress manually (in your code)
   - cmASAP : progress is triggered As Soon As Possible after a previous
    progress (uses windows messages).
       - cmApplicationIdle : will hook Application.OnIdle, this will overwrite
          any previous event handle, and only one cadencer may be in this mode. *)
  TgxCadencerMode = (cmManual, cmASAP, cmApplicationIdle);

  (* Determines which time reference the TgxCadencer should use.
   - cmRTC : the Real Time Clock is used (precise over long periods, but
    not accurate to the millisecond, may limit your effective framerate
          to less than 50 FPS on some systems)
   - cmPerformanceCounter : the windows performance counter is used (nice
    precision, may derive over long periods, this is the default option
    as it allows the smoothest animation on fast systems)
   - cmExternal : the CurrentTime property is used *)
  TgxCadencerTimeReference = (cmRTC, cmPerformanceCounter, cmExternal);

  (* This component allows auto-progression of animation.
   Basicly dropping this component and linking it to your TgxScene will send
   it real-time progression events (time will be measured in seconds) while
   keeping the CPU 100% busy if possible (ie. if things change in your scene).
   The progression time (the one you'll see in you progression events)
   is calculated using  (CurrentTime-OriginTime)*TimeMultiplier,
   CurrentTime being either manually or automatically updated using
   TimeReference (setting CurrentTime does NOT trigger progression). *)
  TgxCadencer = class(TComponent)
  private
    FSubscribedCadenceableComponents: TList;
    FScene: TgxScene;
    FTimeMultiplier: Double;
    lastTime, downTime, lastMultiplier: Double;
    FEnabled: Boolean;
    FSleepLength: Integer;
    FMode: TgxCadencerMode;
    FTimeReference: TgxCadencerTimeReference;
    FCurrentTime: Double;
    FOriginTime: Double;
    FMaxDeltaTime, FMinDeltaTime, FFixedDeltaTime: Double;
  	FOnProgress, FOnTotalProgress : TgxProgressEvent;
    FProgressing: Integer;
    procedure SetCurrentTime(const Value: Double);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    function StoreTimeMultiplier: Boolean;
    procedure SetEnabled(const val: Boolean);
    procedure SetScene(const val: TgxScene);
    procedure SetMode(const val: TgxCadencerMode);
    procedure SetTimeReference(const val: TgxCadencerTimeReference);
    procedure SetTimeMultiplier(const val: Double);
    // Returns raw ref time (no multiplier, no offset)
    function GetRawReferenceTime: Double;
    procedure RestartASAP;
    procedure Loaded; override;
    procedure OnIdleEvent(Sender: TObject; var Done: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Subscribe(aComponent: TgxCadenceAbleComponent);
    procedure UnSubscribe(aComponent: TgxCadenceAbleComponent);
    (* Allows to manually trigger a progression.
     Time stuff is handled automatically.
     If cadencer is disabled, this functions does nothing. *)
    procedure Progress;
    // Adjusts CurrentTime if necessary, then returns its value.
    function GetCurrenttime: Double;
    (* Returns True if a "Progress" is underway.
       Be aware that as long as IsBusy is True, the Cadencer may be
       sending messages and progression calls to cadenceable components
       and scenes. *)
    function IsBusy: Boolean;
    // Reset the time parameters and returns to zero.
    procedure Reset;
    // Value soustracted to current time to obtain progression time.
    property OriginTime: Double read FOriginTime write FOriginTime;
    // Current time (manually or automatically set, see TimeReference).
    property CurrentTime: Double read FCurrentTime write SetCurrentTime;
  published
    // The TgxScene that will be cadenced (progressed).
    property Scene: TgxScene read FScene write SetScene;
    (* Enables/Disables cadencing.
     Disabling won't cause a jump when restarting, it is working like
     a play/pause (ie. may modify OriginTime to keep things smooth). *)
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    (* Defines how CurrentTime is updated.
     See TgxCadencerTimeReference.
     Dynamically changeing the TimeReference may cause a "jump".  *)
    property TimeReference: TgxCadencerTimeReference read FTimeReference write
      SetTimeReference default cmPerformanceCounter;
    (* Multiplier applied to the time reference.
      Zero isn't an allowed value, and be aware that if negative values
      are accepted, they may not be supported by other GLScene objects.
     Changing the TimeMultiplier will alter OriginTime. *)
    property TimeMultiplier: Double read FTimeMultiplier write SetTimeMultiplier
      stored StoreTimeMultiplier;
    (* Maximum value for deltaTime in progression events.
       If null or negative, no max deltaTime is defined, otherwise, whenever
       an event whose actual deltaTime would be superior to MaxDeltaTime
       occurs, deltaTime is clamped to this max, and the extra time is hidden
       by the cadencer (it isn't visible in CurrentTime either).
       This option allows to limit progression rate in simulations where
       high values would result in errors/random behaviour. *)
    property MaxDeltaTime: Double read FMaxDeltaTime write FMaxDeltaTime;
    (* Minimum value for deltaTime in progression events.
       If superior to zero, this value specifies the minimum time step
       between two progression events.
       This option allows to limit progression rate in simulations where
       low values would result in errors/random behaviour. *)
    property MinDeltaTime: Double read FMinDeltaTime write FMinDeltaTime;
    (* Fixed time-step value for progression events.
       If superior to zero, progression steps will happen with that fixed
       delta time. The progression remains time based, so zero to N events
       may be fired depending on the actual deltaTime (if deltaTime is
       inferior to FixedDeltaTime, no event will be fired, if it is superior
       to two times FixedDeltaTime, two events will be fired, etc.).
       This option allows to use fixed time steps in simulations (while the
       animation and rendering itself may happen at a lower or higher
       framerate). *)
    property FixedDeltaTime: Double read FFixedDeltaTime write FFixedDeltaTime;
    (* Adjusts how progression events are triggered.
     See TgxCadencerMode. *)
    property Mode: TgxCadencerMode read FMode write SetMode default cmASAP;
    (* Allows relinquishing time to other threads/processes.
     A "sleep" is issued BEFORE each progress if SleepLength>=0 (see
     help for the "sleep" procedure in delphi for details). *)
    property SleepLength: Integer read FSleepLength write FSleepLength default -1;
    // Happens AFTER scene was progressed.
    property OnProgress: TgxProgressEvent read FOnProgress write FOnProgress;
    // Happens AFTER all iterations with fixed delta time.
    property OnTotalProgress : TgxProgressEvent read FOnTotalProgress write FOnTotalProgress;
  end;

  // Adds a property to connect/subscribe to a cadencer.
  TgxCustomCadencedComponent = class(TgxUpdateAbleComponent)
  private
    FCadencer: TgxCadencer;
  protected
    procedure SetCadencer(const val: TgxCadencer);
    property Cadencer: TgxCadencer read FCadencer write SetCadencer;
  public
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  end;

  TgxCadencedComponent = class(TgxCustomCadencedComponent)
  published
    property Cadencer;
  end;

// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------

const
  cTickCadencer = 'TickCadencer';

type
  // TASAPHandler
  TASAPHandler = class
  private
    FTooFastCounter: Integer;
    FTimer: Cardinal;
    FWindowHandle: HWND;
    procedure WndProc(var Msg: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  vWMTickCadencer: Cardinal;
  vASAPCadencerList: TList;
  vHandler: TASAPHandler;
  vCounterFrequency: Int64;

procedure RegisterASAPCadencer(aCadencer: TgxCadencer);
begin
  if aCadencer.Mode = cmASAP then
  begin
    if not Assigned(vASAPCadencerList) then
      vASAPCadencerList := TList.Create;
    if vASAPCadencerList.IndexOf(aCadencer) < 0 then
    begin
      vASAPCadencerList.Add(aCadencer);
      if not Assigned(vHandler) then
        vHandler := TASAPHandler.Create;
    end;
  end
  else if aCadencer.Mode = cmApplicationIdle then
    Application.OnIdle := aCadencer.OnIdleEvent;
end;

procedure UnRegisterASAPCadencer(aCadencer: TgxCadencer);
var
  i: Integer;
begin
  if aCadencer.Mode = cmASAP then
  begin
    if Assigned(vASAPCadencerList) then
    begin
      i := vASAPCadencerList.IndexOf(aCadencer);
      if i >= 0 then
        vASAPCadencerList[i] := nil;
    end;
  end
  else if aCadencer.Mode = cmApplicationIdle then
    Application.OnIdle := nil;
end;

// ------------------
// ------------------ TASAPHandler ------------------
// ------------------

constructor TASAPHandler.Create;
begin
  inherited Create;
  FWindowHandle := AllocateHWnd(WndProc);
  PostMessage(FWindowHandle, vWMTickCadencer, 0, 0);
end;

destructor TASAPHandler.Destroy;
begin
  if FTimer <> 0 then
    KillTimer(FWindowHandle, FTimer);
  DeallocateHWnd(FWindowHandle);

  inherited Destroy;
end;

var
  vWndProcInLoop: Boolean;

procedure TASAPHandler.WndProc(var Msg: TMessage);
var
  i: Integer;
  cad: TgxCadencer;
begin
  //   Windows.Beep(440, 10);
  with Msg do
  begin
    if Msg = WM_TIMER then
    begin
      KillTimer(FWindowHandle, FTimer);
      FTimer := 0;
    end;
    if (Msg <> WM_TIMER) and (Cardinal(GetMessageTime) = GetTickCount) then
    begin
      // if we're going too fast, "sleep" for 1 msec
      Inc(FTooFastCounter);
      if FTooFastCounter > 5000 then
      begin
        if FTimer = 0 then
          FTimer := SetTimer(FWindowHandle, 1, 1, nil);
        FTooFastCounter := 0;
      end;
    end
    else
      FTooFastCounter := 0;
    if FTimer <> 0 then
    begin
      Result := 0;
      Exit;
    end;
    if not vWndProcInLoop then
    begin
      vWndProcInLoop := True;
      try
        if (Msg = vWMTickCadencer) or (Msg = WM_TIMER) then
        begin
          // Progress
          for i := vASAPCadencerList.Count - 1 downto 0 do
          begin
            cad := TgxCadencer(vASAPCadencerList[i]);
            if Assigned(cad) and (cad.Mode = cmASAP)
              and cad.Enabled and (cad.FProgressing = 0) then
            begin
              if Application.Terminated then
              begin
                // force stop
                cad.Enabled := False
              end
              else
              begin
                try
                  // do stuff
                  cad.Progress;
                except
                  Application.HandleException(Self);
                  // it faulted, stop it
                  cad.Enabled := False
                end
              end;
            end;
          end;
          // care for nils
          vASAPCadencerList.Pack;
          if vASAPCadencerList.Count = 0 then
          begin
            vASAPCadencerList.Free;
            vASAPCadencerList := nil;
            vHandler.Free;
            vHandler := nil;
          end
          else
          begin
            // Prepare the return of the infernal loop...
            PostMessage(FWindowHandle, vWMTickCadencer, 0, 0);
          end;
        end;
      finally
        vWndProcInLoop := False;
      end;
    end;
    Result := 0;
  end;
end;

// ------------------
// ------------------ TgxCadencer ------------------
// ------------------

constructor TgxCadencer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeReference := cmPerformanceCounter;
  downTime := GetRawReferenceTime;
  FOriginTime := downTime;
  FTimeMultiplier := 1;
  FSleepLength := -1;
  Mode := cmASAP;
  Enabled := True;
end;

destructor TgxCadencer.Destroy;
begin
  Assert(FProgressing = 0);
  UnRegisterASAPCadencer(Self);
  FSubscribedCadenceableComponents.Free;
  FSubscribedCadenceableComponents := nil;
  inherited Destroy;
end;

procedure TgxCadencer.Subscribe(aComponent: TgxCadenceAbleComponent);
begin
  if not Assigned(FSubscribedCadenceableComponents) then
    FSubscribedCadenceableComponents := TList.Create;
  if FSubscribedCadenceableComponents.IndexOf(aComponent) < 0 then
  begin
    FSubscribedCadenceableComponents.Add(aComponent);
    aComponent.FreeNotification(Self);
  end;
end;

procedure TgxCadencer.UnSubscribe(aComponent: TgxCadenceAbleComponent);
var
  i: Integer;
begin
  if Assigned(FSubscribedCadenceableComponents) then
  begin
    i := FSubscribedCadenceableComponents.IndexOf(aComponent);
    if i >= 0 then
    begin
      FSubscribedCadenceableComponents.Delete(i);
      aComponent.RemoveFreeNotification(Self);
    end;
  end;
end;

procedure TgxCadencer.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FScene then
      FScene := nil;
    if Assigned(FSubscribedCadenceableComponents) then
      FSubscribedCadenceableComponents.Remove(AComponent);
  end;
  inherited;
end;

procedure TgxCadencer.Loaded;
begin
  inherited Loaded;
  RestartASAP;
end;

procedure TgxCadencer.OnIdleEvent(Sender: TObject; var Done: Boolean);
begin
  Progress;
  Done := False;
end;

procedure TgxCadencer.RestartASAP;
begin
  if not (csLoading in ComponentState) then
  begin
    if (Mode in [cmASAP, cmApplicationIdle]) and (not (csDesigning in
      ComponentState))
      and Assigned(FScene) and Enabled then
      RegisterASAPCadencer(Self)
    else
      UnRegisterASAPCadencer(Self);
  end;
end;

procedure TgxCadencer.SetEnabled(const val: Boolean);
begin
  if FEnabled <> val then
  begin
    FEnabled := val;
    if not (csDesigning in ComponentState) then
    begin
      if Enabled then
        FOriginTime := FOriginTime + GetRawReferenceTime - downTime
      else
        downTime := GetRawReferenceTime;
      RestartASAP;
    end;
  end;
end;

procedure TgxCadencer.SetScene(const val: TgxScene);
begin
  if FScene <> val then
  begin
    if Assigned(FScene) then
      FScene.RemoveFreeNotification(Self);
    FScene := val;
    if Assigned(FScene) then
      FScene.FreeNotification(Self);
    RestartASAP;
  end;
end;

procedure TgxCadencer.SetTimeMultiplier(const val: Double);
var
  rawRef: Double;
begin
  if val <> FTimeMultiplier then
  begin
    if val = 0 then
    begin
      lastMultiplier := FTimeMultiplier;
      Enabled := False;
    end
    else
    begin
      rawRef := GetRawReferenceTime;
      if FTimeMultiplier = 0 then
      begin
        Enabled := True;
        // continuity of time:
        // (rawRef-newOriginTime)*val = (rawRef-FOriginTime)*lastMultiplier
        FOriginTime := rawRef - (rawRef - FOriginTime) * lastMultiplier / val;
      end
      else
      begin
        // continuity of time:
        // (rawRef-newOriginTime)*val = (rawRef-FOriginTime)*FTimeMultiplier
        FOriginTime := rawRef - (rawRef - FOriginTime) * FTimeMultiplier / val;
      end;
    end;
    FTimeMultiplier := val;
  end;
end;

function TgxCadencer.StoreTimeMultiplier: Boolean;
begin
  Result := (FTimeMultiplier <> 1);
end;

procedure TgxCadencer.SetMode(const val: TgxCadencerMode);
begin
  if FMode <> val then
  begin
    if FMode <> cmManual then
      UnRegisterASAPCadencer(Self);
    FMode := val;
    RestartASAP;
  end;
end;

procedure TgxCadencer.SetTimeReference(const val: TgxCadencerTimeReference);
begin
  // nothing more, yet
  FTimeReference := val;
end;

procedure TgxCadencer.Progress;
var
  deltaTime, newTime, totalDelta: Double;
  fullTotalDelta, firstLastTime : Double;
  i: Integer;
  pt: TgxProgressTimes;
begin
  // basic protection against infinite loops,
    // shall never happen, unless there is a bug in user code
  if FProgressing < 0 then
    Exit;
  if Enabled then
  begin
    // avoid stalling everything else...
    if SleepLength >= 0 then
      Sleep(SleepLength);
    // in manual mode, the user is supposed to make sure messages are handled
    // in Idle mode, this processing is implicit
    if Mode = cmASAP then
    begin
      Application.ProcessMessages;
      if (not Assigned(vASAPCadencerList))
        or (vASAPCadencerList.IndexOf(Self) < 0) then
        Exit;
    end;
  end;
  Inc(FProgressing);
  try
    if Enabled then
    begin
      // One of the processed messages might have disabled us
      if Enabled then
      begin
        // ...and progress !
        newTime := GetCurrenttime;
        deltaTime := newTime - lastTime;
        if (deltaTime >= MinDeltaTime) and (deltaTime >= FixedDeltaTime) then
        begin
          if FMaxDeltaTime > 0 then
          begin
            if deltaTime > FMaxDeltaTime then
            begin
              FOriginTime := FOriginTime + (deltaTime - FMaxDeltaTime) /
                FTimeMultiplier;
              deltaTime := FMaxDeltaTime;
              newTime := lastTime + deltaTime;
            end;
          end;
          totalDelta := deltaTime;
          fullTotalDelta := totalDelta;
          firstLastTime := lastTime;
          if FixedDeltaTime > 0 then
            deltaTime := FixedDeltaTime;
          while totalDelta >= deltaTime do
          begin
            lastTime := lastTime + deltaTime;
            if Assigned(FScene) and (deltaTime <> 0) then
            begin
              FProgressing := -FProgressing;
              try
                FScene.Progress(deltaTime, lastTime);
              finally
                FProgressing := -FProgressing;
              end;
            end;
            pt.deltaTime := deltaTime;
            pt.newTime := lastTime;
            i := 0;
            while Assigned(FSubscribedCadenceableComponents) and
              (i <= FSubscribedCadenceableComponents.Count - 1) do
            begin
              TgxCadenceAbleComponent(FSubscribedCadenceableComponents[i]).DoProgress(pt);
              i := i + 1;
            end;
            if Assigned(FOnProgress) and (not (csDesigning in ComponentState))
              then
              FOnProgress(Self, deltaTime, newTime);
            if deltaTime <= 0 then
              Break;
            totalDelta := totalDelta - deltaTime;
          end;
          if Assigned(FOnTotalProgress)
            and (not (csDesigning in ComponentState)) then
            FOnTotalProgress(Self, fullTotalDelta, firstLastTime);
        end;
      end;
    end;
  finally
    Dec(FProgressing);
  end;
end;

function TgxCadencer.GetRawReferenceTime: Double;
var
  counter: Int64;
begin
  case FTimeReference of
    cmRTC: // Real Time Clock
      Result := Now * (3600 * 24);
    cmPerformanceCounter:
      begin // HiRes Performance Counter
        QueryPerformanceCounter(counter);
        Result := counter / vCounterFrequency;
      end;
    cmExternal: // User defined value
      Result := FCurrentTime;
  else
    Result := 0;
    Assert(False);
  end;
end;

function TgxCadencer.GetCurrenttime: Double;
begin
  Result := (GetRawReferenceTime - FOriginTime) * FTimeMultiplier;
  FCurrentTime := Result;
end;

function TgxCadencer.IsBusy: Boolean;
begin
  Result := (FProgressing <> 0);
end;

procedure TgxCadencer.Reset;
begin
  lasttime := 0;
  downTime := GetRawReferenceTime;
  FOriginTime := downTime;
end;

procedure TgxCadencer.SetCurrentTime(const Value: Double);
begin
  LastTime := Value - (FCurrentTime - LastTime);
  FOriginTime := FOriginTime + (FCurrentTime - Value);
  FCurrentTime := Value;
end;

// ------------------
// ------------------ TgxCustomCadencedComponent ------------------
// ------------------

destructor TgxCustomCadencedComponent.Destroy;
begin
  Cadencer := nil;
  inherited Destroy;
end;

procedure TgxCustomCadencedComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FCadencer) then
    Cadencer := nil;
  inherited;
end;

procedure TgxCustomCadencedComponent.SetCadencer(const val: TgxCadencer);
begin
  if FCadencer <> val then
  begin
    if Assigned(FCadencer) then
      FCadencer.UnSubscribe(Self);
    FCadencer := val;
    if Assigned(FCadencer) then
      FCadencer.Subscribe(Self);
  end;
end;

// ---------------------------------------------------------------------
initialization
// ---------------------------------------------------------------------

  RegisterClasses([TgxCadencer]);

  // Get our Windows message ID
  vWMTickCadencer := RegisterWindowMessage(cTickCadencer);

  // Preparation for high resolution timer
  if not QueryPerformanceFrequency(vCounterFrequency) then
    vCounterFrequency := 0;

finalization
  FreeAndNil(vHandler);
  FreeAndNil(vASAPCadencerList);
end.