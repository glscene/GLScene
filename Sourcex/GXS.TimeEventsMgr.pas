//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.TimeEventsMgr;

(*
   Time based events mannager using the Cadencer
   can be useful to make animations
*)

interface

uses
  System.Classes,
  System.SysUtils,

  GXS.Cadencer,
  GXS.BaseClasses;

type

  TgxTimeEvent = class;
  TgxTimeEvents = class;

  TgxTimeEventsMGR = class(TgxUpdateAbleComponent)
  private
    FCadencer: TgxCadencer;
    FEnabled: boolean;
    FFreeEventOnEnd: boolean;
    FEvents: TgxTimeEvents;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetCadencer(const val: TgxCadencer);
    procedure SetEvents(const val: TgxTimeEvents);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoProgress(const progressTime: TgxProgressTimes); override;
    procedure Reset();
  published
    property Cadencer: TgxCadencer read FCadencer write SetCadencer;
    property Enabled: boolean read FEnabled write FEnabled default True;
    property FreeEventOnEnd: boolean read FFreeEventOnEnd write FFreeEventOnEnd default False;
    property Events: TgxTimeEvents read FEvents write SetEvents;
  end;

  TgxTimeEvents = class(TCollection)
  protected
    Owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TgxTimeEvent);
    function GetItems(index: Integer): TgxTimeEvent;
  public
    constructor Create(aOwner: TComponent);
    function Add: TgxTimeEvent;
    function FindItemID(ID: Integer): TgxTimeEvent;
    function EventByName(name: String): TgxTimeEvent;
    property Items[index: Integer]: TgxTimeEvent read GetItems write SetItems; default;
  end;

  TgxTimeEventType = (etOneShot, etContinuous, etPeriodic);
  TgxTimeEventProc = procedure(event: TgxTimeEvent) of object;

  TgxTimeEvent = class(TCollectionItem)
  private
    FName: String;
    FStartTime, FEndTime, FElapsedTime: Double;
    FPeriod: Double;
    FEventType: TgxTimeEventType;
    FOnEvent: TgxTimeEventProc;
    FEnabled: boolean;
    FTickCount: Cardinal;
    procedure SetEnabled(const Value: boolean);
  protected
    function GetDisplayName: String; override;
    procedure SetName(val: String);
    procedure DoEvent(const curTime: Double);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    // Number of times the event was triggered since activation
    property TickCount: Cardinal read FTickCount;
    // Elapsed time since the event was activated
    property ElapsedTime: Double read FElapsedTime;
  published
    property Name: String read FName write SetName;
    property StartTime: Double read FStartTime write FStartTime;
    property EndTime: Double read FEndTime write FEndTime;
    property Period: Double read FPeriod write FPeriod;
    property EventType: TgxTimeEventType read FEventType write FEventType default etOneShot;
    property OnEvent: TgxTimeEventProc read FOnEvent write FOnEvent;
    property Enabled: boolean read FEnabled write SetEnabled default True;
  end;

//---------------------------------------------------------
implementation
//---------------------------------------------------------

// ------------------
// ------------------ TgxTimeEventsMGR ------------------
// ------------------

constructor TgxTimeEventsMGR.Create(aOwner: TComponent);
begin
  inherited;
  FEnabled := True;
  FFreeEventOnEnd := False;
  FEvents := TgxTimeEvents.Create(self);
end;

destructor TgxTimeEventsMGR.Destroy;
begin
  Cadencer := nil;
  FEvents.Free;
  inherited Destroy;
end;

procedure TgxTimeEventsMGR.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = Cadencer) then
    FCadencer := nil;
  inherited;
end;

procedure TgxTimeEventsMGR.SetCadencer(const val: TgxCadencer);
begin
  if FCadencer <> val then
  begin
    if Assigned(FCadencer) then
      FCadencer.UnSubscribe(self);
    FCadencer := val;
    if Assigned(FCadencer) then
      FCadencer.Subscribe(self);
  end;
end;

procedure TgxTimeEventsMGR.SetEvents(const val: TgxTimeEvents);
begin
  FEvents.Assign(val);
end;

procedure TgxTimeEventsMGR.DoProgress(const progressTime: TgxProgressTimes);
var
  i: Integer;
begin
  if not Enabled then
    Exit;

  i := 0;
  with progressTime do
    while i <= Events.Count - 1 do
      with Events.Items[i] do
      begin
        if Enabled and Assigned(FOnEvent) then
        begin
          case EventType of
            etOneShot:
              if (newTime >= StartTime) and (TickCount = 0) then
                DoEvent(newTime);
            etContinuous:
              if (newTime >= StartTime) and ((newTime <= EndTime) or (EndTime <= 0)) then
                DoEvent(newTime);
            etPeriodic:
              if (newTime >= StartTime + TickCount * Period) and ((newTime <= EndTime) or (EndTime <= 0)) then
                DoEvent(newTime);
          else
            Assert(False);
          end;
        end;
        if FreeEventOnEnd and (((EventType <> etOneShot) and (newTime > EndTime) and (EndTime >= 0)) or
          ((EventType = etOneShot) and (TickCount > 0))) then
          Events[i].Free
        else
        begin
          // if we delete current event, the next will have same index
          // so increment only if we don't delete
          Inc(i);
        end;
      end;
end;

procedure TgxTimeEventsMGR.Reset;
var
  i: Integer;
begin
  if FEvents.Count <> 0 then
    for i := 0 to FEvents.Count - 1 do
      FEvents[i].FTickCount := 0;
end;


// ------------------
// ------------------ TgxTimeEvents ------------------
// ------------------

constructor TgxTimeEvents.Create(aOwner: TComponent);
begin
  Owner := aOwner;
  inherited Create(TgxTimeEvent);
end;

function TgxTimeEvents.GetOwner: TPersistent;
begin
  Result := Owner;
end;

procedure TgxTimeEvents.SetItems(index: Integer; const val: TgxTimeEvent);
begin
  inherited Items[index] := val;
end;

function TgxTimeEvents.GetItems(index: Integer): TgxTimeEvent;
begin
  Result := TgxTimeEvent(inherited Items[index]);
end;

function TgxTimeEvents.Add: TgxTimeEvent;
begin
  Result := (inherited Add) as TgxTimeEvent;
end;

function TgxTimeEvents.FindItemID(ID: Integer): TgxTimeEvent;
begin
  Result := (inherited FindItemID(ID)) as TgxTimeEvent;
end;

function TgxTimeEvents.EventByName(name: String): TgxTimeEvent;
var
  i: Integer;
begin
  i := 0;
  while (i < Count) and (Items[i].FName <> name) do
    Inc(i);

  if i = Count then
    Result := nil
  else
    Result := Items[i];
end;

// ------------------
// ------------------ TgxTimeEvent ------------------
// ------------------

constructor TgxTimeEvent.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FEventType := etOneShot;
  FName := Format('Event%d', [index]); // give a default name different for each event
  FEnabled := True;
end;

destructor TgxTimeEvent.Destroy;
begin
  inherited Destroy;
end;

function TgxTimeEvent.GetDisplayName: String;
begin
  case EventType of
    etOneShot:
      Result := Name + Format(' (OneShot ST=%g)', [StartTime]);
    etContinuous:
      Result := Name + Format(' (Continuous ST=%g ET=%g)', [StartTime, EndTime]);
    etPeriodic:
      Result := Name + Format(' (Periodic ST=%g ET=%g P=%g)', [StartTime, EndTime, Period]);
  end;
end;

procedure TgxTimeEvent.SetName(val: String);
var
  i: Integer;
  ok: boolean;
begin
  ok := True;
  with self.Collection as TgxTimeEvents do // we mustn't have 2 events with the same name (for EventByName)
    for i := 0 to Count - 1 do
      if Items[i].FName = val then
        ok := False;

  if ok and (val <> '') then
    FName := val;
end;

procedure TgxTimeEvent.DoEvent(const curTime: Double);
begin
  if Assigned(FOnEvent) then
  begin
    FElapsedTime := curTime - StartTime;
    FOnEvent(self);
  end;
  Inc(FTickCount);
end;

procedure TgxTimeEvent.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
  FStartTime := ((GetOwner as TgxTimeEvents).Owner as TgxTimeEventsMGR).Cadencer.CurrentTime;
end;

end.
