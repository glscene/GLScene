//
// The graphics engine GXScene
//
unit GXS.BaseClasses;

(* Base classes *)

interface

uses
  System.Classes,
  System.SysUtils,

  Stage.Strings,
  GXS.PersistentClasses;

type
  TgxProgressTimes = packed record
    DeltaTime, NewTime: Double;
    SqrDeltaTime, InvSqrDeltaTime: Single;
  end;

  (* Progression event for time-base animations/simulations.
     deltaTime is the time delta since last progress and newTime is the new
     time after the progress event is completed. *)
  TgxProgressEvent = procedure(Sender: TObject; const DeltaTime, NewTime: Double) of object;

  IgxNotifyAble = interface(IInterface)
    ['{00079A6C-D46E-4126-86EE-F9E2951B4593}']
    procedure NotifyChange(Sender: TObject);
  end;

  IgxProgessAble = interface(IInterface)
    ['{95E44548-B0FE-4607-98D0-CA51169AF8B5}']
    procedure DoProgress(const progressTime: TgxProgressTimes);
  end;

  // An abstract class describing the "update" interface.
  TgxUpdateAbleObject = class(TgxInterfacedPersistent, IgxNotifyAble)
  private
    FOwner: TPersistent;
    FUpdating: Integer;
    FOnNotifyChange: TNotifyEvent;
  protected
    function GetOwner: TPersistent; override; final;
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure NotifyChange(Sender: TObject); virtual;
    procedure Notification(Sender: TObject; Operation: TOperation); virtual;
    property Updating: Integer read FUpdating;
    procedure BeginUpdate; inline;
    procedure EndUpdate; inline;
    property Owner: TPersistent read FOwner;
    property OnNotifyChange: TNotifyEvent read FOnNotifyChange write FOnNotifyChange;
  end;

  // A base class describing the "cadenceing" interface.
  TgxCadenceAbleComponent = class(TComponent, IgxProgessAble)
  public
    procedure DoProgress(const progressTime: TgxProgressTimes); virtual;
  end;

  // A base class describing the "update" interface.
  TgxUpdateAbleComponent = class(TgxCadenceAbleComponent, IgxNotifyAble)
  public
    procedure NotifyChange(Sender: TObject); virtual;
  end;

  TGNotifyCollection = class(TOwnedCollection)
  strict private
    FOnNotifyChange: TNotifyEvent;
  strict protected
    procedure Update(item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    property OnNotifyChange: TNotifyEvent read FOnNotifyChange write FOnNotifyChange;
  end;

implementation //---------------------------------------------------------------

constructor TgxUpdateAbleObject.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TgxUpdateAbleObject.NotifyChange(Sender: TObject);
begin
  if FUpdating = 0 then
  begin
    if Assigned(FOwner) then
    begin
      if FOwner is TgxUpdateAbleObject then
        TgxUpdateAbleObject(FOwner).NotifyChange(Self)
      else if FOwner is TgxUpdateAbleComponent then
        TgxUpdateAbleComponent(FOwner).NotifyChange(Self);
    end;
    if Assigned(FOnNotifyChange) then
      FOnNotifyChange(Self);
  end;
end;

procedure TgxUpdateAbleObject.Notification(Sender: TObject; Operation: TOperation);
begin
end;

function TgxUpdateAbleObject.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TgxUpdateAbleObject.BeginUpdate;
begin
  Inc(FUpdating);
end;

procedure TgxUpdateAbleObject.EndUpdate;
begin
  Dec(FUpdating);
  if FUpdating <= 0 then
  begin
    Assert(FUpdating = 0);
    NotifyChange(Self);
  end;
end;

// ------------------
// ------------------ TgxCadenceAbleComponent ------------------
// ------------------

procedure TgxCadenceAbleComponent.DoProgress(const progressTime: TgxProgressTimes);
begin
  // nothing
end;

// ------------------
// ------------------ TgxUpdateAbleObject ------------------
// ------------------

procedure TgxUpdateAbleComponent.NotifyChange(Sender: TObject);
begin
  if Assigned(Owner) then
    if (Owner is TgxUpdateAbleComponent) then
      (Owner as TgxUpdateAbleComponent).NotifyChange(Self);
end;

// ------------------
// ------------------ TGNotifyCollection ------------------
// ------------------

constructor TGNotifyCollection.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, AItemClass);
  if Assigned(AOwner) and (AOwner is TgxUpdateAbleComponent) then
    FOnNotifyChange := TgxUpdateAbleComponent(AOwner).NotifyChange;
end;

procedure TGNotifyCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnNotifyChange) then
    FOnNotifyChange(Self);
end;

//----------------------------------------------------------------------------

end.



