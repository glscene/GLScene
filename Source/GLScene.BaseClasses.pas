//
// The graphics engine GLScene https://github.com/glscene
//
unit GLScene.BaseClasses;

(* Base classes *)

interface

uses
  System.Classes,
  System.SysUtils,

  GLScene.Strings,
  GLScene.PersistentClasses;

type

  TGProgressTimes = packed record
    DeltaTime, NewTime: Double;
    SqrDeltaTime, InvSqrDeltaTime: Single;
  end;

  (* Progression event for time-base animations/simulations.
     deltaTime is the time delta since last progress and newTime is the new
     time after the progress event is completed. *)
  TGProgressEvent = procedure(Sender: TObject; const DeltaTime, NewTime: Double) of object;

  IGNotifyAble = interface(IInterface)
    ['{00079A6C-D46E-4126-86EE-F9E2951B4593}']
    procedure NotifyChange(Sender: TObject);
  end;

  IGProgessAble = interface(IInterface)
    ['{95E44548-B0FE-4607-98D0-CA51169AF8B5}']
    procedure DoProgress(const progressTime: TGProgressTimes);
  end;

  // An abstract class describing the "update" interface.
  TGUpdateAbleObject = class(TGInterfacedPersistent, IGNotifyAble)
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
  TGCadenceAbleComponent = class(TComponent, IGProgessAble)
  public
    procedure DoProgress(const progressTime: TGProgressTimes); virtual;
  end;

  // A base class describing the "update" interface.
  TGUpdateAbleComponent = class(TGCadenceAbleComponent, IGNotifyAble)
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

constructor TGUpdateAbleObject.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TGUpdateAbleObject.NotifyChange(Sender: TObject);
begin
  if FUpdating = 0 then
  begin
    if Assigned(FOwner) then
    begin
      if FOwner is TGUpdateAbleObject then
        TGUpdateAbleObject(FOwner).NotifyChange(Self)
      else if FOwner is TGUpdateAbleComponent then
        TGUpdateAbleComponent(FOwner).NotifyChange(Self);
    end;
    if Assigned(FOnNotifyChange) then
      FOnNotifyChange(Self);
  end;
end;

procedure TGUpdateAbleObject.Notification(Sender: TObject; Operation: TOperation);
begin
end;

function TGUpdateAbleObject.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TGUpdateAbleObject.BeginUpdate;
begin
  Inc(FUpdating);
end;

procedure TGUpdateAbleObject.EndUpdate;
begin
  Dec(FUpdating);
  if FUpdating <= 0 then
  begin
    Assert(FUpdating = 0);
    NotifyChange(Self);
  end;
end;

// ------------------
// ------------------ TGCadenceAbleComponent ------------------
// ------------------

procedure TGCadenceAbleComponent.DoProgress(const progressTime: TGProgressTimes);
begin
  // nothing
end;

// ------------------
// ------------------ TGUpdateAbleObject ------------------
// ------------------

procedure TGUpdateAbleComponent.NotifyChange(Sender: TObject);
begin
  if Assigned(Owner) then
    if (Owner is TGUpdateAbleComponent) then
      (Owner as TGUpdateAbleComponent).NotifyChange(Self);
end;

// ------------------
// ------------------ TGNotifyCollection ------------------
// ------------------

constructor TGNotifyCollection.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, AItemClass);
  if Assigned(AOwner) and (AOwner is TGUpdateAbleComponent) then
    FOnNotifyChange := TGUpdateAbleComponent(AOwner).NotifyChange;
end;

procedure TGNotifyCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnNotifyChange) then
    FOnNotifyChange(Self);
end;

//----------------------------------------------------------------------------

end.



