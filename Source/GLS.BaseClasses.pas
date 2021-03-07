//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.BaseClasses;

(* Base classes *)

interface

uses
  System.Classes,
  System.SysUtils,

  GLS.Strings,
  GLS.PersistentClasses;

type

  TGLProgressTimes = packed record
    DeltaTime, NewTime: Double;
    SqrDeltaTime, InvSqrDeltaTime: Single;
  end;

  (* Progression event for time-base animations/simulations.
     deltaTime is the time delta since last progress and newTime is the new
     time after the progress event is completed. *)
  TGLProgressEvent = procedure(Sender: TObject; const DeltaTime, NewTime: Double) of object;

  IGLNotifyAble = interface(IInterface)
    ['{00079A6C-D46E-4126-86EE-F9E2951B4593}']
    procedure NotifyChange(Sender: TObject);
  end;

  IGLProgessAble = interface(IInterface)
    ['{95E44548-B0FE-4607-98D0-CA51169AF8B5}']
    procedure DoProgress(const progressTime: TGLProgressTimes);
  end;

  // An abstract class describing the "update" interface.
  TGLUpdateAbleObject = class(TGLInterfacedPersistent, IGLNotifyAble)
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
  TGLCadenceAbleComponent = class(TComponent, IGLProgessAble)
  public
    procedure DoProgress(const progressTime: TGLProgressTimes); virtual;
  end;

  // A base class describing the "update" interface.
  TGLUpdateAbleComponent = class(TGLCadenceAbleComponent, IGLNotifyAble)
  public
    procedure NotifyChange(Sender: TObject); virtual;
  end;

  TGLNotifyCollection = class(TOwnedCollection)
  strict private
    FOnNotifyChange: TNotifyEvent;
  strict protected
    procedure Update(item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    property OnNotifyChange: TNotifyEvent read FOnNotifyChange write FOnNotifyChange;
  end;

//-------------------------------------------------------------------------
implementation
//-------------------------------------------------------------------------

//---------------------- TGLUpdateAbleObject -----------------------------------

constructor TGLUpdateAbleObject.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TGLUpdateAbleObject.NotifyChange(Sender: TObject);
begin
  if FUpdating = 0 then
  begin
    if Assigned(FOwner) then
    begin
      if FOwner is TGLUpdateAbleObject then
        TGLUpdateAbleObject(FOwner).NotifyChange(Self)
      else if FOwner is TGLUpdateAbleComponent then
        TGLUpdateAbleComponent(FOwner).NotifyChange(Self);
    end;
    if Assigned(FOnNotifyChange) then
      FOnNotifyChange(Self);
  end;
end;

procedure TGLUpdateAbleObject.Notification(Sender: TObject; Operation: TOperation);
begin
end;

function TGLUpdateAbleObject.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TGLUpdateAbleObject.BeginUpdate;
begin
  Inc(FUpdating);
end;

procedure TGLUpdateAbleObject.EndUpdate;
begin
  Dec(FUpdating);
  if FUpdating <= 0 then
  begin
    Assert(FUpdating = 0);
    NotifyChange(Self);
  end;
end;

// ------------------
// ------------------ TGLCadenceAbleComponent ------------------
// ------------------

procedure TGLCadenceAbleComponent.DoProgress(const progressTime: TGLProgressTimes);
begin
  // nothing
end;

// ------------------
// ------------------ TGLUpdateAbleObject ------------------
// ------------------

procedure TGLUpdateAbleComponent.NotifyChange(Sender: TObject);
begin
  if Assigned(Owner) then
    if (Owner is TGLUpdateAbleComponent) then
      (Owner as TGLUpdateAbleComponent).NotifyChange(Self);
end;

// ------------------
// ------------------ TGLNotifyCollection ------------------
// ------------------

constructor TGLNotifyCollection.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, AItemClass);
  if Assigned(AOwner) and (AOwner is TGLUpdateAbleComponent) then
    FOnNotifyChange := TGLUpdateAbleComponent(AOwner).NotifyChange;
end;

procedure TGLNotifyCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnNotifyChange) then
    FOnNotifyChange(Self);
end;

end.



