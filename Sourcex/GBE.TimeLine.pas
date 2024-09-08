unit GBE.TimeLine;

interface

uses
  System.SysUtils,
  System.Classes,
  FMX.Types,
  FMX.Ani,
  System.Generics.Collections;

type
  TGBEStep = record
    Duration, Delay: single;
    PropertyName: string;
    StartValue, StopValue: single;
    AutoReverse, Inverse, StartFromCurrent: boolean;
    Interpolation: TInterpolationType;
    AnimationType: TAnimationType;
  end;

  TGBETimeline = class(TFloatAnimation)
  private
    fListeAnimation: TList<TGBEStep>;
    fLoopSteps: boolean;
    function GetCount: integer;
    procedure RunAnimation(Indice: integer);
    procedure Finish(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure AddStep(aStep: TGBEStep);
    procedure Run;
  published
    property count: integer read GetCount;
    property loopSteps: boolean read fLoopSteps write fLoopSteps;
  end;

procedure Register;

implementation // -------------------------------------------------------------

// TGBETimeline

procedure TGBETimeline.AddStep(aStep: TGBEStep);
begin
  fListeAnimation.Add(aStep);
end;

procedure TGBETimeline.Clear;
begin
  fListeAnimation.Clear;
end;

constructor TGBETimeline.Create(AOwner: TComponent);
begin
  inherited;
  fListeAnimation := TList<TGBEStep>.Create;
  fLoopSteps := false;
end;

destructor TGBETimeline.Destroy;
begin
  fListeAnimation.Free;
  inherited;
end;

function TGBETimeline.GetCount: integer;
begin
  result := fListeAnimation.count;
end;

procedure TGBETimeline.Run;
begin
  if fListeAnimation.count > 0 then
    RunAnimation(0);
end;

procedure TGBETimeline.RunAnimation(Indice: integer);
begin
  if indice < fListeAnimation.count then
  begin
    self.Duration := fListeAnimation[indice].Duration;
    self.Delay := fListeAnimation[indice].Delay;
    self.PropertyName := fListeAnimation[indice].PropertyName;
    self.StartValue := fListeAnimation[indice].StartValue;
    self.StopValue := fListeAnimation[indice].StopValue;
    self.AutoReverse := fListeAnimation[indice].AutoReverse;
    self.Inverse := fListeAnimation[indice].Inverse;
    self.StartFromCurrent := fListeAnimation[indice].StartFromCurrent;
    self.Interpolation := fListeAnimation[indice].Interpolation;
    self.AnimationType := fListeAnimation[indice].AnimationType;
    self.Tag := Indice;
    self.OnFinish := Finish;
    self.Start;
  end
  else
  begin
    if fLoopSteps then
      RunAnimation(0);
  end;
end;

procedure TGBETimeline.Finish(Sender: TObject);
begin
  RunAnimation((Sender as TFloatAnimation).Tag + 1);
end;

// ---------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GXScene GBE', [TGBETimeline]);
end;

end.
