unit GBE.Clouds;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Math.Vectors,
  System.Threading,

  FMX.Types,
  FMX.Types3D,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.MaterialSources;

type
  TGBEClouds = class(TDummy)
  private
    fListClouds: TList<TPlane>;
    fNbClouds, fLimits: integer;
    fWindSpeed: single;
    fActiveWind, fUseTasks: boolean;
    fTexturesClouds: TList<TTextureMaterialSource>;
    function GetNbClouds: integer;
    function GetWindSpeed: single;
    procedure SetNbClouds(const Value: integer);
    procedure SetWindSpeed(const Value: single);
    function GetLimits: integer;
    procedure SetLimits(const Value: integer);
    function GetActiveWind: boolean;
    procedure SetActiveWind(const Value: boolean);
    procedure DeplacementNuages;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteTexturesClouds;
    procedure AddTextureCloud(TextureMaterial: TTextureMaterialSource);
    procedure MoveClouds;
    procedure GenerateClouds;
  published
    property ActiveWind: boolean read GetActiveWind write SetActiveWind;
    property Limits: integer read GetLimits write SetLimits;
    property NbClouds: integer read GetNbClouds write SetNbClouds;
    property WindSpeed: single read GetWindSpeed write SetWindSpeed;
    property UseTasks: boolean read fUseTasks write fUseTasks;
  end;

procedure Register;

implementation // --------------------------------------------------------------

// TGBEClouds

procedure TGBEClouds.AddTextureCloud(TextureMaterial: TTextureMaterialSource);
begin
  fTexturesClouds.Add(TextureMaterial);
end;

constructor TGBEClouds.Create(AOwner: TComponent);
begin
  inherited;
  hitTest := false;
  fLimits := 200;
  fWindSpeed := 0.5;
  fNbClouds := 0;
  fActiveWind := false;
  fListClouds := TList<TPlane>.Create;
  fTexturesClouds := TList<TTextureMaterialSource>.Create;
  fUseTasks := true;
end;

procedure TGBEClouds.DeleteTexturesClouds;
begin
  if fTexturesClouds.Count > 0 then
    fTexturesClouds.Clear;
end;

destructor TGBEClouds.Destroy;
begin
  fListClouds.Free;
  fTexturesClouds.Free;
  DoDeleteChildren;
  inherited;
end;

function TGBEClouds.GetActiveWind: boolean;
begin
  Result := fActiveWind;
end;

function TGBEClouds.GetLimits: integer;
begin
  Result := fLimits;
end;

function TGBEClouds.GetNbClouds: integer;
begin
  Result := fNbClouds;
end;

function TGBEClouds.GetWindSpeed: single;
begin
  Result := fWindSpeed;
end;

procedure TGBEClouds.MoveClouds;
begin
  if (fActiveWind) and (NbClouds > 0) then
  begin
    if fUseTasks then
    begin
      TTask.Create(
        procedure
        begin
          DeplacementNuages;
        end).start;
    end
    else
    begin
      DeplacementNuages;
    end;
  end;
end;

procedure TGBEClouds.DeplacementNuages;
var
  s: TPlane;
  P: TFmxObject;
  // Will serve as an iterator to traverse all child objects of dmyNuages
begin
  for P in self.Children do // Traversing the child objects of dmyNuages
  begin
    if P is TPlane then // If the object is a TPlane
    begin
      s := TPlane(P); // We will work on this TPlane
      s.position.x := s.position.x + fWindSpeed / (s.position.z);
      if (s.position.x > fLimits) or (s.position.x < -fLimits) then
      // If the X position of the cloud > 1000,
      // then we reposition the cloud at position x = -1000 and Y and Z random values
      begin
        s.position.point := Point3D(-fLimits, random - 0.5,
          random * fLimits * (0.5 - random));
        s.Opacity := random;
      end;
    end;
  end;
end;

procedure TGBEClouds.SetActiveWind(const Value: boolean);
begin
  if Value <> fActiveWind then
    fActiveWind := Value;
end;

procedure TGBEClouds.SetLimits(const Value: integer);
begin
  if Value <> fLimits then
  begin
    fLimits := Value;
  end;
end;

procedure TGBEClouds.SetNbClouds(const Value: integer);
begin
  if Value <> fNbClouds then
  begin
    fNbClouds := Value;
    GenerateClouds;
  end;
end;

procedure TGBEClouds.SetWindSpeed(const Value: single);
begin
  if Value <> fWindSpeed then
    fWindSpeed := Value;
end;

procedure TGBEClouds.GenerateClouds;
var
  s: TPlane;
  taille: integer;
  i, nbTextures: integer;
begin
  self.DeleteChildren;
  fListClouds.Clear;

  Randomize;
  nbTextures := fTexturesClouds.Count;

  for i := 0 to NbClouds - 1 do
  begin
    s := TPlane.Create(nil);
    s.parent := self;
    taille := random(fLimits);

    s.MaterialSource := fTexturesClouds[random(nbTextures) mod nbTextures];
    s.SetSize(taille, taille * 0.5, 0.001);

    s.TwoSide := true;
    // To make the texture apply to both sides of the TPlane
    s.RotationAngle.x := -90; // To orient the TPlanes parallel to the ground
    s.Opacity := random(); // Random opacity to improve rendering
    s.Opaque := false;
    s.ZWrite := false;
    // to avoid the TPlane "frame" rectangle being visible =>
    // but then the depth is no longer managed: the Sun passes in front of the clouds...
    s.hitTest := false; // so you can't click on it
    s.tag := self.tag;
    s.position.point := Point3D(random() * fLimits * (0.5 - Random()),
      Random() - 0.5, Random() * fLimits * (0.5 - Random()));
    // We position the cloud arbitrarily and randomly everywhere above our world
    s.RotationAngle.z := Random() * 360; // Random cloud orientation
  end;
end;

procedure Register;
begin
  RegisterComponents('GXScene GBE', [TGBEClouds]);
end;

end.
