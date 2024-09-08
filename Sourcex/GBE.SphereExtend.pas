unit GBE.SphereExtend;
(*
  The TGBESphereExtend originally allows to create Mesh from a TSphere
  Based on code by Gregory Bersegeay
*)
interface

uses
  System.SysUtils,
  System.Classes,
  FMX.Types,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Types3D,
  System.RTLConsts,
  System.Math.Vectors,
  FMX.MaterialSources;

type
  TCustomMeshHelper = class(TCustomMesh);
  TSpheroid = (capsule, dome, culbuto, sphere, apple, pot, diamond);

  TGBESphereExtend = class(TMesh)
  private
    fSubdivisionsAxes, fSubdivisionsHeight: integer;
    fSpheroid: TSpheroid;
    fLongueur: single;
    fShowlines: boolean;
    fMaterialLignes: TColorMaterialSource;
    procedure CreateGBESphere(Const aData: TMeshData;
      Const aForme: TSpheroid = TSpheroid.sphere; Const aLength: single = 1.0);
  protected
    procedure SetForme(Value: TSpheroid);
    procedure SetLongueur(Value: single);
    procedure SetSubdivisionsAxes(Value: integer);
    procedure SetSubdivisionsHeight(Value: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
  published
    property SubdivisionsAxes: integer read fSubdivisionsAxes
      write SetSubdivisionsAxes;
    property SubdivisionsHeight: integer read fSubdivisionsHeight
      write SetSubdivisionsHeight;
    property ShowLines: boolean read fShowlines write fShowlines;
    property Forme: TSpheroid read fSpheroid write SetForme;
    property Longueur: single read fLongueur write SetLongueur;
    property MaterialLines: TColorMaterialSource read fMaterialLignes
      write fMaterialLignes;
  end;

procedure Register;

implementation // -------------------------------------------------------------

constructor TGBESphereExtend.Create(AOwner: TComponent);
begin
  inherited;
  SubdivisionsHeight := 12;
  SubdivisionsAxes := 16;
  CreateGBESphere(self.Data);
end;

procedure TGBESphereExtend.CreateGBESphere(Const aData: TMeshData;
  Const aForme: TSpheroid = TSpheroid.sphere; Const aLength: single = 1.0);
var
  D: TMeshData;
  Sp: TSphere;
  SbA, SbH, Vw, H, A: integer;
  P: PPoint3d;
  K, Yh, L: single;
begin
  D := TMeshData.Create;
  Sp := TSphere.Create(nil);
  Sp.SubdivisionsAxes := SubdivisionsAxes;
  Sp.SubdivisionsHeight := SubdivisionsHeight;
  SbA := Sp.SubdivisionsAxes;
  SbH := Sp.SubdivisionsHeight div 2;

  D.Assign(TCustomMeshHelper(Sp).Data);

  TCustomMeshHelper(Sp).Data.Clear;
  Sp.Free;

  if (aForme <> TSpheroid.sphere) and (aForme <> TSpheroid.diamond) then
  begin
    L := aLength;
    K := L / SbH;
    Yh := L;

    Vw := SbA + 1;

    for H := 0 to SbH - 1 do
    begin
      for A := 0 to SbA do
      begin
        P := D.VertexBuffer.VerticesPtr[A + (H * Vw)];
        if (aForme = TSpheroid.dome) or (aForme = TSpheroid.pot) then
          P^.Y := -L
        else
          P^.Y := P^.Y - Yh;
      end;

      if (aForme = TSpheroid.culbuto) or (aForme = TSpheroid.apple) then
        Yh := Yh - K;
    end;
  end;

  if (aForme = TSpheroid.dome) or (aForme = TSpheroid.pot) then
    D.CalcTangentBinormals
  else
    D.CalcSmoothNormals;

  aData.Clear;
  aData.Assign(D);

  D.Free;
end;

destructor TGBESphereExtend.Destroy;
begin
  inherited;
end;

procedure TGBESphereExtend.SetLongueur(Value: single);
begin
  if Value <> fLongueur then
  begin
    fLongueur := Value;
    CreateGBESphere(self.Data, fSpheroid, fLongueur);
  end;
end;

procedure TGBESphereExtend.SetForme(Value: TSpheroid);
begin
  if Value <> fSpheroid then
  begin
    fSpheroid := Value;
    case fSpheroid of
      TSpheroid.apple:
        fLongueur := -0.4;
      TSpheroid.pot:
        fLongueur := -0.15;
      TSpheroid.diamond:
        begin
          fSubdivisionsAxes := 4;
          fSubdivisionsHeight := 2;
        end;
    end;
    CreateGBESphere(self.Data, fSpheroid, fLongueur);
  end;
end;

procedure TGBESphereExtend.SetSubdivisionsAxes(Value: integer);
begin
  if Value <> fSubdivisionsAxes then
  begin
    fSubdivisionsAxes := Value;
    CreateGBESphere(self.Data, fSpheroid, fLongueur);
  end;
end;

procedure TGBESphereExtend.SetSubdivisionsHeight(Value: integer);
begin
  if Value <> fSubdivisionsHeight then
  begin
    fSubdivisionsHeight := Value;
    CreateGBESphere(self.Data, fSpheroid, fLongueur);
  end;
end;

procedure TGBESphereExtend.Render;
begin
  inherited;
  if ShowLines then
    Context.DrawLines(self.Data.VertexBuffer, self.Data.IndexBuffer,
      TMaterialSource.ValidMaterial(fMaterialLignes), 1);
end;

// ----------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GXScene GBE', [TGBESphereExtend]);
end;

end.
