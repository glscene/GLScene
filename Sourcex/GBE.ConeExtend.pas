unit GBE.ConeExtend;

interface

uses
  System.SysUtils,
  System.Classes,
  FMX.Types,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Types3D,
  System.Math.Vectors,
  FMX.MaterialSources;

type
  TCustomMeshHelper = class(TCustomMesh);
  TConeForme = (PyramidTriangle, PyramidSquare, Tipi, Cone);

  TGBEConeExtend = class(TCone)
  private
    fForme: TConeForme;
    fShowlines: boolean;
    fMaterialLignes: TColorMaterialSource;
    procedure SetForme(const Value: TConeForme);
    procedure CreateGBECone(const aData: TMeshData;
      const aForme: TConeForme = TConeForme.Cone);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure Render; override;
  published
    property Forme: TConeForme read fForme write SetForme;
    property ShowLines: boolean read fShowlines write fShowlines;
    property MaterialLines: TColorMaterialSource read fMaterialLignes
      write fMaterialLignes;
  end;

procedure Register;

implementation // --------------------------------------------------------------

// TGBEConeExtend

constructor TGBEConeExtend.Create(AOwner: TComponent);
begin
  inherited;
  fForme := TConeForme.Cone;
end;

procedure TGBEConeExtend.SetForme(const Value: TConeForme);
begin
  if Value <> fForme then
  begin
    fForme := Value;
    CreateGBECone(self.Data, fForme);
  end;
end;

procedure TGBEConeExtend.CreateGBECone(Const aData: TMeshData;
  Const aForme: TConeForme = TConeForme.Cone);
var
  D: TMeshData;
  Co: TCone;
begin
  D := TMeshData.Create;
  Co := TCone.Create(nil);
  case fForme of
    PyramidTriangle:
      Co.SubdivisionsAxes := 3;
    PyramidSquare:
      Co.SubdivisionsAxes := 4;
    Tipi:
      Co.SubdivisionsAxes := 6;
    Cone:
      Co.SubdivisionsAxes := 12;
  end;
  Co.SubdivisionsHeight := self.SubdivisionsHeight;
  Co.SubdivisionsCap := self.SubdivisionsCap;

  D.Assign(TCustomMeshHelper(Co).Data);

  TCustomMeshHelper(Co).Data.Clear;
  Co.Free;

  D.CalcTangentBinormals;

  aData.Clear;
  aData.Assign(D);

  D.Free;
end;

procedure TGBEConeExtend.Render;
begin
  inherited;
  if ShowLines then
    Context.DrawLines(self.Data.VertexBuffer, self.Data.IndexBuffer,
      TMaterialSource.ValidMaterial(fMaterialLignes), 1);
end;

//----------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GXScene GBE', [TGBEConeExtend]);
end;

end.
