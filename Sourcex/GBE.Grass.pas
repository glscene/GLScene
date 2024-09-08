unit GBE.Grass;
(*
  TGBEGrass allows you to simulate vegetation in a 3D scene.
  Based on code by Gregory Bersegeay
*)
interface

uses
  System.SysUtils,
  System.Classes,
  System.RTLConsts,
  FMX.Types,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Graphics,
  System.UITypes,
  FMX.Materials,
  FMX.types3D,
  System.Types,
  System.Math.Vectors,
  FMX.Materialsources,
  System.Threading;

type
  TGBEGrass = class(TMesh)
  private
    fWind: boolean;
    fTemps: single;
  protected
    procedure Render; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GenerateGrass;
  published
    property Locked default False;
    property HitTest default False;
    property Temps: single read fTemps write fTemps;
    property Wind: boolean read fWind write fWind;
    property ZWrite default False;
    property Visible default True;
  end;

procedure Register;

implementation //--------------------------------------------------------------

// TGBECubemap

constructor TGBEGrass.Create(AOwner: TComponent);
begin
  inherited;
  Wind := True;
  fTemps := 0;
  ZWrite := False;
  TwoSide := True;
  HitTest := False;
  GenerateGrass;
end;

procedure TGBEGrass.Render;
var
  valeur: single;
begin
  inherited;
  if Wind then
  begin
    TTask.Create(
      procedure
      begin
        fTemps := fTemps + 0.1;
        valeur := sin(fTemps) * 0.1;
        self.Data.VertexBuffer.VerticesPtr[0].X :=
          self.Data.VertexBuffer.VerticesPtr[2].X + valeur;
        self.Data.VertexBuffer.VerticesPtr[1].X :=
          self.Data.VertexBuffer.VerticesPtr[3].X + valeur;
      end).start;
  end;
end;

procedure TGBEGrass.GenerateGrass;
begin
  self.Data.Clear;

  self.Data.Points := '-1 -1 0,  1 -1 0,  -1 1 0,  1 1 0';
  // Positionnement de la texture à chaque points
  self.Data.TexCoordinates := '0.0 0.0, 1 0, 0.0 1, 1 1';
  // Création et indexation des triangles en fonction du besoin
  self.Data.TriangleIndices := '0 1 2 ,2 1 3';
end;

destructor TGBEGrass.Destroy;
begin
  inherited;
end;

//---------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GXScene GBE', [TGBEGrass]);
end;

end.
