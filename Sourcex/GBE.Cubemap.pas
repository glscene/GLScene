unit GBE.Cubemap;
(*
  The TGBECubemap allows you to generate a cubemap from an image
  of 12 thumbnails (3 rows, 4 columns).
  Based on code by Gregory Bersegeay
*)
interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math.Vectors,
  FMX.Types,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Graphics,
  System.UITypes,
  FMX.Materials,
  FMX.types3D,
  FMX.Materialsources;

type
  TGBECubemap = class(TMesh)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GenerateCubemap;
  published
    property Locked default False;
    property HitTest default False;
    property Visible default True;
  end;

procedure Register;

implementation // -------------------------------------------------------------

// TGBECubemap

constructor TGBECubemap.Create(AOwner: TComponent);
begin
  inherited;
  TwoSide := True;
  GenerateCubemap;
end;

procedure TGBECubemap.GenerateCubemap;
begin
  self.Data.Clear;
  (*
    There are 18 points to be able to apply the texture correctly
    (8 points are enough for the cube, but then we can only associate one point
    of the texture with a vertex, so we duplicate the necessary vertices
    to be able to apply the texture correctly on the 6 faces).
  *)
  self.Data.Points :=
    '-1 -1 1,  1 -1 1,  -1 1 1,  1 1 1, 1 -1 -1, 1 1 -1, -1 -1 -1, -1 1 -1, -1 -1 1, -1 1 1,' +
  // faces Left, Front, Right, Back
    '-1 -1 1, 1 -1 1, 1 -1 -1, -1 -1 -1, -1 1 -1, -1 1 1, 1 1 -1, 1 1 1';
  (*
    Top and Bottom faces
    Positioning the texture at each point
  *)
  self.Data.TexCoordinates :=
    '0.0 0.34, 0.25 0.34, 0.0 0.66, 0.25 0.66, 0.5 0.34, 0.5 0.66, 0.75 0.34, 0.75 0.66, 1 0.34, 1 0.66,'
    + ' 0.25 0.0, 0.25 0.34, 0.5 0.34, 0.5 0.0, 0.5 1, 0.25 1, 0.5 0.66, 0.25 0.66';
  // Creation and indexing of triangles according to need
  self.Data.TriangleIndices :=
    '0 1 2 ,2 1 3 ,1 4 3, 3 4 5, 4 6 5, 5 6 7, 6 8 7, 7 8 9, 10 11 12, 12 10 13, 14 15 16, 16 15 17';
end;

destructor TGBECubemap.Destroy;
begin
  inherited;
end;

// ---------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GXScene GBE', [TGBECubemap]);
end;

end.
