unit fdCubemap;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math.Vectors,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Ani,
  FMX.Controls3D,
  FMX.MaterialSources,
  FMX.Objects3D,
  FMX.Viewport3D,

  GBE.Cubemap;

type
  TFormCubemap = class(TForm)
    Viewport3D1: TViewport3D;
    GBECubemap1: TGBECubemap;
    TextureMaterialSource1: TTextureMaterialSource;
    Camera1: TCamera;
    FloatAnimation1: TFloatAnimation;
    procedure FloatAnimation1Process(Sender: TObject);
  private
  public
  end;

var
  FormCubemap: TFormCubemap;

implementation

{$R *.fmx}

procedure TFormCubemap.FloatAnimation1Process(Sender: TObject);
begin
  Camera1.RotationAngle.X := Camera1.RotationAngle.X + 0.1;
  Camera1.RotationAngle.Y := Camera1.RotationAngle.Y + 0.2;
end;

end.
