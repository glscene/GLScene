unit fdCylinderExt;

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
  FMX.Types3D,
  FMX.Controls3D,
  FMX.MaterialSources,
  FMX.Objects3D,
  FMX.Viewport3D,
  FMX.Ani,

  GBE.CylinderExtend;

type
  TFormCylinderExt = class(TForm)
    Viewport3D1: TViewport3D;
    LightMaterialSource1: TLightMaterialSource;
    Light1: TLight;
    LightMaterialSource2: TLightMaterialSource;
    Light2: TLight;
    GBECylinderExtend1: TGBECylinderExtend;
    FloatAnimation1: TFloatAnimation;
    LightMaterialSource3: TLightMaterialSource;
    FloatAnimation2: TFloatAnimation;
  private
  public
  end;

var
  FormCylinderExt: TFormCylinderExt;

implementation //------------------------------------------------------------

{$R *.fmx}

end.
