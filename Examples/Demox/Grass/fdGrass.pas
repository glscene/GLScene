unit fdGrass;

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
  FMX.Viewport3D,
  FMX.Objects3D,
  FMX.MaterialSources,
  FMX.Controls3D,
  FMX.Types3D,
  FMX.Ani,

  GBE.Grass;

type
  TFormGrass = class(TForm)
    Viewport3D1: TViewport3D;
    FloatAnimation1: TFloatAnimation;
    TextureMaterialSource: TTextureMaterialSource;
    TextureMaterialSource1: TTextureMaterialSource;
    TextureMaterialSource2: TTextureMaterialSource;
    Dummy: TDummy;
    FloatAnimation2: TFloatAnimation;
    FloatAnimation3: TFloatAnimation;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  FormGrass: TFormGrass;

implementation

{$R *.fmx}

procedure TFormGrass.FormCreate(Sender: TObject);
var
  i: Integer;
  GBEGrass: TGBEGrass;
begin
  Randomize;
  for i := 0 to 500 do
  begin
    GBEGrass := TGBEGrass.Create(Self);
    try
      GBEGrass.Position.X := random(40)-20;
      GBEGrass.Position.z := random(40)-20;
      GBEGrass.RotationAngle.y := random(360);
      if (i mod 10 = 0) then
        GBEGrass.MaterialSource := TextureMaterialSource2
      else
      begin
        if (i mod 2 = 0) then
          GBEGrass.MaterialSource := TextureMaterialSource
        else
          GBEGrass.MaterialSource := TextureMaterialSource1;
      end;
      GBEGrass.Width := 5;
      GBEGrass.Height := 5;
      GBEGrass.Depth := 0;
      GBEGrass.Parent := Dummy;
      GBEGrass.Temps := 0.1;
    except
      on EOverflow do Exit;
    end
  end;
end;

end.
