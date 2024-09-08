unit fdHeightmap;

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
  FMX.MaterialSources,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Viewport3D,
  FMX.Ani, FMX.Types3D,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts, FMX.Objects,
  FMX.Edit,
  FMX.EditBox,
  FMX.SpinBox,

  GBE.Heightmap;

type
  TFormHeightmap = class(TForm)
    Viewport3D1: TViewport3D;
    GBEHeightmap1: TGBEHeightmap;
    ColorMaterialSource1: TColorMaterialSource;
    Light1: TLight;
    LightMaterialSource1: TLightMaterialSource;
    SwitchToLines: TSwitch;
    Switch2Ramp: TSwitch;
    LightMaterialSource3: TLightMaterialSource;
    Layout1: TLayout;
    LabelGrid: TLabel;
    Label2: TLabel;
    Rectangle1: TRectangle;
    Image1: TImage;
    Label3: TLabel;
    SpinBoxBlur: TSpinBox;
    Cylinder1: TCylinder;
    FloatAnimation1: TFloatAnimation;
    FloatAnimation2: TFloatAnimation;
    Cylinder2: TCylinder;
    FloatAnimation3: TFloatAnimation;
    procedure FormCreate(Sender: TObject);
    procedure SwitchToGrid(Sender: TObject);
    procedure SwitchToRamp(Sender: TObject);
    procedure SpinBoxBlurChange(Sender: TObject);
    procedure FloatAnimation1Process(Sender: TObject);
  private
  public
  end;

var
  FormHeightmap: TFormHeightmap;

implementation

{$R *.fmx}

procedure TFormHeightmap.FormCreate(Sender: TObject);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Image1.Bitmap.SaveToStream(Stream);
  GBEHeightmap1.loadHeightmapFromStream(Stream);
  Stream.Free;
end;

procedure TFormHeightmap.FloatAnimation1Process(Sender: TObject);
begin
  Cylinder1.Position.Y := GBEHeightmap1.GetHeight(Cylinder1.Position.Point);
end;

procedure TFormHeightmap.SpinBoxBlurChange(Sender: TObject);
begin
  GBEHeightmap1.Flou := trunc(SpinBoxBlur.Value);
end;

procedure TFormHeightmap.SwitchToGrid(Sender: TObject);
begin
  GBEHeightmap1.ShowLines := SwitchToLines.IsChecked;
end;

procedure TFormHeightmap.SwitchToRamp(Sender: TObject);
begin
  if Switch2Ramp.IsChecked then
    GBEHeightmap1.MaterialSource := LightMaterialSource3
  else
    GBEHeightmap1.MaterialSource := LightMaterialSource1;
  GBEHeightmap1.UseRamp := Switch2Ramp.IsChecked;
end;

end.
