unit fdTerrain;

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
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Viewport3D,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Edit,
  FMX.EditBox,
  FMX.NumberBox,
  FMX.Types3D,
  FMX.MaterialSources,
  FMX.Ani,
  FMX.Objects,

  GBE.Terrain;

type
  TFormTerrain = class(TForm)
    Viewport3D1: TViewport3D;
    Layout1: TLayout;
    Button1: TButton;
    Label1: TLabel;
    nbAmplitude: TNumberBox;
    Label2: TLabel;
    nbRoughness: TNumberBox;
    FloatAnimation1: TFloatAnimation;
    Label3: TLabel;
    nbSubdivX: TNumberBox;
    Label4: TLabel;
    nbSubdivZ: TNumberBox;
    Label5: TLabel;
    nbOctaves: TNumberBox;
    rbTexture: TRadioButton;
    rbRamp: TRadioButton;
    tmsTexture: TTextureMaterialSource;
    tmsRamp: TTextureMaterialSource;
    Layout2: TLayout;
    Label6: TLabel;
    ColorMaterialSource1: TColorMaterialSource;
    cbShowLines: TCheckBox;
    Rectangle1: TRectangle;
    cbWater: TCheckBox;
    pWater: TPlane;
    ColorMaterialSource2: TColorMaterialSource;
    Label7: TLabel;
    nbSeaRise: TNumberBox;
    dmyScene: TDummy;
    Cylinder1: TCylinder;
    Cylinder2: TCylinder;
    FloatAnimation2: TFloatAnimation;
    FloatAnimation3: TFloatAnimation;
    procedure Button1Click(Sender: TObject);
    procedure rbTextureClick(Sender: TObject);
    procedure rbRampChange(Sender: TObject);
    procedure GBETerrainRender(Sender: TObject; Context: TContext3D);
    procedure nbSeaRiseChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FloatAnimation2Process(Sender: TObject);
  private
    procedure GenerateTerrain;
  public
    GBETerrain : TGBETerrain;
  end;

var
  FormTerrain: TFormTerrain;

implementation  //------------------------------------------------------------

{$R *.fmx}

procedure TFormTerrain.Button1Click(Sender: TObject);
begin
  GenerateTerrain;
end;

procedure TFormTerrain.FloatAnimation2Process(Sender: TObject);
begin
  Cylinder1.Position.Y := GBETerrain.GetHeight(Cylinder1.Position.Point);
end;

procedure TFormTerrain.FormCreate(Sender: TObject);
begin
  GBETerrain := TGBETerrain.Create(dmyScene);
  GBETerrain.Depth := 96;
  GBETerrain.Width := 96;
  GBETerrain.Height := 10;
  GBETerrain.MaterialSource := tmsTexture;
  GBETerrain.TwoSide := true;
  GBETerrain.Parent := dmyScene;
  GBETerrain.OnRender := GBETerrainRender;
  Cylinder1.Parent := GBETerrain;
end;

procedure TFormTerrain.GBETerrainRender(Sender: TObject; Context: TContext3D);
begin
  if cbShowLines.IsChecked then Context.DrawLines(GBETerrain.Data.VertexBuffer, GBETerrain.Data.IndexBuffer, TMaterialSource.ValidMaterial(ColorMaterialSource1), 1);
  pWater.Visible := cbWater.IsChecked;
end;

procedure TFormTerrain.GenerateTerrain;
begin
  layout2.Visible := true;
  FloatAnimation1.Stop;
  GBETerrain.clean;
  GBETerrain.Amplitude := nbAmplitude.Value;
  GBETerrain.Roughness := nbRoughness.Value;
  GBETerrain.SubdivX := trunc(nbSubdivX.Value);
  GBETerrain.SubdivZ := trunc(nbSubdivZ.Value);
  GBETerrain.Octaves := trunc(nbOctaves.value);
  GBETerrain.GenerateTerrain;
  FloatAnimation1.Start;
  FloatAnimation2.Start;
  FloatAnimation3.Start;
  layout2.Visible := false;
end;

procedure TFormTerrain.nbSeaRiseChange(Sender: TObject);
begin
  pWater.Position.Y := nbSeaRise.Value;
end;

procedure TFormTerrain.rbRampChange(Sender: TObject);
begin
  GBETerrain.MaterialSource := tmsRamp;
  GBETerrain.UseRamp := true;
  generateTerrain;
end;

procedure TFormTerrain.rbTextureClick(Sender: TObject);
begin
  GBETerrain.MaterialSource := tmsTexture;
  GBETerrain.UseRamp := false;
  GenerateTerrain;
end;

end.
