unit fdCube;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  System.Math.Vectors,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Viewport3D,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Types3D,
  FMX.MaterialSources,
  FMX.Ani,
  FMX.Objects,

  GBE.CubeExtend;

type
  TFormCube = class(TForm)
    Viewport3D1: TViewport3D;
    Layout1: TLayout;
    CheckBoxFront: TCheckBox;
    CheckBoxRight: TCheckBox;
    CheckBoxBack: TCheckBox;
    CheckBoxLeft: TCheckBox;
    CheckBoxTop: TCheckBox;
    CheckBoxBottom: TCheckBox;
    Light1: TLight;
    LightMaterialSource1: TLightMaterialSource;
    LightMaterialSource2: TLightMaterialSource;
    LightMaterialSource3: TLightMaterialSource;
    LightMaterialSource4: TLightMaterialSource;
    LightMaterialSource5: TLightMaterialSource;
    LightMaterialSource6: TLightMaterialSource;
    Light2: TLight;
    Light3: TLight;
    GBECubeExtend1: TGBECubeExtend;
    FloatAnimation1: TFloatAnimation;
    Rectangle1: TRectangle;
    procedure FloatAnimation1Process(Sender: TObject);
    procedure CheckBoxFrontChange(Sender: TObject);
    procedure CheckBoxRightChange(Sender: TObject);
    procedure CheckBoxBackChange(Sender: TObject);
    procedure CheckBoxLeftChange(Sender: TObject);
    procedure CheckBoxTopChange(Sender: TObject);
    procedure CheckBoxBottomChange(Sender: TObject);
  private
  public
  end;

var
  FormCube: TFormCube;

implementation

{$R *.fmx}

procedure TFormCube.FloatAnimation1Process(Sender: TObject);
begin
  GBECubeExtend1.RotationAngle.X := GBECubeExtend1.RotationAngle.X + 1;
  GBECubeExtend1.RotationAngle.Z := GBECubeExtend1.RotationAngle.Z + 2;
end;

procedure TFormCube.CheckBoxFrontChange(Sender: TObject);
begin
  GBECubeExtend1.FaceFrontVisible := CheckBoxFront.IsChecked;
end;

procedure TFormCube.CheckBoxRightChange(Sender: TObject);
begin
  GBECubeExtend1.FaceRightVisible := CheckBoxRight.IsChecked;
end;

procedure TFormCube.CheckBoxBackChange(Sender: TObject);
begin
  GBECubeExtend1.FaceBackVisible := CheckBoxBack.IsChecked;
end;

procedure TFormCube.CheckBoxLeftChange(Sender: TObject);
begin
  GBECubeExtend1.FaceLeftVisible := CheckBoxLeft.IsChecked;
end;

procedure TFormCube.CheckBoxTopChange(Sender: TObject);
begin
  GBECubeExtend1.FaceTopVisible := CheckBoxTop.IsChecked;
end;

procedure TFormCube.CheckBoxBottomChange(Sender: TObject);
begin
  GBECubeExtend1.FaceBottomVisible := CheckBoxBottom.IsChecked;
end;

end.
