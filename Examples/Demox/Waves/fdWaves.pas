unit fdWaves;

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
  FMX.MaterialSources,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Viewport3D,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Objects,
  FMX.Layouts,
  FMX.Edit,
  FMX.EditBox,
  FMX.SpinBox,
  FMX.Types3D,

  GBE.PlaneExtend;

type
  TFormWaves = class(TForm)
    Viewport3D1: TViewport3D;
    FloatAnimation1: TFloatAnimation;
    Layout1: TLayout;
    Rectangle1: TRectangle;
    TrackBarAmplitude: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    TrackBarLength: TTrackBar;
    LabelSpeed: TLabel;
    TrackBarSpeed: TTrackBar;
    GroupBoxOrigin: TGroupBox;
    Label4: TLabel;
    SpinBoxX: TSpinBox;
    Label5: TLabel;
    SpinBoxY: TSpinBox;
    Label6: TLabel;
    SpinBoxZ: TSpinBox;
    ColorMaterialSource1: TColorMaterialSource;
    SwitchLines: TSwitch;
    LabelLines: TLabel;
    Label8: TLabel;
    TrackBarOpacity: TTrackBar;
    LightMaterialSource1: TLightMaterialSource;
    Light1: TLight;
    GBEPlaneExtend1: TGBEPlaneExtend;
    procedure FloatAnimation1Process(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrackBarAmplitudeTracking(Sender: TObject);
    procedure TrackBarLengthTracking(Sender: TObject);
    procedure TrackBarSpeedTracking(Sender: TObject);
    procedure SpinBoxChange(Sender: TObject);
    procedure SwitchLinesSwitch(Sender: TObject);
    procedure TrackBarOpacityTracking(Sender: TObject);
  private
  public
    center : TPoint3D;
  end;

var
  FormWaves: TFormWaves;

implementation

{$R *.fmx}

procedure TFormWaves.FormCreate(Sender: TObject);
begin
  GBEPlaneExtend1.Origine := Point3D(-13,-13,0);
  TrackBarAmplitude.Value := GBEPlaneExtend1.Amplitude;
  TrackBarLength.Value := GBEPlaneExtend1.Longueur;
  TrackBarSpeed.Value := GBEPlaneExtend1.Vitesse;
  FloatAnimation1.Start;
end;

procedure TFormWaves.FloatAnimation1Process(Sender: TObject);
begin
  Viewport3D1.Repaint;
end;

procedure TFormWaves.SpinBoxChange(Sender: TObject);
begin
  GBEPlaneExtend1.Origine := Point3D(SpinBoxX.Value, SpinBoxY.Value, SpinBoxZ.Value);
end;

procedure TFormWaves.SwitchLinesSwitch(Sender: TObject);
begin
  GBEPlaneExtend1.ShowLines := SwitchLines.IsChecked;
end;

procedure TFormWaves.TrackBarAmplitudeTracking(Sender: TObject);
begin
  GBEPlaneExtend1.Amplitude := TrackBarAmplitude.Value;
end;

procedure TFormWaves.TrackBarLengthTracking(Sender: TObject);
begin
  GBEPlaneExtend1.Longueur := TrackBarLength.Value;
end;

procedure TFormWaves.TrackBarSpeedTracking(Sender: TObject);
begin
  GBEPlaneExtend1.Vitesse := TrackBarSpeed.Value;
end;

procedure TFormWaves.TrackBarOpacityTracking(Sender: TObject);
begin
  GBEPlaneExtend1.Opacity := TrackBarOpacity.Value;
end;

end.
