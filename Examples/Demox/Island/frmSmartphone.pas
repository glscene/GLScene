unit frmSmartphone;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Controls.Presentation, FMX.Objects, FMX.Edit,
  FMX.EditBox, FMX.SpinBox, FMX.ListBox, FMX.Effects;

type
  TfSmartphone = class(TFrame)
    layScreen: TLayout;
    lblHour: TLabel;
    imgCarte: TImage;
    GridLayout1: TGridLayout;
    imgGPS: TImage;
    imgPhoto: TImage;
    imgOptions: TImage;
    imgAide: TImage;
    recGPS: TRectangle;
    Image1: TImage;
    recOptions: TRectangle;
    recAide: TRectangle;
    lblAide: TLabel;
    btnOptionAppliquer: TButton;
    Label1: TLabel;
    cbAliasing: TComboBox;
    cbFPS: TCheckBox;
    sbNuages: TSpinBox;
    cbDetailsOcean: TComboBox;
    Label11: TLabel;
    cbDetailsSol: TComboBox;
    cbAfficherLignes: TCheckBox;
    cbTasks: TCheckBox;
    GlowEffect1: TGlowEffect;
    GlowEffect2: TGlowEffect;
    GlowEffect3: TGlowEffect;
    GlowEffect4: TGlowEffect;
    GlowEffect5: TGlowEffect;
    Rectangle1: TRectangle;
    Image2: TImage;
    layHaut: TLayout;
    Label2: TLabel;
    Label3: TLabel;
    Rectangle2: TRectangle;
    recPhoto: TRectangle;
    Image3: TImage;
    cbHerbe: TCheckBox;
    procedure imgGPSClick(Sender: TObject);
    procedure imgOptionsClick(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure imgAideClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

implementation

{$R *.fmx}

procedure TfSmartphone.FrameResize(Sender: TObject);
begin
  rectangle2.Position.X := (self.Width - rectangle2.Width) * 0.5;
end;

procedure TfSmartphone.Image1Click(Sender: TObject);
begin
  recGPS.Visible := false;
  recOptions.Visible := false;
  recAide.Visible := false;
  recPhoto.Visible := false;
  image1.Visible := false;
end;

procedure TfSmartphone.imgAideClick(Sender: TObject);
begin
  image1.Visible := true;
  recAide.Visible := true;
end;

procedure TfSmartphone.imgGPSClick(Sender: TObject);
begin
  image1.Visible := true;
  recGPS.Visible := true;
end;

procedure TfSmartphone.imgOptionsClick(Sender: TObject);
begin
  image1.Visible := true;
  recOptions.Visible := true;
end;

end.
