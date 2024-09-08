unit fdMultitextures;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.UIConsts,
  System.Math,

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects,
  uGBEImageUtils,
  FMX.Edit,
  FMX.EditBox,
  FMX.SpinBox,
  FMX.Effects,
  FMX.Filter.Effects,
  FMX.Ani,
  FMX.Layouts;

type
  TFormMultitextures = class(TForm)
    imgCarte: TImage;
    imgCanalBleu: TImage;
    imgCanalVert: TImage;
    imgFond: TImage;
    imgCanalRouge: TImage;
    Image6: TImage;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    SpinBox1: TSpinBox;
    Label1: TLabel;
    Button8: TButton;
    SaveDialog1: TSaveDialog;
    Rectangle1: TRectangle;
    Layout1: TLayout;
    Layout2: TLayout;
    GridLayout1: TGridLayout;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    Rectangle4: TRectangle;
    Layout3: TLayout;
    Rectangle5: TRectangle;
    GridLayout2: TGridLayout;
    procedure Button1Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure imgCarteClick(Sender: TObject);
    procedure imgFondClick(Sender: TObject);
    procedure imgCanalRougeClick(Sender: TObject);
    procedure imgCanalVertClick(Sender: TObject);
    procedure imgCanalBleuClick(Sender: TObject);
  private
    procedure CalculerMAxCrop;
  public
  end;

var
  FormMultitextures: TFormMultitextures;

implementation // -------------------------------------------------------------

{$R *.fmx}

procedure TFormMultitextures.Button1Click(Sender: TObject);
begin
  Image6.Bitmap.Width := imgCarte.Bitmap.Width;
  Image6.Bitmap.Height := imgCarte.Bitmap.Height;
  Image6.Bitmap.CopyFromBitmap(MultiTexturing(imgCarte.Bitmap, imgFond.Bitmap,
    imgCanalRouge.Bitmap, imgCanalVert.Bitmap, imgCanalBleu.Bitmap,
    round(SpinBox1.Value)));
end;

procedure TFormMultitextures.CalculerMAxCrop;
var
  maxcrop: integer;
begin
  maxcrop := min(imgCanalBleu.Bitmap.Width, imgCanalBleu.Bitmap.Height);
  maxcrop := min(maxcrop, imgCanalVert.Bitmap.Width);
  maxcrop := min(maxcrop, imgCanalVert.Bitmap.Height);
  maxcrop := min(maxcrop, imgFond.Bitmap.Width);
  maxcrop := min(maxcrop, imgFond.Bitmap.Height);
  maxcrop := min(maxcrop, imgCanalRouge.Bitmap.Width);
  maxcrop := min(maxcrop, imgCanalRouge.Bitmap.Height);

  SpinBox1.Max := maxcrop;
end;

procedure TFormMultitextures.imgCanalBleuClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    imgCanalBleu.Bitmap.LoadFromFile(OpenDialog1.FileName);
  CalculerMAxCrop;
end;

procedure TFormMultitextures.imgCanalRougeClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    imgCanalRouge.Bitmap.LoadFromFile(OpenDialog1.FileName);
  CalculerMAxCrop;
end;

procedure TFormMultitextures.imgCanalVertClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    imgCanalVert.Bitmap.LoadFromFile(OpenDialog1.FileName);
  CalculerMAxCrop;
end;

procedure TFormMultitextures.imgCarteClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    imgCarte.Bitmap.LoadFromFile(OpenDialog1.FileName);
  CalculerMAxCrop;
end;

procedure TFormMultitextures.imgFondClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    imgFond.Bitmap.LoadFromFile(OpenDialog1.FileName);
  CalculerMAxCrop;
end;

procedure TFormMultitextures.Button8Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    Image6.Bitmap.SaveToFile(SaveDialog1.FileName);
end;

end.
