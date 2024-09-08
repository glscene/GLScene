unit fdGenoise;

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
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Colors,

  uGBEImageUtils;

type
  TFormNoise = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Layout1: TLayout;
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    ComboBox2: TComboBox;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    ComboColorBox1: TComboColorBox;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FormNoise: TFormNoise;

implementation

{$R *.fmx}

procedure TFormNoise.Button1Click(Sender: TObject);
begin
  image1.Bitmap.Width := strtointdef(combobox1.Selected.Text,256);
  image1.Bitmap.Height := image1.Bitmap.Width;
  image1.Bitmap.CopyFromBitmap(generateDiamondSquare(image1.Bitmap.Width,strtointdef(combobox2.Selected.Text,0), ComboColorBox1.Color, CheckBox1.IsChecked, TALphaColorRec.white));
end;

procedure TFormNoise.Button2Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    Image1.Bitmap.SaveToFile(SaveDialog1.FileName);
  end;
end;

end.
