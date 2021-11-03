unit fFontgenD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  GLS.BitmapFont,
  GLS.WindowsFont;

type
  TFormFontGen = class(TForm)
    GroupBox1: TGroupBox;
    Panel1: TPanel;
    Button1: TButton;
    FontDialog1: TFontDialog;
    GroupBox2: TGroupBox;
    Image1: TImage;
    GroupBox3: TGroupBox;
    SaveDialog1: TSaveDialog;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  public
    FFont: TGLWindowsBitmapFont;
  end;

var
  FormFontGen: TFormFontGen;

implementation

{$R *.dfm}

procedure TFormFontGen.Button1Click(Sender: TObject);
begin
  FontDialog1.Font := Panel1.Font;
  if FontDialog1.Execute then
  begin
    Panel1.Font := FontDialog1.Font;
    FFont.Font := FontDialog1.Font;
    Image1.Picture.Assign(FFont.Glyphs);
  end;
end;

procedure TFormFontGen.FormCreate(Sender: TObject);
begin
  FFont := TGLWindowsBitmapFont.Create(Self);
end;

procedure TFormFontGen.Button2Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    FFont.Glyphs.SaveToFile(SaveDialog1.FileName);
  end;
end;

end.
