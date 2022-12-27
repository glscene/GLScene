unit fLanguageD;

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  GLS.Scene,
  GLS.Objects,
  GLS.Language;

type

  TForm1 = class(TForm)
    Button: TButton;
    GLSLanguage1: TGLSLanguage;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    AboutScene: TMemo;
    mLanguage: TMenuItem;
    mEnglish: TMenuItem;
    mRussian: TMenuItem;
    mOption: TMenuItem;
    Panel1: TPanel;
    mDeutsch: TMenuItem;
    mHelp: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure mEnglishClick(Sender: TObject);
    procedure mRussianClick(Sender: TObject);
    procedure mDeutschClick(Sender: TObject);
  private
    procedure SetLanguage(const AFile: string);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetLanguage('English.ini');
end;

procedure TForm1.SetLanguage(const AFile: string);
begin
  GLSLanguage1.LoadLanguageFromFile(AFile);
  mOption.Caption := GLSLanguage1.Translate('mOption');
  mLanguage.Caption := GLSLanguage1.Translate('mLanguage');
  mEnglish.Caption := GLSLanguage1.Translate('mEnglish');
  mRussian.Caption := GLSLanguage1.Translate('mRussian');
  mHelp.Caption := GLSLanguage1.Translate('mHelp');
  Form1.Caption := GLSLanguage1.Translate('Form1Caption');
  GroupBox1.Caption := GLSLanguage1.Translate('GroupBox1');
  Button.Caption := GLSLanguage1.Translate('Button');
  Label1.Caption := GLSLanguage1.Translate('label1');
  Panel1.Caption := GLSLanguage1.Translate('Panel1');
  AboutScene.Clear;
  AboutScene.Lines.Add(GLSLanguage1.Translate('AboutScene'));
end;

procedure TForm1.mRussianClick(Sender: TObject);
begin
  SetLanguage('Russian.ini');
end;

procedure TForm1.mEnglishClick(Sender: TObject);
begin
  SetLanguage('English.ini');
end;

procedure TForm1.mDeutschClick(Sender: TObject);
begin
  SetLanguage('Deutsch.ini');
end;

end.
