unit fGLOptions;

interface

uses
  Winapi.Windows, 
  Winapi.Messages,
  System.SysUtils,
  System.UITypes,
  System.Variants, 
  System.Classes, 
  System.IniFiles,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.ExtCtrls,

  //
  dGLSViewer, 
  fGLForm, 
  fGLDialog;

type
  TGLOptions = class(TGLDialog)
    CheckBoxAxis: TCheckBox;
    Label1: TLabel;
    RadioGroupLanguage: TRadioGroup;
    PanelBackground: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RadioGroupLanguageClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure PanelBackgroundClick(Sender: TObject);
    procedure CheckBoxAxisClick(Sender: TObject);
  private
     
  public
     
    CurLangID : Word;
    procedure ReadIniFile; override;
    procedure WriteIniFile; override;
  end;

var
  GLOptions: TGLOptions;

implementation

{$R *.dfm}

uses
  GnuGettext,
  fMain;


procedure TGLOptions.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
end;

procedure TGLOptions.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

procedure TGLOptions.ReadIniFile;
begin
  inherited;
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      CheckBoxAxis.Checked := ReadBool(Name, CheckBoxAxis.Name, True);
      PanelBackground.Color  := ReadInteger(Name, PanelBackground.Name, 0);
      LangID := ReadInteger(Name, RadioGroupLanguage.Name, 0);
      case LangID of
        LANG_ENGLISH : RadioGroupLanguage.ItemIndex := 0;
        LANG_RUSSIAN : RadioGroupLanguage.ItemIndex := 1;
        LANG_SPANISH : RadioGroupLanguage.ItemIndex := 2;
        LANG_FRENCH  : RadioGroupLanguage.ItemIndex := 3;
        LANG_GERMAN  : RadioGroupLanguage.ItemIndex := 4;
        LANG_ITALIAN : RadioGroupLanguage.ItemIndex := 5;
        else
          RadioGroupLanguage.ItemIndex := 0;
      end;
    finally
      IniFile.Free;
    end;
end;

procedure TGLOptions.RadioGroupLanguageClick(Sender: TObject);
begin
  case RadioGroupLanguage.ItemIndex of
    0: CurLangID := LANG_ENGLISH;
    1: CurLangID := LANG_RUSSIAN;
    2: CurLangID := LANG_SPANISH;
    3: CurLangID := LANG_FRENCH;
    4: CurLangID := LANG_GERMAN;
    5: CurLangID := LANG_ITALIAN;
    else
      CurLangID := LANG_ENGLISH;
  end;
end;

procedure TGLOptions.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteBool(Name, CheckBoxAxis.Name, CheckBoxAxis.Checked);
      WriteInteger(Name, RadioGroupLanguage.Name, CurLangID);
      WriteInteger(Name, PanelBackground.Name, PanelBackground.Color);
    finally
      IniFile.Free;
    end;
  inherited;
end;

procedure TGLOptions.CheckBoxAxisClick(Sender: TObject);
begin
  if CheckBoxAxis.Checked then
    MainForm.DCAxis.Visible := True
  else
    MainForm.DCAxis.Visible := False;
end;


procedure TGLOptions.PanelBackgroundClick(Sender: TObject);

begin
   dmGLSViewer.ColorDialog.Color := PanelBackground.Color;
   if dmGLSViewer.ColorDialog.Execute then
   begin
     PanelBackground.Color :=  dmGLSViewer.ColorDialog.Color;
     MainForm.ApplyBgColor;
   end;
end;

procedure TGLOptions.ButtonOKClick(Sender: TObject);
var
  FileName: TFileName;
begin
  if CurLangID <> LangID then
  begin
    MessageDlg(_('Reload to change language'),
      mtInformation, [mbOK], 0);
    FileName := ChangeFileExt(ParamStr(0), '.ini');
    if FileExists(UpperCase(FileName)) then
      DeleteFile(UpperCase(FileName)); //to exclude dublicated sections for each language
  end;
end;

end.
