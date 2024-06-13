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
  dImages,
  dDialogs,
  fGLForm, 
  fGLDialog;

type
  TFormOptions = class(TGLDialog)
    CheckBoxAxis: TCheckBox;
    Label1: TLabel;
    PanelBackground: TPanel;
    RadioGroupLanguage: TRadioGroup;
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
    procedure WriteIniFile;
  end;

var
  FormOptions: TFormOptions;

//---------------------------------------------------------------------------
implementation

{$R *.dfm}

uses
  GnuGettext,
  fGLSViewer;


procedure TFormOptions.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
end;

procedure TFormOptions.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

procedure TFormOptions.ReadIniFile;
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
        LANG_ITALIAN : RadioGroupLanguage.ItemIndex := 3;
        else
          RadioGroupLanguage.ItemIndex := 0;
      end;
    finally
      IniFile.Free;
    end;
end;

procedure TFormOptions.RadioGroupLanguageClick(Sender: TObject);
begin
  case RadioGroupLanguage.ItemIndex of
    0: CurLangID := LANG_ENGLISH;
    1: CurLangID := LANG_RUSSIAN;
    2: CurLangID := LANG_SPANISH;
    3: CurLangID := LANG_ITALIAN;
    else
      CurLangID := LANG_ENGLISH;
  end;
end;

procedure TFormOptions.WriteIniFile;
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

procedure TFormOptions.CheckBoxAxisClick(Sender: TObject);
begin
  if CheckBoxAxis.Checked then
    FormGLSViewer.DCAxis.Visible := True
  else
    FormGLSViewer.DCAxis.Visible := False;
end;


procedure TFormOptions.PanelBackgroundClick(Sender: TObject);
begin
   dmDialogs.ColorDialog.Color := PanelBackground.Color;
   if dmDialogs.ColorDialog.Execute then
   begin
     PanelBackground.Color :=  dmDialogs.ColorDialog.Color;
     FormGLSViewer.ApplyBgColor;
   end;
end;

procedure TFormOptions.ButtonOKClick(Sender: TObject);
var
  FileName: TFileName;
begin
  if CurLangID <> LangID then
  begin
    MessageDlg(_('Reload to change language'), mtInformation, [mbOK], 0);
    FileName := ChangeFileExt(ParamStr(0), '.ini');
    if FileExists(UpperCase(FileName)) then
      DeleteFile(UpperCase(FileName)); //to exclude dublicated sections for each language
  end;
end;

end.
