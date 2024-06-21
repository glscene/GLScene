//---------------------------------------
// This unit is part of the GLSViewer
//---------------------------------------

unit fGLForm;

(* The fGLForm unit for TGLForm class as parent for all child forms of GLSViewer *)

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.IniFiles,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Menus,
  Vcl.Actnlist,

  gnuGettext;

type
  TGLForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
     
  public
    IniFile : TIniFile;
    procedure ReadIniFile; virtual;
    procedure SetLanguage;
  end;

var
  GLForm: TGLForm;
  LangID : Word;

implementation

{$R *.dfm}

//Here goes the translation of all component strings
//
procedure TGLForm.FormCreate(Sender: TObject);
begin
  SetLanguage;
end;

//----------------------------------------------------------
procedure TGLForm.SetLanguage;
var
  LocalePath : TFileName;
  IniFile : TIniFile;

begin
  LocalePath := ExtractFileDir(ParamStr(0)); // Path to GLSViewer
  LocalePath := LocalePath + PathDelim + 'Locale' + PathDelim;

  ReadIniFile;
  if (LangID <> LANG_ENGLISH) then
  begin
    Textdomain('glsviewer');
    BindTextDomain ('glsviewer', LocalePath);
    AddDomainForResourceString('language');
    BindTextDomain ('language', LocalePath);

    //TP_GlobalIgnoreClass(TListBox);
    //TP_GlobalIgnoreClass(TStaticText);
    //TP_GlobalIgnoreClass(TGLLibMaterial);
    //TP_GlobalIgnoreClass(TGLMaterialLibrary);
    // TP_IgnoreClass(TFont);
    // TP_GlobalIgnoreClassProperty(TAction, 'Category');
    // Removing the upper line will cause long loading but Action.Category translation

    case LangID of
      LANG_RUSSIAN:
      begin
        UseLanguage('ru');
        // Help based on demos.htm
        Application.HelpFile := UpperCase(LocalePath + 'ru'+ PathDelim+'Demos.chm');
      end
      else
      begin
        UseLanguage('en');
        Application.HelpFile := UpperCase(LocalePath + 'en'+ PathDelim+'Demos.chm');
      end;
    end;
  end
  else
  begin
    UseLanguage('en');
    Application.HelpFile := UpperCase(LocalePath + 'en'+ PathDelim+'GLSViewer.chm');
  end;
  TranslateComponent(Self);
end;

procedure TGLForm.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      // use correct argument names
      LangID := ReadInteger('FormOptions', 'rgLanguage', 0);
    finally
      IniFile.Free;
    end;
end;

end.
