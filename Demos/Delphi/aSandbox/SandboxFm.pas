unit SandboxFm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.ComCtrls,
  GLS.SceneViewer,
  GLS.BaseClasses,
  GLS.Scene,
  GLS.Cadencer,
  GLS.BitmapFont,
  GLS.Material;

type
  TFormSandox = class(TForm)
    GLScene: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    TreeView1: TTreeView;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Repeat1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    PasteSpecial1: TMenuItem;
    Find1: TMenuItem;
    Replace1: TMenuItem;
    GoTo1: TMenuItem;
    Links1: TMenuItem;
    Object1: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    Help1: TMenuItem;
    Contents1: TMenuItem;
    SearchforHelpOn1: TMenuItem;
    HowtoUseHelp1: TMenuItem;
    About1: TMenuItem;
    GLCadencer: TGLCadencer;
    GLMaterialLibrary: TGLMaterialLibrary;
    GLBitmapFont: TGLBitmapFont;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSandox: TFormSandox;

implementation

{$R *.dfm}

end.
