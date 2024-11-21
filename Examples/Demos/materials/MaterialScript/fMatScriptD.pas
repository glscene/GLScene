unit fMatScriptD;
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
  Vcl.Imaging.Jpeg,
  GLS.Texture,
  GLS.Scene,
  GLS.Objects,
  GLS.SceneViewer,
  GLS.MaterialScript,
  GLS.Cadencer,
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  Stage.Utils,
  GLS.SimpleNavigation;

type
  TFormMatScript = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    ButtonLoadScript: TButton;
    ButtonExecuteScript: TButton;
    Memo2: TMemo;
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLCube1: TGLCube;
    GLCamera1: TGLCamera;
    GLMaterialLibrary1: TGLMaterialLibrary;
    OpenDialog1: TOpenDialog;
    GLMaterialScripter1: TGLMaterialScripter;
    GLCadencer1: TGLCadencer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure ButtonLoadScriptClick(Sender: TObject);
    procedure ButtonExecuteScriptClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    PathToData: TFileName;
  public
  end;
var
  FormMatScript: TFormMatScript;
//---------------------------------
implementation
//---------------------------------
{$R *.dfm}
procedure TFormMatScript.FormCreate(Sender: TObject);
begin
   PathToData := GetCurrentAssetPath();
   SetCurrentDir(PathToData);  // ..glscene\assets
   GLMaterialLibrary1.TexturePaths := PathToData;
   GLMaterialScripter1.DebugMemo := Memo2;
   GLCube1.Material.MaterialLibrary := GLMaterialLibrary1;
end;

procedure TFormMatScript.ButtonLoadScriptClick(Sender: TObject);
begin
   OpenDialog1.InitialDir := PathToData  + '\script';
   if OpenDialog1.Execute then
      if FileExists(Opendialog1.FileName) then
      Memo1.Lines.LoadFromFile(Opendialog1.FileName);
   SetCurrentDir(PathToData);
end;

procedure TFormMatScript.ButtonExecuteScriptClick(Sender: TObject);
begin
   GLMaterialLibrary1.Materials.Clear;
   GLCube1.Material.MaterialLibrary := GLMaterialLibrary1;
   GLMaterialScripter1.Script := Memo1.Lines;
   GLMaterialScripter1.CompileScript;
   GLCube1.Material.LibMaterialName := 'TestMat';
end;
end.
