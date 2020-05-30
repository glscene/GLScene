unit Unit1;

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
  
  GLTexture, GLScene, GLObjects, GLWin32Viewer, GLMaterialScript, GLCadencer,
  GLMaterial, GLCoordinates, GLCrossPlatform, GLBaseClasses;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
     
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
   OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));
   if OpenDialog1.Execute then
      if fileexists(Opendialog1.FileName) then
      Memo1.Lines.LoadFromFile(Opendialog1.FileName);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
   GLMaterialLibrary1.Materials.Clear;
   GLCube1.Material.MaterialLibrary := GLMaterialLibrary1;
   GLMaterialScripter1.Script := Memo1.Lines;
   GLMaterialScripter1.CompileScript;
   GLCube1.Material.LibMaterialName := 'TestMat';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   GLMaterialScripter1.DebugMemo := Memo2;;
   GLCube1.Material.MaterialLibrary := GLMaterialLibrary1;
   SetCurrentDir(ExtractFilePath(ParamStr(0)));
end;


end.
