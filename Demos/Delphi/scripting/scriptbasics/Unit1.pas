unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  
  GLScene,
  GLObjects,
  GLCadencer,
  GLWin32Viewer,
  GLAsyncTimer,
  dwsClasses,
  dwsVectorGeometry,
  dwsGLScene,
  GLDWSObjects,
  dwsComp,
  GLScriptDWS,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLCadencer1: TGLCadencer;
    GLDelphiWebScript1: TGLDelphiWebScript;
    dwsVectorGeometryUnit1: TdwsVectorGeometryUnit;
    GLSphere1: TGLSphere;
    Panel1: TPanel;
    Button3: TButton;
    GLSphere1Script: TMemo;
    CheckBox1: TCheckBox;
    GLCube1Script: TMemo;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLCube1: TGLCube;
    dwsClassesUnit1: TdwsClassesUnit;
    dwsGLSceneUnit1: TdwsGLSceneUnit;
    Label1: TLabel;
    Label2: TLabel;
    AsyncTimer1: TGLAsyncTimer;
    procedure CheckBox1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AsyncTimer1Timer(Sender: TObject);
  private
     
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Set the initial scripts
  Button3Click(Self);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  // Set GLSphere1's script and force a recompile
  with TGLDWSActiveBehaviour(GLSphere1.Behaviours[0]) do begin
    Script.Text:=GLSphere1Script.Lines.Text;
    InvalidateScript;
  end;

  // Set GLCube1's script and force a recompile
  with TGLDWSActiveBehaviour(GLCube1.Behaviours[0]) do begin
    Script.Text:=GLCube1Script.Lines.Text;
    InvalidateScript;
  end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  GLCadencer1.Enabled:=CheckBox1.Checked;
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  Form1.Caption:='GLScene DWS Scripting Basics - '+GLSceneViewer1.FramesPerSecondText;
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
