unit main;

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
  
  GLObjects,
  GLWin32Viewer,
  GLScene,
  GLScriptDWS,
  dwsComp,
  dwsOpenGLAdapter,
  GLDWSObjects,
  dwsExprs,
  dwsVectorGeometry,
  GLTexture,
  GLCadencer,
  GLAsyncTimer,
  GLBitmapFont,
  GLWindowsFont,
  GLHUDObjects,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,
  GLRenderContextInfo;

type
  TForm1 = class(TForm)
    GLDelphiWebScriptII1: TGLDelphiWebScript;
    dws2OpenGL1xUnit1: TdwsOpenGL1xUnit;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDirectOpenGL1: TGLDirectOpenGL;
    GLDummyCube1: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    AsyncTimer1: TGLAsyncTimer;
    GLHUDText1: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    Panel2: TPanel;
    CompileButton: TButton;
    Script: TMemo;
    dws2VectorGeometryUnit1: TdwsVectorGeometryUnit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GLDirectOpenGL1Render(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure CompileButtonClick(Sender: TObject);
  private
     
  public
     
    mx, my : Integer;
    Prog : TProgram;
    Errors : Boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses OpenGL1x, GLContext;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CompileButtonClick(Self);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Prog.Free;
end;

procedure TForm1.CompileButtonClick(Sender: TObject);
begin
  Prog.Free;
  Prog:=GLDelphiWebScriptII1.Compile(Script.Text);
  Errors:=False;
  GLDirectOpenGL1.Visible:=True;
  GLDirectOpenGL1.StructureChanged;
end;

procedure TForm1.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  if Errors then exit;
  try
    Prog.Execute;
    CheckOpenGLError;
  except
    on E : Exception do begin
      GLHUDText1.ModulateColor.AsWinColor:=clRed;
      GLHUDText1.Text:='Error: '+E.Message;
      Errors:=True;
      GLDirectOpenGL1.Visible:=False;
      Exit;
    end;
  end;
  Errors:=False;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx:=x;
  my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my-y, mx-x);
  mx:=x;
  my:=y;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLDirectOpenGL1.Turn(20*deltaTime);
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  if not Errors then begin
    GLHUDText1.ModulateColor.AsWinColor:=clNavy;
    GLHUDText1.Text:=GLSceneViewer1.FramesPerSecondText;
  end;
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
