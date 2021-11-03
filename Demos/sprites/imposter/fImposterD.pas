unit fImposterD;

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
  
  GLS.Scene,
  GLS.Context,
  GLS.VectorTypes,
  GLS.GeomObjects,
  GLS.Objects,
  GLS.SceneViewer,
  GLS.VectorGeometry,
  GLS.Texture,
  GLS.Cadencer,
  GLS.Imposter,
  GLS.SkyDome,
 
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.RenderContextInfo;

type
  TFormImposter = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLTeapot1: TGLTeapot;
    GLLightSource1: TGLLightSource;
    GLDirectOpenGL1: TGLDirectOpenGL;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    GLSkyDome1: TGLSkyDome;
    GLDummyCube1: TGLDummyCube;
    Panel1: TPanel;
    LabelTexSize: TLabel;
    CBShowTeapot: TCheckBox;
    CBShowImposter: TCheckBox;
    CBSampleSize: TComboBox;
    Label2: TLabel;
    LabelFPS: TLabel;
    procedure GLDirectOpenGL1Render(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure CBSampleSizeChange(Sender: TObject);
    procedure CBShowImposterClick(Sender: TObject);
    procedure CBShowTeapotClick(Sender: TObject);
  private
  public
    impBuilder: TGLStaticImposterBuilder;
    renderPoint: TGLRenderPoint;
    mx, my: Integer;
  end;

var
  FormImposter: TFormImposter;

implementation

{$R *.dfm}

procedure TFormImposter.FormCreate(Sender: TObject);
// var
// x, y : Integer;
begin
  renderPoint := TGLRenderPoint(GLDummyCube1.AddNewChild(TGLRenderPoint));

  impBuilder := TGLStaticImposterBuilder.Create(Self);
  impBuilder.SampleSize := 64;
  impBuilder.SamplingRatioBias := 1.3;
  impBuilder.Coronas.Items[0].Samples := 32;
  impBuilder.Coronas.Add(15, 24);
  impBuilder.Coronas.Add(30, 24);
  impBuilder.Coronas.Add(45, 16);
  impBuilder.Coronas.Add(60, 16);
  impBuilder.Coronas.Add(85, 16);
  impBuilder.renderPoint := renderPoint;

  impBuilder.RequestImposterFor(GLTeapot1);
end;

procedure TFormImposter.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
  camPos, pos: TGLVector;
  imp: TImposter;
  X, Y: Integer;
begin
  imp := impBuilder.ImposterFor(GLTeapot1);
  if (imp = nil) or (imp.Texture.Handle = 0) then
    Exit;

  imp.BeginRender(rci);
  for X := -30 to 30 do
    for Y := -30 to 30 do
    begin
      MakePoint(pos, X * 5, 0, Y * 4);
      camPos := VectorSubtract(rci.cameraPosition, pos);
      imp.Render(rci, pos, camPos, 1);
    end;
  imp.EndRender(rci);
end;

procedure TFormImposter.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TFormImposter.Timer1Timer(Sender: TObject);
begin
  LabelFPS.Caption := GLSceneViewer1.FramesPerSecondText;
  if CBShowImposter.Checked then
    LabelFPS.Caption := LabelFPS.Caption + ' - 3721 imposters';
  GLSceneViewer1.ResetPerformanceMonitor;

  LabelTexSize.Caption := Format('%d x %d - %.1f%%', [impBuilder.TextureSize.X,
    impBuilder.TextureSize.Y, impBuilder.TextureFillRatio * 100]);

end;

procedure TFormImposter.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  end;
  mx := X;
  my := Y;
end;

procedure TFormImposter.CBSampleSizeChange(Sender: TObject);
var
  s: Integer;
begin
  s := StrToInt(CBSampleSize.Text);
  if (GLSceneViewer1.Width >= s) and (GLSceneViewer1.Height >= s) then
    impBuilder.SampleSize := s
  else
  begin
    ShowMessage('Viewer is too small to allow rendering the imposter samples');
  end;
end;

procedure TFormImposter.CBShowImposterClick(Sender: TObject);
begin
  GLDirectOpenGL1.Visible := CBShowImposter.Checked;
end;

procedure TFormImposter.CBShowTeapotClick(Sender: TObject);
begin
  GLTeapot1.Visible := CBShowTeapot.Checked;
end;

end.
