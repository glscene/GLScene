unit OcclusionQueryFm;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  
  GLS.Scene,
  GLS.VectorTypes,
  GLS.GeomObjects,
  GLS.Objects,
  GLS.Cadencer,
  GLS.SceneViewer,
 
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.RenderContextInfo,
  GLS.Context;

type
  TFormOcclusionQuery = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLCube1: TGLCube;
    GLCylinder1: TGLCylinder;
    GLDummyCube1: TGLDummyCube;
    OGLBeginQueries: TGLDirectOpenGL;
    dcTestObjects: TGLDummyCube;
    OGLEndQueries: TGLDirectOpenGL;
    GLTorus1: TGLTorus;
    GLLightSource1: TGLLightSource;
    GLDummyCube2: TGLDummyCube;
    GLCube2: TGLCube;
    Timer1: TTimer;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    GLCone1: TGLCone;
    CheckBox1: TCheckBox;
    LabelFPS: TLabel;
    procedure OGLBeginQueriesRender(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure OGLEndQueriesRender(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure FormDestroy(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure GLSceneViewer1BeforeRender(Sender: TObject);
  private
     
  public
     
  end;

var
  FormOcclusionQuery: TFormOcclusionQuery;
  TimerQuery: TGLTimerQueryHandle;
  OcclusionQuery: TGLOcclusionQueryHandle;
  bOcclusionQuery: TGLBooleanOcclusionQueryHandle;

  queriesCreated: boolean;
  timerQuerySupported: Boolean;

  timeTaken: Integer; // in nanoseconds
  samplesPassed: Integer;

implementation

{$R *.dfm}

procedure TFormOcclusionQuery.FormDestroy(Sender: TObject);
begin
  // Delete the queries
  TimerQuery.Free;
  OcclusionQuery.Free;
  bOcclusionQuery.Free;
end;

procedure TFormOcclusionQuery.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  // Move some of the scene objects around
  GLDummyCube1.Position.X := Sin(newTime);
  dcTestObjects.Turn(DeltaTime * 50);
  dcTestObjects.Position.z := 2 * Sin(newTime);
  GLDummyCube2.Position.X := -sin(newTime);
end;

procedure TFormOcclusionQuery.GLSceneViewer1BeforeRender(Sender: TObject);
begin
  // Occlusion queries are supported by extensions with lower version of OpenGL.
  // To use them, you'd need to check if GL_NV_occlusion_query or GL_ARB_occlusion_query
  // extensions are present, and makes the appropriate calls to the functions/procedures
  // they provide.
  if (not TGLOcclusionQueryHandle.IsSupported) then
  begin
    Messagedlg('Requires hardware that supports occlusion queries to run',
      mtError, [mbOK], 0);
    Close;
  end;
end;

procedure TFormOcclusionQuery.OGLBeginQueriesRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  // Generate the queries, if not already created
  if not queriesCreated then
  begin
    OcclusionQuery := TGLOcclusionQueryHandle.CreateAndAllocate();
    CheckBox1.Enabled := TGLBooleanOcclusionQueryHandle.IsSupported;
    if CheckBox1.Enabled then
      bOcclusionQuery := TGLBooleanOcclusionQueryHandle.CreateAndAllocate();

    timerQuerySupported := TGLTimerQueryHandle.IsSupported;
    if timerQuerySupported then
      TimerQuery := TGLTimerQueryHandle.CreateAndAllocate();
    queriesCreated := true;
  end;
  // Begin the timer + occlusion queries

  if timerQuerySupported then
    TimerQuery.BeginQuery;
  if CheckBox1.Checked then
    bOcclusionQuery.BeginQuery
  else
    OcclusionQuery.BeginQuery;
end;

procedure TFormOcclusionQuery.OGLEndQueriesRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
  lQuery: TGLQueryHandle;
begin
  // End the timer + occlusion queries
  if CheckBox1.Checked then
    lQuery := bOcclusionQuery
  else
    lQuery := OcclusionQuery;
  lQuery.EndQuery;
  if timerQuerySupported then
    TimerQuery.EndQuery;

  // Most of the frame rate is lost waiting for results to become available
  //  + updating the captions every frame, but as this is a demo, we want to
  // see what is going on.

  while not lQuery.IsResultAvailable do
    { wait }; // would normally do something in this period before checking if
  // result is available

  samplesPassed := OcclusionQuery.PixelCount;

  if timerQuerySupported then
  begin
    while not TimerQuery.IsResultAvailable do
      { wait }; // would normally do something in this period before checking if
    // result is available
    timeTaken := TimerQuery.Time;
    // Use this line instead of the one above to use 64 bit timer, to allow
    // recording time periods more than a couple of seconds (requires Delphi 7+)
    // timeTaken := TimerQuery.QueryResultUInt64;
  end;

  case CheckBox1.Checked of
    True:
    begin
      label3.Visible := not lQuery.QueryResultBool;
    end;
    False:
    begin
      label3.Visible := (samplesPassed = 0);
      label2.caption := 'Number of test pixels visible: ' + IntToStr(samplesPassed);
    end;  
  end;
end;

procedure TFormOcclusionQuery.Timer1Timer(Sender: TObject);
begin
  // Convert time taken from ns => ms & display
  if timerQuerySupported then
    label1.caption := 'Time taken: ' + FloatToSTr(timeTaken / 1000000) + ' ms'
  else
    label1.Caption := 'Time query unavailable, requires hardware support';

  LabelFPS.Caption := GLSceneViewer1.FramesPerSecondText(0);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.

