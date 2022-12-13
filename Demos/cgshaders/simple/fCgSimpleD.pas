unit fCgSimpleD;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,

  Cg.Import,
  Cg.GL,
  Cg.Shader,

  GLS.Scene,
  GLS.VectorTypes,
  GLS.Objects,
  GLS.SceneViewer,
  GLS.Texture,

  GLS.VectorGeometry,
  GLS.Cadencer,
  GLS.VectorFileObjects,
  GLS.File3DS,
  GLS.Graph,

  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.Utils;

type
  TFormCgSimple = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    CgShader1: TCgShader;
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Splitter1: TSplitter;
    Panel2: TPanel;
    CBVertexProgram: TCheckBox;
    LabelVertProfile: TLabel;
    Panel4: TPanel;
    LabelFragProfile: TLabel;
    CBFragmentProgram: TCheckBox;
    Splitter2: TSplitter;
    Panel6: TPanel;
    Panel7: TPanel;
    MemoFragCode: TMemo;
    Panel8: TPanel;
    Memo3: TMemo;
    Panel3: TPanel;
    ButtonApplyFP: TButton;
    Panel11: TPanel;
    Panel12: TPanel;
    MemoVertCode: TMemo;
    Panel13: TPanel;
    ButtonApplyVP: TButton;
    Splitter3: TSplitter;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Panel5: TPanel;
    Label2: TLabel;
    Memo1: TMemo;
    Button1: TButton;
    Button4: TButton;
    GLFreeForm1: TGLFreeForm;
    Panel9: TPanel;
    PanelFPS: TPanel;
    GLSceneViewer1: TGLSceneViewer;
    Timer1: TTimer;
    GLXYZGrid1: TGLXYZGrid;
    GLDummyCube1: TGLDummyCube;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure CgShader1ApplyVP(CgProgram: TCgProgram; Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure CBVertexProgramClick(Sender: TObject);
    procedure CBFragmentProgramClick(Sender: TObject);
    procedure ButtonApplyFPClick(Sender: TObject);
    procedure MemoFragCodeChange(Sender: TObject);
    procedure MemoVertCodeChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ButtonApplyVPClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure CgShader1Initialize(CgShader: TCustomCgShader);
  private
     
  public
     
    mx, my : Integer;
  end;

var
  FormCgSimple: TFormCgSimple;

implementation

{$R *.dfm}

procedure TFormCgSimple.FormCreate(Sender: TObject);
begin
   var Path: TFileName := GetCurrentAssetPath();
   SetCurrentDir(Path  + '\shader');
   // Load Cg proggy
   with CgShader1 do begin
     VertexProgram.LoadFromFile('simple_vp.cg');
     MemoVertCode.Lines.Assign(VertexProgram.Code);

     FragmentProgram.LoadFromFile('simple_fp.cg');
     MemoFragCode.Lines.Assign(FragmentProgram.Code);

     VertexProgram.Enabled:=false;
     FragmentProgram.Enabled:=false;
   end;

   ButtonApplyFP.Enabled:=false;
   ButtonApplyVP.Enabled:=false;

   // Bind shader to the material
   GLMaterialLibrary1.Materials[0].Shader := CgShader1;

   // Load the teapot model.
   SetCurrentDir(Path  + '\model');
   // Note that GLScene will alter the ModelView matrix
   // internally objects like TGLCylinder & TGLSphere, and Cg shader
   // is not aware of that. If you apply a vertex shader on those objects, they
   // would appear scaled and/or rotated.
  GLFreeForm1.LoadFromFile('Teapot.3ds');
end;


procedure TFormCgSimple.CgShader1ApplyVP(CgProgram: TCgProgram; Sender: TObject);
var
  v : TGLVector;
  Param: TCgParameter;
begin
  // rotate light vector for the "simple lighting" vertex program
  v := ZHmgVector;
  RotateVector(v, YVector, GLCadencer1.CurrentTime);

  Param := CgProgram.ParamByName('LightVec');
  Param.AsVector:=v;
  // or using plain Cg API: cgGLSetParameter4fv(Param.Handle, @v);

  // set uniform parameters that change every frame
  with CgProgram.ParamByName('ModelViewProj') do
    SetAsStateMatrix( CG_GL_MODELVIEW_PROJECTION_MATRIX, CG_GL_MATRIX_IDENTITY);

  with CgProgram.ParamByName('ModelViewIT') do
    SetAsStateMatrix( CG_GL_MODELVIEW_MATRIX, CG_GL_MATRIX_INVERSE_TRANSPOSE);
  //  Or, using plain Cg API:
  //  Param := CgProgram.ParamByName('ModelViewIT');
  //  cgGLSetStateMatrixParameter(Param.Handle, CG_GL_MODELVIEW_MATRIX, CG_GL_MATRIX_INVERSE_TRANSPOSE);
end;


procedure TFormCgSimple.CgShader1Initialize(CgShader: TCustomCgShader);
begin
  // Shows the profiles to be used. The latest support profiles would be detected
  // if you have CgShader1.VertexProgram.Profile set to vpDetectLatest (similarly
  // for the fragment program).
  LabelVertProfile.Caption:='Using profile: ' + CgShader1.VertexProgram.GetProfileStringA;
  LabelFragProfile.Caption:='Using profile: ' + CgShader1.FragmentProgram.GetProfileStringA;
end;

procedure TFormCgSimple.CBVertexProgramClick(Sender: TObject);
begin
   CgShader1.VertexProgram.Enabled:=(Sender as TCheckBox).checked;
end;

procedure TFormCgSimple.CBFragmentProgramClick(Sender: TObject);
begin
   CgShader1.FragmentProgram.Enabled:=(Sender as TCheckBox).checked;
end;

procedure TFormCgSimple.ButtonApplyFPClick(Sender: TObject);
begin
  CgShader1.FragmentProgram.Code:=MemoFragCode.Lines;
  (Sender as TButton).Enabled:=false;
end;

procedure TFormCgSimple.ButtonApplyVPClick(Sender: TObject);
begin
  CgShader1.VertexProgram.Code:=MemoVertCode.Lines;
  (Sender as TButton).Enabled:=false;
end;

procedure TFormCgSimple.MemoFragCodeChange(Sender: TObject);
begin
  ButtonApplyFP.Enabled:=true;
end;

procedure TFormCgSimple.MemoVertCodeChange(Sender: TObject);
begin
  ButtonApplyVP.Enabled:=true;
end;

procedure TFormCgSimple.Button1Click(Sender: TObject);
begin
  CgShader1.VertexProgram.ListParameters(Memo1.Lines);
end;

procedure TFormCgSimple.Button2Click(Sender: TObject);
begin
  CgShader1.FragmentProgram.ListParameters(Memo3.Lines);
end;

procedure TFormCgSimple.Button3Click(Sender: TObject);
begin
  CgShader1.FragmentProgram.ListCompilation(Memo3.Lines);
end;

procedure TFormCgSimple.Button4Click(Sender: TObject);
begin
  CgShader1.VertexProgram.ListCompilation(Memo1.Lines);
end;

procedure TFormCgSimple.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=X;
   my:=Y;
end;

procedure TFormCgSimple.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      mx:=x;
      my:=y;
   end;
end;

procedure TFormCgSimple.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   GLSceneViewer1.Invalidate;
end;

procedure TFormCgSimple.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  with GLSceneViewer1 do
    if PtInRect(ClientRect, ScreenToClient(MousePos)) then begin
      GLCamera1.SceneScale:=GLCamera1.SceneScale * (1000 - WheelDelta) / 1000;
      Handled:=true;
    end;
end;

procedure TFormCgSimple.Timer1Timer(Sender: TObject);
begin
  with GLSceneViewer1 do begin
    PanelFPS.Caption:=Format('%.1f fps', [FramesPerSecond]);
    ResetPerformanceMonitor;
  end;
end;

procedure TFormCgSimple.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#27 then Close();
end;

end.
