unit fReflectD;

interface

uses
  Winapi.OpenGL,
  Winapi.Windows, 
  Winapi.Messages,
  System.SysUtils, 
  System.Variants, 
  System.Classes,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms,
  Vcl.Dialogs, 
  Vcl.ExtCtrls, 
  Vcl.StdCtrls, 
  Vcl.ComCtrls, 
  Vcl.Buttons,
  Vcl.Imaging.Jpeg,
   
  Cg.Import,
  Cg.GL,
  GLS.Scene,
  GLS.Objects,
  GLS.SceneViewer,
  GLS.Texture,
  GLS.CgShader,
  Stage.VectorGeometry,
  GLS.Cadencer,
  GLS.VectorFileObjects, 
  GLS.File3DS,
  GLS.Graph, 
  Stage.VectorTypes, 
  GLS.GeomObjects, 
  GLS.Material, 
  GLS.Coordinates,
  Stage.Utils,
   
  GLS.BaseClasses;

type
  TForm1 = class(TForm)
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
    CheckBox1: TCheckBox;
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
    Panel9: TPanel;
    Panel10: TPanel;
    GLSceneViewer1: TGLSceneViewer;
    Timer1: TTimer;
    plane: TGLFreeForm;
    GLTorus1: TGLTorus;
    GLMemoryViewer1: TGLMemoryViewer;
    GLCamera2: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    bottom: TGLPlane;
    top: TGLPlane;
    back: TGLPlane;
    left: TGLPlane;
    right: TGLPlane;
    front: TGLPlane;
    GLSphere1: TGLSphere;
    TabSheet3: TTabSheet;
    TrackBar1: TTrackBar;
    GLDummyCube2: TGLDummyCube;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
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
    procedure CgShader1Initialize(Sender: TCustomCgShader);
    procedure TrackBar1Change(Sender: TObject);
    procedure CgShader1UnApplyFragmentProgram(Sender: TCgProgram);
    procedure CgShader1ApplyFP(CgProgram: TCgProgram; Sender: TObject);
    procedure CgShader1ApplyVP(CgProgram: TCgProgram; Sender: TObject);
  private
    procedure CreateCubeMap;
  public
    mx, my: Integer;
  end;

var
  Form1: TForm1;
  ref: single;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path + '\model');

  // load a flat plane model for the water FreeForm
  plane.LoadFromFile('plane.3ds');

 // Load Cg shaders for proggy
  SetCurrentDir(Path + '\shader');
  with CgShader1 do
  begin
    VertexProgram.LoadFromFile('reflect_vp.cg');
    MemoVertCode.Lines.Assign(VertexProgram.Code);
    FragmentProgram.LoadFromFile('reflect_fp.cg');
    MemoFragCode.Lines.Assign(FragmentProgram.Code);

    VertexProgram.Enabled := false;
    FragmentProgram.Enabled := false;
  end;

  ButtonApplyFP.Enabled := false;
  ButtonApplyVP.Enabled := false;

  ref := 0;

  CreateCubeMap;
end;


procedure TForm1.CgShader1ApplyFP(CgProgram: TCgProgram; Sender: TObject);
begin
  with CgProgram, GLMaterialLibrary1 do
  begin
    ParamByName('reflectivity').SetAsScalar(ref); // float

    ParamByName('decalMap').SetAsTexture2D(materials[1].Material.Texture.Handle); // sampler2D
    ParamByName('decalMap').EnableTexture;

    ParamByName('environmentMap').SetAsTextureCUBE(materials[0].Material.Texture.Handle); // samplerCUBE
    ParamByName('environmentMap').EnableTexture;
  end;
end;

procedure TForm1.CgShader1ApplyVP(CgProgram: TCgProgram; Sender: TObject);
begin
  with CgProgram do
  begin
    ParamByName('ModelViewProj').SetAsStateMatrix
      (CG_GL_MODELVIEW_PROJECTION_MATRIX, CG_GL_MATRIX_IDENTITY);
    ParamByName('ModelView').SetAsStateMatrix(CG_GL_MODELVIEW_MATRIX,
      CG_GL_MATRIX_IDENTITY);
    ParamByName('ModelViewIT').SetAsStateMatrix(CG_GL_MODELVIEW_MATRIX,
      CG_GL_MATRIX_INVERSE_TRANSPOSE);
    // IT in ModelViewIT means Inversed and Transposed
  end;
end;

procedure TForm1.CgShader1UnApplyFragmentProgram(Sender: TCgProgram);
begin
  with Sender do
  begin
    ParamByName('decalMap').DisableTexture;
    ParamByName('environmentMap').DisableTexture;
  end;
end;

procedure TForm1.CreateCubeMap;
begin
  GLDummyCube2.Visible := false;
  with GLMaterialLibrary1.materials[0].Material.Texture do
  begin
    ImageClassName := TGLCubeMapImage.ClassName;
    with Image as TGLCubeMapImage do
    begin
      GLMemoryViewer1.RenderCubeMapTextures(GLMaterialLibrary1.materials[0]
        .Material.Texture);
    end;
    disabled := false;
  end;
  GLDummyCube2.Visible := true;
end;

procedure TForm1.CgShader1Initialize(Sender: TCustomCgShader);
begin
  // Shows the profiles to be used. The latest support profiles would be detected
  // if you have CgShader1.VertexProgram.Profile set to vpDetectLatest (similarly
  // for the fragment program).
  LabelVertProfile.Caption := 'Using profile: ' + 'Vertex Program';
  // Sender.VertexProgram.GetProfileString;
  LabelFragProfile.Caption := 'Using profile: ' + 'Fragment Program';
  // Sender.FragmentProgram.GetProfileString;
end;

procedure TForm1.CBVertexProgramClick(Sender: TObject);
begin
  CgShader1.VertexProgram.Enabled := (Sender as TCheckBox).checked;
end;

procedure TForm1.CBFragmentProgramClick(Sender: TObject);
begin
  CgShader1.FragmentProgram.Enabled := (Sender as TCheckBox).checked;
end;

procedure TForm1.ButtonApplyFPClick(Sender: TObject);
begin
  CgShader1.FragmentProgram.Code := MemoFragCode.Lines;
  (Sender as TButton).Enabled := false;
end;

procedure TForm1.ButtonApplyVPClick(Sender: TObject);
begin
  CgShader1.VertexProgram.Code := MemoVertCode.Lines;
  (Sender as TButton).Enabled := false;
end;

procedure TForm1.MemoFragCodeChange(Sender: TObject);
begin
  ButtonApplyFP.Enabled := true;
end;

procedure TForm1.MemoVertCodeChange(Sender: TObject);
begin
  ButtonApplyVP.Enabled := true;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  CgShader1.VertexProgram.ListParameters(Memo1.Lines);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  CgShader1.FragmentProgram.ListParameters(Memo3.Lines);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  CgShader1.FragmentProgram.ListCompilation(Memo3.Lines);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  CgShader1.VertexProgram.ListCompilation(Memo1.Lines);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift <> [] then
  begin
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
    mx := X;
    my := Y;
  end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  with GLSceneViewer1 do
    if PtInRect(ClientRect, ScreenToClient(MousePos)) then
    begin
      GLCamera1.SceneScale := GLCamera1.SceneScale * (1000 - WheelDelta) / 1000;
      Handled := true;
    end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Format('Cg Reflect - %.1f fps', [GLSceneViewer1.FramesPerSecond]);
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    close;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  with TrackBar1 do
    ref := position / max;
end;

end.
