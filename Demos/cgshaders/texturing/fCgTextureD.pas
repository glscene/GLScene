unit fCgTextureD;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Imaging.Jpeg,
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
  GLS.Graph,
 
  GLS.Material,
  GLS.Coordinates,
  GLS.Utils,
  GLS.BaseClasses;

type
  TFormCgTexture = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
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
    Panel9: TPanel;
    PanelFPS: TPanel;
    GLSceneViewer1: TGLSceneViewer;
    Timer1: TTimer;
    GLXYZGrid1: TGLXYZGrid;
    GLPlane1: TGLPlane;
    GLMatLib: TGLMaterialLibrary;
    TabSheet3: TTabSheet;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    TrackBar1: TTrackBar;
    Label4: TLabel;
    TrackBar2: TTrackBar;
    Label5: TLabel;
    TrackBar3: TTrackBar;
    Label6: TLabel;
    TrackBar4: TTrackBar;
    GroupBox2: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    TrackBar5: TTrackBar;
    TrackBar6: TTrackBar;
    TrackBar7: TTrackBar;
    TrackBar8: TTrackBar;
    Label11: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Label13: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    CheckBox2: TCheckBox;
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
    procedure CgShader1ApplyVP(CgProgram: TCgProgram; Sender: TObject);
    procedure CgShader1ApplyFP(CgProgram: TCgProgram; Sender: TObject);
    procedure CgShader1UnApplyFP(CgProgram: TCgProgram);
    procedure CgShader1Initialize(CgShader: TCustomCgShader);
    procedure CheckBox2Click(Sender: TObject);
  public
    mx, my: Integer;
  end;

var
  FormCgTexture: TFormCgTexture;

//------------------------------------
implementation
//------------------------------------

{$R *.dfm}

procedure TFormCgTexture.FormCreate(Sender: TObject);
begin
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path  + '\shader');
  // load Cg proggy from project dir
  with CgShader1 do
  begin
    VertexProgram.LoadFromFile('cg_texture_vp.cg');
    MemoVertCode.Lines.Assign(VertexProgram.Code);

    FragmentProgram.LoadFromFile('cg_texture_fp.cg');
    MemoFragCode.Lines.Assign(FragmentProgram.Code);
  end;

  // Load images from texture dir
  SetCurrentDir(Path  + '\texture');
  with GLMatLib do
  begin
    Materials[0].Material.Texture.Image.LoadFromFile('moon.bmp');
    Materials[1].Material.Texture.Image.LoadFromFile('clover.jpg');
    Materials[2].Material.Texture.Image.LoadFromFile('marbletiles.jpg');
    Materials[3].Material.Texture.Image.LoadFromFile('earth.png');
  end;
end;

procedure TFormCgTexture.CgShader1Initialize(CgShader: TCustomCgShader);
begin
  // Due to parameter shadowing (ref. Cg Manual), parameters that doesn't change
  // once set can be assigned for once in the OnInitialize event.
  with CgShader1.FragmentProgram, GLMatLib do
  begin
    ParamByName('Map0').SetToTextureOf(Materials[0]);
    ParamByName('Map1').SetToTextureOf(Materials[1]);
    ParamByName('Map2').SetToTextureOf(Materials[2]);
    ParamByName('Map3').SetToTextureOf(Materials[3]);
    // Alternatively, you can set texture parameters using two other methods:
    // SetTexture('Map0', Materials[0].Material.Texture.Handle);
    // ParamByName('Map0').SetAsTexture2D(Materials[0].Material.Texture.Handle);
  end;

  // Display profiles used
  LabelVertProfile.Caption := 'Using profile: ' +
    CgShader1.VertexProgram.GetProfileStringA;
  LabelFragProfile.Caption := 'Using profile: ' +
    CgShader1.FragmentProgram.GetProfileStringA;
end;

procedure TFormCgTexture.CgShader1ApplyVP(CgProgram: TCgProgram; Sender: TObject);

var
  v: TGLVector;

  function conv(TrackBar: TTrackBar): single;
  var
    half: Integer;
  begin
    half := TrackBar.Max div 2;
    result := (TrackBar.Position - half) / half;
  end;

begin
  with CgProgram.ParamByName('ModelViewProj') do
    SetAsStateMatrix(CG_GL_MODELVIEW_PROJECTION_MATRIX, CG_GL_MATRIX_IDENTITY);
  // Alternatively, you can set it using:
  // CgProgram.SetStateMatrix('ModelViewProj', CG_GL_MODELVIEW_PROJECTION_MATRIX, CG_GL_MATRIX_IDENTITY);

  v := vectormake(conv(TrackBar1), conv(TrackBar2), conv(TrackBar3),
    conv(TrackBar4));

  CgProgram.ParamByName('shifts').SetAsVector(v);
end;

procedure TFormCgTexture.CgShader1ApplyFP(CgProgram: TCgProgram; Sender: TObject);
var
  v: TGLVector;

  function conv(TrackBar: TTrackBar): single;
  var
    half: Integer;
  begin
    half := TrackBar.Max div 2;
    result := (TrackBar.Position - half) / half;
  end;

begin
  with CgProgram do
  begin
    ParamByName('Map0').EnableTexture;
    ParamByName('Map1').EnableTexture;
    ParamByName('Map2').EnableTexture;
    ParamByName('Map3').EnableTexture;
  end;

  v := vectormake(conv(TrackBar5), conv(TrackBar6), conv(TrackBar7),
    conv(TrackBar8));

  CgProgram.ParamByName('weights').SetAsVector(v);
end;

procedure TFormCgTexture.CgShader1UnApplyFP(CgProgram: TCgProgram);
begin
  with CgProgram do
  begin
    ParamByName('Map0').DisableTexture;
    ParamByName('Map1').DisableTexture;
    ParamByName('Map2').DisableTexture;
    ParamByName('Map3').DisableTexture;
  end;
end;

// Code below takes care of the UI

procedure TFormCgTexture.CBVertexProgramClick(Sender: TObject);
begin
  CgShader1.VertexProgram.Enabled := (Sender as TCheckBox).checked;
end;

procedure TFormCgTexture.CBFragmentProgramClick(Sender: TObject);
begin
  CgShader1.FragmentProgram.Enabled := (Sender as TCheckBox).checked;
end;

procedure TFormCgTexture.ButtonApplyFPClick(Sender: TObject);
begin
  CgShader1.FragmentProgram.Code := MemoFragCode.Lines;
  (Sender as TButton).Enabled := false;
end;

procedure TFormCgTexture.ButtonApplyVPClick(Sender: TObject);
begin
  CgShader1.VertexProgram.Code := MemoVertCode.Lines;
  (Sender as TButton).Enabled := false;
end;

procedure TFormCgTexture.MemoFragCodeChange(Sender: TObject);
begin
  ButtonApplyFP.Enabled := true;
end;

procedure TFormCgTexture.MemoVertCodeChange(Sender: TObject);
begin
  ButtonApplyVP.Enabled := true;
end;

procedure TFormCgTexture.Button1Click(Sender: TObject);
begin
  CgShader1.VertexProgram.ListParameters(Memo1.Lines);
end;

procedure TFormCgTexture.Button2Click(Sender: TObject);
begin
  CgShader1.FragmentProgram.ListParameters(Memo3.Lines);
end;

procedure TFormCgTexture.Button3Click(Sender: TObject);
begin
  CgShader1.FragmentProgram.ListCompilation(Memo3.Lines);
end;

procedure TFormCgTexture.Button4Click(Sender: TObject);
begin
  CgShader1.VertexProgram.ListCompilation(Memo1.Lines);
end;

procedure TFormCgTexture.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormCgTexture.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift <> [] then
  begin
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
    mx := X;
    my := Y;
  end;
end;

procedure TFormCgTexture.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TFormCgTexture.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  with GLSceneViewer1 do
    if PtInRect(ClientRect, ScreenToClient(MousePos)) then
    begin
      GLCamera1.SceneScale := GLCamera1.SceneScale * (1000 - WheelDelta) / 1000;
      Handled := true;
    end;
end;

procedure TFormCgTexture.Timer1Timer(Sender: TObject);
begin
  with GLSceneViewer1 do
  begin
    PanelFPS.Caption := Format('%.1f fps', [FramesPerSecond]);
    ResetPerformanceMonitor;
  end;
end;

procedure TFormCgTexture.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    close;
end;

procedure TFormCgTexture.CheckBox2Click(Sender: TObject);
begin
  CgShader1.Enabled := CheckBox2.checked;
end;

end.
