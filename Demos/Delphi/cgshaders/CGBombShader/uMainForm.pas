unit uMainForm;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,

  
  GLScene,
  GLObjects,
  GLWin32Viewer,
  GLTexture,
  GLCadencer,
  GLSimpleNavigation,
  GLVectorFileObjects,
  GLFile3DS,
  GLGraph,
  GLCgBombShader,
  GLMaterial,
  GLVectorGeometry,

  // Formats
  GLFileMD2,
  GLCrossPlatform,
  GLCoordinates,
  GLBaseClasses,
  GLUtils;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel9: TPanel;
    GLSceneViewer1: TGLSceneViewer;
    Timer1: TTimer;
    GLXYZGrid1: TGLXYZGrid;
    GLDummyCube1: TGLDummyCube;
    ffTeapot: TGLFreeForm;
    ffSphere1: TGLFreeForm;
    ComboBox1: TComboBox;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ffSphere2: TGLFreeForm;
    CheckBox4: TCheckBox;
    JustATestCube: TGLCube;
    ShaderEnabledCheckBox: TCheckBox;
    GLSimpleNavigation1: TGLSimpleNavigation;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    TrackBar5: TTrackBar;
    TrackBar6: TTrackBar;
    TrackBar7: TTrackBar;
    TrackBar8: TTrackBar;
    TrackBar9: TTrackBar;
    GLActor1: TGLActor;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const DeltaTime, newTime: Double);
    procedure CheckBox1Click(Sender: TObject);
    procedure ShaderEnabledCheckBoxClick(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure TrackBar5Change(Sender: TObject);
    procedure TrackBar6Change(Sender: TObject);
    procedure TrackBar7Change(Sender: TObject);
    procedure TrackBar8Change(Sender: TObject);
    procedure TrackBar9Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
     
  public
     
    mx, my: Integer;
    procedure ResetPositions;
  end;

var
  Form1:    TForm1;
  MyShader: TCgBombShader;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // First load models from media directory
  SetGLSceneMediaDir();
  GLActor1.LoadFromFile('waste.md2'); //Fighter
  GLActor1.SwitchToAnimation(0, True);
  GLActor1.AnimationMode := aamLoop;
  GLActor1.Scale.Scale(0.05);

  ffTeapot.LoadFromFile('Teapot.3ds');
  ffSphere1.LoadFromFile('Sphere_little.3DS');
  ffSphere2.LoadFromFile('Sphere_big.3DS');
  ffSphere2.Scale.Scale(20);

  GLMaterialLibrary1.LibMaterialByName('marbles1').Material.Texture.Image.LoadFromFile('beigemarble.jpg');
  GLMaterialLibrary1.LibMaterialByName('marbles2').Material.Texture.Image.LoadFromFile('marbletiles.jpg');
  GLMaterialLibrary1.LibMaterialByName('snow').Material.Texture.Image.LoadFromFile('snow512.jpg');
  GLMaterialLibrary1.LibMaterialByName('Fire').Material.Texture.Image.LoadFromFile('FireGrade.bmp');
  GLMaterialLibrary1.LibMaterialByName('FighterTexture').Material.Texture.Image.LoadFromFile('waste.jpg');

  Myshader := TCgBombShader.Create(Self);
  Myshader.MainTexture := GLMaterialLibrary1.LibMaterialByName('FighterTexture').Material.Texture;
  MyShader.Cadencer := GLCadencer1;

  // All models are linked with this material
  GLMaterialLibrary1.LibMaterialByName('LibMaterial').Shader := Myshader;

  ResetPositions;
  ComboBox1Change(Self);
end;


procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  Myshader.GradientTexture := GLMaterialLibrary1.LibMaterialByName(ComboBox1.Text).Material.Texture;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const DeltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate();
end;

procedure TForm1.ResetPositions;
begin
  TrackBar1.Position := Round(InterpolateCombinedSafe(0.01, 10, Myshader.Displacement, 0, 100, 1, itLinear));
  TrackBar2.Position := Round(InterpolateCombinedSafe(-0.13, 10, Myshader.Sharpness, 0, 100, 1, itLinear));
  TrackBar3.Position := Round(InterpolateCombinedSafe(0.01, 6, Myshader.ColorSharpness, 0, 100, 1, itLinear));
  TrackBar4.Position := Round(InterpolateCombinedSafe(0.1, 1, Myshader.Speed, 0, 100, 1, itLinear));
  TrackBar5.Position := Round(InterpolateCombinedSafe(0.01, 8, Myshader.TurbDensity, 0, 100, 1, itLinear));
  TrackBar6.Position := Round(InterpolateCombinedSafe(-0.5, 0.5, Myshader.ColorRange, 0, 100, 1, itLinear));
  TrackBar7.Position := Round(InterpolateCombinedSafe(0, 1, Myshader.Alpha, 0, 100, 1, itLinear));
  TrackBar8.Position := Round(InterpolateCombinedSafe(0, 2, Myshader.MainTextureShare, 0, 100, 1, itLinear));
  TrackBar9.Position := Round(InterpolateCombinedSafe(0, 2, Myshader.GradientTextureShare, 0, 100, 1, itLinear));
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  GLActor1.Visible := CheckBox1.Checked;
  ffTeapot.Visible := CheckBox2.Checked;
  ffSphere1.Visible := CheckBox3.Checked;
  ffSphere2.Visible := CheckBox4.Checked;
end;

procedure TForm1.ShaderEnabledCheckBoxClick(Sender: TObject);
begin
  MyShader.Enabled := ShaderEnabledCheckBox.Checked;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  if Myshader = nil then
    Exit;
  Myshader.Displacement := InterpolateCombinedSafe(0, 100, TrackBar1.Position, 0.01, 10, 1, itLinear);
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  Myshader.Sharpness := InterpolateCombinedSafe(0, 100, TrackBar2.Position, -0.13, 10, 1, itLinear);
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
begin
  Myshader.ColorSharpness := InterpolateCombinedSafe(0, 100, TrackBar3.Position, 0.01, 6, 1, itLinear);
end;

procedure TForm1.TrackBar4Change(Sender: TObject);
begin
  Myshader.Speed := InterpolateCombinedSafe(0, 100, TrackBar4.Position, 0.1, 1, 1, itLinear);
end;

procedure TForm1.TrackBar5Change(Sender: TObject);
begin
  Myshader.TurbDensity := InterpolateCombinedSafe(0, 100, TrackBar5.Position, 0.1, 8, 1, itLinear);
end;

procedure TForm1.TrackBar6Change(Sender: TObject);
begin
  Myshader.ColorRange := InterpolateCombinedSafe(0, 100, TrackBar6.Position, -0.5, 0.5, 1, itLinear);
end;

procedure TForm1.TrackBar7Change(Sender: TObject);
begin
  Myshader.Alpha := InterpolateCombinedSafe(0, 100, TrackBar7.Position, 0, 1, 1, itLinear);
end;

procedure TForm1.TrackBar8Change(Sender: TObject);
begin
  Myshader.MainTextureShare := InterpolateCombinedSafe(0, 100, TrackBar8.Position, 0, 2, 1, itLinear);
end;

procedure TForm1.TrackBar9Change(Sender: TObject);
begin
  Myshader.GradientTextureShare := InterpolateCombinedSafe(0, 100, TrackBar9.Position, 0, 2, 1, itLinear);
end;

end.
