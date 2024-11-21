unit fBunnyBumpD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Imaging.Jpeg,
  
  GLS.Scene,
  Stage.VectorTypes,
  GLS.Objects,
  GLS.Texture,
  GLS.VectorFileObjects,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.AsyncTimer,
 
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  Stage.VectorGeometry,
  GLS.Context,
  GLS.FileOBJ,
  Stage.Utils,
  GLSL.BumpShaders;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCadencer1: TGLCadencer;
    MatLib: TGLMaterialLibrary;
    Camera: TGLCamera;
    WhiteLight: TGLLightSource;
    RedLight: TGLLightSource;
    BlueLight: TGLLightSource;
    GLBumpShader1: TGLBumpShader;
    Panel1: TPanel;
    ComboBox1: TComboBox;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    cbWhite: TCheckBox;
    cbRed: TCheckBox;
    cbBlue: TCheckBox;
    ShapeWhite: TShape;
    ShapeRed: TShape;
    ShapeBlue: TShape;
    ColorDialog1: TColorDialog;
    DCLights: TGLDummyCube;
    AsyncTimer1: TGLAsyncTimer;
    CheckBox4: TCheckBox;
    ComboBox2: TComboBox;
    Label2: TLabel;
    LabelFPS: TLabel;
    ffBunny: TGLFreeForm;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure ShapeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckBoxClick(Sender: TObject);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure GLSceneViewer1BeforeRender(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
  private
     
  public
     
    mx, my, dx, dy: Integer;
    IsInitialized: Boolean;
    StartHeight: Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path  + '\modelext');
  // Load the bunny mesh and scale for viewing
  ffBunny.LoadFromFile('bunny.glsm');
//  ffBunny.LoadFromFile('bunny.obj');
  ffBunny.Scale.Scale(2 / ffBunny.BoundingSphereRadius);
  ffBunny.RollAngle := 90;
  ffBunny.TurnAngle := 90;


  // Load the normal map
  SetCurrentDir(Path  + '\skin');
  MatLib.Materials[0].Material.Texture.Image.LoadFromFile('bunnynormals.jpg');

  // Link the lights to their toggles
  cbWhite.Tag := Integer(WhiteLight);
  cbRed.Tag := Integer(RedLight);
  cbBlue.Tag := Integer(BlueLight);
  ShapeWhite.Tag := Integer(WhiteLight);
  ShapeRed.Tag := Integer(RedLight);
  ShapeBlue.Tag := Integer(BlueLight);

  ComboBox1.ItemIndex := 0;
  ComboBox1Change(Self);

  StartHeight := Height;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  // Orbit the camera
  if (dx <> 0) or (dy <> 0) then
  begin
    Camera.MoveAroundTarget(dy, dx);
    dx := 0;
    dy := 0;
  end;

  // Rotate the light sources
  if CheckBox4.Checked then
    DCLights.Turn(deltaTime * 20);

  GLSceneViewer1.Invalidate;
end;

procedure TForm1.CheckBoxClick(Sender: TObject);
begin
  // Light Shining CheckBox
  TGLLightSource(TCheckBox(Sender).Tag).Shining := TCheckBox(Sender).Checked;
end;

procedure TForm1.ShapeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Light Color Dialog
  ColorDialog1.Color := TShape(Sender).Brush.Color;
  if ColorDialog1.Execute then
  begin
    TShape(Sender).Brush.Color := ColorDialog1.Color;
    with TGLLightSource(TShape(Sender).Tag) do
      Diffuse.AsWinColor := ColorDialog1.Color;
  end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  if ComboBox1.Text = 'Per-Vertex' then
    ffBunny.Material.LibMaterialName := ''
  else if ComboBox1.Text = 'Dot3 Texture Combiner' then
  begin
    ffBunny.Material.LibMaterialName := 'Bump';
    GLBumpShader1.BumpMethod := bmDot3TexCombiner;
  end
  else if ComboBox1.Text = 'Basic Fragment Program' then
  begin
    ffBunny.Material.LibMaterialName := 'Bump';
    GLBumpShader1.BumpMethod := bmBasicARBFP;
  end;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
  dx := 0;
  dy := 0;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    dx := dx + (mx - X);
    dy := dy + (my - Y);
  end
  else
  begin
    dx := 0;
    dy := 0;
  end;
  mx := X;
  my := Y;
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  LabelFPS.Caption := GLSceneViewer1.FramesPerSecondText;
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Camera.SceneScale := Height / StartHeight;
end;

procedure TForm1.GLSceneViewer1BeforeRender(Sender: TObject);
begin
  if IsInitialized then
    exit;

  if GL.ARB_multitexture and GL.ARB_vertex_program and GL.ARB_texture_env_dot3
  then
    ComboBox1.Items.Add('Dot3 Texture Combiner');
  if GL.ARB_multitexture and GL.ARB_vertex_program and GL.ARB_fragment_program
  then
  begin
    ComboBox1.Items.Add('Basic Fragment Program');
    if GLSceneViewer1.Buffer.LimitOf[limNbTextureUnits] < 3 then
      GLBumpShader1.SpecularMode := smOff;
  end;

  IsInitialized := True;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  case ComboBox2.ItemIndex of
    0:
      GLBumpShader1.SpecularMode := smOff;
    1:
      GLBumpShader1.SpecularMode := smBlinn;
    2:
      GLBumpShader1.SpecularMode := smPhong;
  end;
end;

end.
