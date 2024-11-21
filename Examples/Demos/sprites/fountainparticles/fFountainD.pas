unit fFountainD;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Imaging.Jpeg,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ExtDlgs,
  Vcl.ComCtrls,

  Stage.VectorTypes,
  GLS.Texture,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.Scene,
  GLS.Objects,
  Stage.VectorGeometry,
  GLS.GeomObjects,
  GLS.Coordinates,
  GLS.BaseClasses,


  uFountainD, GLS.Material;

type
  TForm1 = class( TForm )
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    Cam: TGLCamera;
    Scene: TGLDummyCube;
    Light: TGLLightSource;
    Timer1: TTimer;
    Panel1: TPanel;
    ColorDialog1: TColorDialog;
    Panel2: TPanel;
    Label1: TLabel;
    PStartColor: TPanel;
    Label2: TLabel;
    PEndColor: TPanel;
    PBackColor: TPanel;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    EdPSizeMax: TEdit;
    Label14: TLabel;
    EdPSizeMin: TEdit;
    Label13: TLabel;
    EdTimesFact: TEdit;
    Label12: TLabel;
    EdLifeFact: TEdit;
    Label11: TLabel;
    EdAngleStart: TEdit;
    Label10: TLabel;
    EdVelMax: TEdit;
    Label9: TLabel;
    EdVelMin: TEdit;
    Label8: TLabel;
    EdMaxP: TEdit;
    Label7: TLabel;
    EdFloor: TEdit;
    Label6: TLabel;
    EdBound: TEdit;
    EdMass: TEdit;
    Label5: TLabel;
    Label4: TLabel;
    CheckActived: TCheckBox;
    CheckBound: TCheckBox;
    Close1: TMenuItem;
    TabSheet2: TTabSheet;
    Panel3: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    GLPlane1: TGLPlane;
    GLTorus1: TGLTorus;
    TrackBar1: TTrackBar;
    Label15: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    Texture1: TMenuItem;
    StatusBar1: TStatusBar;
    GLMatlibColors: TGLMaterialLibrary;
    procedure FormCreate( Sender: TObject );
    procedure GLCadencer1Progress( Sender: TObject; const deltaTime, newTime: Double );
    procedure GLSceneViewer1MouseDown( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
    procedure GLSceneViewer1MouseMove( Sender: TObject; Shift: TShiftState; X, Y: Integer );
    procedure Timer1Timer( Sender: TObject );
    procedure FormMouseWheel( Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean );
    procedure PStartColorClick(Sender: TObject);
    procedure PEndColorClick(Sender: TObject);
    procedure CheckActivedClick(Sender: TObject);
    procedure PBackColorClick(Sender: TObject);
    procedure CheckBoundClick(Sender: TObject);
    procedure EdMassChange(Sender: TObject);
    procedure EdBoundChange(Sender: TObject);
    procedure EdFloorChange(Sender: TObject);
    procedure EdMaxPChange(Sender: TObject);
    procedure EdVelMinChange(Sender: TObject);
    procedure EdVelMaxChange(Sender: TObject);
    procedure EdAngleStartChange(Sender: TObject);
    procedure EdLifeFactChange(Sender: TObject);
    procedure EdTimesFactChange(Sender: TObject);
    procedure EdPSizeMinChange(Sender: TObject);
    procedure EdPSizeMaxChange(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure RadioButtonClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure Texture1Click(Sender: TObject);
  private
    my, mx : integer;
    path : string;
  public
     
  end;

var
  GLFountainDummy : TGLFountainDummy;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate( Sender: TObject );
begin
  SetCurrentDir( ExtractFilePath( ParamStr(0) ) );
  path := ExtractFilePath( ParamStr(0) );


  OpenPictureDialog1.InitialDir := path + 'Textures\';

//  GLPlane1.Material.Texture.Disabled := False;

  GLFountainDummy := TGLFountainDummy(Scene.AddNewChild(TGLFountainDummy));
  with GLFountainDummy do
  begin
    Material.Texture.Image.LoadFromFile( path + 'Textures\Par1.bmp' );
    Scale.X := 0.15; Scale.Y := 0.15; Scale.Z := 0.15;
    Direction.X := 0; Direction.Y := 1; Direction.Z := 0;
    Up.X := 0; Up.Y := 0; Up.Z := 1;
    Position.Z := -3;
    Actived := True;
    Bounding := False;
    ParticleMass := 5.0;
    BoundingFactor := 100.0;
    Floor := 0.0;
    MaxParticles := 60;
    VelocityMin := 5;
    VelocityMax := 15;
    AngleInit := 360;
    LifeFactor := 0.025;
    TimesFactor := 0.00005;
    ParticlesSizeMin := 110;
    ParticlesSizeMax := 130;
    ColorStart := PStartColor.Color;
    ColorEnd := PEndColor.Color;
  end;
  TrackBar1.OnChange( self );

   PStartColor.Color := clRed;
end;

procedure TForm1.GLCadencer1Progress( Sender: TObject; const deltaTime, newTime: Double );
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.GLSceneViewer1MouseDown( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  my := y;
  mx := x;
end;

procedure TForm1.GLSceneViewer1MouseMove( Sender: TObject; Shift: TShiftState; X, Y: Integer );
begin
  if ssright in shift then
    Cam.MoveAroundTarget( my-y, mx-x );
  my := y;
  mx := x;
end;

procedure TForm1.Timer1Timer( Sender: TObject );
begin
  Caption:=Format('%.2f FPS Fountain Particles', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.FormMouseWheel( Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean );
begin
  Cam.AdjustDistanceToTarget( Power( 1.1, WheelDelta / 120 ) );
end;

procedure TForm1.PStartColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    GLFountainDummy.ColorStart := ColorDialog1.Color;
    PStartColor.Color := ColorDialog1.Color;
  end;
end;

procedure TForm1.PEndColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    PEndColor.Color := ColorDialog1.Color;
    GLFountainDummy.ColorEnd := ColorDialog1.Color;
  end;
end;

procedure TForm1.PBackColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    PBackColor.Color := ColorDialog1.Color;
    GLSceneViewer1.Buffer.BackgroundColor := ColorDialog1.Color;
  end;
end;

//------------------------------------------------
// Activate
//------------------------------------------------
procedure TForm1.CheckActivedClick(Sender: TObject);
begin
  if CheckActived.Checked then
    GLFountainDummy.Actived := True else
    GLFountainDummy.Actived := False;
end;

procedure TForm1.CheckBoundClick(Sender: TObject);
begin
  if CheckBound.Checked then
    GLFountainDummy.Bounding := True else
    GLFountainDummy.Bounding := False;
end;

procedure TForm1.EdMassChange(Sender: TObject);
begin
  GLFountainDummy.ParticleMass := StrToFloat( EdMass.Text );
end;

procedure TForm1.EdBoundChange(Sender: TObject);
begin
  GLFountainDummy.BoundingFactor := StrToFloat( EdBound.Text );
end;

procedure TForm1.EdFloorChange(Sender: TObject);
begin
  GLFountainDummy.Floor := StrToFloat( EdFloor.Text );
end;

procedure TForm1.EdMaxPChange(Sender: TObject);
begin
  GLFountainDummy.MaxParticles := StrToInt( EdMaxP.Text );
end;

procedure TForm1.EdVelMinChange(Sender: TObject);
begin
  GLFountainDummy.VelocityMin := StrToInt( EdVelMin.Text );
end;

procedure TForm1.EdVelMaxChange(Sender: TObject);
begin
  GLFountainDummy.VelocityMax := StrToInt( EdVelMax.Text );
end;

procedure TForm1.EdAngleStartChange(Sender: TObject);
begin
  GLFountainDummy.AngleInit := StrToInt( EdAngleStart.Text );
end;

procedure TForm1.EdLifeFactChange(Sender: TObject);
begin
  GLFountainDummy.LifeFactor := StrToFloat( EdLifeFact.Text );
end;

procedure TForm1.EdTimesFactChange(Sender: TObject);
begin
  GLFountainDummy.TimesFactor := StrToFloat( EdTimesFact.Text );
end;

procedure TForm1.EdPSizeMinChange(Sender: TObject);
begin
  GLFountainDummy.ParticlesSizeMin := StrToInt( EdPSizeMin.Text );
end;

procedure TForm1.EdPSizeMaxChange(Sender: TObject);
begin
  GLFountainDummy.ParticlesSizeMax := StrToInt( EdPSizeMax.Text );
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  with GLFountainDummy do
  begin
    Scale.X := TrackBar1.Position / 10;
    Scale.Y := TrackBar1.Position / 10;
    Scale.Z := TrackBar1.Position / 10;
  end;
end;

procedure TForm1.Texture1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    if ( OpenPictureDialog1.FileName <> '' ) then
    begin
      GLFountainDummy.Material.Texture.Image.LoadFromFile( OpenPictureDialog1.FileName );
    end;
  end;
  SetCurrentDir( ExtractFilePath( ParamStr(0) ) );
end;

//---------------------------------------------------
// Particle Styles
//---------------------------------------------------
procedure TForm1.RadioButtonClick(Sender: TObject);
var
  val: integer;
begin
  val := 0;
  if RadioButton1.Checked then
    val := 0 else
  if RadioButton2.Checked then
    val := 1 else
  if RadioButton3.Checked then
    val := 2;
  case val of
    0: begin
         with GLFountainDummy do
         begin
           Material.Texture.Image.LoadFromFile( path + 'Textures\Par1.bmp' );
           Position.Z := -3;
           Actived := True;
           Bounding := False;
           ParticleMass := 5.0;
           BoundingFactor := 100.0;
           Floor := 0.0;
           MaxParticles := 60;
           VelocityMin := 5;
           VelocityMax := 15;
           AngleInit := 360;
           LifeFactor := 0.025;
           TimesFactor := 0.00005;
           ParticlesSizeMin := 110;
           ParticlesSizeMax := 130;
           PStartColor.Color := $0000FF;
           ColorStart := PStartColor.Color;
           PEndColor.Color := $00FFFF;
           ColorEnd := PEndColor.Color;
         end;
       end;
    1: begin
         with GLFountainDummy do
         begin
           Material.Texture.Image.LoadFromFile( path + 'Textures\Par2.bmp' );
           Position.Z := -3;
           Actived := True;
           Bounding := False;
           ParticleMass := 1.0;
           BoundingFactor := 100.0;
           Floor := 0.0;
           MaxParticles := 500;
           VelocityMin := 20;
           VelocityMax := 20;
           AngleInit := 360;
           LifeFactor := 0.003;
           TimesFactor := 0.009;
           ParticlesSizeMin := 30;
           ParticlesSizeMax := 40;
           PStartColor.Color := $FFFFFF;
           ColorStart := PStartColor.Color;
           PEndColor.Color := clFuchsia;
           ColorEnd := PEndColor.Color;
         end;
       end;
    2: begin
         with GLFountainDummy do
         begin
           Material.Texture.Image.LoadFromFile( path + 'Textures\Par3.bmp' );
           Position.Z := -3;
           Actived := True;
           Bounding := True;
           ParticleMass := 10.0;
           BoundingFactor := 75.0;
           Floor := 0.0;
           MaxParticles := 500;
           VelocityMin := 19;
           VelocityMax := 20;
           AngleInit := 360;
           LifeFactor := 0.005;
           TimesFactor := 0.005;
           ParticlesSizeMin := 10;
           ParticlesSizeMax := 20;
           PStartColor.Color := $00FF00;
           ColorStart := PStartColor.Color;
           PEndColor.Color := $00FF00;
           ColorEnd := PEndColor.Color;
         end;
       end;
     end;
end;

procedure TForm1.Close1Click(Sender: TObject);
begin
  Close;
end;

end.
