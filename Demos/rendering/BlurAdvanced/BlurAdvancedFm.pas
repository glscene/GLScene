unit BlurAdvancedFm;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Imaging.Jpeg,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  GLS.SceneViewer,
  GLS.Scene,
  GLS.Objects,
  GLS.HUDObjects,
  GLS.GeomObjects,
  GLS.Cadencer,
  GLS.Blur,
  GLS.Texture,
 
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.Utils;

type
  TFormBlurAdvanced = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLCube1: TGLCube;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLAnnulus1: TGLAnnulus;
    GLBlur1: TGLBlur;
    Timer1: TTimer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Panel1: TPanel;
    edtAdvancedBlurAmp: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtAdvancedBlurPasses: TEdit;
    trkAdvancedBlurHiClamp: TTrackBar;
    Label3: TLabel;
    Label4: TLabel;
    trkAdvancedBlurLoClamp: TTrackBar;
    Label5: TLabel;
    Bevel1: TBevel;
    GLSphere1: TGLSphere;
    TorusImpostor: TGLTorus;
    Memo1: TMemo;
    GLTorus2: TGLTorus;
    LabelFPS: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure edtAdvancedBlurAmpChange(Sender: TObject);
    procedure trkAdvancedBlurHiClampChange(Sender: TObject);
    procedure trkAdvancedBlurLoClampChange(Sender: TObject);
    procedure edtAdvancedBlurPassesChange(Sender: TObject);
    procedure GLBlur1BeforeTargetRender(Sender: TObject);
    procedure GLBlur1AfterTargetRender(Sender: TObject);
  private
     
    mx, my: Integer;
  public
     
  end;

var
  FormBlurAdvanced: TFormBlurAdvanced;

implementation

{$R *.dfm}

procedure TFormBlurAdvanced.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  // Blur GLDummyCube1and it's children
  GLBlur1.TargetObject := GLDummyCube1;
  // point to GLDummyCube1
  GLCamera1.TargetObject := GLDummyCube1;
  // load materials
  GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile
    ('beigemarble.jpg');
  GLMaterialLibrary1.Materials[1].Material.Texture.Image.LoadFromFile
    ('moon.bmp');
end;

procedure TFormBlurAdvanced.GLBlur1BeforeTargetRender(Sender: TObject);
begin
  TorusImpostor.Visible := true; // GLBlur1 must render the Torusimpostor
end;

procedure TFormBlurAdvanced.GLBlur1AfterTargetRender(Sender: TObject);
begin
  TorusImpostor.Visible := false;
  // GLSCeneViewer1 must NOT render the Torusimpostor
end;

procedure TFormBlurAdvanced.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TFormBlurAdvanced.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if (ssRight in Shift) and (Y > 10) then
    GLCamera1.AdjustDistanceToTarget(my / Y);
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;

procedure TFormBlurAdvanced.Timer1Timer(Sender: TObject);
begin
  LabelFPS.Caption := GLSceneViewer1.FramesPerSecondText(0);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormBlurAdvanced.trkAdvancedBlurHiClampChange(Sender: TObject);
begin
  GLBlur1.AdvancedBlurHiClamp := trkAdvancedBlurHiClamp.Position;
end;

procedure TFormBlurAdvanced.trkAdvancedBlurLoClampChange(Sender: TObject);
begin
  GLBlur1.AdvancedBlurLowClamp := trkAdvancedBlurLoClamp.Position;
end;

procedure TFormBlurAdvanced.edtAdvancedBlurAmpChange(Sender: TObject);
begin
  GLBlur1.AdvancedBlurAmp := StrToFloat(edtAdvancedBlurAmp.Text);
end;

procedure TFormBlurAdvanced.edtAdvancedBlurPassesChange(Sender: TObject);
begin
  GLBlur1.AdvancedBlurPasses := StrToInt(edtAdvancedBlurPasses.Text);
end;

end.
