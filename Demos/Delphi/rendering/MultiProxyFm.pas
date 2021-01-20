unit MultiProxyFm;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  
  GLS.Cadencer,
  GLS.Scene,
  GLS.VectorTypes,
  GLS.Objects,
  GLS.Particles,
  GLS.SceneViewer,
  GLS.Color,
  GLS.MultiProxy,
  GLS.Texture,
 
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.VectorGeometry;

type
  TFormMultiProxy = class(TForm)
    GLScene: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera: TGLCamera;
    DCTarget: TGLDummyCube;
    DCReferences: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLParticles: TGLParticles;
    SPHighRes: TGLSphere;
    SPMedRes: TGLSphere;
    SPLowRes: TGLSphere;
    GLCadencer: TGLCadencer;
    Timer1: TTimer;
    MPSphere: TGLMultiProxy;
    Panel1: TPanel;
    RBUseLODs: TRadioButton;
    RBHighRes: TRadioButton;
    CBColorize: TCheckBox;
    RBLowRes: TRadioButton;
    LabelFPS: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure MPSphereProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure RBUseLODsClick(Sender: TObject);
  private
     
  public
     
  end;

var
  FormMultiProxy: TFormMultiProxy;

implementation

{$R *.dfm}

procedure TFormMultiProxy.FormCreate(Sender: TObject);
var
   i : Integer;
begin
   // adjust settings to their default
   RBUseLODsClick(Self);
   // replicate the multiproxy via a TGLParticles object
   for i:=0 to 35 do
      GLParticles.CreateParticle.TagFloat:=DegToRad(i*10);
end;

procedure TFormMultiProxy.MPSphereProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   // this is invoked for each of our MultiProxys, it makes them loop an ellipse 
   with (Sender as TGLBaseSceneObject) do begin
      Position.X:=Sin(newTime+TagFloat)*80-60;
      Position.Z:=Cos(newTime+TagFloat)*7;
   end;
end;

procedure TFormMultiProxy.RBUseLODsClick(Sender: TObject);
begin
   // adjust LOD on/off (by adjusting base sphere's detail)
   if RBUseLODs.Checked then begin
      SPHighRes.Slices:=32;
      SPHighRes.Stacks:=32;
      SPMedRes.Slices:=16;
      SPMedRes.Stacks:=16;
      SPLowRes.Slices:=8;
      SPLowRes.Stacks:=8;
   end else if RBHighRes.Checked then begin
      SPHighRes.Slices:=32;
      SPHighRes.Stacks:=32;
      SPMedRes.Slices:=32;
      SPMedRes.Stacks:=32;
      SPLowRes.Slices:=32;
      SPLowRes.Stacks:=32;
   end else if RBLowRes.Checked then begin
      SPHighRes.Slices:=8;
      SPHighRes.Stacks:=8;
      SPMedRes.Slices:=8;
      SPMedRes.Stacks:=8;
      SPLowRes.Slices:=8;
      SPLowRes.Stacks:=8;
   end;
   // colorize the LODs, to make them clearly visible
   CBColorize.Enabled:=RBUseLODs.Checked;
   if CBColorize.Checked and RBUseLODs.Checked then begin
      SPHighRes.Material.FrontProperties.Diffuse.Color:=clrRed;
      SPMedRes.Material.FrontProperties.Diffuse.Color:=clrBlue;
      SPLowRes.Material.FrontProperties.Diffuse.Color:=clrYellow;
   end else begin
      SPHighRes.Material.FrontProperties.Diffuse.Color:=clrGray80;
      SPMedRes.Material.FrontProperties.Diffuse.Color:=clrGray80;
      SPLowRes.Material.FrontProperties.Diffuse.Color:=clrGray80;
   end;
end;

procedure TFormMultiProxy.Timer1Timer(Sender: TObject);
begin
   LabelFPS.Caption:=Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
