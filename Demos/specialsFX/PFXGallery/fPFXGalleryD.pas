unit fPFXGalleryD;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Imaging.Jpeg,

  GLS.VectorGeometry, 
  GLS.SceneViewer, 
  GLS.Scene, 
  GLS.HUDObjects, 
  GLS.ParticleFX,
  GLS.VectorFileObjects, 
  GLS.VectorTypes, 
  GLS.Objects, 
  GLS.BitmapFont,
  GLS.Utils,
  GLS.Cadencer,  
  GLS.Texture, 
  GLS.Navigator,
  GLS.GeomObjects, 
  GLS.Keyboard, 
  GLS.SpaceText, 
  GLS.Behaviours,
  GLS.PerlinPFX, 
  GLS.Blur,
  GLS.Coordinates, 
  GLS.BaseClasses;

const
     cRunBoost = 10;
     cWalkStep = 20;
     cStrafeStep =20;

type
  TFormPFXGallery = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    PFXRail: TGLPerlinPFXManager;
    PFXBurning: TGLPerlinPFXManager;
    PFXSmoke: TGLPerlinPFXManager;
    BitmapFont1: TGLBitmapFont;
    GLScene1: TGLScene;
    WorldRoot: TGLDummyCube;
    PfxRenderer: TGLParticleFXRenderer;
    GLCamera1: TGLCamera;
    PFXBlueArea: TGLPerlinPFXManager;
    Timer1: TTimer;
    PFXElectro: TGLPerlinPFXManager;
    GLNavigator1: TGLNavigator;
    GLUserInterface1: TGLUserInterface;
    ICE: TGLSpaceText;
    GLPlane1: TGLPlane;
    PFXRedArea: TGLPerlinPFXManager;
    MAGMA: TGLSpaceText;
    SMOKE: TGLSpaceText;
    RAIL: TGLSpaceText;
    FIRE: TGLSpaceText;
    ELECTRIC: TGLSpaceText;
    FOG: TGLSpaceText;
    PFXFog: TGLPerlinPFXManager;
    PFXWaterfall: TGLPerlinPFXManager;
    WATER: TGLSpaceText;
    Panel1: TPanel;
    chkMouseLook: TCheckBox;
    GLLightSource1: TGLLightSource;
    chkFloor: TCheckBox;
    GLBlur1: TGLBlur;
    chkBlur: TCheckBox;
    Label1: TLabel;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure chkMouseLookClick(Sender: TObject);
    procedure chkFloorClick(Sender: TObject);
    procedure chkBlurClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure HandleKeys(const deltaTime: Double);
     
  public
     

  end;

var
  FormPFXGallery: TFormPFXGallery;

implementation

{$R *.dfm}

procedure TFormPFXGallery.FormCreate(Sender: TObject);
begin
  chkFloorClick(Sender);
end;

procedure TFormPFXGallery.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
     HandleKeys(deltaTime);
     GLUserInterface1.Mouselook();

     GLSceneViewer1.Invalidate();
     GLUserInterface1.MouseUpdate();
     GLSceneViewer1.Invalidate();
end;


procedure TFormPFXGallery.HandleKeys(const deltaTime: Double);
var
   boost : Single;
begin
   if IsKeyDown(VK_ESCAPE) then begin
      chkMouseLook.Checked := false;
      chkMouseLookClick(self);
   end;

   if IsKeyDown(VK_SHIFT) then
      boost:=cRunBoost*deltaTime
   else
   if IsKeyDown(VK_CONTROL) then
      boost:=cRunBoost*0.01*deltaTime
   else
      boost:=deltaTime;

   if IsKeyDown('W') then
      GLCamera1.Move(cWalkStep*boost);
   if IsKeyDown('S') then
      GLCamera1.Move(-cWalkStep*boost);

   if IsKeyDown('A') then
          GLCamera1.Slide(-cStrafeStep*boost);
   if IsKeyDown('D') then
          GLCamera1.Slide(cStrafeStep*boost)
end;

procedure TFormPFXGallery.Timer1Timer(Sender: TObject);
begin
     Caption := 'PFXGallery ' + Inttostr(Round(GLSceneViewer1.FramesPerSecond))+' FPS';
     GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormPFXGallery.chkMouseLookClick(Sender: TObject);
begin
     GLUserInterface1.MouseLookActive:= chkMouseLook.Checked;
end;

procedure TFormPFXGallery.chkFloorClick(Sender: TObject);
begin
     GLPlane1.Visible := chkFloor.Checked;
end;

procedure TFormPFXGallery.chkBlurClick(Sender: TObject);
begin
     GLBlur1.Visible := chkBlur.Checked;
end;

end.
