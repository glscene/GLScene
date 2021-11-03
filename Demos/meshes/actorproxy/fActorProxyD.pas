unit fActorProxyD;

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
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Imaging.Jpeg,

  
  GLS.Scene,
  GLS.VectorTypes,
  GLS.SceneViewer,
  GLS.VectorFileObjects,
  GLS.Objects,
  GLS.ProxyObjects,
  GLS.GeomObjects,
  GLS.VectorGeometry,
  GLS.Cadencer,
  GLS.Texture,
  GLS.Material,
  GLS.Coordinates,
 
  GLS.BaseClasses,
  GLS.Utils,
  GLS.FileSMD;

type
  TFormActorProxy = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    InvisibleDummyCube: TGLDummyCube;
    GLDummyCube2: TGLDummyCube;
    MasterActor: TGLActor;
    GLActorProxy1: TGLActorProxy;
    GLArrowLine1: TGLArrowLine;
    GLLightSource1: TGLLightSource;
    Timer1: TTimer;
    GLSphere1: TGLSphere;
    GLArrowLine3: TGLArrowLine;
    GLActorProxy2: TGLActorProxy;
    GLArrowLine2: TGLArrowLine;
    Panel1: TPanel;
    cbActorsAreTurning: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    mouseX, mouseY : Integer;
    procedure DoRaycastStuff;
     
  public
     
  end;

var
  FormActorProxy: TFormActorProxy;

implementation

{$R *.dfm}

procedure TFormActorProxy.FormCreate(Sender: TObject);
var
  i : Integer;
begin
  SetGLSceneMediaDir();
  MasterActor.LoadFromFile('TRINITYrage.smd');
  MasterActor.AddDataFromFile('run.smd');
  MasterActor.AddDataFromFile('jump.smd');

  MasterActor.Animations.Items[0].Name:='still';
  MasterActor.Animations.Items[1].Name:='walk';
  MasterActor.Animations.Items[2].Name:='jump';

  for i := 0 to MasterActor.Animations.Count-1 do
  begin
      MasterActor.Animations[i].MakeSkeletalTranslationStatic;
      MasterActor.SwitchToAnimation(i); // forces animations to be initialized for ActorsProxies
  end;
  MasterActor.SwitchToAnimation(0);   // revert back to empty animation (not necessary)
  MasterActor.AnimationMode:=aamLoop; // animationmode is shared between proxies.

  GLActorProxy1.StoreBonesMatrix:=true;
  GLActorProxy2.StoreBonesMatrix:=true;


  GLActorProxy1.Animation := MasterActor.Animations[1].Name;
  GLActorProxy2.Animation := MasterActor.Animations[2].Name;
end;

procedure TFormActorProxy.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  mouseX := X;
  mouseY := Y;
end;

procedure TFormActorProxy.DoRaycastStuff;
var
   rayStart, rayVector, iPoint, iNormal : TGLVector;
begin
     SetVector(rayStart, GLCamera1.AbsolutePosition);
     SetVector(rayVector, GLSceneViewer1.Buffer.ScreenToVector(
               AffineVectorMake(mouseX, GLSceneViewer1.Height-mouseY, 0)));
     NormalizeVector(rayVector);

     if GLActorProxy1.RayCastIntersect(rayStart,rayVector,@iPoint,@iNormal) then
     begin
        GLSphere1.Position.AsVector:=iPoint;
        GLSphere1.Direction.AsVector:=VectorNormalize(iNormal);
     end
     else
     if GLActorProxy2.RayCastIntersect(rayStart,rayVector,@iPoint,@iNormal) then
     begin
        GLSphere1.Position.AsVector:=iPoint;
        GLSphere1.Direction.AsVector:=VectorNormalize(iNormal);
     end
     else
     begin
        GLSphere1.Position.AsVector:=rayStart;
        GLSphere1.Direction.AsVector:=rayVector;
     end;
end;

procedure TFormActorProxy.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
     // Align object to hand
     GLArrowLine1.Matrix^ := GLActorProxy1.BoneMatrix('Bip01 R Finger1');
     GLArrowLine2.Matrix^ := GLActorProxy2.BoneMatrix('Bip01 R Finger1');

     // turn actors
     if cbActorsAreTurning.Checked then
     begin
       GLActorProxy1.Turn(-deltaTime *130);
       GLActorProxy2.Turn(deltaTime *100);
     end;

     DoRaycastStuff;
end;

procedure TFormActorProxy.Timer1Timer(Sender: TObject);
begin
     Panel1.Caption:=GLSceneViewer1.FramesPerSecondText(0);
     GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
