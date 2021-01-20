unit WhirlwindFm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.OpenGL,
  System.SysUtils,
  System.Math,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,

  
 
  GLS.BaseClasses,
  GLS.Scene,
  GLS.PersistentClasses,
  GLS.VectorGeometry,
  GLS.SceneViewer,
  GLS.Particles,
  GLS.Cadencer,
  GLS.Objects,
  GLS.Coordinates,
  GLS.Behaviours;

type
  TFormWhirlwind = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    Panel1: TPanel;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    GLScene1: TGLScene;
    GLParticles1: TGLParticles;
    DummyCube1: TGLDummyCube;
    Sprite1: TGLSprite;
    GLCamera1: TGLCamera;
    procedure Timer1Timer(Sender: TObject);
    procedure GLDummyCube1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure GLParticles1ActivateParticle(Sender: TObject;
      particle: TGLBaseSceneObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
     
    mx, my : Integer;
  public
     
  end;

var
  FormWhirlwind: TFormWhirlwind;

implementation

{$R *.dfm}

procedure TFormWhirlwind.GLParticles1ActivateParticle(Sender: TObject;
  particle: TGLBaseSceneObject);
var
	r, alpha, cr, sr : Single;
begin
	with particle do begin
		alpha:=Random*2*PI;
		r:=2*Random;
      SinCosine(alpha, r*r, sr, cr);
		Children[0].Position.SetPoint(sr, 3*r-3, cr);
		GetOrCreateInertia(particle).TurnSpeed:=Random(30);
		TGLCustomSceneObject(particle).TagFloat:=GLCadencer1.CurrentTime;
	end;
end;

procedure TFormWhirlwind.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TFormWhirlwind.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
   if Shift<>[] then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      mx:=x; my:=y;
   end;
end;

procedure TFormWhirlwind.GLDummyCube1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
	with TGLCustomSceneObject(Sender) do begin
		if newTime-TagFloat>3 then
			GLParticles1.KillParticle(TGLCustomSceneObject(Sender));
	end;
end;

procedure TFormWhirlwind.Timer1Timer(Sender: TObject);
begin
	Panel1.Caption:=Format('%d particles, %.1f FPS',
						 [GLParticles1.Count, GLSceneViewer1.FramesPerSecond]);
	GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormWhirlwind.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
	GLParticles1.CreateParticle;
end;

end.
