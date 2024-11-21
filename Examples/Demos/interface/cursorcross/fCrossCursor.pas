unit fCrossCursor;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Math,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  GLS.Scene,
  GLS.Cadencer,
  GLS.Objects,
  GLS.GeomObjects,
  GLS.SceneViewer,
  Stage.VectorGeometry,
  Stage.VectorTypes,
  GLS.FileTGA,
  GLS.Coordinates,
  GLS.BaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    Camera: TGLCamera;
    Player: TGLDummyCube;
    Actor: TGLCone;
    GLCadencer1: TGLCadencer;
    GLLightSource1: TGLLightSource;
    PlaneTarget: TGLPlane;
    procedure vpMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

  _mx,_my:integer;
  _look:boolean=false;

implementation

{$R *.dfm}

procedure TForm1.vpMouseMove(Sender:TObject;Shift:TShiftState;X,Y:Integer);
var
   d,c:single;
begin
  _mx:=x;
  _my:=y;
  _look:=true;
  {
  d := round(Form1.GLSceneViewer1.Width / 2);
  c := round(Form1.GLSceneViewer1.Height / 2);
  Form1.Actor.RollAngle := round(NormalizeDegAngle(radtodeg(arctan2(X - d + 32, Y - c + 32))));
  Form1.Caption := inttostr(round(NormalizeDegAngle(radtodeg(arctan2(X - d + 32, Y - c + 32)))));
  }
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  glcadencer1.Enabled:=false;
  vp.Free;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
  v: TVector4f;
begin
  PlaneTarget.Roll(deltaTime * 50);

  if _look then
  begin
    vp.Buffer.ScreenVectorIntersectWithPlane(vectormake(_mx, vp.height - _my,
      0), Player.AbsolutePosition, Player.AbsoluteUp, v);
    PlaneTarget.AbsolutePosition := v;
    Player.PointTo(v, Player.AbsoluteUp);
    _look := false;
  end;
end;

end.
