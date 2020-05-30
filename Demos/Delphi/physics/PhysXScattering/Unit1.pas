unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls,
  
  GLScene, GLObjects, GLCoordinates, GLCadencer,  GLVectorTypes,
  GLWin32Viewer, GLColor,  GLCrossPlatform, GLPhysX, GLKeyboard,
  GLBaseClasses, GLVectorGeometry, GLGeomObjects, GLGui, GLWindows;

type
  TMainForm = class(TForm)
    GLScene: TGLScene;
    GLSV: TGLSceneViewer;
    Cadencer: TGLCadencer;
    Camera: TGLCamera;
    Center: TGLDummyCube;
    Light: TGLLightSource;
    Timer: TTimer;
    Edit1: TEdit;
    Plane: TGLPlane;
    Light1: TGLLightSource;
    procedure CadencerProgress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure GLSVMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
     
  public
    mx, my: Integer;
     
  end;

var
  MainForm: TMainForm;
  Plane: dword;
  boxes, capsules, spheres: array [0 .. 100] of dword;
  GLCube: array [0 .. 100] of TGLCube;
  GLCapsule: array [0 .. 100] of TGLCapsule;
  GLSphere: array [0 .. 100] of TGLSphere;

type
  TVec = packed record
    X, Y, Z: single;
  end;

type
  TQuat = packed record
    X, Y, Z, W: single;
  end;

implementation

{$R *.dfm}

function ByteToStr(b: byte): string;
const
  t: array [0 .. 9] of char = ('0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9');
var
  e: byte;
  d: byte;
  s: byte;
begin
  s := b div 100;
  b := b mod 100;
  d := b div 10;
  e := b mod 10;
  if s > 0 then
  begin
    result := t[s] + t[d] + t[e];
  end
  else
  begin
    if d > 0 then
      result := t[d] + t[e]
    else
      result := t[e];
  end;
end;

procedure DemoInit;
var
  i: Integer;
  dMaj, dMin, dBug: dword;
begin
  SDK_Version(@dMaj, @dMin, @dBug);
  MainForm.Caption := 'PhysX SDK version ' + ByteToStr(dMaj) + '.' +
    ByteToStr(dMin) + '.' + ByteToStr(dBug);
  InitNx;
  CreateGroundPlane(@Plane);
  for i := 0 to High(boxes) do
  begin
    CreateBox(@boxes[i], 0.5, 0.5, 0.5, 10);
    SetActorMass(boxes[i], 10);
    SetActorGlobalPosition(boxes[i], 0, i * 2, i * 0.5 - 20);
  end;
  for i := 0 to High(capsules) do
  begin
    CreateCylinder(@capsules[i], 0.5, 1, 10);
    SetActorMass(capsules[i], 10);
    SetActorGlobalPosition(capsules[i], 5, i * 2, i * 0.5 - 20);
  end;
  for i := 0 to High(spheres) do
  begin
    CreateSphere(@spheres[i], 0.5, 10);
    SetActorMass(spheres[i], 10);
    SetActorGlobalPosition(spheres[i], -5, i * 2, i * 0.5 - 20);
  end;
end;

procedure DrawBox(b: dword; i: Integer);
var
  v: TVec;
  q: TQuat;
  q1: TQuaternion;
begin
  GetActorGlobalPosition(b, @v.X, @v.Y, @v.Z);
  GetActorGlobalOrientation(b, @q.X, @q.Y, @q.Z, @q.W);
  MakeVector(q1.ImagPart, q.X, q.Y, q.Z);
  q1.RealPart := q.W;
  NormalizeQuaternion(q1);
  GLCube[i].Matrix := QuaternionToMatrix(q1);
  GLCube[i].Position.SetPoint(v.X, v.Y, v.Z);
end;

procedure DrawCapsule(b: dword; i: Integer);
var
  v: TVec;
  q: TQuat;
  q1: TQuaternion;
begin
  GetActorGlobalPosition(b, @v.X, @v.Y, @v.Z);
  GetActorGlobalOrientation(b, @q.X, @q.Y, @q.Z, @q.W);
  MakeVector(q1.ImagPart, q.X, q.Y, q.Z);
  q1.RealPart := q.W;
  NormalizeQuaternion(q1);
  GLCapsule[i].Matrix := QuaternionToMatrix(q1);
  GLCapsule[i].Pitch(90);
  GLCapsule[i].Position.SetPoint(v.X, v.Y, v.Z);
end;

procedure DrawSphere(b: dword; i: Integer);
var
  v: TVec;
  q: TQuat;
  q1: TQuaternion;
begin
  GetActorGlobalPosition(b, @v.X, @v.Y, @v.Z);
  GetActorGlobalOrientation(b, @q.X, @q.Y, @q.Z, @q.W);
  MakeVector(q1.ImagPart, q.X, q.Y, q.Z);
  q1.RealPart := q.W;
  NormalizeQuaternion(q1);
  GLSphere[i].Matrix := QuaternionToMatrix(q1);
  GLSphere[i].Position.SetPoint(v.X, v.Y, v.Z);
end;

procedure DemoFree;
begin
  ReleaseNx;
end;

procedure TMainForm.CadencerProgress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  i: Integer;
begin
  SimulateNx(deltaTime);
  GetResultsNx;
  for i := 0 to 100 do
  begin
    DrawBox(boxes[i], i);
    DrawCapsule(capsules[i], i);
    DrawSphere(spheres[i], i);
  end;
  if iskeydown(' ') then
  begin
    for i := 0 to 100 do
    begin
      ActorAddForce(boxes[i], 0, 100, 0);
      ActorAddForce(capsules[i], 0, 100, 0);
      ActorAddForce(spheres[i], 0, 100, 0);
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to High(GLCube) do
  begin
    GLCube[i] := TGLCube.CreateAsChild(GLScene.Objects);
  end;
  for i := 0 to High(GLCapsule) do
  begin
    GLCapsule[i] := TGLCapsule.CreateAsChild(GLScene.Objects);
  end;
  for i := 0 to High(GLSphere) do
  begin
    GLSphere[i] := TGLSphere.CreateAsChild(GLScene.Objects);
  end;
  DemoInit;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DemoFree;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  Edit1.Text := Format('%.1f FPS', [GLSV.FramesPerSecond]);
  GLSV.ResetPerformanceMonitor;
end;

procedure TMainForm.GLSVMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift <> [] then
  begin
    Camera.MoveAroundTarget(my - Y, mx - X);
    mx := X;
    my := Y;
  end;
end;

end.
