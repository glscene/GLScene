unit Unit1;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,

  
  GLScene, GLVectorFileObjects, GLObjects, GLWin32Viewer, GLVectorTypes,
  GLVectorGeometry, GLGeomObjects, GLCrossPlatform, GLCoordinates,
  GLBaseClasses, GLFile3DS, GLUtils;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    FreeForm1: TGLFreeForm;
    Sphere1: TGLSphere;
    ArrowLine1: TGLArrowLine;
    GLSceneViewer2: TGLSceneViewer;
    GLCamera2: TGLCamera;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer2MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer2MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
     
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
   // Load mushroom mesh
   SetGLSceneMediaDir();
   FreeForm1.LoadFromFile('mushroom.3ds');
end;

// Perform the raycasting for the perspective camera & viewer

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   rayStart, rayVector, iPoint, iNormal : TVector;
begin
   // retrieve raycasting data:
   //    rayStart is obtained for camera and screen position
   //    rayVector is the camera direction (i.e direction to target since our camera is targeted)
   // (note that (0, 0) is lower left for the Screen function, whereas Delphi
   //  uses top-left as origin, hence the Y inversion)
   SetVector(rayStart, GLSceneViewer1.Buffer.OrthoScreenToWorld(x, GLSceneViewer1.Height-y));
   SetVector(rayVector, GLCamera1.AbsoluteVectorToTarget);
   NormalizeVector(rayVector);
   // Here we require RayCast intersection
   if FreeForm1.RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal) then begin
      // got one, move the sphere there and orient it appropriately
      Sphere1.Position.AsVector:=iPoint;
      Sphere1.Direction.AsVector:=VectorNormalize(iNormal);
      // make it visible
      Sphere1.Visible:=True;
   end else begin
      // hide it if we did not hit
      Sphere1.Visible:=False;
   end;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   // when mouse moves, recompute intersection
   if Shift<>[] then GLSceneViewer1MouseDown(Sender, TMouseButton(mbLeft), Shift, x, y);
end;

// Perform the raycasting for the perspective camera & viewer

procedure TForm1.GLSceneViewer2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   rayStart, rayVector, iPoint, iNormal : TVector;
begin
   // retrieve raycasting data:
   //    rayStart is the eye (camera) position
   //    rayVector is computed from screen position
   // (note that (0, 0) is lower left for the Screen function, whereas Delphi
   //  uses top-left as origin, hence the Y inversion)
   SetVector(rayStart, GLCamera2.AbsolutePosition);
   SetVector(rayVector, GLSceneViewer2.Buffer.ScreenToVector(AffineVectorMake(x, GLSceneViewer2.Height-y, 0)));
   NormalizeVector(rayVector);
   // Here we request RayCast intersection
   if FreeForm1.RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal) then begin
      // got one, move the sphere there and orient it appropriately
      Sphere1.Position.AsVector:=iPoint;
      Sphere1.Direction.AsVector:=VectorNormalize(iNormal);
      // make it visible
      Sphere1.Visible:=True;
   end else begin
      // hide it if we did not hit
      Sphere1.Visible:=False;
   end;
end;

procedure TForm1.GLSceneViewer2MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then GLSceneViewer2MouseDown(Sender, TMouseButton(mbLeft), Shift, x, y);
end;

end.
