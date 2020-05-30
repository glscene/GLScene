unit ExPolygon1;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  
  GLScene, GLObjects, GLGeomObjects, GLTexture, GLMultiPolygon,
  GLVectorGeometry,  GLWin32Viewer, GLCrossPlatform, GLMaterial,
  GLCoordinates, GLBaseClasses, GLVectorTypes;

type
  TVektor = record
    x,y,z : Double;
  end;

  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLLightSource1: TGLLightSource;
    GLLightSource2: TGLLightSource;
    Container: TGLDummyCube;
    CameraTarget: TGLDummyCube;
    Camera: TGLCamera;
    GLMaterialLibrary1: TGLMaterialLibrary;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormShow(Sender: TObject);
  private
    mx,my : Integer;
    FPlane : array[0..5] of TGLMultiPolygon;
    FDY: Double;
    FDX: Double;
    FDZ: Double;
    function GetPlane(Side: Integer): TGLMultiPolygon;
    procedure SetDX(const Value: Double);
    procedure SetDY(const Value: Double);
    procedure SetDZ(const Value: Double);
    procedure CreatePanel;
    procedure AddMaterial(Obj:TGLSceneObject);
    procedure ReDraw;
    function TransformToPlane(Side:Integer; x,y,z:Double):TVektor; overload;
    function TransformToPlane(Side:Integer; v:TVektor):TVektor; overload;
  public
    { Public-Deklarationen }
    procedure MakeHole(Side:Integer; X,Y,Z,D,T:Double; Phi:Double=0; Rho:Double=0);
    property Plane[Side:Integer]:TGLMultiPolygon read GetPlane;
    property DX:Double read FDX write SetDX;
    property DY:Double read FDY write SetDY;
    property DZ:Double read FDZ write SetDZ;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

function Vektor(x,y,z:Double):TVektor;
begin
  result.x := x;
  result.y := y;
  result.z := z;
end;

const
  cOpposite : array[0..5] of Integer = (5,3,4,1,2,0);

procedure TForm1.MakeHole(Side: Integer; X, Y, Z, D, T, Phi, Rho: Double);
var
  R : Double;
  Dum : TGLDummyCube;
  Cyl : TGLCylinder;
  through : Boolean;
begin
  Dum := TGLDummyCube.Create(nil);
  Dum.Position.x := X;
  Dum.Position.y := Y;
  Dum.Position.z := Z;
  case Side of
    0 : Dum.PitchAngle := -90;
    1 : Dum.RollAngle  :=  90;
    2 : Dum.RollAngle  := 180;
    3 : Dum.RollAngle  := 270;
    4 : Dum.RollAngle  :=   0;
    5 : Dum.PitchAngle :=  90;
  end;
  Dum.PitchAngle := Dum.PitchAngle + Rho;
  Dum.RollAngle := Dum.RollAngle + Phi;
  R := 0.5*D;
  through := true;
  case Side of
    0 : if (Z-T)<=0 then T := Z else through := false;
    1 : if (X+T)>=DX then T := DX-X else through := false;
    2 : if (Y+T)>=DY then T := DY-Y else through := false;
    3 : if (X-T)<=0 then T := X else through := false;
    4 : if (Y-T)<=0 then T := Y else through := false;
    5 : if (Z+T)>=DZ then T := DZ-Z else through := false;
  end;
  Cyl := TGLCylinder.Create(nil);
  AddMaterial(Cyl);
  Cyl.Position.x := 0;
  Cyl.Position.y := - 0.5*T;
  Cyl.Position.z := 0;
  Cyl.Height := T;
  Cyl.BottomRadius := R;
  Cyl.TopRadius := R;
  Cyl.NormalDirection := ndInside;
  if through then Cyl.Parts := [cySides]
  else Cyl.Parts := [cySides,cyBottom];
  Dum.AddChild(Cyl);
  Container.AddChild(Dum);

  Plane[Side].Contours.Add.Nodes.AddXYArc(R/cos(Phi*c180divPi),R,0,360,16, AffineVectorMake(X,Y,0));

  if through then
    Plane[cOpposite[Side]].Contours.Add.Nodes.AddXYArc(R/cos(Phi*c180divPi),R,0,360,16, AffineVectorMake(X,Y,0));
end;

procedure TForm1.CreatePanel;
var
  I : Integer;

  function MakePlane(X,Y,Z,P,T,W,H:Double):TGLMultiPolygon;
  begin
    result := TGLMultiPolygon.Create(nil);
    result.Material.MaterialLibrary := GLMaterialLibrary1;
    result.Material.LibMaterialName := 'MatSurface';
    result.Parts := [ppTop];
    result.AddNode(0,0,0,0);
    result.AddNode(0,W,0,0);
    result.AddNode(0,W,H,0);
    result.AddNode(0,0,H,0);
    result.Position.x := X;
    result.Position.y := Y;
    result.Position.z := Z;
    result.PitchAngle := P;
    result.TurnAngle  := T;
  end;

begin
  Container.DeleteChildren;
  FPlane[2] := MakePlane( 0, 0, 0, -90,  0,DX,DZ);
  FPlane[3] := MakePlane(DX, 0, 0, -90, 90,DY,DZ);
  FPlane[4] := MakePlane(DX,DY, 0, -90,180,DX,DZ);
  FPlane[1] := MakePlane( 0,DY, 0, -90,270,DY,DZ);
  FPlane[5] := MakePlane( 0,DY, 0,-180,  0,DX,DY);
  FPlane[0] := MakePlane( 0, 0,DZ,   0,  0,DX,DY);
  for I:=0 to 5 do Container.AddChild(FPlane[I]);
end;

function TForm1.GetPlane(Side: Integer): TGLMultiPolygon;
begin
  result := FPlane[Side];
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Shift<>[] then
    Camera.MoveAroundTarget(my-y, mx-x);
  mx:=x; my:=y;
end;

procedure TForm1.SetDX(const Value: Double);
begin
  FDX := Value;
  Container.Position.X := -0.5*Value;
end;

procedure TForm1.SetDY(const Value: Double);
begin
  FDY := Value;
  Container.Position.Y := -0.5*Value;
end;

procedure TForm1.SetDZ(const Value: Double);
begin
  FDZ := Value;
  Container.Position.Z := -0.5*Value;
end;

procedure TForm1.AddMaterial(Obj: TGLSceneObject);
begin
  Obj.Material.MaterialLibrary := GLMaterialLibrary1;
  Obj.Material.LibMaterialName := 'MatInner';
end;

procedure TForm1.ReDraw;
begin
  DX := 600;
  DY := 400;
  DZ := 19;
  CreatePanel;
  MakeHole(0,0.5*DX,0.5*DY,DZ,50,DZ);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Redraw;
end;

function TForm1.TransformToPlane(Side: Integer; x, y, z: Double): TVektor;
begin
  case Side of
    0 : result := Vektor(x,y,z-DZ);
    1 : result := Vektor(DY-y,z,x);
    2 : result := Vektor(x,z,-y);
    3 : result := Vektor(y,z,DX-x);
    4 : result := Vektor(DX-x,z,DY-y);
    5 : result := Vektor(x,DY-y,z);
  else result := Vektor(x,y,z);
  end;
end;

function TForm1.TransformToPlane(Side: Integer; v: TVektor): TVektor;
begin
  result := TransformToPlane(Side,v.x,v.y,v.z);
end;

end.
