unit RayCastFm;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,

  GLS.Scene,
  GLS.Objects,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.BaseClasses,
  GLS.Polynomials,
  GLS.Texture,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.GeomObjects,
  GLS.Color,
  GLS.Coordinates,
  GLS.Utils;

type
  TFormRayCast = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    Sphere1: TGLSphere;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    BUCast: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    GLCadencer1: TGLCadencer;
    DummyCube1: TGLDummyCube;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Torus1: TGLTorus;
    PaintBox1: TPaintBox;
    Plane1: TGLPlane;
    Cylinder1: TGLCylinder;
    GLCube1: TGLCube;
    GLAnnulus1: TGLAnnulus;
    procedure BUCastClick(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
  private
  end;

var
  FormRayCast: TFormRayCast;

implementation

{$R *.DFM}

procedure TFormRayCast.BUCastClick(Sender: TObject);
var
  o, v, vLight, light, iPoint, iNormal: TGLVector;
  up, right, dir: TGLVector;
  x, y, dx, dy: Integer;
  f, d: Single;
  color: TColor;
  iObj: TGLBaseSceneObject;
  t: Int64;
begin
  Screen.Cursor := crHourGlass;
  t := StartPrecisionTimer;

  // First we extract/prepare the vector we will use during our raycasting
  // the origin is the camera position, and factor was grossly adjusted so
  // that both view look grossly similar
  MakePoint(o, GLCamera1.AbsolutePosition);
  MakeVector(dir, GLCamera1.AbsoluteDirection);
  MakeVector(up, GLCamera1.AbsoluteUp);
  MakePoint(light, GLLightSource1.AbsolutePosition);
  right := VectorCrossProduct(dir, up);
  f := 1 / 300;
  dx := (PaintBox1.Width div 2);
  dy := (PaintBox1.Height div 2);

  // Cover a square area
  for y := 0 to PaintBox1.Height - 1 do
  begin
    for x := 0 to PaintBox1.Width - 1 do
    begin

      // Calculate our ray vector for current pixel
      v := VectorCombine3(dir, right, up, 1, (x - dx) * f, (dy - y) * f);
      // ray vectors must be of unit length!
      NormalizeVector(v);

      // ray cast
      iObj := GLScene1.RayCastIntersect(o, v, @iPoint, @iNormal);

      if Assigned(iObj) then
      begin
        // if something found, calculate vector to light source
        vLight := VectorSubtract(light, iPoint);
        NormalizeVector(vLight);
        // color is given by the normal/lightsource vectors dot-product
        // and this intensity is composited with the object's diffuse color
        NormalizeVector(iNormal);
        d := VectorDotProduct(iNormal, vLight);
        if d < 0 then
          d := 0;
        with (iObj as TGLCustomSceneObject).Material.FrontProperties do
          color := ConvertColorVector(Diffuse.color, d);
      end
      else
        color := clGray;

      // plot our point
      PaintBox1.Canvas.Pixels[x, y] := color;

    end;
  end;

  Caption := Format('RayCast in %.1f ms', [StopPrecisionTimer(t) * 1000]);
  Screen.Cursor := crDefault;
end;

procedure TFormRayCast.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  DummyCube1.TurnAngle := newTime * 50;
end;

end.
