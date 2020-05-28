unit Unit1;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  
  GLScene,
  GLObjects,
  GLVectorGeometry,
  GLTexture,
  GLCadencer,
  GLVectorTypes,
  GLWin32Viewer,
  GLColor,
  GLCrossPlatform,
  GLCoordinates,
  GLMaterial,
  GLSimpleNavigation,
  GLBaseClasses;

type
  TForm1 = class(TForm)
	 GLSceneViewer1: TGLSceneViewer;
	 GLScene1: TGLScene;
	 GLCamera1: TGLCamera;
	 DummyCube1: TGLDummyCube;
	 GLLightSource1: TGLLightSource;
   GLCadencer1: TGLCadencer;
   GLSimpleNavigation1: TGLSimpleNavigation;
	 procedure FormCreate(Sender: TObject);
   procedure GLCadencer1Progress(Sender: TObject;
     const deltaTime, newTime: Double);
  private
     
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

const
	cSize = 5;

procedure TForm1.FormCreate(Sender: TObject);
var
	x, y, z : Integer;
	cube : TGLCube;
	factor, cubeSize : Single;
  Color : TColor;
begin
	// bench only creation and 1st render (with lists builds, etc...)
	factor:=70/(cSize*2+1);
	cubeSize:=0.4*factor;
	for x := -cSize to cSize do
    for y := -cSize to cSize do
      for z := -cSize to cSize do
      begin
        cube := TGLCube(DummyCube1.AddNewChild(TGLCube));
        cube.Position.AsVector := PointMake(factor * x, factor * y, factor * z);
        cube.CubeWidth := cubeSize;
        cube.CubeHeight := cubeSize;
        cube.CubeDepth := cubeSize;
        cube.Material.BlendingMode := bmTransparency;
        with cube.Material.FrontProperties do
        begin
          Color := Random(1);
          Diffuse.Color := VectorLerp(clrBlue, clrWhite, (x * x + y * y + z * z)
            / (cSize * cSize * 3));
          Diffuse.Alpha := 0.5;
        end;
      end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
	DummyCube1.TurnAngle:=90*newTime; // 90° per second
end;

end.
