unit fObjectMatsD;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  GLS.Scene,
  GLS.VectorFileObjects,
  GLS.Objects,
  GLS.Coordinates,
  GLS.SceneViewer,
  GLS.BaseClasses,
  GLS.Material,
  GLS.Cadencer,
  GLS.GeomObjects,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  GLS.VectorTypes,
  GLS.SimpleNavigation,
  GLS.Utils,
  GLS.Mesh;

type
  TFormMO = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLActor1: TGLActor;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLPolygon1: TGLPolygon;
    dcCubes: TGLDummyCube;
    GLCube1: TGLCube;
    GLHexahedron1: TGLHexahedron;
    Panel1: TPanel;
    RadioGroup1: TRadioGroup;
    GLSimpleNavigation1: TGLSimpleNavigation;
    ffSphere: TGLFreeForm;
    dcPlaneCube: TGLDummyCube;
    PlaneFront: TGLPlane;
    GLPlaneBack: TGLPlane;
    GLPlaneBottom: TGLPlane;
    GLPlaneTop: TGLPlane;
    GLPlaneRight: TGLPlane;
    GLPlaneLeft: TGLPlane;
    GLMesh1: TGLMesh;
    chbRotate: TCheckBox;
    GLPoints1: TGLPoints;
    dcPolyCube: TGLDummyCube;
    PolyBottom: TGLPolygon;
    PolyLeft: TGLPolygon;
    PolyTop: TGLPolygon;
    PolyRight: TGLPolygon;
    PolyBack: TGLPolygon;
    GLPolygon4: TGLPolygon;
    GLDisk1: TGLDisk;
    polyTriangle: TGLPolygon;
    procedure FormCreate(Sender: TObject);
    procedure dcPlaneCubeProgress(Sender: TObject; const DeltaTime, NewTime: Double);
    procedure GLCadencer1Progress(Sender: TObject; const DeltaTime, NewTime: Double);
  private

  public
    Path: TFileName;
    procedure PointsCube(Sender: TObject);
  end;

var
  FormMO: TFormMO;

implementation

{$R *.dfm}

// Make PointsCube
procedure TFormMO.PointsCube(Sender: TObject);
var
  I: Integer;
  Color: TVector3f;
  NumPoints: Integer;
  X, Y, Z: Single;

begin
  NumPoints := 1000;
  GLPoints1.Position.SetPoint(0.0, 0.0, -2);
  GLPoints1.Size := 5.0;
  GLPoints1.Style := psRound;
  for I := 0 to NumPoints - 1 do
  begin
    Color.X := Random();
    Color.Y := Random();
    Color.Z := Random();

    X := Random(10) - 5;
    Y := Random(10) - 5;
    Z := Random(10) - 5;

    GLPoints1.Positions.Add(X * 0.05, Y * 0.05, Z * 0.05);
    // Fill array of GLPoints
    GLPoints1.Colors.AddPoint(Color);
  end;
//  dcWorld.Remove(GLPoints1, False);
//  GLPoints1 := TGLPoints(dcWorld.AddNewChild(TGLPoints));
end;


procedure TFormMO.FormCreate(Sender: TObject);
begin
  Path := GetCurrentAssetPath();
  SetCurrentDir(Path + '\map');
  GLCube1.Material.Texture.Disabled := False;
  GLCube1.Material.Texture.Image.LoadFromFile('earth.jpg');
//  GLMaterialLibrary1.Materials[6].Material.Texture.Image.LoadFromFile('earth.jpg');
//  GLMaterialLibrary1.LibMaterialByName('txEarth').Material.Texture.Image.LoadFromFile('earth.jpg');
  PointsCube(Self);
end;


procedure TFormMO.GLCadencer1Progress(Sender: TObject; const DeltaTime, NewTime: Double);
begin
  GLSceneViewer1.Invalidate;
  if chbRotate.Checked then
  begin
    case RadioGroup1.ItemIndex of
      0: GLCube1.TurnAngle := 90 * NewTime;
      1: GLHexahedron1.TurnAngle := 90 * NewTime;
      2: dcPlaneCube.RollAngle := -90 * NewTime;
      3: GLPoints1.TurnAngle := 90 * NewTime;
      4: dcPolyCube.PitchAngle := -90 * NewTime;
      5: GLDisk1.RollAngle := -90 * NewTime;
      6: PolyTriangle.TurnAngle := -90 * NewTime;
    end;
  end;
end;

procedure TFormMO.dcPlaneCubeProgress(Sender: TObject; const DeltaTime, NewTime: Double);
begin
 // dcPlaneCube.MoveObjectAround(GLCamera1.TargetObject, sin(NewTime) * DeltaTime * 10, DeltaTime * 20);
end;


end.
