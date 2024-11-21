unit fObjMaterialD;

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

  Stage.VectorTypes,
  GLS.SimpleNavigation,
  Stage.Utils,
  GLS.Mesh;

type
  TFormMO = class(TForm)
    GLScene: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    GLCamera: TGLCamera;
    GLLightSource1: TGLLightSource;
    dcDodecahedron: TGLDummyCube;
    GLCadencer: TGLCadencer;
    GLMatLibCube: TGLMaterialLibrary;
    PolyDod12: TGLPolygon;
    dcCube: TGLDummyCube;
    CubeMap: TGLCube;
    Hexahedron: TGLHexahedron;
    Panel1: TPanel;
    RadioGroupObjects: TRadioGroup;
    GLSimpleNavigation1: TGLSimpleNavigation;
    ffSphere: TGLFreeForm;
    dcPlaneCube: TGLDummyCube;
    PlaneFront: TGLPlane;
    PlaneBack: TGLPlane;
    PlaneBottom: TGLPlane;
    PlaneTop: TGLPlane;
    PlaneRight: TGLPlane;
    PlaneLeft: TGLPlane;
    GLMesh1: TGLMesh;
    chbRotate: TCheckBox;
    GLPoints1: TGLPoints;
    dcPolyHex: TGLDummyCube;
    PolyBottom: TGLPolygon;
    PolyLeft: TGLPolygon;
    PolyTop: TGLPolygon;
    PolyRight: TGLPolygon;
    PolyBack: TGLPolygon;
    PolyFront: TGLPolygon;
    DiskMap: TGLDisk;
    plBottom: TGLPlane;
    Dodecahedron: TGLDodecahedron;
    PolyDod1: TGLPolygon;
    PolyDod2: TGLPolygon;
    PolyDod3: TGLPolygon;
    PolyDod5: TGLPolygon;
    PolyDod6: TGLPolygon;
    PolyDod7: TGLPolygon;
    PolyDod8: TGLPolygon;
    PolyDod9: TGLPolygon;
    PolyDod10: TGLPolygon;
    PolyDod11: TGLPolygon;
    dcPolyDod: TGLDummyCube;
    PolyDod4: TGLPolygon;
    GLMatLibOctave: TGLMaterialLibrary;
    dcIcosahedron: TGLDummyCube;
    Icosahedron: TGLIcosahedron;
    dcDiskDod: TGLDummyCube;
    diskDo: TGLDisk;
    GLMatLibOctava: TGLMaterialLibrary;
    procedure FormCreate(Sender: TObject);
    procedure dcPlaneCubeProgress(Sender: TObject; const DeltaTime, NewTime: Double);
    procedure GLCadencerProgress(Sender: TObject; const DeltaTime, NewTime: Double);
  private

  public
    Path: TFileName;
    procedure PointsCube(Sender: TObject);
  end;

var
  FormMO: TFormMO;

implementation //------------------------------------------------------------

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
    X := Random(10) - 5;
    Y := Random(10) - 5;
    Z := Random(10) - 5;
    GLPoints1.Positions.Add(X * 0.05, Y * 0.05, Z * 0.05);
	
	Color.X := Random();
    Color.Y := Random();
    Color.Z := Random();
    GLPoints1.Colors.AddPoint(Color);
  end;
//  dcWorld.Remove(GLPoints1, False);
//  GLPoints1 := TGLPoints(dcWorld.AddNewChild(TGLPoints));
end;


procedure TFormMO.FormCreate(Sender: TObject);
begin
  Path := GetCurrentAssetPath()  + '\map';
  SetCurrentDir(Path);
  CubeMap.Material.Texture.Disabled := False;
  CubeMap.Material.Texture.Image.LoadFromFile('earth.jpg');
//  GLMaterialLibrary1.Materials[6].Material.Texture.Image.LoadFromFile('earth.jpg');
//  GLMaterialLibrary1.LibMaterialByName('txEarth').Material.Texture.Image.LoadFromFile('earth.jpg');
  PointsCube(Self);
end;


procedure TFormMO.GLCadencerProgress(Sender: TObject; const DeltaTime, NewTime: Double);
begin
  GLSceneViewer.Invalidate;
  if chbRotate.Checked then
  begin
    case RadioGroupObjects.ItemIndex of
      0: CubeMap.TurnAngle := -90 * NewTime;
      1: Hexahedron.TurnAngle := -90 * NewTime;
      2: dcPlaneCube.TurnAngle := -90 * NewTime;
      3: GLPoints1.TurnAngle := -90 * NewTime;
      4: dcPolyHex.TurnAngle := -90 * NewTime;
      5: DiskMap.RollAngle := -90 * NewTime;
      6: Dodecahedron.TurnAngle := -90 * NewTime;
      7: Icosahedron.TurnAngle := -90 * NewTime;
    end;
  end;
end;

procedure TFormMO.dcPlaneCubeProgress(Sender: TObject; const DeltaTime, NewTime: Double);
begin
  dcPlaneCube.MoveObjectAround(GLCamera.TargetObject, sin(NewTime) * DeltaTime * 10, DeltaTime * 20);
end;


end.
