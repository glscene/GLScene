unit fNewtonMaterialD;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  GLS.SimpleNavigation,
  GLS.Scene,
  GLS.Coordinates,
  Physics.NGDManager,
  GLS.BitmapFont,
  GLS.WindowsFont,
  GLS.Cadencer,
  GLS.SceneViewer,
 
  GLS.BaseClasses,
  GLS.Objects;

type
  TFormNewtonMaterial = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLSimpleNavigation1: TGLSimpleNavigation;
    Trampoline: TGLCube;
    GLCube1: TGLCube;
    GLSphere1: TGLSphere;
    GLSphere2: TGLSphere;
    GLDummyCube1: TGLDummyCube;
    GLDummyCube2: TGLDummyCube;
    GLCube2: TGLCube;
    Friction: TGLCube;
    GLCube3: TGLCube;
    GLCube4: TGLCube;
    GLDummyCube3: TGLDummyCube;
    GLNGDManager1: TGLNGDManager;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
  private
     
  public
     
  end;

var
  FormNewtonMaterial: TFormNewtonMaterial;

implementation

{$R *.dfm}

procedure TFormNewtonMaterial.FormCreate(Sender: TObject);
var
  SurfaceTrampoline, SurfaceFriction: TGLNGDSurfaceItem;
  SurfaceCube2, SurfaceCube3, SurfaceCube4: TGLNGDSurfaceItem;
  SurfaceSphere1_Sphere2_Cube1: TGLNGDSurfaceItem;

  ObjectOnTrampoline: TGLNGDSurfacePair;
  FrictionOnCube2, FrictionOnCube3, FrictionOnCube4: TGLNGDSurfacePair;
begin
  // Get each SurfaceItem
  SurfaceTrampoline := GLNGDManager1.NewtonSurfaceItem.Items[0]
    as TGLNGDSurfaceItem;
  SurfaceFriction := GLNGDManager1.NewtonSurfaceItem.Items[1]
    as TGLNGDSurfaceItem;
  SurfaceCube2 := GLNGDManager1.NewtonSurfaceItem.Items[2] as TGLNGDSurfaceItem;
  SurfaceCube3 := GLNGDManager1.NewtonSurfaceItem.Items[3] as TGLNGDSurfaceItem;
  SurfaceCube4 := GLNGDManager1.NewtonSurfaceItem.Items[4] as TGLNGDSurfaceItem;
  SurfaceSphere1_Sphere2_Cube1 := GLNGDManager1.NewtonSurfaceItem.Items[5]
    as TGLNGDSurfaceItem;

  // Set them to Behaviours
  GetNGDStatic(Trampoline).NGDSurfaceItem := SurfaceTrampoline;
  GetNGDStatic(Friction).NGDSurfaceItem := SurfaceFriction;
  GetNGDDynamic(GLCube2).NGDSurfaceItem := SurfaceCube2;
  GetNGDDynamic(GLCube3).NGDSurfaceItem := SurfaceCube3;
  GetNGDDynamic(GLCube4).NGDSurfaceItem := SurfaceCube4;
  GetNGDDynamic(GLCube1).NGDSurfaceItem := SurfaceSphere1_Sphere2_Cube1;
  GetNGDDynamic(GLSphere1).NGDSurfaceItem := SurfaceSphere1_Sphere2_Cube1;
  GetNGDDynamic(GLSphere2).NGDSurfaceItem := SurfaceSphere1_Sphere2_Cube1;

  // Get each SurfacePair
  ObjectOnTrampoline := GLNGDManager1.NewtonSurfacePair.Items[0]
    as TGLNGDSurfacePair;
  FrictionOnCube2 := GLNGDManager1.NewtonSurfacePair.Items[1]
    as TGLNGDSurfacePair;
  FrictionOnCube3 := GLNGDManager1.NewtonSurfacePair.Items[2]
    as TGLNGDSurfacePair;
  FrictionOnCube4 := GLNGDManager1.NewtonSurfacePair.Items[3]
    as TGLNGDSurfacePair;

  // Set SurfaceItems to SurfacePair
  ObjectOnTrampoline.SetMaterialItems(SurfaceTrampoline,
    SurfaceSphere1_Sphere2_Cube1);

  FrictionOnCube2.SetMaterialItems(SurfaceFriction, SurfaceCube2);
  FrictionOnCube3.SetMaterialItems(SurfaceFriction, SurfaceCube3);
  FrictionOnCube4.SetMaterialItems(SurfaceFriction, SurfaceCube4);

end;

procedure TFormNewtonMaterial.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLNGDManager1.Step(deltaTime);
end;

end.
