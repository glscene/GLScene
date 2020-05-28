unit Unit1;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Imaging.Jpeg,
  Vcl.Controls,
  Vcl.Forms,

  
  GLScene, GLVectorFileObjects, GLObjects, GLWin32Viewer,
  GLFile3ds, GLCadencer, GLGeomObjects, GLVectorGeometry,
  GLShadowPlane, GLParticleFX, GLPerlinPFX, GLCrossPlatform, GLCoordinates,
  GLBaseClasses, GLUtils;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLFreeForm1: TGLFreeForm;
    GLCadencer1: TGLCadencer;
    GLCylinder1: TGLCylinder;
    GLCylinder2: TGLCylinder;
    GLShadowPlane1: TGLShadowPlane;
    GLPerlinPFXManager1: TGLPerlinPFXManager;
    GLDummyCube3: TGLDummyCube;
    GLParticleFXRenderer1: TGLParticleFXRenderer;
    GLPolygonPFXManager1: TGLPolygonPFXManager;
    GLParticleFXRenderer2: TGLParticleFXRenderer;
    procedure FormActivate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1DblClick(Sender: TObject);
  private
     
  public
     
    mx, my: Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormActivate(Sender: TObject);
begin
  SetGLSceneMediaDir;
  GLFreeForm1.LoadFromFile('beer.3ds');
  GLFreeForm1.Material.Texture.Image.LoadFromFile('clouds.jpg');
  GLShadowPlane1.Material.Texture.Image.LoadFromFile('ashwood.jpg');
  GetOrCreateSourcePFX(GLDummyCube3).Burst(0, 150);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLCamera1.MoveAroundTarget(0, 10 * deltaTime);
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;

procedure TForm1.GLSceneViewer1DblClick(Sender: TObject);
begin
  GLCadencer1.Enabled := not GLCadencer1.Enabled;
end;

end.
