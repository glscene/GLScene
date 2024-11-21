unit fCustomQuadD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Imaging.Jpeg,

  GLS.Cadencer,
  GLS.Scene,
  GLS.Objects,
  GLS.Texture,
  GLS.Behaviours,
  GLS.SceneViewer,
  GLS.GeomObjects,
  GLS.Color,

  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.RenderContextInfo;

type
  TFormCustomQuad = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    MatLib: TGLMaterialLibrary;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    Torus1: TGLTorus;
    DirectOpenGL1: TGLDirectOpenGL;
    GLLightSource1: TGLLightSource;
    GLCadencer1: TGLCadencer;
    procedure DirectOpenGL1Render(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure FormCreate(Sender: TObject);
  private
    Material: TGLLibMaterial;
  end;

var
  FormCustomQuad: TFormCustomQuad;

implementation

{$R *.DFM}

uses
  GLS.Context,
  GLS.State,
  Stage.Utils;

procedure TFormCustomQuad.FormCreate(Sender: TObject);
begin
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path  + '\texture');
  // dynamically create 2 materials and load 2 textures
  MatLib.AddTextureMaterial('wood', 'ashwood.jpg').
    Material.FrontProperties.Emission.Color := clrGray50;
  MatLib.AddTextureMaterial('wood', 'ashwood.jpg').
    Material.FaceCulling := fcNoCull;

  MatLib.AddTextureMaterial('grass', 'grass.jpg').
    Material.FrontProperties.Emission.Color := clrGray50;
  MatLib.AddTextureMaterial('grass', 'grass.jpg').
    Material.FaceCulling := fcNoCull;

  Torus1.Material.Texture.Disabled := False;
  Torus1.Material.Texture.Image.LoadFromFile('walkway.jpg');
end;

procedure TFormCustomQuad.DirectOpenGL1Render(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
	glDisable(GL_CULL_FACE);
  // 1st quad, textured with 'wood', using standard method
  MatLib.ApplyMaterial('wood', rci);
  glBegin(GL_QUADS);
    glTexCoord2f(0, 1);
    glVertex3f(0.5, 0.5, -0.5);
    glTexCoord2f(0, 0);
    glVertex3f(-0.5, 0.5, -0.5);
    glTexCoord2f(1, 0);
    glVertex3f(-0.5, 0, 0.5);
    glTexCoord2f(1, 1);
    glVertex3f(0.5, 0, 0.5);
  glEnd;
  MatLib.UnApplyMaterial(rci);
  // 2nd quad, textured with 'grass'
  // we could apply the material "manually", but this can be usefull if you want to have
  // some dynamic material control
  MatLib.ApplyMaterial('grass', rci);
  glBegin(GL_QUADS);
    glTexCoord2f(0, 1);
    glVertex3f(0.5, -0.5, -0.5);
    glTexCoord2f(0, 0);
    glVertex3f(0.5, 0, 0.5);
    glTexCoord2f(1, 0);
    glVertex3f(-0.5, 0, 0.5);
    glTexCoord2f(1, 1);
    glVertex3f(-0.5, -0.5, -0.5);
  glEnd;
  MatLib.UnApplyMaterial(rci);
 	glEnable(GL_CULL_FACE);
end;

end.
