unit fDisttexD;

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,

  Stage.TextureFormat,
  GLS.Cadencer,
  GLS.Texture,
  GLS.SceneViewer,
  GLS.Scene,
  GLS.Objects,
  GLS.Context,
  GLS.HUDObjects,
  GLS.CgShader,
  GLS.Material,
  GLS.Coordinates,

  GLS.BaseClasses,
  GLS.RenderContextInfo,
  Stage.Utils,

  CG.Import,
  CG.GL;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    viewer: TGLSceneViewer;
    matLib: TGLMaterialLibrary;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDirectOpenGL1: TGLDirectOpenGL;
    GLCadencer1: TGLCadencer;
    GLCube1: TGLCube;
    filterShader: TCgShader;
    GLPlane1: TGLPlane;
    backGround: TGLPlane;
    procedure FormCreate(Sender: TObject);
    procedure GLDirectOpenGL1Render(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
    procedure filterShaderApplyFP(CgProgram: TCgProgram; Sender: TObject);
    procedure filterShaderApplyVP(CgProgram: TCgProgram; Sender: TObject);
    procedure viewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
  end;

var
  Form1: TForm1;
  refract: TGLTextureHandle;

  // ------------------------------------------
implementation

// ------------------------------------------

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path + '\shader');

  // Load the shader
  filterShader.VertexProgram.LoadFromFile('filterV.c');
  filterShader.FragmentProgram.LoadFromFile('filterF.c');

  // Load the texture for the background plane
  SetCurrentDir(Path + '\texture');
  matLib.Materials[1].Material.Texture.Image.LoadFromFile('parquet.bmp');
end;

(*
  Initialize refract texture used for the screen filter. Remember
  this is not an ordinary texture used with GL_TEXTURE_2D, we won't
  its dimensions to be NPOT (Non Power Of Two)
*)
procedure initialize;
begin
  refract := TGLTextureHandle.Create;
  // Create the refract texture
  glEnable(GL_TEXTURE_RECTANGLE_NV);
  refract.AllocateHandle;

  glBindTexture(GL_TEXTURE_RECTANGLE_NV, refract.Handle);
  glTexParameteri(GL_TEXTURE_RECTANGLE_NV, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_RECTANGLE_NV, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_RECTANGLE_NV, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_RECTANGLE_NV, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  (* Important note, if you resize the screen, the size of this texture
    must be modified this case so you need to recreate the texture, otherwise
    it crashes. I was too lazy for that so that's why you can't resize
    the form *)
  glCopyTexImage2d(GL_TEXTURE_RECTANGLE_NV, 0, GL_RGBA8, 0, 0, Form1.viewer.ClientWidth,
    Form1.viewer.ClientHeight, 0);
end; // initialize

var
  initialized: boolean = false;

procedure TForm1.GLDirectOpenGL1Render(Sender: TObject; var rci: TGLRenderContextInfo);
begin
  if not initialized then
  begin
    (* Initializing OpenGL related stuff like textures must be done
      in a OpenGL context. Doing this in the formCreate for example
      won't work. This is not the nicest solution but hey, it's only
      a demo. *)
    initialize;
    initialized := True;
  end;

  // Take a snapshot for the refract texture
  glBindTexture(GL_TEXTURE_RECTANGLE_NV, refract.Handle);
  glCopyTexSubImage2D(GL_TEXTURE_RECTANGLE_NV, 0, 0, 0, 0, 0, viewer.ClientWidth,
    viewer.ClientHeight);
  glDisable(GL_TEXTURE_RECTANGLE_NV);
end; // Render

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
begin
  GLCube1.PitchAngle := GLCube1.PitchAngle + 0.3;
  GLCube1.TurnAngle := GLCube1.TurnAngle + 0.5;
end; // Cadencer progress

// ****************************************************************************//
// ************************* Shader stuff *************************************//
// ****************************************************************************//
var
  cursorX, cursorY: single;

procedure TForm1.filterShaderApplyFP(CgProgram: TCgProgram; Sender: TObject);
begin
  // Pass parameter values to fragment program
  with CgProgram do
  begin
    paramByName('screenW').SetAsScalar(viewer.ClientWidth);
    paramByName('screenH').SetAsScalar(viewer.ClientHeight);
    paramByName('refractTex').SetAsTextureRECT(refract.Handle);
    paramByName('refractTex').EnableTexture;
    // We do a little trick with the cursor position so pass its coordinates
    paramByName('cursorX').SetAsScalar(cursorX);
    paramByName('cursorY').SetAsScalar(cursorY);
  end;
end; // apply Fragment program

procedure TForm1.filterShaderApplyVP(CgProgram: TCgProgram; Sender: TObject);
begin
  with CgProgram do
  begin
    paramByName('MVP').SetAsStateMatrix(CG_GL_MODELVIEW_PROJECTION_MATRIX, CG_GL_MATRIX_IDENTITY);
  end;
end; // apply Vertex program

procedure TForm1.viewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  cursorX := X / viewer.ClientWidth;
  cursorY := 1 - (Y / viewer.ClientHeight);
end;

end.
