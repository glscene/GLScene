unit fImpostersD;

interface

uses
  Winapi.OpenGL,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,

  GLS.Scene,
  GLS.Objects,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.XCollection,
  GLS.VectorFileObjects,
  GLS.RenderContextInfo,
  Stage.Utils,
  GLS.FileDDS,
  GLS.Context,
  Stage.VectorGeometry,
  GLS.AsyncTimer,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.Material,
  GLS.CompositeImage,
  GLS.VectorLists,
  Stage.VectorTypes, GLS.SimpleNavigation;

type

  TFormImposters = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    Ñadencer: TGLCadencer;
    Camera: TGLCamera;
    dcWorld: TGLDummyCube;
    dcCamera: TGLDummyCube;
    dirOGL: TGLDirectOpenGL;
    ff: TGLFreeForm;
    GLAsyncTimer1: TGLAsyncTimer;
    Timer1: TTimer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure FormCreate(Sender: TObject);
    procedure ÑadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure dirOGLRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure GLAsyncTimer1Timer(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  public
    procedure GenFreeForm;
  end;

var
  FormImposters: TFormImposters;

  tcount: integer = 2000;
  glsl: TGLProgramHandle;
  InitDGL: boolean;

implementation

{$R *.dfm}

function GetNewPos: TGLVector;
begin
  SetVector(Result, Random * 20 - 10, 0, Random * 20 - 10);
end;

procedure TFormImposters.GenFreeForm;
var
  i: integer;
  MObj: TGLMeshObject;
  FG: TFGVertexIndexList;
  v: TGLVector;
  c, a: single;

begin
  ff.MeshObjects.Clear;

  MObj := TGLMeshObject.CreateOwned(ff.MeshObjects);
  MObj.Mode := momFaceGroups;
  FG := TFGVertexIndexList.CreateOwned(MObj.FaceGroups);
  FG.Mode := fgmmQuads;

  Randomize;

  for i := 0 to tcount - 1 do
  begin
    v := GetNewPos;

    with MObj.Vertices do
    begin
      Add(v);
      Add(v);
      Add(v);
      Add(v);
    end;

    c := 0.18; // +random*0.06;
    a := (random - 0.5) * 6.284;
    with MObj.Normals do
    begin
      Add(-c, -2 * c, a);
      Add(-c, 2 * c, a);
      Add(c, 2 * c, a);
      Add(c, -2 * c, a);
    end;

    with MObj.TexCoords do
    begin
      Add(0, 0);
      Add(0, 1);
      Add(1, 1);
      Add(1, 0);
    end;
    FG.Add(i * 4);
    FG.Add(i * 4 + 1);
    FG.Add(i * 4 + 2);
    FG.Add(i * 4 + 3);

  end;
  ff.StructureChanged;
end;

procedure TFormImposters.FormCreate(Sender: TObject);
begin
  GenFreeForm;
  ff.Material.Texture.Image.LoadFromFile('halo.dds');
end;

procedure TFormImposters.ÑadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  Camera.MoveAroundTarget(0, deltaTime * 5);
end;

procedure TFormImposters.GLAsyncTimer1Timer(Sender: TObject);
begin
  caption := 'Imposters FF › ' + vp.FramesPerSecondText(2);
  vp.ResetPerformanceMonitor;
end;

procedure TFormImposters.Timer1Timer(Sender: TObject);
begin
//  caption := 'FFImposters › ' + vp.FramesPerSecondText(2);
//  vp.ResetPerformanceMonitor;
end;

procedure TFormImposters.dirOGLRender(Sender: TObject; var rci: TGLRenderContextInfo);
begin
  // init shader
  if not InitDGL then
  begin
    glsl := TGLProgramHandle.CreateAndAllocate;
    glsl.AddShader(TGLVertexShaderHandle, LoadAnsiStringFromFile('vp.glsl'));
    glsl.AddShader(TGLFragmentShaderHandle, LoadAnsiStringFromFile('fp.glsl'));
    if not glsl.LinkProgram then
      raise Exception.Create(glsl.InfoLog);
    if not glsl.ValidateProgram then
      raise Exception.Create(glsl.InfoLog);
    InitDGL := True;
  end;

  // update
  if InitDGL then
  begin
    glsl.UseProgramObject;
    glsl.Uniform1i['BaseTex'] := 0;
    glsl.Uniform4f['camera'] := Camera.AbsolutePosition;
    ff.Render(rci);
    glsl.EndUseProgramObject;
  end;
end;

end.
