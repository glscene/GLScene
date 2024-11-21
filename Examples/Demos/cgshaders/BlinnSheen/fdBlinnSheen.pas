unit fdBlinnSheen;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Imaging.Jpeg,

  GLS.Scene,
  GLS.Objects,
  GLS.Cadencer,
  Stage.VectorTypes,
  GLS.Texture,
  GLS.SceneViewer,
  GLS.VectorFileObjects,
  GLS.AsyncTimer,
  Stage.VectorGeometry,
  GLS.Material,
  GLS.Coordinates,

  GLS.BaseClasses,
  GLS.Behaviours,
  GLS.FileMD2,
  GLS.FileTGA,
  GLS.File3DS,
  GLS.PersistentClasses,
  Stage.Utils,

  Cg.GL,
  GLS.CgShader;

type
  TFormBlinnSheen = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    CgBumpShader: TCgShader;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    AsyncTimer1: TGLAsyncTimer;
    GLFreeForm1: TGLFreeForm;
    CheckBox1: TCheckBox;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure CgBumpShaderApplyVP(CgProgram: TCgProgram; Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CgBumpShaderInitialize(CgShader: TCustomCgShader);
    procedure CgBumpShaderApplyFP(CgProgram: TCgProgram; Sender: TObject);
    procedure CgBumpShaderUnApplyFP(CgProgram: TCgProgram);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure CheckBox1Click(Sender: TObject);
  private
  public
    mx, my: Integer;
  end;

var
  FormBlinnSheen: TFormBlinnSheen;

implementation //-------------------------------------------------------------

{$R *.dfm}

procedure TFormBlinnSheen.FormCreate(Sender: TObject);
begin
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path + '\shader');

  // Load the vertex and fragment Cg programs
  CgBumpShader.VertexProgram.LoadFromFile('NormalMapp_vp.cg');
  CgBumpShader.FragmentProgram.LoadFromFile('NormalMapp_fp.cg');

  SetCurrentDir(Path + '\model');
  GLFreeForm1.LoadFromFile('Head256.3ds');


  // Load the texture
  SetCurrentDir(Path + '\texture');
  for var i: Integer := 0 to GLFreeForm1.MeshObjects.Count - 1 do
  begin
    GLFreeForm1.MeshObjects[i].BuildTangentSpace;
    GLFreeForm1.MeshObjects[i].TangentsTexCoordIndex := 1;
    GLFreeForm1.MeshObjects[i].BinormalsTexCoordIndex := 2;
  end;

  GLMaterialLibrary1.Materials[0].Material.TextureEx.Add;
  GLMaterialLibrary1.Materials[0].Material.TextureEx.Add;
  GLMaterialLibrary1.Materials[0].Material.TextureEx.Add;

  GLMaterialLibrary1.Materials[0].Material.TextureEx[0]
    .Texture.Disabled := False;
  GLMaterialLibrary1.Materials[0].Material.TextureEx[0].Texture.TextureMode :=
    tmModulate;
  GLMaterialLibrary1.Materials[0].Material.TextureEx[0]
    .Texture.Image.LoadFromFile('Head256.tga');
  GLMaterialLibrary1.Materials[0].Material.TextureEx[1]
    .Texture.Disabled := False;
  GLMaterialLibrary1.Materials[0].Material.TextureEx[1].Texture.TextureMode :=
    tmModulate;
  GLMaterialLibrary1.Materials[0].Material.TextureEx[1]
    .Texture.Image.LoadFromFile('HeadN256.tga');
  GLMaterialLibrary1.Materials[0].Material.TextureEx[2]
    .Texture.Disabled := False;
  GLMaterialLibrary1.Materials[0].Material.TextureEx[2].Texture.TextureMode :=
    tmModulate;
  GLMaterialLibrary1.Materials[0].Material.TextureEx[2]
    .Texture.Image.LoadFromFile('HeadS256.tga');
end;

procedure TFormBlinnSheen.CgBumpShaderApplyVP(CgProgram: TCgProgram; Sender: TObject);
var
  ap: array [0 .. 2] of single;
begin
  // Apply the per frame uniform parameters
  ap[0] := GLLightSource1.AbsolutePosition.X;
  ap[1] := GLLightSource1.AbsolutePosition.Y;
  ap[2] := GLLightSource1.AbsolutePosition.Z;
  CgProgram.ParamByName('modelViewProj').SetAsStateMatrix
    (CG_GL_MODELVIEW_PROJECTION_MATRIX, CG_GL_MATRIX_IDENTITY);
  CgProgram.ParamByName('modelView').SetAsStateMatrix(CG_GL_MODELVIEW_MATRIX,
    CG_GL_MATRIX_IDENTITY);
  // ParamByName('vLightPosition').SetAsVector(ap);
  ap[0] := GLCamera1.AbsolutePosition.X;
  ap[1] := GLCamera1.AbsolutePosition.Y;
  ap[2] := GLCamera1.AbsolutePosition.Z;
  // ParamByName('vEyePosition').SetAsVector(ap);

  CgProgram.ParamByName('bumpScale').SetAsScalar(1);
  // ParamByName('ModelViewIT').SetAsStateMatrix( CG_GL_MODELVIEW_MATRIX, CG_GL_MATRIX_INVERSE_TRANSPOSE);
end;

procedure TFormBlinnSheen.CgBumpShaderInitialize(CgShader: TCustomCgShader);
var
  am: array [0 .. 2] of single;
begin
  // Set up the LightDiffuseColor parameter
  am[0] := GLLightSource1.Diffuse.Red;
  am[1] := GLLightSource1.Diffuse.Green;
  am[2] := GLLightSource1.Diffuse.Blue;
  // CgBumpShader.FragmentProgram.ParamByName('LightDiffuseColor').SetAsVector(am);

end;

procedure TFormBlinnSheen.CgBumpShaderApplyFP(CgProgram: TCgProgram; Sender: TObject);
var
  am: array [0 .. 2] of single;
begin
  // Set up the LightDiffuseColor parameter
  // am[0]:=GLLightSource1.Diffuse.Red;
  // am[1]:=GLLightSource1.Diffuse.Green;
  // am[2]:=GLLightSource1.Diffuse.Blue;
  // CgBumpShader.FragmentProgram.ParamByName('LightDiffuseColor').SetAsVector(am);
  // Enable the LightDiffuseColor for use in the fragment
  // program
  // CgProgram.ParamByName('LightDiffuseColor').EnableClientState;
end;

procedure TFormBlinnSheen.CgBumpShaderUnApplyFP(CgProgram: TCgProgram);
begin
  // Disable the LightDiffuseColor
  // CgProgram.ParamByName('LightDiffuseColor').DisableClientState;
end;

procedure TFormBlinnSheen.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormBlinnSheen.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;

procedure TFormBlinnSheen.AsyncTimer1Timer(Sender: TObject);
begin
  FormBlinnSheen.Caption := Format('Cg Normal/Bump Blinn Shading Demo - %.2f FPS',
    [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormBlinnSheen.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TFormBlinnSheen.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  am: array [0 .. 2] of single;
begin
  // Set up the texture sampler parameter
  (* am[0]:=GLLightSource1.Diffuse.Red;
    am[1]:=GLLightSource1.Diffuse.Green;
    am[2]:=GLLightSource1.Diffuse.Blue;
    CgBumpShader.FragmentProgram.ParamByName('LightDiffuseColor').SetAsVector(am);
    CgBumpShader.VertexProgram.ParamByName('modelViewProjMatrix').SetAsStateMatrix( CG_GL_MODELVIEW_PROJECTION_MATRIX, CG_GL_MATRIX_IDENTITY);
    CgBumpShader.VertexProgram.ParamByName('vLightPosition').SetAsVector(GLLightSource1.Position.AsAffineVector);
   *)
end;

procedure TFormBlinnSheen.CheckBox1Click(Sender: TObject);
begin
  If CheckBox1.Checked then
  begin
    CgBumpShader.Enabled := True;
    GLFreeForm1.Material.LibMaterialName :=
      GLMaterialLibrary1.Materials[0].Name;
  end
  Else
  begin
    CgBumpShader.Enabled := False;
    GLFreeForm1.Material.LibMaterialName := '';
  end;

end;

end.
