unit Unit1;

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

  GLScene,
  GLObjects,
  GLWin32Viewer,
  GLTexture,
  GLContext,
  GLGeomObjects,
  GLState,
  GLColor,
  GLMaterial,
  GLCoordinates,
  GLCrossPlatform,
  GLRenderContextInfo,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Torus1: TGLTorus;
    BUBind: TButton;
    Sphere1: TGLSphere;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLAnnulus1: TGLAnnulus;
    GLAnnulus2: TGLAnnulus;
    GLCube1: TGLCube;
    GLSphere1: TGLSphere;
    procedure BUBindClick(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
     
  public
     
    mx, my: Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type

  THiddenLineShader = class(TGLShader)
  private
    BackgroundColor, LineColor: TColorVector;
    PassCount: Integer;
  public
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
  end;

  TOutLineShader = class(TGLShader)
  private
    BackgroundColor, LineColor: TColorVector;
    OutlineSmooth, Lighting: Boolean;
    OutlineWidth, Oldlinewidth: Single;
    PassCount: Integer;
  public
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
  end;

procedure THiddenLineShader.DoApply(var rci: TGLRenderContextInfo; Sender:
  TObject);
begin
  // new object getting rendered, 1st pass
  PassCount := 1;
  with rci.GLStates do
  begin
    // disable lighting, this is a solid fill
    Disable(stLighting);
    PolygonMode := pmFill;
    // use background color
    gl.Color3fv(@BackgroundColor);
    // enable and adjust polygon offset
    Enable(stPolygonOffsetFill);
    SetPolygonOffset(1, 2);
  end;
end;

function THiddenLineShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  case PassCount of
    1:
      with rci.GLStates do
      begin
        // 1st pass completed, we setup for the second
        PassCount := 2;

        // switch to wireframe and its color
        PolygonMode := pmLines;
        Disable(stPolygonOffsetLine);
        Disable(stLineStipple);
        Disable(stLineSmooth);
        LineWidth := 1;
        gl.Color3fv(@LineColor);
        Result := True;
      end;
    2:
      with rci.GLStates do
      begin
        // restore state
        PolygonMode := pmFill;
        Disable(stPolygonOffsetFill);
        // we're done
        Result := False;
      end;
  else
    // doesn't hurt to be cautious
    Assert(False);
    Result := False;
  end;
end;

procedure TOutLineShader.DoApply(var rci: TGLRenderContextInfo; Sender: TObject);
begin
  PassCount := 1;
  with rci.GLStates do
  begin
    Disable(stLighting);

    if outlineSmooth then
    begin
      Enable(stBlend);
      SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      LineSmoothHint := hintNicest;
      Enable(stLineSmooth);
    end
    else
      Disable(stLineSmooth);

    LineStippleFactor := 1;
    LineStipplePattern := $FFFF;
    LineWidth := OutLineWidth;
    PolygonMode := pmLines;
    CullFaceMode := cmFront;
    DepthFunc := cfLEqual;
    gl.Color3fv(@lineColor);
  end;
end;

function TOutLineShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  case PassCount of
    1:
      with rci.GLStates do
      begin
        PassCount := 2;
        if lighting then
          Enable(stLighting)
        else
          gl.Color3fv(@backGroundColor);
        DepthFunc := cfLess;
        PolygonMode := pmFill;
        CullFaceMode := cmBack;
        Result := True;
      end;
    2:
      begin
        Result := False;
      end;
  else
    Assert(False);
    Result := False;
  end;
end;

procedure TForm1.BUBindClick(Sender: TObject);
var
  shader1: THiddenLineShader;
  shader2, shader3: TOutLineShader;

begin
  BUBind.Enabled := False;

  // instantiate our shaders

  shader1 := THiddenLineShader.Create(Self);
  shader1.BackgroundColor :=
    ConvertWinColor(GLSceneViewer1.Buffer.BackgroundColor);
  shader1.LineColor := clrBlue;

  shader2 := TOutLineShader.Create(Self);
  with shader2 do
  begin
    BackgroundColor := ConvertWinColor(GLSceneViewer1.Buffer.BackgroundColor);
    Outlinesmooth := true;
    OutLineWidth := 2;
    Lighting := false;
    LineColor := clrBlack;
  end;

  shader3 := TOutLineShader.Create(Self);
  with shader3 do
  begin
    BackgroundColor := ConvertWinColor(GLSceneViewer1.Buffer.BackgroundColor);
    Outlinesmooth := false;
    OutLineWidth := 4;
    Lighting := true;
    LineColor := clrRed;
  end;

  // binds the shaders to the materials
  GLMaterialLibrary1.Materials[0].Shader := shader1;
  GLMaterialLibrary1.Materials[1].Shader := shader2;
  GLMaterialLibrary1.Materials[2].Shader := shader3;

end;

//
// Classic mouse movement bits
//

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := x;
  my := y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Shift = [ssLeft] then
    GLCamera1.MoveAroundTarget(my - y, mx - x)
  else if Shift = [ssRight] then
    GLCamera1.RotateTarget(my - y, mx - x);
  mx := x;
  my := y;
end;

end.

