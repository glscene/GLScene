//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLSL.PhongShader;

(*  An ARBvp1.0 + ARBfp1.0 shader that implements phong shading. *)

interface

{$I GLScene.inc }

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,

  GLS.Texture,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.OpenGLTokens,
  GLS.Context,
  GLSL.AsmShader,
  GLS.RenderContextInfo,
  GLSL.CustomShader,
  GLS.State;

type
  TGLPhongShader = class(TGLCustomAsmShader)
  private
    FLightIDs: TIntegerList;
    FDesignTimeEnabled: Boolean;
    FAmbientPass: Boolean;
    procedure SetDesignTimeEnabled(const Value: Boolean);
  protected
    procedure DoLightPass(lightID: Cardinal); virtual;
    procedure DoAmbientPass(var rci: TGLRenderContextInfo); virtual;
    procedure UnApplyLights(var rci: TGLRenderContextInfo); virtual;
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
    procedure DoInitialize(var rci : TGLRenderContextInfo; Sender : TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ShaderSupported: Boolean; override;
  published
    property DesignTimeEnabled: Boolean read FDesignTimeEnabled write SetDesignTimeEnabled default False;
  end;

//---------------------------------------------------------------
implementation
//---------------------------------------------------------------

procedure TGLPhongShader.DoApply(var rci: TGLRenderContextInfo; Sender: TObject);
begin
  if (csDesigning in ComponentState) and not DesignTimeEnabled then Exit;

  GetActiveLightsList(FLightIDs);
  FAmbientPass := False;

  if FLightIDs.Count > 0 then
  begin
    rci.GLStates.DepthFunc := cfLEqual;
    rci.GLStates.Disable(stBlend);
    DoLightPass(FLightIDs[0]);
    FLightIDs.Delete(0);
  end
  else
  begin
    DoAmbientPass(rci);
    FAmbientPass := True;
  end;
end;

function TGLPhongShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  Result := False;
  if (csDesigning in ComponentState) and not DesignTimeEnabled then Exit;

  if FLightIDs.Count > 0 then
  begin
    UnApplyLights(rci);
    Result := True;
    Exit;
  end
  else
  if not FAmbientPass then
  begin
    Self.UnApplyShaderPrograms();

    rci.GLStates.Enable(stBlend);
    rci.GLStates.SetBlendFunc(bfOne, bfOne);
    DoAmbientPass(rci);
    FAmbientPass := True;

    Result := True;
    Exit;
  end;
  rci.GLStates.DepthFunc := cfLEqual;
end;

procedure TGLPhongShader.DoInitialize(var rci : TGLRenderContextInfo; Sender : TObject);
begin
  if (csDesigning in ComponentState) and not DesignTimeEnabled then Exit;
  inherited;
end;

procedure TGLPhongShader.SetDesignTimeEnabled(const Value: Boolean);
begin
  if Value <> FDesignTimeEnabled then
  begin
    FDesignTimeEnabled := Value;
    NotifyChange(Self);
  end;
end;

constructor TGLPhongShader.Create(AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    Add('!!ARBvp1.0');
    Add('OPTION ARB_position_invariant;');

    Add('PARAM mvinv[4] = { state.matrix.modelview.inverse };');
    Add('PARAM mvit[4] = { state.matrix.modelview.invtrans };');
    Add('PARAM lightPos = program.local[0];');
    Add('TEMP light, normal, eye;');

    Add('   ADD eye, mvit[3], -vertex.position;');
    Add('   MOV eye.w, 0.0;');

    Add('   DP4 light.x, mvinv[0], lightPos;');
    Add('   DP4 light.y, mvinv[1], lightPos;');
    Add('   DP4 light.z, mvinv[2], lightPos;');
    Add('   ADD light, light, -vertex.position;');
    Add('   MOV light.w, 0.0;');

    Add('   MOV result.texcoord[0], vertex.normal;');
    Add('   MOV result.texcoord[1], light;');
    Add('   MOV result.texcoord[2], eye;');

    Add('END');
  end;

  with FragmentProgram.Code do
  begin
    Add('!!ARBfp1.0');

    Add('PARAM lightDiff = program.local[0];');
    Add('PARAM lightSpec = program.local[1];');
    Add('PARAM materialDiff = state.material.diffuse;');
    Add('PARAM materialSpec = state.material.specular;');
    Add('PARAM shininess = state.material.shininess;');
    Add('TEMP temp, light, normal, eye, R, diff, spec;');

    Add('   DP3 temp, fragment.texcoord[0], fragment.texcoord[0];');
    Add('   RSQ temp, temp.x;');
    Add('   MUL normal, temp.x, fragment.texcoord[0];');
    Add('   DP3 temp, fragment.texcoord[1], fragment.texcoord[1];');
    Add('   RSQ temp, temp.x;');
    Add('   MUL light, temp.x, fragment.texcoord[1];');
    Add('   DP3 temp, fragment.texcoord[2], fragment.texcoord[2];');
    Add('   RSQ temp, temp.x;');
    Add('   MUL eye, temp.x, fragment.texcoord[2];');

    Add('   DP3_SAT diff, normal, light;');
    Add('   MUL diff, diff, lightDiff;');
    Add('   MUL diff, diff, materialDiff;');

    Add('   DP3 R, normal, light;');
    Add('   MUL R, R.x, normal;');
    Add('   MUL R, 2.0, R;');
    Add('   ADD R, R, -light;');

    Add('   DP3_SAT spec, R, eye;');
    Add('   POW spec, spec.x, shininess.x;');
    Add('   MUL spec, spec, lightDiff;');
    Add('   MUL spec, spec, materialDiff;');

    Add('   ADD_SAT result.color, diff, spec;');
    Add('   MOV result.color.w, 1.0;');

    Add('END');
  end;
  FLightIDs := TIntegerList.Create;
end;

function TGLPhongShader.ShaderSupported: Boolean;
var
  MaxTextures: Integer;
begin
  Result := inherited ShaderSupported and GL.ARB_multitexture;

  gl.GetIntegerv(GL_MAX_TEXTURE_UNITS_ARB, @MaxTextures);
  Result := Result and (maxTextures > 2);
end;

procedure TGLPhongShader.UnApplyLights(var rci: TGLRenderContextInfo);
begin
  rci.GLStates.DepthFunc := cfLEqual;
  rci.GLStates.Enable(stBlend);
  rci.GLStates.SetBlendFunc(bfOne, bfOne);
  DoLightPass(FLightIDs[0]);
  FLightIDs.Delete(0);
end;

destructor TGLPhongShader.Destroy;
begin
  FLightIDs.Free;
  inherited;
end;

procedure TGLPhongShader.DoAmbientPass(var rci: TGLRenderContextInfo);
var
  ambient, materialAmbient: TGLVector;
begin
  rci.GLStates.Disable(stLighting);

  gl.GetFloatv(GL_LIGHT_MODEL_AMBIENT, @ambient);
  gl.GetMaterialfv(GL_FRONT, GL_AMBIENT, @materialAmbient);
  ScaleVector(ambient, materialAmbient);
  gl.Color3fv(@ambient);
end;

procedure TGLPhongShader.DoLightPass(lightID: Cardinal);
var
  LightParam: TGLVector;
begin
  Self.ApplyShaderPrograms();

  with CurrentGLContext.GLStates do
  begin
    gl.GetLightfv(GL_LIGHT0+lightID, GL_POSITION, @LightParam);
    LightParam := LightParam;
    gl.ProgramLocalParameter4fv(GL_VERTEX_PROGRAM_NV, 0, @LightParam);
    LightParam := LightDiffuse[lightID];
    gl.ProgramLocalParameter4fv(GL_FRAGMENT_PROGRAM_ARB, 0, @LightParam);
    LightParam := LightSpecular[lightID];
    gl.ProgramLocalParameter4fv(GL_FRAGMENT_PROGRAM_ARB, 1, @LightParam);
  end;
end;

initialization
  RegisterClasses([TGLPhongShader]);

end.
