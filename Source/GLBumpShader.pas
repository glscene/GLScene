//
// This unit is part of the GLScene Engine, http://glscene.org
//

unit GLBumpShader;

(*
   A shader that applies bump mapping.
   Notes:
   The normal map is expected to be the primary texture.

   The secondary texture is used for the diffuse texture,
   to enable set boDiffuseTexture2 in the BumpOptions property.

   The tertiary texture is used for the specular texture,
   to enable set boSpecularTexture3 in the BumpOptions property.
   The SpecularMode determines the specular highlight calculation
   (Blinn or Phong), smOff disables specular highlights in the
   shader.

   External tangent bump space expects tangent data under
   GL_TEXTURE1_ARB and binormal data under GL_TEXTURE2_ARB.

   The boUseSecondaryTexCoords bump option tells the shader to use
   the secondary texture coordinates for the diffuse and specular
   texture lookups.

*)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,

  OpenGLTokens,
  GLVectorTypes,
  GLVectorGeometry,
  GLMaterial,
  GLGraphics,
  GLUtils,
  GLContext,
  GLVectorLists,
  GLColor,
  GLRenderContextInfo,
  GLState,
  GLTextureFormat;

type
  TBumpMethod = (bmDot3TexCombiner, bmBasicARBFP);

  TBumpSpace = (bsObject, bsTangentExternal, bsTangentQuaternion);

  TBumpOption = (boDiffuseTexture2, // Use secondary texture as diffuse
    boSpecularTexture3, // Use tertiary texture as specular
    boUseSecondaryTexCoords, // Pass through secondary texcoords
    boLightAttenuation, // Use light attenuation
    boParallaxMapping // Enable parallax offset mapping
    );
  TBumpOptions = set of TBumpOption;

  TSpecularMode = (smOff, smBlinn, smPhong);

  // A generic bump shader.
  TGLBumpShader = class(TGLShader)
  private
    FVertexProgramHandle: TGLARBVertexProgramHandle;
    FFragmentProgramHandle: TGLARBFragmentProgramHandle;
    FLightIDs: TIntegerList;
    FLightsEnabled: Integer;
    FBumpMethod: TBumpMethod;
    FBumpSpace: TBumpSpace;
    FBumpOptions: TBumpOptions;
    FSpecularMode: TSpecularMode;
    FDesignTimeEnabled: Boolean;
    FAmbientPass: Boolean;
    FDiffusePass: Boolean;
    FVertexProgram: TStringList;
    FFragmentProgram: TStringList;
    FParallaxOffset: Single;
    function GenerateVertexProgram: string;
    function GenerateFragmentProgram: string;
    procedure DoLightPass(var rci: TGLRenderContextInfo; lightID: Cardinal);
  protected
    procedure SetBumpMethod(const Value: TBumpMethod);
    procedure SetBumpSpace(const Value: TBumpSpace);
    procedure SetBumpOptions(const Value: TBumpOptions);
    procedure SetSpecularMode(const Value: TSpecularMode);
    procedure SetDesignTimeEnabled(const Value: Boolean);
    procedure SetParallaxOffset(const Value: Single);
    procedure Loaded; override;
    procedure DeleteVertexPrograms;
    procedure DeleteFragmentPrograms;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
  published
    property BumpMethod: TBumpMethod read FBumpMethod write SetBumpMethod;
    property BumpSpace: TBumpSpace read FBumpSpace write SetBumpSpace;
    property BumpOptions: TBumpOptions read FBumpOptions write SetBumpOptions;
    property SpecularMode: TSpecularMode read FSpecularMode write
      SetSpecularMode;
    property DesignTimeEnabled: Boolean read FDesignTimeEnabled write
      SetDesignTimeEnabled;
    property ParallaxOffset: Single read FParallaxOffset write
      SetParallaxOffset;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLBumpShader ------------------
// ------------------

constructor TGLBumpShader.Create(AOwner: TComponent);
begin
  inherited;
  FLightIDs := TIntegerList.Create;
  FBumpMethod := bmDot3TexCombiner;
  FBumpSpace := bsObject;
  FBumpOptions := [];
  FSpecularMode := smOff;
  ShaderStyle := ssLowLevel;
  FParallaxOffset := 0.04;

  FVertexProgram := TStringList.Create;
  FFragmentProgram := TStringList.Create;
end;


destructor TGLBumpShader.Destroy;
begin
  DeleteVertexPrograms;
  DeleteFragmentPrograms;
  FLightIDs.Free;
  FVertexProgram.Free;
  FFragmentProgram.Free;
  inherited;
end;

procedure TGLBumpShader.Loaded;
begin
  inherited;
end;

function TGLBumpShader.GenerateVertexProgram: string;
var
  VP: TStringList;
  DoTangent, DoSpecular, DoParallaxOffset: Boolean;
  texcoord: Integer;
begin
  DoSpecular := (BumpMethod = bmBasicARBFP) and not (SpecularMode = smOff);
  DoTangent := (BumpSpace = bsTangentExternal) or (BumpSpace =
    bsTangentQuaternion);
  DoParallaxOffset := (BumpMethod = bmBasicARBFP) and (boParallaxMapping in
    BumpOptions) and DoTangent;

  VP := TStringList.Create;

  VP.Add('!!ARBvp1.0');
  VP.Add('OPTION ARB_position_invariant;');

  VP.Add('PARAM mv[4] = { state.matrix.modelview };');
  VP.Add('PARAM mvinv[4] = { state.matrix.modelview.inverse };');
  VP.Add('PARAM mvit[4] = { state.matrix.modelview.invtrans };');
  VP.Add('PARAM tex[4] = { state.matrix.texture[0] };');
  if boUseSecondaryTexCoords in BumpOptions then
    VP.Add('PARAM tex2[4] = { state.matrix.texture[1] };');
  VP.Add('PARAM lightPos = program.local[0];');
  VP.Add('PARAM lightAtten = program.local[1];');
  if BumpSpace = bsTangentExternal then
  begin
    VP.Add('ATTRIB tangent = vertex.texcoord[1];');
    VP.Add('ATTRIB binormal = vertex.texcoord[2];');
    VP.Add('ATTRIB normal = vertex.normal;');
  end;
  VP.Add('TEMP temp, temp2, light, eye, atten;');

  if (boLightAttenuation in BumpOptions) then
  begin

    VP.Add('   DP4 temp.x, mv[0], vertex.position;');
    VP.Add('   DP4 temp.y, mv[1], vertex.position;');
    VP.Add('   DP4 temp.z, mv[2], vertex.position;');
    VP.Add('   ADD light, lightPos, -temp;');

    VP.Add('   DP3 atten.y, light, light;');
    VP.Add('   RSQ atten.y, atten.y;');
    if BumpMethod = bmDot3TexCombiner then
    begin
      VP.Add('   RCP atten.y, atten.y;');
      VP.Add('   MUL atten.z, atten.y, atten.y;');
      VP.Add('   MAD atten.x, lightAtten.y, atten.y, lightAtten.x;');
      VP.Add('   MAD atten.x, lightAtten.z, atten.z, atten.x;');
      VP.Add('   RCP atten.x, atten.x;');
    end
    else if BumpMethod = bmBasicARBFP then
    begin
      // Store the distance in atten.x for ARBFP,
      // fragment program will calculate attenutation
      VP.Add('   RCP atten.x, atten.y;');
    end;

    VP.Add('   DP3 temp.x, mvinv[0], light;');
    VP.Add('   DP3 temp.y, mvinv[1], light;');
    VP.Add('   DP3 temp.z, mvinv[2], light;');
    VP.Add('   MOV light, temp;');
  end
  else
  begin
    VP.Add('   DP4 light.x, mvinv[0], lightPos;');
    VP.Add('   DP4 light.y, mvinv[1], lightPos;');
    VP.Add('   DP4 light.z, mvinv[2], lightPos;');
    VP.Add('   ADD light, light, -vertex.position;');
  end;

  if DoSpecular or DoParallaxOffset then
    VP.Add('   ADD eye, mvit[3], -vertex.position;');

  if DoTangent then
  begin
    if BumpSpace = bsTangentExternal then
    begin

      VP.Add('   DP3 temp.x, light, tangent;');
      VP.Add('   DP3 temp.y, light, binormal;');
      VP.Add('   DP3 temp.z, light, normal;');
      VP.Add('   MOV light, temp;');
      if DoSpecular or DoParallaxOffset then
      begin
        VP.Add('   DP3 temp.x, eye, tangent;');
        VP.Add('   DP3 temp.y, eye, binormal;');
        VP.Add('   DP3 temp.z, eye, normal;');
        VP.Add('   MOV eye, temp;');
      end;

    end
    else if BumpSpace = bsTangentQuaternion then
    begin

      VP.Add('   DP3 temp.x, light, light;');
      VP.Add('   RSQ temp.x, temp.x;');
      VP.Add('   MUL light, temp.x, light;');

      VP.Add('   MOV temp2.x, vertex.normal.y;');
      VP.Add('   ADD temp2.y, 0.0, -vertex.normal.x;');
      VP.Add('   MOV temp2.z, 0.0;');

      VP.Add('   DP3 temp.x, temp2, light;');
      VP.Add('   MUL temp.x, temp2.y, light.z;');
      VP.Add('   MAD temp.y, vertex.normal.z, light.x, temp.x;');
      VP.Add('   MUL temp.x, vertex.normal.y, light.z;');
      VP.Add('   MAD temp.z, vertex.normal.z, light.y, -temp.x;');
      VP.Add('   MUL temp.x, vertex.normal.y, light.y;');
      VP.Add('   MAD temp.x, vertex.normal.z, light.z, temp.x;');
      VP.Add('   MAD temp.w, -temp2.y, light.x, temp.x;');
      VP.Add('   MOV light, temp.yzwy;');

      if DoSpecular or DoParallaxOffset then
      begin
        VP.Add('   DP3 temp.x, temp2, eye;');
        VP.Add('   MUL temp.x, temp2.y, eye.z;');
        VP.Add('   MAD temp.y, vertex.normal.z, eye.x, temp.x;');
        VP.Add('   MUL temp.x, vertex.normal.y, eye.z;');
        VP.Add('   MAD temp.z, vertex.normal.z, eye.y, -temp.x;');
        VP.Add('   MUL temp.x, vertex.normal.y, eye.y;');
        VP.Add('   MAD temp.x, vertex.normal.z, eye.z, temp.x;');
        VP.Add('   MAD temp.w, -temp2.y, eye.x, temp.x;');
        VP.Add('   MOV eye, temp.yzwy;');
      end;

    end;
  end;

  if BumpMethod = bmDot3TexCombiner then
  begin

    if BumpSpace <> bsTangentQuaternion then
    begin
      VP.Add('   DP3 temp.x, light, light;');
      VP.Add('   RSQ temp, temp.x;');
      VP.Add('   MUL light, temp.x, light;');
    end;

    if boLightAttenuation in BumpOptions then
      VP.Add('   MUL light, atten.x, light;');

    VP.Add('   MAD result.color, light, 0.5, 0.5;');
    VP.Add('   MOV result.color.w, 1.0;');

  end
  else if BumpMethod = bmBasicARBFP then
  begin

    if boLightAttenuation in BumpOptions then
      VP.Add('   MOV light.w, atten.x;')
    else
      VP.Add('   MOV light.w, 0.0;');
    if DoSpecular or DoParallaxOffset then
      VP.Add('   MOV eye.w, 0.0;');

  end;

  texcoord := 0;

  VP.Add('   DP4 temp.x, vertex.texcoord[0], tex[0];');
  VP.Add('   DP4 temp.y, vertex.texcoord[0], tex[1];');
  VP.Add('   DP4 temp.z, vertex.texcoord[0], tex[2];');
  VP.Add('   DP4 temp.w, vertex.texcoord[0], tex[3];');
  VP.Add('   MOV result.texcoord[' + IntToStr(texcoord) + '], temp;');
  Inc(texcoord);

  if boUseSecondaryTexCoords in BumpOptions then
  begin
    VP.Add('   DP4 temp.x, vertex.texcoord[1], tex2[0];');
    VP.Add('   DP4 temp.y, vertex.texcoord[1], tex2[1];');
    VP.Add('   DP4 temp.z, vertex.texcoord[1], tex2[2];');
    VP.Add('   DP4 temp.w, vertex.texcoord[1], tex2[3];');
    VP.Add('   MOV result.texcoord[' + IntToStr(texcoord) + '], temp;');
    Inc(texcoord);
  end;

  if BumpMethod = bmDot3TexCombiner then
  begin
    if (boDiffuseTexture2 in BumpOptions)
      and not (boUseSecondaryTexCoords in BumpOptions) then
      VP.Add('   MOV result.texcoord[' + IntToStr(texcoord) + '], temp;');
  end
  else
  begin
    VP.Add('   MOV result.texcoord[' + IntToStr(texcoord) + '], light;');
    Inc(texcoord);
    if DoSpecular then
      VP.Add('   MOV result.texcoord[' + IntToStr(texcoord) + '], eye;');
  end;

  VP.Add('END');

  FVertexProgram.Assign(VP);
  Result := VP.Text;
  VP.Free;
end;

function TGLBumpShader.GenerateFragmentProgram: string;
var
  FP: TStringList;
  DoSpecular,
    DoTangent,
    DoParallaxOffset: Boolean;
  texcoord,
    normalTexCoords,
    diffTexCoords,
    specTexCoords,
    lightTexCoords,
    eyeTexCoords: Integer;
begin
  DoSpecular := not (SpecularMode = smOff);
  DoTangent := (BumpSpace = bsTangentExternal) or (BumpSpace =
    bsTangentQuaternion);
  DoParallaxOffset := (boParallaxMapping in BumpOptions) and DoTangent;

  texcoord := 0;
  normalTexCoords := texcoord;
  if boUseSecondaryTexCoords in BumpOptions then
    Inc(texcoord);
  diffTexCoords := texcoord;
  specTexCoords := texcoord;
  Inc(texcoord);
  lightTexCoords := texcoord;
  Inc(texcoord);
  eyeTexCoords := texcoord;

  FP := TStringList.Create;

  FP.Add('!!ARBfp1.0');

  FP.Add('PARAM lightDiffuse = program.local[0];');
  FP.Add('PARAM lightSpecular = program.local[1];');
  FP.Add('PARAM lightAtten = program.local[2];');
  FP.Add('PARAM materialDiffuse = state.material.diffuse;');
  FP.Add('PARAM materialSpecular = state.material.specular;');
  FP.Add('PARAM shininess = state.material.shininess;');
  FP.Add('TEMP temp, tex, light, eye, normal, col, diff, spec;');
  FP.Add('TEMP textureColor, reflect, atten, offset, texcoord;');

  if DoSpecular or DoParallaxOffset then
  begin
    // Get the eye vector
    FP.Add('   DP3 eye, fragment.texcoord[' + IntToStr(eyeTexCoords) +
      '], fragment.texcoord[' + IntToStr(eyeTexCoords) + '];');
    FP.Add('   RSQ eye, eye.x;');
    FP.Add('   MUL eye, fragment.texcoord[' + IntToStr(eyeTexCoords) +
      '], eye.x;');
  end;

  if DoParallaxOffset then
  begin
    // Get the parallax offset
    FP.Add('   TEX textureColor, fragment.texcoord[' + IntToStr(normalTexCoords)
      + '], texture[0], 2D;');
    FP.Add(Format('   MAD offset.x, textureColor.a, %f, %f;', [FParallaxOffset,
      -0.5 * FParallaxOffset]));
    FP.Add('   MUL offset, eye, offset.x;');
    FP.Add('   ADD texcoord, fragment.texcoord[' + IntToStr(normalTexCoords) +
      '], offset;');
  end
  else
    FP.Add('   MOV texcoord, fragment.texcoord[' + IntToStr(normalTexCoords) +
      '];');

  // Get the normalized normal vector
  FP.Add('   TEX textureColor, texcoord, texture[0], 2D;');
  FP.Add('   ADD normal, textureColor, -0.5;');
  FP.Add('   DP3 temp, normal, normal;');
  FP.Add('   RSQ temp, temp.x;');
  FP.Add('   MUL normal, normal, temp.x;');

  // Get the normalized light vector
  FP.Add('   MOV light, fragment.texcoord[' + IntToStr(lightTexCoords) + '];');
  if boLightAttenuation in BumpOptions then
    FP.Add('   MOV atten.x, light.w;');
  FP.Add('   DP3 light, light, light;');
  FP.Add('   RSQ light, light.x;');
  FP.Add('   MUL light, fragment.texcoord[' + IntToStr(lightTexCoords) +
    '], light.x;');

  // Calculate the diffuse color
  FP.Add('   DP3 diff, normal, light;');
  FP.Add('   MUL diff, diff, lightDiffuse;');
  FP.Add('   MUL diff, diff, materialDiffuse;');
  if boDiffuseTexture2 in BumpOptions then
  begin
    if DoParallaxOffset then
    begin
      FP.Add('   ADD temp, fragment.texcoord[' + IntToStr(diffTexCoords) +
        '], offset;');
      FP.Add('   TEX textureColor, temp, texture[1], 2D;');
    end
    else
      FP.Add('   TEX textureColor, fragment.texcoord[' + IntToStr(diffTexCoords)
        + '], texture[1], 2D;');
    FP.Add('   MUL diff, diff, textureColor;');
  end;

  if DoSpecular then
  begin
    case SpecularMode of
      smBlinn:
        begin
          FP.Add('   ADD eye, eye, light;');
          FP.Add('   DP3 temp, eye, eye;');
          FP.Add('   RSQ temp, temp.x;');
          FP.Add('   MUL eye, eye, temp.x;');
          FP.Add('   DP3_SAT spec, normal, eye;');
        end;
      smPhong:
        begin
          FP.Add('   DP3 reflect, normal, light;');
          FP.Add('   MUL reflect, reflect.x, normal;');
          FP.Add('   MUL reflect, 2.0, reflect;');
          FP.Add('   ADD reflect, reflect, -light;');
          FP.Add('   DP3_SAT spec, reflect, eye;');
        end;
    else
      Assert(False, 'Invalid specular mode!');
    end;

    FP.Add('   POW spec, spec.x, shininess.x;');
    FP.Add('   MUL spec, spec, materialSpecular;');
    FP.Add('   MUL spec, spec, lightSpecular;');
    if boSpecularTexture3 in BumpOptions then
    begin
      if DoParallaxOffset then
      begin
        FP.Add('   ADD temp, fragment.texcoord[' + IntToStr(specTexCoords) +
          '], offset;');
        FP.Add('   TEX textureColor, temp, texture[2], 2D;');
      end
      else
        FP.Add('   TEX textureColor, fragment.texcoord[' +
          IntToStr(specTexCoords) + '], texture[2], 2D;');
      FP.Add('   MUL spec, spec, textureColor;');
    end;
  end;

  // Output
  if DoSpecular then
    FP.Add('   ADD temp, diff, spec;')
  else
    FP.Add('   MOV temp, diff;');

  if boLightAttenuation in BumpOptions then
  begin
    FP.Add('   MUL atten.y, atten.x, atten.x;');
    FP.Add('   MAD atten.x, lightAtten.y, atten.x, lightAtten.x;');
    FP.Add('   MAD atten.x, lightAtten.z, atten.y, atten.x;');
    FP.Add('   RCP atten.x, atten.x;');
    FP.Add('   MUL temp, temp, atten.x;');
  end;

  FP.Add('   MOV_SAT result.color, temp;');
  FP.Add('   MOV result.color.w, 1.0;');

  FP.Add('END');

  FFragmentProgram.Assign(FP);
  Result := FP.Text;
  FP.Free;
end;

// DoLightPass
//

procedure TGLBumpShader.DoLightPass(var rci: TGLRenderContextInfo;
  lightID: Cardinal);
var
  dummyHandle, tempHandle: Integer;
  lightPos, lightAtten,
    materialDiffuse, lightDiffuse, lightSpecular: TVector;
begin
  FVertexProgramHandle.Enable;
  FVertexProgramHandle.Bind;

  // Set the light position to program.local[0]
  gl.GetLightfv(GL_LIGHT0 + FLightIDs[0], GL_POSITION, @lightPos.X);
  gl.ProgramLocalParameter4fv(GL_VERTEX_PROGRAM_ARB, 0, @lightPos.X);

  // Set the light attenutation to program.local[1]
  lightAtten.X := rci.GLStates.LightConstantAtten[FLightIDs[0]];
  lightAtten.Y := rci.GLStates.LightLinearAtten[FLightIDs[0]];
  lightAtten.Z := rci.GLStates.LightQuadraticAtten[FLightIDs[0]];
  gl.ProgramLocalParameter4fv(GL_VERTEX_PROGRAM_ARB, 1, @lightAtten.X);

  case FBumpMethod of
    bmDot3TexCombiner:
      begin
        rci.GLStates.ActiveTexture := 0;
        dummyHandle := rci.GLStates.TextureBinding[0, ttTexture2D];
        gl.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
        gl.TexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB, GL_DOT3_RGB_ARB);
        gl.TexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB, GL_TEXTURE0_ARB);
        gl.TexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_ARB, GL_PRIMARY_COLOR_ARB);

        rci.GLStates.ActiveTexture := 1;
        rci.GLStates.ActiveTextureEnabled[ttTexture2D] := True;
        tempHandle := rci.GLStates.TextureBinding[1, ttTexture2D];
        if tempHandle = 0 then
          rci.GLStates.TextureBinding[1, ttTexture2D] := dummyHandle;
        lightDiffuse := rci.GLStates.LightDiffuse[FLightIDs[0]];
        gl.GetMaterialfv(GL_FRONT, GL_DIFFUSE, @materialDiffuse);
        lightDiffuse.X := lightDiffuse.X * materialDiffuse.X;
        lightDiffuse.Y := lightDiffuse.Y * materialDiffuse.Y;
        lightDiffuse.Z := lightDiffuse.Z * materialDiffuse.Z;
        lightDiffuse.W := lightDiffuse.W * materialDiffuse.W;
        gl.TexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, @lightDiffuse);
        gl.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
        gl.TexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB, GL_MODULATE);
        gl.TexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB, GL_PREVIOUS_ARB);
        gl.TexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_ARB, GL_CONSTANT_COLOR_ARB);

        with rci.GLStates do
        begin
          ActiveTexture := 2;
          ActiveTextureEnabled[ttTexture2D] := False;
          ActiveTexture := 0;
        end;
      end;

    bmBasicARBFP:
      begin
        FFragmentProgramHandle.Enable;
        FFragmentProgramHandle.Bind;
        lightDiffuse := rci.GLStates.LightDiffuse[FLightIDs[0]];
        lightSpecular := rci.GLStates.LightSpecular[FLightIDs[0]];
        lightAtten.X := rci.GLStates.LightConstantAtten[FLightIDs[0]];

        gl.ProgramLocalParameter4fv(GL_FRAGMENT_PROGRAM_ARB, 0,
          @lightDiffuse.X);
        gl.ProgramLocalParameter4fv(GL_FRAGMENT_PROGRAM_ARB, 1,
          @lightSpecular.X);
        gl.ProgramLocalParameter4fv(GL_FRAGMENT_PROGRAM_ARB, 2,
          @lightAtten.X);
      end;

  else
    Assert(False, 'Invalid bump method!');
  end;
end;

procedure TGLBumpShader.DoApply(var rci: TGLRenderContextInfo; Sender: TObject);
var
  maxTextures, i: Integer;
  ambient, LMaterialAmbient: TColorVector;
  success: Boolean;
begin
  if (csDesigning in ComponentState) and not DesignTimeEnabled then
    exit;
  if not Enabled then
    exit;

  gl.GetIntegerv(GL_MAX_TEXTURE_UNITS_ARB, @maxTextures);

  success := False;
  try
    if not gl.ARB_multitexture then
      raise Exception.Create('This shader requires GL_ARB_multitexture.');
    if (maxTextures < 3)
      and ((BumpMethod <> bmDot3TexCombiner) or (BumpSpace = bsTangentExternal)) then
      raise
        Exception.Create('The current shader settings require 3 or more texture units.');
    if (maxTextures < 4)
      and (BumpMethod <> bmDot3TexCombiner)
      and (boUseSecondaryTexCoords in BumpOptions)
      and (SpecularMode <> smOff) then
      raise
        Exception.Create('The current shader settings require 4 or more texture units.');

    if not Assigned(FVertexProgramHandle) then
    begin
      FVertexProgramHandle := TGLARBVertexProgramHandle.CreateAndAllocate;
      FVertexProgramHandle.LoadARBProgram(GenerateVertexProgram);
    end;

    if not Assigned(FFragmentProgramHandle) then
      if FBumpMethod = bmBasicARBFP then
      begin
        FFragmentProgramHandle := TGLARBFragmentProgramHandle.CreateAndAllocate;
        FFragmentProgramHandle.LoadARBProgram(GenerateFragmentProgram);
      end;

    success := True;

  finally
    if not success then
    begin
      Enabled := False;
      DesignTimeEnabled := False;
    end;
  end;

  FLightIDs.Clear;
  rci.GLStates.ActiveTexture := 0;
  if rci.GLStates.ActiveTextureEnabled[ttTexture2D] then
    for i := 0 to rci.GLStates.MaxLights - 1 do
    begin
      if rci.GLStates.LightEnabling[i] then
        FLightIDs.Add(i);
    end;
  FLightsEnabled := FLightIDs.Count;

  FAmbientPass := False;
  FDiffusePass := False;

  if FLightIDs.Count > 0 then
  begin

    rci.GLStates.DepthFunc := cfLEqual;
    rci.GLStates.Disable(stBlend);
    DoLightPass(rci, FLightIDs[0]);
    FLightIDs.Delete(0);

  end
  else
    with rci.GLStates do
    begin
      Disable(stLighting);
      ActiveTexture := 0;
      ActiveTextureEnabled[ttTexture2D] := False;
      ActiveTexture := 1;
      ActiveTextureEnabled[ttTexture2D] := False;
      ActiveTexture := 2;
      ActiveTextureEnabled[ttTexture2D] := False;
      ActiveTexture := 0;

      gl.GetFloatv(GL_LIGHT_MODEL_AMBIENT, @ambient);
      gl.GetMaterialfv(GL_FRONT, GL_AMBIENT, @LMaterialAmbient);
      ambient.X := ambient.X * LMaterialAmbient.X;
      ambient.Y := ambient.Y * LMaterialAmbient.Y;
      ambient.Z := ambient.Z * LMaterialAmbient.Z;
      gl.Color3fv(@ambient);

      FAmbientPass := True;

    end;
end;

function TGLBumpShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
var
  ambient, LMaterialAmbient: TVector;
begin
  Result := False;
  if (csDesigning in ComponentState) and not DesignTimeEnabled then
    exit;
  if not Enabled then
    exit;

  if FLightIDs.Count > 0 then
    with rci.GLStates do
    begin

      DepthFunc := cfLEqual;
      Enable(stBlend);
      SetBlendFunc(bfOne, bfOne);

      DoLightPass(rci, FLightIDs[0]);
      FLightIDs.Delete(0);
      Result := True;
      Exit;

    end
  else if not FDiffusePass and (FLightsEnabled <> 0)
    and (boDiffuseTexture2 in BumpOptions)
    and (BumpMethod = bmDot3TexCombiner) then
    with rci.GLStates do
    begin
      Enable(stBlend);
      SetBlendFunc(bfDstColor, bfZero);
      ActiveTexture := 0;
      ActiveTextureEnabled[ttTexture2D] := False;
      ActiveTexture := 1;
      ActiveTextureEnabled[ttTexture2D] := True;
      gl.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
      ActiveTexture := 2;
      ActiveTextureEnabled[ttTexture2D] := False;
      ActiveTexture := 0;
      FDiffusePass := True;
      Result := True;
      Exit;
    end
  else if not FAmbientPass then
    with rci.GLStates do
    begin
      FVertexProgramHandle.Disable;
      if BumpMethod = bmBasicARBFP then
        FFragmentProgramHandle.Disable;
      Disable(stLighting);
      ActiveTexture := 0;
      ActiveTextureEnabled[ttTexture2D] := False;
      ActiveTexture := 1;
      ActiveTextureEnabled[ttTexture2D] := False;
      ActiveTexture := 2;
      ActiveTextureEnabled[ttTexture2D] := False;
      ActiveTexture := 0;

      DepthFunc := cfLEqual;
      Enable(stBlend);
      SetBlendFunc(bfOne, bfOne);

      gl.GetFloatv(GL_LIGHT_MODEL_AMBIENT, @ambient);
      gl.GetMaterialfv(GL_FRONT, GL_AMBIENT, @LMaterialAmbient);
      ambient.X := ambient.X * LMaterialAmbient.X;
      ambient.Y := ambient.Y * LMaterialAmbient.Y;
      ambient.Z := ambient.Z * LMaterialAmbient.Z;
      gl.Color3fv(@ambient);

      FAmbientPass := True;
      Result := True;
      Exit;
    end;

  FVertexProgramHandle.Disable;
  if BumpMethod = bmBasicARBFP then
    FFragmentProgramHandle.Disable;
end;

procedure TGLBumpShader.DeleteVertexPrograms;
begin
  FVertexProgramHandle.Free;
  FVertexProgramHandle := nil;
  FVertexProgram.Clear;
end;

procedure TGLBumpShader.DeleteFragmentPrograms;
begin
  FFragmentProgramHandle.Free;
  FFragmentProgramHandle := nil;
  FFragmentProgram.Clear;
end;

procedure TGLBumpShader.SetBumpMethod(const Value: TBumpMethod);
begin
  if Value <> FBumpMethod then
  begin
    FBumpMethod := Value;
    DeleteVertexPrograms;
    DeleteFragmentPrograms;
    NotifyChange(Self);
  end;
end;

procedure TGLBumpShader.SetBumpSpace(const Value: TBumpSpace);
begin
  if Value <> FBumpSpace then
  begin
    FBumpSpace := Value;
    DeleteVertexPrograms;
    DeleteFragmentPrograms;
    NotifyChange(Self);
  end;
end;

procedure TGLBumpShader.SetBumpOptions(const Value: TBumpOptions);
begin
  if Value <> FBumpOptions then
  begin
    FBumpOptions := Value;
    DeleteVertexPrograms;
    DeleteFragmentPrograms;
    NotifyChange(Self);
  end;
end;

procedure TGLBumpShader.SetSpecularMode(const Value: TSpecularMode);
begin
  if Value <> FSpecularMode then
  begin
    FSpecularMode := Value;
    DeleteVertexPrograms;
    DeleteFragmentPrograms;
    NotifyChange(Self);
  end;
end;

procedure TGLBumpShader.SetDesignTimeEnabled(const Value: Boolean);
begin
  if Value <> FDesignTimeEnabled then
  begin
    FDesignTimeEnabled := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLBumpShader.SetParallaxOffset(const Value: Single);
begin
  if Value <> FParallaxOffset then
  begin
    FParallaxOffset := Value;
    DeleteVertexPrograms;
    DeleteFragmentPrograms;
    NotifyChange(Self);
  end;
end;

end.

