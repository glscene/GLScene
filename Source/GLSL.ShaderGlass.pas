// This unit is part of the GLScene Engine, http://glscene.org
//

unit GLSL.ShaderGlass;

(*
   Glass shader : Environment mapping with an
   equirectangular 2D texture and refraction mapping
   with a background texture blended together using the Fresnel terms
*)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,

  GLScene,
  GLBaseClasses,
  GLState,
  OpenGLTokens,
  GLContext,
  GLRenderContextInfo,
  GLVectorGeometry,
  GLCoordinates,
  GLTextureFormat,
  GLColor,
  GLTexture,
  GLMaterial,
  GLPersistentClasses,
  GLGraphics,
  GLSL.Shader,
  GLS.ShaderCustom;

(* Custom class for GLSLGlassShader.
  Glass shader : Environment mapping and refraction mapping using the fresnel terms *)
type
  TGLCustomGLSLGlassShader = class(TGLCustomGLSLShader)
  private
    FDiffuseColor: TGLColor;
    FDepth: Single;
    FMix: Single;
    FAlpha: Single;
    FMaterialLibrary: TGLAbstractMaterialLibrary;
    FMainTexture: TGLTexture; // EnvMap
    FMainTexName: TGLLibMaterialName;
    FRefractionTexture: TGLTexture;
    FRefractionTexName: TGLLibMaterialName;
    FOwnerObject: TGLBaseSceneObject;
    FBlendSrc: TGLBlendFunction;
    FBlendDst: TGLBlendFunction;
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
    procedure SetMainTexTexture(const Value: TGLTexture);
    function GetMainTexName: TGLLibMaterialName;
    procedure SetMainTexName(const Value: TGLLibMaterialName);
    procedure SetRefractionTexTexture(const Value: TGLTexture);
    function GetRefractionTexName: TGLLibMaterialName;
    procedure SetRefractionTexName(const Value: TGLLibMaterialName);
    procedure SetDiffuseColor(AValue: TGLColor);
  protected
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
    procedure SetMaterialLibrary(const Value
      : TGLAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DiffuseColor: TGLColor read FDiffuseColor Write SetDiffuseColor;
    property Depth: Single read FDepth write FDepth;
    property Mix: Single read FMix write FMix;
    property Alpha: Single read FAlpha write FAlpha;
    property MaterialLibrary: TGLAbstractMaterialLibrary read GetMaterialLibrary
      write SetMaterialLibrary;
    property MainTexture: TGLTexture read FMainTexture write SetMainTexTexture;
    property MainTextureName: TGLLibMaterialName read GetMainTexName
      write SetMainTexName;
    property RefractionTexture: TGLTexture read FRefractionTexture
      write SetRefractionTexTexture;
    property RefractionTextureName: TGLLibMaterialName read GetRefractionTexName
      write SetRefractionTexName;
    property OwnerObject: TGLBaseSceneObject read FOwnerObject
      write FOwnerObject;
    property BlendSrc: TGLBlendFunction read FBlendSrc write FBlendSrc
      default bfSrcAlpha;
    property BlendDst: TGLBlendFunction read FBlendDst write FBlendDst
      default bfDstAlpha;
  end;

  TGLSLGlassShader = class(TGLCustomGLSLGlassShader)
  published
    property DiffuseColor;
    property Depth;
    property Mix;
    property Alpha;
    property MaterialLibrary;
    property MainTexture;
    property MainTextureName;
    property RefractionTexture;
    property RefractionTextureName;
    property OwnerObject;
    property BlendSrc;
    property BlendDst;
  end;

//===============================================
implementation
//===============================================

const
  fBuffSize: Integer = 512;

constructor TGLCustomGLSLGlassShader.Create(AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    clear;
    Add('varying vec3  Normal; ');
    Add('varying vec3  EyeDir; ');
    Add('varying vec4  EyePos; ');
    Add('varying float LightIntensity; ');

    Add('void main(void) ');
    Add('{ ');
    Add('  gl_Position    = ftransform(); ');
    Add('  vec3 LightPos = gl_LightSource[0].position.xyz;');
    Add('  Normal         = normalize(gl_NormalMatrix * gl_Normal); ');
    Add('  vec4 pos       = gl_ModelViewMatrix * gl_Vertex; ');
    Add('  EyeDir         = -pos.xyz; ');
    Add('  EyePos		   = gl_ModelViewProjectionMatrix * gl_Vertex; ');
    Add('  LightIntensity = max(dot(normalize(LightPos - EyeDir), Normal), 0.0); ');
    Add('} ');
  end;

  With FragmentProgram.Code do
  begin
    clear;
    Add('const vec3 Xunitvec = vec3 (1.0, 0.0, 0.0); ');
    Add('const vec3 Yunitvec = vec3 (0.0, 1.0, 0.0); ');

    Add('uniform vec4  BaseColor; ');
    Add('uniform float Depth; ');
    Add('uniform float MixRatio; ');
    Add('uniform float AlphaIntensity; ');

    // need to scale our framebuffer - it has a fixed width/height of 2048
    Add('uniform float FrameWidth; ');
    Add('uniform float FrameHeight; ');

    Add('uniform sampler2D EnvMap; ');
    Add('uniform sampler2D RefractionMap; ');

    Add('varying vec3  Normal; ');
    Add('varying vec3  EyeDir; ');
    Add('varying vec4  EyePos; ');
    Add('varying float LightIntensity; ');

    Add('void main (void) ');
    Add('{ ');
    // Compute reflection vector
    Add('  vec3 reflectDir = reflect(EyeDir, Normal); ');
    // Compute altitude and azimuth angles
    Add('  vec2 index; ');
    Add('  index.y = dot(normalize(reflectDir), Yunitvec); ');
    Add('  reflectDir.y = 0.0; ');
    Add('  index.x = dot(normalize(reflectDir), Xunitvec) * 0.5; ');
    // Translate index values into proper range
    Add('  if (reflectDir.z >= 0.0) ');
    Add('      index = (index + 1.0) * 0.5; ');
    Add('  else ');
    Add('  { ');
    Add('    index.t = (index.t + 1.0) * 0.5; ');
    Add('    index.s = (-index.s) * 0.5 + 1.0; ');
    Add('  } ');
    // if reflectDir.z >= 0.0, s will go from 0.25 to 0.75
    // if reflectDir.z <  0.0, s will go from 0.75 to 1.25, and
    // that's OK, because we've set the texture to wrap.

    // Do a lookup into the environment map.
    Add('  vec4 envColor = texture2D(EnvMap, index); ');

    // calc fresnels term.  This allows a view dependant blend of reflection/refraction
    Add('  float fresnel = abs(dot(normalize(EyeDir), Normal)); ');
    Add('  fresnel *= MixRatio; ');
    Add('  fresnel = clamp(fresnel, 0.1, 0.9); ');
    // calc refraction
    Add('  vec3 refractionDir = normalize(EyeDir) - normalize(Normal); ');
    // Scale the refraction so the z element is equal to depth
    Add('  float depthVal = Depth / -refractionDir.z; ');
    // perform the div by w
    Add('  float recipW = 1.0 / EyePos.w; ');
    Add('  vec2 eye = EyePos.xy * vec2(recipW); ');
    // calc the refraction lookup
    Add('  index.s = (eye.x + refractionDir.x * depthVal); ');
    Add('  index.t = (eye.y + refractionDir.y * depthVal); ');
    // scale and shift so we're in the range 0-1
    Add('  index.s = index.s / 2.0 + 0.5; ');
    Add('  index.t = index.t / 2.0 + 0.5; ');
    // as we're looking at the framebuffer, we want it clamping at the edge of the rendered scene, not the edge of the texture,
    // so we clamp before scaling to fit
    Add('  float recip1k = 1.0 / 2048.0; ');
    Add('  index.s = clamp(index.s, 0.0, 1.0 - recip1k); ');
    Add('  index.t = clamp(index.t, 0.0, 1.0 - recip1k); ');
    // scale the texture so we just see the rendered framebuffer
    Add('  index.s = index.s * FrameWidth * recip1k; ');
    Add('  index.t = index.t * FrameHeight * recip1k; ');

    Add('  vec4 RefractionColor = texture2D(RefractionMap, index.st); ');
    //Add('  RefractionColor.a = 0.9; ');

  //  Add('  RefractionColor = RefractionColor+vec3(0.75,0.75,0.75); ');//
    // Add lighting to base color and mix
  //  Add('  vec4 base = LightIntensity * BaseColor; ');
    Add('  envColor = mix(envColor, BaseColor,LightIntensity); ');
    Add('  envColor = mix(envColor, RefractionColor, fresnel); ');
    Add('  envColor.a = AlphaIntensity; ');
    Add('  gl_FragColor = envColor; //vec4 (envColor.rgb, 0.3); ');
    Add('} ');
  end;

//  FMainTexture := TGLTexture.Create(nil);
//  FMainTexture.Disabled := False;
//  FMainTexture.Enabled := True;


  //setup initial parameters
  FDiffuseColor := TGLColor.Create(Self);

  FDepth := 0.1;
  FMix:=1.0;
  FAlpha:=1.0;

  FDiffuseColor.SetColor(0.15, 0.15, 0.15, 1.0);

  FBlendSrc := bfSrcAlpha;
  FBlendDst := bfDstAlpha;
end;

destructor TGLCustomGLSLGlassShader.Destroy;
begin
  FDiffuseColor.Destroy;
  inherited;
end;

procedure TGLCustomGLSLGlassShader.DoApply(var rci: TGLRenderContextInfo; Sender: TObject);
begin
  // Auto Render EnvMap
  // capture and create material from framebuffer

  // I don't say why but We need to reset and reaffect our texture otherwise one of the texture is broken
  with FMainTexture do
  begin
    PrepareBuildList;
    gl.ActiveTexture(GL_TEXTURE0_ARB);
    gl.BindTexture(GL_TEXTURE_2D, Handle);
    gl.ActiveTexture(GL_TEXTURE0_ARB);
  end;

  with FRefractionTexture do
  begin
    PrepareBuildList;
    gl.ActiveTexture(GL_TEXTURE1_ARB);
    gl.BindTexture(GL_TEXTURE_2D, Handle);
    gl.ActiveTexture(GL_TEXTURE0_ARB);
  end;

  FOwnerObject.Visible := False;
  TGLSceneBuffer(rci.buffer).CopyToTexture(FMainTexture);
  FOwnerObject.Visible := True;

  GetGLSLProg.UseProgramObject;

//  GetGLSLProg.Uniform4f['BaseColor'] := FDiffuseColor.Color;
//  GetGLSLProg.Uniform1f['Depth'] := FDepth;
//  GetGLSLProg.Uniform1f['MixRatio'] := FMix; // 0 - 2
//  GetGLSLProg.Uniform1f['FrameWidth'] := fBuffSize * 3.125;
//  GetGLSLProg.Uniform1f['FrameHeight'] := fBuffSize * 3.125;

//  SetTex('EnvMap',FMainTexture);  --> BUG
//  SetTex('RefractionMap',FRefractionTexture);

  param['BaseColor'].AsVector4f := FDiffuseColor.Color;
  param['Depth'].AsVector1f := FDepth; // 0 - 0.3
  param['MixRatio'].AsVector1f := FMix; // 0 - 2
  param['AlphaIntensity'].AsVector1f := FAlpha; // 0 - 2
  param['FrameWidth'].AsVector1f := fBuffSize * 3.75;
  param['FrameHeight'].AsVector1f := fBuffSize * 3.75;

  Param['EnvMap'].AsTexture2D[0] := FMainTexture;
  Param['RefractionMap'].AsTexture2D[1] :=FRefractionTexture ;

  gl.Enable(GL_BLEND);
  gl.BlendFunc(cGLBlendFunctionToGLEnum[FBlendSrc],cGLBlendFunctionToGLEnum[FBlendDst]);
end;

function TGLCustomGLSLGlassShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  gl.Disable(GL_BLEND);
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;


function TGLCustomGLSLGlassShader.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TGLCustomGLSLGlassShader.SetMaterialLibrary(const Value: TGLAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TGLAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TGLCustomGLSLGlassShader.SetMainTexTexture(const Value: TGLTexture);
begin
  if FMainTexture = Value then Exit;
  FMainTexture := Value;
  NotifyChange(Self)
end;

function TGLCustomGLSLGlassShader.GetMainTexName: TGLLibMaterialName;
begin
  Result := TGLMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FMainTexture);
  if Result = '' then Result := FMainTexName;
end;

procedure TGLCustomGLSLGlassShader.SetMainTexName(const Value: TGLLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FMainTexName = Value then Exit;
  FMainTexName  := Value;

  FMainTexture := TGLMaterialLibrary(FMaterialLibrary).TextureByName(FMainTexName);
  NotifyChange(Self);
end;

procedure TGLCustomGLSLGlassShader.SetRefractionTexTexture(const Value: TGLTexture);
begin
  if FRefractionTexture = Value then Exit;
  FRefractionTexture := Value;
  NotifyChange(Self)
end;

function TGLCustomGLSLGlassShader.GetRefractionTexName: TGLLibMaterialName;
begin
  Result := TGLMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FRefractionTexture);
  if Result = '' then Result := FRefractionTexName;
end;

procedure TGLCustomGLSLGlassShader.SetRefractionTexName(const Value: TGLLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FRefractionTexName = Value then Exit;
  FRefractionTexName  := Value;

  FRefractionTexture := TGLMaterialLibrary(FMaterialLibrary).TextureByName(FRefractionTexName);
  NotifyChange(Self);
end;

procedure TGLCustomGLSLGlassShader.SetDiffuseColor(AValue: TGLColor);
begin
  FDiffuseColor.DirectColor := AValue.Color;
end;

procedure TGLCustomGLSLGlassShader.Notification(AComponent: TComponent; Operation: TOperation);
var
  Index: Integer;
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FMaterialLibrary then
      if FMaterialLibrary <> nil then
      begin

        if FMainTexture <> nil then
        begin
          Index := TGLMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FMainTexture);
          if Index <> -1 then
            SetMainTexTexture(nil);
        end;

        if FRefractionTexture <> nil then
        begin
          Index := TGLMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FRefractionTexture);
          if Index <> -1 then
            SetRefractionTexTexture(nil);
        end;

        FMaterialLibrary := nil;
      end;
end;

end.
