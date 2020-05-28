//
// This unit is part of the GLScene Engine, http://glscene.org
//
{
   Gooch shader : Gooch shading is used to substitute photorealistic
   rendering by rendering that focuses on structore and shape of the object.
   Instead of usage of light and shadow, Gooch shading uses concept of warm and cool colors.
   Standard Blinn-Phong shading only modulates base color of the object.
   In Gooch shading intensity of diffuse lighting is used to determine how to blend warm and cold colors together. 

   At this time only one light source is supported
}
unit GLSLGoochShader;

interface

{$I GLScene.inc}

uses
  System.Classes,
  
  OpenGLTokens,
  GLScene,
  GLBaseClasses,
  GLState,
  GLContext,
  GLRenderContextInfo,
  GLVectorGeometry,
  GLCoordinates,
  GLTextureFormat,
  GLColor,
  GLTexture,
  GLMaterial,
  GLSLShader,
  GLCustomShader;

{Custom class for GLSLSimpleGoochShader. }
type
  TGLCustomGLSLSimpleGoochShader = class(TGLCustomGLSLShader)
  private
    FDiffuseColor : TGLColor;
    FWarmColor : TGLColor;
    FCoolColor : TGLColor;
    FSpecularColor : TGLColor;
    FAmbientColor : TGLColor;
    FDiffuseWarm : Single;
    FDiffuseCool : Single;
    FAmbientFactor : Single;
    FDiffuseFactor : Single;
    FSpecularFactor : Single;
    FBlendingMode: TGLBlendingModeEx;
    procedure SetDiffuseColor(AValue: TGLColor);
    procedure SetAmbientColor(AValue: TGLColor);
    procedure SetSpecularColor(AValue: TGLColor);
    procedure SetWarmColor(AValue: TGLColor);
    procedure SetCoolColor(AValue: TGLColor);
  protected
    procedure DoApply(var rci : TGLRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property DiffuseColor : TGLColor read FDiffuseColor Write setDiffuseColor;
    property WarmColor : TGLColor read FWarmColor Write setWarmColor;
    property CoolColor : TGLColor Read FCoolColor Write setCoolColor;
    property SpecularColor : TGLColor Read FSpecularColor Write setSpecularColor;
    property AmbientColor : TGLColor Read FAmbientColor Write setAmbientColor;
    property WarmFactor : Single Read FDiffuseWarm Write FDiffuseWarm;
    property CoolFactor : Single Read FDiffuseCool Write FDiffuseCool;
    property AmbientFactor : Single Read FAmbientFactor Write FAmbientFactor;
    property DiffuseFactor : Single Read FDiffuseFactor Write FDiffuseFactor;
    property SpecularFactor : Single Read FSpecularFactor Write FSpecularFactor;
    property BlendingMode: TGLBlendingModeEx read FBlendingMode write FBlendingMode default bmxOpaque;
  end;

type
  TGLSLSimpleGoochShader = class(TGLCustomGLSLSimpleGoochShader)
  published
    property DiffuseColor;
    property WarmColor;
    property CoolColor;
    property SpecularColor;
    property AmbientColor;
    property WarmFactor;
    property CoolFactor;
    property AmbientFactor;
    property DiffuseFactor;
    property SpecularFactor;
  end;

//-------------------------------------------------------------
implementation
//-------------------------------------------------------------


{ TGLCustomGLSLSimpleGoochShader }

constructor TGLCustomGLSLSimpleGoochShader.Create(AOwner: TComponent);
begin
  inherited;

  with VertexProgram.Code do
  begin
    Clear;

    Add('varying vec3 vNormal; ');
    Add('varying vec3 lightVec; ');
    Add('varying vec3 viewVec; ');
    Add('varying vec3 ReflectVec; ');
    Add(' ');
    Add('void main() ');
    Add('{ ');
    Add('  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; ');
    Add('  vec4 lightPos = gl_LightSource[0].position;');
    Add('  vec4 vert =  gl_ModelViewMatrix * gl_Vertex; ');
    Add('  vec3 normal = gl_NormalMatrix * gl_Normal; ');
    Add('  vNormal  = normalize(normal); ');

    Add('  lightVec = vec3(lightPos - vert); ');
    Add('  ReflectVec    = normalize(reflect(-lightVec, vNormal)); ');
    Add('  viewVec = -vec3(vert); ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
    Clear;
    Add('uniform vec4  SurfaceColor; ');
    Add('uniform vec4  WarmColor; ');
    Add('uniform vec4  CoolColor; ');
    Add('uniform vec4  SpecularColor; ');
    Add('uniform vec4  AmbientColor; ');
    Add('uniform float DiffuseWarm; ');
    Add('uniform float DiffuseCool; ');
    Add('uniform float AmbientFactor; ');
    Add('uniform float DiffuseFactor; ');
    Add('uniform float SpecularFactor; ');

    Add('varying vec3 vNormal; ');
    Add('varying vec3 lightVec; ');
    Add('varying vec3 viewVec; ');
    Add('varying vec3 ReflectVec; ');
    Add(' ');
    Add('void main() ');
    Add('{ ');


    Add('vec3 L = normalize(lightVec); ');
    Add('vec3 V = normalize(viewVec); ');
    Add('vec3 halfAngle = normalize(L + V); ');
    Add('float NdotL   = (dot(L, vNormal) + 1.0) * 0.5; ');
    Add('float NdotH = clamp(dot(halfAngle, vNormal), 0.0, 1.0); ');
    Add('// "Half-Lambert" technique for more pleasing diffuse term ');
    Add('float diffuse = 0.5 * NdotL + 0.5; ');
    Add('vec3 nreflect = normalize(ReflectVec); ');
    Add('float specular    = max(dot(nreflect, V), 0.0); ');
    Add('specular          = pow(specular, 64.0); ');

    Add('vec4 kCool    = min(CoolColor + DiffuseCool * SurfaceColor, 1.0); ');
    Add('vec4 kWarm    = min(WarmColor + DiffuseWarm * SurfaceColor, 1.0); ');


    Add('vec4 Cgooch = mix(kWarm, kCool, diffuse); ');

    Add('vec3 result = AmbientFactor * AmbientColor.rgb + DiffuseFactor * Cgooch.rgb + SpecularColor.rgb * SpecularFactor *specular; ');

    Add('gl_FragColor = vec4(result,SurfaceColor.a); ');
    Add('} ');
  end;


  // Initial stuff.
  FDiffuseColor := TGLColor.Create(self);
  FDiffuseColor.SetColor(0.75,0.75,0.75,1.0);
  FWarmColor := TGLColor.Create(self);
  FWarmColor.SetColor(0.88,0.81,0.49,1.0);
  FCoolColor := TGLColor.Create(self);
  FCoolColor.SetColor(0.58,0.10,0.76,1.0);
  FAmbientColor := TGLColor.Create(self);
  FAmbientColor.SetColor(0.3,0.3,0.3,1.0);
  FSpecularColor := TGLColor.Create(self);
  FSpecularColor.SetColor(1.0,1.0,1.0,1.0);

  FDiffuseWarm    := 0.55;
  FDiffuseCool    := 0.30;
  FAmbientFactor  := 1.0;
  FDiffuseFactor  :=0.8;
  FSpecularFactor :=0.9;

  FBlendingMode:=bmxOpaque;
end;

destructor TGLCustomGLSLSimpleGoochShader.Destroy;
begin
  FDiffuseColor.Free;
  FWarmColor.Free;
  FCoolColor.Free;
  FSpecularColor.Free;
  FAmbientColor.Free;
  inherited;
end;

procedure TGLCustomGLSLSimpleGoochShader.DoApply(var rci: TGLRenderContextInfo;
  Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  param['SurfaceColor'].AsVector4f := FDiffuseColor.Color;
  param['WarmColor'].AsVector4f := FWarmColor.Color;
  param['CoolColor'].AsVector4f := FCoolColor.Color;
  param['AmbientColor'].AsVector4f := FAmbientColor.Color;
  param['SpecularColor'].AsVector4f := FSpecularColor.Color;
  param['DiffuseWarm'].AsVector1f := FDiffuseWarm;
  param['DiffuseCool'].AsVector1f := FDiffuseCool;
  param['AmbientFactor'].AsVector1f := FAmbientFactor;
  param['DiffuseFactor'].AsVector1f := FDiffuseFactor;
  param['SpecularFactor'].AsVector1f := FSpecularFactor;

 // gl.PushAttrib(GL_COLOR_BUFFER_BIT);
  ApplyBlendingModeEx(FBlendingMode);
//  gl.Enable(GL_BLEND);
//  gl.BlendFunc(cGLBlendFunctionToGLEnum[FBlendSrc],cGLBlendFunctionToGLEnum[FBlendDst]);
end;

function TGLCustomGLSLSimpleGoochShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin

  gl.ActiveTexture(GL_TEXTURE0_ARB);
  GetGLSLProg.EndUseProgramObject;
  UnApplyBlendingModeEx;
 // gl.PopAttrib;
  Result := False;
end;

procedure TGLCustomGLSLSimpleGoochShader.SetDiffuseColor(AValue: TGLColor);
begin
  FDiffuseColor.DirectColor := AValue.Color;
end;

procedure TGLCustomGLSLSimpleGoochShader.SetAmbientColor(AValue: TGLColor);
begin
  FAmbientColor.DirectColor := AValue.Color;
end;

procedure TGLCustomGLSLSimpleGoochShader.SetSpecularColor(AValue: TGLColor);
begin
  FSpecularColor.DirectColor := AValue.Color;
end;

procedure TGLCustomGLSLSimpleGoochShader.SetWarmColor(AValue: TGLColor);
begin
  FWarmColor.DirectColor := AValue.Color;
end;

procedure TGLCustomGLSLSimpleGoochShader.SetCoolColor(AValue: TGLColor);
begin
  FCoolColor.DirectColor := AValue.Color;
end;

end.
