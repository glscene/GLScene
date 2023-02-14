//
// Graphic Scene Engine, http://glscene.org
//
{
   Gooch shader : Gooch shading is used to substitute photorealistic
   rendering by rendering that focuses on structore and shape of the object.
   Instead of usage of light and shadow, Gooch shading uses concept of warm and cool colors.
   Standard Blinn-Phong shading only modulates base color of the object.
   In Gooch shading intensity of diffuse lighting is used to determine how to blend warm and cold colors together. 

   At this time only one light source is supported
     
}

unit GLX.GLSLGoochShader;

interface

{$I GLX.Scene.inc}

uses
  System.Classes,
  
  GLX.Scene, 
  GLX.CrossPlatform, 
  GLX.BaseClasses, 
  GLX.State, 
  Winapi.OpenGL, 
  Winapi.OpenGLext,  
  GLX.OpenGL1x, 
  GLX.Context, 
  GLX.RenderContextInfo, 
  GLX.VectorGeometry, 
  GLX.Coordinates,
  GLX.TextureFormat, 
  GLX.Color, 
  GLX.Texture, 
  GLX.Material, 
  GLX.GLSLShader, 
  GLX.CustomShader;

{ Custom class for GLSLSimpleGoochShader. }
type
  TgxCustomGLSLSimpleGoochShader = class(TgxCustomGLSLShader)
  private
    FDiffuseColor : TgxColor;
    FWarmColor : TgxColor;
    FCoolColor : TgxColor;
    FSpecularColor : TgxColor;
    FAmbientColor : TgxColor;
    FDiffuseWarm : Single;
    FDiffuseCool : Single;
    FAmbientFactor : Single;
    FDiffuseFactor : Single;
    FSpecularFactor : Single;

    FBlendingMode: TgxBlendingModeEx;

    procedure SetDiffuseColor(AValue: TgxColor);
    procedure SetAmbientColor(AValue: TgxColor);
    procedure SetSpecularColor(AValue: TgxColor);
    procedure SetWarmColor(AValue: TgxColor);
    procedure SetCoolColor(AValue: TgxColor);

  protected
    procedure DoApply(var rci : TgxRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property DiffuseColor : TgxColor read FDiffuseColor Write setDiffuseColor;
    property WarmColor : TgxColor read FWarmColor Write setWarmColor;
    property CoolColor : TgxColor Read FCoolColor Write setCoolColor;
    property SpecularColor : TgxColor Read FSpecularColor Write setSpecularColor;
    property AmbientColor : TgxColor Read FAmbientColor Write setAmbientColor;
    property WarmFactor : Single Read FDiffuseWarm Write FDiffuseWarm;
    property CoolFactor : Single Read FDiffuseCool Write FDiffuseCool;
    property AmbientFactor : Single Read FAmbientFactor Write FAmbientFactor;
    property DiffuseFactor : Single Read FDiffuseFactor Write FDiffuseFactor;
    property SpecularFactor : Single Read FSpecularFactor Write FSpecularFactor;

    property BlendingMode: TgxBlendingModeEx read FBlendingMode write FBlendingMode default bmxOpaque;
  end;

type
  TgxSLSimpleGoochShader = class(TgxCustomGLSLSimpleGoochShader)
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

implementation


{ TgxCustomGLSLSimpleGoochShader }

constructor TgxCustomGLSLSimpleGoochShader.Create(AOwner: TComponent);
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
  FDiffuseColor := TgxColor.Create(self);
  FDiffuseColor.SetColor(0.75,0.75,0.75,1.0);
  FWarmColor := TgxColor.Create(self);
  FWarmColor.SetColor(0.88,0.81,0.49,1.0);
  FCoolColor := TgxColor.Create(self);
  FCoolColor.SetColor(0.58,0.10,0.76,1.0);
  FAmbientColor := TgxColor.Create(self);
  FAmbientColor.SetColor(0.3,0.3,0.3,1.0);
  FSpecularColor := TgxColor.Create(self);
  FSpecularColor.SetColor(1.0,1.0,1.0,1.0);

  FDiffuseWarm    := 0.55;
  FDiffuseCool    := 0.30;
  FAmbientFactor  := 1.0;
  FDiffuseFactor  :=0.8;
  FSpecularFactor :=0.9;

  FBlendingMode:=bmxOpaque;
end;

destructor TgxCustomGLSLSimpleGoochShader.Destroy;
begin
  FDiffuseColor.Free;
  FWarmColor.Free;
  FCoolColor.Free;
  FSpecularColor.Free;
  FAmbientColor.Free;
  inherited;
end;

procedure TgxCustomGLSLSimpleGoochShader.DoApply(var rci: TgxRenderContextInfo;
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

 // glPushAttrib(GL_COLOR_BUFFER_BIT);
  ApplyBlendingModeEx(FBlendingMode);
//  glEnable(GL_BLEND);
//  gl.BlendFunc(cGLBlendFunctionToGLEnum[FBlendSrc],cGLBlendFunctionToGLEnum[FBlendDst]);
end;

function TgxCustomGLSLSimpleGoochShader.DoUnApply(var rci: TgxRenderContextInfo): Boolean;
begin

  gl.ActiveTexture(GL_TEXTURE0_ARB);
  GetGLSLProg.EndUseProgramObject;
  UnApplyBlendingModeEx;
 // glPopAttrib;
  Result := False;
end;

procedure TgxCustomGLSLSimpleGoochShader.SetDiffuseColor(AValue: TgxColor);
begin
  FDiffuseColor.DirectColor := AValue.Color;
end;

procedure TgxCustomGLSLSimpleGoochShader.SetAmbientColor(AValue: TgxColor);
begin
  FAmbientColor.DirectColor := AValue.Color;
end;

procedure TgxCustomGLSLSimpleGoochShader.SetSpecularColor(AValue: TgxColor);
begin
  FSpecularColor.DirectColor := AValue.Color;
end;

procedure TgxCustomGLSLSimpleGoochShader.SetWarmColor(AValue: TgxColor);
begin
  FWarmColor.DirectColor := AValue.Color;
end;

procedure TgxCustomGLSLSimpleGoochShader.SetCoolColor(AValue: TgxColor);
begin
  FCoolColor.DirectColor := AValue.Color;
end;

end.
