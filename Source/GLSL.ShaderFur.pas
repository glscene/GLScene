//
// This unit is part of the GLScene Engine, http://glscene.org
//

unit GLSL.ShaderFur;

(*
   Fur shader that simulate Fur / Hair / Grass. 
   At this time only one light source is supported
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
  GLCoordinates,
  GLVectorGeometry,
  GLVectorTypes,
  GLTextureFormat,
  GLColor,
  GLTexture,
  GLMaterial,
  GLSL.Shader,
  GLS.ShaderCustom;

type
  TGLCustomGLSLFurShader = class(TGLCustomGLSLShader)
  private
    FMaterialLibrary: TGLAbstractMaterialLibrary;
    FCurrentPass: Integer;
    FPassCount: Single;
    FFurLength: Single;
    FMaxFurLength: Single;
    FFurScale: Single;
    FRandomFurLength : Boolean;
    FColorScale: TGLColor;
    FAmbient: TGLColor;
    FGravity : TGLCoordinates;
    FLightIntensity : Single;
    FMainTex  : TGLTexture;
    FNoiseTex : TGLTexture;
    FNoiseTexName  : TGLLibMaterialName;
    FMainTexName   : TGLLibMaterialName;
    FBlendSrc : TGLBlendFunction;
    FBlendDst : TGLBlendFunction;
   // FBlendEquation : TGLBlendEquation;
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
    procedure SetMainTexTexture(const Value: TGLTexture);
    procedure SetNoiseTexTexture(const Value: TGLTexture);
    function GetNoiseTexName: TGLLibMaterialName;
    procedure SetNoiseTexName(const Value: TGLLibMaterialName);
    function GetMainTexName: TGLLibMaterialName;
    procedure SetMainTexName(const Value: TGLLibMaterialName);
    procedure SetGravity(APosition:TGLCoordinates);
    procedure SetAmbient(AValue: TGLColor);
    procedure SetColorScale(AValue: TGLColor);
  protected
    procedure DoApply(var rci : TGLRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
    procedure SetMaterialLibrary(const Value: TGLAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    //Common stuff
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property PassCount: Single read FPassCount write FPassCount;
    property FurLength: Single read FFurLength write FFurLength;
    property MaxFurLength: Single read FMaxFurLength write FMaxFurLength;
    property FurDensity: Single read FFurScale write FFurScale;
    property RandomFurLength : Boolean read FRandomFurLength Write FRandomFurLength;
    property ColorScale: TGLColor read FColorScale Write setColorScale;
    property Ambient: TGLColor read FAmbient write setAmbient;
    property MaterialLibrary: TGLAbstractMaterialLibrary read getMaterialLibrary write SetMaterialLibrary;
    property MainTexture: TGLTexture read FMainTex write SetMainTexTexture;
    property MainTextureName: TGLLibMaterialName read GetMainTexName write SetMainTexName;
    property NoiseTexture: TGLTexture read FNoiseTex write SetNoiseTexTexture;
    property NoiseTextureName: TGLLibMaterialName read GetNoiseTexName write SetNoiseTexName;
    //property BlendEquation : TBlendEquation read FBlendEquation write FBlendEquation default beMin;
    property BlendSrc  : TGLBlendFunction read FBlendSrc write FBlendSrc default bfSrcColor;
    property BlendDst  : TGLBlendFunction read FBlendDst write FBlendDst default bfOneMinusDstColor;
    property Gravity : TGLCoordinates Read FGravity write setGravity;
    property LightIntensity : Single read FLightIntensity Write FLightIntensity;
  end;

  TGLSLFurShader = class(TGLCustomGLSLFurShader)
  published
    property PassCount;
    property FurLength;
    property MaxFurLength;
    property FurDensity;
    property RandomFurLength;
    property ColorScale;
    property Ambient;
    property LightIntensity;
    property Gravity;
    property BlendSrc;
    property BlendDst;
    property MainTexture;
    property MainTextureName;
    property NoiseTexture;
    property NoiseTextureName;
  end;

//------------------------------------------
implementation
//------------------------------------------

//------------------------------------------
// TGLCustomGLSLFurShader
//------------------------------------------

constructor TGLCustomGLSLFurShader.Create(AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    clear;
    Add('uniform float fFurLength; ');
    Add('uniform float fFurMaxLength; ');
    Add('uniform float pass_index; ');
    Add('uniform int UseRandomLength; ');
    Add('uniform float fLayer; // 0 to 1 for the level ');
    Add('uniform vec3 vGravity; ');

    Add('varying vec3 normal; ');
    Add('varying vec2  vTexCoord; ');
    Add('varying vec3 lightVec; ');
   // Add('varying vec3 viewVec; ');

   Add('float rand(vec2 co){ ');
   Add(' return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453); ');
   Add('} ');

    Add('void main() ');
    Add('{ ');
    Add('  mat4 mWorld = gl_ModelViewMatrix; ');
    Add('  vec3 Normal = gl_Normal; ');
    Add('  vec4 Position = gl_Vertex; ');
    Add('  vec4 lightPos = gl_LightSource[0].position;');
    Add('  vec4 vert =  gl_ModelViewMatrix * gl_Vertex; ');
    Add('  normal = gl_NormalMatrix * gl_Normal; ');
    // Additional Gravit/Force Code
    Add('  vec3 vGravity2 = vGravity *mat3(mWorld ); ');
    // We use the pow function, so that only the tips of the hairs bend
    Add('  float k = pow(fLayer, 3.0); ');

    // Random the Hair length  perhaps will can use a texture map for controling.
    Add(' vec3 vNormal = normalize( Normal * mat3(mWorld )); ');
    Add(' float RandomFurLength; ');
    Add('  if (UseRandomLength == 1) { RandomFurLength = fFurLength+fFurLength*rand(vNormal.xy); } ');
    Add('  else { RandomFurLength = fFurLength ; } ');

    Add('  RandomFurLength = pass_index*(RandomFurLength * vNormal); ');
    Add('  if (RandomFurLength > fFurMaxLength ) { RandomFurLength = fFurMaxLength; } ');

    Add('  Position.xyz += RandomFurLength +(vGravity2 * k);  ');

    Add('  Position.xyz += pass_index*(fFurLength * Normal)+(vGravity2 * k);  ');
    Add('  vTexCoord = gl_MultiTexCoord0; ');
    Add('   ');
    Add('   gl_Position =  gl_ModelViewProjectionMatrix * Position; ');
    Add('  lightVec = vec3(lightPos - vert); ');

    //  Add('  viewVec = -vec3(vert); ');
    Add('normal = vNormal; ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
    clear;
    Add('uniform vec4 fcolorScale; ');
    Add('uniform float pass_index; ');
    Add('uniform float fFurScale; ');
    Add('uniform vec4 vAmbient; ');
    Add('uniform float fLayer; // 0 to 1 for the level ');
    Add('uniform float vLightIntensity; ');

    Add('uniform sampler2D FurTexture; ');
    Add('uniform sampler2D ColourTexture; ');

    //textures
    Add('varying vec2 vTexCoord; ');
    Add('varying vec3 normal; ');
    Add('varying vec3 lightVec; ');
//    Add('varying vec3 viewVec; ');

    Add('void main() ');
    Add('{ ');
    // A Faking shadow
    Add('  vec4 fAlpha = texture2D( FurTexture, vTexCoord*fFurScale );     ');
    Add('  float fakeShadow =  mix(0.3, 1.0, fAlpha.a-fLayer); ');
    Add('     ');

    Add('  vec4 FinalColour = vec4(0.0,0.0,0.0,1.0); ');

    Add('FinalColour = (fcolorScale*texture2D( ColourTexture, vTexCoord))*fakeShadow; ');

    // This comment part it's for controling if we must draw the hair according the red channel and the alpha in NoiseMap
    // Don' t work well a this time the NoiseMap must be perfect
//    Add('float visibility = 0.0; ');
//    Add('if (pass_index == 1.0) ');
//    Add('{ ');
//    Add('   visibility = 1.0;  ');
//    Add('} ');
//    Add('else ');
//    Add('{ ');
//    Add('  if (fAlpha.a<fAlpha.r) { visibility = 0.0; } ');
//    Add('  else { visibility =mix(0.1,1.0,(1.02-fLayer)); } //-1.0; ');
//    Add('} ');

    Add('float visibility =mix(0.1,1.0,(1.02-fLayer)); ');   // The Last past must be transparent

    // Simply Lighting - For this time only ONE light source is supported
    Add('vec4 ambient = vAmbient*FinalColour;  ');
    Add('vec4 diffuse = FinalColour; ');
    Add('vec3 L = normalize(lightVec); ');
    Add('float NdotL = dot(L, normal); ');
    Add('// "Half-Lambert" technique for more pleasing diffuse term ');
    Add('diffuse = diffuse*(0.5*NdotL+0.5); ');
    Add('FinalColour = vLightIntensity*(ambient+ diffuse); // + no specular; ');
    Add('FinalColour.a = visibility ; ');
    Add('    // Return the calculated color ');
    Add('    gl_FragColor= FinalColour; ');
    Add('} ');
  end;

  //Fur stuff
  FPassCount := 16; // More is greater more the fur is dense
  FFurLength := 0.3000;  // The minimal Hair length
  FMaxFurLength := 3.0;
  FRandomFurLength := false;
  FFurScale:=1.0;

  FColorScale := TGLColor.Create(Self);
  FColorScale.SetColor(0.2196,0.2201,0.2201,1.0);

  FAmbient := TGLColor.Create(Self);
  FAmbient.SetColor(1.0,1.0,1.0,1.0);

  // The Blend Funcs are very important for realistic fur rendering it can vary follow your textures
  FBlendSrc := bfOneMinusSrcColor;
  FBlendDst := bfOneMinusSrcAlpha;
  FGravity := TGLCoordinates.Create(self);
  FGravity.AsAffineVector := AffinevectorMake(0.0,0.0,0.0);
  FLightIntensity := 2.5;
end;

destructor TGLCustomGLSLFurShader.Destroy;
begin
  Enabled:=false;
  FGravity.Free;
  FColorScale.Destroy;
  FAmbient.Destroy;
  inherited;
end;

procedure TGLCustomGLSLFurShader.DoApply(var rci: TGLRenderContextInfo;Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  //Fur stuff
  FCurrentPass := 1;

  param['pass_index'].AsVector1f := 1.0;
  param['fFurLength'].AsVector1f := FFurLength;

  param['fFurMaxLength'].AsVector1f := FMaxFurLength;
  param['fFurScale'].AsVector1f := FFurScale;
  if FRandomFurLength then param['UseRandomLength'].AsVector1i := 1
  else param['UseRandomLength'].AsVector1i := 0;

  param['fcolorScale'].AsVector4f := FColorScale.Color;
  param['FurTexture'].AsTexture2D[0] := FNoiseTex;
  param['ColourTexture'].AsTexture2D[1] := FMainTex;
  param['vGravity'].AsVector3f := FGravity.AsAffineVector;

  param['vAmbient'].AsVector4f := FAmbient.Color; //vectorMake(0.5,0.5,0.5,1.0);
  param['fLayer'].AsVector1f := 1/PassCount;
  param['vLightIntensity'].AsVector1f := FLightIntensity;


  gl.PushAttrib(GL_COLOR_BUFFER_BIT);
  gl.Enable(GL_BLEND);
  gl.BlendFunc(cGLBlendFunctionToGLEnum[FBlendSrc],cGLBlendFunctionToGLEnum[FBlendDst]);

 // gl.BlendFunc(GL_SRC_ALPHA, cGLBlendFunctionToGLEnum[FBlendSrc]);
 //gl.BlendFunc(GL_DST_ALPHA,cGLBlendFunctionToGLEnum[FBlendDst]);
 // gl.BlendEquation(cGLBlendEquationToGLEnum[BlendEquation]);

end;

function TGLCustomGLSLFurShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  if FCurrentPass < PassCount then
  begin
    Inc(FCurrentPass);
    //GetGLSLProg.Uniform1f['pass_index'] := FCurrentPass;
    param['pass_index'].AsVector1f  := FCurrentPass;
    param['fLayer'].AsVector1f := FCurrentPass/PassCount;
    Result := True;
  end
  else
  begin
   // glActiveTextureARB(GL_TEXTURE0_ARB);
    gl.ActiveTexture(GL_TEXTURE0_ARB);
    GetGLSLProg.EndUseProgramObject;
    gl.PopAttrib;
    Result := False;
  end;
end;


function TGLCustomGLSLFurShader.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TGLCustomGLSLFurShader.SetMaterialLibrary(const Value: TGLAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TGLAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TGLCustomGLSLFurShader.SetMainTexTexture(const Value: TGLTexture);
begin
  if FMainTex = Value then Exit;
  FMainTex := Value;
  NotifyChange(Self)
end;

procedure TGLCustomGLSLFurShader.SetNoiseTexTexture(const Value: TGLTexture);
begin
  if FNoiseTex = Value then Exit;
  FNoiseTex := Value;
  NotifyChange(Self);
end;

function TGLCustomGLSLFurShader.GetNoiseTexName: TGLLibMaterialName;
begin
  Result := TGLMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FNoiseTex);
  if Result = '' then Result := FNoiseTexName;
end;

procedure TGLCustomGLSLFurShader.SetNoiseTexName(const Value: TGLLibMaterialName);
begin
  //Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FNoiseTexName = Value then Exit;
  FNoiseTexName  := Value;
  FNoiseTex := TGLMaterialLibrary(FMaterialLibrary).TextureByName(FNoiseTexName);
  NotifyChange(Self);
end;

function TGLCustomGLSLFurShader.GetMainTexName: TGLLibMaterialName;
begin
  Result := TGLMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FMainTex);
  if Result = '' then Result := FMainTexName;
end;

procedure TGLCustomGLSLFurShader.SetMainTexName(const Value: TGLLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FMainTexName = Value then Exit;
  FMainTexName  := Value;

  FMainTex := TGLMaterialLibrary(FMaterialLibrary).TextureByName(FMainTexName);
  NotifyChange(Self);
end;


procedure TGLCustomGLSLFurShader.Notification(AComponent: TComponent; Operation: TOperation);
var
  Index: Integer;
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FMaterialLibrary then
      if FMaterialLibrary <> nil then
      begin
        // Need to nil the textures that were owned by it
        if FNoiseTex <> nil then
        begin
          Index := TGLMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FNoiseTex);
          if Index <> -1 then
            SetNoiseTexTexture(nil);
        end;

        if FMainTex <> nil then
        begin
          Index := TGLMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FMainTex);
          if Index <> -1 then
            SetMainTexTexture(nil);
        end;

        FMaterialLibrary := nil;
      end;
end;

procedure TGLCustomGLSLFurShader.SetGravity(APosition: TGLCoordinates);
begin
  FGravity.SetPoint(APosition.DirectX, APosition.DirectY, APosition.DirectZ);
end;

procedure TGLCustomGLSLFurShader.SetAmbient(AValue: TGLColor);
begin
  FAmbient.DirectColor := AValue.Color;
end;

procedure TGLCustomGLSLFurShader.SetColorScale(AValue: TGLColor);
begin
  FColorScale.DirectColor := AValue.Color;
end;

end.

