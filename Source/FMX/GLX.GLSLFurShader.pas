//
// Graphic Scene Engine, http://glscene.org
//
{
   Fur shader that simulate Fur / Hair / Grass. 
   At this time only one light source is supported
}
unit GLX.GLSLFurShader;

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
  GLX.Coordinates, 
  GLX.VectorGeometry, 
  GLX.VectorTypes,
  GLX.TextureFormat, 
  GLX.Color, 
  GLX.Texture, 
  GLX.Material, 
  GLX.GLSLShader, 
  GLX.CustomShader;

type
  TgxCustomGLSLFurShader = class(TgxCustomGLSLShader)
  private

    FMaterialLibrary: TgxAbstractMaterialLibrary;

    FCurrentPass: Integer;

    FPassCount: Single;
    FFurLength: Single;
    FMaxFurLength: Single;
    FFurScale: Single;
    FRandomFurLength : Boolean;

    FColorScale: TgxColor;
    FAmbient: TgxColor;

    FGravity : TgxCoordinates;
    FLightIntensity : Single;

    FMainTex  : TgxTexture;
    FNoiseTex : TgxTexture;
    FNoiseTexName  : TgxLibMaterialName;
    FMainTexName   : TgxLibMaterialName;

    FBlendSrc : TBlendFunction;
    FBlendDst : TBlendFunction;
   // FBlendEquation : TBlendEquation;


    function GetMaterialLibrary: TgxAbstractMaterialLibrary;

    procedure SetMainTexTexture(const Value: TgxTexture);
    procedure SetNoiseTexTexture(const Value: TgxTexture);

    function GetNoiseTexName: TgxLibMaterialName;
    procedure SetNoiseTexName(const Value: TgxLibMaterialName);
    function GetMainTexName: TgxLibMaterialName;
    procedure SetMainTexName(const Value: TgxLibMaterialName);

    procedure SetGravity(APosition:TgxCoordinates);
    procedure SetAmbient(AValue: TgxColor);
    procedure SetColorScale(AValue: TgxColor);


  protected
    procedure DoApply(var rci : TgxRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;

    procedure SetMaterialLibrary(const Value: TgxAbstractMaterialLibrary); virtual;
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

    property ColorScale: TgxColor read FColorScale Write setColorScale;
    property Ambient: TgxColor read FAmbient write setAmbient;

    property MaterialLibrary: TgxAbstractMaterialLibrary read getMaterialLibrary write SetMaterialLibrary;
    property MainTexture: TgxTexture read FMainTex write SetMainTexTexture;
    property MainTextureName: TgxLibMaterialName read GetMainTexName write SetMainTexName;
    property NoiseTexture: TgxTexture read FNoiseTex write SetNoiseTexTexture;
    property NoiseTextureName: TgxLibMaterialName read GetNoiseTexName write SetNoiseTexName;

    //property BlendEquation : TBlendEquation read FBlendEquation write FBlendEquation default beMin;
    property BlendSrc  : TBlendFunction read FBlendSrc write FBlendSrc default bfSrcColor;
    property BlendDst  : TBlendFunction read FBlendDst write FBlendDst default bfOneMinusDstColor;
    property Gravity : TgxCoordinates Read FGravity write setGravity;
    property LightIntensity : Single read FLightIntensity Write FLightIntensity;


  end;

  TgxSLFurShader = class(TgxCustomGLSLFurShader)
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

implementation

{ TgxCustomGLSLFurShader }

constructor TgxCustomGLSLFurShader.Create(AOwner: TComponent);
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

  FColorScale := TgxColor.Create(Self);
  FColorScale.SetColor(0.2196,0.2201,0.2201,1.0);

  FAmbient := TgxColor.Create(Self);
  FAmbient.SetColor(1.0,1.0,1.0,1.0);

  // The Blend Funcs are very important for realistic fur rendering it can vary follow your textures
  FBlendSrc := bfOneMinusSrcColor;
  FBlendDst := bfOneMinusSrcAlpha;
  FGravity := TgxCoordinates.Create(self);
  FGravity.AsAffineVector := AffinevectorMake(0.0,0.0,0.0);
  FLightIntensity := 2.5;
end;

destructor TgxCustomGLSLFurShader.Destroy;
begin
  Enabled:=false;
  FGravity.Free;
  FColorScale.Destroy;
  FAmbient.Destroy;
  inherited;
end;

procedure TgxCustomGLSLFurShader.DoApply(var rci: TgxRenderContextInfo;Sender: TObject);
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


  glPushAttrib(GL_COLOR_BUFFER_BIT);
  glEnable(GL_BLEND);
  gl.BlendFunc(cGLBlendFunctionToGLEnum[FBlendSrc],cGLBlendFunctionToGLEnum[FBlendDst]);

 //glBlendFunc(GL_SRC_ALPHA, cGLBlendFunctionToGLEnum[FBlendSrc]);
 //glBlendFunc(GL_DST_ALPHA,cGLBlendFunctionToGLEnum[FBlendDst]);
 //glBlendEquation(cGLBlendEquationToGLEnum[BlendEquation]);

end;

function TgxCustomGLSLFurShader.DoUnApply(var rci: TgxRenderContextInfo): Boolean;
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
    glPopAttrib;
    Result := False;
  end;
end;


function TgxCustomGLSLFurShader.GetMaterialLibrary: TgxAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TgxCustomGLSLFurShader.SetMaterialLibrary(const Value: TgxAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TgxAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TgxCustomGLSLFurShader.SetMainTexTexture(const Value: TgxTexture);
begin
  if FMainTex = Value then Exit;
  FMainTex := Value;
  NotifyChange(Self)
end;

procedure TgxCustomGLSLFurShader.SetNoiseTexTexture(const Value: TgxTexture);
begin
  if FNoiseTex = Value then Exit;
  FNoiseTex := Value;
  NotifyChange(Self);
end;

function TgxCustomGLSLFurShader.GetNoiseTexName: TgxLibMaterialName;
begin
  Result := TgxMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FNoiseTex);
  if Result = '' then Result := FNoiseTexName;
end;

procedure TgxCustomGLSLFurShader.SetNoiseTexName(const Value: TgxLibMaterialName);
begin
  //Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FNoiseTexName = Value then Exit;
  FNoiseTexName  := Value;
  FNoiseTex := TgxMaterialLibrary(FMaterialLibrary).TextureByName(FNoiseTexName);
  NotifyChange(Self);
end;

function TgxCustomGLSLFurShader.GetMainTexName: TgxLibMaterialName;
begin
  Result := TgxMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FMainTex);
  if Result = '' then Result := FMainTexName;
end;

procedure TgxCustomGLSLFurShader.SetMainTexName(const Value: TgxLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FMainTexName = Value then Exit;
  FMainTexName  := Value;

  FMainTex := TgxMaterialLibrary(FMaterialLibrary).TextureByName(FMainTexName);
  NotifyChange(Self);
end;


procedure TgxCustomGLSLFurShader.Notification(AComponent: TComponent; Operation: TOperation);
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
          Index := TgxMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FNoiseTex);
          if Index <> -1 then
            SetNoiseTexTexture(nil);
        end;

        if FMainTex <> nil then
        begin
          Index := TgxMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FMainTex);
          if Index <> -1 then
            SetMainTexTexture(nil);
        end;

        FMaterialLibrary := nil;
      end;
end;

procedure TgxCustomGLSLFurShader.SetGravity(APosition: TgxCoordinates);
begin
  FGravity.SetPoint(APosition.DirectX, APosition.DirectY, APosition.DirectZ);
end;

procedure TgxCustomGLSLFurShader.SetAmbient(AValue: TgxColor);
begin
  FAmbient.DirectColor := AValue.Color;
end;

procedure TgxCustomGLSLFurShader.SetColorScale(AValue: TgxColor);
begin
  FColorScale.DirectColor := AValue.Color;
end;

end.

