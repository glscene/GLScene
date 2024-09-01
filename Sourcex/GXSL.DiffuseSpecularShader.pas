//
// The graphics engine GXScene https://github.com/glscene
//
unit GXSL.DiffuseSpecularShader;

(*
    This is a collection of GLSL Diffuse Specular shaders, comes in these variaties
              (to know what these suffixes and prefixes mean see GXS.CustomShader.pas):
      - TgxSLDiffuseSpecularShader
      - TgxSLDiffuseSpecularShaderMT

      - TgxSLMLDiffuseSpecularShader
      - TgxSLMLDiffuseSpecularShaderMT

    Notes:
     1) TgxSLDiffuseSpecularShader takes all Material parameters directly
      from OpenGL (that includes TgxMaterial's)
     2) TgxSLDiffuseSpecularShader takes all Light parameters directly
      from OpenGL (that includes TgxLightSource's)
*)

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,

  GXS.Texture,
  GXS.Scene,
  GXS.VectorGeometry,
  GXS.Strings,
  GXSL.CustomShader,
  GXSL.Shader,
  GXS.Color,
  GXS.RenderContextInfo,
  GXS.Material;

type
  EgxslDiffuseSpecularShaderException = class(EgxslShaderException);

  // Abstract class.
  TgxBaseCustomGLSLDiffuseSpecular = class(TgxslCustomShader)
  private
    FLightPower: Single;
    FRealisticSpecular: Boolean;
    FFogSupport: TgxShaderFogSupport;
    procedure SetRealisticSpecular(const Value: Boolean);
    procedure SetFogSupport(const Value: TgxShaderFogSupport);
  protected
    procedure DoApply(var rci : TgxRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    property LightPower: Single read FLightPower write FLightPower;
    property RealisticSpecular: Boolean read FRealisticSpecular write SetRealisticSpecular;

    // User can disable fog support and save some FPS if he doesn't need it.
    property FogSupport: TgxShaderFogSupport read FFogSupport write SetFogSupport default sfsAuto;
  end;

  // Abstract class.
  TgxBaseGLSLDiffuseSpecularShaderMT = class(TgxBaseCustomGLSLDiffuseSpecular, IgxMaterialLibrarySupported)
  private
    FMaterialLibrary: TgxMaterialLibrary;
    FMainTexture: TgxTexture;
    FMainTextureName: TgxLibMaterialName;
    function GetMainTextureName: TgxLibMaterialName;
    procedure SetMainTextureName(const Value: TgxLibMaterialName);
    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TgxAbstractMaterialLibrary;
  protected
    procedure SetMaterialLibrary(const Value: TgxMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoApply(var rci : TgxRenderContextInfo; Sender : TObject); override;
  public
    property MainTexture: TgxTexture read FMainTexture write FMainTexture;
    property MainTextureName: TgxLibMaterialName read GetMainTextureName write SetMainTextureName;
    property MaterialLibrary: TgxMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
  end;

                     {********  Single Light  ************}

  TgxCustomGLSLDiffuseSpecularShader = class(TgxBaseCustomGLSLDiffuseSpecular)
  protected
    procedure DoInitialize(var rci: TgxRenderContextInfo; Sender: TObject); override;
    procedure DoApply(var rci : TgxRenderContextInfo; Sender : TObject); override;
  end;


  TgxCustomGLSLDiffuseSpecularShaderMT = class(TgxBaseGLSLDiffuseSpecularShaderMT)
  protected
    procedure DoInitialize(var rci: TgxRenderContextInfo; Sender: TObject); override;
  end;

//*******  Multi Light  *********

  TLightRecord = record
    Enabled: Boolean;
    Style: TgxLightStyle;
  end;

// Note: probably LightCount should be replaced by LightSources, like in GLSLBumpShader.pas

  TgxCustomGLSLMLDiffuseSpecularShader = class(TgxBaseCustomGLSLDiffuseSpecular)
  private
    FLightTrace: array[0..7] of TLightRecord;
  protected
    procedure DoInitialize(var rci: TgxRenderContextInfo; Sender: TObject); override;
    procedure DoApply(var rci: TgxRenderContextInfo; Sender: TObject); override;
  public
    constructor Create(AOwner : TComponent); override;
  end;

  TgxCustomGLSLMLDiffuseSpecularShaderMT = class(TgxBaseGLSLDiffuseSpecularShaderMT)
  private
    FLightTrace: array[0..7] of TLightRecord;
  protected
    procedure DoInitialize(var rci: TgxRenderContextInfo; Sender: TObject); override;
    procedure DoApply(var rci: TgxRenderContextInfo; Sender: TObject); override;
  public
    constructor Create(AOwner : TComponent); override;
  end;


                     {********  Published Stuff  ************}

  TgxSLDiffuseSpecularShaderMT = class(TgxCustomGLSLDiffuseSpecularShaderMT)
  published
    property MainTextureName;

    property LightPower;
    property FogSupport;
  end;

  TgxSLDiffuseSpecularShader = class(TgxCustomGLSLDiffuseSpecularShader)
  published
    property LightPower;
    property FogSupport;
  end;


  TgxSLMLDiffuseSpecularShaderMT = class(TgxCustomGLSLMLDiffuseSpecularShaderMT)
  published
    property MainTextureName;

    property LightPower;
    property FogSupport;
  end;

  TgxSLMLDiffuseSpecularShader = class(TgxCustomGLSLMLDiffuseSpecularShader)
  published
    property LightPower;
    property FogSupport;
  end;

//---------------------------------------------------------------------------
implementation
//---------------------------------------------------------------------------

procedure GetVertexProgramCode(const Code: TStrings;
  AFogSupport: Boolean; var rci: TgxRenderContextInfo);
begin
  with Code do
  begin
    Clear;
    Add('varying vec3 Normal; ');
    Add('varying vec4 Position; ');
    if AFogSupport then
    begin
      Add('varying float fogFactor; ');
    end;
    Add(' ');
    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add('  gl_Position = ftransform(); ');
    Add('  gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0; ');
    Add('  Normal = normalize(gl_NormalMatrix * gl_Normal); ');
    Add('  Position = gl_ModelViewMatrix * gl_Vertex; ');

    if AFogSupport then
    begin
    Add('  const float LOG2 = 1.442695; ');
    Add('  gl_FogFragCoord = length(Position.xyz); ');

    case TgxSceneBuffer(rci.buffer).FogEnvironment.FogMode of
      fmLinear:
        Add('  fogFactor = (gl_Fog.end - gl_FogFragCoord) * gl_Fog.scale; ');
      fmExp, // Yep, I don't know about this type, so I use fmExp2.
      fmExp2:
      begin
        Add('  fogFactor = exp2( -gl_Fog.density * ');
        Add('  				   gl_Fog.density * ');
        Add('  				   gl_FogFragCoord * ');
        Add('  				   gl_FogFragCoord * ');
        Add('  				   LOG2 ); ');
      end;
    else
      Assert(False, strUnknownType);
    end;

      Add('  fogFactor = clamp(fogFactor, 0.0, 1.0); ');
    end;

    Add('} ');
  end;
end;

procedure AddLightSub(const Code: TStrings);
begin
  with Code do
  begin
    Add('void pointLight(in int i, in vec3 normal, in vec3 eye, in vec3 ecPosition3)');
    Add('{');
    Add('   float nDotVP;       // normal . light direction');
    Add('   float nDotHV;       // normal . light half vector');
    Add('   float pf;           // power factor');
    Add('   float attenuation;  // computed attenuation factor');
    Add('   float d;            // distance from surface to light source');
    Add('   vec3  VP;           // direction from surface to light position');
    Add('   vec3  halfVector;   // direction of maximum highlights');
    Add(' ');
    Add('   // Compute vector from surface to light position');
    Add('   VP = vec3 (gl_LightSource[i].position) - ecPosition3;');
    Add(' ');
    Add('   // Compute distance between surface and light position');
    Add('   d = length(VP);');
    Add(' ');
    Add('   // Normalize the vector from surface to light position');
    Add('   VP = normalize(VP);');
    Add(' ');
    Add('   // Compute attenuation');
    Add('   attenuation = 1.0 / (gl_LightSource[i].constantAttenuation +');
    Add('       gl_LightSource[i].linearAttenuation * d +');
    Add('       gl_LightSource[i].quadraticAttenuation * d * d);');
    Add(' ');
    Add('   halfVector = normalize(VP + eye);');
    Add(' ');
    Add('   nDotVP = max(0.0, dot(normal, VP));');
    Add('   nDotHV = max(0.0, dot(normal, halfVector));');
    Add(' ');
    Add('   if (nDotVP == 0.0)');
    Add('   {');
    Add('       pf = 0.0;');
    Add('   }');
    Add('   else');
    Add('   {');
    Add('       pf = pow(nDotHV, gl_FrontMaterial.shininess);');
    Add(' ');
    Add('   }');
    Add('   Ambient  += gl_LightSource[i].ambient * attenuation;');
    Add('   Diffuse  += gl_LightSource[i].diffuse * nDotVP * attenuation;');
    Add('   Specular += gl_LightSource[i].specular * pf * attenuation;');
    Add('}');
    Add(' ');
    Add('void directionalLight(in int i, in vec3 normal)');
    Add('{');
    Add('   float nDotVP;         // normal . light direction');
    Add('   float nDotHV;         // normal . light half vector');
    Add('   float pf;             // power factor');
    Add(' ');
    Add('   nDotVP = max(0.0, dot(normal, normalize(vec3 (gl_LightSource[i].position))));');
    Add('   nDotHV = max(0.0, dot(normal, vec3 (gl_LightSource[i].halfVector)));');
    Add(' ');
    Add('   if (nDotVP == 0.0)');
    Add('   {');
    Add('       pf = 0.0;');
    Add('   }');
    Add('   else');
    Add('   {');
    Add('       pf = pow(nDotHV, gl_FrontMaterial.shininess);');
    Add(' ');
    Add('   }');
    Add('   Ambient  += gl_LightSource[i].ambient;');
    Add('   Diffuse  += gl_LightSource[i].diffuse * nDotVP;');
    Add('   Specular += gl_LightSource[i].specular * pf;');
    Add('}');
    Add('void spotLight(in int i, in vec3 normal, in vec3 eye, in vec3 ecPosition3)');
    Add('{');
    Add('   float nDotVP;            // normal . light direction');
    Add('   float nDotHV;            // normal . light half vector');
    Add('   float pf;                // power factor');
    Add('   float spotDot;           // cosine of angle between spotlight');
    Add('   float spotAttenuation;   // spotlight attenuation factor');
    Add('   float attenuation;       // computed attenuation factor');
    Add('   float d;                 // distance from surface to light source');
    Add('   vec3  VP;                // direction from surface to light position');
    Add('   vec3  halfVector;        // direction of maximum highlights');
    Add(' ');
    Add('   // Compute vector from surface to light position');
    Add('   VP = vec3 (gl_LightSource[i].position) - ecPosition3;');
    Add(' ');
    Add('   // Compute distance between surface and light position');
    Add('   d = length(VP);');
    Add(' ');
    Add('   // Normalize the vector from surface to light position');
    Add('   VP = normalize(VP);');
    Add(' ');
    Add('   // Compute attenuation');
    Add('   attenuation = 1.0 / (gl_LightSource[i].constantAttenuation +');
    Add('       gl_LightSource[i].linearAttenuation * d +');
    Add('       gl_LightSource[i].quadraticAttenuation * d * d);');
    Add(' ');
    Add('   // See if point on surface is inside cone of illumination');
    Add('   spotDot = dot(-VP, normalize(gl_LightSource[i].spotDirection));');
    Add(' ');
    Add('   if (spotDot < gl_LightSource[i].spotCosCutoff)');
    Add('   {');
    Add('       spotAttenuation = 0.0; // light adds no contribution');
    Add('   }');
    Add('   else');
    Add('   {');
    Add('       spotAttenuation = pow(spotDot, gl_LightSource[i].spotExponent);');
    Add(' ');
    Add('   }');
    Add('   // Combine the spotlight and distance attenuation.');
    Add('   attenuation *= spotAttenuation;');
    Add(' ');
    Add('   halfVector = normalize(VP + eye);');
    Add(' ');
    Add('   nDotVP = max(0.0, dot(normal, VP));');
    Add('   nDotHV = max(0.0, dot(normal, halfVector));');
    Add(' ');
    Add('   if (nDotVP == 0.0)');
    Add('   {');
    Add('       pf = 0.0;');
    Add('   }');
    Add('   else');
    Add('   {');
    Add('       pf = pow(nDotHV, gl_FrontMaterial.shininess);');
    Add(' ');
    Add('   }');
    Add('   Ambient  += gl_LightSource[i].ambient * attenuation;');
    Add('   Diffuse  += gl_LightSource[i].diffuse * nDotVP * attenuation;');
    Add('   Specular += gl_LightSource[i].specular * pf * attenuation;');
    Add(' ');
    Add('}');
    Add('void infiniteSpotLight(in int i, in vec3 normal)');
    Add('{');
    Add('   float nDotVP;         // normal . light direction');
    Add('   float nDotHV;         // normal . light half vector');
    Add('   float pf;             // power factor');
    Add('   float spotAttenuation;');
    Add('   vec3  Ppli;');
    Add('   vec3  Sdli;');
    Add(' ');
    Add('   nDotVP = max(0.0, dot(normal, normalize(vec3 (gl_LightSource[i].position))));');
    Add('   nDotHV = max(0.0, dot(normal, vec3 (gl_LightSource[i].halfVector)));');
    Add(' ');
    Add('   Ppli = -normalize(vec3(gl_LightSource[i].position));');
    Add('   Sdli = normalize(vec3(gl_LightSource[i].spotDirection));');
    Add(' ');
    Add('   spotAttenuation = pow(dot(Ppli, Sdli), gl_LightSource[i].spotExponent);');
    Add('   if (nDotVP == 0.0)');
    Add('   {');
    Add('       pf = 0.0;');
    Add('   }');
    Add('   else');
    Add('   {');
    Add('       pf = pow(nDotHV, gl_FrontMaterial.shininess);');
    Add(' ');
    Add('   }');
    Add('   Ambient  += gl_LightSource[i].ambient * spotAttenuation;');
    Add('   Diffuse  += gl_LightSource[i].diffuse * nDotVP * spotAttenuation;');
    Add('   Specular += gl_LightSource[i].specular * pf * spotAttenuation;');
    Add('}');
  end;
end;

procedure GetMLFragmentProgramCodeMid(const Code: TStrings;
  const CurrentLight: Integer; AStyle: TgxLightStyle);
begin
  with Code do
  begin
    case AStyle of
      lsOmni: Add(Format('  pointLight(%d, N, eye, Pos); ', [CurrentLight]));
      lsSpot: Add(Format('  spotLight(%d, N, eye, Pos); ', [CurrentLight]));
      lsParallel: Add(Format('  directionalLight(%d, N); ', [CurrentLight]));
      lsParallelSpot: Add(Format('  infiniteSpotLight(%d, N); ', [CurrentLight]));
    end;
  end;
end;

procedure GetFragmentProgramCode(const Code: TStrings;
  const ARealisticSpecular: Boolean; const AFogSupport: Boolean;
  aRci: TgxRenderContextInfo);
var
  scene: TgxScene;
begin
  with Code do
  begin
    Clear;
    Add('uniform float LightIntensity; ');
    Add('uniform sampler2D MainTexture; ');
    Add(' ');
    Add('varying vec3 Normal; ');
    Add('varying vec4 Position; ');

    if AFogSupport then
    begin
      Add('varying float fogFactor; ');
    end;

    Add('vec4 Ambient;');
    Add('vec4 Diffuse;');
    Add('vec4 Specular;');

    AddLightSub(Code);

    Add('void main(void) ');
    Add('{ ');
    Add('  vec4 TextureContrib = texture2D(MainTexture, gl_TexCoord[0].xy); ');
    Add('  vec3 eye = vec3(0.0, 0.0, 1.0); ');
    Add('  Diffuse = vec4(0.0); ');
    Add('  Specular = vec4(0.0); ');
    Add('  Ambient = vec4(0.0); ');
    Add('  vec3 Pos = Position.xyz; ');
    Add('  vec3 N = normalize(Normal); ');
    scene := TgxScene(ARci.scene);
    if (scene.Lights.Count > 0) then
      GetMLFragmentProgramCodeMid(Code, 0,
      TgxLightSource(scene.Lights[0]).LightStyle);

    if ARealisticSpecular then
      Add('  gl_FragColor = LightIntensity * (TextureContrib * (gl_FrontLightModelProduct.sceneColor + Ambient * gl_FrontMaterial.ambient + Diffuse * gl_FrontMaterial.diffuse) + Specular * gl_FrontMaterial.specular); ')
    else
      Add('  gl_FragColor = LightIntensity * TextureContrib * (gl_FrontLightModelProduct.sceneColor + Ambient * gl_FrontMaterial.ambient + Diffuse * gl_FrontMaterial.diffuse + Specular * gl_FrontMaterial.specular); ');

    if AFogSupport then
      Add('  gl_FragColor = mix(gl_Fog.color, gl_FragColor, fogFactor);');

    Add('  gl_FragColor.a = TextureContrib.a; ');
    Add('} ');
  end;
end;

procedure GetMLFragmentProgramCodeBeg(const Code: TStrings;
  const AFogSupport: Boolean);
begin
  with Code do
  begin
    Clear;
    Add('uniform sampler2D MainTexture;');
    Add('uniform float LightIntensity; ');
    Add('uniform float SpecPower; ');
    Add('varying vec3 Normal;');
    Add('varying vec4 Position;');
    if AFogSupport then
    begin
      Add('varying float fogFactor;');
    end;
    Add('vec4 Ambient;');
    Add('vec4 Diffuse;');
    Add('vec4 Specular;');
    AddLightSub(Code);

    Add('void main(void) ');
    Add('{ ');
    Add('  vec4 TextureContrib = texture2D(MainTexture, gl_TexCoord[0].st); ');
    Add('  vec3 eye = vec3(0.0, 0.0, 1.0); ');
    Add('  Diffuse = vec4(0.0); ');
    Add('  Specular = vec4(0.0); ');
    Add('  Ambient = vec4(0.0); ');
    Add('  vec3 Pos = Position.xyz; ');
    Add('  vec3 N = normalize(Normal); ');
  end;
end;

procedure GetMLFragmentProgramCodeEnd(const Code: TStrings;
  const ARealisticSpecular: Boolean;
  AFogSupport: Boolean);
begin
  with Code do
  begin
    if ARealisticSpecular then
      Add('  gl_FragColor = LightIntensity * (TextureContrib * (gl_FrontLightModelProduct.sceneColor + Ambient * gl_FrontMaterial.ambient + Diffuse * gl_FrontMaterial.diffuse) + Specular * gl_FrontMaterial.specular); ')
    else
      Add('  gl_FragColor = LightIntensity * TextureContrib * (gl_FrontLightModelProduct.sceneColor + Ambient * gl_FrontMaterial.ambient + Diffuse * gl_FrontMaterial.diffuse + Specular * gl_FrontMaterial.specular); ');

    if AFogSupport then
      Add('  gl_FragColor = mix(gl_Fog.color, gl_FragColor, fogFactor);');

    Add('  gl_FragColor.a = TextureContrib.a; ');
    Add('} ');
  end;
end;


{ TgxBaseCustomGLSLDiffuseSpecular }

constructor TgxBaseCustomGLSLDiffuseSpecular.Create(
  AOwner: TComponent);
begin
  inherited;

  FLightPower     := 1;
  FFogSupport := sfsAuto;
  TStringList(VertexProgram.Code).OnChange := nil;
  TStringList(FragmentProgram.Code).OnChange := nil;
  VertexProgram.Enabled := true;
  FragmentProgram.Enabled := true;
end;

procedure TgxBaseCustomGLSLDiffuseSpecular.DoApply(
  var rci: TgxRenderContextInfo; Sender: TObject);
begin
  GetGXSLProg.UseProgramObject;
  Param['LightIntensity'].AsVector1f := FLightPower;
end;

function TgxBaseCustomGLSLDiffuseSpecular.DoUnApply(
  var rci: TgxRenderContextInfo): Boolean;
begin
  Result := False;
  GetGXSLProg.EndUseProgramObject;
end;

procedure TgxBaseCustomGLSLDiffuseSpecular.SetFogSupport(
  const Value: TgxShaderFogSupport);
begin
  if FFogSupport <> Value then
  begin
    FFogSupport := Value;
    Self.FinalizeShader;
  end;
end;

procedure TgxBaseCustomGLSLDiffuseSpecular.SetRealisticSpecular(
  const Value: Boolean);
begin
  if FRealisticSpecular <> Value then
  begin
    FRealisticSpecular := Value;
    Self.FinalizeShader;
  end;
end;


{ TgxBaseGLSLDiffuseSpecularShaderMT }

procedure TgxBaseGLSLDiffuseSpecularShaderMT.DoApply(
  var rci: TgxRenderContextInfo; Sender: TObject);
begin
  inherited;
  Param['MainTexture'].AsTexture2D[0] := FMainTexture;
end;

function TgxBaseGLSLDiffuseSpecularShaderMT.GetMainTextureName: TgxLibMaterialName;
begin
  Result := FMaterialLibrary.GetNameOfTexture(FMainTexture);
end;

function TgxBaseGLSLDiffuseSpecularShaderMT.GetMaterialLibrary: TgxAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TgxBaseGLSLDiffuseSpecularShaderMT.Notification(
  AComponent: TComponent; Operation: TOperation);
var
  Index: Integer;
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FMaterialLibrary then
      if FMaterialLibrary <> nil then
      begin
        //need to nil the textures that were ownned by it
        if FMainTexture <> nil then
        begin
          Index := FMaterialLibrary.Materials.GetTextureIndex(FMainTexture);
          if Index <> -1 then
            FMainTexture := nil;
        end;
        FMaterialLibrary := nil;
      end;
end;

procedure TgxBaseGLSLDiffuseSpecularShaderMT.SetMainTextureName(
  const Value: TgxLibMaterialName);
begin
  if FMaterialLibrary = nil then
  begin
    FMainTextureName := Value;
    if not (csLoading in ComponentState) then
      raise EgxslDiffuseSpecularShaderException.Create(strErrorEx + strMatLibNotDefined);
  end
  else
  begin
    FMainTexture := FMaterialLibrary.TextureByName(Value);
    FMainTextureName := '';
  end;
end;

procedure TgxBaseGLSLDiffuseSpecularShaderMT.SetMaterialLibrary(
  const Value: TgxMaterialLibrary);
begin
  if FMaterialLibrary <> nil then
    FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;

  if FMaterialLibrary <> nil then
  begin
    FMaterialLibrary.FreeNotification(Self);

    if FMainTextureName <> '' then
      SetMainTextureName(FMainTextureName);
  end
  else
    FMainTextureName := '';
end;

{ TgxCustomGLSLDiffuseSpecularShaderMT }

procedure TgxCustomGLSLDiffuseSpecularShaderMT.DoInitialize(var rci: TgxRenderContextInfo; Sender: TObject);
begin
  GetVertexProgramCode(VertexProgram.Code, IsFogEnabled(FFogSupport, rci), rci);
  GetFragmentProgramCode(FragmentProgram.Code, FRealisticSpecular, IsFogEnabled(FFogSupport, rci), rci);
  VertexProgram.Enabled := True;
  FragmentProgram.Enabled := True;
  inherited;
end;

{ TgxCustomGLSLDiffuseSpecularShader }

procedure TgxCustomGLSLDiffuseSpecularShader.DoApply(
  var rci: TgxRenderContextInfo; Sender: TObject);
begin
  inherited;
  Param['MainTexture'].AsVector1i := 0;  // Use the current texture.
end;

procedure TgxCustomGLSLDiffuseSpecularShader.DoInitialize(var rci: TgxRenderContextInfo; Sender: TObject);
begin
  GetVertexProgramCode(VertexProgram.Code, IsFogEnabled(FFogSupport, rci), rci);
  GetFragmentProgramCode(FragmentProgram.Code, FRealisticSpecular, IsFogEnabled(FFogSupport, rci), rci);
  VertexProgram.Enabled := True;
  FragmentProgram.Enabled := True;
  inherited;
end;

{ TgxCustomGLSLMLDiffuseSpecularShader }

constructor TgxCustomGLSLMLDiffuseSpecularShader.Create(
  AOwner: TComponent);
begin
  inherited;
end;

procedure TgxCustomGLSLMLDiffuseSpecularShader.DoApply(var rci: TgxRenderContextInfo; Sender: TObject);
var
  I: Integer;
  scene: TgxScene;
  needRecompile: Boolean;
begin
  scene := TgxScene(rci.scene);
  needRecompile := False;
  for I := 0 to scene.Lights.Count - 1 do
  begin
    if Assigned(scene.Lights[I]) then
    begin
      if FLightTrace[I].Enabled <> TgxLightSource(scene.Lights[I]).Shining then
      begin
        needRecompile := True;
        break;
      end;
      if FLightTrace[I].Style <> TgxLightSource(scene.Lights[I]).LightStyle then
      begin
        needRecompile := True;
        break;
      end;
    end
    else
      if FLightTrace[I].Enabled then
      begin
        needRecompile := True;
        break;
      end;
  end;
  if needRecompile then
  begin
    FinalizeShader;
    InitializeShader(rci, Sender);
  end;

  inherited;
  Param['MainTexture'].AsVector1i := 0;  // Use the current texture.
end;

procedure TgxCustomGLSLMLDiffuseSpecularShader.DoInitialize(var rci: TgxRenderContextInfo; Sender: TObject);
var
  I: Integer;
  scene: TgxScene;
begin
  GetVertexProgramCode(VertexProgram.Code, IsFogEnabled(FFogSupport, rci), rci);
  with FragmentProgram.Code do
  begin
    GetMLFragmentProgramCodeBeg(FragmentProgram.Code, IsFogEnabled(FFogSupport, rci));

    // Repeat for all lights.
    scene := TgxScene(rci.scene);
    for I := 0 to scene.Lights.Count - 1 do
    begin
      if Assigned(scene.Lights[I]) then
      begin
        FLightTrace[I].Enabled := TgxLightSource(scene.Lights[I]).Shining;
        FLightTrace[I].Style := TgxLightSource(scene.Lights[I]).LightStyle;
        if FLightTrace[I].Enabled then
          GetMLFragmentProgramCodeMid(FragmentProgram.Code, I, FLightTrace[I].Style);
      end
      else
        FLightTrace[I].Enabled := False;
    end;

    GetMLFragmentProgramCodeEnd(FragmentProgram.Code,
      FRealisticSpecular, IsFogEnabled(FFogSupport, rci));
  end;
  VertexProgram.Enabled := True;
  FragmentProgram.Enabled := True;
  inherited DoInitialize(rci, Sender);
end;

{ TgxCustomGLSLMLDiffuseSpecularShaderMT }

constructor TgxCustomGLSLMLDiffuseSpecularShaderMT.Create(
  AOwner: TComponent);
begin
  inherited;
end;

procedure TgxCustomGLSLMLDiffuseSpecularShaderMT.DoApply(
  var rci: TgxRenderContextInfo; Sender: TObject);
var
  I: Integer;
  scene: TgxScene;
  needRecompile: Boolean;
begin
  scene := TgxScene(rci.scene);
  needRecompile := False;
  for I := 0 to scene.Lights.Count - 1 do
  begin
    if Assigned(scene.Lights[I]) then
    begin
      if FLightTrace[I].Enabled <> TgxLightSource(scene.Lights[I]).Shining then
      begin
        needRecompile := True;
        break;
      end;
      if FLightTrace[I].Style <> TgxLightSource(scene.Lights[I]).LightStyle then
      begin
        needRecompile := True;
        break;
      end;
    end
    else
      if FLightTrace[I].Enabled then
      begin
        needRecompile := True;
        break;
      end;
  end;
  if needRecompile then
  begin
    FinalizeShader;
    InitializeShader(rci, Sender);
  end;

  inherited;
end;

procedure TgxCustomGLSLMLDiffuseSpecularShaderMT.DoInitialize(var rci: TgxRenderContextInfo; Sender: TObject);
var
  I: Integer;
  scene: TgxScene;
begin
  GetVertexProgramCode(VertexProgram.Code, IsFogEnabled(FFogSupport, rci), rci);
  with FragmentProgram.Code do
  begin
    GetMLFragmentProgramCodeBeg(FragmentProgram.Code, IsFogEnabled(FFogSupport, rci));

    // Repeat for all lights.
    scene := TgxScene(rci.scene);
    for I := 0 to scene.Lights.Count - 1 do
    begin
      if Assigned(scene.Lights[I]) then
      begin
        FLightTrace[I].Enabled := TgxLightSource(scene.Lights[I]).Shining;
        FLightTrace[I].Style := TgxLightSource(scene.Lights[I]).LightStyle;
        if FLightTrace[I].Enabled then
          GetMLFragmentProgramCodeMid(FragmentProgram.Code, I, FLightTrace[I].Style);
      end
      else
        FLightTrace[I].Enabled := False;
    end;

    GetMLFragmentProgramCodeEnd(FragmentProgram.Code,
      FRealisticSpecular, IsFogEnabled(FFogSupport, rci));
  end;
  VertexProgram.Enabled := True;
  FragmentProgram.Enabled := True;
  inherited;
end;

initialization
  RegisterClasses([
                  TgxCustomGLSLDiffuseSpecularShader,
                  TgxCustomGLSLDiffuseSpecularShaderMT,
                  TgxCustomGLSLMLDiffuseSpecularShader,
                  TgxCustomGLSLMLDiffuseSpecularShaderMT,

                  TgxSLDiffuseSpecularShader,
                  TgxSLDiffuseSpecularShaderMT,
                  TgxSLMLDiffuseSpecularShader,
                  TgxSLMLDiffuseSpecularShaderMT
                  ]);

end.

