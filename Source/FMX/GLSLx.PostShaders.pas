//
// The graphics platform GLArena https://github.com/glscene
//
unit GLSLx.PostShaders;

(* Post shaders that simulate shader visions for a mask or the entire scene *)

interface

{$I Scena.inc}

uses
  System.Classes,

  GLX.Texture,
  GLX.Scene,
  Scena.VectorTypes,
  Scena.VectorGeometry,
  GLX.Context,
  GLX.Material,
  GLSLx.Shader,
  GLSLx.CustomShader,
  GLX.RenderContextInfo,
  Scena.TextureFormat;

// Custom class for GLSLxPostBlurShader. A shader that blurs the entire scene }
type
  TgxCustomGLSLPostBlurShader = class(TgxCustomGLSLShader, IgxPostShader)
  private
    FThreshold: Single;
    // Implementing IPostShader.
    procedure DoUseTempTexture(const TempTexture: TgxTextureHandle;
      TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;
    function StoreThreshold: Boolean;
  protected
    procedure DoApply(var rci: TgxRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Threshold: Single read FThreshold write FThreshold stored StoreThreshold;
  end;

  TgxSLPostBlurShader = class(TgxCustomGLSLPostBlurShader)
  published
    property Threshold;
  end;

  // A shader that simulate a Thermal Vision of the entire scene
  TgxCustomGLSLPostThermalVisionShader = class(TgxCustomGLSLShader, IgxPostShader)
  private
    FThreshold : Single;
    Fintensity : Single;
    // Implementing IPostShader.
    procedure DoUseTempTexture(const TempTexture: TgxTextureHandle;TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;
    function StoreThreshold: Boolean;
    function StoreIntensity: Boolean;
  protected
    procedure DoApply(var rci: TgxRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
     property Threshold: Single read FThreshold write FThreshold stored StoreThreshold;
     property Intensity: Single read FIntensity write FIntensity stored StoreIntensity;
  end;

  TgxSLPostThermalVisionShader = class(TgxCustomGLSLPostThermalVisionShader)
  published
    property Threshold;
    property Intensity;
  end;

  // A shader that simulate a grayscale threshold vision (aka dream) of the entire scene
  TgxCustomGLSLPostDreamVisionShader = class(TgxCustomGLSLShader, IgxPostShader)
  private
    FThreshold : Single; // In percent 0..100;
    // Implementing IPostShader.
    procedure DoUseTempTexture(const TempTexture: TgxTextureHandle;TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;
    function StoreThreshold: Boolean;
  protected
    procedure DoApply(var rci: TgxRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Threshold: Single read FThreshold write FThreshold stored StoreThreshold;
  end;

  TgxSLPostDreamVisionShader = class(TgxCustomGLSLPostDreamVisionShader)
  published
    property Threshold;
  end;

  (* Custom shader that simulate a Night Vision of the scene throw a mask if enabled,
   or of the entire scene *)
  TgxCustomGLSLPostNightVisionShader = class(TgxCustomGLSLShader, IgxPostShader)
  private
    FMaterialLibrary: TgxAbstractMaterialLibrary;
    FLuminanceThreshold: Single;
    FColorAmplification:Single;
    FElapsedTime : Single;
    FUseMask : Integer;
    FNoiseTex : TgxTexture;
    FMaskTex : TgxTexture;
    FNoiseTexName  : TgxLibMaterialName;
    FMaskTexName        : TgxLibMaterialName;
    procedure DoUseTempTexture(const TempTexture: TgxTextureHandle;TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;
    function StoreLuminanceThreshold: Boolean;
    function StoreColorAmplification: Boolean;
    procedure SetMaskTexTexture(const Value: TgxTexture);
    procedure SetNoiseTexTexture(const Value: TgxTexture);
    function GetNoiseTexName: TgxLibMaterialName;
    procedure SetNoiseTexName(const Value: TgxLibMaterialName);
    function GetMaskTexName: TgxLibMaterialName;
    procedure SetMaskTexName(const Value: TgxLibMaterialName);
    function GetMaterialLibrary: TgxAbstractMaterialLibrary;
  protected
    procedure DoApply(var rci: TgxRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;
    procedure SetMaterialLibrary(const Value: TgxAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    property LuminanceThreshold: Single read FLuminanceThreshold write FLuminanceThreshold stored StoreLuminanceThreshold;
    property ColorAmplification: Single read FColorAmplification write FColorAmplification stored StoreColorAmplification;
    property ElapsedTime: Single read FElapsedTime write FElapsedTime stored false;
    property MaterialLibrary: TgxAbstractMaterialLibrary read getMaterialLibrary write SetMaterialLibrary;
    property NoiseTex: TgxTexture read FNoiseTex write SetNoiseTexTexture;
    property NoiseTexName: TgxLibMaterialName read GetNoiseTexName write SetNoiseTexName;
    property MaskTex: TgxTexture read FMaskTex write SetMaskTexTexture;
    property MaskTexName: TgxLibMaterialName read GetMaskTexName write SetMaskTexName;
    property UseMask : Integer read FUseMask write FUseMask;
  end;

  TgxSLPostNightVisionShader = class(TgxCustomGLSLPostNightVisionShader)
  published
    property LuminanceThreshold;
    property ColorAmplification;
    property ElapsedTime;
    property MaterialLibrary;
    property NoiseTexName;
    property MaskTexName;
    property UseMask;
  end;

  // Custom shader that pixelate of the entire scene
  TgxCustomGLSLPostPixelateShader = class(TgxCustomGLSLShader, IgxPostShader)
  private
    FPixelWidth  : Single;
    FPixelHeight : Single;
    procedure DoUseTempTexture(const TempTexture: TgxTextureHandle;TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;
    function StorePixelWidth: Boolean;
    function StorePixelHeight: Boolean;
  protected
    procedure DoApply(var rci: TgxRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    property PixelWidth: Single read FPixelWidth write FPixelWidth stored StorePixelWidth;
    property PixelHeight: Single read FPixelHeight write FPixelHeight stored StorePixelHeight;
  end;

  TgxSLPostPixelateShader = class(TgxCustomGLSLPostPixelateShader)
  published
    property PixelWidth;
    property PixelHeight;
  end;

  // Custom shader that posterize of the entire scene
  TgxCustomGLSLPostPosterizeShader = class(TgxCustomGLSLShader, IgxPostShader)
  private
    FGamma  : Single;
    FNumColors : Single;
    procedure DoUseTempTexture(const TempTexture: TgxTextureHandle;TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;
    function StoreGamma: Boolean;
    function StoreNumColors: Boolean;
  protected
    procedure DoApply(var rci: TgxRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
     property Gamma: Single read FGamma write FGamma stored StoreGamma;
     property NumColors: Single read FNumColors write FNumColors stored StoreNumColors;
  end;

  TgxSLPostPosterizeShader = class(TgxCustomGLSLPostPosterizeShader)
  published
    property Gamma;
    property NumColors;
  end;

  // Custom class for shader that frost of the entire scene
  TgxCustomGLSLPostFrostShader = class(TgxCustomGLSLShader, IgxPostShader)
  private
    FRandScale  : Single;
    FRandFactor : Single;
    procedure DoUseTempTexture(const TempTexture: TgxTextureHandle;TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;
    function StoreRandScale: Boolean;
    function StoreRandFactor: Boolean;
  protected
    procedure DoApply(var rci: TgxRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    property RandScale: Single read FRandScale write FRandScale stored StoreRandScale;
    property RandFactor: Single read FRandFactor write FRandFactor stored StoreRandFactor;
  end;

  TgxSLPostFrostShader = class(TgxCustomGLSLPostFrostShader)
  published
    property RandScale;
    property RandFactor;
  end;

  (* Custom class for GLSLPostTroubleShader.
   A shader that trouble of the entire scene. v2
   This Shader is experimental it can do smooth the scene or double the scene and it's
   depends of PixelX, PixelY and Freq values if they are less than 1 or greater
   the effects will be very different *)
  TgxCustomGLSLPostTroubleShader = class(TgxCustomGLSLShader, IgxPostShader)
  private
    FPixelX  : Single;
    FPixelY : Single;
    FFreq   : Single;
    FMaterialLibrary: TgxAbstractMaterialLibrary;
    FNoiseTex : TgxTexture;
    FNoiseTexName  : TgxLibMaterialName;
    procedure DoUseTempTexture(const TempTexture: TgxTextureHandle;TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;
    procedure SetNoiseTexTexture(const Value: TgxTexture);
    function GetNoiseTexName: TgxLibMaterialName;
    procedure SetNoiseTexName(const Value: TgxLibMaterialName);
    function GetMaterialLibrary: TgxAbstractMaterialLibrary;
    function StorePixelX: Boolean;
    function StorePixelY: Boolean;
    function StoreFreq: Boolean;
  protected
    procedure DoApply(var rci: TgxRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;
    procedure SetMaterialLibrary(const Value: TgxAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
     property PixelX: Single read FPixelX write FPixelX stored StorePixelX;
     property PixelY: Single read FPixelY write FPixelY stored StorePixelY;
     property Freq: Single read FFreq write FFreq stored StoreFreq;
     property MaterialLibrary: TgxAbstractMaterialLibrary read getMaterialLibrary write SetMaterialLibrary;
     property NoiseTex: TgxTexture read FNoiseTex write SetNoiseTexTexture;
     property NoiseTexName: TgxLibMaterialName read GetNoiseTexName write SetNoiseTexName;
  end;

  TgxSLPostTroubleShader = class(TgxCustomGLSLPostTroubleShader)
  published
    property PixelX;
    property PixelY;
    property Freq;
    property MaterialLibrary;
    property NoiseTexName;
  end;

//----------------------------------------------------------------------
implementation
//----------------------------------------------------------------------

//-------------------------------------
// TgxCustomGLSLPostBlurShader
//-------------------------------------

constructor TgxCustomGLSLPostBlurShader.Create(
  AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    Add('varying vec2 vTexCoord; ');
    Add(' ');
    Add('void main(void) ');
    Add('{ '); 
    Add(' '); 
    Add('   // Clean up inaccuracies '); 
    Add('   vec2 Position; '); 
    Add('   Position.xy = sign(gl_Vertex.xy); '); 
    Add(' '); 
    Add('   gl_Position = vec4(Position.xy, 0.0, 1.0); '); 
    Add('   vTexCoord = Position.xy *.5 + .5; '); 
    Add('    ');
    Add('} '); 
  end;

  with FragmentProgram.Code do
  begin
    Add('uniform float threshold; ');
    Add('uniform vec2 ScreenExtents; ');
    Add('uniform sampler2DRect Image; ');
    Add(' ');
    Add('varying vec2 vTexCoord; ');
    Add(' ');
    Add('void main() ');
    Add('{ ');
    Add('   ');
    Add('   vec2 samples[8]; ');
    Add('   vec2 vTexCoordScr = vTexCoord * ScreenExtents; ');
    Add('    ');
    Add('   samples[0]  = vTexCoordScr + vec2(-1.0, -1.0); ');
    Add('   samples[1]  = vTexCoordScr + vec2( 0.0, -1.0); ');
    Add('   samples[2]  = vTexCoordScr + vec2( 1.0, -1.0); ');
    Add('   samples[3]  = vTexCoordScr + vec2(-1.0,  0.0); ');
    Add('   samples[4]  = vTexCoordScr + vec2( 1.0,  0.0); ');
    Add('   samples[5]  = vTexCoordScr + vec2(-1.0,  1.0); ');
    Add('   samples[6]  = vTexCoordScr + vec2( 0.0,  1.0); ');
    Add('   samples[7]  = vTexCoordScr + vec2( 1.0,  1.0); ');
    Add(' ');
    Add('   vec4 sample = texture2DRect(Image, vTexCoordScr); ');
    Add(' ');
    Add('   // Neighborhood average ');
    Add('   vec4 avg = sample; ');
    Add('   for (int i = 0; i < 8; i++) ');
    Add('   { ');
    Add('      avg += texture2DRect(Image,  samples[i]); ');
    Add('   } ');
    Add('    ');
    Add(' ');
    Add('   avg /= 9.0; ');
    Add(' ');
    Add('   // If the difference between the average and the sample is ');
    Add('   // large, we''ll assume it''s noise. ');
    Add('   vec4  diff = abs(sample - avg); ');
    Add('   float sel  = float(dot(diff, vec4(0.25)) > threshold); ');
    Add(' ');
    Add('   gl_FragColor =  mix(sample, avg, sel); ');
    Add('} '); 
  end;
  FThreshold := 0.1;
end;

procedure TgxCustomGLSLPostBlurShader.DoApply(
  var rci: TgxRenderContextInfo; Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  GetGLSLProg.Uniform1f['threshold'] := FThreshold;
  GetGLSLProg.Uniform2f['ScreenExtents'] := Vector2fMake(rci.viewPortSize.cx, rci.viewPortSize.cy);
end;

function TgxCustomGLSLPostBlurShader.DoUnApply(
  var rci: TgxRenderContextInfo): Boolean;
begin
  rci.gxStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TgxCustomGLSLPostBlurShader.DoUseTempTexture(
  const TempTexture: TgxTextureHandle; TextureTarget: TGLTextureTarget);
begin
  Param['Image'].AsCustomTexture[2, TextureTarget] := TempTexture.Handle;
end;

function TgxCustomGLSLPostBlurShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect;
end;

function TgxCustomGLSLPostBlurShader.StoreThreshold: Boolean;
begin
  Result := Abs(FThreshold - 0.1) > 0.00001;
end;

//-------------------------------------------
// TgxCustomGLSLPostThermalVisionShader
//-------------------------------------------

constructor TgxCustomGLSLPostThermalVisionShader.Create(
  AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    Add('varying vec2 vTexCoord; ');
    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add(' ');
    Add('   // Clean up inaccuracies ');
    Add('   vec2 Position; ');
    Add('   Position.xy = sign(gl_Vertex.xy); ');
    Add(' ');
    Add('   gl_Position = vec4(Position.xy, 0.0, 1.0); ');
    Add('   vTexCoord = Position.xy *.5 + .5; ');
    Add('    ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
   Add('uniform float Threshold; ');
   Add('uniform float Intensity; ');
   Add('uniform vec2 ScreenExtents; ');
   Add('uniform sampler2DRect ScreenTex; ');
   Add(' ');
   Add('varying vec2 vTexCoord; ');
   Add(' ');
   Add('void main() ');
   Add('{ ');
   Add('   vec2 uv = vTexCoord * ScreenExtents; ');
   Add('   vec3 tc = vec3(1.0, 0.0, 0.0);');
   Add('   vec3 pixcol = texture2DRect(ScreenTex, uv).rgb; ');
   Add('   vec3 colors[3];');
   Add('   colors[0] = vec3(0.,0.,1.); ');
   Add('   colors[1] = vec3(1.,1.,0.); ');
   Add('   colors[2] = vec3(1.,0.,0.); ');
   Add('   float lum = dot(vec3(0.30, 0.59, 0.11), pixcol.rgb); ');
   Add('//   float lum = (pixcol.r+pixcol.g+pixcol.b)/3.;');
   Add('   tc = (lum < 0.5)?  mix(colors[0],colors[1],lum/Threshold): mix(colors[1],colors[2],(lum-Intensity)/Threshold); ');
   Add('   gl_FragColor = vec4(tc, 1); ');
   Add('} ');

  end;

  FThreshold  := 0.5;
  FIntensity  := 0.5;
end;

procedure TgxCustomGLSLPostThermalVisionShader.DoApply(
  var rci: TgxRenderContextInfo; Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  GetGLSLProg.Uniform1f['Threshold'] := FThreshold;
  GetGLSLProg.Uniform1f['Intensity'] := FIntensity;
  GetGLSLProg.Uniform2f['ScreenExtents'] := Vector2fMake(rci.viewPortSize.cx, rci.viewPortSize.cy);

end;

function TgxCustomGLSLPostThermalVisionShader.DoUnApply(
  var rci: TgxRenderContextInfo): Boolean;
begin
  rci.gxStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TgxCustomGLSLPostThermalVisionShader.DoUseTempTexture(
  const TempTexture: TgxTextureHandle; TextureTarget: TGLTextureTarget);
begin
  Param['ScreenTex'].AsCustomTexture[3, TextureTarget] := TempTexture.Handle;
end;

function TgxCustomGLSLPostThermalVisionShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect; //ttTexture2D;
end;

function TgxCustomGLSLPostThermalVisionShader.StoreThreshold: Boolean;
begin
  Result := (Abs(FThreshold) > 0) and (Abs(FThreshold) <= 1.0);
end;

function TgxCustomGLSLPostThermalVisionShader.StoreIntensity: Boolean;
begin
  Result := (Abs(FIntensity) >= 0) and (Abs(FIntensity) <= 2.0);
end;

{ TgxCustomGLSLPostThermalVisionShader }

constructor TgxCustomGLSLPostDreamVisionShader.Create(
  AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    Add('varying vec2 vTexCoord; ');
    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add(' ');
    Add('   // Clean up inaccuracies ');
    Add('   vec2 Position; ');
    Add('   Position.xy = sign(gl_Vertex.xy); ');
    Add(' ');
    Add('   gl_Position = vec4(Position.xy, 0.0, 1.0); ');
    Add('   vTexCoord = Position.xy *.5 + .5; ');
    Add('    ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
   Add('uniform float Threshold; ');
   Add('uniform vec2 ScreenExtents; ');
   Add('uniform sampler2DRect ScreenTex; ');
   Add(' ');
   Add('varying vec2 vTexCoord; ');
   Add(' ');
   Add('void main() ');
   Add('{ ');
   Add('   vec2 uv = vTexCoord * ScreenExtents; ');
   Add('   vec3 c = texture2DRect(ScreenTex, uv).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv+0.001).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv+0.003).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv+0.005).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv+0.007).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv+0.009).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv+0.011).rgb; ');
   Add(' ');
   Add('   c += texture2DRect(ScreenTex, uv-0.001).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv-0.003).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv-0.005).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv-0.007).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv-0.009).rgb; ');
   Add('   c += texture2DRect(ScreenTex, uv-0.011).rgb; ');
   Add(' ');
   Add('   c.rgb = vec3((c.r+c.g+c.b)/3.0); ');
   Add('   c = c / Threshold; ');
   Add('   gl_FragColor = vec4(c,1.0); ');
   Add('} ');

  end;

  FThreshold  := 5;
end;

procedure TgxCustomGLSLPostDreamVisionShader.DoApply(
  var rci: TgxRenderContextInfo; Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  GetGLSLProg.Uniform1f['Threshold'] := (FThreshold*255)/100;
  GetGLSLProg.Uniform2f['ScreenExtents'] := Vector2fMake(rci.viewPortSize.cx, rci.viewPortSize.cy);

end;

function TgxCustomGLSLPostDreamVisionShader.DoUnApply(
  var rci: TgxRenderContextInfo): Boolean;
begin
  rci.gxStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TgxCustomGLSLPostDreamVisionShader.DoUseTempTexture(
  const TempTexture: TgxTextureHandle; TextureTarget: TGLTextureTarget);
begin
  Param['ScreenTex'].AsCustomTexture[2, TextureTarget] := TempTexture.Handle;
end;

function TgxCustomGLSLPostDreamVisionShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect; //ttTexture2D;
end;

function TgxCustomGLSLPostDreamVisionShader.StoreThreshold: Boolean;
begin
  Result := (Abs(FThreshold) > 0) and (Abs(FThreshold) <= 100);
end;

{ TgxCustomGLSLPostThermalVisionShader }

constructor TgxCustomGLSLPostNightVisionShader.Create(
  AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    Add('varying vec2 vTexCoord; ');
    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add(' ');
    Add('   // Clean up inaccuracies ');
    Add('   vec2 Position; ');
    Add('   Position.xy = sign(gl_Vertex.xy); ');
    Add(' ');
    Add('   gl_Position = vec4(Position.xy, 0.0, 1.0); ');
    Add('   vTexCoord = Position.xy *.5 + .5; ');
    Add('    ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
   Add('uniform float luminanceThreshold; ');
   Add('uniform float colorAmplification; ');
   Add('uniform float elapsedTime; ');
   Add('uniform int useMask;');
   Add('uniform vec2 ScreenExtents; ');
   Add('uniform sampler2D noiseTex; ');
   Add('uniform sampler2D maskTex; ');
   Add('uniform sampler2DRect ScreenTex; ');
   Add(' ');
   Add('varying vec2 vTexCoord; ');
   Add(' ');
   Add('void main () ');
   Add('{ ');
   Add('  vec2 uv = vTexCoord * ScreenExtents; ');
   Add('  vec4 finalColor; ');
   Add('  vec2 uvv; ');
   Add('  uvv.x = 0.4*sin(elapsedTime*50.0); ');
   Add('  uvv.y = 0.4*cos(elapsedTime*50.0); ');
   Add('  float m = 1; ');
   Add('  if (useMask==1) { m = texture2D(maskTex, vTexCoord.st).r; } ');  // Problem Here I don't know how to solve ????
   Add('  vec3 n = texture2D(noiseTex,(uv.st*3.5) + uvv).rgb; ');
   Add('  vec3 c = texture2DRect(ScreenTex, uv.st+(n.xy*0.005)).rgb; ');
   Add('    float lum = dot(vec3(0.30, 0.59, 0.11), c); ');
   Add('    if (lum < luminanceThreshold) ');
   Add('        c *= colorAmplification; ');
   Add('  vec3 visionColor = vec3(0.1, 0.95, 0.2); ');
   Add('  finalColor.rgb = (c + (n*0.2)) * visionColor * m; ');
   Add(' ');
   Add('  gl_FragColor.rgb = finalColor.rgb; ');
   Add('  gl_FragColor.a = 1.0; ');
   Add('} ');

  end;

  FLuminanceThreshold := 0.2;
  FColorAmplification := 4.0;
  FElapsedTime:=0.1;
  FUseMask:=0; // Shader not working if we want to use mask

end;

procedure TgxCustomGLSLPostNightVisionShader.DoApply(var rci: TgxRenderContextInfo; Sender: TObject);
begin

  GetGLSLProg.UseProgramObject;
  param['luminanceThreshold'].AsVector1f := FLuminanceThreshold;
  param['colorAmplification'].AsVector1f := FColorAmplification;
  param['elapsedTime'].AsVector1f := FElapsedTime;
  param['useMask'].AsVector1i := FUseMask;
  param['ScreenExtents'].AsVector2f := Vector2fMake(rci.viewPortSize.cx, rci.viewPortSize.cy);
  param['noiseTex'].AsTexture2D[1]:= FNoiseTex;
  param['maskTex'].AsTexture2D[2]:= FMaskTex;
end;

function TgxCustomGLSLPostNightVisionShader.DoUnApply(
  var rci: TgxRenderContextInfo): Boolean;
begin
  rci.gxStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TgxCustomGLSLPostNightVisionShader.DoUseTempTexture(
  const TempTexture: TgxTextureHandle; TextureTarget: TGLTextureTarget);
begin
  Param['ScreenTex'].AsCustomTexture[7, TextureTarget] := TempTexture.Handle;
end;

function TgxCustomGLSLPostNightVisionShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect; //ttTexture2D;
end;


function TgxCustomGLSLPostNightVisionShader.StoreLuminanceThreshold: Boolean;
begin
  Result := Abs(FLuminanceThreshold - 0.1) > 0.00001;
end;

function TgxCustomGLSLPostNightVisionShader.StoreColorAmplification: Boolean;
begin
  Result := Abs(FColorAmplification - 0.1) > 0.00001;
end;


procedure TgxCustomGLSLPostNightVisionShader.SetMaskTexTexture(const Value: TgxTexture);
begin
  if FMaskTex = Value then Exit;
  MaskTex := Value;
  NotifyChange(Self)
end;

procedure TgxCustomGLSLPostNightVisionShader.SetNoiseTexTexture(const Value: TgxTexture);
begin
  if FNoiseTex = Value then Exit;
  NoiseTex := Value;
  NotifyChange(Self);
end;

function TgxCustomGLSLPostNightVisionShader.GetNoiseTexName: TgxLibMaterialName;
begin
  Result := TgxMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FNoiseTex);
  if Result = '' then Result := FNoiseTexName;
end;

procedure TgxCustomGLSLPostNightVisionShader.SetNoiseTexName(const Value: TgxLibMaterialName);
begin
  //Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FNoiseTexName = Value then Exit;
  FNoiseTexName  := Value;
  FNoiseTex := TgxMaterialLibrary(FMaterialLibrary).TextureByName(FNoiseTexName);
  NotifyChange(Self);
end;

function TgxCustomGLSLPostNightVisionShader.GetMaskTexName: TgxLibMaterialName;
begin
  Result := TgxMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FMaskTex);
  if Result = '' then Result := FMaskTexName;
end;

procedure TgxCustomGLSLPostNightVisionShader.SetMaskTexName(const Value: TgxLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FMaskTexName = Value then Exit;
  FMaskTexName  := Value;

  FMaskTex := TgxMaterialLibrary(FMaterialLibrary).TextureByName(FMaskTexName);
  NotifyChange(Self);
end;

function TgxCustomGLSLPostNightVisionShader.GetMaterialLibrary: TgxAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TgxCustomGLSLPostNightVisionShader.SetMaterialLibrary(const Value: TgxAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TgxAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TgxCustomGLSLPostNightVisionShader.Notification(AComponent: TComponent; Operation: TOperation);
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

        if FMaskTex <> nil then
        begin
          Index := TgxMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FMaskTex);
          if Index <> -1 then
            SetMaskTexTexture(nil);
        end;

        FMaterialLibrary := nil;
      end;
end;

//----------------------------------------
// TgxCustomGLSLPostPixelateShader
//----------------------------------------

constructor TgxCustomGLSLPostPixelateShader.Create(
  AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    Add('varying vec2 vTexCoord; ');
    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add(' ');
    Add('   // Clean up inaccuracies ');
    Add('   vec2 Position; ');
    Add('   Position.xy = sign(gl_Vertex.xy); ');
    Add(' ');
    Add('   gl_Position = vec4(Position.xy, 0.0, 1.0); ');
    Add('   vTexCoord = Position.xy *.5 + .5; ');
    Add('    ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
   Add('uniform float pixel_w; // 8.0 ');
   Add('uniform float pixel_h; // 8.0 ');
   Add('uniform vec2 ScreenExtents; ');
   Add('uniform sampler2DRect ScreenTex; ');
   Add(' ');
   Add('varying vec2 vTexCoord; ');
   Add(' ');
   Add('void main() ');
   Add('{ ');
   Add('   vec2 uv = vTexCoord * ScreenExtents; ');
   Add('   vec3 tc = vec3(1.0, 0.0, 0.0); ');
   Add('   vec2 coord = vec2(pixel_w*floor(uv.x/pixel_w),pixel_h*floor(uv.y/pixel_h)); ');
   Add('   tc = texture2DRect(ScreenTex, coord).rgb; ');
   Add('   gl_FragColor = vec4(tc, 1); ');
   Add('} ');

  end;

  FPixelWidth  := 8;
  FPixelHeight := 12;
end;

procedure TgxCustomGLSLPostPixelateShader.DoApply(
  var rci: TgxRenderContextInfo; Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  GetGLSLProg.Uniform1f['pixel_w'] := FPixelWidth;
  GetGLSLProg.Uniform1f['pixel_h'] := FPixelHeight;
  GetGLSLProg.Uniform2f['ScreenExtents'] := Vector2fMake(rci.viewPortSize.cx, rci.viewPortSize.cy);

end;

function TgxCustomGLSLPostPixelateShader.DoUnApply(
  var rci: TgxRenderContextInfo): Boolean;
begin
  rci.gxStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TgxCustomGLSLPostPixelateShader.DoUseTempTexture(
  const TempTexture: TgxTextureHandle; TextureTarget: TGLTextureTarget);
begin
  Param['ScreenTex'].AsCustomTexture[3, TextureTarget] := TempTexture.Handle;
end;

function TgxCustomGLSLPostPixelateShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect; //ttTexture2D;
end;

function TgxCustomGLSLPostPixelateShader.StorePixelWidth: Boolean;
begin
  Result := (Abs(FPixelWidth) > 0) and (Abs(FPixelWidth) <= 64);
end;

function TgxCustomGLSLPostPixelateShader.StorePixelHeight: Boolean;
begin
  Result := (Abs(FPixelHeight) > 0) and (Abs(FPixelHeight) <= 64);
end;

{ TgxCustomGLSLPostPosterizeShader }

constructor TgxCustomGLSLPostPosterizeShader.Create(
  AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    Add('varying vec2 vTexCoord; ');
    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add(' ');
    Add('   // Clean up inaccuracies ');
    Add('   vec2 Position; ');
    Add('   Position.xy = sign(gl_Vertex.xy); ');
    Add(' ');
    Add('   gl_Position = vec4(Position.xy, 0.0, 1.0); ');
    Add('   vTexCoord = Position.xy *.5 + .5; ');
    Add('    ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
   Add('uniform float gamma; // 8.0 ');
   Add('uniform float numColors; // 8.0 ');
   Add('uniform vec2 ScreenExtents; ');
   Add('uniform sampler2DRect ScreenTex; ');
   Add(' ');
   Add('varying vec2 vTexCoord; ');
   Add(' ');
   Add('void main() ');
   Add('{ ');
   Add('   vec2 uv = vTexCoord * ScreenExtents; ');
   Add('   vec3 c = texture2DRect(ScreenTex, uv.xy).rgb; ');
   Add('   c = pow(c, vec3(gamma, gamma, gamma)); ');
   Add('   c = c * numColors; ');
   Add('   c = floor(c); ');
   Add('   c = c / numColors; ');
   Add('   c = pow(c, vec3(1.0/gamma)); ');
   Add('   gl_FragColor = vec4(c, 1.0); ');
   Add('} ');

  end;

  FGamma  := 0.6;
  FNumColors := 8;
end;

procedure TgxCustomGLSLPostPosterizeShader.DoApply(
  var rci: TgxRenderContextInfo; Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  GetGLSLProg.Uniform1f['gamma'] := FGamma;
  GetGLSLProg.Uniform1f['numColors'] := FNumColors;
  GetGLSLProg.Uniform2f['ScreenExtents'] := Vector2fMake(rci.viewPortSize.cx, rci.viewPortSize.cy);

end;

function TgxCustomGLSLPostPosterizeShader.DoUnApply(
  var rci: TgxRenderContextInfo): Boolean;
begin
  rci.gxStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TgxCustomGLSLPostPosterizeShader.DoUseTempTexture(
  const TempTexture: TgxTextureHandle; TextureTarget: TGLTextureTarget);
begin
  Param['ScreenTex'].AsCustomTexture[3, TextureTarget] := TempTexture.Handle;
end;

function TgxCustomGLSLPostPosterizeShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect; //ttTexture2D;
end;

function TgxCustomGLSLPostPosterizeShader.StoreGamma: Boolean;
begin
  Result := (Abs(FGamma) > 0) and (Abs(FGamma) <= 3.0);
end;

function TgxCustomGLSLPostPosterizeShader.StoreNumColors: Boolean;
begin
  Result := (Abs(FNumColors) > 0) and (Abs(FNumColors) <= 255);
end;

{ TgxCustomGLSLPostFrostShader }

constructor TgxCustomGLSLPostFrostShader.Create(
  AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    Add('varying vec2 vTexCoord; ');
    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add(' ');
    Add('   // Clean up inaccuracies ');
    Add('   vec2 Position; ');
    Add('   Position.xy = sign(gl_Vertex.xy); ');
    Add(' ');
    Add('   gl_Position = vec4(Position.xy, 0.0, 1.0); ');
    Add('   vTexCoord = Position.xy *.5 + .5; ');
    Add('    ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
   Add('uniform float rnd_scale; // 250 ');
   Add('uniform float rnd_factor; // 50 ');
//   Add('uniform vec2 v1; ');
//   Add('uniform vec2 v2; ');
   Add('uniform vec2 ScreenExtents; ');
   Add('uniform sampler2DRect ScreenTex; ');
   Add(' ');
   Add('varying vec2 vTexCoord; ');
   Add(' ');
   Add('float rand(vec2 co) ');
   Add('{ ');
   Add('  vec2 v1 = vec2(92.,80.); ');
   Add('  vec2 v2 = vec2(41.,62.); ');
   Add('  return fract(sin(dot(co.xy ,v1)) + cos(dot(co.xy ,v2)) * rnd_scale); ');
   Add('} ');
   Add(' ');
   Add('void main() ');
   Add('{ ');
   Add('  vec2 uv = vTexCoord * ScreenExtents; ');
   Add('  vec3 tc = vec3(1.0, 0.0, 0.0); ');
   Add('  vec2 rnd = vec2(rand(uv.xy),rand(uv.yx)); ');
   Add('  tc = texture2DRect(ScreenTex, uv+rnd*rnd_factor).rgb; ');
   Add('  gl_FragColor = vec4(tc, 1.0); ');
   Add('} ');

  end;

  FRandScale  := 50;
  FRandFactor := 50;
end;

procedure TgxCustomGLSLPostFrostShader.DoApply(
  var rci: TgxRenderContextInfo; Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  GetGLSLProg.Uniform1f['rnd_scale'] := FRandScale;
  GetGLSLProg.Uniform1f['rnd_factor'] := FRandFactor;
  GetGLSLProg.Uniform2f['ScreenExtents'] := Vector2fMake(rci.viewPortSize.cx, rci.viewPortSize.cy);

end;

function TgxCustomGLSLPostFrostShader.DoUnApply(
  var rci: TgxRenderContextInfo): Boolean;
begin
  rci.gxStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TgxCustomGLSLPostFrostShader.DoUseTempTexture(
  const TempTexture: TgxTextureHandle; TextureTarget: TGLTextureTarget);
begin
  Param['ScreenTex'].AsCustomTexture[3, TextureTarget] := TempTexture.Handle;
end;

function TgxCustomGLSLPostFrostShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect; //ttTexture2D;
end;

function TgxCustomGLSLPostFrostShader.StoreRandScale: Boolean;
begin
  Result := (Abs(FRandScale) > 0) and (Abs(FRandScale) <= 1000);
end;

function TgxCustomGLSLPostFrostShader.StoreRandFactor: Boolean;
begin
  Result := (Abs(FRandFactor) > 0) and (Abs(FRandFactor) <= 1000);
end;

//----------------------------------------
// TgxCustomGLSLPostTroubleShader
//----------------------------------------

constructor TgxCustomGLSLPostTroubleShader.Create( AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    Add('varying vec2 vTexCoord; ');
    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add(' ');
    Add('   // Clean up inaccuracies ');
    Add('   vec2 Position; ');
    Add('   Position.xy = sign(gl_Vertex.xy); ');
    Add(' ');
    Add('   gl_Position = vec4(Position.xy, 0.0, 1.0); ');
    Add('   vTexCoord = Position.xy *.5 + .5; ');
    Add('    ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
   Add('uniform float PixelX; // = 2.0; ');
   Add('uniform float PixelY; // = 2.0; ');
   Add('uniform float Freq; // = 0.115; ');
   Add('uniform vec2 ScreenExtents; ');
   Add('uniform sampler2D noiseTex; '); // 1
   Add('uniform sampler2DRect ScreenTex; ');
   Add(' ');
   Add('varying vec2 vTexCoord; ');
   Add(' ');
   Add('vec4 spline(float x, vec4 c1, vec4 c2, vec4 c3, vec4 c4, vec4 c5, vec4 c6, vec4 c7, vec4 c8, vec4 c9) ');
   Add('{ ');
   Add('  float w1, w2, w3, w4, w5, w6, w7, w8, w9; ');
   Add('  w1 = 0.0; ');
   Add('  w2 = 0.0; ');
   Add('  w3 = 0.0; ');
   Add('  w4 = 0.0; ');
   Add('  w5 = 0.0; ');
   Add('  w6 = 0.0; ');
   Add('  w7 = 0.0; ');
   Add('  w8 = 0.0; ');
   Add('  w9 = 0.0; ');
   Add('  float tmp = x * 8.0; ');
   Add('  if (tmp<=1.0) { ');
   Add('    w1 = 1.0 - tmp; ');
   Add('    w2 = tmp; ');
   Add('  } ');
   Add('  else if (tmp<=2.0) { ');
   Add('    tmp = tmp - 1.0; ');
   Add('    w2 = 1.0 - tmp; ');
   Add('    w3 = tmp; ');
   Add('  } ');
   Add('  else if (tmp<=3.0) { ');
   Add('    tmp = tmp - 2.0; ');
   Add('    w3 = 1.0-tmp; ');
   Add('    w4 = tmp; ');
   Add('  } ');
   Add('  else if (tmp<=4.0) { ');
   Add('    tmp = tmp - 3.0; ');
   Add('    w4 = 1.0-tmp; ');
   Add('    w5 = tmp; ');
   Add('  } ');
   Add('  else if (tmp<=5.0) { ');
   Add('    tmp = tmp - 4.0; ');
   Add('    w5 = 1.0-tmp; ');
   Add('    w6 = tmp; ');
   Add('  } ');
   Add('  else if (tmp<=6.0) { ');
   Add('    tmp = tmp - 5.0; ');
   Add('    w6 = 1.0-tmp; ');
   Add('    w7 = tmp; ');
   Add('  } ');
   Add('  else if (tmp<=7.0) { ');
   Add('    tmp = tmp - 6.0; ');
   Add('    w7 = 1.0 - tmp; ');
   Add('    w8 = tmp; ');
   Add('  } ');
   Add('  else  ');
   Add('  { ');
   Add('    //tmp = saturate(tmp - 7.0); ');
    // http://www.ozone3d.net/blogs/lab/20080709/saturate-function-in-glsl/
   Add('    tmp = clamp(tmp - 7.0, 0.0, 1.0); ');
   Add('    w8 = 1.0-tmp; ');
   Add('    w9 = tmp; ');
   Add('  } ');
   Add('  return w1*c1 + w2*c2 + w3*c3 + w4*c4 + w5*c5 + w6*c6 + w7*c7 + w8*c8 + w9*c9; ');
   Add('} ');
   Add(' ');
   Add('vec3 NOISE2D(vec2 p) ');
   Add('  { return texture2D(noiseTex,p).xyz; } ');
   Add(' ');
   Add('void main() ');
   Add('{ ');
   Add('  vec2 uv = vTexCoord * ScreenExtents; ');
   Add('  vec3 tc = vec3(1.0, 0.0, 0.0); ');
   Add('  float DeltaX = PixelX; ');
   Add('  float DeltaY = PixelY; ');
   Add('  vec2 ox = vec2(DeltaX,0.0); ');
   Add('  vec2 oy = vec2(0.0,DeltaY); ');
   Add('  vec2 PP = uv - oy; ');
   Add('  vec4 C00 = texture2DRect(ScreenTex,PP - ox); ');
   Add('  vec4 C01 = texture2DRect(ScreenTex,PP); ');
   Add('  vec4 C02 = texture2DRect(ScreenTex,PP + ox); ');
   Add('  PP = uv; ');
   Add('  vec4 C10 = texture2DRect(ScreenTex,PP - ox); ');
   Add('  vec4 C11 = texture2DRect(ScreenTex,PP); ');
   Add('  vec4 C12 = texture2DRect(ScreenTex,PP + ox); ');
   Add('  PP = uv + oy; ');
   Add('  vec4 C20 = texture2DRect(ScreenTex,PP - ox); ');
   Add('  vec4 C21 = texture2DRect(ScreenTex,PP); ');
   Add('  vec4 C22 = texture2DRect(ScreenTex,PP + ox); ');
   Add('  float n = NOISE2D(Freq*uv).x; ');
   Add('  n = mod(n, 0.111111)/0.111111; ');
   Add('  vec4 result = spline(n,C00,C01,C02,C10,C11,C12,C20,C21,C22); ');
   Add('  tc = result.rgb; ');
   Add('  gl_FragColor = vec4(tc, 1.0); ');
   Add('} ');
  end;

  FPixelX  := 0.5;
  FPixelY := 0.5;
  FFreq:= 2.115;
end;

procedure TgxCustomGLSLPostTroubleShader.DoApply(
  var rci: TgxRenderContextInfo; Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  GetGLSLProg.Uniform1f['PixelX'] := FPixelX;
  GetGLSLProg.Uniform1f['PixelY'] := FPixelY;
  GetGLSLProg.Uniform1f['Freq'] := FFreq;
  GetGLSLProg.Uniform2f['ScreenExtents'] := Vector2fMake(rci.viewPortSize.cx, rci.viewPortSize.cy);

  param['noiseTex'].AsTexture2D[1]:= FNoiseTex;

end;

function TgxCustomGLSLPostTroubleShader.DoUnApply(
  var rci: TgxRenderContextInfo): Boolean;
begin
  rci.gxStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TgxCustomGLSLPostTroubleShader.DoUseTempTexture(
  const TempTexture: TgxTextureHandle; TextureTarget: TGLTextureTarget);
begin
  Param['ScreenTex'].AsCustomTexture[5, TextureTarget] := TempTexture.Handle;
end;

function TgxCustomGLSLPostTroubleShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect; //ttTexture2D;
end;

function TgxCustomGLSLPostTroubleShader.StorePixelX: Boolean;
begin
  Result := (Abs(FPixelX) > 0) and (Abs(FPixelX) <= 1000);
end;

function TgxCustomGLSLPostTroubleShader.StorePixelY: Boolean;
begin
  Result := (Abs(FPixelY) > 0) and (Abs(FPixelY) <= 1000);
end;

function TgxCustomGLSLPostTroubleShader.StoreFreq: Boolean;
begin
  Result := (Abs(FPixelY) > 0) and (Abs(FPixelY) <= 5.0);
end;

procedure TgxCustomGLSLPostTroubleShader.SetNoiseTexTexture(const Value: TgxTexture);
begin
  if FNoiseTex = Value then Exit;
  NoiseTex := Value;
  NotifyChange(Self);
end;

function TgxCustomGLSLPostTroubleShader.GetNoiseTexName: TgxLibMaterialName;
begin
  Result := TgxMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FNoiseTex);
  if Result = '' then Result := FNoiseTexName;
end;

procedure TgxCustomGLSLPostTroubleShader.SetNoiseTexName(const Value: TgxLibMaterialName);
begin
  //Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FNoiseTexName = Value then Exit;
  FNoiseTexName  := Value;
  FNoiseTex := TgxMaterialLibrary(FMaterialLibrary).TextureByName(FNoiseTexName);
  NotifyChange(Self);
end;

function TgxCustomGLSLPostTroubleShader.GetMaterialLibrary: TgxAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TgxCustomGLSLPostTroubleShader.SetMaterialLibrary(const Value: TgxAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TgxAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TgxCustomGLSLPostTroubleShader.Notification(AComponent: TComponent; Operation: TOperation);
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

        FMaterialLibrary := nil;
      end;
end;

end.

