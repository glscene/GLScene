//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLSL.PostShaders;

(*
  Post shaders that simulate shader visions for a mask or the entire scene:
    - TLSLPostBlurShader;
    - TGLSLPostThermalVisionShader;
    - TGLSLPostDreamVisionShader;
    - TGLSLPostNightVisionShader;
    - TGLSLPostPixelateShader;
    - TGLSLPostPosterizeShader
    - TGLSLPostFrostShader;
    - TGLSLPostTroubleShader;
*)

interface

{$I GLScene.inc}

uses
  System.Classes,

  GLS.OpenGLTokens,
  GLS.Texture,
  GLS.Scene,
  GLS.State,
  GLS.VectorGeometry,
  GLS.RenderContextInfo,
  GLS.TextureFormat,
  GLS.Context,
  GLS.Material,
  GLSL.Shader,
  GLSL.CustomShader,
  GLS.VectorTypes;

type
  // Custom class for GLSLPostBlurShader. A shader that blurs the entire scene
  TGLCustomGLSLPostBlurShader = class(TGLCustomGLSLShader, IGLPostShader)
  private
    FThreshold: Single;
    // Implementing IGLPostShader.
    procedure DoUseTempTexture(const TempTexture: TGLTextureHandle;
      TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;
    function StoreThreshold: Boolean;
  protected
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Threshold: Single read FThreshold write FThreshold stored StoreThreshold;
  end;

  TGLSLPostBlurShader = class(TGLCustomGLSLPostBlurShader)
  published
    property Threshold;
  end;

  (* Custom class for GLSLPostThermalVisionShader.
   A Shader that simulate a thermal vision of the entire scene *)
  // A shader that simulate a Thermal Vision of the entire scene
  TGLCustomGLSLPostThermalVisionShader = class(TGLCustomGLSLShader, IGLPostShader)
  private
    FThreshold : Single;
    Fintensity : Single;
    // Implementing IGLPostShader.
    procedure DoUseTempTexture(const TempTexture: TGLTextureHandle;TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;
    function StoreThreshold: Boolean;
    function StoreIntensity: Boolean;
  protected
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
     property Threshold: Single read FThreshold write FThreshold stored StoreThreshold;
     property Intensity: Single read FIntensity write FIntensity stored StoreIntensity;
  end;

  TGLSLPostThermalVisionShader = class(TGLCustomGLSLPostThermalVisionShader)
  published
    property Threshold;
    property Intensity;
  end;

  (* Custom class for GLSLPostDreamVisionShader.
   A shader that simulate a grayscale threshold vision (aka dream) of the entire scene*)
  TGLCustomGLSLPostDreamVisionShader = class(TGLCustomGLSLShader, IGLPostShader)
  private
    FThreshold : Single; // In percent 0..100;
    // Implementing IGLPostShader.
    procedure DoUseTempTexture(const TempTexture: TGLTextureHandle;TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;
    function StoreThreshold: Boolean;
  protected
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
     property Threshold: Single read FThreshold write FThreshold stored StoreThreshold;
  end;

  TGLSLPostDreamVisionShader = class(TGLCustomGLSLPostDreamVisionShader)
  published
    property Threshold;
  end;

  (* Custom class for GLSLPostNightVisionShader.
   A shader that simulate a Night Vision of the scene throw a mask if enabled,
   or of the entire scene*)
  TGLCustomGLSLPostNightVisionShader = class(TGLCustomGLSLShader, IGLPostShader)
  private
    FMaterialLibrary: TGLAbstractMaterialLibrary;
    FLuminanceThreshold: Single;
    FColorAmplification:Single;
    FElapsedTime: Single;
    FUseMask: Integer;
    FNoiseTex: TGLTexture;
    FMaskTex: TGLTexture;
    FNoiseTexName: TGLLibMaterialName;
    FMaskTexName: TGLLibMaterialName;
    // Implementing IGLPostShader.
    procedure DoUseTempTexture(const TempTexture: TGLTextureHandle;TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;
    function StoreLuminanceThreshold: Boolean;
    function StoreColorAmplification: Boolean;
    procedure SetMaskTexTexture(const Value: TGLTexture);
    procedure SetNoiseTexTexture(const Value: TGLTexture);
    function GetNoiseTexName: TGLLibMaterialName;
    procedure SetNoiseTexName(const Value: TGLLibMaterialName);
    function GetMaskTexName: TGLLibMaterialName;
    procedure SetMaskTexName(const Value: TGLLibMaterialName);
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
  protected
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
    procedure SetMaterialLibrary(const Value: TGLAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    property LuminanceThreshold: Single read FLuminanceThreshold write FLuminanceThreshold stored StoreLuminanceThreshold;
    property ColorAmplification: Single read FColorAmplification write FColorAmplification stored StoreColorAmplification;
    property ElapsedTime: Single read FElapsedTime write FElapsedTime stored false;
    property MaterialLibrary: TGLAbstractMaterialLibrary read getMaterialLibrary write SetMaterialLibrary;
    property NoiseTex: TGLTexture read FNoiseTex write SetNoiseTexTexture;
    property NoiseTexName: TGLLibMaterialName read GetNoiseTexName write SetNoiseTexName;
    property MaskTex: TGLTexture read FMaskTex write SetMaskTexTexture;
    property MaskTexName: TGLLibMaterialName read GetMaskTexName write SetMaskTexName;
    property UseMask : Integer read FUseMask write FUseMask;
  end;

  TGLSLPostNightVisionShader = class(TGLCustomGLSLPostNightVisionShader)
  published
    property LuminanceThreshold;
    property ColorAmplification;
    property ElapsedTime;
    property MaterialLibrary;
    property NoiseTexName;
    property MaskTexName;
    property UseMask;
  end;

  (* Custom class for GLSLPostPixelateShader.
   A shader that pixelate of the entire scene*)
  TGLCustomGLSLPostPixelateShader = class(TGLCustomGLSLShader, IGLPostShader)
  private
    FPixelWidth  : Single;
    FPixelHeight : Single;
    // Implementing IGLPostShader.
    procedure DoUseTempTexture(const TempTexture: TGLTextureHandle;TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;
    function StorePixelWidth: Boolean;
    function StorePixelHeight: Boolean;
  protected
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
     property PixelWidth: Single read FPixelWidth write FPixelWidth stored StorePixelWidth;
     property PixelHeight: Single read FPixelHeight write FPixelHeight stored StorePixelHeight;
  end;

  TGLSLPostPixelateShader = class(TGLCustomGLSLPostPixelateShader)
  published
    property PixelWidth;
    property PixelHeight;
  end;

  (* Custom class for GLSLPostPosterizeShader.
   A shader that posterize of the entire scene*)
  TGLCustomGLSLPostPosterizeShader = class(TGLCustomGLSLShader, IGLPostShader)
  private
    FGamma  : Single;
    FNumColors : Single;
    // Implementing IGLPostShader.
    procedure DoUseTempTexture(const TempTexture: TGLTextureHandle;TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;
    function StoreGamma: Boolean;
    function StoreNumColors: Boolean;
  protected
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
     property Gamma: Single read FGamma write FGamma stored StoreGamma;
     property NumColors: Single read FNumColors write FNumColors stored StoreNumColors;
  end;

  TGLSLPostPosterizeShader = class(TGLCustomGLSLPostPosterizeShader)
  published
    property Gamma;
    property NumColors;
  end;

  (* Custom class for GLSLPostFrostShader.
   A shader that frost of the entire scene *)
  TGLCustomGLSLPostFrostShader = class(TGLCustomGLSLShader, IGLPostShader)
  private
    FRandScale  : Single;
    FRandFactor : Single;
    // Implementing IGLPostShader.
    procedure DoUseTempTexture(const TempTexture: TGLTextureHandle;TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;
    function StoreRandScale: Boolean;
    function StoreRandFactor: Boolean;
  protected
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
     property RandScale: Single read FRandScale write FRandScale stored StoreRandScale;
     property RandFactor: Single read FRandFactor write FRandFactor stored StoreRandFactor;
  end;

  TGLSLPostFrostShader = class(TGLCustomGLSLPostFrostShader)
  published
    property RandScale;
    property RandFactor;
  end;

  (* Custom class for GLSLPostTroubleShader. A shader that trouble of the entire scene. v2
   This Shader is experimental it can do smooth the scene or double the scene and it's
   depends of PixelX, PixelY and Freq values if they are less than 1 or greater
   the effects will be very different *)
  TGLCustomGLSLPostTroubleShader = class(TGLCustomGLSLShader, IGLPostShader)
  private
    FPixelX: Single;
    FPixelY: Single;
    FFreq: Single;
    FMaterialLibrary: TGLAbstractMaterialLibrary;
    FNoiseTex : TGLTexture;
    FNoiseTexName  : TGLLibMaterialName;
    // Implementing IGLPostShader.
    procedure DoUseTempTexture(const TempTexture: TGLTextureHandle;TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;
    procedure SetNoiseTexTexture(const Value: TGLTexture);
    function GetNoiseTexName: TGLLibMaterialName;
    procedure SetNoiseTexName(const Value: TGLLibMaterialName);
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
    function StorePixelX: Boolean;
    function StorePixelY: Boolean;
    function StoreFreq: Boolean;
  protected
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
    procedure SetMaterialLibrary(const Value: TGLAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
     property PixelX: Single read FPixelX write FPixelX stored StorePixelX;
     property PixelY: Single read FPixelY write FPixelY stored StorePixelY;
     property Freq: Single read FFreq write FFreq stored StoreFreq;
     property MaterialLibrary: TGLAbstractMaterialLibrary read getMaterialLibrary write SetMaterialLibrary;
     property NoiseTex: TGLTexture read FNoiseTex write SetNoiseTexTexture;
     property NoiseTexName: TGLLibMaterialName read GetNoiseTexName write SetNoiseTexName;
  end;

  TGLSLPostTroubleShader = class(TGLCustomGLSLPostTroubleShader)
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

//---------------------------------------
// TGLCustomGLSLPostBlurShader
//---------------------------------------

constructor TGLCustomGLSLPostBlurShader.Create(
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

procedure TGLCustomGLSLPostBlurShader.DoApply(
  var rci: TGLRenderContextInfo; Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  GetGLSLProg.Uniform1f['threshold'] := FThreshold;
  GetGLSLProg.Uniform2f['ScreenExtents'] := Vector2fMake(rci.viewPortSize.cx, rci.viewPortSize.cy);
end;

function TGLCustomGLSLPostBlurShader.DoUnApply(
  var rci: TGLRenderContextInfo): Boolean;
begin
  rci.GLStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TGLCustomGLSLPostBlurShader.DoUseTempTexture(
  const TempTexture: TGLTextureHandle; TextureTarget: TGLTextureTarget);
begin
  Param['Image'].AsCustomTexture[2, TextureTarget] := TempTexture.Handle;
end;

function TGLCustomGLSLPostBlurShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect;
end;

function TGLCustomGLSLPostBlurShader.StoreThreshold: Boolean;
begin
  Result := Abs(FThreshold - 0.1) > 0.00001;
end;

//---------------------------------------
// TGLCustomGLSLPostThermalVisionShader
//---------------------------------------

constructor TGLCustomGLSLPostThermalVisionShader.Create(
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

procedure TGLCustomGLSLPostThermalVisionShader.DoApply(
  var rci: TGLRenderContextInfo; Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  GetGLSLProg.Uniform1f['Threshold'] := FThreshold;
  GetGLSLProg.Uniform1f['Intensity'] := FIntensity;
  GetGLSLProg.Uniform2f['ScreenExtents'] := Vector2fMake(rci.viewPortSize.cx, rci.viewPortSize.cy);

end;

function TGLCustomGLSLPostThermalVisionShader.DoUnApply(
  var rci: TGLRenderContextInfo): Boolean;
begin
  rci.GLStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TGLCustomGLSLPostThermalVisionShader.DoUseTempTexture(
  const TempTexture: TGLTextureHandle; TextureTarget: TGLTextureTarget);
begin
  Param['ScreenTex'].AsCustomTexture[3, TextureTarget] := TempTexture.Handle;
end;

function TGLCustomGLSLPostThermalVisionShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect; //ttTexture2D;
end;

function TGLCustomGLSLPostThermalVisionShader.StoreThreshold: Boolean;
begin
  Result := (Abs(FThreshold) > 0) and (Abs(FThreshold) <= 1.0);
end;

function TGLCustomGLSLPostThermalVisionShader.StoreIntensity: Boolean;
begin
  Result := (Abs(FIntensity) >= 0) and (Abs(FIntensity) <= 2.0);
end;

//---------------------------------------
// TGLCustomGLSLPostThermalVisionShader
//---------------------------------------

constructor TGLCustomGLSLPostDreamVisionShader.Create(
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

procedure TGLCustomGLSLPostDreamVisionShader.DoApply(
  var rci: TGLRenderContextInfo; Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  GetGLSLProg.Uniform1f['Threshold'] := (FThreshold*255)/100;
  GetGLSLProg.Uniform2f['ScreenExtents'] := Vector2fMake(rci.viewPortSize.cx, rci.viewPortSize.cy);

end;

function TGLCustomGLSLPostDreamVisionShader.DoUnApply(
  var rci: TGLRenderContextInfo): Boolean;
begin
  rci.GLStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TGLCustomGLSLPostDreamVisionShader.DoUseTempTexture(
  const TempTexture: TGLTextureHandle; TextureTarget: TGLTextureTarget);
begin
  Param['ScreenTex'].AsCustomTexture[2, TextureTarget] := TempTexture.Handle;
end;

function TGLCustomGLSLPostDreamVisionShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect; //ttTexture2D;
end;

function TGLCustomGLSLPostDreamVisionShader.StoreThreshold: Boolean;
begin
  Result := (Abs(FThreshold) > 0) and (Abs(FThreshold) <= 100);
end;

//---------------------------------------
// TGLCustomGLSLPostThermalVisionShader
//---------------------------------------

constructor TGLCustomGLSLPostNightVisionShader.Create(
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

procedure TGLCustomGLSLPostNightVisionShader.DoApply(var rci: TGLRenderContextInfo; Sender: TObject);
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

function TGLCustomGLSLPostNightVisionShader.DoUnApply(
  var rci: TGLRenderContextInfo): Boolean;
begin
  rci.GLStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TGLCustomGLSLPostNightVisionShader.DoUseTempTexture(
  const TempTexture: TGLTextureHandle; TextureTarget: TGLTextureTarget);
begin
  Param['ScreenTex'].AsCustomTexture[7, TextureTarget] := TempTexture.Handle;
end;

function TGLCustomGLSLPostNightVisionShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect; //ttTexture2D;
end;


function TGLCustomGLSLPostNightVisionShader.StoreLuminanceThreshold: Boolean;
begin
  Result := Abs(FLuminanceThreshold - 0.1) > 0.00001;
end;

function TGLCustomGLSLPostNightVisionShader.StoreColorAmplification: Boolean;
begin
  Result := Abs(FColorAmplification - 0.1) > 0.00001;
end;


procedure TGLCustomGLSLPostNightVisionShader.SetMaskTexTexture(const Value: TGLTexture);
begin
  if FMaskTex = Value then Exit;
  MaskTex := Value;
  NotifyChange(Self)
end;

procedure TGLCustomGLSLPostNightVisionShader.SetNoiseTexTexture(const Value: TGLTexture);
begin
  if FNoiseTex = Value then Exit;
  NoiseTex := Value;
  NotifyChange(Self);
end;

function TGLCustomGLSLPostNightVisionShader.GetNoiseTexName: TGLLibMaterialName;
begin
  Result := TGLMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FNoiseTex);
  if Result = '' then Result := FNoiseTexName;
end;

procedure TGLCustomGLSLPostNightVisionShader.SetNoiseTexName(const Value: TGLLibMaterialName);
begin
  //Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FNoiseTexName = Value then Exit;
  FNoiseTexName  := Value;
  FNoiseTex := TGLMaterialLibrary(FMaterialLibrary).TextureByName(FNoiseTexName);
  NotifyChange(Self);
end;

function TGLCustomGLSLPostNightVisionShader.GetMaskTexName: TGLLibMaterialName;
begin
  Result := TGLMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FMaskTex);
  if Result = '' then Result := FMaskTexName;
end;

procedure TGLCustomGLSLPostNightVisionShader.SetMaskTexName(const Value: TGLLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FMaskTexName = Value then Exit;
  FMaskTexName  := Value;

  FMaskTex := TGLMaterialLibrary(FMaterialLibrary).TextureByName(FMaskTexName);
  NotifyChange(Self);
end;

function TGLCustomGLSLPostNightVisionShader.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TGLCustomGLSLPostNightVisionShader.SetMaterialLibrary(const Value: TGLAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TGLAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TGLCustomGLSLPostNightVisionShader.Notification(AComponent: TComponent; Operation: TOperation);
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

        if FMaskTex <> nil then
        begin
          Index := TGLMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FMaskTex);
          if Index <> -1 then
            SetMaskTexTexture(nil);
        end;

        FMaterialLibrary := nil;
      end;
end;

//---------------------------------------
// TGLCustomGLSLPostPixelateShader
//---------------------------------------

constructor TGLCustomGLSLPostPixelateShader.Create(
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

procedure TGLCustomGLSLPostPixelateShader.DoApply(
  var rci: TGLRenderContextInfo; Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  GetGLSLProg.Uniform1f['pixel_w'] := FPixelWidth;
  GetGLSLProg.Uniform1f['pixel_h'] := FPixelHeight;
  GetGLSLProg.Uniform2f['ScreenExtents'] := Vector2fMake(rci.viewPortSize.cx, rci.viewPortSize.cy);

end;

function TGLCustomGLSLPostPixelateShader.DoUnApply(
  var rci: TGLRenderContextInfo): Boolean;
begin
  rci.GLStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TGLCustomGLSLPostPixelateShader.DoUseTempTexture(
  const TempTexture: TGLTextureHandle; TextureTarget: TGLTextureTarget);
begin
  Param['ScreenTex'].AsCustomTexture[3, TextureTarget] := TempTexture.Handle;
end;

function TGLCustomGLSLPostPixelateShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect; //ttTexture2D;
end;

function TGLCustomGLSLPostPixelateShader.StorePixelWidth: Boolean;
begin
  Result := (Abs(FPixelWidth) > 0) and (Abs(FPixelWidth) <= 64);
end;

function TGLCustomGLSLPostPixelateShader.StorePixelHeight: Boolean;
begin
  Result := (Abs(FPixelHeight) > 0) and (Abs(FPixelHeight) <= 64);
end;

//---------------------------------------
// TGLCustomGLSLPostPosterizeShader
//---------------------------------------

constructor TGLCustomGLSLPostPosterizeShader.Create(
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

procedure TGLCustomGLSLPostPosterizeShader.DoApply(
  var rci: TGLRenderContextInfo; Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  GetGLSLProg.Uniform1f['gamma'] := FGamma;
  GetGLSLProg.Uniform1f['numColors'] := FNumColors;
  GetGLSLProg.Uniform2f['ScreenExtents'] := Vector2fMake(rci.viewPortSize.cx, rci.viewPortSize.cy);

end;

function TGLCustomGLSLPostPosterizeShader.DoUnApply(
  var rci: TGLRenderContextInfo): Boolean;
begin
  rci.GLStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TGLCustomGLSLPostPosterizeShader.DoUseTempTexture(
  const TempTexture: TGLTextureHandle; TextureTarget: TGLTextureTarget);
begin
  Param['ScreenTex'].AsCustomTexture[3, TextureTarget] := TempTexture.Handle;
end;

function TGLCustomGLSLPostPosterizeShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect; //ttTexture2D;
end;

function TGLCustomGLSLPostPosterizeShader.StoreGamma: Boolean;
begin
  Result := (Abs(FGamma) > 0) and (Abs(FGamma) <= 3.0);
end;

function TGLCustomGLSLPostPosterizeShader.StoreNumColors: Boolean;
begin
  Result := (Abs(FNumColors) > 0) and (Abs(FNumColors) <= 255);
end;

//---------------------------------------
// TGLCustomGLSLPostFrostShader
//---------------------------------------

constructor TGLCustomGLSLPostFrostShader.Create(
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

procedure TGLCustomGLSLPostFrostShader.DoApply(
  var rci: TGLRenderContextInfo; Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  GetGLSLProg.Uniform1f['rnd_scale'] := FRandScale;
  GetGLSLProg.Uniform1f['rnd_factor'] := FRandFactor;
  GetGLSLProg.Uniform2f['ScreenExtents'] := Vector2fMake(rci.viewPortSize.cx, rci.viewPortSize.cy);

end;

function TGLCustomGLSLPostFrostShader.DoUnApply(
  var rci: TGLRenderContextInfo): Boolean;
begin
  rci.GLStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TGLCustomGLSLPostFrostShader.DoUseTempTexture(
  const TempTexture: TGLTextureHandle; TextureTarget: TGLTextureTarget);
begin
  Param['ScreenTex'].AsCustomTexture[3, TextureTarget] := TempTexture.Handle;
end;

function TGLCustomGLSLPostFrostShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect; //ttTexture2D;
end;

function TGLCustomGLSLPostFrostShader.StoreRandScale: Boolean;
begin
  Result := (Abs(FRandScale) > 0) and (Abs(FRandScale) <= 1000);
end;

function TGLCustomGLSLPostFrostShader.StoreRandFactor: Boolean;
begin
  Result := (Abs(FRandFactor) > 0) and (Abs(FRandFactor) <= 1000);
end;

//---------------------------------------
// TGLCustomGLSLPostTroubleShader
//---------------------------------------

constructor TGLCustomGLSLPostTroubleShader.Create( AOwner: TComponent);
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

procedure TGLCustomGLSLPostTroubleShader.DoApply(
  var rci: TGLRenderContextInfo; Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  GetGLSLProg.Uniform1f['PixelX'] := FPixelX;
  GetGLSLProg.Uniform1f['PixelY'] := FPixelY;
  GetGLSLProg.Uniform1f['Freq'] := FFreq;
  GetGLSLProg.Uniform2f['ScreenExtents'] := Vector2fMake(rci.viewPortSize.cx, rci.viewPortSize.cy);

  param['noiseTex'].AsTexture2D[1]:= FNoiseTex;

end;

function TGLCustomGLSLPostTroubleShader.DoUnApply(
  var rci: TGLRenderContextInfo): Boolean;
begin
  rci.GLStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TGLCustomGLSLPostTroubleShader.DoUseTempTexture(
  const TempTexture: TGLTextureHandle; TextureTarget: TGLTextureTarget);
begin
  Param['ScreenTex'].AsCustomTexture[5, TextureTarget] := TempTexture.Handle;
end;

function TGLCustomGLSLPostTroubleShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect; //ttTexture2D;
end;

function TGLCustomGLSLPostTroubleShader.StorePixelX: Boolean;
begin
  Result := (Abs(FPixelX) > 0) and (Abs(FPixelX) <= 1000);
end;

function TGLCustomGLSLPostTroubleShader.StorePixelY: Boolean;
begin
  Result := (Abs(FPixelY) > 0) and (Abs(FPixelY) <= 1000);
end;

function TGLCustomGLSLPostTroubleShader.StoreFreq: Boolean;
begin
  Result := (Abs(FPixelY) > 0) and (Abs(FPixelY) <= 5.0);
end;

procedure TGLCustomGLSLPostTroubleShader.SetNoiseTexTexture(const Value: TGLTexture);
begin
  if FNoiseTex = Value then Exit;
  NoiseTex := Value;
  NotifyChange(Self);
end;

function TGLCustomGLSLPostTroubleShader.GetNoiseTexName: TGLLibMaterialName;
begin
  Result := TGLMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FNoiseTex);
  if Result = '' then Result := FNoiseTexName;
end;

procedure TGLCustomGLSLPostTroubleShader.SetNoiseTexName(const Value: TGLLibMaterialName);
begin
  //Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FNoiseTexName = Value then Exit;
  FNoiseTexName  := Value;
  FNoiseTex := TGLMaterialLibrary(FMaterialLibrary).TextureByName(FNoiseTexName);
  NotifyChange(Self);
end;

function TGLCustomGLSLPostTroubleShader.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TGLCustomGLSLPostTroubleShader.SetMaterialLibrary(const Value: TGLAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TGLAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TGLCustomGLSLPostTroubleShader.Notification(AComponent: TComponent; Operation: TOperation);
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

        FMaterialLibrary := nil;
      end;
end;

end.

