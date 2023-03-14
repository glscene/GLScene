//
//
// The graphics platform GLXcene https://github.com/glscene
//
//

unit GLX.GLSLToonShader;

(*
   Toon shader : Toon shading also called Cell Shading 
   At this time only one light source is supported
*)

interface

{$I Scena.inc}

uses
  Winapi.OpenGL, 
  Winapi.OpenGLext,  
  System.Classes,
  
  GXL.OpenGLx, 
  GLX.BaseClasses, 
  Scena.VectorGeometry, 
  GLX.Scene, 
  GLX.CrossPlatform, 
  GLX.State, 
  GLX.Context, 
  GLX.RenderContextInfo, 
  GLX.Coordinates,
  Scena.TextureFormat, 
  GLX.Color, 
  GLX.Texture, 
  GLX.Material, 
  GLX.GLSLShader, 
  GLX.CustomShader;

{ Custom class for GLSLToonShader. }
type
  TgxCustomGLSLToonShader = class(TgxCustomGLSLShader)
  private
    FHighlightColor : TgxColor;
    FMidColor : TgxColor;
    FLightenShadowColor : TgxColor;
    FDarkenShadowColor : TgxColor;
    FOutlineColor : TgxColor;
    FHighlightSize : Single;
    FMidSize : Single;
    FShadowSize : Single;
    FOutlineWidth : Single;


    procedure SetHighLightColor(AValue: TgxColor);
    procedure SetMidColor(AValue: TgxColor);
    procedure SetLightenShadowColor(AValue: TgxColor);
    procedure SetDarkenShadowColor(AValue: TgxColor);
    procedure SetOutlineColor(AValue: TgxColor);

  protected
    procedure DoApply(var rci : TgxRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property HighlightColor : TgxColor read FHighlightColor Write setHighlightColor;
    property MidColor : TgxColor read FMidColor Write setMidColor;
    property LightenShadowColor : TgxColor Read FLightenShadowColor Write setLightenShadowColor;
    property DarkenShadowrColor : TgxColor Read FDarkenShadowColor Write setDarkenShadowColor;
    property OutlinetColor : TgxColor Read FOutlineColor Write setOutlineColor;

    property HighlightSize : Single read FHighlightSize write FHighlightSize;
    property MidSize : Single read FMidSize write FMidSize;
    property ShadowSize : Single read FShadowSize write FShadowSize;
    property OutlineWidth : Single read FOutlineWidth write FOutlineWidth;

  end;

type
  TgxSLToonShader = class(TgxCustomGLSLToonShader)
  published
    property HighlightColor;
    property MidColor;
    property LightenShadowColor;
    property DarkenShadowrColor;
    property OutlinetColor;

    property HighlightSize;
    property MidSize;
    property ShadowSize;
    property OutlineWidth;
  end;

implementation


{ TgxCustomGLSLToonShader }

constructor TgxCustomGLSLToonShader.Create(AOwner: TComponent);
begin
  inherited;

  with VertexProgram.Code do
  begin
    Clear;
    Add('varying vec3 vNormal; ');
    Add('varying vec3 LightVec; ');
    Add('varying vec3 ViewVec; ');
    Add(' ');
    Add('void main() ');
    Add('{ ');
    Add('  vec4 lightPos = gl_LightSource[0].position;');
    Add('  vec4 vert =  gl_ModelViewMatrix * gl_Vertex; ');
    Add('  vec3 normal = gl_NormalMatrix * gl_Normal; ');
    Add('  vNormal  = normalize(normal); ');
    Add('  LightVec = vec3(lightPos - vert); ');
    Add('  ViewVec = -vec3(vert); ');
    //Add('  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; ');
    Add('  gl_Position = ftransform(); ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
    Clear;
    Add('uniform vec4 HighlightColor; ');
    Add('uniform vec4 MidColor; ');
    Add('uniform vec4 LightenShadowColor; ');
    Add('uniform vec4 DarkenShadowColor; ');
    Add('uniform vec4 OutlineColor; ');

    Add('uniform float HighlightSize; '); // 0.95
    Add('uniform float MidSize; ');       // 0.5
    Add('uniform float ShadowSize; ');    // 0.25

    Add('uniform float OutlineWidth; ');

    Add('varying vec3 vNormal; ');
    Add('varying vec3 LightVec; ');
    Add('varying vec3 ViewVec; ');

    Add('void main() ');
    Add('{ ');
    Add('  vec3 n = normalize(vNormal); ');
    Add('  vec3 l = normalize(LightVec); ');
    Add('  vec3 v = normalize(ViewVec); ');

    Add('    float lambert = dot(l,n); ');
    Add('    vec4 colour = MidColor; ');
    Add('    if (lambert>HighlightSize) colour = HighlightColor; ');
    Add('    else if (lambert>MidSize) colour = MidColor; ');
    Add('    else if (lambert>ShadowSize) colour = LightenShadowColor; ');
    Add('    else if (lambert<ShadowSize) colour = DarkenShadowColor; ');

    Add('    if (dot(n,v)<OutlineWidth) colour = OutlineColor; ');

    Add('    gl_FragColor = colour; ');
    Add('} ');
  end;


  // Initial stuff.
  FHighLightColor := TgxColor.Create(self);
  FHighLightColor.SetColor(0.9,0.9,0.9,1.0);
  FMidColor := TgxColor.Create(self);
  FMidColor.SetColor(0.75,0.75,0.75,1.0);
  FLightenShadowColor := TgxColor.Create(self);
  FLightenShadowColor.SetColor(0.5,0.5,0.5,1.0);
  FDarkenShadowColor := TgxColor.Create(self);
  FDarkenShadowColor.SetColor(0.3,0.3,0.3,1.0);
  FOutlineColor := TgxColor.Create(self);
  FOutlineColor.SetColor(0,0,0,1.0);

  FHighlightSize := 0.95;
  FMidSize       := 0.50;
  FShadowSize    := 0.25;
  FOutlineWidth  := 0.25;

end;

destructor TgxCustomGLSLToonShader.Destroy;
begin
  FHighLightColor.Free;
  FMidColor.Free;
  FLightenShadowColor.Free;
  FDarkenShadowColor.Free;
  FOutlineColor.Free;
  inherited;
end;

procedure TgxCustomGLSLToonShader.DoApply(var rci: TgxRenderContextInfo;Sender: TObject);
begin

  GetGLSLProg.UseProgramObject;
  param['HighlightColor'].AsVector4f := FHighlightColor.Color;
  param['MidColor'].AsVector4f := FMidColor.Color;
  param['LightenShadowColor'].AsVector4f := FLightenShadowColor.Color;
  param['DarkenShadowColor'].AsVector4f := FDarkenShadowColor.Color;
  param['OutlineColor'].AsVector4f := FOutlineColor.Color;

  param['HighlightSize'].AsVector1f := FHighlightSize;
  param['MidSize'].AsVector1f := FMidSize;
  param['ShadowSize'].AsVector1f := FShadowSize;
  param['OutlineWidth'].AsVector1f := FOutlineWidth;

end;

function TgxCustomGLSLToonShader.DoUnApply(var rci: TgxRenderContextInfo): Boolean;
begin
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TgxCustomGLSLToonShader.SetHighlightColor(AValue: TgxColor);
begin
  FHighlightColor.DirectColor := AValue.Color;
end;

procedure TgxCustomGLSLToonShader.SetMidColor(AValue: TgxColor);
begin
  FMidColor.DirectColor := AValue.Color;
end;

procedure TgxCustomGLSLToonShader.SetLightenShadowColor(AValue: TgxColor);
begin
  FLightenShadowColor.DirectColor := AValue.Color;
end;

procedure TgxCustomGLSLToonShader.SetDarkenShadowColor(AValue: TgxColor);
begin
  FDarkenShadowColor.DirectColor := AValue.Color;
end;

procedure TgxCustomGLSLToonShader.SetOutlineColor(AValue: TgxColor);
begin
  FOutlineColor.DirectColor := AValue.Color;
end;

end.
