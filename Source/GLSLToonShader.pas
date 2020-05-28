//
// This unit is part of the GLScene Engine, http://glscene.org
//
{
   Toon shader : Toon shading also called Cell Shading 
   At this time only one light source is supported

}
unit GLSLToonShader;

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

{Custom class for GLSLToonShader. }
type
  TGLCustomGLSLToonShader = class(TGLCustomGLSLShader)
  private
    FHighlightColor : TGLColor;
    FMidColor : TGLColor;
    FLightenShadowColor : TGLColor;
    FDarkenShadowColor : TGLColor;
    FOutlineColor : TGLColor;
    FHighlightSize : Single;
    FMidSize : Single;
    FShadowSize : Single;
    FOutlineWidth : Single;
    procedure SetHighLightColor(AValue: TGLColor);
    procedure SetMidColor(AValue: TGLColor);
    procedure SetLightenShadowColor(AValue: TGLColor);
    procedure SetDarkenShadowColor(AValue: TGLColor);
    procedure SetOutlineColor(AValue: TGLColor);
  protected
    procedure DoApply(var rci : TGLRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property HighlightColor : TGLColor read FHighlightColor Write setHighlightColor;
    property MidColor : TGLColor read FMidColor Write setMidColor;
    property LightenShadowColor : TGLColor Read FLightenShadowColor Write setLightenShadowColor;
    property DarkenShadowrColor : TGLColor Read FDarkenShadowColor Write setDarkenShadowColor;
    property OutlinetColor : TGLColor Read FOutlineColor Write setOutlineColor;
    property HighlightSize : Single read FHighlightSize write FHighlightSize;
    property MidSize : Single read FMidSize write FMidSize;
    property ShadowSize : Single read FShadowSize write FShadowSize;
    property OutlineWidth : Single read FOutlineWidth write FOutlineWidth;
  end;

type
  TGLSLToonShader = class(TGLCustomGLSLToonShader)
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

//==============================================================
implementation
//==============================================================


{ TGLCustomGLSLToonShader }

constructor TGLCustomGLSLToonShader.Create(AOwner: TComponent);
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
  FHighLightColor := TGLColor.Create(self);
  FHighLightColor.SetColor(0.9,0.9,0.9,1.0);
  FMidColor := TGLColor.Create(self);
  FMidColor.SetColor(0.75,0.75,0.75,1.0);
  FLightenShadowColor := TGLColor.Create(self);
  FLightenShadowColor.SetColor(0.5,0.5,0.5,1.0);
  FDarkenShadowColor := TGLColor.Create(self);
  FDarkenShadowColor.SetColor(0.3,0.3,0.3,1.0);
  FOutlineColor := TGLColor.Create(self);
  FOutlineColor.SetColor(0,0,0,1.0);

  FHighlightSize := 0.95;
  FMidSize       := 0.50;
  FShadowSize    := 0.25;
  FOutlineWidth  := 0.25;

end;

destructor TGLCustomGLSLToonShader.Destroy;
begin
  FHighLightColor.Free;
  FMidColor.Free;
  FLightenShadowColor.Free;
  FDarkenShadowColor.Free;
  FOutlineColor.Free;
  inherited;
end;

procedure TGLCustomGLSLToonShader.DoApply(var rci: TGLRenderContextInfo;Sender: TObject);
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

function TGLCustomGLSLToonShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TGLCustomGLSLToonShader.SetHighlightColor(AValue: TGLColor);
begin
  FHighlightColor.DirectColor := AValue.Color;
end;

procedure TGLCustomGLSLToonShader.SetMidColor(AValue: TGLColor);
begin
  FMidColor.DirectColor := AValue.Color;
end;

procedure TGLCustomGLSLToonShader.SetLightenShadowColor(AValue: TGLColor);
begin
  FLightenShadowColor.DirectColor := AValue.Color;
end;

procedure TGLCustomGLSLToonShader.SetDarkenShadowColor(AValue: TGLColor);
begin
  FDarkenShadowColor.DirectColor := AValue.Color;
end;

procedure TGLCustomGLSLToonShader.SetOutlineColor(AValue: TGLColor);
begin
  FOutlineColor.DirectColor := AValue.Color;
end;

end.
