//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit Cg.PostTransformationShader;
(*
   A shader that uses a texture to distort the view by adjusting texture
   coordinates.
   Does not have any practical use, but is fun to play around with.
*)
interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  
  GLS.Texture,
  GLS.Cadencer, 
  GLS.Context, 
  GLS.Scene, 
  GLS.RenderContextInfo,
  GLS.TextureFormat,

  Cg.Import,
  Cg.GL,
  Cg.Shader,
  GLSL.CustomShader;

type

  TGLCustomCGPostTransformationShader = class(TCustomCGShader, IGLPostShader)
  private
    FTransformationPower:      Single;
    FTransformationTexture: TGLTexture;
  protected
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    // Implementing IGLPostShader.
    procedure DoUseTempTexture(const TempTexture: TGLTextureHandle; TextureTarget: TGLTextureTarget);
    function GetTextureTarget: TGLTextureTarget;
  public
    constructor Create(AOwner: TComponent); override;
    property TransformationPower: Single read FTransformationPower write FTransformationPower;
    property TransformationTexture: TGLTexture read FTransformationTexture write FTransformationTexture;
  end;

  TGLCGPostTransformationShader = class(TGLCustomCGPostTransformationShader)
  published
    property TransformationPower;
    property TransformationTexture;
  end;

//------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------


//----------------------------------------
// TGLCustomCGPostTransformationShader
//----------------------------------------

constructor TGLCustomCGPostTransformationShader.Create(AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    Add(' ');
    Add('void main( ');
    Add('                    float4  iPos  	: POSITION, ');
    Add('                    float2  iTex0	: TEXCOORD0, ');
    Add('                out float4  oPos  	: POSITION, ');
    Add('                out float2  oTex0 	: TEXCOORD0 ');
    Add('         ) ');
    Add('{ ');
    Add('			oPos  = iPos; ');
    Add('			oTex0 = iTex0; ');
    Add('} ');
  end;


  with FragmentProgram.Code do
  begin
    Add('void main( ');
    Add('                    float2 iTex0 	: TEXCOORD0, '); 
    Add('                out float4 oCol  	: COLOR, '); 
    Add(' ');
    Add('            uniform samplerRECT snapshotTex, ');
    Add('            uniform sampler2D	transformTex, ');
    Add('            uniform float		screenW, ');
    Add('            uniform float		screenH, '); 
    Add('            uniform float		transformPower '); 
    Add('         ) '); 
    Add('{ '); 
    Add(' '); 
    Add('	/* Read the offset from the transformation texture '); 
    Add('		x offset is in the red channel, '); 
    Add('		y offset is in the green channel '); 
    Add('	*/ '); 
    Add('	float2	offset	= 2 * tex2D( transformTex, iTex0 ).rg -1; '); 
    Add(' '); 
    Add('	/* When using NPOT texture RECT, you need to scale up the texcoords with '); 
    Add('		the screenSize	*/ '); 
    Add('            iTex0.x    *= screenW; '); 
    Add('            iTex0.y    *= screenH; '); 
    Add(' '); 
    Add('    /* Apply offset		*/ '); 
    Add('    		iTex0	+= offset * transformPower; '); 
    Add(' '); 
    Add('	/* The result is the pixel from the snapshot, with offset	*/ '); 
    Add('    		oCol.rgb	= texRECT( snapshotTex, iTex0 ).rgb; '); 
    Add('			oCol.a		= 1; '); 
    Add('} '); 
  end;

  VertexProgram.OnApply := OnApplyVP;
  FragmentProgram.OnApply := OnApplyFP;
  FragmentProgram.OnUnApply := OnUnApplyFP;

  FTransformationPower := 70;
end;

procedure TGLCustomCGPostTransformationShader.DoApply(
  var rci: TGLRenderContextInfo; Sender: TObject);
begin
  inherited;
  FragmentProgram.ParamByName('screenW').SetAsScalar(rci.viewPortSize.cx);
  FragmentProgram.ParamByName('screenH').SetAsScalar(rci.viewPortSize.cy);
  FragmentProgram.ParamByName('transformTex').SetAsTexture2D(FTransformationTexture.Handle);
  FragmentProgram.ParamByName('transformTex').EnableTexture;
  FragmentProgram.ParamByName('transformPower').SetAsScalar(FTransformationPower);
end;

procedure TGLCustomCGPostTransformationShader.DoUseTempTexture(
  const TempTexture: TGLTextureHandle; TextureTarget: TGLTextureTarget);
begin
  FragmentProgram.ParamByName('snapshotTex').SetAsTextureRECT(TempTexture.Handle);
  FragmentProgram.ParamByName('snapshotTex').EnableTexture;
end;

function TGLCustomCGPostTransformationShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect;
end;

end.

