//
// The graphics rendering engine GLScene http://glscene.org
//
unit DWS.OpenGL;
(*
   DelphiWebScript symbol creation for OpenGL procedures and functions.
   This unit is still under development.
*)
interface

{$I GLScene.inc}

uses
  System.Classes,
  System.Variants,
  
  GLS.OpenGLTokens,
  GLS.Context,
  GLS.VectorGeometry,
  
  dwsExprs,
  dwsSymbols,
  dwsComp,
  dwsFunctions;

type
  TdwsOpenGLUnit = class(TdwsUnitComponent)
    protected
      procedure AddUnitSymbols(SymbolTable: TSymbolTable); override;
      procedure AddExtensionUnitSymbols(SymbolTable: TSymbolTable);
    public
      constructor Create(AOwner: TComponent); override;
  end;

const
  TypCardinalID : TBaseTypeId = 10;
  TypByteID     : TBaseTypeId = 11;


procedure Register;

//===========================================================
implementation
//===========================================================

type
  TGLPushAttrib = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLPopAttrib = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLPushClientAttrib = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLPopClientAttrib = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLEnable = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLDisable = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLEnableClientState = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLDisableClientState = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLMatrixMode = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLPushMatrix = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLPopMatrix = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLLoadIdentity = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLLoadMatrixf = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLTranslatef = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLRotatef = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLScalef = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLBegin = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLCullFace = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLShadeModel = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLFrontFace = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLPolygonMode = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLEnd = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLColor3f = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLColor4f = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLNormal3f = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLVertex3f = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLTexCoord1f = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLTexCoord2f = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLTexCoord3f = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLTexCoord4f = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLMultiTexCoord1f = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLMultiTexCoord2f = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLMultiTexCoord3f = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLMultiTexCoord4f = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLActiveTexture = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLClientActiveTexture = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLTexEnvf = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLTexEnvi = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLBlendFunc = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLDepthFunc = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLDepthMask = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLDepthRange = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLStencilFunc = class(TInternalFunction)
  public
    procedure Execute; override;
  end;
  
  TGLStencilMask = class(TInternalFunction)
  public
    procedure Execute; override;
  end;
  
  TGLStencilOp = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLLogicOp = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TGLLineWidth = class(TInternalFunction)
  public
    procedure Execute; override;
  end;


procedure Register;
begin
  RegisterComponents('GLScene DWS', [TdwsOpenGLUnit]);
end;

function GetMatrixFromInfo(Info : IInfo) : TGLMatrix;
var
  i : Integer;
begin
  for i:=0 to 3 do
    Result[i]:=VectorMake(Info.Element([i]).Element([0]).Value,
                          Info.Element([i]).Element([1]).Value,
                          Info.Element([i]).Element([2]).Value,
                          Info.Element([i]).Element([3]).Value);
end;


// ----------
// ---------- TdwsOpenGLUnit ----------
// ----------

procedure TdwsOpenGLUnit.AddUnitSymbols(SymbolTable: TSymbolTable);
var
  CardinalSymbol,
  ByteSymbol : TSymbol;
begin
  CardinalSymbol:=SymbolTable.FindSymbol('Cardinal');
  if not Assigned(CardinalSymbol) then 
  begin
    CardinalSymbol:=TBaseSymbol.Create('Cardinal', TypCardinalID, VarAsType(0, varLongWord));
    SymbolTable.AddSymbol(CardinalSymbol);
  end;

  ByteSymbol:=SymbolTable.FindSymbol('Byte');
  if not Assigned(ByteSymbol) then 
  begin
    ByteSymbol:=TBaseSymbol.Create('Byte', TypByteID, VarAsType(0, varByte));
    SymbolTable.AddSymbol(ByteSymbol);
  end;

  // ---------- GL generic constants ----------

  // errors
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NO_ERROR', CardinalSymbol, GL_NO_ERROR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INVALID_ENUM', CardinalSymbol, GL_INVALID_ENUM));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INVALID_VALUE', CardinalSymbol, GL_INVALID_VALUE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INVALID_OPERATION', CardinalSymbol, GL_INVALID_OPERATION));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STACK_OVERFLOW', CardinalSymbol, GL_STACK_OVERFLOW));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STACK_UNDERFLOW', CardinalSymbol, GL_STACK_UNDERFLOW));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OUT_OF_MEMORY', CardinalSymbol, GL_OUT_OF_MEMORY));

  // attribute bits
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_BIT', CardinalSymbol, GL_CURRENT_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POINT_BIT', CardinalSymbol, GL_POINT_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LINE_BIT', CardinalSymbol, GL_LINE_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POLYGON_BIT', CardinalSymbol, GL_POLYGON_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POLYGON_STIPPLE_BIT', CardinalSymbol, GL_POLYGON_STIPPLE_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MODE_BIT', CardinalSymbol, GL_PIXEL_MODE_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LIGHTING_BIT', CardinalSymbol, GL_LIGHTING_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FOG_BIT', CardinalSymbol, GL_FOG_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DEPTH_BUFFER_BIT', CardinalSymbol, GL_DEPTH_BUFFER_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ACCUM_BUFFER_BIT', CardinalSymbol, GL_ACCUM_BUFFER_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STENCIL_BUFFER_BIT', CardinalSymbol, GL_STENCIL_BUFFER_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VIEWPORT_BIT', CardinalSymbol, GL_VIEWPORT_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TRANSFORM_BIT', CardinalSymbol, GL_TRANSFORM_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ENABLE_BIT', CardinalSymbol, GL_ENABLE_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_BUFFER_BIT', CardinalSymbol, GL_COLOR_BUFFER_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HINT_BIT', CardinalSymbol, GL_HINT_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_EVAL_BIT', CardinalSymbol, GL_EVAL_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LIST_BIT', CardinalSymbol, GL_LIST_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_BIT', CardinalSymbol, GL_TEXTURE_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SCISSOR_BIT', CardinalSymbol, GL_SCISSOR_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ALL_ATTRIB_BITS', CardinalSymbol, GL_ALL_ATTRIB_BITS));

  // client attribute bits
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CLIENT_PIXEL_STORE_BIT', CardinalSymbol, GL_CLIENT_PIXEL_STORE_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CLIENT_VERTEX_ARRAY_BIT', CardinalSymbol, GL_CLIENT_VERTEX_ARRAY_BIT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CLIENT_ALL_ATTRIB_BITS', CardinalSymbol, GL_CLIENT_ALL_ATTRIB_BITS));

  // boolean values
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FALSE', CardinalSymbol, GL_FALSE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TRUE', CardinalSymbol, GL_TRUE));

  // primitives
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POINTS', CardinalSymbol, GL_POINTS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LINES', CardinalSymbol, GL_LINES));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LINE_LOOP', CardinalSymbol, GL_LINE_LOOP));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LINE_STRIP', CardinalSymbol, GL_LINE_STRIP));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TRIANGLES', CardinalSymbol, GL_TRIANGLES));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TRIANGLE_STRIP', CardinalSymbol, GL_TRIANGLE_STRIP));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TRIANGLE_FAN', CardinalSymbol, GL_TRIANGLE_FAN));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_QUADS', CardinalSymbol, GL_QUADS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_QUAD_STRIP', CardinalSymbol, GL_QUAD_STRIP));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POLYGON', CardinalSymbol, GL_POLYGON));

  // blending
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ZERO', CardinalSymbol, GL_ZERO));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ONE', CardinalSymbol, GL_ONE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SRC_COLOR', CardinalSymbol, GL_SRC_COLOR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ONE_MINUS_SRC_COLOR', CardinalSymbol, GL_ONE_MINUS_SRC_COLOR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SRC_ALPHA', CardinalSymbol, GL_SRC_ALPHA));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ONE_MINUS_SRC_ALPHA', CardinalSymbol, GL_ONE_MINUS_SRC_ALPHA));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DST_ALPHA', CardinalSymbol, GL_DST_ALPHA));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ONE_MINUS_DST_ALPHA', CardinalSymbol, GL_ONE_MINUS_DST_ALPHA));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DST_COLOR', CardinalSymbol, GL_DST_COLOR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ONE_MINUS_DST_COLOR', CardinalSymbol, GL_ONE_MINUS_DST_COLOR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SRC_ALPHA_SATURATE', CardinalSymbol, GL_SRC_ALPHA_SATURATE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BLEND_DST', CardinalSymbol, GL_BLEND_DST));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BLEND_SRC', CardinalSymbol, GL_BLEND_SRC));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BLEND', CardinalSymbol, GL_BLEND));

  // blending (GL 1.2 ARB imaging)
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BLEND_COLOR', CardinalSymbol, GL_BLEND_COLOR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONSTANT_COLOR', CardinalSymbol, GL_CONSTANT_COLOR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ONE_MINUS_CONSTANT_COLOR', CardinalSymbol, GL_ONE_MINUS_CONSTANT_COLOR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONSTANT_ALPHA', CardinalSymbol, GL_CONSTANT_ALPHA));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ONE_MINUS_CONSTANT_ALPHA', CardinalSymbol, GL_ONE_MINUS_CONSTANT_ALPHA));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FUNC_ADD', CardinalSymbol, GL_FUNC_ADD));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MIN', CardinalSymbol, GL_MIN));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX', CardinalSymbol, GL_MAX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FUNC_SUBTRACT', CardinalSymbol, GL_FUNC_SUBTRACT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FUNC_REVERSE_SUBTRACT', CardinalSymbol, GL_FUNC_REVERSE_SUBTRACT));

  // color table GL 1.2 ARB imaging
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_TABLE', CardinalSymbol, GL_COLOR_TABLE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_CONVOLUTION_COLOR_TABLE', CardinalSymbol, GL_POST_CONVOLUTION_COLOR_TABLE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_COLOR_MATRIX_COLOR_TABLE', CardinalSymbol, GL_POST_COLOR_MATRIX_COLOR_TABLE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROXY_COLOR_TABLE', CardinalSymbol, GL_PROXY_COLOR_TABLE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROXY_POST_CONVOLUTION_COLOR_TABLE', CardinalSymbol, GL_PROXY_POST_CONVOLUTION_COLOR_TABLE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE', CardinalSymbol, GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_TABLE_SCALE', CardinalSymbol, GL_COLOR_TABLE_SCALE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_TABLE_BIAS', CardinalSymbol, GL_COLOR_TABLE_BIAS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_TABLE_FORMAT', CardinalSymbol, GL_COLOR_TABLE_FORMAT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_TABLE_WIDTH', CardinalSymbol, GL_COLOR_TABLE_WIDTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_TABLE_RED_SIZE', CardinalSymbol, GL_COLOR_TABLE_RED_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_TABLE_GREEN_SIZE', CardinalSymbol, GL_COLOR_TABLE_GREEN_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_TABLE_BLUE_SIZE', CardinalSymbol, GL_COLOR_TABLE_BLUE_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_TABLE_ALPHA_SIZE', CardinalSymbol, GL_COLOR_TABLE_ALPHA_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_TABLE_LUMINANCE_SIZE', CardinalSymbol, GL_COLOR_TABLE_LUMINANCE_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_TABLE_INTENSITY_SIZE', CardinalSymbol, GL_COLOR_TABLE_INTENSITY_SIZE));

  // convolutions GL 1.2 ARB imaging
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONVOLUTION_1D', CardinalSymbol, GL_CONVOLUTION_1D));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONVOLUTION_2D', CardinalSymbol, GL_CONVOLUTION_2D));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SEPARABLE_2D', CardinalSymbol, GL_SEPARABLE_2D));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONVOLUTION_BORDER_MODE', CardinalSymbol, GL_CONVOLUTION_BORDER_MODE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONVOLUTION_FILTER_SCALE', CardinalSymbol, GL_CONVOLUTION_FILTER_SCALE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONVOLUTION_FILTER_BIAS', CardinalSymbol, GL_CONVOLUTION_FILTER_BIAS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_REDUCE', CardinalSymbol, GL_REDUCE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONVOLUTION_FORMAT', CardinalSymbol, GL_CONVOLUTION_FORMAT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONVOLUTION_WIDTH', CardinalSymbol, GL_CONVOLUTION_WIDTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONVOLUTION_HEIGHT', CardinalSymbol, GL_CONVOLUTION_HEIGHT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_CONVOLUTION_WIDTH', CardinalSymbol, GL_MAX_CONVOLUTION_WIDTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_CONVOLUTION_HEIGHT', CardinalSymbol, GL_MAX_CONVOLUTION_HEIGHT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_CONVOLUTION_RED_SCALE', CardinalSymbol, GL_POST_CONVOLUTION_RED_SCALE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_CONVOLUTION_GREEN_SCALE', CardinalSymbol, GL_POST_CONVOLUTION_GREEN_SCALE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_CONVOLUTION_BLUE_SCALE', CardinalSymbol, GL_POST_CONVOLUTION_BLUE_SCALE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_CONVOLUTION_ALPHA_SCALE', CardinalSymbol, GL_POST_CONVOLUTION_ALPHA_SCALE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_CONVOLUTION_RED_BIAS', CardinalSymbol, GL_POST_CONVOLUTION_RED_BIAS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_CONVOLUTION_GREEN_BIAS', CardinalSymbol, GL_POST_CONVOLUTION_GREEN_BIAS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_CONVOLUTION_BLUE_BIAS', CardinalSymbol, GL_POST_CONVOLUTION_BLUE_BIAS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_CONVOLUTION_ALPHA_BIAS', CardinalSymbol, GL_POST_CONVOLUTION_ALPHA_BIAS));

  // histogram GL 1.2 ARB imaging
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HISTOGRAM', CardinalSymbol, GL_HISTOGRAM));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROXY_HISTOGRAM', CardinalSymbol, GL_PROXY_HISTOGRAM));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HISTOGRAM_WIDTH', CardinalSymbol, GL_HISTOGRAM_WIDTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HISTOGRAM_FORMAT', CardinalSymbol, GL_HISTOGRAM_FORMAT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HISTOGRAM_RED_SIZE', CardinalSymbol, GL_HISTOGRAM_RED_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HISTOGRAM_GREEN_SIZE', CardinalSymbol, GL_HISTOGRAM_GREEN_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HISTOGRAM_BLUE_SIZE', CardinalSymbol, GL_HISTOGRAM_BLUE_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HISTOGRAM_ALPHA_SIZE', CardinalSymbol, GL_HISTOGRAM_ALPHA_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HISTOGRAM_LUMINANCE_SIZE', CardinalSymbol, GL_HISTOGRAM_LUMINANCE_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HISTOGRAM_SINK', CardinalSymbol, GL_HISTOGRAM_SINK));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MINMAX', CardinalSymbol, GL_MINMAX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MINMAX_FORMAT', CardinalSymbol, GL_MINMAX_FORMAT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MINMAX_SINK', CardinalSymbol, GL_MINMAX_SINK));

  // buffers
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NONE', CardinalSymbol, GL_NONE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FRONT_LEFT', CardinalSymbol, GL_FRONT_LEFT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FRONT_RIGHT', CardinalSymbol, GL_FRONT_RIGHT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BACK_LEFT', CardinalSymbol, GL_BACK_LEFT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BACK_RIGHT', CardinalSymbol, GL_BACK_RIGHT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FRONT', CardinalSymbol, GL_FRONT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BACK', CardinalSymbol, GL_BACK));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LEFT', CardinalSymbol, GL_LEFT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RIGHT', CardinalSymbol, GL_RIGHT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FRONT_AND_BACK', CardinalSymbol, GL_FRONT_AND_BACK));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_AUX0', CardinalSymbol, GL_AUX0));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_AUX1', CardinalSymbol, GL_AUX1));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_AUX2', CardinalSymbol, GL_AUX2));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_AUX3', CardinalSymbol, GL_AUX3));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_AUX_BUFFERS', CardinalSymbol, GL_AUX_BUFFERS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DRAW_BUFFER', CardinalSymbol, GL_DRAW_BUFFER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_READ_BUFFER', CardinalSymbol, GL_READ_BUFFER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DOUBLEBUFFER', CardinalSymbol, GL_DOUBLEBUFFER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STEREO', CardinalSymbol, GL_STEREO));

  // depth buffer
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DEPTH_RANGE', CardinalSymbol, GL_DEPTH_RANGE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DEPTH_TEST', CardinalSymbol, GL_DEPTH_TEST));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DEPTH_WRITEMASK', CardinalSymbol, GL_DEPTH_WRITEMASK));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DEPTH_CLEAR_VALUE', CardinalSymbol, GL_DEPTH_CLEAR_VALUE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DEPTH_FUNC', CardinalSymbol, GL_DEPTH_FUNC));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NEVER', CardinalSymbol, GL_NEVER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LESS', CardinalSymbol, GL_LESS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_EQUAL', CardinalSymbol, GL_EQUAL));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LEQUAL', CardinalSymbol, GL_LEQUAL));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_GREATER', CardinalSymbol, GL_GREATER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NOTEQUAL', CardinalSymbol, GL_NOTEQUAL));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_GEQUAL', CardinalSymbol, GL_GEQUAL));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ALWAYS', CardinalSymbol, GL_ALWAYS));

  // accumulation buffer
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ACCUM', CardinalSymbol, GL_ACCUM));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LOAD', CardinalSymbol, GL_LOAD));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RETURN', CardinalSymbol, GL_RETURN));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MULT', CardinalSymbol, GL_MULT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ADD', CardinalSymbol, GL_ADD));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ACCUM_CLEAR_VALUE', CardinalSymbol, GL_ACCUM_CLEAR_VALUE));

  // feedback buffer
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FEEDBACK_BUFFER_POINTER', CardinalSymbol, GL_FEEDBACK_BUFFER_POINTER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FEEDBACK_BUFFER_SIZE', CardinalSymbol, GL_FEEDBACK_BUFFER_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FEEDBACK_BUFFER_TYPE', CardinalSymbol, GL_FEEDBACK_BUFFER_TYPE));

  // feedback types
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_2D', CardinalSymbol, GL_2D));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_3D', CardinalSymbol, GL_3D));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_3D_COLOR', CardinalSymbol, GL_3D_COLOR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_3D_COLOR_TEXTURE', CardinalSymbol, GL_3D_COLOR_TEXTURE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_4D_COLOR_TEXTURE', CardinalSymbol, GL_4D_COLOR_TEXTURE));

  // feedback tokens
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PASS_THROUGH_TOKEN', CardinalSymbol, GL_PASS_THROUGH_TOKEN));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POINT_TOKEN', CardinalSymbol, GL_POINT_TOKEN));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LINE_TOKEN', CardinalSymbol, GL_LINE_TOKEN));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POLYGON_TOKEN', CardinalSymbol, GL_POLYGON_TOKEN));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BITMAP_TOKEN', CardinalSymbol, GL_BITMAP_TOKEN));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DRAW_PIXEL_TOKEN', CardinalSymbol, GL_DRAW_PIXEL_TOKEN));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COPY_PIXEL_TOKEN', CardinalSymbol, GL_COPY_PIXEL_TOKEN));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LINE_RESET_TOKEN', CardinalSymbol, GL_LINE_RESET_TOKEN));

  // fog
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_EXP', CardinalSymbol, GL_EXP));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_EXP2', CardinalSymbol, GL_EXP2));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FOG', CardinalSymbol, GL_FOG));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FOG_INDEX', CardinalSymbol, GL_FOG_INDEX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FOG_DENSITY', CardinalSymbol, GL_FOG_DENSITY));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FOG_START', CardinalSymbol, GL_FOG_START));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FOG_END', CardinalSymbol, GL_FOG_END));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FOG_MODE', CardinalSymbol, GL_FOG_MODE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FOG_COLOR', CardinalSymbol, GL_FOG_COLOR));

  // pixel mode, transfer
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_I_TO_I', CardinalSymbol, GL_PIXEL_MAP_I_TO_I));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_S_TO_S', CardinalSymbol, GL_PIXEL_MAP_S_TO_S));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_I_TO_R', CardinalSymbol, GL_PIXEL_MAP_I_TO_R));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_I_TO_G', CardinalSymbol, GL_PIXEL_MAP_I_TO_G));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_I_TO_B', CardinalSymbol, GL_PIXEL_MAP_I_TO_B));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_I_TO_A', CardinalSymbol, GL_PIXEL_MAP_I_TO_A));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_R_TO_R', CardinalSymbol, GL_PIXEL_MAP_R_TO_R));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_G_TO_G', CardinalSymbol, GL_PIXEL_MAP_G_TO_G));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_B_TO_B', CardinalSymbol, GL_PIXEL_MAP_B_TO_B));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_A_TO_A', CardinalSymbol, GL_PIXEL_MAP_A_TO_A));

  // vertex arrays
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ARRAY_POINTER', CardinalSymbol, GL_VERTEX_ARRAY_POINTER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NORMAL_ARRAY_POINTER', CardinalSymbol, GL_NORMAL_ARRAY_POINTER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_ARRAY_POINTER', CardinalSymbol, GL_COLOR_ARRAY_POINTER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INDEX_ARRAY_POINTER', CardinalSymbol, GL_INDEX_ARRAY_POINTER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_COORD_ARRAY_POINTER', CardinalSymbol, GL_TEXTURE_COORD_ARRAY_POINTER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_EDGE_FLAG_ARRAY_POINTER', CardinalSymbol, GL_EDGE_FLAG_ARRAY_POINTER));

  // stenciling
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STENCIL_TEST', CardinalSymbol, GL_STENCIL_TEST));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STENCIL_CLEAR_VALUE', CardinalSymbol, GL_STENCIL_CLEAR_VALUE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STENCIL_FUNC', CardinalSymbol, GL_STENCIL_FUNC));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STENCIL_VALUE_MASK', CardinalSymbol, GL_STENCIL_VALUE_MASK));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STENCIL_FAIL', CardinalSymbol, GL_STENCIL_FAIL));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STENCIL_PASS_DEPTH_FAIL', CardinalSymbol, GL_STENCIL_PASS_DEPTH_FAIL));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STENCIL_PASS_DEPTH_PASS', CardinalSymbol, GL_STENCIL_PASS_DEPTH_PASS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STENCIL_REF', CardinalSymbol, GL_STENCIL_REF));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STENCIL_WRITEMASK', CardinalSymbol, GL_STENCIL_WRITEMASK));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_KEEP', CardinalSymbol, GL_KEEP));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_REPLACE', CardinalSymbol, GL_REPLACE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INCR', CardinalSymbol, GL_INCR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DECR', CardinalSymbol, GL_DECR));

  // color material
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_MATERIAL_FACE', CardinalSymbol, GL_COLOR_MATERIAL_FACE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_MATERIAL_PARAMETER', CardinalSymbol, GL_COLOR_MATERIAL_PARAMETER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_MATERIAL', CardinalSymbol, GL_COLOR_MATERIAL));

  // points
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POINT_SMOOTH', CardinalSymbol, GL_POINT_SMOOTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POINT_SIZE', CardinalSymbol, GL_POINT_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POINT_SIZE_RANGE', CardinalSymbol, GL_POINT_SIZE_RANGE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POINT_SIZE_GRANULARITY', CardinalSymbol, GL_POINT_SIZE_GRANULARITY));

  // lines
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LINE_SMOOTH', CardinalSymbol, GL_LINE_SMOOTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LINE_WIDTH', CardinalSymbol, GL_LINE_WIDTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LINE_WIDTH_RANGE', CardinalSymbol, GL_LINE_WIDTH_RANGE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LINE_WIDTH_GRANULARITY', CardinalSymbol, GL_LINE_WIDTH_GRANULARITY));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LINE_STIPPLE', CardinalSymbol, GL_LINE_STIPPLE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LINE_STIPPLE_PATTERN', CardinalSymbol, GL_LINE_STIPPLE_PATTERN));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LINE_STIPPLE_REPEAT', CardinalSymbol, GL_LINE_STIPPLE_REPEAT));

  // polygons
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POLYGON_MODE', CardinalSymbol, GL_POLYGON_MODE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POLYGON_SMOOTH', CardinalSymbol, GL_POLYGON_SMOOTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POLYGON_STIPPLE', CardinalSymbol, GL_POLYGON_STIPPLE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_EDGE_FLAG', CardinalSymbol, GL_EDGE_FLAG));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CULL_FACE', CardinalSymbol, GL_CULL_FACE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CULL_FACE_MODE', CardinalSymbol, GL_CULL_FACE_MODE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FRONT_FACE', CardinalSymbol, GL_FRONT_FACE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CW', CardinalSymbol, GL_CW));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CCW', CardinalSymbol, GL_CCW));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POINT', CardinalSymbol, GL_POINT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LINE', CardinalSymbol, GL_LINE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FILL', CardinalSymbol, GL_FILL));

  // display lists
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LIST_MODE', CardinalSymbol, GL_LIST_MODE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LIST_BASE', CardinalSymbol, GL_LIST_BASE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LIST_INDEX', CardinalSymbol, GL_LIST_INDEX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMPILE', CardinalSymbol, GL_COMPILE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMPILE_AND_EXECUTE', CardinalSymbol, GL_COMPILE_AND_EXECUTE));

  // lighting
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LIGHTING', CardinalSymbol, GL_LIGHTING));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LIGHT_MODEL_LOCAL_VIEWER', CardinalSymbol, GL_LIGHT_MODEL_LOCAL_VIEWER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LIGHT_MODEL_TWO_SIDE', CardinalSymbol, GL_LIGHT_MODEL_TWO_SIDE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LIGHT_MODEL_AMBIENT', CardinalSymbol, GL_LIGHT_MODEL_AMBIENT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LIGHT_MODEL_COLOR_CONTROL', CardinalSymbol, GL_LIGHT_MODEL_COLOR_CONTROL));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SHADE_MODEL', CardinalSymbol, GL_SHADE_MODEL));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NORMALIZE', CardinalSymbol, GL_NORMALIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_AMBIENT', CardinalSymbol, GL_AMBIENT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DIFFUSE', CardinalSymbol, GL_DIFFUSE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SPECULAR', CardinalSymbol, GL_SPECULAR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POSITION', CardinalSymbol, GL_POSITION));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SPOT_DIRECTION', CardinalSymbol, GL_SPOT_DIRECTION));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SPOT_EXPONENT', CardinalSymbol, GL_SPOT_EXPONENT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SPOT_CUTOFF', CardinalSymbol, GL_SPOT_CUTOFF));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONSTANT_ATTENUATION', CardinalSymbol, GL_CONSTANT_ATTENUATION));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LINEAR_ATTENUATION', CardinalSymbol, GL_LINEAR_ATTENUATION));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_QUADRATIC_ATTENUATION', CardinalSymbol, GL_QUADRATIC_ATTENUATION));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_EMISSION', CardinalSymbol, GL_EMISSION));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SHININESS', CardinalSymbol, GL_SHININESS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_AMBIENT_AND_DIFFUSE', CardinalSymbol, GL_AMBIENT_AND_DIFFUSE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_INDEXES', CardinalSymbol, GL_COLOR_INDEXES));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FLAT', CardinalSymbol, GL_FLAT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SMOOTH', CardinalSymbol, GL_SMOOTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LIGHT0', CardinalSymbol, GL_LIGHT0));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LIGHT1', CardinalSymbol, GL_LIGHT1));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LIGHT2', CardinalSymbol, GL_LIGHT2));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LIGHT3', CardinalSymbol, GL_LIGHT3));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LIGHT4', CardinalSymbol, GL_LIGHT4));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LIGHT5', CardinalSymbol, GL_LIGHT5));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LIGHT6', CardinalSymbol, GL_LIGHT6));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LIGHT7', CardinalSymbol, GL_LIGHT7));

  // matrix modes
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX_MODE', CardinalSymbol, GL_MATRIX_MODE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW', CardinalSymbol, GL_MODELVIEW));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROJECTION', CardinalSymbol, GL_PROJECTION));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE', CardinalSymbol, GL_TEXTURE));

  // gets
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_COLOR', CardinalSymbol, GL_CURRENT_COLOR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_INDEX', CardinalSymbol, GL_CURRENT_INDEX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_NORMAL', CardinalSymbol, GL_CURRENT_NORMAL));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_TEXTURE_COORDS', CardinalSymbol, GL_CURRENT_TEXTURE_COORDS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_RASTER_COLOR', CardinalSymbol, GL_CURRENT_RASTER_COLOR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_RASTER_INDEX', CardinalSymbol, GL_CURRENT_RASTER_INDEX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_RASTER_TEXTURE_COORDS', CardinalSymbol, GL_CURRENT_RASTER_TEXTURE_COORDS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_RASTER_POSITION', CardinalSymbol, GL_CURRENT_RASTER_POSITION));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_RASTER_POSITION_VALID', CardinalSymbol, GL_CURRENT_RASTER_POSITION_VALID));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_RASTER_DISTANCE', CardinalSymbol, GL_CURRENT_RASTER_DISTANCE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_LIST_NESTING', CardinalSymbol, GL_MAX_LIST_NESTING));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VIEWPORT', CardinalSymbol, GL_VIEWPORT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW_STACK_DEPTH', CardinalSymbol, GL_MODELVIEW_STACK_DEPTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROJECTION_STACK_DEPTH', CardinalSymbol, GL_PROJECTION_STACK_DEPTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_STACK_DEPTH', CardinalSymbol, GL_TEXTURE_STACK_DEPTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW_MATRIX', CardinalSymbol, GL_MODELVIEW_MATRIX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROJECTION_MATRIX', CardinalSymbol, GL_PROJECTION_MATRIX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_MATRIX', CardinalSymbol, GL_TEXTURE_MATRIX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ATTRIB_STACK_DEPTH', CardinalSymbol, GL_ATTRIB_STACK_DEPTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CLIENT_ATTRIB_STACK_DEPTH', CardinalSymbol, GL_CLIENT_ATTRIB_STACK_DEPTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SINGLE_COLOR', CardinalSymbol, GL_SINGLE_COLOR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SEPARATE_SPECULAR_COLOR', CardinalSymbol, GL_SEPARATE_SPECULAR_COLOR));

  // alpha testing
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ALPHA_TEST', CardinalSymbol, GL_ALPHA_TEST));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ALPHA_TEST_FUNC', CardinalSymbol, GL_ALPHA_TEST_FUNC));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ALPHA_TEST_REF', CardinalSymbol, GL_ALPHA_TEST_REF));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LOGIC_OP_MODE', CardinalSymbol, GL_LOGIC_OP_MODE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INDEX_LOGIC_OP', CardinalSymbol, GL_INDEX_LOGIC_OP));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LOGIC_OP', CardinalSymbol, GL_LOGIC_OP));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_LOGIC_OP', CardinalSymbol, GL_COLOR_LOGIC_OP));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SCISSOR_BOX', CardinalSymbol, GL_SCISSOR_BOX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SCISSOR_TEST', CardinalSymbol, GL_SCISSOR_TEST));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INDEX_CLEAR_VALUE', CardinalSymbol, GL_INDEX_CLEAR_VALUE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INDEX_WRITEMASK', CardinalSymbol, GL_INDEX_WRITEMASK));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_CLEAR_VALUE', CardinalSymbol, GL_COLOR_CLEAR_VALUE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_WRITEMASK', CardinalSymbol, GL_COLOR_WRITEMASK));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INDEX_MODE', CardinalSymbol, GL_INDEX_MODE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGBA_MODE', CardinalSymbol, GL_RGBA_MODE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RENDER_MODE', CardinalSymbol, GL_RENDER_MODE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PERSPECTIVE_CORRECTION_HINT', CardinalSymbol, GL_PERSPECTIVE_CORRECTION_HINT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POINT_SMOOTH_HINT', CardinalSymbol, GL_POINT_SMOOTH_HINT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LINE_SMOOTH_HINT', CardinalSymbol, GL_LINE_SMOOTH_HINT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POLYGON_SMOOTH_HINT', CardinalSymbol, GL_POLYGON_SMOOTH_HINT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FOG_HINT', CardinalSymbol, GL_FOG_HINT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_GEN_S', CardinalSymbol, GL_TEXTURE_GEN_S));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_GEN_T', CardinalSymbol, GL_TEXTURE_GEN_T));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_GEN_R', CardinalSymbol, GL_TEXTURE_GEN_R));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_GEN_Q', CardinalSymbol, GL_TEXTURE_GEN_Q));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_I_TO_I_SIZE', CardinalSymbol, GL_PIXEL_MAP_I_TO_I_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_S_TO_S_SIZE', CardinalSymbol, GL_PIXEL_MAP_S_TO_S_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_I_TO_R_SIZE', CardinalSymbol, GL_PIXEL_MAP_I_TO_R_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_I_TO_G_SIZE', CardinalSymbol, GL_PIXEL_MAP_I_TO_G_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_I_TO_B_SIZE', CardinalSymbol, GL_PIXEL_MAP_I_TO_B_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_I_TO_A_SIZE', CardinalSymbol, GL_PIXEL_MAP_I_TO_A_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_R_TO_R_SIZE', CardinalSymbol, GL_PIXEL_MAP_R_TO_R_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_G_TO_G_SIZE', CardinalSymbol, GL_PIXEL_MAP_G_TO_G_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_B_TO_B_SIZE', CardinalSymbol, GL_PIXEL_MAP_B_TO_B_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_MAP_A_TO_A_SIZE', CardinalSymbol, GL_PIXEL_MAP_A_TO_A_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNPACK_SWAP_BYTES', CardinalSymbol, GL_UNPACK_SWAP_BYTES));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNPACK_LSB_FIRST', CardinalSymbol, GL_UNPACK_LSB_FIRST));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNPACK_ROW_LENGTH', CardinalSymbol, GL_UNPACK_ROW_LENGTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNPACK_SKIP_ROWS', CardinalSymbol, GL_UNPACK_SKIP_ROWS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNPACK_SKIP_PIXELS', CardinalSymbol, GL_UNPACK_SKIP_PIXELS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNPACK_ALIGNMENT', CardinalSymbol, GL_UNPACK_ALIGNMENT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PACK_SWAP_BYTES', CardinalSymbol, GL_PACK_SWAP_BYTES));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PACK_LSB_FIRST', CardinalSymbol, GL_PACK_LSB_FIRST));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PACK_ROW_LENGTH', CardinalSymbol, GL_PACK_ROW_LENGTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PACK_SKIP_ROWS', CardinalSymbol, GL_PACK_SKIP_ROWS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PACK_SKIP_PIXELS', CardinalSymbol, GL_PACK_SKIP_PIXELS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PACK_ALIGNMENT', CardinalSymbol, GL_PACK_ALIGNMENT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PACK_SKIP_IMAGES', CardinalSymbol, GL_PACK_SKIP_IMAGES));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PACK_IMAGE_HEIGHT', CardinalSymbol, GL_PACK_IMAGE_HEIGHT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNPACK_SKIP_IMAGES', CardinalSymbol, GL_UNPACK_SKIP_IMAGES));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNPACK_IMAGE_HEIGHT', CardinalSymbol, GL_UNPACK_IMAGE_HEIGHT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP_COLOR', CardinalSymbol, GL_MAP_COLOR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP_STENCIL', CardinalSymbol, GL_MAP_STENCIL));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INDEX_SHIFT', CardinalSymbol, GL_INDEX_SHIFT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INDEX_OFFSET', CardinalSymbol, GL_INDEX_OFFSET));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RED_SCALE', CardinalSymbol, GL_RED_SCALE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RED_BIAS', CardinalSymbol, GL_RED_BIAS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ZOOM_X', CardinalSymbol, GL_ZOOM_X));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ZOOM_Y', CardinalSymbol, GL_ZOOM_Y));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_GREEN_SCALE', CardinalSymbol, GL_GREEN_SCALE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_GREEN_BIAS', CardinalSymbol, GL_GREEN_BIAS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BLUE_SCALE', CardinalSymbol, GL_BLUE_SCALE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BLUE_BIAS', CardinalSymbol, GL_BLUE_BIAS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ALPHA_SCALE', CardinalSymbol, GL_ALPHA_SCALE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ALPHA_BIAS', CardinalSymbol, GL_ALPHA_BIAS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DEPTH_SCALE', CardinalSymbol, GL_DEPTH_SCALE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DEPTH_BIAS', CardinalSymbol, GL_DEPTH_BIAS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_EVAL_ORDER', CardinalSymbol, GL_MAX_EVAL_ORDER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_LIGHTS', CardinalSymbol, GL_MAX_LIGHTS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_CLIP_PLANES', CardinalSymbol, GL_MAX_CLIP_PLANES));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_TEXTURE_SIZE', CardinalSymbol, GL_MAX_TEXTURE_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_3D_TEXTURE_SIZE', CardinalSymbol, GL_MAX_3D_TEXTURE_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_PIXEL_MAP_TABLE', CardinalSymbol, GL_MAX_PIXEL_MAP_TABLE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_ATTRIB_STACK_DEPTH', CardinalSymbol, GL_MAX_ATTRIB_STACK_DEPTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_MODELVIEW_STACK_DEPTH', CardinalSymbol, GL_MAX_MODELVIEW_STACK_DEPTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_NAME_STACK_DEPTH', CardinalSymbol, GL_MAX_NAME_STACK_DEPTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_PROJECTION_STACK_DEPTH', CardinalSymbol, GL_MAX_PROJECTION_STACK_DEPTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_TEXTURE_STACK_DEPTH', CardinalSymbol, GL_MAX_TEXTURE_STACK_DEPTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_VIEWPORT_DIMS', CardinalSymbol, GL_MAX_VIEWPORT_DIMS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_CLIENT_ATTRIB_STACK_DEPTH', CardinalSymbol, GL_MAX_CLIENT_ATTRIB_STACK_DEPTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_ELEMENTS_VERTICES', CardinalSymbol, GL_MAX_ELEMENTS_VERTICES));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_ELEMENTS_INDICES', CardinalSymbol, GL_MAX_ELEMENTS_INDICES));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RESCALE_NORMAL', CardinalSymbol, GL_RESCALE_NORMAL));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SUBPIXEL_BITS', CardinalSymbol, GL_SUBPIXEL_BITS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INDEX_BITS', CardinalSymbol, GL_INDEX_BITS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RED_BITS', CardinalSymbol, GL_RED_BITS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_GREEN_BITS', CardinalSymbol, GL_GREEN_BITS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BLUE_BITS', CardinalSymbol, GL_BLUE_BITS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ALPHA_BITS', CardinalSymbol, GL_ALPHA_BITS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DEPTH_BITS', CardinalSymbol, GL_DEPTH_BITS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STENCIL_BITS', CardinalSymbol, GL_STENCIL_BITS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ACCUM_RED_BITS', CardinalSymbol, GL_ACCUM_RED_BITS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ACCUM_GREEN_BITS', CardinalSymbol, GL_ACCUM_GREEN_BITS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ACCUM_BLUE_BITS', CardinalSymbol, GL_ACCUM_BLUE_BITS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ACCUM_ALPHA_BITS', CardinalSymbol, GL_ACCUM_ALPHA_BITS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NAME_STACK_DEPTH', CardinalSymbol, GL_NAME_STACK_DEPTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_AUTO_NORMAL', CardinalSymbol, GL_AUTO_NORMAL));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_COLOR_4', CardinalSymbol, GL_MAP1_COLOR_4));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_INDEX', CardinalSymbol, GL_MAP1_INDEX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_NORMAL', CardinalSymbol, GL_MAP1_NORMAL));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_TEXTURE_COORD_1', CardinalSymbol, GL_MAP1_TEXTURE_COORD_1));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_TEXTURE_COORD_2', CardinalSymbol, GL_MAP1_TEXTURE_COORD_2));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_TEXTURE_COORD_3', CardinalSymbol, GL_MAP1_TEXTURE_COORD_3));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_TEXTURE_COORD_4', CardinalSymbol, GL_MAP1_TEXTURE_COORD_4));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_VERTEX_3', CardinalSymbol, GL_MAP1_VERTEX_3));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_VERTEX_4', CardinalSymbol, GL_MAP1_VERTEX_4));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_COLOR_4', CardinalSymbol, GL_MAP2_COLOR_4));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_INDEX', CardinalSymbol, GL_MAP2_INDEX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_NORMAL', CardinalSymbol, GL_MAP2_NORMAL));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_TEXTURE_COORD_1', CardinalSymbol, GL_MAP2_TEXTURE_COORD_1));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_TEXTURE_COORD_2', CardinalSymbol, GL_MAP2_TEXTURE_COORD_2));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_TEXTURE_COORD_3', CardinalSymbol, GL_MAP2_TEXTURE_COORD_3));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_TEXTURE_COORD_4', CardinalSymbol, GL_MAP2_TEXTURE_COORD_4));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_VERTEX_3', CardinalSymbol, GL_MAP2_VERTEX_3));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_VERTEX_4', CardinalSymbol, GL_MAP2_VERTEX_4));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_GRID_DOMAIN', CardinalSymbol, GL_MAP1_GRID_DOMAIN));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_GRID_SEGMENTS', CardinalSymbol, GL_MAP1_GRID_SEGMENTS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_GRID_DOMAIN', CardinalSymbol, GL_MAP2_GRID_DOMAIN));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_GRID_SEGMENTS', CardinalSymbol, GL_MAP2_GRID_SEGMENTS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_1D', CardinalSymbol, GL_TEXTURE_1D));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_2D', CardinalSymbol, GL_TEXTURE_2D));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_3D', CardinalSymbol, GL_TEXTURE_3D));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SELECTION_BUFFER_POINTER', CardinalSymbol, GL_SELECTION_BUFFER_POINTER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SELECTION_BUFFER_SIZE', CardinalSymbol, GL_SELECTION_BUFFER_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POLYGON_OFFSET_UNITS', CardinalSymbol, GL_POLYGON_OFFSET_UNITS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POLYGON_OFFSET_POINT', CardinalSymbol, GL_POLYGON_OFFSET_POINT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POLYGON_OFFSET_LINE', CardinalSymbol, GL_POLYGON_OFFSET_LINE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POLYGON_OFFSET_FILL', CardinalSymbol, GL_POLYGON_OFFSET_FILL));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POLYGON_OFFSET_FACTOR', CardinalSymbol, GL_POLYGON_OFFSET_FACTOR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_BINDING_1D', CardinalSymbol, GL_TEXTURE_BINDING_1D));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_BINDING_2D', CardinalSymbol, GL_TEXTURE_BINDING_2D));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ARRAY', CardinalSymbol, GL_VERTEX_ARRAY));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NORMAL_ARRAY', CardinalSymbol, GL_NORMAL_ARRAY));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_ARRAY', CardinalSymbol, GL_COLOR_ARRAY));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INDEX_ARRAY', CardinalSymbol, GL_INDEX_ARRAY));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_COORD_ARRAY', CardinalSymbol, GL_TEXTURE_COORD_ARRAY));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_EDGE_FLAG_ARRAY', CardinalSymbol, GL_EDGE_FLAG_ARRAY));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ARRAY_SIZE', CardinalSymbol, GL_VERTEX_ARRAY_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ARRAY_TYPE', CardinalSymbol, GL_VERTEX_ARRAY_TYPE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ARRAY_STRIDE', CardinalSymbol, GL_VERTEX_ARRAY_STRIDE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NORMAL_ARRAY_TYPE', CardinalSymbol, GL_NORMAL_ARRAY_TYPE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NORMAL_ARRAY_STRIDE', CardinalSymbol, GL_NORMAL_ARRAY_STRIDE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_ARRAY_SIZE', CardinalSymbol, GL_COLOR_ARRAY_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_ARRAY_TYPE', CardinalSymbol, GL_COLOR_ARRAY_TYPE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_ARRAY_STRIDE', CardinalSymbol, GL_COLOR_ARRAY_STRIDE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INDEX_ARRAY_TYPE', CardinalSymbol, GL_INDEX_ARRAY_TYPE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INDEX_ARRAY_STRIDE', CardinalSymbol, GL_INDEX_ARRAY_STRIDE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_COORD_ARRAY_SIZE', CardinalSymbol, GL_TEXTURE_COORD_ARRAY_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_COORD_ARRAY_TYPE', CardinalSymbol, GL_TEXTURE_COORD_ARRAY_TYPE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_COORD_ARRAY_STRIDE', CardinalSymbol, GL_TEXTURE_COORD_ARRAY_STRIDE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_EDGE_FLAG_ARRAY_STRIDE', CardinalSymbol, GL_EDGE_FLAG_ARRAY_STRIDE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_MATRIX', CardinalSymbol, GL_COLOR_MATRIX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_MATRIX_STACK_DEPTH', CardinalSymbol, GL_COLOR_MATRIX_STACK_DEPTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_COLOR_MATRIX_STACK_DEPTH', CardinalSymbol, GL_MAX_COLOR_MATRIX_STACK_DEPTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_COLOR_MATRIX_RED_SCALE', CardinalSymbol, GL_POST_COLOR_MATRIX_RED_SCALE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_COLOR_MATRIX_GREEN_SCALE', CardinalSymbol, GL_POST_COLOR_MATRIX_GREEN_SCALE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_COLOR_MATRIX_BLUE_SCALE', CardinalSymbol, GL_POST_COLOR_MATRIX_BLUE_SCALE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_COLOR_MATRIX_ALPHA_SCALE', CardinalSymbol, GL_POST_COLOR_MATRIX_ALPHA_SCALE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_COLOR_MATRIX_RED_BIAS', CardinalSymbol, GL_POST_COLOR_MATRIX_RED_BIAS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_COLOR_MATRIX_GREEN_BIAS', CardinalSymbol, GL_POST_COLOR_MATRIX_GREEN_BIAS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_COLOR_MATRIX_BLUE_BIAS', CardinalSymbol, GL_POST_COLOR_MATRIX_BLUE_BIAS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_COLOR_MATRIX_ALPHA_BIAS', CardinalSymbol, GL_POST_COLOR_MATRIX_ALPHA_BIAS));

  // evaluators
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COEFF', CardinalSymbol, GL_COEFF));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ORDER', CardinalSymbol, GL_ORDER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DOMAIN', CardinalSymbol, GL_DOMAIN));

  // texture mapping
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_WIDTH', CardinalSymbol, GL_TEXTURE_WIDTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_HEIGHT', CardinalSymbol, GL_TEXTURE_HEIGHT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_INTERNAL_FORMAT', CardinalSymbol, GL_TEXTURE_INTERNAL_FORMAT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_COMPONENTS', CardinalSymbol, GL_TEXTURE_COMPONENTS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_BORDER_COLOR', CardinalSymbol, GL_TEXTURE_BORDER_COLOR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_BORDER', CardinalSymbol, GL_TEXTURE_BORDER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_RED_SIZE', CardinalSymbol, GL_TEXTURE_RED_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_GREEN_SIZE', CardinalSymbol, GL_TEXTURE_GREEN_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_BLUE_SIZE', CardinalSymbol, GL_TEXTURE_BLUE_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_ALPHA_SIZE', CardinalSymbol, GL_TEXTURE_ALPHA_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_LUMINANCE_SIZE', CardinalSymbol, GL_TEXTURE_LUMINANCE_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_INTENSITY_SIZE', CardinalSymbol, GL_TEXTURE_INTENSITY_SIZE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_PRIORITY', CardinalSymbol, GL_TEXTURE_PRIORITY));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_RESIDENT', CardinalSymbol, GL_TEXTURE_RESIDENT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BGR', CardinalSymbol, GL_BGR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BGRA', CardinalSymbol, GL_BGRA));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_S', CardinalSymbol, GL_S));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_T', CardinalSymbol, GL_T));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_R', CardinalSymbol, GL_R));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_Q', CardinalSymbol, GL_Q));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODULATE', CardinalSymbol, GL_MODULATE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DECAL', CardinalSymbol, GL_DECAL));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_ENV_MODE', CardinalSymbol, GL_TEXTURE_ENV_MODE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_ENV_COLOR', CardinalSymbol, GL_TEXTURE_ENV_COLOR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_ENV', CardinalSymbol, GL_TEXTURE_ENV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_EYE_LINEAR', CardinalSymbol, GL_EYE_LINEAR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OBJECT_LINEAR', CardinalSymbol, GL_OBJECT_LINEAR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SPHERE_MAP', CardinalSymbol, GL_SPHERE_MAP));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_GEN_MODE', CardinalSymbol, GL_TEXTURE_GEN_MODE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OBJECT_PLANE', CardinalSymbol, GL_OBJECT_PLANE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_EYE_PLANE', CardinalSymbol, GL_EYE_PLANE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NEAREST', CardinalSymbol, GL_NEAREST));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LINEAR', CardinalSymbol, GL_LINEAR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NEAREST_MIPMAP_NEAREST', CardinalSymbol, GL_NEAREST_MIPMAP_NEAREST));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LINEAR_MIPMAP_NEAREST', CardinalSymbol, GL_LINEAR_MIPMAP_NEAREST));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NEAREST_MIPMAP_LINEAR', CardinalSymbol, GL_NEAREST_MIPMAP_LINEAR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LINEAR_MIPMAP_LINEAR', CardinalSymbol, GL_LINEAR_MIPMAP_LINEAR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_MAG_FILTER', CardinalSymbol, GL_TEXTURE_MAG_FILTER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_MIN_FILTER', CardinalSymbol, GL_TEXTURE_MIN_FILTER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_WRAP_R', CardinalSymbol, GL_TEXTURE_WRAP_R));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_WRAP_S', CardinalSymbol, GL_TEXTURE_WRAP_S));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_WRAP_T', CardinalSymbol, GL_TEXTURE_WRAP_T));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CLAMP_TO_EDGE', CardinalSymbol, GL_CLAMP_TO_EDGE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_MIN_LOD', CardinalSymbol, GL_TEXTURE_MIN_LOD));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_MAX_LOD', CardinalSymbol, GL_TEXTURE_MAX_LOD));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_BASE_LEVEL', CardinalSymbol, GL_TEXTURE_BASE_LEVEL));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_MAX_LEVEL', CardinalSymbol, GL_TEXTURE_MAX_LEVEL));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_DEPTH', CardinalSymbol, GL_TEXTURE_DEPTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROXY_TEXTURE_1D', CardinalSymbol, GL_PROXY_TEXTURE_1D));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROXY_TEXTURE_2D', CardinalSymbol, GL_PROXY_TEXTURE_2D));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROXY_TEXTURE_3D', CardinalSymbol, GL_PROXY_TEXTURE_3D));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CLAMP', CardinalSymbol, GL_CLAMP));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_REPEAT', CardinalSymbol, GL_REPEAT));

  // hints
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DONT_CARE', CardinalSymbol, GL_DONT_CARE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FASTEST', CardinalSymbol, GL_FASTEST));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NICEST', CardinalSymbol, GL_NICEST));

  // data types
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BYTE', CardinalSymbol, GL_BYTE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_BYTE', CardinalSymbol, GL_UNSIGNED_BYTE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SHORT', CardinalSymbol, GL_SHORT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_SHORT', CardinalSymbol, GL_UNSIGNED_SHORT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INT', CardinalSymbol, GL_INT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_INT', CardinalSymbol, GL_UNSIGNED_INT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FLOAT', CardinalSymbol, GL_FLOAT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_2_BYTES', CardinalSymbol, GL_2_BYTES));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_3_BYTES', CardinalSymbol, GL_3_BYTES));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_4_BYTES', CardinalSymbol, GL_4_BYTES));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DOUBLE', CardinalSymbol, GL_DOUBLE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DOUBLE_EXT', CardinalSymbol, GL_DOUBLE_EXT));

  // logic operations
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CLEAR', CardinalSymbol, GL_CLEAR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_AND', CardinalSymbol, GL_AND));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_AND_REVERSE', CardinalSymbol, GL_AND_REVERSE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COPY', CardinalSymbol, GL_COPY));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_AND_INVERTED', CardinalSymbol, GL_AND_INVERTED));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NOOP', CardinalSymbol, GL_NOOP));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_XOR', CardinalSymbol, GL_XOR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OR', CardinalSymbol, GL_OR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NOR', CardinalSymbol, GL_NOR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_EQUIV', CardinalSymbol, GL_EQUIV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INVERT', CardinalSymbol, GL_INVERT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OR_REVERSE', CardinalSymbol, GL_OR_REVERSE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COPY_INVERTED', CardinalSymbol, GL_COPY_INVERTED));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OR_INVERTED', CardinalSymbol, GL_OR_INVERTED));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NAND', CardinalSymbol, GL_NAND));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SET', CardinalSymbol, GL_SET));

  // PixelCopyType
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR', CardinalSymbol, GL_COLOR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DEPTH', CardinalSymbol, GL_DEPTH));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STENCIL', CardinalSymbol, GL_STENCIL));

  // pixel formats
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_INDEX', CardinalSymbol, GL_COLOR_INDEX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STENCIL_INDEX', CardinalSymbol, GL_STENCIL_INDEX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DEPTH_COMPONENT', CardinalSymbol, GL_DEPTH_COMPONENT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RED', CardinalSymbol, GL_RED));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_GREEN', CardinalSymbol, GL_GREEN));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BLUE', CardinalSymbol, GL_BLUE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ALPHA', CardinalSymbol, GL_ALPHA));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB', CardinalSymbol, GL_RGB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGBA', CardinalSymbol, GL_RGBA));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE', CardinalSymbol, GL_LUMINANCE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE_ALPHA', CardinalSymbol, GL_LUMINANCE_ALPHA));

  // pixel type
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BITMAP', CardinalSymbol, GL_BITMAP));

  // rendering modes
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RENDER', CardinalSymbol, GL_RENDER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FEEDBACK', CardinalSymbol, GL_FEEDBACK));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SELECT', CardinalSymbol, GL_SELECT));

  // implementation strings
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VENDOR', CardinalSymbol, GL_VENDOR));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RENDERER', CardinalSymbol, GL_RENDERER));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERSION', CardinalSymbol, GL_VERSION));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_EXTENSIONS', CardinalSymbol, GL_EXTENSIONS));

  // pixel formats
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_R3_G3_B2', CardinalSymbol, GL_R3_G3_B2));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ALPHA4', CardinalSymbol, GL_ALPHA4));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ALPHA8', CardinalSymbol, GL_ALPHA8));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ALPHA12', CardinalSymbol, GL_ALPHA12));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ALPHA16', CardinalSymbol, GL_ALPHA16));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE4', CardinalSymbol, GL_LUMINANCE4));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE8', CardinalSymbol, GL_LUMINANCE8));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE12', CardinalSymbol, GL_LUMINANCE12));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE16', CardinalSymbol, GL_LUMINANCE16));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE4_ALPHA4', CardinalSymbol, GL_LUMINANCE4_ALPHA4));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE6_ALPHA2', CardinalSymbol, GL_LUMINANCE6_ALPHA2));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE8_ALPHA8', CardinalSymbol, GL_LUMINANCE8_ALPHA8));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE12_ALPHA4', CardinalSymbol, GL_LUMINANCE12_ALPHA4));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE12_ALPHA12', CardinalSymbol, GL_LUMINANCE12_ALPHA12));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE16_ALPHA16', CardinalSymbol, GL_LUMINANCE16_ALPHA16));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INTENSITY', CardinalSymbol, GL_INTENSITY));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INTENSITY4', CardinalSymbol, GL_INTENSITY4));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INTENSITY8', CardinalSymbol, GL_INTENSITY8));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INTENSITY12', CardinalSymbol, GL_INTENSITY12));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INTENSITY16', CardinalSymbol, GL_INTENSITY16));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB4', CardinalSymbol, GL_RGB4));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB5', CardinalSymbol, GL_RGB5));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB8', CardinalSymbol, GL_RGB8));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB10', CardinalSymbol, GL_RGB10));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB12', CardinalSymbol, GL_RGB12));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB16', CardinalSymbol, GL_RGB16));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGBA2', CardinalSymbol, GL_RGBA2));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGBA4', CardinalSymbol, GL_RGBA4));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB5_A1', CardinalSymbol, GL_RGB5_A1));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGBA8', CardinalSymbol, GL_RGBA8));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB10_A2', CardinalSymbol, GL_RGB10_A2));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGBA12', CardinalSymbol, GL_RGBA12));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGBA16', CardinalSymbol, GL_RGBA16));

  // interleaved arrays formats
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_V2F', CardinalSymbol, GL_V2F));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_V3F', CardinalSymbol, GL_V3F));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_C4UB_V2F', CardinalSymbol, GL_C4UB_V2F));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_C4UB_V3F', CardinalSymbol, GL_C4UB_V3F));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_C3F_V3F', CardinalSymbol, GL_C3F_V3F));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_N3F_V3F', CardinalSymbol, GL_N3F_V3F));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_C4F_N3F_V3F', CardinalSymbol, GL_C4F_N3F_V3F));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_T2F_V3F', CardinalSymbol, GL_T2F_V3F));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_T4F_V4F', CardinalSymbol, GL_T4F_V4F));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_T2F_C4UB_V3F', CardinalSymbol, GL_T2F_C4UB_V3F));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_T2F_C3F_V3F', CardinalSymbol, GL_T2F_C3F_V3F));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_T2F_N3F_V3F', CardinalSymbol, GL_T2F_N3F_V3F));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_T2F_C4F_N3F_V3F', CardinalSymbol, GL_T2F_C4F_N3F_V3F));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_T4F_C4F_N3F_V4F', CardinalSymbol, GL_T4F_C4F_N3F_V4F));

  // clip planes
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CLIP_PLANE0', CardinalSymbol, GL_CLIP_PLANE0));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CLIP_PLANE1', CardinalSymbol, GL_CLIP_PLANE1));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CLIP_PLANE2', CardinalSymbol, GL_CLIP_PLANE2));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CLIP_PLANE3', CardinalSymbol, GL_CLIP_PLANE3));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CLIP_PLANE4', CardinalSymbol, GL_CLIP_PLANE4));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CLIP_PLANE5', CardinalSymbol, GL_CLIP_PLANE5));

  // miscellaneous
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DITHER', CardinalSymbol, GL_DITHER));

  // Add extensions
  AddExtensionUnitSymbols(SymbolTable);

  // ---------- GL procedures ----------

  TGLPushAttrib.Create(SymbolTable, 'glPushAttrib', ['mask', 'Cardinal'], '');
  TGLPopAttrib.Create(SymbolTable, 'glPopAttrib', [], '');
  TGLPushClientAttrib.Create(SymbolTable, 'glPushClientAttrib', ['mask', 'Cardinal'], '');
  TGLPopClientAttrib.Create(SymbolTable, 'glPopClientAttrib', [], '');

  TGLEnable.Create(SymbolTable, 'glEnable', ['cap', 'Cardinal'], '');
  TGLDisable.Create(SymbolTable, 'glDisable', ['cap', 'Cardinal'], '');
  TGLEnableClientState.Create(SymbolTable, 'glEnableClientState', ['aarray', 'Cardinal'], '');
  TGLDisableClientState.Create(SymbolTable, 'glDisableClientState', ['aarray', 'Cardinal'], '');

  TGLMatrixMode.Create(SymbolTable, 'glMatrixMode', ['mode', 'Cardinal'], '');
  TGLPushMatrix.Create(SymbolTable, 'glPushMatrix', [], '');
  TGLPopMatrix.Create(SymbolTable, 'glPopMatrix', [], '');
  TGLLoadIdentity.Create(SymbolTable, 'glLoadIdentity', [], '');
  TGLLoadMatrixf.Create(SymbolTable, 'glLoadMatrixf', ['m', 'TGLMatrix'], '');
  TGLTranslatef.Create(SymbolTable, 'glTranslatef', ['x', 'Float', 'y', 'Float', 'z', 'Float'], '');
  TGLRotatef.Create(SymbolTable, 'glRotatef', ['angle', 'Float', 'x', 'Float', 'y', 'Float', 'z', 'Float'], '');
  TGLScalef.Create(SymbolTable, 'glScalef', ['x', 'Float', 'y', 'Float', 'z', 'Float'], '');

  TGLShadeModel.Create(SymbolTable, 'glShadeModel', ['mode', 'Cardinal'], '');
  TGLCullFace.Create(SymbolTable, 'glCullFace', ['mode', 'Cardinal'], '');
  TGLFrontFace.Create(SymbolTable, 'glFrontFace', ['mode', 'Cardinal'], '');
  TGLPolygonMode.Create(SymbolTable, 'glPolygonMode', ['face', 'Cardinal', 'mode', 'Cardinal'], '');
  TGLBegin.Create(SymbolTable, 'glBegin', ['mode', 'Cardinal'], '');
  TGLEnd.Create(SymbolTable, 'glEnd', [], '');
  TGLColor3f.Create(SymbolTable, 'glColor3f', ['red', 'Float', 'green', 'Float', 'blue', 'Float'], '');
  TGLColor4f.Create(SymbolTable, 'glColor4f', ['red', 'Float', 'green', 'Float', 'blue', 'Float', 'alpha', 'Float'], '');
  TGLNormal3f.Create(SymbolTable, 'glNormal3f', ['x', 'Float', 'y', 'Float', 'z', 'Float'], '');
  TGLVertex3f.Create(SymbolTable, 'glVertex3f', ['x', 'Float', 'y', 'Float', 'z', 'Float'], '');
  TGLTexCoord1f.Create(SymbolTable, 'glTexCoord1f', ['s', 'Float'], '');
  TGLTexCoord2f.Create(SymbolTable, 'glTexCoord2f', ['s', 'Float', 't', 'Float'], '');
  TGLTexCoord3f.Create(SymbolTable, 'glTexCoord3f', ['s', 'Float', 't', 'Float', 'r', 'Float'], '');
  TGLTexCoord4f.Create(SymbolTable, 'glTexCoord4f', ['s', 'Float', 't', 'Float', 'r', 'Float', 'q', 'Float'], '');
  TGLLineWidth.Create(SymbolTable, 'glLineWidth', ['width', 'Float'], '');

  TGLMultiTexCoord1f.Create(SymbolTable, 'glMultiTexCoord1f', ['target', 'Cardinal', 's', 'Float'], '');
  TGLMultiTexCoord2f.Create(SymbolTable, 'glMultiTexCoord2f', ['target', 'Cardinal', 's', 'Float', 't', 'Float'], '');
  TGLMultiTexCoord3f.Create(SymbolTable, 'glMultiTexCoord3f', ['target', 'Cardinal', 's', 'Float', 't', 'Float', 'r', 'Float'], '');
  TGLMultiTexCoord4f.Create(SymbolTable, 'glMultiTexCoord4f', ['target', 'Cardinal', 's', 'Float', 't', 'Float', 'r', 'Float', 'q', 'Float'], '');
  TGLActiveTexture.Create(SymbolTable, 'glActiveTexture', ['target', 'Cardinal'], '');
  TGLClientActiveTexture.Create(SymbolTable, 'glClientActiveTexture', ['target', 'Cardinal'], '');

  TGLTexEnvf.Create(SymbolTable, 'glTexEnvf', ['target', 'Cardinal', 'pname', 'Cardinal', 'param', 'Float'], '');
  TGLTexEnvi.Create(SymbolTable, 'glTexEnvi', ['target', 'Cardinal', 'pname', 'Cardinal', 'param', 'Integer'], '');

  TGLBlendFunc.Create(SymbolTable, 'glBlendFunc', ['sfactor', 'Cardinal', 'dfactor', 'Cardinal'], '');

  TGLDepthFunc.Create(SymbolTable, 'glDepthFunc', ['func', 'Cardinal'], '');
  TGLDepthMask.Create(SymbolTable, 'glDepthMask', ['flag', 'Byte'], '');
  TGLDepthRange.Create(SymbolTable, 'glDepthRange', ['znear', 'Float', 'zfar', 'Float'], '');

  TGLStencilFunc.Create(SymbolTable, 'glStencilFunc', ['func', 'Cardinal', 'ref', 'Integer', 'mask', 'Cardinal'], '');
  TGLStencilMask.Create(SymbolTable, 'glStencilMask', ['mask', 'Cardinal'], '');
  TGLStencilOp.Create(SymbolTable, 'glStencilOp', ['fail', 'Cardinal', 'zfail', 'Cardinal', 'zpass', 'Cardinal'], '');

  TGLLogicOp.Create(SymbolTable, 'glLogicOp', ['opcode', 'Cardinal'], '');
end;

procedure TdwsOpenGLUnit.AddExtensionUnitSymbols(SymbolTable: TSymbolTable);
var
  CardinalSymbol,
  ByteSymbol : TSymbol;
begin
  CardinalSymbol:=SymbolTable.FindSymbol('Cardinal');
  if not Assigned(CardinalSymbol) then 
  begin
    CardinalSymbol:=TBaseSymbol.Create('Cardinal', TypCardinalID, VarAsType(0, varLongWord));
    SymbolTable.AddSymbol(CardinalSymbol);
  end;

  ByteSymbol:=SymbolTable.FindSymbol('Byte');
  if not Assigned(ByteSymbol) then 
  begin
    ByteSymbol:=TBaseSymbol.Create('Byte', TypByteID, VarAsType(0, varByte));
    SymbolTable.AddSymbol(ByteSymbol);
  end;

  // ---------- extensions enumerants ----------

  // EXT_abgr
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ABGR_EXT', CardinalSymbol, GL_ABGR_EXT));

  // EXT_packed_pixels
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_BYTE_3_3_2_EXT', CardinalSymbol, GL_UNSIGNED_BYTE_3_3_2_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_SHORT_4_4_4_4_EXT', CardinalSymbol, GL_UNSIGNED_SHORT_4_4_4_4_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_SHORT_5_5_5_1_EXT', CardinalSymbol, GL_UNSIGNED_SHORT_5_5_5_1_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_INT_8_8_8_8_EXT', CardinalSymbol, GL_UNSIGNED_INT_8_8_8_8_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_INT_10_10_10_2_EXT', CardinalSymbol, GL_UNSIGNED_INT_10_10_10_2_EXT));

  // EXT_bgra
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BGR_EXT', CardinalSymbol, GL_BGR_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BGRA_EXT', CardinalSymbol, GL_BGRA_EXT));

  // EXT_paletted_texture
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_INDEX1_EXT', CardinalSymbol, GL_COLOR_INDEX1_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_INDEX2_EXT', CardinalSymbol, GL_COLOR_INDEX2_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_INDEX4_EXT', CardinalSymbol, GL_COLOR_INDEX4_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_INDEX8_EXT', CardinalSymbol, GL_COLOR_INDEX8_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_INDEX12_EXT', CardinalSymbol, GL_COLOR_INDEX12_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_INDEX16_EXT', CardinalSymbol, GL_COLOR_INDEX16_EXT));

  // EXT_blend_color
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONSTANT_COLOR_EXT', CardinalSymbol, GL_CONSTANT_COLOR_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ONE_MINUS_CONSTANT_COLOR_EXT', CardinalSymbol, GL_ONE_MINUS_CONSTANT_COLOR_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONSTANT_ALPHA_EXT', CardinalSymbol, GL_CONSTANT_ALPHA_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ONE_MINUS_CONSTANT_ALPHA_EXT', CardinalSymbol, GL_ONE_MINUS_CONSTANT_ALPHA_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BLEND_COLOR_EXT', CardinalSymbol, GL_BLEND_COLOR_EXT));

  // EXT_blend_minmax
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FUNC_ADD_EXT', CardinalSymbol, GL_FUNC_ADD_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MIN_EXT', CardinalSymbol, GL_MIN_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_EXT', CardinalSymbol, GL_MAX_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BLEND_EQUATION_EXT', CardinalSymbol, GL_BLEND_EQUATION_EXT));

  // EXT_blend_subtract
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FUNC_SUBTRACT_EXT', CardinalSymbol, GL_FUNC_SUBTRACT_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FUNC_REVERSE_SUBTRACT_EXT', CardinalSymbol, GL_FUNC_REVERSE_SUBTRACT_EXT));

  // EXT_convolution
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONVOLUTION_1D_EXT', CardinalSymbol, GL_CONVOLUTION_1D_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONVOLUTION_2D_EXT', CardinalSymbol, GL_CONVOLUTION_2D_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SEPARABLE_2D_EXT', CardinalSymbol, GL_SEPARABLE_2D_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONVOLUTION_BORDER_MODE_EXT', CardinalSymbol, GL_CONVOLUTION_BORDER_MODE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONVOLUTION_FILTER_SCALE_EXT', CardinalSymbol, GL_CONVOLUTION_FILTER_SCALE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONVOLUTION_FILTER_BIAS_EXT', CardinalSymbol, GL_CONVOLUTION_FILTER_BIAS_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_REDUCE_EXT', CardinalSymbol, GL_REDUCE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONVOLUTION_FORMAT_EXT', CardinalSymbol, GL_CONVOLUTION_FORMAT_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONVOLUTION_WIDTH_EXT', CardinalSymbol, GL_CONVOLUTION_WIDTH_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONVOLUTION_HEIGHT_EXT', CardinalSymbol, GL_CONVOLUTION_HEIGHT_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_CONVOLUTION_WIDTH_EXT', CardinalSymbol, GL_MAX_CONVOLUTION_WIDTH_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_CONVOLUTION_HEIGHT_EXT', CardinalSymbol, GL_MAX_CONVOLUTION_HEIGHT_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_CONVOLUTION_RED_SCALE_EXT', CardinalSymbol, GL_POST_CONVOLUTION_RED_SCALE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_CONVOLUTION_GREEN_SCALE_EXT', CardinalSymbol, GL_POST_CONVOLUTION_GREEN_SCALE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_CONVOLUTION_BLUE_SCALE_EXT', CardinalSymbol, GL_POST_CONVOLUTION_BLUE_SCALE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_CONVOLUTION_ALPHA_SCALE_EXT', CardinalSymbol, GL_POST_CONVOLUTION_ALPHA_SCALE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_CONVOLUTION_RED_BIAS_EXT', CardinalSymbol, GL_POST_CONVOLUTION_RED_BIAS_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_CONVOLUTION_GREEN_BIAS_EXT', CardinalSymbol, GL_POST_CONVOLUTION_GREEN_BIAS_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_CONVOLUTION_BLUE_BIAS_EXT', CardinalSymbol, GL_POST_CONVOLUTION_BLUE_BIAS_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_CONVOLUTION_ALPHA_BIAS_EXT', CardinalSymbol, GL_POST_CONVOLUTION_ALPHA_BIAS_EXT));

  // EXT_histogram
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HISTOGRAM_EXT', CardinalSymbol, GL_HISTOGRAM_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROXY_HISTOGRAM_EXT', CardinalSymbol, GL_PROXY_HISTOGRAM_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HISTOGRAM_WIDTH_EXT', CardinalSymbol, GL_HISTOGRAM_WIDTH_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HISTOGRAM_FORMAT_EXT', CardinalSymbol, GL_HISTOGRAM_FORMAT_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HISTOGRAM_RED_SIZE_EXT', CardinalSymbol, GL_HISTOGRAM_RED_SIZE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HISTOGRAM_GREEN_SIZE_EXT', CardinalSymbol, GL_HISTOGRAM_GREEN_SIZE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HISTOGRAM_BLUE_SIZE_EXT', CardinalSymbol, GL_HISTOGRAM_BLUE_SIZE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HISTOGRAM_ALPHA_SIZE_EXT', CardinalSymbol, GL_HISTOGRAM_ALPHA_SIZE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HISTOGRAM_LUMINANCE_SIZE_EXT', CardinalSymbol, GL_HISTOGRAM_LUMINANCE_SIZE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HISTOGRAM_SINK_EXT', CardinalSymbol, GL_HISTOGRAM_SINK_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MINMAX_EXT', CardinalSymbol, GL_MINMAX_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MINMAX_FORMAT_EXT', CardinalSymbol, GL_MINMAX_FORMAT_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MINMAX_SINK_EXT', CardinalSymbol, GL_MINMAX_SINK_EXT));

  // EXT_polygon_offset
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POLYGON_OFFSET_EXT', CardinalSymbol, GL_POLYGON_OFFSET_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POLYGON_OFFSET_FACTOR_EXT', CardinalSymbol, GL_POLYGON_OFFSET_FACTOR_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POLYGON_OFFSET_BIAS_EXT', CardinalSymbol, GL_POLYGON_OFFSET_BIAS_EXT));

  // EXT_texture
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ALPHA4_EXT', CardinalSymbol, GL_ALPHA4_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ALPHA8_EXT', CardinalSymbol, GL_ALPHA8_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ALPHA12_EXT', CardinalSymbol, GL_ALPHA12_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ALPHA16_EXT', CardinalSymbol, GL_ALPHA16_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE4_EXT', CardinalSymbol, GL_LUMINANCE4_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE8_EXT', CardinalSymbol, GL_LUMINANCE8_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE12_EXT', CardinalSymbol, GL_LUMINANCE12_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE16_EXT', CardinalSymbol, GL_LUMINANCE16_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE4_ALPHA4_EXT', CardinalSymbol, GL_LUMINANCE4_ALPHA4_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE6_ALPHA2_EXT', CardinalSymbol, GL_LUMINANCE6_ALPHA2_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE8_ALPHA8_EXT', CardinalSymbol, GL_LUMINANCE8_ALPHA8_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE12_ALPHA4_EXT', CardinalSymbol, GL_LUMINANCE12_ALPHA4_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE12_ALPHA12_EXT', CardinalSymbol, GL_LUMINANCE12_ALPHA12_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LUMINANCE16_ALPHA16_EXT', CardinalSymbol, GL_LUMINANCE16_ALPHA16_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INTENSITY_EXT', CardinalSymbol, GL_INTENSITY_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INTENSITY4_EXT', CardinalSymbol, GL_INTENSITY4_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INTENSITY8_EXT', CardinalSymbol, GL_INTENSITY8_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INTENSITY12_EXT', CardinalSymbol, GL_INTENSITY12_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INTENSITY16_EXT', CardinalSymbol, GL_INTENSITY16_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB2_EXT', CardinalSymbol, GL_RGB2_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB4_EXT', CardinalSymbol, GL_RGB4_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB5_EXT', CardinalSymbol, GL_RGB5_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB8_EXT', CardinalSymbol, GL_RGB8_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB10_EXT', CardinalSymbol, GL_RGB10_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB12_EXT', CardinalSymbol, GL_RGB12_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB16_EXT', CardinalSymbol, GL_RGB16_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGBA2_EXT', CardinalSymbol, GL_RGBA2_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGBA4_EXT', CardinalSymbol, GL_RGBA4_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB5_A1_EXT', CardinalSymbol, GL_RGB5_A1_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGBA8_EXT', CardinalSymbol, GL_RGBA8_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB10_A2_EXT', CardinalSymbol, GL_RGB10_A2_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGBA12_EXT', CardinalSymbol, GL_RGBA12_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGBA16_EXT', CardinalSymbol, GL_RGBA16_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_RED_SIZE_EXT', CardinalSymbol, GL_TEXTURE_RED_SIZE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_GREEN_SIZE_EXT', CardinalSymbol, GL_TEXTURE_GREEN_SIZE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_BLUE_SIZE_EXT', CardinalSymbol, GL_TEXTURE_BLUE_SIZE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_ALPHA_SIZE_EXT', CardinalSymbol, GL_TEXTURE_ALPHA_SIZE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_LUMINANCE_SIZE_EXT', CardinalSymbol, GL_TEXTURE_LUMINANCE_SIZE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_INTENSITY_SIZE_EXT', CardinalSymbol, GL_TEXTURE_INTENSITY_SIZE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_REPLACE_EXT', CardinalSymbol, GL_REPLACE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROXY_TEXTURE_1D_EXT', CardinalSymbol, GL_PROXY_TEXTURE_1D_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROXY_TEXTURE_2D_EXT', CardinalSymbol, GL_PROXY_TEXTURE_2D_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_TOO_LARGE_EXT', CardinalSymbol, GL_TEXTURE_TOO_LARGE_EXT));

  // EXT_texture_object
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_PRIORITY_EXT', CardinalSymbol, GL_TEXTURE_PRIORITY_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_RESIDENT_EXT', CardinalSymbol, GL_TEXTURE_RESIDENT_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_1D_BINDING_EXT', CardinalSymbol, GL_TEXTURE_1D_BINDING_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_2D_BINDING_EXT', CardinalSymbol, GL_TEXTURE_2D_BINDING_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_3D_BINDING_EXT', CardinalSymbol, GL_TEXTURE_3D_BINDING_EXT));

  // EXT_texture3D
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PACK_SKIP_IMAGES_EXT', CardinalSymbol, GL_PACK_SKIP_IMAGES_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PACK_IMAGE_HEIGHT_EXT', CardinalSymbol, GL_PACK_IMAGE_HEIGHT_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNPACK_SKIP_IMAGES_EXT', CardinalSymbol, GL_UNPACK_SKIP_IMAGES_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNPACK_IMAGE_HEIGHT_EXT', CardinalSymbol, GL_UNPACK_IMAGE_HEIGHT_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_3D_EXT', CardinalSymbol, GL_TEXTURE_3D_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROXY_TEXTURE_3D_EXT', CardinalSymbol, GL_PROXY_TEXTURE_3D_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_DEPTH_EXT', CardinalSymbol, GL_TEXTURE_DEPTH_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_WRAP_R_EXT', CardinalSymbol, GL_TEXTURE_WRAP_R_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_3D_TEXTURE_SIZE_EXT', CardinalSymbol, GL_MAX_3D_TEXTURE_SIZE_EXT));

  // SGI_color_matrix
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_MATRIX_SGI', CardinalSymbol, GL_COLOR_MATRIX_SGI));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_MATRIX_STACK_DEPTH_SGI', CardinalSymbol, GL_COLOR_MATRIX_STACK_DEPTH_SGI));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI', CardinalSymbol, GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_COLOR_MATRIX_RED_SCALE_SGI', CardinalSymbol, GL_POST_COLOR_MATRIX_RED_SCALE_SGI));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI', CardinalSymbol, GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI', CardinalSymbol, GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI', CardinalSymbol, GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_COLOR_MATRIX_RED_BIAS_SGI', CardinalSymbol, GL_POST_COLOR_MATRIX_RED_BIAS_SGI));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI', CardinalSymbol, GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI', CardinalSymbol, GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI', CardinalSymbol, GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI));

  // ARB_point_parameters
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POINT_SIZE_MIN_ARB', CardinalSymbol, GL_POINT_SIZE_MIN_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POINT_SIZE_MAX_ARB', CardinalSymbol, GL_POINT_SIZE_MAX_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_POINT_FADE_THRESHOLD_SIZE_ARB', CardinalSymbol, GL_POINT_FADE_THRESHOLD_SIZE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DISTANCE_ATTENUATION_ARB', CardinalSymbol, GL_DISTANCE_ATTENUATION_ARB));

  // EXT_rescale_normal
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RESCALE_NORMAL_EXT', CardinalSymbol, GL_RESCALE_NORMAL_EXT));

  // EXT_shared_texture_palette
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SHARED_TEXTURE_PALETTE_EXT', CardinalSymbol, GL_SHARED_TEXTURE_PALETTE_EXT));

  // EXT_compiled_vertex_array
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ARRAY_ELEMENT_LOCK_FIRST_EXT', CardinalSymbol, GL_ARRAY_ELEMENT_LOCK_FIRST_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ARRAY_ELEMENT_LOCK_COUNT_EXT', CardinalSymbol, GL_ARRAY_ELEMENT_LOCK_COUNT_EXT));

  // ARB_multitexture
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ACTIVE_TEXTURE_ARB', CardinalSymbol, GL_ACTIVE_TEXTURE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CLIENT_ACTIVE_TEXTURE_ARB', CardinalSymbol, GL_CLIENT_ACTIVE_TEXTURE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_TEXTURE_UNITS_ARB', CardinalSymbol, GL_MAX_TEXTURE_UNITS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE0_ARB', CardinalSymbol, GL_TEXTURE0_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE1_ARB', CardinalSymbol, GL_TEXTURE1_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE2_ARB', CardinalSymbol, GL_TEXTURE2_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE3_ARB', CardinalSymbol, GL_TEXTURE3_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE4_ARB', CardinalSymbol, GL_TEXTURE4_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE5_ARB', CardinalSymbol, GL_TEXTURE5_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE6_ARB', CardinalSymbol, GL_TEXTURE6_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE7_ARB', CardinalSymbol, GL_TEXTURE7_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE8_ARB', CardinalSymbol, GL_TEXTURE8_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE9_ARB', CardinalSymbol, GL_TEXTURE9_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE10_ARB', CardinalSymbol, GL_TEXTURE10_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE11_ARB', CardinalSymbol, GL_TEXTURE11_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE12_ARB', CardinalSymbol, GL_TEXTURE12_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE13_ARB', CardinalSymbol, GL_TEXTURE13_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE14_ARB', CardinalSymbol, GL_TEXTURE14_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE15_ARB', CardinalSymbol, GL_TEXTURE15_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE16_ARB', CardinalSymbol, GL_TEXTURE16_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE17_ARB', CardinalSymbol, GL_TEXTURE17_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE18_ARB', CardinalSymbol, GL_TEXTURE18_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE19_ARB', CardinalSymbol, GL_TEXTURE19_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE20_ARB', CardinalSymbol, GL_TEXTURE20_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE21_ARB', CardinalSymbol, GL_TEXTURE21_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE22_ARB', CardinalSymbol, GL_TEXTURE22_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE23_ARB', CardinalSymbol, GL_TEXTURE23_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE24_ARB', CardinalSymbol, GL_TEXTURE24_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE25_ARB', CardinalSymbol, GL_TEXTURE25_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE26_ARB', CardinalSymbol, GL_TEXTURE26_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE27_ARB', CardinalSymbol, GL_TEXTURE27_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE28_ARB', CardinalSymbol, GL_TEXTURE28_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE29_ARB', CardinalSymbol, GL_TEXTURE29_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE30_ARB', CardinalSymbol, GL_TEXTURE30_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE31_ARB', CardinalSymbol, GL_TEXTURE31_ARB));

  // EXT_stencil_wrap
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INCR_WRAP_EXT', CardinalSymbol, GL_INCR_WRAP_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DECR_WRAP_EXT', CardinalSymbol, GL_DECR_WRAP_EXT));

  // EXT_stencil_two_side
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STENCIL_TEST_TWO_SIDE_EXT', CardinalSymbol, GL_STENCIL_TEST_TWO_SIDE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ACTIVE_STENCIL_FACE_EXT', CardinalSymbol, GL_ACTIVE_STENCIL_FACE_EXT));

  // NV_texgen_reflection
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NORMAL_MAP_NV', CardinalSymbol, GL_NORMAL_MAP_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_REFLECTION_MAP_NV', CardinalSymbol, GL_REFLECTION_MAP_NV));

  // NV_fence
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ALL_COMPLETED_NV', CardinalSymbol, GL_ALL_COMPLETED_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FENCE_STATUS_NV', CardinalSymbol, GL_FENCE_STATUS_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FENCE_CONDITION_NV', CardinalSymbol, GL_FENCE_CONDITION_NV));

  // NV_occlusion_query
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_COUNTER_BITS_NV', CardinalSymbol, GL_PIXEL_COUNTER_BITS_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_OCCLUSION_QUERY_ID_NV', CardinalSymbol, GL_CURRENT_OCCLUSION_QUERY_ID_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_COUNT_NV', CardinalSymbol, GL_PIXEL_COUNT_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PIXEL_COUNT_AVAILABLE_NV', CardinalSymbol, GL_PIXEL_COUNT_AVAILABLE_NV));

  // NV_texture_rectangle
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_RECTANGLE_NV', CardinalSymbol, GL_TEXTURE_RECTANGLE_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_BINDING_RECTANGLE_NV', CardinalSymbol, GL_TEXTURE_BINDING_RECTANGLE_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROXY_TEXTURE_RECTANGLE_NV', CardinalSymbol, GL_PROXY_TEXTURE_RECTANGLE_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_RECTANGLE_TEXTURE_SIZE_NV', CardinalSymbol, GL_MAX_RECTANGLE_TEXTURE_SIZE_NV));

  // EXT_texture_env_combine
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINE_EXT', CardinalSymbol, GL_COMBINE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINE_RGB_EXT', CardinalSymbol, GL_COMBINE_RGB_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINE_ALPHA_EXT', CardinalSymbol, GL_COMBINE_ALPHA_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB_SCALE_EXT', CardinalSymbol, GL_RGB_SCALE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ADD_SIGNED_EXT', CardinalSymbol, GL_ADD_SIGNED_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INTERPOLATE_EXT', CardinalSymbol, GL_INTERPOLATE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONSTANT_EXT', CardinalSymbol, GL_CONSTANT_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PRIMARY_COLOR_EXT', CardinalSymbol, GL_PRIMARY_COLOR_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PREVIOUS_EXT', CardinalSymbol, GL_PREVIOUS_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE0_RGB_EXT', CardinalSymbol, GL_SOURCE0_RGB_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE1_RGB_EXT', CardinalSymbol, GL_SOURCE1_RGB_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE2_RGB_EXT', CardinalSymbol, GL_SOURCE2_RGB_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE0_ALPHA_EXT', CardinalSymbol, GL_SOURCE0_ALPHA_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE1_ALPHA_EXT', CardinalSymbol, GL_SOURCE1_ALPHA_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE2_ALPHA_EXT', CardinalSymbol, GL_SOURCE2_ALPHA_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND0_RGB_EXT', CardinalSymbol, GL_OPERAND0_RGB_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND1_RGB_EXT', CardinalSymbol, GL_OPERAND1_RGB_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND2_RGB_EXT', CardinalSymbol, GL_OPERAND2_RGB_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND0_ALPHA_EXT', CardinalSymbol, GL_OPERAND0_ALPHA_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND1_ALPHA_EXT', CardinalSymbol, GL_OPERAND1_ALPHA_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND2_ALPHA_EXT', CardinalSymbol, GL_OPERAND2_ALPHA_EXT));

  // ARB_texture_env_combine
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINE_ARB', CardinalSymbol, GL_COMBINE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINE_RGB_ARB', CardinalSymbol, GL_COMBINE_RGB_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINE_ALPHA_ARB', CardinalSymbol, GL_COMBINE_ALPHA_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE0_RGB_ARB', CardinalSymbol, GL_SOURCE0_RGB_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE1_RGB_ARB', CardinalSymbol, GL_SOURCE1_RGB_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE2_RGB_ARB', CardinalSymbol, GL_SOURCE2_RGB_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE0_ALPHA_ARB', CardinalSymbol, GL_SOURCE0_ALPHA_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE1_ALPHA_ARB', CardinalSymbol, GL_SOURCE1_ALPHA_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE2_ALPHA_ARB', CardinalSymbol, GL_SOURCE2_ALPHA_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND0_RGB_ARB', CardinalSymbol, GL_OPERAND0_RGB_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND1_RGB_ARB', CardinalSymbol, GL_OPERAND1_RGB_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND2_RGB_ARB', CardinalSymbol, GL_OPERAND2_RGB_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND0_ALPHA_ARB', CardinalSymbol, GL_OPERAND0_ALPHA_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND1_ALPHA_ARB', CardinalSymbol, GL_OPERAND1_ALPHA_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND2_ALPHA_ARB', CardinalSymbol, GL_OPERAND2_ALPHA_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RGB_SCALE_ARB', CardinalSymbol, GL_RGB_SCALE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ADD_SIGNED_ARB', CardinalSymbol, GL_ADD_SIGNED_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INTERPOLATE_ARB', CardinalSymbol, GL_INTERPOLATE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SUBTRACT_ARB', CardinalSymbol, GL_SUBTRACT_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONSTANT_ARB', CardinalSymbol, GL_CONSTANT_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONSTANT_COLOR_ARB', CardinalSymbol, GL_CONSTANT_COLOR_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PRIMARY_COLOR_ARB', CardinalSymbol, GL_PRIMARY_COLOR_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PREVIOUS_ARB', CardinalSymbol, GL_PREVIOUS_ARB));

  // ARB_texture_env_dot3
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DOT3_RGB_ARB', CardinalSymbol, GL_DOT3_RGB_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DOT3_RGBA_ARB', CardinalSymbol, GL_DOT3_RGBA_ARB));

  // ARB_vertex_program
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_PROGRAM_ARB', CardinalSymbol, GL_VERTEX_PROGRAM_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_PROGRAM_POINT_SIZE_ARB', CardinalSymbol, GL_VERTEX_PROGRAM_POINT_SIZE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_PROGRAM_TWO_SIDE_ARB', CardinalSymbol, GL_VERTEX_PROGRAM_TWO_SIDE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_SUM_ARB', CardinalSymbol, GL_COLOR_SUM_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_FORMAT_ASCII_ARB', CardinalSymbol, GL_PROGRAM_FORMAT_ASCII_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_VERTEX_ATTRIB_ARB', CardinalSymbol, GL_CURRENT_VERTEX_ATTRIB_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_LENGTH_ARB', CardinalSymbol, GL_PROGRAM_LENGTH_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_FORMAT_ARB', CardinalSymbol, GL_PROGRAM_FORMAT_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_BINDING_ARB', CardinalSymbol, GL_PROGRAM_BINDING_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_INSTRUCTIONS_ARB', CardinalSymbol, GL_PROGRAM_INSTRUCTIONS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_PROGRAM_INSTRUCTIONS_ARB', CardinalSymbol, GL_MAX_PROGRAM_INSTRUCTIONS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_NATIVE_INSTRUCTIONS_ARB', CardinalSymbol, GL_PROGRAM_NATIVE_INSTRUCTIONS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_PROGRAM_NATIVE_INSTRUCTIONS_ARB', CardinalSymbol, GL_MAX_PROGRAM_NATIVE_INSTRUCTIONS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_TEMPORARIES_ARB', CardinalSymbol, GL_PROGRAM_TEMPORARIES_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_PROGRAM_TEMPORARIES_ARB', CardinalSymbol, GL_MAX_PROGRAM_TEMPORARIES_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_NATIVE_TEMPORARIES_ARB', CardinalSymbol, GL_PROGRAM_NATIVE_TEMPORARIES_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_PROGRAM_NATIVE_TEMPORARIES_ARB', CardinalSymbol, GL_MAX_PROGRAM_NATIVE_TEMPORARIES_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_PARAMETERS_ARB', CardinalSymbol, GL_PROGRAM_PARAMETERS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_PROGRAM_PARAMETERS_ARB', CardinalSymbol, GL_MAX_PROGRAM_PARAMETERS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_NATIVE_PARAMETERS_ARB', CardinalSymbol, GL_PROGRAM_NATIVE_PARAMETERS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_PROGRAM_NATIVE_PARAMETERS_ARB', CardinalSymbol, GL_MAX_PROGRAM_NATIVE_PARAMETERS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_ATTRIBS_ARB', CardinalSymbol, GL_PROGRAM_ATTRIBS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_PROGRAM_ATTRIBS_ARB', CardinalSymbol, GL_MAX_PROGRAM_ATTRIBS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_NATIVE_ATTRIBS_ARB', CardinalSymbol, GL_PROGRAM_NATIVE_ATTRIBS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_PROGRAM_NATIVE_ATTRIBS_ARB', CardinalSymbol, GL_MAX_PROGRAM_NATIVE_ATTRIBS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_ADDRESS_REGISTERS_ARB', CardinalSymbol, GL_PROGRAM_ADDRESS_REGISTERS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_PROGRAM_ADDRESS_REGISTERS_ARB', CardinalSymbol, GL_MAX_PROGRAM_ADDRESS_REGISTERS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB', CardinalSymbol, GL_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB', CardinalSymbol, GL_MAX_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_PROGRAM_LOCAL_PARAMETERS_ARB', CardinalSymbol, GL_MAX_PROGRAM_LOCAL_PARAMETERS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_PROGRAM_ENV_PARAMETERS_ARB', CardinalSymbol, GL_MAX_PROGRAM_ENV_PARAMETERS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_UNDER_NATIVE_LIMITS_ARB', CardinalSymbol, GL_PROGRAM_UNDER_NATIVE_LIMITS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_STRING_ARB', CardinalSymbol, GL_PROGRAM_STRING_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_ERROR_POSITION_ARB', CardinalSymbol, GL_PROGRAM_ERROR_POSITION_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_MATRIX_ARB', CardinalSymbol, GL_CURRENT_MATRIX_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TRANSPOSE_CURRENT_MATRIX_ARB', CardinalSymbol, GL_TRANSPOSE_CURRENT_MATRIX_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_MATRIX_STACK_DEPTH_ARB', CardinalSymbol, GL_CURRENT_MATRIX_STACK_DEPTH_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_VERTEX_ATTRIBS_ARB', CardinalSymbol, GL_MAX_VERTEX_ATTRIBS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_PROGRAM_MATRICES_ARB', CardinalSymbol, GL_MAX_PROGRAM_MATRICES_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_PROGRAM_MATRIX_STACK_DEPTH_ARB', CardinalSymbol, GL_MAX_PROGRAM_MATRIX_STACK_DEPTH_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_ERROR_STRING_ARB', CardinalSymbol, GL_PROGRAM_ERROR_STRING_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX0_ARB', CardinalSymbol, GL_MATRIX0_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX1_ARB', CardinalSymbol, GL_MATRIX1_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX2_ARB', CardinalSymbol, GL_MATRIX2_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX3_ARB', CardinalSymbol, GL_MATRIX3_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX4_ARB', CardinalSymbol, GL_MATRIX4_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX5_ARB', CardinalSymbol, GL_MATRIX5_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX6_ARB', CardinalSymbol, GL_MATRIX6_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX7_ARB', CardinalSymbol, GL_MATRIX7_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX8_ARB', CardinalSymbol, GL_MATRIX8_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX9_ARB', CardinalSymbol, GL_MATRIX9_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX10_ARB', CardinalSymbol, GL_MATRIX10_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX11_ARB', CardinalSymbol, GL_MATRIX11_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX12_ARB', CardinalSymbol, GL_MATRIX12_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX13_ARB', CardinalSymbol, GL_MATRIX13_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX14_ARB', CardinalSymbol, GL_MATRIX14_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX15_ARB', CardinalSymbol, GL_MATRIX15_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX16_ARB', CardinalSymbol, GL_MATRIX16_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX17_ARB', CardinalSymbol, GL_MATRIX17_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX18_ARB', CardinalSymbol, GL_MATRIX18_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX19_ARB', CardinalSymbol, GL_MATRIX19_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX20_ARB', CardinalSymbol, GL_MATRIX20_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX21_ARB', CardinalSymbol, GL_MATRIX21_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX22_ARB', CardinalSymbol, GL_MATRIX22_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX23_ARB', CardinalSymbol, GL_MATRIX23_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX24_ARB', CardinalSymbol, GL_MATRIX24_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX25_ARB', CardinalSymbol, GL_MATRIX25_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX26_ARB', CardinalSymbol, GL_MATRIX26_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX27_ARB', CardinalSymbol, GL_MATRIX27_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX28_ARB', CardinalSymbol, GL_MATRIX28_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX29_ARB', CardinalSymbol, GL_MATRIX29_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX30_ARB', CardinalSymbol, GL_MATRIX30_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX31_ARB', CardinalSymbol, GL_MATRIX31_ARB));

  // ARB_vertex_buffer_object
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ARRAY_BUFFER_ARB', CardinalSymbol, GL_ARRAY_BUFFER_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ELEMENT_ARRAY_BUFFER_ARB', CardinalSymbol, GL_ELEMENT_ARRAY_BUFFER_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ARRAY_BUFFER_BINDING_ARB', CardinalSymbol, GL_ARRAY_BUFFER_BINDING_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB', CardinalSymbol, GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ARRAY_BUFFER_BINDING_ARB', CardinalSymbol, GL_VERTEX_ARRAY_BUFFER_BINDING_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NORMAL_ARRAY_BUFFER_BINDING_ARB', CardinalSymbol, GL_NORMAL_ARRAY_BUFFER_BINDING_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_ARRAY_BUFFER_BINDING_ARB', CardinalSymbol, GL_COLOR_ARRAY_BUFFER_BINDING_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INDEX_ARRAY_BUFFER_BINDING_ARB', CardinalSymbol, GL_INDEX_ARRAY_BUFFER_BINDING_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB', CardinalSymbol, GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB', CardinalSymbol, GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB', CardinalSymbol, GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB', CardinalSymbol, GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB', CardinalSymbol, GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STREAM_DRAW_ARB', CardinalSymbol, GL_STREAM_DRAW_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STREAM_READ_ARB', CardinalSymbol, GL_STREAM_READ_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STREAM_COPY_ARB', CardinalSymbol, GL_STREAM_COPY_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STATIC_DRAW_ARB', CardinalSymbol, GL_STATIC_DRAW_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STATIC_READ_ARB', CardinalSymbol, GL_STATIC_READ_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_STATIC_COPY_ARB', CardinalSymbol, GL_STATIC_COPY_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DYNAMIC_DRAW_ARB', CardinalSymbol, GL_DYNAMIC_DRAW_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DYNAMIC_READ_ARB', CardinalSymbol, GL_DYNAMIC_READ_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DYNAMIC_COPY_ARB', CardinalSymbol, GL_DYNAMIC_COPY_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_READ_ONLY_ARB', CardinalSymbol, GL_READ_ONLY_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_WRITE_ONLY_ARB', CardinalSymbol, GL_WRITE_ONLY_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_READ_WRITE_ARB', CardinalSymbol, GL_READ_WRITE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BUFFER_SIZE_ARB', CardinalSymbol, GL_BUFFER_SIZE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BUFFER_USAGE_ARB', CardinalSymbol, GL_BUFFER_USAGE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BUFFER_ACCESS_ARB', CardinalSymbol, GL_BUFFER_ACCESS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BUFFER_MAPPED_ARB', CardinalSymbol, GL_BUFFER_MAPPED_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BUFFER_MAP_POINTER_ARB', CardinalSymbol, GL_BUFFER_MAP_POINTER_ARB));

  // ARB_shader_objects
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_OBJECT_ARB', CardinalSymbol, GL_PROGRAM_OBJECT_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OBJECT_TYPE_ARB', CardinalSymbol, GL_OBJECT_TYPE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OBJECT_SUBTYPE_ARB', CardinalSymbol, GL_OBJECT_SUBTYPE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OBJECT_DELETE_STATUS_ARB', CardinalSymbol, GL_OBJECT_DELETE_STATUS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OBJECT_COMPILE_STATUS_ARB', CardinalSymbol, GL_OBJECT_COMPILE_STATUS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OBJECT_LINK_STATUS_ARB', CardinalSymbol, GL_OBJECT_LINK_STATUS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OBJECT_VALIDATE_STATUS_ARB', CardinalSymbol, GL_OBJECT_VALIDATE_STATUS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OBJECT_INFO_LOG_LENGTH_ARB', CardinalSymbol, GL_OBJECT_INFO_LOG_LENGTH_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OBJECT_ATTACHED_OBJECTS_ARB', CardinalSymbol, GL_OBJECT_ATTACHED_OBJECTS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OBJECT_ACTIVE_UNIFORMS_ARB', CardinalSymbol, GL_OBJECT_ACTIVE_UNIFORMS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB', CardinalSymbol, GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OBJECT_SHADER_SOURCE_LENGTH_ARB', CardinalSymbol, GL_OBJECT_SHADER_SOURCE_LENGTH_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SHADER_OBJECT_ARB', CardinalSymbol, GL_SHADER_OBJECT_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FLOAT_VEC2_ARB', CardinalSymbol, GL_FLOAT_VEC2_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FLOAT_VEC3_ARB', CardinalSymbol, GL_FLOAT_VEC3_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FLOAT_VEC4_ARB', CardinalSymbol, GL_FLOAT_VEC4_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INT_VEC2_ARB', CardinalSymbol, GL_INT_VEC2_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INT_VEC3_ARB', CardinalSymbol, GL_INT_VEC3_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INT_VEC4_ARB', CardinalSymbol, GL_INT_VEC4_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BOOL_ARB', CardinalSymbol, GL_BOOL_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BOOL_VEC2_ARB', CardinalSymbol, GL_BOOL_VEC2_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BOOL_VEC3_ARB', CardinalSymbol, GL_BOOL_VEC3_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BOOL_VEC4_ARB', CardinalSymbol, GL_BOOL_VEC4_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FLOAT_MAT2_ARB', CardinalSymbol, GL_FLOAT_MAT2_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FLOAT_MAT3_ARB', CardinalSymbol, GL_FLOAT_MAT3_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FLOAT_MAT4_ARB', CardinalSymbol, GL_FLOAT_MAT4_ARB));

  // ARB_vertex_shader
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_SHADER_ARB', CardinalSymbol, GL_VERTEX_SHADER_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB', CardinalSymbol, GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_VARYING_FLOATS_ARB', CardinalSymbol, GL_MAX_VARYING_FLOATS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB', CardinalSymbol, GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB', CardinalSymbol, GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OBJECT_ACTIVE_ATTRIBUTES_ARB', CardinalSymbol, GL_OBJECT_ACTIVE_ATTRIBUTES_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB', CardinalSymbol, GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB));

  // ARB_fragment_shader
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FRAGMENT_SHADER_ARB', CardinalSymbol, GL_FRAGMENT_SHADER_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB', CardinalSymbol, GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB));

  // ARB_fragment_program
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FRAGMENT_PROGRAM_ARB', CardinalSymbol, GL_FRAGMENT_PROGRAM_ARB));

  // NV_texture_env_combine4
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINE4_NV', CardinalSymbol, GL_COMBINE4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE3_RGB_NV', CardinalSymbol, GL_SOURCE3_RGB_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE3_ALPHA_NV', CardinalSymbol, GL_SOURCE3_ALPHA_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND3_RGB_NV', CardinalSymbol, GL_OPERAND3_RGB_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND3_ALPHA_NV', CardinalSymbol, GL_OPERAND3_ALPHA_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BLEND_EQUATION', CardinalSymbol, GL_BLEND_EQUATION));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TABLE_TOO_LARGE', CardinalSymbol, GL_TABLE_TOO_LARGE));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_BYTE_3_3_2', CardinalSymbol, GL_UNSIGNED_BYTE_3_3_2));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_SHORT_4_4_4_4', CardinalSymbol, GL_UNSIGNED_SHORT_4_4_4_4));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_SHORT_5_5_5_1', CardinalSymbol, GL_UNSIGNED_SHORT_5_5_5_1));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_INT_8_8_8_8', CardinalSymbol, GL_UNSIGNED_INT_8_8_8_8));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_INT_10_10_10_2', CardinalSymbol, GL_UNSIGNED_INT_10_10_10_2));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_BYTE_2_3_3_REV', CardinalSymbol, GL_UNSIGNED_BYTE_2_3_3_REV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_SHORT_5_6_5', CardinalSymbol, GL_UNSIGNED_SHORT_5_6_5));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_SHORT_5_6_5_REV', CardinalSymbol, GL_UNSIGNED_SHORT_5_6_5_REV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_SHORT_4_4_4_4_REV', CardinalSymbol, GL_UNSIGNED_SHORT_4_4_4_4_REV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_SHORT_1_5_5_5_REV', CardinalSymbol, GL_UNSIGNED_SHORT_1_5_5_5_REV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_INT_8_8_8_8_REV', CardinalSymbol, GL_UNSIGNED_INT_8_8_8_8_REV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_INT_2_10_10_10_REV', CardinalSymbol, GL_UNSIGNED_INT_2_10_10_10_REV));

  // GL_ARB_transpose_matrix
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TRANSPOSE_MODELVIEW_MATRIX_ARB', CardinalSymbol, GL_TRANSPOSE_MODELVIEW_MATRIX_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TRANSPOSE_PROJECTION_MATRIX_ARB', CardinalSymbol, GL_TRANSPOSE_PROJECTION_MATRIX_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TRANSPOSE_TEXTURE_MATRIX_ARB', CardinalSymbol, GL_TRANSPOSE_TEXTURE_MATRIX_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TRANSPOSE_COLOR_MATRIX_ARB', CardinalSymbol, GL_TRANSPOSE_COLOR_MATRIX_ARB));

  // GL_ARB_multisample
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MULTISAMPLE_ARB', CardinalSymbol, GL_MULTISAMPLE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_ALPHA_TO_COVERAGE_ARB', CardinalSymbol, GL_SAMPLE_ALPHA_TO_COVERAGE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_ALPHA_TO_ONE_ARB', CardinalSymbol, GL_SAMPLE_ALPHA_TO_ONE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_COVERAGE_ARB', CardinalSymbol, GL_SAMPLE_COVERAGE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_BUFFERS_ARB', CardinalSymbol, GL_SAMPLE_BUFFERS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLES_ARB', CardinalSymbol, GL_SAMPLES_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_COVERAGE_VALUE_ARB', CardinalSymbol, GL_SAMPLE_COVERAGE_VALUE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_COVERAGE_INVERT_ARB', CardinalSymbol, GL_SAMPLE_COVERAGE_INVERT_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MULTISAMPLE_BIT_ARB', CardinalSymbol, GL_MULTISAMPLE_BIT_ARB));

  // GL_ARB_depth_texture
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DEPTH_COMPONENT16_ARB', CardinalSymbol, GL_DEPTH_COMPONENT16_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DEPTH_COMPONENT24_ARB', CardinalSymbol, GL_DEPTH_COMPONENT24_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DEPTH_COMPONENT32_ARB', CardinalSymbol, GL_DEPTH_COMPONENT32_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_DEPTH_SIZE_ARB', CardinalSymbol, GL_TEXTURE_DEPTH_SIZE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DEPTH_TEXTURE_MODE_ARB', CardinalSymbol, GL_DEPTH_TEXTURE_MODE_ARB));

  // GL_ARB_shadow
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_COMPARE_MODE_ARB', CardinalSymbol, GL_TEXTURE_COMPARE_MODE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_COMPARE_FUNC_ARB', CardinalSymbol, GL_TEXTURE_COMPARE_FUNC_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMPARE_R_TO_TEXTURE_ARB', CardinalSymbol, GL_COMPARE_R_TO_TEXTURE_ARB));

  // GL_ARB_texture_cube_map
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NORMAL_MAP_ARB', CardinalSymbol, GL_NORMAL_MAP_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_REFLECTION_MAP_ARB', CardinalSymbol, GL_REFLECTION_MAP_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_CUBE_MAP_ARB', CardinalSymbol, GL_TEXTURE_CUBE_MAP_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_BINDING_CUBE_MAP_ARB', CardinalSymbol, GL_TEXTURE_BINDING_CUBE_MAP_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB', CardinalSymbol, GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB', CardinalSymbol, GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB', CardinalSymbol, GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB', CardinalSymbol, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB', CardinalSymbol, GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB', CardinalSymbol, GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROXY_TEXTURE_CUBE_MAP_ARB', CardinalSymbol, GL_PROXY_TEXTURE_CUBE_MAP_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB', CardinalSymbol, GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB));

  // GL_ARB_texture_border_clamp
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CLAMP_TO_BORDER_ARB', CardinalSymbol, GL_CLAMP_TO_BORDER_ARB));

  // GL_ARB_texture_compression
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMPRESSED_ALPHA_ARB', CardinalSymbol, GL_COMPRESSED_ALPHA_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMPRESSED_LUMINANCE_ARB', CardinalSymbol, GL_COMPRESSED_LUMINANCE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMPRESSED_LUMINANCE_ALPHA_ARB', CardinalSymbol, GL_COMPRESSED_LUMINANCE_ALPHA_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMPRESSED_INTENSITY_ARB', CardinalSymbol, GL_COMPRESSED_INTENSITY_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMPRESSED_RGB_ARB', CardinalSymbol, GL_COMPRESSED_RGB_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMPRESSED_RGBA_ARB', CardinalSymbol, GL_COMPRESSED_RGBA_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_COMPRESSION_HINT_ARB', CardinalSymbol, GL_TEXTURE_COMPRESSION_HINT_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB', CardinalSymbol, GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_COMPRESSED_ARB', CardinalSymbol, GL_TEXTURE_COMPRESSED_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB', CardinalSymbol, GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMPRESSED_TEXTURE_FORMATS_ARB', CardinalSymbol, GL_COMPRESSED_TEXTURE_FORMATS_ARB));

  // GL_ARB_vertex_blend
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_VERTEX_UNITS_ARB', CardinalSymbol, GL_MAX_VERTEX_UNITS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ACTIVE_VERTEX_UNITS_ARB', CardinalSymbol, GL_ACTIVE_VERTEX_UNITS_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_WEIGHT_SUM_UNITY_ARB', CardinalSymbol, GL_WEIGHT_SUM_UNITY_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_BLEND_ARB', CardinalSymbol, GL_VERTEX_BLEND_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_WEIGHT_ARB', CardinalSymbol, GL_CURRENT_WEIGHT_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_WEIGHT_ARRAY_TYPE_ARB', CardinalSymbol, GL_WEIGHT_ARRAY_TYPE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_WEIGHT_ARRAY_STRIDE_ARB', CardinalSymbol, GL_WEIGHT_ARRAY_STRIDE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_WEIGHT_ARRAY_SIZE_ARB', CardinalSymbol, GL_WEIGHT_ARRAY_SIZE_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_WEIGHT_ARRAY_POINTER_ARB', CardinalSymbol, GL_WEIGHT_ARRAY_POINTER_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_WEIGHT_ARRAY_ARB', CardinalSymbol, GL_WEIGHT_ARRAY_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW0_ARB', CardinalSymbol, GL_MODELVIEW0_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW1_ARB', CardinalSymbol, GL_MODELVIEW1_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW2_ARB', CardinalSymbol, GL_MODELVIEW2_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW3_ARB', CardinalSymbol, GL_MODELVIEW3_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW4_ARB', CardinalSymbol, GL_MODELVIEW4_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW5_ARB', CardinalSymbol, GL_MODELVIEW5_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW6_ARB', CardinalSymbol, GL_MODELVIEW6_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW7_ARB', CardinalSymbol, GL_MODELVIEW7_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW8_ARB', CardinalSymbol, GL_MODELVIEW8_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW9_ARB', CardinalSymbol, GL_MODELVIEW9_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW10_ARB', CardinalSymbol, GL_MODELVIEW10_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW11_ARB', CardinalSymbol, GL_MODELVIEW11_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW12_ARB', CardinalSymbol, GL_MODELVIEW12_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW13_ARB', CardinalSymbol, GL_MODELVIEW13_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW14_ARB', CardinalSymbol, GL_MODELVIEW14_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW15_ARB', CardinalSymbol, GL_MODELVIEW15_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW16_ARB', CardinalSymbol, GL_MODELVIEW16_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW17_ARB', CardinalSymbol, GL_MODELVIEW17_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW18_ARB', CardinalSymbol, GL_MODELVIEW18_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW19_ARB', CardinalSymbol, GL_MODELVIEW19_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW20_ARB', CardinalSymbol, GL_MODELVIEW20_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW21_ARB', CardinalSymbol, GL_MODELVIEW21_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW22_ARB', CardinalSymbol, GL_MODELVIEW22_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW23_ARB', CardinalSymbol, GL_MODELVIEW23_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW24_ARB', CardinalSymbol, GL_MODELVIEW24_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW25_ARB', CardinalSymbol, GL_MODELVIEW25_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW26_ARB', CardinalSymbol, GL_MODELVIEW26_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW27_ARB', CardinalSymbol, GL_MODELVIEW27_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW28_ARB', CardinalSymbol, GL_MODELVIEW28_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW29_ARB', CardinalSymbol, GL_MODELVIEW29_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW30_ARB', CardinalSymbol, GL_MODELVIEW30_ARB));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW31_ARB', CardinalSymbol, GL_MODELVIEW31_ARB));

  // GL_SGIS_texture_lod
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_MIN_LOD_SGIS', CardinalSymbol, GL_TEXTURE_MIN_LOD_SGIS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_MAX_LOD_SGIS', CardinalSymbol, GL_TEXTURE_MAX_LOD_SGIS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_BASE_LEVEL_SGIS', CardinalSymbol, GL_TEXTURE_BASE_LEVEL_SGIS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_MAX_LEVEL_SGIS', CardinalSymbol, GL_TEXTURE_MAX_LEVEL_SGIS));

  // GL_SGIS_multisample
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MULTISAMPLE_SGIS', CardinalSymbol, GL_MULTISAMPLE_SGIS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_ALPHA_TO_MASK_SGIS', CardinalSymbol, GL_SAMPLE_ALPHA_TO_MASK_SGIS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_ALPHA_TO_ONE_SGIS', CardinalSymbol, GL_SAMPLE_ALPHA_TO_ONE_SGIS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_MASK_SGIS', CardinalSymbol, GL_SAMPLE_MASK_SGIS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_1PASS_SGIS', CardinalSymbol, GL_1PASS_SGIS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_2PASS_0_SGIS', CardinalSymbol, GL_2PASS_0_SGIS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_2PASS_1_SGIS', CardinalSymbol, GL_2PASS_1_SGIS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_4PASS_0_SGIS', CardinalSymbol, GL_4PASS_0_SGIS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_4PASS_1_SGIS', CardinalSymbol, GL_4PASS_1_SGIS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_4PASS_2_SGIS', CardinalSymbol, GL_4PASS_2_SGIS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_4PASS_3_SGIS', CardinalSymbol, GL_4PASS_3_SGIS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_BUFFERS_SGIS', CardinalSymbol, GL_SAMPLE_BUFFERS_SGIS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLES_SGIS', CardinalSymbol, GL_SAMPLES_SGIS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_MASK_VALUE_SGIS', CardinalSymbol, GL_SAMPLE_MASK_VALUE_SGIS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_MASK_INVERT_SGIS', CardinalSymbol, GL_SAMPLE_MASK_INVERT_SGIS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_PATTERN_SGIS', CardinalSymbol, GL_SAMPLE_PATTERN_SGIS));

  // GL_SGIS_generate_mipmap
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_GENERATE_MIPMAP_SGIS', CardinalSymbol, GL_GENERATE_MIPMAP_SGIS));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_GENERATE_MIPMAP_HINT_SGIS', CardinalSymbol, GL_GENERATE_MIPMAP_HINT_SGIS));

  // GL_SGIX_shadow
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_COMPARE_SGIX', CardinalSymbol, GL_TEXTURE_COMPARE_SGIX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_COMPARE_OPERATOR_SGIX', CardinalSymbol, GL_TEXTURE_COMPARE_OPERATOR_SGIX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_LEQUAL_R_SGIX', CardinalSymbol, GL_TEXTURE_LEQUAL_R_SGIX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_GEQUAL_R_SGIX', CardinalSymbol, GL_TEXTURE_GEQUAL_R_SGIX));

  // GL_SGIS_texture_edge_clamp
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CLAMP_TO_EDGE_SGIS', CardinalSymbol, GL_CLAMP_TO_EDGE_SGIS));

  // GL_SGIS_texture_border_clamp
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CLAMP_TO_BORDER_SGIS', CardinalSymbol, GL_CLAMP_TO_BORDER_SGIS));

  // GL_EXT_paletted_texture
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_INDEX_SIZE_EXT', CardinalSymbol, GL_TEXTURE_INDEX_SIZE_EXT));

  // GL_SGIX_shadow_ambient
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SHADOW_AMBIENT_SGIX', CardinalSymbol, GL_SHADOW_AMBIENT_SGIX));

  // GL_IBM_rasterpos_clip
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_RASTER_POSITION_UNCLIPPED_IBM', CardinalSymbol, GL_RASTER_POSITION_UNCLIPPED_IBM));

  // GL_EXT_draw_range_elements
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_ELEMENTS_VERTICES_EXT', CardinalSymbol, GL_MAX_ELEMENTS_VERTICES_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_ELEMENTS_INDICES_EXT', CardinalSymbol, GL_MAX_ELEMENTS_INDICES_EXT));

  // GL_HP_occlusion_test
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OCCLUSION_TEST_HP', CardinalSymbol, GL_OCCLUSION_TEST_HP));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OCCLUSION_TEST_RESULT_HP', CardinalSymbol, GL_OCCLUSION_TEST_RESULT_HP));

  // GL_EXT_separate_specular_color
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_LIGHT_MODEL_COLOR_CONTROL_EXT', CardinalSymbol, GL_LIGHT_MODEL_COLOR_CONTROL_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SINGLE_COLOR_EXT', CardinalSymbol, GL_SINGLE_COLOR_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SEPARATE_SPECULAR_COLOR_EXT', CardinalSymbol, GL_SEPARATE_SPECULAR_COLOR_EXT));

  // GL_EXT_secondary_color
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_SUM_EXT', CardinalSymbol, GL_COLOR_SUM_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_SECONDARY_COLOR_EXT', CardinalSymbol, GL_CURRENT_SECONDARY_COLOR_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SECONDARY_COLOR_ARRAY_SIZE_EXT', CardinalSymbol, GL_SECONDARY_COLOR_ARRAY_SIZE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SECONDARY_COLOR_ARRAY_TYPE_EXT', CardinalSymbol, GL_SECONDARY_COLOR_ARRAY_TYPE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT', CardinalSymbol, GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SECONDARY_COLOR_ARRAY_POINTER_EXT', CardinalSymbol, GL_SECONDARY_COLOR_ARRAY_POINTER_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SECONDARY_COLOR_ARRAY_EXT', CardinalSymbol, GL_SECONDARY_COLOR_ARRAY_EXT));

  // GL_EXT_fog_coord
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FOG_COORDINATE_SOURCE_EXT', CardinalSymbol, GL_FOG_COORDINATE_SOURCE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FOG_COORDINATE_EXT', CardinalSymbol, GL_FOG_COORDINATE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FRAGMENT_DEPTH_EXT', CardinalSymbol, GL_FRAGMENT_DEPTH_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_FOG_COORDINATE_EXT', CardinalSymbol, GL_CURRENT_FOG_COORDINATE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FOG_COORDINATE_ARRAY_TYPE_EXT', CardinalSymbol, GL_FOG_COORDINATE_ARRAY_TYPE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FOG_COORDINATE_ARRAY_STRIDE_EXT', CardinalSymbol, GL_FOG_COORDINATE_ARRAY_STRIDE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FOG_COORDINATE_ARRAY_POINTER_EXT', CardinalSymbol, GL_FOG_COORDINATE_ARRAY_POINTER_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FOG_COORDINATE_ARRAY_EXT', CardinalSymbol, GL_FOG_COORDINATE_ARRAY_EXT));

  // GL_EXT_texture_env_combine
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE3_RGB_EXT', CardinalSymbol, GL_SOURCE3_RGB_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE4_RGB_EXT', CardinalSymbol, GL_SOURCE4_RGB_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE5_RGB_EXT', CardinalSymbol, GL_SOURCE5_RGB_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE6_RGB_EXT', CardinalSymbol, GL_SOURCE6_RGB_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE7_RGB_EXT', CardinalSymbol, GL_SOURCE7_RGB_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE3_ALPHA_EXT', CardinalSymbol, GL_SOURCE3_ALPHA_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE4_ALPHA_EXT', CardinalSymbol, GL_SOURCE4_ALPHA_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE5_ALPHA_EXT', CardinalSymbol, GL_SOURCE5_ALPHA_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE6_ALPHA_EXT', CardinalSymbol, GL_SOURCE6_ALPHA_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SOURCE7_ALPHA_EXT', CardinalSymbol, GL_SOURCE7_ALPHA_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND3_RGB_EXT', CardinalSymbol, GL_OPERAND3_RGB_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND4_RGB_EXT', CardinalSymbol, GL_OPERAND4_RGB_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND5_RGB_EXT', CardinalSymbol, GL_OPERAND5_RGB_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND6_RGB_EXT', CardinalSymbol, GL_OPERAND6_RGB_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND7_RGB_EXT', CardinalSymbol, GL_OPERAND7_RGB_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND3_ALPHA_EXT', CardinalSymbol, GL_OPERAND3_ALPHA_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND4_ALPHA_EXT', CardinalSymbol, GL_OPERAND4_ALPHA_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND5_ALPHA_EXT', CardinalSymbol, GL_OPERAND5_ALPHA_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND6_ALPHA_EXT', CardinalSymbol, GL_OPERAND6_ALPHA_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_OPERAND7_ALPHA_EXT', CardinalSymbol, GL_OPERAND7_ALPHA_EXT));

  // GL_EXT_blend_func_separate
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BLEND_DST_RGB_EXT', CardinalSymbol, GL_BLEND_DST_RGB_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BLEND_SRC_RGB_EXT', CardinalSymbol, GL_BLEND_SRC_RGB_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BLEND_DST_ALPHA_EXT', CardinalSymbol, GL_BLEND_DST_ALPHA_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BLEND_SRC_ALPHA_EXT', CardinalSymbol, GL_BLEND_SRC_ALPHA_EXT));

  // GL_EXT_texture_cube_map
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NORMAL_MAP_EXT', CardinalSymbol, GL_NORMAL_MAP_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_REFLECTION_MAP_EXT', CardinalSymbol, GL_REFLECTION_MAP_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_CUBE_MAP_EXT', CardinalSymbol, GL_TEXTURE_CUBE_MAP_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_BINDING_CUBE_MAP_EXT', CardinalSymbol, GL_TEXTURE_BINDING_CUBE_MAP_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT', CardinalSymbol, GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT', CardinalSymbol, GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT', CardinalSymbol, GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT', CardinalSymbol, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT', CardinalSymbol, GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT', CardinalSymbol, GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROXY_TEXTURE_CUBE_MAP_EXT', CardinalSymbol, GL_PROXY_TEXTURE_CUBE_MAP_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT', CardinalSymbol, GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT));

  // GL_EXT_texture_lod_bias
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_TEXTURE_LOD_BIAS_EXT', CardinalSymbol, GL_MAX_TEXTURE_LOD_BIAS_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_FILTER_CONTROL_EXT', CardinalSymbol, GL_TEXTURE_FILTER_CONTROL_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_LOD_BIAS_EXT', CardinalSymbol, GL_TEXTURE_LOD_BIAS_EXT));

  // GL_EXT_texture_filter_anisotropic
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_MAX_ANISOTROPY_EXT', CardinalSymbol, GL_TEXTURE_MAX_ANISOTROPY_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT', CardinalSymbol, GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT));

  // GL_NV_light_max_exponent
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_SHININESS_NV', CardinalSymbol, GL_MAX_SHININESS_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_SPOT_EXPONENT_NV', CardinalSymbol, GL_MAX_SPOT_EXPONENT_NV));

  // GL_NV_vertex_array_range
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ARRAY_RANGE_NV', CardinalSymbol, GL_VERTEX_ARRAY_RANGE_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ARRAY_RANGE_LENGTH_NV', CardinalSymbol, GL_VERTEX_ARRAY_RANGE_LENGTH_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ARRAY_RANGE_VALID_NV', CardinalSymbol, GL_VERTEX_ARRAY_RANGE_VALID_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV', CardinalSymbol, GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ARRAY_RANGE_POINTER_NV', CardinalSymbol, GL_VERTEX_ARRAY_RANGE_POINTER_NV));

  // GL_NV_vertex_array_range2
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ARRAY_RANGE_WITHOUT_FLUSH_NV', CardinalSymbol, GL_VERTEX_ARRAY_RANGE_WITHOUT_FLUSH_NV));

  // GL_NV_register_combiners
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_REGISTER_COMBINERS_NV', CardinalSymbol, GL_REGISTER_COMBINERS_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VARIABLE_A_NV', CardinalSymbol, GL_VARIABLE_A_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VARIABLE_B_NV', CardinalSymbol, GL_VARIABLE_B_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VARIABLE_C_NV', CardinalSymbol, GL_VARIABLE_C_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VARIABLE_D_NV', CardinalSymbol, GL_VARIABLE_D_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VARIABLE_E_NV', CardinalSymbol, GL_VARIABLE_E_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VARIABLE_F_NV', CardinalSymbol, GL_VARIABLE_F_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VARIABLE_G_NV', CardinalSymbol, GL_VARIABLE_G_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONSTANT_COLOR0_NV', CardinalSymbol, GL_CONSTANT_COLOR0_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CONSTANT_COLOR1_NV', CardinalSymbol, GL_CONSTANT_COLOR1_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PRIMARY_COLOR_NV', CardinalSymbol, GL_PRIMARY_COLOR_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SECONDARY_COLOR_NV', CardinalSymbol, GL_SECONDARY_COLOR_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SPARE0_NV', CardinalSymbol, GL_SPARE0_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SPARE1_NV', CardinalSymbol, GL_SPARE1_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_DISCARD_NV', CardinalSymbol, GL_DISCARD_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_E_TIMES_F_NV', CardinalSymbol, GL_E_TIMES_F_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SPARE0_PLUS_SECONDARY_COLOR_NV', CardinalSymbol, GL_SPARE0_PLUS_SECONDARY_COLOR_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_IDENTITY_NV', CardinalSymbol, GL_UNSIGNED_IDENTITY_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_UNSIGNED_INVERT_NV', CardinalSymbol, GL_UNSIGNED_INVERT_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_EXPAND_NORMAL_NV', CardinalSymbol, GL_EXPAND_NORMAL_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_EXPAND_NEGATE_NV', CardinalSymbol, GL_EXPAND_NEGATE_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HALF_BIAS_NORMAL_NV', CardinalSymbol, GL_HALF_BIAS_NORMAL_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_HALF_BIAS_NEGATE_NV', CardinalSymbol, GL_HALF_BIAS_NEGATE_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SIGNED_IDENTITY_NV', CardinalSymbol, GL_SIGNED_IDENTITY_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SIGNED_NEGATE_NV', CardinalSymbol, GL_SIGNED_NEGATE_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SCALE_BY_TWO_NV', CardinalSymbol, GL_SCALE_BY_TWO_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SCALE_BY_FOUR_NV', CardinalSymbol, GL_SCALE_BY_FOUR_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SCALE_BY_ONE_HALF_NV', CardinalSymbol, GL_SCALE_BY_ONE_HALF_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_BIAS_BY_NEGATIVE_ONE_HALF_NV', CardinalSymbol, GL_BIAS_BY_NEGATIVE_ONE_HALF_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINER_INPUT_NV', CardinalSymbol, GL_COMBINER_INPUT_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINER_MAPPING_NV', CardinalSymbol, GL_COMBINER_MAPPING_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINER_COMPONENT_USAGE_NV', CardinalSymbol, GL_COMBINER_COMPONENT_USAGE_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINER_AB_DOT_PRODUCT_NV', CardinalSymbol, GL_COMBINER_AB_DOT_PRODUCT_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINER_CD_DOT_PRODUCT_NV', CardinalSymbol, GL_COMBINER_CD_DOT_PRODUCT_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINER_MUX_SUM_NV', CardinalSymbol, GL_COMBINER_MUX_SUM_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINER_SCALE_NV', CardinalSymbol, GL_COMBINER_SCALE_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINER_BIAS_NV', CardinalSymbol, GL_COMBINER_BIAS_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINER_AB_OUTPUT_NV', CardinalSymbol, GL_COMBINER_AB_OUTPUT_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINER_CD_OUTPUT_NV', CardinalSymbol, GL_COMBINER_CD_OUTPUT_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINER_SUM_OUTPUT_NV', CardinalSymbol, GL_COMBINER_SUM_OUTPUT_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_GENERAL_COMBINERS_NV', CardinalSymbol, GL_MAX_GENERAL_COMBINERS_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_NUM_GENERAL_COMBINERS_NV', CardinalSymbol, GL_NUM_GENERAL_COMBINERS_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COLOR_SUM_CLAMP_NV', CardinalSymbol, GL_COLOR_SUM_CLAMP_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINER0_NV', CardinalSymbol, GL_COMBINER0_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINER1_NV', CardinalSymbol, GL_COMBINER1_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINER2_NV', CardinalSymbol, GL_COMBINER2_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINER3_NV', CardinalSymbol, GL_COMBINER3_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINER4_NV', CardinalSymbol, GL_COMBINER4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINER5_NV', CardinalSymbol, GL_COMBINER5_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINER6_NV', CardinalSymbol, GL_COMBINER6_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMBINER7_NV', CardinalSymbol, GL_COMBINER7_NV));

  // GL_NV_fog_distance
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_FOG_DISTANCE_MODE_NV', CardinalSymbol, GL_FOG_DISTANCE_MODE_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_EYE_RADIAL_NV', CardinalSymbol, GL_EYE_RADIAL_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_EYE_PLANE_ABSOLUTE_NV', CardinalSymbol, GL_EYE_PLANE_ABSOLUTE_NV));

  // GL_EXT_texture_compression_s3tc
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMPRESSED_RGB_S3TC_DXT1_EXT', CardinalSymbol, GL_COMPRESSED_RGB_S3TC_DXT1_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMPRESSED_RGBA_S3TC_DXT1_EXT', CardinalSymbol, GL_COMPRESSED_RGBA_S3TC_DXT1_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMPRESSED_RGBA_S3TC_DXT3_EXT', CardinalSymbol, GL_COMPRESSED_RGBA_S3TC_DXT3_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMPRESSED_RGBA_S3TC_DXT5_EXT', CardinalSymbol, GL_COMPRESSED_RGBA_S3TC_DXT5_EXT));

  // GL_3DFX_texture_compression_FXT1
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMPRESSED_RGB_FXT1_3DFX', CardinalSymbol, GL_COMPRESSED_RGB_FXT1_3DFX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_COMPRESSED_RGBA_FXT1_3DFX', CardinalSymbol, GL_COMPRESSED_RGBA_FXT1_3DFX));

  // GL_3DFX_multisample
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MULTISAMPLE_3DFX', CardinalSymbol, GL_MULTISAMPLE_3DFX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_BUFFERS_3DFX', CardinalSymbol, GL_SAMPLE_BUFFERS_3DFX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLES_3DFX', CardinalSymbol, GL_SAMPLES_3DFX));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MULTISAMPLE_BIT_3DFX', CardinalSymbol, GL_MULTISAMPLE_BIT_3DFX));

  // GL_EXT_multisample
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MULTISAMPLE_EXT', CardinalSymbol, GL_MULTISAMPLE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_ALPHA_TO_MASK_EXT', CardinalSymbol, GL_SAMPLE_ALPHA_TO_MASK_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_ALPHA_TO_ONE_EXT', CardinalSymbol, GL_SAMPLE_ALPHA_TO_ONE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_MASK_EXT', CardinalSymbol, GL_SAMPLE_MASK_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_1PASS_EXT', CardinalSymbol, GL_1PASS_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_2PASS_0_EXT', CardinalSymbol, GL_2PASS_0_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_2PASS_1_EXT', CardinalSymbol, GL_2PASS_1_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_4PASS_0_EXT', CardinalSymbol, GL_4PASS_0_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_4PASS_1_EXT', CardinalSymbol, GL_4PASS_1_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_4PASS_2_EXT', CardinalSymbol, GL_4PASS_2_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_4PASS_3_EXT', CardinalSymbol, GL_4PASS_3_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_BUFFERS_EXT', CardinalSymbol, GL_SAMPLE_BUFFERS_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLES_EXT', CardinalSymbol, GL_SAMPLES_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_MASK_VALUE_EXT', CardinalSymbol, GL_SAMPLE_MASK_VALUE_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_MASK_INVERT_EXT', CardinalSymbol, GL_SAMPLE_MASK_INVERT_EXT));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_SAMPLE_PATTERN_EXT', CardinalSymbol, GL_SAMPLE_PATTERN_EXT));

  // GL_EXT_clip_volume_hint
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CLIP_VOLUME_CLIPPING_HINT_EXT', CardinalSymbol, GL_CLIP_VOLUME_CLIPPING_HINT_EXT));

  // GL_SGIS_texture_color_mask
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TEXTURE_COLOR_WRITEMASK_SGIS', CardinalSymbol, GL_TEXTURE_COLOR_WRITEMASK_SGIS));

  // GL_NV_vertex_program
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_PROGRAM_NV', CardinalSymbol, GL_VERTEX_PROGRAM_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_STATE_PROGRAM_NV', CardinalSymbol, GL_VERTEX_STATE_PROGRAM_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ATTRIB_ARRAY_SIZE_NV', CardinalSymbol, GL_ATTRIB_ARRAY_SIZE_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ATTRIB_ARRAY_STRIDE_NV', CardinalSymbol, GL_ATTRIB_ARRAY_STRIDE_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ATTRIB_ARRAY_TYPE_NV', CardinalSymbol, GL_ATTRIB_ARRAY_TYPE_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_ATTRIB_NV', CardinalSymbol, GL_CURRENT_ATTRIB_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_LENGTH_NV', CardinalSymbol, GL_PROGRAM_LENGTH_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_STRING_NV', CardinalSymbol, GL_PROGRAM_STRING_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MODELVIEW_PROJECTION_NV', CardinalSymbol, GL_MODELVIEW_PROJECTION_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_IDENTITY_NV', CardinalSymbol, GL_IDENTITY_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INVERSE_NV', CardinalSymbol, GL_INVERSE_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TRANSPOSE_NV', CardinalSymbol, GL_TRANSPOSE_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_INVERSE_TRANSPOSE_NV', CardinalSymbol, GL_INVERSE_TRANSPOSE_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV', CardinalSymbol, GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAX_TRACK_MATRICES_NV', CardinalSymbol, GL_MAX_TRACK_MATRICES_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX0_NV', CardinalSymbol, GL_MATRIX0_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX1_NV', CardinalSymbol, GL_MATRIX1_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX2_NV', CardinalSymbol, GL_MATRIX2_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX3_NV', CardinalSymbol, GL_MATRIX3_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX4_NV', CardinalSymbol, GL_MATRIX4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX5_NV', CardinalSymbol, GL_MATRIX5_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX6_NV', CardinalSymbol, GL_MATRIX6_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MATRIX7_NV', CardinalSymbol, GL_MATRIX7_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_MATRIX_STACK_DEPTH_NV', CardinalSymbol, GL_CURRENT_MATRIX_STACK_DEPTH_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_CURRENT_MATRIX_NV', CardinalSymbol, GL_CURRENT_MATRIX_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_PROGRAM_POINT_SIZE_NV', CardinalSymbol, GL_VERTEX_PROGRAM_POINT_SIZE_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_PROGRAM_TWO_SIDE_NV', CardinalSymbol, GL_VERTEX_PROGRAM_TWO_SIDE_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_PARAMETER_NV', CardinalSymbol, GL_PROGRAM_PARAMETER_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_ATTRIB_ARRAY_POINTER_NV', CardinalSymbol, GL_ATTRIB_ARRAY_POINTER_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_TARGET_NV', CardinalSymbol, GL_PROGRAM_TARGET_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_RESIDENT_NV', CardinalSymbol, GL_PROGRAM_RESIDENT_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TRACK_MATRIX_NV', CardinalSymbol, GL_TRACK_MATRIX_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_TRACK_MATRIX_TRANSFORM_NV', CardinalSymbol, GL_TRACK_MATRIX_TRANSFORM_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_PROGRAM_BINDING_NV', CardinalSymbol, GL_VERTEX_PROGRAM_BINDING_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_PROGRAM_ERROR_POSITION_NV', CardinalSymbol, GL_PROGRAM_ERROR_POSITION_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY0_NV', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY0_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY1_NV', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY1_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY2_NV', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY2_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY3_NV', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY3_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY4_NV', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY5_NV', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY5_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY6_NV', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY6_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY7_NV', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY7_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY8_NV', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY8_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY9_NV', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY9_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY10_NV', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY10_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY11_NV', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY11_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY12_NV', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY12_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY13_NV', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY13_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY14_NV', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY14_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_VERTEX_ATTRIB_ARRAY15_NV', CardinalSymbol, GL_VERTEX_ATTRIB_ARRAY15_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_VERTEX_ATTRIB0_4_NV', CardinalSymbol, GL_MAP1_VERTEX_ATTRIB0_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_VERTEX_ATTRIB1_4_NV', CardinalSymbol, GL_MAP1_VERTEX_ATTRIB1_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_VERTEX_ATTRIB2_4_NV', CardinalSymbol, GL_MAP1_VERTEX_ATTRIB2_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_VERTEX_ATTRIB3_4_NV', CardinalSymbol, GL_MAP1_VERTEX_ATTRIB3_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_VERTEX_ATTRIB4_4_NV', CardinalSymbol, GL_MAP1_VERTEX_ATTRIB4_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_VERTEX_ATTRIB5_4_NV', CardinalSymbol, GL_MAP1_VERTEX_ATTRIB5_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_VERTEX_ATTRIB6_4_NV', CardinalSymbol, GL_MAP1_VERTEX_ATTRIB6_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_VERTEX_ATTRIB7_4_NV', CardinalSymbol, GL_MAP1_VERTEX_ATTRIB7_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_VERTEX_ATTRIB8_4_NV', CardinalSymbol, GL_MAP1_VERTEX_ATTRIB8_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_VERTEX_ATTRIB9_4_NV', CardinalSymbol, GL_MAP1_VERTEX_ATTRIB9_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_VERTEX_ATTRIB10_4_NV', CardinalSymbol, GL_MAP1_VERTEX_ATTRIB10_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_VERTEX_ATTRIB11_4_NV', CardinalSymbol, GL_MAP1_VERTEX_ATTRIB11_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_VERTEX_ATTRIB12_4_NV', CardinalSymbol, GL_MAP1_VERTEX_ATTRIB12_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_VERTEX_ATTRIB13_4_NV', CardinalSymbol, GL_MAP1_VERTEX_ATTRIB13_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_VERTEX_ATTRIB14_4_NV', CardinalSymbol, GL_MAP1_VERTEX_ATTRIB14_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP1_VERTEX_ATTRIB15_4_NV', CardinalSymbol, GL_MAP1_VERTEX_ATTRIB15_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_VERTEX_ATTRIB0_4_NV', CardinalSymbol, GL_MAP2_VERTEX_ATTRIB0_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_VERTEX_ATTRIB1_4_NV', CardinalSymbol, GL_MAP2_VERTEX_ATTRIB1_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_VERTEX_ATTRIB2_4_NV', CardinalSymbol, GL_MAP2_VERTEX_ATTRIB2_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_VERTEX_ATTRIB3_4_NV', CardinalSymbol, GL_MAP2_VERTEX_ATTRIB3_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_VERTEX_ATTRIB4_4_NV', CardinalSymbol, GL_MAP2_VERTEX_ATTRIB4_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_VERTEX_ATTRIB5_4_NV', CardinalSymbol, GL_MAP2_VERTEX_ATTRIB5_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_VERTEX_ATTRIB6_4_NV', CardinalSymbol, GL_MAP2_VERTEX_ATTRIB6_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_VERTEX_ATTRIB7_4_NV', CardinalSymbol, GL_MAP2_VERTEX_ATTRIB7_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_VERTEX_ATTRIB8_4_NV', CardinalSymbol, GL_MAP2_VERTEX_ATTRIB8_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_VERTEX_ATTRIB9_4_NV', CardinalSymbol, GL_MAP2_VERTEX_ATTRIB9_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_VERTEX_ATTRIB10_4_NV', CardinalSymbol, GL_MAP2_VERTEX_ATTRIB10_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_VERTEX_ATTRIB11_4_NV', CardinalSymbol, GL_MAP2_VERTEX_ATTRIB11_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_VERTEX_ATTRIB12_4_NV', CardinalSymbol, GL_MAP2_VERTEX_ATTRIB12_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_VERTEX_ATTRIB13_4_NV', CardinalSymbol, GL_MAP2_VERTEX_ATTRIB13_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_VERTEX_ATTRIB14_4_NV', CardinalSymbol, GL_MAP2_VERTEX_ATTRIB14_4_NV));
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MAP2_VERTEX_ATTRIB15_4_NV', CardinalSymbol, GL_MAP2_VERTEX_ATTRIB15_4_NV));

  // NV_multisample_filter_hint
  SymbolTable.AddSymbol(TConstSymbol.Create('GL_MULTISAMPLE_FILTER_HINT_NV', CardinalSymbol, GL_MULTISAMPLE_FILTER_HINT_NV));

end;

constructor TdwsOpenGLUnit.Create(AOwner: TComponent);
begin
  inherited;
  FUnitName := 'OpenGLx';
  FDependencies.Add('GLS.Context');
  FDependencies.Add('GLS.VectorGeometry');
end;

procedure TGLPushAttrib.Execute;
var
  mask: Cardinal;
begin
  mask := Info['mask'];
  glPushAttrib(mask);
end;

procedure TGLPopAttrib.Execute;
begin
  glPopAttrib;
end;

procedure TGLPushClientAttrib.Execute;
var
  mask: Cardinal;
begin
  mask := Info['mask'];
  glPushClientAttrib(mask);
end;

procedure TGLPopClientAttrib.Execute;
begin
  glPopClientAttrib;
end;

procedure TGLEnable.Execute;
var
  cap: Cardinal;
begin
  cap := Info['cap'];
  glEnable(cap);
end;

procedure TGLDisable.Execute;
var
  cap: Cardinal;
begin
  cap := Info['cap'];
  glDisable(cap);
end;

procedure TGLEnableClientState.Execute;
var
  aarray: Cardinal;
begin
  aarray := Info['aarray'];
  glEnableClientState(aarray);
end;

procedure TGLDisableClientState.Execute;
var
  aarray: Cardinal;
begin
  aarray := Info['aarray'];
  glDisableClientState(aarray);
end;

procedure TGLMatrixMode.Execute;
var
  mode: Cardinal;
begin
  mode := Info['mode'];
  glMatrixMode(mode);
end;

procedure TGLPushMatrix.Execute;
begin
  glPushMatrix;
end;

procedure TGLPopMatrix.Execute;
begin
  glPopMatrix;
end;

procedure TGLLoadIdentity.Execute;
begin
  glLoadIdentity;
end;

procedure TGLLoadMatrixf.Execute;
var
  m: TGLMatrix;
begin
  m := GetMatrixFromInfo(Info.Vars['m']);
  glLoadMatrixf(@m[0]);
end;

procedure TGLTranslatef.Execute;
var
  x, y, z: Single;
begin
  x := Info['x'];
  y := Info['y'];
  z := Info['z'];
  glTranslatef(x, y, z);
end;

procedure TGLRotatef.Execute;
var
  angle, x, y, z: Single;
begin
  angle := Info['angle'];
  x := Info['x'];
  y := Info['y'];
  z := Info['z'];
  glRotatef(angle, x, y, z);
end;

procedure TGLScalef.Execute;
var
  x, y, z: Single;
begin
  x := Info['x'];
  y := Info['y'];
  z := Info['z'];
  glScalef(x, y, z);
end;

procedure TGLShadeModel.Execute;
var
  mode: Cardinal;
begin
  mode := Info['mode'];
  glShadeModel(mode);
end;

procedure TGLCullFace.Execute;
var
  mode: Cardinal;
begin
  mode := Info['mode'];
  glCullFace(mode);
end;

procedure TGLFrontFace.Execute;
var
  mode: Cardinal;
begin
  mode := Info['mode'];
  glFrontFace(mode);
end;

procedure TGLPolygonMode.Execute;
var
  face, mode: Cardinal;
begin
  face := Info['face'];
  mode := Info['mode'];
  glPolygonMode(face, mode);
end;

procedure TGLBegin.Execute;
var
  mode: Cardinal;
begin
  mode := Info['mode'];
  glBegin(mode);
end;

procedure TGLEnd.Execute;
begin
  glEnd;
end;

procedure TGLColor3f.Execute;
var
  red, green, blue: Single;
begin
  red := Info['red'];
  green := Info['green'];
  blue := Info['blue'];
  glColor3f(red, green, blue);
end;

procedure TGLColor4f.Execute;
var
  red, green, blue, alpha: Single;
begin
  red := Info['red'];
  green := Info['green'];
  blue := Info['blue'];
  alpha := Info['alpha'];
  glColor4f(red, green, blue, alpha);
end;

procedure TGLNormal3f.Execute;
var
  x, y, z: Single;
begin
  x := Info['x'];
  y := Info['y'];
  z := Info['z'];
  glNormal3f(x, y, z);
end;

procedure TGLVertex3f.Execute;
var
  x, y, z: Single;
begin
  x := Info['x'];
  y := Info['y'];
  z := Info['z'];
  glVertex3f(x, y, z);
end;

procedure TGLTexCoord1f.Execute;
var
  s: Single;
begin
  s := Info['s'];
  glTexCoord1f(s);
end;

procedure TGLTexCoord2f.Execute;
var
  s, t: Single;
begin
  s := Info['s'];
  t := Info['t'];
  glTexCoord2f(s, t);
end;

procedure TGLTexCoord3f.Execute;
var
  s, t, r: Single;
begin
  s := Info['s'];
  t := Info['t'];
  r := Info['r'];
  glTexCoord3f(s, t, r);
end;

procedure TGLTexCoord4f.Execute;
var
  s, t, r, q: Single;
begin
  s := Info['s'];
  t := Info['t'];
  r := Info['r'];
  q := Info['q'];
  glTexCoord4f(s, t, r, q);
end;

procedure TGLLineWidth.Execute;
var
  width: Single;
begin
  width := Info['width'];
  glLineWidth(width);
end;

procedure TGLMultiTexCoord1f.Execute;
var
  target: Cardinal;
  s: Single;
begin
  target := Info['target'];
  s := Info['s'];
  glMultiTexCoord1f(target, s);
end;

procedure TGLMultiTexCoord2f.Execute;
var
  target: Cardinal;
  s, t: Single;
begin
  target := Info['target'];
  s := Info['s'];
  t := Info['t'];
  glMultiTexCoord2f(target, s, t);
end;

procedure TGLMultiTexCoord3f.Execute;
var
  target: Cardinal;
  s, t, r: Single;
begin
  target := Info['target'];
  s := Info['s'];
  t := Info['t'];
  r := Info['r'];
  glMultiTexCoord3f(target, s, t, r);
end;

procedure TGLMultiTexCoord4f.Execute;
var
  target: Cardinal;
  s, t, r, q: Single;
begin
  target := Info['target'];
  s := Info['s'];
  t := Info['t'];
  r := Info['r'];
  q := Info['q'];
  glMultiTexCoord4f(target, s, t, r, q);
end;

procedure TGLActiveTexture.Execute;
var
  target: Cardinal;
begin
  target := Info['target'];
  glActiveTexture(target);
end;

procedure TGLClientActiveTexture.Execute;
var
  target: Cardinal;
begin
  target := Info['target'];
  glClientActiveTexture(target);
end;

procedure TGLTexEnvf.Execute;
var
  target, pname: Cardinal;
  param: Single;
begin
  target := Info['target'];
  pname := Info['pname'];
  param := Info['param'];
  glTexEnvf(target, pname, param);
end;

procedure TGLTexEnvi.Execute;
var
  target, pname: Cardinal;
  param: Integer;
begin
  target := Info['target'];
  pname := Info['pname'];
  param := Info['param'];
  glTexEnvi(target, pname, param);
end;

procedure TGLBlendFunc.Execute;
var
  sfactor, dfactor: Cardinal;
begin
  sfactor := Info['sfactor'];
  dfactor := Info['dfactor'];
  glBlendFunc(sfactor, dfactor);
end;

procedure TGLDepthFunc.Execute;
var
  func: Cardinal;
begin
  func := Info['func'];
  glDepthFunc(func);
end;

procedure TGLDepthMask.Execute;
var
  flag: Byte;
begin
  flag := Info['flag'];
  glDepthMask(BYTEBOOL(flag));
end;

procedure TGLDepthRange.Execute;
var
  znear, zfar: Double;
begin
  znear := Info['znear'];
  zfar := Info['zfar'];
  glDepthRange(znear, zfar);
end;

procedure TGLStencilFunc.Execute;
var
  func, mask: Cardinal;
  ref: Integer;
begin
  func := Info['func'];
  ref := Info['ref'];
  mask := Info['mask'];
  glStencilFunc(func, ref, mask);
end;

procedure TGLStencilMask.Execute;
var
  mask: Cardinal;
begin
  mask := Info['mask'];
  glStencilMask(mask);
end;

procedure TGLStencilOp.Execute;
var
  fail, zfail, zpass: Cardinal;
begin
  fail := Info['fail'];
  zfail := Info['zfail'];
  zpass := Info['zpass'];
  glStencilOp(fail, zfail, zpass);
end;

procedure TGLLogicOp.Execute;
var
  opcode: Cardinal;
begin
  opcode := Info['opcode'];
  glLogicOp(opcode);
end;


end.
