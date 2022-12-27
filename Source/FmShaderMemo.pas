//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit FmShaderMemo;
(*
  Shader code editor.
// TODO: need to decide how to load templates from external file
//       and update it without package recompilation

*)
interface

{$I GLScene.inc}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Win.Registry,
  VCL.Controls,
  VCL.Forms,
  VCL.ComCtrls,
  VCL.ImgList,
  VCL.Dialogs,
  VCL.Menus,
  VCL.ActnList,
  VCL.ToolWin,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Graphics,
   
  GLS.Memo;

type

  TShaderMemoForm = class(TForm)
    ImageList: TImageList;
    ToolBar: TToolBar;
    TBOpen: TToolButton;
    TBSave: TToolButton;
    TBStayOnTop: TToolButton;
    TBHelp: TToolButton;
    ToolButton2: TToolButton;
    TBCopy: TToolButton;
    TBPaste: TToolButton;
    TBCut: TToolButton;
    ToolButton10: TToolButton;
    TBTemplate: TToolButton;
    TBUndo: TToolButton;
    TBRedo: TToolButton;
    ToolButton4: TToolButton;
    GLSLMemo: TGLSSynHiMemo;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    TemplateMenu: TPopupMenu;
    GLSL120: TMenuItem;
    GLSL330: TMenuItem;
    GLSL400: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    CompilatorLog: TMemo;
    TBIncIndent: TToolButton;
    TBDecIndent: TToolButton;
    TBComment: TToolButton;
    TBUncoment: TToolButton;
    ToolButton1: TToolButton;
    Panel1: TPanel;
    CancelButton: TButton;
    OKButton: TButton;
    CheckButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GLSLMemoGutterClick(Sender: TObject; LineNo: Integer);
    procedure GLSLMemoGutterDraw(Sender: TObject; ACanvas: TCanvas; LineNo: Integer; rct: TRect);
    procedure TBOpenClick(Sender: TObject);
    procedure TBSaveClick(Sender: TObject);
    procedure TBStayOnTopClick(Sender: TObject);
    procedure TBUndoClick(Sender: TObject);
    procedure GLSLMemoUndoChange(Sender: TObject; CanUndo, CanRedo: Boolean);
    procedure TBRedoClick(Sender: TObject);
    procedure TBCopyClick(Sender: TObject);
    procedure TBPasteClick(Sender: TObject);
    procedure TBCutClick(Sender: TObject);
    procedure CheckButtonClick(Sender: TObject);
    procedure TBIncIndentClick(Sender: TObject);
    procedure TBDecIndentClick(Sender: TObject);
    procedure TBCommentClick(Sender: TObject);
    procedure TBUncomentClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FLightLineStyle: Integer;
    FOnCheck: TNotifyEvent;
    procedure OnTemplateClick(Sender: TObject);
  public
    property OnCheck: TNotifyEvent read FOnCheck write FOnCheck;
  end;

function GLShaderEditorForm: TShaderMemoForm;
procedure ReleaseGLShaderEditor;

//------------------------------------------------------------------
implementation
//------------------------------------------------------------------

{$R *.dfm}

const
  cRegistryKey = 'Software\GLScene\GLSceneShaderEdit';

// ---------------------Syntax keywords 

const

  GLSLDirectives: array[0..12] of string =
    (
    '#define',
    '#undef',
    '#if',
    '#ifdef',
    '#ifndef',
    '#else',
    '#elif',
    '#endif',
    '#error',
    '#pragma',
    '#extension',
    '#version',
    '#line'
    );

  GLSLQualifiers: array[0..33] of string =
    (
    'attribute', 'const', 'uniform', 'varying',
    'layout',
    'centroid', 'flat', 'smooth', 'noperspective',
    'patch', 'sample',
    'break', 'continue', 'do', 'for', 'while', 'switch', 'case', 'default',
    'if', 'else',
    'subroutine',
    'in', 'out', 'inout',
    'true', 'false',
    'invariant',
    'discard', 'return',
    'lowp', 'mediump', 'highp', 'precision'
    );

  GLSLTypes: array[0..85] of string =
    (
    'float', 'double', 'int', 'void', 'bool',
    'mat2', 'mat3', 'mat4', 'dmat2', 'dmat3', 'dmat4',
    'mat2x2', 'mat2x3', 'mat2x4', 'dmat2x2', 'dmat2x3', 'dmat2x4',
    'mat3x2', 'mat3x3', 'mat3x4', 'dmat3x2', 'dmat3x3', 'dmat3x4',
    'mat4x2', 'mat4x3', 'mat4x4', 'dmat4x2', 'dmat4x3', 'dmat4x4',
    'vec2', 'vec3', 'vec4', 'ivec2', 'ivec3', 'ivec4', 'bvec2', 'bvec3', 'bvec4', 'dvec2', 'dvec3', 'dvec4',
    'uint', 'uvec2', 'uvec3', 'uvec4',
    'sampler1D', 'sampler2D', 'sampler3D', 'samplerCube',
    'sampler1DShadow', 'sampler2DShadow', 'samplerCubeShadow',
    'sampler1DArray', 'sampler2DArray',
    'sampler1DArrayShadow', 'sampler2DArrayShadow',
    'isampler1D', 'isampler2D', 'isampler3D', 'isamplerCube',
    'isampler1DArray', 'isampler2DArray',
    'usampler1D', 'usampler2D', 'usampler3D', 'usamplerCube',
    'usampler1DArray', 'usampler2DArray',
    'sampler2DRect', 'sampler2DRectShadow', 'isampler2DRect', 'usampler2DRect',
    'samplerBuffer', 'isamplerBuffer', 'usamplerBuffer',
    'sampler2DMS', 'isampler2DMS', 'usampler2DMS',
    'sampler2DMSArray', 'isampler2DMSArray', 'usampler2DMSArray',
    'samplerCubeArray', 'samplerCubeArrayShadow', 'isamplerCubeArray', 'usamplerCubeArray',
    'struct'
    );

  GLSLBuildIn: array[0..117] of string =
    (
    'gl_Color',
    'gl_SecondaryColor',
    'gl_Normal',
    'gl_Vertex',
    'gl_MultiTexCoord0',
    'gl_MultiTexCoord1',
    'gl_MultiTexCoord2',
    'gl_MultiTexCoord3',
    'gl_MultiTexCoord4',
    'gl_MultiTexCoord5',
    'gl_MultiTexCoord6',
    'gl_MultiTexCoord7',
    'gl_FogCoord',
    'gl_FrontColor',
    'gl_BackColor',
    'gl_FrontSecondaryColor',
    'gl_BackSecondaryColor',
    'gl_TexCoord',
    'gl_FogFragCoord',
    'gl_PointCoord',
    'gl_Position',
    'gl_PointSize',
    'gl_ClipVertex',
    'gl_FragCoord',
    'gl_FrontFacing',
    'gl_FragColor',
    'gl_FragData',
    'gl_FragDepth',
    'gl_VertexID',
    'gl_InstanceID',
    'gl_in',
    'gl_out',
    'gl_PrimitiveIDIn',
    'gl_InvocationID',
    'gl_PrimitiveID',
    'gl_Layer',
    'gl_PatchVerticesIn',
    'gl_TessLevelOuter',
    'gl_TessLevelInner',
    'gl_TessCoord',
    'gl_SampleID',
    'gl_SamplePosition',
    'gl_SampleMask',
    'gl_FrontFacing',
    'gl_ClipDistance',
    'gl_MaxVertexAttribs',
    'gl_MaxVertexUniformComponents',
    'gl_MaxVaryingFloats',
    'gl_MaxVaryingComponents',
    'gl_MaxVertexOutputComponents',
    'gl_MaxGeometryInputComponents',
    'gl_MaxGeometryOutputComponents',
    'gl_MaxFragmentInputComponents',
    'gl_MaxVertexTextureImageUnits',
    'gl_MaxCombinedTextureImageUnits',
    'gl_MaxTextureImageUnits',
    'gl_MaxFragmentUniformComponents',
    'gl_MaxDrawBuffers',
    'gl_MaxClipDistances',
    'gl_MaxGeometryTextureImageUnits',
    'gl_MaxGeometryOutputVertices',
    'gl_MaxGeometryTotalOutputComponents',
    'gl_MaxGeometryUniformComponents',
    'gl_MaxGeometryVaryingComponents',
    'gl_MaxTessControlInputComponents',
    'gl_MaxTessControlOutputComponents',
    'gl_MaxTessControlTextureImageUnits',
    'gl_MaxTessControlUniformComponents',
    'gl_MaxTessControlTotalOutputComponents',
    'gl_MaxTessEvaluationInputComponents',
    'gl_MaxTessEvaluationOutputComponents',
    'gl_MaxTessEvaluationTextureImageUnits',
    'gl_MaxTessEvaluationUniformComponents',
    'gl_MaxTessPatchComponents',
    'gl_MaxPatchVertices',
    'gl_MaxTessGenLevel',
    'gl_MaxTextureUnits',
    'gl_MaxTextureCoords',
    'gl_MaxClipPlanes',
    'gl_DepthRange',
    'gl_ModelViewMatrix',
    'gl_ProjectionMatrix',
    'gl_ModelViewProjectionMatrix',
    'gl_TextureMatrix',
    'gl_NormalMatrix',
    'gl_ModelViewMatrixInverse',
    'gl_ProjectionMatrixInverse',
    'gl_ModelViewProjectionMatrixInverse',
    'gl_TextureMatrixInverse',
    'gl_ModelViewMatrixTranspose',
    'gl_ProjectionMatrixTranspose',
    'gl_ModelViewProjectionMatrixTranspose',
    'gl_TextureMatrixTranspose',
    'gl_ModelViewMatrixInverseTranspose',
    'gl_ProjectionMatrixInverseTranspose',
    'gl_ModelViewProjectionMatrixInverseTranspose',
    'gl_TextureMatrixInverseTranspose',
    'gl_NormalScale',
    'gl_ClipPlane',
    'gl_Point',
    'gl_FrontMaterial',
    'gl_BackMaterial',
    'gl_LightSource',
    'gl_LightModel',
    'gl_FrontLightModelProduct',
    'gl_BackLightModelProduct',
    'gl_FrontLightProduct',
    'gl_BackLightProduct',
    'gl_TextureEnvColor',
    'gl_EyePlaneS',
    'gl_EyePlaneT',
    'gl_EyePlaneR',
    'gl_EyePlaneQ',
    'gl_ObjectPlaneS',
    'gl_ObjectPlaneT',
    'gl_ObjectPlaneR',
    'gl_ObjectPlaneQ',
    'gl_Fog'
    );

  GLSLFunctions: array[0..139] of string =
    (

    // Angle and Trigonometry Functions
    'radians',
    'degrees',
    'sin',
    'cos',
    'tan',
    'asin',
    'acos',
    'atan',
    'sinh',
    'cosh',
    'tanh',
    'asinh',
    'acosh',
    'atanh',

    // Exponetial
    'pow',
    'exp',
    'log',
    'exp2',
    'log2',
    'sqrt',
    'inversesqrt',

    // Common
    'abs',
    'sign',
    'floor',
    'trunc',
    'round',
    'roundEven',
    'ceil',
    'fract',
    'mod',
    'min',
    'max',
    'clamp',
    'mix',
    'step',
    'smoothstep',
    'isnan',
    'isinf',
    'floatBitsToInt',
    'floatBitsToUint',
    'intBitsToFloat',
    'uintBitsToFloat',
    'fma',
    'frexp',
    'ldexp',

    // Floating-Point Pack and Unpack Functions
    'packUnorm2x16',
    'packUnorm4x8',
    'packSnorm4x8',
    'unpackUnorm2x16',
    'unpackUnorm4x8',
    'unpackSnorm4x8',
    'packDouble2x32',
    'unpackDouble2x32',

    // Geometric
    'length',
    'distance',
    'dot',
    'cross',
    'normalize',
    'ftransform',
    'faceforward',
    'reflect',
    'maxtrixCompMult',
    'outerProduct',
    'transpose',
    'determinant',
    'inverse',

    // Vector
    'lessThan',
    'lessThanEqual',
    'greaterThan',
    'greaterThanEqual',
    'equal',
    'notEqual',
    'any',
    'all',
    'not',

    // Integer Functions
    'uaddCarry',
    'usubBorrow',
    'umulExtended',
    'bitfieldExtract',
    'bitfieldInsert',
    'bitfieldReverse',
    'bitCount',
    'findLSB',
    'findMSB',

    // Texture
    'texture1D',
    'texture1DProj',
    'texture1DLod',
    'texture1DProjLod',
    'texture2D',
    'texture2DProj',
    'texture2DLod',
    'texture2DProjLod',
    'texture3D',
    'texture3DProj',
    'texture3DLod',
    'texture3DProjLod',
    'textureCube',
    'textureCubeLod',
    'shadow1D',
    'shadow1DProj',
    'shadow1DLod',
    'shadow1DProjLod',
    'shadow2D',
    'shadow2DProj',
    'shadow2DLod',
    'shadow2DProjLod',
    'textureSize',
    'textureQueryLod',
    'texture',
    'textureProj',
    'textureLod',
    'textureOffset',
    'texelFetch',
    'texelFetchOffset',
    'textureProjOffset',
    'textureLodOffset',
    'textureProjLod',
    'textureProjLodOffset',
    'textureGrad',
    'textureGradOffset',
    'textureProjGrad',
    'textureProjGradOffset',
    'textureGather',
    'textureGatherOffset',
    'textureGatherOffsets',

    // Fragment
    'dFdx',
    'dFdy',
    'fwidth',

    // Interpolation Functions
    'interpolateAtCentroid',
    'interpolateAtSample',
    'interpolateAtOffset',
    // Noise
    'noise1',
    'noise2',
    'noise3',
    'noise4',

    // Geometry Shader Functions
    'EmitStreamVertex',
    'EndStreamPrimitive',
    'EmitVertex',
    'EndPrimitive',

    // Shader Invocation Control Functions
    'barrier'
    );
 

// ---------------------Shader template 
const
  cTemplates: array[0..6] of string =
  (
    '#version 120'#10#13+
    #10#13+
    'attribute vec3 Position;'#10#13+
    'attribute vec3 Normal;'#10#13+
    'varying vec3 v2f_Normal;'#10#13+
    'varying vec3 v2f_LightDir;'#10#13+
    'varying vec3 v2f_ViewDir;'#10#13+
    #10#13+
    'uniform mat4 ModelMatrix;'#10#13+
    'uniform mat4 ViewProjectionMatrix;'#10#13+
    'uniform mat3 NormalMatrix;'#10#13+
    'uniform vec4 LightPosition;'#10#13+
    'uniform vec4 CameraPosition;'#10#13+
    #10#13+
    'void main()'#10#13+
    '{'#10#13+
    '  vec4 WorldPos = ModelMatrix * vec4(Position,1.0);'#10#13+
    '  gl_Position = ViewProjectionMatrix * WorldPos;'#10#13+
    '  v2f_Normal = NormalMatrix * Normal;'#10#13+
    '  v2f_LightDir = LightPosition.xyz - WorldPos.xyz;'#10#13+
    '  v2f_ViewDir = CameraPosition.xyz - WorldPos.xyz;'#10#13+
    '}'#10#13,
//-----------------------------------------------------------------------
    '#version 120'#10#13+
    #10#13+
    'attribute vec3 Position;'#10#13+
    'attribute vec3 Normal;'#10#13+
    'attribute vec3 Tangent;'#10#13+
    'attribute vec3 Binormal;'#10#13+
    'attribute vec2 TexCoord0;'#10#13+
    #10#13+
    'varying vec2 v2f_TexCoord;'#10#13+
    'varying vec3 v2f_Normal;'#10#13+
    'varying vec3 v2f_Tangent;'#10#13+
    'varying vec3 v2f_Binormal;'#10#13+
    'varying vec3 v2f_LightDir;'#10#13+
    'varying vec3 v2f_ViewDir;'#10#13+
    #10#13+
    'uniform mat4 ModelMatrix;'#10#13+
    'uniform mat4 ViewProjectionMatrix;'#10#13+
    'uniform vec4 LightPosition;'#10#13+
    'uniform vec4 CameraPosition;'#10#13+
    'uniform vec2 BaseTextureRepeat;'#10#13+
    #10#13+
    'void main()'#10#13+
    '{'#10#13+
    '  vec3 WorldPos = (ModelMatrix * vec4(Position,1.0)).xyz;'#10#13+
    '  gl_Position = ViewProjectionMatrix * vec4(WorldPos, 1.0);'#10#13+
    '  v2f_TexCoord = TexCoord0.xy*BaseTextureRepeat;'#10#13+
    '  v2f_Normal = Normal;'#10#13+
    '  v2f_Tangent = Tangent;'#10#13+
    '  v2f_Binormal = Binormal;'#10#13+
    '  v2f_LightDir = LightPosition.xyz - WorldPos.xyz;'#10#13+
    '  v2f_ViewDir = CameraPosition.xyz - WorldPos.xyz;'#10#13+
    '}'#10#13,
//-----------------------------------------------------------------------
    '#version 120'#10#13+
    #10#13+
    'varying vec3 v2f_Normal;'#10#13+
    'varying vec3 v2f_LightDir;'#10#13+
    'varying vec3 v2f_ViewDir;'#10#13+
    #10#13+
    'uniform vec4 MaterialAmbientColor;'#10#13+
    'uniform vec4 MaterialDiffuseColor;'#10#13+
    'uniform vec4 MaterialSpecularColor;'#10#13+
    'uniform float MaterialShiness;'#10#13+
    #10#13+
    'void main()'#10#13+
    '{'#10#13+
    '  vec3 N = normalize(v2f_Normal);'#10#13+
    #10#13+
    '  vec3 L = normalize(v2f_LightDir);'#10#13+
    '  float DiffuseIntensity = max( dot( N, L ), 0.0);'#10#13+
    '  vec4  cDiffuse = DiffuseIntensity * MaterialDiffuseColor;'#10#13+
    #10#13+
    '  vec4 cSpecular = vec4(0.0);'#10#13+
    '  if (DiffuseIntensity > 0.0)'#10#13+
    '  {'#10#13+
    '    vec3 V = normalize(v2f_ViewDir);'#10#13+
    '    vec3 R = reflect(-V, N);'#10#13+
    '    float RdotL = max( dot( L, R ), 0.0);'#10#13+
    '    if (RdotL > 0.0)'#10#13+
    '      cSpecular = pow( RdotL, MaterialShiness) * MaterialSpecularColor;'#10#13+
    '  }'#10#13+
    #10#13+
    '  gl_FragColor = MaterialAmbientColor + cDiffuse  + cSpecular;'#10#13+
    '}'#10#13,
//-----------------------------------------------------------------------
    '#version 120'#10#13+
    #10#13+
    'varying vec3 v2f_Normal;'#10#13+
    'varying vec3 v2f_Tangent;'#10#13+
    'varying vec3 v2f_Binormal;'#10#13+
    'varying vec3 v2f_LightDir;'#10#13+
    'varying vec3 v2f_ViewDir;'#10#13+
    'varying vec2 v2f_TexCoord;'#10#13+
    #10#13+
    'uniform mat3 NormalMatrix;'#10#13+
    'uniform sampler2D DiffuseMap;'#10#13+
    'uniform sampler2D NormalMap;'#10#13+
    #10#13+
    'uniform vec4 MaterialAmbientColor;'#10#13+
    'uniform vec4 MaterialDiffuseColor;'#10#13+
    'uniform vec4 MaterialSpecularColor;'#10#13+
    'uniform float MaterialShiness;'#10#13+
    #10#13+
    'void main()'#10#13+
    '{'#10#13+
    '  vec3 normal = normalize(v2f_Normal);'#10#13+
    '  vec3 tangent = normalize(v2f_Tangent);'#10#13+
    '  vec3 binormal = normalize(v2f_Binormal);'#10#13+
    '  mat3 basis = mat3(tangent, binormal, normal);'#10#13+
    '  vec3 N = texture2D(NormalMap, v2f_TexCoord).xyz;'#10#13+
    '  N = N * vec3(2.0) - vec3(1.0);'#10#13+
    '  N = basis*N;'#10#13+
    '  N = NormalMatrix*N;'#10#13+
    '  N = normalize(N);'#10#13+
    #10#13+
    '  vec3 L = normalize(v2f_LightDir);'#10#13+
    '  float DiffuseIntensity = max( dot( N, L ), 0.0);'#10#13+
    '  vec4  cDiffuse = DiffuseIntensity * MaterialDiffuseColor;'#10#13+
    #10#13+
    '  vec4 cSpecular = vec4(0.0);'#10#13+
    '  if (DiffuseIntensity > 0.0)'#10#13+
    '  {'#10#13+
    '    vec3 V = normalize(v2f_ViewDir);'#10#13+
    '    vec3 R = reflect(-V, N);'#10#13+
    '    float RdotL = max( dot( L, R ), 0.0);'#10#13+
    '    if (RdotL > 0.0)'#10#13+
    '      cSpecular = pow( RdotL, MaterialShiness) * MaterialSpecularColor;'#10#13+
    '  }'#10#13+
    #10#13+
    '  vec4 cBaseColor = texture2D( DiffuseMap, v2f_TexCoord);'#10#13+
    '  gl_FragColor = (MaterialAmbientColor + cDiffuse ) * cBaseColor + cSpecular;'#10#13+
    '}'#10#13,
//-----------------------------------------------------------------------
    '#version 330'#10#13+
    'layout(triangles_adjacency) in;'#10#13+
    'layout(line_strip, max_vertices = 6) out;'#10#13+
    'in vec4 v2g_WorldPos[]; // Vertex position in view space'#10#13+
    'uniform vec4 CameraPosition;'#10#13+
    #10#13+
    '// calculating facing of a triangle relative to eye'#10#13+
    'float facing(vec4 v0, vec4 v1, vec4 v2, vec4 eye_pos)'#10#13+
    '{'#10#13+
    '    vec3 e0 = v1.xyz - v0.xyz;'#10#13+
    '    vec3 e1 = v2.xyz - v0.xyz;'#10#13+
    '    vec4 p;'#10#13+
    '    p.xyz = cross(e1, e0);'#10#13+
    '    p.w = -dot(v0.xyz, p.xyz);'#10#13+
    '    return -dot(p, eye_pos);'#10#13+
    '}'#10#13+
    #10#13+
    '// output lines on silhouette edges by comparing facing of adjacent triangles'#10#13+
    'void main()'#10#13+
    '{'#10#13+
    '    float f = facing(v2g_WorldPos[0], v2g_WorldPos[2], v2g_WorldPos[4], CameraPosition);'#10#13+
    '    // only look at front facing triangles'#10#13+
    '    if (f > 0.0)'#10#13+
    '    {'#10#13+
    '        float f;'#10#13+
    '        // test edge 0'#10#13+
    '        f = facing(v2g_WorldPos[0], v2g_WorldPos[1], v2g_WorldPos[2], CameraPosition);'#10#13+
    '        if (f <= 0)'#10#13+
    '        {'#10#13+
    '            gl_Position = gl_in[0].gl_Position;'#10#13+
    '            EmitVertex();'#10#13+
    '            gl_Position = gl_in[2].gl_Position;'#10#13+
    '            EmitVertex();'#10#13+
    '            EndPrimitive();'#10#13+
    '        }'#10#13+
    #10#13+
    '        // test edge 1'#10#13+
    '        f = facing(v2g_WorldPos[2], v2g_WorldPos[3], v2g_WorldPos[4], CameraPosition);'#10#13+
    '        if (f <= 0.0)'#10#13+
    '        {'#10#13+
    '            gl_Position = gl_in[2].gl_Position;'#10#13+
    '            EmitVertex();'#10#13+
    '            gl_Position = gl_in[4].gl_Position;'#10#13+
    '            EmitVertex();'#10#13+
    '            EndPrimitive();'#10#13+
    '        }'#10#13+
    #10#13+
    '        // test edge 2'#10#13+
    '        f = facing(v2g_WorldPos[4], v2g_WorldPos[5], v2g_WorldPos[0], CameraPosition);'#10#13+
    '        if (f <= 0.0)'#10#13+
    '        {'#10#13+
    '            gl_Position = gl_in[4].gl_Position;'#10#13+
    '            EmitVertex();'#10#13+
    '            gl_Position = gl_in[0].gl_Position;'#10#13+
    '            EmitVertex();'#10#13+
    '            EndPrimitive();'#10#13+
    '        }'#10#13+
    '    }'#10#13+
    '}'#10#13,
//-----------------------------------------------------------------------
    'attribute vec3 Position;'#10#13+
    'attribute vec4 Color;'#10#13+
    'attribute vec3 Normal;'#10#13+
    'attribute vec3 Tangent;'#10#13+
    'attribute vec3 Binormal;'#10#13+
    'attribute vec2 TexCoord0;'#10#13+
    'attribute vec2 TexCoord1;'#10#13+
    'attribute vec2 TexCoord2;'#10#13+
    'attribute vec2 TexCoord3;'#10#13+
    'attribute vec2 TexCoord4;'#10#13+
    'attribute vec2 TexCoord5;'#10#13+
    'attribute vec2 TexCoord6;'#10#13+
    'attribute vec2 TexCoord7;'#10#13+
    'attribute vec4 Custom0;'#10#13+
    'attribute vec2 Custom1;'#10#13+
    'attribute vec2 Custom2;'#10#13,
//-----------------------------------------------------------------------
    'in vec3 Position;'#10#13+
    'in vec4 Color;'#10#13+
    'in vec3 Normal;'#10#13+
    'in vec3 Tangent;'#10#13+
    'in vec3 Binormal;'#10#13+
    'in vec2 TexCoord0;'#10#13+
    'in vec2 TexCoord1;'#10#13+
    'in vec2 TexCoord2;'#10#13+
    'in vec2 TexCoord3;'#10#13+
    'in vec2 TexCoord4;'#10#13+
    'in vec2 TexCoord5;'#10#13+
    'in vec2 TexCoord6;'#10#13+
    'in vec2 TexCoord7;'#10#13+
    'in vec4 Custom0;'#10#13+
    'in vec2 Custom1;'#10#13+
    'in vec2 Custom2;'#10#13
  );

 

type
  TFriendlyMemo = class(TGLSSynHiMemo);

var
  vGLShaderEditor: TShaderMemoForm;

function GLShaderEditorForm: TShaderMemoForm;
begin
  if not Assigned(vGLShaderEditor) then
    vGLShaderEditor := TShaderMemoForm.Create(nil);
  Result := vGLShaderEditor;
end;

procedure ReleaseGLShaderEditor;
begin
  if Assigned(vGLShaderEditor) then
  begin
    vGLShaderEditor.Free;
    vGLShaderEditor := nil;
  end;
end;

function ReadRegistryInteger(reg: TRegistry; const name: string;
  defaultValue: Integer): Integer;
begin
  if reg.ValueExists(name) then
    Result := reg.ReadInteger(name)
  else
    Result := defaultValue;
end;

procedure TShaderMemoForm.FormCreate(Sender: TObject);
var
  reg: TRegistry;
  No: Integer;
  item: TMenuItem;
begin
  reg := TRegistry.Create;
  try
    if reg.OpenKey(cRegistryKey, True) then
    begin
      Left := ReadRegistryInteger(reg, 'Left', Left);
      Top := ReadRegistryInteger(reg, 'Top', Top);
      Width := ReadRegistryInteger(reg, 'Width', 500);
      Height := ReadRegistryInteger(reg, 'Height', 640);
    end;
  finally
    reg.Free;
  end;

  No := GLSLMemo.Styles.Add(clRed, clWhite, []);
  GLSLMemo.AddWord(No, GLSLDirectives);

  No := GLSLMemo.Styles.Add(clPurple, clWhite, [fsBold]);
  GLSLMemo.AddWord(No, GLSLQualifiers);

  No := GLSLMemo.Styles.Add(clBlue, clWhite, [fsBold]);
  GLSLMemo.AddWord(No, GLSLTypes);

  No := GLSLMemo.Styles.Add(clGray, clWhite, [fsBold]);
  GLSLMemo.AddWord(No, GLSLBuildIn);

  No := GLSLMemo.Styles.Add(clGreen, clWhite, [fsItalic]);
  GLSLMemo.AddWord(No, GLSLFunctions);

  No := GLSLMemo.Styles.Add(clYellow, clSilver, [fsItalic]);
  GLSLMemo.AddWord(No, GLSLFunctions);

  FLightLineStyle := GLSLMemo.Styles.Add(clBlack, clLtGray, []);

  GLSLMemo.MultiCommentLeft := '/*';
  GLSLMemo.MultiCommentRight := '*/';
  GLSLMemo.LineComment := '//';

  GLSLMemo.CaseSensitive := True;
  GLSLMemo.DelErase := True;

  item := NewItem('Attribute block', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 5;
  GLSL120.Add(item);

  item := NewItem('Basic vertex program', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 0;
  GLSL120.Add(item);

  item := NewItem('Basic vertex program with TBN pass', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 1;
  GLSL120.Add(item);

  item := NewItem('Basic fragment program, Phong lighting', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 2;
  GLSL120.Add(item);

  item := NewItem('Fragment program, normal mapping', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 3;
  GLSL120.Add(item);

  item := NewItem('Attribute block', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 6;
  GLSL330.Add(item);

  item := NewItem('Geometry program, edge detection', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 4;
  GLSL330.Add(item);
end;

procedure TShaderMemoForm.FormDestroy(Sender: TObject);
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    if reg.OpenKey(cRegistryKey, True) then
    begin
      reg.WriteInteger('Left', Left);
      reg.WriteInteger('Top', Top);
      reg.WriteInteger('Width', Width);
      reg.WriteInteger('Height', Height);
    end;
  finally
    reg.Free;
  end;
end;

procedure TShaderMemoForm.FormShow(Sender: TObject);
begin
  if GLSLMemo.Lines.Count = 0 then
    GLSLMemo.Lines.Add('');
end;

procedure TShaderMemoForm.GLSLMemoGutterClick(Sender: TObject; LineNo: Integer);
begin
  with GLSLMemo do
  begin
    LineStyle[LineNo] := FLightLineStyle - LineStyle[LineNo];
  end;
end;

procedure TShaderMemoForm.GLSLMemoGutterDraw(Sender: TObject; ACanvas: TCanvas;
  LineNo: Integer; rct: TRect);
var
  txt: string;
begin
  if GLSLMemo.LineStyle[LineNo] = FLightLineStyle then
    with rct, ACanvas do
    begin
      Pen.Color := GLSLMemo.GutterColor;
      Brush.Color := clWhite;
      Font.Style := Font.Style + [fsBold];
      txt := IntToStr(LineNo+1);
      TextRect(rct, txt, [tfCenter]);
    end;
end;

procedure TShaderMemoForm.GLSLMemoUndoChange(Sender: TObject; CanUndo, CanRedo: Boolean);
begin
  TBUndo.Enabled := CanUndo;
  TBRedo.Enabled := CanRedo;
end;

procedure TShaderMemoForm.OnTemplateClick(Sender: TObject);
begin
  GLSLMemo.InsertTemplate(cTemplates[TMenuItem(Sender).Tag]);
end;

procedure TShaderMemoForm.TBCommentClick(Sender: TObject);
var
  I: Integer;
  S, E: Integer;
begin
  with TFriendlyMemo(GLSLMemo) do
  begin
    if SelLength > 0 then
    begin
      S := SelStart.Y;
      E := SelEnd.Y;
      for I := S to E do
      begin
        SetCursor(0, I);
        InsertChar('/');
        InsertChar('/');
      end;
      SelectLines(S, E);
    end;
  end;
end;

procedure TShaderMemoForm.TBUncomentClick(Sender: TObject);
var
  I: Integer;
  S, E: Integer;
  C: string;
begin
  with TFriendlyMemo(GLSLMemo) do
  begin
    if SelLength > 0 then
    begin
      S := SelStart.Y;
      E := SelEnd.Y;
      for I := S to E do
      begin
        C := Lines[I];
        if (C[1] = '/') and (C[2] = '/') then
        begin
          Delete(C, 1, 2);
          Lines[I] := C;
          LineStyle[I] := 0;
        end;
      end;
      SelectLines(S, E);
    end;
  end;
end;

procedure TShaderMemoForm.TBCopyClick(Sender: TObject);
begin
  GLSLMemo.CopyToClipBoard;
end;

procedure TShaderMemoForm.TBCutClick(Sender: TObject);
begin
  GLSLMemo.CutToClipBoard;
end;

procedure TShaderMemoForm.TBIncIndentClick(Sender: TObject);
var
  I: Integer;
  S, E: Integer;
begin
  with TFriendlyMemo(GLSLMemo) do
  begin
    if SelLength > 0 then
    begin
      S := SelStart.Y;
      E := SelEnd.Y;
      for I := S to E do
      begin
        SetCursor(0, I);
        InsertChar(#9);
      end;
      SelectLines(S, E);
    end;
  end;
end;

procedure TShaderMemoForm.TBDecIndentClick(Sender: TObject);
var
  I, J: Integer;
  S, E: Integer;
  C: string;
begin
  with TFriendlyMemo(GLSLMemo) do
  begin
    if SelLength > 0 then
    begin
      S := SelStart.Y;
      E := SelEnd.Y;
      for I := S to E do
      begin
        C := Lines[I];
        for J := 1 to TabSize do
          if C[1] = ' ' then
            Delete(C, 1, 1);
        Lines[I] := C;
      end;
      SelectLines(S, E);
    end;
  end;
end;

procedure TShaderMemoForm.TBOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    GLSLMemo.Lines.LoadFromFile(OpenDialog.FileName);
  SetFocus;
end;

procedure TShaderMemoForm.TBPasteClick(Sender: TObject);
begin
  GLSLMemo.PasteFromClipBoard;
end;

procedure TShaderMemoForm.TBRedoClick(Sender: TObject);
begin
  with GLSLMemo do
  begin
    Redo;
    SetFocus;
  end;
end;

procedure TShaderMemoForm.TBSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    GLSLMemo.Lines.SaveToFile(SaveDialog.FileName);
  SetFocus;
end;

procedure TShaderMemoForm.TBStayOnTopClick(Sender: TObject);
begin
  if TBStayOnTop.Down then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TShaderMemoForm.TBUndoClick(Sender: TObject);
begin
  with GLSLMemo do
  begin
    Undo;
    SetFocus;
  end;
end;

procedure TShaderMemoForm.CheckButtonClick(Sender: TObject);
begin
  if Assigned(FOnCheck) then
    FOnCheck(Self);
end;

//-----------------------------------------------
initialization
//-----------------------------------------------

finalization

  ReleaseGLShaderEditor;

end.

