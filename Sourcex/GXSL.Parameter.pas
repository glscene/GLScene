//
// The graphics engine GXScene https://github.com/glscene
//
unit GXSL.Parameter;

interface

{$I GXS.Scene.inc}
{$M-}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,

  GXS.VectorTypes,
  GXS.TextureFormat,
  GXS.RenderContextInfo;

type

  TgxSLDataType = (
    GLSLTypeUndefined,
    GLSLType1F,
    GLSLType2F,
    GLSLType3F,
    GLSLType4F,
    GLSLType1I,
    GLSLType2I,
    GLSLType3I,
    GLSLType4I,
    GLSLType1UI,
    GLSLType2UI,
    GLSLType3UI,
    GLSLType4UI,
    GLSLTypeMat2F,
    GLSLTypeMat3F,
    GLSLTypeMat4F,
    GLSLTypeVoid);

  TgxSLSamplerType = (
    GLSLSamplerUndefined,
    GLSLSampler1D,
    GLSLSampler2D,
    GLSLSampler3D,
    GLSLSamplerCube,
    GLSLSampler1DShadow,
    GLSLSampler2DShadow,
    GLSLSampler1DArray,
    GLSLSampler2DArray,
    GLSLSampler1DArrayShadow,
    GLSLSampler2DArrayShadow,
    GLSLSamplerCubeShadow,
    GLSLIntSampler1D,
    GLSLIntSampler2D,
    GLSLIntSampler3D,
    GLSLIntSamplerCube,
    GLSLIntSampler1DArray,
    GLSLIntSampler2DArray,
    GLSLUIntSampler1D,
    GLSLUIntSampler2D,
    GLSLUIntSampler3D,
    GLSLUIntSamplerCube,
    GLSLUIntSampler1DArray,
    GLSLUIntSampler2DArray,
    GLSLSamplerRect,
    GLSLSamplerRectShadow,
    GLSLSamplerBuffer,
    GLSLIntSamplerRect,
    GLSLIntSamplerBuffer,
    GLSLUIntSamplerRect,
    GLSLUIntSamplerBuffer,
    GLSLSamplerMS,
    GLSLIntSamplerMS,
    GLSLUIntSamplerMS,
    GLSLSamplerMSArray,
    GLSLIntSamplerMSArray,
    GLSLUIntSamplerMSArray
    );

  TgxgsInTypes = (
    gsInPoints,
    gsInLines,
    gsInAdjLines,
    gsInTriangles,
    gsInAdjTriangles
  );

  TgxgsOutTypes = (
    gsOutPoints,
    gsOutLineStrip,
    sOutTriangleStrip
  );


  IgxShaderParameter = interface(IInterface)
    function GetName: string;
    function GetGLSLType: TgxSLDataType;
    function GetGLSLSamplerType: TgxSLSamplerType;
    function GetAutoSetMethod: string;
    function GetTextureName: string;
    function GetSamplerName: string;
    function GetTextureSwizzle: TgxSwizzleVector;
    procedure SetTextureName(const AValue: string);
    procedure SetSamplerName(const AValue: string);
    procedure SetAutoSetMethod(const AValue: string);
    procedure SetTextureSwizzle(const AValue: TgxSwizzleVector);
    function GetFloat: Single;
    function GetVec2: TVector2f;
    function GetVec3: TVector3f;
    function GetVec4: TVector4f;
    function GetInt: Integer;
    function GetIVec2: TVector2i;
    function GetIVec3: TVector3i;
    function GetIVec4: TVector4i;
    function GetUInt: Cardinal;
    function GetUVec2: TVector2ui;
    function GetUVec3: TVector3ui;
    function GetUVec4: TVector4ui;
    procedure SetFloat(const Value: Single);
    procedure SetVec2(const Value: TVector2f);
    procedure SetVec3(const Value: TVector3f);
    procedure SetVec4(const Value: TVector4f);
    procedure SetInt(const Value: Integer);
    procedure SetIVec2(const Value: TVector2i);
    procedure SetIVec3(const Value: TVector3i);
    procedure SetIVec4(const Value: TVector4i);
    procedure SetUInt(const Value: Cardinal);
    procedure SetUVec2(const Value: TVector2ui);
    procedure SetUVec3(const Value: TVector3ui);
    procedure SetUVec4(const Value: TVector4ui);
    function GetMat2: TMatrix2f;
    function GetMat3: TMatrix3f;
    function GetMat4: TMatrix4f;
    procedure SetMat2(const Value: TMatrix2f);
    procedure SetMat3(const Value: TMatrix3f);
    procedure SetMat4(const Value: TMatrix4f);
    procedure SetFloatArray(const Values: PGLFloat; Count: Integer);
    procedure SetIntArray(const Values: PGLInt; Count: Integer);
    procedure SetUIntArray(const Values: PGLUInt; Count: Integer);
    property Name: string read GetName;
    property GLSLType: TgxSLDataType read GetGLSLType;
    property GLSLSamplerType: TgxSLSamplerType read GetGLSLSamplerType;

    // Scalar types.
    property float: Single read GetFloat write SetFloat;
    property int: GLint read GetInt write SetInt;
    property uint: GLuint read GetUInt write SetUInt;

    // Float vector types.
    property vec2: TVector2f read GetVec2 write SetVec2;
    property vec3: TVector3f read GetVec3 write SetVec3;
    property vec4: TVector4f read GetVec4 write SetVec4;

    // Integer vector  types.
    property ivec2: TVector2i read GetIVec2 write SetIVec2;
    property ivec3: TVector3i read GetIVec3 write SetIVec3;
    property ivec4: TVector4i read GetIVec4 write SetIVec4;

    // Unsigned integer vector  types.
    property uvec2: TVector2ui read GetUVec2 write SetUVec2;
    property uvec3: TVector3ui read GetUVec3 write SetUVec3;
    property uvec4: TVector4ui read GetUVec4 write SetUVec4;

    // Matrix Types.
    property mat2: TMatrix2f read GetMat2 write SetMat2;
    property mat3: TMatrix3f read GetMat3 write SetMat3;
    property mat4: TMatrix4f read GetMat4 write SetMat4;

    // Bindings.
    property AutoSetMethod: string read GetAutoSetMethod write SetAutoSetMethod;
    property TextureName: string read GetTextureName write SetTextureName;
    property SamplerName: string read GetSamplerName write SetSamplerName;
    property TextureSwizzle: TgxSwizzleVector read GetTextureSwizzle write SetTextureSwizzle;
  end;

const
  cGLXLTypeString: array[TgxSLDataType] of AnsiString = (
    'undefined',
    'float',
    'vec2',
    'vec3',
    'vec4',
    'int',
    'ivec2',
    'ivec3',
    'ivec4',
    'uint',
    'uivec2',
    'uivec3',
    'uivec4',
    'mat2',
    'mat3',
    'mat4',
    'void');

  cGLXLSamplerString: array[TgxSLSamplerType] of AnsiString = (
    'undefined',
    'sampler1D',
    'sampler2D',
    'sampler3D',
    'samplerCube',
    'sampler1DShadow',
    'sampler2DShadow',
    'sampler1DArray',
    'sampler2DArray',
    'sampler1DArrayShadow',
    'sampler2DArrayShadow',
    'samplerCubeShadow',
    'isampler1D',
    'isampler2D',
    'isampler3D',
    'isamplerCube',
    'isampler1DArray',
    'isampler2DArray',
    'usampler1D',
    'usampler2D',
    'usampler3D',
    'usamplerCube',
    'usampler1DArray',
    'usampler2DArray',
    'samplerRect',
    'samplerRectShadow',
    'samplerBuffer',
    'isamplerRect',
    'isamplerBuffer',
    'usamplerRect',
    'usamplerBuffer',
    'samplerMS',
    'isamplerMS',
    'usamplerMS',
    'samplerMSArray',
    'isamplerMSArray',
    'usamplerMSArray');

const
  cGXgsInTypes : array[TgxgsInTypes] of GLenum =
    (GL_POINTS, GL_LINES, GL_LINES_ADJACENCY_EXT, GL_TRIANGLES,
     GL_TRIANGLES_ADJACENCY_EXT);
  cGXgsOutTypes: array[TgxgsOutTypes] of GLenum =
    (GL_POINTS, GL_LINE_STRIP, GL_TRIANGLE_STRIP);

type
  TUniformAutoSetMethod = procedure(Sender: IgxShaderParameter; var ARci: TgxRenderContextInfo) of object;

function GLXLTypeEnum(AType: TgxSLDataType): GLEnum;
function GLXLTypeComponentCount(AType: TgxSLDataType): Integer;
procedure RegisterUniformAutoSetMethod(AMethodName: string;
  AType: TgxSLDataType; AMethod: TUniformAutoSetMethod);
procedure FillUniformAutoSetMethodList(AList: TStrings;
  TypeFilter: TgxSLDataType); overload;
procedure FillUniformAutoSetMethodList(AList: TStrings;
  TypeFilter: TgxSLSamplerType); overload;
function GetUniformAutoSetMethod(AMethodName: string): TUniformAutoSetMethod;
function GetUniformAutoSetMethodName(AMethod: TUniformAutoSetMethod): string;

//---------------------------------------------------------------------------
implementation
//---------------------------------------------------------------------------

const
  cGLXLTypeComponents: array[TgxSLDataType] of Integer =
  (
    0,
    1,
    2,
    3,
    4,
    1,
    2,
    3,
    4,
    1,
    2,
    3,
    4,
    4,
    9,
    16,
    0
  );

  cGLXLTypeEnum: array[TgxSLDataType] of Integer =
  (
    0,
    GL_FLOAT,
    GL_FLOAT,
    GL_FLOAT,
    GL_FLOAT,
    GL_INT,
    GL_INT,
    GL_INT,
    GL_INT,
    GL_UNSIGNED_INT,
    GL_UNSIGNED_INT,
    GL_UNSIGNED_INT,
    GL_UNSIGNED_INT,
    GL_FLOAT,
    GL_FLOAT,
    GL_FLOAT,
    0
  );

type
  TAutoSetMethodRec = record
    Name: string;
    UniformType: TgxSLDataType;
    SamplerType: TgxSLSamplerType;
    Method: TUniformAutoSetMethod;
  end;

var
  vMethods: array of TAutoSetMethodRec;

function GLXLTypeEnum(AType: TgxSLDataType): GLEnum;
begin
  Result := cGLXLTypeEnum[AType];
end;

function GLXLTypeComponentCount(AType: TgxSLDataType): Integer;
begin
  Result := cGLXLTypeComponents[AType];
end;

procedure RegisterUniformAutoSetMethod(AMethodName: string;
  AType: TgxSLDataType; AMethod: TUniformAutoSetMethod);
var
  I: Integer;
begin
  for I := 0 to High(vMethods) do
    if vMethods[I].Name = AMethodName then
    begin
      vMethods[I].UniformType := AType;
      vMethods[I].Method := AMethod;
      exit;
    end;
  I := Length(vMethods);
  SetLength(vMethods, I+1);
  vMethods[I].Name := AMethodName;
  vMethods[I].UniformType := AType;
  vMethods[I].SamplerType := GLSLSamplerUndefined;
  vMethods[I].Method := AMethod;
end;

procedure FillUniformAutoSetMethodList(AList: TStrings; TypeFilter: TgxSLDataType);
var
  I: Integer;
begin
  for I := 0 to High(vMethods) do
    if vMethods[I].UniformType = TypeFilter then
      AList.Add(vMethods[I].Name);
end;

procedure FillUniformAutoSetMethodList(AList: TStrings; TypeFilter: TgxSLSamplerType);
var
  I: Integer;
begin
  for I := 0 to High(vMethods) do
    if vMethods[I].SamplerType = TypeFilter then
      AList.Add(vMethods[I].Name);
end;

function GetUniformAutoSetMethod(AMethodName: string): TUniformAutoSetMethod;
var
  I: Integer;
begin
  for I := 0 to High(vMethods) do
    if vMethods[I].Name = AMethodName then
    begin
      Result := vMethods[I].Method;
      exit;
    end;
  Result := nil;
end;

function GetUniformAutoSetMethodName(AMethod: TUniformAutoSetMethod): string;
var
  I: Integer;
begin
  for I := 0 to High(vMethods) do
    if @vMethods[I].Method = @AMethod then
    begin
      Result := vMethods[I].Name;
      exit;
    end;
  Result := '';
end;

end.
