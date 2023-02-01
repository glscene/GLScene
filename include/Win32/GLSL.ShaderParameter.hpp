// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSL.ShaderParameter.pas' rev: 35.00 (Windows)

#ifndef Glsl_ShaderparameterHPP
#define Glsl_ShaderparameterHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.OpenGLext.hpp>
#include <System.Classes.hpp>
#include <GLS.Strings.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.RenderContextInfo.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsl
{
namespace Shaderparameter
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE IShaderParameter;
typedef System::DelphiInterface<IShaderParameter> _di_IShaderParameter;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLSLDataType : unsigned char { GLSLTypeUndefined, GLSLType1F, GLSLType2F, GLSLType3F, GLSLType4F, GLSLType1I, GLSLType2I, GLSLType3I, GLSLType4I, GLSLType1UI, GLSLType2UI, GLSLType3UI, GLSLType4UI, GLSLTypeMat2F, GLSLTypeMat3F, GLSLTypeMat4F, GLSLTypeVoid };

enum DECLSPEC_DENUM TGLSLSamplerType : unsigned char { GLSLSamplerUndefined, GLSLSampler1D, GLSLSampler2D, GLSLSampler3D, GLSLSamplerCube, GLSLSampler1DShadow, GLSLSampler2DShadow, GLSLSampler1DArray, GLSLSampler2DArray, GLSLSampler1DArrayShadow, GLSLSampler2DArrayShadow, GLSLSamplerCubeShadow, GLSLIntSampler1D, GLSLIntSampler2D, GLSLIntSampler3D, GLSLIntSamplerCube, GLSLIntSampler1DArray, GLSLIntSampler2DArray, GLSLUIntSampler1D, GLSLUIntSampler2D, GLSLUIntSampler3D, GLSLUIntSamplerCube, GLSLUIntSampler1DArray, GLSLUIntSampler2DArray, GLSLSamplerRect, GLSLSamplerRectShadow, GLSLSamplerBuffer, GLSLIntSamplerRect, GLSLIntSamplerBuffer, GLSLUIntSamplerRect, GLSLUIntSamplerBuffer, GLSLSamplerMS, GLSLIntSamplerMS, GLSLUIntSamplerMS, GLSLSamplerMSArray, 
	GLSLIntSamplerMSArray, GLSLUIntSamplerMSArray };

enum DECLSPEC_DENUM TGLgsInTypes : unsigned char { gsInPoints, gsInLines, gsInAdjLines, gsInTriangles, gsInAdjTriangles };

enum DECLSPEC_DENUM TGLgsOutTypes : unsigned char { gsOutPoints, gsOutLineStrip, sOutTriangleStrip };

__interface IShaderParameter  : public System::IInterface 
{
	virtual System::UnicodeString __fastcall GetName() = 0 ;
	virtual TGLSLDataType __fastcall GetGLSLType() = 0 ;
	virtual TGLSLSamplerType __fastcall GetGLSLSamplerType() = 0 ;
	virtual System::UnicodeString __fastcall GetAutoSetMethod() = 0 ;
	virtual System::UnicodeString __fastcall GetTextureName() = 0 ;
	virtual System::UnicodeString __fastcall GetSamplerName() = 0 ;
	virtual Gls::Textureformat::TSwizzleVector __fastcall GetTextureSwizzle() = 0 ;
	virtual void __fastcall SetTextureName(const System::UnicodeString AValue) = 0 ;
	virtual void __fastcall SetSamplerName(const System::UnicodeString AValue) = 0 ;
	virtual void __fastcall SetAutoSetMethod(const System::UnicodeString AValue) = 0 ;
	virtual void __fastcall SetTextureSwizzle(const Gls::Textureformat::TSwizzleVector AValue) = 0 ;
	virtual float __fastcall GetFloat() = 0 ;
	virtual Gls::Vectortypes::TVector2f __fastcall GetVec2() = 0 ;
	virtual Gls::Vectortypes::TVector3f __fastcall GetVec3() = 0 ;
	virtual Gls::Vectortypes::TVector4f __fastcall GetVec4() = 0 ;
	virtual int __fastcall GetInt() = 0 ;
	virtual Gls::Vectortypes::TVector2i __fastcall GetIVec2() = 0 ;
	virtual Gls::Vectortypes::TVector3i __fastcall GetIVec3() = 0 ;
	virtual Gls::Vectortypes::TVector4i __fastcall GetIVec4() = 0 ;
	virtual unsigned __fastcall GetUInt() = 0 ;
	virtual Gls::Vectortypes::TVector2ui __fastcall GetUVec2() = 0 ;
	virtual Gls::Vectortypes::TVector3ui __fastcall GetUVec3() = 0 ;
	virtual Gls::Vectortypes::TVector4ui __fastcall GetUVec4() = 0 ;
	virtual void __fastcall SetFloat(const float Value) = 0 ;
	virtual void __fastcall SetVec2(const Gls::Vectortypes::TVector2f &Value) = 0 ;
	virtual void __fastcall SetVec3(const Gls::Vectortypes::TVector3f &Value) = 0 ;
	virtual void __fastcall SetVec4(const Gls::Vectortypes::TVector4f &Value) = 0 ;
	virtual void __fastcall SetInt(const int Value) = 0 ;
	virtual void __fastcall SetIVec2(const Gls::Vectortypes::TVector2i &Value) = 0 ;
	virtual void __fastcall SetIVec3(const Gls::Vectortypes::TVector3i &Value) = 0 ;
	virtual void __fastcall SetIVec4(const Gls::Vectortypes::TVector4i &Value) = 0 ;
	virtual void __fastcall SetUInt(const unsigned Value) = 0 ;
	virtual void __fastcall SetUVec2(const Gls::Vectortypes::TVector2ui &Value) = 0 ;
	virtual void __fastcall SetUVec3(const Gls::Vectortypes::TVector3ui &Value) = 0 ;
	virtual void __fastcall SetUVec4(const Gls::Vectortypes::TVector4ui &Value) = 0 ;
	virtual Gls::Vectortypes::TMatrix2f __fastcall GetMat2() = 0 ;
	virtual Gls::Vectortypes::TMatrix3f __fastcall GetMat3() = 0 ;
	virtual Gls::Vectortypes::TMatrix4f __fastcall GetMat4() = 0 ;
	virtual void __fastcall SetMat2(const Gls::Vectortypes::TMatrix2f &Value) = 0 ;
	virtual void __fastcall SetMat3(const Gls::Vectortypes::TMatrix3f &Value) = 0 ;
	virtual void __fastcall SetMat4(const Gls::Vectortypes::TMatrix4f &Value) = 0 ;
	virtual void __fastcall SetFloatArray(const System::PSingle Values, int Count) = 0 ;
	virtual void __fastcall SetIntArray(const System::PInteger Values, int Count) = 0 ;
	virtual void __fastcall SetUIntArray(const System::PCardinal Values, int Count) = 0 ;
	__property System::UnicodeString Name = {read=GetName};
	__property TGLSLDataType GLSLType = {read=GetGLSLType};
	__property TGLSLSamplerType GLSLSamplerType = {read=GetGLSLSamplerType};
	__property float Float = {read=GetFloat, write=SetFloat};
	__property int Int = {read=GetInt, write=SetInt};
	__property unsigned uint = {read=GetUInt, write=SetUInt};
	__property Gls::Vectortypes::TVector2f vec2 = {read=GetVec2, write=SetVec2};
	__property Gls::Vectortypes::TVector3f vec3 = {read=GetVec3, write=SetVec3};
	__property Gls::Vectortypes::TVector4f vec4 = {read=GetVec4, write=SetVec4};
	__property Gls::Vectortypes::TVector2i ivec2 = {read=GetIVec2, write=SetIVec2};
	__property Gls::Vectortypes::TVector3i ivec3 = {read=GetIVec3, write=SetIVec3};
	__property Gls::Vectortypes::TVector4i ivec4 = {read=GetIVec4, write=SetIVec4};
	__property Gls::Vectortypes::TVector2ui uvec2 = {read=GetUVec2, write=SetUVec2};
	__property Gls::Vectortypes::TVector3ui uvec3 = {read=GetUVec3, write=SetUVec3};
	__property Gls::Vectortypes::TVector4ui uvec4 = {read=GetUVec4, write=SetUVec4};
	__property Gls::Vectortypes::TMatrix2f mat2 = {read=GetMat2, write=SetMat2};
	__property Gls::Vectortypes::TMatrix3f mat3 = {read=GetMat3, write=SetMat3};
	__property Gls::Vectortypes::TMatrix4f mat4 = {read=GetMat4, write=SetMat4};
	__property System::UnicodeString AutoSetMethod = {read=GetAutoSetMethod, write=SetAutoSetMethod};
	__property System::UnicodeString TextureName = {read=GetTextureName, write=SetTextureName};
	__property System::UnicodeString SamplerName = {read=GetSamplerName, write=SetSamplerName};
	__property Gls::Textureformat::TSwizzleVector TextureSwizzle = {read=GetTextureSwizzle, write=SetTextureSwizzle};
};

typedef System::StaticArray<System::AnsiString, 17> Glsl_Shaderparameter__1;

typedef System::StaticArray<System::AnsiString, 37> Glsl_Shaderparameter__2;

typedef void __fastcall (__closure *TUniformAutoSetMethod)(_di_IShaderParameter Sender, Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Glsl_Shaderparameter__1 cGLSLTypeString;
extern DELPHI_PACKAGE Glsl_Shaderparameter__2 cGLSLSamplerString;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 5> cGLgsInTypes;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 3> cGLgsOutTypes;
extern DELPHI_PACKAGE unsigned __fastcall GLSLTypeEnum(TGLSLDataType AType);
extern DELPHI_PACKAGE int __fastcall GLSLTypeComponentCount(TGLSLDataType AType);
extern DELPHI_PACKAGE void __fastcall RegisterUniformAutoSetMethod(System::UnicodeString AMethodName, TGLSLDataType AType, TUniformAutoSetMethod AMethod);
extern DELPHI_PACKAGE void __fastcall FillUniformAutoSetMethodList(System::Classes::TStrings* AList, TGLSLDataType TypeFilter)/* overload */;
extern DELPHI_PACKAGE void __fastcall FillUniformAutoSetMethodList(System::Classes::TStrings* AList, TGLSLSamplerType TypeFilter)/* overload */;
extern DELPHI_PACKAGE TUniformAutoSetMethod __fastcall GetUniformAutoSetMethod(System::UnicodeString AMethodName);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetUniformAutoSetMethodName(TUniformAutoSetMethod AMethod);
}	/* namespace Shaderparameter */
}	/* namespace Glsl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL_SHADERPARAMETER)
using namespace Glsl::Shaderparameter;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL)
using namespace Glsl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glsl_ShaderparameterHPP
