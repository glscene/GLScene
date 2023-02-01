// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSL.CustomShader.pas' rev: 35.00 (Windows)

#ifndef Glsl_CustomshaderHPP
#define Glsl_CustomshaderHPP

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
#include <System.SysUtils.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Cadencer.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Strings.hpp>
#include <GLS.Context.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.Material.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLSL.ShaderParameter.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsl
{
namespace Customshader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EGLCustomShaderException;
__interface DELPHIINTERFACE IGLShaderDescription;
typedef System::DelphiInterface<IGLShaderDescription> _di_IGLShaderDescription;
__interface DELPHIINTERFACE IGLPostShader;
typedef System::DelphiInterface<IGLPostShader> _di_IGLPostShader;
class DELPHICLASS TGLCustomShader;
class DELPHICLASS TGLShaderProgram;
class DELPHICLASS TGLVertexProgram;
class DELPHICLASS TGLFragmentProgram;
class DELPHICLASS TGLGeometryProgram;
class DELPHICLASS TGLCustomShaderParameter;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLShaderFogSupport : unsigned char { sfsEnabled, sfsDisabled, sfsAuto };

enum DECLSPEC_DENUM TGLTransformFeedBackMode : unsigned char { tfbmInterleaved, tfbmSeparate };

#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLCustomShaderException : public Gls::Texture::EGLShaderException
{
	typedef Gls::Texture::EGLShaderException inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLCustomShaderException(const System::UnicodeString Msg) : Gls::Texture::EGLShaderException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLCustomShaderException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : Gls::Texture::EGLShaderException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLCustomShaderException(NativeUInt Ident)/* overload */ : Gls::Texture::EGLShaderException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLCustomShaderException(System::PResStringRec ResStringRec)/* overload */ : Gls::Texture::EGLShaderException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLCustomShaderException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : Gls::Texture::EGLShaderException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLCustomShaderException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : Gls::Texture::EGLShaderException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLCustomShaderException(const System::UnicodeString Msg, int AHelpContext) : Gls::Texture::EGLShaderException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLCustomShaderException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : Gls::Texture::EGLShaderException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLCustomShaderException(NativeUInt Ident, int AHelpContext)/* overload */ : Gls::Texture::EGLShaderException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLCustomShaderException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Gls::Texture::EGLShaderException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLCustomShaderException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Gls::Texture::EGLShaderException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLCustomShaderException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Gls::Texture::EGLShaderException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLCustomShaderException() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TGLShaderEvent)(TGLCustomShader* Shader);

typedef void __fastcall (__closure *TGLShaderUnAplyEvent)(TGLCustomShader* Shader, bool &ThereAreMorePasses);

typedef System::Int8 TGLLightSourceEnum;

typedef System::Set<TGLLightSourceEnum, 1, 8> TGLLightSourceSet;

__interface  INTERFACE_UUID("{04089C64-60C2-43F5-AC9C-38ED46264812}") IGLShaderDescription  : public System::IInterface 
{
	virtual void __fastcall SetShaderTextures(Gls::Texture::TGLTexture* const *Textures, const int Textures_High) = 0 ;
	virtual void __fastcall GetShaderTextures(Gls::Texture::TGLTexture* *Textures, const int Textures_High) = 0 ;
	virtual void __fastcall SetShaderColorParams(const Gls::Vectortypes::TVector4f &AAmbientColor, const Gls::Vectortypes::TVector4f &ADiffuseColor, const Gls::Vectortypes::TVector4f &ASpecularcolor) = 0 ;
	virtual void __fastcall GetShaderColorParams(Gls::Vectortypes::TVector4f &AAmbientColor, Gls::Vectortypes::TVector4f &ADiffuseColor, Gls::Vectortypes::TVector4f &ASpecularcolor) = 0 ;
	virtual void __fastcall SetShaderMiscParameters(Gls::Cadencer::TGLCadencer* const ACadencer, Gls::Material::TGLMaterialLibrary* const AMatLib, const TGLLightSourceSet ALightSources) = 0 ;
	virtual void __fastcall GetShaderMiscParameters(Gls::Cadencer::TGLCadencer* &ACadencer, Gls::Material::TGLMaterialLibrary* &AMatLib, TGLLightSourceSet &ALightSources) = 0 ;
	virtual float __fastcall GetShaderAlpha() = 0 ;
	virtual void __fastcall SetShaderAlpha(const float Value) = 0 ;
	virtual System::UnicodeString __fastcall GetShaderDescription() = 0 ;
};

__interface  INTERFACE_UUID("{68A62362-AF0A-4CE8-A9E1-714FE02AFA4A}") IGLPostShader  : public System::IInterface 
{
	virtual void __fastcall DoUseTempTexture(Gls::Context::TGLTextureHandle* const TempTexture, Gls::Textureformat::TGLTextureTarget TextureTarget) = 0 ;
	virtual Gls::Textureformat::TGLTextureTarget __fastcall GetTextureTarget() = 0 ;
};

class PASCALIMPLEMENTATION TGLCustomShader : public Gls::Material::TGLShader
{
	typedef Gls::Material::TGLShader inherited;
	
private:
	TGLFragmentProgram* FFragmentProgram;
	TGLVertexProgram* FVertexProgram;
	TGLGeometryProgram* FGeometryProgram;
	System::TObject* FTagObject;
	void __fastcall SetFragmentProgram(TGLFragmentProgram* const Value);
	void __fastcall SetGeometryProgram(TGLGeometryProgram* const Value);
	void __fastcall SetVertexProgram(TGLVertexProgram* const Value);
	bool __fastcall StoreFragmentProgram();
	bool __fastcall StoreGeometryProgram();
	bool __fastcall StoreVertexProgram();
	
protected:
	bool FDebugMode;
	virtual void __fastcall SetDebugMode(const bool Value);
	__property TGLFragmentProgram* FragmentProgram = {read=FFragmentProgram, write=SetFragmentProgram, stored=StoreFragmentProgram};
	__property TGLVertexProgram* VertexProgram = {read=FVertexProgram, write=SetVertexProgram, stored=StoreVertexProgram};
	__property TGLGeometryProgram* GeometryProgram = {read=FGeometryProgram, write=SetGeometryProgram, stored=StoreGeometryProgram};
	__property bool DebugMode = {read=FDebugMode, write=SetDebugMode, default=0};
	__property System::TObject* TagObject = {read=FTagObject, write=FTagObject, default=0};
	
public:
	__fastcall virtual TGLCustomShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomShader();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall LoadShaderPrograms(const System::UnicodeString VPFilename, const System::UnicodeString FPFilename, const System::UnicodeString GPFilename = System::UnicodeString());
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLShaderProgram : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TGLCustomShader* FParent;
	bool FEnabled;
	System::Classes::TStrings* FCode;
	void __fastcall SetCode(System::Classes::TStrings* const Value);
	void __fastcall SetEnabled(const bool Value);
	void __fastcall OnChangeCode(System::TObject* Sender);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	
public:
	void __fastcall LoadFromFile(const System::UnicodeString AFileName);
	virtual void __fastcall Apply();
	__fastcall virtual TGLShaderProgram(TGLCustomShader* const AParent);
	__fastcall virtual ~TGLShaderProgram();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property System::Classes::TStrings* Code = {read=FCode, write=SetCode};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=0};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLVertexProgram : public TGLShaderProgram
{
	typedef TGLShaderProgram inherited;
	
__published:
	__property Code;
	__property Enabled = {default=0};
public:
	/* TGLShaderProgram.Create */ inline __fastcall virtual TGLVertexProgram(TGLCustomShader* const AParent) : TGLShaderProgram(AParent) { }
	/* TGLShaderProgram.Destroy */ inline __fastcall virtual ~TGLVertexProgram() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFragmentProgram : public TGLShaderProgram
{
	typedef TGLShaderProgram inherited;
	
__published:
	__property Code;
	__property Enabled = {default=0};
public:
	/* TGLShaderProgram.Create */ inline __fastcall virtual TGLFragmentProgram(TGLCustomShader* const AParent) : TGLShaderProgram(AParent) { }
	/* TGLShaderProgram.Destroy */ inline __fastcall virtual ~TGLFragmentProgram() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGeometryProgram : public TGLShaderProgram
{
	typedef TGLShaderProgram inherited;
	
private:
	Glsl::Shaderparameter::TGLgsInTypes FInputPrimitiveType;
	Glsl::Shaderparameter::TGLgsOutTypes FOutputPrimitiveType;
	int FVerticesOut;
	void __fastcall SetInputPrimitiveType(const Glsl::Shaderparameter::TGLgsInTypes Value);
	void __fastcall SetOutputPrimitiveType(const Glsl::Shaderparameter::TGLgsOutTypes Value);
	void __fastcall SetVerticesOut(const int Value);
	
public:
	__fastcall virtual TGLGeometryProgram(TGLCustomShader* const AParent);
	
__published:
	__property Code;
	__property Enabled = {default=0};
	__property Glsl::Shaderparameter::TGLgsInTypes InputPrimitiveType = {read=FInputPrimitiveType, write=SetInputPrimitiveType, default=0};
	__property Glsl::Shaderparameter::TGLgsOutTypes OutputPrimitiveType = {read=FOutputPrimitiveType, write=SetOutputPrimitiveType, default=0};
	__property int VerticesOut = {read=FVerticesOut, write=SetVerticesOut, default=0};
public:
	/* TGLShaderProgram.Destroy */ inline __fastcall virtual ~TGLGeometryProgram() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLCustomShaderParameter : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	virtual float __fastcall GetAsVector1f() = 0 ;
	virtual Gls::Vectortypes::TVector2f __fastcall GetAsVector2f() = 0 ;
	virtual Gls::Vectortypes::TVector3f __fastcall GetAsVector3f() = 0 ;
	virtual Gls::Vectortypes::TVector4f __fastcall GetAsVector4f() = 0 ;
	virtual int __fastcall GetAsVector1i() = 0 ;
	virtual Gls::Vectortypes::TVector2i __fastcall GetAsVector2i() = 0 ;
	virtual Gls::Vectortypes::TVector3i __fastcall GetAsVector3i() = 0 ;
	virtual Gls::Vectortypes::TVector4i __fastcall GetAsVector4i() = 0 ;
	virtual unsigned __fastcall GetAsVector1ui() = 0 ;
	virtual Gls::Vectortypes::TVector2ui __fastcall GetAsVector2ui() = 0 ;
	virtual Gls::Vectortypes::TVector3ui __fastcall GetAsVector3ui() = 0 ;
	virtual Gls::Vectortypes::TVector4ui __fastcall GetAsVector4ui() = 0 ;
	virtual void __fastcall SetAsVector1f(const float Value) = 0 ;
	virtual void __fastcall SetAsVector2f(const Gls::Vectortypes::TVector2f &Value) = 0 ;
	virtual void __fastcall SetAsVector3f(const Gls::Vectortypes::TVector3f &Value) = 0 ;
	virtual void __fastcall SetAsVector4f(const Gls::Vectortypes::TVector4f &Value) = 0 ;
	virtual void __fastcall SetAsVector1i(const int Value) = 0 ;
	virtual void __fastcall SetAsVector2i(const Gls::Vectortypes::TVector2i &Value) = 0 ;
	virtual void __fastcall SetAsVector3i(const Gls::Vectortypes::TVector3i &Value) = 0 ;
	virtual void __fastcall SetAsVector4i(const Gls::Vectortypes::TVector4i &Value) = 0 ;
	virtual void __fastcall SetAsVector1ui(const unsigned Value) = 0 ;
	virtual void __fastcall SetAsVector2ui(const Gls::Vectortypes::TVector2ui &Value) = 0 ;
	virtual void __fastcall SetAsVector3ui(const Gls::Vectortypes::TVector3ui &Value) = 0 ;
	virtual void __fastcall SetAsVector4ui(const Gls::Vectortypes::TVector4ui &Value) = 0 ;
	virtual Gls::Vectortypes::TMatrix2f __fastcall GetAsMatrix2f() = 0 ;
	virtual Gls::Vectortypes::TMatrix3f __fastcall GetAsMatrix3f() = 0 ;
	virtual Gls::Vectortypes::TMatrix4f __fastcall GetAsMatrix4f() = 0 ;
	virtual void __fastcall SetAsMatrix2f(const Gls::Vectortypes::TMatrix2f &Value) = 0 ;
	virtual void __fastcall SetAsMatrix3f(const Gls::Vectortypes::TMatrix3f &Value) = 0 ;
	virtual void __fastcall SetAsMatrix4f(const Gls::Vectortypes::TMatrix4f &Value) = 0 ;
	void __fastcall SetAsTexture(const int TextureIndex, Gls::Texture::TGLTexture* const Value);
	void __fastcall SetAsTexture1D(const int TextureIndex, Gls::Texture::TGLTexture* const Value);
	void __fastcall SetAsTexture2D(const int TextureIndex, Gls::Texture::TGLTexture* const Value);
	void __fastcall SetAsTexture3D(const int TextureIndex, Gls::Texture::TGLTexture* const Value);
	void __fastcall SetAsTextureCube(const int TextureIndex, Gls::Texture::TGLTexture* const Value);
	void __fastcall SetAsTextureRect(const int TextureIndex, Gls::Texture::TGLTexture* const Value);
	virtual unsigned __fastcall GetAsCustomTexture(const int TextureIndex, Gls::Textureformat::TGLTextureTarget TextureTarget) = 0 ;
	virtual void __fastcall SetAsCustomTexture(const int TextureIndex, Gls::Textureformat::TGLTextureTarget TextureTarget, const unsigned Value) = 0 ;
	virtual unsigned __fastcall GetAsUniformBuffer() = 0 ;
	virtual void __fastcall SetAsUniformBuffer(unsigned UBO) = 0 ;
	
public:
	void __fastcall SetAsVectorF(const float *Values, const int Values_High)/* overload */;
	void __fastcall SetAsVectorI(const int *Values, const int Values_High)/* overload */;
	void __fastcall SetToTextureOf(Gls::Material::TGLLibMaterial* const LibMaterial, const int TextureIndex)/* overload */;
	void __fastcall SetToTextureOf(Gls::Texture::TGLTexture* const Texture, const int TextureIndex)/* overload */;
	__property Gls::Vectortypes::TVector4f AsVector = {read=GetAsVector4f, write=SetAsVector4f};
	__property Gls::Vectortypes::TVector3f AsAffineVector = {read=GetAsVector3f, write=SetAsVector3f};
	__property float AsFloat = {read=GetAsVector1f, write=SetAsVector1f};
	__property int AsInteger = {read=GetAsVector1i, write=SetAsVector1i, nodefault};
	__property float AsVector1f = {read=GetAsVector1f, write=SetAsVector1f};
	__property Gls::Vectortypes::TVector2f AsVector2f = {read=GetAsVector2f, write=SetAsVector2f};
	__property Gls::Vectortypes::TVector3f AsVector3f = {read=GetAsVector3f, write=SetAsVector3f};
	__property Gls::Vectortypes::TVector4f AsVector4f = {read=GetAsVector4f, write=SetAsVector4f};
	__property int AsVector1i = {read=GetAsVector1i, write=SetAsVector1i, nodefault};
	__property Gls::Vectortypes::TVector2i AsVector2i = {read=GetAsVector2i, write=SetAsVector2i};
	__property Gls::Vectortypes::TVector3i AsVector3i = {read=GetAsVector3i, write=SetAsVector3i};
	__property Gls::Vectortypes::TVector4i AsVector4i = {read=GetAsVector4i, write=SetAsVector4i};
	__property unsigned AsVector1ui = {read=GetAsVector1ui, write=SetAsVector1ui, nodefault};
	__property Gls::Vectortypes::TVector2ui AsVector2ui = {read=GetAsVector2ui, write=SetAsVector2ui};
	__property Gls::Vectortypes::TVector3ui AsVector3ui = {read=GetAsVector3ui, write=SetAsVector3ui};
	__property Gls::Vectortypes::TVector4ui AsVector4ui = {read=GetAsVector4ui, write=SetAsVector4ui};
	__property Gls::Vectortypes::TMatrix2f AsMatrix2f = {read=GetAsMatrix2f, write=SetAsMatrix2f};
	__property Gls::Vectortypes::TMatrix3f AsMatrix3f = {read=GetAsMatrix3f, write=SetAsMatrix3f};
	__property Gls::Vectortypes::TMatrix4f AsMatrix4f = {read=GetAsMatrix4f, write=SetAsMatrix4f};
	__property Gls::Texture::TGLTexture* AsTexture[const int TextureIndex] = {write=SetAsTexture};
	__property Gls::Texture::TGLTexture* AsTexture1D[const int TextureIndex] = {write=SetAsTexture1D};
	__property Gls::Texture::TGLTexture* AsTexture2D[const int TextureIndex] = {write=SetAsTexture2D};
	__property Gls::Texture::TGLTexture* AsTexture3D[const int TextureIndex] = {write=SetAsTexture3D};
	__property Gls::Texture::TGLTexture* AsTextureRect[const int TextureIndex] = {write=SetAsTextureRect};
	__property Gls::Texture::TGLTexture* AsTextureCube[const int TextureIndex] = {write=SetAsTextureCube};
	__property unsigned AsCustomTexture[const int TextureIndex][Gls::Textureformat::TGLTextureTarget TextureTarget] = {read=GetAsCustomTexture, write=SetAsCustomTexture};
	__property unsigned AsUniformBuffer = {read=GetAsUniformBuffer, write=SetAsUniformBuffer, nodefault};
public:
	/* TObject.Create */ inline __fastcall TGLCustomShaderParameter() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLCustomShaderParameter() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TGLBlendingModeEx : unsigned char { bmxOpaque, bmxTransparency, bmxAdditive, bmxAlphaTest50, bmxAlphaTest100, bmxModulate, bmxDestColorOne, bmxDestAlphaOne };

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 glsShaderMaxLightSources = System::Int8(0x8);
extern DELPHI_PACKAGE void __fastcall GetActiveLightsList(Gls::Vectorlists::TGLIntegerList* const ALightIDs);
extern DELPHI_PACKAGE bool __fastcall IsFogEnabled(const TGLShaderFogSupport AFogSupportMode, Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
extern DELPHI_PACKAGE void __fastcall CopyScreentoTexture(const Gls::Rendercontextinfo::TGLSize &ViewPortSize, const System::Word TextureTarget = (System::Word)(0xde1));
extern DELPHI_PACKAGE void __fastcall CopyScreentoTexture2(const Gls::Rendercontextinfo::TGLSize &ViewPortSize, const System::Word TextureTarget = (System::Word)(0xde1));
extern DELPHI_PACKAGE void __fastcall ApplyBlendingModeEx(const TGLBlendingModeEx BlendingMode);
extern DELPHI_PACKAGE void __fastcall UnApplyBlendingModeEx(void);
extern DELPHI_PACKAGE void __fastcall DrawTexturedScreenQuad(void);
extern DELPHI_PACKAGE void __fastcall DrawTexturedScreenQuad2(const Gls::Rendercontextinfo::TGLSize &ViewPortSize);
extern DELPHI_PACKAGE void __fastcall DrawTexturedScreenQuad4(const Gls::Rendercontextinfo::TGLSize &ViewPortSize);
extern DELPHI_PACKAGE void __fastcall DrawTexturedScreenQuad5(const Gls::Rendercontextinfo::TGLSize &ViewPortSize);
extern DELPHI_PACKAGE void __fastcall DrawTexturedScreenQuad6(const Gls::Rendercontextinfo::TGLSize &ViewPortSize);
extern DELPHI_PACKAGE void __fastcall DrawTexturedScreenQuad3(void);
extern DELPHI_PACKAGE void __fastcall InitTexture(const unsigned TextureHandle, const Gls::Rendercontextinfo::TGLSize &TextureSize, const Gls::Textureformat::TGLTextureTarget TextureTarget = (Gls::Textureformat::TGLTextureTarget)(0x2));
}	/* namespace Customshader */
}	/* namespace Glsl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL_CUSTOMSHADER)
using namespace Glsl::Customshader;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL)
using namespace Glsl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glsl_CustomshaderHPP
