// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Cg.Shader.pas' rev: 35.00 (Windows)

#ifndef Cg_ShaderHPP
#define Cg_ShaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Strings.hpp>
#include <GLS.Cadencer.hpp>
#include <GLS.Context.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.Material.hpp>
#include <GLS.TextureFormat.hpp>
#include <Cg.Import.hpp>
#include <Cg.GL.hpp>

//-- user supplied -----------------------------------------------------------

namespace Cg
{
namespace Shader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS ECgShaderException;
class DELPHICLASS TCgProgram;
class DELPHICLASS TCgParameter;
class DELPHICLASS TCgVertexProgram;
class DELPHICLASS TCgFragmentProgram;
class DELPHICLASS TCustomCgShader;
class DELPHICLASS TCadencableCustomCgShader;
class DELPHICLASS TCgShader;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION ECgShaderException : public Gls::Texture::EGLShaderException
{
	typedef Gls::Texture::EGLShaderException inherited;
	
public:
	/* Exception.Create */ inline __fastcall ECgShaderException(const System::UnicodeString Msg) : Gls::Texture::EGLShaderException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ECgShaderException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : Gls::Texture::EGLShaderException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ECgShaderException(NativeUInt Ident)/* overload */ : Gls::Texture::EGLShaderException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ECgShaderException(System::PResStringRec ResStringRec)/* overload */ : Gls::Texture::EGLShaderException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ECgShaderException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : Gls::Texture::EGLShaderException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall ECgShaderException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : Gls::Texture::EGLShaderException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ECgShaderException(const System::UnicodeString Msg, int AHelpContext) : Gls::Texture::EGLShaderException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ECgShaderException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : Gls::Texture::EGLShaderException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ECgShaderException(NativeUInt Ident, int AHelpContext)/* overload */ : Gls::Texture::EGLShaderException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ECgShaderException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Gls::Texture::EGLShaderException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ECgShaderException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Gls::Texture::EGLShaderException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ECgShaderException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Gls::Texture::EGLShaderException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ECgShaderException() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TCgApplyEvent)(TCgProgram* CgProgram, System::TObject* Sender);

typedef void __fastcall (__closure *TCgUnApplyEvent)(TCgProgram* CgProgram);

typedef void __fastcall (__closure *TCgShaderEvent)(TCustomCgShader* CgShader);

enum DECLSPEC_DENUM TcgProgramType : unsigned char { ptVertex, ptFragment };

enum DECLSPEC_DENUM TCgVPProfile : unsigned char { vpDetectLatest, vp20, vp30, vp40, arbvp1 };

enum DECLSPEC_DENUM TCgFPProfile : unsigned char { fpDetectLatest, fp20, fp30, fp40, arbfp1 };

enum DECLSPEC_DENUM TPrecisionSetting : unsigned char { psFull, psFast };

class PASCALIMPLEMENTATION TCgProgram : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	Cg::Import::_CGcontext *FCgContext;
	System::Classes::TStrings* FCode;
	System::UnicodeString FProgramName;
	Cg::Import::_CGprogram *FHandle;
	System::Classes::TList* FParams;
	TCgApplyEvent FOnApply;
	TCgUnApplyEvent FOnUnApply;
	System::Classes::TNotifyEvent FOnProgramChanged;
	bool FEnabled;
	bool FDetectProfile;
	TPrecisionSetting FPrecision;
	void __fastcall SetPrecision(const TPrecisionSetting Value);
	bool __fastcall GetManualNotification();
	void __fastcall SetManualNotification(const bool Value);
	
protected:
	TcgProgramType FProgramType;
	Cg::Import::TCGprofile FProfile;
	void __fastcall SetCode(System::Classes::TStrings* const val);
	void __fastcall SetProgramName(const System::UnicodeString val);
	TCgParameter* __fastcall GetParam(System::UnicodeString index);
	void __fastcall AddParamsItem(const Cg::Import::PCGparameter Param);
	void __fastcall BuildParamsList();
	void __fastcall ClearParamsList();
	
public:
	__fastcall virtual TCgProgram(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TCgProgram();
	virtual Cg::Import::TCGprofile __fastcall GetLatestProfile() = 0 ;
	virtual void __fastcall Initialize();
	void __fastcall Finalize();
	void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	void __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	TCgParameter* __fastcall ParamByName(const System::UnicodeString name);
	__property TCgParameter* Param[System::UnicodeString index] = {read=GetParam};
	__property System::Classes::TList* Params = {read=FParams};
	Cg::Import::PCGparameter __fastcall DirectParamByName(const System::UnicodeString name);
	int __fastcall ParamCount();
	System::UnicodeString __fastcall GetProfileStringA();
	void __fastcall LoadFromFile(const System::UnicodeString fileName);
	void __fastcall ListCompilation(System::Classes::TStrings* Output);
	void __fastcall ListParameters(System::Classes::TStrings* Output);
	void __fastcall SetParam(System::UnicodeString ParamName, float SingleVal)/* overload */;
	void __fastcall SetParam(System::UnicodeString ParamName, const Gls::Vectortypes::TVector2f &Vector2fVal)/* overload */;
	void __fastcall SetParam(System::UnicodeString ParamName, const Gls::Vectortypes::TVector3f &Vector3fVal)/* overload */;
	void __fastcall SetParam(System::UnicodeString ParamName, const Gls::Vectortypes::TVector4f &Vector4fVal)/* overload */;
	void __fastcall SetStateMatrix(System::UnicodeString ParamName, unsigned matrix, unsigned Transform);
	void __fastcall SetTexture(System::UnicodeString ParamName, unsigned TextureID);
	System::UnicodeString __fastcall LongName();
	__property Cg::Import::TCGprofile DirectProfile = {read=FProfile, write=FProfile, nodefault};
	__property System::Classes::TNotifyEvent OnProgramChanged = {read=FOnProgramChanged, write=FOnProgramChanged};
	__property bool ManualNotification = {read=GetManualNotification, write=SetManualNotification, default=0};
	
__published:
	__property System::Classes::TStrings* Code = {read=FCode, write=SetCode};
	__property System::UnicodeString ProgramName = {read=FProgramName, write=SetProgramName};
	__property bool Enabled = {read=FEnabled, write=FEnabled, default=1};
	__property TPrecisionSetting Precision = {read=FPrecision, write=SetPrecision, default=0};
	__property TCgApplyEvent OnApply = {read=FOnApply, write=FOnApply};
	__property TCgUnApplyEvent OnUnApply = {read=FOnUnApply, write=FOnUnApply};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TCgParameter : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TCgProgram* FOwner;
	System::UnicodeString FName;
	Cg::Import::_CGparameter *FHandle;
	Cg::Import::TCGtype FValueType;
	Cg::Import::TCGenum FDirection;
	Cg::Import::TCGenum FVariability;
	
protected:
	System::UnicodeString __fastcall TypeMismatchMessage();
	void __fastcall CheckValueType(Cg::Import::TCGtype aType)/* overload */;
	void __fastcall CheckValueType(const Cg::Import::TCGtype *types, const int types_High)/* overload */;
	void __fastcall CheckAllTextureTypes();
	void __fastcall CheckAllScalarTypes();
	void __fastcall CheckAllVector2fTypes();
	void __fastcall CheckAllVector3fTypes();
	void __fastcall CheckAllVector4fTypes();
	void __fastcall SetAsVector2f(const Gls::Vectortypes::TVector2f &val);
	void __fastcall SetAsVector3f(const Gls::Vectortypes::TVector3f &val);
	void __fastcall SetAsVector4f(const Gls::Vectortypes::TVector4f &val);
	
public:
	__fastcall virtual TCgParameter();
	__fastcall virtual ~TCgParameter();
	void __fastcall SetAsScalar(const float val)/* overload */;
	void __fastcall SetAsScalar(const bool val)/* overload */;
	void __fastcall SetAsVector(const Gls::Vectortypes::TVector2f &val)/* overload */;
	void __fastcall SetAsVector(const Gls::Vectortypes::TVector3f &val)/* overload */;
	void __fastcall SetAsVector(const Gls::Vectortypes::TVector4f &val)/* overload */;
	void __fastcall SetAsVector(const float *val, const int val_High)/* overload */;
	void __fastcall SetAsStateMatrix(unsigned matrix, unsigned Transform);
	void __fastcall SetAsMatrix(const Gls::Vectortypes::TMatrix4f &val);
	void __fastcall SetAsTexture(unsigned TextureID);
	void __fastcall SetAsTexture1D(unsigned TextureID);
	void __fastcall SetAsTexture2D(unsigned TextureID);
	void __fastcall SetAsTexture3D(unsigned TextureID);
	void __fastcall SetAsTextureCUBE(unsigned TextureID);
	void __fastcall SetAsTextureRECT(unsigned TextureID);
	void __fastcall SetToTextureOf(Gls::Material::TGLLibMaterial* LibMaterial);
	void __fastcall EnableTexture();
	void __fastcall DisableTexture();
	void __fastcall SetParameterPointer(Gls::Vectorlists::TGLVectorList* Values)/* overload */;
	void __fastcall SetParameterPointer(Gls::Vectorlists::TGLAffineVectorList* Values)/* overload */;
	void __fastcall EnableClientState();
	void __fastcall DisableClientState();
	System::UnicodeString __fastcall LongName();
	__property TCgProgram* Owner = {read=FOwner};
	__property System::UnicodeString Name = {read=FName};
	__property Cg::Import::TCGtype ValueType = {read=FValueType, nodefault};
	__property Cg::Import::PCGparameter Handle = {read=FHandle, write=FHandle};
	__property Cg::Import::TCGenum Direction = {read=FDirection, write=FDirection, nodefault};
	__property Cg::Import::TCGenum Variability = {read=FVariability, write=FVariability, nodefault};
	__property Gls::Vectortypes::TVector4f AsVector = {write=SetAsVector4f};
	__property Gls::Vectortypes::TVector3f AsAffineVector = {write=SetAsVector3f};
	__property Gls::Vectortypes::TVector2f AsVector2f = {write=SetAsVector2f};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCgVertexProgram : public TCgProgram
{
	typedef TCgProgram inherited;
	
private:
	TCgVPProfile FVPProfile;
	void __fastcall SetVPProfile(TCgVPProfile v);
	
public:
	__fastcall virtual TCgVertexProgram(System::Classes::TPersistent* AOwner);
	virtual Cg::Import::TCGprofile __fastcall GetLatestProfile();
	
__published:
	__property TCgVPProfile Profile = {read=FVPProfile, write=SetVPProfile, default=0};
public:
	/* TCgProgram.Destroy */ inline __fastcall virtual ~TCgVertexProgram() { }
	
};


class PASCALIMPLEMENTATION TCgFragmentProgram : public TCgProgram
{
	typedef TCgProgram inherited;
	
private:
	TCgFPProfile FFPProfile;
	bool FManageTexture;
	void __fastcall SetFPProfile(TCgFPProfile v);
	void __fastcall SetManageTexture(const bool Value);
	
public:
	__fastcall virtual TCgFragmentProgram(System::Classes::TPersistent* AOwner);
	virtual void __fastcall Initialize();
	virtual Cg::Import::TCGprofile __fastcall GetLatestProfile();
	
__published:
	__property TCgFPProfile Profile = {read=FFPProfile, write=SetFPProfile, default=0};
	__property bool ManageTexture = {read=FManageTexture, write=SetManageTexture, default=0};
public:
	/* TCgProgram.Destroy */ inline __fastcall virtual ~TCgFragmentProgram() { }
	
};


class PASCALIMPLEMENTATION TCustomCgShader : public Gls::Material::TGLShader
{
	typedef Gls::Material::TGLShader inherited;
	
private:
	TCgVertexProgram* FVertexProgram;
	TCgFragmentProgram* FFragmentProgram;
	TCgShaderEvent FOnInitialize;
	bool FDesignEnable;
	
protected:
	void __fastcall SetVertexProgram(TCgVertexProgram* const val);
	void __fastcall SetOnApplyVertexProgram(const TCgApplyEvent val);
	TCgApplyEvent __fastcall GetOnApplyVertexProgram();
	void __fastcall SetOnUnApplyVertexProgram(const TCgUnApplyEvent val);
	TCgUnApplyEvent __fastcall GetOnUnApplyVertexProgram();
	void __fastcall SetFragmentProgram(TCgFragmentProgram* const val);
	void __fastcall SetOnApplyFragmentProgram(const TCgApplyEvent val);
	TCgApplyEvent __fastcall GetOnApplyFragmentProgram();
	void __fastcall SetOnUnApplyFragmentProgram(const TCgUnApplyEvent val);
	TCgUnApplyEvent __fastcall GetOnUnApplyFragmentProgram();
	TCgShaderEvent __fastcall GetOnInitialize();
	void __fastcall SetOnInitialize(const TCgShaderEvent val);
	virtual void __fastcall DoInitialize(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoFinalize();
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	bool __fastcall IsProfileSupported(Cg::Import::TCGprofile Profile);
	__property TCgApplyEvent OnApplyVP = {read=GetOnApplyVertexProgram, write=SetOnApplyVertexProgram};
	__property TCgApplyEvent OnApplyFP = {read=GetOnApplyFragmentProgram, write=SetOnApplyFragmentProgram};
	__property TCgUnApplyEvent OnUnApplyVP = {read=GetOnUnApplyVertexProgram, write=SetOnUnApplyVertexProgram};
	__property TCgUnApplyEvent OnUnApplyFP = {read=GetOnUnApplyFragmentProgram, write=SetOnUnApplyFragmentProgram};
	__property TCgShaderEvent OnInitialize = {read=GetOnInitialize, write=SetOnInitialize};
	__property bool DesignEnable = {read=FDesignEnable, write=FDesignEnable, default=0};
	__property TCgVertexProgram* VertexProgram = {read=FVertexProgram, write=SetVertexProgram};
	__property TCgFragmentProgram* FragmentProgram = {read=FFragmentProgram, write=SetFragmentProgram};
	
public:
	__fastcall virtual TCustomCgShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomCgShader();
	void __fastcall LoadShaderPrograms(const System::UnicodeString VPFilename, const System::UnicodeString FPFilename);
	virtual bool __fastcall ShaderSupported();
};


class PASCALIMPLEMENTATION TCadencableCustomCgShader : public TCustomCgShader
{
	typedef TCustomCgShader inherited;
	
private:
	Gls::Cadencer::TGLCadencer* FCadencer;
	void __fastcall SetCadencer(Gls::Cadencer::TGLCadencer* const Value);
	
protected:
	virtual void __fastcall DoInitialize(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__property Gls::Cadencer::TGLCadencer* Cadencer = {read=FCadencer, write=SetCadencer};
public:
	/* TCustomCgShader.Create */ inline __fastcall virtual TCadencableCustomCgShader(System::Classes::TComponent* AOwner) : TCustomCgShader(AOwner) { }
	/* TCustomCgShader.Destroy */ inline __fastcall virtual ~TCadencableCustomCgShader() { }
	
};


class PASCALIMPLEMENTATION TCgShader : public TCustomCgShader
{
	typedef TCustomCgShader inherited;
	
__published:
	__property DesignEnable = {default=0};
	__property ShaderStyle = {default=1};
	__property FailedInitAction = {default=1};
	__property VertexProgram;
	__property FragmentProgram;
	__property OnApplyVP;
	__property OnApplyFP;
	__property OnUnApplyVP;
	__property OnUnApplyFP;
	__property OnInitialize;
public:
	/* TCustomCgShader.Create */ inline __fastcall virtual TCgShader(System::Classes::TComponent* AOwner) : TCustomCgShader(AOwner) { }
	/* TCustomCgShader.Destroy */ inline __fastcall virtual ~TCgShader() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::UnicodeString IncludeFilePath;
extern DELPHI_PACKAGE bool __fastcall IsCgProfileSupported(Cg::Import::TCGprofile Profile);
}	/* namespace Shader */
}	/* namespace Cg */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CG_SHADER)
using namespace Cg::Shader;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CG)
using namespace Cg;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Cg_ShaderHPP
