// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.ProxyObjects.pas' rev: 35.00 (Windows)

#ifndef Gls_ProxyobjectsHPP
#define Gls_ProxyobjectsHPP

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
#include <GLS.Scene.hpp>
#include <GLS.XCollection.hpp>
#include <GLS.PipelineTransformation.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Texture.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.Strings.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.Material.hpp>
#include <GLS.Context.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.VectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Proxyobjects
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EGLProxyException;
class DELPHICLASS TGLColorProxy;
class DELPHICLASS TGLMaterialProxy;
class DELPHICLASS TGLFreeFormProxy;
class DELPHICLASS TBoneMatrixObj;
class DELPHICLASS TGLActorProxy;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLProxyException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLProxyException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLProxyException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLProxyException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLProxyException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLProxyException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLProxyException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLProxyException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLProxyException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLProxyException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLProxyException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLProxyException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLProxyException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLProxyException() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLColorProxy : public Gls::Scene::TGLProxyObject
{
	typedef Gls::Scene::TGLProxyObject inherited;
	
private:
	Gls::Material::TGLFaceProperties* FFrontColor;
	Gls::Scene::TGLCustomSceneObject* __fastcall GetMasterMaterialObject();
	void __fastcall SetMasterMaterialObject(Gls::Scene::TGLCustomSceneObject* const Value);
	void __fastcall SetFrontColor(Gls::Material::TGLFaceProperties* AValue);
	
public:
	__fastcall virtual TGLColorProxy(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLColorProxy();
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	
__published:
	__property Gls::Material::TGLFaceProperties* FrontColor = {read=FFrontColor, write=SetFrontColor};
	__property Gls::Scene::TGLCustomSceneObject* MasterObject = {read=GetMasterMaterialObject, write=SetMasterMaterialObject};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLColorProxy(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLProxyObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLMaterialProxy : public Gls::Scene::TGLProxyObject
{
	typedef Gls::Scene::TGLProxyObject inherited;
	
private:
	System::UnicodeString FTempLibMaterialName;
	Gls::Material::TGLLibMaterial* FMasterLibMaterial;
	Gls::Material::TGLMaterialLibrary* FMaterialLibrary;
	void __fastcall SetMaterialLibrary(Gls::Material::TGLMaterialLibrary* const Value);
	System::UnicodeString __fastcall GetMasterLibMaterialName();
	void __fastcall SetMasterLibMaterialName(const System::UnicodeString Value);
	Gls::Scene::TGLCustomSceneObject* __fastcall GetMasterMaterialObject();
	void __fastcall SetMasterMaterialObject(Gls::Scene::TGLCustomSceneObject* const Value);
	Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	
public:
	__fastcall virtual TGLMaterialProxy(System::Classes::TComponent* AOwner);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	__fastcall virtual ~TGLMaterialProxy();
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	__property Gls::Material::TGLLibMaterial* MasterLibMaterial = {read=FMasterLibMaterial, write=FMasterLibMaterial, stored=false};
	
__published:
	__property Gls::Material::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property System::UnicodeString MasterLibMaterialName = {read=GetMasterLibMaterialName, write=SetMasterLibMaterialName};
	__property Gls::Scene::TGLCustomSceneObject* MasterObject = {read=GetMasterMaterialObject, write=SetMasterMaterialObject};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLMaterialProxy(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLProxyObject(aParentOwner) { }
	
private:
	void *__IGLMaterialLibrarySupported;	// Gls::Material::IGLMaterialLibrarySupported 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator Gls::Material::_di_IGLMaterialLibrarySupported()
	{
		Gls::Material::_di_IGLMaterialLibrarySupported intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Gls::Material::IGLMaterialLibrarySupported*(void) { return (Gls::Material::IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLFreeFormProxy : public Gls::Scene::TGLProxyObject
{
	typedef Gls::Scene::TGLProxyObject inherited;
	
private:
	Gls::Vectorfileobjects::TGLFreeForm* __fastcall GetMasterFreeFormObject();
	void __fastcall SetMasterFreeFormObject(Gls::Vectorfileobjects::TGLFreeForm* const Value);
	
public:
	bool __fastcall OctreeRayCastIntersect(const Gls::Vectortypes::TVector4f &rayStart, const Gls::Vectortypes::TVector4f &rayVector, Gls::Vectortypes::PGLVector intersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector intersectNormal = (Gls::Vectortypes::PGLVector)(0x0));
	bool __fastcall OctreeSphereSweepIntersect(const Gls::Vectortypes::TVector4f &rayStart, const Gls::Vectortypes::TVector4f &rayVector, const float velocity, const float radius, const float modelscale, Gls::Vectortypes::PGLVector intersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector intersectNormal = (Gls::Vectortypes::PGLVector)(0x0));
	
__published:
	__property Gls::Vectorfileobjects::TGLFreeForm* MasterObject = {read=GetMasterFreeFormObject, write=SetMasterFreeFormObject};
public:
	/* TGLProxyObject.Create */ inline __fastcall virtual TGLFreeFormProxy(System::Classes::TComponent* AOwner) : Gls::Scene::TGLProxyObject(AOwner) { }
	/* TGLProxyObject.Destroy */ inline __fastcall virtual ~TGLFreeFormProxy() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFreeFormProxy(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLProxyObject(aParentOwner) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TBoneMatrixObj : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	Gls::Vectortypes::TMatrix4f Matrix;
	System::UnicodeString BoneName;
	int BoneIndex;
public:
	/* TObject.Create */ inline __fastcall TBoneMatrixObj() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TBoneMatrixObj() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TGLActorProxyAnimationMode : unsigned char { pamInherited, pamNone, pamPlayOnce };

class PASCALIMPLEMENTATION TGLActorProxy : public Gls::Scene::TGLProxyObject
{
	typedef Gls::Scene::TGLProxyObject inherited;
	
private:
	int FCurrentFrame;
	int FStartFrame;
	int FEndFrame;
	int FLastFrame;
	float FCurrentFrameDelta;
	Gls::Baseclasses::TGLProgressTimes FCurrentTime;
	System::UnicodeString FAnimation;
	System::UnicodeString FTempLibMaterialName;
	Gls::Material::TGLLibMaterial* FMasterLibMaterial;
	Gls::Material::TGLMaterialLibrary* FMaterialLibrary;
	System::Classes::TStringList* FBonesMatrices;
	bool FStoreBonesMatrix;
	System::Classes::TStrings* FStoredBoneNames;
	Gls::Baseclasses::TGLProgressEvent FOnBeforeRender;
	TGLActorProxyAnimationMode FAnimationMode;
	void __fastcall SetAnimation(const System::UnicodeString Value);
	void __fastcall SetMasterActorObject(Gls::Vectorfileobjects::TGLActor* const Value);
	Gls::Vectorfileobjects::TGLActor* __fastcall GetMasterActorObject();
	System::UnicodeString __fastcall GetLibMaterialName();
	void __fastcall SetLibMaterialName(const System::UnicodeString Value);
	void __fastcall SetMaterialLibrary(Gls::Material::TGLMaterialLibrary* const Value);
	Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	void __fastcall SetStoreBonesMatrix(const bool Value);
	void __fastcall SetStoredBoneNames(System::Classes::TStrings* const Value);
	void __fastcall SetOnBeforeRender(const Gls::Baseclasses::TGLProgressEvent Value);
	
protected:
	void __fastcall DoStoreBonesMatrices();
	
public:
	__fastcall virtual TGLActorProxy(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLActorProxy();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	__property int CurrentFrame = {read=FCurrentFrame, nodefault};
	__property int StartFrame = {read=FStartFrame, nodefault};
	__property int EndFrame = {read=FEndFrame, nodefault};
	__property float CurrentFrameDelta = {read=FCurrentFrameDelta};
	__property Gls::Baseclasses::TGLProgressTimes CurrentTime = {read=FCurrentTime};
	Gls::Vectortypes::TMatrix4f __fastcall BoneMatrix(int BoneIndex)/* overload */;
	Gls::Vectortypes::TMatrix4f __fastcall BoneMatrix(System::UnicodeString BoneName)/* overload */;
	void __fastcall BoneMatricesClear();
	virtual bool __fastcall RayCastIntersect(const Gls::Vectortypes::TVector4f &rayStart, const Gls::Vectortypes::TVector4f &rayVector, Gls::Vectortypes::PGLVector intersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector intersectNormal = (Gls::Vectortypes::PGLVector)(0x0));
	bool __fastcall RayCastIntersectEx(Gls::Vectorfileobjects::TGLActor* RefActor, const Gls::Vectortypes::TVector4f &rayStart, const Gls::Vectortypes::TVector4f &rayVector, Gls::Vectortypes::PGLVector intersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector intersectNormal = (Gls::Vectortypes::PGLVector)(0x0))/* overload */;
	
__published:
	__property TGLActorProxyAnimationMode AnimationMode = {read=FAnimationMode, write=FAnimationMode, default=0};
	__property System::UnicodeString Animation = {read=FAnimation, write=SetAnimation};
	__property Gls::Vectorfileobjects::TGLActor* MasterObject = {read=GetMasterActorObject, write=SetMasterActorObject};
	__property ProxyOptions = {default=3};
	__property Gls::Material::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property System::UnicodeString LibMaterialName = {read=GetLibMaterialName, write=SetLibMaterialName};
	__property bool StoreBonesMatrix = {read=FStoreBonesMatrix, write=SetStoreBonesMatrix, nodefault};
	__property System::Classes::TStrings* StoredBoneNames = {read=FStoredBoneNames, write=SetStoredBoneNames};
	__property Gls::Baseclasses::TGLProgressEvent OnBeforeRender = {read=FOnBeforeRender, write=SetOnBeforeRender};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLActorProxy(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLProxyObject(aParentOwner) { }
	
private:
	void *__IGLMaterialLibrarySupported;	// Gls::Material::IGLMaterialLibrarySupported 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator Gls::Material::_di_IGLMaterialLibrarySupported()
	{
		Gls::Material::_di_IGLMaterialLibrarySupported intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Gls::Material::IGLMaterialLibrarySupported*(void) { return (Gls::Material::IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Proxyobjects */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_PROXYOBJECTS)
using namespace Gls::Proxyobjects;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ProxyobjectsHPP
