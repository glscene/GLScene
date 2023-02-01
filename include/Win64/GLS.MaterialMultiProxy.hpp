// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.MaterialMultiProxy.pas' rev: 35.00 (Windows)

#ifndef Gls_MaterialmultiproxyHPP
#define Gls_MaterialmultiproxyHPP

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
#include <GLS.Scene.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Material.hpp>
#include <GLS.Silhouette.hpp>
#include <GLS.Strings.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.Context.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.PipelineTransformation.hpp>
#include <GLS.Coordinates.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Materialmultiproxy
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLMaterialMultiProxyMaster;
class DELPHICLASS TGLMaterialMultiProxyMasters;
class DELPHICLASS TGLMaterialMultiProxy;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLMaterialMultiProxyMaster : public Gls::Persistentclasses::TGLInterfacedCollectionItem
{
	typedef Gls::Persistentclasses::TGLInterfacedCollectionItem inherited;
	
private:
	Gls::Scene::TGLBaseSceneObject* FMasterObject;
	Gls::Material::TGLLibMaterial* FMasterLibMaterial;
	System::UnicodeString FTempLibMaterialName;
	float FDistanceMin2;
	float FDistanceMax2;
	void __fastcall SetMasterLibMaterialName(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetMasterLibMaterialName();
	Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	void __fastcall SetMasterObject(Gls::Scene::TGLBaseSceneObject* const Val);
	void __fastcall SetDistanceMin(const float Val);
	void __fastcall SetDistanceMax(const float Val);
	float __fastcall GetDistanceMin();
	float __fastcall GetDistanceMax();
	
public:
	__fastcall virtual TGLMaterialMultiProxyMaster(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLMaterialMultiProxyMaster();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	TGLMaterialMultiProxy* __fastcall OwnerObject();
	void __fastcall NotifyChange();
	__property Gls::Material::TGLLibMaterial* MasterLibMaterial = {read=FMasterLibMaterial, write=FMasterLibMaterial, stored=false};
	
__published:
	__property Gls::Scene::TGLBaseSceneObject* MasterObject = {read=FMasterObject, write=SetMasterObject};
	__property System::UnicodeString MasterLibMaterialName = {read=GetMasterLibMaterialName, write=SetMasterLibMaterialName};
	__property float DistanceMin = {read=GetDistanceMin, write=SetDistanceMin};
	__property float DistanceMax = {read=GetDistanceMax, write=SetDistanceMax};
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


class PASCALIMPLEMENTATION TGLMaterialMultiProxyMasters : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLMaterialMultiProxyMaster* operator[](int index) { return this->Items[index]; }
	
protected:
	void __fastcall SetItems(int index, TGLMaterialMultiProxyMaster* const Val);
	TGLMaterialMultiProxyMaster* __fastcall GetItems(int index);
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent);
	
public:
	__fastcall TGLMaterialMultiProxyMasters(System::Classes::TPersistent* AOwner);
	HIDESBASE TGLMaterialMultiProxyMaster* __fastcall Add()/* overload */;
	HIDESBASE TGLMaterialMultiProxyMaster* __fastcall Add(Gls::Scene::TGLBaseSceneObject* Master, float DistanceMin, float DistanceMax)/* overload */;
	HIDESBASE TGLMaterialMultiProxyMaster* __fastcall Add(Gls::Scene::TGLBaseSceneObject* Master, Gls::Material::TGLLibMaterial* MasterLibMaterial, float DistanceMin, float DistanceMax)/* overload */;
	__property TGLMaterialMultiProxyMaster* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall NotifyChange();
	virtual void __fastcall EndUpdate();
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLMaterialMultiProxyMasters() { }
	
};


class PASCALIMPLEMENTATION TGLMaterialMultiProxy : public Gls::Scene::TGLBaseSceneObject
{
	typedef Gls::Scene::TGLBaseSceneObject inherited;
	
private:
	TGLMaterialMultiProxyMasters* FMasterObjects;
	bool FRendering;
	Gls::Material::TGLMaterialLibrary* FMaterialLibrary;
	void __fastcall SetMaterialLibrary(Gls::Material::TGLMaterialLibrary* const Value);
	
protected:
	void __fastcall SetMasterObjects(TGLMaterialMultiProxyMasters* const Val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	Gls::Scene::TGLBaseSceneObject* __fastcall PrimaryMaster();
	
public:
	__fastcall virtual TGLMaterialMultiProxy(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLMaterialMultiProxy();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	virtual bool __fastcall RayCastIntersect(const Gls::Vectortypes::TVector4f &rayStart, const Gls::Vectortypes::TVector4f &rayVector, Gls::Vectortypes::PGLVector intersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector intersectNormal = (Gls::Vectortypes::PGLVector)(0x0));
	virtual Gls::Silhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Gls::Silhouette::TGLSilhouetteParameters &silhouetteParameters);
	
__published:
	__property TGLMaterialMultiProxyMasters* MasterObjects = {read=FMasterObjects, write=SetMasterObjects};
	__property Gls::Material::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property ObjectsSorting = {default=0};
	__property Direction;
	__property PitchAngle = {default=0};
	__property Position;
	__property RollAngle = {default=0};
	__property Scale;
	__property ShowAxes = {default=0};
	__property TurnAngle = {default=0};
	__property Up;
	__property Visible = {default=1};
	__property OnProgress;
	__property Behaviours;
	__property Effects;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLMaterialMultiProxy(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLBaseSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Materialmultiproxy */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_MATERIALMULTIPROXY)
using namespace Gls::Materialmultiproxy;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_MaterialmultiproxyHPP
