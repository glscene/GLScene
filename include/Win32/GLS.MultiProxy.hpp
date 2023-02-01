// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.MultiProxy.pas' rev: 35.00 (Windows)

#ifndef Gls_MultiproxyHPP
#define Gls_MultiproxyHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.Context.hpp>
#include <GLS.Scene.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Silhouette.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Coordinates.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Multiproxy
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLMultiProxyMaster;
class DELPHICLASS TGLMultiProxyMasters;
class DELPHICLASS TGLMultiProxy;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMultiProxyMaster : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Gls::Scene::TGLBaseSceneObject* FMasterObject;
	float FDistanceMin;
	float FDistanceMin2;
	float FDistanceMax;
	float FDistanceMax2;
	bool FVisible;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	void __fastcall SetMasterObject(Gls::Scene::TGLBaseSceneObject* const val);
	void __fastcall SetDistanceMin(const float val);
	void __fastcall SetDistanceMax(const float val);
	void __fastcall SetVisible(const bool val);
	
public:
	__fastcall virtual TGLMultiProxyMaster(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLMultiProxyMaster();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	TGLMultiProxy* __fastcall OwnerObject();
	void __fastcall NotifyChange();
	
__published:
	__property Gls::Scene::TGLBaseSceneObject* MasterObject = {read=FMasterObject, write=SetMasterObject};
	__property float DistanceMin = {read=FDistanceMin, write=SetDistanceMin};
	__property float DistanceMax = {read=FDistanceMax, write=SetDistanceMax};
	__property bool Visible = {read=FVisible, write=SetVisible, default=1};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMultiProxyMasters : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLMultiProxyMaster* operator[](int index) { return this->Items[index]; }
	
protected:
	void __fastcall SetItems(int index, TGLMultiProxyMaster* const val);
	TGLMultiProxyMaster* __fastcall GetItems(int index);
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	
public:
	__fastcall TGLMultiProxyMasters(System::Classes::TPersistent* AOwner);
	HIDESBASE TGLMultiProxyMaster* __fastcall Add()/* overload */;
	HIDESBASE TGLMultiProxyMaster* __fastcall Add(Gls::Scene::TGLBaseSceneObject* master, float distanceMin, float distanceMax)/* overload */;
	__property TGLMultiProxyMaster* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall Notification(System::Classes::TComponent* AComponent);
	void __fastcall NotifyChange();
	virtual void __fastcall EndUpdate();
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLMultiProxyMasters() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLMultiProxy : public Gls::Scene::TGLSceneObject
{
	typedef Gls::Scene::TGLSceneObject inherited;
	
private:
	TGLMultiProxyMasters* FMasterObjects;
	bool FRendering;
	
protected:
	void __fastcall SetMasterObjects(TGLMultiProxyMasters* const val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	Gls::Scene::TGLBaseSceneObject* __fastcall PrimaryMaster();
	
public:
	__fastcall virtual TGLMultiProxy(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLMultiProxy();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	virtual bool __fastcall RayCastIntersect(const Gls::Vectortypes::TVector4f &rayStart, const Gls::Vectortypes::TVector4f &rayVector, Gls::Vectortypes::PGLVector intersectPoint = (Gls::Vectortypes::PGLVector)(0x0), Gls::Vectortypes::PGLVector intersectNormal = (Gls::Vectortypes::PGLVector)(0x0));
	virtual Gls::Silhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Gls::Silhouette::TGLSilhouetteParameters &silhouetteParameters);
	
__published:
	__property TGLMultiProxyMasters* MasterObjects = {read=FMasterObjects, write=SetMasterObjects};
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
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLMultiProxy(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Multiproxy */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_MULTIPROXY)
using namespace Gls::Multiproxy;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_MultiproxyHPP
