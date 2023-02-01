// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.FPSMovement.pas' rev: 35.00 (Windows)

#ifndef Gls_FpsmovementHPP
#define Gls_FpsmovementHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Graphics.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Context.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Scene.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.XCollection.hpp>
#include <GLS.GeomObjects.hpp>
#include <GLS.Navigator.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.Manager.hpp>
#include <GLS.State.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Fpsmovement
{
//-- forward type declarations -----------------------------------------------
struct TGLContactPoint;
class DELPHICLASS TGLCollisionState;
class DELPHICLASS TGLCollisionStates;
class DELPHICLASS TGLMapCollectionItem;
class DELPHICLASS TGLMapCollection;
class DELPHICLASS TGLFPSMovementManager;
class DELPHICLASS TGLBFPSMovement;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TGLContactPoint
{
public:
	Gls::Vectortypes::TVector4f intPoint;
	Gls::Vectortypes::TVector4f intNormal;
};


class PASCALIMPLEMENTATION TGLCollisionState : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	Gls::Vectortypes::TVector4f Position;
	TGLContactPoint Contact;
	__int64 Time;
public:
	/* TObject.Create */ inline __fastcall TGLCollisionState() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLCollisionState() { }
	
};


class PASCALIMPLEMENTATION TGLCollisionStates : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	/* TList.Destroy */ inline __fastcall virtual ~TGLCollisionStates() { }
	
public:
	/* TObject.Create */ inline __fastcall TGLCollisionStates() : System::Classes::TList() { }
	
};


class PASCALIMPLEMENTATION TGLMapCollectionItem : public Gls::Xcollection::TXCollectionItem
{
	typedef Gls::Xcollection::TXCollectionItem inherited;
	
private:
	Gls::Vectorfileobjects::TGLFreeForm* FMap;
	System::UnicodeString FMapName;
	int FCollisionGroup;
	void __fastcall setMap(Gls::Vectorfileobjects::TGLFreeForm* value);
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded();
	
public:
	__fastcall virtual TGLMapCollectionItem(Gls::Xcollection::TXCollection* aOwner);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	
__published:
	__property Gls::Vectorfileobjects::TGLFreeForm* Map = {read=FMap, write=setMap};
	__property int CollisionGroup = {read=FCollisionGroup, write=FCollisionGroup, nodefault};
public:
	/* TXCollectionItem.Destroy */ inline __fastcall virtual ~TGLMapCollectionItem() { }
	
};


_DECLARE_METACLASS(System::TMetaClass, TGLMapCollectionItemClass);

class PASCALIMPLEMENTATION TGLMapCollection : public Gls::Xcollection::TXCollection
{
	typedef Gls::Xcollection::TXCollection inherited;
	
public:
	__classmethod virtual Gls::Xcollection::TXCollectionItemClass __fastcall ItemsClass();
	TGLMapCollectionItem* __fastcall addMap(Gls::Vectorfileobjects::TGLFreeForm* Map, int CollisionGroup = 0x0);
	TGLMapCollectionItem* __fastcall findMap(Gls::Vectorfileobjects::TGLFreeForm* mapFreeForm);
public:
	/* TXCollection.Create */ inline __fastcall virtual TGLMapCollection(System::Classes::TPersistent* aOwner) : Gls::Xcollection::TXCollection(aOwner) { }
	/* TXCollection.Destroy */ inline __fastcall virtual ~TGLMapCollection() { }
	
};


class PASCALIMPLEMENTATION TGLFPSMovementManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Gls::Navigator::TGLNavigator* FNavigator;
	int FDisplayTime;
	float FMovementScale;
	TGLMapCollection* FMaps;
	Gls::Scene::TGLScene* FScene;
	void __fastcall SetNavigator(Gls::Navigator::TGLNavigator* value);
	void __fastcall setScene(Gls::Scene::TGLScene* value);
	void __fastcall DrawArrows(const Gls::Vectortypes::TVector4f &intPoint, const Gls::Vectortypes::TVector4f &intNormal, const Gls::Vectortypes::TVector4f &Ray, Gls::Geomobjects::TGLArrowLine* Arrow1, Gls::Geomobjects::TGLArrowLine* Arrow2);
	
protected:
	virtual void __fastcall Loaded();
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall WriteMaps(System::Classes::TStream* stream);
	void __fastcall ReadMaps(System::Classes::TStream* stream);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLFPSMovementManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLFPSMovementManager();
	bool __fastcall SphereSweepAndSlide(Gls::Vectorfileobjects::TGLFreeForm* freeform, TGLBFPSMovement* behaviour, const Gls::Vectortypes::TVector4f &SphereStart, Gls::Vectortypes::TVector4f &Velocity, Gls::Vectortypes::TVector4f &newPosition, float sphereRadius)/* overload */;
	void __fastcall SphereSweepAndSlide(TGLBFPSMovement* behaviour, const Gls::Vectortypes::TVector4f &SphereStart, Gls::Vectortypes::TVector4f &Velocity, Gls::Vectortypes::TVector4f &newPosition, float sphereRadius)/* overload */;
	
__published:
	__property TGLMapCollection* Maps = {read=FMaps, write=FMaps};
	__property Gls::Navigator::TGLNavigator* Navigator = {read=FNavigator, write=SetNavigator};
	__property Gls::Scene::TGLScene* Scene = {read=FScene, write=setScene};
	__property int DisplayTime = {read=FDisplayTime, write=FDisplayTime, nodefault};
	__property float MovementScale = {read=FMovementScale, write=FMovementScale};
};


class PASCALIMPLEMENTATION TGLBFPSMovement : public Gls::Scene::TGLBehaviour
{
	typedef Gls::Scene::TGLBehaviour inherited;
	
private:
	TGLFPSMovementManager* FManager;
	TGLCollisionStates* CollisionStates;
	Gls::Geomobjects::TGLArrowLine* ArrowLine1;
	Gls::Geomobjects::TGLArrowLine* ArrowLine2;
	Gls::Geomobjects::TGLArrowLine* ArrowLine3;
	Gls::Geomobjects::TGLArrowLine* ArrowLine4;
	Gls::Geomobjects::TGLArrowLine* ArrowLine5;
	Gls::Geomobjects::TGLArrowLine* ArrowLine6;
	Gls::Scene::TGLDirectOpenGL* dirGl;
	__int64 tickCount;
	Gls::Vectortypes::TVector4f oldPosition;
	bool FGravityEnabled;
	float FSphereRadius;
	bool FShowArrows;
	int FCollisionGroup;
	System::UnicodeString FManagerName;
	void __fastcall setShowArrows(bool value);
	void __fastcall RenderArrowLines(System::TObject* Sender, Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded();
	
public:
	Gls::Vectortypes::TVector4f Velocity;
	__fastcall virtual TGLBFPSMovement(Gls::Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLBFPSMovement();
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	void __fastcall TurnHorizontal(float Angle);
	void __fastcall TurnVertical(float Angle);
	void __fastcall MoveForward(float Distance);
	void __fastcall StrafeHorizontal(float Distance);
	void __fastcall StrafeVertical(float Distance);
	void __fastcall Straighten();
	
__published:
	__property TGLFPSMovementManager* Manager = {read=FManager, write=FManager};
	__property float sphereRadius = {read=FSphereRadius, write=FSphereRadius};
	__property bool ShowArrows = {read=FShowArrows, write=setShowArrows, nodefault};
	__property int CollisionGroup = {read=FCollisionGroup, write=FCollisionGroup, nodefault};
	__property bool GravityEnabled = {read=FGravityEnabled, write=FGravityEnabled, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLBFPSMovement* __fastcall GetFPSMovement(Gls::Scene::TGLBehaviours* behaviours)/* overload */;
extern DELPHI_PACKAGE TGLBFPSMovement* __fastcall GetFPSMovement(Gls::Scene::TGLBaseSceneObject* obj)/* overload */;
extern DELPHI_PACKAGE TGLBFPSMovement* __fastcall GetOrCreateFPSMovement(Gls::Scene::TGLBehaviours* behaviours)/* overload */;
extern DELPHI_PACKAGE TGLBFPSMovement* __fastcall GetOrCreateFPSMovement(Gls::Scene::TGLBaseSceneObject* obj)/* overload */;
}	/* namespace Fpsmovement */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_FPSMOVEMENT)
using namespace Gls::Fpsmovement;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_FpsmovementHPP
