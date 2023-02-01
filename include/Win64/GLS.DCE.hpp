// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.DCE.pas' rev: 35.00 (Windows)

#ifndef Gls_DceHPP
#define Gls_DceHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <GLS.Scene.hpp>
#include <GLS.XCollection.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.EllipseCollision.hpp>
#include <GLS.TerrainRenderer.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.ProxyObjects.hpp>
#include <GLS.MultiProxy.hpp>
#include <GLS.Manager.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Strings.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Dce
{
//-- forward type declarations -----------------------------------------------
struct TDCECollision;
class DELPHICLASS TGLDCEManager;
class DELPHICLASS TGLDCEStatic;
class DELPHICLASS TGLDCEDynamic;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TDCEShape : unsigned char { csEllipsoid, csBox, csFreeform, csTerrain };

enum DECLSPEC_DENUM TDCECollisionSelection : unsigned char { ccsDCEStandard, ccsCollisionStandard, ccsHybrid };

struct DECLSPEC_DRECORD TDCECollision
{
public:
	Gls::Vectortypes::TVector3f Position;
	Gls::Vectortypes::TVector3f Normal;
	Gls::Vectortypes::TVector3f Bounce;
	bool Nearest;
	bool RootCollision;
	float Distance;
};


typedef void __fastcall (__closure *TDCECollisionEvent)(System::TObject* Sender, Gls::Scene::TGLBaseSceneObject* object1, Gls::Scene::TGLBaseSceneObject* object2, const TDCECollision &CollisionInfo);

typedef void __fastcall (__closure *TDCEObjectCollisionEvent)(System::TObject* Sender, Gls::Scene::TGLBaseSceneObject* ObjectCollided, const TDCECollision &CollisionInfo);

class PASCALIMPLEMENTATION TGLDCEManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Classes::TList* FStatics;
	System::Classes::TList* FDynamics;
	float FGravity;
	Gls::Coordinates::TGLCoordinates3* FWorldDirection;
	float FWorldScale;
	float FMovimentScale;
	TDCECollisionSelection FStandardiseLayers;
	bool FManualStep;
	TDCECollisionEvent FOnCollision;
	void __fastcall SetWorldDirection(Gls::Coordinates::TGLCoordinates3* const Value);
	void __fastcall SetWorldScale(const float Value);
	int __fastcall GetDynamicCount();
	int __fastcall GetStaticCount();
	
protected:
	void __fastcall RegisterStatic(TGLDCEStatic* aClient);
	void __fastcall DeRegisterStatic(TGLDCEStatic* aClient);
	void __fastcall DeRegisterAllStatics();
	void __fastcall RegisterDynamic(TGLDCEDynamic* aClient);
	void __fastcall DeRegisterDynamic(TGLDCEDynamic* aClient);
	void __fastcall DeRegisterAllDynamics();
	
public:
	__fastcall virtual TGLDCEManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLDCEManager();
	float __fastcall MoveByDistance(TGLDCEDynamic* &Body, const Gls::Vectortypes::TVector3f &deltaS, const Gls::Vectortypes::TVector3f &deltaAbsS);
	void __fastcall Step(double deltaTime);
	__property int DynamicCount = {read=GetDynamicCount, nodefault};
	__property int StaticCount = {read=GetStaticCount, nodefault};
	
__published:
	__property float Gravity = {read=FGravity, write=FGravity};
	__property Gls::Coordinates::TGLCoordinates3* WorldDirection = {read=FWorldDirection, write=SetWorldDirection};
	__property float WorldScale = {read=FWorldScale, write=SetWorldScale};
	__property float MovimentScale = {read=FMovimentScale, write=FMovimentScale};
	__property TDCECollisionSelection StandardiseLayers = {read=FStandardiseLayers, write=FStandardiseLayers, nodefault};
	__property bool ManualStep = {read=FManualStep, write=FManualStep, nodefault};
	__property TDCECollisionEvent OnCollision = {read=FOnCollision, write=FOnCollision};
};


class PASCALIMPLEMENTATION TGLDCEStatic : public Gls::Scene::TGLBehaviour
{
	typedef Gls::Scene::TGLBehaviour inherited;
	
private:
	TGLDCEManager* FManager;
	System::UnicodeString FManagerName;
	bool FActive;
	TDCEShape FShape;
	int FLayer;
	bool FSolid;
	float FFriction;
	float FBounceFactor;
	Gls::Coordinates::TGLCoordinates3* FSize;
	TDCEObjectCollisionEvent FOnCollision;
	void __fastcall SetShape(const TDCEShape Value);
	void __fastcall SetFriction(const float Value);
	void __fastcall SetBounceFactor(const float Value);
	void __fastcall SetSize(Gls::Coordinates::TGLCoordinates3* const Value);
	
protected:
	void __fastcall SetManager(TGLDCEManager* const val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded();
	
public:
	__fastcall virtual TGLDCEStatic(Gls::Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLDCEStatic();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__property TDCEObjectCollisionEvent OnCollision = {read=FOnCollision, write=FOnCollision};
	
__published:
	__property bool Active = {read=FActive, write=FActive, nodefault};
	__property TGLDCEManager* Manager = {read=FManager, write=SetManager};
	__property TDCEShape Shape = {read=FShape, write=SetShape, nodefault};
	__property int Layer = {read=FLayer, write=FLayer, nodefault};
	__property bool Solid = {read=FSolid, write=FSolid, nodefault};
	__property float Friction = {read=FFriction, write=SetFriction};
	__property float BounceFactor = {read=FBounceFactor, write=SetBounceFactor};
	__property Gls::Coordinates::TGLCoordinates3* Size = {read=FSize, write=SetSize};
};


enum DECLSPEC_DENUM TDCESlideOrBounce : unsigned char { csbSlide, csbBounce };

class PASCALIMPLEMENTATION TGLDCEDynamic : public Gls::Scene::TGLBehaviour
{
	typedef Gls::Scene::TGLBehaviour inherited;
	
private:
	TGLDCEManager* FManager;
	System::UnicodeString FManagerName;
	bool FActive;
	bool FUseGravity;
	int FLayer;
	bool FSolid;
	float FFriction;
	float FBounceFactor;
	Gls::Coordinates::TGLCoordinates3* FSize;
	System::Byte FMaxRecursionDepth;
	TDCESlideOrBounce FSlideOrBounce;
	Gls::Vectortypes::TVector3f FAccel;
	Gls::Vectortypes::TVector3f FSpeed;
	Gls::Vectortypes::TVector3f FAbsAccel;
	Gls::Vectortypes::TVector3f FAbsSpeed;
	Gls::Vectortypes::TVector3f FGravSpeed;
	float FTotalFriction;
	bool FInGround;
	Gls::Vectortypes::TVector3f FGroundNormal;
	float FJumpHeight;
	float FJumpForce;
	float FJumpSpeed;
	bool FJumping;
	TDCEObjectCollisionEvent FOnCollision;
	void __fastcall SetFriction(const float Value);
	void __fastcall SetBounceFactor(const float Value);
	void __fastcall SetSize(Gls::Coordinates::TGLCoordinates3* const Value);
	
protected:
	void __fastcall SetManager(TGLDCEManager* const val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded();
	
public:
	__fastcall virtual TGLDCEDynamic(Gls::Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLDCEDynamic();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	void __fastcall ApplyAccel(const Gls::Vectortypes::TVector3f &NewAccel)/* overload */;
	void __fastcall ApplyAccel(float x, float y, float z)/* overload */;
	void __fastcall ApplyAbsAccel(const Gls::Vectortypes::TVector3f &NewAccel)/* overload */;
	void __fastcall ApplyAbsAccel(float x, float y, float z)/* overload */;
	void __fastcall StopAccel();
	void __fastcall StopAbsAccel();
	void __fastcall Jump(float jHeight, float jSpeed);
	void __fastcall Move(const Gls::Vectortypes::TVector3f &deltaS, double deltaTime);
	void __fastcall MoveTo(const Gls::Vectortypes::TVector3f &Position, float Amount);
	void __fastcall DoMove(double deltaTime);
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	__property Gls::Vectortypes::TVector3f Speed = {read=FSpeed, write=FSpeed};
	__property bool InGround = {read=FInGround, nodefault};
	__property System::Byte MaxRecursionDepth = {read=FMaxRecursionDepth, write=FMaxRecursionDepth, nodefault};
	__property TDCEObjectCollisionEvent OnCollision = {read=FOnCollision, write=FOnCollision};
	
__published:
	__property bool Active = {read=FActive, write=FActive, nodefault};
	__property TGLDCEManager* Manager = {read=FManager, write=SetManager};
	__property bool UseGravity = {read=FUseGravity, write=FUseGravity, nodefault};
	__property int Layer = {read=FLayer, write=FLayer, nodefault};
	__property bool Solid = {read=FSolid, write=FSolid, nodefault};
	__property float Friction = {read=FFriction, write=SetFriction};
	__property float BounceFactor = {read=FBounceFactor, write=SetBounceFactor};
	__property Gls::Coordinates::TGLCoordinates3* Size = {read=FSize, write=SetSize};
	__property TDCESlideOrBounce SlideOrBounce = {read=FSlideOrBounce, write=FSlideOrBounce, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::StaticArray<Gls::Vectortypes::TVector3f, 36> DCEBox;
extern DELPHI_PACKAGE void __fastcall ECSetCollisionRange(Gls::Ellipsecollision::TECMovePack &MovePack);
extern DELPHI_PACKAGE void __fastcall ECResetColliders(Gls::Ellipsecollision::TECMovePack &MovePack);
extern DELPHI_PACKAGE void __fastcall ECAddFreeForm(Gls::Ellipsecollision::TECMovePack &MovePack, Gls::Scene::TGLBaseSceneObject* FreeForm, bool Solid, int ObjectID);
extern DELPHI_PACKAGE void __fastcall ECAddBox(Gls::Ellipsecollision::TECMovePack &MovePack, Gls::Scene::TGLBaseSceneObject* BoxObj, const Gls::Vectortypes::TVector3f &BoxSize, bool Solid, int ObjectID);
extern DELPHI_PACKAGE void __fastcall ECAddTerrain(Gls::Ellipsecollision::TECMovePack &MovePack, Gls::Terrainrenderer::TGLTerrainRenderer* TerrainRenderer, float Resolution, bool Solid, int ObjectID);
extern DELPHI_PACKAGE void __fastcall ECAddEllipsoid(Gls::Ellipsecollision::TECMovePack &MovePack, const Gls::Vectortypes::TVector3f &ePos, const Gls::Vectortypes::TVector3f &eRadius, bool Solid, int ObjectID);
extern DELPHI_PACKAGE TGLDCEStatic* __fastcall GetOrCreateDCEStatic(Gls::Scene::TGLBehaviours* behaviours)/* overload */;
extern DELPHI_PACKAGE TGLDCEStatic* __fastcall GetOrCreateDCEStatic(Gls::Scene::TGLBaseSceneObject* obj)/* overload */;
extern DELPHI_PACKAGE TGLDCEDynamic* __fastcall GetOrCreateDCEDynamic(Gls::Scene::TGLBehaviours* behaviours)/* overload */;
extern DELPHI_PACKAGE TGLDCEDynamic* __fastcall GetOrCreateDCEDynamic(Gls::Scene::TGLBaseSceneObject* obj)/* overload */;
}	/* namespace Dce */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_DCE)
using namespace Gls::Dce;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_DceHPP
