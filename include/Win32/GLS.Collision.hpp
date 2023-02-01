// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Collision.pas' rev: 35.00 (Windows)

#ifndef Gls_CollisionHPP
#define Gls_CollisionHPP

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
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Scene.hpp>
#include <GLS.XCollection.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.GeometryBB.hpp>
#include <GLS.Manager.hpp>
#include <GLS.VectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Collision
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLCollisionManager;
class DELPHICLASS TGLBCollision;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TObjectCollisionEvent)(System::TObject* Sender, Gls::Scene::TGLBaseSceneObject* object1, Gls::Scene::TGLBaseSceneObject* object2);

enum DECLSPEC_DENUM TCollisionBoundingMode : unsigned char { cbmPoint, cbmSphere, cbmEllipsoid, cbmCube, cbmFaces };

typedef bool __fastcall (*TFastCollisionChecker)(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);

typedef TFastCollisionChecker *PFastCollisionChecker;

class PASCALIMPLEMENTATION TGLCollisionManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Classes::TList* FClients;
	TObjectCollisionEvent FOnCollision;
	
protected:
	void __fastcall RegisterClient(TGLBCollision* aClient);
	void __fastcall DeRegisterClient(TGLBCollision* aClient);
	void __fastcall DeRegisterAllClients();
	
public:
	__fastcall virtual TGLCollisionManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCollisionManager();
	void __fastcall CheckCollisions();
	
__published:
	__property TObjectCollisionEvent OnCollision = {read=FOnCollision, write=FOnCollision};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBCollision : public Gls::Scene::TGLBehaviour
{
	typedef Gls::Scene::TGLBehaviour inherited;
	
private:
	TCollisionBoundingMode FBoundingMode;
	TGLCollisionManager* FManager;
	System::UnicodeString FManagerName;
	int FGroupIndex;
	
protected:
	void __fastcall SetGroupIndex(const int value);
	void __fastcall SetManager(TGLCollisionManager* const val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded();
	
public:
	__fastcall virtual TGLBCollision(Gls::Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLBCollision();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	
__published:
	__property TGLCollisionManager* Manager = {read=FManager, write=SetManager};
	__property TCollisionBoundingMode BoundingMode = {read=FBoundingMode, write=FBoundingMode, nodefault};
	__property int GroupIndex = {read=FGroupIndex, write=SetGroupIndex, nodefault};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool __fastcall FastCheckPointVsPoint(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckPointVsSphere(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckPointVsEllipsoid(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckPointVsCube(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckSphereVsPoint(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckSphereVsSphere(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckSphereVsEllipsoid(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckSphereVsCube(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckEllipsoidVsPoint(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckEllipsoidVsSphere(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckEllipsoidVsEllipsoid(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckEllipsoidVsCube(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckCubeVsPoint(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckCubeVsSphere(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckCubeVsEllipsoid(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckCubeVsCube(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckCubeVsFace(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckFaceVsCube(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall FastCheckFaceVsFace(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2);
extern DELPHI_PACKAGE bool __fastcall IntersectCubes(Gls::Scene::TGLBaseSceneObject* obj1, Gls::Scene::TGLBaseSceneObject* obj2)/* overload */;
extern DELPHI_PACKAGE TGLBCollision* __fastcall GetOrCreateCollision(Gls::Scene::TGLBehaviours* behaviours)/* overload */;
extern DELPHI_PACKAGE TGLBCollision* __fastcall GetOrCreateCollision(Gls::Scene::TGLBaseSceneObject* obj)/* overload */;
}	/* namespace Collision */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_COLLISION)
using namespace Gls::Collision;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_CollisionHPP
