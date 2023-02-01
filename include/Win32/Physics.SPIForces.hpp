// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Physics.SPIForces.pas' rev: 35.00 (Windows)

#ifndef Physics_SpiforcesHPP
#define Physics_SpiforcesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <Vcl.Dialogs.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.XCollection.hpp>
#include <GLS.Scene.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Behaviours.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.Strings.hpp>

//-- user supplied -----------------------------------------------------------

namespace Physics
{
namespace Spiforces
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLForce;
class DELPHICLASS TGLHookesSpring;
class DELPHICLASS TGLHookesString;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLForceType : unsigned char { ftHookes, ftGravitation, ftCustom };

typedef void __fastcall (__closure *TOnCustomForce)(void);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLForce : public Gls::Xcollection::TXCollectionItem
{
	typedef Gls::Xcollection::TXCollectionItem inherited;
	
private:
	Gls::Scene::TGLBaseSceneObject* fObject1;
	Gls::Scene::TGLBaseSceneObject* fObject2;
	Gls::Coordinates::TGLCoordinates3* fposition1;
	Gls::Coordinates::TGLCoordinates3* fposition2;
	System::UnicodeString object1Name;
	System::UnicodeString object2Name;
	
protected:
	DYNAMIC void __fastcall Loaded();
	virtual void __fastcall SetName(const System::UnicodeString val);
	
public:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	__fastcall virtual TGLForce(Gls::Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLForce();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual bool __fastcall UniqueItem();
	void __fastcall SetObject1(Gls::Scene::TGLBaseSceneObject* const val);
	void __fastcall SetObject2(Gls::Scene::TGLBaseSceneObject* const val);
	void __fastcall SetPosition1(Gls::Coordinates::TGLCoordinates3* const val);
	void __fastcall SetPosition2(Gls::Coordinates::TGLCoordinates3* const val);
	virtual Gls::Vectortypes::TVector3f __fastcall CalculateForce();
	
__published:
	__property Gls::Scene::TGLBaseSceneObject* Object1 = {read=fObject1, write=SetObject1};
	__property Gls::Scene::TGLBaseSceneObject* Object2 = {read=fObject2, write=SetObject2};
	__property Gls::Coordinates::TGLCoordinates3* Position1 = {read=fposition1, write=SetPosition1};
	__property Gls::Coordinates::TGLCoordinates3* Position2 = {read=fposition2, write=SetPosition2};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLHookesSpring : public TGLForce
{
	typedef TGLForce inherited;
	
private:
	double fNaturalLength;
	double fElasticity;
	double fLength;
	double fExtension;
	Gls::Behaviours::TGLDamping* fDamping;
	
public:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	__fastcall virtual TGLHookesSpring(Gls::Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLHookesSpring();
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual bool __fastcall UniqueItem();
	void __fastcall SetDamping(Gls::Behaviours::TGLDamping* const val);
	virtual Gls::Vectortypes::TVector3f __fastcall CalculateForce();
	
__published:
	__property double NaturalLength = {read=fNaturalLength, write=fNaturalLength};
	__property double Elasticity = {read=fElasticity, write=fElasticity};
	__property Gls::Behaviours::TGLDamping* Damping = {read=fDamping, write=SetDamping};
};


class PASCALIMPLEMENTATION TGLHookesString : public TGLHookesSpring
{
	typedef TGLHookesSpring inherited;
	
public:
	__fastcall virtual TGLHookesString(Gls::Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLHookesString();
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual bool __fastcall UniqueItem();
	virtual Gls::Vectortypes::TVector3f __fastcall CalculateForce();
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Spiforces */
}	/* namespace Physics */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS_SPIFORCES)
using namespace Physics::Spiforces;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS)
using namespace Physics;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Physics_SpiforcesHPP
