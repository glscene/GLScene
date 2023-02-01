// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Physics.SPIFields.pas' rev: 35.00 (Windows)

#ifndef Physics_SpifieldsHPP
#define Physics_SpifieldsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.XCollection.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.Behaviours.hpp>
#include <Physics.SPIInertias.hpp>
#include <Physics.SPIManager.hpp>
#include <GLS.VectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Physics
{
namespace Spifields
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLUniformGravityEmitter;
class DELPHICLASS TGLRadialGravityEmitter;
class DELPHICLASS TGLDampingFieldEmitter;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLUniformGravityEmitter : public Physics::Spimanager::TGLBaseForceFieldEmitter
{
	typedef Physics::Spimanager::TGLBaseForceFieldEmitter inherited;
	
private:
	Gls::Coordinates::TGLCoordinates3* fGravity;
	
protected:
	void __fastcall SetGravity(Gls::Coordinates::TGLCoordinates3* const val);
	
public:
	__fastcall virtual TGLUniformGravityEmitter(Gls::Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLUniformGravityEmitter();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual bool __fastcall UniqueItem();
	virtual Gls::Vectortypes::TVector3f __fastcall CalculateForceField(Gls::Scene::TGLBaseSceneObject* Body);
	
__published:
	__property Gls::Coordinates::TGLCoordinates3* Gravity = {read=fGravity, write=SetGravity};
};


class PASCALIMPLEMENTATION TGLRadialGravityEmitter : public Physics::Spimanager::TGLBaseForceFieldEmitter
{
	typedef Physics::Spimanager::TGLBaseForceFieldEmitter inherited;
	
private:
	double fMass;
	double fMassOverG;
	
public:
	__fastcall virtual TGLRadialGravityEmitter(Gls::Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLRadialGravityEmitter();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual bool __fastcall UniqueItem();
	virtual Gls::Vectortypes::TVector3f __fastcall CalculateForceField(Gls::Scene::TGLBaseSceneObject* Body);
	
__published:
	__property double Mass = {read=fMass, write=fMass};
};


class PASCALIMPLEMENTATION TGLDampingFieldEmitter : public Physics::Spimanager::TGLBaseForceFieldEmitter
{
	typedef Physics::Spimanager::TGLBaseForceFieldEmitter inherited;
	
private:
	Gls::Behaviours::TGLDamping* fDamping;
	
protected:
	void __fastcall SetDamping(Gls::Behaviours::TGLDamping* const val);
	
public:
	__fastcall virtual TGLDampingFieldEmitter(Gls::Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLDampingFieldEmitter();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual bool __fastcall UniqueItem();
	virtual Gls::Vectortypes::TVector3f __fastcall CalculateForceField(Gls::Scene::TGLBaseSceneObject* Body);
	
__published:
	__property Gls::Behaviours::TGLDamping* Damping = {read=fDamping, write=SetDamping};
};


//-- var, const, procedure ---------------------------------------------------
static const double GravitationalConstant = 6.672600E-11;
}	/* namespace Spifields */
}	/* namespace Physics */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS_SPIFIELDS)
using namespace Physics::Spifields;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS)
using namespace Physics;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Physics_SpifieldsHPP
