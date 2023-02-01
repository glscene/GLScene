// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Physics.SPIInertias.pas' rev: 35.00 (Windows)

#ifndef Physics_SpiinertiasHPP
#define Physics_SpiinertiasHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Dialogs.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.XCollection.hpp>
#include <GLS.Scene.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorTypes.hpp>
#include <Physics.SPIManager.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.Strings.hpp>
#include <GLS.Behaviours.hpp>

//-- user supplied -----------------------------------------------------------

namespace Physics
{
namespace Spiinertias
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLParticleInertia;
class DELPHICLASS TGLInertiaTensor;
class DELPHICLASS TGLRigidBodyInertia;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLParticleInertia : public Physics::Spimanager::TGLBaseInertia
{
	typedef Physics::Spimanager::TGLBaseInertia inherited;
	
private:
	float FMass;
	Gls::Coordinates::TGLCoordinates3* FTranslationSpeed;
	Gls::Behaviours::TGLDamping* FTranslationDamping;
	
protected:
	Gls::Vectortypes::TVector3f __fastcall CalcLinearPositionDot();
	Gls::Vectortypes::TVector3f __fastcall CalcLinearMomentumDot();
	void __fastcall SetTranslationSpeed(Gls::Coordinates::TGLCoordinates3* const val);
	void __fastcall SetTranslationDamping(Gls::Behaviours::TGLDamping* const val);
	
public:
	Gls::Vectortypes::TVector3f fForce;
	Gls::Vectortypes::TVector3f LinearPosition;
	Gls::Vectortypes::TVector3f LinearMomentum;
	virtual void __fastcall StateToArray(Physics::Spimanager::TStateArray &StateArray, int StatePos);
	virtual void __fastcall ArrayToState(Physics::Spimanager::TStateArray StateArray, int StatePos);
	virtual void __fastcall CalcStateDot(Physics::Spimanager::TStateArray &StateArray, int StatePos);
	virtual void __fastcall RemoveForces();
	virtual void __fastcall CalculateForceFieldForce(Physics::Spimanager::TGLBaseForceFieldEmitter* ForceFieldEmitter);
	virtual void __fastcall CalcAuxiliary();
	virtual void __fastcall SetUpStartingState();
	virtual double __fastcall CalculateKE();
	virtual double __fastcall CalculatePE();
	virtual void __fastcall SetForce(double x, double y, double z);
	virtual void __fastcall ApplyForce(double x, double y, double z)/* overload */;
	virtual void __fastcall ApplyForce(const Gls::Vectortypes::TVector3f &Force)/* overload */;
	virtual void __fastcall ApplyForce(const Gls::Vectortypes::TVector3f &pos, const Gls::Vectortypes::TVector3f &Force)/* overload */;
	virtual void __fastcall ApplyLocalForce(const Gls::Vectortypes::TVector3f &pos, const Gls::Vectortypes::TVector3f &Force);
	virtual void __fastcall ApplyImpulse(double j, double x, double y, double z)/* overload */;
	virtual void __fastcall ApplyImpulse(float j, const Gls::Vectortypes::TVector3f &normal)/* overload */;
	virtual void __fastcall ApplyDamping(Gls::Behaviours::TGLDamping* damping);
	__fastcall virtual TGLParticleInertia(Gls::Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLParticleInertia();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual bool __fastcall UniqueItem();
	void __fastcall MirrorTranslation();
	void __fastcall SurfaceBounce(const Gls::Vectortypes::TVector4f &surfaceNormal, float restitution);
	
__published:
	__property float Mass = {read=FMass, write=FMass};
	__property Gls::Coordinates::TGLCoordinates3* TranslationSpeed = {read=FTranslationSpeed, write=SetTranslationSpeed};
	__property Gls::Behaviours::TGLDamping* TranslationDamping = {read=FTranslationDamping, write=SetTranslationDamping};
};


class PASCALIMPLEMENTATION TGLInertiaTensor : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	float fm11;
	float fm12;
	float fm13;
	float fm21;
	float fm22;
	float fm23;
	float fm31;
	float fm32;
	float fm33;
	
public:
	__fastcall virtual TGLInertiaTensor(System::Classes::TPersistent* aOwner);
	__fastcall virtual ~TGLInertiaTensor();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	
__published:
	__property float m11 = {read=fm11, write=fm11};
	__property float m12 = {read=fm12, write=fm12};
	__property float m13 = {read=fm13, write=fm13};
	__property float m21 = {read=fm21, write=fm21};
	__property float m22 = {read=fm22, write=fm22};
	__property float m23 = {read=fm23, write=fm23};
	__property float m31 = {read=fm31, write=fm31};
	__property float m32 = {read=fm32, write=fm32};
	__property float m33 = {read=fm33, write=fm33};
};


class PASCALIMPLEMENTATION TGLRigidBodyInertia : public TGLParticleInertia
{
	typedef TGLParticleInertia inherited;
	
private:
	double fDensity;
	Gls::Vectortypes::TMatrix3f fBodyInertiaTensor;
	Gls::Vectortypes::TMatrix3f fBodyInverseInertiaTensor;
	TGLInertiaTensor* fInertiaTensor;
	Gls::Vectortypes::TMatrix3f InverseInertiaTensor;
	Gls::Coordinates::TGLCoordinates3* fRotationSpeed;
	Gls::Behaviours::TGLDamping* FRotationDamping;
	
protected:
	Gls::Vectortypes::TVector3f fTorque;
	void __fastcall SetLinearDamping(Gls::Behaviours::TGLDamping* const val);
	void __fastcall SetAngularDamping(Gls::Behaviours::TGLDamping* const val);
	
public:
	Gls::Vectorgeometry::TQuaternion AngularOrientation;
	Gls::Vectortypes::TMatrix3f R;
	Gls::Vectortypes::TVector3f AngularMomentum;
	virtual void __fastcall StateToArray(Physics::Spimanager::TStateArray &StateArray, int StatePos);
	virtual void __fastcall ArrayToState(Physics::Spimanager::TStateArray StateArray, int StatePos);
	virtual void __fastcall CalcStateDot(Physics::Spimanager::TStateArray &StateArray, int StatePos);
	HIDESBASE void __fastcall ApplyImpulse(double j, double xpos, double ypos, double zpos, double x, double y, double z)/* overload */;
	HIDESBASE void __fastcall ApplyImpulse(float j, const Gls::Vectortypes::TVector3f &position, const Gls::Vectortypes::TVector3f &normal)/* overload */;
	virtual void __fastcall ApplyDamping(Gls::Behaviours::TGLDamping* damping);
	Gls::Vectorgeometry::TQuaternion __fastcall CalcAngularOrientationDot();
	Gls::Vectortypes::TVector3f __fastcall CalcAngularVelocityDot();
	virtual double __fastcall CalculateKE();
	virtual double __fastcall CalculatePE();
	virtual void __fastcall CalcAuxiliary();
	virtual void __fastcall SetUpStartingState();
	__fastcall virtual TGLRigidBodyInertia(Gls::Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLRigidBodyInertia();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual bool __fastcall UniqueItem();
	System::UnicodeString __fastcall QuaternionToString(const Gls::Vectorgeometry::TQuaternion &Quat);
	virtual void __fastcall RemoveForces();
	void __fastcall SetTorque(double x, double y, double z);
	void __fastcall ApplyTorque(double x, double y, double z);
	virtual void __fastcall ApplyForce(const Gls::Vectortypes::TVector3f &pos, const Gls::Vectortypes::TVector3f &Force)/* overload */;
	virtual void __fastcall ApplyLocalForce(const Gls::Vectortypes::TVector3f &pos, const Gls::Vectortypes::TVector3f &Force);
	void __fastcall ApplyLocalImpulse(double xpos, double ypos, double zpos, double x, double y, double z);
	void __fastcall SetInertiaTensor(TGLInertiaTensor* newVal);
	void __fastcall SetRotationSpeed(Gls::Coordinates::TGLCoordinates3* const val);
	void __fastcall SetRotationDamping(Gls::Behaviours::TGLDamping* const val);
	
__published:
	__property double Density = {read=fDensity, write=fDensity};
	__property TGLInertiaTensor* InertiaTensor = {read=fInertiaTensor, write=SetInertiaTensor};
	__property Gls::Coordinates::TGLCoordinates3* RotationSpeed = {read=fRotationSpeed, write=SetRotationSpeed};
	__property Gls::Behaviours::TGLDamping* RotationDamping = {read=FRotationDamping, write=SetRotationDamping};
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  ApplyImpulse(double j, double x, double y, double z){ TGLParticleInertia::ApplyImpulse(j, x, y, z); }
	inline void __fastcall  ApplyImpulse(float j, const Gls::Vectortypes::TVector3f &normal){ TGLParticleInertia::ApplyImpulse(j, normal); }
	inline void __fastcall  ApplyForce(double x, double y, double z){ TGLParticleInertia::ApplyForce(x, y, z); }
	inline void __fastcall  ApplyForce(const Gls::Vectortypes::TVector3f &Force){ TGLParticleInertia::ApplyForce(Force); }
	
};


//-- var, const, procedure ---------------------------------------------------
static const bool DebugMode = false;
extern DELPHI_PACKAGE TGLParticleInertia* __fastcall GetOrCreateParticleInertia(Gls::Scene::TGLBehaviours* behaviours)/* overload */;
extern DELPHI_PACKAGE TGLParticleInertia* __fastcall GetOrCreateParticleInertia(Gls::Scene::TGLBaseSceneObject* obj)/* overload */;
extern DELPHI_PACKAGE TGLRigidBodyInertia* __fastcall GetOrCreateRigidBodyInertia(Gls::Scene::TGLBehaviours* behaviours)/* overload */;
extern DELPHI_PACKAGE TGLRigidBodyInertia* __fastcall GetOrCreateRigidBodyInertia(Gls::Scene::TGLBaseSceneObject* obj)/* overload */;
}	/* namespace Spiinertias */
}	/* namespace Physics */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS_SPIINERTIAS)
using namespace Physics::Spiinertias;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS)
using namespace Physics;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Physics_SpiinertiasHPP
