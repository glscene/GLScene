// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Behaviours.pas' rev: 35.00 (Windows)

#ifndef Gls_BehavioursHPP
#define Gls_BehavioursHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Scene.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.XCollection.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.Coordinates.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Behaviours
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLDamping;
class DELPHICLASS TGLBInertia;
class DELPHICLASS TGLBAcceleration;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLDamping : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	float FConstant;
	float FLinear;
	float FQuadratic;
	
public:
	__fastcall virtual TGLDamping(System::Classes::TPersistent* aOwner);
	__fastcall virtual ~TGLDamping();
	void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	double __fastcall Calculate(double speed, double deltaTime);
	System::UnicodeString __fastcall AsString(TGLDamping* const damping);
	void __fastcall SetDamping(const float constant = 0.000000E+00f, const float linear = 0.000000E+00f, const float quadratic = 0.000000E+00f);
	
__published:
	__property float Constant = {read=FConstant, write=FConstant};
	__property float Linear = {read=FLinear, write=FLinear};
	__property float Quadratic = {read=FQuadratic, write=FQuadratic};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBInertia : public Gls::Scene::TGLBehaviour
{
	typedef Gls::Scene::TGLBehaviour inherited;
	
private:
	float FMass;
	Gls::Coordinates::TGLCoordinates3* FTranslationSpeed;
	float FTurnSpeed;
	float FRollSpeed;
	float FPitchSpeed;
	TGLDamping* FTranslationDamping;
	TGLDamping* FRotationDamping;
	bool FDampingEnabled;
	
protected:
	void __fastcall SetTranslationSpeed(Gls::Coordinates::TGLCoordinates3* const val);
	void __fastcall SetTranslationDamping(TGLDamping* const val);
	void __fastcall SetRotationDamping(TGLDamping* const val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	
public:
	__fastcall virtual TGLBInertia(Gls::Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLBInertia();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual bool __fastcall UniqueItem();
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	void __fastcall ApplyTranslationAcceleration(const double deltaTime, const Gls::Vectortypes::TVector4f &accel);
	void __fastcall ApplyForce(const double deltaTime, const Gls::Vectortypes::TVector4f &Force);
	void __fastcall ApplyTorque(const double deltaTime, const float turnTorque, const float rollTorque, const float pitchTorque);
	void __fastcall MirrorTranslation();
	void __fastcall SurfaceBounce(const Gls::Vectortypes::TVector4f &surfaceNormal, float restitution);
	
__published:
	__property float Mass = {read=FMass, write=FMass};
	__property Gls::Coordinates::TGLCoordinates3* TranslationSpeed = {read=FTranslationSpeed, write=SetTranslationSpeed};
	__property float TurnSpeed = {read=FTurnSpeed, write=FTurnSpeed};
	__property float RollSpeed = {read=FRollSpeed, write=FRollSpeed};
	__property float PitchSpeed = {read=FPitchSpeed, write=FPitchSpeed};
	__property bool DampingEnabled = {read=FDampingEnabled, write=FDampingEnabled, nodefault};
	__property TGLDamping* TranslationDamping = {read=FTranslationDamping, write=SetTranslationDamping};
	__property TGLDamping* RotationDamping = {read=FRotationDamping, write=SetRotationDamping};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBAcceleration : public Gls::Scene::TGLBehaviour
{
	typedef Gls::Scene::TGLBehaviour inherited;
	
private:
	Gls::Coordinates::TGLCoordinates3* FAcceleration;
	
protected:
	void __fastcall SetAcceleration(Gls::Coordinates::TGLCoordinates3* const val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	
public:
	__fastcall virtual TGLBAcceleration(Gls::Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLBAcceleration();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__classmethod virtual bool __fastcall UniqueItem();
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	
__published:
	__property Gls::Coordinates::TGLCoordinates3* Acceleration = {read=FAcceleration, write=FAcceleration};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLBInertia* __fastcall GetInertia(Gls::Scene::TGLBaseSceneObject* const AGLSceneObject);
extern DELPHI_PACKAGE TGLBInertia* __fastcall GetOrCreateInertia(Gls::Scene::TGLBehaviours* behaviours)/* overload */;
extern DELPHI_PACKAGE TGLBInertia* __fastcall GetOrCreateInertia(Gls::Scene::TGLBaseSceneObject* obj)/* overload */;
extern DELPHI_PACKAGE TGLBAcceleration* __fastcall GetOrCreateAcceleration(Gls::Scene::TGLBehaviours* behaviours)/* overload */;
extern DELPHI_PACKAGE TGLBAcceleration* __fastcall GetOrCreateAcceleration(Gls::Scene::TGLBaseSceneObject* obj)/* overload */;
}	/* namespace Behaviours */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_BEHAVIOURS)
using namespace Gls::Behaviours;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_BehavioursHPP
