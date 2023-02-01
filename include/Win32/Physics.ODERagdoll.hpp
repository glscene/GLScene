// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Physics.ODERagdoll.pas' rev: 35.00 (Windows)

#ifndef Physics_OderagdollHPP
#define Physics_OderagdollHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Ragdoll.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Objects.hpp>
#include <GLS.Texture.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <Physics.ODEImport.hpp>
#include <Physics.ODEUtils.hpp>
#include <System.Classes.hpp>
#include <GLS.PersistentClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Physics
{
namespace Oderagdoll
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLODERagdollCube;
class DELPHICLASS TGLODERagdollWorld;
class DELPHICLASS TGLODERagdollDummyJoint;
class DELPHICLASS TGLODERagdollHingeJoint;
class DELPHICLASS TGLODERagdollUniversalJoint;
class DELPHICLASS TGLODERagdollBone;
class DELPHICLASS TGLODERagdoll;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLODERagdollCube : public Gls::Objects::TGLCube
{
	typedef Gls::Objects::TGLCube inherited;
	
public:
	TGLODERagdollBone* Bone;
	TGLODERagdoll* Ragdoll;
public:
	/* TGLCube.Create */ inline __fastcall virtual TGLODERagdollCube(System::Classes::TComponent* AOwner) : Gls::Objects::TGLCube(AOwner) { }
	
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLODERagdollCube() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLODERagdollCube(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Objects::TGLCube(aParentOwner) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLODERagdollWorld : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Physics::Odeimport::TdxSpace *FSpace;
	Physics::Odeimport::TdxWorld *FWorld;
	Physics::Odeimport::TdxJointGroup *FContactGroup;
	TGLODERagdoll* FRagdoll;
	bool isWorldCreated;
	
public:
	__fastcall TGLODERagdollWorld();
	__fastcall TGLODERagdollWorld(Physics::Odeimport::PdxWorld World, Physics::Odeimport::PdxSpace Space, Physics::Odeimport::PdxJointGroup ContactGroup);
	__fastcall virtual ~TGLODERagdollWorld();
	void __fastcall WorldUpdate();
	__property Physics::Odeimport::PdxWorld World = {read=FWorld};
	__property Physics::Odeimport::PdxSpace Space = {read=FSpace};
	__property Physics::Odeimport::PdxJointGroup ContactGroup = {read=FContactGroup};
	__property TGLODERagdoll* Ragdoll = {read=FRagdoll};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLODERagdollDummyJoint : public Gls::Ragdoll::TGLRagdolJoint
{
	typedef Gls::Ragdoll::TGLRagdolJoint inherited;
	
public:
	/* TObject.Create */ inline __fastcall TGLODERagdollDummyJoint() : Gls::Ragdoll::TGLRagdolJoint() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLODERagdollDummyJoint() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLODERagdollHingeJoint : public Gls::Ragdoll::TGLRagdolJoint
{
	typedef Gls::Ragdoll::TGLRagdolJoint inherited;
	
private:
	float FParamHiStop;
	float FParamLoStop;
	Gls::Vectortypes::TVector3f FAxis;
	
public:
	__fastcall TGLODERagdollHingeJoint(const Gls::Vectortypes::TVector3f &Axis, float ParamLoStop, float ParamHiStop);
	__property Gls::Vectortypes::TVector3f Axis = {read=FAxis};
	__property float ParamLoStop = {read=FParamLoStop, write=FParamLoStop};
	__property float ParamHiStop = {read=FParamHiStop, write=FParamHiStop};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLODERagdollHingeJoint() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLODERagdollUniversalJoint : public TGLODERagdollHingeJoint
{
	typedef TGLODERagdollHingeJoint inherited;
	
private:
	float FParamHiStop2;
	float FParamLoStop2;
	Gls::Vectortypes::TVector3f FAxis2;
	
public:
	__fastcall TGLODERagdollUniversalJoint(const Gls::Vectortypes::TVector3f &Axis, float ParamLoStop, float ParamHiStop, const Gls::Vectortypes::TVector3f &Axis2, float ParamLoStop2, float ParamHiStop2);
	__property Gls::Vectortypes::TVector3f Axis2 = {read=FAxis2};
	__property float ParamLoStop2 = {read=FParamLoStop2, write=FParamLoStop2};
	__property float ParamHiStop2 = {read=FParamHiStop2, write=FParamHiStop2};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLODERagdollUniversalJoint() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLODERagdollBone : public Gls::Ragdoll::TGLRagdolBone
{
	typedef Gls::Ragdoll::TGLRagdolBone inherited;
	
private:
	TGLODERagdollBone* FOwner;
	TGLODERagdoll* FRagdoll;
	Physics::Odeimport::TdxBody *FBody;
	Physics::Odeimport::TdxGeom *FGeom;
	Physics::Odeimport::TdxJoint *FJointId;
	void __fastcall AlignBodyToMatrix(const Gls::Vectortypes::TMatrix4f &Mat);
	
protected:
	virtual void __fastcall Start();
	virtual void __fastcall Align();
	virtual void __fastcall Update();
	virtual void __fastcall Stop();
	
public:
	__fastcall TGLODERagdollBone(TGLODERagdollBone* aOwner);
	__fastcall TGLODERagdollBone(TGLODERagdoll* Ragdoll);
	__property Physics::Odeimport::PdxBody Body = {read=FBody};
	__property Physics::Odeimport::PdxGeom Geom = {read=FGeom};
public:
	/* TGLRagdolBone.Destroy */ inline __fastcall virtual ~TGLODERagdollBone() { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLODERagdollBone(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Ragdoll::TGLRagdolBone(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLODERagdoll : public Gls::Ragdoll::TGLRagdoll
{
	typedef Gls::Ragdoll::TGLRagdoll inherited;
	
private:
	TGLODERagdollWorld* FODEWorld;
	Gls::Scene::TGLBaseSceneObject* FGLSceneRoot;
	bool FShowBoundingBoxes;
	bool FEnabled;
	
public:
	__fastcall TGLODERagdoll(Gls::Vectorfileobjects::TGLBaseMesh* aOwner);
	__property TGLODERagdollWorld* ODEWorld = {read=FODEWorld, write=FODEWorld};
	__property Gls::Scene::TGLBaseSceneObject* GLSceneRoot = {read=FGLSceneRoot, write=FGLSceneRoot};
	__property bool ShowBoundingBoxes = {read=FShowBoundingBoxes, write=FShowBoundingBoxes, nodefault};
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
public:
	/* TGLRagdoll.Destroy */ inline __fastcall virtual ~TGLODERagdoll() { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLODERagdoll(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Ragdoll::TGLRagdoll(reader) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 cMaxContacts = System::Int8(0x4);
extern DELPHI_PACKAGE float vODERagdoll_cDensity;
extern DELPHI_PACKAGE float vODERagdoll_cMass;
}	/* namespace Oderagdoll */
}	/* namespace Physics */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS_ODERAGDOLL)
using namespace Physics::Oderagdoll;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS)
using namespace Physics;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Physics_OderagdollHPP
