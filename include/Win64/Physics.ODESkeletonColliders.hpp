// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Physics.ODESkeletonColliders.pas' rev: 35.00 (Windows)

#ifndef Physics_OdeskeletoncollidersHPP
#define Physics_OdeskeletoncollidersHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <Physics.ODEImport.hpp>

//-- user supplied -----------------------------------------------------------

namespace Physics
{
namespace Odeskeletoncolliders
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSCODEBase;
class DELPHICLASS TSCODESphere;
class DELPHICLASS TSCODECCylinder;
class DELPHICLASS TSCODEBox;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TSCODEBase : public Gls::Vectorfileobjects::TGLSkeletonCollider
{
	typedef Gls::Vectorfileobjects::TGLSkeletonCollider inherited;
	
private:
	Physics::Odeimport::TdxGeom *FGeom;
	
public:
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* reader);
	virtual void __fastcall AddToSpace(Physics::Odeimport::PdxSpace Space);
	virtual void __fastcall AlignCollider();
	__property Physics::Odeimport::PdxGeom Geom = {read=FGeom};
public:
	/* TGLSkeletonCollider.Create */ inline __fastcall virtual TSCODEBase() : Gls::Vectorfileobjects::TGLSkeletonCollider() { }
	/* TGLSkeletonCollider.CreateOwned */ inline __fastcall TSCODEBase(Gls::Vectorfileobjects::TGLSkeletonColliderList* AOwner) : Gls::Vectorfileobjects::TGLSkeletonCollider(AOwner) { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TSCODEBase(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Vectorfileobjects::TGLSkeletonCollider(reader) { }
	/* TGLPersistentObject.Destroy */ inline __fastcall virtual ~TSCODEBase() { }
	
};


class PASCALIMPLEMENTATION TSCODESphere : public TSCODEBase
{
	typedef TSCODEBase inherited;
	
private:
	float FRadius;
	
protected:
	void __fastcall SetRadius(const float val);
	
public:
	__fastcall virtual TSCODESphere();
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* reader);
	virtual void __fastcall AddToSpace(Physics::Odeimport::PdxSpace Space);
	__property float Radius = {read=FRadius, write=SetRadius};
public:
	/* TGLSkeletonCollider.CreateOwned */ inline __fastcall TSCODESphere(Gls::Vectorfileobjects::TGLSkeletonColliderList* AOwner) : TSCODEBase(AOwner) { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TSCODESphere(Gls::Persistentclasses::TGLVirtualReader* reader) : TSCODEBase(reader) { }
	/* TGLPersistentObject.Destroy */ inline __fastcall virtual ~TSCODESphere() { }
	
};


class PASCALIMPLEMENTATION TSCODECCylinder : public TSCODEBase
{
	typedef TSCODEBase inherited;
	
private:
	float FRadius;
	float FLength;
	
protected:
	void __fastcall SetRadius(const float val);
	void __fastcall SetLength(const float val);
	
public:
	__fastcall virtual TSCODECCylinder();
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* reader);
	virtual void __fastcall AddToSpace(Physics::Odeimport::PdxSpace Space);
	__property float Radius = {read=FRadius, write=SetRadius};
	__property float Length = {read=FLength, write=SetLength};
public:
	/* TGLSkeletonCollider.CreateOwned */ inline __fastcall TSCODECCylinder(Gls::Vectorfileobjects::TGLSkeletonColliderList* AOwner) : TSCODEBase(AOwner) { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TSCODECCylinder(Gls::Persistentclasses::TGLVirtualReader* reader) : TSCODEBase(reader) { }
	/* TGLPersistentObject.Destroy */ inline __fastcall virtual ~TSCODECCylinder() { }
	
};


class PASCALIMPLEMENTATION TSCODEBox : public TSCODEBase
{
	typedef TSCODEBase inherited;
	
private:
	float FBoxWidth;
	float FBoxHeight;
	float FBoxDepth;
	
protected:
	void __fastcall SetBoxWidth(const float val);
	void __fastcall SetBoxHeight(const float val);
	void __fastcall SetBoxDepth(const float val);
	
public:
	__fastcall virtual TSCODEBox();
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* reader);
	virtual void __fastcall AddToSpace(Physics::Odeimport::PdxSpace Space);
	__property float BoxWidth = {read=FBoxWidth, write=SetBoxWidth};
	__property float BoxHeight = {read=FBoxHeight, write=SetBoxHeight};
	__property float BoxDepth = {read=FBoxDepth, write=SetBoxDepth};
public:
	/* TGLSkeletonCollider.CreateOwned */ inline __fastcall TSCODEBox(Gls::Vectorfileobjects::TGLSkeletonColliderList* AOwner) : TSCODEBase(AOwner) { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TSCODEBox(Gls::Persistentclasses::TGLVirtualReader* reader) : TSCODEBase(reader) { }
	/* TGLPersistentObject.Destroy */ inline __fastcall virtual ~TSCODEBox() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall AddSCODEGeomsToODESpace(Gls::Vectorfileobjects::TGLSkeletonColliderList* colliders, Physics::Odeimport::PdxSpace Space);
}	/* namespace Odeskeletoncolliders */
}	/* namespace Physics */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS_ODESKELETONCOLLIDERS)
using namespace Physics::Odeskeletoncolliders;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS)
using namespace Physics;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Physics_OdeskeletoncollidersHPP
