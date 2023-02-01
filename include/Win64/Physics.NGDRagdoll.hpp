// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Physics.NGDRagdoll.pas' rev: 35.00 (Windows)

#ifndef Physics_NgdragdollHPP
#define Physics_NgdragdollHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <Physics.NGDImport.hpp>

//-- user supplied -----------------------------------------------------------

namespace Physics
{
namespace Ngdragdoll
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNewtonRagdoll;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TNewtonRagdoll : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<void *> _TNewtonRagdoll__1;
	
	typedef System::DynamicArray<Gls::Vectortypes::TMatrix4f> _TNewtonRagdoll__2;
	
	struct DECLSPEC_DRECORD _TNewtonRagdoll__3
	{
	public:
		System::Byte kind;
		Gls::Vectortypes::TMatrix4f Mat;
		Gls::Vectortypes::TVector3f Pt;
		float W;
		float D;
		float H;
		float Mass;
	};
	
	
	typedef System::DynamicArray<_TNewtonRagdoll__3> _TNewtonRagdoll__4;
	
	
private:
	float FERP;
	float FSlideLimit;
	float FAngleLimit;
	bool FEnabled;
	void *newtonworld;
	void __fastcall SetSlideLimit(float value);
	void __fastcall SetAngleLimit(float value);
	void __fastcall SetERP(float value);
	void __fastcall SetEnabled(bool value);
	void __fastcall Clean();
	
public:
	Gls::Vectorfileobjects::TGLActor* Actor;
	System::Classes::TList* Bodies;
	_TNewtonRagdoll__1 Joints;
	_TNewtonRagdoll__2 Norm_matrices;
	_TNewtonRagdoll__4 Envelopes;
	__property bool Enabled = {read=FEnabled, write=SetEnabled, nodefault};
	__property float SlideLimit = {read=FSlideLimit, write=SetSlideLimit};
	__property float AngleLimit = {read=FAngleLimit, write=SetAngleLimit};
	__property float ERP = {read=FERP, write=SetERP};
	__fastcall TNewtonRagdoll(Gls::Vectorfileobjects::TGLActor* model, void * world, float min_env_size, float slide_limit, float erp_, float angle_limit, bool full);
	void __fastcall Conform();
	__fastcall virtual ~TNewtonRagdoll();
	void __fastcall LoadFromFile(System::UnicodeString filename);
	void __fastcall SaveToFile(System::UnicodeString filename);
	Gls::Vectortypes::TVector4f __fastcall TranslatePos(int n, bool add);
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE int __fastcall GetBoneParent(Gls::Vectorfileobjects::TGLActor* Actor, int bone);
}	/* namespace Ngdragdoll */
}	/* namespace Physics */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS_NGDRAGDOLL)
using namespace Physics::Ngdragdoll;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS)
using namespace Physics;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Physics_NgdragdollHPP
