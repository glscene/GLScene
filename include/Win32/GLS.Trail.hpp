// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Trail.pas' rev: 35.00 (Windows)

#ifndef Gls_TrailHPP
#define Gls_TrailHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.Scene.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.MeshUtils.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.Mesh.hpp>
#include <GLS.Objects.hpp>
#include <GLS.Material.hpp>
#include <GLS.Strings.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Trail
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTrail;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TMarkStyle : unsigned char { msUp, msDirection, msFaceCamera, msRight };

class PASCALIMPLEMENTATION TGLTrail : public Gls::Mesh::TGLMesh
{
	typedef Gls::Mesh::TGLMesh inherited;
	
private:
	int fVertLimit;
	float fTimeLimit;
	float fMinDistance;
	float fAlpha;
	bool fAlphaFade;
	float fUVScale;
	System::StaticArray<Gls::Vectortypes::TVector3f, 2000> fVerts;
	System::StaticArray<Gls::Vectorgeometry::TTexPoint, 2000> fUVs;
	System::StaticArray<double, 2000> fTimeStamps;
	int fVertStart;
	int fVertEnd;
	int fVertCount;
	Gls::Vectortypes::TVector3f fLastV0Pos;
	Gls::Vectortypes::TVector3f fLastPos;
	Gls::Vectortypes::TVector3f fLastDir;
	Gls::Vectortypes::TVector3f fLastUp;
	float FLastUVs;
	Gls::Vectortypes::TVector3f fLastP1;
	Gls::Vectortypes::TVector3f fLastP2;
	Gls::Scene::TGLBaseSceneObject* FTrailObject;
	TMarkStyle FMarkStyle;
	float FMarkWidth;
	bool FEnabled;
	float FAntiZFightOffset;
	void __fastcall SetTrailObject(Gls::Scene::TGLBaseSceneObject* const Value);
	void __fastcall SetMarkStyle(const TMarkStyle Value);
	void __fastcall SetAlpha(const float Value);
	void __fastcall SetAlphaFade(const bool Value);
	void __fastcall SetMinDistance(const float Value);
	void __fastcall SetTimeLimit(const float Value);
	void __fastcall SetUVScale(const float Value);
	void __fastcall SetVertLimit(const int Value);
	void __fastcall SetMarkWidth(const float Value);
	void __fastcall SetEnabled(const bool Value);
	bool __fastcall StoreAntiZFightOffset();
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	__fastcall virtual TGLTrail(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTrail();
	void __fastcall CreateMark(Gls::Scene::TGLBaseSceneObject* obj, float width, double CurrentTime)/* overload */;
	void __fastcall CreateMark(const Gls::Vectortypes::TVector3f &APos, const Gls::Vectortypes::TVector3f &ADir, const Gls::Vectortypes::TVector3f &AUp, float AWidth, double ACurrentTime)/* overload */;
	bool __fastcall CreateMark(const Gls::Vectortypes::TVector3f &p1, const Gls::Vectortypes::TVector3f &p2, double CurrentTime)/* overload */;
	void __fastcall ClearMarks();
	
__published:
	__property float AntiZFightOffset = {read=FAntiZFightOffset, write=FAntiZFightOffset, stored=StoreAntiZFightOffset};
	__property int VertLimit = {read=fVertLimit, write=SetVertLimit, default=150};
	__property float TimeLimit = {read=fTimeLimit, write=SetTimeLimit};
	__property float MinDistance = {read=fMinDistance, write=SetMinDistance};
	__property float Alpha = {read=fAlpha, write=SetAlpha};
	__property bool AlphaFade = {read=fAlphaFade, write=SetAlphaFade, default=1};
	__property float UVScale = {read=fUVScale, write=SetUVScale};
	__property TMarkStyle MarkStyle = {read=FMarkStyle, write=SetMarkStyle, default=2};
	__property Gls::Scene::TGLBaseSceneObject* TrailObject = {read=FTrailObject, write=SetTrailObject, default=0};
	__property float MarkWidth = {read=FMarkWidth, write=SetMarkWidth};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTrail(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Mesh::TGLMesh(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word cMaxVerts = System::Word(0x7d0);
}	/* namespace Trail */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_TRAIL)
using namespace Gls::Trail;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_TrailHPP
