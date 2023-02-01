// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Atmosphere.pas' rev: 35.00 (Windows)

#ifndef Gls_AtmosphereHPP
#define Gls_AtmosphereHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Objects.hpp>
#include <GLS.Cadencer.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Context.hpp>
#include <GLS.Strings.hpp>
#include <GLS.Color.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.State.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Atmosphere
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EGLAtmosphereException;
class DELPHICLASS TGLCustomAtmosphere;
class DELPHICLASS TGLAtmosphere;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION EGLAtmosphereException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLAtmosphereException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLAtmosphereException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLAtmosphereException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLAtmosphereException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLAtmosphereException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLAtmosphereException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLAtmosphereException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLAtmosphereException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLAtmosphereException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLAtmosphereException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLAtmosphereException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLAtmosphereException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLAtmosphereException() { }
	
};


enum DECLSPEC_DENUM TGLAtmosphereBlendingMode : unsigned char { abmOneMinusDstColor, abmOneMinusSrcAlpha };

class PASCALIMPLEMENTATION TGLCustomAtmosphere : public Gls::Scene::TGLBaseSceneObject
{
	typedef Gls::Scene::TGLBaseSceneObject inherited;
	
	
private:
	typedef System::DynamicArray<float> _TGLCustomAtmosphere__1;
	
	
private:
	_TGLCustomAtmosphere__1 cosCache;
	_TGLCustomAtmosphere__1 sinCache;
	Gls::Vectorgeometry::TVectorArray *pVertex;
	Gls::Vectorgeometry::TVectorArray *pColor;
	int FSlices;
	TGLAtmosphereBlendingMode FBlendingMode;
	float FPlanetRadius;
	float FAtmosphereRadius;
	float FOpacity;
	Gls::Color::TGLColor* FLowAtmColor;
	Gls::Color::TGLColor* FHighAtmColor;
	Gls::Scene::TGLBaseSceneObject* FSun;
	void __fastcall SetSun(Gls::Scene::TGLBaseSceneObject* const Value);
	void __fastcall SetAtmosphereRadius(const float Value);
	void __fastcall SetPlanetRadius(const float Value);
	void __fastcall EnableGLBlendingMode(Gls::State::TGLStateCache* StateCache);
	bool __fastcall StoreAtmosphereRadius();
	bool __fastcall StoreOpacity();
	bool __fastcall StorePlanetRadius();
	void __fastcall SetSlices(const int Value);
	void __fastcall SetLowAtmColor(Gls::Color::TGLColor* const AValue);
	void __fastcall SetHighAtmColor(Gls::Color::TGLColor* const AValue);
	bool __fastcall StoreLowAtmColor();
	bool __fastcall StoreHighAtmColor();
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__property Gls::Scene::TGLBaseSceneObject* Sun = {read=FSun, write=SetSun};
	__property int Slices = {read=FSlices, write=SetSlices, default=60};
	__property float Opacity = {read=FOpacity, write=FOpacity, stored=StoreOpacity};
	__property float AtmosphereRadius = {read=FAtmosphereRadius, write=SetAtmosphereRadius, stored=StoreAtmosphereRadius};
	__property float PlanetRadius = {read=FPlanetRadius, write=SetPlanetRadius, stored=StorePlanetRadius};
	__property Gls::Color::TGLColor* LowAtmColor = {read=FLowAtmColor, write=SetLowAtmColor, stored=StoreLowAtmColor};
	__property Gls::Color::TGLColor* HighAtmColor = {read=FHighAtmColor, write=SetHighAtmColor, stored=StoreHighAtmColor};
	__property TGLAtmosphereBlendingMode BlendingMode = {read=FBlendingMode, write=FBlendingMode, default=1};
	void __fastcall SetOptimalAtmosphere(const float ARadius);
	void __fastcall SetOptimalAtmosphere2(const float ARadius);
	void __fastcall TogleBlendingMode();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual TGLCustomAtmosphere(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomAtmosphere();
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLCustomAtmosphere(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLBaseSceneObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLAtmosphere : public TGLCustomAtmosphere
{
	typedef TGLCustomAtmosphere inherited;
	
__published:
	__property Sun;
	__property Slices = {default=60};
	__property Opacity = {default=0};
	__property AtmosphereRadius = {default=0};
	__property PlanetRadius = {default=0};
	__property LowAtmColor;
	__property HighAtmColor;
	__property BlendingMode = {default=1};
	__property Position;
	__property ObjectsSorting = {default=0};
	__property ShowAxes = {default=0};
	__property Visible = {default=1};
	__property OnProgress;
	__property Behaviours;
	__property Effects;
public:
	/* TGLCustomAtmosphere.Create */ inline __fastcall virtual TGLAtmosphere(System::Classes::TComponent* AOwner) : TGLCustomAtmosphere(AOwner) { }
	/* TGLCustomAtmosphere.Destroy */ inline __fastcall virtual ~TGLAtmosphere() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLAtmosphere(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLCustomAtmosphere(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Atmosphere */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_ATMOSPHERE)
using namespace Gls::Atmosphere;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_AtmosphereHPP
