// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.SkyDome.pas' rev: 35.00 (Windows)

#ifndef Gls_SkydomeHPP
#define Gls_SkydomeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.UITypes.hpp>
#include <System.Math.hpp>
#include <Vcl.Graphics.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Scene.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Color.hpp>
#include <GLS.Material.hpp>
#include <GLS.RenderContextInfo.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Skydome
{
//-- forward type declarations -----------------------------------------------
struct TGLStarRecord;
class DELPHICLASS TGLSkyBox;
class DELPHICLASS TGLSkyDomeBand;
class DELPHICLASS TGLSkyDomeBands;
class DELPHICLASS TGLSkyDomeStar;
class DELPHICLASS TGLSkyDomeStars;
class DELPHICLASS TGLSkyDome;
class DELPHICLASS TGLEarthSkyDome;
//-- type declarations -------------------------------------------------------
#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLStarRecord
{
public:
	System::Word RA;
	short DEC;
	System::Byte BVColorIndex;
	System::Byte VMagnitude;
};
#pragma pack(pop)


typedef TGLStarRecord *PGLStarRecord;

enum DECLSPEC_DENUM TGLSkyBoxStyle : unsigned char { sbsFull, sbsTopHalf, sbsBottomHalf, sbTopTwoThirds, sbsTopHalfClamped };

class PASCALIMPLEMENTATION TGLSkyBox : public Gls::Scene::TGLCameraInvariantObject
{
	typedef Gls::Scene::TGLCameraInvariantObject inherited;
	
private:
	System::UnicodeString FMatNameTop;
	System::UnicodeString FMatNameRight;
	System::UnicodeString FMatNameFront;
	System::UnicodeString FMatNameLeft;
	System::UnicodeString FMatNameBack;
	System::UnicodeString FMatNameBottom;
	System::UnicodeString FMatNameClouds;
	Gls::Material::TGLMaterialLibrary* FMaterialLibrary;
	float FCloudsPlaneOffset;
	float FCloudsPlaneSize;
	TGLSkyBoxStyle FStyle;
	Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	
protected:
	void __fastcall SetMaterialLibrary(Gls::Material::TGLMaterialLibrary* const Value);
	void __fastcall SetMatNameBack(const System::UnicodeString Value);
	void __fastcall SetMatNameBottom(const System::UnicodeString Value);
	void __fastcall SetMatNameFront(const System::UnicodeString Value);
	void __fastcall SetMatNameLeft(const System::UnicodeString Value);
	void __fastcall SetMatNameRight(const System::UnicodeString Value);
	void __fastcall SetMatNameTop(const System::UnicodeString Value);
	void __fastcall SetMatNameClouds(const System::UnicodeString Value);
	void __fastcall SetCloudsPlaneOffset(const float Value);
	void __fastcall SetCloudsPlaneSize(const float Value);
	void __fastcall SetStyle(const TGLSkyBoxStyle value);
	
public:
	__fastcall virtual TGLSkyBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSkyBox();
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
__published:
	__property Gls::Material::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property System::UnicodeString MatNameTop = {read=FMatNameTop, write=SetMatNameTop};
	__property System::UnicodeString MatNameBottom = {read=FMatNameBottom, write=SetMatNameBottom};
	__property System::UnicodeString MatNameLeft = {read=FMatNameLeft, write=SetMatNameLeft};
	__property System::UnicodeString MatNameRight = {read=FMatNameRight, write=SetMatNameRight};
	__property System::UnicodeString MatNameFront = {read=FMatNameFront, write=SetMatNameFront};
	__property System::UnicodeString MatNameBack = {read=FMatNameBack, write=SetMatNameBack};
	__property System::UnicodeString MatNameClouds = {read=FMatNameClouds, write=SetMatNameClouds};
	__property float CloudsPlaneOffset = {read=FCloudsPlaneOffset, write=SetCloudsPlaneOffset};
	__property float CloudsPlaneSize = {read=FCloudsPlaneSize, write=SetCloudsPlaneSize};
	__property TGLSkyBoxStyle Style = {read=FStyle, write=FStyle, default=0};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSkyBox(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLCameraInvariantObject(aParentOwner) { }
	
private:
	void *__IGLMaterialLibrarySupported;	// Gls::Material::IGLMaterialLibrarySupported 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator Gls::Material::_di_IGLMaterialLibrarySupported()
	{
		Gls::Material::_di_IGLMaterialLibrarySupported intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Gls::Material::IGLMaterialLibrarySupported*(void) { return (Gls::Material::IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};


class PASCALIMPLEMENTATION TGLSkyDomeBand : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	float FStartAngle;
	float FStopAngle;
	Gls::Color::TGLColor* FStartColor;
	Gls::Color::TGLColor* FStopColor;
	int FSlices;
	int FStacks;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	void __fastcall SetStartAngle(const float val);
	void __fastcall SetStartColor(Gls::Color::TGLColor* const val);
	void __fastcall SetStopAngle(const float val);
	void __fastcall SetStopColor(Gls::Color::TGLColor* const val);
	void __fastcall SetSlices(const int val);
	void __fastcall SetStacks(const int val);
	void __fastcall OnColorChange(System::TObject* sender);
	
public:
	__fastcall virtual TGLSkyDomeBand(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLSkyDomeBand();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
__published:
	__property float StartAngle = {read=FStartAngle, write=SetStartAngle};
	__property Gls::Color::TGLColor* StartColor = {read=FStartColor, write=SetStartColor};
	__property float StopAngle = {read=FStopAngle, write=SetStopAngle};
	__property Gls::Color::TGLColor* StopColor = {read=FStopColor, write=SetStopColor};
	__property int Slices = {read=FSlices, write=SetSlices, default=12};
	__property int Stacks = {read=FStacks, write=SetStacks, default=1};
};


class PASCALIMPLEMENTATION TGLSkyDomeBands : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TGLSkyDomeBand* operator[](int index) { return this->Items[index]; }
	
protected:
	System::Classes::TComponent* owner;
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	void __fastcall SetItems(int index, TGLSkyDomeBand* const val);
	TGLSkyDomeBand* __fastcall GetItems(int index);
	
public:
	__fastcall TGLSkyDomeBands(System::Classes::TComponent* AOwner);
	HIDESBASE TGLSkyDomeBand* __fastcall Add();
	HIDESBASE TGLSkyDomeBand* __fastcall FindItemID(int ID);
	__property TGLSkyDomeBand* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall NotifyChange();
	void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLSkyDomeBands() { }
	
};


class PASCALIMPLEMENTATION TGLSkyDomeStar : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	float FRA;
	float FDec;
	float FMagnitude;
	System::Uitypes::TColor FColor;
	Gls::Vectortypes::TVector3f FCacheCoord;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	
public:
	__fastcall virtual TGLSkyDomeStar(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLSkyDomeStar();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property float RA = {read=FRA, write=FRA};
	__property float Dec = {read=FDec, write=FDec};
	__property float Magnitude = {read=FMagnitude, write=FMagnitude};
	__property System::Uitypes::TColor Color = {read=FColor, write=FColor, nodefault};
};


class PASCALIMPLEMENTATION TGLSkyDomeStars : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TGLSkyDomeStar* operator[](int index) { return this->Items[index]; }
	
protected:
	System::Classes::TComponent* owner;
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	void __fastcall SetItems(int index, TGLSkyDomeStar* const val);
	TGLSkyDomeStar* __fastcall GetItems(int index);
	void __fastcall PrecomputeCartesianCoordinates();
	
public:
	__fastcall TGLSkyDomeStars(System::Classes::TComponent* AOwner);
	HIDESBASE TGLSkyDomeStar* __fastcall Add();
	HIDESBASE TGLSkyDomeStar* __fastcall FindItemID(int ID);
	__property TGLSkyDomeStar* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool twinkle);
	void __fastcall AddRandomStars(const int nb, const System::Uitypes::TColor color, const bool limitToTopDome = false)/* overload */;
	void __fastcall AddRandomStars(const int nb, const Gls::Vectortypes::TVector3b ColorMin, const Gls::Vectortypes::TVector3b ColorMax, const float Magnitude_min, const float Magnitude_max, const bool limitToTopDome = false)/* overload */;
	void __fastcall LoadStarsFile(const System::UnicodeString starsFileName);
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLSkyDomeStars() { }
	
};


enum DECLSPEC_DENUM TGLSkyDomeOption : unsigned char { sdoTwinkle };

typedef System::Set<TGLSkyDomeOption, TGLSkyDomeOption::sdoTwinkle, TGLSkyDomeOption::sdoTwinkle> TGLSkyDomeOptions;

class PASCALIMPLEMENTATION TGLSkyDome : public Gls::Scene::TGLCameraInvariantObject
{
	typedef Gls::Scene::TGLCameraInvariantObject inherited;
	
private:
	TGLSkyDomeOptions FOptions;
	TGLSkyDomeBands* FBands;
	TGLSkyDomeStars* FStars;
	
protected:
	void __fastcall SetBands(TGLSkyDomeBands* const val);
	void __fastcall SetStars(TGLSkyDomeStars* const val);
	void __fastcall SetOptions(const TGLSkyDomeOptions val);
	
public:
	__fastcall virtual TGLSkyDome(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSkyDome();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	
__published:
	__property TGLSkyDomeBands* Bands = {read=FBands, write=SetBands};
	__property TGLSkyDomeStars* Stars = {read=FStars, write=SetStars};
	__property TGLSkyDomeOptions Options = {read=FOptions, write=SetOptions, default=0};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSkyDome(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLCameraInvariantObject(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TEarthSkydomeOption : unsigned char { esoFadeStarsWithSun, esoRotateOnTwelveHours, esoDepthTest };

typedef System::Set<TEarthSkydomeOption, TEarthSkydomeOption::esoFadeStarsWithSun, TEarthSkydomeOption::esoDepthTest> TEarthSkydomeOptions;

class PASCALIMPLEMENTATION TGLEarthSkyDome : public TGLSkyDome
{
	typedef TGLSkyDome inherited;
	
private:
	float FSunElevation;
	float FTurbidity;
	Gls::Vectortypes::TVector4f FCurSunColor;
	Gls::Vectortypes::TVector4f FCurSkyColor;
	Gls::Vectortypes::TVector4f FCurHazeColor;
	float FCurHazeTurbid;
	float FCurSunSkyTurbid;
	Gls::Color::TGLColor* FSunZenithColor;
	Gls::Color::TGLColor* FSunDawnColor;
	Gls::Color::TGLColor* FHazeColor;
	Gls::Color::TGLColor* FSkyColor;
	Gls::Color::TGLColor* FNightColor;
	Gls::Color::TGLColor* FDeepColor;
	int FSlices;
	int FStacks;
	TEarthSkydomeOptions FExtendedOptions;
	bool FMorning;
	
protected:
	virtual void __fastcall Loaded();
	void __fastcall SetSunElevation(const float val);
	void __fastcall SetTurbidity(const float val);
	void __fastcall SetSunZenithColor(Gls::Color::TGLColor* const val);
	void __fastcall SetSunDawnColor(Gls::Color::TGLColor* const val);
	void __fastcall SetHazeColor(Gls::Color::TGLColor* const val);
	void __fastcall SetSkyColor(Gls::Color::TGLColor* const val);
	void __fastcall SetNightColor(Gls::Color::TGLColor* const val);
	void __fastcall SetDeepColor(Gls::Color::TGLColor* const val);
	void __fastcall SetSlices(const int val);
	void __fastcall SetStacks(const int val);
	void __fastcall OnColorChanged(System::TObject* Sender);
	void __fastcall PreCalculate();
	void __fastcall RenderDome();
	Gls::Vectortypes::TVector4f __fastcall CalculateColor(const float theta, const float cosGamma);
	
public:
	__fastcall virtual TGLEarthSkyDome(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLEarthSkyDome();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall SetSunAtTime(float HH, float MM);
	
__published:
	__property float SunElevation = {read=FSunElevation, write=SetSunElevation};
	__property float Turbidity = {read=FTurbidity, write=SetTurbidity};
	__property Gls::Color::TGLColor* SunZenithColor = {read=FSunZenithColor, write=SetSunZenithColor};
	__property Gls::Color::TGLColor* SunDawnColor = {read=FSunDawnColor, write=SetSunDawnColor};
	__property Gls::Color::TGLColor* HazeColor = {read=FHazeColor, write=SetHazeColor};
	__property Gls::Color::TGLColor* SkyColor = {read=FSkyColor, write=SetSkyColor};
	__property Gls::Color::TGLColor* NightColor = {read=FNightColor, write=SetNightColor};
	__property Gls::Color::TGLColor* DeepColor = {read=FDeepColor, write=SetDeepColor};
	__property TEarthSkydomeOptions ExtendedOptions = {read=FExtendedOptions, write=FExtendedOptions, nodefault};
	__property int Slices = {read=FSlices, write=SetSlices, default=24};
	__property int Stacks = {read=FStacks, write=SetStacks, default=48};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLEarthSkyDome(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TGLSkyDome(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Gls::Vectortypes::TVector3f __fastcall StarRecordPositionYUp(const TGLStarRecord &starRecord);
extern DELPHI_PACKAGE Gls::Vectortypes::TVector3f __fastcall StarRecordPositionZUp(const TGLStarRecord &starRecord);
extern DELPHI_PACKAGE Gls::Vectortypes::TVector4f __fastcall StarRecordColor(const TGLStarRecord &starRecord, float bias);
}	/* namespace Skydome */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_SKYDOME)
using namespace Gls::Skydome;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_SkydomeHPP
