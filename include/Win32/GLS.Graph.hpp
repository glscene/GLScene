// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Graph.pas' rev: 35.00 (Windows)

#ifndef Gls_GraphHPP
#define Gls_GraphHPP

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
#include <GLS.Scene.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Context.hpp>
#include <GLS.XOpenGL.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Material.hpp>
#include <GLS.Objects.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.Color.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.State.hpp>
#include <GLS.VectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Graph
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLSamplingScale;
class DELPHICLASS TGLHeightField;
class DELPHICLASS TGLXYZGrid;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLSamplingScale : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	float FMin;
	float FMax;
	float FOrigin;
	float FStep;
	
protected:
	void __fastcall SetMin(const float val);
	void __fastcall SetMax(const float val);
	void __fastcall SetOrigin(const float val);
	void __fastcall SetStep(const float val);
	
public:
	__fastcall virtual TGLSamplingScale(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLSamplingScale();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	float __fastcall StepBase();
	int __fastcall MaxStepCount();
	bool __fastcall IsValid();
	void __fastcall SetBaseStepMaxToVars(float &Base, float &Step, float &Max, bool SamplingEnabled = true);
	
__published:
	__property float Min = {read=FMin, write=SetMin};
	__property float Max = {read=FMax, write=SetMax};
	__property float Origin = {read=FOrigin, write=SetOrigin};
	__property float Step = {read=FStep, write=SetStep};
};


typedef void __fastcall (__closure *TGLHeightFieldGetHeightEvent)(const float x, const float y, float &z, Gls::Vectortypes::TVector4f &Color, Gls::Vectorgeometry::TTexPoint &TexPoint);

typedef void __fastcall (__closure *TGLHeightFieldGetHeight2Event)(System::TObject* Sender, const float x, const float y, float &z, Gls::Vectortypes::TVector4f &Color, Gls::Vectorgeometry::TTexPoint &TexPoint);

enum DECLSPEC_DENUM TGLHeightFieldOption : unsigned char { hfoTextureCoordinates, hfoTwoSided };

typedef System::Set<TGLHeightFieldOption, TGLHeightFieldOption::hfoTextureCoordinates, TGLHeightFieldOption::hfoTwoSided> TGLHeightFieldOptions;

enum DECLSPEC_DENUM TGLHeightFieldColorMode : unsigned char { hfcmNone, hfcmEmission, hfcmAmbient, hfcmDiffuse, hfcmAmbientAndDiffuse };

class PASCALIMPLEMENTATION TGLHeightField : public Gls::Scene::TGLSceneObject
{
	typedef Gls::Scene::TGLSceneObject inherited;
	
private:
	TGLHeightFieldGetHeightEvent FOnGetHeight;
	TGLHeightFieldGetHeight2Event FOnGetHeight2;
	TGLSamplingScale* FXSamplingScale;
	TGLSamplingScale* FYSamplingScale;
	TGLHeightFieldOptions FOptions;
	int FTriangleCount;
	TGLHeightFieldColorMode FColorMode;
	
protected:
	void __fastcall SetXSamplingScale(TGLSamplingScale* const val);
	void __fastcall SetYSamplingScale(TGLSamplingScale* const val);
	void __fastcall SetOptions(const TGLHeightFieldOptions val);
	void __fastcall SetOnGetHeight(const TGLHeightFieldGetHeightEvent val);
	void __fastcall SetOnGetHeight2(const TGLHeightFieldGetHeight2Event val);
	void __fastcall SetColorMode(const TGLHeightFieldColorMode val);
	void __fastcall DefaultHeightField(const float x, const float y, float &z, Gls::Vectortypes::TVector4f &Color, Gls::Vectorgeometry::TTexPoint &TexPoint);
	void __fastcall Height2Field(const float x, const float y, float &z, Gls::Vectortypes::TVector4f &Color, Gls::Vectorgeometry::TTexPoint &texPoint);
	
public:
	__fastcall virtual TGLHeightField(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLHeightField();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	__property int TriangleCount = {read=FTriangleCount, nodefault};
	
__published:
	__property TGLSamplingScale* XSamplingScale = {read=FXSamplingScale, write=SetXSamplingScale};
	__property TGLSamplingScale* YSamplingScale = {read=FYSamplingScale, write=SetYSamplingScale};
	__property TGLHeightFieldColorMode ColorMode = {read=FColorMode, write=SetColorMode, default=0};
	__property TGLHeightFieldOptions Options = {read=FOptions, write=SetOptions, default=2};
	__property TGLHeightFieldGetHeightEvent OnGetHeight = {read=FOnGetHeight, write=SetOnGetHeight};
	__property TGLHeightFieldGetHeight2Event OnGetHeight2 = {read=FOnGetHeight2, write=SetOnGetHeight2};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLHeightField(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLSceneObject(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TGLXYZGridPart : unsigned char { gpX, gpY, gpZ };

typedef System::Set<TGLXYZGridPart, TGLXYZGridPart::gpX, TGLXYZGridPart::gpZ> TGLXYZGridParts;

enum DECLSPEC_DENUM TGLXYZGridLinesStyle : unsigned char { glsLine, glsSegments };

class PASCALIMPLEMENTATION TGLXYZGrid : public Gls::Objects::TGLLineBase
{
	typedef Gls::Objects::TGLLineBase inherited;
	
private:
	TGLSamplingScale* FXSamplingScale;
	TGLSamplingScale* FYSamplingScale;
	TGLSamplingScale* FZSamplingScale;
	TGLXYZGridParts FParts;
	TGLXYZGridLinesStyle FLinesStyle;
	
protected:
	void __fastcall SetXSamplingScale(TGLSamplingScale* const val);
	void __fastcall SetYSamplingScale(TGLSamplingScale* const val);
	void __fastcall SetZSamplingScale(TGLSamplingScale* const val);
	void __fastcall SetParts(const TGLXYZGridParts val);
	void __fastcall SetLinesStyle(const TGLXYZGridLinesStyle val);
	void __fastcall SetLinesSmoothing(const bool val);
	
public:
	__fastcall virtual TGLXYZGrid(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLXYZGrid();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	
__published:
	__property TGLSamplingScale* XSamplingScale = {read=FXSamplingScale, write=SetXSamplingScale};
	__property TGLSamplingScale* YSamplingScale = {read=FYSamplingScale, write=SetYSamplingScale};
	__property TGLSamplingScale* ZSamplingScale = {read=FZSamplingScale, write=SetZSamplingScale};
	__property TGLXYZGridParts Parts = {read=FParts, write=SetParts, default=3};
	__property TGLXYZGridLinesStyle LinesStyle = {read=FLinesStyle, write=SetLinesStyle, default=1};
	__property bool LinesSmoothing = {write=SetLinesSmoothing, stored=false, nodefault};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLXYZGrid(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Objects::TGLLineBase(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Graph */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_GRAPH)
using namespace Gls::Graph;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_GraphHPP
