// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.RenderContextInfo.pas' rev: 35.00 (Windows)

#ifndef Gls_RendercontextinfoHPP
#define Gls_RendercontextinfoHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.State.hpp>
#include <GLS.PipelineTransformation.hpp>
#include <GLS.Color.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Rendercontextinfo
{
//-- forward type declarations -----------------------------------------------
struct TGLSize;
struct TGLRenderContextClippingInfo;
struct TGLRenderContextInfo;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLDrawState : unsigned char { dsRendering, dsPicking, dsPrinting };

struct DECLSPEC_DRECORD TGLSize
{
public:
	int cx;
	int cy;
};


enum DECLSPEC_DENUM TGLObjectsSorting : unsigned char { osInherited, osNone, osRenderFarthestFirst, osRenderBlendedLast, osRenderNearestFirst };

enum DECLSPEC_DENUM TGLVisibilityCulling : unsigned char { vcInherited, vcNone, vcObjectBased, vcHierarchical };

struct DECLSPEC_DRECORD TGLRenderContextClippingInfo
{
public:
	Gls::Vectortypes::TVector4f Origin;
	Gls::Vectortypes::TVector4f ClippingDirection;
	float ViewPortRadius;
	float NearClippingDistance;
	float FarClippingDistance;
	Gls::Vectorgeometry::TFrustum Frustum;
};


struct DECLSPEC_DRECORD TGLRenderContextInfo
{
public:
	System::TObject* Scene;
	System::TObject* Buffer;
	Gls::Vectortypes::TVector4f CameraPosition;
	Gls::Vectortypes::TVector4f CameraDirection;
	Gls::Vectortypes::TVector4f CameraUp;
	TGLSize ViewPortSize;
	int RenderDPI;
	System::TObject* MaterialLibrary;
	System::TObject* LightMapLibrary;
	int FogDisabledCounter;
	TGLDrawState DrawState;
	TGLObjectsSorting ObjectsSorting;
	TGLVisibilityCulling VisibilityCulling;
	Gls::State::TGLStateCache* GLStates;
	Gls::Pipelinetransformation::TGLTransformation* PipelineTransformation;
	TGLRenderContextClippingInfo Rcci;
	Gls::Vectortypes::TVector4f SceneAmbientColor;
	bool BufferFaceCull;
	bool BufferLighting;
	bool BufferFog;
	bool BufferDepthTest;
	bool ProxySubObject;
	bool IgnoreMaterials;
	bool IgnoreBlendingRequests;
	bool IgnoreDepthRequests;
	bool Amalgamating;
	Gls::Persistentclasses::TGLPersistentObjectList* Lights;
	Gls::Persistentclasses::TGLPersistentObjectList* AfterRenderEffects;
	Gls::State::TGLMaterialLevel CurrentMaterialLevel;
	Gls::State::TGLMeshPrimitives PrimitiveMask;
	int OrderCounter;
};


typedef TGLRenderContextInfo *PGLRenderContextInfo;

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Rendercontextinfo */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_RENDERCONTEXTINFO)
using namespace Gls::Rendercontextinfo;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_RendercontextinfoHPP
