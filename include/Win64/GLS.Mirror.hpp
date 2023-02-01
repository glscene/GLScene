// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Mirror.pas' rev: 35.00 (Windows)

#ifndef Gls_MirrorHPP
#define Gls_MirrorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.Classes.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.OpenGLAdapter.hpp>
#include <GLS.Scene.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Context.hpp>
#include <GLS.Material.hpp>
#include <GLS.Color.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.State.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.PipelineTransformation.hpp>
#include <GLS.XCollection.hpp>
#include <GLS.Texture.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Mirror
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLMirror;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLMirrorOption : unsigned char { moUseStencil, moOpaque, moMirrorPlaneClip, moClearZBuffer };

typedef System::Set<TGLMirrorOption, TGLMirrorOption::moUseStencil, TGLMirrorOption::moClearZBuffer> TGLMirrorOptions;

enum DECLSPEC_DENUM TMirrorShapes : unsigned char { msRect, msDisk };

class PASCALIMPLEMENTATION TGLMirror : public Gls::Scene::TGLSceneObject
{
	typedef Gls::Scene::TGLSceneObject inherited;
	
private:
	bool FRendering;
	Gls::Scene::TGLBaseSceneObject* FMirrorObject;
	float FWidth;
	float FHeight;
	TGLMirrorOptions FMirrorOptions;
	System::Classes::TNotifyEvent FOnBeginRenderingMirrors;
	System::Classes::TNotifyEvent FOnEndRenderingMirrors;
	TMirrorShapes FShape;
	float FRadius;
	int FSlices;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetMirrorObject(Gls::Scene::TGLBaseSceneObject* const val);
	void __fastcall SetMirrorOptions(const TGLMirrorOptions val);
	void __fastcall ClearZBufferArea(Gls::Scene::TGLSceneBuffer* aBuffer);
	void __fastcall SetHeight(float AValue);
	void __fastcall SetWidth(float AValue);
	void __fastcall SetRadius(const float aValue);
	void __fastcall SetSlices(const int aValue);
	void __fastcall SetShape(TMirrorShapes aValue);
	float __fastcall GetRadius();
	int __fastcall GetSlices();
	
public:
	__fastcall virtual TGLMirror(System::Classes::TComponent* AOwner);
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	
__published:
	__property Gls::Scene::TGLBaseSceneObject* MirrorObject = {read=FMirrorObject, write=SetMirrorObject};
	__property TGLMirrorOptions MirrorOptions = {read=FMirrorOptions, write=SetMirrorOptions, default=1};
	__property float Height = {read=FHeight, write=SetHeight};
	__property float Width = {read=FWidth, write=SetWidth};
	__property System::Classes::TNotifyEvent OnBeginRenderingMirrors = {read=FOnBeginRenderingMirrors, write=FOnBeginRenderingMirrors};
	__property System::Classes::TNotifyEvent OnEndRenderingMirrors = {read=FOnEndRenderingMirrors, write=FOnEndRenderingMirrors};
	__property float Radius = {read=FRadius, write=SetRadius};
	__property int Slices = {read=FSlices, write=SetSlices, default=16};
	__property TMirrorShapes Shape = {read=FShape, write=SetShape, default=0};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLMirror() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLMirror(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define cDefaultMirrorOptions (System::Set<TGLMirrorOption, TGLMirrorOption::moUseStencil, TGLMirrorOption::moClearZBuffer>() << TGLMirrorOption::moUseStencil )
}	/* namespace Mirror */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_MIRROR)
using namespace Gls::Mirror;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_MirrorHPP
