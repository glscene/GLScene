// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.ShadowPlane.pas' rev: 35.00 (Windows)

#ifndef Gls_ShadowplaneHPP
#define Gls_ShadowplaneHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.PipelineTransformation.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Scene.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Objects.hpp>
#include <GLS.Color.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.State.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.Context.hpp>
#include <GLS.Material.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Utils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Shadowplane
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLShadowPlane;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TShadowPlaneOption : unsigned char { spoUseStencil, spoScissor, spoTransparent, spoIgnoreZ };

typedef System::Set<TShadowPlaneOption, TShadowPlaneOption::spoUseStencil, TShadowPlaneOption::spoIgnoreZ> TShadowPlaneOptions;

class PASCALIMPLEMENTATION TGLShadowPlane : public Gls::Objects::TGLPlane
{
	typedef Gls::Objects::TGLPlane inherited;
	
private:
	bool FRendering;
	Gls::Scene::TGLBaseSceneObject* FShadowingObject;
	Gls::Scene::TGLLightSource* FShadowedLight;
	Gls::Color::TGLColor* FShadowColor;
	TShadowPlaneOptions FShadowOptions;
	System::Classes::TNotifyEvent FOnBeginRenderingShadows;
	System::Classes::TNotifyEvent FOnEndRenderingShadows;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetShadowingObject(Gls::Scene::TGLBaseSceneObject* const val);
	void __fastcall SetShadowedLight(Gls::Scene::TGLLightSource* const val);
	void __fastcall SetShadowColor(Gls::Color::TGLColor* const val);
	void __fastcall SetShadowOptions(const TShadowPlaneOptions val);
	
public:
	__fastcall virtual TGLShadowPlane(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLShadowPlane();
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Gls::Scene::TGLBaseSceneObject* ShadowingObject = {read=FShadowingObject, write=SetShadowingObject};
	__property Gls::Scene::TGLLightSource* ShadowedLight = {read=FShadowedLight, write=SetShadowedLight};
	__property Gls::Color::TGLColor* ShadowColor = {read=FShadowColor, write=SetShadowColor};
	__property TShadowPlaneOptions ShadowOptions = {read=FShadowOptions, write=SetShadowOptions, default=3};
	__property System::Classes::TNotifyEvent OnBeginRenderingShadows = {read=FOnBeginRenderingShadows, write=FOnBeginRenderingShadows};
	__property System::Classes::TNotifyEvent OnEndRenderingShadows = {read=FOnEndRenderingShadows, write=FOnEndRenderingShadows};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLShadowPlane(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Objects::TGLPlane(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define cDefaultShadowPlaneOptions (System::Set<TShadowPlaneOption, TShadowPlaneOption::spoUseStencil, TShadowPlaneOption::spoIgnoreZ>() << TShadowPlaneOption::spoUseStencil << TShadowPlaneOption::spoScissor )
}	/* namespace Shadowplane */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_SHADOWPLANE)
using namespace Gls::Shadowplane;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ShadowplaneHPP
