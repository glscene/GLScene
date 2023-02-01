// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSL.ProjectedTextures.pas' rev: 35.00 (Windows)

#ifndef Glsl_ProjectedtexturesHPP
#define Glsl_ProjectedtexturesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.PipelineTransformation.hpp>
#include <GLS.Scene.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.Texture.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Context.hpp>
#include <GLS.Color.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsl
{
namespace Projectedtextures
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLSLTextureEmitter;
class DELPHICLASS TGLSLTextureEmitterItem;
class DELPHICLASS TGLSLTextureEmitters;
class DELPHICLASS TGLSLProjectedTextures;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLSLProjectedTexturesStyle : unsigned char { ptsLight, ptsShadow };

class PASCALIMPLEMENTATION TGLSLTextureEmitter : public Gls::Scene::TGLBaseSceneObject
{
	typedef Gls::Scene::TGLBaseSceneObject inherited;
	
private:
	float FFOV;
	float FAspect;
	float FBrightness;
	float FAttenuation;
	TGLSLProjectedTexturesStyle FStyle;
	Gls::Color::TGLColor* FColor;
	bool FUseAttenuation;
	bool FAllowReverseProjection;
	bool FUseQuadraticAttenuation;
	
protected:
	TGLSLProjectedTextures* ProjectedTexturesObject;
	Gls::Vectortypes::TMatrix4f TexMatrix;
	void __fastcall SetupTexMatrix();
	void __fastcall SetStyle(TGLSLProjectedTexturesStyle val);
	void __fastcall SetUseAttenuation(bool val);
	void __fastcall SetUseQuadraticAttenuation(bool val);
	void __fastcall SetAllowReverseProjection(bool val);
	
public:
	__fastcall virtual TGLSLTextureEmitter(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSLTextureEmitter();
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	
__published:
	__property float FOV = {read=FFOV, write=FFOV};
	__property float Aspect = {read=FAspect, write=FAspect};
	__property TGLSLProjectedTexturesStyle Style = {read=FStyle, write=SetStyle, nodefault};
	__property float Attenuation = {read=FAttenuation, write=FAttenuation};
	__property float Brightness = {read=FBrightness, write=FBrightness};
	__property Gls::Color::TGLColor* Color = {read=FColor, write=FColor};
	__property bool UseAttenuation = {read=FUseAttenuation, write=SetUseAttenuation, nodefault};
	__property bool UseQuadraticAttenuation = {read=FUseQuadraticAttenuation, write=SetUseQuadraticAttenuation, nodefault};
	__property bool AllowReverseProjection = {read=FAllowReverseProjection, write=SetAllowReverseProjection, nodefault};
	__property ObjectsSorting = {default=0};
	__property VisibilityCulling = {default=0};
	__property Direction;
	__property PitchAngle = {default=0};
	__property Position;
	__property RollAngle = {default=0};
	__property Scale;
	__property ShowAxes = {default=0};
	__property TurnAngle = {default=0};
	__property Up;
	__property Visible = {default=1};
	__property OnProgress;
	__property Behaviours;
	__property Effects;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSLTextureEmitter(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLBaseSceneObject(aParentOwner) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSLTextureEmitterItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TGLSLTextureEmitter* FEmitter;
	
protected:
	void __fastcall SetEmitter(TGLSLTextureEmitter* const val);
	void __fastcall RemoveNotification(System::Classes::TComponent* aComponent);
	virtual System::UnicodeString __fastcall GetDisplayName();
	
public:
	__fastcall virtual TGLSLTextureEmitterItem(System::Classes::TCollection* Collection);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property TGLSLTextureEmitter* Emitter = {read=FEmitter, write=SetEmitter};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TGLSLTextureEmitterItem() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSLTextureEmitters : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TGLSLTextureEmitterItem* operator[](int index) { return this->Items[index]; }
	
private:
	TGLSLProjectedTextures* FOwner;
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	TGLSLTextureEmitterItem* __fastcall GetItems(int index);
	void __fastcall RemoveNotification(System::Classes::TComponent* aComponent);
	
public:
	void __fastcall AddEmitter(TGLSLTextureEmitter* texEmitter);
	__property TGLSLTextureEmitterItem* Items[int index] = {read=GetItems/*, default*/};
public:
	/* TCollection.Create */ inline __fastcall TGLSLTextureEmitters(System::Classes::TCollectionItemClass ItemClass) : System::Classes::TCollection(ItemClass) { }
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLSLTextureEmitters() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLSLProjectedTextures : public Gls::Scene::TGLSceneObject
{
	typedef Gls::Scene::TGLSceneObject inherited;
	
private:
	bool ShaderSupported;
	TGLSLTextureEmitters* FEmitters;
	bool FUseLightmaps;
	Gls::Context::TGLProgramHandle* Shader;
	Gls::Color::TGLColor* FAmbient;
	void __fastcall SetupShader();
	
protected:
	bool ShaderChanged;
	void __fastcall SetUseLightmaps(bool val);
	
public:
	__fastcall virtual TGLSLProjectedTextures(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSLProjectedTextures();
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual void __fastcall StructureChanged();
	
__published:
	__property TGLSLTextureEmitters* Emitters = {read=FEmitters, write=FEmitters};
	__property Gls::Color::TGLColor* Ambient = {read=FAmbient, write=FAmbient};
	__property bool UseLightmaps = {read=FUseLightmaps, write=SetUseLightmaps, nodefault};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSLProjectedTextures(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Projectedtextures */
}	/* namespace Glsl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL_PROJECTEDTEXTURES)
using namespace Glsl::Projectedtextures;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL)
using namespace Glsl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glsl_ProjectedtexturesHPP
