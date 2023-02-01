// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.ProjectedTextures.pas' rev: 35.00 (Windows)

#ifndef Gls_ProjectedtexturesHPP
#define Gls_ProjectedtexturesHPP

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
#include <GLS.Scene.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Texture.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.State.hpp>
#include <GLS.Material.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Projectedtextures
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTextureEmitter;
class DELPHICLASS TGLTextureEmitterItem;
class DELPHICLASS TGLTextureEmitters;
class DELPHICLASS TGLProjectedTextures;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLProjectedTexturesStyle : unsigned char { ptsOriginal, ptsInverse };

class PASCALIMPLEMENTATION TGLTextureEmitter : public Gls::Scene::TGLSceneObject
{
	typedef Gls::Scene::TGLSceneObject inherited;
	
private:
	float FFOVy;
	float FAspect;
	
protected:
	void __fastcall SetupTexMatrix(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	
public:
	__fastcall virtual TGLTextureEmitter(System::Classes::TComponent* AOwner);
	
__published:
	__property float FOVy = {read=FFOVy, write=FFOVy};
	__property float Aspect = {read=FAspect, write=FAspect};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLTextureEmitter() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTextureEmitter(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLSceneObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLTextureEmitterItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TGLTextureEmitter* FEmitter;
	
protected:
	void __fastcall SetEmitter(TGLTextureEmitter* const val);
	void __fastcall RemoveNotification(System::Classes::TComponent* aComponent);
	virtual System::UnicodeString __fastcall GetDisplayName();
	
public:
	__fastcall virtual TGLTextureEmitterItem(System::Classes::TCollection* ACollection);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property TGLTextureEmitter* Emitter = {read=FEmitter, write=SetEmitter};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TGLTextureEmitterItem() { }
	
};


class PASCALIMPLEMENTATION TGLTextureEmitters : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TGLTextureEmitterItem* operator[](int index) { return this->Items[index]; }
	
private:
	TGLProjectedTextures* FOwner;
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	TGLTextureEmitterItem* __fastcall GetItems(int index);
	void __fastcall RemoveNotification(System::Classes::TComponent* aComponent);
	
public:
	void __fastcall AddEmitter(TGLTextureEmitter* texEmitter);
	__property TGLTextureEmitterItem* Items[int index] = {read=GetItems/*, default*/};
public:
	/* TCollection.Create */ inline __fastcall TGLTextureEmitters(System::Classes::TCollectionItemClass ItemClass) : System::Classes::TCollection(ItemClass) { }
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLTextureEmitters() { }
	
};


class PASCALIMPLEMENTATION TGLProjectedTextures : public Gls::Scene::TGLImmaterialSceneObject
{
	typedef Gls::Scene::TGLImmaterialSceneObject inherited;
	
private:
	TGLTextureEmitters* FEmitters;
	TGLProjectedTexturesStyle FStyle;
	
public:
	__fastcall virtual TGLProjectedTextures(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLProjectedTextures();
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	
__published:
	__property TGLTextureEmitters* Emitters = {read=FEmitters, write=FEmitters};
	__property TGLProjectedTexturesStyle Style = {read=FStyle, write=FStyle, nodefault};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLProjectedTextures(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Projectedtextures */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_PROJECTEDTEXTURES)
using namespace Gls::Projectedtextures;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ProjectedtexturesHPP
