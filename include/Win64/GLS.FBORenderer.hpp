// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.FBORenderer.pas' rev: 35.00 (Windows)

#ifndef Gls_FborendererHPP
#define Gls_FborendererHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.OpenGLext.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.PipelineTransformation.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Context.hpp>
#include <GLS.Color.hpp>
#include <GLS.Material.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.State.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.MultiSampleImage.hpp>
#include <GLS.Logger.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Fborenderer
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLRenderbuffer;
class DELPHICLASS TGLDepthRBO;
class DELPHICLASS TGLStencilRBO;
class DELPHICLASS TGLFrameBuffer;
class DELPHICLASS TGLFBORenderer;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLRenderbuffer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Gls::Context::TGLRenderbufferHandle* FRenderbufferHandle;
	int FWidth;
	int FHeight;
	bool FStorageValid;
	unsigned __fastcall GetHandle();
	void __fastcall SetHeight(const int Value);
	void __fastcall SetWidth(const int Value);
	
protected:
	virtual unsigned __fastcall GetInternalFormat() = 0 ;
	void __fastcall InvalidateStorage();
	
public:
	__fastcall TGLRenderbuffer();
	__fastcall virtual ~TGLRenderbuffer();
	void __fastcall Bind();
	void __fastcall Unbind();
	__property unsigned Handle = {read=GetHandle, nodefault};
	__property int Width = {read=FWidth, write=SetWidth, nodefault};
	__property int Height = {read=FHeight, write=SetHeight, nodefault};
};


class PASCALIMPLEMENTATION TGLDepthRBO : public TGLRenderbuffer
{
	typedef TGLRenderbuffer inherited;
	
private:
	Gls::Scene::TGLDepthPrecision FDepthPrecision;
	void __fastcall SetDepthPrecision(const Gls::Scene::TGLDepthPrecision Value);
	
protected:
	virtual unsigned __fastcall GetInternalFormat();
	
public:
	__fastcall TGLDepthRBO();
	__property Gls::Scene::TGLDepthPrecision DepthPrecision = {read=FDepthPrecision, write=SetDepthPrecision, nodefault};
public:
	/* TGLRenderbuffer.Destroy */ inline __fastcall virtual ~TGLDepthRBO() { }
	
};


enum DECLSPEC_DENUM TGLStencilPrecision : unsigned char { spDefault, sp1bit, sp4bits, sp8bits, sp16bits };

class PASCALIMPLEMENTATION TGLStencilRBO : public TGLRenderbuffer
{
	typedef TGLRenderbuffer inherited;
	
private:
	TGLStencilPrecision FStencilPrecision;
	void __fastcall SetStencilPrecision(const TGLStencilPrecision Value);
	
protected:
	virtual unsigned __fastcall GetInternalFormat();
	
public:
	__fastcall TGLStencilRBO();
	__property TGLStencilPrecision StencilPrecision = {read=FStencilPrecision, write=SetStencilPrecision, nodefault};
public:
	/* TGLRenderbuffer.Destroy */ inline __fastcall virtual ~TGLStencilRBO() { }
	
};


class PASCALIMPLEMENTATION TGLFrameBuffer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Gls::Context::TGLFramebufferHandle* FFrameBufferHandle;
	unsigned FTarget;
	int FWidth;
	int FHeight;
	int FLayer;
	int FLevel;
	unsigned FTextureMipmap;
	System::StaticArray<Gls::Texture::TGLTexture*, 32> FAttachedTexture;
	Gls::Texture::TGLTexture* FDepthTexture;
	TGLDepthRBO* FDRBO;
	TGLStencilRBO* FSRBO;
	Gls::Context::TGLFramebufferStatus __fastcall GetStatus();
	void __fastcall SetHeight(const int Value);
	void __fastcall SetWidth(const int Value);
	void __fastcall SetLayer(const int Value);
	void __fastcall SetLevel(const int Value);
	
protected:
	void __fastcall AttachTexture(const unsigned attachment, const unsigned textarget, const unsigned texture, const int level, const int layer)/* overload */;
	void __fastcall ReattachTextures();
	
public:
	__fastcall TGLFrameBuffer();
	__fastcall virtual ~TGLFrameBuffer();
	void __fastcall AttachDepthBuffer(TGLDepthRBO* DepthBuffer)/* overload */;
	void __fastcall DetachDepthBuffer();
	void __fastcall AttachStencilBuffer(TGLStencilRBO* StencilBuffer)/* overload */;
	void __fastcall DetachStencilBuffer();
	void __fastcall AttachDepthTexture(Gls::Texture::TGLTexture* Texture)/* overload */;
	void __fastcall DetachDepthTexture();
	void __fastcall AttachTexture(unsigned n, Gls::Texture::TGLTexture* Texture)/* overload */;
	void __fastcall DetachTexture(unsigned n);
	Gls::Context::TGLFramebufferStatus __fastcall GetStringStatus(/* out */ System::UnicodeString &clarification);
	__property Gls::Context::TGLFramebufferStatus Status = {read=GetStatus, nodefault};
	void __fastcall Bind();
	void __fastcall Unbind();
	void __fastcall PreRender();
	void __fastcall Render(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, Gls::Scene::TGLBaseSceneObject* baseObject);
	void __fastcall PostRender(const bool PostGenerateMipmap);
	__property Gls::Context::TGLFramebufferHandle* Handle = {read=FFrameBufferHandle};
	__property int Width = {read=FWidth, write=SetWidth, nodefault};
	__property int Height = {read=FHeight, write=SetHeight, nodefault};
	__property int Layer = {read=FLayer, write=SetLayer, nodefault};
	__property int Level = {read=FLevel, write=SetLevel, nodefault};
};


enum DECLSPEC_DENUM TGLEnabledRenderBuffer : unsigned char { erbDepth, erbStencil };

typedef System::Set<TGLEnabledRenderBuffer, TGLEnabledRenderBuffer::erbDepth, TGLEnabledRenderBuffer::erbStencil> TGLEnabledRenderBuffers;

enum DECLSPEC_DENUM TGLFBOTargetVisibility : unsigned char { tvDefault, tvFBOOnly };

enum DECLSPEC_DENUM TGLFBOClearOption : unsigned char { coColorBufferClear, coDepthBufferClear, coStencilBufferClear, coUseBufferBackground };

typedef System::Set<TGLFBOClearOption, TGLFBOClearOption::coColorBufferClear, TGLFBOClearOption::coUseBufferBackground> TGLFBOClearOptions;

typedef System::DynamicArray<Gls::Texture::TGLTexture*> TGLTextureArray;

typedef void __fastcall (__closure *TSetTextureTargetsEvent)(System::TObject* Sender, TGLTextureArray &colorTexs);

class PASCALIMPLEMENTATION TGLFBORenderer : public Gls::Scene::TGLBaseSceneObject
{
	typedef Gls::Scene::TGLBaseSceneObject inherited;
	
private:
	TGLFrameBuffer* FFbo;
	TGLDepthRBO* FDepthRBO;
	TGLStencilRBO* FStencilRBO;
	int FColorAttachment;
	bool FRendering;
	bool FHasColor;
	bool FHasDepth;
	bool FHasStencil;
	Gls::Material::TGLMaterialLibrary* FMaterialLibrary;
	System::UnicodeString FColorTextureName;
	System::UnicodeString FDepthTextureName;
	int FWidth;
	int FHeight;
	bool FForceTextureDimensions;
	TGLStencilPrecision FStencilPrecision;
	Gls::Scene::TGLBaseSceneObject* FRootObject;
	bool FRootVisible;
	Gls::Scene::TGLCamera* FCamera;
	TGLEnabledRenderBuffers FEnabledRenderBuffers;
	TGLFBOTargetVisibility FTargetVisibility;
	Gls::Scene::TGLDirectRenderEvent FBeforeRender;
	System::Classes::TNotifyEvent FPostInitialize;
	Gls::Scene::TGLDirectRenderEvent FAfterRender;
	System::Classes::TNotifyEvent FPreInitialize;
	Gls::Color::TGLColor* FBackgroundColor;
	TGLFBOClearOptions FClearOptions;
	float FAspect;
	float FSceneScaleFactor;
	bool FUseLibraryAsMultiTarget;
	bool FPostGenerateMipmap;
	int FMaxSize;
	int FMaxAttachment;
	System::StaticArray<Gls::Vectortypes::TVector4f, 3> FStoreCamera;
	TSetTextureTargetsEvent FOnSetTextureTargets;
	Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	void __fastcall SetMaterialLibrary(Gls::Material::TGLAbstractMaterialLibrary* const Value);
	void __fastcall SetDepthTextureName(const System::UnicodeString Value);
	void __fastcall SetColorTextureName(const System::UnicodeString Value);
	void __fastcall SetForceTextureDimentions(const bool Value);
	void __fastcall SetHeight(int Value);
	void __fastcall SetWidth(int Value);
	void __fastcall SetLayer(const int Value);
	int __fastcall GetLayer();
	void __fastcall SetLevel(const int Value);
	int __fastcall GetLevel();
	void __fastcall SetStencilPrecision(const TGLStencilPrecision Value);
	void __fastcall SetRootObject(Gls::Scene::TGLBaseSceneObject* const Value);
	Gls::Vectorgeometry::TRectangle __fastcall GetViewport();
	void __fastcall SetCamera(Gls::Scene::TGLCamera* const Value);
	void __fastcall SetEnabledRenderBuffers(const TGLEnabledRenderBuffers Value);
	void __fastcall SetTargetVisibility(const TGLFBOTargetVisibility Value);
	void __fastcall SetBackgroundColor(Gls::Color::TGLColor* const Value);
	bool __fastcall StoreSceneScaleFactor();
	bool __fastcall StoreAspect();
	void __fastcall SetUseLibraryAsMultiTarget(bool Value);
	void __fastcall SetPostGenerateMipmap(const bool Value);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall Initialize();
	void __fastcall ForceDimensions(Gls::Texture::TGLTexture* Texture);
	void __fastcall RenderToFBO(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	void __fastcall ApplyCamera(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	void __fastcall UnApplyCamera(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	void __fastcall DoBeforeRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	void __fastcall DoAfterRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	void __fastcall DoPreInitialize();
	void __fastcall DoPostInitialize();
	__property bool HasColor = {read=FHasColor, nodefault};
	__property bool HasDepth = {read=FHasDepth, nodefault};
	__property bool HasStencil = {read=FHasStencil, nodefault};
	__property Gls::Vectorgeometry::TRectangle Viewport = {read=GetViewport};
	
public:
	__fastcall virtual TGLFBORenderer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLFBORenderer();
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	__property int Layer = {read=GetLayer, write=SetLayer, nodefault};
	__property int Level = {read=GetLevel, write=SetLevel, nodefault};
	
__published:
	__property bool Active = {read=GetVisible, write=SetVisible, default=1};
	__property bool PickableTarget = {read=GetPickable, write=SetPickable, default=0};
	__property bool ForceTextureDimensions = {read=FForceTextureDimensions, write=SetForceTextureDimentions, default=1};
	__property int Width = {read=FWidth, write=SetWidth, default=256};
	__property int Height = {read=FHeight, write=SetHeight, default=256};
	__property float Aspect = {read=FAspect, write=FAspect, stored=StoreAspect};
	__property System::UnicodeString ColorTextureName = {read=FColorTextureName, write=SetColorTextureName};
	__property System::UnicodeString DepthTextureName = {read=FDepthTextureName, write=SetDepthTextureName};
	__property Gls::Material::TGLAbstractMaterialLibrary* MaterialLibrary = {read=GetMaterialLibrary, write=SetMaterialLibrary};
	__property Gls::Color::TGLColor* BackgroundColor = {read=FBackgroundColor, write=SetBackgroundColor};
	__property TGLFBOClearOptions ClearOptions = {read=FClearOptions, write=FClearOptions, nodefault};
	__property Gls::Scene::TGLCamera* Camera = {read=FCamera, write=SetCamera};
	__property float SceneScaleFactor = {read=FSceneScaleFactor, write=FSceneScaleFactor, stored=StoreSceneScaleFactor};
	__property Gls::Scene::TGLBaseSceneObject* RootObject = {read=FRootObject, write=SetRootObject};
	__property TGLFBOTargetVisibility TargetVisibility = {read=FTargetVisibility, write=SetTargetVisibility, default=0};
	__property TGLEnabledRenderBuffers EnabledRenderBuffers = {read=FEnabledRenderBuffers, write=SetEnabledRenderBuffers, nodefault};
	__property TGLStencilPrecision StencilPrecision = {read=FStencilPrecision, write=SetStencilPrecision, default=0};
	__property Gls::Scene::TGLDirectRenderEvent BeforeRender = {read=FBeforeRender, write=FBeforeRender};
	__property Gls::Scene::TGLDirectRenderEvent AfterRender = {read=FAfterRender, write=FAfterRender};
	__property System::Classes::TNotifyEvent PreInitialize = {read=FPreInitialize, write=FPreInitialize};
	__property System::Classes::TNotifyEvent PostInitialize = {read=FPostInitialize, write=FPostInitialize};
	__property bool UseLibraryAsMultiTarget = {read=FUseLibraryAsMultiTarget, write=SetUseLibraryAsMultiTarget, default=0};
	__property bool PostGenerateMipmap = {read=FPostGenerateMipmap, write=SetPostGenerateMipmap, default=1};
	__property TSetTextureTargetsEvent OnSetTextureTargets = {read=FOnSetTextureTargets, write=FOnSetTextureTargets};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFBORenderer(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLBaseSceneObject(aParentOwner) { }
	
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


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MaxColorAttachments = System::Int8(0x20);
}	/* namespace Fborenderer */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_FBORENDERER)
using namespace Gls::Fborenderer;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_FborendererHPP
