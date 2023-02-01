// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.AnimatedSprite.pas' rev: 35.00 (Windows)

#ifndef Gls_AnimatedspriteHPP
#define Gls_AnimatedspriteHPP

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
#include <System.Math.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Scene.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Material.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.XCollection.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.Context.hpp>
#include <GLS.State.hpp>
#include <GLS.Coordinates.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Animatedsprite
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLSpriteAnimFrame;
class DELPHICLASS TGLSpriteAnimFrameList;
class DELPHICLASS TGLSpriteAnimMargins;
class DELPHICLASS TGLSpriteAnimation;
class DELPHICLASS TGLSpriteAnimationList;
class DELPHICLASS TGLAnimatedSprite;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSpriteAnimFrame : public Gls::Xcollection::TXCollectionItem
{
	typedef Gls::Xcollection::TXCollectionItem inherited;
	
private:
	int FOffsetX;
	int FOffsetY;
	int FWidth;
	int FHeight;
	void __fastcall DoChanged();
	
protected:
	void __fastcall SetOffsetX(const int Value);
	void __fastcall SetOffsetY(const int Value);
	void __fastcall SetWidth(const int Value);
	void __fastcall SetHeight(const int Value);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	
public:
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	
__published:
	__property int OffsetX = {read=FOffsetX, write=SetOffsetX, nodefault};
	__property int OffsetY = {read=FOffsetY, write=SetOffsetY, nodefault};
	__property int Width = {read=FWidth, write=SetWidth, nodefault};
	__property int Height = {read=FHeight, write=SetHeight, nodefault};
public:
	/* TXCollectionItem.Create */ inline __fastcall virtual TGLSpriteAnimFrame(Gls::Xcollection::TXCollection* aOwner) : Gls::Xcollection::TXCollectionItem(aOwner) { }
	/* TXCollectionItem.Destroy */ inline __fastcall virtual ~TGLSpriteAnimFrame() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSpriteAnimFrameList : public Gls::Xcollection::TXCollection
{
	typedef Gls::Xcollection::TXCollection inherited;
	
public:
	__fastcall virtual TGLSpriteAnimFrameList(System::Classes::TPersistent* aOwner);
	__classmethod virtual Gls::Xcollection::TXCollectionItemClass __fastcall ItemsClass();
public:
	/* TXCollection.Destroy */ inline __fastcall virtual ~TGLSpriteAnimFrameList() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TGLSpriteFrameDimensions : unsigned char { sfdAuto, sfdManual };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSpriteAnimMargins : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TGLSpriteAnimation* FOwner;
	int FLeft;
	int FTop;
	int FRight;
	int FBottom;
	
protected:
	void __fastcall SetLeft(const int Value);
	void __fastcall SetTop(const int Value);
	void __fastcall SetRight(const int Value);
	void __fastcall SetBottom(const int Value);
	void __fastcall DoChanged();
	
public:
	__fastcall TGLSpriteAnimMargins(TGLSpriteAnimation* Animation);
	__property TGLSpriteAnimation* Owner = {read=FOwner};
	
__published:
	__property int Left = {read=FLeft, write=SetLeft, nodefault};
	__property int Top = {read=FTop, write=SetTop, nodefault};
	__property int Right = {read=FRight, write=SetRight, nodefault};
	__property int Bottom = {read=FBottom, write=SetBottom, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLSpriteAnimMargins() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSpriteAnimation : public Gls::Xcollection::TXCollectionItem
{
	typedef Gls::Xcollection::TXCollectionItem inherited;
	
private:
	int FCurrentFrame;
	int FStartFrame;
	int FEndFrame;
	int FFrameWidth;
	int FFrameHeight;
	int FInterval;
	TGLSpriteAnimFrameList* FFrames;
	System::UnicodeString FLibMaterialName;
	Gls::Material::TGLLibMaterial* FLibMaterialCached;
	TGLSpriteFrameDimensions FDimensions;
	TGLSpriteAnimMargins* FMargins;
	void __fastcall DoChanged();
	
protected:
	void __fastcall SetCurrentFrame(const int Value);
	void __fastcall SetFrameWidth(const int Value);
	void __fastcall SetFrameHeight(const int Value);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	void __fastcall SetDimensions(const TGLSpriteFrameDimensions Value);
	void __fastcall SetLibMaterialName(const System::UnicodeString val);
	Gls::Material::TGLLibMaterial* __fastcall GetLibMaterialCached();
	void __fastcall SetInterval(const int Value);
	void __fastcall SetFrameRate(const float Value);
	float __fastcall GetFrameRate();
	Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	
public:
	__fastcall virtual TGLSpriteAnimation(Gls::Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLSpriteAnimation();
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__property Gls::Material::TGLLibMaterial* LibMaterialCached = {read=GetLibMaterialCached};
	
__published:
	__property int CurrentFrame = {read=FCurrentFrame, write=SetCurrentFrame, nodefault};
	__property int StartFrame = {read=FStartFrame, write=FStartFrame, nodefault};
	__property int EndFrame = {read=FEndFrame, write=FEndFrame, nodefault};
	__property int FrameWidth = {read=FFrameWidth, write=SetFrameWidth, nodefault};
	__property int FrameHeight = {read=FFrameHeight, write=SetFrameHeight, nodefault};
	__property System::UnicodeString LibMaterialName = {read=FLibMaterialName, write=SetLibMaterialName};
	__property TGLSpriteAnimFrameList* Frames = {read=FFrames};
	__property TGLSpriteFrameDimensions Dimensions = {read=FDimensions, write=SetDimensions, nodefault};
	__property int Interval = {read=FInterval, write=SetInterval, nodefault};
	__property float FrameRate = {read=GetFrameRate, write=SetFrameRate};
	__property TGLSpriteAnimMargins* Margins = {read=FMargins};
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

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSpriteAnimationList : public Gls::Xcollection::TXCollection
{
	typedef Gls::Xcollection::TXCollection inherited;
	
public:
	__fastcall virtual TGLSpriteAnimationList(System::Classes::TPersistent* aOwner);
	__classmethod virtual Gls::Xcollection::TXCollectionItemClass __fastcall ItemsClass();
public:
	/* TXCollection.Destroy */ inline __fastcall virtual ~TGLSpriteAnimationList() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TGLSpriteAnimationMode : unsigned char { samNone, samPlayOnce, samLoop, samBounceForward, samBounceBackward, samLoopBackward };

class PASCALIMPLEMENTATION TGLAnimatedSprite : public Gls::Scene::TGLBaseSceneObject
{
	typedef Gls::Scene::TGLBaseSceneObject inherited;
	
private:
	TGLSpriteAnimationList* FAnimations;
	Gls::Material::TGLMaterialLibrary* FMaterialLibrary;
	int FAnimationIndex;
	int FInterval;
	int FRotation;
	int FPixelRatio;
	bool FMirrorU;
	bool FMirrorV;
	TGLSpriteAnimationMode FAnimationMode;
	double FCurrentFrameDelta;
	System::Classes::TNotifyEvent FOnFrameChanged;
	System::Classes::TNotifyEvent FOnEndFrameReached;
	System::Classes::TNotifyEvent FOnStartFrameReached;
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall WriteAnimations(System::Classes::TStream* Stream);
	void __fastcall ReadAnimations(System::Classes::TStream* Stream);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetInterval(const int val);
	void __fastcall SetAnimationIndex(const int val);
	void __fastcall SetAnimationMode(const TGLSpriteAnimationMode val);
	void __fastcall SetMaterialLibrary(Gls::Material::TGLMaterialLibrary* const val);
	void __fastcall SetPixelRatio(const int val);
	HIDESBASE void __fastcall SetRotation(const int val);
	void __fastcall SetMirrorU(const bool val);
	void __fastcall SetMirrorV(const bool val);
	void __fastcall SetFrameRate(const float Value);
	float __fastcall GetFrameRate();
	
public:
	__fastcall virtual TGLAnimatedSprite(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLAnimatedSprite();
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoProgress(const Gls::Baseclasses::TGLProgressTimes &progressTime);
	void __fastcall NextFrame();
	
__published:
	__property TGLSpriteAnimationList* Animations = {read=FAnimations};
	__property Gls::Material::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property int Interval = {read=FInterval, write=SetInterval, nodefault};
	__property int AnimationIndex = {read=FAnimationIndex, write=SetAnimationIndex, nodefault};
	__property TGLSpriteAnimationMode AnimationMode = {read=FAnimationMode, write=SetAnimationMode, nodefault};
	__property int PixelRatio = {read=FPixelRatio, write=SetPixelRatio, nodefault};
	__property int Rotation = {read=FRotation, write=SetRotation, nodefault};
	__property bool MirrorU = {read=FMirrorU, write=SetMirrorU, nodefault};
	__property bool MirrorV = {read=FMirrorV, write=SetMirrorV, nodefault};
	__property float FrameRate = {read=GetFrameRate, write=SetFrameRate};
	__property Position;
	__property Scale;
	__property Visible = {default=1};
	__property System::Classes::TNotifyEvent OnFrameChanged = {read=FOnFrameChanged, write=FOnFrameChanged};
	__property System::Classes::TNotifyEvent OnEndFrameReached = {read=FOnEndFrameReached, write=FOnEndFrameReached};
	__property System::Classes::TNotifyEvent OnStartFrameReached = {read=FOnStartFrameReached, write=FOnStartFrameReached};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLAnimatedSprite(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLBaseSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Animatedsprite */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_ANIMATEDSPRITE)
using namespace Gls::Animatedsprite;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_AnimatedspriteHPP
