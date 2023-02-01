// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.BitmapFont.pas' rev: 35.00 (Windows)

#ifndef Gls_BitmapfontHPP
#define Gls_BitmapfontHPP

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
#include <System.Types.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.StdCtrls.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Scene.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Context.hpp>
#include <GLS.Texture.hpp>
#include <GLS.State.hpp>
#include <GLS.Utils.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.Color.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.PersistentClasses.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Bitmapfont
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLBitmapFontRange;
class DELPHICLASS TGLBitmapFontRanges;
struct TCharInfo;
class DELPHICLASS TGLCustomBitmapFont;
class DELPHICLASS TGLBitmapFont;
class DELPHICLASS TGLFlatText;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLBitmapFontRange : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::WideString __fastcall GetStartASCII();
	System::WideString __fastcall GetStopASCII();
	
protected:
	System::WideChar FStartASCII;
	System::WideChar FStopASCII;
	int FStartGlyphIdx;
	int FStopGlyphIdx;
	int FCharCount;
	void __fastcall SetStartASCII(const System::WideString val);
	void __fastcall SetStopASCII(const System::WideString val);
	void __fastcall SetStartGlyphIdx(int val);
	virtual System::UnicodeString __fastcall GetDisplayName();
	
public:
	__fastcall virtual TGLBitmapFontRange(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLBitmapFontRange();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall NotifyChange();
	
__published:
	__property System::WideString StartASCII = {read=GetStartASCII, write=SetStartASCII};
	__property System::WideString StopASCII = {read=GetStopASCII, write=SetStopASCII};
	__property int StartGlyphIdx = {read=FStartGlyphIdx, write=SetStartGlyphIdx, nodefault};
	__property int StopGlyphIdx = {read=FStopGlyphIdx, nodefault};
	__property int CharCount = {read=FCharCount, nodefault};
};


class PASCALIMPLEMENTATION TGLBitmapFontRanges : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TGLBitmapFontRange* operator[](int index) { return this->Items[index]; }
	
private:
	int FCharCount;
	
protected:
	System::Classes::TComponent* FOwner;
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	void __fastcall SetItems(int index, TGLBitmapFontRange* const val);
	TGLBitmapFontRange* __fastcall GetItems(int index);
	int __fastcall CalcCharacterCount();
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	
public:
	__fastcall TGLBitmapFontRanges(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLBitmapFontRanges();
	HIDESBASE TGLBitmapFontRange* __fastcall Add()/* overload */;
	HIDESBASE TGLBitmapFontRange* __fastcall Add(const System::WideChar StartASCII, const System::WideChar StopASCII)/* overload */;
	HIDESBASE TGLBitmapFontRange* __fastcall Add(const char StartASCII, const char StopASCII)/* overload */;
	HIDESBASE TGLBitmapFontRange* __fastcall FindItemID(int ID);
	__property TGLBitmapFontRange* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	int __fastcall CharacterToTileIndex(System::WideChar aChar);
	System::WideChar __fastcall TileIndexToChar(int aIndex);
	void __fastcall NotifyChange();
	__property int CharacterCount = {read=FCharCount, nodefault};
};


typedef TCharInfo *PCharInfo;

struct DECLSPEC_DRECORD TCharInfo
{
public:
	System::Word l;
	System::Word t;
	System::Word w;
};


class PASCALIMPLEMENTATION TGLCustomBitmapFont : public Gls::Baseclasses::TGLUpdateAbleComponent
{
	typedef Gls::Baseclasses::TGLUpdateAbleComponent inherited;
	
	
private:
	typedef System::DynamicArray<TCharInfo> _TGLCustomBitmapFont__1;
	
	
private:
	TGLBitmapFontRanges* FRanges;
	Vcl::Graphics::TPicture* FGlyphs;
	int FCharWidth;
	int FCharHeight;
	int FGlyphsIntervalX;
	int FGlyphsIntervalY;
	int FHSpace;
	int FVSpace;
	int FHSpaceFix;
	System::Classes::TList* FUsers;
	Gls::Texture::TGLMinFilter FMinFilter;
	Gls::Texture::TGLMagFilter FMagFilter;
	int FTextureWidth;
	int FTextureHeight;
	int FTextRows;
	int FTextCols;
	Gls::Texture::TGLTextureImageAlpha FGlyphsAlpha;
	System::Classes::TList* FTextures;
	bool FTextureModified;
	Gls::Context::TGLTextureHandle* FLastTexture;
	
protected:
	_TGLCustomBitmapFont__1 FChars;
	bool FCharsLoaded;
	void __fastcall ResetCharWidths(int w = 0xffffffff);
	void __fastcall SetCharWidths(int index, int value);
	void __fastcall SetRanges(TGLBitmapFontRanges* const val);
	void __fastcall SetGlyphs(Vcl::Graphics::TPicture* const val);
	void __fastcall SetCharWidth(const int val);
	void __fastcall SetCharHeight(const int val);
	void __fastcall SetGlyphsIntervalX(const int val);
	void __fastcall SetGlyphsIntervalY(const int val);
	void __fastcall OnGlyphsChanged(System::TObject* Sender);
	void __fastcall SetHSpace(const int val);
	void __fastcall SetVSpace(const int val);
	void __fastcall SetMagFilter(Gls::Texture::TGLMagFilter AValue);
	void __fastcall SetMinFilter(Gls::Texture::TGLMinFilter AValue);
	void __fastcall SetGlyphsAlpha(Gls::Texture::TGLTextureImageAlpha val);
	void __fastcall TextureChanged();
	virtual void __fastcall FreeTextureHandle();
	virtual int __fastcall TextureFormat();
	void __fastcall InvalidateUsers();
	int __fastcall CharactersPerRow();
	void __fastcall GetCharTexCoords(System::WideChar Ch, Gls::Vectorgeometry::TTexPoint &TopLeft, Gls::Vectorgeometry::TTexPoint &BottomRight);
	void __fastcall GetICharTexCoords(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, int Chi, /* out */ Gls::Vectorgeometry::TTexPoint &TopLeft, /* out */ Gls::Vectorgeometry::TTexPoint &BottomRight);
	virtual void __fastcall PrepareImage(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	void __fastcall PrepareParams(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	__property Vcl::Graphics::TPicture* Glyphs = {read=FGlyphs, write=SetGlyphs};
	__property int GlyphsIntervalX = {read=FGlyphsIntervalX, write=SetGlyphsIntervalX, nodefault};
	__property int GlyphsIntervalY = {read=FGlyphsIntervalY, write=SetGlyphsIntervalY, nodefault};
	__property TGLBitmapFontRanges* Ranges = {read=FRanges, write=SetRanges};
	__property int CharWidth = {read=FCharWidth, write=SetCharWidth, default=16};
	__property int HSpace = {read=FHSpace, write=SetHSpace, default=1};
	__property int VSpace = {read=FVSpace, write=SetVSpace, default=1};
	__property int HSpaceFix = {read=FHSpaceFix, write=FHSpaceFix, nodefault};
	__property Gls::Texture::TGLMagFilter MagFilter = {read=FMagFilter, write=SetMagFilter, default=1};
	__property Gls::Texture::TGLMinFilter MinFilter = {read=FMinFilter, write=SetMinFilter, default=1};
	__property Gls::Texture::TGLTextureImageAlpha GlyphsAlpha = {read=FGlyphsAlpha, write=FGlyphsAlpha, default=0};
	
public:
	__fastcall virtual TGLCustomBitmapFont(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomBitmapFont();
	virtual void __fastcall RegisterUser(Gls::Scene::TGLBaseSceneObject* anObject);
	virtual void __fastcall UnRegisterUser(Gls::Scene::TGLBaseSceneObject* anObject);
	virtual void __fastcall RenderString(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, const System::UnicodeString aText, System::Classes::TAlignment aAlignment, Vcl::Stdctrls::TTextLayout aLayout, const Gls::Vectortypes::TVector4f &aColor, Gls::Vectortypes::PGLVector aPosition = (Gls::Vectortypes::PGLVector)(0x0), bool aReverseY = false)/* overload */;
	void __fastcall TextOut(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, float X, float Y, const System::UnicodeString Text, const Gls::Vectortypes::TVector4f &Color)/* overload */;
	void __fastcall TextOut(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, float X, float Y, const System::UnicodeString Text, const System::Uitypes::TColor Color)/* overload */;
	int __fastcall TextWidth(const System::UnicodeString Text);
	virtual int __fastcall CharacterToTileIndex(System::WideChar aChar);
	virtual System::WideChar __fastcall TileIndexToChar(int aIndex);
	virtual int __fastcall CharacterCount();
	int __fastcall GetCharWidth(System::WideChar Ch);
	virtual int __fastcall CalcStringWidth(const System::UnicodeString aText)/* overload */;
	void __fastcall CheckTexture(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
	__property int CharHeight = {read=FCharHeight, write=SetCharHeight, default=16};
	__property int TextureWidth = {read=FTextureWidth, write=FTextureWidth, nodefault};
	__property int TextureHeight = {read=FTextureHeight, write=FTextureHeight, nodefault};
};


class PASCALIMPLEMENTATION TGLBitmapFont : public TGLCustomBitmapFont
{
	typedef TGLCustomBitmapFont inherited;
	
__published:
	__property Glyphs;
	__property GlyphsIntervalX;
	__property GlyphsIntervalY;
	__property Ranges;
	__property CharWidth = {default=16};
	__property CharHeight = {default=16};
	__property HSpace = {default=1};
	__property VSpace = {default=1};
	__property MagFilter = {default=1};
	__property MinFilter = {default=1};
	__property GlyphsAlpha = {default=0};
public:
	/* TGLCustomBitmapFont.Create */ inline __fastcall virtual TGLBitmapFont(System::Classes::TComponent* AOwner) : TGLCustomBitmapFont(AOwner) { }
	/* TGLCustomBitmapFont.Destroy */ inline __fastcall virtual ~TGLBitmapFont() { }
	
};


enum DECLSPEC_DENUM TGLFlatTextOption : unsigned char { ftoTwoSided };

typedef System::Set<TGLFlatTextOption, TGLFlatTextOption::ftoTwoSided, TGLFlatTextOption::ftoTwoSided> TGLFlatTextOptions;

class PASCALIMPLEMENTATION TGLFlatText : public Gls::Scene::TGLImmaterialSceneObject
{
	typedef Gls::Scene::TGLImmaterialSceneObject inherited;
	
private:
	TGLCustomBitmapFont* FBitmapFont;
	System::UnicodeString FText;
	System::Classes::TAlignment FAlignment;
	Vcl::Stdctrls::TTextLayout FLayout;
	Gls::Color::TGLColor* FModulateColor;
	TGLFlatTextOptions FOptions;
	
protected:
	void __fastcall SetBitmapFont(TGLCustomBitmapFont* const val);
	void __fastcall SetText(const System::UnicodeString val);
	void __fastcall SetAlignment(const System::Classes::TAlignment val);
	void __fastcall SetLayout(const Vcl::Stdctrls::TTextLayout val);
	void __fastcall SetModulateColor(Gls::Color::TGLColor* const val);
	void __fastcall SetOptions(const TGLFlatTextOptions val);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLFlatText(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLFlatText();
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property TGLCustomBitmapFont* BitmapFont = {read=FBitmapFont, write=SetBitmapFont};
	__property System::UnicodeString Text = {read=FText, write=SetText};
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, nodefault};
	__property Vcl::Stdctrls::TTextLayout Layout = {read=FLayout, write=SetLayout, nodefault};
	__property Gls::Color::TGLColor* ModulateColor = {read=FModulateColor, write=SetModulateColor};
	__property TGLFlatTextOptions Options = {read=FOptions, write=SetOptions, nodefault};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFlatText(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Bitmapfont */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_BITMAPFONT)
using namespace Gls::Bitmapfont;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_BitmapfontHPP
