// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.SpaceText.pas' rev: 35.00 (Windows)

#ifndef Gls_SpacetextHPP
#define Gls_SpacetextHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Context.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Strings.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.State.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Spacetext
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTextAdjust;
struct TFontEntry;
class DELPHICLASS TGLSpaceText;
class DELPHICLASS TFontManager;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLSpaceTextCharRange : unsigned char { stcrDefault, stcrAlphaNum, stcrNumbers, stcrWide };

enum DECLSPEC_DENUM TGLTextHorzAdjust : unsigned char { haLeft, haCenter, haRight, haAligned, haCentrically, haFitIn };

enum DECLSPEC_DENUM TGLTextVertAdjust : unsigned char { vaTop, vaCenter, vaBottom, vaBaseLine };

class PASCALIMPLEMENTATION TGLTextAdjust : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TGLTextHorzAdjust FHorz;
	TGLTextVertAdjust FVert;
	System::Classes::TNotifyEvent FOnChange;
	void __fastcall SetHorz(const TGLTextHorzAdjust Value);
	void __fastcall SetVert(const TGLTextVertAdjust Value);
	
public:
	__fastcall TGLTextAdjust();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	
__published:
	__property TGLTextHorzAdjust Horz = {read=FHorz, write=SetHorz, default=0};
	__property TGLTextVertAdjust Vert = {read=FVert, write=SetVert, default=3};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLTextAdjust() { }
	
};


typedef TFontEntry *PFontEntry;

struct DECLSPEC_DRECORD TFontEntry
{
	
private:
	typedef System::DynamicArray<_GLYPHMETRICSFLOAT> _TFontEntry__1;
	
	
public:
	System::UnicodeString Name;
	Gls::Context::TGLVirtualHandleTransf* FVirtualHandle;
	System::Uitypes::TFontStyles Styles;
	float Extrusion;
	int RefCount;
	float allowedDeviation;
	int firstChar;
	int lastChar;
	_TFontEntry__1 glyphMetrics;
	System::Classes::TList* FClients;
};


class PASCALIMPLEMENTATION TGLSpaceText : public Gls::Scene::TGLSceneObject
{
	typedef Gls::Scene::TGLSceneObject inherited;
	
private:
	Vcl::Graphics::TFont* FFont;
	float FExtrusion;
	float FAllowedDeviation;
	TGLSpaceTextCharRange FCharacterRange;
	TGLTextAdjust* FAdjust;
	float FAspectRatio;
	float FOblique;
	float FTextHeight;
	System::Classes::TStringList* FLines;
	void __fastcall SetCharacterRange(const TGLSpaceTextCharRange val);
	void __fastcall SetAllowedDeviation(const float val);
	void __fastcall SetExtrusion(float AValue);
	void __fastcall SetFont(Vcl::Graphics::TFont* AFont);
	System::WideString __fastcall GetText();
	void __fastcall SetLines(System::Classes::TStringList* const Value);
	void __fastcall SetText(const System::WideString AText);
	void __fastcall SetAdjust(TGLTextAdjust* const Value);
	void __fastcall SetAspectRatio(const float Value);
	void __fastcall SetOblique(const float Value);
	void __fastcall SetTextHeight(const float Value);
	
protected:
	TFontEntry *FTextFontEntry;
	bool FontChanged;
	virtual void __fastcall DestroyHandle();
	void __fastcall OnFontChange(System::TObject* sender);
	void __fastcall GetFirstAndLastChar(int &firstChar, int &lastChar);
	void __fastcall DoOnLinesChange(System::TObject* sender);
	
public:
	__fastcall virtual TGLSpaceText(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSpaceText();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	float __fastcall TextWidth(const System::WideString str = System::WideString());
	float __fastcall TextMaxHeight(const System::WideString str = System::WideString());
	float __fastcall TextMaxUnder(const System::WideString str = System::WideString());
	void __fastcall TextMetrics(const System::WideString str, /* out */ float &width, /* out */ float &maxHeight, /* out */ float &maxUnder);
	void __fastcall NotifyFontChanged();
	virtual void __fastcall NotifyChange(System::TObject* sender);
	virtual void __fastcall DefaultHandler(void *Message);
	virtual Gls::Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled();
	virtual Gls::Vectortypes::TVector4f __fastcall BarycenterAbsolutePosition();
	
__published:
	__property float Extrusion = {read=FExtrusion, write=SetExtrusion};
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property System::WideString Text = {read=GetText, write=SetText, stored=false};
	__property System::Classes::TStringList* Lines = {read=FLines, write=SetLines};
	__property float allowedDeviation = {read=FAllowedDeviation, write=SetAllowedDeviation};
	__property TGLSpaceTextCharRange CharacterRange = {read=FCharacterRange, write=SetCharacterRange, default=0};
	__property float AspectRatio = {read=FAspectRatio, write=SetAspectRatio};
	__property float TextHeight = {read=FTextHeight, write=SetTextHeight};
	__property float Oblique = {read=FOblique, write=SetOblique};
	__property TGLTextAdjust* Adjust = {read=FAdjust, write=SetAdjust};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSpaceText(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLSceneObject(aParentOwner) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TFontManager : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
private:
	int FCurrentBase;
	
protected:
	void __fastcall NotifyClients(System::Classes::TList* Clients);
	void __fastcall VirtualHandleAlloc(Gls::Context::TGLVirtualHandle* sender, unsigned &handle);
	void __fastcall VirtualHandleDestroy(Gls::Context::TGLVirtualHandle* sender, unsigned &handle);
	
public:
	__fastcall TFontManager();
	__fastcall virtual ~TFontManager();
	PFontEntry __fastcall FindFont(System::UnicodeString AName, System::Uitypes::TFontStyles FStyles, float FExtrusion, float FAllowedDeviation, int FFirstChar, int FLastChar);
	PFontEntry __fastcall GetFontBase(System::UnicodeString AName, System::Uitypes::TFontStyles FStyles, float FExtrusion, float allowedDeviation, int firstChar, int lastChar, System::TObject* client);
	void __fastcall Release(PFontEntry entry, System::TObject* client);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE unsigned vFontManagerMsgID;
extern DELPHI_PACKAGE TFontManager* __fastcall FontManager(void);
extern DELPHI_PACKAGE void __fastcall ReleaseFontManager(void);
}	/* namespace Spacetext */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_SPACETEXT)
using namespace Gls::Spacetext;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_SpacetextHPP
