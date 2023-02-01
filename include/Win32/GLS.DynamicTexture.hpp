// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.DynamicTexture.pas' rev: 35.00 (Windows)

#ifndef Gls_DynamictextureHPP
#define Gls_DynamictextureHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.OpenGLext.hpp>
#include <System.Types.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Utils.hpp>
#include <GLS.Context.hpp>
#include <GLS.Texture.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Dynamictexture
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLDynamicTextureImage;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLDynamicTextureImage : public Gls::Texture::TGLBlankImage
{
	typedef Gls::Texture::TGLBlankImage inherited;
	
private:
	int FUpdating;
	int FTexSize;
	void *FBuffer;
	Gls::Context::TGLBufferObjectHandle* FPBO;
	void *FData;
	System::Types::TRect FDirtyRect;
	bool FUseBGR;
	bool FUsePBO;
	void __fastcall SetDirtyRectangle(const System::Types::TRect &Value);
	void __fastcall SetUsePBO(const bool Value);
	
protected:
	int __fastcall GetTexSize();
	int __fastcall GetBitsPerPixel();
	int __fastcall GetDataFormat();
	int __fastcall GetTextureFormat();
	void __fastcall FreePBO();
	void __fastcall FreeBuffer();
	__property int BitsPerPixel = {read=GetBitsPerPixel, nodefault};
	__property int DataFormat = {read=GetDataFormat, nodefault};
	__property int TextureFormat = {read=GetTextureFormat, nodefault};
	
public:
	__fastcall virtual TGLDynamicTextureImage(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLDynamicTextureImage();
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	HIDESBASE void __fastcall BeginUpdate();
	HIDESBASE void __fastcall EndUpdate();
	__property void * Data = {read=FData};
	__property System::Types::TRect DirtyRectangle = {read=FDirtyRect, write=SetDirtyRectangle};
	__property bool UseBGR = {read=FUseBGR, write=FUseBGR, nodefault};
	__property bool UsePBO = {read=FUsePBO, write=SetUsePBO, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Dynamictexture */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_DYNAMICTEXTURE)
using namespace Gls::Dynamictexture;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_DynamictextureHPP
