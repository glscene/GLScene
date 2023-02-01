// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.TextureImageEditors.pas' rev: 35.00 (Windows)

#ifndef Gls_TextureimageeditorsHPP
#define Gls_TextureimageeditorsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.Texture.hpp>
#include <GLS.ProcTextures.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Textureimageeditors
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTextureImageEditor;
class DELPHICLASS TGLBlankTIE;
class DELPHICLASS TGLPersistentTIE;
class DELPHICLASS TGLPicFileTIE;
class DELPHICLASS TGLProcTextureNoiseTIE;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTextureImageEditor : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod virtual bool __fastcall Edit(Gls::Texture::TGLTextureImage* aTexImage);
public:
	/* TObject.Create */ inline __fastcall TGLTextureImageEditor() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLTextureImageEditor() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TGLTextureImageEditorClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBlankTIE : public TGLTextureImageEditor
{
	typedef TGLTextureImageEditor inherited;
	
public:
	__classmethod virtual bool __fastcall Edit(Gls::Texture::TGLTextureImage* aTexImage);
public:
	/* TObject.Create */ inline __fastcall TGLBlankTIE() : TGLTextureImageEditor() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLBlankTIE() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLPersistentTIE : public TGLTextureImageEditor
{
	typedef TGLTextureImageEditor inherited;
	
public:
	__classmethod virtual bool __fastcall Edit(Gls::Texture::TGLTextureImage* aTexImage);
public:
	/* TObject.Create */ inline __fastcall TGLPersistentTIE() : TGLTextureImageEditor() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLPersistentTIE() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLPicFileTIE : public TGLTextureImageEditor
{
	typedef TGLTextureImageEditor inherited;
	
public:
	__classmethod virtual bool __fastcall Edit(Gls::Texture::TGLTextureImage* aTexImage);
public:
	/* TObject.Create */ inline __fastcall TGLPicFileTIE() : TGLTextureImageEditor() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLPicFileTIE() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLProcTextureNoiseTIE : public TGLTextureImageEditor
{
	typedef TGLTextureImageEditor inherited;
	
public:
	__classmethod virtual bool __fastcall Edit(Gls::Texture::TGLTextureImage* aTexImage);
public:
	/* TObject.Create */ inline __fastcall TGLProcTextureNoiseTIE() : TGLTextureImageEditor() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLProcTextureNoiseTIE() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool __fastcall EditGLTextureImage(Gls::Texture::TGLTextureImage* aTexImage);
extern DELPHI_PACKAGE void __fastcall RegisterGLTextureImageEditor(Gls::Texture::TGLTextureImageClass aTexImageClass, TGLTextureImageEditorClass texImageEditor);
extern DELPHI_PACKAGE void __fastcall UnRegisterGLTextureImageEditor(TGLTextureImageEditorClass texImageEditor);
}	/* namespace Textureimageeditors */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_TEXTUREIMAGEEDITORS)
using namespace Gls::Textureimageeditors;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_TextureimageeditorsHPP
