// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.XOpenGL.pas' rev: 35.00 (Windows)

#ifndef Gls_XopenglHPP
#define Gls_XopenglHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGLext.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.State.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Xopengl
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLMultitextureCoordinator;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TMapTexCoordMode : unsigned char { mtcmUndefined, mtcmNull, mtcmMain, mtcmDual, mtcmSecond, mtcmArbitrary };

class PASCALIMPLEMENTATION TGLMultitextureCoordinator : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<TMapTexCoordMode> _TGLMultitextureCoordinator__1;
	
	typedef System::DynamicArray<unsigned> _TGLMultitextureCoordinator__2;
	
	
private:
	TMapTexCoordMode FMapTexCoordMode;
	bool FSecondTextureUnitForbidden;
	int FUpdCount;
	TMapTexCoordMode FUpdNewMode;
	_TGLMultitextureCoordinator__1 FStateStack;
	_TGLMultitextureCoordinator__2 FComplexMapping;
	int FComplexMappingN;
	
public:
	void __stdcall (*TexCoord2f)(float s, float t);
	void __stdcall (*TexCoord2fv)(System::PSingle v);
	void __stdcall (*TexCoord3f)(float s, float t, float r);
	void __stdcall (*TexCoord3fv)(System::PSingle v);
	void __stdcall (*TexCoord4f)(float s, float t, float r, float q);
	void __stdcall (*TexCoord4fv)(System::PSingle v);
	void __stdcall (*TexGenf)(unsigned coord, unsigned pname, float param);
	void __stdcall (*TexGenfv)(unsigned coord, unsigned pname, System::PSingle params);
	void __stdcall (*TexGeni)(unsigned coord, unsigned pname, int param);
	void __stdcall (*TexGeniv)(unsigned coord, unsigned pname, System::PInteger params);
	void __stdcall (*TexCoordPointer)(int size, unsigned atype, int stride, void * data);
	void __stdcall (*EnableClientState)(unsigned aarray);
	void __stdcall (*DisableClientState)(unsigned aarray);
	void __stdcall (*Enable)(unsigned cap);
	void __stdcall (*Disable)(unsigned cap);
	__fastcall TGLMultitextureCoordinator();
	void __fastcall MapTexCoordToNull();
	void __fastcall MapTexCoordToMain();
	void __fastcall MapTexCoordToSecond();
	void __fastcall MapTexCoordToDual();
	void __fastcall MapTexCoordToArbitrary(const unsigned *units, const int units_High)/* overload */;
	void __fastcall MapTexCoordToArbitrary(const unsigned bitWiseUnits)/* overload */;
	void __fastcall MapTexCoordToArbitraryAdd(const unsigned bitWiseUnits);
	void __fastcall BeginUpdate();
	void __fastcall EndUpdate();
	void __fastcall PushState();
	void __fastcall PopState();
	void __fastcall ForbidSecondTextureUnit();
	void __fastcall AllowSecondTextureUnit();
	unsigned __fastcall GetBitWiseMapping();
	__property TMapTexCoordMode MapTexCoordMode = {read=FMapTexCoordMode, write=FMapTexCoordMode, nodefault};
	__property bool SecondTextureUnitForbidden = {read=FSecondTextureUnitForbidden, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLMultitextureCoordinator() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Xopengl */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_XOPENGL)
using namespace Gls::Xopengl;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_XopenglHPP
