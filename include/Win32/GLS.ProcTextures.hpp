// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.ProcTextures.pas' rev: 35.00 (Windows)

#ifndef Gls_ProctexturesHPP
#define Gls_ProctexturesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <GLS.Texture.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Proctextures
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLProcTextureNoise;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLProcTextureNoise : public Gls::Texture::TGLTextureImage
{
	typedef Gls::Texture::TGLTextureImage inherited;
	
private:
	Gls::Graphics::TGLImage* FNoiseMap;
	int FWidth;
	int FHeight;
	System::Byte FMinCut;
	float FNoiseSharpness;
	float FNoiseAnimate;
	bool FSeamless;
	int FNoiseRandSeed;
	
protected:
	System::StaticArray<float, 768> FGradients;
	System::StaticArray<System::Byte, 256> PERM;
	virtual int __fastcall GetWidth();
	virtual int __fastcall GetHeight();
	virtual int __fastcall GetDepth();
	virtual Gls::Textureformat::TGLTextureTarget __fastcall GetTextureTarget();
	float __fastcall Noise(float x, float y);
	void __fastcall SetMinCut(const System::Byte val);
	void __fastcall SetSeamless(const bool val);
	void __fastcall SetWidth(const int val);
	void __fastcall SetHeight(const int val);
	void __fastcall SetNoiseSharpness(const float val);
	void __fastcall SetNoiseRandSeed(const int val);
	void __fastcall UpdateNoise();
	
public:
	__fastcall virtual TGLProcTextureNoise(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLProcTextureNoise();
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Gls::Graphics::TGLImage* __fastcall GetBitmap32();
	virtual void __fastcall ReleaseBitmap32();
	virtual void __fastcall SaveToFile(const System::UnicodeString fileName);
	virtual void __fastcall LoadFromFile(const System::UnicodeString fileName);
	void __fastcall NoiseAnimate(float speed);
	void __fastcall SetPermFromData(const System::Byte *inPERM, const int inPERM_High);
	void __fastcall SetPermToDefault();
	
__published:
	__property int Width = {read=GetWidth, write=SetWidth, default=128};
	__property int Height = {read=GetHeight, write=SetHeight, default=128};
	__property int Depth = {read=GetDepth, nodefault};
	__property System::Byte MinCut = {read=FMinCut, write=SetMinCut, nodefault};
	__property float NoiseSharpness = {read=FNoiseSharpness, write=SetNoiseSharpness};
	__property bool Seamless = {read=FSeamless, write=SetSeamless, nodefault};
	__property int NoiseRandSeed = {read=FNoiseRandSeed, write=SetNoiseRandSeed, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word GRADIENT_TABLE_SIZE = System::Word(0x100);
static const System::Int8 DAMP_SHIFT = System::Int8(0x14);
}	/* namespace Proctextures */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_PROCTEXTURES)
using namespace Gls::Proctextures;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ProctexturesHPP
