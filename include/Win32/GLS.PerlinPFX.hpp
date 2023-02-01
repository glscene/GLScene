// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.PerlinPFX.pas' rev: 35.00 (Windows)

#ifndef Gls_PerlinpfxHPP
#define Gls_PerlinpfxHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.Classes.hpp>
#include <System.Math.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.ParticleFX.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Cadencer.hpp>
#include <GLS.Color.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Perlinpfx
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLPerlinPFXManager;
class DELPHICLASS TGLPerlin3DNoise;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLPerlinPFXManager : public Gls::Particlefx::TGLBaseSpritePFXManager
{
	typedef Gls::Particlefx::TGLBaseSpritePFXManager inherited;
	
private:
	int FTexMapSize;
	int FNoiseSeed;
	int FNoiseScale;
	int FNoiseAmplitude;
	float FSmoothness;
	float FBrightness;
	float FGamma;
	
protected:
	virtual void __fastcall PrepareImage(Gls::Graphics::TGLImage* bmp32, int &texFormat);
	void __fastcall SetTexMapSize(const int val);
	void __fastcall SetNoiseSeed(const int val);
	void __fastcall SetNoiseScale(const int val);
	void __fastcall SetNoiseAmplitude(const int val);
	void __fastcall SetSmoothness(const float val);
	void __fastcall SetBrightness(const float val);
	void __fastcall SetGamma(const float val);
	
public:
	__fastcall virtual TGLPerlinPFXManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLPerlinPFXManager();
	
__published:
	__property int TexMapSize = {read=FTexMapSize, write=SetTexMapSize, default=6};
	__property float Smoothness = {read=FSmoothness, write=SetSmoothness};
	__property float Brightness = {read=FBrightness, write=SetBrightness};
	__property float Gamma = {read=FGamma, write=SetGamma};
	__property int NoiseSeed = {read=FNoiseSeed, write=SetNoiseSeed, default=0};
	__property int NoiseScale = {read=FNoiseScale, write=SetNoiseScale, default=100};
	__property int NoiseAmplitude = {read=FNoiseAmplitude, write=SetNoiseAmplitude, default=50};
	__property ColorMode = {default=1};
	__property SpritesPerTexture = {default=1};
	__property ParticleSize = {default=0};
	__property ColorInner;
	__property ColorOuter;
	__property LifeColors;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLPerlin3DNoise : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	System::StaticArray<int, 256> FPermutations;
	System::StaticArray<float, 768> FGradients;
	float __fastcall Lattice(int ix, int iy, int iz, float fx, float fy, float fz)/* overload */;
	float __fastcall Lattice(int ix, int iy, float fx, float fy)/* overload */;
	
public:
	__fastcall TGLPerlin3DNoise(int randomSeed);
	void __fastcall Initialize(int randomSeed);
	float __fastcall Noise(const float x, const float y)/* overload */;
	float __fastcall Noise(const float x, const float y, const float z)/* overload */;
	float __fastcall Noise(const Gls::Vectortypes::TVector3f &v)/* overload */;
	float __fastcall Noise(const Gls::Vectortypes::TVector4f &v)/* overload */;
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLPerlin3DNoise() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Word cPERLIN_TABLE_SIZE = System::Word(0x100);
}	/* namespace Perlinpfx */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_PERLINPFX)
using namespace Gls::Perlinpfx;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_PerlinpfxHPP
