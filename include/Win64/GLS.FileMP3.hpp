// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.FileMP3.pas' rev: 35.00 (Windows)

#ifndef Gls_Filemp3HPP
#define Gls_Filemp3HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.SoundFileObjects.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Filemp3
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLMP3File;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLMP3File : public Gls::Soundfileobjects::TGLSoundFile
{
	typedef Gls::Soundfileobjects::TGLSoundFile inherited;
	
	
private:
	typedef System::DynamicArray<System::Byte> _TGLMP3File__1;
	
	
private:
	_TGLMP3File__1 data;
	
public:
	virtual Gls::Applicationfileio::TGLDataFile* __fastcall CreateCopy(System::Classes::TPersistent* AOwner);
	__classmethod virtual Gls::Applicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* Stream);
	virtual void __fastcall PlayOnWaveOut();
	virtual void * __fastcall WAVData();
	virtual int __fastcall WAVDataSize();
	virtual void * __fastcall PCMData();
	virtual int __fastcall LengthInBytes();
public:
	/* TGLSoundFile.Create */ inline __fastcall virtual TGLMP3File(System::Classes::TPersistent* AOwner) : Gls::Soundfileobjects::TGLSoundFile(AOwner) { }
	/* TGLSoundFile.Destroy */ inline __fastcall virtual ~TGLMP3File() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Filemp3 */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_FILEMP3)
using namespace Gls::Filemp3;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_Filemp3HPP
