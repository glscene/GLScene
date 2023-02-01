// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.FileVfsPAK.pas' rev: 35.00 (Windows)

#ifndef Gls_FilevfspakHPP
#define Gls_FilevfspakHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Contnrs.hpp>
#include <System.SysUtils.hpp>
#include <GLS.Strings.hpp>
#include <GLS.ApplicationFileIO.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Filevfspak
{
//-- forward type declarations -----------------------------------------------
struct TPakHeader;
struct TFileSection;
class DELPHICLASS TGLVfsPAK;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TZCompressedMode : unsigned char { Good, Fast, Auto, None };

struct DECLSPEC_DRECORD TPakHeader
{
public:
	System::StaticArray<char, 4> Signature;
	int DirOffset;
	int DirLength;
};


struct DECLSPEC_DRECORD TFileSection
{
public:
	System::StaticArray<char, 120> FileName;
	int FilePos;
	int FileLength;
};


class PASCALIMPLEMENTATION TGLVfsPAK : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
	
private:
	typedef System::DynamicArray<TPakHeader> _TGLVfsPAK__1;
	
	
private:
	System::Classes::TStringList* FPakFiles;
	TPakHeader FHeader;
	_TGLVfsPAK__1 FHeaderList;
	System::Classes::TFileStream* FStream;
	System::Contnrs::TObjectList* FStreamList;
	System::Classes::TStrings* FFiles;
	System::Contnrs::TObjectList* FFilesLists;
	System::UnicodeString FFileName;
	TZCompressedMode FCompressionLevel;
	bool FCompressed;
	int __fastcall GetFileCount();
	void __fastcall MakeFileList();
	int __fastcall GetStreamNumber();
	void __fastcall SetStreamNumber(int i);
	
public:
	__property System::Classes::TStringList* PakFiles = {read=FPakFiles};
	__property System::Classes::TStrings* Files = {read=FFiles};
	__property int ActivePakNum = {read=GetStreamNumber, write=SetStreamNumber, nodefault};
	__property int FileCount = {read=GetFileCount, nodefault};
	__property System::UnicodeString PakFileName = {read=FFileName};
	__property bool Compressed = {read=FCompressed, nodefault};
	__property TZCompressedMode CompressionLevel = {read=FCompressionLevel, nodefault};
	__fastcall virtual TGLVfsPAK(System::Classes::TComponent* AOwner)/* overload */;
	__fastcall TGLVfsPAK(System::Classes::TComponent* AOwner, const TZCompressedMode CbrMode)/* overload */;
	__fastcall virtual ~TGLVfsPAK();
	void __fastcall LoadFromFile(const System::UnicodeString FileName, System::Word Mode);
	void __fastcall ClearPakFiles();
	bool __fastcall FileExists(const System::UnicodeString FileName);
	System::Classes::TStream* __fastcall GetFile(int index)/* overload */;
	System::Classes::TStream* __fastcall GetFile(const System::UnicodeString FileName)/* overload */;
	int __fastcall GetFileSize(int index)/* overload */;
	int __fastcall GetFileSize(const System::UnicodeString FileName)/* overload */;
	void __fastcall AddFromStream(const System::UnicodeString FileName, const System::UnicodeString Path, System::Classes::TStream* F);
	void __fastcall AddFromFile(const System::UnicodeString FileName, const System::UnicodeString Path);
	void __fastcall AddEmptyFile(const System::UnicodeString FileName, const System::UnicodeString Path);
	void __fastcall RemoveFile(int index)/* overload */;
	void __fastcall RemoveFile(const System::UnicodeString FileName)/* overload */;
	void __fastcall Extract(int index, const System::UnicodeString NewName)/* overload */;
	void __fastcall Extract(const System::UnicodeString FileName, const System::UnicodeString NewName)/* overload */;
};


//-- var, const, procedure ---------------------------------------------------
#define SIGN L"PACK"
#define SIGN_COMPRESSED L"PACZ"
extern DELPHI_PACKAGE TGLVfsPAK* ActiveVfsPAK;
extern DELPHI_PACKAGE System::Classes::TStream* __fastcall PAKCreateFileStream(const System::UnicodeString FileName, System::Word Mode);
extern DELPHI_PACKAGE bool __fastcall PAKFileStreamExists(const System::UnicodeString FileName);
}	/* namespace Filevfspak */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_FILEVFSPAK)
using namespace Gls::Filevfspak;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_FilevfspakHPP
