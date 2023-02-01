// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Formats.m3DSUtils.pas' rev: 35.00 (Windows)

#ifndef Formats_M3dsutilsHPP
#define Formats_M3dsutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Formats.m3DS.hpp>
#include <Formats.m3DSTypes.hpp>
#include <Formats.m3DSConst.hpp>
#include <GLS.Strings.hpp>

//-- user supplied -----------------------------------------------------------

namespace Formats
{
namespace M3dsutils
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall ShowError(const System::UnicodeString ErrorMessage);
extern DELPHI_PACKAGE void __fastcall ShowErrorFormatted(const System::UnicodeString ErrorMessage, const System::TVarRec *Args, const int Args_High);
extern DELPHI_PACKAGE Formats::M3dstypes::TMeshSet3DS __fastcall GetMeshSet(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB);
extern DELPHI_PACKAGE Formats::M3dstypes::TAtmosphere3DS __fastcall GetAtmosphere(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB);
extern DELPHI_PACKAGE Formats::M3dstypes::TBackground3DS __fastcall GetBackground(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB);
extern DELPHI_PACKAGE Formats::M3dstypes::TViewport3DS __fastcall GetViewport(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ChunkTagToString(System::Word Tag);
extern DELPHI_PACKAGE void __fastcall ChunkHeaderReport(System::Classes::TStrings* &Strings, Formats::M3dstypes::PChunk3DS Chunk, int IndentLevel);
extern DELPHI_PACKAGE void __fastcall DumpKeyHeader(System::Classes::TStrings* Strings, const Formats::M3dstypes::TKeyHeader3DS &Key, int IndentLevel);
extern DELPHI_PACKAGE void __fastcall DumpChunk(Formats::M3ds::TFile3DS* const Source, System::Classes::TStrings* &Strings, Formats::M3dstypes::PChunk3DS Chunk, int IndentLevel, Formats::M3dstypes::TDumpLevel DumpLevel);
extern DELPHI_PACKAGE void __fastcall AddChild(Formats::M3dstypes::PChunk3DS Parent, Formats::M3dstypes::PChunk3DS Child);
extern DELPHI_PACKAGE void __fastcall AddChildOrdered(Formats::M3dstypes::PChunk3DS Parent, Formats::M3dstypes::PChunk3DS Child);
extern DELPHI_PACKAGE Formats::M3dstypes::PChunk3DS __fastcall FindChunk(Formats::M3dstypes::PChunk3DS Top, System::Word Tag);
extern DELPHI_PACKAGE Formats::M3dstypes::PChunk3DS __fastcall FindNextChunk(Formats::M3dstypes::PChunk3DS Local, System::Word Tag);
extern DELPHI_PACKAGE void __fastcall FreeChunkData(Formats::M3dstypes::PChunk3DS &Chunk);
extern DELPHI_PACKAGE void __fastcall InitChunk(Formats::M3dstypes::PChunk3DS &Chunk);
extern DELPHI_PACKAGE void __fastcall ReleaseChunk(Formats::M3dstypes::PChunk3DS &Chunk);
extern DELPHI_PACKAGE void __fastcall ReleaseChunkList(Formats::M3dstypes::PChunkList3DS &List);
extern DELPHI_PACKAGE int __fastcall GetMaterialCount(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB);
extern DELPHI_PACKAGE void __fastcall ReleaseMaterial(Formats::M3dstypes::PMaterial3DS Mat);
extern DELPHI_PACKAGE Formats::M3dstypes::PChunk3DS __fastcall FindNamedObjectByIndex(Formats::M3ds::TFile3DS* Source, const Formats::M3dstypes::TDatabase3DS &DB, System::Word AType, int Index);
extern DELPHI_PACKAGE void __fastcall DeleteChunk(Formats::M3dstypes::PChunk3DS &Chunk);
extern DELPHI_PACKAGE int __fastcall GetChunkValue(System::Word Tag);
extern DELPHI_PACKAGE Formats::M3dstypes::TMaterial3DS __fastcall GetMaterialByIndex(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB, int Index);
extern DELPHI_PACKAGE int __fastcall GetMeshCount(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB);
extern DELPHI_PACKAGE void __fastcall ReleaseMeshObj(Formats::M3dstypes::PMesh3DS Mesh);
extern DELPHI_PACKAGE Formats::M3dstypes::TMesh3DS __fastcall GetMeshByIndex(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB, int Index);
extern DELPHI_PACKAGE int __fastcall GetOmnilightCount(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB);
extern DELPHI_PACKAGE int __fastcall GetSpotlightCount(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB);
extern DELPHI_PACKAGE void __fastcall ReleaseLight(Formats::M3dstypes::PLight3DS Light);
extern DELPHI_PACKAGE Formats::M3dstypes::TLight3DS __fastcall GetOmnilightByIndex(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB, int Index);
extern DELPHI_PACKAGE Formats::M3dstypes::TLight3DS __fastcall GetSpotlightByIndex(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB, int Index);
extern DELPHI_PACKAGE void __fastcall ReleaseCamera(Formats::M3dstypes::PCamera3DS Camera);
extern DELPHI_PACKAGE int __fastcall GetCameraCount(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB);
extern DELPHI_PACKAGE Formats::M3dstypes::TCamera3DS __fastcall GetCameraByIndex(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB, int Index);
extern DELPHI_PACKAGE Formats::M3dstypes::TKFSets3DS __fastcall GetKFSettings(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB);
extern DELPHI_PACKAGE void __fastcall ReleaseCameraMotion(Formats::M3dstypes::PKFCamera3DS Camera);
extern DELPHI_PACKAGE void __fastcall GetCameraNodeNameList(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB, System::Classes::TStringList* List);
extern DELPHI_PACKAGE int __fastcall GetCameraNodeCount(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB);
extern DELPHI_PACKAGE Formats::M3dstypes::TKFCamera3DS __fastcall GetCameraMotion(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::PChunk3DS CamChunk, Formats::M3dstypes::PChunk3DS TargetChunk);
extern DELPHI_PACKAGE Formats::M3dstypes::TKFCamera3DS __fastcall GetCameraMotionByIndex(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB, int Index);
extern DELPHI_PACKAGE void __fastcall ReleaseAmbientLightMotion(Formats::M3dstypes::PKFAmbient3DS Light);
extern DELPHI_PACKAGE Formats::M3dstypes::TKFAmbient3DS __fastcall GetAmbientLightMotion(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB);
extern DELPHI_PACKAGE void __fastcall InitObjectMotion(Formats::M3dstypes::TKFMesh3DS &Obj, unsigned NewNPKeys, unsigned NewNRKeys, unsigned NewNSKeys, unsigned NewNMKeys, unsigned NewNHKeys);
extern DELPHI_PACKAGE void __fastcall ReleaseObjectMotion(Formats::M3dstypes::PKFMesh3DS Obj);
extern DELPHI_PACKAGE int __fastcall GetObjectNodeCount(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB);
extern DELPHI_PACKAGE void __fastcall GetObjectNodeNameList(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB, System::Classes::TStringList* List);
extern DELPHI_PACKAGE Formats::M3dstypes::TKFMesh3DS __fastcall GetObjectMotionByName(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB, const System::UnicodeString Name);
extern DELPHI_PACKAGE Formats::M3dstypes::TKFMesh3DS __fastcall GetObjectMotionByIndex(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB, unsigned Index);
extern DELPHI_PACKAGE void __fastcall ReleaseOmnilightMotion(Formats::M3dstypes::PKFOmni3DS Light);
extern DELPHI_PACKAGE unsigned __fastcall GetOmnilightNodeCount(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB);
extern DELPHI_PACKAGE void __fastcall GetOmnilightNodeNameList(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB, System::Classes::TStringList* List);
extern DELPHI_PACKAGE Formats::M3dstypes::TKFOmni3DS __fastcall GetOmnilightMotionByName(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB, const System::UnicodeString Name);
extern DELPHI_PACKAGE Formats::M3dstypes::TKFOmni3DS __fastcall GetOmnilightMotionByIndex(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB, unsigned Index);
extern DELPHI_PACKAGE void __fastcall ReleaseSpotlightMotion(Formats::M3dstypes::PKFSpot3DS Spot);
extern DELPHI_PACKAGE unsigned __fastcall GetSpotlightNodeCount(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB);
extern DELPHI_PACKAGE void __fastcall GetSpotlightNodeNameList(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB, System::Classes::TStringList* List);
extern DELPHI_PACKAGE Formats::M3dstypes::TKFSpot3DS __fastcall GetSpotlightMotionByName(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB, const System::UnicodeString Name);
extern DELPHI_PACKAGE Formats::M3dstypes::TKFSpot3DS __fastcall GetSpotlightMotionByIndex(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB, unsigned Index);
extern DELPHI_PACKAGE Formats::M3dstypes::TReleaseLevel __fastcall GetM3dMagicRelease(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB);
extern DELPHI_PACKAGE Formats::M3dstypes::TReleaseLevel __fastcall GetMeshRelease(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB);
extern DELPHI_PACKAGE Formats::M3dstypes::TReleaseLevel __fastcall GetKfRelease(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB);
extern DELPHI_PACKAGE Formats::M3dstypes::TReleaseLevel __fastcall GetDatabaseRelease(Formats::M3ds::TFile3DS* const Source, Formats::M3dstypes::TDatabase3DS &DB);
}	/* namespace M3dsutils */
}	/* namespace Formats */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FORMATS_M3DSUTILS)
using namespace Formats::M3dsutils;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FORMATS)
using namespace Formats;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Formats_M3dsutilsHPP
