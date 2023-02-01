// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.FileDXF.pas' rev: 35.00 (Windows)

#ifndef Gls_FiledxfHPP
#define Gls_FiledxfHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Texture.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.Material.hpp>
#include <GLS.Utils.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Filedxf
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLDXFVectorFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLDXFVectorFile : public Gls::Vectorfileobjects::TGLVectorFile
{
	typedef Gls::Vectorfileobjects::TGLVectorFile inherited;
	
private:
	System::Classes::TStream* FSourceStream;
	System::UnicodeString FBuffer;
	int FLineNo;
	bool FEof;
	int FBufPos;
	bool HasPushedCode;
	int PushedCode;
	System::Classes::TStringList* FLayers;
	System::Classes::TStringList* FBlocks;
	System::Byte FLastpercentdone;
	
protected:
	void __fastcall PushCode(int code);
	int __fastcall GetCode();
	void __fastcall SkipTable();
	void __fastcall SkipSection();
	Gls::Vectorfileobjects::TGLMeshObject* __fastcall NeedMesh(Gls::Vectorfileobjects::TGLBaseMesh* basemesh, System::UnicodeString layer);
	Gls::Vectorfileobjects::TFGVertexIndexList* __fastcall NeedFaceGroup(Gls::Vectorfileobjects::TGLMeshObject* m, Gls::Vectorfileobjects::TGLFaceGroupMeshMode fgmode, System::UnicodeString fgmat);
	void __fastcall NeedMeshAndFaceGroup(Gls::Vectorfileobjects::TGLBaseMesh* basemesh, System::UnicodeString layer, Gls::Vectorfileobjects::TGLFaceGroupMeshMode fgmode, System::UnicodeString fgmat, Gls::Vectorfileobjects::TGLMeshObject* &m, Gls::Vectorfileobjects::TFGVertexIndexList* &fg);
	System::UnicodeString __fastcall ReadLine();
	int __fastcall ReadInt();
	double __fastcall ReadDouble();
	void __fastcall ReadTables();
	void __fastcall ReadLayer();
	void __fastcall ReadLayerTable();
	void __fastcall ReadBlocks();
	void __fastcall ReadInsert(Gls::Vectorfileobjects::TGLBaseMesh* basemesh);
	void __fastcall ReadEntity3Dface(Gls::Vectorfileobjects::TGLBaseMesh* basemesh);
	void __fastcall ReadEntityPolyLine(Gls::Vectorfileobjects::TGLBaseMesh* basemesh);
	void __fastcall ReadEntities(Gls::Vectorfileobjects::TGLBaseMesh* basemesh);
	
public:
	__classmethod virtual Gls::Applicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGLDXFVectorFile(System::Classes::TPersistent* AOwner) : Gls::Vectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLDXFVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Filedxf */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_FILEDXF)
using namespace Gls::Filedxf;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_FiledxfHPP
