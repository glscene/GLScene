// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Silhouette.pas' rev: 35.00 (Windows)

#ifndef Gls_SilhouetteHPP
#define Gls_SilhouetteHPP

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
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorLists.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Silhouette
{
//-- forward type declarations -----------------------------------------------
struct TGLSilhouetteParameters;
class DELPHICLASS TGLSilhouette;
class DELPHICLASS TGLBaseConnectivity;
class DELPHICLASS TGLConnectivity;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLSilhouetteStyle : unsigned char { ssOmni, ssParallel };

#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLSilhouetteParameters
{
public:
	Gls::Vectortypes::TVector3f SeenFrom;
	Gls::Vectortypes::TVector3f LightDirection;
	TGLSilhouetteStyle Style;
	bool CappingRequired;
};
#pragma pack(pop)


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSilhouette : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Gls::Vectorlists::TGLVectorList* FVertices;
	Gls::Vectorlists::TGLIntegerList* FIndices;
	Gls::Vectorlists::TGLIntegerList* FCapIndices;
	TGLSilhouetteParameters FParameters;
	
protected:
	void __fastcall SetIndices(Gls::Vectorlists::TGLIntegerList* const value);
	void __fastcall SetCapIndices(Gls::Vectorlists::TGLIntegerList* const value);
	void __fastcall SetVertices(Gls::Vectorlists::TGLVectorList* const value);
	
public:
	__fastcall virtual TGLSilhouette();
	__fastcall virtual ~TGLSilhouette();
	__property TGLSilhouetteParameters Parameters = {read=FParameters, write=FParameters};
	__property Gls::Vectorlists::TGLVectorList* Vertices = {read=FVertices, write=SetVertices};
	__property Gls::Vectorlists::TGLIntegerList* Indices = {read=FIndices, write=SetIndices};
	__property Gls::Vectorlists::TGLIntegerList* CapIndices = {read=FCapIndices, write=SetCapIndices};
	virtual void __fastcall Flush();
	void __fastcall Clear();
	void __fastcall ExtrudeVerticesToInfinity(const Gls::Vectortypes::TVector3f &origin);
	void __fastcall AddEdgeToSilhouette(const Gls::Vectortypes::TVector3f &v0, const Gls::Vectortypes::TVector3f &v1, bool tightButSlow);
	void __fastcall AddIndexedEdgeToSilhouette(const int Vi0, const int Vi1);
	void __fastcall AddCapToSilhouette(const Gls::Vectortypes::TVector3f &v0, const Gls::Vectortypes::TVector3f &v1, const Gls::Vectortypes::TVector3f &v2, bool tightButSlow);
	void __fastcall AddIndexedCapToSilhouette(const int Vi0, const int Vi1, const int vi2);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBaseConnectivity : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	bool FPrecomputeFaceNormal;
	virtual int __fastcall GetEdgeCount();
	virtual int __fastcall GetFaceCount();
	
public:
	__property int EdgeCount = {read=GetEdgeCount, nodefault};
	__property int FaceCount = {read=GetFaceCount, nodefault};
	__property bool PrecomputeFaceNormal = {read=FPrecomputeFaceNormal, nodefault};
	virtual void __fastcall CreateSilhouette(const TGLSilhouetteParameters &ASilhouetteParameters, TGLSilhouette* &ASilhouette, bool AddToSilhouette);
	__fastcall virtual TGLBaseConnectivity(bool APrecomputeFaceNormal);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLBaseConnectivity() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLConnectivity : public TGLBaseConnectivity
{
	typedef TGLBaseConnectivity inherited;
	
protected:
	Gls::Vectorlists::TGLIntegerList* FEdgeVertices;
	Gls::Vectorlists::TGLIntegerList* FEdgeFaces;
	Gls::Vectorlists::TGLByteList* FFaceVisible;
	Gls::Vectorlists::TGLIntegerList* FFaceVertexIndex;
	Gls::Vectorlists::TGLAffineVectorList* FFaceNormal;
	Gls::Vectorlists::TGLIntegerList* FVertexMemory;
	Gls::Vectorlists::TGLAffineVectorList* FVertices;
	HIDESBASE int __fastcall GetEdgeCount();
	HIDESBASE int __fastcall GetFaceCount();
	int __fastcall ReuseOrFindVertexID(const Gls::Vectortypes::TVector3f &SeenFrom, TGLSilhouette* ASilhouette, int index);
	
public:
	virtual void __fastcall Clear();
	HIDESBASE void __fastcall CreateSilhouette(const TGLSilhouetteParameters &silhouetteParameters, TGLSilhouette* &ASilhouette, bool AddToSilhouette);
	int __fastcall AddIndexedEdge(int vertexIndex0, int vertexIndex1, int FaceID);
	int __fastcall AddIndexedFace(int Vi0, int Vi1, int vi2);
	int __fastcall AddFace(const Gls::Vectortypes::TVector3f &vertex0, const Gls::Vectortypes::TVector3f &vertex1, const Gls::Vectortypes::TVector3f &vertex2);
	int __fastcall AddQuad(const Gls::Vectortypes::TVector3f &vertex0, const Gls::Vectortypes::TVector3f &vertex1, const Gls::Vectortypes::TVector3f &vertex2, const Gls::Vectortypes::TVector3f &vertex3);
	__property int EdgeCount = {read=GetEdgeCount, nodefault};
	__property int FaceCount = {read=GetFaceCount, nodefault};
	__fastcall virtual TGLConnectivity(bool APrecomputeFaceNormal);
	__fastcall virtual ~TGLConnectivity();
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Silhouette */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_SILHOUETTE)
using namespace Gls::Silhouette;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_SilhouetteHPP
