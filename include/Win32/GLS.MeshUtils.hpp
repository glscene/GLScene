// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.MeshUtils.pas' rev: 35.00 (Windows)

#ifndef Gls_MeshutilsHPP
#define Gls_MeshutilsHPP

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
#include <GLS.PersistentClasses.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Meshutils
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef void __fastcall (*TSubdivideEdgeEvent)(const int idxA, const int idxB, const int newIdx);

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool vImprovedFixingOpenTriangleEdge;
extern DELPHI_PACKAGE unsigned vEdgeInfoReserveSize;
extern DELPHI_PACKAGE void __fastcall ConvertStripToList(Gls::Vectorlists::TGLAffineVectorList* const strip, Gls::Vectorlists::TGLAffineVectorList* list)/* overload */;
extern DELPHI_PACKAGE void __fastcall ConvertStripToList(Gls::Vectorlists::TGLIntegerList* const strip, Gls::Vectorlists::TGLIntegerList* list)/* overload */;
extern DELPHI_PACKAGE void __fastcall ConvertStripToList(Gls::Vectorlists::TGLAffineVectorList* const strip, Gls::Vectorlists::TGLIntegerList* const indices, Gls::Vectorlists::TGLAffineVectorList* list)/* overload */;
extern DELPHI_PACKAGE void __fastcall ConvertIndexedListToList(Gls::Vectorlists::TGLAffineVectorList* const data, Gls::Vectorlists::TGLIntegerList* const indices, Gls::Vectorlists::TGLAffineVectorList* list);
extern DELPHI_PACKAGE Gls::Vectorlists::TGLIntegerList* __fastcall BuildVectorCountOptimizedIndices(Gls::Vectorlists::TGLAffineVectorList* const vertices, Gls::Vectorlists::TGLAffineVectorList* const normals = (Gls::Vectorlists::TGLAffineVectorList*)(0x0), Gls::Vectorlists::TGLAffineVectorList* const texCoords = (Gls::Vectorlists::TGLAffineVectorList*)(0x0));
extern DELPHI_PACKAGE void __fastcall RemapReferences(Gls::Vectorlists::TGLAffineVectorList* reference, Gls::Vectorlists::TGLIntegerList* const indices)/* overload */;
extern DELPHI_PACKAGE void __fastcall RemapReferences(Gls::Vectorlists::TGLIntegerList* reference, Gls::Vectorlists::TGLIntegerList* const indices)/* overload */;
extern DELPHI_PACKAGE void __fastcall RemapAndCleanupReferences(Gls::Vectorlists::TGLAffineVectorList* reference, Gls::Vectorlists::TGLIntegerList* indices);
extern DELPHI_PACKAGE Gls::Vectorlists::TGLIntegerList* __fastcall RemapIndicesToIndicesMap(Gls::Vectorlists::TGLIntegerList* remapIndices);
extern DELPHI_PACKAGE void __fastcall RemapTrianglesIndices(Gls::Vectorlists::TGLIntegerList* indices, Gls::Vectorlists::TGLIntegerList* indicesMap);
extern DELPHI_PACKAGE void __fastcall remapIndices(Gls::Vectorlists::TGLIntegerList* indices, Gls::Vectorlists::TGLIntegerList* indicesMap);
extern DELPHI_PACKAGE void __fastcall UnifyTrianglesWinding(Gls::Vectorlists::TGLIntegerList* indices);
extern DELPHI_PACKAGE void __fastcall InvertTrianglesWinding(Gls::Vectorlists::TGLIntegerList* indices);
extern DELPHI_PACKAGE Gls::Vectorlists::TGLAffineVectorList* __fastcall BuildNormals(Gls::Vectorlists::TGLAffineVectorList* reference, Gls::Vectorlists::TGLIntegerList* indices);
extern DELPHI_PACKAGE Gls::Vectorlists::TGLIntegerList* __fastcall BuildNonOrientedEdgesList(Gls::Vectorlists::TGLIntegerList* triangleIndices, Gls::Vectorlists::TGLIntegerList* triangleEdges = (Gls::Vectorlists::TGLIntegerList*)(0x0), Gls::Vectorlists::TGLIntegerList* edgesTriangles = (Gls::Vectorlists::TGLIntegerList*)(0x0));
extern DELPHI_PACKAGE void __fastcall IncreaseCoherency(Gls::Vectorlists::TGLIntegerList* indices, int cacheSize);
extern DELPHI_PACKAGE void __fastcall WeldVertices(Gls::Vectorlists::TGLAffineVectorList* vertices, Gls::Vectorlists::TGLIntegerList* indicesMap, float weldRadius);
extern DELPHI_PACKAGE Gls::Persistentclasses::TGLPersistentObjectList* __fastcall StripifyMesh(Gls::Vectorlists::TGLIntegerList* indices, int maxVertexIndex, bool agglomerateLoneTriangles = false);
extern DELPHI_PACKAGE void __fastcall SubdivideTriangles(float smoothFactor, Gls::Vectorlists::TGLAffineVectorList* vertices, Gls::Vectorlists::TGLIntegerList* triangleIndices, Gls::Vectorlists::TGLAffineVectorList* normals = (Gls::Vectorlists::TGLAffineVectorList*)(0x0), TSubdivideEdgeEvent onSubdivideEdge = 0x0);
extern DELPHI_PACKAGE Gls::Vectorlists::TGLLongWordList* __fastcall MakeTriangleAdjacencyList(const Gls::Vectorgeometry::PLongWordVector AindicesList, unsigned Count, const Gls::Vectorgeometry::PAffineVectorArray AVerticesList);
extern DELPHI_PACKAGE Gls::Vectorlists::TGLLongWordList* __fastcall ConvertStripToList(const Gls::Vectorgeometry::PLongWordVector AindicesList, unsigned Count, unsigned RestartIndex)/* overload */;
extern DELPHI_PACKAGE Gls::Vectorlists::TGLLongWordList* __fastcall ConvertFansToList(const Gls::Vectorgeometry::PLongWordVector AindicesList, unsigned Count, unsigned RestartIndex);
}	/* namespace Meshutils */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_MESHUTILS)
using namespace Gls::Meshutils;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_MeshutilsHPP
