// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Isolines.pas' rev: 35.00 (Windows)

#ifndef Gls_IsolinesHPP
#define Gls_IsolinesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Math.hpp>
#include <System.Generics.Collections.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.Objects.hpp>
#include <GLS.MultiPolygon.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.VectorTypesExt.hpp>
#include <GLS.Color.hpp>
#include <GLS.Spline.hpp>
#include <GLS.SpaceText.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.Scene.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Isolines
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLIsoline;
class DELPHICLASS TGLIsolines;
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<float> TVectorArr;

typedef System::DynamicArray<System::Byte> TByteVectorArr;

typedef System::DynamicArray<float> Gls_Isolines__1;

typedef System::DynamicArray<System::DynamicArray<float> > TMatrixArr;

typedef System::DynamicArray<System::Byte> Gls_Isolines__2;

typedef System::DynamicArray<System::DynamicArray<System::Byte> > TByteMatrixArr;

typedef System::StaticArray<float, 5> TVectorL4D;

typedef System::StaticArray<int, 5> TVectorL4I;

typedef System::StaticArray<System::StaticArray<System::StaticArray<int, 3>, 3>, 3> TCastArray;

typedef System::StaticArray<Gls::Vectortypesext::TPoint2DRec, 32768> TVertex2DArr;

typedef TVertex2DArr *PVertex2DArr;

typedef TGLIsoline* *PGLIsoline;

class PASCALIMPLEMENTATION TGLIsoline : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	int NP;
	TVertex2DArr *Line;
	__fastcall virtual TGLIsoline(int LineSize);
	__fastcall virtual ~TGLIsoline();
};


enum DECLSPEC_DENUM TGLIsolineState : unsigned char { ilsEmpty, ilsCalculating, ilsReady };

class PASCALIMPLEMENTATION TGLIsolines : public Gls::Objects::TGLLines
{
	typedef Gls::Objects::TGLLines inherited;
	
	
private:
	typedef System::DynamicArray<Gls::Spacetext::TGLSpaceText*> _TGLIsolines__1;
	
	
public:
	Gls::Vectortypes::TVector3f IsoVertex;
	_TGLIsolines__1 GLSpaceTextSF;
	void __fastcall MakeIsolines(TMatrixArr &Depths, int bmSize, float StartDepth, float EndDepth, int Interval);
	void __fastcall FreeList();
	__fastcall virtual TGLIsolines(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLIsolines();
	void __fastcall Conrec(int PlaneSFindex, Gls::Vectorfileobjects::TGLFreeForm* PlaneSF, TMatrixArr Data, int ilb, int iub, int jlb, int jub, TVectorArr X, TVectorArr Y, int NC, TVectorArr HgtL, float Z_Kfix, float res3Dmax, float res3Dmin);
	
private:
	int CoordRange;
	System::Classes::TList* LineList;
	TGLIsolineState IsolineState;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLIsolines(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Objects::TGLLines(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Initialize_Contouring(TMatrixArr &DataGrid, int NXpoints, int NYpoints, float CurrentIsoline);
extern DELPHI_PACKAGE void __fastcall Release_Memory_Isoline(void);
extern DELPHI_PACKAGE bool __fastcall GetNextIsoline(TGLIsoline* &Isoline);
extern DELPHI_PACKAGE void __fastcall TriangleElevationSegments(const Gls::Vectortypes::TVector3f &p1, const Gls::Vectortypes::TVector3f &p2, const Gls::Vectortypes::TVector3f &p3, float ElevationDelta, Gls::Vectorlists::TGLAffineVectorList* Segments);
}	/* namespace Isolines */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_ISOLINES)
using namespace Gls::Isolines;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_IsolinesHPP
