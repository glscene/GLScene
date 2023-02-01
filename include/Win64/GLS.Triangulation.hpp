// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Triangulation.pas' rev: 35.00 (Windows)

#ifndef Gls_TriangulationHPP
#define Gls_TriangulationHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <Vcl.Dialogs.hpp>
#include <GLS.VectorGeometry.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Triangulation
{
//-- forward type declarations -----------------------------------------------
struct DVertex;
struct DTriangle;
class DELPHICLASS TGLDelaunay2D;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TDProgressEvent)(System::UnicodeString State, int Pos, int Max, bool AlwaysUpdate/* = false*/);

struct DECLSPEC_DRECORD DVertex
{
public:
	float X;
	float Y;
	float Z;
	float U;
	float V;
	int MatIndex;
};


struct DECLSPEC_DRECORD DTriangle
{
public:
	int vv0;
	int vv1;
	int vv2;
	int PreCalc;
	float Xc;
	float Yc;
	float R;
};


typedef System::DynamicArray<DVertex> TDVertex;

typedef System::DynamicArray<DTriangle> TDTriangle;

typedef System::DynamicArray<bool> TDComplete;

typedef System::DynamicArray<int> Gls_Triangulation__1;

typedef System::DynamicArray<System::DynamicArray<int> > TDEdges;

class PASCALIMPLEMENTATION TGLDelaunay2D : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool __fastcall InCircle(float Xp, float Yp, float X1, float Y1, float X2, float Y2, float X3, float Y3, /* out */ float &Xc, /* out */ float &Yc, /* out */ float &R, int j);
	int __fastcall Triangulate(int nvert);
	
public:
	TDVertex Vertex;
	TDTriangle Triangle;
	int HowMany;
	int TPoints;
	TDProgressEvent OnProgress;
	__fastcall TGLDelaunay2D();
	__fastcall virtual ~TGLDelaunay2D();
	void __fastcall Mesh(bool sort);
	void __fastcall AddPoint(float X, float Y, float Z, float U, float V, int MatIndex);
	void __fastcall AddPointNoCheck(float X, float Y, float Z, float U, float V, int MatIndex);
	void __fastcall RemoveLastPoint();
	void __fastcall QuickSort(TDVertex &A, int Low, int High);
};


//-- var, const, procedure ---------------------------------------------------
static const int MaxVertices = int(0x7a120);
static const int MaxTriangles = int(0xf4240);
static const double ExPtTolerance = 1.000000E-06;
}	/* namespace Triangulation */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_TRIANGULATION)
using namespace Gls::Triangulation;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_TriangulationHPP
