// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.VectorTypesExt.pas' rev: 35.00 (Windows)

#ifndef Gls_VectortypesextHPP
#define Gls_VectortypesextHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Types.hpp>
#include <System.TypInfo.hpp>
#include <System.SysUtils.hpp>
#include <System.Rtti.hpp>
#include <System.Math.hpp>
#include <System.Math.Vectors.hpp>
#include <GLS.VectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Vectortypesext
{
//-- forward type declarations -----------------------------------------------
struct TQuaternionRec;
struct TVectorRec;
struct TMatrixRec;
struct TQuaternionHelper /* Helper for record 'TQuaternionRec' */;
struct TVectorHelper /* Helper for record 'TVectorRec' */;
class DELPHICLASS TDim;
struct TVertexRec;
struct TFaceRec;
struct TPoint2DRec;
struct TPoint3DRec;
struct TVoxelRec;
struct TVector2DRec;
struct TVector3DRec;
struct TMatrix2DRec;
struct TMatrix3DRec;
struct TTriangleRec;
struct TMesh2DVert;
struct TMesh3DVert;
struct TQuat3DRec;
struct TBoxRec;
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<int> TIntegerArray;

typedef System::DynamicArray<Gls::Vectortypes::TVector3f> TVertexArray;

typedef System::DynamicArray<double> TDoubleArray;

typedef System::DynamicArray<System::Extended> TVectorExt;

typedef System::DynamicArray<System::Extended> Gls_Vectortypesext__1;

typedef System::DynamicArray<System::DynamicArray<System::Extended> > TMatrixExt;

typedef System::DynamicArray<System::Extended> Gls_Vectortypesext__2;

typedef System::DynamicArray<System::DynamicArray<System::Extended> > Gls_Vectortypesext__3;

typedef System::DynamicArray<System::DynamicArray<System::DynamicArray<System::Extended> > > TArray3DExt;

struct DECLSPEC_DRECORD TQuaternionRec
{
public:
	System::Extended operator[](System::Byte index) { return this->Element[index]; }
	
private:
	System::StaticArray<System::Extended, 4> FData;
	void __fastcall SetElement(System::Byte Index, System::Extended Value);
	System::Extended __fastcall GetElement(System::Byte Index);
	
public:
	__fastcall TQuaternionRec(TVectorExt Q);
	static TQuaternionRec __fastcall _op_Multiply(const TQuaternionRec &Q1, const TQuaternionRec &Q2);
	static TQuaternionRec __fastcall _op_Multiply(const TQuaternionRec &Q, System::Extended Sc);
	static TQuaternionRec __fastcall _op_Multiply(System::Extended Scalar, const TQuaternionRec &Q);
	static TQuaternionRec __fastcall _op_Implicit(TVectorExt V);
	TQuaternionRec __fastcall Inv();
	TQuaternionRec __fastcall TruncateSTI();
	__property System::Extended Element[System::Byte index] = {read=GetElement, write=SetElement};
	TQuaternionRec() {}
	
	friend TQuaternionRec operator *(const TQuaternionRec &Q1, const TQuaternionRec &Q2) { return TQuaternionRec::_op_Multiply(Q1, Q2); }
	friend TQuaternionRec operator *(const TQuaternionRec &Q, System::Extended Sc) { return TQuaternionRec::_op_Multiply(Q, Sc); }
	friend TQuaternionRec operator *(System::Extended Scalar, const TQuaternionRec &Q) { return TQuaternionRec::_op_Multiply(Scalar, Q); }
	TQuaternionRec& operator =(TVectorExt V) { *this = TQuaternionRec::_op_Implicit(V); return *this; }
};


typedef TVectorRec *PVectorRec;

struct DECLSPEC_DRECORD TVectorRec
{
public:
	System::Extended operator[](System::Word index) { return this->Elements[index]; }
	
private:
	TVectorExt FData;
	System::Word FCount;
	void __fastcall SetElement(System::Word Index, System::Extended Value);
	System::Extended __fastcall GetElement(System::Word Index);
	void __fastcall CheckUnique();
	
public:
	__fastcall TVectorRec(System::Word ElementsCount)/* overload */;
	__fastcall TVectorRec(TVectorExt V)/* overload */;
	static TVectorRec __fastcall _op_Addition(const TVectorRec &V1, const TVectorRec &V2);
	static TVectorRec __fastcall _op_Addition(const TVectorRec &V, System::Extended Scalar);
	static TVectorRec __fastcall _op_Addition(System::Extended Scalar, const TVectorRec &V);
	static TVectorRec __fastcall _op_Subtraction(const TVectorRec &V1, const TVectorRec &V2);
	static TVectorRec __fastcall _op_Subtraction(System::Extended Scalar, const TVectorRec &V);
	static TVectorRec __fastcall _op_Subtraction(const TVectorRec &V, System::Extended Scalar);
	static TVectorRec __fastcall _op_Multiply(const TVectorRec &V1, const TVectorRec &V2);
	static TVectorRec __fastcall _op_Multiply(const TVectorRec &V, System::Extended Scalar);
	static TVectorRec __fastcall _op_Multiply(System::Extended Scalar, const TVectorRec &V);
	static TVectorRec __fastcall _op_Division(const TVectorRec &V, System::Extended Scalar);
	static TVectorRec __fastcall _op_Division(const TVectorRec &V1, const TVectorRec &V2);
	static TVectorRec __fastcall _op_Implicit(TVectorExt V);
	System::Extended __fastcall Norm();
	System::Extended __fastcall SumOfSquares();
	System::Extended __fastcall SumOfElments();
	TVectorRec __fastcall TruncateSTI();
	TQuaternionRec __fastcall ToQuat();
	void __fastcall Fill(System::Extended Value);
	System::Extended __fastcall ScalarMult(const TVectorRec &V);
	__property System::Word Count = {read=FCount};
	__property System::Extended Elements[System::Word index] = {read=GetElement, write=SetElement};
	TVectorRec() {}
	
	friend TVectorRec operator +(const TVectorRec &V1, const TVectorRec &V2) { return TVectorRec::_op_Addition(V1, V2); }
	friend TVectorRec operator +(const TVectorRec &V, System::Extended Scalar) { return TVectorRec::_op_Addition(V, Scalar); }
	friend TVectorRec operator +(System::Extended Scalar, const TVectorRec &V) { return TVectorRec::_op_Addition(Scalar, V); }
	friend TVectorRec operator -(const TVectorRec &V1, const TVectorRec &V2) { return TVectorRec::_op_Subtraction(V1, V2); }
	friend TVectorRec operator -(System::Extended Scalar, const TVectorRec &V) { return TVectorRec::_op_Subtraction(Scalar, V); }
	friend TVectorRec operator -(const TVectorRec &V, System::Extended Scalar) { return TVectorRec::_op_Subtraction(V, Scalar); }
	friend TVectorRec operator *(const TVectorRec &V1, const TVectorRec &V2) { return TVectorRec::_op_Multiply(V1, V2); }
	friend TVectorRec operator *(const TVectorRec &V, System::Extended Scalar) { return TVectorRec::_op_Multiply(V, Scalar); }
	friend TVectorRec operator *(System::Extended Scalar, const TVectorRec &V) { return TVectorRec::_op_Multiply(Scalar, V); }
	friend TVectorRec operator /(const TVectorRec &V, System::Extended Scalar) { return TVectorRec::_op_Division(V, Scalar); }
	friend TVectorRec operator /(const TVectorRec &V1, const TVectorRec &V2) { return TVectorRec::_op_Division(V1, V2); }
	TVectorRec& operator =(TVectorExt V) { *this = TVectorRec::_op_Implicit(V); return *this; }
};


typedef TMatrixRec *PMatrixRec;

struct DECLSPEC_DRECORD TMatrixRec
{
private:
	TMatrixExt FData;
	System::Word FRowsCount;
	System::Word FColsCount;
	void __fastcall SetElement(System::Word Row, System::Word Col, System::Extended Value);
	System::Extended __fastcall GetElement(System::Word Row, System::Word Col);
	TVectorRec __fastcall GetRow(System::Word Row);
	void __fastcall SetRow(System::Word Row, const TVectorRec &Value);
	TVectorRec __fastcall GetCol(System::Word Col);
	void __fastcall SetCol(System::Word Col, const TVectorRec &Value);
	TMatrixRec __fastcall Del(const TMatrixRec &A, int I, int J, int M);
	System::Extended __fastcall Det(const TMatrixRec &A, int M);
	void __fastcall CheckUnique();
	
public:
	__fastcall TMatrixRec(System::Word RowsCount, System::Word ColsCount)/* overload */;
	__fastcall TMatrixRec(System::Word Dim, System::Extended Value);
	__fastcall TMatrixRec(TMatrixExt M)/* overload */;
	static TMatrixRec __fastcall _op_Addition(const TMatrixRec &M1, const TMatrixRec &M2);
	static TMatrixRec __fastcall _op_Subtraction(const TMatrixRec &M1, const TMatrixRec &M2);
	static TMatrixRec __fastcall _op_Multiply(const TMatrixRec &M1, const TMatrixRec &M2);
	static TVectorRec __fastcall _op_Multiply(const TMatrixRec &M, const TVectorRec &V);
	static TVectorRec __fastcall _op_Multiply(const TVectorRec &V, const TMatrixRec &M);
	static TMatrixRec __fastcall _op_Multiply(const TMatrixRec &M, System::Extended Scalar);
	static TMatrixRec __fastcall _op_Multiply(System::Extended Scalar, const TMatrixRec &M);
	static TQuaternionRec __fastcall _op_Multiply(const TMatrixRec &M, const TQuaternionRec &Q);
	static TMatrixRec __fastcall _op_Implicit(TMatrixExt M);
	TMatrixRec __fastcall Transp();
	TMatrixRec __fastcall Inv();
	TQuaternionRec __fastcall ToQuat();
	System::Extended __fastcall Determinant();
	TMatrixRec __fastcall TruncateSTI();
	System::Extended __fastcall Trace();
	void __fastcall Fill(System::Extended Scalar);
	__property System::Word RowCount = {read=FRowsCount};
	__property System::Word ColCount = {read=FColsCount};
	__property TVectorRec Row[System::Word Row] = {read=GetRow, write=SetRow};
	__property TVectorRec Col[System::Word Col] = {read=GetCol, write=SetCol};
	__property System::Extended Elements[System::Word Row][System::Word Col] = {read=GetElement, write=SetElement};
	TMatrixRec() {}
	
	friend TMatrixRec operator +(const TMatrixRec &M1, const TMatrixRec &M2) { return TMatrixRec::_op_Addition(M1, M2); }
	friend TMatrixRec operator -(const TMatrixRec &M1, const TMatrixRec &M2) { return TMatrixRec::_op_Subtraction(M1, M2); }
	friend TMatrixRec operator *(const TMatrixRec &M1, const TMatrixRec &M2) { return TMatrixRec::_op_Multiply(M1, M2); }
	friend TVectorRec operator *(const TMatrixRec &M, const TVectorRec &V) { return TMatrixRec::_op_Multiply(M, V); }
	friend TVectorRec operator *(const TVectorRec &V, const TMatrixRec &M) { return TMatrixRec::_op_Multiply(V, M); }
	friend TMatrixRec operator *(const TMatrixRec &M, System::Extended Scalar) { return TMatrixRec::_op_Multiply(M, Scalar); }
	friend TMatrixRec operator *(System::Extended Scalar, const TMatrixRec &M) { return TMatrixRec::_op_Multiply(Scalar, M); }
	friend TQuaternionRec operator *(const TMatrixRec &M, const TQuaternionRec &Q) { return TMatrixRec::_op_Multiply(M, Q); }
	TMatrixRec& operator =(TMatrixExt M) { *this = TMatrixRec::_op_Implicit(M); return *this; }
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TDim : public System::TCustomAttribute
{
	typedef System::TCustomAttribute inherited;
	
private:
	int FRowCount;
	int FColCount;
	
public:
	__fastcall TDim(int ARowCount, int AColCount)/* overload */;
	__property int RowCount = {read=FRowCount, nodefault};
	__property int ColCount = {read=FColCount, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TDim() { }
	
};

#pragma pack(pop)

typedef System::Extended TGLScalarValue;

typedef System::Extended __fastcall (*TGLScalarField)(System::Extended X, System::Extended Y, System::Extended Z);

typedef System::Extended __fastcall (__closure *TGLScalarFieldInt)(int iX, int iY, int iZ);

struct DECLSPEC_DRECORD TVertexRec
{
public:
	Gls::Vectortypes::TVector3f P;
	Gls::Vectortypes::TVector3f N;
	System::Extended Density;
};


struct DECLSPEC_DRECORD TFaceRec
{
public:
	Gls::Vectortypes::TVector3f Normal;
	Gls::Vectortypes::TVector3f V1;
	Gls::Vectortypes::TVector3f V2;
	Gls::Vectortypes::TVector3f V3;
	System::StaticArray<System::Byte, 2> Padding;
};


typedef TPoint2DRec *PPoint2DRec;

struct DECLSPEC_DRECORD TPoint2DRec
{
public:
	System::Extended X;
	System::Extended Y;
	TPoint2DRec __fastcall Create(System::Extended X, System::Extended Y);
	void __fastcall SetPosition(const System::Extended X, const System::Extended Y);
	TPoint2DRec __fastcall Add(const TPoint2DRec &APoint2D);
	System::Extended __fastcall Length();
	System::Extended __fastcall Distance(const TPoint2DRec &APoint2D);
	static bool __fastcall PointInCircle(const TPoint2DRec &Point, const TPoint2DRec &Center, const int Radius);
	void __fastcall Offset(const System::Extended ADeltaX, const System::Extended ADeltaY);
};


typedef TPoint3DRec *PPoint3DRec;

struct DECLSPEC_DRECORD TPoint3DRec
{
public:
	System::Extended X;
	System::Extended Y;
	System::Extended Z;
	TPoint3DRec __fastcall Create(System::Extended X, System::Extended Y, System::Extended Z);
	void __fastcall SetPosition(const System::Extended X, const System::Extended Y, const System::Extended Z);
	TPoint3DRec __fastcall Add(const TPoint3DRec &AGLPoint3D);
	float __fastcall Length();
	System::Extended __fastcall Distance(const TPoint3DRec &APoint3D);
	void __fastcall Offset(const System::Extended ADeltaX, const System::Extended ADeltaY, const System::Extended ADeltaZ);
};


typedef System::DynamicArray<TPoint2DRec> TPoint2DArray;

typedef System::DynamicArray<TPoint3DRec> TPoint3DArray;

enum DECLSPEC_DENUM TVoxelStatus : unsigned char { bpExternal, bpInternal };

typedef TVoxelRec *PVoxelRec;

struct DECLSPEC_DRECORD TVoxelRec
{
public:
	Gls::Vectortypes::TVector3f P;
	System::Extended Density;
	TVoxelStatus Status;
};


typedef System::StaticArray<TVoxelRec, 8388608> TVoxelData;

typedef TVoxelData *PVoxelData;

typedef System::StaticArray<System::Extended, 2> TVector2DType;

typedef System::StaticArray<System::Extended, 3> TVector3DType;

struct DECLSPEC_DRECORD TVector2DRec
{
public:
	TVector2DRec __fastcall Create(const float AX, const float AY, const float AW);
	TVector2DRec __fastcall Add(const TVector2DRec &AVector2D);
	System::Extended __fastcall Length();
	System::Extended __fastcall Norm();
	TVector2DRec __fastcall Normalize();
	TVector2DRec __fastcall CrossProduct(const TVector2DRec &AVector);
	System::Extended __fastcall DotProduct(const TVector2DRec &AVector);
	
public:
	union
	{
		struct 
		{
			System::Extended X;
			System::Extended Y;
			System::Extended W;
		};
		struct 
		{
			TVector2DType V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector3DRec
{
public:
	TVector3DRec __fastcall Create(const float AX, const float AY, const float AZ, const float AW);
	TVector3DRec __fastcall Add(const TVector3DRec &AVector3D);
	float __fastcall Length();
	float __fastcall Norm();
	TVector3DRec __fastcall Normalize();
	Gls::Vectortypes::TVector3d __fastcall CrossProduct(const Gls::Vectortypes::TVector3d &AVector3D);
	float __fastcall DotProduct(const Gls::Vectortypes::TVector3d &AVector3D);
	
public:
	union
	{
		struct 
		{
			System::Extended X;
			System::Extended Y;
			System::Extended Z;
			System::Extended W;
		};
		struct 
		{
			TVector3DType V;
		};
		
	};
};


typedef System::DynamicArray<TVector2DRec> TVector2DArray;

typedef System::DynamicArray<TVector3DRec> TVector3DArray;

typedef System::StaticArray<TVector2DRec, 4> TMatrix2DType;

typedef System::StaticArray<TVector3DRec, 4> TMatrix3DType;

struct DECLSPEC_DRECORD TMatrix2DRec
{
	
public:
	union
	{
		struct 
		{
			float e11;
			float e12;
			float e13;
			float e21;
			float e22;
			float e23;
			float e31;
			float e32;
			float e33;
		};
		struct 
		{
			TMatrix2DType M;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix3DRec
{
	
public:
	union
	{
		struct 
		{
			float e11;
			float e12;
			float e13;
			float e14;
			float e21;
			float e22;
			float e23;
			float e24;
			float e31;
			float e32;
			float e33;
			float e34;
			float e41;
			float e42;
			float e43;
			float e44;
		};
		struct 
		{
			TMatrix3DType M;
		};
		
	};
};


typedef System::DynamicArray<TMatrix2DRec> TMatrix2DArray;

typedef System::DynamicArray<TMatrix3DRec> TMatrix3DArray;

typedef TPoint2DArray TPolygon2D;

typedef TPoint3DArray TPolygon3D;

typedef System::StaticArray<TVertexRec, 8388608> TVertArray;

typedef TVertArray *PVertArray;

struct DECLSPEC_DRECORD TTriangleRec
{
public:
	int V1;
	int V2;
	int V3;
};


typedef System::StaticArray<TTriangleRec, 8388608> TTriangleRecArray;

typedef TTriangleRecArray *PTriangleRecArray;

typedef System::DynamicArray<TPoint3DArray> TPolyhedronArray;

struct DECLSPEC_DRECORD TMesh2DVert
{
public:
	float X;
	float Y;
	float NX;
	float NY;
	float tU;
	float tV;
};


#pragma pack(push,1)
struct DECLSPEC_DRECORD TMesh3DVert
{
public:
	float X;
	float Y;
	float Z;
	float NX;
	float NY;
	float NZ;
	float tU;
	float tV;
};
#pragma pack(pop)


typedef System::DynamicArray<TMesh2DVert> TMesh2DArray;

typedef System::DynamicArray<TMesh3DVert> TMesh3DArray;

struct DECLSPEC_DRECORD TQuat3DRec
{
public:
	TVector3DRec ImPart;
	float RePart;
};


typedef System::DynamicArray<TQuat3DRec> TQuatArray;

struct DECLSPEC_DRECORD TBoxRec
{
public:
	float ALeft;
	float ATop;
	float ANear;
	float ARight;
	float ABottom;
	float AFar;
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TPoint2DRec ClosedPolygon2D;
extern DELPHI_PACKAGE TPoint3DRec ClosedPolygon3D;
#define sWRONG_ELEMENT L"Wrong element"
#define sWRONG_SIZE L"Wrong size"
#define sNOT_QUAD L"Matrix not quadratic"
#define sSINGULAR L"Singular matrix founded"
extern DELPHI_PACKAGE TVectorRec __fastcall GetVectorRec(TVectorExt V);
extern DELPHI_PACKAGE TMatrixRec __fastcall GetMatrixRec(TMatrixExt M);
extern DELPHI_PACKAGE TQuaternionRec __fastcall GetQuaternionRec(TVectorExt Q);
extern DELPHI_PACKAGE void __fastcall Init(void * Obj, void * TypeInfoOfObj, int Offset = 0x0);
}	/* namespace Vectortypesext */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_VECTORTYPESEXT)
using namespace Gls::Vectortypesext;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_VectortypesextHPP
