// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.VectorTypes.pas' rev: 35.00 (Windows)

#ifndef Gls_VectortypesHPP
#define Gls_VectortypesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Vectortypes
{
//-- forward type declarations -----------------------------------------------
struct _WGLSWAP;
struct DECLSPEC_DRECORD T_cl_context
{
};


struct DECLSPEC_DRECORD T_cl_event
{
};


struct TVector2d;
struct TVector2f;
struct TVector2h;
struct TVector2i;
struct TVector2ui;
struct TVector2s;
struct TVector2b;
struct TVector2sb;
struct TVector2e;
struct TVector2w;
struct TVector2p;
struct TVector3d;
struct TVector3f;
struct TVector3h;
struct TVector3i;
struct TVector3ui;
struct TVector3s;
struct TVector3b;
struct TVector3sb;
struct TVector3e;
struct TVector3w;
struct TVector3p;
struct TVector4d;
struct TVector4f;
struct TVector4h;
struct TVector4i;
struct TVector4ui;
struct TVector4s;
struct TVector4b;
struct TVector4sb;
struct TVector4e;
struct TVector4w;
struct TVector4p;
struct TMatrix2d;
struct TMatrix2f;
struct TMatrix2i;
struct TMatrix2s;
struct TMatrix2b;
struct TMatrix2e;
struct TMatrix2w;
struct TMatrix2p;
struct TMatrix3d;
struct TMatrix3f;
struct TMatrix3i;
struct TMatrix3s;
struct TMatrix3b;
struct TMatrix3e;
struct TMatrix3w;
struct TMatrix3p;
struct TMatrix4d;
struct TMatrix4f;
struct TMatrix4i;
struct TMatrix4s;
struct TMatrix4b;
struct TMatrix4e;
struct TMatrix4w;
struct TMatrix4p;
struct TD3DVector;
struct TD3DMatrix;
//-- type declarations -------------------------------------------------------
typedef System::ByteBool TGLboolean;

typedef System::ByteBool *PGLboolean;

typedef unsigned TGLbitfield;

typedef unsigned *PGLbitfield;

typedef System::Int8 TGLbyte;

typedef System::Int8 *PGLbyte;

typedef short TGLshort;

typedef short *PGLshort;

typedef int TGLint;

typedef System::PInteger PGLint;

typedef int TGLsizei;

typedef System::PInteger PGLsizei;

typedef __int64 TGLint64;

typedef System::PInt64 PGLint64;

typedef __int64 TGLint64EXT;

typedef System::PInt64 PGLint64EXT;

typedef unsigned __int64 TGLuint64;

typedef System::PUInt64 PGLuint64;

typedef unsigned __int64 TGLuint64EXT;

typedef System::PUInt64 PGLuint64EXT;

typedef System::Byte TGLubyte;

typedef System::PByte PGLubyte;

typedef System::Word TGLushort;

typedef System::PWord PGLushort;

typedef unsigned TGLenum;

typedef unsigned *PGLenum;

typedef unsigned TGLuint;

typedef System::PCardinal PGLuint;

typedef float TGLfloat;

typedef double TGLdouble;

typedef System::PDouble PGLdouble;

typedef System::PSingle PGLclampf;

typedef double TGLclampd;

typedef System::PDouble PGLclampd;

typedef char * *PGLPCharArray;

typedef void * PGLvoid;

typedef void * *PGLPointer;

typedef unsigned TGLhandleARB;

typedef unsigned *PGLhandleARB;

typedef NativeInt TGLintptr;

typedef NativeInt *PGLintptr;

typedef NativeInt TGLsizeiptr;

typedef NativeInt *PGLsizeiptr;

typedef NativeInt TGLsync;

typedef NativeInt *PGLsync;

typedef System::Byte TGLchar;

typedef char * PGLchar;

typedef System::Word TGLhalf;

typedef System::Word *PGLhalf;

typedef _WGLSWAP *PWGLswap;

#pragma pack(push,1)
struct DECLSPEC_DRECORD _WGLSWAP
{
public:
	HDC hdc;
	unsigned uiFlags;
};
#pragma pack(pop)


typedef _WGLSWAP TWGLswap;

typedef _WGLSWAP WGLSWAP;

typedef NativeUInt HPBUFFERARB;

typedef T_cl_context *P_cl_context;

typedef T_cl_event *P_cl_event;

struct DECLSPEC_DRECORD TVector2d
{
	
public:
	union
	{
		struct 
		{
			double X;
			double Y;
		};
		struct 
		{
			System::StaticArray<double, 2> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector2f
{
	
public:
	union
	{
		struct 
		{
			float X;
			float Y;
		};
		struct 
		{
			System::StaticArray<float, 2> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector2h
{
	
public:
	union
	{
		struct 
		{
			System::Word X;
			System::Word Y;
		};
		struct 
		{
			System::StaticArray<System::Word, 2> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector2i
{
	
public:
	union
	{
		struct 
		{
			int X;
			int Y;
		};
		struct 
		{
			System::StaticArray<int, 2> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector2ui
{
	
public:
	union
	{
		struct 
		{
			unsigned X;
			unsigned Y;
		};
		struct 
		{
			System::StaticArray<unsigned, 2> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector2s
{
	
public:
	union
	{
		struct 
		{
			short X;
			short Y;
		};
		struct 
		{
			System::StaticArray<short, 2> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector2b
{
	
public:
	union
	{
		struct 
		{
			System::Byte X;
			System::Byte Y;
		};
		struct 
		{
			System::StaticArray<System::Byte, 2> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector2sb
{
	
public:
	union
	{
		struct 
		{
			System::Int8 X;
			System::Int8 Y;
		};
		struct 
		{
			System::StaticArray<System::Int8, 2> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector2e
{
	
public:
	union
	{
		struct 
		{
			System::Extended X;
			System::Extended Y;
		};
		struct 
		{
			System::StaticArray<System::Extended, 2> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector2w
{
	
public:
	union
	{
		struct 
		{
			System::Word X;
			System::Word Y;
		};
		struct 
		{
			System::StaticArray<System::Word, 2> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector2p
{
	
public:
	union
	{
		struct 
		{
			void *X;
			void *Y;
		};
		struct 
		{
			System::StaticArray<void *, 2> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector3d
{
	
public:
	union
	{
		struct 
		{
			double X;
			double Y;
			double Z;
		};
		struct 
		{
			System::StaticArray<double, 3> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector3f
{
	
public:
	union
	{
		struct 
		{
			float X;
			float Y;
			float Z;
		};
		struct 
		{
			System::StaticArray<float, 3> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector3h
{
	
public:
	union
	{
		struct 
		{
			System::Word X;
			System::Word Y;
			System::Word Z;
		};
		struct 
		{
			System::StaticArray<System::Word, 3> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector3i
{
	
public:
	union
	{
		struct 
		{
			int X;
			int Y;
			int Z;
		};
		struct 
		{
			System::StaticArray<int, 3> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector3ui
{
	
public:
	union
	{
		struct 
		{
			unsigned X;
			unsigned Y;
			unsigned Z;
		};
		struct 
		{
			System::StaticArray<unsigned, 3> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector3s
{
	
public:
	union
	{
		struct 
		{
			short X;
			short Y;
			short Z;
		};
		struct 
		{
			System::StaticArray<short, 3> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector3b
{
	
public:
	union
	{
		struct 
		{
			System::Byte X;
			System::Byte Y;
			System::Byte Z;
		};
		struct 
		{
			System::StaticArray<System::Byte, 3> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector3sb
{
	
public:
	union
	{
		struct 
		{
			System::Int8 X;
			System::Int8 Y;
			System::Int8 Z;
		};
		struct 
		{
			System::StaticArray<System::Int8, 3> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector3e
{
	
public:
	union
	{
		struct 
		{
			System::Extended X;
			System::Extended Y;
			System::Extended Z;
		};
		struct 
		{
			System::StaticArray<System::Extended, 3> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector3w
{
	
public:
	union
	{
		struct 
		{
			System::Word X;
			System::Word Y;
			System::Word Z;
		};
		struct 
		{
			System::StaticArray<System::Word, 3> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector3p
{
	
public:
	union
	{
		struct 
		{
			void *X;
			void *Y;
			void *Z;
		};
		struct 
		{
			System::StaticArray<void *, 3> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector4d
{
	
public:
	union
	{
		struct 
		{
			double X;
			double Y;
			double Z;
			double W;
		};
		struct 
		{
			System::StaticArray<double, 4> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector4f
{
	
public:
	union
	{
		struct 
		{
			float X;
			float Y;
			float Z;
			float W;
		};
		struct 
		{
			System::StaticArray<float, 4> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector4h
{
	
public:
	union
	{
		struct 
		{
			System::Word X;
			System::Word Y;
			System::Word Z;
			System::Word W;
		};
		struct 
		{
			System::StaticArray<System::Word, 4> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector4i
{
	
public:
	union
	{
		struct 
		{
			int X;
			int Y;
			int Z;
			int W;
		};
		struct 
		{
			System::StaticArray<int, 4> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector4ui
{
	
public:
	union
	{
		struct 
		{
			unsigned X;
			unsigned Y;
			unsigned Z;
			unsigned W;
		};
		struct 
		{
			System::StaticArray<unsigned, 4> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector4s
{
	
public:
	union
	{
		struct 
		{
			short X;
			short Y;
			short Z;
			short W;
		};
		struct 
		{
			System::StaticArray<short, 4> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector4b
{
	
public:
	union
	{
		struct 
		{
			System::Byte X;
			System::Byte Y;
			System::Byte Z;
			System::Byte W;
		};
		struct 
		{
			System::StaticArray<System::Byte, 4> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector4sb
{
	
public:
	union
	{
		struct 
		{
			System::Int8 X;
			System::Int8 Y;
			System::Int8 Z;
			System::Int8 W;
		};
		struct 
		{
			System::StaticArray<System::Int8, 4> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector4e
{
	
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
			System::StaticArray<System::Extended, 4> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector4w
{
	
public:
	union
	{
		struct 
		{
			System::Word X;
			System::Word Y;
			System::Word Z;
			System::Word W;
		};
		struct 
		{
			System::StaticArray<System::Word, 4> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TVector4p
{
	
public:
	union
	{
		struct 
		{
			void *X;
			void *Y;
			void *Z;
			void *W;
		};
		struct 
		{
			System::StaticArray<void *, 4> V;
		};
		
	};
};


typedef TVector4f *PGLVector;

typedef TVector4f TGLVector;

struct DECLSPEC_DRECORD TMatrix2d
{
	
public:
	union
	{
		struct 
		{
			TVector2d X;
			TVector2d Y;
		};
		struct 
		{
			System::StaticArray<TVector2d, 2> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix2f
{
	
public:
	union
	{
		struct 
		{
			TVector2f X;
			TVector2f Y;
		};
		struct 
		{
			System::StaticArray<TVector2f, 2> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix2i
{
	
public:
	union
	{
		struct 
		{
			TVector2i X;
			TVector2i Y;
		};
		struct 
		{
			System::StaticArray<TVector2i, 2> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix2s
{
	
public:
	union
	{
		struct 
		{
			TVector2s X;
			TVector2s Y;
		};
		struct 
		{
			System::StaticArray<TVector2s, 2> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix2b
{
	
public:
	union
	{
		struct 
		{
			TVector2b X;
			TVector2b Y;
		};
		struct 
		{
			System::StaticArray<TVector2b, 2> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix2e
{
	
public:
	union
	{
		struct 
		{
			TVector2e X;
			TVector2e Y;
		};
		struct 
		{
			System::StaticArray<TVector2e, 2> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix2w
{
	
public:
	union
	{
		struct 
		{
			TVector2w X;
			TVector2w Y;
		};
		struct 
		{
			System::StaticArray<TVector2w, 2> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix2p
{
	
public:
	union
	{
		struct 
		{
			TVector2p X;
			TVector2p Y;
		};
		struct 
		{
			System::StaticArray<TVector2p, 2> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix3d
{
	
public:
	union
	{
		struct 
		{
			TVector3d X;
			TVector3d Y;
			TVector3d Z;
		};
		struct 
		{
			System::StaticArray<TVector3d, 3> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix3f
{
	
public:
	union
	{
		struct 
		{
			TVector3f X;
			TVector3f Y;
			TVector3f Z;
		};
		struct 
		{
			System::StaticArray<TVector3f, 3> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix3i
{
	
public:
	union
	{
		struct 
		{
			TVector3i X;
			TVector3i Y;
			TVector3i Z;
		};
		struct 
		{
			System::StaticArray<TVector3i, 3> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix3s
{
	
public:
	union
	{
		struct 
		{
			TVector3s X;
			TVector3s Y;
			TVector3s Z;
		};
		struct 
		{
			System::StaticArray<TVector3s, 3> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix3b
{
	
public:
	union
	{
		struct 
		{
			TVector3b X;
			TVector3b Y;
			TVector3b Z;
		};
		struct 
		{
			System::StaticArray<TVector3b, 3> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix3e
{
	
public:
	union
	{
		struct 
		{
			TVector3e X;
			TVector3e Y;
			TVector3e Z;
		};
		struct 
		{
			System::StaticArray<TVector3e, 3> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix3w
{
	
public:
	union
	{
		struct 
		{
			TVector3w X;
			TVector3w Y;
			TVector3w Z;
		};
		struct 
		{
			System::StaticArray<TVector3w, 3> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix3p
{
	
public:
	union
	{
		struct 
		{
			TVector3p X;
			TVector3p Y;
			TVector3p Z;
		};
		struct 
		{
			System::StaticArray<TVector3p, 3> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix4d
{
	
public:
	union
	{
		struct 
		{
			TVector4d X;
			TVector4d Y;
			TVector4d Z;
			TVector4d W;
		};
		struct 
		{
			System::StaticArray<TVector4d, 4> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix4f
{
	
public:
	union
	{
		struct 
		{
			TVector4f X;
			TVector4f Y;
			TVector4f Z;
			TVector4f W;
		};
		struct 
		{
			System::StaticArray<TVector4f, 4> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix4i
{
	
public:
	union
	{
		struct 
		{
			TVector4i X;
			TVector4i Y;
			TVector4i Z;
			TVector4i W;
		};
		struct 
		{
			System::StaticArray<TVector4i, 4> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix4s
{
	
public:
	union
	{
		struct 
		{
			TVector4s X;
			TVector4s Y;
			TVector4s Z;
			TVector4s W;
		};
		struct 
		{
			System::StaticArray<TVector4s, 4> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix4b
{
	
public:
	union
	{
		struct 
		{
			TVector4b X;
			TVector4b Y;
			TVector4b Z;
			TVector4b W;
		};
		struct 
		{
			System::StaticArray<TVector4b, 4> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix4e
{
	
public:
	union
	{
		struct 
		{
			TVector4e X;
			TVector4e Y;
			TVector4e Z;
			TVector4e W;
		};
		struct 
		{
			System::StaticArray<TVector4e, 4> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix4w
{
	
public:
	union
	{
		struct 
		{
			TVector4w X;
			TVector4w Y;
			TVector4w Z;
			TVector4w W;
		};
		struct 
		{
			System::StaticArray<TVector4w, 4> V;
		};
		
	};
};


struct DECLSPEC_DRECORD TMatrix4p
{
	
public:
	union
	{
		struct 
		{
			TVector4p X;
			TVector4p Y;
			TVector4p Z;
			TVector4p W;
		};
		struct 
		{
			System::StaticArray<TVector4p, 4> V;
		};
		
	};
};


#pragma pack(push,1)
struct DECLSPEC_DRECORD TD3DVector
{
	
public:
	union
	{
		struct 
		{
			TVector3f V;
		};
		struct 
		{
			float X;
			float Y;
			float Z;
		};
		
	};
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TD3DMatrix
{
	
public:
	union
	{
		struct 
		{
			TMatrix4f M;
		};
		struct 
		{
			float _11;
			float _12;
			float _13;
			float _14;
			float _21;
			float _22;
			float _23;
			float _24;
			float _31;
			float _32;
			float _33;
			float _34;
			float _41;
			float _42;
			float _43;
			float _44;
		};
		
	};
};
#pragma pack(pop)


typedef TMatrix4f *PGLMatrix;

typedef TMatrix4f TGLMatrix;

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Vectortypes */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_VECTORTYPES)
using namespace Gls::Vectortypes;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_VectortypesHPP
