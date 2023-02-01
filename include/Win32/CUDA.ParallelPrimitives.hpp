// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CUDA.ParallelPrimitives.pas' rev: 35.00 (Windows)

#ifndef Cuda_ParallelprimitivesHPP
#define Cuda_ParallelprimitivesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Cuda
{
namespace Parallelprimitives
{
//-- forward type declarations -----------------------------------------------
struct TCUDPPConfiguration;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TCUDPPResult : unsigned char { CUDPP_SUCCESS, CUDPP_ERROR_INVALID_HANDLE, CUDPP_ERROR_ILLEGAL_CONFIGURATION, CUDPP_ERROR_UNKNOWN };

enum DECLSPEC_DENUM TCUDPPOption : unsigned char { CUDPP_OPTION_FORWARD, CUDPP_OPTION_BACKWARD, CUDPP_OPTION_EXCLUSIVE, CUDPP_OPTION_INCLUSIVE, CUDPP_OPTION_CTA_LOCAL, CUDPP_OPTION_KEYS_ONLY, CUDPP_OPTION_KEY_VALUE_PAIRS };

enum DECLSPEC_DENUM TCUDPPDatatype : unsigned char { CUDPP_CHAR, CUDPP_UCHAR, CUDPP_INT, CUDPP_UINT, CUDPP_FLOAT };

enum DECLSPEC_DENUM TCUDPPOperator : unsigned char { CUDPP_ADD, CUDPP_MULTIPLY, CUDPP_MIN, CUDPP_MAX };

enum DECLSPEC_DENUM TCUDPPAlgorithm : unsigned char { CUDPP_SCAN, CUDPP_SEGMENTED_SCAN, CUDPP_COMPACT, CUDPP_REDUCE, CUDPP_SORT_RADIX, CUDPP_SPMVMULT, CUDPP_RAND_MD5, CUDPP_ALGORITHM_INVALID };

struct DECLSPEC_DRECORD TCUDPPConfiguration
{
public:
	TCUDPPAlgorithm algorithm;
	TCUDPPOperator op;
	TCUDPPDatatype datatype;
	TCUDPPOption options;
};


typedef NativeUInt TCUDPPHandle;

//-- var, const, procedure ---------------------------------------------------
#define CUDPPDLL L"cudpp32.dll"
static const unsigned CUDPP_INVALID_HANDLE = unsigned(0xc0dabad1);
extern "C" TCUDPPResult __stdcall cudppPlan(NativeUInt &planHandle, TCUDPPConfiguration config, NativeUInt n, NativeUInt rows, NativeUInt rowPitch);
extern "C" TCUDPPResult __stdcall cudppDestroyPlan(NativeUInt plan);
extern "C" TCUDPPResult __stdcall cudppScan(NativeUInt planHandle, void *d_out, NativeUInt &d_in, NativeUInt &numElements);
extern "C" TCUDPPResult __stdcall cudppMultiScan(NativeUInt planHandle, void *d_out, void *d_in, NativeUInt numElements, NativeUInt numRows);
extern "C" TCUDPPResult __stdcall cudppSegmentedScan(NativeUInt planHandle, void *d_out, void *d_idata, const System::PCardinal d_iflags, NativeUInt numElements);
extern "C" TCUDPPResult __stdcall cudppCompact(NativeUInt planHandle, void *d_out, NativeUInt &d_numValidElements, void *d_in, const System::PCardinal d_isValid, NativeUInt numElements);
extern "C" TCUDPPResult __stdcall cudppSort(NativeUInt planHandle, void *d_keys, void *d_values, int keybits, NativeUInt numElements);
extern "C" TCUDPPResult __stdcall cudppSparseMatrix(NativeUInt &sparseMatrixHandle, TCUDPPConfiguration config, NativeUInt n, NativeUInt rows, void *A, const System::PCardinal h_rowIndices, const System::PCardinal h_indices);
extern "C" TCUDPPResult __stdcall cudppDestroySparseMatrix(NativeUInt sparseMatrixHandle);
extern "C" TCUDPPResult __stdcall cudppSparseMatrixVectorMultiply(NativeUInt sparseMatrixHandle, void *d_y, void *d_x);
extern "C" TCUDPPResult __stdcall cudppRand(NativeUInt planHandle, void *d_out, NativeUInt numElements);
extern "C" TCUDPPResult __stdcall cudppRandSeed(const NativeUInt planHandle, unsigned seed);
}	/* namespace Parallelprimitives */
}	/* namespace Cuda */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA_PARALLELPRIMITIVES)
using namespace Cuda::Parallelprimitives;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA)
using namespace Cuda;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Cuda_ParallelprimitivesHPP
