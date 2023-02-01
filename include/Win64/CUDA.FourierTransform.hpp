// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CUDA.FourierTransform.pas' rev: 35.00 (Windows)

#ifndef Cuda_FouriertransformHPP
#define Cuda_FouriertransformHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Strings.hpp>
#include <CUDA.Import.hpp>
#include <CUDA.Runtime.hpp>

//-- user supplied -----------------------------------------------------------

namespace Cuda
{
namespace Fouriertransform
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<System::UnicodeString, 8> Cuda_Fouriertransform__1;

typedef unsigned TcufftHandle;

typedef float TcufftReal;

typedef float *PcufftReal;

typedef float TcufftRealfloat;

typedef double *PcufftDoubleReal;

typedef double TcufftDoubleReal;

typedef Gls::Vectortypes::TVector2d *PcufftDoubleComplex;

typedef Gls::Vectortypes::TVector2d TcufftDoubleComplex;

typedef Gls::Vectortypes::TVector2f *PcufftComplex;

typedef Gls::Vectortypes::TVector2f TcufftComplex;

typedef System::Byte TcufftResult;

typedef unsigned TcufftType;

enum DECLSPEC_DENUM TcudaRoundMode : unsigned char { cudaRoundNearest, cudaRoundZero, cudaRoundPosInf, cudaRoundMinInf };

typedef unsigned TcufftCompatibility;

typedef TcufftResult __stdcall (*TcufftPlan1d)(/* out */ TcufftHandle &plan, int nx, TcufftType atype, int batch);

typedef TcufftResult __stdcall (*TcufftPlan2d)(/* out */ TcufftHandle &plan, int nx, int ny, TcufftType atype);

typedef TcufftResult __stdcall (*TcufftPlan3d)(/* out */ TcufftHandle &plan, int nx, int ny, int nz, TcufftType atype);

typedef TcufftResult __stdcall (*TcufftDestroy)(TcufftHandle plan);

typedef TcufftResult __stdcall (*TcufftPlanMany)(/* out */ TcufftHandle &plan, int rank, int &n, int &inembed, int istride, int idist, int &onembed, int ostride, int odist, TcufftType ctype, int batch);

typedef TcufftResult __stdcall (*TcufftExecC2C)(TcufftHandle plan, PcufftComplex idata, PcufftComplex odata, int direction);

typedef TcufftResult __stdcall (*TcufftExecR2C)(TcufftHandle plan, PcufftReal idata, PcufftComplex odata);

typedef TcufftResult __stdcall (*TcufftExecC2R)(TcufftHandle plan, PcufftComplex idata, PcufftReal odata);

typedef TcufftResult __stdcall (*TcufftExecZ2Z)(TcufftHandle plan, PcufftDoubleComplex idata, PcufftDoubleComplex odata, int direction);

typedef TcufftResult __stdcall (*TcufftExecD2Z)(TcufftHandle plan, PcufftDoubleReal idata, PcufftDoubleComplex odata);

typedef TcufftResult __stdcall (*TcufftExecZ2D)(TcufftHandle plan, PcufftDoubleComplex idata, PcufftDoubleReal odata);

typedef TcufftResult __stdcall (*TcufftSetStream)(TcufftHandle p, int stream);

typedef TcufftResult __stdcall (*TcufftSetCompatibilityMode)(TcufftHandle plan, TcufftCompatibility mode);

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Cuda_Fouriertransform__1 CUFFTDLLNAMES;
static const unsigned INVALID_CUFFT_HANDLE = unsigned(0xffffffff);
extern DELPHI_PACKAGE TcufftResult CUFFT_SUCCESS;
extern DELPHI_PACKAGE TcufftResult CUFFT_INVALID_PLAN;
extern DELPHI_PACKAGE TcufftResult CUFFT_ALLOC_FAILED;
extern DELPHI_PACKAGE TcufftResult CUFFT_INVALID_TYPE;
extern DELPHI_PACKAGE TcufftResult CUFFT_INVALID_VALUE;
extern DELPHI_PACKAGE TcufftResult CUFFT_INTERNAL_ERROR;
extern DELPHI_PACKAGE TcufftResult CUFFT_EXEC_FAILED;
extern DELPHI_PACKAGE TcufftResult CUFFT_SETUP_FAILED;
extern DELPHI_PACKAGE TcufftResult CUFFT_INVALID_SIZE;
static const System::Int8 CUFFT_FORWARD = System::Int8(-1);
static const System::Int8 CUFFT_INVERSE = System::Int8(0x1);
extern DELPHI_PACKAGE TcufftType CUFFT_R2C;
extern DELPHI_PACKAGE TcufftType CUFFT_C2R;
extern DELPHI_PACKAGE TcufftType CUFFT_C2C;
extern DELPHI_PACKAGE TcufftType CUFFT_D2Z;
extern DELPHI_PACKAGE TcufftType CUFFT_Z2D;
extern DELPHI_PACKAGE TcufftType CUFFT_Z2Z;
extern DELPHI_PACKAGE TcufftCompatibility CUFFT_COMPATIBILITY_NORMAL;
extern DELPHI_PACKAGE TcufftCompatibility CUFFT_COMPATIBILITY_FFTW_PADDING;
extern DELPHI_PACKAGE TcufftCompatibility CUFFT_COMPATIBILITY_FFTW_C2R_ASYMMETRIC;
extern DELPHI_PACKAGE TcufftCompatibility CUFFT_COMPATIBILITY_FFTW;
extern DELPHI_PACKAGE TcufftPlan1d cufftPlan1d;
extern DELPHI_PACKAGE TcufftPlan2d cufftPlan2d;
extern DELPHI_PACKAGE TcufftPlan3d cufftPlan3d;
extern DELPHI_PACKAGE TcufftDestroy cufftDestroy;
extern DELPHI_PACKAGE TcufftPlanMany cufftPlanMany;
extern DELPHI_PACKAGE TcufftExecC2C cufftExecC2C;
extern DELPHI_PACKAGE TcufftExecR2C cufftExecR2C;
extern DELPHI_PACKAGE TcufftExecC2R cufftExecC2R;
extern DELPHI_PACKAGE TcufftExecZ2Z cufftExecZ2Z;
extern DELPHI_PACKAGE TcufftExecD2Z cufftExecD2Z;
extern DELPHI_PACKAGE TcufftExecZ2D cufftExecZ2D;
extern DELPHI_PACKAGE TcufftSetStream cufftSetStream;
extern DELPHI_PACKAGE TcufftSetCompatibilityMode cufftSetCompatibilityMode;
extern DELPHI_PACKAGE bool __fastcall InitCUFFT(void);
extern DELPHI_PACKAGE void __fastcall CloseCUFFT(void);
extern DELPHI_PACKAGE bool __fastcall InitCUFFTFromLibrary(const System::WideString LibName);
extern DELPHI_PACKAGE bool __fastcall IsCUFFTInitialized(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall Get_CUDA_FFT_Error_String(TcufftResult AError);
}	/* namespace Fouriertransform */
}	/* namespace Cuda */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA_FOURIERTRANSFORM)
using namespace Cuda::Fouriertransform;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA)
using namespace Cuda;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Cuda_FouriertransformHPP
