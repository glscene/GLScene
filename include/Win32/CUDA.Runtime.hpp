// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CUDA.Runtime.pas' rev: 35.00 (Windows)

#ifndef Cuda_RuntimeHPP
#define Cuda_RuntimeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <CUDA.Import.hpp>
#include <GLS.Logger.hpp>

//-- user supplied -----------------------------------------------------------

namespace Cuda
{
namespace Runtime
{
//-- forward type declarations -----------------------------------------------
struct TCudaChannelFormatDesc;
struct DECLSPEC_DRECORD TcudaArray
{
};


struct TcudaPitchedPtr;
struct TcudaExtent;
struct TcudaPos;
struct TcudaMemcpy3DParms;
struct TCudaDeviceProp;
struct TTextureReference;
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<System::UnicodeString, 10> Cuda_Runtime__1;

enum DECLSPEC_DENUM TcudaError : unsigned char { cudaSuccess, cudaErrorMissingConfiguration, cudaErrorMemoryAllocation, cudaErrorInitializationError, cudaErrorLaunchFailure, cudaErrorPriorLaunchFailure, cudaErrorLaunchTimeout, cudaErrorLaunchOutOfResources, cudaErrorInvalidDeviceFunction, cudaErrorInvalidConfiguration, cudaErrorInvalidDevice, cudaErrorInvalidValue, cudaErrorInvalidPitchValue, cudaErrorInvalidSymbol, cudaErrorMapBufferObjectFailed, cudaErrorUnmapBufferObjectFailed, cudaErrorInvalidHostPointer, cudaErrorInvalidDevicePointer, cudaErrorInvalidTexture, cudaErrorInvalidTextureBinding, cudaErrorInvalidChannelDescriptor, cudaErrorInvalidMemcpyDirection, cudaErrorAddressOfConstant, cudaErrorTextureFetchFailed, cudaErrorTextureNotBound, 
	cudaErrorSynchronizationError, cudaErrorInvalidFilterSetting, cudaErrorInvalidNormSetting, cudaErrorMixedDeviceExecution, cudaErrorCudartUnloading, cudaErrorUnknown, cudaErrorNotYetImplemented, cudaErrorMemoryValueTooLarge, cudaErrorInvalidResourceHandle, cudaErrorNotReady, cudaErrorStartupFailure, cudaErrorApiFailureBase };

enum DECLSPEC_DENUM TCudaChannelFormatKind : unsigned char { cudaChannelFormatKindSigned, cudaChannelFormatKindUnsigned, cudaChannelFormatKindFloat };

enum DECLSPEC_DENUM TCudaGLMapFlags : unsigned char { cudaGLMapFlagsNone, cudaGLMapFlagsReadOnly, cudaGLMapFlagsWriteDiscard };

typedef TCudaChannelFormatDesc *PcudaChannelFormatDesc;

struct DECLSPEC_DRECORD TCudaChannelFormatDesc
{
public:
	int x;
	int y;
	int z;
	int w;
	TCudaChannelFormatKind f;
};


enum DECLSPEC_DENUM TcudaMemcpyKind : unsigned char { cudaMemcpyHostToHost, cudaMemcpyHostToDevice, cudaMemcpyDeviceToHost, cudaMemcpyDeviceToDevice };

struct DECLSPEC_DRECORD TcudaPitchedPtr
{
public:
	void *ptr;
	NativeUInt pitch;
	NativeUInt xsize;
	NativeUInt ysize;
};


struct DECLSPEC_DRECORD TcudaExtent
{
public:
	NativeUInt width;
	NativeUInt height;
	NativeUInt depth;
};


struct DECLSPEC_DRECORD TcudaPos
{
public:
	NativeUInt x;
	NativeUInt y;
	NativeUInt z;
};


struct DECLSPEC_DRECORD TcudaMemcpy3DParms
{
public:
	void *srcArray;
	TcudaPos srcPos;
	TcudaPitchedPtr srcPtr;
	void *dstArray;
	TcudaPos dstPos;
	TcudaPitchedPtr dstPtr;
	TcudaExtent extent;
	TcudaMemcpyKind kind;
};


typedef TCudaDeviceProp *PCudaDeviceProp;

struct DECLSPEC_DRECORD TCudaDeviceProp
{
public:
	System::StaticArray<char, 256> name;
	NativeUInt totalGlobalMem;
	NativeUInt sharedMemPerBlock;
	int regsPerBlock;
	int warpSize;
	NativeUInt memPitch;
	int maxThreadsPerBlock;
	System::StaticArray<int, 3> maxThreadsDim;
	System::StaticArray<int, 3> maxGridSize;
	int clockRate;
	NativeUInt totalConstMem;
	int major;
	int minor;
	NativeUInt textureAlignment;
	int deviceOverlap;
	int multiProcessorCount;
	int kernelExecTimeoutEnabled;
	int egrated;
	int canMapHostMemory;
	int computeMode;
	int maxTexture1D;
	System::StaticArray<int, 2> maxTexture2D;
	System::StaticArray<int, 3> maxTexture3D;
	System::StaticArray<int, 3> maxTexture2DArray;
	NativeUInt surfaceAlignment;
	int concurrentKernels;
	int ECCEnabled;
	int pciBusID;
	int pciDeviceID;
	int tccDriver;
	System::StaticArray<int, 21> __cudaReserved;
};


enum DECLSPEC_DENUM TcudaTextureAddressMode : unsigned char { cudaAddressModeWrap, cudaAddressModeClamp, cudaAddressModeMirror };

enum DECLSPEC_DENUM TcudaTextureFilterMode : unsigned char { cudaFilterModePoint, cudaFilterModeLinear };

enum DECLSPEC_DENUM TcudaTextureReadMode : unsigned char { cudaReadModeElementType, cudaReadModeNormalizedFloat };

typedef TTextureReference *PTextureReference;

struct DECLSPEC_DRECORD TTextureReference
{
public:
	int normalized;
	TcudaTextureFilterMode filterMode;
	System::StaticArray<TcudaTextureAddressMode, 3> addressMode;
	TCudaChannelFormatDesc channelDesc;
	System::StaticArray<int, 16> __cudaReserved;
};


typedef TcudaArray *PcudaArray;

typedef TcudaError cudaError_t;

typedef int cudaStream_t;

typedef int cudaEvent_t;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Cuda_Runtime__1 CUDARTDLLNAMES;
extern DELPHI_PACKAGE float CUDART_INF_F;
extern DELPHI_PACKAGE float CUDART_NAN_F;
extern DELPHI_PACKAGE float CUDART_MIN_DENORM_F;
extern DELPHI_PACKAGE float CUDART_MAX_NORMAL_F;
extern DELPHI_PACKAGE float CUDART_NEG_ZERO_F;
#define CUDART_ZERO_F  (0.000000E+00)
#define CUDART_ONE_F  (1.000000E+00)
static const System::Extended CUDART_SQRT_HALF_F = 7.071068E-01;
static const System::Extended CUDART_SQRT_TWO_F = 1.414214E+00;
static const System::Extended CUDART_THIRD_F = 3.333333E-01;
static const System::Extended CUDART_PIO4_F = 7.853982E-01;
static const System::Extended CUDART_PIO2_F = 1.570796E+00;
static const System::Extended CUDART_3PIO4_F = 2.356194E+00;
static const System::Extended CUDART_2_OVER_PI_F = 6.366198E-01;
static const System::Extended CUDART_PI_F = 3.141593E+00;
static const System::Extended CUDART_L2E_F = 1.442695E+00;
static const System::Extended CUDART_L2T_F = 3.321928E+00;
static const System::Extended CUDART_LG2_F = 3.010300E-01;
static const System::Extended CUDART_LGE_F = 4.342945E-01;
static const System::Extended CUDART_LN2_F = 6.931472E-01;
static const System::Extended CUDART_LNT_F = 2.302585E+00;
static const System::Extended CUDART_LNPI_F = 1.144730E+00;
static const System::Extended CUDART_TWO_TO_M126_F = 1.175494E-38;
static const System::Extended CUDART_TWO_TO_126_F = 8.507059E+37;
static const System::Extended CUDART_NORM_HUGE_F = 3.402823E+38;
#define CUDART_TWO_TO_23_F  (8.388608E+06)
#define CUDART_TWO_TO_24_F  (1.677722E+07)
#define CUDART_TWO_TO_31_F  (2.147484E+09)
#define CUDART_TWO_TO_32_F  (4.294967E+09)
static const System::Int8 CUDART_REMQUO_BITS_F = System::Int8(0x3);
static const System::Int8 CUDART_REMQUO_MASK_F = System::Int8(0x3);
#define CUDART_TRIG_PLOSS_F  (4.803900E+04)
extern DELPHI_PACKAGE double CUDART_INF;
extern DELPHI_PACKAGE double CUDART_NAN;
extern DELPHI_PACKAGE double CUDART_NEG_ZERO;
extern DELPHI_PACKAGE double CUDART_MIN_DENORM;
#define CUDART_ZERO  (0.000000E+00)
#define CUDART_ONE  (1.000000E+00)
static const System::Extended CUDART_SQRT_TWO = 1.414214E+00;
static const System::Extended CUDART_SQRT_HALF = 7.071068E-01;
static const System::Extended CUDART_THIRD = 3.333333E-01;
static const System::Extended CUDART_TWOTHIRD = 6.666667E-01;
static const System::Extended CUDART_PIO4 = 7.853982E-01;
static const System::Extended CUDART_PIO4_HI = 7.853982E-01;
static const System::Extended CUDART_PIO4_LO = 3.061617E-17;
static const System::Extended CUDART_PIO2 = 1.570796E+00;
static const System::Extended CUDART_PIO2_HI = 1.570796E+00;
static const System::Extended CUDART_PIO2_LO = 6.123234E-17;
static const System::Extended CUDART_3PIO4 = 2.356194E+00;
static const System::Extended CUDART_2_OVER_PI = 6.366198E-01;
static const System::Extended CUDART_PI = 3.141593E+00;
static const System::Extended CUDART_PI_HI = 3.141593E+00;
static const System::Extended CUDART_PI_LO = 1.224647E-16;
static const System::Extended CUDART_SQRT_2PI_HI = 2.506628E+00;
static const System::Extended CUDART_SQRT_2PI_LO = -1.832858E-16;
static const System::Extended CUDART_SQRT_PIO2_HI = 1.253314E+00;
static const System::Extended CUDART_SQRT_PIO2_LO = -9.164290E-17;
static const System::Extended CUDART_L2E = 1.442695E+00;
static const System::Extended CUDART_L2E_HI = 1.442695E+00;
static const System::Extended CUDART_L2E_LO = 2.035527E-17;
static const System::Extended CUDART_L2T = 3.321928E+00;
static const System::Extended CUDART_LG2 = 3.010300E-01;
static const System::Extended CUDART_LG2_HI = 3.010300E-01;
static const System::Extended CUDART_LG2_LO = -2.803728E-18;
static const System::Extended CUDART_LGE = 4.342945E-01;
static const System::Extended CUDART_LGE_HI = 4.342945E-01;
static const System::Extended CUDART_LGE_LO = 1.098320E-17;
static const System::Extended CUDART_LN2 = 6.931472E-01;
static const System::Extended CUDART_LN2_HI = 6.931472E-01;
static const System::Extended CUDART_LN2_LO = 2.319047E-17;
static const System::Extended CUDART_LNT = 2.302585E+00;
static const System::Extended CUDART_LNT_HI = 2.302585E+00;
static const System::Extended CUDART_LNT_LO = -2.170756E-16;
static const System::Extended CUDART_LNPI = 1.144730E+00;
static const System::Extended CUDART_LN2_X_1024 = 7.097827E+02;
static const System::Extended CUDART_LN2_X_1025 = 7.104759E+02;
static const System::Extended CUDART_LN2_X_1075 = 7.451332E+02;
static const System::Extended CUDART_LG2_X_1024 = 3.082547E+02;
static const System::Extended CUDART_LG2_X_1075 = 3.236072E+02;
#define CUDART_TWO_TO_23  (8.388608E+06)
static const System::Extended CUDART_TWO_TO_52 = 4.503600E+15;
static const System::Extended CUDART_TWO_TO_54 = 1.801440E+16;
static const System::Extended CUDART_TWO_TO_M54 = 5.551115E-17;
static const System::Extended CUDART_TWO_TO_M1022 = 2.225074E-308;
#define CUDART_TRIG_PLOSS  (2.147484E+09)
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaBindTexture)(NativeUInt &offset, const PTextureReference texref, void * &devPtr, TCudaChannelFormatDesc &desc, NativeUInt size);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaBindTexture2D)(NativeUInt &offset, const PTextureReference texref, const void * devPtr, TCudaChannelFormatDesc &desc, NativeUInt width, NativeUInt height, NativeUInt pitch);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaBindTextureToArray)(const PTextureReference texref, const PcudaArray cudaArray);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaUnbindTexture)(const PTextureReference texref);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGetTextureAlignmentOffset)(NativeUInt offset, const PTextureReference texref);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGetTextureReference)(const PTextureReference texref, const char * symbol);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGetChannelDesc)(TCudaChannelFormatDesc &desc, const void * array_);
extern DELPHI_PACKAGE TCudaChannelFormatDesc __stdcall (*cudaCreateChannelDesc)(int x, int y, int z, int w, TCudaChannelFormatKind f);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMalloc3D)(TcudaPitchedPtr &pitchedDevPtr, TcudaExtent extent);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMalloc3DArray)(PcudaArray &arrayPtr, const TCudaChannelFormatDesc &desc, TcudaExtent extent, unsigned flags);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemset3D)(TcudaPitchedPtr pitchedDevPtr, int value, TcudaExtent extent);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpy3D)(const TcudaMemcpy3DParms &p);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpy3DAsync)(const TcudaMemcpy3DParms &p, int stream);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMalloc)(void *devPtr, NativeUInt size);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMallocHost)(void * &ptr, NativeUInt size);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMallocPitch)(void *devPtr, NativeUInt &pitch, NativeUInt width, NativeUInt height);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMallocArray)(void * &aarray, TCudaChannelFormatDesc &desc, NativeUInt width, NativeUInt height);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaFree)(void * devPtr);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaFreeHost)(void * ptr);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaFreeArray)(const void * aarray);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaHostAlloc)(void * &pHost, NativeUInt bytes, unsigned flags);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaHostGetDevicePointer)(void * &pDevice, void * pHost, unsigned flags);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaHostGetFlags)(unsigned &pFlags, void * pHost);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemGetInfo)(NativeUInt &free, NativeUInt &total);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpy)(void * dst, void * src, NativeUInt count, TcudaMemcpyKind kind);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpyToArray)(PcudaArray &dst, NativeUInt wOffset, NativeUInt hOffset, void *src, NativeUInt count, TcudaMemcpyKind kind);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpyFromArray)(void *dst, const PcudaArray src, NativeUInt wOffset, NativeUInt hOffset, NativeUInt count, TcudaMemcpyKind kind);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpyArrayToArray)(PcudaArray dst, NativeUInt wOffsetDst, NativeUInt hOffsetDst, const PcudaArray src, NativeUInt wOffsetSrc, NativeUInt hOffsetSrc, NativeUInt count, const TcudaMemcpyKind kind/* = (TcudaMemcpyKind)(0x3)*/);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpy2D)(void *dst, NativeUInt dpitch, void *src, NativeUInt spitch, NativeUInt width, NativeUInt height, TcudaMemcpyKind kind);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpy2DToArray)(PcudaArray dst, NativeUInt wOffset, NativeUInt hOffset, void *src, NativeUInt spitch, NativeUInt width, NativeUInt height, TcudaMemcpyKind kind);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpy2DFromArray)(void *dst, NativeUInt dpitch, PcudaArray src, NativeUInt wOffset, NativeUInt hOffset, NativeUInt width, NativeUInt height, TcudaMemcpyKind kind);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpy2DArrayToArray)(PcudaArray dst, NativeUInt wOffsetDst, NativeUInt hOffsetDst, PcudaArray src, NativeUInt wOffsetSrc, NativeUInt hOffsetSrc, NativeUInt width, NativeUInt height, const TcudaMemcpyKind kind/* = (TcudaMemcpyKind)(0x3)*/);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpyToSymbol)(char * symbol, void *src, NativeUInt count, const NativeUInt offset/* = (NativeUInt)(0x0)*/, const TcudaMemcpyKind kind/* = (TcudaMemcpyKind)(0x1)*/);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpyFromSymbol)(void *dst, char * symbol, NativeUInt count, const NativeUInt offset/* = (NativeUInt)(0x0)*/, const TcudaMemcpyKind kind/* = (TcudaMemcpyKind)(0x2)*/);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpyAsync)(void *dst, const void *src, NativeUInt count, TcudaMemcpyKind kind, int stream);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpyToArrayAsync)(PcudaArray dst, NativeUInt wOffset, NativeUInt hOffset, const void *src, NativeUInt count, TcudaMemcpyKind kind, int stream);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpyFromArrayAsync)(void *dst, const PcudaArray src, NativeUInt wOffset, NativeUInt hOffset, NativeUInt count, TcudaMemcpyKind kind, int stream);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpy2DAsync)(void *dst, NativeUInt dpitch, const void *src, NativeUInt spitch, NativeUInt width, NativeUInt height, TcudaMemcpyKind kind, int stream);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpy2DToArrayAsync)(PcudaArray dst, NativeUInt wOffset, NativeUInt hOffset, const void *src, NativeUInt spitch, NativeUInt width, NativeUInt height, TcudaMemcpyKind kind, int stream);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpy2DFromArrayAsync)(void *dst, NativeUInt dpitch, const PcudaArray src, NativeUInt wOffset, NativeUInt hOffset, NativeUInt width, NativeUInt height, TcudaMemcpyKind kind, int stream);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpyToSymbolAsync)(const char * symbol, const void *src, NativeUInt count, NativeUInt offset, TcudaMemcpyKind kind, int stream);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemcpyFromSymbolAsync)(void *dst, const char * symbol, NativeUInt count, NativeUInt offset, TcudaMemcpyKind kind, int stream);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemset)(void *devPtr, int value, NativeUInt count);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaMemset2D)(void *devPtr, NativeUInt pitch, int value, NativeUInt width, NativeUInt height);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGetSymbolAddress)(void * &devPtr, const char * symbol);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGetSymbolSize)(NativeUInt &size, const char * symbol);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGetDeviceCount)(int &count);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGetDeviceProperties)(TCudaDeviceProp &prop, int device);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaChooseDevice)(int &device, const PCudaDeviceProp prop);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaSetDevice)(int device);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGetDevice)(int &device);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaSetDeviceFlags)(int flags);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaSetValidDevices)(System::PInteger device_arr, int len);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaConfigureCall)(const Cuda::Import::TDim3 &gridDim, const Cuda::Import::TDim3 &blockDim, NativeUInt sharedMem, int stream);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaSetupArgument)(const void * arg, NativeUInt size, NativeUInt offset);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaFuncSetCacheConfig)(const char * func, Cuda::Import::TcudaFuncCache cacheConfig);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaLaunch)(const char * entry);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaFuncGetAttributes)(Cuda::Import::TcudaFuncAttributes &attr, const char * func);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGetLastError)(void);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGLSetGLDevice)(int device);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGLRegisterBufferObject)(unsigned bufObj);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGraphicsGLRegisterImage)(const Cuda::Import::PCUgraphicsResource resource, unsigned image, unsigned target, unsigned flags);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGraphicsGLRegisterBuffer)(const Cuda::Import::PCUgraphicsResource resource, unsigned buffer, unsigned flags);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGLMapBufferObject)(void * devPtr, unsigned bufObj);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGLUnmapBufferObject)(unsigned bufObj);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGLUnregisterBufferObject)(unsigned bufObj);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGLSetBufferObjectMapFlags)(unsigned bufObj, TCudaGLMapFlags flags);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGLMapBufferObjectAsync)(void * &devPtr, unsigned bufObj, int stream);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGLUnmapBufferObjectAsync)(unsigned bufObj, int stream);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGraphicsUnregisterResource)(Cuda::Import::PCUgraphicsResource resource);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGraphicsResourceSetMapFlags)(Cuda::Import::PCUgraphicsResource resource, unsigned flags);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGraphicsMapResources)(int count, const Cuda::Import::PCUgraphicsResource resources, int stream);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGraphicsUnmapResources)(int count, const Cuda::Import::PCUgraphicsResource resources, int stream);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGraphicsResourceGetMappedPointer)(void * &pDevPtr, unsigned &pSize, Cuda::Import::PCUgraphicsResource resource);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaGraphicsSubResourceGetMappedArray)(Cuda::Import::PCUarray &pArray, Cuda::Import::PCUgraphicsResource resource, unsigned arrayIndex, unsigned mipLevel);
extern DELPHI_PACKAGE char * __stdcall (*cudaGetErrorString)(TcudaError error);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaDriverGetVersion)(/* out */ int &driverVersion);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaRuntimeGetVersion)(/* out */ int &runtimeVersion);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaSetDoubleForDevice)(double &d);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaSetDoubleForHost)(double &d);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaStreamCreate)(int &pStream);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaStreamDestroy)(int stream);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaStreamSynchronize)(int stream);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaStreamQuery)(int stream);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaEventCreate)(int &event);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaEventCreateWithFlags)(int &event, int flags);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaEventRecord)(int event, int stream);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaEventQuery)(int event);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaEventSynchronize)(int event);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaEventDestroy)(int event);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaEventElapsedTime)(float &ms, int start, int ending);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaWGLGetDevice)(int &device, void * hGpu);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaThreadExit)(void);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaThreadSynchronize)(void);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaThreadSetLimit)(Cuda::Import::TcudaLimit limit, NativeUInt value);
extern DELPHI_PACKAGE TcudaError __stdcall (*cudaThreadGetLimit)(NativeUInt &value, Cuda::Import::TcudaLimit limit);
extern DELPHI_PACKAGE System::UnicodeString __fastcall cudaGetLastErrorString(void);
extern DELPHI_PACKAGE bool __fastcall InitCUDART(void);
extern DELPHI_PACKAGE void __fastcall CloseCUDART(void);
extern DELPHI_PACKAGE bool __fastcall InitCUDARTFromLibrary(const System::WideString LibName);
extern DELPHI_PACKAGE bool __fastcall IsCUDARTInitialized(void);
}	/* namespace Runtime */
}	/* namespace Cuda */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA_RUNTIME)
using namespace Cuda::Runtime;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA)
using namespace Cuda;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Cuda_RuntimeHPP
