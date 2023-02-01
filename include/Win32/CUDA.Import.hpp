// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CUDA.Import.pas' rev: 35.00 (Windows)

#ifndef Cuda_ImportHPP
#define Cuda_ImportHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>

//-- user supplied -----------------------------------------------------------

namespace Cuda
{
namespace Import
{
//-- forward type declarations -----------------------------------------------
struct DECLSPEC_DRECORD TCUcontext
{
};


struct DECLSPEC_DRECORD TCUmodule
{
};


struct DECLSPEC_DRECORD TCUfunction
{
};


struct DECLSPEC_DRECORD TCUarray
{
};


struct DECLSPEC_DRECORD TCUtexref
{
};


struct DECLSPEC_DRECORD TCUevent
{
};


struct DECLSPEC_DRECORD TCUstream
{
};


struct DECLSPEC_DRECORD TCUgraphicsResource
{
};


struct TCUdevprop;
struct TcudaFuncAttributes;
struct TCUDA_MEMCPY2D;
struct TCUDA_MEMCPY3D;
struct TCUDA_ARRAY_DESCRIPTOR;
struct TCUDA_ARRAY3D_DESCRIPTOR;
//-- type declarations -------------------------------------------------------
typedef void * TCUdeviceptr;

typedef int TCUdevice;

typedef TCUcontext *PCUcontext;

typedef TCUmodule *PCUmodule;

typedef TCUfunction *PCUfunction;

typedef TCUarray *PCUarray;

typedef TCUtexref *PCUtexref;

typedef TCUevent *PCUevent;

typedef TCUstream *PCUstream;

typedef TCUgraphicsResource *PCUgraphicsResource;

typedef PCUgraphicsResource *PPCUgraphicsResource;

enum DECLSPEC_DENUM TCUctx_flags : unsigned char { CU_CTX_SCHED_AUTO, CU_CTX_SCHED_SPIN, CU_CTX_SCHED_YIELD, CU_CTX_SCHED_MASK, CU_CTX_BLOCKING_SYNC, CU_CTX_MAP_HOST = 8, CU_CTX_FLAGS_MASK = 15 };

enum DECLSPEC_DENUM TCUevent_flags : unsigned char { CU_EVENT_DEFAULT, CU_EVENT_BLOCKING_SYNC };

enum DECLSPEC_DENUM TCUarray_format : unsigned char { CU_AD_FORMAT_UNSIGNED_INT8 = 1, CU_AD_FORMAT_UNSIGNED_INT16, CU_AD_FORMAT_UNSIGNED_INT32, CU_AD_FORMAT_SIGNED_INT8 = 8, CU_AD_FORMAT_SIGNED_INT16, CU_AD_FORMAT_SIGNED_INT32, CU_AD_FORMAT_HALF = 16, CU_AD_FORMAT_FLOAT = 32 };

enum DECLSPEC_DENUM TCUaddress_mode : unsigned char { CU_TR_ADDRESS_MODE_WRAP, CU_TR_ADDRESS_MODE_CLAMP, CU_TR_ADDRESS_MODE_MIRROR };

enum DECLSPEC_DENUM TCUfilter_mode : unsigned char { CU_TR_FILTER_MODE_POINT, CU_TR_FILTER_MODE_LINEAR };

enum DECLSPEC_DENUM TCUdevice_attribute : unsigned char { CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK = 1, CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X, CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Y, CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Z, CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X, CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Y, CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Z, CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK, CU_DEVICE_ATTRIBUTE_SHARED_MEMORY_PER_BLOCK = 8, CU_DEVICE_ATTRIBUTE_TOTAL_CONSTANT_MEMORY, CU_DEVICE_ATTRIBUTE_WARP_SIZE, CU_DEVICE_ATTRIBUTE_MAX_PITCH, CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_BLOCK, CU_DEVICE_ATTRIBUTE_REGISTERS_PER_BLOCK = 12, CU_DEVICE_ATTRIBUTE_CLOCK_RATE, CU_DEVICE_ATTRIBUTE_TEXTURE_ALIGNMENT, CU_DEVICE_ATTRIBUTE_GPU_OVERLAP, CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT, 
	CU_DEVICE_ATTRIBUTE_KERNEL_EXEC_TIMEOUT, CU_DEVICE_ATTRIBUTE_INTEGRATED, CU_DEVICE_ATTRIBUTE_CAN_MAP_HOST_MEMORY, CU_DEVICE_ATTRIBUTE_COMPUTE_MODE };

enum DECLSPEC_DENUM TcudaLimit : unsigned char { cudaLimitStackSize, cudaLimitPrintfFifoSize };

struct DECLSPEC_DRECORD TCUdevprop
{
public:
	int maxThreadsPerBlock;
	System::StaticArray<int, 3> maxThreadsDim;
	System::StaticArray<int, 3> maxGridSize;
	int sharedMemPerBlock;
	int totalConstantMemory;
	int SIMDWidth;
	int memPitch;
	int regsPerBlock;
	int clockRate;
	int textureAlign;
};


enum DECLSPEC_DENUM TCUfunction_attribute : unsigned char { CU_FUNC_ATTRIBUTE_MAX_THREADS_PER_BLOCK, CU_FUNC_ATTRIBUTE_SHARED_SIZE_BYTES, CU_FUNC_ATTRIBUTE_CONST_SIZE_BYTES, CU_FUNC_ATTRIBUTE_LOCAL_SIZE_BYTES, CU_FUNC_ATTRIBUTE_NUM_REGS, CU_FUNC_ATTRIBUTE_MAX };

enum DECLSPEC_DENUM TCUmemorytype : unsigned char { CU_MEMORYTYPE_HOST = 1, CU_MEMORYTYPE_DEVICE, CU_MEMORYTYPE_ARRAY };

enum DECLSPEC_DENUM TCUcomputemode : unsigned char { CU_COMPUTEMODE_DEFAULT, CU_COMPUTEMODE_EXCLUSIVE, CU_COMPUTEMODE_PROHIBITED };

enum DECLSPEC_DENUM TCUjit_option : unsigned char { CU_JIT_MAX_REGISTERS, CU_JIT_THREADS_PER_BLOCK, CU_JIT_WALL_TIME, CU_JIT_INFO_LOG_BUFFER, CU_JIT_INFO_LOG_BUFFER_SIZE_BYTES, CU_JIT_ERROR_LOG_BUFFER, CU_JIT_ERROR_LOG_BUFFER_SIZE_BYTES, CU_JIT_OPTIMIZATION_LEVEL, CU_JIT_TARGET_FROM_CUCONTEXT, CU_JIT_TARGET, CU_JIT_FALLBACK_STRATEGY };

enum DECLSPEC_DENUM TCUjit_target : unsigned char { CU_TARGET_COMPUTE_10, CU_TARGET_COMPUTE_11, CU_TARGET_COMPUTE_12, CU_TARGET_COMPUTE_13 };

enum DECLSPEC_DENUM TCUjit_fallback : unsigned char { CU_PREFER_PTX, CU_PREFER_BINARY };

enum DECLSPEC_DENUM TCUgraphicsRegisterFlags : unsigned char { CU_GRAPHICS_REGISTER_FLAGS_NONE };

enum DECLSPEC_DENUM TCUgraphicsMapResourceFlags : unsigned char { CU_GRAPHICS_MAP_RESOURCE_FLAGS_NONE, CU_GRAPHICS_MAP_RESOURCE_FLAGS_READ_ONLY, CU_GRAPHICS_MAP_RESOURCE_FLAGS_WRITE_DISCARD };

enum DECLSPEC_DENUM TCUarray_cubemap_face : unsigned char { CU_CUBEMAP_FACE_POSITIVE_X, CU_CUBEMAP_FACE_NEGATIVE_X, CU_CUBEMAP_FACE_POSITIVE_Y, CU_CUBEMAP_FACE_NEGATIVE_Y, CU_CUBEMAP_FACE_POSITIVE_Z, CU_CUBEMAP_FACE_NEGATIVE_Z };

struct DECLSPEC_DRECORD TcudaFuncAttributes
{
public:
	NativeUInt sharedSizeBytes;
	NativeUInt constSizeBytes;
	NativeUInt localSizeBytes;
	int maxThreadsPerBlock;
	int numRegs;
	int ptxVersion;
	int binaryVersion;
	System::StaticArray<int, 6> __cudaReserved;
};


enum DECLSPEC_DENUM TcudaFuncCache : unsigned char { cudaFuncCachePreferNone, cudaFuncCachePreferShared, cudaFuncCachePreferL1 };

typedef unsigned TCUresult;

typedef TCUDA_MEMCPY2D *PCUDA_MEMCPY2D;

struct DECLSPEC_DRECORD TCUDA_MEMCPY2D
{
public:
	unsigned srcXInBytes;
	unsigned srcY;
	TCUmemorytype srcMemoryType;
	void *srcHost;
	void *srcDevice;
	TCUarray *srcArray;
	unsigned srcPitch;
	unsigned dstXInBytes;
	unsigned dstY;
	TCUmemorytype dstMemoryType;
	void *dstHost;
	void *dstDevice;
	TCUarray *dstArray;
	unsigned dstPitch;
	unsigned WidthInBytes;
	unsigned Height;
};


struct DECLSPEC_DRECORD TCUDA_MEMCPY3D
{
public:
	unsigned srcXInBytes;
	unsigned srcY;
	unsigned srcZ;
	unsigned srcLOD;
	TCUmemorytype srcMemoryType;
	void *srcHost;
	void *srcDevice;
	TCUarray *srcArray;
	void *reserved0;
	unsigned srcPitch;
	unsigned srcHeight;
	unsigned dstXInBytes;
	unsigned dstY;
	unsigned dstZ;
	unsigned dstLOD;
	TCUmemorytype dstMemoryType;
	void *dstHost;
	void *dstDevice;
	TCUarray *dstArray;
	void *reserved1;
	unsigned dstPitch;
	unsigned dstHeight;
	unsigned WidthInBytes;
	unsigned Height;
	unsigned Depth;
};


typedef TCUDA_ARRAY_DESCRIPTOR *PCUDA_ARRAY_DESCRIPTOR;

struct DECLSPEC_DRECORD TCUDA_ARRAY_DESCRIPTOR
{
public:
	unsigned Width;
	unsigned Height;
	TCUarray_format Format;
	unsigned NumChannels;
};


struct DECLSPEC_DRECORD TCUDA_ARRAY3D_DESCRIPTOR
{
public:
	unsigned Width;
	unsigned Height;
	unsigned Depth;
	TCUarray_format Format;
	unsigned NumChannels;
	unsigned Flags;
};


enum DECLSPEC_DENUM TCUGLmap_flags : unsigned char { CU_GL_MAP_RESOURCE_FLAGS_NONE, CU_GL_MAP_RESOURCE_FLAGS_READ_ONLY, CU_GL_MAP_RESOURCE_FLAGS_WRITE_DISCARD };

typedef System::StaticArray<unsigned, 3> TDim3;

typedef void * HGPUNV;

typedef TCUresult __stdcall (*TcuInit)(unsigned Flags);

typedef TCUresult __stdcall (*TcuDriverGetVersion)(/* out */ int &driverVersion);

typedef TCUresult __stdcall (*TcuDeviceGet)(int &device, int ordinal);

typedef TCUresult __stdcall (*TcuDeviceGetCount)(int &count);

typedef TCUresult __stdcall (*TcuDeviceGetName)(char * name, int len, int dev);

typedef TCUresult __stdcall (*TcuDeviceComputeCapability)(int &major, int &minor, int dev);

typedef TCUresult __stdcall (*TcuDeviceTotalMem)(PSIZE_T bytes, int dev);

typedef TCUresult __stdcall (*TcuDeviceGetProperties)(TCUdevprop &prop, int dev);

typedef TCUresult __stdcall (*TcuDeviceGetAttribute)(PSIZE_T pi, TCUdevice_attribute attrib, int dev);

typedef TCUresult __stdcall (*TcuCtxCreate)(PCUcontext &pctx, unsigned Flags, int dev);

typedef TCUresult __stdcall (*TcuCtxDestroy)(PCUcontext ctx);

typedef TCUresult __stdcall (*TcuCtxAttach)(PCUcontext &pctx, unsigned Flags);

typedef TCUresult __stdcall (*TcuCtxDetach)(PCUcontext ctx);

typedef TCUresult __stdcall (*TcuCtxPushCurrent)(PCUcontext ctx);

typedef TCUresult __stdcall (*TcuCtxPopCurrent)(PCUcontext &pctx);

typedef TCUresult __stdcall (*TcuCtxGetDevice)(int &device);

typedef TCUresult __stdcall (*TcuCtxSynchronize)(void);

typedef TCUresult __stdcall (*TcuModuleLoad)(PCUmodule &module, const char * fname);

typedef TCUresult __stdcall (*TcuModuleLoadData)(PCUmodule &module, const char * image);

typedef TCUresult __stdcall (*TcuModuleLoadDataEx)(PCUmodule &module, void *image, unsigned numOptions, TCUjit_option &options, void *optionValues);

typedef TCUresult __stdcall (*TcuModuleLoadFatBinary)(PCUmodule &module, void *fatCubin);

typedef TCUresult __stdcall (*TcuModuleUnload)(PCUmodule hmod);

typedef TCUresult __stdcall (*TcuModuleGetFunction)(/* out */ PCUfunction &hfunc, PCUmodule hmod, const char * name);

typedef TCUresult __stdcall (*TcuModuleGetGlobal)(/* out */ void * &dptr, unsigned &bytes, PCUmodule hmod, const char * name);

typedef TCUresult __stdcall (*TcuModuleGetTexRef)(/* out */ PCUtexref &pTexRef, PCUmodule hmod, const char * name);

typedef TCUresult __stdcall (*TcuMemGetInfo)(unsigned &free, unsigned &total);

typedef TCUresult __stdcall (*TcuMemAlloc)(void * &dptr, unsigned bytesize);

typedef TCUresult __stdcall (*TcuMemAllocPitch)(void * &dptr, unsigned &pPitch, unsigned WidthInBytes, unsigned Height, unsigned ElementSizeBytes);

typedef TCUresult __stdcall (*TcuMemFree)(void * dptr);

typedef TCUresult __stdcall (*TcuMemGetAddressRange)(void * &pbase, unsigned &psize, void * dptr);

typedef TCUresult __stdcall (*TcuMemAllocHost)(void *pp, unsigned bytesize);

typedef TCUresult __stdcall (*TcuMemFreeHost)(void * p);

typedef TCUresult __stdcall (*TcuMemHostAlloc)(void * &pp, unsigned bytesize, unsigned Flags);

typedef TCUresult __stdcall (*TcuMemHostGetDevicePointer)(void * &pdptr, void * p, unsigned Flags);

typedef TCUresult __stdcall (*TcuMemHostGetFlags)(unsigned &pFlags, void *p);

typedef TCUresult __stdcall (*TcuMemcpyHtoD)(void * dstDevice, const void * srcHost, unsigned ByteCount);

typedef TCUresult __stdcall (*TcuMemcpyDtoH)(const void * dstHost, void * srcDevice, unsigned ByteCount);

typedef TCUresult __stdcall (*TcuMemcpyDtoD)(void * dstDevice, void * srcDevice, unsigned ByteCount);

typedef TCUresult __stdcall (*TcuMemcpyDtoDAsync)(void * dstDevice, void * srcDevice, unsigned ByteCount, PCUstream hStream);

typedef TCUresult __stdcall (*TcuMemcpyDtoA)(PCUarray dstArray, unsigned dstIndex, void * srcDevice, unsigned ByteCount);

typedef TCUresult __stdcall (*TcuMemcpyAtoD)(void * dstDevice, PCUarray hSrc, unsigned SrcIndex, unsigned ByteCount);

typedef TCUresult __stdcall (*TcuMemcpyHtoA)(PCUarray dstArray, unsigned dstIndex, void * pSrc, unsigned ByteCount);

typedef TCUresult __stdcall (*TcuMemcpyAtoH)(void * dstHost, PCUarray srcArray, unsigned SrcIndex, unsigned ByteCount);

typedef TCUresult __stdcall (*TcuMemcpyAtoA)(PCUarray dstArray, unsigned dstIndex, PCUarray srcArray, unsigned SrcIndex, unsigned ByteCount);

typedef TCUresult __stdcall (*TcuMemcpy2D)(const PCUDA_MEMCPY2D pCopy);

typedef TCUresult __stdcall (*TcuMemcpy2DUnaligned)(TCUDA_MEMCPY2D &pCopy);

typedef TCUresult __stdcall (*TcuMemcpy3D)(TCUDA_MEMCPY3D &pCopy);

typedef TCUresult __stdcall (*TcuMemcpyHtoDAsync)(void * dstDevice, void *srcHost, unsigned ByteCount, PCUstream hStream);

typedef TCUresult __stdcall (*TcuMemcpyDtoHAsync)(void *dstHost, void * srcDevice, unsigned ByteCount, PCUstream hStream);

typedef TCUresult __stdcall (*TcuMemcpyHtoAAsync)(PCUarray dstArray, unsigned dstIndex, void *pSrc, unsigned ByteCount, PCUstream hStream);

typedef TCUresult __stdcall (*TcuMemcpyAtoHAsync)(void *dstHost, PCUstream srcArray, unsigned SrcIndex, unsigned ByteCount, PCUstream hStream);

typedef TCUresult __stdcall (*TcuMemcpy2DAsync)(TCUDA_MEMCPY2D &pCopy, PCUstream hStream);

typedef TCUresult __stdcall (*TcuMemcpy3DAsync)(TCUDA_MEMCPY3D &pCopy, PCUstream hStream);

typedef TCUresult __stdcall (*TcuMemsetD8)(void * dstDevice, System::Byte ub, unsigned N);

typedef TCUresult __stdcall (*TcuMemsetD16)(void * dstDevice, System::Word uw, unsigned N);

typedef TCUresult __stdcall (*TcuMemsetD32)(void * dstDevice, unsigned ui, unsigned N);

typedef TCUresult __stdcall (*TcuMemsetD2D8)(void * dstDevice, unsigned dstPitch, System::Byte ub, unsigned Width, unsigned Height);

typedef TCUresult __stdcall (*TcuMemsetD2D16)(void * dstDevice, unsigned dstPitch, System::Word uw, unsigned Width, unsigned Height);

typedef TCUresult __stdcall (*TcuMemsetD2D32)(void * dstDevice, unsigned dstPitch, unsigned ui, unsigned Width, unsigned Height);

typedef TCUresult __stdcall (*TcuFuncSetBlockShape)(PCUfunction hfunc, int x, int y, int z);

typedef TCUresult __stdcall (*TcuFuncSetSharedSize)(PCUfunction hfunc, unsigned bytes);

typedef TCUresult __stdcall (*TcuFuncGetAttribute)(int &pi, TCUfunction_attribute attrib, PCUfunction hfunc);

typedef TCUresult __stdcall (*TcuArrayCreate)(PCUarray &pHandle, TCUDA_ARRAY_DESCRIPTOR &pAllocateArray);

typedef TCUresult __stdcall (*TcuArrayGetDescriptor)(TCUDA_ARRAY_DESCRIPTOR &pArrayDescriptor, PCUarray hArray);

typedef TCUresult __stdcall (*TcuArrayDestroy)(PCUarray hArray);

typedef TCUresult __stdcall (*TcuArray3DCreate)(PCUarray &pHandle, TCUDA_ARRAY3D_DESCRIPTOR &pAllocateArray);

typedef TCUresult __stdcall (*TcuArray3DGetDescriptor)(TCUDA_ARRAY3D_DESCRIPTOR &pArrayDescriptor, PCUarray hArray);

typedef TCUresult __stdcall (*TcuTexRefCreate)(PCUtexref &pTexRef);

typedef TCUresult __stdcall (*TcuTexRefDestroy)(PCUtexref hTexRef);

typedef TCUresult __stdcall (*TcuTexRefSetArray)(PCUtexref hTexRef, PCUarray hArray, unsigned Flags);

typedef TCUresult __stdcall (*TcuTexRefSetAddress)(unsigned &ByteOffset, PCUtexref hTexRef, void * dptr, unsigned bytes);

typedef TCUresult __stdcall (*TcuTexRefSetAddress2D)(PCUtexref hTexRef, TCUDA_ARRAY_DESCRIPTOR &desc, void * dptr, unsigned Pitch);

typedef TCUresult __stdcall (*TcuTexRefSetFormat)(PCUtexref hTexRef, TCUarray_format fmt, int NumPackedComponents);

typedef TCUresult __stdcall (*TcuTexRefSetAddressMode)(PCUtexref hTexRef, int dim, TCUaddress_mode am);

typedef TCUresult __stdcall (*TcuTexRefSetFilterMode)(PCUtexref hTexRef, TCUfilter_mode fm);

typedef TCUresult __stdcall (*TcuTexRefSetFlags)(PCUtexref hTexRef, unsigned Flags);

typedef TCUresult __stdcall (*TcuTexRefGetAddress)(void * &pdptr, PCUtexref hTexRef);

typedef TCUresult __stdcall (*TcuTexRefGetArray)(PCUarray &phArray, PCUtexref hTexRef);

typedef TCUresult __stdcall (*TcuTexRefGetAddressMode)(TCUaddress_mode &pam, PCUtexref hTexRef, int dim);

typedef TCUresult __stdcall (*TcuTexRefGetFilterMode)(TCUfilter_mode &pfm, PCUtexref hTexRef);

typedef TCUresult __stdcall (*TcuTexRefGetFormat)(TCUarray_format &pFormat, int &pNumChannels, PCUtexref hTexRef);

typedef TCUresult __stdcall (*TcuTexRefGetFlags)(unsigned &pFlags, PCUtexref hTexRef);

typedef TCUresult __stdcall (*TcuParamSetSize)(PCUfunction hfunc, unsigned numbytes);

typedef TCUresult __stdcall (*TcuParamSeti)(PCUfunction hfunc, int offset, unsigned value);

typedef TCUresult __stdcall (*TcuParamSetf)(PCUfunction hfunc, int offset, float value);

typedef TCUresult __stdcall (*TcuParamSetv)(PCUfunction hfunc, int offset, void *ptr, unsigned numbytes);

typedef TCUresult __stdcall (*TcuParamSetTexRef)(PCUfunction hfunc, int texunit, PCUtexref hTexRef);

typedef TCUresult __stdcall (*TcuLaunch)(PCUfunction f);

typedef TCUresult __stdcall (*TcuLaunchGrid)(PCUfunction f, int grid_width, int grid_height);

typedef TCUresult __stdcall (*TcuLaunchGridAsync)(PCUfunction f, int grid_width, int grid_height, PCUstream hStream);

typedef TCUresult __stdcall (*TcuEventCreate)(PCUevent &phEvent, unsigned Flags);

typedef TCUresult __stdcall (*TcuEventRecord)(PCUevent hEvent, PCUstream hStream);

typedef TCUresult __stdcall (*TcuEventQuery)(PCUevent hEvent);

typedef TCUresult __stdcall (*TcuEventSynchronize)(PCUevent hEvent);

typedef TCUresult __stdcall (*TcuEventDestroy)(PCUevent hEvent);

typedef TCUresult __stdcall (*TcuEventElapsedTime)(float &pMilliseconds, PCUevent hStart, PCUevent hEnd);

typedef TCUresult __stdcall (*TcuStreamCreate)(PCUstream &phStream, unsigned Flags);

typedef TCUresult __stdcall (*TcuStreamQuery)(PCUstream hStream);

typedef TCUresult __stdcall (*TcuStreamSynchronize)(PCUstream hStream);

typedef TCUresult __stdcall (*TcuStreamDestroy)(PCUstream hStream);

typedef TCUresult __stdcall (*TcuGLCtxCreate)(PCUcontext &pctx, unsigned Flags, int device);

typedef TCUresult __stdcall (*TcuGraphicsGLRegisterBuffer)(PCUgraphicsResource &pCudaResource, unsigned buffer, TCUgraphicsMapResourceFlags Flags);

typedef TCUresult __stdcall (*TcuGraphicsGLRegisterImage)(PCUgraphicsResource &pCudaResource, unsigned image, unsigned target, TCUgraphicsMapResourceFlags Flags);

typedef TCUresult __stdcall (*TcuWGLGetDevice)(int &pDevice, void * hGpu);

typedef TCUresult __stdcall (*TcuGraphicsUnregisterResource)(PCUgraphicsResource resource);

typedef TCUresult __stdcall (*TcuGraphicsSubResourceGetMappedArray)(PCUarray &pArray, PCUgraphicsResource resource, unsigned arrayIndex, unsigned mipLevel);

typedef TCUresult __stdcall (*TcuGraphicsResourceGetMappedPointer)(void * &pDevPtr, /* out */ unsigned &psize, PCUgraphicsResource resource);

typedef TCUresult __stdcall (*TcuGraphicsResourceSetMapFlags)(PCUgraphicsResource resource, unsigned Flags);

typedef TCUresult __stdcall (*TcuGraphicsMapResources)(unsigned count, PPCUgraphicsResource resources, PCUstream hStream);

typedef TCUresult __stdcall (*TcuGraphicsUnmapResources)(unsigned count, PPCUgraphicsResource resources, PCUstream hStream);

typedef void __stdcall (*TcuGLInit)(void);

typedef TCUresult __stdcall (*TcuGLRegisterBufferObject)(unsigned buffer);

typedef TCUresult __stdcall (*TcuGLMapBufferObject)(void * &dptr, unsigned &size, unsigned buffer);

typedef TCUresult __stdcall (*TcuGLUnmapBufferObject)(unsigned buffer);

typedef TCUresult __stdcall (*TcuGLUnregisterBufferObject)(unsigned buffer);

typedef TCUresult __stdcall (*TcuGLSetBufferObjectMapFlags)(unsigned buffer, unsigned Flags);

typedef TCUresult __stdcall (*TcuGLMapBufferObjectAsync)(void * &dptr, unsigned &size, unsigned buffer, PCUstream hStream);

typedef TCUresult __stdcall (*TcuGLUnmapBufferObjectAsync)(unsigned buffer, PCUstream hStream);

//-- var, const, procedure ---------------------------------------------------
#define CUDAAPIDLL L"nvcuda.dll"
extern DELPHI_PACKAGE TCUresult CUDA_SUCCESS;
static const System::Int8 CUDA_ERROR_INVALID_VALUE = System::Int8(0x1);
static const System::Int8 CUDA_ERROR_OUT_OF_MEMORY = System::Int8(0x2);
static const System::Int8 CUDA_ERROR_NOT_INITIALIZED = System::Int8(0x3);
static const System::Int8 CUDA_ERROR_DEINITIALIZED = System::Int8(0x4);
static const System::Int8 CUDA_ERROR_NO_DEVICE = System::Int8(0x64);
static const System::Int8 CUDA_ERROR_INVALID_DEVICE = System::Int8(0x65);
static const System::Byte CUDA_ERROR_INVALID_IMAGE = System::Byte(0xc8);
static const System::Byte CUDA_ERROR_INVALID_CONTEXT = System::Byte(0xc9);
static const System::Byte CUDA_ERROR_CONTEXT_ALREADY_CURRENT = System::Byte(0xca);
static const System::Byte CUDA_ERROR_MAP_FAILED = System::Byte(0xcd);
static const System::Byte CUDA_ERROR_UNMAP_FAILED = System::Byte(0xce);
static const System::Byte CUDA_ERROR_ARRAY_IS_MAPPED = System::Byte(0xcf);
static const System::Byte CUDA_ERROR_ALREADY_MAPPED = System::Byte(0xd0);
static const System::Byte CUDA_ERROR_NO_BINARY_FOR_GPU = System::Byte(0xd1);
static const System::Byte CUDA_ERROR_ALREADY_ACQUIRED = System::Byte(0xd2);
static const System::Byte CUDA_ERROR_NOT_MAPPED = System::Byte(0xd3);
static const System::Byte CUDA_ERROR_NOT_MAPPED_AS_ARRAY = System::Byte(0xd4);
static const System::Byte CUDA_ERROR_NOT_MAPPED_AS_POINTER = System::Byte(0xd5);
static const System::Word CUDA_ERROR_INVALID_SOURCE = System::Word(0x12c);
static const System::Word CUDA_ERROR_FILE_NOT_FOUND = System::Word(0x12d);
static const System::Word CUDA_ERROR_INVALID_HANDLE = System::Word(0x190);
static const System::Word CUDA_ERROR_NOT_FOUND = System::Word(0x1f4);
static const System::Word CUDA_ERROR_NOT_READY = System::Word(0x258);
static const System::Word CUDA_ERROR_LAUNCH_FAILED = System::Word(0x2bc);
static const System::Word CUDA_ERROR_LAUNCH_OUT_OF_RESOURCES = System::Word(0x2bd);
static const System::Word CUDA_ERROR_LAUNCH_TIMEOUT = System::Word(0x2be);
static const System::Word CUDA_ERROR_LAUNCH_INCOMPATIBLE_TEXTURING = System::Word(0x2bf);
static const System::Word CUDA_ERROR_POINTER_IS_64BIT = System::Word(0x320);
static const System::Word CUDA_ERROR_SIZE_IS_64BIT = System::Word(0x321);
static const System::Word CUDA_ERROR_UNKNOWN = System::Word(0x3e7);
static const System::Int8 CU_MEMHOSTALLOC_PORTABLE = System::Int8(0x1);
static const System::Int8 CU_MEMHOSTALLOC_DEVICEMAP = System::Int8(0x2);
static const System::Int8 CU_MEMHOSTALLOC_WRITECOMBINED = System::Int8(0x4);
static const System::Int8 CU_TRSA_OVERRIDE_FORMAT = System::Int8(0x1);
static const System::Int8 CU_TRSF_READ_AS_INTEGER = System::Int8(0x1);
static const System::Int8 CU_TRSF_NORMALIZED_COORDINATES = System::Int8(0x2);
static const System::Int8 CU_PARAM_TR_DEFAULT = System::Int8(-1);
extern DELPHI_PACKAGE TcuInit cuInit;
extern DELPHI_PACKAGE TcuDriverGetVersion cuDriverGetVersion;
extern DELPHI_PACKAGE TcuDeviceGet cuDeviceGet;
extern DELPHI_PACKAGE TcuDeviceGetCount cuDeviceGetCount;
extern DELPHI_PACKAGE TcuDeviceGetName cuDeviceGetName;
extern DELPHI_PACKAGE TcuDeviceComputeCapability cuDeviceComputeCapability;
extern DELPHI_PACKAGE TcuDeviceTotalMem cuDeviceTotalMem;
extern DELPHI_PACKAGE TcuDeviceGetProperties cuDeviceGetProperties;
extern DELPHI_PACKAGE TcuDeviceGetAttribute cuDeviceGetAttribute;
extern DELPHI_PACKAGE TcuCtxCreate cuCtxCreate;
extern DELPHI_PACKAGE TcuCtxDestroy cuCtxDestroy;
extern DELPHI_PACKAGE TcuCtxAttach cuCtxAttach;
extern DELPHI_PACKAGE TcuCtxDetach cuCtxDetach;
extern DELPHI_PACKAGE TcuCtxPushCurrent cuCtxPushCurrent;
extern DELPHI_PACKAGE TcuCtxPopCurrent cuCtxPopCurrent;
extern DELPHI_PACKAGE TcuCtxGetDevice cuCtxGetDevice;
extern DELPHI_PACKAGE TcuCtxSynchronize cuCtxSynchronize;
extern DELPHI_PACKAGE TcuModuleLoad cuModuleLoad;
extern DELPHI_PACKAGE TcuModuleLoadData cuModuleLoadData;
extern DELPHI_PACKAGE TcuModuleLoadDataEx cuModuleLoadDataEx;
extern DELPHI_PACKAGE TcuModuleLoadFatBinary cuModuleLoadFatBinary;
extern DELPHI_PACKAGE TcuModuleUnload cuModuleUnload;
extern DELPHI_PACKAGE TcuModuleGetFunction cuModuleGetFunction;
extern DELPHI_PACKAGE TcuModuleGetGlobal cuModuleGetGlobal;
extern DELPHI_PACKAGE TcuModuleGetTexRef cuModuleGetTexRef;
extern DELPHI_PACKAGE TcuMemGetInfo cuMemGetInfo;
extern DELPHI_PACKAGE TcuMemAlloc cuMemAlloc;
extern DELPHI_PACKAGE TcuMemAllocPitch cuMemAllocPitch;
extern DELPHI_PACKAGE TcuMemFree cuMemFree;
extern DELPHI_PACKAGE TcuMemGetAddressRange cuMemGetAddressRange;
extern DELPHI_PACKAGE TcuMemAllocHost cuMemAllocHost;
extern DELPHI_PACKAGE TcuMemFreeHost cuMemFreeHost;
extern DELPHI_PACKAGE TcuMemHostAlloc cuMemHostAlloc;
extern DELPHI_PACKAGE TcuMemHostGetDevicePointer cuMemHostGetDevicePointer;
extern DELPHI_PACKAGE TcuMemHostGetFlags cuMemHostGetFlags;
extern DELPHI_PACKAGE TcuMemcpyHtoD cuMemcpyHtoD;
extern DELPHI_PACKAGE TcuMemcpyDtoH cuMemcpyDtoH;
extern DELPHI_PACKAGE TcuMemcpyDtoD cuMemcpyDtoD;
extern DELPHI_PACKAGE TcuMemcpyDtoDAsync cuMemcpyDtoDAsync;
extern DELPHI_PACKAGE TcuMemcpyDtoA cuMemcpyDtoA;
extern DELPHI_PACKAGE TcuMemcpyAtoD cuMemcpyAtoD;
extern DELPHI_PACKAGE TcuMemcpyHtoA cuMemcpyHtoA;
extern DELPHI_PACKAGE TcuMemcpyAtoH cuMemcpyAtoH;
extern DELPHI_PACKAGE TcuMemcpyAtoA cuMemcpyAtoA;
extern DELPHI_PACKAGE TcuMemcpy2D cuMemcpy2D;
extern DELPHI_PACKAGE TcuMemcpy2DUnaligned cuMemcpy2DUnaligned;
extern DELPHI_PACKAGE TcuMemcpy3D cuMemcpy3D;
extern DELPHI_PACKAGE TcuMemcpyHtoDAsync cuMemcpyHtoDAsync;
extern DELPHI_PACKAGE TcuMemcpyDtoHAsync cuMemcpyDtoHAsync;
extern DELPHI_PACKAGE TcuMemcpyHtoAAsync cuMemcpyHtoAAsync;
extern DELPHI_PACKAGE TcuMemcpyAtoHAsync cuMemcpyAtoHAsync;
extern DELPHI_PACKAGE TcuMemcpy2DAsync cuMemcpy2DAsync;
extern DELPHI_PACKAGE TcuMemcpy3DAsync cuMemcpy3DAsync;
extern DELPHI_PACKAGE TcuMemsetD8 cuMemsetD8;
extern DELPHI_PACKAGE TcuMemsetD16 cuMemsetD16;
extern DELPHI_PACKAGE TcuMemsetD32 cuMemsetD32;
extern DELPHI_PACKAGE TcuMemsetD2D8 cuMemsetD2D8;
extern DELPHI_PACKAGE TcuMemsetD2D16 cuMemsetD2D16;
extern DELPHI_PACKAGE TcuMemsetD2D32 cuMemsetD2D32;
extern DELPHI_PACKAGE TcuFuncSetBlockShape cuFuncSetBlockShape;
extern DELPHI_PACKAGE TcuFuncSetSharedSize cuFuncSetSharedSize;
extern DELPHI_PACKAGE TcuFuncGetAttribute cuFuncGetAttribute;
extern DELPHI_PACKAGE TcuArrayCreate cuArrayCreate;
extern DELPHI_PACKAGE TcuArrayGetDescriptor cuArrayGetDescriptor;
extern DELPHI_PACKAGE TcuArrayDestroy cuArrayDestroy;
extern DELPHI_PACKAGE TcuArray3DCreate cuArray3DCreate;
extern DELPHI_PACKAGE TcuArray3DGetDescriptor cuArray3DGetDescriptor;
extern DELPHI_PACKAGE TcuTexRefCreate cuTexRefCreate;
extern DELPHI_PACKAGE TcuTexRefDestroy cuTexRefDestroy;
extern DELPHI_PACKAGE TcuTexRefSetArray cuTexRefSetArray;
extern DELPHI_PACKAGE TcuTexRefSetAddress cuTexRefSetAddress;
extern DELPHI_PACKAGE TcuTexRefSetAddress2D cuTexRefSetAddress2D;
extern DELPHI_PACKAGE TcuTexRefSetFormat cuTexRefSetFormat;
extern DELPHI_PACKAGE TcuTexRefSetAddressMode cuTexRefSetAddressMode;
extern DELPHI_PACKAGE TcuTexRefSetFilterMode cuTexRefSetFilterMode;
extern DELPHI_PACKAGE TcuTexRefSetFlags cuTexRefSetFlags;
extern DELPHI_PACKAGE TcuTexRefGetAddress cuTexRefGetAddress;
extern DELPHI_PACKAGE TcuTexRefGetArray cuTexRefGetArray;
extern DELPHI_PACKAGE TcuTexRefGetAddressMode cuTexRefGetAddressMode;
extern DELPHI_PACKAGE TcuTexRefGetFilterMode cuTexRefGetFilterMode;
extern DELPHI_PACKAGE TcuTexRefGetFormat cuTexRefGetFormat;
extern DELPHI_PACKAGE TcuTexRefGetFlags cuTexRefGetFlags;
extern DELPHI_PACKAGE TcuParamSetSize cuParamSetSize;
extern DELPHI_PACKAGE TcuParamSeti cuParamSeti;
extern DELPHI_PACKAGE TcuParamSetf cuParamSetf;
extern DELPHI_PACKAGE TcuParamSetv cuParamSetv;
extern DELPHI_PACKAGE TcuParamSetTexRef cuParamSetTexRef;
extern DELPHI_PACKAGE TcuLaunch cuLaunch;
extern DELPHI_PACKAGE TcuLaunchGrid cuLaunchGrid;
extern DELPHI_PACKAGE TcuLaunchGridAsync cuLaunchGridAsync;
extern DELPHI_PACKAGE TcuEventCreate cuEventCreate;
extern DELPHI_PACKAGE TcuEventRecord cuEventRecord;
extern DELPHI_PACKAGE TcuEventQuery cuEventQuery;
extern DELPHI_PACKAGE TcuEventSynchronize cuEventSynchronize;
extern DELPHI_PACKAGE TcuEventDestroy cuEventDestroy;
extern DELPHI_PACKAGE TcuEventElapsedTime cuEventElapsedTime;
extern DELPHI_PACKAGE TcuStreamCreate cuStreamCreate;
extern DELPHI_PACKAGE TcuStreamQuery cuStreamQuery;
extern DELPHI_PACKAGE TcuStreamSynchronize cuStreamSynchronize;
extern DELPHI_PACKAGE TcuStreamDestroy cuStreamDestroy;
extern DELPHI_PACKAGE TcuGLInit cuGLInit;
extern DELPHI_PACKAGE TcuGLCtxCreate cuGLCtxCreate;
extern DELPHI_PACKAGE TcuGraphicsGLRegisterBuffer cuGraphicsGLRegisterBuffer;
extern DELPHI_PACKAGE TcuGraphicsGLRegisterImage cuGraphicsGLRegisterImage;
extern DELPHI_PACKAGE TcuWGLGetDevice cuWGLGetDevice;
extern DELPHI_PACKAGE TcuGraphicsUnregisterResource cuGraphicsUnregisterResource;
extern DELPHI_PACKAGE TcuGraphicsSubResourceGetMappedArray cuGraphicsSubResourceGetMappedArray;
extern DELPHI_PACKAGE TcuGraphicsResourceGetMappedPointer cuGraphicsResourceGetMappedPointer;
extern DELPHI_PACKAGE TcuGraphicsResourceSetMapFlags cuGraphicsResourceSetMapFlags;
extern DELPHI_PACKAGE TcuGraphicsMapResources cuGraphicsMapResources;
extern DELPHI_PACKAGE TcuGraphicsUnmapResources cuGraphicsUnmapResources;
extern DELPHI_PACKAGE TcuGLRegisterBufferObject cuGLRegisterBufferObject;
extern DELPHI_PACKAGE TcuGLMapBufferObject cuGLMapBufferObject;
extern DELPHI_PACKAGE TcuGLUnmapBufferObject cuGLUnmapBufferObject;
extern DELPHI_PACKAGE TcuGLUnregisterBufferObject cuGLUnregisterBufferObject;
extern DELPHI_PACKAGE TcuGLSetBufferObjectMapFlags cuGLSetBufferObjectMapFlags;
extern DELPHI_PACKAGE TcuGLMapBufferObjectAsync cuGLMapBufferObjectAsync;
extern DELPHI_PACKAGE TcuGLUnmapBufferObjectAsync cuGLUnmapBufferObjectAsync;
extern DELPHI_PACKAGE bool __fastcall InitCUDA(void);
extern DELPHI_PACKAGE void __fastcall CloseCUDA(void);
extern DELPHI_PACKAGE bool __fastcall InitCUDAFromLibrary(const System::WideString LibName);
extern DELPHI_PACKAGE bool __fastcall IsCUDAInitialized(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall Get_CUDA_API_Error_String(TCUresult AError);
}	/* namespace Import */
}	/* namespace Cuda */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA_IMPORT)
using namespace Cuda::Import;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA)
using namespace Cuda;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Cuda_ImportHPP
