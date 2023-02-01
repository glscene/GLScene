// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CUDA.Context.pas' rev: 35.00 (Windows)

#ifndef Cuda_ContextHPP
#define Cuda_ContextHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.Strings.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.Context.hpp>
#include <GLS.Generics.hpp>
#include <CUDA.Import.hpp>
#include <CUDA.Runtime.hpp>
#include <System.SyncObjs.hpp>

//-- user supplied -----------------------------------------------------------

namespace Cuda
{
namespace Context
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCUDADimensions;
class DELPHICLASS TCUDADevice;
class DELPHICLASS TGLCUDADevice;
class DELPHICLASS TCUDAHandlesMaster;
class DELPHICLASS TCUDAContext;
class DELPHICLASS CUDAContextManager;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TCUDADimensions : public Gls::Baseclasses::TGLUpdateAbleObject
{
	typedef Gls::Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	Cuda::Import::TDim3 FXYZ;
	Cuda::Import::TDim3 FMaxXYZ;
	bool FReadOnly;
	int __fastcall GetDimComponent(int index);
	void __fastcall SetDimComponent(int index, int Value);
	int __fastcall GetMaxDimComponent(int index);
	void __fastcall SetMaxDimComponent(int index, int Value);
	
public:
	__fastcall virtual TCUDADimensions(System::Classes::TPersistent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property int MaxSizeX = {read=GetMaxDimComponent, write=SetMaxDimComponent, index=0, nodefault};
	__property int MaxSizeY = {read=GetMaxDimComponent, write=SetMaxDimComponent, index=1, nodefault};
	__property int MaxSizeZ = {read=GetMaxDimComponent, write=SetMaxDimComponent, index=2, nodefault};
	__property bool ReadOnlyValue = {read=FReadOnly, write=FReadOnly, nodefault};
	
__published:
	__property int SizeX = {read=GetDimComponent, write=SetDimComponent, index=0, default=1};
	__property int SizeY = {read=GetDimComponent, write=SetDimComponent, index=1, default=1};
	__property int SizeZ = {read=GetDimComponent, write=SetDimComponent, index=2, default=1};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TCUDADimensions() { }
	
};


typedef void __fastcall (__closure *TOnOpenGLInteropInit)(/* out */ Gls::Context::TGLContext* &Context);

class PASCALIMPLEMENTATION TCUDADevice : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	int fID;
	int fHandle;
	int fGFlops;
	Cuda::Runtime::TCudaDeviceProp fDeviceProperties;
	bool FSuitable;
	bool FUsed;
	TCUDADimensions* fMaxThreadsDim;
	TCUDADimensions* fMaxGridSize;
	
protected:
	System::UnicodeString __fastcall GetName();
	
public:
	__fastcall TCUDADevice();
	__fastcall virtual ~TCUDADevice();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	unsigned __fastcall TotalMemory();
	
__published:
	__property System::UnicodeString Name = {read=GetName};
	__property NativeUInt TotalGlobalMem = {read=fDeviceProperties.totalGlobalMem};
	__property NativeUInt SharedMemPerBlock = {read=fDeviceProperties.sharedMemPerBlock};
	__property int RegsPerBlock = {read=fDeviceProperties.regsPerBlock, nodefault};
	__property int WarpSize = {read=fDeviceProperties.warpSize, nodefault};
	__property NativeUInt MemPitch = {read=fDeviceProperties.memPitch};
	__property int MaxThreadsPerBlock = {read=fDeviceProperties.maxThreadsPerBlock, nodefault};
	__property TCUDADimensions* MaxThreadsDim = {read=fMaxThreadsDim};
	__property TCUDADimensions* MaxGridSize = {read=fMaxGridSize};
	__property int ClockRate = {read=fDeviceProperties.clockRate, nodefault};
	__property NativeUInt TotalConstMem = {read=fDeviceProperties.totalConstMem};
	__property int Major = {read=fDeviceProperties.major, nodefault};
	__property int Minor = {read=fDeviceProperties.minor, nodefault};
	__property NativeUInt TextureAlignment = {read=fDeviceProperties.textureAlignment};
	__property int DeviceOverlap = {read=fDeviceProperties.deviceOverlap, nodefault};
	__property int MultiProcessorCount = {read=fDeviceProperties.multiProcessorCount, nodefault};
};


class PASCALIMPLEMENTATION TGLCUDADevice : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FSelectDeviceName;
	TCUDADevice* __fastcall GetDevice();
	void __fastcall SetDevice(TCUDADevice* AValue);
	void __fastcall SetDeviceName(const System::UnicodeString AName);
	
public:
	__fastcall virtual TGLCUDADevice(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCUDADevice();
	bool __fastcall Suitable();
	
__published:
	__property System::UnicodeString SelectDevice = {read=FSelectDeviceName, write=SetDeviceName};
	__property TCUDADevice* Device = {read=GetDevice, write=SetDevice};
};


class PASCALIMPLEMENTATION TCUDAHandlesMaster : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
protected:
	virtual TCUDAContext* __fastcall GetContext() = 0 ;
	virtual void __fastcall AllocateHandles();
	virtual void __fastcall DestroyHandles();
public:
	/* TComponent.Create */ inline __fastcall virtual TCUDAHandlesMaster(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TCUDAHandlesMaster() { }
	
};


typedef Gls::Generics::GThreadList__1<TCUDAHandlesMaster*>* TCUDAHandleList;

class PASCALIMPLEMENTATION TCUDAContext : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Cuda::Import::TCUcontext *fHandle;
	TCUDADevice* FDevice;
	TOnOpenGLInteropInit FOnOpenGLInteropInit;
	Gls::Generics::GThreadList__1<TCUDAHandlesMaster*>* FHandleList;
	void __fastcall SetDevice(TCUDADevice* ADevice);
	
public:
	__fastcall TCUDAContext();
	__fastcall virtual ~TCUDAContext();
	void __fastcall DestroyAllHandles();
	void __fastcall Requires();
	void __fastcall Release();
	bool __fastcall IsValid();
	__property TCUDADevice* Device = {read=FDevice, write=SetDevice};
	__property TOnOpenGLInteropInit OnOpenGLInteropInit = {read=FOnOpenGLInteropInit, write=FOnOpenGLInteropInit};
};


typedef Gls::Generics::GList__1<TCUDADevice*>* TCUDADeviceList;

typedef Gls::Generics::GList__1<TCUDAContext*>* TCUDAContextList;

class PASCALIMPLEMENTATION CUDAContextManager : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<Gls::Generics::GList__1<TCUDAContext*>*> _CUDAContextManager__1;
	
	
private:
	static Gls::Generics::GList__1<TCUDADevice*>* fDeviceList;
	static Gls::Generics::GList__1<TCUDAContext*>* fContextList;
	static _CUDAContextManager__1 FContextStacks;
	
protected:
	__classmethod TCUDADevice* __fastcall GetDevice(int i);
	__classmethod TCUDADevice* __fastcall GetNextUnusedDevice();
	__classmethod void __fastcall RegisterContext(TCUDAContext* aContext);
	__classmethod void __fastcall UnRegisterContext(TCUDAContext* aContext);
	__classmethod Gls::Generics::GList__1<TCUDAContext*>* __fastcall GetThreadStack();
	__classmethod TCUDAContext* __fastcall GetContext(int i);
	
public:
	__classmethod void __fastcall Init();
	__classmethod void __fastcall Done();
	__classmethod void __fastcall CreateContext(TCUDAContext* aContext);
	__classmethod void __fastcall DestroyContext(TCUDAContext* aContext);
	__classmethod void __fastcall CreateContextOf(TCUDADevice* ADevice);
	__classmethod void __fastcall DestroyContextOf(TCUDADevice* ADevice);
	__classmethod void __fastcall PushContext(TCUDAContext* aContext);
	__classmethod TCUDAContext* __fastcall PopContext();
	__classmethod void __fastcall FillUnusedDeviceList(System::Classes::TStringList* &AList);
	__classmethod TCUDADevice* __fastcall GetDeviceByName(const System::UnicodeString AName);
	__classmethod int __fastcall DeviceCount();
	__property TCUDADevice* Devices[int i] = {read=GetDevice};
	__classmethod TCUDADevice* __fastcall GetMaxGflopsDevice();
	__classmethod int __fastcall ContextCount();
	__classmethod TCUDAContext* __fastcall GetCurrentThreadContext();
	__property TCUDAContext* Contexts[int i] = {read=GetContext};
	
private:
	// __classmethod void __fastcall Create@();
	// __classmethod void __fastcall Destroy@();
public:
	/* TObject.Create */ inline __fastcall CUDAContextManager() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~CUDAContextManager() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Context */
}	/* namespace Cuda */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA_CONTEXT)
using namespace Cuda::Context;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA)
using namespace Cuda;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Cuda_ContextHPP
