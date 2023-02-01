// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CUDA.FFTPlan.pas' rev: 35.00 (Windows)

#ifndef Cuda_FftplanHPP
#define Cuda_FftplanHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <CUDA.Import.hpp>
#include <CUDA.Context.hpp>
#include <CUDA.APIComps.hpp>
#include <CUDA.FourierTransform.hpp>
#include <GLS.Strings.hpp>
#include <GLS.Logger.hpp>

//-- user supplied -----------------------------------------------------------

namespace Cuda
{
namespace Fftplan
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCUDAFFTPlan;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TCUDAFFTransform : unsigned char { fftRealToComplex, fftComplexToReal, fftComplexToComplex, fftDoubleToDoubleComplex, fftDoubleComplexToDouble, fftDoubleComplexToDoubleComplex };

enum DECLSPEC_DENUM TCUDAFFTdir : unsigned char { fftdForward, fftdInverse };

class PASCALIMPLEMENTATION TCUDAFFTPlan : public Cuda::Apicomps::TCUDAComponent
{
	typedef Cuda::Apicomps::TCUDAComponent inherited;
	
private:
	Cuda::Fouriertransform::TcufftHandle FHandle;
	int FWidth;
	int FHeight;
	int FDepth;
	int FBatch;
	int FSize;
	int FPaddedSize;
	TCUDAFFTransform FTransform;
	Cuda::Fouriertransform::TcufftResult FStatus;
	void __fastcall SetWidth(int Value);
	void __fastcall SetHeight(int Value);
	void __fastcall SetDepth(int Value);
	void __fastcall SetBatch(int Value);
	void __fastcall SetTransform(TCUDAFFTransform Value);
	
protected:
	virtual void __fastcall AllocateHandles();
	virtual void __fastcall DestroyHandles();
	__classmethod void __fastcall CheckLib();
	
public:
	__fastcall virtual TCUDAFFTPlan(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCUDAFFTPlan();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Execute(Cuda::Apicomps::TCUDAMemData* ASrc, Cuda::Apicomps::TCUDAMemData* ADst, const TCUDAFFTdir ADir = (TCUDAFFTdir)(0x0));
	
__published:
	__property int Width = {read=FWidth, write=SetWidth, default=256};
	__property int Height = {read=FHeight, write=SetHeight, default=0};
	__property int Depth = {read=FDepth, write=SetDepth, default=0};
	__property int Batch = {read=FBatch, write=SetBatch, default=1};
	__property TCUDAFFTransform Transform = {read=FTransform, write=SetTransform, default=0};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Fftplan */
}	/* namespace Cuda */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA_FFTPLAN)
using namespace Cuda::Fftplan;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA)
using namespace Cuda;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Cuda_FftplanHPP
