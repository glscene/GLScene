// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.PipelineTransformation.pas' rev: 35.00 (Windows)

#ifndef Gls_PipelinetransformationHPP
#define Gls_PipelinetransformationHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Logger.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Pipelinetransformation
{
//-- forward type declarations -----------------------------------------------
struct TTransformationRec;
class DELPHICLASS TGLTransformation;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLPipelineTransformationState : unsigned char { trsModelViewChanged, trsInvModelViewChanged, trsInvModelChanged, trsNormalModelChanged, trsViewProjChanged, trsFrustum };

typedef System::Set<TGLPipelineTransformationState, TGLPipelineTransformationState::trsModelViewChanged, TGLPipelineTransformationState::trsFrustum> TGLPipelineTransformationStates;

typedef TTransformationRec *PTransformationRec;

struct DECLSPEC_DRECORD TTransformationRec
{
public:
	TGLPipelineTransformationStates FStates;
	Gls::Vectortypes::TMatrix4f FModelMatrix;
	Gls::Vectortypes::TMatrix4f FViewMatrix;
	Gls::Vectortypes::TMatrix4f FProjectionMatrix;
	Gls::Vectortypes::TMatrix4f FInvModelMatrix;
	Gls::Vectortypes::TMatrix3f FNormalModelMatrix;
	Gls::Vectortypes::TMatrix4f FModelViewMatrix;
	Gls::Vectortypes::TMatrix4f FInvModelViewMatrix;
	Gls::Vectortypes::TMatrix4f FViewProjectionMatrix;
	Gls::Vectorgeometry::TFrustum FFrustum;
};


typedef void __fastcall (__closure *TOnMatricesPush)(void);

class PASCALIMPLEMENTATION TGLTransformation : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<TTransformationRec> _TGLTransformation__1;
	
	
private:
	int FStackPos;
	_TGLTransformation__1 FStack;
	bool FLoadMatricesEnabled;
	TOnMatricesPush FOnPush;
	Gls::Vectortypes::PGLMatrix __fastcall GetModelMatrix();
	Gls::Vectortypes::PGLMatrix __fastcall GetViewMatrix();
	Gls::Vectortypes::PGLMatrix __fastcall GetProjectionMatrix();
	Gls::Vectortypes::PGLMatrix __fastcall GetModelViewMatrix();
	Gls::Vectortypes::PGLMatrix __fastcall GetInvModelViewMatrix();
	Gls::Vectortypes::PGLMatrix __fastcall GetInvModelMatrix();
	Gls::Vectorgeometry::PAffineMatrix __fastcall GetNormalModelMatrix();
	Gls::Vectortypes::PGLMatrix __fastcall GetViewProjectionMatrix();
	Gls::Vectorgeometry::TFrustum __fastcall GetFrustum();
	
protected:
	void __fastcall LoadModelViewMatrix();
	void __fastcall LoadProjectionMatrix();
	void __fastcall DoMatricesLoaded();
	__property TOnMatricesPush OnPush = {read=FOnPush, write=FOnPush};
	
public:
	__fastcall TGLTransformation();
	void __fastcall SetModelMatrix(const Gls::Vectortypes::TMatrix4f &AMatrix);
	void __fastcall SetViewMatrix(const Gls::Vectortypes::TMatrix4f &AMatrix);
	void __fastcall SetProjectionMatrix(const Gls::Vectortypes::TMatrix4f &AMatrix);
	void __fastcall IdentityAll();
	void __fastcall Push(PTransformationRec AValue)/* overload */;
	void __fastcall Push()/* overload */;
	void __fastcall Pop();
	void __fastcall ReplaceFromStack();
	TTransformationRec __fastcall StackTop();
	__property Gls::Vectortypes::PGLMatrix ModelMatrix = {read=GetModelMatrix};
	__property Gls::Vectortypes::PGLMatrix ViewMatrix = {read=GetViewMatrix};
	__property Gls::Vectortypes::PGLMatrix ProjectionMatrix = {read=GetProjectionMatrix};
	__property Gls::Vectortypes::PGLMatrix InvModelMatrix = {read=GetInvModelMatrix};
	__property Gls::Vectortypes::PGLMatrix ModelViewMatrix = {read=GetModelViewMatrix};
	__property Gls::Vectorgeometry::PAffineMatrix NormalModelMatrix = {read=GetNormalModelMatrix};
	__property Gls::Vectortypes::PGLMatrix InvModelViewMatrix = {read=GetInvModelViewMatrix};
	__property Gls::Vectortypes::PGLMatrix ViewProjectionMatrix = {read=GetViewProjectionMatrix};
	__property Gls::Vectorgeometry::TFrustum Frustum = {read=GetFrustum};
	__property bool LoadMatricesEnabled = {read=FLoadMatricesEnabled, write=FLoadMatricesEnabled, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLTransformation() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Byte MAX_MATRIX_STACK_DEPTH = System::Byte(0x80);
#define cAllStatesChanged (System::Set<TGLPipelineTransformationState, TGLPipelineTransformationState::trsModelViewChanged, TGLPipelineTransformationState::trsFrustum>() << TGLPipelineTransformationState::trsModelViewChanged << TGLPipelineTransformationState::trsInvModelViewChanged << TGLPipelineTransformationState::trsInvModelChanged << TGLPipelineTransformationState::trsNormalModelChanged << TGLPipelineTransformationState::trsViewProjChanged << TGLPipelineTransformationState::trsFrustum )
}	/* namespace Pipelinetransformation */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_PIPELINETRANSFORMATION)
using namespace Gls::Pipelinetransformation;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_PipelinetransformationHPP
