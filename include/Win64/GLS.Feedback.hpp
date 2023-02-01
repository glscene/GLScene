// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Feedback.pas' rev: 35.00 (Windows)

#ifndef Gls_FeedbackHPP
#define Gls_FeedbackHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.PipelineTransformation.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.Scene.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.Texture.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.Context.hpp>
#include <GLS.State.hpp>
#include <GLS.MeshUtils.hpp>
#include <GLS.VectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Feedback
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLFeedback;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TGLFeedbackMode : unsigned char { fm2D, fm3D, fm3DColor, fm3DColorTexture, fm4DColorTexture };

class PASCALIMPLEMENTATION TGLFeedback : public Gls::Scene::TGLBaseSceneObject
{
	typedef Gls::Scene::TGLBaseSceneObject inherited;
	
private:
	bool FActive;
	Gls::Vectorlists::TGLSingleList* FBuffer;
	unsigned FMaxBufferSize;
	bool FBuffered;
	float FCorrectionScaling;
	TGLFeedbackMode FMode;
	
protected:
	void __fastcall SetMaxBufferSize(const unsigned Value);
	void __fastcall SetMode(const TGLFeedbackMode Value);
	
public:
	__fastcall virtual TGLFeedback(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLFeedback();
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	void __fastcall BuildMeshFromBuffer(Gls::Vectorlists::TGLAffineVectorList* Vertices = (Gls::Vectorlists::TGLAffineVectorList*)(0x0), Gls::Vectorlists::TGLAffineVectorList* Normals = (Gls::Vectorlists::TGLAffineVectorList*)(0x0), Gls::Vectorlists::TGLVectorList* Colors = (Gls::Vectorlists::TGLVectorList*)(0x0), Gls::Vectorlists::TGLAffineVectorList* TexCoords = (Gls::Vectorlists::TGLAffineVectorList*)(0x0), Gls::Vectorlists::TGLIntegerList* VertexIndices = (Gls::Vectorlists::TGLIntegerList*)(0x0));
	__property bool Buffered = {read=FBuffered, nodefault};
	__property Gls::Vectorlists::TGLSingleList* Buffer = {read=FBuffer};
	__property float CorrectionScaling = {read=FCorrectionScaling};
	
__published:
	__property unsigned MaxBufferSize = {read=FMaxBufferSize, write=SetMaxBufferSize, nodefault};
	__property bool Active = {read=FActive, write=FActive, nodefault};
	__property TGLFeedbackMode Mode = {read=FMode, write=SetMode, nodefault};
	__property Visible = {default=1};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFeedback(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLBaseSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Feedback */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_FEEDBACK)
using namespace Gls::Feedback;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_FeedbackHPP
