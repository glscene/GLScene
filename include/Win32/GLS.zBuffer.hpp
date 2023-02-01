// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.zBuffer.pas' rev: 35.00 (Windows)

#ifndef Gls_ZbufferHPP
#define Gls_ZbufferHPP

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
#include <System.Math.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.XOpenGL.hpp>
#include <GLS.Scene.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.Objects.hpp>
#include <GLS.Context.hpp>
#include <GLS.SceneViewer.hpp>
#include <GLS.Color.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.State.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.PersistentClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Zbuffer
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EZBufferException;
class DELPHICLASS TGLzBuffer;
class DELPHICLASS TGLZShadows;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EZBufferException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EZBufferException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EZBufferException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EZBufferException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EZBufferException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EZBufferException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EZBufferException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EZBufferException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EZBufferException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EZBufferException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EZBufferException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EZBufferException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EZBufferException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EZBufferException() { }
	
};

#pragma pack(pop)

typedef System::StaticArray<float, 268435456> TZArray;

typedef TZArray *PZArray;

typedef System::DynamicArray<PZArray> TZArrayIdx;

typedef System::StaticArray<System::Byte, 268435456> TAArray;

typedef TAArray *PAArray;

typedef System::DynamicArray<PAArray> TAArrayIdx;

enum DECLSPEC_DENUM TOptimise : unsigned char { opNone, op4in1, op9in1, op16in1 };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLzBuffer : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TZArray *FData;
	TZArrayIdx FDataIdx;
	TZArrayIdx FDataInvIdx;
	int FWidth;
	int FHeight;
	int FDataSize;
	float Ang1;
	float Ang2;
	float Scal;
	float C1;
	float S1;
	float C2;
	float S2;
	float Vw;
	float Vh;
	Gls::Vectortypes::TVector3f Lt;
	Gls::Vectortypes::TVector3f Rt;
	Gls::Vectortypes::TVector3f Lb;
	Gls::Vectortypes::TVector3f Rb;
	Gls::Vectortypes::TVector3f UpVec;
	Gls::Vectortypes::TVector3f RiVec;
	Gls::Vectortypes::TVector3f LtW;
	Gls::Vectortypes::TVector3f RtW;
	Gls::Vectortypes::TVector3f LbW;
	Gls::Vectortypes::TVector3f RbW;
	Gls::Vectortypes::TVector3f UpVecW;
	Gls::Vectortypes::TVector3f RiVecW;
	float OrthInvDov;
	float OrthAddX;
	float OrthMulX;
	float OrthAddY;
	float OrthMulY;
	float Dov;
	float Np;
	float Fp;
	float NpFp;
	float OneMinNp_Fp;
	float InvOneMinNp_Fp;
	Gls::Scene::TGLCamera* Cam;
	void __fastcall DoCalcVectors();
	
protected:
	void __fastcall PrepareBufferMemory();
	void __fastcall SetWidth(int val);
	void __fastcall SetHeight(const int val);
	
public:
	Gls::Sceneviewer::TGLSceneViewer* SceneViewer;
	Gls::Scene::TGLMemoryViewer* MemoryViewer;
	Gls::Scene::TGLSceneBuffer* Buffer;
	Gls::Vectortypes::TVector3f Normal;
	__fastcall TGLzBuffer();
	__fastcall virtual ~TGLzBuffer();
	void __fastcall LinkToViewer(Gls::Sceneviewer::TGLSceneViewer* viewer)/* overload */;
	void __fastcall LinkToViewer(Gls::Scene::TGLMemoryViewer* viewer)/* overload */;
	PZArray __fastcall GetDepthBuffer(bool CalcVectors, bool ContextIsActive);
	float __fastcall GetPixelzDepth(int x, int y);
	float __fastcall PixelToDistance_OLD(int x, int y);
	float __fastcall PixelToDistance(int x, int y);
	__property int Width = {read=FWidth, write=SetWidth, nodefault};
	__property int Height = {read=FHeight, write=SetHeight, nodefault};
	__property int DataSize = {read=FDataSize, nodefault};
	__property PZArray Data = {read=FData};
	__property TZArrayIdx DataIdx = {read=FDataIdx};
	__property TZArrayIdx DataInvIdx = {read=FDataIdx};
	void __fastcall Refresh();
	Gls::Vectortypes::TVector3f __fastcall FastScreenToVector(int x, int y);
	Gls::Vectortypes::TVector3f __fastcall FastVectorToScreen(const Gls::Vectortypes::TVector3f &vec);
	Gls::Vectortypes::TVector3f __fastcall PixelToWorld(const int x, const int y);
	bool __fastcall WorldToPixel(const Gls::Vectortypes::TVector3f &aPoint, /* out */ int &pixX, /* out */ int &pixY, /* out */ float &pixZ);
	bool __fastcall WorldToPixelZ(const Gls::Vectortypes::TVector3f &aPoint, /* out */ int &pixX, /* out */ int &pixY, /* out */ float &pixZ)/* overload */;
	bool __fastcall WorldToPixelZ(const Gls::Vectortypes::TVector3f &aPoint, /* out */ float &pixX, /* out */ float &pixY, /* out */ float &pixZ)/* overload */;
	bool __fastcall OrthWorldToPixelZ(const Gls::Vectortypes::TVector3f &aPoint, /* out */ float &pixX, /* out */ float &pixY, /* out */ float &pixZ);
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLZShadows : public Gls::Scene::TGLBaseSceneObject
{
	typedef Gls::Scene::TGLBaseSceneObject inherited;
	
private:
	Gls::Sceneviewer::TGLSceneViewer* FViewer;
	Gls::Scene::TGLMemoryViewer* FCaster;
	bool FDepthFade;
	bool FFrustShadow;
	bool FSkyShadow;
	TOptimise FOptimise;
	TAArray *FData;
	TAArrayIdx FDataIdx;
	TAArrayIdx FDataInvIdx;
	int FDataSize;
	int FWidth;
	int FHeight;
	int FXRes;
	int FYRes;
	bool Fsoft;
	float FTolerance;
	Gls::Color::TGLColor* FColor;
	Gls::Graphics::TGLPixel32 SCol;
	bool FTexturePrepared;
	Gls::Context::TGLTextureHandle* FTexHandle;
	
protected:
	void __fastcall PrepareAlphaMemory();
	Gls::Sceneviewer::TGLSceneViewer* __fastcall GetViewer();
	void __fastcall SetViewer(Gls::Sceneviewer::TGLSceneViewer* const val);
	Gls::Scene::TGLMemoryViewer* __fastcall GetCaster();
	void __fastcall SetCaster(Gls::Scene::TGLMemoryViewer* const val);
	void __fastcall CalcShadowTexture(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	System::Byte __fastcall HardSet(const int x, const int y);
	System::Byte __fastcall SoftTest(const int x, const int y);
	void __fastcall SetWidth(const int val);
	void __fastcall SetHeight(const int val);
	void __fastcall SetXRes(const int val);
	void __fastcall SetYRes(const int val);
	void __fastcall SetSoft(const bool val);
	void __fastcall BindTexture();
	
public:
	TGLzBuffer* ViewerZBuf;
	TGLzBuffer* CasterZBuf;
	__fastcall virtual TGLZShadows(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLZShadows();
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	
__published:
	__property Gls::Sceneviewer::TGLSceneViewer* Viewer = {read=GetViewer, write=SetViewer};
	__property Gls::Scene::TGLMemoryViewer* Caster = {read=GetCaster, write=SetCaster};
	__property bool FrustShadow = {read=FFrustShadow, write=FFrustShadow, nodefault};
	__property bool SkyShadow = {read=FSkyShadow, write=FSkyShadow, nodefault};
	__property TOptimise Optimise = {read=FOptimise, write=FOptimise, nodefault};
	__property int Width = {read=FWidth, write=SetWidth, nodefault};
	__property int Height = {read=FHeight, write=SetHeight, nodefault};
	__property Gls::Color::TGLColor* Color = {read=FColor, write=FColor};
	__property bool Soft = {read=Fsoft, write=SetSoft, nodefault};
	__property float Tolerance = {read=FTolerance, write=FTolerance};
	__property ObjectsSorting = {default=0};
	__property Visible = {default=1};
	__property bool DepthFade = {read=FDepthFade, write=FDepthFade, nodefault};
	bool __fastcall CastShadow();
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLZShadows(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLBaseSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Zbuffer */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_ZBUFFER)
using namespace Gls::Zbuffer;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_ZbufferHPP
