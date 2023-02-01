// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CUDA.Graphics.pas' rev: 35.00 (Windows)

#ifndef Cuda_GraphicsHPP
#define Cuda_GraphicsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGL.hpp>
#include <Winapi.OpenGLext.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <CUDA.Import.hpp>
#include <CUDA.APIComps.hpp>
#include <GLS.Context.hpp>
#include <GLS.State.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.Material.hpp>
#include <GLS.Strings.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.Texture.hpp>
#include <GLSL.Shader.hpp>
#include <GLSL.ShaderParameter.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Cuda
{
namespace Graphics
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLVertexAttribute;
class DELPHICLASS TGLVertexAttributes;
class DELPHICLASS TCUDACustomFeedBackMesh;
class DELPHICLASS TCUDAFeedbackMesh;
class DELPHICLASS TCUDAImageResource;
class DELPHICLASS TCUDAGeometryResource;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TOnBeforeKernelLaunch)(TGLVertexAttribute* Sender);

class PASCALIMPLEMENTATION TGLVertexAttribute : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FName;
	Glsl::Shaderparameter::TGLSLDataType FType;
	Cuda::Apicomps::TCUDAFunction* FFunc;
	int FLocation;
	TOnBeforeKernelLaunch FOnBeforeKernelLaunch;
	void __fastcall SetName(const System::UnicodeString AName);
	void __fastcall SetType(Glsl::Shaderparameter::TGLSLDataType AType);
	void __fastcall SetFunc(Cuda::Apicomps::TCUDAFunction* AFunc);
	int __fastcall GetLocation();
	HIDESBASE TGLVertexAttributes* __fastcall GetOwner();
	
public:
	__fastcall virtual TGLVertexAttribute(System::Classes::TCollection* ACollection);
	void __fastcall NotifyChange(System::TObject* Sender);
	__property int Location = {read=GetLocation, nodefault};
	
__published:
	__property System::UnicodeString Name = {read=FName, write=SetName};
	__property Glsl::Shaderparameter::TGLSLDataType GLSLType = {read=FType, write=SetType, nodefault};
	__property Cuda::Apicomps::TCUDAFunction* KernelFunction = {read=FFunc, write=SetFunc};
	__property TOnBeforeKernelLaunch OnBeforeKernelLaunch = {read=FOnBeforeKernelLaunch, write=FOnBeforeKernelLaunch};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TGLVertexAttribute() { }
	
};


class PASCALIMPLEMENTATION TGLVertexAttributes : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLVertexAttribute* operator[](int Index) { return this->Attributes[Index]; }
	
private:
	void __fastcall SetItems(int Index, TGLVertexAttribute* const AValue);
	TGLVertexAttribute* __fastcall GetItems(int Index);
	
public:
	__fastcall TGLVertexAttributes(System::Classes::TComponent* AOwner);
	void __fastcall NotifyChange(System::TObject* Sender);
	System::UnicodeString __fastcall MakeUniqueName(const System::UnicodeString ANameRoot);
	TGLVertexAttribute* __fastcall GetAttributeByName(const System::UnicodeString AName);
	HIDESBASE TGLVertexAttribute* __fastcall Add();
	__property TGLVertexAttribute* Attributes[int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLVertexAttributes() { }
	
};


enum DECLSPEC_DENUM TFeedBackMeshPrimitive : unsigned char { fbmpPoint, fbmpLine, fbmpTriangle };

enum DECLSPEC_DENUM TFeedBackMeshLaunching : unsigned char { fblCommon, fblOnePerAtttribute };

class PASCALIMPLEMENTATION TCUDACustomFeedBackMesh : public Gls::Scene::TGLBaseSceneObject
{
	typedef Gls::Scene::TGLBaseSceneObject inherited;
	
private:
	Cuda::Apicomps::TCUDAGraphicResource* FGeometryResource;
	TGLVertexAttributes* FAttributes;
	Gls::Context::TGLVertexArrayHandle* FVAO;
	Gls::Context::TGLVBOArrayBufferHandle* FVBO;
	Gls::Context::TGLVBOElementArrayHandle* FEBO;
	TFeedBackMeshPrimitive FPrimitiveType;
	int FVertexNumber;
	int FElementNumber;
	Glsl::Shader::TGLSLShader* FShader;
	Cuda::Apicomps::TCUDAFunction* FCommonFunc;
	TFeedBackMeshLaunching FLaunching;
	bool FBlend;
	void __fastcall SetAttributes(TGLVertexAttributes* AValue);
	void __fastcall SetPrimitiveType(TFeedBackMeshPrimitive AValue);
	void __fastcall SetVertexNumber(int AValue);
	void __fastcall SetElementNumber(int AValue);
	void __fastcall SetShader(Glsl::Shader::TGLSLShader* AShader);
	void __fastcall SetCommonFunc(Cuda::Apicomps::TCUDAFunction* AFunc);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall RefreshAttributes();
	void __fastcall AllocateHandles();
	void __fastcall LaunchKernels();
	__property TGLVertexAttributes* Attributes = {read=FAttributes, write=SetAttributes};
	__property Glsl::Shader::TGLSLShader* Shader = {read=FShader, write=SetShader};
	__property TFeedBackMeshPrimitive PrimitiveType = {read=FPrimitiveType, write=SetPrimitiveType, default=0};
	__property int VertexNumber = {read=FVertexNumber, write=SetVertexNumber, default=1};
	__property int ElementNumber = {read=FElementNumber, write=SetElementNumber, default=0};
	__property Cuda::Apicomps::TCUDAFunction* CommonKernelFunction = {read=FCommonFunc, write=SetCommonFunc};
	__property TFeedBackMeshLaunching Launching = {read=FLaunching, write=FLaunching, default=0};
	__property bool Blend = {read=FBlend, write=FBlend, default=0};
	
public:
	__fastcall virtual TCUDACustomFeedBackMesh(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCUDACustomFeedBackMesh();
	virtual void __fastcall DoRender(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	__property Gls::Context::TGLVBOArrayBufferHandle* ArrayBufferHandle = {read=FVBO};
	__property Gls::Context::TGLVBOElementArrayHandle* ElementArrayHandle = {read=FEBO};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TCUDACustomFeedBackMesh(Gls::Scene::TGLBaseSceneObject* aParentOwner) : Gls::Scene::TGLBaseSceneObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TCUDAFeedbackMesh : public TCUDACustomFeedBackMesh
{
	typedef TCUDACustomFeedBackMesh inherited;
	
__published:
	__property Attributes;
	__property Shader;
	__property PrimitiveType = {default=0};
	__property VertexNumber = {default=1};
	__property ElementNumber = {default=0};
	__property CommonKernelFunction;
	__property Launching = {default=0};
	__property Blend = {default=0};
	__property ObjectsSorting = {default=0};
	__property VisibilityCulling = {default=0};
	__property Direction;
	__property PitchAngle = {default=0};
	__property Position;
	__property RollAngle = {default=0};
	__property Scale;
	__property ShowAxes = {default=0};
	__property TurnAngle = {default=0};
	__property Up;
	__property Visible = {default=1};
	__property Pickable = {default=1};
	__property OnProgress;
	__property OnPicked;
	__property Behaviours;
	__property Effects;
public:
	/* TCUDACustomFeedBackMesh.Create */ inline __fastcall virtual TCUDAFeedbackMesh(System::Classes::TComponent* AOwner) : TCUDACustomFeedBackMesh(AOwner) { }
	/* TCUDACustomFeedBackMesh.Destroy */ inline __fastcall virtual ~TCUDAFeedbackMesh() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TCUDAFeedbackMesh(Gls::Scene::TGLBaseSceneObject* aParentOwner) : TCUDACustomFeedBackMesh(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TCUDAImageResource : public Cuda::Apicomps::TCUDAGraphicResource
{
	typedef Cuda::Apicomps::TCUDAGraphicResource inherited;
	
private:
	Gls::Material::TGLMaterialLibrary* fMaterialLibrary;
	System::UnicodeString fTextureName;
	void __fastcall SetMaterialLibrary(Gls::Material::TGLMaterialLibrary* const Value);
	void __fastcall SetTextureName(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall AllocateHandles();
	virtual void __fastcall DestroyHandles();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TCUDAImageResource(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCUDAImageResource();
	virtual void __fastcall MapResources();
	virtual void __fastcall UnMapResources();
	virtual void __fastcall BindArrayToTexture(Cuda::Apicomps::TCUDAMemData* &cudaArray, unsigned ALeyer, unsigned ALevel);
	
__published:
	__property System::UnicodeString TextureName = {read=fTextureName, write=SetTextureName};
	__property Gls::Material::TGLMaterialLibrary* MaterialLibrary = {read=fMaterialLibrary, write=SetMaterialLibrary};
	__property Mapping = {default=0};
};


class PASCALIMPLEMENTATION TCUDAGeometryResource : public Cuda::Apicomps::TCUDAGraphicResource
{
	typedef Cuda::Apicomps::TCUDAGraphicResource inherited;
	
private:
	TCUDACustomFeedBackMesh* FFeedBackMesh;
	void __fastcall SetFeedBackMesh(TCUDACustomFeedBackMesh* const Value);
	unsigned __fastcall GetAttribArraySize(TGLVertexAttribute* AAttr);
	
protected:
	virtual void __fastcall AllocateHandles();
	virtual void __fastcall DestroyHandles();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual unsigned __fastcall GetAttributeArraySize(const System::UnicodeString AName);
	virtual void * __fastcall GetAttributeArrayAddress(const System::UnicodeString AName);
	virtual unsigned __fastcall GetElementArrayDataSize();
	virtual void * __fastcall GetElementArrayAddress();
	
public:
	__fastcall virtual TCUDAGeometryResource(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCUDAGeometryResource();
	virtual void __fastcall MapResources();
	virtual void __fastcall UnMapResources();
	__property unsigned AttributeDataSize[const System::UnicodeString AttribName] = {read=GetAttributeArraySize};
	__property void * AttributeDataAddress[const System::UnicodeString AttribName] = {read=GetAttributeArrayAddress};
	__property unsigned IndexDataSize = {read=GetElementArrayDataSize, nodefault};
	__property void * IndexDataAddress = {read=GetElementArrayAddress};
	
__published:
	__property TCUDACustomFeedBackMesh* FeedBackMesh = {read=FFeedBackMesh, write=SetFeedBackMesh};
	__property Mapping = {default=0};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Graphics */
}	/* namespace Cuda */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA_GRAPHICS)
using namespace Cuda::Graphics;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CUDA)
using namespace Cuda;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Cuda_GraphicsHPP
