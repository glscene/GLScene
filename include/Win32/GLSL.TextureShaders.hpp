// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSL.TextureShaders.pas' rev: 35.00 (Windows)

#ifndef Glsl_TextureshadersHPP
#define Glsl_TextureshadersHPP

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
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Context.hpp>
#include <GLS.Texture.hpp>
#include <GLS.TextureCombiners.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Color.hpp>
#include <GLS.Material.hpp>
#include <GLS.Strings.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.XOpenGL.hpp>
#include <GLS.State.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.Utils.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glsl
{
namespace Textureshaders
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLTextureSharingShaderMaterial;
class DELPHICLASS TGLTextureSharingShaderMaterials;
class DELPHICLASS TGLTextureSharingShader;
class DELPHICLASS TGLTexCombineShader;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTextureSharingShaderMaterial : public Gls::Persistentclasses::TGLInterfacedCollectionItem
{
	typedef Gls::Persistentclasses::TGLInterfacedCollectionItem inherited;
	
private:
	Gls::Vectortypes::TMatrix4f FTextureMatrix;
	bool FNeedToUpdateTextureMatrix;
	bool FTextureMatrixIsUnitary;
	Gls::Material::TGLLibMaterial* FLibMaterial;
	Gls::Coordinates::TGLCoordinates2* FTexOffset;
	Gls::Coordinates::TGLCoordinates2* FTexScale;
	Gls::Material::TGLBlendingMode FBlendingMode;
	Gls::Color::TGLColor* FSpecular;
	Gls::Color::TGLColor* FAmbient;
	Gls::Color::TGLColor* FDiffuse;
	Gls::Color::TGLColor* FEmission;
	Gls::Material::TGLShininess FShininess;
	Gls::Material::TGLMaterialLibrary* FMaterialLibrary;
	System::UnicodeString FLibMaterialName;
	void __fastcall SetAmbient(Gls::Color::TGLColor* const Value);
	void __fastcall SetDiffuse(Gls::Color::TGLColor* const Value);
	void __fastcall SetEmission(Gls::Color::TGLColor* const Value);
	void __fastcall SetShininess(const Gls::Material::TGLShininess Value);
	void __fastcall SetSpecular(Gls::Color::TGLColor* const Value);
	void __fastcall SetMaterialLibrary(Gls::Material::TGLMaterialLibrary* const Value);
	void __fastcall SetLibMaterialName(const System::UnicodeString Value);
	void __fastcall SetBlendingMode(const Gls::Material::TGLBlendingMode Value);
	void __fastcall SetLibMaterial(Gls::Material::TGLLibMaterial* const Value);
	void __fastcall SetTexOffset(Gls::Coordinates::TGLCoordinates2* const Value);
	void __fastcall SetTexScale(Gls::Coordinates::TGLCoordinates2* const Value);
	Gls::Vectortypes::TMatrix4f __fastcall GetTextureMatrix();
	bool __fastcall GetTextureMatrixIsUnitary();
	
protected:
	void __fastcall coordNotifychange(System::TObject* Sender);
	void __fastcall OtherNotifychange(System::TObject* Sender);
	virtual System::UnicodeString __fastcall GetDisplayName();
	TGLTextureSharingShader* __fastcall GetTextureSharingShader();
	virtual Gls::Material::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary();
	
public:
	void __fastcall Apply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall UnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	__fastcall virtual TGLTextureSharingShaderMaterial(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLTextureSharingShaderMaterial();
	__property Gls::Material::TGLLibMaterial* LibMaterial = {read=FLibMaterial, write=SetLibMaterial};
	__property Gls::Vectortypes::TMatrix4f TextureMatrix = {read=GetTextureMatrix};
	__property bool TextureMatrixIsUnitary = {read=GetTextureMatrixIsUnitary, nodefault};
	
__published:
	__property Gls::Coordinates::TGLCoordinates2* TexOffset = {read=FTexOffset, write=SetTexOffset};
	__property Gls::Coordinates::TGLCoordinates2* TexScale = {read=FTexScale, write=SetTexScale};
	__property Gls::Material::TGLBlendingMode BlendingMode = {read=FBlendingMode, write=SetBlendingMode, nodefault};
	__property Gls::Color::TGLColor* Emission = {read=FEmission, write=SetEmission};
	__property Gls::Color::TGLColor* Ambient = {read=FAmbient, write=SetAmbient};
	__property Gls::Color::TGLColor* Diffuse = {read=FDiffuse, write=SetDiffuse};
	__property Gls::Color::TGLColor* Specular = {read=FSpecular, write=SetSpecular};
	__property Gls::Material::TGLShininess Shininess = {read=FShininess, write=SetShininess, nodefault};
	__property Gls::Material::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property System::UnicodeString LibMaterialName = {read=FLibMaterialName, write=SetLibMaterialName};
private:
	void *__IGLMaterialLibrarySupported;	// Gls::Material::IGLMaterialLibrarySupported 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator Gls::Material::_di_IGLMaterialLibrarySupported()
	{
		Gls::Material::_di_IGLMaterialLibrarySupported intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Gls::Material::IGLMaterialLibrarySupported*(void) { return (Gls::Material::IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTextureSharingShaderMaterials : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLTextureSharingShaderMaterial* operator[](const int AIndex) { return this->Items[AIndex]; }
	
protected:
	TGLTextureSharingShaderMaterial* __fastcall GetItems(const int AIndex);
	void __fastcall SetItems(const int AIndex, TGLTextureSharingShaderMaterial* const Value);
	TGLTextureSharingShader* __fastcall GetParent();
	
public:
	HIDESBASE TGLTextureSharingShaderMaterial* __fastcall Add();
	__fastcall TGLTextureSharingShaderMaterials(TGLTextureSharingShader* AOwner);
	__property TGLTextureSharingShaderMaterial* Items[const int AIndex] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLTextureSharingShaderMaterials() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLTextureSharingShader : public Gls::Material::TGLShader
{
	typedef Gls::Material::TGLShader inherited;
	
private:
	TGLTextureSharingShaderMaterials* FMaterials;
	int FCurrentPass;
	void __fastcall SetMaterials(TGLTextureSharingShaderMaterials* const Value);
	
protected:
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLTextureSharingShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTextureSharingShader();
	TGLTextureSharingShaderMaterial* __fastcall AddLibMaterial(Gls::Material::TGLLibMaterial* const ALibMaterial);
	TGLTextureSharingShaderMaterial* __fastcall FindLibMaterial(Gls::Material::TGLLibMaterial* const ALibMaterial);
	
__published:
	__property TGLTextureSharingShaderMaterials* Materials = {read=FMaterials, write=SetMaterials};
};


class PASCALIMPLEMENTATION TGLTexCombineShader : public Gls::Material::TGLShader
{
	typedef Gls::Material::TGLShader inherited;
	
private:
	System::Classes::TStringList* FCombiners;
	Gls::Texturecombiners::TCombinerCache FCommandCache;
	bool FCombinerIsValid;
	bool FDesignTimeEnabled;
	Gls::Material::TGLMaterialLibrary* FMaterialLibrary;
	System::UnicodeString FLibMaterial3Name;
	Gls::Material::TGLLibMaterial* currentLibMaterial3;
	System::UnicodeString FLibMaterial4Name;
	Gls::Material::TGLLibMaterial* currentLibMaterial4;
	bool FApplied3;
	bool FApplied4;
	
protected:
	void __fastcall SetCombiners(System::Classes::TStringList* const val);
	void __fastcall SetDesignTimeEnabled(const bool val);
	void __fastcall SetMaterialLibrary(Gls::Material::TGLMaterialLibrary* const val);
	void __fastcall SetLibMaterial3Name(const System::UnicodeString val);
	void __fastcall SetLibMaterial4Name(const System::UnicodeString val);
	void __fastcall NotifyLibMaterial3Destruction();
	void __fastcall NotifyLibMaterial4Destruction();
	virtual void __fastcall DoInitialize(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall DoApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Gls::Rendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall DoFinalize();
	
public:
	__fastcall virtual TGLTexCombineShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLTexCombineShader();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	
__published:
	__property System::Classes::TStringList* Combiners = {read=FCombiners, write=SetCombiners};
	__property bool DesignTimeEnabled = {read=FDesignTimeEnabled, write=SetDesignTimeEnabled, nodefault};
	__property Gls::Material::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property System::UnicodeString LibMaterial3Name = {read=FLibMaterial3Name, write=SetLibMaterial3Name};
	__property System::UnicodeString LibMaterial4Name = {read=FLibMaterial4Name, write=SetLibMaterial4Name};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Textureshaders */
}	/* namespace Glsl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL_TEXTURESHADERS)
using namespace Glsl::Textureshaders;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSL)
using namespace Glsl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glsl_TextureshadersHPP
