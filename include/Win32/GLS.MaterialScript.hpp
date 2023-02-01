// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.MaterialScript.pas' rev: 35.00 (Windows)

#ifndef Gls_MaterialscriptHPP
#define Gls_MaterialscriptHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.StdCtrls.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.Texture.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.Utils.hpp>
#include <GLS.Color.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.Material.hpp>
#include <GLS.State.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Materialscript
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLShaderItem;
class DELPHICLASS TGLShaderItems;
class DELPHICLASS TGLMaterialLibraryItem;
class DELPHICLASS TGLMaterialLibraryItems;
class DELPHICLASS TGLMaterialScripter;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLShaderItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Gls::Material::TGLShader* FShader;
	System::UnicodeString FName;
	void __fastcall SetShader(Gls::Material::TGLShader* const Value);
	void __fastcall SetName(const System::UnicodeString Value);
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	
public:
	__fastcall virtual TGLShaderItem(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLShaderItem();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Gls::Material::TGLShader* Shader = {read=FShader, write=SetShader};
	__property System::UnicodeString Name = {read=FName, write=SetName};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLShaderItems : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLShaderItem* operator[](int Index) { return this->Items[Index]; }
	
private:
	void __fastcall SetItems(int Index, TGLShaderItem* const Val);
	TGLShaderItem* __fastcall GetItems(int Index);
	
public:
	__fastcall TGLShaderItems(System::Classes::TPersistent* AOwner);
	__property TGLShaderItem* Items[int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLShaderItems() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMaterialLibraryItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Gls::Material::TGLMaterialLibrary* FMaterialLibrary;
	System::UnicodeString FName;
	void __fastcall SetMaterialLibrary(Gls::Material::TGLMaterialLibrary* const Value);
	void __fastcall SetName(const System::UnicodeString Value);
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	
public:
	__fastcall virtual TGLMaterialLibraryItem(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLMaterialLibraryItem();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Gls::Material::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property System::UnicodeString Name = {read=FName, write=SetName};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMaterialLibraryItems : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLMaterialLibraryItem* operator[](int Index) { return this->Items[Index]; }
	
private:
	void __fastcall SetItems(int Index, TGLMaterialLibraryItem* const Val);
	TGLMaterialLibraryItem* __fastcall GetItems(int Index);
	
public:
	__fastcall TGLMaterialLibraryItems(System::Classes::TPersistent* AOwner);
	__property TGLMaterialLibraryItem* Items[int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLMaterialLibraryItems() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLMaterialScripter : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TGLShaderItems* FShaderItems;
	TGLMaterialLibraryItems* FMaterialLibraryItems;
	bool FAppend;
	bool FOverwrite;
	System::Classes::TStrings* FScript;
	Vcl::Stdctrls::TMemo* FMemo;
	Gls::Material::TGLMaterialLibrary* FMaterialLibrary;
	int Count;
	int Infini;
	bool Done;
	Gls::Material::TGLLibMaterial* NewMat;
	Gls::Coordinates::TGLCoordinates3* TmpCoords;
	Gls::Color::TGLColor* TmpColor;
	Gls::Coordinates::TGLCoordinates4* TmpCoords4;
	System::UnicodeString TmpStr;
	void __fastcall SeTGLShaderItems(TGLShaderItems* const Value);
	void __fastcall SeTGLMaterialLibraryItems(TGLMaterialLibraryItems* const Value);
	void __fastcall SetAppend(const bool Value);
	void __fastcall SetOverwrite(const bool Value);
	void __fastcall SetScript(System::Classes::TStrings* const Value);
	void __fastcall SetMaterialLibrary(Gls::Material::TGLMaterialLibrary* const Value);
	void __fastcall SetMemo(Vcl::Stdctrls::TMemo* const Value);
	void __fastcall CheckError();
	bool __fastcall ClassExists(System::UnicodeString arguement);
	bool __fastcall CheckRepeatDone();
	System::UnicodeString __fastcall ExtractValue();
	void __fastcall ExtractCoords3();
	void __fastcall ExtractCoords4();
	void __fastcall ExtractColors();
	System::UnicodeString __fastcall DeleteSpaces(System::UnicodeString Value);
	bool __fastcall SubstrExists(System::UnicodeString substr);
	bool __fastcall ValueExists(System::UnicodeString Value);
	void __fastcall ZMaterial();
	void __fastcall XMaterial();
	void __fastcall XName();
	void __fastcall XShader();
	void __fastcall XTexture2Name();
	void __fastcall XTextureOffset();
	void __fastcall XTextureScale();
	void __fastcall XTexture();
	void __fastcall XCompression();
	void __fastcall XEnvColor();
	void __fastcall XFilteringQuality();
	void __fastcall XImageAlpha();
	void __fastcall XImageBrightness();
	void __fastcall XImageClass();
	void __fastcall XImageGamma();
	void __fastcall XMagFilter();
	void __fastcall XMappingMode();
	void __fastcall XMappingSCoordinates();
	void __fastcall XMappingTCoordinates();
	void __fastcall XMinFilter();
	void __fastcall XNormalMapScale();
	void __fastcall XTextureFormat();
	void __fastcall XTextureMode();
	void __fastcall XTextureWrap();
	void __fastcall XBlendingMode();
	void __fastcall XPolygonMode();
	void __fastcall XFacingCulling();
	void __fastcall XLibMaterialName();
	void __fastcall XMaterialOptions();
	void __fastcall XMaterialLibrary();
	void __fastcall XBackProperties();
	void __fastcall XBackAmbient();
	void __fastcall XBackDiffuse();
	void __fastcall XBackEmission();
	void __fastcall XBackShininess();
	void __fastcall XBackSpecular();
	void __fastcall XFrontProperties();
	void __fastcall XFrontAmbient();
	void __fastcall XFrontDiffuse();
	void __fastcall XFrontEmission();
	void __fastcall XFrontShininess();
	void __fastcall XFrontSpecular();
	void __fastcall XPersistantImage();
	void __fastcall XBlankImage();
	void __fastcall XPictureFileName();
	void __fastcall XPicturePX();
	void __fastcall XPictureNX();
	void __fastcall XPicturePY();
	void __fastcall XPictureNY();
	void __fastcall XPicturePZ();
	void __fastcall XPictureNZ();
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__property Vcl::Stdctrls::TMemo* DebugMemo = {read=FMemo, write=SetMemo};
	__fastcall virtual TGLMaterialScripter(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLMaterialScripter();
	void __fastcall CompileScript();
	
__published:
	__property System::Classes::TStrings* Script = {read=FScript, write=SetScript};
	__property Gls::Material::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property TGLShaderItems* Shaders = {read=FShaderItems, write=SeTGLShaderItems};
	__property TGLMaterialLibraryItems* MaterialLibraries = {read=FMaterialLibraryItems, write=SeTGLMaterialLibraryItems};
	__property bool AppendToMaterialLibrary = {read=FAppend, write=SetAppend, nodefault};
	__property bool OverwriteToMaterialLibrary = {read=FOverwrite, write=SetOverwrite, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Materialscript */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_MATERIALSCRIPT)
using namespace Gls::Materialscript;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_MaterialscriptHPP
