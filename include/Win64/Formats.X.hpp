// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Formats.X.pas' rev: 35.00 (Windows)

#ifndef Formats_XHPP
#define Formats_XHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.Utils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Formats
{
namespace X
{
//-- forward type declarations -----------------------------------------------
struct TDXFileHeader;
class DELPHICLASS TDXNode;
class DELPHICLASS TDXMaterial;
class DELPHICLASS TDXMaterialList;
class DELPHICLASS TDXFrame;
class DELPHICLASS TDXMesh;
class DELPHICLASS TDXFile;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TDXFileHeader
{
public:
	System::StaticArray<char, 4> Magic;
	System::StaticArray<char, 2> Major;
	System::StaticArray<char, 2> Minor;
	System::StaticArray<char, 4> FileType;
	System::StaticArray<char, 4> FloatType;
};


class PASCALIMPLEMENTATION TDXNode : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
private:
	System::UnicodeString FName;
	System::UnicodeString FTypeName;
	TDXNode* FOwner;
	TDXNode* __fastcall GetItem(int index);
	
public:
	__fastcall TDXNode(TDXNode* AOwner);
	__fastcall virtual TDXNode();
	virtual void __fastcall Clear();
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property System::UnicodeString TypeName = {read=FTypeName, write=FTypeName};
	__property TDXNode* Owner = {read=FOwner};
	__property TDXNode* Items[int index] = {read=GetItem};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TDXNode() { }
	
};


class PASCALIMPLEMENTATION TDXMaterial : public Gls::Persistentclasses::TGLPersistentObject
{
	typedef Gls::Persistentclasses::TGLPersistentObject inherited;
	
private:
	Gls::Vectortypes::TVector4f FDiffuse;
	float FSpecPower;
	Gls::Vectortypes::TVector3f FSpecular;
	Gls::Vectortypes::TVector3f FEmissive;
	System::UnicodeString FTexture;
	
public:
	__fastcall TDXMaterial(TDXMaterialList* AOwner);
	__property Gls::Vectortypes::TVector4f Diffuse = {read=FDiffuse, write=FDiffuse};
	__property float SpecPower = {read=FSpecPower, write=FSpecPower};
	__property Gls::Vectortypes::TVector3f Specular = {read=FSpecular, write=FSpecular};
	__property Gls::Vectortypes::TVector3f Emissive = {read=FEmissive, write=FEmissive};
	__property System::UnicodeString Texture = {read=FTexture, write=FTexture};
public:
	/* TGLPersistentObject.Create */ inline __fastcall virtual TDXMaterial() : Gls::Persistentclasses::TGLPersistentObject() { }
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TDXMaterial(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Persistentclasses::TGLPersistentObject(reader) { }
	/* TGLPersistentObject.Destroy */ inline __fastcall virtual ~TDXMaterial() { }
	
};


class PASCALIMPLEMENTATION TDXMaterialList : public TDXNode
{
	typedef TDXNode inherited;
	
private:
	TDXMaterial* __fastcall GetMaterial(int index);
	
public:
	__property TDXMaterial* Items[int index] = {read=GetMaterial};
public:
	/* TDXNode.CreateOwned */ inline __fastcall TDXMaterialList(TDXNode* AOwner) : TDXNode(AOwner) { }
	/* TDXNode.Create */ inline __fastcall virtual TDXMaterialList() : TDXNode() { }
	
public:
	/* TList.Destroy */ inline __fastcall virtual ~TDXMaterialList() { }
	
};


class PASCALIMPLEMENTATION TDXFrame : public TDXNode
{
	typedef TDXNode inherited;
	
private:
	Gls::Vectortypes::TMatrix4f FMatrix;
	
public:
	__fastcall virtual TDXFrame();
	Gls::Vectortypes::TMatrix4f __fastcall GlobalMatrix();
	__property Gls::Vectortypes::TMatrix4f Matrix = {read=FMatrix, write=FMatrix};
public:
	/* TDXNode.CreateOwned */ inline __fastcall TDXFrame(TDXNode* AOwner) : TDXNode(AOwner) { }
	
public:
	/* TList.Destroy */ inline __fastcall virtual ~TDXFrame() { }
	
};


class PASCALIMPLEMENTATION TDXMesh : public TDXNode
{
	typedef TDXNode inherited;
	
private:
	Gls::Vectorlists::TGLAffineVectorList* FVertices;
	Gls::Vectorlists::TGLAffineVectorList* FNormals;
	Gls::Vectorlists::TGLAffineVectorList* FTexCoords;
	Gls::Vectorlists::TGLIntegerList* FVertexIndices;
	Gls::Vectorlists::TGLIntegerList* FNormalIndices;
	Gls::Vectorlists::TGLIntegerList* FMaterialIndices;
	Gls::Vectorlists::TGLIntegerList* FVertCountIndices;
	TDXMaterialList* FMaterialList;
	
public:
	__fastcall virtual TDXMesh();
	__fastcall virtual ~TDXMesh();
	__property Gls::Vectorlists::TGLAffineVectorList* Vertices = {read=FVertices};
	__property Gls::Vectorlists::TGLAffineVectorList* Normals = {read=FNormals};
	__property Gls::Vectorlists::TGLAffineVectorList* TexCoords = {read=FTexCoords};
	__property Gls::Vectorlists::TGLIntegerList* VertexIndices = {read=FVertexIndices};
	__property Gls::Vectorlists::TGLIntegerList* NormalIndices = {read=FNormalIndices};
	__property Gls::Vectorlists::TGLIntegerList* MaterialIndices = {read=FMaterialIndices};
	__property Gls::Vectorlists::TGLIntegerList* VertCountIndices = {read=FVertCountIndices};
	__property TDXMaterialList* MaterialList = {read=FMaterialList};
public:
	/* TDXNode.CreateOwned */ inline __fastcall TDXMesh(TDXNode* AOwner) : TDXNode(AOwner) { }
	
};


class PASCALIMPLEMENTATION TDXFile : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TDXNode* FRootNode;
	TDXFileHeader FHeader;
	
protected:
	void __fastcall ParseText(System::Classes::TStream* Stream);
	void __fastcall ParseBinary(System::Classes::TStream* Stream);
	
public:
	__fastcall TDXFile();
	__fastcall virtual ~TDXFile();
	void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	__property TDXFileHeader Header = {read=FHeader};
	__property TDXNode* RootNode = {read=FRootNode};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace X */
}	/* namespace Formats */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FORMATS_X)
using namespace Formats::X;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FORMATS)
using namespace Formats;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Formats_XHPP
