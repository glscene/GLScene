// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Formats.VRML.pas' rev: 35.00 (Windows)

#ifndef Formats_VrmlHPP
#define Formats_VrmlHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.Utils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Formats
{
namespace Vrml
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TVRMLNode;
class DELPHICLASS TVRMLSingleArray;
class DELPHICLASS TVRMLIntegerArray;
class DELPHICLASS TVRMLMaterial;
class DELPHICLASS TVRMLUse;
class DELPHICLASS TVRMLShapeHints;
class DELPHICLASS TVRMLTransform;
class DELPHICLASS TVRMLParser;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVRMLNode : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TVRMLNode* operator[](int index) { return this->Nodes[index]; }
	
private:
	System::Classes::TList* FNodes;
	TVRMLNode* FParent;
	System::UnicodeString FName;
	System::UnicodeString FDefName;
	TVRMLNode* __fastcall GetNode(int index);
	
public:
	__fastcall virtual TVRMLNode();
	__fastcall TVRMLNode(TVRMLNode* AParent);
	__fastcall virtual ~TVRMLNode();
	int __fastcall Count();
	void __fastcall Clear();
	void __fastcall Add(TVRMLNode* node);
	void __fastcall Remove(TVRMLNode* node);
	void __fastcall Delete(int index);
	__property TVRMLNode* Nodes[int index] = {read=GetNode/*, default*/};
	__property TVRMLNode* Parent = {read=FParent};
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property System::UnicodeString DefName = {read=FDefName, write=FDefName};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVRMLSingleArray : public TVRMLNode
{
	typedef TVRMLNode inherited;
	
private:
	Gls::Vectorlists::TGLSingleList* FValues;
	
public:
	__fastcall virtual TVRMLSingleArray();
	__fastcall virtual ~TVRMLSingleArray();
	__property Gls::Vectorlists::TGLSingleList* Values = {read=FValues};
public:
	/* TVRMLNode.CreateOwned */ inline __fastcall TVRMLSingleArray(TVRMLNode* AParent) : TVRMLNode(AParent) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVRMLIntegerArray : public TVRMLNode
{
	typedef TVRMLNode inherited;
	
private:
	Gls::Vectorlists::TGLIntegerList* FValues;
	
public:
	__fastcall virtual TVRMLIntegerArray();
	__fastcall virtual ~TVRMLIntegerArray();
	__property Gls::Vectorlists::TGLIntegerList* Values = {read=FValues};
public:
	/* TVRMLNode.CreateOwned */ inline __fastcall TVRMLIntegerArray(TVRMLNode* AParent) : TVRMLNode(AParent) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVRMLMaterial : public TVRMLNode
{
	typedef TVRMLNode inherited;
	
private:
	Gls::Vectortypes::TVector3f FDiffuseColor;
	Gls::Vectortypes::TVector3f FAmbientColor;
	Gls::Vectortypes::TVector3f FSpecularColor;
	Gls::Vectortypes::TVector3f FEmissiveColor;
	float FTransparency;
	float FShininess;
	bool FHasDiffuse;
	bool FHasAmbient;
	bool FHasSpecular;
	bool FHasEmissive;
	bool FHasTransparency;
	bool FHasShininess;
	
public:
	__fastcall virtual TVRMLMaterial();
	__property Gls::Vectortypes::TVector3f DiffuseColor = {read=FDiffuseColor, write=FDiffuseColor};
	__property Gls::Vectortypes::TVector3f AmbientColor = {read=FAmbientColor, write=FAmbientColor};
	__property Gls::Vectortypes::TVector3f SpecularColor = {read=FSpecularColor, write=FSpecularColor};
	__property Gls::Vectortypes::TVector3f EmissiveColor = {read=FEmissiveColor, write=FEmissiveColor};
	__property float Transparency = {read=FTransparency, write=FTransparency};
	__property float Shininess = {read=FShininess, write=FShininess};
	__property bool HasDiffuse = {read=FHasDiffuse, write=FHasDiffuse, nodefault};
	__property bool HasAmbient = {read=FHasAmbient, write=FHasAmbient, nodefault};
	__property bool HasSpecular = {read=FHasSpecular, write=FHasSpecular, nodefault};
	__property bool HasEmissive = {read=FHasEmissive, write=FHasEmissive, nodefault};
	__property bool HasTransparency = {read=FHasTransparency, write=FHasTransparency, nodefault};
	__property bool HasShininess = {read=FHasShininess, write=FHasShininess, nodefault};
public:
	/* TVRMLNode.CreateOwned */ inline __fastcall TVRMLMaterial(TVRMLNode* AParent) : TVRMLNode(AParent) { }
	/* TVRMLNode.Destroy */ inline __fastcall virtual ~TVRMLMaterial() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVRMLUse : public TVRMLNode
{
	typedef TVRMLNode inherited;
	
private:
	System::UnicodeString FValue;
	
public:
	__property System::UnicodeString Value = {read=FValue, write=FValue};
public:
	/* TVRMLNode.Create */ inline __fastcall virtual TVRMLUse() : TVRMLNode() { }
	/* TVRMLNode.CreateOwned */ inline __fastcall TVRMLUse(TVRMLNode* AParent) : TVRMLNode(AParent) { }
	/* TVRMLNode.Destroy */ inline __fastcall virtual ~TVRMLUse() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVRMLShapeHints : public TVRMLNode
{
	typedef TVRMLNode inherited;
	
private:
	float FCreaseAngle;
	
public:
	__property float CreaseAngle = {read=FCreaseAngle, write=FCreaseAngle};
public:
	/* TVRMLNode.Create */ inline __fastcall virtual TVRMLShapeHints() : TVRMLNode() { }
	/* TVRMLNode.CreateOwned */ inline __fastcall TVRMLShapeHints(TVRMLNode* AParent) : TVRMLNode(AParent) { }
	/* TVRMLNode.Destroy */ inline __fastcall virtual ~TVRMLShapeHints() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVRMLTransform : public TVRMLNode
{
	typedef TVRMLNode inherited;
	
private:
	Gls::Vectortypes::TVector3f FCenter;
	Gls::Vectortypes::TVector4f FRotation;
	Gls::Vectortypes::TVector3f FScaleFactor;
	
public:
	__fastcall virtual TVRMLTransform();
	__property Gls::Vectortypes::TVector3f Center = {read=FCenter, write=FCenter};
	__property Gls::Vectortypes::TVector4f Rotation = {read=FRotation, write=FRotation};
	__property Gls::Vectortypes::TVector3f ScaleFactor = {read=FScaleFactor, write=FScaleFactor};
public:
	/* TVRMLNode.CreateOwned */ inline __fastcall TVRMLTransform(TVRMLNode* AParent) : TVRMLNode(AParent) { }
	/* TVRMLNode.Destroy */ inline __fastcall virtual ~TVRMLTransform() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVRMLParser : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FCursor;
	System::Classes::TStringList* FTokens;
	TVRMLNode* FRootNode;
	TVRMLNode* FCurrentNode;
	bool FAllowUnknownNodes;
	System::Classes::TList* FDefines;
	
protected:
	System::UnicodeString __fastcall ReadToken();
	float __fastcall ReadSingle();
	Gls::Vectortypes::TVector3f __fastcall ReadVector3f();
	Gls::Vectortypes::TVector4f __fastcall ReadVector4f();
	void __fastcall ReadUnknownArray(System::UnicodeString DefName = System::UnicodeString());
	void __fastcall ReadUnknownHeirachy(System::UnicodeString DefName = System::UnicodeString());
	void __fastcall ReadUnknown(System::UnicodeString unknown_token, System::UnicodeString DefName = System::UnicodeString());
	void __fastcall ReadPointArray(System::UnicodeString DefName = System::UnicodeString());
	void __fastcall ReadCoordIndexArray(System::UnicodeString DefName = System::UnicodeString());
	void __fastcall ReadNormalIndexArray(System::UnicodeString DefName = System::UnicodeString());
	void __fastcall ReadTextureCoordIndexArray(System::UnicodeString DefName = System::UnicodeString());
	void __fastcall ReadCoordinate3(System::UnicodeString DefName = System::UnicodeString());
	void __fastcall ReadNormal(System::UnicodeString DefName = System::UnicodeString());
	void __fastcall ReadTextureCoordinate2(System::UnicodeString DefName = System::UnicodeString());
	void __fastcall ReadMaterial(System::UnicodeString DefName = System::UnicodeString());
	void __fastcall ReadIndexedFaceSet(System::UnicodeString DefName = System::UnicodeString());
	void __fastcall ReadTransform(System::UnicodeString DefName = System::UnicodeString());
	void __fastcall ReadShapeHints(System::UnicodeString DefName = System::UnicodeString());
	void __fastcall ReadSeparator(System::UnicodeString DefName = System::UnicodeString());
	void __fastcall ReadGroup(System::UnicodeString DefName = System::UnicodeString());
	void __fastcall ReadDef();
	void __fastcall ReadUse();
	
public:
	__fastcall TVRMLParser();
	__fastcall virtual ~TVRMLParser();
	void __fastcall Parse(System::UnicodeString Text);
	__property TVRMLNode* RootNode = {read=FRootNode};
	__property bool AllowUnknownNodes = {read=FAllowUnknownNodes, write=FAllowUnknownNodes, nodefault};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Vrml */
}	/* namespace Formats */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FORMATS_VRML)
using namespace Formats::Vrml;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FORMATS)
using namespace Formats;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Formats_VrmlHPP
