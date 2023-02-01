// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.File3DS.pas' rev: 35.00 (Windows)

#ifndef Gls_File3dsHPP
#define Gls_File3dsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.Strings.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Objects.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.Texture.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.Context.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.File3DSSceneObjects.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.Material.hpp>
#include <Formats.m3DS.hpp>
#include <Formats.m3DSTypes.hpp>
#include <GLS.BaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace File3ds
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EGLFile3DS;
struct TGLFile3DSAnimationData;
class DELPHICLASS TGLFile3DSAnimationKeys;
class DELPHICLASS TGLFile3DSScaleAnimationKeys;
class DELPHICLASS TGLFile3DSRotationAnimationKeys;
class DELPHICLASS TGLFile3DSPositionAnimationKeys;
class DELPHICLASS TGLFile3DSColorAnimationKeys;
class DELPHICLASS TTGLFile3DSPositionAnimationKeys;
class DELPHICLASS TGLFile3DSSpotLightCutOffAnimationKeys;
class DELPHICLASS TGLFile3DSLightHotSpotAnimationKeys;
class DELPHICLASS TGLFile3DSRollAnimationKeys;
class DELPHICLASS TGLFile3DSAnimationKeyList;
class DELPHICLASS TGLFile3DSDummyObject;
class DELPHICLASS TGLFile3DSMeshObject;
class DELPHICLASS TGLFile3DSOmniLightObject;
class DELPHICLASS TGLFile3DSSpotLightObject;
class DELPHICLASS TGLFile3DSCameraObject;
class DELPHICLASS TGL3DSVectorFile;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLFile3DS : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLFile3DS(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLFile3DS(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLFile3DS(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLFile3DS(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLFile3DS(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLFile3DS(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLFile3DS(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLFile3DS(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLFile3DS(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLFile3DS(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLFile3DS(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLFile3DS(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLFile3DS() { }
	
};

#pragma pack(pop)

#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLFile3DSAnimationData
{
public:
	Gls::Vectortypes::TMatrix4f ModelMatrix;
	Gls::Vectortypes::TVector4f Color;
	Gls::Vectortypes::TVector3f TargetPos;
	float SpotLightCutOff;
	float HotSpot;
	float Roll;
};
#pragma pack(pop)


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSAnimationKeys : public Gls::Persistentclasses::TGLPersistentObject
{
	typedef Gls::Persistentclasses::TGLPersistentObject inherited;
	
	
private:
	typedef System::DynamicArray<Formats::M3dstypes::TKeyHeader3DS> _TGLFile3DSAnimationKeys__1;
	
	
private:
	int FNumKeys;
	_TGLFile3DSAnimationKeys__1 FKeys;
	void __fastcall InterpolateFrame(int &I, double &w, const double AFrame);
	
protected:
	float __fastcall InterpolateValue(const float *AValues, const int AValues_High, const double AFrame)/* overload */;
	Gls::Vectortypes::TVector3f __fastcall InterpolateValue(const Gls::Vectortypes::TVector3f *AValues, const int AValues_High, const double AFrame)/* overload */;
	Gls::Vectortypes::TMatrix4f __fastcall InterpolateValue(const Formats::M3dstypes::TKFRotKey3DS *AValues, const int AValues_High, const double AFrame)/* overload */;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Formats::M3dstypes::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame) = 0 ;
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* Reader);
public:
	/* TGLPersistentObject.Create */ inline __fastcall virtual TGLFile3DSAnimationKeys() : Gls::Persistentclasses::TGLPersistentObject() { }
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSAnimationKeys(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Persistentclasses::TGLPersistentObject(reader) { }
	/* TGLPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSAnimationKeys() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSScaleAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
	
private:
	typedef System::DynamicArray<Gls::Vectortypes::TVector3f> _TGLFile3DSScaleAnimationKeys__1;
	
	
private:
	_TGLFile3DSScaleAnimationKeys__1 FScale;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Formats::M3dstypes::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* Reader);
public:
	/* TGLPersistentObject.Create */ inline __fastcall virtual TGLFile3DSScaleAnimationKeys() : TGLFile3DSAnimationKeys() { }
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSScaleAnimationKeys(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TGLPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSScaleAnimationKeys() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSRotationAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
	
private:
	typedef System::DynamicArray<Formats::M3dstypes::TKFRotKey3DS> _TGLFile3DSRotationAnimationKeys__1;
	
	
private:
	_TGLFile3DSRotationAnimationKeys__1 FRot;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Formats::M3dstypes::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* Reader);
public:
	/* TGLPersistentObject.Create */ inline __fastcall virtual TGLFile3DSRotationAnimationKeys() : TGLFile3DSAnimationKeys() { }
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSRotationAnimationKeys(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TGLPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSRotationAnimationKeys() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSPositionAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
	
private:
	typedef System::DynamicArray<Gls::Vectortypes::TVector3f> _TGLFile3DSPositionAnimationKeys__1;
	
	
private:
	_TGLFile3DSPositionAnimationKeys__1 FPos;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Formats::M3dstypes::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* Reader);
public:
	/* TGLPersistentObject.Create */ inline __fastcall virtual TGLFile3DSPositionAnimationKeys() : TGLFile3DSAnimationKeys() { }
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSPositionAnimationKeys(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TGLPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSPositionAnimationKeys() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSColorAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
	
private:
	typedef System::DynamicArray<Gls::Vectortypes::TVector3f> _TGLFile3DSColorAnimationKeys__1;
	
	
private:
	_TGLFile3DSColorAnimationKeys__1 FCol;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Formats::M3dstypes::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* Reader);
public:
	/* TGLPersistentObject.Create */ inline __fastcall virtual TGLFile3DSColorAnimationKeys() : TGLFile3DSAnimationKeys() { }
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSColorAnimationKeys(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TGLPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSColorAnimationKeys() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTGLFile3DSPositionAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
	
private:
	typedef System::DynamicArray<Gls::Vectortypes::TVector3f> _TTGLFile3DSPositionAnimationKeys__1;
	
	
private:
	_TTGLFile3DSPositionAnimationKeys__1 FTPos;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Formats::M3dstypes::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* Reader);
public:
	/* TGLPersistentObject.Create */ inline __fastcall virtual TTGLFile3DSPositionAnimationKeys() : TGLFile3DSAnimationKeys() { }
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TTGLFile3DSPositionAnimationKeys(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TGLPersistentObject.Destroy */ inline __fastcall virtual ~TTGLFile3DSPositionAnimationKeys() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSSpotLightCutOffAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
	
private:
	typedef System::DynamicArray<float> _TGLFile3DSSpotLightCutOffAnimationKeys__1;
	
	
private:
	_TGLFile3DSSpotLightCutOffAnimationKeys__1 FFall;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Formats::M3dstypes::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* Reader);
public:
	/* TGLPersistentObject.Create */ inline __fastcall virtual TGLFile3DSSpotLightCutOffAnimationKeys() : TGLFile3DSAnimationKeys() { }
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSSpotLightCutOffAnimationKeys(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TGLPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSSpotLightCutOffAnimationKeys() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSLightHotSpotAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
	
private:
	typedef System::DynamicArray<float> _TGLFile3DSLightHotSpotAnimationKeys__1;
	
	
private:
	_TGLFile3DSLightHotSpotAnimationKeys__1 FHot;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Formats::M3dstypes::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* Reader);
public:
	/* TGLPersistentObject.Create */ inline __fastcall virtual TGLFile3DSLightHotSpotAnimationKeys() : TGLFile3DSAnimationKeys() { }
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSLightHotSpotAnimationKeys(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TGLPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSLightHotSpotAnimationKeys() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSRollAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
	
private:
	typedef System::DynamicArray<float> _TGLFile3DSRollAnimationKeys__1;
	
	
private:
	_TGLFile3DSRollAnimationKeys__1 FRoll;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Formats::M3dstypes::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* Reader);
public:
	/* TGLPersistentObject.Create */ inline __fastcall virtual TGLFile3DSRollAnimationKeys() : TGLFile3DSAnimationKeys() { }
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSRollAnimationKeys(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TGLPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSRollAnimationKeys() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSAnimationKeyList : public Gls::Persistentclasses::TGLPersistentObject
{
	typedef Gls::Persistentclasses::TGLPersistentObject inherited;
	
	
private:
	typedef System::DynamicArray<TGLFile3DSAnimationKeys*> _TGLFile3DSAnimationKeyList__1;
	
	
private:
	_TGLFile3DSAnimationKeyList__1 FAnimKeysList;
	
protected:
	void __fastcall ApplyAnimKeys(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	
public:
	void __fastcall AddKeys(TGLFile3DSAnimationKeys* const AItem);
	void __fastcall ClearKeys();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* Reader);
	__fastcall virtual ~TGLFile3DSAnimationKeyList();
public:
	/* TGLPersistentObject.Create */ inline __fastcall virtual TGLFile3DSAnimationKeyList() : Gls::Persistentclasses::TGLPersistentObject() { }
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSAnimationKeyList(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Persistentclasses::TGLPersistentObject(reader) { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TGLFile3DSAnimKeysClassType : unsigned char { ctScale, ctRot, ctPos, ctCol, ctTPos, ctFall, ctHot, ctRoll };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSDummyObject : public Gls::Vectorfileobjects::TGLMorphableMeshObject
{
	typedef Gls::Vectorfileobjects::TGLMorphableMeshObject inherited;
	
private:
	TGLFile3DSAnimationKeyList* FAnimList;
	void *FAnimData;
	TGLFile3DSAnimationData FRefTranf;
	TGLFile3DSAnimationData FAnimTransf;
	TGLFile3DSDummyObject* FParent;
	Formats::M3dstypes::String64 FParentName;
	bool FStatic;
	
public:
	virtual void __fastcall LoadAnimation(const void * AData);
	virtual void __fastcall SetFrame(const double AFrame);
	virtual void __fastcall MorphTo(int morphTargetIndex);
	virtual void __fastcall Lerp(int morphTargetIndex1, int morphTargetIndex2, float lerpFactor);
	virtual void __fastcall GetExtents(/* out */ Gls::Vectortypes::TVector3f &min, /* out */ Gls::Vectortypes::TVector3f &max)/* overload */;
	virtual Gls::Vectorlists::TGLAffineVectorList* __fastcall ExtractTriangles(Gls::Vectorlists::TGLAffineVectorList* texCoords = (Gls::Vectorlists::TGLAffineVectorList*)(0x0), Gls::Vectorlists::TGLAffineVectorList* normals = (Gls::Vectorlists::TGLAffineVectorList*)(0x0));
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* Reader);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual TGLFile3DSDummyObject();
	__fastcall virtual ~TGLFile3DSDummyObject();
	__property TGLFile3DSAnimationKeyList* AnimList = {read=FAnimList};
	__property TGLFile3DSDummyObject* Parent = {read=FParent, write=FParent};
	__property TGLFile3DSAnimationData RefrenceTransf = {read=FRefTranf, write=FRefTranf};
public:
	/* TGLMeshObject.CreateOwned */ inline __fastcall TGLFile3DSDummyObject(Gls::Vectorfileobjects::TGLMeshObjectList* AOwner) : Gls::Vectorfileobjects::TGLMorphableMeshObject(AOwner) { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSDummyObject(Gls::Persistentclasses::TGLVirtualReader* reader) : Gls::Vectorfileobjects::TGLMorphableMeshObject(reader) { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  GetExtents(/* out */ Gls::Geometrybb::TAABB &aabb){ Gls::Vectorfileobjects::TGLMeshObject::GetExtents(aabb); }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSMeshObject : public TGLFile3DSDummyObject
{
	typedef TGLFile3DSDummyObject inherited;
	
public:
	virtual void __fastcall LoadAnimation(const void * AData);
	virtual void __fastcall BuildList(Gls::Rendercontextinfo::TGLRenderContextInfo &ARci);
public:
	/* TGLFile3DSDummyObject.Create */ inline __fastcall virtual TGLFile3DSMeshObject() : TGLFile3DSDummyObject() { }
	/* TGLFile3DSDummyObject.Destroy */ inline __fastcall virtual ~TGLFile3DSMeshObject() { }
	
public:
	/* TGLMeshObject.CreateOwned */ inline __fastcall TGLFile3DSMeshObject(Gls::Vectorfileobjects::TGLMeshObjectList* AOwner) : TGLFile3DSDummyObject(AOwner) { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSMeshObject(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLFile3DSDummyObject(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSOmniLightObject : public TGLFile3DSDummyObject
{
	typedef TGLFile3DSDummyObject inherited;
	
private:
	Gls::File3dssceneobjects::TGLFile3DSLight* FLightSrc;
	Formats::M3dstypes::String64 FLightSrcName;
	
public:
	__fastcall virtual TGLFile3DSOmniLightObject();
	virtual void __fastcall LoadData(Gls::Vectorfileobjects::TGLBaseMesh* const AOwner, const Formats::M3dstypes::PLight3DS AData);
	virtual void __fastcall LoadAnimation(const void * AData);
	virtual void __fastcall SetFrame(const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* Reader);
	__fastcall virtual ~TGLFile3DSOmniLightObject();
public:
	/* TGLMeshObject.CreateOwned */ inline __fastcall TGLFile3DSOmniLightObject(Gls::Vectorfileobjects::TGLMeshObjectList* AOwner) : TGLFile3DSDummyObject(AOwner) { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSOmniLightObject(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLFile3DSDummyObject(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSSpotLightObject : public TGLFile3DSOmniLightObject
{
	typedef TGLFile3DSOmniLightObject inherited;
	
public:
	virtual void __fastcall LoadData(Gls::Vectorfileobjects::TGLBaseMesh* const AOwner, const Formats::M3dstypes::PLight3DS AData);
	virtual void __fastcall LoadAnimation(const void * AData);
	virtual void __fastcall SetFrame(const double AFrame);
public:
	/* TGLFile3DSOmniLightObject.Create */ inline __fastcall virtual TGLFile3DSSpotLightObject() : TGLFile3DSOmniLightObject() { }
	/* TGLFile3DSOmniLightObject.Destroy */ inline __fastcall virtual ~TGLFile3DSSpotLightObject() { }
	
public:
	/* TGLMeshObject.CreateOwned */ inline __fastcall TGLFile3DSSpotLightObject(Gls::Vectorfileobjects::TGLMeshObjectList* AOwner) : TGLFile3DSOmniLightObject(AOwner) { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSSpotLightObject(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLFile3DSOmniLightObject(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFile3DSCameraObject : public TGLFile3DSDummyObject
{
	typedef TGLFile3DSDummyObject inherited;
	
private:
	Gls::Objects::TGLDummyCube* FTargetObj;
	Gls::File3dssceneobjects::TGLFile3DSCamera* FCameraSrc;
	Formats::M3dstypes::String64 FCameraSrcName;
	
public:
	__fastcall virtual TGLFile3DSCameraObject();
	void __fastcall LoadData(Gls::Vectorfileobjects::TGLBaseMesh* Owner, Formats::M3dstypes::PCamera3DS AData);
	virtual void __fastcall LoadAnimation(const void * AData);
	virtual void __fastcall SetFrame(const double AFrame);
	DYNAMIC void __fastcall WriteToFiler(Gls::Persistentclasses::TGLVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Gls::Persistentclasses::TGLVirtualReader* Reader);
	__fastcall virtual ~TGLFile3DSCameraObject();
public:
	/* TGLMeshObject.CreateOwned */ inline __fastcall TGLFile3DSCameraObject(Gls::Vectorfileobjects::TGLMeshObjectList* AOwner) : TGLFile3DSDummyObject(AOwner) { }
	
public:
	/* TGLPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSCameraObject(Gls::Persistentclasses::TGLVirtualReader* reader) : TGLFile3DSDummyObject(reader) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGL3DSVectorFile : public Gls::Vectorfileobjects::TGLVectorFile
{
	typedef Gls::Vectorfileobjects::TGLVectorFile inherited;
	
public:
	__classmethod virtual Gls::Applicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGL3DSVectorFile(System::Classes::TPersistent* AOwner) : Gls::Vectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGL3DSVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool vGLFile3DS_UseTextureEx;
extern DELPHI_PACKAGE bool vGLFile3DS_EnableAnimation;
extern DELPHI_PACKAGE bool vGLFile3DS_FixDefaultUpAxisY;
extern DELPHI_PACKAGE int vGLFile3DS_LoadedStaticFrame;
}	/* namespace File3ds */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_FILE3DS)
using namespace Gls::File3ds;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_File3dsHPP
