// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Physics.NGDManager.pas' rev: 35.00 (Windows)

#ifndef Physics_NgdmanagerHPP
#define Physics_NgdmanagerHPP

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
#include <System.Types.hpp>
#include <Physics.NGDImport.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.XCollection.hpp>
#include <GLS.GeometryBB.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.Scene.hpp>
#include <GLS.Manager.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.Objects.hpp>
#include <GLS.GeomObjects.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.Color.hpp>

//-- user supplied -----------------------------------------------------------

namespace Physics
{
namespace Ngdmanager
{
//-- forward type declarations -----------------------------------------------
struct TGLNGDHeightField;
class DELPHICLASS TGLNGDBehaviourList;
class DELPHICLASS TGLNGDDebugOption;
class DELPHICLASS TGLNGDManager;
class DELPHICLASS TGLNGDBehaviour;
class DELPHICLASS TGLNGDDynamic;
class DELPHICLASS TGLNGDStatic;
class DELPHICLASS TGLNGDSurfaceItem;
class DELPHICLASS TGLNGDSurfacePair;
class DELPHICLASS TGLNGDJointPivot;
class DELPHICLASS TGLNGDJointPin;
class DELPHICLASS TGLNGDJointPin2;
class DELPHICLASS TGLNGDJointBallAndSocket;
class DELPHICLASS TGLNGDJointHinge;
class DELPHICLASS TGLNGDJointSlider;
class DELPHICLASS TGLNGDJointKinematicController;
class DELPHICLASS TGLNGDJoint;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TGLNGDHeightField
{
	
private:
	typedef System::DynamicArray<System::Word> _TGLNGDHeightField__1;
	
	
public:
	_TGLNGDHeightField__1 heightArray;
	int width;
	int depth;
	bool gridDiagonals;
	float widthDepthScale;
	float heightScale;
};


enum DECLSPEC_DENUM TGLNGDSolverModels : unsigned char { smExact, smLinear1, smLinear2, smLinear3, smLinear4, smLinear5, smLinear6, smLinear7, smLinear8, smLinear9 };

enum DECLSPEC_DENUM TGLNGDFrictionModels : unsigned char { fmExact, fmAdaptive };

enum DECLSPEC_DENUM TGLNGDPickedActions : unsigned char { paAttach, paMove, paDetach };

enum DECLSPEC_DENUM TGLNGDManagerDebug : unsigned char { mdShowGeometry, mdShowAABB, mdShowCenterOfMass, mdShowContact, mdShowJoint, mdShowForce, mdShowAppliedForce, mdShowAppliedVelocity };

typedef System::Set<TGLNGDManagerDebug, TGLNGDManagerDebug::mdShowGeometry, TGLNGDManagerDebug::mdShowAppliedVelocity> TGLNGDManagerDebugs;

enum DECLSPEC_DENUM TGLNGDCollisions : unsigned char { nc_Primitive, nc_Convex, nc_BBox, nc_BSphere, nc_Tree, nc_Mesh, nc_Null, nc_HeightField, nc_NGDFile };

enum DECLSPEC_DENUM TGLNGDJoints : unsigned char { nj_BallAndSocket, nj_Hinge, nj_Slider, nj_Corkscrew, nj_Universal, nj_CustomBallAndSocket, nj_CustomHinge, nj_CustomSlider, nj_UpVector, nj_KinematicController };

class PASCALIMPLEMENTATION TGLNGDBehaviourList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TGLNGDBehaviour* operator[](int index) { return this->ItemsBehav[index]; }
	
protected:
	TGLNGDBehaviour* __fastcall GetBehav(int index);
	void __fastcall PutBehav(int index, TGLNGDBehaviour* Item);
	
public:
	__property TGLNGDBehaviour* ItemsBehav[int index] = {read=GetBehav, write=PutBehav/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TGLNGDBehaviourList() { }
	
public:
	/* TObject.Create */ inline __fastcall TGLNGDBehaviourList() : System::Classes::TList() { }
	
};


typedef void __fastcall (__closure *TCollisionIteratorEvent)(const void * userData, int vertexCount, const Physics::Ngdimport::PdFloat cfaceArray, int faceId);

typedef void __fastcall (__closure *TApplyForceAndTorqueEvent)(const void * cbody, float timestep, int threadIndex);

typedef void __fastcall (__closure *TSetTransformEvent)(const void * cbody, const Physics::Ngdimport::PdFloat cmatrix, int threadIndex);

typedef void __fastcall (__closure *TSerializeEvent)(void * serializeHandle, const void * cbuffer, unsigned size);

typedef void __fastcall (__closure *TDeSerializeEvent)(void * serializeHandle, void * buffer, unsigned size);

typedef bool __fastcall (__closure *TAABBOverlapEvent)(const void * cmaterial, const void * cbody0, const void * cbody1, int threadIndex);

typedef void __fastcall (__closure *TContactProcessEvent)(const void * ccontact, float timestep, int threadIndex);

class PASCALIMPLEMENTATION TGLNGDDebugOption : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TGLNGDManager* FManager;
	Gls::Color::TGLColor* FGeomColorDyn;
	Gls::Color::TGLColor* FGeomColorStat;
	Gls::Color::TGLColor* FAABBColor;
	Gls::Color::TGLColor* FAABBColorSleep;
	Gls::Color::TGLColor* FCenterOfMassColor;
	Gls::Color::TGLColor* FContactColor;
	Gls::Color::TGLColor* FJointAxisColor;
	Gls::Color::TGLColor* FJointPivotColor;
	Gls::Color::TGLColor* FForceColor;
	Gls::Color::TGLColor* FAppliedForceColor;
	Gls::Color::TGLColor* FAppliedVelocityColor;
	Gls::Color::TGLColor* FCustomColor;
	float FDotAxisSize;
	TGLNGDManagerDebugs FManagerDebugs;
	void __fastcall SetManagerDebugs(const TGLNGDManagerDebugs Value);
	void __fastcall SetDotAxisSize(const float Value);
	bool __fastcall StoredDotAxis();
	
public:
	__fastcall TGLNGDDebugOption(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLNGDDebugOption();
	
__published:
	__property Gls::Color::TGLColor* GeomColorDyn = {read=FGeomColorDyn, write=FGeomColorDyn};
	__property Gls::Color::TGLColor* GeomColorStat = {read=FGeomColorStat, write=FGeomColorStat};
	__property Gls::Color::TGLColor* AABBColor = {read=FAABBColor, write=FAABBColor};
	__property Gls::Color::TGLColor* AABBColorSleep = {read=FAABBColorSleep, write=FAABBColorSleep};
	__property Gls::Color::TGLColor* CenterOfMassColor = {read=FCenterOfMassColor, write=FCenterOfMassColor};
	__property Gls::Color::TGLColor* ContactColor = {read=FContactColor, write=FContactColor};
	__property Gls::Color::TGLColor* JointAxisColor = {read=FJointAxisColor, write=FJointAxisColor};
	__property Gls::Color::TGLColor* JointPivotColor = {read=FJointPivotColor, write=FJointPivotColor};
	__property Gls::Color::TGLColor* ForceColor = {read=FForceColor, write=FForceColor};
	__property Gls::Color::TGLColor* AppliedForceColor = {read=FAppliedForceColor, write=FAppliedForceColor};
	__property Gls::Color::TGLColor* AppliedVelocityColor = {read=FAppliedVelocityColor, write=FAppliedVelocityColor};
	__property Gls::Color::TGLColor* CustomColor = {read=FCustomColor, write=FCustomColor};
	__property TGLNGDManagerDebugs NGDManagerDebugs = {read=FManagerDebugs, write=SetManagerDebugs, default=0};
	__property float DotAxisSize = {read=FDotAxisSize, write=SetDotAxisSize, stored=StoredDotAxis};
};


class PASCALIMPLEMENTATION TGLNGDManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FVisible;
	bool FVisibleAtRunTime;
	int FDllVersion;
	TGLNGDSolverModels FSolverModel;
	TGLNGDFrictionModels FFrictionModel;
	int FMinimumFrameRate;
	Gls::Coordinates::TGLCoordinates3* FWorldSizeMin;
	Gls::Coordinates::TGLCoordinates3* FWorldSizeMax;
	int FThreadCount;
	Gls::Coordinates::TGLCoordinates3* FGravity;
	System::Classes::TCollection* FNewtonSurfaceItem;
	System::Classes::TOwnedCollection* FNewtonSurfacePair;
	System::Classes::TOwnedCollection* FNewtonJointGroup;
	TGLNGDDebugOption* FNewtonDebugOption;
	Gls::Objects::TGLLines* FGLLines;
	
private:
	void *FNewtonWorld;
	TGLNGDBehaviourList* FNGDBehaviours;
	Gls::Color::TGLColor* FCurrentColor;
	
protected:
	virtual void __fastcall Loaded();
	void __fastcall SetVisible(const bool Value);
	void __fastcall SetVisibleAtRunTime(const bool Value);
	void __fastcall SetSolverModel(const TGLNGDSolverModels Value);
	void __fastcall SetFrictionModel(const TGLNGDFrictionModels Value);
	void __fastcall SetMinimumFrameRate(const int Value);
	void __fastcall SetThreadCount(const int Value);
	void __fastcall SetGLLines(Gls::Objects::TGLLines* const Value);
	int __fastcall GetBodyCount();
	int __fastcall GetConstraintCount();
	void __fastcall AddNode(Gls::Coordinates::TGLCustomCoordinates* const coords)/* overload */;
	void __fastcall AddNode(const float X, const float Y, const float Z)/* overload */;
	void __fastcall AddNode(const Gls::Vectortypes::TVector4f &Value)/* overload */;
	void __fastcall AddNode(const Gls::Vectortypes::TVector3f &Value)/* overload */;
	void __fastcall RebuildAllMaterial();
	void __fastcall RebuildAllJoint(System::TObject* Sender);
	void __fastcall NotifyWorldSizeChange(System::TObject* Sender);
	void __fastcall NotifyChange(System::TObject* Sender);
	
public:
	__fastcall virtual TGLNGDManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLNGDManager();
	void __fastcall Step(float deltatime);
	
__published:
	__property bool Visible = {read=FVisible, write=SetVisible, default=1};
	__property bool VisibleAtRunTime = {read=FVisibleAtRunTime, write=SetVisibleAtRunTime, default=0};
	__property TGLNGDSolverModels SolverModel = {read=FSolverModel, write=SetSolverModel, default=0};
	__property TGLNGDFrictionModels FrictionModel = {read=FFrictionModel, write=SetFrictionModel, default=0};
	__property int MinimumFrameRate = {read=FMinimumFrameRate, write=SetMinimumFrameRate, default=60};
	__property int ThreadCount = {read=FThreadCount, write=SetThreadCount, default=1};
	__property int DllVersion = {read=FDllVersion, nodefault};
	__property int NewtonBodyCount = {read=GetBodyCount, nodefault};
	__property int NewtonConstraintCount = {read=GetConstraintCount, nodefault};
	__property Gls::Coordinates::TGLCoordinates3* Gravity = {read=FGravity, write=FGravity};
	__property Gls::Coordinates::TGLCoordinates3* WorldSizeMin = {read=FWorldSizeMin, write=FWorldSizeMin};
	__property Gls::Coordinates::TGLCoordinates3* WorldSizeMax = {read=FWorldSizeMax, write=FWorldSizeMax};
	__property System::Classes::TCollection* NewtonSurfaceItem = {read=FNewtonSurfaceItem, write=FNewtonSurfaceItem};
	__property System::Classes::TOwnedCollection* NewtonSurfacePair = {read=FNewtonSurfacePair, write=FNewtonSurfacePair};
	__property TGLNGDDebugOption* DebugOption = {read=FNewtonDebugOption, write=FNewtonDebugOption};
	__property Gls::Objects::TGLLines* Line = {read=FGLLines, write=SetGLLines};
	__property System::Classes::TOwnedCollection* NewtonJoint = {read=FNewtonJointGroup, write=FNewtonJointGroup};
};


class PASCALIMPLEMENTATION TGLNGDBehaviour : public Gls::Scene::TGLBehaviour
{
	typedef Gls::Scene::TGLBehaviour inherited;
	
private:
	TGLNGDManager* FManager;
	System::UnicodeString FManagerName;
	bool FInitialized;
	void *FNewtonBody;
	void *FCollision;
	Gls::Vectortypes::TMatrix4f FNewtonBodyMatrix;
	bool FContinuousCollisionMode;
	TGLNGDCollisions FNewtonCollisions;
	TCollisionIteratorEvent FCollisionIteratorEvent;
	Gls::Scene::TGLBaseSceneObject* FOwnerBaseSceneObject;
	bool FTreeCollisionOptimize;
	float FConvexCollisionTolerance;
	System::UnicodeString FFileCollision;
	TGLNGDSurfaceItem* FSurfaceItem;
	TGLNGDHeightField FHeightFieldOptions;
	
protected:
	virtual void __fastcall Initialize();
	virtual void __fastcall Finalize();
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded();
	void __fastcall SetManager(TGLNGDManager* Value);
	void __fastcall SetNewtonBodyMatrix(const Gls::Vectortypes::TMatrix4f &Value);
	void __fastcall SetContinuousCollisionMode(const bool Value);
	Gls::Vectortypes::TMatrix4f __fastcall GetNewtonBodyMatrix();
	Gls::Geometrybb::TAABB __fastcall GetNewtonBodyAABB();
	virtual void __fastcall UpdCollision();
	virtual void __fastcall Render();
	void __fastcall SetNewtonCollisions(const TGLNGDCollisions Value);
	void __fastcall SetNewtonSurfaceItem(TGLNGDSurfaceItem* const Value);
	void __fastcall SetHeightFieldOptions(const TGLNGDHeightField &Value);
	void * __fastcall GetPrimitiveCollision();
	void * __fastcall GetConvexCollision();
	void * __fastcall GetBBoxCollision();
	void * __fastcall GetBSphereCollision();
	void * __fastcall GetTreeCollision();
	void * __fastcall GetMeshCollision();
	void * __fastcall GetNullCollision();
	void * __fastcall GetHeightFieldCollision();
	void * __fastcall GetNGDFileCollision();
	bool __fastcall StoredTolerance();
	void __fastcall OnCollisionIteratorEvent(const void * userData, int vertexCount, const Physics::Ngdimport::PdFloat cfaceArray, int faceId);
	static void __cdecl NewtonCollisionIterator(const void * userData, int vertexCount, const Physics::Ngdimport::PdFloat faceArray, int faceId);
	static void __cdecl NewtonSerialize(void * serializeHandle, const void * buffer, unsigned size);
	static void __cdecl NewtonDeserialize(void * serializeHandle, void * buffer, unsigned size);
	
public:
	__fastcall virtual TGLNGDBehaviour(Gls::Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLNGDBehaviour();
	void __fastcall Reinitialize();
	__property bool Initialized = {read=FInitialized, nodefault};
	__classmethod virtual bool __fastcall UniqueItem();
	__property Gls::Vectortypes::TMatrix4f NewtonBodyMatrix = {read=GetNewtonBodyMatrix, write=SetNewtonBodyMatrix};
	__property Gls::Geometrybb::TAABB NewtonBodyAABB = {read=GetNewtonBodyAABB};
	void __fastcall Serialize(System::UnicodeString filename);
	void __fastcall DeSerialize(System::UnicodeString filename);
	__property TGLNGDHeightField HeightFieldOptions = {read=FHeightFieldOptions, write=SetHeightFieldOptions};
	
__published:
	__property TGLNGDManager* Manager = {read=FManager, write=SetManager};
	__property bool ContinuousCollisionMode = {read=FContinuousCollisionMode, write=SetContinuousCollisionMode, default=0};
	__property TGLNGDCollisions NGDNewtonCollisions = {read=FNewtonCollisions, write=SetNewtonCollisions, default=0};
	__property bool TreeCollisionOptimize = {read=FTreeCollisionOptimize, write=FTreeCollisionOptimize, default=1};
	__property float ConvexCollisionTolerance = {read=FConvexCollisionTolerance, write=FConvexCollisionTolerance, stored=StoredTolerance};
	__property System::UnicodeString FileCollision = {read=FFileCollision, write=FFileCollision};
	__property TGLNGDSurfaceItem* NGDSurfaceItem = {read=FSurfaceItem, write=SetNewtonSurfaceItem};
};


class PASCALIMPLEMENTATION TGLNGDDynamic : public TGLNGDBehaviour
{
	typedef TGLNGDBehaviour inherited;
	
private:
	Gls::Coordinates::TGLCoordinates3* FAABBmin;
	Gls::Coordinates::TGLCoordinates3* FAABBmax;
	Gls::Coordinates::TGLCoordinates3* FForce;
	Gls::Coordinates::TGLCoordinates3* FTorque;
	Gls::Coordinates::TGLCoordinates3* FCenterOfMass;
	bool FAutoSleep;
	float FLinearDamping;
	Gls::Coordinates::TGLCoordinates3* FAngularDamping;
	float FDensity;
	bool FUseGravity;
	float FNullCollisionVolume;
	TApplyForceAndTorqueEvent FApplyForceAndTorqueEvent;
	TSetTransformEvent FSetTransformEvent;
	TApplyForceAndTorqueEvent FCustomForceAndTorqueEvent;
	float FVolume;
	float FMass;
	Gls::Coordinates::TGLCoordinates3* FAppliedForce;
	Gls::Coordinates::TGLCoordinates3* FAppliedTorque;
	Gls::Coordinates::TGLCoordinates3* FAppliedOmega;
	Gls::Coordinates::TGLCoordinates3* FAppliedVelocity;
	bool __fastcall StoredDensity();
	bool __fastcall StoredLinearDamping();
	bool __fastcall StoredNullCollisionVolume();
	
protected:
	void __fastcall SetAutoSleep(const bool Value);
	void __fastcall SetLinearDamping(const float Value);
	virtual void __fastcall SetDensity(const float Value);
	virtual void __fastcall Initialize();
	virtual void __fastcall Finalize();
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded();
	virtual void __fastcall Render();
	void __fastcall NotifyCenterOfMassChange(System::TObject* Sender);
	void __fastcall NotifyAngularDampingChange(System::TObject* Sender);
	void __fastcall OnApplyForceAndTorqueEvent(const void * cbody, float timestep, int threadIndex);
	void __fastcall OnSetTransformEvent(const void * cbody, const Physics::Ngdimport::PdFloat cmatrix, int threadIndex);
	static void __cdecl NewtonApplyForceAndTorque(const void * body, float timestep, int threadIndex);
	static void __cdecl NewtonSetTransform(const void * body, const Physics::Ngdimport::PdFloat matrix, int threadIndex);
	
public:
	__fastcall virtual TGLNGDDynamic(Gls::Xcollection::TXCollection* AOwner);
	__fastcall virtual ~TGLNGDDynamic();
	void __fastcall AddImpulse(const Gls::Vectortypes::TVector4f &veloc, const Gls::Vectortypes::TVector4f &pointposit);
	Gls::Vectortypes::TVector4f __fastcall GetOmega();
	void __fastcall SetOmega(const Gls::Vectortypes::TVector4f &Omega);
	Gls::Vectortypes::TVector4f __fastcall GetVelocity();
	void __fastcall SetVelocity(const Gls::Vectortypes::TVector4f &Velocity);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__property TApplyForceAndTorqueEvent CustomForceAndTorqueEvent = {read=FCustomForceAndTorqueEvent, write=FCustomForceAndTorqueEvent};
	__property Gls::Vectortypes::TVector4f Velocity = {read=GetVelocity, write=SetVelocity};
	__property Gls::Vectortypes::TVector4f Omega = {read=GetOmega, write=SetOmega};
	
__published:
	__property Gls::Coordinates::TGLCoordinates3* Force = {read=FForce, write=FForce};
	__property Gls::Coordinates::TGLCoordinates3* Torque = {read=FTorque, write=FTorque};
	__property Gls::Coordinates::TGLCoordinates3* CenterOfMass = {read=FCenterOfMass, write=FCenterOfMass};
	__property bool AutoSleep = {read=FAutoSleep, write=SetAutoSleep, default=1};
	__property float LinearDamping = {read=FLinearDamping, write=SetLinearDamping, stored=StoredLinearDamping};
	__property Gls::Coordinates::TGLCoordinates3* AngularDamping = {read=FAngularDamping, write=FAngularDamping};
	__property float Density = {read=FDensity, write=SetDensity, stored=StoredDensity};
	__property bool UseGravity = {read=FUseGravity, write=FUseGravity, default=1};
	__property float NullCollisionVolume = {read=FNullCollisionVolume, write=FNullCollisionVolume, stored=StoredNullCollisionVolume};
	__property Gls::Coordinates::TGLCoordinates3* AppliedOmega = {read=FAppliedOmega};
	__property Gls::Coordinates::TGLCoordinates3* AppliedVelocity = {read=FAppliedVelocity};
	__property Gls::Coordinates::TGLCoordinates3* AppliedForce = {read=FAppliedForce};
	__property Gls::Coordinates::TGLCoordinates3* AppliedTorque = {read=FAppliedTorque};
	__property float Volume = {read=FVolume};
	__property float Mass = {read=FMass};
};


class PASCALIMPLEMENTATION TGLNGDStatic : public TGLNGDBehaviour
{
	typedef TGLNGDBehaviour inherited;
	
protected:
	virtual void __fastcall Render();
	
public:
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
public:
	/* TGLNGDBehaviour.Create */ inline __fastcall virtual TGLNGDStatic(Gls::Xcollection::TXCollection* AOwner) : TGLNGDBehaviour(AOwner) { }
	/* TGLNGDBehaviour.Destroy */ inline __fastcall virtual ~TGLNGDStatic() { }
	
};


class PASCALIMPLEMENTATION TGLNGDSurfaceItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FDisplayName;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	virtual void __fastcall SetDisplayName(const System::UnicodeString Value);
	
__published:
	__property DisplayName = {default=0};
	__property ID;
public:
	/* TCollectionItem.Create */ inline __fastcall virtual TGLNGDSurfaceItem(System::Classes::TCollection* Collection) : System::Classes::TCollectionItem(Collection) { }
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TGLNGDSurfaceItem() { }
	
};


class PASCALIMPLEMENTATION TGLNGDSurfacePair : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TGLNGDManager* FManager;
	TGLNGDSurfaceItem* FSurfaceItem1;
	TGLNGDSurfaceItem* FSurfaceItem2;
	TAABBOverlapEvent FAABBOverlapEvent;
	TContactProcessEvent FContactProcessEvent;
	float FSoftness;
	float FElasticity;
	bool FCollidable;
	float FStaticFriction;
	float FKineticFriction;
	bool FContinuousCollisionMode;
	bool FThickness;
	void __fastcall SetCollidable(const bool Value);
	void __fastcall SetElasticity(const float Value);
	void __fastcall SetKineticFriction(const float Value);
	void __fastcall SetSoftness(const float Value);
	void __fastcall SetStaticFriction(const float Value);
	void __fastcall SetContinuousCollisionMode(const bool Value);
	void __fastcall SetThickness(const bool Value);
	bool __fastcall StoredElasticity();
	bool __fastcall StoredKineticFriction();
	bool __fastcall StoredSoftness();
	bool __fastcall StoredStaticFriction();
	
private:
	static int __cdecl NewtonAABBOverlap(const void * material, const void * body0, const void * body1, int threadIndex);
	static void __cdecl NewtonContactsProcess(const void * contact, float timestep, int threadIndex);
	bool __fastcall OnNewtonAABBOverlapEvent(const void * cmaterial, const void * cbody0, const void * cbody1, int threadIndex);
	void __fastcall OnNewtonContactsProcessEvent(const void * ccontact, float timestep, int threadIndex);
	
public:
	__fastcall virtual TGLNGDSurfacePair(System::Classes::TCollection* Collection);
	void __fastcall SetMaterialItems(TGLNGDSurfaceItem* const item1, TGLNGDSurfaceItem* const item2);
	__property TGLNGDSurfaceItem* NGDSurfaceItem1 = {read=FSurfaceItem1};
	__property TGLNGDSurfaceItem* NGDSurfaceItem2 = {read=FSurfaceItem2};
	
__published:
	__property float Softness = {read=FSoftness, write=SetSoftness, stored=StoredSoftness};
	__property float Elasticity = {read=FElasticity, write=SetElasticity, stored=StoredElasticity};
	__property bool Collidable = {read=FCollidable, write=SetCollidable, default=1};
	__property float StaticFriction = {read=FStaticFriction, write=SetStaticFriction, stored=StoredStaticFriction};
	__property float KineticFriction = {read=FKineticFriction, write=SetKineticFriction, stored=StoredKineticFriction};
	__property bool ContinuousCollisionMode = {read=FContinuousCollisionMode, write=SetContinuousCollisionMode, default=0};
	__property bool Thickness = {read=FThickness, write=SetThickness, default=0};
	__property TContactProcessEvent ContactProcessEvent = {read=FContactProcessEvent, write=FContactProcessEvent};
	__property TAABBOverlapEvent AABBOverlapEvent = {read=FAABBOverlapEvent, write=FAABBOverlapEvent};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TGLNGDSurfacePair() { }
	
};


class PASCALIMPLEMENTATION TGLNGDJointPivot : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TGLNGDManager* FManager;
	Gls::Coordinates::TGLCoordinates3* FPivotPoint;
	TGLNGDJoint* FOuter;
	
public:
	__fastcall virtual TGLNGDJointPivot(System::Classes::TComponent* AOwner, TGLNGDJoint* aOuter);
	__fastcall virtual ~TGLNGDJointPivot();
	
__published:
	__property Gls::Coordinates::TGLCoordinates3* PivotPoint = {read=FPivotPoint, write=FPivotPoint};
};


class PASCALIMPLEMENTATION TGLNGDJointPin : public TGLNGDJointPivot
{
	typedef TGLNGDJointPivot inherited;
	
private:
	Gls::Coordinates::TGLCoordinates3* FPinDirection;
	
public:
	__fastcall virtual TGLNGDJointPin(System::Classes::TComponent* AOwner, TGLNGDJoint* aOuter);
	__fastcall virtual ~TGLNGDJointPin();
	
__published:
	__property Gls::Coordinates::TGLCoordinates3* PinDirection = {read=FPinDirection, write=FPinDirection};
};


class PASCALIMPLEMENTATION TGLNGDJointPin2 : public TGLNGDJointPin
{
	typedef TGLNGDJointPin inherited;
	
private:
	Gls::Coordinates::TGLCoordinates3* FPinDirection2;
	
public:
	__fastcall virtual TGLNGDJointPin2(System::Classes::TComponent* AOwner, TGLNGDJoint* aOuter);
	__fastcall virtual ~TGLNGDJointPin2();
	
__published:
	__property Gls::Coordinates::TGLCoordinates3* PinDirection2 = {read=FPinDirection2, write=FPinDirection2};
};


class PASCALIMPLEMENTATION TGLNGDJointBallAndSocket : public TGLNGDJointPivot
{
	typedef TGLNGDJointPivot inherited;
	
private:
	float FConeAngle;
	float FMinTwistAngle;
	float FMaxTwistAngle;
	void __fastcall SetConeAngle(const float Value);
	void __fastcall SetMaxTwistAngle(const float Value);
	void __fastcall SetMinTwistAngle(const float Value);
	bool __fastcall StoredMaxTwistAngle();
	bool __fastcall StoredMinTwistAngle();
	bool __fastcall StoredConeAngle();
	
public:
	__fastcall virtual TGLNGDJointBallAndSocket(System::Classes::TComponent* AOwner, TGLNGDJoint* aOuter);
	
__published:
	__property float ConeAngle = {read=FConeAngle, write=SetConeAngle, stored=StoredConeAngle};
	__property float MinTwistAngle = {read=FMinTwistAngle, write=SetMinTwistAngle, stored=StoredMinTwistAngle};
	__property float MaxTwistAngle = {read=FMaxTwistAngle, write=SetMaxTwistAngle, stored=StoredMaxTwistAngle};
public:
	/* TGLNGDJointPivot.Destroy */ inline __fastcall virtual ~TGLNGDJointBallAndSocket() { }
	
};


class PASCALIMPLEMENTATION TGLNGDJointHinge : public TGLNGDJointPin
{
	typedef TGLNGDJointPin inherited;
	
private:
	float FMinAngle;
	float FMaxAngle;
	void __fastcall SetMaxAngle(const float Value);
	void __fastcall SetMinAngle(const float Value);
	bool __fastcall StoredMaxAngle();
	bool __fastcall StoredMinAngle();
	
public:
	__fastcall virtual TGLNGDJointHinge(System::Classes::TComponent* AOwner, TGLNGDJoint* aOuter);
	
__published:
	__property float MinAngle = {read=FMinAngle, write=SetMinAngle, stored=StoredMinAngle};
	__property float MaxAngle = {read=FMaxAngle, write=SetMaxAngle, stored=StoredMaxAngle};
public:
	/* TGLNGDJointPin.Destroy */ inline __fastcall virtual ~TGLNGDJointHinge() { }
	
};


class PASCALIMPLEMENTATION TGLNGDJointSlider : public TGLNGDJointPin
{
	typedef TGLNGDJointPin inherited;
	
private:
	float FMinDistance;
	float FMaxDistance;
	void __fastcall SetMaxDistance(const float Value);
	void __fastcall SetMinDistance(const float Value);
	bool __fastcall StoredMaxDistance();
	bool __fastcall StoredMinDistance();
	
public:
	__fastcall virtual TGLNGDJointSlider(System::Classes::TComponent* AOwner, TGLNGDJoint* aOuter);
	
__published:
	__property float MinDistance = {read=FMinDistance, write=SetMinDistance, stored=StoredMinDistance};
	__property float MaxDistance = {read=FMaxDistance, write=SetMaxDistance, stored=StoredMaxDistance};
public:
	/* TGLNGDJointPin.Destroy */ inline __fastcall virtual ~TGLNGDJointSlider() { }
	
};


class PASCALIMPLEMENTATION TGLNGDJointKinematicController : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FPickModeLinear;
	float FLinearFriction;
	float FAngularFriction;
	bool __fastcall StoredAngularFriction();
	bool __fastcall StoredLinearFriction();
	
public:
	__fastcall TGLNGDJointKinematicController();
	
__published:
	__property bool PickModeLinear = {read=FPickModeLinear, write=FPickModeLinear, default=0};
	__property float LinearFriction = {read=FLinearFriction, write=FLinearFriction, stored=StoredLinearFriction};
	__property float AngularFriction = {read=FAngularFriction, write=FAngularFriction, stored=StoredAngularFriction};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLNGDJointKinematicController() { }
	
};


class PASCALIMPLEMENTATION TGLNGDJoint : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TGLNGDManager* FManager;
	Gls::Scene::TGLBaseSceneObject* FParentObject;
	TGLNGDJoints FJointType;
	float FStiffness;
	Gls::Scene::TGLBaseSceneObject* FChildObject;
	bool FCollisionState;
	void *FNewtonJoint;
	void *FNewtonUserJoint;
	Gls::Coordinates::TGLCoordinates3* FUPVectorDirection;
	TGLNGDJointPivot* FBallAndSocketOptions;
	TGLNGDJointPin* FHingeOptions;
	TGLNGDJointPin* FSliderOptions;
	TGLNGDJointPin* FCorkscrewOptions;
	TGLNGDJointPin2* FUniversalOptions;
	TGLNGDJointBallAndSocket* FCustomBallAndSocketOptions;
	TGLNGDJointHinge* FCustomHingeOptions;
	TGLNGDJointSlider* FCustomSliderOptions;
	TGLNGDJointKinematicController* FKinematicOptions;
	void __fastcall SetJointType(const TGLNGDJoints Value);
	void __fastcall SetChildObject(Gls::Scene::TGLBaseSceneObject* const Value);
	void __fastcall SetCollisionState(const bool Value);
	void __fastcall SetParentObject(Gls::Scene::TGLBaseSceneObject* const Value);
	void __fastcall SetStiffness(const float Value);
	void __fastcall Render();
	bool __fastcall StoredStiffness();
	void __fastcall DestroyNewtonData();
	
public:
	__fastcall virtual TGLNGDJoint(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLNGDJoint();
	void __fastcall KinematicControllerPick(const Gls::Vectortypes::TVector4f &pickpoint, TGLNGDPickedActions PickedActions);
	
__published:
	__property TGLNGDJointPivot* BallAndSocketOptions = {read=FBallAndSocketOptions, write=FBallAndSocketOptions};
	__property TGLNGDJointPin* HingeOptions = {read=FHingeOptions, write=FHingeOptions};
	__property TGLNGDJointPin* SliderOptions = {read=FSliderOptions, write=FSliderOptions};
	__property TGLNGDJointPin* CorkscrewOptions = {read=FCorkscrewOptions, write=FCorkscrewOptions};
	__property TGLNGDJointPin2* UniversalOptions = {read=FUniversalOptions, write=FUniversalOptions};
	__property TGLNGDJointBallAndSocket* CustomBallAndSocketOptions = {read=FCustomBallAndSocketOptions, write=FCustomBallAndSocketOptions};
	__property TGLNGDJointHinge* CustomHingeOptions = {read=FCustomHingeOptions, write=FCustomHingeOptions};
	__property TGLNGDJointSlider* CustomSliderOptions = {read=FCustomSliderOptions, write=FCustomSliderOptions};
	__property TGLNGDJointKinematicController* KinematicControllerOptions = {read=FKinematicOptions, write=FKinematicOptions};
	__property TGLNGDJoints JointType = {read=FJointType, write=SetJointType, nodefault};
	__property Gls::Scene::TGLBaseSceneObject* ParentObject = {read=FParentObject, write=SetParentObject};
	__property Gls::Scene::TGLBaseSceneObject* ChildObject = {read=FChildObject, write=SetChildObject};
	__property bool CollisionState = {read=FCollisionState, write=SetCollisionState, default=0};
	__property float Stiffness = {read=FStiffness, write=SetStiffness, stored=StoredStiffness};
	__property Gls::Coordinates::TGLCoordinates3* UPVectorDirection = {read=FUPVectorDirection, write=FUPVectorDirection};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TGLNGDStatic* __fastcall GetNGDStatic(Gls::Scene::TGLBaseSceneObject* Obj);
extern DELPHI_PACKAGE TGLNGDStatic* __fastcall GetOrCreateNGDStatic(Gls::Scene::TGLBaseSceneObject* Obj);
extern DELPHI_PACKAGE TGLNGDDynamic* __fastcall GetNGDDynamic(Gls::Scene::TGLBaseSceneObject* Obj);
extern DELPHI_PACKAGE TGLNGDDynamic* __fastcall GetOrCreateNGDDynamic(Gls::Scene::TGLBaseSceneObject* Obj);
extern DELPHI_PACKAGE void * __fastcall GetBodyFromGLSceneObject(Gls::Scene::TGLBaseSceneObject* Obj);
}	/* namespace Ngdmanager */
}	/* namespace Physics */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS_NGDMANAGER)
using namespace Physics::Ngdmanager;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS)
using namespace Physics;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Physics_NgdmanagerHPP
