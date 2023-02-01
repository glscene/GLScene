// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Physics.NGDImport.pas' rev: 35.00 (Windows)

#ifndef Physics_NgdimportHPP
#define Physics_NgdimportHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Physics
{
namespace Ngdimport
{
//-- forward type declarations -----------------------------------------------
struct TNewtonMaterialData;
struct TNewtonCollisionMaterial;
struct TNewtonBoxParam;
struct TNewtonSphereParam;
struct TNewtonCylinderParam;
struct TNewtonCapsuleParam;
struct TNewtonConeParam;
struct TNewtonTaperedCapsuleParam;
struct TNewtonTaperedCylinderParam;
struct TNewtonChamferCylinderParam;
struct TNewtonConvexHullParam;
struct TNewtonCompoundCollisionParam;
struct TNewtonCollisionTreeParam;
struct TNewtonDeformableMeshParam;
struct TNewtonHeightFieldCollisionParam;
struct TNewtonSceneCollisionParam;
#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonCollisionNullParam
{
};
#pragma pack(pop)


struct TNewtonCollisionInfoRecord;
struct NewtonJointRecord;
struct NewtonUserMeshCollisionCollideDesc;
struct NewtonWorldConvexCastReturnInfo;
struct NewtonUserMeshCollisionRayHitDesc;
struct NewtonHingeSliderUpdateDesc;
struct NewtonUserContactPoint;
struct NewtonMeshDoubleData;
struct NewtonMeshFloatData;
struct NewtonMeshVertexFormat;
//-- type declarations -------------------------------------------------------
typedef float dFloat;

typedef double dFloat64;

typedef __int64 dLong;

typedef float *PdFloat;

typedef double *PdFloat64;

typedef __int64 *PdLong;

typedef System::Extended Long_double;

typedef System::Int8 __Int8;

typedef short __Int16;

typedef int __Int32;

typedef __int64 __Int64;

typedef System::Int8 nchar;

typedef System::Byte unsigned_char;

typedef short Short;

typedef System::Word unsigned_short;

typedef int Long;

typedef unsigned unsigned_long;

typedef unsigned unsigned_int;

typedef unsigned size_t;

typedef System::StaticArray<System::WideChar, 256> charArray;

typedef System::Int8 *P__int8;

typedef short *P__int16;

typedef int *P__int32;

typedef __int64 *P__int64;

typedef System::Int8 *P2Char;

typedef System::Byte *PUnsigned_char;

typedef short *PShort;

typedef System::Word *PUnsigned_short;

typedef int *PLong;

typedef unsigned *PUnsigned_long;

typedef unsigned *PUnsigned_int;

typedef unsigned *Psize_t;

typedef System::Extended *PLong_double;

typedef charArray *PCharArray;

typedef bool Bool;

typedef void * Pvoid;

typedef bool *PBool;

typedef void * PNewtonMesh;

typedef void * PNewtonBody;

typedef void * PNewtonWorld;

typedef void * PNewtonJoint;

typedef void * PNewtonMaterial;

typedef void * PNewtonCollision;

typedef void * PNewtonDeformableMeshSegment;

typedef void * PNewtonFracturedCompoundMeshPart;

typedef void * PNewtonSerializeHandle;

typedef void * PNewtonMeshHandle;

typedef void * PNewtonMeshVertex;

typedef void * PNewtonMeshPoint;

typedef void * PNewtonMeshEdge;

typedef void * PNewtonMeshFace;

typedef void * PNewtonSceneProxy;

typedef void * PNewtonBreakableComponentMesh;

typedef void * PNewtonListener;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonMaterialData
{
public:
	void *m_ptr;
	__int64 m_int;
	float m_float;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonCollisionMaterial
{
public:
	__int64 m_userId;
	TNewtonMaterialData m_userData;
	System::StaticArray<TNewtonMaterialData, 6> m_userParam;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonBoxParam
{
public:
	float m_x;
	float m_y;
	float m_z;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonSphereParam
{
public:
	float m_radio;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonCylinderParam
{
public:
	float m_radio;
	float m_height;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonCapsuleParam
{
public:
	float m_radio;
	float m_height;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonConeParam
{
public:
	float m_radio;
	float m_height;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonTaperedCapsuleParam
{
public:
	float m_radio0;
	float m_radio1;
	float m_height;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonTaperedCylinderParam
{
public:
	float m_radio0;
	float m_radio1;
	float m_height;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonChamferCylinderParam
{
public:
	float m_radio;
	float m_height;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonConvexHullParam
{
public:
	int m_vertexCount;
	int m_vertexStrideInBytes;
	int m_faceCount;
	float *m_vertex;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonCompoundCollisionParam
{
public:
	int m_chidrenCount;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonCollisionTreeParam
{
public:
	int m_vertexCount;
	int m_indexCount;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonDeformableMeshParam
{
public:
	int m_vertexCount;
	int m_triangleCount;
	int m_vertexStrideInBytes;
	System::Word *m_indexList;
	float *m_vertexList;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonHeightFieldCollisionParam
{
public:
	int m_width;
	int m_height;
	int m_gridsDiagonals;
	int m_elevationDataType;
	float m_verticalScale;
	float m_horizonalScale_x;
	float m_horizonalScale_z;
	void *m_vertialElevation;
	System::WideChar *m_atributes;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonSceneCollisionParam
{
public:
	int m_childrenProxyCount;
};
#pragma pack(pop)


typedef TNewtonCollisionInfoRecord *PNewtonCollisionInfoRecord;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonCollisionInfoRecord
{
public:
	System::StaticArray<System::StaticArray<float, 4>, 4> m_offsetMatrix;
	TNewtonCollisionMaterial m_collisionMaterial;
	int m_collisionType;
	
public:
	union
	{
		struct 
		{
			TNewtonSceneCollisionParam sdSceneCollision;
		};
		struct 
		{
			TNewtonHeightFieldCollisionParam sdHeightfield;
		};
		struct 
		{
			TNewtonCollisionTreeParam sdTree;
		};
		struct 
		{
			TNewtonCompoundCollisionParam sdCompound;
		};
		struct 
		{
			
		};
		struct 
		{
			TNewtonConvexHullParam sdConvexhull;
		};
		struct 
		{
			TNewtonChamferCylinderParam sdChamfercylinder;
		};
		struct 
		{
			TNewtonCylinderParam sdCylinder;
		};
		struct 
		{
			TNewtonCapsuleParam sdCapsule;
		};
		struct 
		{
			TNewtonSphereParam sdSphere;
		};
		struct 
		{
			TNewtonConeParam shapedata;
		};
		struct 
		{
			TNewtonBoxParam sdbox;
		};
		
	};
};
#pragma pack(pop)


typedef NewtonJointRecord *PNewtonJointRecord;

#pragma pack(push,1)
struct DECLSPEC_DRECORD NewtonJointRecord
{
public:
	System::StaticArray<System::StaticArray<float, 4>, 4> m_attachmenMatrix_0;
	System::StaticArray<System::StaticArray<float, 4>, 4> m_attachmenMatrix_1;
	System::StaticArray<float, 3> m_minLinearDof;
	System::StaticArray<float, 3> m_maxLinearDof;
	System::StaticArray<float, 3> m_minAngularDof;
	System::StaticArray<float, 3> m_maxAngularDof;
	void *m_attachBody_0;
	void *m_attachBody_1;
	System::StaticArray<float, 16> m_extraParameters;
	int m_bodiesCollisionOn;
	System::StaticArray<float, 32> m_descriptionType;
};
#pragma pack(pop)


typedef NewtonUserMeshCollisionCollideDesc *PNewtonUserMeshCollisionCollideDesc;

struct DECLSPEC_DRECORD NewtonUserMeshCollisionCollideDesc
{
public:
	System::StaticArray<float, 4> m_boxP0;
	System::StaticArray<float, 4> m_boxP1;
	System::StaticArray<float, 4> m_boxDistanceTravel;
	int m_threadNumber;
	int m_faceCount;
	int m_vertexStrideInBytes;
	float m_skinThickness;
	void *m_userData;
	void *m_objBody;
	void *m_polySoupBody;
	void *m_objCollision;
	void *m_polySoupCollision;
	float *m_vertex;
	int *m_faceIndexCount;
	int *m_faceVertexIndex;
};


typedef NewtonWorldConvexCastReturnInfo *PNewtonWorldConvexCastReturnInfo;

struct DECLSPEC_DRECORD NewtonWorldConvexCastReturnInfo
{
public:
	System::StaticArray<float, 4> m_point;
	System::StaticArray<float, 4> m_normal;
	System::StaticArray<float, 4> m_normalOnHitPoint;
	int m_contactID;
	void *m_hitBody;
	float m_penetration;
};


typedef NewtonUserMeshCollisionRayHitDesc *PNewtonUserMeshCollisionRayHitDesc;

struct DECLSPEC_DRECORD NewtonUserMeshCollisionRayHitDesc
{
public:
	System::StaticArray<float, 4> m_p0;
	System::StaticArray<float, 4> m_p1;
	System::StaticArray<float, 4> m_normalOut;
	int m_userIdOut;
	void *m_userData;
};


typedef NewtonHingeSliderUpdateDesc *PNewtonHingeSliderUpdateDesc;

#pragma pack(push,1)
struct DECLSPEC_DRECORD NewtonHingeSliderUpdateDesc
{
public:
	float m_accel;
	float m_minFriction;
	float m_maxFriction;
	float m_timestep;
};
#pragma pack(pop)


typedef NewtonUserContactPoint *PNewtonUserContactPoint;

#pragma pack(push,1)
struct DECLSPEC_DRECORD NewtonUserContactPoint
{
public:
	System::StaticArray<float, 4> m_point;
	System::StaticArray<float, 4> m_normal;
	__int64 m_shapeId0;
	__int64 m_shapeId1;
	float m_penetration;
	System::StaticArray<int, 3> m_unused;
};
#pragma pack(pop)


typedef NewtonMeshDoubleData *PNewtonMeshDoubleData;

#pragma pack(push,1)
struct DECLSPEC_DRECORD NewtonMeshDoubleData
{
public:
	double *m_data;
	int *m_indexList;
	int m_strideInBytes;
};
#pragma pack(pop)


typedef NewtonMeshFloatData *PNewtonMeshFloatData;

#pragma pack(push,1)
struct DECLSPEC_DRECORD NewtonMeshFloatData
{
public:
	float *m_data;
	int *m_indexList;
	int m_strideInBytes;
};
#pragma pack(pop)


typedef NewtonMeshVertexFormat *PNewtonMeshVertexFormat;

#pragma pack(push,1)
struct DECLSPEC_DRECORD NewtonMeshVertexFormat
{
public:
	int m_faceCount;
	int *m_faceIndexCount;
	int *m_faceMaterial;
	NewtonMeshDoubleData m_vertex;
	NewtonMeshFloatData m_normal;
	NewtonMeshFloatData m_binormal;
	NewtonMeshFloatData m_uv0;
	NewtonMeshFloatData m_uv1;
	NewtonMeshFloatData m_vertexColor;
};
#pragma pack(pop)


typedef void * __cdecl (*NewtonAllocMemory)(int sizeInBytes);

typedef NewtonAllocMemory *PNewtonAllocMemory;

typedef void __cdecl (*NewtonFreeMemory)(void * ptr, int sizeInBytes);

typedef NewtonFreeMemory *PNewtonFreeMemory;

typedef void __cdecl (*NewtonDestroyWorld)(const void * NewtonWorld);

typedef NewtonDestroyWorld *PNewtonDestroyWorld;

typedef unsigned __cdecl (*NewtonGetTicksCountCallback)(void);

typedef NewtonGetTicksCountCallback *PNewtonGetTicksCountCallback;

typedef void __cdecl (*NewtonSerialize)(void * serializeHandle, const void * buffer, unsigned size);

typedef NewtonSerialize *PNewtonSerialize;

typedef void __cdecl (*NewtonDeserialize)(void * serializeHandle, void * buffer, unsigned size);

typedef NewtonDeserialize *PNewtonDeserialize;

typedef void __cdecl (*NewtonUserMeshCollisionDestroyCallback)(void * descData);

typedef NewtonUserMeshCollisionDestroyCallback *PNewtonUserMeshCollisionDestroyCallback;

typedef void __cdecl (*NewtonUserMeshCollisionCollideCallback)(PNewtonUserMeshCollisionCollideDesc NewtonUserMeshCollisionCollideDesc);

typedef NewtonUserMeshCollisionCollideCallback *PNewtonUserMeshCollisionCollideCallback;

typedef int __cdecl (*NewtonUserMeshCollisionRayHitCallback)(PNewtonUserMeshCollisionRayHitDesc NewtonUserMeshCollisionRayHitDesc);

typedef NewtonUserMeshCollisionRayHitCallback *PNewtonUserMeshCollisionRayHitCallback;

typedef void __cdecl (*NewtonUserMeshCollisionGetCollisionInfo)(void * userData, PNewtonCollisionInfoRecord infoRecord);

typedef NewtonUserMeshCollisionGetCollisionInfo *PNewtonUserMeshCollisionGetCollisionInfo;

typedef int __cdecl (*NewtonUserMeshCollisionGetFacesInAABB)(void * userData, const PdFloat p0, const PdFloat p1, const PdFloat vertexArray, System::PInteger vertexCount, System::PInteger vertexStrideInBytes, const System::PInteger indexList, int maxIndexCount, const System::PInteger userDataList);

typedef NewtonUserMeshCollisionGetFacesInAABB *PNewtonUserMeshCollisionGetFacesInAABB;

typedef float __cdecl (*NewtonCollisionTreeRayCastCallback)(const void * Body, const void * TreeCollision, float interception, PdFloat normal, int faceId, void * usedData);

typedef NewtonCollisionTreeRayCastCallback *PNewtonCollisionTreeRayCastCallback;

typedef float __cdecl (*NewtonHeightFieldRayCastCallback)(const void * Body, const void * HeightFieldCollision, float interception, int Row, int Col, PdFloat normal, int faceId, void * usedData);

typedef NewtonHeightFieldRayCastCallback *PNewtonHeightFieldRayCastCallback;

typedef void __cdecl (*NewtonTreeCollisionCallback)(const void * bodyWithTreeCollision, const void * Body, int faceId, const PdFloat vertex, int vertexStrideInBytes);

typedef NewtonTreeCollisionCallback *PNewtonTreeCollisionCallback;

typedef void __cdecl (*NewtonBodyDestructor)(const void * Body);

typedef NewtonBodyDestructor *PNewtonBodyDestructor;

typedef void __cdecl (*NewtonApplyForceAndTorque)(const void * Body, float timestep, int threadIndex);

typedef NewtonApplyForceAndTorque *PNewtonApplyForceAndTorque;

typedef void __cdecl (*NewtonSetTransform)(const void * Body, const PdFloat matrix, int threadIndex);

typedef NewtonSetTransform *PNewtonSetTransform;

typedef int __cdecl (*NewtonIslandUpdate)(const void * World, void * islandHandle, int bodyCount);

typedef NewtonIslandUpdate *PNewtonIslandUpdate;

typedef void __cdecl (*NewtonBodyLeaveWorld)(const void * Body, int threadIndex);

typedef NewtonBodyLeaveWorld *PNewtonBodyLeaveWorld;

typedef void __cdecl (*NewtonDestroyBodyByExeciveForce)(const void * Body, const void * contact);

typedef NewtonDestroyBodyByExeciveForce *PNewtonDestroyBodyByExeciveForce;

typedef void __cdecl (*NewtonCollisionDestructor)(const void * World, const void * collision);

typedef NewtonCollisionDestructor *PNewtonCollisionDestructor;

typedef int __cdecl (*NewtonCollisionCompoundBreakableCallback)(const void * Mesh, void * userData, PdFloat planeMatrixOut);

typedef NewtonCollisionCompoundBreakableCallback *PNewtonCollisionCompoundBreakableCallback;

typedef int __cdecl (*NewtonGetBuoyancyPlane)(const int collisionID, void * context, const PdFloat globalSpaceMatrix, PdFloat globalSpacePlane);

typedef NewtonGetBuoyancyPlane *PNewtonGetBuoyancyPlane;

typedef unsigned __cdecl (*NewtonWorldRayPrefilterCallback)(const void * Body, const void * collision, void * userData);

typedef NewtonWorldRayPrefilterCallback *PNewtonWorldRayPrefilterCallback;

typedef float __cdecl (*NewtonWorldRayFilterCallback)(const void * Body, const PdFloat hitNormal, int collisionID, void * userData, float intersetParam);

typedef NewtonWorldRayFilterCallback *PNewtonWorldRayFilterCallback;

typedef int __cdecl (*NewtonOnAABBOverlap)(const void * material, const void * body0, const void * body1, int threadIndex);

typedef NewtonOnAABBOverlap *PNewtonOnAABBOverlap;

typedef void __cdecl (*NewtonContactsProcess)(const void * contact, float timestep, int threadIndex);

typedef NewtonContactsProcess *PNewtonContactsProcess;

typedef void __cdecl (*NewtonBodyIterator)(const void * Body, void * userData);

typedef NewtonBodyIterator *PNewtonBodyIterator;

typedef void __cdecl (*NewtonJointIterator)(const void * joint, void * userData);

typedef NewtonJointIterator *PNewtonJointIterator;

typedef void __cdecl (*NewtonCollisionIterator)(void * userData, int vertexCount, const PdFloat FaceArray, int faceId);

typedef NewtonCollisionIterator *PNewtonCollisionIterator;

typedef void __cdecl (*NewtonBallCallBack)(const void * ball, float timestep);

typedef NewtonBallCallBack *PNewtonBallCallBack;

typedef unsigned __cdecl (*NewtonHingeCallBack)(const void * hinge, PNewtonHingeSliderUpdateDesc desc);

typedef NewtonHingeCallBack *PNewtonHingeCallBack;

typedef unsigned __cdecl (*NewtonSliderCallBack)(const void * slider, PNewtonHingeSliderUpdateDesc desc);

typedef NewtonSliderCallBack *PNewtonSliderCallBack;

typedef unsigned __cdecl (*NewtonUniversalCallBack)(const void * universal, PNewtonHingeSliderUpdateDesc desc);

typedef NewtonUniversalCallBack *PNewtonUniversalCallBack;

typedef unsigned __cdecl (*NewtonCorkscrewCallBack)(const void * corkscrew, PNewtonHingeSliderUpdateDesc desc);

typedef NewtonCorkscrewCallBack *PNewtonCorkscrewCallBack;

typedef void __cdecl (*NewtonUserBilateralCallBack)(const void * userJoint, float timestep, int threadIndex);

typedef NewtonUserBilateralCallBack *PNewtonUserBilateralCallBack;

typedef void __cdecl (*NewtonUserBilateralGetInfoCallBack)(const void * userJoint, PNewtonJointRecord info);

typedef NewtonUserBilateralGetInfoCallBack *PNewtonUserBilateralGetInfoCallBack;

typedef void __cdecl (*NewtonConstraintDestructor)(const void * me);

typedef NewtonConstraintDestructor *PNewtonConstraintDestructor;

typedef System::DynamicArray<void *> TCollisionPrimitiveArray;

//-- var, const, procedure ---------------------------------------------------
#define NewtonDLL L"newton32.dll"
static const System::Int8 NEWTON_MAJOR_VERSION = System::Int8(0x3);
static const System::Int8 NEWTON_MINOR_VERSION = System::Int8(0xf);
static const System::Int8 NEWTON_BROADPHASE_DEFAULT = System::Int8(0x0);
static const System::Int8 NEWTON_PROFILER_WORLD_UPDATE = System::Int8(0x0);
static const System::Int8 NEWTON_DYNAMIC_BODY = System::Int8(0x0);
static const System::Int8 NEWTON_PROFILER_COLLISION_UPDATE = System::Int8(0x1);
static const System::Int8 NEWTON_PROFILER_COLLISION_UPDATE_BROAD_PHASE = System::Int8(0x2);
static const System::Int8 NEWTON_PROFILER_COLLISION_UPDATE_NARROW_PHASE = System::Int8(0x3);
static const System::Int8 NEWTON_PROFILER_DYNAMICS_UPDATE = System::Int8(0x4);
static const System::Int8 NEWTON_PROFILER_DYNAMICS_CONSTRAINT_GRAPH = System::Int8(0x5);
static const System::Int8 NEWTON_PROFILER_FORCE_CALLBACK_UPDATE = System::Int8(0x6);
static const System::Int8 NEWTON_PROFILER_DYNAMICS_SOLVE_CONSTRAINT_GRAPH = System::Int8(0x7);
static const System::Int8 SERIALIZE_ID_SPHERE = System::Int8(0x0);
static const System::Int8 SERIALIZE_ID_CAPSULE = System::Int8(0x1);
static const System::Int8 SERIALIZE_ID_CYLINDER = System::Int8(0x2);
static const System::Int8 SERIALIZE_ID_CHAMFERCYLINDER = System::Int8(0x3);
static const System::Int8 SERIALIZE_ID_BOX = System::Int8(0x4);
static const System::Int8 SERIALIZE_ID_CONE = System::Int8(0x5);
static const System::Int8 SERIALIZE_ID_CONVEXHULL = System::Int8(0x6);
static const System::Int8 SERIALIZE_ID_NULL = System::Int8(0x7);
static const System::Int8 SERIALIZE_ID_COMPOUND = System::Int8(0x8);
static const System::Int8 SERIALIZE_ID_TREE = System::Int8(0x9);
static const System::Int8 SERIALIZE_ID_HEIGHTFIELD = System::Int8(0xa);
static const System::Int8 SERIALIZE_ID_CLOTH_PATCH = System::Int8(0xb);
static const System::Int8 SERIALIZE_ID_DEFORMABLE_SOLID = System::Int8(0xc);
static const System::Int8 SERIALIZE_ID_USERMESH = System::Int8(0xd);
static const System::Int8 SERIALIZE_ID_SCENE = System::Int8(0xe);
static const System::Int8 SERIALIZE_ID_FRACTURED_COMPOUND = System::Int8(0xf);
extern "C" int __cdecl NewtonWorldGetVersion(const void * NewtonWorld);
extern "C" int __cdecl NewtonWorldFloatSize(const void * NewtonWorld);
extern "C" void * __cdecl NewtonCreate(NewtonAllocMemory malloc, NewtonFreeMemory mfree);
extern "C" void __cdecl NewtonDestroy(const void * NewtonWorld);
extern "C" void __cdecl NewtonDestroyAllBodies(const void * NewtonWorld);
extern "C" int __cdecl NewtonGetMemoryUsed(void);
extern "C" void __cdecl NewtonSetMemorySystem(NewtonAllocMemory malloc, NewtonFreeMemory mfree);
extern "C" void __cdecl NewtonUpdate(const void * NewtonWorld, float timestep);
extern "C" void __cdecl NewtonInvalidateCache(const void * NewtonWorld);
extern "C" void __cdecl NewtonCollisionUpdate(const void * NewtonWorld);
extern "C" void __cdecl NewtonSetSolverModel(const void * NewtonWorld, int Model);
extern "C" void __cdecl NewtonSetPlatformArchitecture(const void * NewtonWorld, int mode);
extern "C" int __cdecl NewtonGetPlatformArchitecture(const void * NewtonWorld, PCharArray description);
extern "C" void __cdecl NewtonSetMultiThreadSolverOnSingleIsland(const void * NewtonWorld, int mode);
extern "C" int __cdecl NewtonGetMultiThreadSolverOnSingleIsland(const void * NewtonWorld);
extern "C" void __cdecl NewtonSetPerformanceClock(const void * NewtonWorld, PNewtonGetTicksCountCallback NewtonGetTicksCountCallback);
extern "C" unsigned __cdecl NewtonReadPerformanceTicks(const void * NewtonWorld, unsigned performanceEntry);
extern "C" unsigned __cdecl NewtonReadThreadPerformanceTicks(const void * NewtonWorld, unsigned threadIndex);
extern "C" void __cdecl NewtonWorldCriticalSectionLock(const void * NewtonWorld);
extern "C" void __cdecl NewtonWorldCriticalSectionUnlock(const void * NewtonWorld);
extern "C" void __cdecl NewtonSetThreadsCount(const void * NewtonWorld, int threads);
extern "C" int __cdecl NewtonGetThreadsCount(const void * NewtonWorld);
extern "C" int __cdecl NewtonGetMaxThreadsCount(const void * NewtonWorld);
extern "C" void __cdecl NewtonSetFrictionModel(const void * NewtonWorld, int Model);
extern "C" void __cdecl NewtonSetMinimumFrameRate(const void * NewtonWorld, float frameRate);
extern "C" void __cdecl NewtonSetBodyLeaveWorldEvent(const void * NewtonWorld, PNewtonBodyLeaveWorld callback);
extern "C" void __cdecl NewtonSetWorldSize(const void * NewtonWorld, const PdFloat minPoint, const PdFloat maxPoint);
extern "C" void __cdecl NewtonSetIslandUpdateEvent(const void * NewtonWorld, PNewtonIslandUpdate NewtonIslandUpdate);
extern "C" void __cdecl NewtonSetCollisionDestructor(const void * NewtonWorld, PNewtonCollisionDestructor callback);
extern "C" void __cdecl NewtonSetDestroyBodyByExeciveForce(const void * NewtonWorld, PNewtonDestroyBodyByExeciveForce callback);
extern "C" void __cdecl NewtonWorldForEachJointDo(const void * NewtonWorld, PNewtonJointIterator callback, void * userData);
extern "C" void __cdecl NewtonWorldForEachBodyInAABBDo(const void * NewtonWorld, const PdFloat p0, const PdFloat p1, PNewtonBodyIterator callback, void * userData);
extern "C" void __cdecl NewtonWorldSetUserData(const void * NewtonWorld, void * userData);
extern "C" void * __cdecl NewtonWorldGetUserData(const void * NewtonWorld);
extern "C" void __cdecl NewtonWorldSetDestructorCallBack(const void * NewtonWorld, PNewtonDestroyWorld NewtonDestroyWorld);
extern "C" PNewtonDestroyWorld __cdecl NewtonWorldGetDestructorCallBack(const void * NewtonWorld);
extern "C" void __cdecl NewtonWorldRayCast(const void * NewtonWorld, const PdFloat p0, const PdFloat p1, PNewtonWorldRayFilterCallback filter, void * userData, NewtonWorldRayPrefilterCallback prefilter);
extern "C" int __cdecl NewtonWorldConvexCast(const void * NewtonWorld, const PdFloat matrix, const PdFloat target, const void * shape, PdFloat hitParam, void * userData, NewtonWorldRayPrefilterCallback prefilter, PNewtonWorldConvexCastReturnInfo info, int maxContactsCount, int threadIndex);
extern "C" int __cdecl NewtonWorldGetBodyCount(const void * NewtonWorld);
extern "C" int __cdecl NewtonWorldGetConstraintCount(const void * NewtonWorld);
extern "C" void * __cdecl NewtonIslandGetBody(const void * island, int bodyIndex);
extern "C" void __cdecl NewtonIslandGetBodyAABB(const void * island, int bodyIndex, PdFloat p0, PdFloat p1);
extern "C" int __cdecl NewtonMaterialCreateGroupID(const void * NewtonWorld);
extern "C" int __cdecl NewtonMaterialGetDefaultGroupID(const void * NewtonWorld);
extern "C" void __cdecl NewtonMaterialDestroyAllGroupID(const void * NewtonWorld);
extern "C" void * __cdecl NewtonMaterialGetUserData(const void * NewtonWorld, int id0, int id1);
extern "C" void __cdecl NewtonMaterialSetSurfaceThickness(const void * NewtonWorld, int id0, int id1, float thickness);
extern "C" void __cdecl NewtonMaterialSetContinuousCollisionMode(const void * NewtonWorld, int id0, int id1, int state);
extern "C" void __cdecl NewtonMaterialSetCollisionCallback(const void * NewtonWorld, int id0, int id1, void * userData, PNewtonOnAABBOverlap AABBOverlap, PNewtonContactsProcess process);
extern "C" void __cdecl NewtonMaterialSetDefaultSoftness(const void * NewtonWorld, int id0, int id1, float value);
extern "C" void __cdecl NewtonMaterialSetDefaultElasticity(const void * NewtonWorld, int id0, int id1, float elasticCoef);
extern "C" void __cdecl NewtonMaterialSetDefaultCollidable(const void * NewtonWorld, int id0, int id1, int state);
extern "C" void __cdecl NewtonMaterialSetDefaultFriction(const void * NewtonWorld, int id0, int id1, float staticFriction, float kineticFriction);
extern "C" void * __cdecl NewtonWorldGetFirstMaterial(const void * NewtonWorld);
extern "C" void * __cdecl NewtonWorldGetNextMaterial(const void * NewtonWorld, const void * material);
extern "C" void * __cdecl NewtonWorldGetFirstBody(const void * NewtonWorld);
extern "C" void * __cdecl NewtonWorldGetNextBody(const void * NewtonWorld, const void * curBody);
extern "C" void * __cdecl NewtonMaterialGetMaterialPairUserData(const void * material);
extern "C" unsigned __cdecl NewtonMaterialGetContactFaceAttribute(const void * material);
extern "C" void * __fastcall NewtonMaterialGetBodyCollidingShape(const void * material, const void * Body);
extern "C" unsigned __cdecl NewtonMaterialGetBodyCollisionID(const void * material, void * Body);
extern "C" float __cdecl NewtonMaterialGetContactNormalSpeed(const void * material);
extern "C" void __cdecl NewtonMaterialGetContactForce(const void * material, const void * Body, PdFloat Force);
extern "C" void __cdecl NewtonMaterialGetContactPositionAndNormal(const void * material, const void * Body, const PdFloat Posit, const PdFloat normal);
extern "C" void __cdecl NewtonMaterialGetContactTangentDirections(const void * material, const void * Body, const PdFloat Dir0, const PdFloat Dir1);
extern "C" float __cdecl NewtonMaterialGetContactTangentSpeed(const void * material, int index);
extern "C" void __cdecl NewtonMaterialSetContactSoftness(const void * material, float softness);
extern "C" void __cdecl NewtonMaterialSetContactElasticity(const void * material, float restitution);
extern "C" void __cdecl NewtonMaterialSetContactFrictionState(const void * material, int state, int index);
extern "C" void __cdecl NewtonMaterialSetContactFrictionCoef(const void * material, float staticFrictionCoef, float kineticFrictionCoef, int index);
extern "C" void __cdecl NewtonMaterialSetContactNormalAcceleration(const void * material, float accel);
extern "C" void __cdecl NewtonMaterialSetContactNormalDirection(const void * material, PdFloat directionVector);
extern "C" void __cdecl NewtonMaterialSetContactTangentAcceleration(const void * material, float accel, int index);
extern "C" void __cdecl NewtonMaterialContactRotateTangentDirections(const void * material, const PdFloat directionVector);
extern "C" void * __cdecl NewtonCreateNull(const void * NewtonWorld);
extern "C" void * __cdecl NewtonCreateSphere(const void * NewtonWorld, float radiusX, float radiusY, float radiusZ, int shapeID, const PdFloat offsetMatrix);
extern "C" void * __cdecl NewtonCreateBox(const void * NewtonWorld, float dx, float dy, float dz, int shapeID, const PdFloat offsetMatrix);
extern "C" void * __cdecl NewtonCreateCone(const void * NewtonWorld, float radius, float height, int shapeID, const PdFloat offsetMatrix);
extern "C" void * __cdecl NewtonCreateCapsule(const void * NewtonWorld, float radius, float height, int shapeID, const PdFloat offsetMatrix);
extern "C" void * __cdecl NewtonCreateCylinder(const void * NewtonWorld, float radius, float height, int shapeID, const PdFloat offsetMatrix);
extern "C" void * __cdecl NewtonCreateChamferCylinder(const void * NewtonWorld, float raduis, float height, int shapeID, const PdFloat offsetMatrix);
extern "C" void * __cdecl NewtonCreateConvexHull(const void * NewtonWorld, int count, const PdFloat vertexCloud, int strideInBytes, float tolerance, int shapeID, const PdFloat offsetMatrix);
extern "C" void * __cdecl NewtonCreateConvexHullFromMesh(const void * NewtonWorld, void * мesh, float tolerance, int shapeID);
extern "C" void * __cdecl NewtonCreateConvexHullModifier(const void * NewtonWorld, const void * convexHullCollision, int shapeID);
extern "C" void __cdecl NewtonConvexHullModifierGetMatrix(const void * convexHullCollision, PdFloat matrix);
extern "C" void __cdecl NewtonConvexHullModifierSetMatrix(const void * convexHullCollision, const PdFloat matrix);
extern "C" int __cdecl NewtonCollisionIsTriggerVolume(const void * convexCollision);
extern "C" void __cdecl NewtonCollisionSetAsTriggerVolume(const void * convexCollision, int trigger);
extern "C" void __cdecl NewtonCollisionSetMaxBreakImpactImpulse(const void * convexHullCollision, float maxImpactImpulse);
extern "C" float __cdecl NewtonCollisionGetMaxBreakImpactImpulse(const void * convexHullCollision);
extern "C" void __cdecl NewtonCollisionSetUserID(const void * convexCollision, unsigned id);
extern "C" unsigned __cdecl NewtonCollisionGetUserID(const void * convexCollision);
extern "C" int __cdecl NewtonConvexHullGetFaceIndices(const void * convexHullCollision, int face, System::PInteger faceIndices);
extern "C" float __cdecl NewtonConvexCollisionCalculateVolume(const void * convexCollision);
extern "C" void __cdecl NewtonConvexCollisionCalculateInertialMatrix(const void * convexCollision, PdFloat inertia, PdFloat origin);
extern "C" void __cdecl NewtonCollisionMakeUnique(const void * NewtonWorld, const void * collision);
extern "C" void __cdecl NewtonReleaseCollision(const void * NewtonWorld, const void * collision);
extern "C" int __cdecl NewtonAddCollisionReference(const void * collision);
extern "C" void * __cdecl NewtonCreateCompoundCollision(const void * NewtonWorld, int count, const TCollisionPrimitiveArray collisionPrimitiveArray, int shapeID);
extern "C" void * __cdecl NewtonCreateCompoundCollisionFromMesh(const void * NewtonWorld, const void * Mesh, int maxSubShapesCount, int shapeID, int subShapeID);
extern "C" void * __cdecl NewtonCreateUserMeshCollision(const void * NewtonWorld, const PdFloat minBox, const PdFloat maxBox, void * userData, NewtonUserMeshCollisionCollideCallback collideCallback, NewtonUserMeshCollisionRayHitCallback rayHitCallback, NewtonUserMeshCollisionDestroyCallback destroyCallback, NewtonUserMeshCollisionGetCollisionInfo getInfoCallback, NewtonUserMeshCollisionGetFacesInAABB facesInAABBCallback, int shapeID);
extern "C" void * __cdecl NewtonCreateSceneCollision(const void * NewtonWorld, int shapeID);
extern "C" void * __cdecl NewtonSceneCollisionCreateProxy(void * scene, void * collision);
extern "C" void __cdecl NewtonSceneCollisionDestroyProxy(void * scene, void * Proxy);
extern "C" void __cdecl NewtonSceneProxySetMatrix(void * Proxy, const PdFloat matrix);
extern "C" void __cdecl NewtonSceneProxyGetMatrix(void * Proxy, PdFloat matrix);
extern "C" void __cdecl NewtonSceneSetProxyUserData(const void * Proxy, void * userData);
extern "C" void * __cdecl NewtonSceneGetProxyUserData(const void * Proxy);
extern "C" void * __cdecl NewtonSceneGetFirstProxy(const void * scene);
extern "C" void * __cdecl NewtonSceneGetNextProxy(const void * scene, const void * Proxy);
extern "C" void __cdecl NewtonSceneCollisionOptimize(void * scene);
extern "C" void * __cdecl NewtonCreateCompoundBreakable(const void * NewtonWorld, int meshCount, const void * SolidsArray, const System::PInteger ShapeIDArray, PdFloat Densities, System::PInteger internalFaceMaterial, int shapeID, int debrisID, float DebrisSeparationGap);
extern "C" void __cdecl NewtonCompoundBreakableResetAnchoredPieces(const void * compoundBreakable);
extern "C" void __cdecl NewtonCompoundBreakableSetAnchoredPieces(const void * compoundBreakable, int fixshapesCount, PdFloat matrixPallete, void * fixedShapesArray);
extern "C" int __cdecl NewtonCompoundBreakableGetVertexCount(const void * compoundBreakable);
extern "C" void __cdecl NewtonCompoundBreakableGetVertexStreams(const void * compoundBreakable, int vertexStrideInByte, PdFloat vertex, int normalStrideInByte, PdFloat normal, int uvStrideInByte, PdFloat uv);
extern "C" void * __cdecl NewtonBreakableGetMainMesh(const void * compoundBreakable);
extern "C" void * __cdecl NewtonBreakableGetFirstComponent(const void * compoundBreakable);
extern "C" void * __cdecl NewtonBreakableGetNextComponent(const void * component);
extern "C" void __cdecl NewtonBreakableBeginDelete(const void * compoundBreakable);
extern "C" void * __cdecl NewtonBreakableCreateDebrieBody(const void * compoundBreakable, const void * component);
extern "C" void __cdecl NewtonBreakableDeleteComponent(const void * compoundBreakable, const void * component);
extern "C" void __cdecl NewtonBreakableEndDelete(const void * compoundBreakable);
extern "C" int __cdecl NewtonBreakableGetComponentsInRadius(const void * compoundBreakable, const PdFloat position, float radius, void * Segments, int maxCount);
extern "C" void * __cdecl NewtonBreakableGetFirstSegment(const void * BreakableComponent);
extern "C" void * __cdecl NewtonBreakableGetNextSegment(const void * Segment);
extern "C" int __cdecl NewtonBreakableSegmentGetMaterial(const void * Segment);
extern "C" int __cdecl NewtonBreakableSegmentGetIndexCount(const void * Segment);
extern "C" int __cdecl NewtonBreakableSegmentGetIndexStream(void * compoundBreakable, const void * MeshOwner, const void * Segment, System::PInteger index);
extern "C" int __cdecl NewtonBreakableSegmentGetIndexStreamShort(void * compoundBreakable, const void * MeshOwner, const void * Segment, PShort index);
extern "C" void * __cdecl NewtonCreateCollisionFromSerialization(const void * NewtonWorld, PNewtonDeserialize deserializeFunction, void * serializeHandle);
extern "C" void __cdecl NewtonCollisionSerialize(const void * NewtonWorld, const void * collision, PNewtonSerialize serializeFunction, void * serializeHandle);
extern "C" void __cdecl NewtonCollisionGetInfo(const void * collision, PNewtonCollisionInfoRecord collisionInfo);
extern "C" void * __cdecl NewtonCreateHeightFieldCollision(const void * NewtonWorld, int width, int height, int gridDiagonals, PUnsigned_short elevationMap, P2Char attributeMap, float horizontalScale, float verticalScale, int shapeID);
extern "C" void __cdecl NewtonHeightFieldSetUserRayCastCallback(const void * TreeCollision, PNewtonHeightFieldRayCastCallback rayHitCallback);
extern "C" void * __cdecl NewtonCreateTreeCollision(const void * NewtonWorld, int shapeID);
extern "C" void * __cdecl NewtonCreateTreeCollisionFromMesh(const void * NewtonWorld, const void * Mesh, int shapeID);
extern "C" void __cdecl NewtonTreeCollisionSetUserRayCastCallback(const void * TreeCollision, PNewtonCollisionTreeRayCastCallback rayHitCallback);
extern "C" void __cdecl NewtonTreeCollisionBeginBuild(const void * TreeCollision);
extern "C" void __cdecl NewtonTreeCollisionAddFace(const void * TreeCollision, int vertexCount, const PdFloat vertexPtr, int strideInBytes, int faceAttribute);
extern "C" void __cdecl NewtonTreeCollisionEndBuild(const void * TreeCollision, int optimize);
extern "C" int __cdecl NewtonTreeCollisionGetFaceAtribute(const void * TreeCollision, const System::PInteger faceIndexArray);
extern "C" void __cdecl NewtonTreeCollisionSetFaceAtribute(const void * TreeCollision, const System::PInteger faceIndexArray, int attribute);
extern "C" int __cdecl NewtonTreeCollisionGetVertexListIndexListInAABB(const void * TreeCollision, const PdFloat p0, const PdFloat p1, const PdFloat vertexArray, System::PInteger vertexCount, System::PInteger vertexStrideInBytes, const System::PInteger indexList, int maxIndexCount, const System::PInteger faceAttribute);
extern "C" void __cdecl NewtonStaticCollisionSetDebugCallback(const void * staticCollision, PNewtonTreeCollisionCallback userCallback);
extern "C" int __cdecl NewtonCollisionPointDistance(const void * NewtonWorld, const PdFloat point, const void * collision, const PdFloat matrix, PdFloat contact, PdFloat normal, int threadIndex);
extern "C" int __cdecl NewtonCollisionClosestPoint(const void * NewtonWorld, const void * collsionA, const PdFloat matrixA, const void * collisionB, const PdFloat matrixB, PdFloat contactA, PdFloat contactB, PdFloat normalAB, int threadIndex);
extern "C" int __cdecl NewtonCollisionCollide(const void * NewtonWorld, int maxSize, const void * collsionA, const PdFloat matrixA, const void * collisionB, const PdFloat matrixB, PdFloat contacts, PdFloat normals, PdFloat penetration, int threadIndex);
extern "C" int __cdecl NewtonCollisionCollideContinue(const void * NewtonWorld, int maxSize, const float timestep, const void * collsionA, const PdFloat matrixA, const PdFloat velocA, const float omegaA, const void * collsionB, const PdFloat matrixB, const PdFloat velocB, const float omegaB, PdFloat timeOfImpact, PdFloat contacts, PdFloat normals, PdFloat penetration, int threadIndex);
extern "C" void __cdecl NewtonCollisionSupportVertex(const void * collision, const PdFloat dir, PdFloat vertex);
extern "C" float __cdecl NewtonCollisionRayCast(const void * collision, const PdFloat p0, const PdFloat p1, PdFloat normals, System::PInteger attribute);
extern "C" void __cdecl NewtonCollisionCalculateAABB(const void * collision, const PdFloat matrix, PdFloat p0, PdFloat p1);
extern "C" void __cdecl NewtonCollisionForEachPolygonDo(const void * collision, const PdFloat matrix, NewtonCollisionIterator callback, void * userData);
extern "C" void __cdecl NewtonSetEulerAngle(const PdFloat eulersAngles, PdFloat matrix);
extern "C" void __cdecl NewtonGetEulerAngle(const PdFloat matrix, PdFloat eulersAngles);
extern "C" float __cdecl NewtonCalculateSpringDamperAcceleration(float dt, float ks, float x, float kd, float s);
extern "C" void * __cdecl NewtonCreateBody(const void * NewtonWorld, const void * collision, const PdFloat matrix);
extern "C" void __cdecl NewtonDestroyBody(const void * NewtonWorld, const void * Body);
extern "C" void __cdecl NewtonBodyAddForce(const void * Body, const PdFloat Force);
extern "C" void __cdecl NewtonBodyAddTorque(const void * Body, const PdFloat torque);
extern "C" void __cdecl NewtonBodyCalculateInverseDynamicsForce(const void * Body, float timestep, const PdFloat desiredVeloc, PdFloat forceOut);
extern "C" void __cdecl NewtonBodySetMatrix(const void * Body, const PdFloat matrix);
extern "C" void __cdecl NewtonBodySetMatrixRecursive(const void * Body, const PdFloat matrix);
extern "C" void __cdecl NewtonBodySetMassMatrix(const void * Body, float mass, float Ixx, float Iyy, float Izz);
extern "C" void __cdecl NewtonBodySetMaterialGroupID(const void * Body, int id);
extern "C" void __cdecl NewtonBodySetContinuousCollisionMode(const void * Body, int state);
extern "C" void __cdecl NewtonBodySetJointRecursiveCollision(const void * Body, unsigned state);
extern "C" void __cdecl NewtonBodySetOmega(const void * Body, const PdFloat omega);
extern "C" void __cdecl NewtonBodySetVelocity(const void * Body, const PdFloat velocity);
extern "C" void __cdecl NewtonBodySetForce(const void * Body, const PdFloat Force);
extern "C" void __cdecl NewtonBodySetTorque(const void * Body, const PdFloat torque);
extern "C" void __cdecl NewtonBodySetCentreOfMass(const void * Body, const PdFloat com);
extern "C" void __cdecl NewtonBodySetLinearDamping(const void * Body, float linearDamp);
extern "C" void __cdecl NewtonBodySetAngularDamping(const void * Body, const PdFloat angularDamp);
extern "C" void __cdecl NewtonBodySetUserData(const void * Body, void * userData);
extern "C" void __cdecl NewtonBodySetCollision(const void * Body, const void * collision);
extern "C" int __cdecl NewtonBodyGetSleepState(const void * Body);
extern "C" int __cdecl NewtonBodyGetAutoSleep(const void * Body);
extern "C" void __cdecl NewtonBodySetAutoSleep(const void * Body, int state);
extern "C" int __cdecl NewtonBodyGetFreezeState(const void * Body);
extern "C" void __cdecl NewtonBodySetFreezeState(const void * Body, int state);
extern "C" void __cdecl NewtonBodySetDestructorCallback(const void * Body, NewtonBodyDestructor callback);
extern "C" void __cdecl NewtonBodySetTransformCallback(const void * Body, NewtonSetTransform callback);
extern "C" NewtonSetTransform __cdecl NewtonBodyGetTransformCallback(const void * Body);
extern "C" void __cdecl NewtonBodySetForceAndTorqueCallback(const void * Body, NewtonApplyForceAndTorque callback);
extern "C" NewtonApplyForceAndTorque __cdecl NewtonBodyGetForceAndTorqueCallback(const void * Body);
extern "C" void * __cdecl NewtonBodyGetUserData(const void * Body);
extern "C" void * __cdecl NewtonBodyGetWorld(const void * Body);
extern "C" void * __cdecl NewtonBodyGetCollision(const void * Body);
extern "C" int __cdecl NewtonBodyGetMaterialGroupID(const void * Body);
extern "C" int __cdecl NewtonBodyGetContinuousCollisionMode(const void * Body);
extern "C" int __cdecl NewtonBodyGetJointRecursiveCollision(const void * Body);
extern "C" void __cdecl NewtonBodyGetMatrix(const void * Body, PdFloat matrix);
extern "C" void __cdecl NewtonBodyGetRotation(const void * Body, PdFloat rotation);
extern "C" void __cdecl NewtonBodyGetMassMatrix(const void * Body, PdFloat mass, PdFloat Ixx, PdFloat Iyy, PdFloat Izz);
extern "C" void __cdecl NewtonBodyGetInvMass(const void * Body, PdFloat invMass, PdFloat invIxx, PdFloat invIyy, PdFloat invIzz);
extern "C" void __cdecl NewtonBodyGetOmega(const void * Body, PdFloat vector);
extern "C" void __cdecl NewtonBodyGetVelocity(const void * Body, PdFloat vector);
extern "C" void __cdecl NewtonBodyGetForce(const void * Body, PdFloat vector);
extern "C" void __cdecl NewtonBodyGetTorque(const void * Body, PdFloat vector);
extern "C" void __cdecl NewtonBodyGetForceAcc(const void * Body, PdFloat vector);
extern "C" void __cdecl NewtonBodyGetTorqueAcc(const void * Body, PdFloat vector);
extern "C" void __cdecl NewtonBodyGetCentreOfMass(const void * Body, PdFloat com);
extern "C" float __cdecl NewtonBodyGetLinearDamping(const void * Body);
extern "C" void __cdecl NewtonBodyGetAngularDamping(const void * Body, PdFloat vector);
extern "C" void __cdecl NewtonBodyGetAABB(const void * Body, PdFloat p0, PdFloat p1);
extern "C" void __cdecl NewtonBodyGetFreezeTreshold(const void * Body, PdFloat freezeSpeed2, PdFloat freezeOmega2);
extern "C" void * __cdecl NewtonBodyGetFirstJoint(const void * Body);
extern "C" void * __cdecl NewtonBodyGetNextJoint(const void * Body, const void * joint);
extern "C" void * __cdecl NewtonBodyGetFirstContactJoint(const void * Body);
extern "C" void * __cdecl NewtonBodyGetNextContactJoint(const void * Body, const void * contactJoint);
extern "C" void * __cdecl NewtonContactJointGetFirstContact(const void * contactJoint);
extern "C" void * __cdecl NewtonContactJointGetNextContact(const void * contactJoint, void * contact);
extern "C" int __cdecl NewtonContactJointGetContactCount(const void * contactJoint);
extern "C" void __cdecl NewtonContactJointRemoveContact(const void * contactJoint, void * contact);
extern "C" void * __cdecl NewtonContactGetMaterial(const void * contact);
extern "C" void __cdecl NewtonBodyAddBuoyancyForce(const void * Body, float fluidDensity, float fluidLinearViscosity, float fluidAngularViscosity, const PdFloat gravityVector, NewtonGetBuoyancyPlane buoyancyPlane, void * context);
extern "C" void __cdecl NewtonBodyAddImpulse(const void * Body, const PdFloat pointDeltaVeloc, const PdFloat pointPosit);
extern "C" void __cdecl NewtonBodyApplyImpulseArray(const void * Body, int ImpuleCount, int StrideInByte, const PdFloat impulseArray, const PdFloat pointArray);
extern "C" void * __cdecl NewtonJointGetUserData(const void * joint);
extern "C" void __cdecl NewtonJointSetUserData(const void * joint, void * userData);
extern "C" void * __cdecl NewtonJointGetBody0(const void * joint);
extern "C" void * __cdecl NewtonJointGetBody1(const void * joint);
extern "C" void __cdecl NewtonJointGetInfo(const void * joint, PNewtonJointRecord info);
extern "C" int __cdecl NewtonJointGetCollisionState(const void * joint);
extern "C" void __cdecl NewtonJointSetCollisionState(const void * joint, int state);
extern "C" float __cdecl NewtonJointGetStiffness(const void * joint);
extern "C" void __cdecl NewtonJointSetStiffness(const void * joint, float state);
extern "C" void __cdecl NewtonDestroyJoint(const void * NewtonWorld, const void * joint);
extern "C" void __cdecl NewtonJointSetDestructor(const void * joint, NewtonConstraintDestructor _destructor);
extern "C" void * __cdecl NewtonConstraintCreateBall(const void * NewtonWorld, const PdFloat pivotPoint, const void * childBody, const void * parentBody);
extern "C" void __cdecl NewtonBallSetUserCallback(const void * ball, NewtonBallCallBack callback);
extern "C" void __cdecl NewtonBallGetJointAngle(const void * ball, PdFloat angle);
extern "C" void __cdecl NewtonBallGetJointOmega(const void * ball, PdFloat omega);
extern "C" void __cdecl NewtonBallGetJointForce(const void * ball, PdFloat Force);
extern "C" void __cdecl NewtonBallSetConeLimits(const void * ball, const PdFloat pin, float maxConeAngle, float maxTwistAngle);
extern "C" void * __cdecl NewtonConstraintCreateHinge(const void * NewtonWorld, const PdFloat pivotPoint, const PdFloat pinDir, const void * childBody, const void * parentBody);
extern "C" void __cdecl NewtonHingeSetUserCallback(const void * hinge, NewtonHingeCallBack callback);
extern "C" float __cdecl NewtonHingeGetJointAngle(const void * hinge);
extern "C" float __cdecl NewtonHingeGetJointOmega(const void * hinge);
extern "C" void __cdecl NewtonHingeGetJointForce(const void * hinge, PdFloat Force);
extern "C" float __cdecl NewtonHingeCalculateStopAlpha(const void * hinge, const PNewtonHingeSliderUpdateDesc desc, float angle);
extern "C" void * __cdecl NewtonConstraintCreateSlider(const void * NewtonWorld, const PdFloat pivotPoint, const PdFloat pinDir, const void * childBody, const void * parentBody);
extern "C" void __cdecl NewtonSliderSetUserCallback(const void * slider, NewtonSliderCallBack callback);
extern "C" float __cdecl NewtonSliderGetJointPosit(const void * slider);
extern "C" float __cdecl NewtonSliderGetJointVeloc(const void * slider);
extern "C" void __cdecl NewtonSliderGetJointForce(const void * slider, PdFloat Force);
extern "C" float __cdecl NewtonSliderCalculateStopAccel(const void * slider, const PNewtonHingeSliderUpdateDesc desc, float position);
extern "C" void * __cdecl NewtonConstraintCreateCorkscrew(const void * NewtonWorld, const PdFloat pivotPoint, const PdFloat pinDir, const void * childBody, const void * parentBody);
extern "C" void __cdecl NewtonCorkscrewSetUserCallback(const void * corkscrew, NewtonCorkscrewCallBack callback);
extern "C" float __cdecl NewtonCorkscrewGetJointPosit(const void * corkscrew);
extern "C" float __cdecl NewtonCorkscrewGetJointAngle(const void * corkscrew);
extern "C" float __cdecl NewtonCorkscrewGetJointVeloc(const void * corkscrew);
extern "C" float __cdecl NewtonCorkscrewGetJointOmega(const void * corkscrew);
extern "C" void __cdecl NewtonCorkscrewGetJointForce(const void * corkscrew, PdFloat Force);
extern "C" float __cdecl NewtonCorkscrewCalculateStopAlpha(const void * corkscrew, const PNewtonHingeSliderUpdateDesc desc, float angle);
extern "C" float __cdecl NewtonCorkscrewCalculateStopAccel(const void * corkscrew, const PNewtonHingeSliderUpdateDesc desc, float position);
extern "C" void * __cdecl NewtonConstraintCreateUniversal(const void * NewtonWorld, const PdFloat pivotPoint, const PdFloat pinDir0, const PdFloat pinDir1, const void * childBody, const void * parentBody);
extern "C" void __cdecl NewtonUniversalSetUserCallback(const void * universal, NewtonUniversalCallBack callback);
extern "C" float __cdecl NewtonUniversalGetJointAngle0(const void * universal);
extern "C" float __cdecl NewtonUniversalGetJointAngle1(const void * universal);
extern "C" float __cdecl NewtonUniversalGetJointOmega0(const void * universal);
extern "C" float __cdecl NewtonUniversalGetJointOmega1(const void * universal);
extern "C" void __cdecl NewtonUniversalGetJointForce(const void * universal, PdFloat Force);
extern "C" float __cdecl NewtonUniversalCalculateStopAlpha0(const void * universal, const PNewtonHingeSliderUpdateDesc desc, float angle);
extern "C" float __cdecl NewtonUniversalCalculateStopAlpha1(const void * universal, const PNewtonHingeSliderUpdateDesc desc, float angle);
extern "C" void * __cdecl NewtonConstraintCreateUpVector(const void * NewtonWorld, const PdFloat pinDir, const void * Body);
extern "C" void __cdecl NewtonUpVectorGetPin(const void * upVector, PdFloat pin);
extern "C" void __cdecl NewtonUpVectorSetPin(const void * upVector, const PdFloat pin);
extern "C" void * __cdecl NewtonConstraintCreateUserJoint(const void * NewtonWorld, int MaxDOF, PNewtonUserBilateralCallBack callback, PNewtonUserBilateralGetInfoCallBack GetInfo, const void * childBody, const void * parentBody);
extern "C" void __cdecl NewtonUserJointSetFeedbackCollectorCallback(const void * joint, PNewtonUserBilateralCallBack GetFeedback);
extern "C" void __cdecl NewtonUserJointAddLinearRow(const void * joint, const PdFloat pivot0, const PdFloat pivot1, const PdFloat dir);
extern "C" void __cdecl NewtonUserJointAddAngularRow(const void * joint, float RelativeAngle, const PdFloat dir);
extern "C" void __cdecl NewtonUserJointAddGeneralRow(const void * joint, const PdFloat Jacobian0, const PdFloat Jacobian1);
extern "C" void __cdecl NewtonUserJointSetRowMinimumFriction(const void * joint, float Friction);
extern "C" void __cdecl NewtonUserJointSetRowMaximumFriction(const void * joint, float Friction);
extern "C" void __cdecl NewtonUserJointSetRowAcceleration(const void * joint, float Acceleration);
extern "C" void __cdecl NewtonUserJointSetRowSpringDamperAcceleration(const void * joint, float springK, float springD);
extern "C" void __cdecl NewtonUserJointSetRowStiffness(const void * joint, float Stiffness);
extern "C" float __cdecl NewtonUserJointGetRowForce(const void * joint, int Row);
extern "C" void * __cdecl NewtonMeshCreate(const void * World);
extern "C" void * __cdecl NewtonMeshCreateFromMesh(const void * Mesh);
extern "C" void * __cdecl NewtonMeshCreateFromCollision(const void * collision);
extern "C" void * __cdecl NewtonMeshConvexHull(const void * NewtonWorld, int count, const PdFloat vertexCloud, int strideInBytes, float tolerance);
extern "C" void * __cdecl NewtonMeshCreatePlane(const void * World, const PdFloat locationMatrix, float width, float breadth, int material, const PdFloat textureMatrix0, const void *textureMatrix1);
extern "C" void __cdecl NewtonMeshDestroy(const void * Mesh);
extern "C" void __cdecl NewtonMeshCalculateOOBB(const void * Mesh, const PdFloat matrix, PdFloat x, PdFloat y, PdFloat z);
extern "C" void __cdecl NewtonMesApplyTransform(const void * Mesh, const PdFloat matrix);
extern "C" void __cdecl NewtonMeshCalculateVertexNormals(const void * Mesh, float angleInRadians);
extern "C" void __cdecl NewtonMeshApplySphericalMapping(const void * Mesh, int material);
extern "C" void __cdecl NewtonMeshApplyBoxMapping(const void * Mesh, int front, int side, int top);
extern "C" void __cdecl NewtonMeshApplyCylindricalMapping(const void * Mesh, int cylinderMaterial, int capMaterial);
extern "C" int __cdecl NewtonMeshIsOpenMesh(const void * Mesh);
extern "C" void __cdecl NewtonMeshFixTJoints(const void * Mesh);
extern "C" void __cdecl NewtonMeshPolygonize(const void * Mesh);
extern "C" void __cdecl NewtonMeshTriangulate(const void * Mesh);
extern "C" void * __cdecl NewtonMeshUnion(const void * Mesh, void * clipper, PdFloat clipperMatrix);
extern "C" void * __cdecl NewtonMeshDifference(const void * Mesh, void * clipper, PdFloat clipperMatrix);
extern "C" void * __cdecl NewtonMeshIntersection(const void * Mesh, void * clipper, PdFloat clipperMatrix);
extern "C" void __cdecl NewtonMeshClip(const void * Mesh, const void * clipper, const PdFloat clipperMatrix, const void * topMesh, const void * bottomMesh);
extern "C" void __cdecl NewtonMeshPlaneClip(const void * Mesh, const PdFloat planeMatrix, const PdFloat PlaneTextureMatrix, int PlaneMaterial, const void * topMesh, const void * bottomMesh);
extern "C" void * __cdecl NewtonMeshConvexDecomposition(const void * Mesh, int maxCount);
extern "C" void * __cdecl NewtonMeshVoronoiDecomposition(const void * Mesh, int PointCount, int PointStrideInBytes, const PdFloat PointCloud, int InternalMaterial, const PdFloat TextureMatrix);
extern "C" void __cdecl NewtonRemoveUnusedVertices(const void * Mesh, System::PInteger vertexRemapTable);
extern "C" void __cdecl NewtonMeshBeginFace(const void * Mesh);
extern "C" void __cdecl NewtonMeshAddFace(const void * Mesh, int vertexCount, const PdFloat vertex, int strideInBytes, int materialIndex);
extern "C" void __cdecl NewtonMeshEndFace(const void * Mesh);
extern "C" void __cdecl NewtonMeshBuildFromVertexListIndexList(const void * Mesh, int FaceCount, const System::PInteger faceIndexCount, const System::PInteger faceMaterialIndex, const PdFloat vertex, int vertexStrideInBytes, const System::PInteger vertexIndex, const PdFloat normal, int normalStrideInBytes, const System::PInteger normalIndex, const PdFloat uv0, int uv0StrideInBytes, const System::PInteger uv0Index, const PdFloat uv1, int uv1StrideInBytes, const System::PInteger uv1Index);
extern "C" void __cdecl NewtonMeshGetVertexStreams(const void * Mesh, int vertexStrideInByte, PdFloat vertex, int normalStrideInByte, PdFloat normal, int uvStrideInByte0, PdFloat uv0, int uvStrideInByte1, PdFloat uv1);
extern "C" void __cdecl NewtonMeshGetIndirectVertexStreams(const void * Mesh, int vertexStrideInByte, PdFloat vertex, System::PInteger vertexIndices, System::PInteger vertexCount, int normalStrideInByte, PdFloat normal, System::PInteger normalIndices, System::PInteger normalCount, int uvStrideInByte0, PdFloat uv0, System::PInteger uvIndices0, System::PInteger uvCount0, int uvStrideInByte1, PdFloat uv1, System::PInteger uvIndices1, System::PInteger uvCount1);
extern "C" void * __cdecl NewtonMeshBeginHandle(const void * Mesh);
extern "C" void __cdecl NewtonMeshEndHandle(const void * Mesh, void * Handle);
extern "C" int __cdecl NewtonMeshFirstMaterial(const void * Mesh, void * Handle);
extern "C" int __cdecl NewtonMeshNextMaterial(const void * Mesh, void * Handle, int materialID);
extern "C" int __cdecl NewtonMeshMaterialGetMaterial(const void * Mesh, void * Handle, int materialID);
extern "C" int __cdecl NewtonMeshMaterialGetIndexCount(const void * Mesh, void * Handle, int materialID);
extern "C" void __cdecl NewtonMeshMaterialGetIndexStream(const void * Mesh, void * Handle, int materialID, System::PInteger index);
extern "C" void __cdecl NewtonMeshMaterialGetIndexStreamShort(const void * Mesh, void * Handle, int materialID, PShort index);
extern "C" void * __cdecl NewtonMeshCreateFirstSingleSegment(const void * Mesh);
extern "C" void * __cdecl NewtonMeshCreateNextSingleSegment(const void * Mesh, void * Segment);
extern "C" void * __cdecl NewtonMeshCreateFirstLayer(const void * Mesh);
extern "C" void * __cdecl NewtonMeshCreateNextLayer(const void * Mesh, const void * Segment);
extern "C" int __cdecl NewtonMeshGetTotalFaceCount(const void * Mesh);
extern "C" int __cdecl NewtonMeshGetTotalIndexCount(const void * Mesh);
extern "C" void __cdecl NewtonMeshGetFaces(const void * Mesh, const System::PInteger faceIndexCount, System::PInteger faceMaterial, System::PInteger faceIndices);
extern "C" int __cdecl NewtonMeshGetPointCount(const void * Mesh);
extern "C" int __cdecl NewtonMeshGetPointStrideInByte(const void * Mesh);
extern "C" PdFloat __cdecl NewtonMeshGetPointArray(const void * Mesh);
extern "C" PdFloat __cdecl NewtonMeshGetNormalArray(const void * Mesh);
extern "C" PdFloat __cdecl NewtonMeshGetUV0Array(const void * Mesh);
extern "C" PdFloat __cdecl NewtonMeshGetUV1Array(const void * Mesh);
extern "C" int __cdecl NewtonMeshGetVertexCount(const void * Mesh);
extern "C" int __cdecl NewtonMeshGetVertexStrideInByte(const void * Mesh);
extern "C" PdFloat __cdecl NewtonMeshGetVertexArray(const void * Mesh);
extern "C" void * __cdecl NewtonMeshGetFirstVertex(const void * Mesh);
extern "C" void * __cdecl NewtonMeshGetNextVertex(const void * Mesh, const void * vertex);
extern "C" int __cdecl NewtonMeshGetVertexIndex(const void * Mesh, const void * vertex);
extern "C" void * __cdecl NewtonMeshGetFirstPoint(const void * Mesh);
extern "C" void * __cdecl NewtonMeshGetNextPoint(const void * Mesh, const void * point);
extern "C" int __cdecl NewtonMeshGetPointIndex(const void * Mesh, const void * point);
extern "C" int __cdecl NewtonMeshGetVertexIndexFromPoint(const void * Mesh, const void * point);
extern "C" void * __cdecl NewtonMeshGetFirstEdge(const void * Mesh);
extern "C" void * __cdecl NewtonMeshGetNextEdge(const void * Mesh, const void * Edge);
extern "C" void __cdecl NewtonMeshGetEdgeIndices(const void * Mesh, const void * Edge, System::PInteger v0, System::PInteger v1);
extern "C" void * __cdecl NewtonMeshGetFirstFace(const void * Mesh);
extern "C" void * __cdecl NewtonMeshGetNextFace(const void * Mesh, const void * face);
extern "C" int __cdecl NewtonMeshIsFaceOpen(const void * Mesh, const void * face);
extern "C" int __cdecl NewtonMeshGetFaceMaterial(const void * Mesh, const void * face);
extern "C" int __cdecl NewtonMeshGetFaceIndexCount(const void * Mesh, const void * face);
extern "C" void __cdecl NewtonMeshGetFaceIndices(const void * Mesh, const void * face, System::PInteger Indices);
extern "C" void __cdecl NewtonMeshGetFacePointIndices(const void * Mesh, const void * face, System::PInteger Indices);
}	/* namespace Ngdimport */
}	/* namespace Physics */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS_NGDIMPORT)
using namespace Physics::Ngdimport;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS)
using namespace Physics;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Physics_NgdimportHPP
