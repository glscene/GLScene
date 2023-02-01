// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Physics.NewtonImport.pas' rev: 35.00 (Windows)

#ifndef Physics_NewtonimportHPP
#define Physics_NewtonimportHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Physics
{
namespace Newtonimport
{
//-- forward type declarations -----------------------------------------------
struct TNewtonFuncLinkRecord;
struct TNewtonMaterialData;
struct TNewtonCollisionMaterial;
struct TNewtonBoxParam;
struct TNewtonSphereParam;
struct TNewtonCapsuleParam;
struct TNewtonCylinderParam;
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
struct TNewtonJointRecord;
struct TNewtonUserMeshCollisionCollideDesc;
struct TNewtonWorldConvexCastReturnInfo;
struct TNewtonUserMeshCollisionRayHitDesc;
struct TNewtonHingeSliderUpdateDesc;
struct TNewtonUserContactPoint;
struct TNewtonImmediateModeConstraint;
struct TNewtonMeshDoubleData;
struct TNewtonMeshFloatData;
struct TNewtonMeshVertexFormat;
//-- type declarations -------------------------------------------------------
typedef float Float;

typedef float dFloat;

typedef float *PFloat;

typedef float *PdFloat;

typedef int *PInteger;

typedef short *Psmallint;

typedef void * *PPointer;

typedef double dFloat64;

typedef double *PdFloat64;

typedef __int64 dLong;

typedef __int64 *PdLong;

typedef void * *PNewtonMesh;

typedef void * *PNewtonBody;

typedef void * *PNewtonWorld;

typedef void * *PNewtonJoint;

typedef void * *PNewtonMaterial;

typedef void * *PNewtonCollision;

typedef void * *PNewtonSceneProxy;

typedef void * *PNewtonFracturedCompoundMeshPart;

typedef void * *PNewtonDeformableMeshSegment;

typedef void * *PNewtonAcyclicArticulation;

typedef void * *PNewtonInverseDynamics;

struct DECLSPEC_DRECORD TNewtonFuncLinkRecord
{
public:
	System::WideString function_name;
	void *function_ptr;
};


#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonMaterialData
{
	
public:
	union
	{
		struct 
		{
			float m_float;
		};
		struct 
		{
			__int64 m_int;
		};
		struct 
		{
			void *m_ptr;
		};
		
	};
};
#pragma pack(pop)


typedef TNewtonMaterialData *PNewtonMaterialData;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonCollisionMaterial
{
public:
	__int64 m_userId;
	TNewtonMaterialData m_userData;
	System::StaticArray<TNewtonMaterialData, 6> m_userParam;
};
#pragma pack(pop)


typedef TNewtonCollisionMaterial *PNewtonCollisionMaterial;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonBoxParam
{
public:
	float m_x;
	float m_y;
	float m_z;
};
#pragma pack(pop)


typedef TNewtonBoxParam *PNewtonBoxParam;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonSphereParam
{
public:
	float m_radio;
};
#pragma pack(pop)


typedef TNewtonSphereParam *PNewtonSphereParam;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonCapsuleParam
{
public:
	float m_radio0;
	float m_radio1;
	float m_height;
};
#pragma pack(pop)


typedef TNewtonCapsuleParam *PNewtonCapsuleParam;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonCylinderParam
{
public:
	float m_radio0;
	float m_radio1;
	float m_height;
};
#pragma pack(pop)


typedef TNewtonCylinderParam *PNewtonCylinderParam;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonConeParam
{
public:
	float m_radio;
	float m_height;
};
#pragma pack(pop)


typedef TNewtonConeParam *PNewtonConeParam;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonTaperedCapsuleParam
{
public:
	float m_radio0;
	float m_radio1;
	float m_height;
};
#pragma pack(pop)


typedef TNewtonTaperedCapsuleParam *PNewtonTaperedCapsuleParam;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonTaperedCylinderParam
{
public:
	float m_radio0;
	float m_radio1;
	float m_height;
};
#pragma pack(pop)


typedef TNewtonTaperedCylinderParam *PNewtonTaperedCylinderParam;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonChamferCylinderParam
{
public:
	float m_radio;
	float m_height;
};
#pragma pack(pop)


typedef TNewtonChamferCylinderParam *PNewtonChamferCylinderParam;

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


typedef TNewtonConvexHullParam *PNewtonConvexHullParam;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonCompoundCollisionParam
{
public:
	int m_chidrenCount;
};
#pragma pack(pop)


typedef TNewtonCompoundCollisionParam *PNewtonCompoundCollisionParam;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonCollisionTreeParam
{
public:
	int m_vertexCount;
	int m_indexCount;
};
#pragma pack(pop)


typedef TNewtonCollisionTreeParam *PNewtonCollisionTreeParam;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonDeformableMeshParam
{
public:
	int m_vertexCount;
	int m_triangleCount;
	int m_vrtexStrideInBytes;
	System::Word *m_indexList;
	float *m_vertexList;
};
#pragma pack(pop)


typedef TNewtonDeformableMeshParam *PNewtonDeformableMeshParam;

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
	System::Int8 *m_atributes;
};
#pragma pack(pop)


typedef TNewtonHeightFieldCollisionParam *PNewtonHeightFieldCollisionParam;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonSceneCollisionParam
{
public:
	int m_childrenProxyCount;
};
#pragma pack(pop)


typedef TNewtonSceneCollisionParam *PNewtonSceneCollisionParam;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonCollisionInfoRecord
{
public:
	System::StaticArray<System::StaticArray<float, 4>, 4> m_offsetMatrix;
	TNewtonCollisionMaterial m_collisionMaterial;
	int m_collisionType;
	int m_collisionUserID;
	
public:
	union
	{
		struct 
		{
			System::StaticArray<float, 64> m_paramArray;
		};
		struct 
		{
			TNewtonSceneCollisionParam shapedataSceneCollision;
		};
		struct 
		{
			TNewtonHeightFieldCollisionParam shapedataHeightField;
		};
		struct 
		{
			TNewtonCollisionTreeParam shapedataTree;
		};
		struct 
		{
			TNewtonCompoundCollisionParam shapedataCompound;
		};
		struct 
		{
			TNewtonDeformableMeshParam m_deformableMesh;
		};
		struct 
		{
			TNewtonConvexHullParam shapedataConvexHull;
		};
		struct 
		{
			TNewtonChamferCylinderParam shapedataChamferCylinder;
		};
		struct 
		{
			TNewtonCylinderParam shapedataCylinder;
		};
		struct 
		{
			TNewtonCapsuleParam shapedataCapsule;
		};
		struct 
		{
			TNewtonSphereParam shapedataSphere;
		};
		struct 
		{
			TNewtonConeParam shapedataCone;
		};
		struct 
		{
			TNewtonBoxParam shapedataBox;
		};
		
	};
};
#pragma pack(pop)


typedef TNewtonCollisionInfoRecord *PNewtonCollisionInfoRecord;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonJointRecord
{
public:
	System::StaticArray<System::StaticArray<float, 4>, 4> m_attachmenMatrix_0;
	System::StaticArray<System::StaticArray<float, 4>, 4> m_attachmenMatrix_1;
	System::StaticArray<float, 3> m_minLinearDof;
	System::StaticArray<float, 3> m_maxLinearDof;
	System::StaticArray<float, 3> m_minAngularDof;
	System::StaticArray<float, 3> m_maxAngularDof;
	void * *m_attachBody_0;
	void * *m_attachBody_1;
	System::StaticArray<float, 64> m_extraParameters;
	int m_bodiesCollisionOn;
	System::StaticArray<char, 128> m_descriptionType;
};
#pragma pack(pop)


typedef TNewtonJointRecord *PNewtonJointRecord;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonUserMeshCollisionCollideDesc
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
	void * *m_objBody;
	void * *m_polySoupBody;
	void * *m_objCollision;
	void * *m_polySoupCollision;
	float *m_vertex;
	int *m_faceIndexCount;
	int *m_faceVertexIndex;
};
#pragma pack(pop)


typedef TNewtonUserMeshCollisionCollideDesc *PNewtonUserMeshCollisionCollideDesc;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonWorldConvexCastReturnInfo
{
public:
	System::StaticArray<float, 4> m_point;
	System::StaticArray<float, 4> m_normal;
	int m_contactID;
	void * *m_hitBody;
	float m_penetration;
};
#pragma pack(pop)


typedef TNewtonWorldConvexCastReturnInfo *PNewtonWorldConvexCastReturnInfo;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonUserMeshCollisionRayHitDesc
{
public:
	System::StaticArray<float, 4> m_p0;
	System::StaticArray<float, 4> m_p1;
	System::StaticArray<float, 4> m_normalOut;
	__int64 m_userIdOut;
	void *m_userData;
};
#pragma pack(pop)


typedef TNewtonUserMeshCollisionRayHitDesc *PNewtonUserMeshCollisionRayHitDesc;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonHingeSliderUpdateDesc
{
public:
	float m_accel;
	float m_minFriction;
	float m_maxFriction;
	float m_timestep;
};
#pragma pack(pop)


typedef TNewtonHingeSliderUpdateDesc *PNewtonHingeSliderUpdateDesc;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonUserContactPoint
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


typedef TNewtonUserContactPoint *PNewtonUserContactPoint;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonImmediateModeConstraint
{
public:
	System::StaticArray<System::StaticArray<float, 6>, 8> m_jacobian01;
	System::StaticArray<System::StaticArray<float, 6>, 8> m_jacobian10;
	System::StaticArray<float, 8> m_minFriction;
	System::StaticArray<float, 8> m_maxFriction;
	System::StaticArray<float, 8> m_jointAccel;
	System::StaticArray<float, 8> m_jointStiffness;
};
#pragma pack(pop)


typedef TNewtonImmediateModeConstraint *PNewtonImmediateModeConstraint;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonMeshDoubleData
{
public:
	double *m_data;
	int *m_indexList;
	int m_strideInBytes;
};
#pragma pack(pop)


typedef TNewtonMeshDoubleData *PNewtonMeshDoubleData;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonMeshFloatData
{
public:
	float *m_data;
	int *m_indexList;
	int m_strideInBytes;
};
#pragma pack(pop)


typedef TNewtonMeshFloatData *PNewtonMeshFloatData;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TNewtonMeshVertexFormat
{
public:
	int m_faceCount;
	int *m_faceIndexCount;
	int *m_faceMaterial;
	TNewtonMeshDoubleData m_vertex;
	TNewtonMeshFloatData m_normal;
	TNewtonMeshFloatData m_binormal;
	TNewtonMeshFloatData m_uv0;
	TNewtonMeshFloatData m_uv1;
	TNewtonMeshFloatData m_vertexColor;
};
#pragma pack(pop)


typedef TNewtonMeshVertexFormat *PNewtonMeshVertexFormat;

typedef void * __cdecl (*NewtonAllocMemory)(int sizeInBytes);

typedef NewtonAllocMemory *PNewtonAllocMemory;

typedef void __cdecl (*NewtonFreeMemory)(const void * ptr, int sizeInBytes);

typedef NewtonFreeMemory *PNewtonFreeMemory;

typedef void __cdecl (*NewtonWorldDestructorCallback)(const PNewtonWorld world);

typedef NewtonWorldDestructorCallback *PNewtonWorldDestructorCallback;

typedef void __cdecl (*NewtonPostUpdateCallback)(const PNewtonWorld world, float timestep);

typedef NewtonPostUpdateCallback *PNewtonPostUpdateCallback;

typedef void __cdecl (*NewtonCreateContactCallback)(const PNewtonWorld newtonWorld, const PNewtonJoint contact);

typedef NewtonCreateContactCallback *PNewtonCreateContactCallback;

typedef void __cdecl (*NewtonDestroyContactCallback)(const PNewtonWorld newtonWorld, const PNewtonJoint contact);

typedef NewtonDestroyContactCallback *PNewtonDestroyContactCallback;

typedef void __cdecl (*NewtonWorldListenerDebugCallback)(const PNewtonWorld world, const void * listener, const void * debugContext);

typedef NewtonWorldListenerDebugCallback *PNewtonWorldListenerDebugCallback;

typedef void __cdecl (*NewtonWorldListenerBodyDestroyCallback)(const PNewtonWorld world, const void * listenerUserData, const PNewtonBody body);

typedef NewtonWorldListenerBodyDestroyCallback *PNewtonWorldListenerBodyDestroyCallback;

typedef void __cdecl (*NewtonWorldUpdateListenerCallback)(const PNewtonWorld world, const void * listenerUserData, float timestep);

typedef NewtonWorldUpdateListenerCallback *PNewtonWorldUpdateListenerCallback;

typedef void __cdecl (*NewtonWorldDestroyListenerCallback)(const PNewtonWorld world, const void * listenerUserData);

typedef NewtonWorldDestroyListenerCallback *PNewtonWorldDestroyListenerCallback;

typedef __int64 __cdecl (*NewtonGetTimeInMicrosencondsCallback)(void);

typedef NewtonGetTimeInMicrosencondsCallback *PNewtonGetTimeInMicrosencondsCallback;

typedef void __cdecl (*NewtonSerializeCallback)(const void * serializeHandle, const void * buffer, int size);

typedef NewtonSerializeCallback *PNewtonSerializeCallback;

typedef void __cdecl (*NewtonDeserializeCallback)(const void * serializeHandle, const void * buffer, int size);

typedef NewtonDeserializeCallback *PNewtonDeserializeCallback;

typedef void __cdecl (*NewtonOnBodySerializationCallback)(const PNewtonBody body, const void * userData, PNewtonSerializeCallback functionparam, const void * serializeHandle);

typedef NewtonOnBodySerializationCallback *PNewtonOnBodySerializationCallback;

typedef void __cdecl (*NewtonOnBodyDeserializationCallback)(const PNewtonBody body, const void * userData, PNewtonDeserializeCallback functionparam, const void * serializeHandle);

typedef NewtonOnBodyDeserializationCallback *PNewtonOnBodyDeserializationCallback;

typedef void __cdecl (*NewtonOnJointSerializationCallback)(const PNewtonJoint joint, PNewtonSerializeCallback functionparam, const void * serializeHandle);

typedef NewtonOnJointSerializationCallback *PNewtonOnJointSerializationCallback;

typedef void __cdecl (*NewtonOnJointDeserializationCallback)(const PNewtonBody body0, const PNewtonBody body1, PNewtonDeserializeCallback functionparam, const void * serializeHandle);

typedef NewtonOnJointDeserializationCallback *PNewtonOnJointDeserializationCallback;

typedef void __cdecl (*NewtonOnUserCollisionSerializationCallback)(const void * userData, PNewtonSerializeCallback functionparam, const void * serializeHandle);

typedef NewtonOnUserCollisionSerializationCallback *PNewtonOnUserCollisionSerializationCallback;

typedef void __cdecl (*NewtonUserMeshCollisionDestroyCallback)(const void * userData);

typedef NewtonUserMeshCollisionDestroyCallback *PNewtonUserMeshCollisionDestroyCallback;

typedef float __cdecl (*NewtonUserMeshCollisionRayHitCallback)(const PNewtonUserMeshCollisionRayHitDesc lineDescData);

typedef NewtonUserMeshCollisionRayHitCallback *PNewtonUserMeshCollisionRayHitCallback;

typedef void __cdecl (*NewtonUserMeshCollisionGetCollisionInfo)(const void * userData, const PNewtonCollisionInfoRecord infoRecord);

typedef NewtonUserMeshCollisionGetCollisionInfo *PNewtonUserMeshCollisionGetCollisionInfo;

typedef int __cdecl (*NewtonUserMeshCollisionAABBTest)(const void * userData, const PFloat boxP0, const PFloat boxP1);

typedef NewtonUserMeshCollisionAABBTest *PNewtonUserMeshCollisionAABBTest;

typedef int __cdecl (*NewtonUserMeshCollisionGetFacesInAABB)(const void * userData, const PFloat p0, const PFloat p1, const PFloat vertexArray, const PInteger vertexCount, const PInteger vertexStrideInBytes, const PInteger indexList, int maxIndexCount, const PInteger userDataList);

typedef NewtonUserMeshCollisionGetFacesInAABB *PNewtonUserMeshCollisionGetFacesInAABB;

typedef void __cdecl (*NewtonUserMeshCollisionCollideCallback)(const PNewtonUserMeshCollisionCollideDesc collideDescData, const void * continueCollisionHandle);

typedef NewtonUserMeshCollisionCollideCallback *PNewtonUserMeshCollisionCollideCallback;

typedef int __cdecl (*NewtonTreeCollisionFaceCallback)(const void * context, const PFloat polygon, int strideInBytes, const PInteger indexArray, int indexCount);

typedef NewtonTreeCollisionFaceCallback *PNewtonTreeCollisionFaceCallback;

typedef float __cdecl (*NewtonCollisionTreeRayCastCallback)(const PNewtonBody body, const PNewtonCollision treeCollision, float intersection, const PFloat normal, int faceId, const void * usedData);

typedef NewtonCollisionTreeRayCastCallback *PNewtonCollisionTreeRayCastCallback;

typedef float __cdecl (*NewtonHeightFieldRayCastCallback)(const PNewtonBody body, const PNewtonCollision heightFieldCollision, float intersection, int row, int col, const PFloat normal, int faceId, const void * usedData);

typedef NewtonHeightFieldRayCastCallback *PNewtonHeightFieldRayCastCallback;

typedef void __cdecl (*NewtonCollisionCopyConstructionCallback)(const PNewtonWorld newtonWorld, const PNewtonCollision collision, const PNewtonCollision sourceCollision);

typedef NewtonCollisionCopyConstructionCallback *PNewtonCollisionCopyConstructionCallback;

typedef void __cdecl (*NewtonCollisionDestructorCallback)(const PNewtonWorld newtonWorld, const PNewtonCollision collision);

typedef NewtonCollisionDestructorCallback *PNewtonCollisionDestructorCallback;

typedef void __cdecl (*NewtonTreeCollisionCallback)(const PNewtonBody bodyWithTreeCollision, const PNewtonBody body, int faceID, int vertexCount, const PFloat vertex, int vertexStrideInBytes);

typedef NewtonTreeCollisionCallback *PNewtonTreeCollisionCallback;

typedef void __cdecl (*NewtonBodyDestructor)(const PNewtonBody body);

typedef NewtonBodyDestructor *PNewtonBodyDestructor;

typedef void __cdecl (*NewtonApplyForceAndTorque)(const PNewtonBody body, float timestep, int threadIndex);

typedef NewtonApplyForceAndTorque *PNewtonApplyForceAndTorque;

typedef void __cdecl (*NewtonSetTransform)(const PNewtonBody body, const PFloat matrix, int threadIndex);

typedef NewtonSetTransform *PNewtonSetTransform;

typedef int __cdecl (*NewtonIslandUpdate)(const PNewtonWorld newtonWorld, const void * islandHandle, int bodyCount);

typedef NewtonIslandUpdate *PNewtonIslandUpdate;

typedef void __cdecl (*NewtonFractureCompoundCollisionOnEmitCompoundFractured)(const PNewtonBody fracturedBody);

typedef NewtonFractureCompoundCollisionOnEmitCompoundFractured *PNewtonFractureCompoundCollisionOnEmitCompoundFractured;

typedef void __cdecl (*NewtonFractureCompoundCollisionOnEmitChunk)(const PNewtonBody chunkBody, const PNewtonFracturedCompoundMeshPart fracturexChunkMesh, const PNewtonCollision fracturedCompountCollision);

typedef NewtonFractureCompoundCollisionOnEmitChunk *PNewtonFractureCompoundCollisionOnEmitChunk;

typedef void __cdecl (*NewtonFractureCompoundCollisionReconstructMainMeshCallBack)(const PNewtonBody body, const PNewtonFracturedCompoundMeshPart mainMesh, const PNewtonCollision fracturedCompountCollision);

typedef NewtonFractureCompoundCollisionReconstructMainMeshCallBack *PNewtonFractureCompoundCollisionReconstructMainMeshCallBack;

typedef unsigned __cdecl (*NewtonWorldRayPrefilterCallback)(const PNewtonBody body, const PNewtonCollision collision, const void * userData);

typedef NewtonWorldRayPrefilterCallback *PNewtonWorldRayPrefilterCallback;

typedef float __cdecl (*NewtonWorldRayFilterCallback)(const PNewtonBody body, const PNewtonCollision shapeHit, const PFloat hitContact, const PFloat hitNormal, __int64 collisionID, const void * userData, float intersectParam);

typedef NewtonWorldRayFilterCallback *PNewtonWorldRayFilterCallback;

typedef int __cdecl (*NewtonOnAABBOverlap)(const PNewtonJoint contact, float timestep, int threadIndex);

typedef NewtonOnAABBOverlap *PNewtonOnAABBOverlap;

typedef void __cdecl (*NewtonContactsProcess)(const PNewtonJoint contact, float timestep, int threadIndex);

typedef NewtonContactsProcess *PNewtonContactsProcess;

typedef int __cdecl (*NewtonOnCompoundSubCollisionAABBOverlap)(const PNewtonJoint contact, float timestep, const PNewtonBody body0, const void * collisionNode0, const PNewtonBody body1, const void * collisionNode1, int threadIndex);

typedef NewtonOnCompoundSubCollisionAABBOverlap *PNewtonOnCompoundSubCollisionAABBOverlap;

typedef int __cdecl (*NewtonOnContactGeneration)(const PNewtonMaterial material, const PNewtonBody body0, const PNewtonCollision collision0, const PNewtonBody body1, const PNewtonCollision collision1, const PNewtonUserContactPoint contactBuffer, int maxCount, int threadIndex);

typedef NewtonOnContactGeneration *PNewtonOnContactGeneration;

typedef int __cdecl (*NewtonBodyIterator)(const PNewtonBody body, const void * userData);

typedef NewtonBodyIterator *PNewtonBodyIterator;

typedef void __cdecl (*NewtonJointIterator)(const PNewtonJoint joint, const void * userData);

typedef NewtonJointIterator *PNewtonJointIterator;

typedef void __cdecl (*NewtonCollisionIterator)(const void * userData, int vertexCount, const PFloat faceArray, int faceId);

typedef NewtonCollisionIterator *PNewtonCollisionIterator;

typedef void __cdecl (*NewtonBallCallback)(const PNewtonJoint ball, float timestep);

typedef NewtonBallCallback *PNewtonBallCallback;

typedef unsigned __cdecl (*NewtonHingeCallback)(const PNewtonJoint hinge, const PNewtonHingeSliderUpdateDesc desc);

typedef NewtonHingeCallback *PNewtonHingeCallback;

typedef unsigned __cdecl (*NewtonSliderCallback)(const PNewtonJoint slider, const PNewtonHingeSliderUpdateDesc desc);

typedef NewtonSliderCallback *PNewtonSliderCallback;

typedef unsigned __cdecl (*NewtonUniversalCallback)(const PNewtonJoint universal, const PNewtonHingeSliderUpdateDesc desc);

typedef NewtonUniversalCallback *PNewtonUniversalCallback;

typedef unsigned __cdecl (*NewtonCorkscrewCallback)(const PNewtonJoint corkscrew, const PNewtonHingeSliderUpdateDesc desc);

typedef NewtonCorkscrewCallback *PNewtonCorkscrewCallback;

typedef void __cdecl (*NewtonUserBilateralCallback)(const PNewtonJoint userJoint, float timestep, int threadIndex);

typedef NewtonUserBilateralCallback *PNewtonUserBilateralCallback;

typedef void __cdecl (*NewtonUserBilateralGetInfoCallback)(const PNewtonJoint userJoint, const PNewtonJointRecord info);

typedef NewtonUserBilateralGetInfoCallback *PNewtonUserBilateralGetInfoCallback;

typedef void __cdecl (*NewtonConstraintDestructor)(const PNewtonJoint me);

typedef NewtonConstraintDestructor *PNewtonConstraintDestructor;

typedef void __cdecl (*NewtonJobTask)(const PNewtonWorld world, const void * userData, int threadIndex);

typedef NewtonJobTask *PNewtonJobTask;

typedef int __cdecl (*NewtonReportProgress)(float normalizedProgressPercent, const void * userData);

typedef NewtonReportProgress *PNewtonReportProgress;

//-- var, const, procedure ---------------------------------------------------
#define NEWTON_API L"newton32s.dll"
static const System::Int8 NEWTON_MAJOR_VERSION = System::Int8(0x3);
static const System::Int8 NEWTON_MINOR_VERSION = System::Int8(0xf);
static const System::Int8 NEWTON_BROADPHASE_DEFAULT = System::Int8(0x0);
static const System::Int8 NEWTON_BROADPHASE_PERSINTENT = System::Int8(0x1);
static const System::Int8 NEWTON_DYNAMIC_BODY = System::Int8(0x0);
static const System::Int8 NEWTON_KINEMATIC_BODY = System::Int8(0x1);
static const System::Int8 NEWTON_DYNAMIC_ASYMETRIC_BODY = System::Int8(0x2);
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
extern "C" int __cdecl NewtonWorldGetVersion(void);
extern "C" int __cdecl NewtonWorldFloatSize(void);
extern "C" int __cdecl NewtonGetMemoryUsed(void);
extern "C" void __cdecl NewtonSetMemorySystem(NewtonAllocMemory malloc, NewtonFreeMemory free);
extern "C" PNewtonWorld __cdecl NewtonCreate(void);
extern "C" void __cdecl NewtonDestroy(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonDestroyAllBodies(const PNewtonWorld newtonWorld);
extern "C" PNewtonPostUpdateCallback __cdecl NewtonGetPostUpdateCallback(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonSetPostUpdateCallback(const PNewtonWorld newtonWorld, NewtonPostUpdateCallback callback);
extern "C" void * __cdecl NewtonAlloc(int sizeInBytes);
extern "C" void __cdecl NewtonFree(const void * ptr);
extern "C" void __cdecl NewtonLoadPlugins(const PNewtonWorld newtonWorld, const System::WideChar * plugInPath);
extern "C" void __cdecl NewtonUnloadPlugins(const PNewtonWorld newtonWorld);
extern "C" void * __cdecl NewtonCurrentPlugin(const PNewtonWorld newtonWorld);
extern "C" void * __cdecl NewtonGetFirstPlugin(const PNewtonWorld newtonWorld);
extern "C" void * __cdecl NewtonGetPreferedPlugin(const PNewtonWorld newtonWorld);
extern "C" void * __cdecl NewtonGetNextPlugin(const PNewtonWorld newtonWorld, const void * plugin);
extern "C" System::WideChar * __cdecl NewtonGetPluginString(const PNewtonWorld newtonWorld, const void * plugin);
extern "C" void __cdecl NewtonSelectPlugin(const PNewtonWorld newtonWorld, const void * plugin);
extern "C" float __cdecl NewtonGetContactMergeTolerance(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonSetContactMergeTolerance(const PNewtonWorld newtonWorld, float tolerance);
extern "C" void __cdecl NewtonInvalidateCache(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonSetSolverIterations(const PNewtonWorld newtonWorld, int model);
extern "C" int __cdecl NewtonGetSolverIterations(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonSetParallelSolverOnLargeIsland(const PNewtonWorld newtonWorld, int mode);
extern "C" int __cdecl NewtonGetParallelSolverOnLargeIsland(const PNewtonWorld newtonWorld);
extern "C" int __cdecl NewtonGetBroadphaseAlgorithm(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonSelectBroadphaseAlgorithm(const PNewtonWorld newtonWorld, int algorithmType);
extern "C" void __cdecl NewtonResetBroadphase(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonUpdate(const PNewtonWorld newtonWorld, float timestep);
extern "C" void __cdecl NewtonUpdateAsync(const PNewtonWorld newtonWorld, float timestep);
extern "C" void __cdecl NewtonWaitForUpdateToFinish(const PNewtonWorld newtonWorld);
extern "C" int __cdecl NewtonGetNumberOfSubsteps(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonSetNumberOfSubsteps(const PNewtonWorld newtonWorld, int subSteps);
extern "C" float __cdecl NewtonGetLastUpdateTime(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonSerializeToFile(const PNewtonWorld newtonWorld, const System::WideChar * filename, NewtonOnBodySerializationCallback bodyCallback, const void * bodyUserData);
extern "C" void __cdecl NewtonDeserializeFromFile(const PNewtonWorld newtonWorld, const System::WideChar * filename, NewtonOnBodyDeserializationCallback bodyCallback, const void * bodyUserData);
extern "C" void __cdecl NewtonSerializeScene(const PNewtonWorld newtonWorld, NewtonOnBodySerializationCallback bodyCallback, const void * bodyUserData, NewtonSerializeCallback serializeCallback, const void * serializeHandle);
extern "C" void __cdecl NewtonDeserializeScene(const PNewtonWorld newtonWorld, NewtonOnBodyDeserializationCallback bodyCallback, const void * bodyUserData, NewtonDeserializeCallback serializeCallback, const void * serializeHandle);
extern "C" PNewtonBody __cdecl NewtonFindSerializedBody(const PNewtonWorld newtonWorld, int bodySerializedID);
extern "C" void __cdecl NewtonSetJointSerializationCallbacks(const PNewtonWorld newtonWorld, NewtonOnJointSerializationCallback serializeJoint, NewtonOnJointDeserializationCallback deserializeJoint);
extern "C" void __cdecl NewtonGetJointSerializationCallbacks(const PNewtonWorld newtonWorld, const NewtonOnJointSerializationCallback serializeJoint, const NewtonOnJointDeserializationCallback deserializeJoint);
extern "C" void __cdecl NewtonWorldCriticalSectionLock(const PNewtonWorld newtonWorld, int threadIndex);
extern "C" void __cdecl NewtonWorldCriticalSectionUnlock(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonSetThreadsCount(const PNewtonWorld newtonWorld, int threads);
extern "C" int __cdecl NewtonGetThreadsCount(const PNewtonWorld newtonWorld);
extern "C" int __cdecl NewtonGetMaxThreadsCount(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonDispachThreadJob(const PNewtonWorld newtonWorld, NewtonJobTask task, const void * usedData, const System::WideChar * functionName);
extern "C" void __cdecl NewtonSyncThreadJobs(const PNewtonWorld newtonWorld);
extern "C" int __cdecl NewtonAtomicAdd(const PInteger ptr, int value);
extern "C" int __cdecl NewtonAtomicSwap(const PInteger ptr, int value);
extern "C" void __cdecl NewtonYield(void);
extern "C" void __cdecl NewtonSetIslandUpdateEvent(const PNewtonWorld newtonWorld, NewtonIslandUpdate islandUpdate);
extern "C" void __cdecl NewtonWorldForEachJointDo(const PNewtonWorld newtonWorld, NewtonJointIterator callback, const void * userData);
extern "C" void __cdecl NewtonWorldForEachBodyInAABBDo(const PNewtonWorld newtonWorld, const PFloat p0, const PFloat p1, NewtonBodyIterator callback, const void * userData);
extern "C" void __cdecl NewtonWorldSetUserData(const PNewtonWorld newtonWorld, const void * userData);
extern "C" void * __cdecl NewtonWorldGetUserData(const PNewtonWorld newtonWorld);
extern "C" void * __cdecl NewtonWorldAddListener(const PNewtonWorld newtonWorld, const System::WideChar * nameId, const void * listenerUserData);
extern "C" void * __cdecl NewtonWorldGetListener(const PNewtonWorld newtonWorld, const System::WideChar * nameId);
extern "C" void __cdecl NewtonWorldListenerSetDebugCallback(const PNewtonWorld newtonWorld, const void * listener, NewtonWorldListenerDebugCallback callback);
extern "C" void __cdecl NewtonWorldListenerSetPostStepCallback(const PNewtonWorld newtonWorld, const void * listener, NewtonWorldUpdateListenerCallback callback);
extern "C" void __cdecl NewtonWorldListenerSetPreUpdateCallback(const PNewtonWorld newtonWorld, const void * listener, NewtonWorldUpdateListenerCallback callback);
extern "C" void __cdecl NewtonWorldListenerSetPostUpdateCallback(const PNewtonWorld newtonWorld, const void * listener, NewtonWorldUpdateListenerCallback callback);
extern "C" void __cdecl NewtonWorldListenerSetDestructorCallback(const PNewtonWorld newtonWorld, const void * listener, NewtonWorldDestroyListenerCallback callback);
extern "C" void __cdecl NewtonWorldListenerSetBodyDestroyCallback(const PNewtonWorld newtonWorld, const void * listener, NewtonWorldListenerBodyDestroyCallback callback);
extern "C" void __cdecl NewtonWorldListenerDebug(const PNewtonWorld newtonWorld, const void * context);
extern "C" void * __cdecl NewtonWorldGetListenerUserData(const PNewtonWorld newtonWorld, const void * listener);
extern "C" PNewtonWorldListenerBodyDestroyCallback __cdecl NewtonWorldListenerGetBodyDestroyCallback(const PNewtonWorld newtonWorld, const void * listener);
extern "C" void __cdecl NewtonWorldSetDestructorCallback(const PNewtonWorld newtonWorld, NewtonWorldDestructorCallback destructorparam);
extern "C" PNewtonWorldDestructorCallback __cdecl NewtonWorldGetDestructorCallback(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonWorldSetCollisionConstructorDestructorCallback(const PNewtonWorld newtonWorld, NewtonCollisionCopyConstructionCallback constructorparam, NewtonCollisionDestructorCallback destructorparam);
extern "C" void __cdecl NewtonWorldSetCreateDestroyContactCallback(const PNewtonWorld newtonWorld, NewtonCreateContactCallback createContact, NewtonDestroyContactCallback destroyContact);
extern "C" void __cdecl NewtonWorldRayCast(const PNewtonWorld newtonWorld, const PFloat p0, const PFloat p1, NewtonWorldRayFilterCallback filter, const void * userData, NewtonWorldRayPrefilterCallback prefilter, int threadIndex);
extern "C" int __cdecl NewtonWorldConvexCast(const PNewtonWorld newtonWorld, const PFloat matrix, const PFloat target, const PNewtonCollision shape, const PFloat param, const void * userData, NewtonWorldRayPrefilterCallback prefilter, const PNewtonWorldConvexCastReturnInfo info, int maxContactsCount, int threadIndex);
extern "C" int __cdecl NewtonWorldCollide(const PNewtonWorld newtonWorld, const PFloat matrix, const PNewtonCollision shape, const void * userData, NewtonWorldRayPrefilterCallback prefilter, const PNewtonWorldConvexCastReturnInfo info, int maxContactsCount, int threadIndex);
extern "C" int __cdecl NewtonWorldGetBodyCount(const PNewtonWorld newtonWorld);
extern "C" int __cdecl NewtonWorldGetConstraintCount(const PNewtonWorld newtonWorld);
extern "C" PNewtonJoint __cdecl NewtonWorldFindJoint(const PNewtonBody body0, const PNewtonBody body1);
extern "C" PNewtonBody __cdecl NewtonIslandGetBody(const void * island, int bodyIndex);
extern "C" void __cdecl NewtonIslandGetBodyAABB(const void * island, int bodyIndex, const PFloat p0, const PFloat p1);
extern "C" int __cdecl NewtonMaterialCreateGroupID(const PNewtonWorld newtonWorld);
extern "C" int __cdecl NewtonMaterialGetDefaultGroupID(const PNewtonWorld newtonWorld);
extern "C" void __cdecl NewtonMaterialDestroyAllGroupID(const PNewtonWorld newtonWorld);
extern "C" void * __cdecl NewtonMaterialGetUserData(const PNewtonWorld newtonWorld, int id0, int id1);
extern "C" void __cdecl NewtonMaterialSetSurfaceThickness(const PNewtonWorld newtonWorld, int id0, int id1, float thickness);
extern "C" void __cdecl NewtonMaterialSetCallbackUserData(const PNewtonWorld newtonWorld, int id0, int id1, const void * userData);
extern "C" void __cdecl NewtonMaterialSetContactGenerationCallback(const PNewtonWorld newtonWorld, int id0, int id1, NewtonOnContactGeneration contactGeneration);
extern "C" void __cdecl NewtonMaterialSetCompoundCollisionCallback(const PNewtonWorld newtonWorld, int id0, int id1, NewtonOnCompoundSubCollisionAABBOverlap compoundAabbOverlap);
extern "C" void __cdecl NewtonMaterialSetCollisionCallback(const PNewtonWorld newtonWorld, int id0, int id1, NewtonOnAABBOverlap aabbOverlap, NewtonContactsProcess process);
extern "C" void __cdecl NewtonMaterialSetDefaultSoftness(const PNewtonWorld newtonWorld, int id0, int id1, float value);
extern "C" void __cdecl NewtonMaterialSetDefaultElasticity(const PNewtonWorld newtonWorld, int id0, int id1, float elasticCoef);
extern "C" void __cdecl NewtonMaterialSetDefaultCollidable(const PNewtonWorld newtonWorld, int id0, int id1, int state);
extern "C" void __cdecl NewtonMaterialSetDefaultFriction(const PNewtonWorld newtonWorld, int id0, int id1, float staticFriction, float kineticFriction);
extern "C" void __cdecl NewtonMaterialJointResetIntraJointCollision(const PNewtonWorld newtonWorld, int id0, int id1);
extern "C" void __cdecl NewtonMaterialJointResetSelftJointCollision(const PNewtonWorld newtonWorld, int id0, int id1);
extern "C" PNewtonMaterial __cdecl NewtonWorldGetFirstMaterial(const PNewtonWorld newtonWorld);
extern "C" PNewtonMaterial __cdecl NewtonWorldGetNextMaterial(const PNewtonWorld newtonWorld, const PNewtonMaterial material);
extern "C" PNewtonBody __cdecl NewtonWorldGetFirstBody(const PNewtonWorld newtonWorld);
extern "C" PNewtonBody __cdecl NewtonWorldGetNextBody(const PNewtonWorld newtonWorld, const PNewtonBody curBody);
extern "C" void * __cdecl NewtonMaterialGetMaterialPairUserData(const PNewtonMaterial material);
extern "C" unsigned __cdecl NewtonMaterialGetContactFaceAttribute(const PNewtonMaterial material);
extern "C" PNewtonCollision __cdecl NewtonMaterialGetBodyCollidingShape(const PNewtonMaterial material, const PNewtonBody body);
extern "C" float __cdecl NewtonMaterialGetContactNormalSpeed(const PNewtonMaterial material);
extern "C" void __cdecl NewtonMaterialGetContactForce(const PNewtonMaterial material, const PNewtonBody body, const PFloat force);
extern "C" void __cdecl NewtonMaterialGetContactPositionAndNormal(const PNewtonMaterial material, const PNewtonBody body, const PFloat posit, const PFloat normal);
extern "C" void __cdecl NewtonMaterialGetContactTangentDirections(const PNewtonMaterial material, const PNewtonBody body, const PFloat dir0, const PFloat dir1);
extern "C" float __cdecl NewtonMaterialGetContactTangentSpeed(const PNewtonMaterial material, int index);
extern "C" float __cdecl NewtonMaterialGetContactMaxNormalImpact(const PNewtonMaterial material);
extern "C" float __cdecl NewtonMaterialGetContactMaxTangentImpact(const PNewtonMaterial material, int index);
extern "C" float __cdecl NewtonMaterialGetContactPenetration(const PNewtonMaterial material);
extern "C" void __cdecl NewtonMaterialSetAsSoftContact(const PNewtonMaterial material, float relaxation);
extern "C" void __cdecl NewtonMaterialSetContactSoftness(const PNewtonMaterial material, float softness);
extern "C" void __cdecl NewtonMaterialSetContactThickness(const PNewtonMaterial material, float thickness);
extern "C" void __cdecl NewtonMaterialSetContactElasticity(const PNewtonMaterial material, float restitution);
extern "C" void __cdecl NewtonMaterialSetContactFrictionState(const PNewtonMaterial material, int state, int index);
extern "C" void __cdecl NewtonMaterialSetContactFrictionCoef(const PNewtonMaterial material, float staticFrictionCoef, float kineticFrictionCoef, int index);
extern "C" void __cdecl NewtonMaterialSetContactNormalAcceleration(const PNewtonMaterial material, float accel);
extern "C" void __cdecl NewtonMaterialSetContactNormalDirection(const PNewtonMaterial material, const PFloat directionVector);
extern "C" void __cdecl NewtonMaterialSetContactPosition(const PNewtonMaterial material, const PFloat position);
extern "C" void __cdecl NewtonMaterialSetContactTangentFriction(const PNewtonMaterial material, float friction, int index);
extern "C" void __cdecl NewtonMaterialSetContactTangentAcceleration(const PNewtonMaterial material, float accel, int index);
extern "C" void __cdecl NewtonMaterialContactRotateTangentDirections(const PNewtonMaterial material, const PFloat directionVector);
extern "C" float __cdecl NewtonMaterialGetContactPruningTolerance(const PNewtonJoint contactJoint);
extern "C" void __cdecl NewtonMaterialSetContactPruningTolerance(const PNewtonJoint contactJoint, float tolerance);
extern "C" PNewtonCollision __cdecl NewtonCreateNull(const PNewtonWorld newtonWorld);
extern "C" PNewtonCollision __cdecl NewtonCreateSphere(const PNewtonWorld newtonWorld, float radius, int shapeID, const PFloat offsetMatrix);
extern "C" PNewtonCollision __cdecl NewtonCreateBox(const PNewtonWorld newtonWorld, float dx, float dy, float dz, int shapeID, const PFloat offsetMatrix);
extern "C" PNewtonCollision __cdecl NewtonCreateCone(const PNewtonWorld newtonWorld, float radius, float height, int shapeID, const PFloat offsetMatrix);
extern "C" PNewtonCollision __cdecl NewtonCreateCapsule(const PNewtonWorld newtonWorld, float radius0, float radius1, float height, int shapeID, const PFloat offsetMatrix);
extern "C" PNewtonCollision __cdecl NewtonCreateCylinder(const PNewtonWorld newtonWorld, float radio0, float radio1, float height, int shapeID, const PFloat offsetMatrix);
extern "C" PNewtonCollision __cdecl NewtonCreateChamferCylinder(const PNewtonWorld newtonWorld, float radius, float height, int shapeID, const PFloat offsetMatrix);
extern "C" PNewtonCollision __cdecl NewtonCreateConvexHull(const PNewtonWorld newtonWorld, int count, const PFloat vertexCloud, int strideInBytes, float tolerance, int shapeID, const PFloat offsetMatrix);
extern "C" PNewtonCollision __cdecl NewtonCreateConvexHullFromMesh(const PNewtonWorld newtonWorld, const PNewtonMesh mesh, float tolerance, int shapeID);
extern "C" int __cdecl NewtonCollisionGetMode(const PNewtonCollision convexCollision);
extern "C" void __cdecl NewtonCollisionSetMode(const PNewtonCollision convexCollision, int mode);
extern "C" int __cdecl NewtonConvexHullGetFaceIndices(const PNewtonCollision convexHullCollision, int face, const PInteger faceIndices);
extern "C" int __cdecl NewtonConvexHullGetVertexData(const PNewtonCollision convexHullCollision, const PFloat vertexData, PInteger strideInBytes);
extern "C" float __cdecl NewtonConvexCollisionCalculateVolume(const PNewtonCollision convexCollision);
extern "C" void __cdecl NewtonConvexCollisionCalculateInertialMatrix(const PNewtonCollision convexCollision, const PFloat inertia, const PFloat origin);
extern "C" float __cdecl NewtonConvexCollisionCalculateBuoyancyVolume(const PNewtonCollision convexCollision, const PFloat matrix, const PFloat fluidPlane, const PFloat centerOfBuoyancy);
extern "C" void * __cdecl NewtonCollisionDataPointer(const PNewtonCollision convexCollision);
extern "C" PNewtonCollision __cdecl NewtonCreateCompoundCollision(const PNewtonWorld newtonWorld, int shapeID);
extern "C" PNewtonCollision __cdecl NewtonCreateCompoundCollisionFromMesh(const PNewtonWorld newtonWorld, const PNewtonMesh mesh, float hullTolerance, int shapeID, int subShapeID);
extern "C" void __cdecl NewtonCompoundCollisionBeginAddRemove(const PNewtonCollision compoundCollision);
extern "C" void * __cdecl NewtonCompoundCollisionAddSubCollision(const PNewtonCollision compoundCollision, const PNewtonCollision convexCollision);
extern "C" void __cdecl NewtonCompoundCollisionRemoveSubCollision(const PNewtonCollision compoundCollision, const void * collisionNode);
extern "C" void __cdecl NewtonCompoundCollisionRemoveSubCollisionByIndex(const PNewtonCollision compoundCollision, int nodeIndex);
extern "C" void __cdecl NewtonCompoundCollisionSetSubCollisionMatrix(const PNewtonCollision compoundCollision, const void * collisionNode, const PFloat matrix);
extern "C" void __cdecl NewtonCompoundCollisionEndAddRemove(const PNewtonCollision compoundCollision);
extern "C" void * __cdecl NewtonCompoundCollisionGetFirstNode(const PNewtonCollision compoundCollision);
extern "C" void * __cdecl NewtonCompoundCollisionGetNextNode(const PNewtonCollision compoundCollision, const void * collisionNode);
extern "C" void * __cdecl NewtonCompoundCollisionGetNodeByIndex(const PNewtonCollision compoundCollision, int index);
extern "C" int __cdecl NewtonCompoundCollisionGetNodeIndex(const PNewtonCollision compoundCollision, const void * collisionNode);
extern "C" PNewtonCollision __cdecl NewtonCompoundCollisionGetCollisionFromNode(const PNewtonCollision compoundCollision, const void * collisionNode);
extern "C" PNewtonCollision __cdecl NewtonCreateFracturedCompoundCollision(const PNewtonWorld newtonWorld, const PNewtonMesh solidMesh, int shapeID, int fracturePhysicsMaterialID, int pointcloudCount, const PFloat vertexCloud, int strideInBytes, int materialID, const PFloat textureMatrix, NewtonFractureCompoundCollisionReconstructMainMeshCallBack regenerateMainMeshCallback, NewtonFractureCompoundCollisionOnEmitCompoundFractured emitFracturedCompound, NewtonFractureCompoundCollisionOnEmitChunk emitFracfuredChunk);
extern "C" PNewtonCollision __cdecl NewtonFracturedCompoundPlaneClip(const PNewtonCollision fracturedCompound, const PFloat plane);
extern "C" void __cdecl NewtonFracturedCompoundSetCallbacks(const PNewtonCollision fracturedCompound, NewtonFractureCompoundCollisionReconstructMainMeshCallBack regenerateMainMeshCallback, NewtonFractureCompoundCollisionOnEmitCompoundFractured emitFracturedCompound, NewtonFractureCompoundCollisionOnEmitChunk emitFracfuredChunk);
extern "C" int __cdecl NewtonFracturedCompoundIsNodeFreeToDetach(const PNewtonCollision fracturedCompound, const void * collisionNode);
extern "C" int __cdecl NewtonFracturedCompoundNeighborNodeList(const PNewtonCollision fracturedCompound, const void * collisionNode, const PPointer list, int maxCount);
extern "C" PNewtonFracturedCompoundMeshPart __cdecl NewtonFracturedCompoundGetMainMesh(const PNewtonCollision fracturedCompound);
extern "C" PNewtonFracturedCompoundMeshPart __cdecl NewtonFracturedCompoundGetFirstSubMesh(const PNewtonCollision fracturedCompound);
extern "C" PNewtonFracturedCompoundMeshPart __cdecl NewtonFracturedCompoundGetNextSubMesh(const PNewtonCollision fracturedCompound, const PNewtonFracturedCompoundMeshPart subMesh);
extern "C" int __cdecl NewtonFracturedCompoundCollisionGetVertexCount(const PNewtonCollision fracturedCompound, const PNewtonFracturedCompoundMeshPart meshOwner);
extern "C" PFloat __cdecl NewtonFracturedCompoundCollisionGetVertexPositions(const PNewtonCollision fracturedCompound, const PNewtonFracturedCompoundMeshPart meshOwner);
extern "C" PFloat __cdecl NewtonFracturedCompoundCollisionGetVertexNormals(const PNewtonCollision fracturedCompound, const PNewtonFracturedCompoundMeshPart meshOwner);
extern "C" PFloat __cdecl NewtonFracturedCompoundCollisionGetVertexUVs(const PNewtonCollision fracturedCompound, const PNewtonFracturedCompoundMeshPart meshOwner);
extern "C" int __cdecl NewtonFracturedCompoundMeshPartGetIndexStream(const PNewtonCollision fracturedCompound, const PNewtonFracturedCompoundMeshPart meshOwner, const void * segment, const PInteger index);
extern "C" void * __cdecl NewtonFracturedCompoundMeshPartGetFirstSegment(const PNewtonFracturedCompoundMeshPart fractureCompoundMeshPart);
extern "C" void * __cdecl NewtonFracturedCompoundMeshPartGetNextSegment(const void * fractureCompoundMeshSegment);
extern "C" int __cdecl NewtonFracturedCompoundMeshPartGetMaterial(const void * fractureCompoundMeshSegment);
extern "C" int __cdecl NewtonFracturedCompoundMeshPartGetIndexCount(const void * fractureCompoundMeshSegment);
extern "C" PNewtonCollision __cdecl NewtonCreateSceneCollision(const PNewtonWorld newtonWorld, int shapeID);
extern "C" void __cdecl NewtonSceneCollisionBeginAddRemove(const PNewtonCollision sceneCollision);
extern "C" void * __cdecl NewtonSceneCollisionAddSubCollision(const PNewtonCollision sceneCollision, const PNewtonCollision collision);
extern "C" void __cdecl NewtonSceneCollisionRemoveSubCollision(const PNewtonCollision compoundCollision, const void * collisionNode);
extern "C" void __cdecl NewtonSceneCollisionRemoveSubCollisionByIndex(const PNewtonCollision sceneCollision, int nodeIndex);
extern "C" void __cdecl NewtonSceneCollisionSetSubCollisionMatrix(const PNewtonCollision sceneCollision, const void * collisionNode, const PFloat matrix);
extern "C" void __cdecl NewtonSceneCollisionEndAddRemove(const PNewtonCollision sceneCollision);
extern "C" void * __cdecl NewtonSceneCollisionGetFirstNode(const PNewtonCollision sceneCollision);
extern "C" void * __cdecl NewtonSceneCollisionGetNextNode(const PNewtonCollision sceneCollision, const void * collisionNode);
extern "C" void * __cdecl NewtonSceneCollisionGetNodeByIndex(const PNewtonCollision sceneCollision, int index);
extern "C" int __cdecl NewtonSceneCollisionGetNodeIndex(const PNewtonCollision sceneCollision, const void * collisionNode);
extern "C" PNewtonCollision __cdecl NewtonSceneCollisionGetCollisionFromNode(const PNewtonCollision sceneCollision, const void * collisionNode);
extern "C" PNewtonCollision __cdecl NewtonCreateUserMeshCollision(const PNewtonWorld newtonWorld, const PFloat minBox, const PFloat maxBox, const void * userData, NewtonUserMeshCollisionCollideCallback collideCallback, NewtonUserMeshCollisionRayHitCallback rayHitCallback, NewtonUserMeshCollisionDestroyCallback destroyCallback, NewtonUserMeshCollisionGetCollisionInfo getInfoCallback, NewtonUserMeshCollisionAABBTest getLocalAABBCallback, NewtonUserMeshCollisionGetFacesInAABB facesInAABBCallback, NewtonOnUserCollisionSerializationCallback serializeCallback, int shapeID);
extern "C" int __cdecl NewtonUserMeshCollisionContinuousOverlapTest(const PNewtonUserMeshCollisionCollideDesc collideDescData, const void * continueCollisionHandle, const PFloat minAabb, const PFloat maxAabb);
extern "C" PNewtonCollision __cdecl NewtonCreateCollisionFromSerialization(const PNewtonWorld newtonWorld, NewtonDeserializeCallback deserializeFunction, const void * serializeHandle);
extern "C" void __cdecl NewtonCollisionSerialize(const PNewtonWorld newtonWorld, const PNewtonCollision collision, NewtonSerializeCallback serializeFunction, const void * serializeHandle);
extern "C" void __cdecl NewtonCollisionGetInfo(const PNewtonCollision collision, const PNewtonCollisionInfoRecord collisionInfo);
extern "C" PNewtonCollision __cdecl NewtonCreateHeightFieldCollision(const PNewtonWorld newtonWorld, int width, int height, int gridsDiagonals, int elevationdatType, const void * elevationMap, const System::WideChar * attributeMap, float verticalScale, float horizontalScale_x, float horizontalScale_z, int shapeID);
extern "C" void __cdecl NewtonHeightFieldSetUserRayCastCallback(const PNewtonCollision heightfieldCollision, NewtonHeightFieldRayCastCallback rayHitCallback);
extern "C" void __cdecl NewtonHeightFieldSetHorizontalDisplacement(PNewtonCollision heightfieldCollision, System::PWord horizontalMap, float scale);
extern "C" PNewtonCollision __cdecl NewtonCreateTreeCollision(const PNewtonWorld newtonWorld, int shapeID);
extern "C" PNewtonCollision __cdecl NewtonCreateTreeCollisionFromMesh(const PNewtonWorld newtonWorld, const PNewtonMesh mesh, int shapeID);
extern "C" void __cdecl NewtonTreeCollisionSetUserRayCastCallback(const PNewtonCollision treeCollision, NewtonCollisionTreeRayCastCallback rayHitCallback);
extern "C" void __cdecl NewtonTreeCollisionBeginBuild(const PNewtonCollision treeCollision);
extern "C" void __cdecl NewtonTreeCollisionAddFace(const PNewtonCollision treeCollision, int vertexCount, const PFloat vertexPtr, int strideInBytes, int faceAttribute);
extern "C" void __cdecl NewtonTreeCollisionEndBuild(const PNewtonCollision treeCollision, int optimize);
extern "C" int __cdecl NewtonTreeCollisionGetFaceAttribute(const PNewtonCollision treeCollision, const PInteger faceIndexArray, int indexCount);
extern "C" void __cdecl NewtonTreeCollisionSetFaceAttribute(const PNewtonCollision treeCollision, const PInteger faceIndexArray, int indexCount, int attribute);
extern "C" void __cdecl NewtonTreeCollisionForEachFace(const PNewtonCollision treeCollision, NewtonTreeCollisionFaceCallback forEachFaceCallback, const void * context);
extern "C" int __cdecl NewtonTreeCollisionGetVertexListTriangleListInAABB(const PNewtonCollision treeCollision, const PFloat p0, const PFloat p1, const PFloat vertexArray, const PInteger vertexCount, const PInteger vertexStrideInBytes, const PInteger indexList, int maxIndexCount, const PInteger faceAttribute);
extern "C" void __cdecl NewtonStaticCollisionSetDebugCallback(const PNewtonCollision staticCollision, NewtonTreeCollisionCallback userCallback);
extern "C" PNewtonCollision __cdecl NewtonCollisionCreateInstance(const PNewtonCollision collision);
extern "C" int __cdecl NewtonCollisionGetType(const PNewtonCollision collision);
extern "C" int __cdecl NewtonCollisionIsConvexShape(const PNewtonCollision collision);
extern "C" int __cdecl NewtonCollisionIsStaticShape(const PNewtonCollision collision);
extern "C" void __cdecl NewtonCollisionSetUserData(const PNewtonCollision collision, const void * userData);
extern "C" void * __cdecl NewtonCollisionGetUserData(const PNewtonCollision collision);
extern "C" void __cdecl NewtonCollisionSetUserID(const PNewtonCollision collision, __int64 id);
extern "C" __int64 __cdecl NewtonCollisionGetUserID(const PNewtonCollision collision);
extern "C" void __cdecl NewtonCollisionGetMaterial(const PNewtonCollision collision, const PNewtonCollisionMaterial userData);
extern "C" void __cdecl NewtonCollisionSetMaterial(const PNewtonCollision collision, const PNewtonCollisionMaterial userData);
extern "C" void * __cdecl NewtonCollisionGetSubCollisionHandle(const PNewtonCollision collision);
extern "C" PNewtonCollision __cdecl NewtonCollisionGetParentInstance(const PNewtonCollision collision);
extern "C" void __cdecl NewtonCollisionSetMatrix(const PNewtonCollision collision, const PFloat matrix);
extern "C" void __cdecl NewtonCollisionGetMatrix(const PNewtonCollision collision, const PFloat matrix);
extern "C" void __cdecl NewtonCollisionSetScale(const PNewtonCollision collision, float scaleX, float scaleY, float scaleZ);
extern "C" void __cdecl NewtonCollisionGetScale(const PNewtonCollision collision, const PFloat scaleX, const PFloat scaleY, const PFloat scaleZ);
extern "C" void __cdecl NewtonDestroyCollision(const PNewtonCollision collision);
extern "C" float __cdecl NewtonCollisionGetSkinThickness(const PNewtonCollision collision);
extern "C" void __cdecl NewtonCollisionSetSkinThickness(const PNewtonCollision collision, float thickness);
extern "C" int __cdecl NewtonCollisionIntersectionTest(const PNewtonWorld newtonWorld, const PNewtonCollision collisionA, const PFloat matrixA, const PNewtonCollision collisionB, const PFloat matrixB, int threadIndex);
extern "C" int __cdecl NewtonCollisionPointDistance(const PNewtonWorld newtonWorld, const PFloat point, const PNewtonCollision collision, const PFloat matrix, const PFloat contact, const PFloat normal, int threadIndex);
extern "C" int __cdecl NewtonCollisionClosestPoint(const PNewtonWorld newtonWorld, const PNewtonCollision collisionA, const PFloat matrixA, const PNewtonCollision collisionB, const PFloat matrixB, const PFloat contactA, const PFloat contactB, const PFloat normalAB, int threadIndex);
extern "C" int __cdecl NewtonCollisionCollide(const PNewtonWorld newtonWorld, int maxSize, const PNewtonCollision collisionA, const PFloat matrixA, const PNewtonCollision collisionB, const PFloat matrixB, const PFloat contacts, const PFloat normals, const PFloat penetration, const System::PInt64 attributeA, const System::PInt64 attributeB, int threadIndex);
extern "C" int __cdecl NewtonCollisionCollideContinue(const PNewtonWorld newtonWorld, int maxSize, float timestep, const PNewtonCollision collisionA, const PFloat matrixA, const PFloat velocA, const PFloat omegaA, const PNewtonCollision collisionB, const PFloat matrixB, const PFloat velocB, const PFloat omegaB, const PFloat timeOfImpact, const PFloat contacts, const PFloat normals, const PFloat penetration, const System::PInt64 attributeA, const System::PInt64 attributeB, int threadIndex);
extern "C" void __cdecl NewtonCollisionSupportVertex(const PNewtonCollision collision, const PFloat dir, const PFloat vertex);
extern "C" float __cdecl NewtonCollisionRayCast(const PNewtonCollision collision, const PFloat p0, const PFloat p1, const PFloat normal, const System::PInt64 attribute);
extern "C" void __cdecl NewtonCollisionCalculateAABB(const PNewtonCollision collision, const PFloat matrix, const PFloat p0, const PFloat p1);
extern "C" void __cdecl NewtonCollisionForEachPolygonDo(const PNewtonCollision collision, const PFloat matrix, NewtonCollisionIterator callback, const void * userData);
extern "C" void * __cdecl NewtonCollisionAggregateCreate(const PNewtonWorld world);
extern "C" void __cdecl NewtonCollisionAggregateDestroy(const void * aggregate);
extern "C" void __cdecl NewtonCollisionAggregateAddBody(const void * aggregate, const PNewtonBody body);
extern "C" void __cdecl NewtonCollisionAggregateRemoveBody(const void * aggregate, const PNewtonBody body);
extern "C" int __cdecl NewtonCollisionAggregateGetSelfCollision(const void * aggregate);
extern "C" void __cdecl NewtonCollisionAggregateSetSelfCollision(const void * aggregate, int state);
extern "C" void __cdecl NewtonSetEulerAngle(const PFloat eulersAngles, const PFloat matrix);
extern "C" void __cdecl NewtonGetEulerAngle(const PFloat matrix, const PFloat eulersAngles0, const PFloat eulersAngles1);
extern "C" float __cdecl NewtonCalculateSpringDamperAcceleration(float dt, float ks, float x, float kd, float s);
extern "C" PNewtonBody __cdecl NewtonCreateDynamicBody(const PNewtonWorld newtonWorld, const PNewtonCollision collision, const PFloat matrix);
extern "C" PNewtonBody __cdecl NewtonCreateKinematicBody(const PNewtonWorld newtonWorld, const PNewtonCollision collision, const PFloat matrix);
extern "C" PNewtonBody __cdecl NewtonCreateAsymetricDynamicBody(const PNewtonWorld newtonWorld, const PNewtonCollision collision, const PFloat matrix);
extern "C" void __cdecl NewtonDestroyBody(const PNewtonBody body);
extern "C" int __cdecl NewtonBodyGetSimulationState(const PNewtonBody body);
extern "C" void __cdecl NewtonBodySetSimulationState(const PNewtonBody bodyPtr, const int state);
extern "C" int __cdecl NewtonBodyGetType(const PNewtonBody body);
extern "C" int __cdecl NewtonBodyGetCollidable(const PNewtonBody body);
extern "C" void __cdecl NewtonBodySetCollidable(const PNewtonBody body, int collidableState);
extern "C" void __cdecl NewtonBodyAddForce(const PNewtonBody body, const PFloat force);
extern "C" void __cdecl NewtonBodyAddTorque(const PNewtonBody body, const PFloat torque);
extern "C" void __cdecl NewtonBodyCalculateInverseDynamicsForce(PNewtonBody body, float timestep, PdFloat desiredVeloc, PdFloat forceOut);
extern "C" void __cdecl NewtonBodySetCentreOfMass(const PNewtonBody body, const PFloat com);
extern "C" void __cdecl NewtonBodySetMassMatrix(const PNewtonBody body, float mass, float Ixx, float Iyy, float Izz);
extern "C" void __cdecl NewtonBodySetFullMassMatrix(const PNewtonBody body, float mass, const PFloat inertiaMatrix);
extern "C" void __cdecl NewtonBodySetMassProperties(const PNewtonBody body, float mass, const PNewtonCollision collision);
extern "C" void __cdecl NewtonBodySetMatrix(const PNewtonBody body, const PFloat matrix);
extern "C" void __cdecl NewtonBodySetMatrixNoSleep(const PNewtonBody body, const PFloat matrix);
extern "C" void __cdecl NewtonBodySetMatrixRecursive(const PNewtonBody body, const PFloat matrix);
extern "C" void __cdecl NewtonBodySetMaterialGroupID(const PNewtonBody body, int id);
extern "C" void __cdecl NewtonBodySetContinuousCollisionMode(const PNewtonBody body, unsigned state);
extern "C" void __cdecl NewtonBodySetJointRecursiveCollision(const PNewtonBody body, unsigned state);
extern "C" void __cdecl NewtonBodySetOmega(const PNewtonBody body, const PFloat omega);
extern "C" void __cdecl NewtonBodySetOmegaNoSleep(const PNewtonBody body, const PFloat omega);
extern "C" void __cdecl NewtonBodySetVelocity(const PNewtonBody body, const PFloat velocity);
extern "C" void __cdecl NewtonBodySetVelocityNoSleep(const PNewtonBody body, const PFloat velocity);
extern "C" void __cdecl NewtonBodySetForce(const PNewtonBody body, const PFloat force);
extern "C" void __cdecl NewtonBodySetTorque(const PNewtonBody body, const PFloat torque);
extern "C" void __cdecl NewtonBodySetLinearDamping(const PNewtonBody body, float linearDamp);
extern "C" void __cdecl NewtonBodySetAngularDamping(const PNewtonBody body, const PFloat angularDamp);
extern "C" void __cdecl NewtonBodySetCollision(const PNewtonBody body, const PNewtonCollision collision);
extern "C" void __cdecl NewtonBodySetCollisionScale(const PNewtonBody body, float scaleX, float scaleY, float scaleZ);
extern "C" int __cdecl NewtonBodyGetSleepState(const PNewtonBody body);
extern "C" void __cdecl NewtonBodySetSleepState(const PNewtonBody body, int state);
extern "C" int __cdecl NewtonBodyGetAutoSleep(const PNewtonBody body);
extern "C" void __cdecl NewtonBodySetAutoSleep(const PNewtonBody body, int state);
extern "C" int __cdecl NewtonBodyGetFreezeState(const PNewtonBody body);
extern "C" void __cdecl NewtonBodySetFreezeState(const PNewtonBody body, int state);
extern "C" int __cdecl NewtonBodyGetGyroscopicTorque(const PNewtonBody body);
extern "C" void __cdecl NewtonBodySetGyroscopicTorque(const PNewtonBody body, int state);
extern "C" void __cdecl NewtonBodySetDestructorCallback(const PNewtonBody body, NewtonBodyDestructor callback);
extern "C" PNewtonBodyDestructor __cdecl NewtonBodyGetDestructorCallback(const PNewtonBody body);
extern "C" void __cdecl NewtonBodySetTransformCallback(const PNewtonBody body, NewtonSetTransform callback);
extern "C" PNewtonSetTransform __cdecl NewtonBodyGetTransformCallback(const PNewtonBody body);
extern "C" void __cdecl NewtonBodySetForceAndTorqueCallback(const PNewtonBody body, NewtonApplyForceAndTorque callback);
extern "C" PNewtonApplyForceAndTorque __cdecl NewtonBodyGetForceAndTorqueCallback(const PNewtonBody body);
extern "C" int __cdecl NewtonBodyGetID(const PNewtonBody body);
extern "C" void __cdecl NewtonBodySetUserData(const PNewtonBody body, const void * userData);
extern "C" void * __cdecl NewtonBodyGetUserData(const PNewtonBody body);
extern "C" PNewtonWorld __cdecl NewtonBodyGetWorld(const PNewtonBody body);
extern "C" PNewtonCollision __cdecl NewtonBodyGetCollision(const PNewtonBody body);
extern "C" int __cdecl NewtonBodyGetMaterialGroupID(const PNewtonBody body);
extern "C" int __cdecl NewtonBodyGetSerializedID(const PNewtonBody body);
extern "C" int __cdecl NewtonBodyGetContinuousCollisionMode(const PNewtonBody body);
extern "C" int __cdecl NewtonBodyGetJointRecursiveCollision(const PNewtonBody body);
extern "C" void __cdecl NewtonBodyGetPosition(const PNewtonBody body, const PFloat pos);
extern "C" void __cdecl NewtonBodyGetMatrix(const PNewtonBody body, const PFloat matrix);
extern "C" void __cdecl NewtonBodyGetRotation(const PNewtonBody body, const PFloat rotation);
extern "C" void __cdecl NewtonBodyGetMass(const PNewtonBody body, PFloat mass, const PFloat Ixx, const PFloat Iyy, const PFloat Izz);
extern "C" void __cdecl NewtonBodyGetInvMass(const PNewtonBody body, const PFloat invMass, const PFloat invIxx, const PFloat invIyy, const PFloat invIzz);
extern "C" void __cdecl NewtonBodyGetInertiaMatrix(const PNewtonBody body, const PFloat inertiaMatrix);
extern "C" void __cdecl NewtonBodyGetInvInertiaMatrix(const PNewtonBody body, const PFloat invInertiaMatrix);
extern "C" void __cdecl NewtonBodyGetOmega(const PNewtonBody body, const PFloat vector);
extern "C" void __cdecl NewtonBodyGetVelocity(const PNewtonBody body, const PFloat vector);
extern "C" void __cdecl NewtonBodyGetAlpha(const PNewtonBody body, const PFloat vector);
extern "C" void __cdecl NewtonBodyGetAcceleration(const PNewtonBody body, const PFloat vector);
extern "C" void __cdecl NewtonBodyGetForce(const PNewtonBody body, const PFloat vector);
extern "C" void __cdecl NewtonBodyGetTorque(const PNewtonBody body, const PFloat vector);
extern "C" void __cdecl NewtonBodyGetCentreOfMass(const PNewtonBody body, const PFloat com);
extern "C" void __cdecl NewtonBodyGetPointVelocity(const PNewtonBody body, const PFloat point, const PFloat velocOut);
extern "C" void __cdecl NewtonBodyApplyImpulsePair(const PNewtonBody body, const PFloat linearImpulse, const PFloat angularImpulse, float timestep);
extern "C" void __cdecl NewtonBodyAddImpulse(const PNewtonBody body, const PFloat pointDeltaVeloc, const PFloat pointPosit, float timestep);
extern "C" void __cdecl NewtonBodyApplyImpulseArray(const PNewtonBody body, int impuleCount, int strideInByte, const PFloat impulseArray, const PFloat pointArray, float timestep);
extern "C" void __cdecl NewtonBodyIntegrateVelocity(const PNewtonBody body, float timestep);
extern "C" float __cdecl NewtonBodyGetLinearDamping(const PNewtonBody body);
extern "C" void __cdecl NewtonBodyGetAngularDamping(const PNewtonBody body, const PFloat vector);
extern "C" void __cdecl NewtonBodyGetAABB(const PNewtonBody body, const PFloat p0, const PFloat p1);
extern "C" PNewtonJoint __cdecl NewtonBodyGetFirstJoint(const PNewtonBody body);
extern "C" PNewtonJoint __cdecl NewtonBodyGetNextJoint(const PNewtonBody body, const PNewtonJoint joint);
extern "C" PNewtonJoint __cdecl NewtonBodyGetFirstContactJoint(const PNewtonBody body);
extern "C" PNewtonJoint __cdecl NewtonBodyGetNextContactJoint(const PNewtonBody body, const PNewtonJoint contactJoint);
extern "C" PNewtonJoint __cdecl NewtonBodyFindContact(const PNewtonBody body0, const PNewtonBody body1);
extern "C" void * __cdecl NewtonContactJointGetFirstContact(const PNewtonJoint contactJoint);
extern "C" void * __cdecl NewtonContactJointGetNextContact(const PNewtonJoint contactJoint, const void * contact);
extern "C" int __cdecl NewtonContactJointGetContactCount(const PNewtonJoint contactJoint);
extern "C" void __cdecl NewtonContactJointRemoveContact(const PNewtonJoint contactJoint, const void * contact);
extern "C" float __cdecl NewtonContactJointGetClosestDistance(const PNewtonJoint contactJoint);
extern "C" void __cdecl NewtonContactJointResetSelftJointCollision(const PNewtonJoint contactJoint);
extern "C" void __cdecl NewtonContactJointResetIntraJointCollision(const PNewtonJoint contactJoint);
extern "C" PNewtonMaterial __cdecl NewtonContactGetMaterial(const void * contact);
extern "C" PNewtonCollision __cdecl NewtonContactGetCollision0(const void * contact);
extern "C" PNewtonCollision __cdecl NewtonContactGetCollision1(const void * contact);
extern "C" void * __cdecl NewtonContactGetCollisionID0(const void * contact);
extern "C" void * __cdecl NewtonContactGetCollisionID1(const void * contact);
extern "C" void * __cdecl NewtonJointGetUserData(const PNewtonJoint joint);
extern "C" void __cdecl NewtonJointSetUserData(const PNewtonJoint joint, const void * userData);
extern "C" PNewtonBody __cdecl NewtonJointGetBody0(const PNewtonJoint joint);
extern "C" PNewtonBody __cdecl NewtonJointGetBody1(const PNewtonJoint joint);
extern "C" void __cdecl NewtonJointGetInfo(const PNewtonJoint joint, const PNewtonJointRecord info);
extern "C" int __cdecl NewtonJointGetCollisionState(const PNewtonJoint joint);
extern "C" void __cdecl NewtonJointSetCollisionState(const PNewtonJoint joint, int state);
extern "C" float __cdecl NewtonJointGetStiffness(const PNewtonJoint joint);
extern "C" void __cdecl NewtonJointSetStiffness(const PNewtonJoint joint, float state);
extern "C" void __cdecl NewtonDestroyJoint(const PNewtonWorld newtonWorld, const PNewtonJoint joint);
extern "C" void __cdecl NewtonJointSetDestructor(const PNewtonJoint joint, NewtonConstraintDestructor destructorparam);
extern "C" int __cdecl NewtonJointIsActive(const PNewtonJoint joint);
extern "C" PNewtonCollision __cdecl NewtonCreateMassSpringDamperSystem(const PNewtonWorld newtonWorld, int shapeID, const PFloat points, int pointCount, int strideInBytes, const PFloat pointMass, const PInteger links, int linksCount, const PFloat linksSpring, const PFloat linksDamper);
extern "C" PNewtonCollision __cdecl NewtonCreateDeformableSolid(const PNewtonWorld newtonWorld, const PNewtonMesh mesh, int shapeID);
extern "C" int __cdecl NewtonDeformableMeshGetParticleCount(const PNewtonCollision deformableMesh);
extern "C" int __cdecl NewtonDeformableMeshGetParticleStrideInBytes(const PNewtonCollision deformableMesh);
extern "C" PFloat __cdecl NewtonDeformableMeshGetParticleArray(const PNewtonCollision deformableMesh);
extern "C" PNewtonJoint __cdecl NewtonConstraintCreateBall(const PNewtonWorld newtonWorld, const PFloat pivotPoint, const PNewtonBody childBody, const PNewtonBody parentBody);
extern "C" void __cdecl NewtonBallSetUserCallback(const PNewtonJoint ball, NewtonBallCallback callback);
extern "C" void __cdecl NewtonBallGetJointAngle(const PNewtonJoint ball, PFloat angle);
extern "C" void __cdecl NewtonBallGetJointOmega(const PNewtonJoint ball, PFloat omega);
extern "C" void __cdecl NewtonBallGetJointForce(const PNewtonJoint ball, const PFloat force);
extern "C" void __cdecl NewtonBallSetConeLimits(const PNewtonJoint ball, const PFloat pin, float maxConeAngle, float maxTwistAngle);
extern "C" PNewtonJoint __cdecl NewtonConstraintCreateHinge(const PNewtonWorld newtonWorld, const PFloat pivotPoint, const PFloat pinDir, const PNewtonBody childBody, const PNewtonBody parentBody);
extern "C" void __cdecl NewtonHingeSetUserCallback(const PNewtonJoint hinge, NewtonHingeCallback callback);
extern "C" float __cdecl NewtonHingeGetJointAngle(const PNewtonJoint hinge);
extern "C" float __cdecl NewtonHingeGetJointOmega(const PNewtonJoint hinge);
extern "C" void __cdecl NewtonHingeGetJointForce(const PNewtonJoint hinge, const PFloat force);
extern "C" float __cdecl NewtonHingeCalculateStopAlpha(const PNewtonJoint hinge, const PNewtonHingeSliderUpdateDesc desc, float angle);
extern "C" PNewtonJoint __cdecl NewtonConstraintCreateSlider(const PNewtonWorld newtonWorld, const PFloat pivotPoint, const PFloat pinDir, const PNewtonBody childBody, const PNewtonBody parentBody);
extern "C" void __cdecl NewtonSliderSetUserCallback(const PNewtonJoint slider, NewtonSliderCallback callback);
extern "C" float __cdecl NewtonSliderGetJointPosit(const PNewtonJoint slider);
extern "C" float __cdecl NewtonSliderGetJointVeloc(const PNewtonJoint slider);
extern "C" void __cdecl NewtonSliderGetJointForce(const PNewtonJoint slider, const PFloat force);
extern "C" float __cdecl NewtonSliderCalculateStopAccel(const PNewtonJoint slider, const PNewtonHingeSliderUpdateDesc desc, float position);
extern "C" PNewtonJoint __cdecl NewtonConstraintCreateCorkscrew(const PNewtonWorld newtonWorld, const PFloat pivotPoint, const PFloat pinDir, const PNewtonBody childBody, const PNewtonBody parentBody);
extern "C" void __cdecl NewtonCorkscrewSetUserCallback(const PNewtonJoint corkscrew, NewtonCorkscrewCallback callback);
extern "C" float __cdecl NewtonCorkscrewGetJointPosit(const PNewtonJoint corkscrew);
extern "C" float __cdecl NewtonCorkscrewGetJointAngle(const PNewtonJoint corkscrew);
extern "C" float __cdecl NewtonCorkscrewGetJointVeloc(const PNewtonJoint corkscrew);
extern "C" float __cdecl NewtonCorkscrewGetJointOmega(const PNewtonJoint corkscrew);
extern "C" void __cdecl NewtonCorkscrewGetJointForce(const PNewtonJoint corkscrew, const PFloat force);
extern "C" float __cdecl NewtonCorkscrewCalculateStopAlpha(const PNewtonJoint corkscrew, const PNewtonHingeSliderUpdateDesc desc, float angle);
extern "C" float __cdecl NewtonCorkscrewCalculateStopAccel(const PNewtonJoint corkscrew, const PNewtonHingeSliderUpdateDesc desc, float position);
extern "C" PNewtonJoint __cdecl NewtonConstraintCreateUniversal(const PNewtonWorld newtonWorld, const PFloat pivotPoint, const PFloat pinDir0, const PFloat pinDir1, const PNewtonBody childBody, const PNewtonBody parentBody);
extern "C" void __cdecl NewtonUniversalSetUserCallback(const PNewtonJoint universal, NewtonUniversalCallback callback);
extern "C" float __cdecl NewtonUniversalGetJointAngle0(const PNewtonJoint universal);
extern "C" float __cdecl NewtonUniversalGetJointAngle1(const PNewtonJoint universal);
extern "C" float __cdecl NewtonUniversalGetJointOmega0(const PNewtonJoint universal);
extern "C" float __cdecl NewtonUniversalGetJointOmega1(const PNewtonJoint universal);
extern "C" void __cdecl NewtonUniversalGetJointForce(const PNewtonJoint universal, const PFloat force);
extern "C" float __cdecl NewtonUniversalCalculateStopAlpha0(const PNewtonJoint universal, const PNewtonHingeSliderUpdateDesc desc, float angle);
extern "C" float __cdecl NewtonUniversalCalculateStopAlpha1(const PNewtonJoint universal, const PNewtonHingeSliderUpdateDesc desc, float angle);
extern "C" PNewtonJoint __cdecl NewtonConstraintCreateUpVector(const PNewtonWorld newtonWorld, const PFloat pinDir, const PNewtonBody body);
extern "C" void __cdecl NewtonUpVectorGetPin(const PNewtonJoint upVector, float pin);
extern "C" void __cdecl NewtonUpVectorSetPin(const PNewtonJoint upVector, const float pin);
extern "C" PNewtonJoint __cdecl NewtonConstraintCreateUserJoint(const PNewtonWorld newtonWorld, int maxDOF, NewtonUserBilateralCallback callback, const PNewtonBody childBody, const PNewtonBody parentBody);
extern "C" int __cdecl NewtonUserJointGetSolverModel(const PNewtonJoint joint);
extern "C" void __cdecl NewtonUserJointSetSolverModel(const PNewtonJoint joint, int model);
extern "C" void __cdecl NewtonUserJointMassScale(const PNewtonJoint joint, float scaleBody0, float scaleBody1);
extern "C" void __cdecl NewtonUserJointSetFeedbackCollectorCallback(const PNewtonJoint joint, NewtonUserBilateralCallback getFeedback);
extern "C" void __cdecl NewtonUserJointAddLinearRow(const PNewtonJoint joint, const PFloat pivot0, const PFloat pivot1, const PFloat dir);
extern "C" void __cdecl NewtonUserJointAddAngularRow(const PNewtonJoint joint, float relativeAngle, const PFloat dir);
extern "C" void __cdecl NewtonUserJointAddGeneralRow(const PNewtonJoint joint, const PFloat jacobian0, const PFloat jacobian1);
extern "C" void __cdecl NewtonUserJointSetRowMinimumFriction(const PNewtonJoint joint, float friction);
extern "C" void __cdecl NewtonUserJointSetRowMaximumFriction(const PNewtonJoint joint, float friction);
extern "C" float __cdecl NewtonUserJointCalculateRowZeroAcceleration(const PNewtonJoint joint);
extern "C" float __cdecl NewtonUserJointGetRowAcceleration(const PNewtonJoint joint);
extern "C" void __cdecl NewtonUserJointGetRowJacobian(const PNewtonJoint joint, const PFloat linear0, const PFloat angula0, const PFloat linear1, const PFloat angula1);
extern "C" void __cdecl NewtonUserJointSetRowAcceleration(const PNewtonJoint joint, float acceleration);
extern "C" void __cdecl NewtonUserJointSetRowSpringDamperAcceleration(const PNewtonJoint joint, float rowStiffness, float spring, float damper);
extern "C" void __cdecl NewtonUserJointSetRowStiffness(const PNewtonJoint joint, float stiffness);
extern "C" int __cdecl NewtonUserJoinRowsCount(const PNewtonJoint joint);
extern "C" void __cdecl NewtonUserJointGetGeneralRow(const PNewtonJoint joint, int index, const PFloat jacobian0, const PFloat jacobian1);
extern "C" float __cdecl NewtonUserJointGetRowForce(const PNewtonJoint joint, int row);
extern "C" PNewtonMesh __cdecl NewtonMeshCreate(const PNewtonWorld newtonWorld);
extern "C" PNewtonMesh __cdecl NewtonMeshCreateFromMesh(const PNewtonMesh mesh);
extern "C" PNewtonMesh __cdecl NewtonMeshCreateFromCollision(const PNewtonCollision collision);
extern "C" PNewtonMesh __cdecl NewtonMeshCreateTetrahedraIsoSurface(const PNewtonMesh mesh);
extern "C" PNewtonMesh __cdecl NewtonMeshCreateConvexHull(const PNewtonWorld newtonWorld, int pointCount, const PFloat vertexCloud, int strideInBytes, float tolerance);
extern "C" PNewtonMesh __cdecl NewtonMeshCreateVoronoiConvexDecomposition(const PNewtonWorld newtonWorld, int pointCount, const PFloat vertexCloud, int strideInBytes, int materialID, const PFloat textureMatrix);
extern "C" PNewtonMesh __cdecl NewtonMeshCreateFromSerialization(const PNewtonWorld newtonWorld, NewtonDeserializeCallback deserializeFunction, const void * serializeHandle);
extern "C" void __cdecl NewtonMeshDestroy(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshSerialize(const PNewtonMesh mesh, NewtonSerializeCallback serializeFunction, const void * serializeHandle);
extern "C" void __cdecl NewtonMeshSaveOFF(const PNewtonMesh mesh, const System::WideChar * filename);
extern "C" PNewtonMesh __cdecl NewtonMeshLoadOFF(const PNewtonWorld newtonWorld, const System::WideChar * filename);
extern "C" PNewtonMesh __cdecl NewtonMeshLoadTetrahedraMesh(const PNewtonWorld newtonWorld, const System::WideChar * filename);
extern "C" void __cdecl NewtonMeshFlipWinding(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshApplyTransform(const PNewtonMesh mesh, const PFloat matrix);
extern "C" void __cdecl NewtonMeshCalculateOOBB(const PNewtonMesh mesh, const PFloat matrix, const PFloat x, const PFloat y, const PFloat z);
extern "C" void __cdecl NewtonMeshCalculateVertexNormals(const PNewtonMesh mesh, float angleInRadians);
extern "C" void __cdecl NewtonMeshApplySphericalMapping(const PNewtonMesh mesh, int material, const PFloat aligmentMatrix);
extern "C" void __cdecl NewtonMeshApplyCylindricalMapping(const PNewtonMesh mesh, int cylinderMaterial, int capMaterial, const PFloat aligmentMatrix);
extern "C" void __cdecl NewtonMeshApplyBoxMapping(const PNewtonMesh mesh, int frontMaterial, int sideMaterial, int topMaterial, const PFloat aligmentMatrix);
extern "C" void __cdecl NewtonMeshApplyAngleBasedMapping(const PNewtonMesh mesh, int material, NewtonReportProgress reportPrograssCallback, const void * reportPrgressUserData, const PFloat aligmentMatrix);
extern "C" void __cdecl NewtonCreateTetrahedraLinearBlendSkinWeightsChannel(const PNewtonMesh tetrahedraMesh, const PNewtonMesh skinMesh);
extern "C" void __cdecl NewtonMeshOptimize(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshOptimizePoints(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshOptimizeVertex(const PNewtonMesh mesh);
extern "C" int __cdecl NewtonMeshIsOpenMesh(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshFixTJoints(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshPolygonize(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshTriangulate(const PNewtonMesh mesh);
extern "C" PNewtonMesh __cdecl NewtonMeshUnion(const PNewtonMesh mesh, const PNewtonMesh clipper, const PFloat clipperMatrix);
extern "C" PNewtonMesh __cdecl NewtonMeshDifference(const PNewtonMesh mesh, const PNewtonMesh clipper, const PFloat clipperMatrix);
extern "C" PNewtonMesh __cdecl NewtonMeshIntersection(const PNewtonMesh mesh, const PNewtonMesh clipper, const PFloat clipperMatrix);
extern "C" void __cdecl NewtonMeshClip(const PNewtonMesh mesh, const PNewtonMesh clipper, const PFloat clipperMatrix, const PNewtonMesh topMesh, const PNewtonMesh bottomMesh);
extern "C" PNewtonMesh __cdecl NewtonMeshConvexMeshIntersection(const PNewtonMesh mesh, const PNewtonMesh convexMesh);
extern "C" PNewtonMesh __cdecl NewtonMeshSimplify(const PNewtonMesh mesh, int maxVertexCount, NewtonReportProgress reportPrograssCallback, const void * reportPrgressUserData);
extern "C" PNewtonMesh __cdecl NewtonMeshApproximateConvexDecomposition(const PNewtonMesh mesh, float maxConcavity, float backFaceDistanceFactor, int maxCount, int maxVertexPerHull, NewtonReportProgress reportProgressCallback, const void * reportProgressUserData);
extern "C" void __cdecl NewtonRemoveUnusedVertices(const PNewtonMesh mesh, const PInteger vertexRemapTable);
extern "C" void __cdecl NewtonMeshBeginBuild(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshBeginFace(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshAddPoint(const PNewtonMesh mesh, double x, double y, double z);
extern "C" void __cdecl NewtonMeshAddLayer(const PNewtonMesh mesh, int layerIndex);
extern "C" void __cdecl NewtonMeshAddMaterial(const PNewtonMesh mesh, int materialIndex);
extern "C" void __cdecl NewtonMeshAddNormal(const PNewtonMesh mesh, float x, float y, float z);
extern "C" void __cdecl NewtonMeshAddBinormal(const PNewtonMesh mesh, float x, float y, float z);
extern "C" void __cdecl NewtonMeshAddUV0(const PNewtonMesh mesh, float u, float v);
extern "C" void __cdecl NewtonMeshAddUV1(const PNewtonMesh mesh, float u, float v);
extern "C" void __cdecl NewtonMeshAddVertexColor(const PNewtonMesh mesh, float r, float g, float b, float a);
extern "C" void __cdecl NewtonMeshEndFace(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshEndBuild(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshClearVertexFormat(const PNewtonMeshVertexFormat format);
extern "C" void __cdecl NewtonMeshBuildFromVertexListIndexList(const PNewtonMesh mesh, const PNewtonMeshVertexFormat format);
extern "C" int __cdecl NewtonMeshGetPointCount(const PNewtonMesh mesh);
extern "C" PInteger __cdecl NewtonMeshGetIndexToVertexMap(const PNewtonMesh mesh);
extern "C" int __cdecl NewtonMeshGetVertexWeights(PNewtonMesh mesh, int vertexIndex, PInteger weightIndex, PdFloat weightFactor);
extern "C" void __cdecl NewtonMeshGetVertexDoubleChannel(const PNewtonMesh mesh, int vertexStrideInByte, const System::PDouble outBuffer);
extern "C" void __cdecl NewtonMeshGetVertexChannel(const PNewtonMesh mesh, int vertexStrideInByte, const PFloat outBuffer);
extern "C" void __cdecl NewtonMeshGetNormalChannel(const PNewtonMesh mesh, int vertexStrideInByte, const PFloat outBuffer);
extern "C" void __cdecl NewtonMeshGetBinormalChannel(const PNewtonMesh mesh, int vertexStrideInByte, const PFloat outBuffer);
extern "C" void __cdecl NewtonMeshGetUV0Channel(const PNewtonMesh mesh, int vertexStrideInByte, const PFloat outBuffer);
extern "C" void __cdecl NewtonMeshGetUV1Channel(const PNewtonMesh mesh, int vertexStrideInByte, const PFloat outBuffer);
extern "C" void __cdecl NewtonMeshGetVertexColorChannel(const PNewtonMesh mesh, int vertexStrideInByte, const PFloat outBuffer);
extern "C" int __cdecl NewtonMeshHasNormalChannel(const PNewtonMesh mesh);
extern "C" int __cdecl NewtonMeshHasBinormalChannel(const PNewtonMesh mesh);
extern "C" int __cdecl NewtonMeshHasUV0Channel(const PNewtonMesh mesh);
extern "C" int __cdecl NewtonMeshHasUV1Channel(const PNewtonMesh mesh);
extern "C" int __cdecl NewtonMeshHasVertexColorChannel(const PNewtonMesh mesh);
extern "C" void * __cdecl NewtonMeshBeginHandle(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshEndHandle(const PNewtonMesh mesh, const void * handle);
extern "C" int __cdecl NewtonMeshFirstMaterial(const PNewtonMesh mesh, const void * handle);
extern "C" int __cdecl NewtonMeshNextMaterial(const PNewtonMesh mesh, const void * handle, int materialId);
extern "C" int __cdecl NewtonMeshMaterialGetMaterial(const PNewtonMesh mesh, const void * handle, int materialId);
extern "C" int __cdecl NewtonMeshMaterialGetIndexCount(const PNewtonMesh mesh, const void * handle, int materialId);
extern "C" void __cdecl NewtonMeshMaterialGetIndexStream(const PNewtonMesh mesh, const void * handle, int materialId, const PInteger index);
extern "C" void __cdecl NewtonMeshMaterialGetIndexStreamShort(const PNewtonMesh mesh, const void * handle, int materialId, const short index);
extern "C" PNewtonMesh __cdecl NewtonMeshCreateFirstSingleSegment(const PNewtonMesh mesh);
extern "C" PNewtonMesh __cdecl NewtonMeshCreateNextSingleSegment(const PNewtonMesh mesh, const PNewtonMesh segment);
extern "C" PNewtonMesh __cdecl NewtonMeshCreateFirstLayer(const PNewtonMesh mesh);
extern "C" PNewtonMesh __cdecl NewtonMeshCreateNextLayer(const PNewtonMesh mesh, const PNewtonMesh segment);
extern "C" int __cdecl NewtonMeshGetTotalFaceCount(const PNewtonMesh mesh);
extern "C" int __cdecl NewtonMeshGetTotalIndexCount(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshGetFaces(const PNewtonMesh mesh, const PInteger faceIndexCount, const PInteger faceMaterial, const PPointer faceIndices);
extern "C" int __cdecl NewtonMeshGetVertexCount(const PNewtonMesh mesh);
extern "C" int __cdecl NewtonMeshGetVertexStrideInByte(const PNewtonMesh mesh);
extern "C" System::PDouble __cdecl NewtonMeshGetVertexArray(const PNewtonMesh mesh);
extern "C" int __cdecl NewtonMeshGetVertexBaseCount(const PNewtonMesh mesh);
extern "C" void __cdecl NewtonMeshSetVertexBaseCount(const PNewtonMesh mesh, int baseCount);
extern "C" void * __cdecl NewtonMeshGetFirstVertex(const PNewtonMesh mesh);
extern "C" void * __cdecl NewtonMeshGetNextVertex(const PNewtonMesh mesh, const void * vertex);
extern "C" int __cdecl NewtonMeshGetVertexIndex(const PNewtonMesh mesh, const void * vertex);
extern "C" void * __cdecl NewtonMeshGetFirstPoint(const PNewtonMesh mesh);
extern "C" void * __cdecl NewtonMeshGetNextPoint(const PNewtonMesh mesh, const void * point);
extern "C" int __cdecl NewtonMeshGetPointIndex(const PNewtonMesh mesh, const void * point);
extern "C" int __cdecl NewtonMeshGetVertexIndexFromPoint(const PNewtonMesh mesh, const void * point);
extern "C" void * __cdecl NewtonMeshGetFirstEdge(const PNewtonMesh mesh);
extern "C" void * __cdecl NewtonMeshGetNextEdge(const PNewtonMesh mesh, const void * edge);
extern "C" void __cdecl NewtonMeshGetEdgeIndices(const PNewtonMesh mesh, const void * edge, const PInteger v0, const PInteger v1);
extern "C" void * __cdecl NewtonMeshGetFirstFace(const PNewtonMesh mesh);
extern "C" void * __cdecl NewtonMeshGetNextFace(const PNewtonMesh mesh, const void * face);
extern "C" int __cdecl NewtonMeshIsFaceOpen(const PNewtonMesh mesh, const void * face);
extern "C" int __cdecl NewtonMeshGetFaceMaterial(const PNewtonMesh mesh, const void * face);
extern "C" int __cdecl NewtonMeshGetFaceIndexCount(const PNewtonMesh mesh, const void * face);
extern "C" void __cdecl NewtonMeshGetFaceIndices(const PNewtonMesh mesh, const void * face, const PInteger indices);
extern "C" void __cdecl NewtonMeshGetFacePointIndices(const PNewtonMesh mesh, const void * face, const PInteger indices);
extern "C" void __cdecl NewtonMeshCalculateFaceNormal(const PNewtonMesh mesh, const void * face, const System::PDouble normal);
extern "C" void __cdecl NewtonMeshSetFaceMaterial(const PNewtonMesh mesh, const void * face, int matId);
}	/* namespace Newtonimport */
}	/* namespace Physics */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS_NEWTONIMPORT)
using namespace Physics::Newtonimport;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PHYSICS)
using namespace Physics;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Physics_NewtonimportHPP
