(* Copyright (c) <2003-2020> <Julio Jerez, Newton Game Dynamics>
*
* This software is provided 'as-is', without any express or implied
* warranty. In no event will the authors be held liable for any damages
* arising from the use of this software.
*
* Permission is granted to anyone to use this software for any purpose,
* including commercial applications, and to alter it and redistribute it
* freely, subject to the following restrictions:
*
* 1. The origin of this software must not be misrepresented; you must not
* claim that you wrote the original software. If you use this software
* in a product, an acknowledgment in the product documentation would be
* appreciated but is not required.
*
* 2. Altered source versions must be plainly marked as such, and must not be
* misrepresented as being the original software.
*
* 3. This notice may not be removed or altered from any source distribution.
*)
unit NGD.NewtonImport;

// Define double to use newton in double precision
{.$DEFINE USE_DOUBLE_PRECISION}

interface
 const
{$IFDEF WIN32}
  {$IFDEF USE_DOUBLE_PRECISION}
   NEWTON_API = 'newton32d.dll';
  {$ELSE}
   NEWTON_API = 'newton32s.dll';
  {$ENDIF}
{$ENDIF}

{$IFDEF WIN64}
  {$IFDEF USE_DOUBLE_PRECISION}
   NEWTON_API = 'newton64d.dll';
  {$ELSE}
   NEWTON_API = 'newton64s.dll';
 {$ENDIF}
{$ENDIF}

type
{$IFDEF USE_DOUBLE_PRECISION}
  	  Float = Double;
     dFloat = Double;
{$ELSE}
      Float = Single;
     dFloat = Single;
{$ENDIF}

PFloat = ^Float;
PdFloat = ^dFloat;

PInteger = ^Integer;
Psmallint = ^smallint;
PPointer = ^Pointer;
dFloat64 = Double;
PdFloat64 = ^dFloat64;
dLong = Int64;
PdLong = ^dLong;

PNewtonMesh = ^Pointer;
PNewtonBody = ^Pointer;
PNewtonWorld = ^Pointer;
PNewtonJoint = ^Pointer;
PNewtonMaterial = ^Pointer;
PNewtonCollision = ^Pointer;
PNewtonSceneProxy = ^Pointer;
PNewtonFracturedCompoundMeshPart = ^Pointer;
PNewtonDeformableMeshSegment = ^Pointer;
PNewtonAcyclicArticulation = ^Pointer;
PNewtonInverseDynamics = ^Pointer;
(*
PNewtonSerializeHandle = ^Pointer;
PNewtonMeshHandle = ^Pointer;
PNewtonMeshVertex = ^Pointer;
PNewtonMeshPoint = ^Pointer;
PNewtonMeshEdge = ^Pointer;
PNewtonMeshFace = ^Pointer;
PNewtonListener = ^Pointer;
*)
TNewtonFuncLinkRecord = record
	function_name: widestring;
	function_ptr: pointer
end;
 

const 
 NEWTON_MAJOR_VERSION = 3;
 NEWTON_MINOR_VERSION = 15;
 NEWTON_BROADPHASE_DEFAULT = 0;
 NEWTON_BROADPHASE_PERSINTENT = 1;
 NEWTON_DYNAMIC_BODY = 0;
 NEWTON_KINEMATIC_BODY = 1;
 NEWTON_DYNAMIC_ASYMETRIC_BODY	= 2;
// NEWTON_DEFORMABLE_BODY	=	2;

 SERIALIZE_ID_SPHERE = 0;
 SERIALIZE_ID_CAPSULE = 1;
 SERIALIZE_ID_CYLINDER = 2;
 SERIALIZE_ID_CHAMFERCYLINDER = 3;
 SERIALIZE_ID_BOX = 4;
 SERIALIZE_ID_CONE = 5;
 SERIALIZE_ID_CONVEXHULL = 6;
 SERIALIZE_ID_NULL = 7;
 SERIALIZE_ID_COMPOUND = 8;
 SERIALIZE_ID_TREE = 9;
 SERIALIZE_ID_HEIGHTFIELD = 10;
 SERIALIZE_ID_CLOTH_PATCH = 11;
 SERIALIZE_ID_DEFORMABLE_SOLID = 12;
 SERIALIZE_ID_USERMESH = 13;
 SERIALIZE_ID_SCENE = 14;
 SERIALIZE_ID_FRACTURED_COMPOUND = 15;

 
type  
 
TNewtonMaterialData = packed record
case integer of
1: (
	m_ptr: Pointer;
);
2: (
	m_int: dLong;
);
3: (
	m_float: dFloat;
);
  end; 
PNewtonMaterialData = ^TNewtonMaterialData;

TNewtonCollisionMaterial = packed record
  m_userId: dLong;
  m_userData: TNewtonMaterialData;
  m_userParam: array[0..5] of TNewtonMaterialData;
 end;
PNewtonCollisionMaterial = ^TNewtonCollisionMaterial;

TNewtonBoxParam = packed record
  m_x: dFloat;
  m_y: dFloat;
  m_z: dFloat;
 end;
PNewtonBoxParam = ^TNewtonBoxParam;

TNewtonSphereParam = packed record
  m_radio: dFloat;
 end;
PNewtonSphereParam = ^TNewtonSphereParam;

TNewtonCapsuleParam = packed record
	m_radio0: dfloat;
	m_radio1: dfloat;
	m_height: dfloat;
end;
PNewtonCapsuleParam = ^TNewtonCapsuleParam;

TNewtonCylinderParam = packed record
	m_radio0: dfloat;
	m_radio1: dfloat;
	m_height: dfloat;
end;
PNewtonCylinderParam = ^TNewtonCylinderParam;

TNewtonConeParam = packed record
	m_radio: dfloat;
	m_height: dfloat;
end;
PNewtonConeParam = ^TNewtonConeParam;

TNewtonTaperedCapsuleParam = packed record
  m_radio0,
  m_radio1,
  m_height: dFloat;
end;
PNewtonTaperedCapsuleParam = ^TNewtonTaperedCapsuleParam;


TNewtonTaperedCylinderParam = packed record
  m_radio0,
  m_radio1,
  m_height: dFloat;
end;
PNewtonTaperedCylinderParam = ^TNewtonTaperedCylinderParam;

TNewtonChamferCylinderParam = packed record
  m_radio: dFloat;
  m_height: dFloat;
 end;
PNewtonChamferCylinderParam = ^TNewtonChamferCylinderParam;

TNewtonConvexHullParam = packed record
  m_vertexCount: Integer;
  m_vertexStrideInBytes: Integer;
  m_faceCount: Integer;
  m_vertex: PdFloat;
 end;
PNewtonConvexHullParam = ^TNewtonConvexHullParam;

TNewtonCompoundCollisionParam = packed record
	m_chidrenCount: Integer;
end;
PNewtonCompoundCollisionParam = ^TNewtonCompoundCollisionParam;

TNewtonCollisionTreeParam = packed record
	m_vertexCount: Integer;
	m_indexCount: Integer;
end;
PNewtonCollisionTreeParam = ^TNewtonCollisionTreeParam;

TNewtonDeformableMeshParam = packed record
	m_vertexCount: Integer;
	m_triangleCount: Integer;
	m_vrtexStrideInBytes: Integer;
	m_indexList: PWord;
	m_vertexList: PdFloat;
end;
PNewtonDeformableMeshParam = ^TNewtonDeformableMeshParam;

TNewtonHeightFieldCollisionParam = packed record
  m_width                 : Integer;
  m_height                : Integer; 
  m_gridsDiagonals        : Integer;
  m_elevationDataType     : Integer;// 0 = 32 bit floats, 1 = unsigned 16 bit integers
  m_verticalScale         : dFloat;
  m_horizonalScale_x      : dFloat;
  m_horizonalScale_z      : dFloat;
  m_vertialElevation      : Pointer;
  m_atributes             : PShortInt;
end;
PNewtonHeightFieldCollisionParam = ^TNewtonHeightFieldCollisionParam;

TNewtonSceneCollisionParam = packed record
  m_childrenProxyCount   : Integer;
end;
PNewtonSceneCollisionParam = ^TNewtonSceneCollisionParam;

TNewtonCollisionNullParam = packed record
end;

TNewtonCollisionInfoRecord = packed record
  m_offsetMatrix: array [0..3,0..3] of dFloat;
  m_collisionMaterial: TNewtonCollisionMaterial; 
  m_collisionType,    // tag id to identify the collision primitive
  m_collisionUserID: Integer;
  case Integer of
  	SERIALIZE_ID_BOX: (shapedataBox: TNewtonBoxParam);
	SERIALIZE_ID_CONE: (shapedataCone: TNewtonConeParam);
	SERIALIZE_ID_SPHERE: (shapedataSphere: TNewtonSphereParam);
    SERIALIZE_ID_CAPSULE: (shapedataCapsule: TNewtonCapsuleParam);
    SERIALIZE_ID_CYLINDER: (shapedataCylinder: TNewtonCylinderParam);
	//SERIALIZE_ID_TAPEREDCAPSULE: (m_taperedCapsule: TNewtonTaperedCapsuleParam);
	//SERIALIZE_ID_TAPEREDCYLINDER:	(m_taperedCylinder: TNewtonTaperedCylinderParam);
    // SERIALIZE_ID_NULL: (shapedataNull: TNewtonCollisionNullParam);
     SERIALIZE_ID_CHAMFERCYLINDER: (shapedataChamferCylinder: TNewtonChamferCylinderParam);
     SERIALIZE_ID_CONVEXHULL: (shapedataConvexHull: TNewtonConvexHullParam);
	 
	 SERIALIZE_ID_DEFORMABLE_SOLID:	(m_deformableMesh: TNewtonDeformableMeshParam);

     SERIALIZE_ID_COMPOUND: (shapedataCompound: TNewtonCompoundCollisionParam);
     SERIALIZE_ID_TREE: (shapedataTree: TNewtonCollisionTreeParam);
     SERIALIZE_ID_HEIGHTFIELD: (shapedataHeightField: TNewtonHeightFieldCollisionParam);
     SERIALIZE_ID_SCENE: (shapedataSceneCollision: TNewtonSceneCollisionParam);
	 SERIALIZE_ID_USERMESH: (m_paramArray: array[0..63] of float);
 end;
PNewtonCollisionInfoRecord = ^TNewtonCollisionInfoRecord;  

TNewtonJointRecord = packed record
	m_attachmenMatrix_0: array[0..3, 0..3] of dfloat;
	m_attachmenMatrix_1: array[0..3, 0..3] of dfloat;
	m_minLinearDof: array[0..2] of dfloat;
	m_maxLinearDof: array[0..2] of dfloat;
	m_minAngularDof: array[0..2] of dfloat;
	m_maxAngularDof: array[0..2] of dfloat;
	m_attachBody_0: pNewtonBody;
	m_attachBody_1: pNewtonBody;
	m_extraParameters: array[0..63] of dfloat;
	m_bodiesCollisionOn: Integer;
	m_descriptionType: array[0..127] of AnsiChar;
end;
PNewtonJointRecord = ^TNewtonJointRecord;

 TNewtonUserMeshCollisionCollideDesc = packed record
  m_boxP0: array [0..3] of dFloat;                      // lower bounding box of intersection query in local space
  m_boxP1: array [0..3] of dFloat;                      // upper bounding box of intersection query in local space
  m_boxDistanceTravel: array [0..3] of dFloat;          // max distance that box bpxP0 and boxP1 can travel on this timestep, used this for continue collision mode.
  m_threadNumber: Integer;                              // current thread executing this query
  m_faceCount: Integer;                                 // the application should set here how many polygons intersect the query box
  m_vertexStrideInBytes: Integer;                       // the application should set here the size of each vertex
  m_skinThickness: dFloat;                              // this is the minimum skin separation specified by the material between these two colliding shapes
  m_userData: Pointer;                                  // user data passed to the collision geometry at creation time

  m_objBody              : PNewtonBody;                  // pointer to the colliding body
  m_polySoupBody         : PNewtonBody;                  // pointer to the rigid body owner of this collision tree
  m_objCollision         : PNewtonCollision;             // collision shape of the colliding body, (no necessarily the collision of m_objBody)
  m_polySoupCollision    : PNewtonCollision;             // collision shape of teh collsion tree, (no necessarily the collision of m_polySoupBody)
  m_vertex               : PdFloat;                 // the application should set here the pointer to the global vertex of the mesh.
  m_faceIndexCount       : PInteger;                // the application should set here the pointer to the vertex count of each face.
  m_faceVertexIndex      : PInteger;                // the application should set here the pointer index array for each vertex on a face.
													// the format of a face is I0, I1, I2, I3, ..., M, N, E0, E1, E2, ..., A
                                                	// I0, I1, I2, .. are the indices to the vertex, relative to m_vertex pointer
		                                        	// M is the index to the material sub shape id
													// N in the index to the vertex normal relative to m_vertex pointer
													// E0, E1, E2, ... are the indices of the the face normal that is shared to that face edge, when the edge does not share a face normal then the edge index is set to index N, which the index to the face normal    
													// A is and estimate of the largest diagonal of the face, this used internally as a hint to improve floating point accuracy and algorithm performance. 
 end;
 PNewtonUserMeshCollisionCollideDesc = ^TNewtonUserMeshCollisionCollideDesc;
 
 
 TNewtonWorldConvexCastReturnInfo = Packed Record
   m_point: array [0..3] of dFloat;              // collision point in global space
   m_normal: array [0..3] of dFloat;             // surface normal at collision point in global space
   //m_normalOnHitPoint: array [0..3] of dFloat; // surface normal at the surface of the hit body,
                                                 // is the same as the normal calculated by a ray cast hitting the body at the hit point
   m_contactID: Integer;                         // collision ID at contact point
   m_hitBody: PNewtonBody;                        // body hit at contact point
   m_penetration: dFloat;                        // contact penetration at collision point
 end;
 PNewtonWorldConvexCastReturnInfo = ^TNewtonWorldConvexCastReturnInfo;
 
 TNewtonUserMeshCollisionRayHitDesc = Packed Record
  m_p0                   : array [0..3] of dFloat;      // ray origin in collision local space
  m_p1                   : array [0..3] of dFloat;      // ray destination in collision local space
  m_normalOut            : array [0..3] of dFloat;      // copy here the normal at the ray intersection
  m_userIdOut            : dLong;                       // copy here a user defined id for further feedback
  m_userData             : Pointer;                     // user data passed to the collision geometry at creation time
 end;
 PNewtonUserMeshCollisionRayHitDesc = ^TNewtonUserMeshCollisionRayHitDesc;
 
 TNewtonHingeSliderUpdateDesc = Packed Record
  m_accel                : dFloat;
  m_minFriction          : dFloat;  
  m_maxFriction          : dFloat;
  m_timestep             : dFloat;
 end;
PNewtonHingeSliderUpdateDesc = ^TNewtonHingeSliderUpdateDesc;


 TNewtonUserContactPoint = packed record
  m_point                : array [0..3] of dFloat;
  m_normal               : array [0..3] of dFloat;
  m_shapeId0             : dLong;
  m_shapeId1             : dLong;
  m_penetration          : dFloat;
  m_unused               : array [0..2] of Integer;
 end;
 PNewtonUserContactPoint = ^TNewtonUserContactPoint;

TNewtonImmediateModeConstraint = packed record
	m_jacobian01: array[0..7, 0..5] of dfloat;
	m_jacobian10: array[0..7, 0..5] of dfloat;
	m_minFriction: array[0..7] of dfloat;
	m_maxFriction: array[0..7] of dfloat;
	m_jointAccel: array[0..7] of dfloat;
	m_jointStiffness: array[0..7] of dfloat;
end;
PNewtonImmediateModeConstraint = ^TNewtonImmediateModeConstraint;
 // data structure for interfacing with NewtonMesh
 TNewtonMeshDoubleData = packed record
  m_data          : Pdouble;
  m_indexList     : PInteger;
  m_strideInBytes : Integer;
 end;
 PNewtonMeshDoubleData = ^TNewtonMeshDoubleData;
  
 TNewtonMeshFloatData = packed record
  m_data          : PdFloat;
  m_indexList     : PInteger;
  m_strideInBytes : Integer;
 end;
 PNewtonMeshFloatData = ^TNewtonMeshFloatData;
 
 TNewtonMeshVertexFormat = packed record
  m_faceCount : Integer;
  m_faceIndexCount: PInteger;
  m_faceMaterial: PInteger;
  m_vertex: TNewtonMeshDoubleData;
  m_normal: TNewtonMeshFloatData;
  m_binormal: TNewtonMeshFloatData;
  m_uv0: TNewtonMeshFloatData;
  m_uv1: TNewtonMeshFloatData;
  m_vertexColor: TNewtonMeshFloatData;
 end;
 PNewtonMeshVertexFormat = ^TNewtonMeshVertexFormat;
 
 // Newton callback functions
NewtonAllocMemory = function(sizeInBytes: Integer): Pointer; cdecl;
PNewtonAllocMemory = ^NewtonAllocMemory;
NewtonFreeMemory = procedure(const ptr: Pointer; sizeInBytes: Integer); cdecl;
PNewtonFreeMemory = ^NewtonFreeMemory;
 
NewtonWorldDestructorCallback = procedure(const world: PNewtonWorld); cdecl;
PNewtonWorldDestructorCallback = ^NewtonWorldDestructorCallback;

NewtonPostUpdateCallback = procedure(const world: PNewtonWorld; timestep: dfloat); cdecl;
PNewtonPostUpdateCallback = ^NewtonPostUpdateCallback;

NewtonCreateContactCallback = procedure(const newtonWorld: PNewtonWorld; const contact: PNewtonJoint); cdecl;
PNewtonCreateContactCallback = ^NewtonCreateContactCallback;

NewtonDestroyContactCallback = procedure(const newtonWorld: PNewtonWorld; const contact: PNewtonJoint); cdecl;
PNewtonDestroyContactCallback = ^NewtonDestroyContactCallback;

NewtonWorldListenerDebugCallback = procedure(const world: PNewtonWorld; const listener: Pointer; const debugContext: Pointer); cdecl;
PNewtonWorldListenerDebugCallback = ^NewtonWorldListenerDebugCallback;

NewtonWorldListenerBodyDestroyCallback = procedure(const world: PNewtonWorld; const listenerUserData: Pointer; const body: pNewtonBody); cdecl;
PNewtonWorldListenerBodyDestroyCallback = ^NewtonWorldListenerBodyDestroyCallback;

NewtonWorldUpdateListenerCallback = procedure(const world: PNewtonWorld; const listenerUserData: Pointer; timestep: dfloat); cdecl;
PNewtonWorldUpdateListenerCallback = ^NewtonWorldUpdateListenerCallback;

NewtonWorldDestroyListenerCallback = procedure(const world: PNewtonWorld; const listenerUserData: Pointer); cdecl;
PNewtonWorldDestroyListenerCallback = ^NewtonWorldDestroyListenerCallback;

NewtonGetTimeInMicrosencondsCallback = function(): int64; cdecl;
PNewtonGetTimeInMicrosencondsCallback = ^NewtonGetTimeInMicrosencondsCallback;

NewtonSerializeCallback = procedure(const serializeHandle: Pointer; const buffer: Pointer; size: Integer); cdecl;
PNewtonSerializeCallback = ^NewtonSerializeCallback;

NewtonDeserializeCallback = procedure(const serializeHandle: Pointer; const buffer: Pointer; size: Integer); cdecl;
PNewtonDeserializeCallback = ^NewtonDeserializeCallback;

NewtonOnBodySerializationCallback = procedure(const body: pNewtonBody; const userData: Pointer; functionparam: PNewtonSerializeCallback; const serializeHandle: Pointer); cdecl;
PNewtonOnBodySerializationCallback = ^NewtonOnBodySerializationCallback;

NewtonOnBodyDeserializationCallback = procedure(const body: pNewtonBody; const userData: Pointer; functionparam: PNewtonDeserializeCallback; const serializeHandle: Pointer); cdecl;
PNewtonOnBodyDeserializationCallback = ^NewtonOnBodyDeserializationCallback;

NewtonOnJointSerializationCallback = procedure(const joint: PNewtonJoint; functionparam: PNewtonSerializeCallback; const serializeHandle: Pointer); cdecl;
PNewtonOnJointSerializationCallback = ^NewtonOnJointSerializationCallback;

NewtonOnJointDeserializationCallback = procedure(const body0: pNewtonBody; const body1: pNewtonBody; functionparam: PNewtonDeserializeCallback; const serializeHandle: Pointer); cdecl;
PNewtonOnJointDeserializationCallback = ^NewtonOnJointDeserializationCallback;

 // user collision callbacks
NewtonOnUserCollisionSerializationCallback = procedure(const userData: Pointer; functionparam: PNewtonSerializeCallback; const serializeHandle: Pointer); cdecl;
PNewtonOnUserCollisionSerializationCallback = ^NewtonOnUserCollisionSerializationCallback;

NewtonUserMeshCollisionDestroyCallback = procedure(const userData: Pointer); cdecl;
PNewtonUserMeshCollisionDestroyCallback = ^NewtonUserMeshCollisionDestroyCallback;

NewtonUserMeshCollisionRayHitCallback = function(const lineDescData: PNewtonUserMeshCollisionRayHitDesc): dfloat; cdecl;
PNewtonUserMeshCollisionRayHitCallback = ^NewtonUserMeshCollisionRayHitCallback;

NewtonUserMeshCollisionGetCollisionInfo = procedure(const userData: Pointer; const infoRecord: PNewtonCollisionInfoRecord); cdecl;
PNewtonUserMeshCollisionGetCollisionInfo = ^NewtonUserMeshCollisionGetCollisionInfo;

NewtonUserMeshCollisionAABBTest = function(const userData: Pointer; const boxP0: pfloat; const boxP1: pfloat): Integer; cdecl;
PNewtonUserMeshCollisionAABBTest = ^NewtonUserMeshCollisionAABBTest;

NewtonUserMeshCollisionGetFacesInAABB = function(const userData: Pointer; const p0: pfloat; const p1: pfloat; const vertexArray: Pfloat; const vertexCount: Pinteger; const vertexStrideInBytes: Pinteger; const indexList: Pinteger; maxIndexCount: Integer; const userDataList: Pinteger): Integer; cdecl;
PNewtonUserMeshCollisionGetFacesInAABB = ^NewtonUserMeshCollisionGetFacesInAABB;

NewtonUserMeshCollisionCollideCallback = procedure(const collideDescData: PNewtonUserMeshCollisionCollideDesc; const continueCollisionHandle: Pointer); cdecl;
PNewtonUserMeshCollisionCollideCallback = ^NewtonUserMeshCollisionCollideCallback;

NewtonTreeCollisionFaceCallback = function(const context: Pointer; const polygon: pfloat; strideInBytes: Integer; const indexArray: Pinteger; indexCount: Integer): Integer; cdecl;
PNewtonTreeCollisionFaceCallback = ^NewtonTreeCollisionFaceCallback;

NewtonCollisionTreeRayCastCallback = function(const body: pNewtonBody; const treeCollision: PNewtonCollision; intersection: dfloat; const normal: pfloat; faceId: Integer; const usedData: Pointer): dfloat; cdecl;
PNewtonCollisionTreeRayCastCallback = ^NewtonCollisionTreeRayCastCallback;

NewtonHeightFieldRayCastCallback = function(const body: pNewtonBody; const heightFieldCollision: PNewtonCollision; intersection: dfloat; row: Integer; col: Integer; const normal: pfloat; faceId: Integer; const usedData: Pointer): dfloat; cdecl;
PNewtonHeightFieldRayCastCallback = ^NewtonHeightFieldRayCastCallback;

NewtonCollisionCopyConstructionCallback = procedure(const newtonWorld: PNewtonWorld; const collision: PNewtonCollision; const sourceCollision: PNewtonCollision); cdecl;
PNewtonCollisionCopyConstructionCallback = ^NewtonCollisionCopyConstructionCallback;

NewtonCollisionDestructorCallback = procedure(const newtonWorld: PNewtonWorld; const collision: PNewtonCollision); cdecl;
PNewtonCollisionDestructorCallback = ^NewtonCollisionDestructorCallback;

NewtonTreeCollisionCallback = procedure(const bodyWithTreeCollision: pNewtonBody; const body: pNewtonBody; faceID: Integer; vertexCount: Integer; const vertex: pfloat; vertexStrideInBytes: Integer); cdecl;
PNewtonTreeCollisionCallback = ^NewtonTreeCollisionCallback;

NewtonBodyDestructor = procedure(const body: pNewtonBody); cdecl;
PNewtonBodyDestructor = ^NewtonBodyDestructor;

NewtonApplyForceAndTorque = procedure(const body: pNewtonBody; timestep: dfloat; threadIndex: Integer); cdecl;
PNewtonApplyForceAndTorque = ^NewtonApplyForceAndTorque;

NewtonSetTransform = procedure(const body: pNewtonBody; const matrix: pfloat; threadIndex: Integer); cdecl;
PNewtonSetTransform = ^NewtonSetTransform;

NewtonIslandUpdate = function(const newtonWorld: PNewtonWorld; const islandHandle: Pointer; bodyCount: Integer): Integer; cdecl;
PNewtonIslandUpdate = ^NewtonIslandUpdate;

NewtonFractureCompoundCollisionOnEmitCompoundFractured = procedure(const fracturedBody: pNewtonBody); cdecl;
PNewtonFractureCompoundCollisionOnEmitCompoundFractured = ^NewtonFractureCompoundCollisionOnEmitCompoundFractured;

NewtonFractureCompoundCollisionOnEmitChunk = procedure(const chunkBody: pNewtonBody; const fracturexChunkMesh: PNewtonFracturedCompoundMeshPart; const fracturedCompountCollision: PNewtonCollision); cdecl;
PNewtonFractureCompoundCollisionOnEmitChunk = ^NewtonFractureCompoundCollisionOnEmitChunk;

NewtonFractureCompoundCollisionReconstructMainMeshCallBack = procedure(const body: pNewtonBody; const mainMesh: PNewtonFracturedCompoundMeshPart; const fracturedCompountCollision: PNewtonCollision); cdecl;
PNewtonFractureCompoundCollisionReconstructMainMeshCallBack = ^NewtonFractureCompoundCollisionReconstructMainMeshCallBack;

NewtonWorldRayPrefilterCallback = function(const body: pNewtonBody; const collision: PNewtonCollision; const userData: Pointer): LongWord; cdecl;
PNewtonWorldRayPrefilterCallback = ^NewtonWorldRayPrefilterCallback;

NewtonWorldRayFilterCallback = function(const body: pNewtonBody; const shapeHit: PNewtonCollision; const hitContact: pfloat; const hitNormal: pfloat; collisionID: int64; const userData: Pointer; intersectParam: dfloat): dfloat; cdecl;
PNewtonWorldRayFilterCallback = ^NewtonWorldRayFilterCallback;

NewtonOnAABBOverlap = function(const contact: PNewtonJoint; timestep: dfloat; threadIndex: Integer): Integer; cdecl;
PNewtonOnAABBOverlap = ^NewtonOnAABBOverlap;

NewtonContactsProcess = procedure(const contact: PNewtonJoint; timestep: dfloat; threadIndex: Integer); cdecl;
PNewtonContactsProcess = ^NewtonContactsProcess;

NewtonOnCompoundSubCollisionAABBOverlap = function(const contact: PNewtonJoint; timestep: dfloat; const body0: pNewtonBody; const collisionNode0: Pointer; const body1: pNewtonBody; const collisionNode1: Pointer; threadIndex: Integer): Integer; cdecl;
PNewtonOnCompoundSubCollisionAABBOverlap = ^NewtonOnCompoundSubCollisionAABBOverlap;

NewtonOnContactGeneration = function(const material: PNewtonMaterial; const body0: pNewtonBody; const collision0: PNewtonCollision; const body1: pNewtonBody; const collision1: PNewtonCollision; const contactBuffer: PNewtonUserContactPoint; maxCount: Integer; threadIndex: Integer): Integer; cdecl;
PNewtonOnContactGeneration = ^NewtonOnContactGeneration;

NewtonBodyIterator = function(const body: pNewtonBody; const userData: Pointer): Integer; cdecl;
PNewtonBodyIterator = ^NewtonBodyIterator;

NewtonJointIterator = procedure(const joint: PNewtonJoint; const userData: Pointer); cdecl;
PNewtonJointIterator = ^NewtonJointIterator;

NewtonCollisionIterator = procedure(const userData: Pointer; vertexCount: Integer; const faceArray: pfloat; faceId: Integer); cdecl;
PNewtonCollisionIterator = ^NewtonCollisionIterator;

NewtonBallCallback = procedure(const ball: PNewtonJoint; timestep: dfloat); cdecl;
PNewtonBallCallback = ^NewtonBallCallback;

NewtonHingeCallback = function(const hinge: PNewtonJoint; const desc: PNewtonHingeSliderUpdateDesc): LongWord; cdecl;
PNewtonHingeCallback = ^NewtonHingeCallback;

NewtonSliderCallback = function(const slider: PNewtonJoint; const desc: PNewtonHingeSliderUpdateDesc): LongWord; cdecl;
PNewtonSliderCallback = ^NewtonSliderCallback;

NewtonUniversalCallback = function(const universal: PNewtonJoint; const desc: PNewtonHingeSliderUpdateDesc): LongWord; cdecl;
PNewtonUniversalCallback = ^NewtonUniversalCallback;

NewtonCorkscrewCallback = function(const corkscrew: PNewtonJoint; const desc: PNewtonHingeSliderUpdateDesc): LongWord; cdecl;
PNewtonCorkscrewCallback = ^NewtonCorkscrewCallback;

NewtonUserBilateralCallback = procedure(const userJoint: PNewtonJoint; timestep: dfloat; threadIndex: Integer); cdecl;
PNewtonUserBilateralCallback = ^NewtonUserBilateralCallback;

NewtonUserBilateralGetInfoCallback = procedure(const userJoint: PNewtonJoint; const info: PNewtonJointRecord); cdecl;
PNewtonUserBilateralGetInfoCallback = ^NewtonUserBilateralGetInfoCallback;

NewtonConstraintDestructor = procedure(const me: PNewtonJoint); cdecl;
PNewtonConstraintDestructor = ^NewtonConstraintDestructor;

NewtonJobTask = procedure(const world: PNewtonWorld; const userData: Pointer; threadIndex: Integer); cdecl;
PNewtonJobTask = ^NewtonJobTask;

NewtonReportProgress = function(normalizedProgressPercent: dfloat; const userData: Pointer): Integer; cdecl;
PNewtonReportProgress = ^NewtonReportProgress;

 // **********************************************************************************************
 //
 // world control functions
 //
 // **********************************************************************************************
function NewtonWorldGetVersion (): Integer; cdecl; external NEWTON_API;
function NewtonWorldFloatSize (): Integer; cdecl; external NEWTON_API;
function NewtonGetMemoryUsed (): Integer; cdecl; external NEWTON_API;
procedure NewtonSetMemorySystem (malloc: NewtonAllocMemory; free: NewtonFreeMemory); cdecl; external NEWTON_API;
function NewtonCreate (): PNewtonWorld; cdecl; external NEWTON_API;
procedure NewtonDestroy (const newtonWorld: PNewtonWorld); cdecl; external NEWTON_API;
procedure NewtonDestroyAllBodies (const newtonWorld: PNewtonWorld); cdecl; external NEWTON_API;
function NewtonGetPostUpdateCallback (const newtonWorld: PNewtonWorld): PNewtonPostUpdateCallback; cdecl; external NEWTON_API;
procedure NewtonSetPostUpdateCallback (const newtonWorld: PNewtonWorld; callback: NewtonPostUpdateCallback); cdecl; external NEWTON_API;
function NewtonAlloc (sizeInBytes: Integer): Pointer; cdecl; external NEWTON_API;
procedure NewtonFree (const ptr: Pointer); cdecl; external NEWTON_API;
procedure NewtonLoadPlugins (const newtonWorld: PNewtonWorld; const plugInPath: pchar); cdecl; external NEWTON_API;
procedure NewtonUnloadPlugins (const newtonWorld: PNewtonWorld); cdecl; external NEWTON_API;
function NewtonCurrentPlugin (const newtonWorld: PNewtonWorld): Pointer; cdecl; external NEWTON_API;
function NewtonGetFirstPlugin (const newtonWorld: PNewtonWorld): Pointer; cdecl; external NEWTON_API;
function NewtonGetPreferedPlugin (const newtonWorld: PNewtonWorld): Pointer; cdecl; external NEWTON_API;
function NewtonGetNextPlugin (const newtonWorld: PNewtonWorld; const plugin: Pointer): Pointer; cdecl; external NEWTON_API;
function NewtonGetPluginString (const newtonWorld: PNewtonWorld; const plugin: Pointer): pchar; cdecl; external NEWTON_API;
procedure NewtonSelectPlugin (const newtonWorld: PNewtonWorld; const plugin: Pointer); cdecl; external NEWTON_API;
function NewtonGetContactMergeTolerance (const newtonWorld: PNewtonWorld): dfloat; cdecl; external NEWTON_API;
procedure NewtonSetContactMergeTolerance (const newtonWorld: PNewtonWorld; tolerance: dfloat); cdecl; external NEWTON_API;
procedure NewtonInvalidateCache (const newtonWorld: PNewtonWorld); cdecl; external NEWTON_API;
procedure NewtonSetSolverIterations (const newtonWorld: PNewtonWorld; model: Integer); cdecl; external NEWTON_API;
function NewtonGetSolverIterations (const newtonWorld: PNewtonWorld): Integer; cdecl; external NEWTON_API;
procedure NewtonSetParallelSolverOnLargeIsland (const newtonWorld: PNewtonWorld; mode: Integer); cdecl; external NEWTON_API;
function NewtonGetParallelSolverOnLargeIsland (const newtonWorld: PNewtonWorld): Integer; cdecl; external NEWTON_API;
function NewtonGetBroadphaseAlgorithm (const newtonWorld: PNewtonWorld): Integer; cdecl; external NEWTON_API;
procedure NewtonSelectBroadphaseAlgorithm (const newtonWorld: PNewtonWorld; algorithmType: Integer); cdecl; external NEWTON_API;
procedure NewtonResetBroadphase (const newtonWorld: PNewtonWorld); cdecl; external NEWTON_API;
procedure NewtonUpdate (const newtonWorld: PNewtonWorld; timestep: dfloat); cdecl; external NEWTON_API;
procedure NewtonUpdateAsync (const newtonWorld: PNewtonWorld; timestep: dfloat); cdecl; external NEWTON_API;
procedure NewtonWaitForUpdateToFinish (const newtonWorld: PNewtonWorld); cdecl; external NEWTON_API;
function NewtonGetNumberOfSubsteps (const newtonWorld: PNewtonWorld): Integer; cdecl; external NEWTON_API;
procedure NewtonSetNumberOfSubsteps (const newtonWorld: PNewtonWorld; subSteps: Integer); cdecl; external NEWTON_API;
function NewtonGetLastUpdateTime (const newtonWorld: PNewtonWorld): dfloat; cdecl; external NEWTON_API;
procedure NewtonSerializeToFile (const newtonWorld: PNewtonWorld; const filename: pchar; bodyCallback: NewtonOnBodySerializationCallback; const bodyUserData: Pointer); cdecl; external NEWTON_API;
procedure NewtonDeserializeFromFile (const newtonWorld: PNewtonWorld; const filename: pchar; bodyCallback: NewtonOnBodyDeserializationCallback; const bodyUserData: Pointer); cdecl; external NEWTON_API;
procedure NewtonSerializeScene (const newtonWorld: PNewtonWorld; bodyCallback: NewtonOnBodySerializationCallback; const bodyUserData: Pointer; serializeCallback: NewtonSerializeCallback; const serializeHandle: Pointer); cdecl; external NEWTON_API;
procedure NewtonDeserializeScene (const newtonWorld: PNewtonWorld; bodyCallback: NewtonOnBodyDeserializationCallback; const bodyUserData: Pointer; serializeCallback: NewtonDeserializeCallback; const serializeHandle: Pointer); cdecl; external NEWTON_API;
function NewtonFindSerializedBody (const newtonWorld: PNewtonWorld; bodySerializedID: Integer): pNewtonBody; cdecl; external NEWTON_API;
procedure NewtonSetJointSerializationCallbacks (const newtonWorld: PNewtonWorld; serializeJoint: NewtonOnJointSerializationCallback; deserializeJoint: NewtonOnJointDeserializationCallback); cdecl; external NEWTON_API;
procedure NewtonGetJointSerializationCallbacks (const newtonWorld: PNewtonWorld; const serializeJoint: NewtonOnJointSerializationCallback; const deserializeJoint: NewtonOnJointDeserializationCallback); cdecl; external NEWTON_API;
procedure NewtonWorldCriticalSectionLock (const newtonWorld: PNewtonWorld; threadIndex: Integer); cdecl; external NEWTON_API;
procedure NewtonWorldCriticalSectionUnlock (const newtonWorld: PNewtonWorld); cdecl; external NEWTON_API;
procedure NewtonSetThreadsCount (const newtonWorld: PNewtonWorld; threads: Integer); cdecl; external NEWTON_API;
function NewtonGetThreadsCount (const newtonWorld: PNewtonWorld): Integer; cdecl; external NEWTON_API;
function NewtonGetMaxThreadsCount (const newtonWorld: PNewtonWorld): Integer; cdecl; external NEWTON_API;
procedure NewtonDispachThreadJob (const newtonWorld: PNewtonWorld; task: NewtonJobTask; const usedData: Pointer; const functionName: pchar); cdecl; external NEWTON_API;
procedure NewtonSyncThreadJobs (const newtonWorld: PNewtonWorld); cdecl; external NEWTON_API;
function NewtonAtomicAdd (const ptr: Pinteger; value: Integer): Integer; cdecl; external NEWTON_API;
function NewtonAtomicSwap (const ptr: Pinteger; value: Integer): Integer; cdecl; external NEWTON_API;
procedure NewtonYield (); cdecl; external NEWTON_API;
procedure NewtonSetIslandUpdateEvent (const newtonWorld: PNewtonWorld; islandUpdate: NewtonIslandUpdate); cdecl; external NEWTON_API;
procedure NewtonWorldForEachJointDo (const newtonWorld: PNewtonWorld; callback: NewtonJointIterator; const userData: Pointer); cdecl; external NEWTON_API;
procedure NewtonWorldForEachBodyInAABBDo (const newtonWorld: PNewtonWorld; const p0: pfloat; const p1: pfloat; callback: NewtonBodyIterator; const userData: Pointer); cdecl; external NEWTON_API;
procedure NewtonWorldSetUserData (const newtonWorld: PNewtonWorld; const userData: Pointer); cdecl; external NEWTON_API;
function NewtonWorldGetUserData (const newtonWorld: PNewtonWorld): Pointer; cdecl; external NEWTON_API;
function NewtonWorldAddListener (const newtonWorld: PNewtonWorld; const nameId: pchar; const listenerUserData: Pointer): Pointer; cdecl; external NEWTON_API;
function NewtonWorldGetListener (const newtonWorld: PNewtonWorld; const nameId: pchar): Pointer; cdecl; external NEWTON_API;
procedure NewtonWorldListenerSetDebugCallback (const newtonWorld: PNewtonWorld; const listener: Pointer; callback: NewtonWorldListenerDebugCallback); cdecl; external NEWTON_API;
procedure NewtonWorldListenerSetPostStepCallback (const newtonWorld: PNewtonWorld; const listener: Pointer; callback: NewtonWorldUpdateListenerCallback); cdecl; external NEWTON_API;
procedure NewtonWorldListenerSetPreUpdateCallback (const newtonWorld: PNewtonWorld; const listener: Pointer; callback: NewtonWorldUpdateListenerCallback); cdecl; external NEWTON_API;
procedure NewtonWorldListenerSetPostUpdateCallback (const newtonWorld: PNewtonWorld; const listener: Pointer; callback: NewtonWorldUpdateListenerCallback); cdecl; external NEWTON_API;
procedure NewtonWorldListenerSetDestructorCallback (const newtonWorld: PNewtonWorld; const listener: Pointer; callback: NewtonWorldDestroyListenerCallback); cdecl; external NEWTON_API;
procedure NewtonWorldListenerSetBodyDestroyCallback (const newtonWorld: PNewtonWorld; const listener: Pointer; callback: NewtonWorldListenerBodyDestroyCallback); cdecl; external NEWTON_API;
procedure NewtonWorldListenerDebug (const newtonWorld: PNewtonWorld; const context: Pointer); cdecl; external NEWTON_API;
function NewtonWorldGetListenerUserData (const newtonWorld: PNewtonWorld; const listener: Pointer): Pointer; cdecl; external NEWTON_API;
function NewtonWorldListenerGetBodyDestroyCallback (const newtonWorld: PNewtonWorld; const listener: Pointer): PNewtonWorldListenerBodyDestroyCallback; cdecl; external NEWTON_API;
procedure NewtonWorldSetDestructorCallback (const newtonWorld: PNewtonWorld; destructorparam: NewtonWorldDestructorCallback); cdecl; external NEWTON_API;
function NewtonWorldGetDestructorCallback (const newtonWorld: PNewtonWorld): PNewtonWorldDestructorCallback; cdecl; external NEWTON_API;
procedure NewtonWorldSetCollisionConstructorDestructorCallback (const newtonWorld: PNewtonWorld; constructorparam: NewtonCollisionCopyConstructionCallback; destructorparam: NewtonCollisionDestructorCallback); cdecl; external NEWTON_API;
procedure NewtonWorldSetCreateDestroyContactCallback (const newtonWorld: PNewtonWorld; createContact: NewtonCreateContactCallback; destroyContact: NewtonDestroyContactCallback); cdecl; external NEWTON_API;
procedure NewtonWorldRayCast (const newtonWorld: PNewtonWorld; const p0: pfloat; const p1: pfloat; filter: NewtonWorldRayFilterCallback; const userData: Pointer; prefilter: NewtonWorldRayPrefilterCallback; threadIndex: Integer); cdecl; external NEWTON_API;
function NewtonWorldConvexCast (const newtonWorld: PNewtonWorld; const matrix: pfloat; const target: pfloat; const shape: PNewtonCollision; const param: pfloat; const userData: Pointer; prefilter: NewtonWorldRayPrefilterCallback; const info: PNewtonWorldConvexCastReturnInfo; maxContactsCount: Integer; threadIndex: Integer): Integer; cdecl; external NEWTON_API;
function NewtonWorldCollide (const newtonWorld: PNewtonWorld; const matrix: pfloat; const shape: PNewtonCollision; const userData: Pointer; prefilter: NewtonWorldRayPrefilterCallback; const info: PNewtonWorldConvexCastReturnInfo; maxContactsCount: Integer; threadIndex: Integer): Integer; cdecl; external NEWTON_API;
	
// world utility functions
function NewtonWorldGetBodyCount (const newtonWorld: PNewtonWorld): Integer; cdecl; external NEWTON_API;
function NewtonWorldGetConstraintCount (const newtonWorld: PNewtonWorld): Integer; cdecl; external NEWTON_API;
function NewtonWorldFindJoint (const body0: pNewtonBody; const body1: pNewtonBody): PNewtonJoint; cdecl; external NEWTON_API;

// **********************************************************************************************
//
// Simulation islands 
//
// **********************************************************************************************
function NewtonIslandGetBody (const island: Pointer; bodyIndex: Integer): pNewtonBody; cdecl; external NEWTON_API;
procedure NewtonIslandGetBodyAABB (const island: Pointer; bodyIndex: Integer; const p0: pfloat; const p1: pfloat); cdecl; external NEWTON_API;

// **********************************************************************************************
//
// Physics Material Section
//
// **********************************************************************************************
function NewtonMaterialCreateGroupID (const newtonWorld: PNewtonWorld): Integer; cdecl; external NEWTON_API;
function NewtonMaterialGetDefaultGroupID (const newtonWorld: PNewtonWorld): Integer; cdecl; external NEWTON_API;
procedure NewtonMaterialDestroyAllGroupID (const newtonWorld: PNewtonWorld); cdecl; external NEWTON_API;

// material definitions that can not be overwritten in function callback
function NewtonMaterialGetUserData (const newtonWorld: PNewtonWorld; id0: Integer; id1: Integer): Pointer; cdecl; external NEWTON_API;
procedure NewtonMaterialSetSurfaceThickness (const newtonWorld: PNewtonWorld; id0: Integer; id1: Integer; thickness: dfloat); cdecl; external NEWTON_API;

//	deprecated, not longer continue collision is set on the material  	
//	NEWTON_API void NewtonMaterialSetContinuousCollisionMode (const NewtonWorld* const newtonWorld, int id0, int id1, int state);
	
procedure NewtonMaterialSetCallbackUserData (const newtonWorld: PNewtonWorld; id0: Integer; id1: Integer; const userData: Pointer); cdecl; external NEWTON_API;
procedure NewtonMaterialSetContactGenerationCallback (const newtonWorld: PNewtonWorld; id0: Integer; id1: Integer; contactGeneration: NewtonOnContactGeneration); cdecl; external NEWTON_API;
procedure NewtonMaterialSetCompoundCollisionCallback (const newtonWorld: PNewtonWorld; id0: Integer; id1: Integer; compoundAabbOverlap: NewtonOnCompoundSubCollisionAABBOverlap); cdecl; external NEWTON_API;
procedure NewtonMaterialSetCollisionCallback (const newtonWorld: PNewtonWorld; id0: Integer; id1: Integer; aabbOverlap: NewtonOnAABBOverlap; process: NewtonContactsProcess); cdecl; external NEWTON_API;
procedure NewtonMaterialSetDefaultSoftness (const newtonWorld: PNewtonWorld; id0: Integer; id1: Integer; value: dfloat); cdecl; external NEWTON_API;
procedure NewtonMaterialSetDefaultElasticity (const newtonWorld: PNewtonWorld; id0: Integer; id1: Integer; elasticCoef: dfloat); cdecl; external NEWTON_API;
procedure NewtonMaterialSetDefaultCollidable (const newtonWorld: PNewtonWorld; id0: Integer; id1: Integer; state: Integer); cdecl; external NEWTON_API;
procedure NewtonMaterialSetDefaultFriction (const newtonWorld: PNewtonWorld; id0: Integer; id1: Integer; staticFriction: dfloat; kineticFriction: dfloat); cdecl; external NEWTON_API;
procedure NewtonMaterialJointResetIntraJointCollision (const newtonWorld: PNewtonWorld; id0: Integer; id1: Integer); cdecl; external NEWTON_API;
procedure NewtonMaterialJointResetSelftJointCollision (const newtonWorld: PNewtonWorld; id0: Integer; id1: Integer); cdecl; external NEWTON_API;
function NewtonWorldGetFirstMaterial (const newtonWorld: PNewtonWorld): PNewtonMaterial; cdecl; external NEWTON_API;
function NewtonWorldGetNextMaterial (const newtonWorld: PNewtonWorld; const material: PNewtonMaterial): PNewtonMaterial; cdecl; external NEWTON_API;
function NewtonWorldGetFirstBody (const newtonWorld: PNewtonWorld): pNewtonBody; cdecl; external NEWTON_API;
function NewtonWorldGetNextBody (const newtonWorld: PNewtonWorld; const curBody: pNewtonBody): pNewtonBody; cdecl; external NEWTON_API;


// **********************************************************************************************
//
// Physics Contact control functions
//
// **********************************************************************************************
function NewtonMaterialGetMaterialPairUserData (const material: PNewtonMaterial): Pointer; cdecl; external NEWTON_API;
function NewtonMaterialGetContactFaceAttribute (const material: PNewtonMaterial): LongWord; cdecl; external NEWTON_API;
function NewtonMaterialGetBodyCollidingShape (const material: PNewtonMaterial; const body: pNewtonBody): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonMaterialGetContactNormalSpeed (const material: PNewtonMaterial): dfloat; cdecl; external NEWTON_API;
procedure NewtonMaterialGetContactForce (const material: PNewtonMaterial; const body: pNewtonBody; const force: pfloat); cdecl; external NEWTON_API;
procedure NewtonMaterialGetContactPositionAndNormal (const material: PNewtonMaterial; const body: pNewtonBody; const posit: pfloat; const normal: pfloat); cdecl; external NEWTON_API;
procedure NewtonMaterialGetContactTangentDirections (const material: PNewtonMaterial; const body: pNewtonBody; const dir0: pfloat; const dir1: pfloat); cdecl; external NEWTON_API;
function NewtonMaterialGetContactTangentSpeed (const material: PNewtonMaterial; index: Integer): dfloat; cdecl; external NEWTON_API;
function NewtonMaterialGetContactMaxNormalImpact (const material: PNewtonMaterial): dfloat; cdecl; external NEWTON_API;
function NewtonMaterialGetContactMaxTangentImpact (const material: PNewtonMaterial; index: Integer): dfloat; cdecl; external NEWTON_API;
function NewtonMaterialGetContactPenetration (const material: PNewtonMaterial): dfloat; cdecl; external NEWTON_API;
procedure NewtonMaterialSetAsSoftContact (const material: PNewtonMaterial; relaxation: dfloat); cdecl; external NEWTON_API;
procedure NewtonMaterialSetContactSoftness (const material: PNewtonMaterial; softness: dfloat); cdecl; external NEWTON_API;
procedure NewtonMaterialSetContactThickness (const material: PNewtonMaterial; thickness: dfloat); cdecl; external NEWTON_API;
procedure NewtonMaterialSetContactElasticity (const material: PNewtonMaterial; restitution: dfloat); cdecl; external NEWTON_API;
procedure NewtonMaterialSetContactFrictionState (const material: PNewtonMaterial; state: Integer; index: Integer); cdecl; external NEWTON_API;
procedure NewtonMaterialSetContactFrictionCoef (const material: PNewtonMaterial; staticFrictionCoef: dfloat; kineticFrictionCoef: dfloat; index: Integer); cdecl; external NEWTON_API;
procedure NewtonMaterialSetContactNormalAcceleration (const material: PNewtonMaterial; accel: dfloat); cdecl; external NEWTON_API;
procedure NewtonMaterialSetContactNormalDirection (const material: PNewtonMaterial; const directionVector: pfloat); cdecl; external NEWTON_API;
procedure NewtonMaterialSetContactPosition (const material: PNewtonMaterial; const position: pfloat); cdecl; external NEWTON_API;
procedure NewtonMaterialSetContactTangentFriction (const material: PNewtonMaterial; friction: dfloat; index: Integer); cdecl; external NEWTON_API;
procedure NewtonMaterialSetContactTangentAcceleration (const material: PNewtonMaterial; accel: dfloat; index: Integer); cdecl; external NEWTON_API;
procedure NewtonMaterialContactRotateTangentDirections (const material: PNewtonMaterial; const directionVector: pfloat); cdecl; external NEWTON_API;
{}function NewtonMaterialGetContactPruningTolerance (const contactJoint: PNewtonJoint): dfloat; cdecl; external NEWTON_API;
{}procedure NewtonMaterialSetContactPruningTolerance (const contactJoint: PNewtonJoint; tolerance: dfloat); cdecl; external NEWTON_API;

// **********************************************************************************************
//
// convex collision primitives creation functions
//
// **********************************************************************************************
function NewtonCreateNull (const newtonWorld: PNewtonWorld): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonCreateSphere (const newtonWorld: PNewtonWorld; radius: dfloat; shapeID: Integer; const offsetMatrix: pfloat): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonCreateBox (const newtonWorld: PNewtonWorld; dx: dfloat; dy: dfloat; dz: dfloat; shapeID: Integer; const offsetMatrix: pfloat): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonCreateCone (const newtonWorld: PNewtonWorld; radius: dfloat; height: dfloat; shapeID: Integer; const offsetMatrix: pfloat): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonCreateCapsule (const newtonWorld: PNewtonWorld; radius0: dfloat; radius1: dfloat; height: dfloat; shapeID: Integer; const offsetMatrix: pfloat): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonCreateCylinder (const newtonWorld: PNewtonWorld; radio0: dfloat; radio1: dfloat; height: dfloat; shapeID: Integer; const offsetMatrix: pfloat): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonCreateChamferCylinder (const newtonWorld: PNewtonWorld; radius: dfloat; height: dfloat; shapeID: Integer; const offsetMatrix: pfloat): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonCreateConvexHull (const newtonWorld: PNewtonWorld; count: Integer; const vertexCloud: pfloat; strideInBytes: Integer; tolerance: dfloat; shapeID: Integer; const offsetMatrix: pfloat): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonCreateConvexHullFromMesh (const newtonWorld: PNewtonWorld; const mesh: PNewtonMesh; tolerance: dfloat; shapeID: Integer): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonCollisionGetMode (const convexCollision: PNewtonCollision): Integer; cdecl; external NEWTON_API;
procedure NewtonCollisionSetMode (const convexCollision: PNewtonCollision; mode: Integer); cdecl; external NEWTON_API;
function NewtonConvexHullGetFaceIndices (const convexHullCollision: PNewtonCollision; face: Integer; const faceIndices: Pinteger): Integer; cdecl; external NEWTON_API;
function NewtonConvexHullGetVertexData (const convexHullCollision: PNewtonCollision; const vertexData: Pfloat; strideInBytes: Pinteger): Integer; cdecl; external NEWTON_API;
function NewtonConvexCollisionCalculateVolume (const convexCollision: PNewtonCollision): dfloat; cdecl; external NEWTON_API;
procedure NewtonConvexCollisionCalculateInertialMatrix (const convexCollision: PNewtonCollision; const inertia: pfloat; const origin: pfloat); cdecl; external NEWTON_API;
function NewtonConvexCollisionCalculateBuoyancyVolume (const convexCollision: PNewtonCollision; const matrix: pfloat; const fluidPlane: pfloat; const centerOfBuoyancy: pfloat): dfloat; cdecl; external NEWTON_API;
function NewtonCollisionDataPointer (const convexCollision: PNewtonCollision): Pointer; cdecl; external NEWTON_API;

// **********************************************************************************************
//
// compound collision primitives creation functions
//
// **********************************************************************************************
function NewtonCreateCompoundCollision (const newtonWorld: PNewtonWorld; shapeID: Integer): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonCreateCompoundCollisionFromMesh (const newtonWorld: PNewtonWorld; const mesh: PNewtonMesh; hullTolerance: dfloat; shapeID: Integer; subShapeID: Integer): PNewtonCollision; cdecl; external NEWTON_API;
procedure NewtonCompoundCollisionBeginAddRemove (const compoundCollision: PNewtonCollision); cdecl; external NEWTON_API;
function NewtonCompoundCollisionAddSubCollision (const compoundCollision: PNewtonCollision; const convexCollision: PNewtonCollision): Pointer; cdecl; external NEWTON_API;
procedure NewtonCompoundCollisionRemoveSubCollision (const compoundCollision: PNewtonCollision; const collisionNode: Pointer); cdecl; external NEWTON_API;
procedure NewtonCompoundCollisionRemoveSubCollisionByIndex (const compoundCollision: PNewtonCollision; nodeIndex: Integer); cdecl; external NEWTON_API;
procedure NewtonCompoundCollisionSetSubCollisionMatrix (const compoundCollision: PNewtonCollision; const collisionNode: Pointer; const matrix: pfloat); cdecl; external NEWTON_API;
procedure NewtonCompoundCollisionEndAddRemove (const compoundCollision: PNewtonCollision); cdecl; external NEWTON_API;
function NewtonCompoundCollisionGetFirstNode (const compoundCollision: PNewtonCollision): Pointer; cdecl; external NEWTON_API;
function NewtonCompoundCollisionGetNextNode (const compoundCollision: PNewtonCollision; const collisionNode: Pointer): Pointer; cdecl; external NEWTON_API;
function NewtonCompoundCollisionGetNodeByIndex (const compoundCollision: PNewtonCollision; index: Integer): Pointer; cdecl; external NEWTON_API;
function NewtonCompoundCollisionGetNodeIndex (const compoundCollision: PNewtonCollision; const collisionNode: Pointer): Integer; cdecl; external NEWTON_API;
function NewtonCompoundCollisionGetCollisionFromNode (const compoundCollision: PNewtonCollision; const collisionNode: Pointer): PNewtonCollision; cdecl; external NEWTON_API;

// **********************************************************************************************
//
// Fractured compound collision primitives interface
//
// **********************************************************************************************
function NewtonCreateFracturedCompoundCollision (const newtonWorld: PNewtonWorld; const solidMesh: PNewtonMesh; shapeID: Integer; fracturePhysicsMaterialID: Integer; pointcloudCount: Integer; const vertexCloud: pfloat; strideInBytes: Integer; materialID: Integer; const textureMatrix: pfloat; regenerateMainMeshCallback: NewtonFractureCompoundCollisionReconstructMainMeshCallBack; emitFracturedCompound: NewtonFractureCompoundCollisionOnEmitCompoundFractured; emitFracfuredChunk: NewtonFractureCompoundCollisionOnEmitChunk): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonFracturedCompoundPlaneClip (const fracturedCompound: PNewtonCollision; const plane: pfloat): PNewtonCollision; cdecl; external NEWTON_API;
procedure NewtonFracturedCompoundSetCallbacks (const fracturedCompound: PNewtonCollision; regenerateMainMeshCallback: NewtonFractureCompoundCollisionReconstructMainMeshCallBack; emitFracturedCompound: NewtonFractureCompoundCollisionOnEmitCompoundFractured; emitFracfuredChunk: NewtonFractureCompoundCollisionOnEmitChunk); cdecl; external NEWTON_API;
function NewtonFracturedCompoundIsNodeFreeToDetach (const fracturedCompound: PNewtonCollision; const collisionNode: Pointer): Integer; cdecl; external NEWTON_API;
function NewtonFracturedCompoundNeighborNodeList (const fracturedCompound: PNewtonCollision; const collisionNode: Pointer; const list: PPointer; maxCount: Integer): Integer; cdecl; external NEWTON_API;
function NewtonFracturedCompoundGetMainMesh (const fracturedCompound: PNewtonCollision): PNewtonFracturedCompoundMeshPart; cdecl; external NEWTON_API;
function NewtonFracturedCompoundGetFirstSubMesh (const fracturedCompound: PNewtonCollision): PNewtonFracturedCompoundMeshPart; cdecl; external NEWTON_API;
function NewtonFracturedCompoundGetNextSubMesh (const fracturedCompound: PNewtonCollision; const subMesh: PNewtonFracturedCompoundMeshPart): PNewtonFracturedCompoundMeshPart; cdecl; external NEWTON_API;
function NewtonFracturedCompoundCollisionGetVertexCount (const fracturedCompound: PNewtonCollision; const meshOwner: PNewtonFracturedCompoundMeshPart): Integer; cdecl; external NEWTON_API;
function NewtonFracturedCompoundCollisionGetVertexPositions (const fracturedCompound: PNewtonCollision; const meshOwner: PNewtonFracturedCompoundMeshPart): pfloat; cdecl; external NEWTON_API;
function NewtonFracturedCompoundCollisionGetVertexNormals (const fracturedCompound: PNewtonCollision; const meshOwner: PNewtonFracturedCompoundMeshPart): pfloat; cdecl; external NEWTON_API;
function NewtonFracturedCompoundCollisionGetVertexUVs (const fracturedCompound: PNewtonCollision; const meshOwner: PNewtonFracturedCompoundMeshPart): pfloat; cdecl; external NEWTON_API;
function NewtonFracturedCompoundMeshPartGetIndexStream (const fracturedCompound: PNewtonCollision; const meshOwner: PNewtonFracturedCompoundMeshPart; const segment: Pointer; const index: Pinteger): Integer; cdecl; external NEWTON_API;
function NewtonFracturedCompoundMeshPartGetFirstSegment (const fractureCompoundMeshPart: PNewtonFracturedCompoundMeshPart): Pointer; cdecl; external NEWTON_API;
function NewtonFracturedCompoundMeshPartGetNextSegment (const fractureCompoundMeshSegment: Pointer): Pointer; cdecl; external NEWTON_API;
function NewtonFracturedCompoundMeshPartGetMaterial (const fractureCompoundMeshSegment: Pointer): Integer; cdecl; external NEWTON_API;
function NewtonFracturedCompoundMeshPartGetIndexCount (const fractureCompoundMeshSegment: Pointer): Integer; cdecl; external NEWTON_API;

// **********************************************************************************************
//
// scene collision are static compound collision that can take polygonal static collisions
//
// **********************************************************************************************
function NewtonCreateSceneCollision (const newtonWorld: PNewtonWorld; shapeID: Integer): PNewtonCollision; cdecl; external NEWTON_API;
procedure NewtonSceneCollisionBeginAddRemove (const sceneCollision: PNewtonCollision); cdecl; external NEWTON_API;
function NewtonSceneCollisionAddSubCollision (const sceneCollision: PNewtonCollision; const collision: PNewtonCollision): Pointer; cdecl; external NEWTON_API;
procedure NewtonSceneCollisionRemoveSubCollision (const compoundCollision: PNewtonCollision; const collisionNode: Pointer); cdecl; external NEWTON_API;
procedure NewtonSceneCollisionRemoveSubCollisionByIndex (const sceneCollision: PNewtonCollision; nodeIndex: Integer); cdecl; external NEWTON_API;
procedure NewtonSceneCollisionSetSubCollisionMatrix (const sceneCollision: PNewtonCollision; const collisionNode: Pointer; const matrix: pfloat); cdecl; external NEWTON_API;
procedure NewtonSceneCollisionEndAddRemove (const sceneCollision: PNewtonCollision); cdecl; external NEWTON_API;
function NewtonSceneCollisionGetFirstNode (const sceneCollision: PNewtonCollision): Pointer; cdecl; external NEWTON_API;
function NewtonSceneCollisionGetNextNode (const sceneCollision: PNewtonCollision; const collisionNode: Pointer): Pointer; cdecl; external NEWTON_API;
function NewtonSceneCollisionGetNodeByIndex (const sceneCollision: PNewtonCollision; index: Integer): Pointer; cdecl; external NEWTON_API;
function NewtonSceneCollisionGetNodeIndex (const sceneCollision: PNewtonCollision; const collisionNode: Pointer): Integer; cdecl; external NEWTON_API;
function NewtonSceneCollisionGetCollisionFromNode (const sceneCollision: PNewtonCollision; const collisionNode: Pointer): PNewtonCollision; cdecl; external NEWTON_API;

//  ***********************************************************************************************************
//
//	User Static mesh collision interface
//
// ***********************************************************************************************************
function NewtonCreateUserMeshCollision (const newtonWorld: PNewtonWorld; const minBox: pfloat; const maxBox: pfloat; const userData: Pointer; collideCallback: NewtonUserMeshCollisionCollideCallback; rayHitCallback: NewtonUserMeshCollisionRayHitCallback; destroyCallback: NewtonUserMeshCollisionDestroyCallback; getInfoCallback: NewtonUserMeshCollisionGetCollisionInfo; getLocalAABBCallback: NewtonUserMeshCollisionAABBTest; facesInAABBCallback: NewtonUserMeshCollisionGetFacesInAABB; serializeCallback: NewtonOnUserCollisionSerializationCallback; shapeID: Integer): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonUserMeshCollisionContinuousOverlapTest (const collideDescData: PNewtonUserMeshCollisionCollideDesc; const continueCollisionHandle: Pointer; const minAabb: pfloat; const maxAabb: pfloat): Integer; cdecl; external NEWTON_API;
	
//  ***********************************************************************************************************
//
//	Collision serialization functions
//
// ***********************************************************************************************************
function NewtonCreateCollisionFromSerialization (const newtonWorld: PNewtonWorld; deserializeFunction: NewtonDeserializeCallback; const serializeHandle: Pointer): PNewtonCollision; cdecl; external NEWTON_API;
procedure NewtonCollisionSerialize (const newtonWorld: PNewtonWorld; const collision: PNewtonCollision; serializeFunction: NewtonSerializeCallback; const serializeHandle: Pointer); cdecl; external NEWTON_API;
procedure NewtonCollisionGetInfo (const collision: PNewtonCollision; const collisionInfo: PNewtonCollisionInfoRecord); cdecl; external NEWTON_API;

// **********************************************************************************************
//
// Static collision shapes functions
//
// **********************************************************************************************
function NewtonCreateHeightFieldCollision (const newtonWorld: PNewtonWorld; width: Integer; height: Integer; gridsDiagonals: Integer; elevationdatType: Integer; const elevationMap: Pointer; const attributeMap: pchar; verticalScale: dfloat; horizontalScale_x: dfloat; horizontalScale_z: dfloat; shapeID: Integer): PNewtonCollision; cdecl; external NEWTON_API;
procedure NewtonHeightFieldSetUserRayCastCallback (const heightfieldCollision: PNewtonCollision; rayHitCallback: NewtonHeightFieldRayCastCallback); cdecl; external NEWTON_API;
procedure NewtonHeightFieldSetHorizontalDisplacement(heightfieldCollision : PNewtonCollision; horizontalMap : Pword; scale : dFloat); cdecl; external NEWTON_API;
function NewtonCreateTreeCollision (const newtonWorld: PNewtonWorld; shapeID: Integer): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonCreateTreeCollisionFromMesh (const newtonWorld: PNewtonWorld; const mesh: PNewtonMesh; shapeID: Integer): PNewtonCollision; cdecl; external NEWTON_API;
procedure NewtonTreeCollisionSetUserRayCastCallback (const treeCollision: PNewtonCollision; rayHitCallback: NewtonCollisionTreeRayCastCallback); cdecl; external NEWTON_API;
procedure NewtonTreeCollisionBeginBuild (const treeCollision: PNewtonCollision); cdecl; external NEWTON_API;
procedure NewtonTreeCollisionAddFace (const treeCollision: PNewtonCollision; vertexCount: Integer; const vertexPtr: pfloat; strideInBytes: Integer; faceAttribute: Integer); cdecl; external NEWTON_API;
procedure NewtonTreeCollisionEndBuild (const treeCollision: PNewtonCollision; optimize: Integer); cdecl; external NEWTON_API;
function NewtonTreeCollisionGetFaceAttribute (const treeCollision: PNewtonCollision; const faceIndexArray: Pinteger; indexCount: Integer): Integer; cdecl; external NEWTON_API;
procedure NewtonTreeCollisionSetFaceAttribute (const treeCollision: PNewtonCollision; const faceIndexArray: Pinteger; indexCount: Integer; attribute: Integer); cdecl; external NEWTON_API;
procedure NewtonTreeCollisionForEachFace (const treeCollision: PNewtonCollision; forEachFaceCallback: NewtonTreeCollisionFaceCallback; const context: Pointer); cdecl; external NEWTON_API;
function NewtonTreeCollisionGetVertexListTriangleListInAABB (const treeCollision: PNewtonCollision; const p0: pfloat; const p1: pfloat; const vertexArray: Pfloat; const vertexCount: Pinteger; const vertexStrideInBytes: Pinteger; const indexList: Pinteger; maxIndexCount: Integer; const faceAttribute: Pinteger): Integer; cdecl; external NEWTON_API;
procedure NewtonStaticCollisionSetDebugCallback (const staticCollision: PNewtonCollision; userCallback: NewtonTreeCollisionCallback); cdecl; external NEWTON_API;

// **********************************************************************************************
//
// General purpose collision library functions
//
// **********************************************************************************************
function NewtonCollisionCreateInstance (const collision: PNewtonCollision): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonCollisionGetType (const collision: PNewtonCollision): Integer; cdecl; external NEWTON_API;
function NewtonCollisionIsConvexShape (const collision: PNewtonCollision): Integer; cdecl; external NEWTON_API;
function NewtonCollisionIsStaticShape (const collision: PNewtonCollision): Integer; cdecl; external NEWTON_API;
procedure NewtonCollisionSetUserData (const collision: PNewtonCollision; const userData: Pointer); cdecl; external NEWTON_API;
function NewtonCollisionGetUserData (const collision: PNewtonCollision): Pointer; cdecl; external NEWTON_API;
procedure NewtonCollisionSetUserID (const collision: PNewtonCollision; id: int64); cdecl; external NEWTON_API;
function NewtonCollisionGetUserID (const collision: PNewtonCollision): int64; cdecl; external NEWTON_API;
procedure NewtonCollisionGetMaterial (const collision: PNewtonCollision; const userData: PNewtonCollisionMaterial); cdecl; external NEWTON_API;
procedure NewtonCollisionSetMaterial (const collision: PNewtonCollision; const userData: PNewtonCollisionMaterial); cdecl; external NEWTON_API;
function NewtonCollisionGetSubCollisionHandle (const collision: PNewtonCollision): Pointer; cdecl; external NEWTON_API;
function NewtonCollisionGetParentInstance (const collision: PNewtonCollision): PNewtonCollision; cdecl; external NEWTON_API;
procedure NewtonCollisionSetMatrix (const collision: PNewtonCollision; const matrix: pfloat); cdecl; external NEWTON_API;
procedure NewtonCollisionGetMatrix (const collision: PNewtonCollision; const matrix: pfloat); cdecl; external NEWTON_API;
procedure NewtonCollisionSetScale (const collision: PNewtonCollision; scaleX: dfloat; scaleY: dfloat; scaleZ: dfloat); cdecl; external NEWTON_API;
procedure NewtonCollisionGetScale (const collision: PNewtonCollision; const scaleX: pfloat; const scaleY: pfloat; const scaleZ: pfloat); cdecl; external NEWTON_API;
procedure NewtonDestroyCollision (const collision: PNewtonCollision); cdecl; external NEWTON_API;
function NewtonCollisionGetSkinThickness (const collision: PNewtonCollision): dfloat; cdecl; external NEWTON_API;
procedure NewtonCollisionSetSkinThickness (const collision: PNewtonCollision; thickness: dfloat); cdecl; external NEWTON_API;
function NewtonCollisionIntersectionTest (const newtonWorld: PNewtonWorld; const collisionA: PNewtonCollision; const matrixA: pfloat; const collisionB: PNewtonCollision; const matrixB: pfloat; threadIndex: Integer): Integer; cdecl; external NEWTON_API;
function NewtonCollisionPointDistance (const newtonWorld: PNewtonWorld; const point: pfloat; const collision: PNewtonCollision; const matrix: pfloat; const contact: pfloat; const normal: pfloat; threadIndex: Integer): Integer; cdecl; external NEWTON_API;
function NewtonCollisionClosestPoint (const newtonWorld: PNewtonWorld; const collisionA: PNewtonCollision; const matrixA: pfloat; const collisionB: PNewtonCollision; const matrixB: pfloat; const contactA: pfloat; const contactB: pfloat; const normalAB: pfloat; threadIndex: Integer): Integer; cdecl; external NEWTON_API;
function NewtonCollisionCollide (const newtonWorld: PNewtonWorld; maxSize: Integer; const collisionA: PNewtonCollision; const matrixA: pfloat; const collisionB: PNewtonCollision; const matrixB: pfloat; const contacts: pfloat; const normals: pfloat; const penetration: pfloat; const attributeA: Pint64; const attributeB: Pint64; threadIndex: Integer): Integer; cdecl; external NEWTON_API;
function NewtonCollisionCollideContinue (const newtonWorld: PNewtonWorld; maxSize: Integer; timestep: dfloat; const collisionA: PNewtonCollision; const matrixA: pfloat; const velocA: pfloat; const omegaA: pfloat; const collisionB: PNewtonCollision; const matrixB: pfloat; const velocB: pfloat; const omegaB: pfloat; const timeOfImpact: pfloat; const contacts: pfloat; const normals: pfloat; const penetration: pfloat; const attributeA: Pint64; const attributeB: Pint64; threadIndex: Integer): Integer; cdecl; external NEWTON_API;
procedure NewtonCollisionSupportVertex (const collision: PNewtonCollision; const dir: pfloat; const vertex: pfloat); cdecl; external NEWTON_API;
function NewtonCollisionRayCast (const collision: PNewtonCollision; const p0: pfloat; const p1: pfloat; const normal: pfloat; const attribute: Pint64): dfloat; cdecl; external NEWTON_API;
procedure NewtonCollisionCalculateAABB (const collision: PNewtonCollision; const matrix: pfloat; const p0: pfloat; const p1: pfloat); cdecl; external NEWTON_API;
procedure NewtonCollisionForEachPolygonDo (const collision: PNewtonCollision; const matrix: pfloat; callback: NewtonCollisionIterator; const userData: Pointer); cdecl; external NEWTON_API;
	
// **********************************************************************************************
// 
// collision aggregates, are a collision node on eh broad phase the serve as the root nod for a collection of rigid bodies
// that shared the property of being in close proximity all the time, they are similar to compound collision by the group bodies instead of collision instances
// These are good for speeding calculation calculation of rag doll, Vehicles or contractions of rigid bodied lined by joints.
// also for example if you know that many the life time of a group of bodies like the object on a house of a building will be localize to the confide of the building
// then warping the bodies under an aggregate will reduce collision calculation of almost an order of magnitude.
//
// **********************************************************************************************
function NewtonCollisionAggregateCreate (const world: PNewtonWorld): Pointer; cdecl; external NEWTON_API;
procedure NewtonCollisionAggregateDestroy (const aggregate: Pointer); cdecl; external NEWTON_API;
procedure NewtonCollisionAggregateAddBody (const aggregate: Pointer; const body: pNewtonBody); cdecl; external NEWTON_API;
procedure NewtonCollisionAggregateRemoveBody (const aggregate: Pointer; const body: pNewtonBody); cdecl; external NEWTON_API;
function NewtonCollisionAggregateGetSelfCollision (const aggregate: Pointer): Integer; cdecl; external NEWTON_API;
procedure NewtonCollisionAggregateSetSelfCollision (const aggregate: Pointer; state: Integer); cdecl; external NEWTON_API;
{ transforms utility functions }
procedure NewtonSetEulerAngle (const eulersAngles: pfloat; const matrix: pfloat); cdecl; external NEWTON_API;
procedure NewtonGetEulerAngle (const matrix: pfloat; const eulersAngles0: pfloat; const eulersAngles1: pfloat); cdecl; external NEWTON_API;
function NewtonCalculateSpringDamperAcceleration (dt: dfloat; ks: dfloat; x: dfloat; kd: dfloat; s: dfloat): dfloat; cdecl; external NEWTON_API;
{ body manipulation functions }
function NewtonCreateDynamicBody (const newtonWorld: PNewtonWorld; const collision: PNewtonCollision; const matrix: pfloat): pNewtonBody; cdecl; external NEWTON_API;
function NewtonCreateKinematicBody (const newtonWorld: PNewtonWorld; const collision: PNewtonCollision; const matrix: pfloat): pNewtonBody; cdecl; external NEWTON_API;
function NewtonCreateAsymetricDynamicBody (const newtonWorld: PNewtonWorld; const collision: PNewtonCollision; const matrix: pfloat): pNewtonBody; cdecl; external NEWTON_API;
procedure NewtonDestroyBody (const body: pNewtonBody); cdecl; external NEWTON_API;
function NewtonBodyGetSimulationState (const body: pNewtonBody): Integer; cdecl; external NEWTON_API;
procedure NewtonBodySetSimulationState (const bodyPtr: pNewtonBody; const state: Integer); cdecl; external NEWTON_API;
function NewtonBodyGetType (const body: pNewtonBody): Integer; cdecl; external NEWTON_API;
function NewtonBodyGetCollidable (const body: pNewtonBody): Integer; cdecl; external NEWTON_API;
procedure NewtonBodySetCollidable (const body: pNewtonBody; collidableState: Integer); cdecl; external NEWTON_API;
procedure NewtonBodyAddForce (const body: pNewtonBody; const force: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodyAddTorque (const body: pNewtonBody; const torque: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodyCalculateInverseDynamicsForce(body : PNewtonBody; timestep : dFloat; desiredVeloc : PdFloat; forceOut : PdFloat); cdecl; external NEWTON_API;
procedure NewtonBodySetCentreOfMass (const body: pNewtonBody; const com: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodySetMassMatrix (const body: pNewtonBody; mass: dfloat; Ixx: dfloat; Iyy: dfloat; Izz: dfloat); cdecl; external NEWTON_API;
procedure NewtonBodySetFullMassMatrix (const body: pNewtonBody; mass: dfloat; const inertiaMatrix: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodySetMassProperties (const body: pNewtonBody; mass: dfloat; const collision: PNewtonCollision); cdecl; external NEWTON_API;
procedure NewtonBodySetMatrix (const body: pNewtonBody; const matrix: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodySetMatrixNoSleep (const body: pNewtonBody; const matrix: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodySetMatrixRecursive (const body: pNewtonBody; const matrix: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodySetMaterialGroupID (const body: pNewtonBody; id: Integer); cdecl; external NEWTON_API;
procedure NewtonBodySetContinuousCollisionMode (const body: pNewtonBody; state: LongWord); cdecl; external NEWTON_API;
procedure NewtonBodySetJointRecursiveCollision (const body: pNewtonBody; state: LongWord); cdecl; external NEWTON_API;
procedure NewtonBodySetOmega (const body: pNewtonBody; const omega: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodySetOmegaNoSleep (const body: pNewtonBody; const omega: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodySetVelocity (const body: pNewtonBody; const velocity: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodySetVelocityNoSleep (const body: pNewtonBody; const velocity: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodySetForce (const body: pNewtonBody; const force: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodySetTorque (const body: pNewtonBody; const torque: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodySetLinearDamping (const body: pNewtonBody; linearDamp: dfloat); cdecl; external NEWTON_API;
procedure NewtonBodySetAngularDamping (const body: pNewtonBody; const angularDamp: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodySetCollision (const body: pNewtonBody; const collision: PNewtonCollision); cdecl; external NEWTON_API;
procedure NewtonBodySetCollisionScale (const body: pNewtonBody; scaleX: dfloat; scaleY: dfloat; scaleZ: dfloat); cdecl; external NEWTON_API;
function NewtonBodyGetSleepState (const body: pNewtonBody): Integer; cdecl; external NEWTON_API;
procedure NewtonBodySetSleepState (const body: pNewtonBody; state: Integer); cdecl; external NEWTON_API;
function NewtonBodyGetAutoSleep (const body: pNewtonBody): Integer; cdecl; external NEWTON_API;
procedure NewtonBodySetAutoSleep (const body: pNewtonBody; state: Integer); cdecl; external NEWTON_API;
function NewtonBodyGetFreezeState (const body: pNewtonBody): Integer; cdecl; external NEWTON_API;
procedure NewtonBodySetFreezeState (const body: pNewtonBody; state: Integer); cdecl; external NEWTON_API;
function NewtonBodyGetGyroscopicTorque (const body: pNewtonBody): Integer; cdecl; external NEWTON_API;
procedure NewtonBodySetGyroscopicTorque (const body: pNewtonBody; state: Integer); cdecl; external NEWTON_API;
procedure NewtonBodySetDestructorCallback (const body: pNewtonBody; callback: NewtonBodyDestructor); cdecl; external NEWTON_API;
function NewtonBodyGetDestructorCallback (const body: pNewtonBody): PNewtonBodyDestructor; cdecl; external NEWTON_API;
procedure NewtonBodySetTransformCallback (const body: pNewtonBody; callback: NewtonSetTransform); cdecl; external NEWTON_API;
function NewtonBodyGetTransformCallback (const body: pNewtonBody): PNewtonSetTransform; cdecl; external NEWTON_API;
procedure NewtonBodySetForceAndTorqueCallback (const body: pNewtonBody; callback: NewtonApplyForceAndTorque); cdecl; external NEWTON_API;
function NewtonBodyGetForceAndTorqueCallback (const body: pNewtonBody): PNewtonApplyForceAndTorque; cdecl; external NEWTON_API;
function NewtonBodyGetID (const body: pNewtonBody): Integer; cdecl; external NEWTON_API;
procedure NewtonBodySetUserData (const body: pNewtonBody; const userData: Pointer); cdecl; external NEWTON_API;
function NewtonBodyGetUserData (const body: pNewtonBody): Pointer; cdecl; external NEWTON_API;
function NewtonBodyGetWorld (const body: pNewtonBody): PNewtonWorld; cdecl; external NEWTON_API;
function NewtonBodyGetCollision (const body: pNewtonBody): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonBodyGetMaterialGroupID (const body: pNewtonBody): Integer; cdecl; external NEWTON_API;
function NewtonBodyGetSerializedID (const body: pNewtonBody): Integer; cdecl; external NEWTON_API;
function NewtonBodyGetContinuousCollisionMode (const body: pNewtonBody): Integer; cdecl; external NEWTON_API;
function NewtonBodyGetJointRecursiveCollision (const body: pNewtonBody): Integer; cdecl; external NEWTON_API;
procedure NewtonBodyGetPosition (const body: pNewtonBody; const pos: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodyGetMatrix (const body: pNewtonBody; const matrix: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodyGetRotation (const body: pNewtonBody; const rotation: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodyGetMass (const body: pNewtonBody; mass: pfloat; const Ixx: pfloat; const Iyy: pfloat; const Izz: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodyGetInvMass (const body: pNewtonBody; const invMass: pfloat; const invIxx: pfloat; const invIyy: pfloat; const invIzz: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodyGetInertiaMatrix (const body: pNewtonBody; const inertiaMatrix: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodyGetInvInertiaMatrix (const body: pNewtonBody; const invInertiaMatrix: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodyGetOmega (const body: pNewtonBody; const vector: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodyGetVelocity (const body: pNewtonBody; const vector: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodyGetAlpha (const body: pNewtonBody; const vector: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodyGetAcceleration (const body: pNewtonBody; const vector: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodyGetForce (const body: pNewtonBody; const vector: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodyGetTorque (const body: pNewtonBody; const vector: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodyGetCentreOfMass (const body: pNewtonBody; const com: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodyGetPointVelocity (const body: pNewtonBody; const point: pfloat; const velocOut: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodyApplyImpulsePair (const body: pNewtonBody; const linearImpulse: pfloat; const angularImpulse: pfloat; timestep: dfloat); cdecl; external NEWTON_API;
procedure NewtonBodyAddImpulse (const body: pNewtonBody; const pointDeltaVeloc: pfloat; const pointPosit: pfloat; timestep: dfloat); cdecl; external NEWTON_API;
procedure NewtonBodyApplyImpulseArray (const body: pNewtonBody; impuleCount: Integer; strideInByte: Integer; const impulseArray: pfloat; const pointArray: pfloat; timestep: dfloat); cdecl; external NEWTON_API;
procedure NewtonBodyIntegrateVelocity (const body: pNewtonBody; timestep: dfloat); cdecl; external NEWTON_API;
function NewtonBodyGetLinearDamping (const body: pNewtonBody): dfloat; cdecl; external NEWTON_API;
procedure NewtonBodyGetAngularDamping (const body: pNewtonBody; const vector: pfloat); cdecl; external NEWTON_API;
procedure NewtonBodyGetAABB (const body: pNewtonBody; const p0: pfloat; const p1: pfloat); cdecl; external NEWTON_API;
function NewtonBodyGetFirstJoint (const body: pNewtonBody): PNewtonJoint; cdecl; external NEWTON_API;
function NewtonBodyGetNextJoint (const body: pNewtonBody; const joint: PNewtonJoint): PNewtonJoint; cdecl; external NEWTON_API;
function NewtonBodyGetFirstContactJoint (const body: pNewtonBody): PNewtonJoint; cdecl; external NEWTON_API;
function NewtonBodyGetNextContactJoint (const body: pNewtonBody; const contactJoint: PNewtonJoint): PNewtonJoint; cdecl; external NEWTON_API;
function NewtonBodyFindContact (const body0: pNewtonBody; const body1: pNewtonBody): PNewtonJoint; cdecl; external NEWTON_API;
	
// **********************************************************************************************
//
// contact joints interface
//
// **********************************************************************************************
function NewtonContactJointGetFirstContact (const contactJoint: PNewtonJoint): Pointer; cdecl; external NEWTON_API;
function NewtonContactJointGetNextContact (const contactJoint: PNewtonJoint; const contact: Pointer): Pointer; cdecl; external NEWTON_API;
function NewtonContactJointGetContactCount (const contactJoint: PNewtonJoint): Integer; cdecl; external NEWTON_API;
procedure NewtonContactJointRemoveContact (const contactJoint: PNewtonJoint; const contact: Pointer); cdecl; external NEWTON_API;
function NewtonContactJointGetClosestDistance (const contactJoint: PNewtonJoint): dfloat; cdecl; external NEWTON_API;
procedure NewtonContactJointResetSelftJointCollision (const contactJoint: PNewtonJoint); cdecl; external NEWTON_API;
procedure NewtonContactJointResetIntraJointCollision (const contactJoint: PNewtonJoint); cdecl; external NEWTON_API;
function NewtonContactGetMaterial (const contact: Pointer): PNewtonMaterial; cdecl; external NEWTON_API;
function NewtonContactGetCollision0 (const contact: Pointer): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonContactGetCollision1 (const contact: Pointer): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonContactGetCollisionID0 (const contact: Pointer): Pointer; cdecl; external NEWTON_API;
function NewtonContactGetCollisionID1 (const contact: Pointer): Pointer; cdecl; external NEWTON_API;
	
	
// **********************************************************************************************
//
// Common joint functions
//
// **********************************************************************************************
function NewtonJointGetUserData (const joint: PNewtonJoint): Pointer; cdecl; external NEWTON_API;
procedure NewtonJointSetUserData (const joint: PNewtonJoint; const userData: Pointer); cdecl; external NEWTON_API;
function NewtonJointGetBody0 (const joint: PNewtonJoint): pNewtonBody; cdecl; external NEWTON_API;
function NewtonJointGetBody1 (const joint: PNewtonJoint): pNewtonBody; cdecl; external NEWTON_API;
procedure NewtonJointGetInfo (const joint: PNewtonJoint; const info: PNewtonJointRecord); cdecl; external NEWTON_API;
function NewtonJointGetCollisionState (const joint: PNewtonJoint): Integer; cdecl; external NEWTON_API;
procedure NewtonJointSetCollisionState (const joint: PNewtonJoint; state: Integer); cdecl; external NEWTON_API;
function NewtonJointGetStiffness (const joint: PNewtonJoint): dfloat; cdecl; external NEWTON_API;
procedure NewtonJointSetStiffness (const joint: PNewtonJoint; state: dfloat); cdecl; external NEWTON_API;
procedure NewtonDestroyJoint (const newtonWorld: PNewtonWorld; const joint: PNewtonJoint); cdecl; external NEWTON_API;
procedure NewtonJointSetDestructor (const joint: PNewtonJoint; destructorparam: NewtonConstraintDestructor); cdecl; external NEWTON_API;
function NewtonJointIsActive (const joint: PNewtonJoint): Integer; cdecl; external NEWTON_API;

// **********************************************************************************************
//
// particle system interface (soft bodies, individual, pressure bodies and cloth)   
//
// **********************************************************************************************
function NewtonCreateMassSpringDamperSystem (const newtonWorld: PNewtonWorld; shapeID: Integer; const points: pfloat; pointCount: Integer; strideInBytes: Integer; const pointMass: pfloat; const links: Pinteger; linksCount: Integer; const linksSpring: pfloat; const linksDamper: pfloat): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonCreateDeformableSolid (const newtonWorld: PNewtonWorld; const mesh: PNewtonMesh; shapeID: Integer): PNewtonCollision; cdecl; external NEWTON_API;
function NewtonDeformableMeshGetParticleCount (const deformableMesh: PNewtonCollision): Integer; cdecl; external NEWTON_API;
function NewtonDeformableMeshGetParticleStrideInBytes (const deformableMesh: PNewtonCollision): Integer; cdecl; external NEWTON_API;
function NewtonDeformableMeshGetParticleArray (const deformableMesh: PNewtonCollision): pfloat; cdecl; external NEWTON_API;

(*
	NEWTON_API NewtonCollision* NewtonCreateClothPatch (const NewtonWorld* const newtonWorld, NewtonMesh* const mesh, int shapeID, NewtonClothPatchMaterial* const structuralMaterial, NewtonClothPatchMaterial* const bendMaterial);
	NEWTON_API void NewtonDeformableMeshCreateClusters (NewtonCollision* const deformableMesh, int clusterCount, dFloat overlapingWidth);
	NEWTON_API void NewtonDeformableMeshSetDebugCallback (NewtonCollision* const deformableMesh, NewtonCollisionIterator callback);

	
	NEWTON_API void NewtonDeformableMeshGetParticlePosition (NewtonCollision* const deformableMesh, int particleIndex, dFloat* const posit);

	NEWTON_API void NewtonDeformableMeshBeginConfiguration (const NewtonCollision* const deformableMesh); 
	NEWTON_API void NewtonDeformableMeshUnconstraintParticle (NewtonCollision* const deformableMesh, int particleIndex);
	NEWTON_API void NewtonDeformableMeshConstraintParticle (NewtonCollision* const deformableMesh, int particleIndex, const dFloat* const posit, const NewtonBody* const body);
	NEWTON_API void NewtonDeformableMeshEndConfiguration (const NewtonCollision* const deformableMesh); 

//	NEWTON_API void NewtonDeformableMeshSetPlasticity (NewtonCollision* const deformableMesh, dFloat plasticity);
//	NEWTON_API void NewtonDeformableMeshSetStiffness (NewtonCollision* const deformableMesh, dFloat stiffness);
	NEWTON_API void NewtonDeformableMeshSetSkinThickness (NewtonCollision* const deformableMesh, dFloat skinThickness);

	NEWTON_API void NewtonDeformableMeshUpdateRenderNormals (const NewtonCollision* const deformableMesh); 
	NEWTON_API int NewtonDeformableMeshGetVertexCount (const NewtonCollision* const deformableMesh); 
	NEWTON_API void NewtonDeformableMeshGetVertexStreams (const NewtonCollision* const deformableMesh, int vertexStrideInByte, dFloat* const vertex, int normalStrideInByte, dFloat* const normal, int uvStrideInByte0, dFloat* const uv0);
	NEWTON_API NewtonDeformableMeshSegment* NewtonDeformableMeshGetFirstSegment (const NewtonCollision* const deformableMesh);
	NEWTON_API NewtonDeformableMeshSegment* NewtonDeformableMeshGetNextSegment (const NewtonCollision* const deformableMesh, const NewtonDeformableMeshSegment* const segment);

	NEWTON_API int NewtonDeformableMeshSegmentGetMaterialID (const NewtonCollision* const deformableMesh, const NewtonDeformableMeshSegment* const segment);
	NEWTON_API int NewtonDeformableMeshSegmentGetIndexCount (const NewtonCollision* const deformableMesh, const NewtonDeformableMeshSegment* const segment);
	NEWTON_API const int* NewtonDeformableMeshSegmentGetIndexList (const NewtonCollision* const deformableMesh, const NewtonDeformableMeshSegment* const segment);
*)
// **********************************************************************************************
//
// Ball and Socket joint functions
//
// **********************************************************************************************
function NewtonConstraintCreateBall (const newtonWorld: PNewtonWorld; const pivotPoint: pfloat; const childBody: pNewtonBody; const parentBody: pNewtonBody): PNewtonJoint; cdecl; external NEWTON_API;
procedure NewtonBallSetUserCallback (const ball: PNewtonJoint; callback: NewtonBallCallback); cdecl; external NEWTON_API;
procedure NewtonBallGetJointAngle (const ball: PNewtonJoint; angle: pfloat); cdecl; external NEWTON_API;
procedure NewtonBallGetJointOmega (const ball: PNewtonJoint; omega: pfloat); cdecl; external NEWTON_API;
procedure NewtonBallGetJointForce (const ball: PNewtonJoint; const force: pfloat); cdecl; external NEWTON_API;
procedure NewtonBallSetConeLimits (const ball: PNewtonJoint; const pin: pfloat; maxConeAngle: dfloat; maxTwistAngle: dfloat); cdecl; external NEWTON_API;

// **********************************************************************************************
//
// Hinge joint functions
//
// **********************************************************************************************
function NewtonConstraintCreateHinge (const newtonWorld: PNewtonWorld; const pivotPoint: pfloat; const pinDir: pfloat; const childBody: pNewtonBody; const parentBody: pNewtonBody): PNewtonJoint; cdecl; external NEWTON_API;
procedure NewtonHingeSetUserCallback (const hinge: PNewtonJoint; callback: NewtonHingeCallback); cdecl; external NEWTON_API;
function NewtonHingeGetJointAngle (const hinge: PNewtonJoint): dfloat; cdecl; external NEWTON_API;
function NewtonHingeGetJointOmega (const hinge: PNewtonJoint): dfloat; cdecl; external NEWTON_API;
procedure NewtonHingeGetJointForce (const hinge: PNewtonJoint; const force: pfloat); cdecl; external NEWTON_API;
function NewtonHingeCalculateStopAlpha (const hinge: PNewtonJoint; const desc: PNewtonHingeSliderUpdateDesc; angle: dfloat): dfloat; cdecl; external NEWTON_API;

// **********************************************************************************************
//
// Slider joint functions
//
// **********************************************************************************************
function NewtonConstraintCreateSlider (const newtonWorld: PNewtonWorld; const pivotPoint: pfloat; const pinDir: pfloat; const childBody: pNewtonBody; const parentBody: pNewtonBody): PNewtonJoint; cdecl; external NEWTON_API;
procedure NewtonSliderSetUserCallback (const slider: PNewtonJoint; callback: NewtonSliderCallback); cdecl; external NEWTON_API;
function NewtonSliderGetJointPosit (const slider: PNewtonJoint): dfloat; cdecl; external NEWTON_API;
function NewtonSliderGetJointVeloc (const slider: PNewtonJoint): dfloat; cdecl; external NEWTON_API;
procedure NewtonSliderGetJointForce (const slider: PNewtonJoint; const force: pfloat); cdecl; external NEWTON_API;
function NewtonSliderCalculateStopAccel (const slider: PNewtonJoint; const desc: PNewtonHingeSliderUpdateDesc; position: dfloat): dfloat; cdecl; external NEWTON_API;


// **********************************************************************************************
//
// Corkscrew joint functions
//
// **********************************************************************************************
function NewtonConstraintCreateCorkscrew (const newtonWorld: PNewtonWorld; const pivotPoint: pfloat; const pinDir: pfloat; const childBody: pNewtonBody; const parentBody: pNewtonBody): PNewtonJoint; cdecl; external NEWTON_API;
procedure NewtonCorkscrewSetUserCallback (const corkscrew: PNewtonJoint; callback: NewtonCorkscrewCallback); cdecl; external NEWTON_API;
function NewtonCorkscrewGetJointPosit (const corkscrew: PNewtonJoint): dfloat; cdecl; external NEWTON_API;
function NewtonCorkscrewGetJointAngle (const corkscrew: PNewtonJoint): dfloat; cdecl; external NEWTON_API;
function NewtonCorkscrewGetJointVeloc (const corkscrew: PNewtonJoint): dfloat; cdecl; external NEWTON_API;
function NewtonCorkscrewGetJointOmega (const corkscrew: PNewtonJoint): dfloat; cdecl; external NEWTON_API;
procedure NewtonCorkscrewGetJointForce (const corkscrew: PNewtonJoint; const force: pfloat); cdecl; external NEWTON_API;
function NewtonCorkscrewCalculateStopAlpha (const corkscrew: PNewtonJoint; const desc: PNewtonHingeSliderUpdateDesc; angle: dfloat): dfloat; cdecl; external NEWTON_API;
function NewtonCorkscrewCalculateStopAccel (const corkscrew: PNewtonJoint; const desc: PNewtonHingeSliderUpdateDesc; position: dfloat): dfloat; cdecl; external NEWTON_API;


// **********************************************************************************************
//
// Universal joint functions
//
// **********************************************************************************************
function NewtonConstraintCreateUniversal (const newtonWorld: PNewtonWorld; const pivotPoint: pfloat; const pinDir0: pfloat; const pinDir1: pfloat; const childBody: pNewtonBody; const parentBody: pNewtonBody): PNewtonJoint; cdecl; external NEWTON_API;
procedure NewtonUniversalSetUserCallback (const universal: PNewtonJoint; callback: NewtonUniversalCallback); cdecl; external NEWTON_API;
function NewtonUniversalGetJointAngle0 (const universal: PNewtonJoint): dfloat; cdecl; external NEWTON_API;
function NewtonUniversalGetJointAngle1 (const universal: PNewtonJoint): dfloat; cdecl; external NEWTON_API;
function NewtonUniversalGetJointOmega0 (const universal: PNewtonJoint): dfloat; cdecl; external NEWTON_API;
function NewtonUniversalGetJointOmega1 (const universal: PNewtonJoint): dfloat; cdecl; external NEWTON_API;
procedure NewtonUniversalGetJointForce (const universal: PNewtonJoint; const force: pfloat); cdecl; external NEWTON_API;
function NewtonUniversalCalculateStopAlpha0 (const universal: PNewtonJoint; const desc: PNewtonHingeSliderUpdateDesc; angle: dfloat): dfloat; cdecl; external NEWTON_API;
function NewtonUniversalCalculateStopAlpha1 (const universal: PNewtonJoint; const desc: PNewtonHingeSliderUpdateDesc; angle: dfloat): dfloat; cdecl; external NEWTON_API;


// **********************************************************************************************
//
// Up vector joint functions
//
// **********************************************************************************************
function NewtonConstraintCreateUpVector (const newtonWorld: PNewtonWorld; const pinDir: pfloat; const body: pNewtonBody): PNewtonJoint; cdecl; external NEWTON_API;
procedure NewtonUpVectorGetPin (const upVector: PNewtonJoint; pin: dfloat); cdecl; external NEWTON_API;
procedure NewtonUpVectorSetPin (const upVector: PNewtonJoint; const pin: dfloat); cdecl; external NEWTON_API;


// **********************************************************************************************
//
// User defined bilateral Joint
//
// **********************************************************************************************
function NewtonConstraintCreateUserJoint (const newtonWorld: PNewtonWorld; maxDOF: Integer; callback: NewtonUserBilateralCallback; const childBody: pNewtonBody; const parentBody: pNewtonBody): PNewtonJoint; cdecl; external NEWTON_API;
function NewtonUserJointGetSolverModel (const joint: PNewtonJoint): Integer; cdecl; external NEWTON_API;
procedure NewtonUserJointSetSolverModel (const joint: PNewtonJoint; model: Integer); cdecl; external NEWTON_API;
{}procedure NewtonUserJointMassScale (const joint: PNewtonJoint; scaleBody0: dfloat; scaleBody1: dfloat); cdecl; external NEWTON_API;
procedure NewtonUserJointSetFeedbackCollectorCallback (const joint: PNewtonJoint; getFeedback: NewtonUserBilateralCallback); cdecl; external NEWTON_API;
procedure NewtonUserJointAddLinearRow (const joint: PNewtonJoint; const pivot0: pfloat; const pivot1: pfloat; const dir: pfloat); cdecl; external NEWTON_API;
procedure NewtonUserJointAddAngularRow (const joint: PNewtonJoint; relativeAngle: dfloat; const dir: pfloat); cdecl; external NEWTON_API;
procedure NewtonUserJointAddGeneralRow (const joint: PNewtonJoint; const jacobian0: pfloat; const jacobian1: pfloat); cdecl; external NEWTON_API;
procedure NewtonUserJointSetRowMinimumFriction (const joint: PNewtonJoint; friction: dfloat); cdecl; external NEWTON_API;
procedure NewtonUserJointSetRowMaximumFriction (const joint: PNewtonJoint; friction: dfloat); cdecl; external NEWTON_API;
function NewtonUserJointCalculateRowZeroAcceleration (const joint: PNewtonJoint): dfloat; cdecl; external NEWTON_API;
function NewtonUserJointGetRowAcceleration (const joint: PNewtonJoint): dfloat; cdecl; external NEWTON_API;
{}procedure NewtonUserJointGetRowJacobian (const joint: PNewtonJoint; const linear0: pfloat; const angula0: pfloat; const linear1: pfloat; const angula1: pfloat); cdecl; external NEWTON_API;
procedure NewtonUserJointSetRowAcceleration (const joint: PNewtonJoint; acceleration: dfloat); cdecl; external NEWTON_API;
procedure NewtonUserJointSetRowSpringDamperAcceleration (const joint: PNewtonJoint; rowStiffness: dfloat; spring: dfloat; damper: dfloat); cdecl; external NEWTON_API;
procedure NewtonUserJointSetRowStiffness (const joint: PNewtonJoint; stiffness: dfloat); cdecl; external NEWTON_API;
function NewtonUserJoinRowsCount (const joint: PNewtonJoint): Integer; cdecl; external NEWTON_API;
procedure NewtonUserJointGetGeneralRow (const joint: PNewtonJoint; index: Integer; const jacobian0: pfloat; const jacobian1: pfloat); cdecl; external NEWTON_API;
function NewtonUserJointGetRowForce (const joint: PNewtonJoint; row: Integer): dfloat; cdecl; external NEWTON_API;

// **********************************************************************************************
//
// Mesh joint functions
//
// **********************************************************************************************
function NewtonMeshCreate (const newtonWorld: PNewtonWorld): PNewtonMesh; cdecl; external NEWTON_API;
function NewtonMeshCreateFromMesh (const mesh: PNewtonMesh): PNewtonMesh; cdecl; external NEWTON_API;
function NewtonMeshCreateFromCollision (const collision: PNewtonCollision): PNewtonMesh; cdecl; external NEWTON_API;
function NewtonMeshCreateTetrahedraIsoSurface (const mesh: PNewtonMesh): PNewtonMesh; cdecl; external NEWTON_API;
function NewtonMeshCreateConvexHull (const newtonWorld: PNewtonWorld; pointCount: Integer; const vertexCloud: pfloat; strideInBytes: Integer; tolerance: dfloat): PNewtonMesh; cdecl; external NEWTON_API;
function NewtonMeshCreateVoronoiConvexDecomposition (const newtonWorld: PNewtonWorld; pointCount: Integer; const vertexCloud: pfloat; strideInBytes: Integer; materialID: Integer; const textureMatrix: pfloat): PNewtonMesh; cdecl; external NEWTON_API;
function NewtonMeshCreateFromSerialization (const newtonWorld: PNewtonWorld; deserializeFunction: NewtonDeserializeCallback; const serializeHandle: Pointer): PNewtonMesh; cdecl; external NEWTON_API;
procedure NewtonMeshDestroy (const mesh: PNewtonMesh); cdecl; external NEWTON_API;
procedure NewtonMeshSerialize (const mesh: PNewtonMesh; serializeFunction: NewtonSerializeCallback; const serializeHandle: Pointer); cdecl; external NEWTON_API;
procedure NewtonMeshSaveOFF (const mesh: PNewtonMesh; const filename: pchar); cdecl; external NEWTON_API;
function NewtonMeshLoadOFF (const newtonWorld: PNewtonWorld; const filename: pchar): PNewtonMesh; cdecl; external NEWTON_API;
function NewtonMeshLoadTetrahedraMesh (const newtonWorld: PNewtonWorld; const filename: pchar): PNewtonMesh; cdecl; external NEWTON_API;
{}procedure NewtonMeshFlipWinding (const mesh: PNewtonMesh); cdecl; external NEWTON_API;

procedure NewtonMeshApplyTransform (const mesh: PNewtonMesh; const matrix: pfloat); cdecl; external NEWTON_API;
procedure NewtonMeshCalculateOOBB (const mesh: PNewtonMesh; const matrix: pfloat; const x: pfloat; const y: pfloat; const z: pfloat); cdecl; external NEWTON_API;
procedure NewtonMeshCalculateVertexNormals (const mesh: PNewtonMesh; angleInRadians: dfloat); cdecl; external NEWTON_API;
procedure NewtonMeshApplySphericalMapping (const mesh: PNewtonMesh; material: Integer; const aligmentMatrix: pfloat); cdecl; external NEWTON_API;
procedure NewtonMeshApplyCylindricalMapping (const mesh: PNewtonMesh; cylinderMaterial: Integer; capMaterial: Integer; const aligmentMatrix: pfloat); cdecl; external NEWTON_API;
procedure NewtonMeshApplyBoxMapping (const mesh: PNewtonMesh; frontMaterial: Integer; sideMaterial: Integer; topMaterial: Integer; const aligmentMatrix: pfloat); cdecl; external NEWTON_API;
procedure NewtonMeshApplyAngleBasedMapping (const mesh: PNewtonMesh; material: Integer; reportPrograssCallback: NewtonReportProgress; const reportPrgressUserData: Pointer; const aligmentMatrix: pfloat); cdecl; external NEWTON_API;
procedure NewtonCreateTetrahedraLinearBlendSkinWeightsChannel (const tetrahedraMesh: PNewtonMesh; const skinMesh: PNewtonMesh); cdecl; external NEWTON_API;
procedure NewtonMeshOptimize (const mesh: PNewtonMesh); cdecl; external NEWTON_API;
procedure NewtonMeshOptimizePoints (const mesh: PNewtonMesh); cdecl; external NEWTON_API;
procedure NewtonMeshOptimizeVertex (const mesh: PNewtonMesh); cdecl; external NEWTON_API;
function NewtonMeshIsOpenMesh (const mesh: PNewtonMesh): Integer; cdecl; external NEWTON_API;
procedure NewtonMeshFixTJoints (const mesh: PNewtonMesh); cdecl; external NEWTON_API;
procedure NewtonMeshPolygonize (const mesh: PNewtonMesh); cdecl; external NEWTON_API;
procedure NewtonMeshTriangulate (const mesh: PNewtonMesh); cdecl; external NEWTON_API;
function NewtonMeshUnion (const mesh: PNewtonMesh; const clipper: PNewtonMesh; const clipperMatrix: pfloat): PNewtonMesh; cdecl; external NEWTON_API;
function NewtonMeshDifference (const mesh: PNewtonMesh; const clipper: PNewtonMesh; const clipperMatrix: pfloat): PNewtonMesh; cdecl; external NEWTON_API;
function NewtonMeshIntersection (const mesh: PNewtonMesh; const clipper: PNewtonMesh; const clipperMatrix: pfloat): PNewtonMesh; cdecl; external NEWTON_API;
procedure NewtonMeshClip (const mesh: PNewtonMesh; const clipper: PNewtonMesh; const clipperMatrix: pfloat; const topMesh: PNewtonMesh; const bottomMesh: PNewtonMesh); cdecl; external NEWTON_API;
function NewtonMeshConvexMeshIntersection (const mesh: PNewtonMesh; const convexMesh: PNewtonMesh): PNewtonMesh; cdecl; external NEWTON_API;
function NewtonMeshSimplify (const mesh: PNewtonMesh; maxVertexCount: Integer; reportPrograssCallback: NewtonReportProgress; const reportPrgressUserData: Pointer): PNewtonMesh; cdecl; external NEWTON_API;
function NewtonMeshApproximateConvexDecomposition (const mesh: PNewtonMesh; maxConcavity: dfloat; backFaceDistanceFactor: dfloat; maxCount: Integer; maxVertexPerHull: Integer; reportProgressCallback: NewtonReportProgress; const reportProgressUserData: Pointer): PNewtonMesh; cdecl; external NEWTON_API;
procedure NewtonRemoveUnusedVertices (const mesh: PNewtonMesh; const vertexRemapTable: Pinteger); cdecl; external NEWTON_API;
procedure NewtonMeshBeginBuild (const mesh: PNewtonMesh); cdecl; external NEWTON_API;
procedure NewtonMeshBeginFace (const mesh: PNewtonMesh); cdecl; external NEWTON_API;
procedure NewtonMeshAddPoint (const mesh: PNewtonMesh; x: double; y: double; z: double); cdecl; external NEWTON_API;
procedure NewtonMeshAddLayer (const mesh: PNewtonMesh; layerIndex: Integer); cdecl; external NEWTON_API;
procedure NewtonMeshAddMaterial (const mesh: PNewtonMesh; materialIndex: Integer); cdecl; external NEWTON_API;
procedure NewtonMeshAddNormal (const mesh: PNewtonMesh; x: dfloat; y: dfloat; z: dfloat); cdecl; external NEWTON_API;
procedure NewtonMeshAddBinormal (const mesh: PNewtonMesh; x: dfloat; y: dfloat; z: dfloat); cdecl; external NEWTON_API;
procedure NewtonMeshAddUV0 (const mesh: PNewtonMesh; u: dfloat; v: dfloat); cdecl; external NEWTON_API;
procedure NewtonMeshAddUV1 (const mesh: PNewtonMesh; u: dfloat; v: dfloat); cdecl; external NEWTON_API;
{}procedure NewtonMeshAddVertexColor (const mesh: PNewtonMesh; r: Single; g: Single; b: Single; a: Single); cdecl; external NEWTON_API;
procedure NewtonMeshEndFace (const mesh: PNewtonMesh); cdecl; external NEWTON_API;
procedure NewtonMeshEndBuild (const mesh: PNewtonMesh); cdecl; external NEWTON_API;
procedure NewtonMeshClearVertexFormat (const format: PNewtonMeshVertexFormat); cdecl; external NEWTON_API;
procedure NewtonMeshBuildFromVertexListIndexList (const mesh: PNewtonMesh; const format: PNewtonMeshVertexFormat); cdecl; external NEWTON_API;
function NewtonMeshGetPointCount (const mesh: PNewtonMesh): Integer; cdecl; external NEWTON_API;
function NewtonMeshGetIndexToVertexMap (const mesh: PNewtonMesh): Pinteger; cdecl; external NEWTON_API;
function NewtonMeshGetVertexWeights(mesh : PNewtonMesh; vertexIndex : Integer; weightIndex : PInteger; weightFactor : PdFloat) : Integer; cdecl; external NEWTON_API;
procedure NewtonMeshGetVertexDoubleChannel (const mesh: PNewtonMesh; vertexStrideInByte: Integer; const outBuffer: Pdouble); cdecl; external NEWTON_API;
procedure NewtonMeshGetVertexChannel (const mesh: PNewtonMesh; vertexStrideInByte: Integer; const outBuffer: pfloat); cdecl; external NEWTON_API;
procedure NewtonMeshGetNormalChannel (const mesh: PNewtonMesh; vertexStrideInByte: Integer; const outBuffer: pfloat); cdecl; external NEWTON_API;
procedure NewtonMeshGetBinormalChannel (const mesh: PNewtonMesh; vertexStrideInByte: Integer; const outBuffer: pfloat); cdecl; external NEWTON_API;
procedure NewtonMeshGetUV0Channel (const mesh: PNewtonMesh; vertexStrideInByte: Integer; const outBuffer: pfloat); cdecl; external NEWTON_API;
procedure NewtonMeshGetUV1Channel (const mesh: PNewtonMesh; vertexStrideInByte: Integer; const outBuffer: pfloat); cdecl; external NEWTON_API;
procedure NewtonMeshGetVertexColorChannel (const mesh: PNewtonMesh; vertexStrideInByte: Integer; const outBuffer: pfloat); cdecl; external NEWTON_API;
function NewtonMeshHasNormalChannel (const mesh: PNewtonMesh): Integer; cdecl; external NEWTON_API;
function NewtonMeshHasBinormalChannel (const mesh: PNewtonMesh): Integer; cdecl; external NEWTON_API;
function NewtonMeshHasUV0Channel (const mesh: PNewtonMesh): Integer; cdecl; external NEWTON_API;
function NewtonMeshHasUV1Channel (const mesh: PNewtonMesh): Integer; cdecl; external NEWTON_API;
function NewtonMeshHasVertexColorChannel (const mesh: PNewtonMesh): Integer; cdecl; external NEWTON_API;
function NewtonMeshBeginHandle (const mesh: PNewtonMesh): Pointer; cdecl; external NEWTON_API;
procedure NewtonMeshEndHandle (const mesh: PNewtonMesh; const handle: Pointer); cdecl; external NEWTON_API;
function NewtonMeshFirstMaterial (const mesh: PNewtonMesh; const handle: Pointer): Integer; cdecl; external NEWTON_API;
function NewtonMeshNextMaterial (const mesh: PNewtonMesh; const handle: Pointer; materialId: Integer): Integer; cdecl; external NEWTON_API;
function NewtonMeshMaterialGetMaterial (const mesh: PNewtonMesh; const handle: Pointer; materialId: Integer): Integer; cdecl; external NEWTON_API;
function NewtonMeshMaterialGetIndexCount (const mesh: PNewtonMesh; const handle: Pointer; materialId: Integer): Integer; cdecl; external NEWTON_API;
procedure NewtonMeshMaterialGetIndexStream (const mesh: PNewtonMesh; const handle: Pointer; materialId: Integer; const index: Pinteger); cdecl; external NEWTON_API;
procedure NewtonMeshMaterialGetIndexStreamShort (const mesh: PNewtonMesh; const handle: Pointer; materialId: Integer; const index: SmallInt); cdecl; external NEWTON_API;
function NewtonMeshCreateFirstSingleSegment (const mesh: PNewtonMesh): PNewtonMesh; cdecl; external NEWTON_API;
function NewtonMeshCreateNextSingleSegment (const mesh: PNewtonMesh; const segment: PNewtonMesh): PNewtonMesh; cdecl; external NEWTON_API;
function NewtonMeshCreateFirstLayer (const mesh: PNewtonMesh): PNewtonMesh; cdecl; external NEWTON_API;
function NewtonMeshCreateNextLayer (const mesh: PNewtonMesh; const segment: PNewtonMesh): PNewtonMesh; cdecl; external NEWTON_API;
function NewtonMeshGetTotalFaceCount (const mesh: PNewtonMesh): Integer; cdecl; external NEWTON_API;
function NewtonMeshGetTotalIndexCount (const mesh: PNewtonMesh): Integer; cdecl; external NEWTON_API;
procedure NewtonMeshGetFaces (const mesh: PNewtonMesh; const faceIndexCount: Pinteger; const faceMaterial: Pinteger; const faceIndices: PPointer); cdecl; external NEWTON_API;
function NewtonMeshGetVertexCount (const mesh: PNewtonMesh): Integer; cdecl; external NEWTON_API;
function NewtonMeshGetVertexStrideInByte (const mesh: PNewtonMesh): Integer; cdecl; external NEWTON_API;
function NewtonMeshGetVertexArray (const mesh: PNewtonMesh): Pdouble; cdecl; external NEWTON_API;
function NewtonMeshGetVertexBaseCount (const mesh: PNewtonMesh): Integer; cdecl; external NEWTON_API;
procedure NewtonMeshSetVertexBaseCount (const mesh: PNewtonMesh; baseCount: Integer); cdecl; external NEWTON_API;
function NewtonMeshGetFirstVertex (const mesh: PNewtonMesh): Pointer; cdecl; external NEWTON_API;
function NewtonMeshGetNextVertex (const mesh: PNewtonMesh; const vertex: Pointer): Pointer; cdecl; external NEWTON_API;
function NewtonMeshGetVertexIndex (const mesh: PNewtonMesh; const vertex: Pointer): Integer; cdecl; external NEWTON_API;
function NewtonMeshGetFirstPoint (const mesh: PNewtonMesh): Pointer; cdecl; external NEWTON_API;
function NewtonMeshGetNextPoint (const mesh: PNewtonMesh; const point: Pointer): Pointer; cdecl; external NEWTON_API;
function NewtonMeshGetPointIndex (const mesh: PNewtonMesh; const point: Pointer): Integer; cdecl; external NEWTON_API;
function NewtonMeshGetVertexIndexFromPoint (const mesh: PNewtonMesh; const point: Pointer): Integer; cdecl; external NEWTON_API;
function NewtonMeshGetFirstEdge (const mesh: PNewtonMesh): Pointer; cdecl; external NEWTON_API;
function NewtonMeshGetNextEdge (const mesh: PNewtonMesh; const edge: Pointer): Pointer; cdecl; external NEWTON_API;
procedure NewtonMeshGetEdgeIndices (const mesh: PNewtonMesh; const edge: Pointer; const v0: Pinteger; const v1: Pinteger); cdecl; external NEWTON_API;
	//NEWTON_API void NewtonMeshGetEdgePointIndices (const NewtonMesh* const mesh, const void* const edge, int* const v0, int* const v1);

function NewtonMeshGetFirstFace (const mesh: PNewtonMesh): Pointer; cdecl; external NEWTON_API;
function NewtonMeshGetNextFace (const mesh: PNewtonMesh; const face: Pointer): Pointer; cdecl; external NEWTON_API;
function NewtonMeshIsFaceOpen (const mesh: PNewtonMesh; const face: Pointer): Integer; cdecl; external NEWTON_API;
function NewtonMeshGetFaceMaterial (const mesh: PNewtonMesh; const face: Pointer): Integer; cdecl; external NEWTON_API;
function NewtonMeshGetFaceIndexCount (const mesh: PNewtonMesh; const face: Pointer): Integer; cdecl; external NEWTON_API;
procedure NewtonMeshGetFaceIndices (const mesh: PNewtonMesh; const face: Pointer; const indices: Pinteger); cdecl; external NEWTON_API;
procedure NewtonMeshGetFacePointIndices (const mesh: PNewtonMesh; const face: Pointer; const indices: Pinteger); cdecl; external NEWTON_API;
procedure NewtonMeshCalculateFaceNormal (const mesh: PNewtonMesh; const face: Pointer; const normal: Pdouble); cdecl; external NEWTON_API;
procedure NewtonMeshSetFaceMaterial (const mesh: PNewtonMesh; const face: Pointer; matId: Integer); cdecl; external NEWTON_API;


implementation

end.