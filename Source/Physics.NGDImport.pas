//
// The graphics rendering engine GLScene http://glscene.org
//
unit Physics.NGDImport;

(* Copyright (c) <2003-2014> <Julio Jerez, Newton Game Dynamics>
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

{.$DEFINE NEWTON_DOUBLE_PRECISION} 

interface

uses
  System.Classes;

const
{.$DEFINE NEWTON_DOUBLE_PRECISION} // This is needed when you want to use double precision
{$IFDEF WIN32}
   NewtonDLL = 'newton32.dll';
{$ENDIF}

{$IFDEF WIN64}
   NewtonDLL = 'newton64.dll';
{$ENDIF}

type
{$IFDEF NEWTON_DOUBLE_PRECISION}
   dFloat               = Double;
{$ELSE}
   dFloat               = Single;
{$ENDIF}
   dFloat64             = Double;
   dLong                = Int64;

   PdFloat              = ^dFloat;
   PdFloat64            = ^dFloat64;
   PdLong               = ^dLong;
   Long_double          = Extended;

const
 NEWTON_MAJOR_VERSION                            =  3;
 NEWTON_MINOR_VERSION                            = 15;
 NEWTON_BROADPHASE_DEFAULT                       =  0;
 NEWTON_PROFILER_WORLD_UPDATE                    =  0;
 NEWTON_DYNAMIC_BODY                             =  0;
 NEWTON_PROFILER_COLLISION_UPDATE                =  1;
 NEWTON_PROFILER_COLLISION_UPDATE_BROAD_PHASE    =  2;
 NEWTON_PROFILER_COLLISION_UPDATE_NARROW_PHASE   =  3;
 NEWTON_PROFILER_DYNAMICS_UPDATE                 =  4;
 NEWTON_PROFILER_DYNAMICS_CONSTRAINT_GRAPH       =  5;
 NEWTON_PROFILER_FORCE_CALLBACK_UPDATE           =  6;
 NEWTON_PROFILER_DYNAMICS_SOLVE_CONSTRAINT_GRAPH =  7;

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
// SERIALIZE_ID_COMPOUND_BREAKABLE                 = 14;
type
// This C++ types for Delphi syntax to speed up the translation process and avoid bugs
  __int8 = ShortInt;
  __int16 = SmallInt;
  __int32 = LongInt;
  __int64 = Int64;
  nchar = ShortInt;
  unsigned_char = Byte;
  short = SmallInt;
  unsigned_short = Word;
  long = LongInt;
  unsigned_long = LongWord;
  unsigned_int = Cardinal;
  size_t = Cardinal;
  charArray = array [0..255] of Char;

  P__int8 = ^__int8;
  P__int16 = ^__int16;
  P__int32 = ^__int32;
  P__int64 = ^__int64;
  P2Char = ^nchar;
  PUnsigned_char = ^unsigned_char;
  PShort = ^short;
  PUnsigned_short = ^unsigned_short;
  PLong = ^long;
  PUnsigned_long = ^unsigned_long;
  PUnsigned_int = ^unsigned_int;
  Psize_t = ^size_t;
  PLong_double = ^long_double;
  PCharArray = ^charArray;

  //Pascal to C++
  Bool = Boolean;

 //Pointer types
  Pvoid = Pointer; //void pointer
  PBool = ^Bool;

  //Moved Maths related C++ Definitions to Maths3D.pas

  (* Next done in order to make code complete and code parameters hint window
  to show the actual type for ex. PNewtonWorld instead of just "Pointer",
  thus making programming a lot easier *)

(*
#ifdef __cplusplus
	class NewtonMesh;
	class NewtonBody;
	class NewtonWorld;
	class NewtonJoint;
	class NewtonMaterial;
	class NewtonCollision;
	class NewtonDeformableMeshSegment;
	class NewtonFracturedCompoundMeshPart;
#else
*)

  PNewtonMesh = Pointer;
  PNewtonBody = Pointer;
  PNewtonWorld = Pointer;
  PNewtonJoint = Pointer;
  PNewtonMaterial = Pointer;
  PNewtonCollision = Pointer;
  PNewtonDeformableMeshSegment  = Pointer;
  PNewtonFracturedCompoundMeshPart = Pointer;
//  PNewtonContact = Pointer;
  PNewtonSerializeHandle = Pointer;
  PNewtonMeshHandle = Pointer;
  PNewtonMeshVertex = Pointer;
  PNewtonMeshPoint = Pointer;
  PNewtonMeshEdge = Pointer;
  PNewtonMeshFace = Pointer;

  PNewtonSceneProxy = Pointer;
  PNewtonBreakableComponentMesh = Pointer;

  PNewtonListener = Pointer;

  //PNewtonRagDoll = Pointer;
  //PNewtonRagDollBone = Pointer;
  // NewtonCollisionInfoRecord


 TNewtonMaterialData = packed record // union

	m_ptr   : Pointer;
	m_int   : dLong;
	m_float : dFloat;
 end;

TNewtonCollisionMaterial = packed record
  m_userId: dLong;
  m_userData: TNewtonMaterialData;
  m_userParam: array[0..5] of TNewtonMaterialData;
 end;


 TNewtonBoxParam = packed record
  m_x : dFloat;
  m_y : dFloat;
  m_z : dFloat;
 end;

 TNewtonSphereParam = packed record
  m_radio                : dFloat;
 end;

 TNewtonCylinderParam = packed record
  m_radio, m_height               : dFloat;
 end;

 TNewtonCapsuleParam = packed record
  m_radio, m_height               : dFloat;
 end;

 TNewtonConeParam = packed record
  m_radio,
  m_height               : dFloat;
 end;

 TNewtonTaperedCapsuleParam = packed record
  m_radio0,
  m_radio1,
  m_height               : dFloat;
 end;


 TNewtonTaperedCylinderParam = packed record
  m_radio0,
  m_radio1,
  m_height               : dFloat;
 end;

 TNewtonChamferCylinderParam = packed record
  m_radio  : dFloat;
  m_height : dFloat;
 end;

 TNewtonConvexHullParam = packed record
  m_vertexCount           : integer;
  m_vertexStrideInBytes   : integer;
  m_faceCount             : integer;
  m_vertex                : PdFloat;
 end;


  TNewtonCompoundCollisionParam = packed record
    m_chidrenCount: integer;

  end;

  TNewtonCollisionTreeParam = packed record
    m_vertexCount: integer;
    m_indexCount: integer;
  end;

 TNewtonDeformableMeshParam = packed record
  m_vertexCount           : integer;
  m_triangleCount         : integer;
  m_vertexStrideInBytes   : integer;
  m_indexList             : PWord;
  m_vertexList            : PdFloat;
 end;


  TNewtonHeightFieldCollisionParam = packed record
    m_width: integer;
    m_height: integer;
    m_gridsDiagonals: integer;
    m_elevationDataType: integer; // 0 = 32 bit floats, 1 = unsigned 16 bit integers
    m_verticalScale: dFloat;
    m_horizonalScale_x: dFloat;
    m_horizonalScale_z: dFloat;
    m_vertialElevation: Pointer;
    m_atributes: pchar;
  end;

  TNewtonSceneCollisionParam = packed record
    m_childrenProxyCount: integer;
  end;

  TNewtonCollisionNullParam = packed record
    // nothing.
  end;

  PNewtonCollisionInfoRecord = ^TNewtonCollisionInfoRecord;
  TNewtonCollisionInfoRecord = packed record
    m_offsetMatrix: array[0..3,0..3] of dFloat;
	m_collisionMaterial: TNewtonCollisionMaterial;
    m_collisionType: integer;          // tag id to identify the collision primitive
    case Integer of
      SERIALIZE_ID_BOX: (sdbox: TNewtonBoxParam);
      SERIALIZE_ID_CONE: (shapedata: TNewtonConeParam);
      SERIALIZE_ID_SPHERE: (sdSphere: TNewtonSphereParam);
      SERIALIZE_ID_CAPSULE: (sdCapsule: TNewtonCapsuleParam);
      SERIALIZE_ID_CYLINDER: (sdCylinder: TNewtonCylinderParam);
      SERIALIZE_ID_CHAMFERCYLINDER: (sdChamfercylinder: TNewtonChamferCylinderParam);
      SERIALIZE_ID_CONVEXHULL: (sdConvexhull: TNewtonConvexHullParam);
      SERIALIZE_ID_NULL: (sdNull: TNewtonCollisionNullParam);
      SERIALIZE_ID_COMPOUND: (sdCompound: TNewtonCompoundCollisionParam);
      SERIALIZE_ID_TREE: (sdTree: TNewtonCollisionTreeParam);
      SERIALIZE_ID_HEIGHTFIELD: (sdHeightfield: TNewtonHeightFieldCollisionParam);
      SERIALIZE_ID_SCENE: (sdSceneCollision: TNewtonSceneCollisionParam);

  end;


  PNewtonJointRecord = ^NewtonJointRecord;
  NewtonJointRecord = packed record
    m_attachmenMatrix_0 : array[ 0..3,0..3 ] of dFloat;
    m_attachmenMatrix_1 : array[ 0..3,0..3 ] of dFloat;
    m_minLinearDof      : array[ 0..2 ] of dFloat;
    m_maxLinearDof      : array[ 0..2 ] of dFloat;
    m_minAngularDof     : array[ 0..2 ] of dFloat;
    m_maxAngularDof     : array[ 0..2 ] of dFloat;
    m_attachBody_0      : PNewtonBody;
    m_attachBody_1      : PNewtonBody;
    m_extraParameters   : array[ 0..15 ] of dFloat;
    m_bodiesCollisionOn : integer;
    m_descriptionType   : array[ 0..31 ] of dFloat;
  end;


  PNewtonUserMeshCollisionCollideDesc = ^NewtonUserMeshCollisionCollideDesc;
  NewtonUserMeshCollisionCollideDesc = record
    m_boxP0               : array[ 0..3 ] of dFloat; // lower bounding box of intersection query in local space
    m_boxP1               : array[ 0..3 ] of dFloat; // upper bounding box of intersection query in local space
    m_boxDistanceTravel	  : array[ 0..3 ] of dFloat; // max distance that box bpxP0 and boxP1 can travel on this timestep, used this for continue collision mode.
    m_threadNumber        : integer;                     // current thread executing this query
    m_faceCount           : integer;                     // the application should set here how many polygons intersect the query box
    m_vertexStrideInBytes : integer;                     // the application should set here the size of each vertex
  	m_skinThickness       : dFloat;                  // this is the minimum skin separation specified by the material between these two colliding shapes
    m_userData            : Pointer;                 // user data passed to the collision geometry at creation time

    m_objBody             : PNewtonBody;             // pointer to the colliding body
    m_polySoupBody        : PNewtonBody;             // pointer to the rigid body owner of this collision tree
	m_objCollision 	      : PNewtonCollision;	     // collision shape of the colliding body, (no necessarily the collision of m_objBody)
	m_polySoupCollision   : PNewtonCollision;  	     // collision shape of the collision tree, (no necessarily the collision of m_polySoupBody)

    m_vertex              : ^dFloat;                 // the application should set here the pointer to the global vertex of the mesh.
    m_faceIndexCount      : ^integer;                    // the application should set here the pointer to the vertex count of each face.
    m_faceVertexIndex     : ^integer;                    // the application should set here the pointer index array for each vertex on a face.
												  // the format of a face is I0, I1, I2, I3, ..., M, N, E0, E1, E2, ..., A
                          // I0, I1, I2, .. are the indices to the vertex, relative to m_vertex pointer
                         	// M is the index to the material sub shape id
													// N in the index to the vertex normal relative to m_vertex pointer
													// E0, E1, E2, ... are the indices of the the face normal that is shared to that face edge, when the edge does not share a face normal then the edge index is set to index N, which the index to the face normal
													// A is and estimate of the largest diagonal of the face, this used internally as a hint to improve floating point accuracy and algorithm performance.
  end;

  PNewtonWorldConvexCastReturnInfo = ^NewtonWorldConvexCastReturnInfo;
  NewtonWorldConvexCastReturnInfo = record
    m_point            : array[ 0..3 ] of dFloat; // collision point in global space
    m_normal           : array[ 0..3 ] of dFloat; // surface normal at collision point in global space
    m_normalOnHitPoint : array[ 0..3 ] of dFloat; // surface normal at the surface of the hit body,
					         // is the same as the normal calculated by a ray cast hitting the body at the hit poi

    m_contactID        : integer;                    // collision ID at contact point
    m_hitBody          : PNewtonBody;            // body hit at contact point
    m_penetration      : dFloat;                 // contact penetration at collision point
  end;

  PNewtonUserMeshCollisionRayHitDesc = ^NewtonUserMeshCollisionRayHitDesc;
  NewtonUserMeshCollisionRayHitDesc = record
    m_p0        : array[ 0..3 ] of dFloat; // ray origin in collision local space
    m_p1        : array[ 0..3 ] of dFloat; // ray destination in collision local space
    m_normalOut : array[ 0..3 ] of dFloat; // copy here the normal at the ray intersection
    m_userIdOut : integer;                     // copy here a user defined id for further feedback
    m_userData  : Pointer;                 // user data passed to the collision geometry at creation time
  end;


  PNewtonHingeSliderUpdateDesc = ^NewtonHingeSliderUpdateDesc;
  NewtonHingeSliderUpdateDesc = packed record
    m_accel       : dFloat;
    m_minFriction : dFloat;
    m_maxFriction : dFloat;
    m_timestep    : dFloat;
  end;

 PNewtonUserContactPoint = ^NewtonUserContactPoint;
 NewtonUserContactPoint = packed record
  m_point                : array [0..3] of dFloat;
  m_normal               : array [0..3] of dFloat;
  m_shapeId0             : dLong;
  m_shapeId1             : dLong;
  m_penetration          : dFloat;
  m_unused               : array [0..2] of integer;
 end;

 // data structure for interfacing with NewtonMesh
 PNewtonMeshDoubleData = ^NewtonMeshDoubleData;
 NewtonMeshDoubleData = packed record
  m_data          : PdFloat64;
  m_indexList     : ^integer;
  m_strideInBytes : integer;
 end;

 PNewtonMeshFloatData = ^NewtonMeshFloatData;
 NewtonMeshFloatData = packed record
  m_data          : PdFloat;
  m_indexList     : ^integer;
  m_strideInBytes : integer;
 end;

 PNewtonMeshVertexFormat = ^NewtonMeshVertexFormat;
 NewtonMeshVertexFormat = packed record
  m_faceCount : integer;
  m_faceIndexCount : ^integer;
  m_faceMaterial : ^integer;
  m_vertex : NewtonMeshDoubleData;
  m_normal : NewtonMeshFloatData;
  m_binormal : NewtonMeshFloatData;
  m_uv0 : NewtonMeshFloatData;
  m_uv1 : NewtonMeshFloatData;
  m_vertexColor : NewtonMeshFloatData;
 end;

  PNewtonAllocMemory = ^NewtonAllocMemory;
  NewtonAllocMemory = function(sizeInBytes: integer): Pointer; cdecl;

  PNewtonFreeMemory = ^NewtonFreeMemory;
  NewtonFreeMemory = procedure(ptr: Pointer; sizeInBytes: integer); cdecl;

  PNewtonDestroyWorld = ^NewtonDestroyWorld;
  NewtonDestroyWorld = procedure(const NewtonWorld: PNewtonWorld); cdecl;

  PNewtonGetTicksCountCallback = ^NewtonGetTicksCountCallback;
  NewtonGetTicksCountCallback = function(): Cardinal; cdecl;

  PNewtonSerialize = ^NewtonSerialize;
  NewtonSerialize = procedure(serializeHandle: Pointer; const buffer: Pointer; size: size_t); cdecl;

  PNewtonDeserialize = ^NewtonDeserialize;
  NewtonDeserialize = procedure(serializeHandle: Pointer; buffer: Pointer; size: size_t); cdecl;

  PNewtonUserMeshCollisionDestroyCallback = ^NewtonUserMeshCollisionDestroyCallback;
  NewtonUserMeshCollisionDestroyCallback = procedure(descData: Pointer); cdecl;

  PNewtonUserMeshCollisionCollideCallback = ^NewtonUserMeshCollisionCollideCallback;
  NewtonUserMeshCollisionCollideCallback = procedure(NewtonUserMeshCollisionCollideDesc: PNewtonUserMeshCollisionCollideDesc); cdecl;

  PNewtonUserMeshCollisionRayHitCallback = ^NewtonUserMeshCollisionRayHitCallback;
  NewtonUserMeshCollisionRayHitCallback = function(NewtonUserMeshCollisionRayHitDesc: PNewtonUserMeshCollisionRayHitDesc): integer; cdecl;

  PNewtonUserMeshCollisionGetCollisionInfo = ^NewtonUserMeshCollisionGetCollisionInfo;
  NewtonUserMeshCollisionGetCollisionInfo = procedure(userData: Pointer; infoRecord: PNewtonCollisionInfoRecord); cdecl;

  PNewtonUserMeshCollisionGetFacesInAABB = ^NewtonUserMeshCollisionGetFacesInAABB;
  NewtonUserMeshCollisionGetFacesInAABB = function(userData: Pointer;
    const p0: PdFloat; const p1: PdFloat; const vertexArray: PdFloat;
    vertexCount: PInteger; vertexStrideInBytes: PInteger; const indexList: PInteger;
    maxIndexCount: integer; const userDataList: PInteger): integer; cdecl;

  PNewtonCollisionTreeRayCastCallback = ^NewtonCollisionTreeRayCastCallback;
  NewtonCollisionTreeRayCastCallback = function(const Body: PNewtonBody;
    const TreeCollision: PNewtonCollision; interception: dFloat;
    normal: PdFloat; faceId: integer; usedData: Pointer): dFloat; cdecl;

  PNewtonHeightFieldRayCastCallback = ^NewtonHeightFieldRayCastCallback;
  NewtonHeightFieldRayCastCallback = function(const Body: PNewtonBody;
    const HeightFieldCollision: PNewtonCollision; interception: dFloat;
    Row, Col: integer; normal: PdFloat; faceId: integer; usedData: Pointer)
    : dFloat; cdecl;

  PNewtonTreeCollisionCallback = ^NewtonTreeCollisionCallback;
  NewtonTreeCollisionCallback = procedure(const bodyWithTreeCollision
    : PNewtonBody; const Body: PNewtonBody; faceId: integer; const vertex: PdFloat;
    vertexStrideInBytes: integer); cdecl;

  PNewtonBodyDestructor = ^NewtonBodyDestructor;
  NewtonBodyDestructor = procedure(const Body: PNewtonBody); cdecl;

  PNewtonApplyForceAndTorque = ^NewtonApplyForceAndTorque;
  NewtonApplyForceAndTorque = procedure(const Body: PNewtonBody;
    timestep: dFloat; threadIndex: integer); cdecl;

  PNewtonSetTransform = ^NewtonSetTransform;
  NewtonSetTransform = procedure(const Body: PNewtonBody; const matrix: PdFloat; threadIndex: integer); cdecl;

  PNewtonIslandUpdate = ^NewtonIslandUpdate;
  NewtonIslandUpdate = function(const World: PNewtonWorld; islandHandle: Pointer; bodyCount: integer): integer; cdecl;

  PNewtonBodyLeaveWorld = ^NewtonBodyLeaveWorld;
  NewtonBodyLeaveWorld = procedure(const Body: PNewtonBody; threadIndex: integer); cdecl;

  PNewtonDestroyBodyByExeciveForce = ^NewtonDestroyBodyByExeciveForce;
  NewtonDestroyBodyByExeciveForce = procedure(const Body: PNewtonBody; const contact: PNewtonJoint); cdecl;

  PNewtonCollisionDestructor = ^NewtonCollisionDestructor;
  NewtonCollisionDestructor = procedure(const World: PNewtonWorld; const collision: PNewtonCollision); cdecl;

  PNewtonCollisionCompoundBreakableCallback = ^NewtonCollisionCompoundBreakableCallback;
  NewtonCollisionCompoundBreakableCallback = function(const Mesh: PNewtonMesh; userData: Pointer; planeMatrixOut: PdFloat): integer; cdecl;

  PNewtonGetBuoyancyPlane = ^NewtonGetBuoyancyPlane;
  NewtonGetBuoyancyPlane = function(const collisionID: integer; context: Pointer; const globalSpaceMatrix: PdFloat; globalSpacePlane: PdFloat): integer; cdecl;

  PNewtonWorldRayPrefilterCallback = ^NewtonWorldRayPrefilterCallback;
  NewtonWorldRayPrefilterCallback = function(const Body: PNewtonBody; const collision: PNewtonCollision; userData: Pointer): Cardinal; cdecl;

  PNewtonWorldRayFilterCallback = ^NewtonWorldRayFilterCallback;
  NewtonWorldRayFilterCallback = function(const Body: PNewtonBody; const hitNormal: PdFloat; collisionID: integer; userData: Pointer; intersetParam: dFloat): dFloat; cdecl;

  PNewtonOnAABBOverlap = ^NewtonOnAABBOverlap;
  NewtonOnAABBOverlap = function(const material: PNewtonMaterial; const body0: PNewtonBody; const body1: PNewtonBody; threadIndex: integer): integer; cdecl;

  PNewtonContactsProcess = ^NewtonContactsProcess;
  NewtonContactsProcess = procedure(const contact: PNewtonJoint; timestep: dFloat; threadIndex: integer); cdecl;

  PNewtonBodyIterator = ^NewtonBodyIterator;
  NewtonBodyIterator = procedure(const Body: PNewtonBody; userData: Pointer); cdecl;

  PNewtonJointIterator = ^NewtonJointIterator;
  NewtonJointIterator = procedure(const joint: PNewtonJoint; userData: Pointer); cdecl;

  PNewtonCollisionIterator = ^NewtonCollisionIterator;
  NewtonCollisionIterator = procedure(userData: Pointer; vertexCount: integer; const FaceArray: PdFloat; faceId: integer); cdecl;

  PNewtonBallCallBack = ^NewtonBallCallBack;
  NewtonBallCallBack = procedure(const ball: PNewtonJoint; timestep: dFloat); cdecl;

  PNewtonHingeCallBack = ^NewtonHingeCallBack;
  NewtonHingeCallBack = function(const hinge: PNewtonJoint; desc: PNewtonHingeSliderUpdateDesc): unsigned_int; cdecl;

  PNewtonSliderCallBack = ^NewtonSliderCallBack;
  NewtonSliderCallBack = function(const slider: PNewtonJoint; desc: PNewtonHingeSliderUpdateDesc): unsigned_int; cdecl;

  PNewtonUniversalCallBack = ^NewtonUniversalCallBack;
  NewtonUniversalCallBack = function(const universal: PNewtonJoint; desc: PNewtonHingeSliderUpdateDesc): unsigned_int; cdecl;

  PNewtonCorkscrewCallBack = ^NewtonCorkscrewCallBack;
  NewtonCorkscrewCallBack = function(const corkscrew: PNewtonJoint; desc: PNewtonHingeSliderUpdateDesc): unsigned_int; cdecl;

  PNewtonUserBilateralCallBack = ^NewtonUserBilateralCallBack;
  NewtonUserBilateralCallBack = procedure(const userJoint: PNewtonJoint; timestep: dFloat; threadIndex: integer); cdecl;

  PNewtonUserBilateralGetInfoCallBack = ^NewtonUserBilateralGetInfoCallBack;
  NewtonUserBilateralGetInfoCallBack = procedure(const userJoint: PNewtonJoint; info: PNewtonJointRecord); cdecl;

  PNewtonConstraintDestructor = ^NewtonConstraintDestructor;
  NewtonConstraintDestructor = procedure(const me: PNewtonJoint); cdecl;


// *****************************************************************************************************************************
//
// world control functions
//
// *****************************************************************************************************************************
function NewtonWorldGetVersion(const NewtonWorld: PNewtonWorld): Integer; cdecl; external NewtonDLL;
function NewtonWorldFloatSize(const NewtonWorld: PNewtonWorld): Integer; cdecl; external NewtonDLL;

function NewtonCreate(malloc: NewtonAllocMemory; mfree: NewtonFreeMemory): PNewtonWorld; cdecl; external NewtonDLL;
procedure NewtonDestroy(const NewtonWorld: PNewtonWorld); cdecl; external NewtonDLL;
procedure NewtonDestroyAllBodies(const NewtonWorld: PNewtonWorld); cdecl; external NewtonDLL;

function NewtonGetMemoryUsed(): Integer; cdecl; external NewtonDLL;

procedure NewtonSetMemorySystem(malloc: NewtonAllocMemory; mfree: NewtonFreeMemory); cdecl; external NewtonDLL;
procedure NewtonUpdate(const NewtonWorld: PNewtonWorld; timestep: dFloat); cdecl; external NewtonDLL;
procedure NewtonInvalidateCache(const NewtonWorld: PNewtonWorld); cdecl; external NewtonDLL;
procedure NewtonCollisionUpdate(const NewtonWorld: PNewtonWorld); cdecl; external NewtonDLL;

procedure NewtonSetSolverModel(const NewtonWorld: PNewtonWorld; Model: integer); cdecl; external NewtonDLL;
procedure NewtonSetPlatformArchitecture(const NewtonWorld: PNewtonWorld; mode: integer); cdecl; external NewtonDLL;

function NewtonGetPlatformArchitecture(const NewtonWorld: PNewtonWorld; description: PCharArray): integer; cdecl; external NewtonDLL;

procedure NewtonSetMultiThreadSolverOnSingleIsland(const NewtonWorld: PNewtonWorld; mode: integer); cdecl; external NewtonDLL;

function NewtonGetMultiThreadSolverOnSingleIsland(const NewtonWorld: PNewtonWorld): integer; cdecl; external NewtonDLL;

procedure NewtonSetPerformanceClock(const NewtonWorld: PNewtonWorld; NewtonGetTicksCountCallback: PNewtonGetTicksCountCallback); cdecl; external NewtonDLL;

function NewtonReadPerformanceTicks(const NewtonWorld: PNewtonWorld; performanceEntry: Cardinal): Cardinal; cdecl; external NewtonDLL;
function NewtonReadThreadPerformanceTicks(const NewtonWorld: PNewtonWorld; threadIndex: Cardinal): Cardinal; cdecl; external NewtonDLL;

procedure NewtonWorldCriticalSectionLock(const NewtonWorld: PNewtonWorld); cdecl; external NewtonDLL;
procedure NewtonWorldCriticalSectionUnlock(const NewtonWorld: PNewtonWorld); cdecl; external NewtonDLL;

procedure NewtonSetThreadsCount(const NewtonWorld: PNewtonWorld; threads: integer); cdecl; external NewtonDLL;

function NewtonGetThreadsCount(const NewtonWorld: PNewtonWorld): integer; cdecl; external NewtonDLL;
function NewtonGetMaxThreadsCount(const NewtonWorld: PNewtonWorld): integer; cdecl; external NewtonDLL;

procedure NewtonSetFrictionModel(const NewtonWorld: PNewtonWorld; Model: integer); cdecl; external NewtonDLL;

procedure NewtonSetMinimumFrameRate(const NewtonWorld: PNewtonWorld; frameRate: dFloat); cdecl; external NewtonDLL;
procedure NewtonSetBodyLeaveWorldEvent(const NewtonWorld: PNewtonWorld; callback: PNewtonBodyLeaveWorld); cdecl; external NewtonDLL;
procedure NewtonSetWorldSize(const NewtonWorld: PNewtonWorld; const minPoint: PdFloat; const maxPoint: PdFloat); cdecl; external NewtonDLL;

procedure NewtonSetIslandUpdateEvent(const NewtonWorld: PNewtonWorld; NewtonIslandUpdate: PNewtonIslandUpdate); cdecl; external NewtonDLL;

procedure NewtonSetCollisionDestructor(const NewtonWorld: PNewtonWorld; callback: PNewtonCollisionDestructor); cdecl; external NewtonDLL;

procedure NewtonSetDestroyBodyByExeciveForce(const NewtonWorld: PNewtonWorld; callback: PNewtonDestroyBodyByExeciveForce); cdecl; external NewtonDLL;

procedure NewtonWorldForEachJointDo(const NewtonWorld: PNewtonWorld; callback: PNewtonJointIterator; userData: Pointer); cdecl; external NewtonDLL;

procedure NewtonWorldForEachBodyInAABBDo(const NewtonWorld: PNewtonWorld; const p0: PdFloat; const p1: PdFloat; callback: PNewtonBodyIterator; userData: Pointer); cdecl; external NewtonDLL;

procedure NewtonWorldSetUserData(const NewtonWorld: PNewtonWorld; userData: Pointer); cdecl; external NewtonDLL;
function NewtonWorldGetUserData(const NewtonWorld: PNewtonWorld): Pointer; cdecl; external NewtonDLL;

procedure NewtonWorldSetDestructorCallBack(const NewtonWorld: PNewtonWorld; NewtonDestroyWorld: PNewtonDestroyWorld); cdecl; external NewtonDLL;
function NewtonWorldGetDestructorCallBack(const NewtonWorld: PNewtonWorld): PNewtonDestroyWorld; cdecl; external NewtonDLL;

procedure NewtonWorldRayCast(const newtonWorld: PNewtonWorld; const p0: PdFloat; const p1: PdFloat;
                             filter: PNewtonWorldRayFilterCallback; userData: Pointer;
                             prefilter: NewtonWorldRayPrefilterCallback); cdecl; external NewtonDLL;

function NewtonWorldConvexCast(const newtonWorld: PNewtonWorld; const matrix: PdFloat; const target: PdFloat;
                               const shape: PNewtonCollision; hitParam: PdFloat; userData: Pointer;
                               prefilter: NewtonWorldRayPrefilterCallback; info: PNewtonWorldConvexCastReturnInfo;
                               maxContactsCount: integer; threadIndex: integer): Integer; cdecl; external NewtonDLL;

function NewtonWorldGetBodyCount(const NewtonWorld: PNewtonWorld): integer; cdecl; external NewtonDLL;

function NewtonWorldGetConstraintCount(const NewtonWorld: PNewtonWorld): integer; cdecl; external NewtonDLL;


// *****************************************************************************************************************************
//
// Simulation islands
//
// *****************************************************************************************************************************

function NewtonIslandGetBody(const island: Pointer; bodyIndex: integer): PNewtonBody; cdecl; external NewtonDLL;

procedure NewtonIslandGetBodyAABB(const island: Pointer; bodyIndex: integer; p0: PdFloat; p1: PdFloat); cdecl; external NewtonDLL;

// *****************************************************************************************************************************
//
//  Physics Material Section
//
// *****************************************************************************************************************************
function NewtonMaterialCreateGroupID(const NewtonWorld: PNewtonWorld): integer; cdecl; external NewtonDLL;
function NewtonMaterialGetDefaultGroupID(const NewtonWorld: PNewtonWorld): integer; cdecl; external NewtonDLL;
procedure NewtonMaterialDestroyAllGroupID(const NewtonWorld: PNewtonWorld); cdecl; external NewtonDLL;

// material definitions that can not be overwritten in function callback
function NewtonMaterialGetUserData(const NewtonWorld: PNewtonWorld; id0: integer; id1: integer): Pointer; cdecl; external NewtonDLL;
procedure NewtonMaterialSetSurfaceThickness(const NewtonWorld: PNewtonWorld; id0: integer; id1: integer; thickness: dFloat); cdecl; external NewtonDLL;

//	deprecated, not longer continue collision is set on the material
procedure NewtonMaterialSetContinuousCollisionMode(const NewtonWorld: PNewtonWOrld; id0, id1, state: Integer); cdecl; external NewtonDLL;

///procedure NewtonMaterialSetCompoundCollisionCallback(const NewtonWorld: PNewtonWOrld; id0, id1: integer; compoundAabbOverlap: PNewtonOnCompoundSubCollisionAABBOverlap);
procedure NewtonMaterialSetCollisionCallback(const NewtonWorld: PNewtonWorld; id0: integer; id1: integer; userData: Pointer; AABBOverlap: PNewtonOnAABBOverlap; process: PNewtonContactsProcess); cdecl; external NewtonDLL;

procedure NewtonMaterialSetDefaultSoftness(const NewtonWorld: PNewtonWorld; id0: integer; id1: integer; value: dFloat); cdecl; external NewtonDLL;
procedure NewtonMaterialSetDefaultElasticity(const NewtonWorld: PNewtonWorld; id0: integer; id1: integer; elasticCoef: dFloat); cdecl; external NewtonDLL;
procedure NewtonMaterialSetDefaultCollidable(const NewtonWorld: PNewtonWorld; id0: integer; id1: integer; state: integer); cdecl; external NewtonDLL;
procedure NewtonMaterialSetDefaultFriction(const NewtonWorld: PNewtonWorld; id0: integer; id1: integer; staticFriction: dFloat; kineticFriction: dFloat); cdecl; external NewtonDLL;


function NewtonWorldGetFirstMaterial(const NewtonWorld: PNewtonWorld): PNewtonMaterial; cdecl; external NewtonDLL;

function NewtonWorldGetNextMaterial(const NewtonWorld: PNewtonWorld; const material: PNewtonMaterial): PNewtonMaterial; cdecl; external NewtonDLL;

function NewtonWorldGetFirstBody(const NewtonWorld: PNewtonWorld): PNewtonBody; cdecl; external NewtonDLL;

function NewtonWorldGetNextBody(const NewtonWorld: PNewtonWorld; const curBody: PNewtonBody): PNewtonBody; cdecl; external NewtonDLL;

// *****************************************************************************************************************************
//
// Physics Contact control functions
//
// *****************************************************************************************************************************
function NewtonMaterialGetMaterialPairUserData(const material: PNewtonMaterial): Pointer; cdecl; external NewtonDLL;
function NewtonMaterialGetContactFaceAttribute(const material: PNewtonMaterial): Unsigned_int; cdecl; external NewtonDLL;
function NewtonMaterialGetBodyCollidingShape(const material: PNewtonMaterial; const Body: PNewtonBody): PNewtonCollision; external NewtonDLL;
function NewtonMaterialGetBodyCollisionID(const material: PNewtonMaterial; body: PNewtonBody): Unsigned_int; cdecl; external NewtonDLL;

function NewtonMaterialGetContactNormalSpeed(const material: PNewtonMaterial): dFloat; cdecl; external NewtonDLL;
procedure NewtonMaterialGetContactForce(const material: PNewtonMaterial; const Body: PNewtonBody; Force: PdFloat); cdecl; external NewtonDLL;
procedure NewtonMaterialGetContactPositionAndNormal(const material: PNewtonMaterial; const Body: PNewtonBody; const Posit, normal: PdFloat); cdecl; external NewtonDLL;
procedure NewtonMaterialGetContactTangentDirections(const material: PNewtonMaterial; const Body: PNewtonBody; const Dir0, Dir1: PdFloat); cdecl; external NewtonDLL;

function NewtonMaterialGetContactTangentSpeed(const material: PNewtonMaterial;index: Integer): dFloat; cdecl; external NewtonDLL;

procedure NewtonMaterialSetContactSoftness(const material: PNewtonMaterial; softness: dFloat); cdecl; external NewtonDLL;
procedure NewtonMaterialSetContactElasticity(const material: PNewtonMaterial; restitution: dFloat); cdecl; external NewtonDLL;
procedure NewtonMaterialSetContactFrictionState(const material: PNewtonMaterial; state: Integer; index: Integer); cdecl; external NewtonDLL;
procedure NewtonMaterialSetContactFrictionCoef(const material: PNewtonMaterial; staticFrictionCoef, kineticFrictionCoef: dFloat; index: Integer); cdecl; external NewtonDLL;

procedure NewtonMaterialSetContactNormalAcceleration(const material: PNewtonMaterial; accel: dFloat); cdecl; external NewtonDLL;
procedure NewtonMaterialSetContactNormalDirection(const material: PNewtonMaterial; directionVector: PdFloat); cdecl; external NewtonDLL;

procedure NewtonMaterialSetContactTangentAcceleration(const material: PNewtonMaterial; accel: dFloat; index: Integer); cdecl; external NewtonDLL;
procedure NewtonMaterialContactRotateTangentDirections(const material: PNewtonMaterial; const directionVector: PdFloat); cdecl; external NewtonDLL;


// **********************************************************************************************
//
// convex collision primitives creation functions
//
// **********************************************************************************************
function NewtonCreateNull(const NewtonWorld: PNewtonWorld): PNewtonCollision; cdecl; external NewtonDLL;
function NewtonCreateSphere(const NewtonWorld: PNewtonWorld; radiusX, radiusY, radiusZ: dFloat; shapeID: Integer; const offsetMatrix: PdFloat): PNewtonCollision; cdecl; external NewtonDLL;
function NewtonCreateBox(const NewtonWorld: PNewtonWorld; dx: dFloat; dy: dFloat; dz: dFloat; shapeID: Integer; const offsetMatrix: PdFloat): PNewtonCollision; cdecl; external NewtonDLL;
function NewtonCreateCone(const NewtonWorld: PNewtonWorld; radius: dFloat; height: dFloat; shapeID: Integer; const offsetMatrix: PdFloat): PNewtonCollision; cdecl; external NewtonDLL;
function NewtonCreateCapsule(const NewtonWorld: PNewtonWorld; radius: dFloat; height: dFloat; shapeID: Integer; const offsetMatrix: PdFloat): PNewtonCollision; cdecl; external NewtonDLL;
function NewtonCreateCylinder(const NewtonWorld: PNewtonWorld; radius: dFloat; height: dFloat; shapeID: Integer; const offsetMatrix: PdFloat): PNewtonCollision; cdecl; external NewtonDLL;
function NewtonCreateChamferCylinder(const NewtonWorld: PNewtonWorld; raduis: dFloat; height: dFloat; shapeID: Integer; const offsetMatrix: PdFloat): PNewtonCollision; cdecl; external NewtonDLL;
function NewtonCreateConvexHull(const NewtonWorld: PNewtonWorld; count: Integer; const vertexCloud: PdFloat; strideInBytes: Integer; tolerance: dFloat; shapeID: Integer; const offsetMatrix: PdFloat): PNewtonCollision; cdecl; external NewtonDLL;
function NewtonCreateConvexHullFromMesh(const NewtonWorld: PNewtonWorld; ìesh: PNewtonMesh; tolerance: dFloat; shapeID: Integer): PNewtonCollision; cdecl; external NewtonDLL;

function NewtonCreateConvexHullModifier(const NewtonWorld: PNewtonWorld; const convexHullCollision: PNewtonCollision; shapeID: Integer): PNewtonCollision; cdecl; external NewtonDLL;
procedure NewtonConvexHullModifierGetMatrix(const convexHullCollision: PNewtonCollision; matrix: PdFloat); cdecl; external NewtonDLL;
procedure NewtonConvexHullModifierSetMatrix(const convexHullCollision: PNewtonCollision; const matrix: PdFloat); cdecl; external NewtonDLL;

function NewtonCollisionIsTriggerVolume(const convexCollision: PNewtonCollision): Integer; cdecl; external NewtonDLL;
procedure NewtonCollisionSetAsTriggerVolume(const convexCollision: PNewtonCollision; trigger: Integer); cdecl; external NewtonDLL;

procedure NewtonCollisionSetMaxBreakImpactImpulse(const convexHullCollision: PNewtonCollision; maxImpactImpulse: dFloat); cdecl; external NewtonDLL;
function NewtonCollisionGetMaxBreakImpactImpulse(const convexHullCollision: PNewtonCollision): dFloat; cdecl; external NewtonDLL;

procedure NewtonCollisionSetUserID(const convexCollision: PNewtonCollision; id: Unsigned_int); cdecl; external NewtonDLL;
function NewtonCollisionGetUserID(const convexCollision: PNewtonCollision): Unsigned_int; cdecl; external NewtonDLL;

function NewtonConvexHullGetFaceIndices(const convexHullCollision: PNewtonCollision; face: Integer; faceIndices: PInteger): Integer; cdecl; external NewtonDLL;
function NewtonConvexCollisionCalculateVolume(const convexCollision: PNewtonCollision): dFloat; cdecl; external NewtonDLL;
procedure NewtonConvexCollisionCalculateInertialMatrix(const convexCollision: PNewtonCollision; inertia, origin: PdFloat); cdecl; external NewtonDLL;

procedure NewtonCollisionMakeUnique(const NewtonWorld: PNewtonWorld; const collision: PNewtonCollision); cdecl; external NewtonDLL;
procedure NewtonReleaseCollision(const NewtonWorld: PNewtonWorld; const collision: PNewtonCollision); cdecl; external NewtonDLL;
function NewtonAddCollisionReference(const collision: PNewtonCollision): Integer; cdecl; external NewtonDLL;

// **********************************************************************************************
//
// complex collision primitives creation functions
// note: can only be used with static bodies (bodies with infinite mass)
//
// **********************************************************************************************
type
  TCollisionPrimitiveArray = array of PNewtonCollision;

function NewtonCreateCompoundCollision( const newtonWorld : PNewtonWorld; count : integer;
                                        const collisionPrimitiveArray : TcollisionPrimitiveArray; shapeID : Integer) : PNewtonCollision;            cdecl; external{$IFDEF __GPC__}name 'NewtonCreateCompoundCollision'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function NewtonCreateCompoundCollisionFromMesh( const newtonWorld : PNewtonWorld; const mesh : PNewtonMesh; maxSubShapesCount : integer;
                                                shapeID : Integer; subShapeID : integer ) : PNewtonCollision;                                            cdecl; external{$IFDEF __GPC__}name 'NewtonCreateCompoundCollisionFromMesh'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function NewtonCreateUserMeshCollision( const newtonWorld : PNewtonWorld; const minBox : PdFloat;
                                        const maxBox : PdFloat; userData : Pointer; collideCallback : NewtonUserMeshCollisionCollideCallback;
                                        rayHitCallback : NewtonUserMeshCollisionRayHitCallback;
                                        destroyCallback : NewtonUserMeshCollisionDestroyCallback;
                                        getInfoCallback : NewtonUserMeshCollisionGetCollisionInfo;
                                        facesInAABBCallback : NewtonUserMeshCollisionGetFacesInAABB; shapeID : Integer) : PNewtonCollision;         cdecl; external{$IFDEF __GPC__}name 'NewtonCreateUserMeshCollision'{$ELSE}NewtonDLL{$ENDIF __GPC__};




function NewtonCreateSceneCollision(const NewtonWorld: PNewtonWorld; shapeID: Integer): PNewtonCollision; cdecl; external NewtonDLL;
function NewtonSceneCollisionCreateProxy(scene: PNewtonCollision; collision: PNewtonCollision): PNewtonSceneProxy; cdecl; external NewtonDLL;
procedure NewtonSceneCollisionDestroyProxy(scene: PNewtonCollision; Proxy: PNewtonSceneProxy); cdecl; external NewtonDLL;
procedure NewtonSceneProxySetMatrix(Proxy: PNewtonSceneProxy; const matrix: PdFloat); cdecl; external NewtonDLL;
procedure NewtonSceneProxyGetMatrix(Proxy: PNewtonSceneProxy; matrix: PdFloat); cdecl; external NewtonDLL;
procedure NewtonSceneSetProxyUserData(const Proxy: PNewtonSceneProxy; userData: Pointer); cdecl; external NewtonDLL;
function NewtonSceneGetProxyUserData(const Proxy: PNewtonSceneProxy): Pointer; cdecl; external NewtonDLL;
function NewtonSceneGetFirstProxy(const scene: PNewtonCollision): PNewtonSceneProxy; cdecl; external NewtonDLL;
function NewtonSceneGetNextProxy(const scene: PNewtonCollision; const Proxy: PNewtonSceneProxy): PNewtonSceneProxy; cdecl; external NewtonDLL;

procedure NewtonSceneCollisionOptimize( scene : PNewtonCollision );                                                                              cdecl; external NewtonDLL;


// **********************************************************************************************
//
// Fractured compound collision primitives interface
//
// **********************************************************************************************
function NewtonCreateCompoundBreakable( const NewtonWorld : PNewtonWorld; meshCount : integer; const SolidsArray : PNewtonMesh; const ShapeIDArray : PInteger; Densities : PdFloat; internalFaceMaterial : PInteger; ShapeID : Integer; debrisID : Integer; DebrisSeparationGap : dFloat ) : PNewtonCollision; cdecl; external{$IFDEF __GPC__}name 'NewtonCreateCompoundBreakable'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonCompoundBreakableResetAnchoredPieces( const compoundBreakable : PNewtonCollision );                                                                                                                                                                                      cdecl; external{$IFDEF __GPC__}name 'NewtonCompoundBreakableResetAnchoredPieces'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonCompoundBreakableSetAnchoredPieces( const compoundBreakable : PNewtonCollision; fixshapesCount : Integer; matrixPallete : PdFloat; fixedShapesArray : PNewtonCollision );                                                                                                     cdecl; external{$IFDEF __GPC__}name 'NewtonCompoundBreakableSetAnchoredPieces'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonCompoundBreakableGetVertexCount( const compoundBreakable : PNewtonCollision ) : Integer;                                                                                                                                                                                     cdecl; external{$IFDEF __GPC__}name 'NewtonCompoundBreakableGetVertexCount'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonCompoundBreakableGetVertexStreams( const compoundBreakable : PNewtonCollision; vertexStrideInByte : Integer; Vertex : PdFloat; normalStrideInByte : Integer; normal : PdFloat; uvStrideInByte : Integer; uv : PdFloat );                                                                cdecl; external{$IFDEF __GPC__}name 'NewtonCompoundBreakableGetVertexStreams'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonBreakableGetMainMesh( const compoundBreakable : PNewtonCollision ) : PNewtonBreakableComponentMesh;                                                                                                                                                                      cdecl; external{$IFDEF __GPC__}name 'NewtonBreakableGetMainMesh'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonBreakableGetFirstComponent( const compoundBreakable : PNewtonCollision ) : PNewtonBreakableComponentMesh;                                                                                                                                                                cdecl; external{$IFDEF __GPC__}name 'NewtonBreakableGetFirstComponent'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonBreakableGetNextComponent( const component : PNewtonBreakableComponentMesh ) : PNewtonBreakableComponentMesh;                                                                                                                                                            cdecl; external{$IFDEF __GPC__}name 'NewtonBreakableGetNextComponent'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonBreakableBeginDelete( const compoundBreakable : PNewtonCollision );                                                                                                                                                                                                      cdecl; external{$IFDEF __GPC__}name 'NewtonBreakableBeginDelete'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonBreakableCreateDebrieBody( const compoundBreakable : PNewtonCollision; const component : PNewtonBreakableComponentMesh ) : PNewtonBody;                                                                                                                                  cdecl; external{$IFDEF __GPC__}name 'NewtonBreakableCreateDebrieBody'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBreakableDeleteComponent( const compoundBreakable : PNewtonCollision; const component : PNewtonBreakableComponentMesh );                                                                                                                                                 cdecl; external{$IFDEF __GPC__}name 'NewtonBreakableDeleteComponent'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBreakableEndDelete( const compoundBreakable : PNewtonCollision );                                                                                                                                                                                                        cdecl; external{$IFDEF __GPC__}name 'NewtonBreakableBeginDelete'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonBreakableGetComponentsInRadius( const compoundBreakable : PNewtonCollision; const position : PdFloat; radius : dFloat; Segments : PNewtonBreakableComponentMesh; maxCount : Integer) : Integer;                                                                                   cdecl; external{$IFDEF __GPC__}name 'NewtonBreakableGetComponentsInRadius'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonBreakableGetFirstSegment( const BreakableComponent : PNewtonBreakableComponentMesh ) : Pointer;                                                                                                                                                                          cdecl; external{$IFDEF __GPC__}name 'NewtonBreakableGetFirstSegment'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonBreakableGetNextSegment( const Segment : Pointer ) : Pointer;                                                                                                                                                                                                            cdecl; external{$IFDEF __GPC__}name 'NewtonBreakableGetNextSegment'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonBreakableSegmentGetMaterial( const Segment : Pointer ) : Integer;                                                                                                                                                                                                            cdecl; external{$IFDEF __GPC__}name 'NewtonBreakableSegmentGetMaterial'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonBreakableSegmentGetIndexCount( const Segment : Pointer ) : Integer;                                                                                                                                                                                                          cdecl; external{$IFDEF __GPC__}name 'NewtonBreakableSegmentGetIndexCount'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonBreakableSegmentGetIndexStream( CompoundBreakable : PNewtonCollision; const MeshOwner : PNewtonBreakableComponentMesh; const Segment : Pointer; Index : PInteger) : Integer;                                                                                                    cdecl; external{$IFDEF __GPC__}name 'NewtonBreakableSegmentGetIndexStream'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonBreakableSegmentGetIndexStreamShort( CompoundBreakable : PNewtonCollision; const MeshOwner : PNewtonBreakableComponentMesh; const Segment : Pointer; Index : PShort ) : Integer;                                                                                             cdecl; external{$IFDEF __GPC__}name 'NewtonBreakableSegmentGetIndexStreamShort'{$ELSE}NewtonDLL{$ENDIF __GPC__};


//  ***********************************************************************************************************
//
//	Collision serialization functions
//
	// ***********************************************************************************************************

function NewtonCreateCollisionFromSerialization( const newtonWorld : PNewtonWorld; deserializeFunction : PNewtonDeserialize; serializeHandle : Pointer ) : PNewtonCollision; cdecl; external{$IFDEF __GPC__}name 'NewtonCreateCollisionFromSerialization'{$ELSE}NewtonDLL{$ENDIF __GPC__};

//  ***********************************************************************************************************
//
//	Collision serialization functions
//
// ***********************************************************************************************************
procedure NewtonCollisionSerialize( const newtonWorld : PNewtonWorld; const collision : PNewtonCollision; serializeFunction : PNewtonSerialize; serializeHandle : Pointer ); cdecl; external{$IFDEF __GPC__}name 'NewtonCollisionSerialize'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonCollisionGetInfo( const collision : PNewtonCollision; collisionInfo : PNewtonCollisionInfoRecord); cdecl; external{$IFDEF __GPC__}name 'NewtonCollisionGetInfo'{$ELSE}NewtonDLL{$ENDIF __GPC__};

// **********************************************************************************************
//
// Static collision shapes functions
//
// **********************************************************************************************
function  NewtonCreateHeightFieldCollision( const newtonWorld : PNewtonWorld; width, height, gridDiagonals : integer; elevationMap : PUnsigned_short; attributeMap : P2Char; horizontalScale,verticalScale : dFloat; shapeID : Integer) : PNewtonCollision; cdecl; external{$IFDEF __GPC__}name 'NewtonCreateHeightFieldCollision'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonHeightFieldSetUserRayCastCallback (const TreeCollision : PNewtonCollision; RayHitCallBack : PNewtonHeightFieldRayCastCallback); cdecl; external{$IFDEF __GPC__}name 'NewtonHeightFieldSetUserRayCastCallback'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonCreateTreeCollision( const newtonWorld : PNewtonWorld; shapeID : Integer) : PNewtonCollision; cdecl; external NewtonDLL;
function  NewtonCreateTreeCollisionFromMesh (const newtonWorld : PNewtonWorld; const mesh: PNewtonMesh; shapeID: integer) : PNewtonCollision; cdecl; external NewtonDLL;
procedure NewtonTreeCollisionSetUserRayCastCallback( const treeCollision : PNewtonCollision; rayHitCallback : PNewtonCollisionTreeRayCastCallback ); cdecl; external NewtonDLL;

procedure NewtonTreeCollisionBeginBuild( const treeCollision : PNewtonCollision ); cdecl; external NewtonDLL;
procedure NewtonTreeCollisionAddFace( const treeCollision : PNewtonCollision; vertexCount : integer; const vertexPtr : PdFloat;
                                      strideInBytes : integer; faceAttribute : integer ); cdecl; external NewtonDLL;
procedure NewtonTreeCollisionEndBuild( const treeCollision : PNewtonCollision; optimize : integer ); cdecl; external NewtonDLL;

function  NewtonTreeCollisionGetFaceAtribute( const treeCollision : PNewtonCollision; const faceIndexArray : Pinteger): integer; cdecl; external NewtonDLL;
procedure NewtonTreeCollisionSetFaceAtribute( const treeCollision : PNewtonCollision; const faceIndexArray : Pinteger;
                                              attribute : integer ); cdecl; external{$IFDEF __GPC__}name 'NewtonTreeCollisionSetFaceAtribute'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonTreeCollisionGetVertexListIndexListInAABB( const treeCollision : PNewtonCollision; const p0, p1 : PdFloat; const vertexArray : PdFloat; vertexCount,vertexStrideInBytes : PInteger; const indexList : PInteger; maxIndexCount : Integer; const faceAttribute : PInteger): integer; cdecl; external NewtonDLL;


procedure NewtonStaticCollisionSetDebugCallback( const staticCollision : PNewtonCollision; userCallback : PNewtonTreeCollisionCallback ); cdecl; external{$IFDEF __GPC__}name 'NewtonStaticCollisionSetDebugCallback'{$ELSE}NewtonDLL{$ENDIF __GPC__};


// *****************************************************************************************************************************
//
// General purpose collision library functions
//
// *****************************************************************************************************************************

function  NewtonCollisionPointDistance (const newtonWorld : PNewtonWorld; const point : PdFloat;
		                                    const collision : PNewtonCollision; const matrix : PdFloat;	contact : PdFloat;
                                        normal : PdFloat; threadIndex : integer) : Integer;                                                               cdecl; external{$IFDEF __GPC__}name 'NewtonCollisionPointDistance'{$ELSE}NewtonDLL{$ENDIF __GPC__};
	// for the end user
function  NewtonCollisionClosestPoint (const newtonWorld : PNewtonWorld; const collsionA : PNewtonCollision;
                                       const matrixA : PdFloat; const collisionB : PNewtonCollision; const matrixB : PdFloat;
		                                   contactA, contactB, normalAB : PdFloat; threadIndex : integer) : Integer;                                          cdecl; external{$IFDEF __GPC__}name 'NewtonCollisionClosestPoint'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonCollisionCollide (const newtonWorld : PNewtonWorld; maxSize : Integer; const collsionA : PNewtonCollision;
                                  const matrixA : PdFloat; const collisionB : PNewtonCollision; const matrixB : PdFloat;
                                  contacts, normals, penetration : PdFloat; threadIndex : integer) : Integer;                                             cdecl; external{$IFDEF __GPC__}name 'NewtonCollisionCollide'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function NewtonCollisionCollideContinue(const newtonWorld : PNewtonWorld; maxSize : Integer; const timestep : dFloat;
		                                    const collsionA : PNewtonCollision; const matrixA : PdFloat; const velocA : PdFloat; const omegaA : dFloat;
		                                    const collsionB : PNewtonCollision; const matrixB : PdFloat; const velocB : PdFloat; const omegaB : dFloat;
		                                    timeOfImpact : PdFloat; contacts : PdFloat; normals : PdFloat;
                                        penetration : PdFloat; threadIndex : integer) : Integer;                                                          cdecl; external{$IFDEF __GPC__}name 'NewtonCollisionCollideContinue'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonCollisionSupportVertex( const collision : PNewtonCollision; const dir : PdFloat; vertex : PdFloat);                                cdecl; external{$IFDEF __GPC__}name 'NewtonCollisionSupportVertex'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonCollisionRayCast(const collision : PNewtonCollision; const p0: PdFloat; const p1: PdFloat;
                                 normals: PdFloat; attribute: PInteger): dFloat;                                                                       cdecl; external NewtonDLL;
procedure NewtonCollisionCalculateAABB( const collision : PNewtonCollision; const matrix : PdFloat; p0 : PdFloat; p1 : PdFloat );                   cdecl; external NewtonDLL;

procedure NewtonCollisionForEachPolygonDo (const collision : PNewtonCollision; const matrix : PdFloat; callback : NewtonCollisionIterator;
	// **********************************************************************************************
	//
	// collision aggregates, are a collision node on eh broad phase the serve as the root nod for a collection of rigid bodies
	// that shared the property of being in close proximity all the time, they are similar to compound collision by the group bodies instead of collision instances
	// These are good for speeding calculation calculation of rag doll, Vehicles or contractions of rigid bodied lined by joints.
	// also for example if you know that many the life time of a group of bodies like the object on a house of a building will be localize to the confide of the building
	// then warping the bodies under an aggregate will reduce collision calculation of almost an order of magnitude.
	//
	// **********************************************************************************************
                                           UserData : Pointer);                                                                                  cdecl; external{$IFDEF __GPC__}name 'NewtonCollisionForEachPolygonDo'{$ELSE}NewtonDLL{$ENDIF __GPC__};


// *****************************************************************************************************************************
//
// transforms utility functions
//
// *****************************************************************************************************************************
procedure NewtonSetEulerAngle( const eulersAngles : PdFloat; matrix : PdFloat );                                                                   cdecl; external{$IFDEF __GPC__}name 'NewtonSetEulerAngle'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonGetEulerAngle( const matrix : PdFloat; eulersAngles : PdFloat );                                                                   cdecl; external{$IFDEF __GPC__}name 'NewtonGetEulerAngle'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonCalculateSpringDamperAcceleration(dt, ks, x, kd, s : dFloat): dFloat;                                                              cdecl; external{$IFDEF __GPC__}name 'NewtonCalculateSpringDamperAcceleration'{$ELSE}NewtonDLL{$ENDIF __GPC__};

// *****************************************************************************************************************************
//
// body manipulation functions
//
// *****************************************************************************************************************************
function  NewtonCreateBody( const newtonWorld : PNewtonWorld; const collision : PNewtonCollision; const Matrix : PdFloat) : PNewtonBody;          cdecl; external{$IFDEF __GPC__}name 'NewtonCreateBody'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonDestroyBody( const newtonWorld : PNewtonWorld; const body : PNewtonBody );                                                       cdecl; external{$IFDEF __GPC__}name 'NewtonDestroyBody'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonBodyAddForce( const body : PNewtonBody; const force : PdFloat );                                                                  cdecl; external{$IFDEF __GPC__}name 'NewtonBodyAddForce'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodyAddTorque( const body : PNewtonBody; const torque : PdFloat );                                                                cdecl; external{$IFDEF __GPC__}name 'NewtonBodyAddTorque'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonBodyCalculateInverseDynamicsForce(const body : PNewtonBody; timestep : dFloat; const desiredVeloc : PdFloat; forceOut : PdFloat );  cdecl; external{$IFDEF __GPC__}name 'NewtonBodyCalculateInverseDynamicsForce'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonBodySetMatrix( const body : PNewtonBody; const matrix : PdFloat );                                                                cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetMatrix'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodySetMatrixRecursive( const body : PNewtonBody; const matrix : PdFloat );                                                       cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetMatrixRecursive'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodySetMassMatrix( const body : PNewtonBody; mass : dFloat; Ixx : dFloat; Iyy : dFloat; Izz : dFloat );                              cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetMassMatrix'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodySetMaterialGroupID( const body : PNewtonBody; id : integer );                                                                    cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetMaterialGroupID'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodySetContinuousCollisionMode(const body : PNewtonbody; state : integer);                                                           cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetContinuousCollisionMode'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodySetJointRecursiveCollision( const body : PNewtonBody; state : unsigned_int );                                                cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetJointRecursiveCollision'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodySetOmega( const body : PNewtonBody; const omega : PdFloat );                                                                  cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetOmega'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodySetVelocity( const body : PNewtonBody; const velocity : PdFloat );                                                            cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetVelocity'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodySetForce( const body : PNewtonBody; const force : PdFloat );                                                                  cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetForce'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodySetTorque( const body : PNewtonBody; const torque : PdFloat );                                                                cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetTorque'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonBodySetCentreOfMass(const body : PNewtonBody; const com : PdFloat);                                                               cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetCentreOfMass'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodySetLinearDamping( const body : PNewtonBody; linearDamp : dFloat );                                                            cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetLinearDamping'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodySetAngularDamping( const body : PNewtonBody; const angularDamp : PdFloat );                                                   cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetAngularDamping'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodySetUserData( const body : PNewtonBody; userData : Pointer );                                                                 cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetUserData'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonBodySetCollision( const body : PNewtonBody; const collision : PNewtonCollision );                                                cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetCollision'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonBodyGetSleepState( const body : PNewtonBody) : Integer;                                                                              cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetSleepState'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonBodyGetAutoSleep( const body : PNewtonBody) : Integer;                                                                               cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetAutoSleep'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonBodySetAutoSleep( const body : PNewtonBody; state : integer );                                                                       cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetAutoSleep'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonBodyGetFreezeState( const body : PNewtonBody) : Integer;                                                                             cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetFreezeState'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonBodySetFreezeState( const body : PNewtonBody; state : integer );                                                                     cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetFreezeState'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonBodySetDestructorCallback( const body : PNewtonBody; callback : NewtonBodyDestructor );                                          cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetDestructorCallback'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonBodySetTransformCallback( const body : PNewtonBody; callback : NewtonSetTransform );                                             cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetTransformCallback'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function NewtonBodyGetTransformCallback( const body : PNewtonBody ): NewtonSetTransform;                                                         cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetForceAndTorqueCallback'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonBodySetForceAndTorqueCallback( const body : PNewtonBody; callback : NewtonApplyForceAndTorque );                                 cdecl; external{$IFDEF __GPC__}name 'NewtonBodySetForceAndTorqueCallback'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function NewtonBodyGetForceAndTorqueCallback( const body : PNewtonBody ): NewtonApplyForceAndTorque;                                             cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetForceAndTorqueCallback'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonBodyGetUserData( const body : PNewtonBody ) : Pointer;                                                                           cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetUserData'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonBodyGetWorld( const body : PNewtonBody) : PNewtonWorld;                                                                          cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetWorld'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonBodyGetCollision( const body : PNewtonBody ) : PNewtonCollision;                                                                 cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetCollision'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonBodyGetMaterialGroupID( const body : PNewtonBody ) : Integer;                                                                        cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetMaterialGroupID'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonBodyGetContinuousCollisionMode( const body : PNewtonBody ) : Integer;                                                                cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetContinuousCollisionMode'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonBodyGetJointRecursiveCollision( const body : PNewtonBody ) : Integer;                                                                cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetJointRecursiveCollision'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonBodyGetMatrix( const body : PNewtonBody; matrix : PdFloat );                                                                      cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetMatrix'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonBodyGetRotation( const body : PNewtonBody; rotation : PdFloat );                                                                  cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetRotation'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodyGetMassMatrix( const body : PNewtonBody; mass : PdFloat; Ixx : PdFloat; Iyy : PdFloat; Izz : PdFloat );                          cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetMassMatrix'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodyGetInvMass( const body : PNewtonBody; invMass : PdFloat; invIxx : PdFloat; invIyy : PdFloat; invIzz : PdFloat );                 cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetInvMass'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodyGetOmega( const body : PNewtonBody; vector : PdFloat );                                                                       cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetOmega'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodyGetVelocity( const body : PNewtonBody; vector : PdFloat );                                                                    cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetVelocity'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodyGetForce( const body : PNewtonBody; vector : PdFloat );                                                                       cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetForce'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodyGetTorque( const body : PNewtonBody; vector : PdFloat);                                                                       cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetTorque'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonBodyGetForceAcc( const body : PNewtonBody; vector : PdFloat );                                                                    cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetForceAcc'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonBodyGetTorqueAcc( const body : PNewtonBody; vector : PdFloat );                                                                   cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetTorqueAcc'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodyGetCentreOfMass(const body : PNewtonBody; com : PdFloat);                                                                     cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetCentreOfMass'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonBodyGetLinearDamping( const body : PNewtonBody ) : dFloat;                                                                        cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetLinearDamping'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodyGetAngularDamping( const body : PNewtonBody; vector : PdFloat );                                                              cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetAngularDamping'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBodyGetAABB( const body : PNewtonBody; p0 : PdFloat; p1 : PdFloat );                                                               cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetAABB'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonBodyGetFreezeTreshold( const body : PNewtonBody; freezeSpeed2 : PdFloat; freezeOmega2 : PdFloat );                                 cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetFreezeTreshold'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonBodyGetFirstJoint( const body : PNewtonBody ) : PNewtonJoint;                                                                    cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetFirstJoint'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonBodyGetNextJoint( const body : PNewtonBody; const joint : PNewtonJoint ) : PNewtonJoint;                                         cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetNextJoint'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonBodyGetFirstContactJoint( const body : PNewtonBody ) : PNewtonJoint;                                                             cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetFirstContactJoint'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonBodyGetNextContactJoint( const body : PNewtonBody; const contactJoint : PNewtonJoint ) : PNewtonJoint;                           cdecl; external{$IFDEF __GPC__}name 'NewtonBodyGetNextContactJoint'{$ELSE}NewtonDLL{$ENDIF __GPC__};

// **********************************************************************************************
//
// contact joints interface
//
// **********************************************************************************************

function  NewtonContactJointGetFirstContact( const contactJoint : PNewtonJoint ) : Pointer;                                                      cdecl; external NewtonDLL;
function  NewtonContactJointGetNextContact( const contactJoint : PNewtonJoint; contact : Pointer ) : Pointer;                                    cdecl; external NewtonDLL;

function  NewtonContactJointGetContactCount( const contactJoint : PNewtonJoint ) : integer;                                                          cdecl; external{$IFDEF __GPC__}name 'NewtonContactJointGetContactCount'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonContactJointRemoveContact( const contactJoint : PNewtonJoint; contact : Pointer );                                               cdecl; external{$IFDEF __GPC__}name 'NewtonContactJointRemoveContact'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function NewtonContactGetMaterial( const contact : Pointer ) : PNewtonMaterial;                                                                  cdecl; external NewtonDLL;

procedure NewtonBodyAddBuoyancyForce( const body : PNewtonBody; fluidDensity : dFloat; fluidLinearViscosity, fluidAngularViscosity : dFloat;
                                      const gravityVector : PdFloat; buoyancyPlane : NewtonGetBuoyancyPlane; context : Pointer );                 cdecl; external{$IFDEF __GPC__}name 'NewtonBodyAddBuoyancyForce'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonBodyAddImpulse(const body : PNewtonBody; const pointDeltaVeloc : PdFloat; const pointPosit : PdFloat );                            cdecl; external{$IFDEF __GPC__}name 'NewtonAddBodyImpulse'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonBodyApplyImpulseArray (const Body : PNewtonBody; ImpuleCount : Integer; StrideInByte : Integer; const impulseArray : PdFloat;
                                       const pointArray : PdFloat);                                                                               cdecl; external{$IFDEF __GPC__}name 'NewtonBodyApplyImpulseArray'{$ELSE}NewtonDLL{$ENDIF __GPC__};

// *****************************************************************************************************************************
//
// Common joint funtions
//
// *****************************************************************************************************************************
function  NewtonJointGetUserData( const joint : PNewtonJoint ) : Pointer;                                                                        cdecl; external{$IFDEF __GPC__}name 'NewtonJointGetUserData'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonJointSetUserData( const joint : PNewtonJoint; userData : Pointer );                                                              cdecl; external{$IFDEF __GPC__}name 'NewtonJointSetUserData'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonJointGetBody0( const joint : PNewtonJoint ) : PNewtonBody;                                                                       cdecl; external{$IFDEF __GPC__}name 'NewtonJointGetBody0'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonJointGetBody1( const joint : PNewtonJoint ) : PNewtonBody;                                                                       cdecl; external{$IFDEF __GPC__}name 'NewtonJointGetBody1'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonJointGetInfo( const joint : PNewtonJoint; info : PNewtonJointRecord );                                                           cdecl; external{$IFDEF __GPC__}name 'NewtonJointGetInfo'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonJointGetCollisionState( const joint : PNewtonJoint ) : integer;                                                                      cdecl; external{$IFDEF __GPC__}name 'NewtonJointGetCollisionState'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonJointSetCollisionState( const joint : PNewtonJoint; state : integer );                                                               cdecl; external{$IFDEF __GPC__}name 'NewtonJointSetCollisionState'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonJointGetStiffness( const joint : PNewtonJoint): dFloat;                                                                           cdecl; external{$IFDEF __GPC__}name 'NewtonJointGetStiffness'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonJointSetStiffness( const joint: PNewtonJoint; state: dFloat);                                                                     cdecl; external{$IFDEF __GPC__}name 'NewtonJointSetStiffness'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonDestroyJoint( const newtonWorld : PNewtonWorld; const joint : PNewtonJoint );                                                    cdecl; external{$IFDEF __GPC__}name 'NewtonDestroyJoint'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonJointSetDestructor( const joint : PNewtonJoint; _destructor : NewtonConstraintDestructor );                                      cdecl; external{$IFDEF __GPC__}name 'NewtonJointSetDestructor'{$ELSE}NewtonDLL{$ENDIF __GPC__};

// *****************************************************************************************************************************
//
// Ball and Socket joint functions
//
// *****************************************************************************************************************************
function  NewtonConstraintCreateBall( const newtonWorld : PNewtonWorld; const pivotPoint : PdFloat;
                                      const childBody : PNewtonBody; const parentBody : PNewtonBody ) : PNewtonJoint;                            cdecl; external{$IFDEF __GPC__}name 'NewtonConstraintCreateBall'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBallSetUserCallback( const ball : PNewtonJoint; callback : NewtonBallCallBack );                                                 cdecl; external{$IFDEF __GPC__}name 'NewtonBallSetUserCallback'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBallGetJointAngle( const ball : PNewtonJoint; angle : PdFloat );                                                                  cdecl; external{$IFDEF __GPC__}name 'NewtonBallGetJointAngle'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBallGetJointOmega( const ball : PNewtonJoint; omega : PdFloat );                                                                  cdecl; external{$IFDEF __GPC__}name 'NewtonBallGetJointOmega'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBallGetJointForce( const ball : PNewtonJoint; force : PdFloat );                                                                  cdecl; external{$IFDEF __GPC__}name 'NewtonBallGetJointForce'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonBallSetConeLimits( const ball : PNewtonJoint; const pin : PdFloat; maxConeAngle : dFloat; maxTwistAngle : dFloat );                 cdecl; external{$IFDEF __GPC__}name 'NewtonBallSetConeLimits'{$ELSE}NewtonDLL{$ENDIF __GPC__};

// *****************************************************************************************************************************
//
// Hinge joint functions
//
// *****************************************************************************************************************************
function  NewtonConstraintCreateHinge( const newtonWorld : PNewtonWorld;
                                       const pivotPoint : PdFloat; const pinDir : PdFloat;
                                       const childBody : PNewtonBody; const parentBody : PNewtonBody ) : PNewtonJoint;                           cdecl; external{$IFDEF __GPC__}name 'NewtonConstraintCreateHinge'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonHingeSetUserCallback( const hinge : PNewtonJoint; callback : NewtonHingeCallBack );                                              cdecl; external{$IFDEF __GPC__}name 'NewtonHingeSetUserCallback'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonHingeGetJointAngle( const hinge : PNewtonJoint ) : dFloat;                                                                        cdecl; external{$IFDEF __GPC__}name 'NewtonHingeGetJointAngle'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonHingeGetJointOmega( const hinge : PNewtonJoint ) : dFloat;                                                                        cdecl; external{$IFDEF __GPC__}name 'NewtonHingeGetJointOmega'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonHingeGetJointForce( const hinge : PNewtonJoint; force : PdFloat );                                                                cdecl; external{$IFDEF __GPC__}name 'NewtonHingeGetJointForce'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonHingeCalculateStopAlpha( const hinge : PNewtonJoint; const desc : PNewtonHingeSliderUpdateDesc; angle : dFloat): dFloat;         cdecl; external{$IFDEF __GPC__}name 'NewtonHingeCalculateStopAlpha'{$ELSE}NewtonDLL{$ENDIF __GPC__};

// *****************************************************************************************************************************
//
// Slider joint functions
//
// *****************************************************************************************************************************
function  NewtonConstraintCreateSlider( const newtonWorld : PNewtonWorld;
                                        const pivotPoint : PdFloat; const pinDir : PdFloat;
                                        const childBody : PNewtonBody; const parentBody : PNewtonBody ) : PNewtonJoint;                          cdecl; external{$IFDEF __GPC__}name 'NewtonConstraintCreateSlider'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonSliderSetUserCallback( const slider : PNewtonJoint; callback : NewtonSliderCallBack );                                           cdecl; external{$IFDEF __GPC__}name 'NewtonSliderSetUserCallback'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonSliderGetJointPosit( const slider : PNewtonJoint ) : dFloat;                                                                      cdecl; external{$IFDEF __GPC__}name 'NewtonSliderGetJointPosit'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonSliderGetJointVeloc( const slider : PNewtonJoint ) : dFloat;                                                                      cdecl; external{$IFDEF __GPC__}name 'NewtonSliderGetJointVeloc'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonSliderGetJointForce( const slider : PNewtonJoint; force : PdFloat );                                                              cdecl; external{$IFDEF __GPC__}name 'NewtonSliderGetJointForce'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonSliderCalculateStopAccel( const slider : PNewtonJoint; const desc : PNewtonHingeSliderUpdateDesc; position : dFloat ) : dFloat;    cdecl; external{$IFDEF __GPC__}name 'NewtonSliderCalculateStopAccel'{$ELSE}NewtonDLL{$ENDIF __GPC__};


// *****************************************************************************************************************************
//
// Corkscrew joint functions
//
// *****************************************************************************************************************************
function  NewtonConstraintCreateCorkscrew( const newtonWorld : PNewtonWorld;
                                           const pivotPoint : PdFloat; const pinDir : PdFloat;
                                           const childBody : PNewtonBody; const parentBody : PNewtonBody ) : PNewtonJoint;                       cdecl; external{$IFDEF __GPC__}name 'NewtonConstraintCreateCorkscrew'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonCorkscrewSetUserCallback( const corkscrew : PNewtonJoint; callback : NewtonCorkscrewCallBack );                                  cdecl; external{$IFDEF __GPC__}name 'NewtonCorkscrewSetUserCallback'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonCorkscrewGetJointPosit( const corkscrew : PNewtonJoint ) : dFloat;                                                                cdecl; external{$IFDEF __GPC__}name 'NewtonCorkscrewGetJointPosit'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonCorkscrewGetJointAngle( const corkscrew : PNewtonJoint ) : dFloat;                                                                cdecl; external{$IFDEF __GPC__}name 'NewtonCorkscrewGetJointAngle'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonCorkscrewGetJointVeloc( const corkscrew : PNewtonJoint ) : dFloat;                                                                cdecl; external{$IFDEF __GPC__}name 'NewtonCorkscrewGetJointVeloc'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonCorkscrewGetJointOmega( const corkscrew : PNewtonJoint ) : dFloat;                                                                cdecl; external{$IFDEF __GPC__}name 'NewtonCorkscrewGetJointOmega'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonCorkscrewGetJointForce( const corkscrew : PNewtonJoint; force : PdFloat );                                                        cdecl; external{$IFDEF __GPC__}name 'NewtonCorkscrewGetJointForce'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonCorkscrewCalculateStopAlpha(const corkscrew : PNewtonJoint;const desc : PNewtonHingeSliderUpdateDesc;angle : dFloat) : dFloat;     cdecl; external{$IFDEF __GPC__}name 'NewtonCorkscrewCalculateStopAlpha'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonCorkscrewCalculateStopAccel(const corkscrew : PNewtonJoint;const desc : PNewtonHingeSliderUpdateDesc;position : dFloat) : dFloat;  cdecl; external{$IFDEF __GPC__}name 'NewtonCorkscrewCalculateStopAccel'{$ELSE}NewtonDLL{$ENDIF __GPC__};


// *****************************************************************************************************************************
//
// Universal joint functions
//
// *****************************************************************************************************************************
function  NewtonConstraintCreateUniversal( const newtonWorld: PNewtonWorld; const pivotPoint: PdFloat; const pinDir0: PdFloat;
                                          const pinDir1: PdFloat; const childBody: PNewtonBody; const parentBody: PNewtonBody): PNewtonJoint;     cdecl; external{$IFDEF __GPC__}name 'NewtonConstraintCreateUniversal'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonUniversalSetUserCallback(const universal: PNewtonJoint; callback: NewtonUniversalCallback);                                      cdecl; external{$IFDEF __GPC__}name 'NewtonUniversalSetUserCallback'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonUniversalGetJointAngle0(const universal: PNewtonJoint):dFloat;                                                                    cdecl; external{$IFDEF __GPC__}name 'NewtonUniversalGetJointAngle0'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonUniversalGetJointAngle1(const universal: PNewtonJoint):dFloat;                                                                    cdecl; external{$IFDEF __GPC__}name 'NewtonUniversalGetJointAngle1'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonUniversalGetJointOmega0(const universal: PNewtonJoint):dFloat;                                                                    cdecl; external{$IFDEF __GPC__}name 'NewtonUniversalGetJointOmega0'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonUniversalGetJointOmega1(const universal: PNewtonJoint):dFloat;                                                                    cdecl; external{$IFDEF __GPC__}name 'NewtonUniversalGetJointOmega1'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonUniversalGetJointForce(const universal: PNewtonJoint; force: PdFloat);                                                            cdecl; external{$IFDEF __GPC__}name 'NewtonUniversalGetJointForce'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonUniversalCalculateStopAlpha0(const universal : PNewtonJoint; const desc: PNewtonHingeSliderUpdateDesc; angle: dFloat): dFloat;     cdecl; external{$IFDEF __GPC__}name 'NewtonUniversalCalculateStopAlpha0'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonUniversalCalculateStopAlpha1(const universal : PNewtonJoint; const desc: PNewtonHingeSliderUpdateDesc; angle: dFloat): dFloat;     cdecl; external{$IFDEF __GPC__}name 'NewtonUniversalCalculateStopAlpha1'{$ELSE}NewtonDLL{$ENDIF __GPC__};

// *****************************************************************************************************************************
//
// Up vector joint unctions
//
// *****************************************************************************************************************************
function  NewtonConstraintCreateUpVector( const newtonWorld : PNewtonWorld; const pinDir : PdFloat; const body : PNewtonBody ) : PNewtonJoint;    cdecl; external{$IFDEF __GPC__}name 'NewtonConstraintCreateUpVector'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonUpVectorGetPin( const upVector : PNewtonJoint; pin : PdFloat );                                                                   cdecl; external{$IFDEF __GPC__}name 'NewtonUpVectorGetPin'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonUpVectorSetPin( const upVector : PNewtonJoint; const pin : PdFloat );                                                             cdecl; external{$IFDEF __GPC__}name 'NewtonUpVectorSetPin'{$ELSE}NewtonDLL{$ENDIF __GPC__};

// *****************************************************************************************************************************
//
// User defined bilateral Joint
//
// *****************************************************************************************************************************
function  NewtonConstraintCreateUserJoint(const NewtonWorld : PNewtonWorld; MaxDOF : Integer; Callback : PNewtonUserBilateralCallBack;
                                          GetInfo : PNewtonUserBilateralGetInfoCallBack; const ChildBody: PNewtonBody;
                                          const parentBody: PNewtonBody): PNewtonJoint;                                                          cdecl; external{$IFDEF __GPC__}name 'NewtonConstraintCreateUserJoint'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonUserJointSetFeedbackCollectorCallback(const Joint : PNewtonJoint; GetFeedback : PNewtonUserBilateralCallBack);                   cdecl; external{$IFDEF __GPC__}name 'NewtonUserJointSetFeedbackCollectorCallback'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonUserJointAddLinearRow(const Joint : PNewtonJoint; const pivot0 : PdFloat; const pivot1 : PdFloat; const Dir : PdFloat);             cdecl; external{$IFDEF __GPC__}name 'NewtonUserJointAddLinearRow'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonUserJointAddAngularRow(const Joint : PNewtonJoint; RelativeAngle : dFloat; const Dir : PdFloat);                                   cdecl; external{$IFDEF __GPC__}name 'NewtonUserJointAddAngularRow'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonUserJointAddGeneralRow(const Joint : PNewtonJoint; const Jacobian0 : PdFloat; const Jacobian1 : PdFloat);                          cdecl; external{$IFDEF __GPC__}name 'NewtonUserJointAddGeneralRow'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonUserJointSetRowMinimumFriction(const Joint : PNewtonJoint; Friction : dFloat);                                                    cdecl; external{$IFDEF __GPC__}name 'NewtonUserJointSetRowMinimumFriction'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonUserJointSetRowMaximumFriction(const Joint : PNewtonJoint; Friction : dFloat);                                                    cdecl; external{$IFDEF __GPC__}name 'NewtonUserJointSetRowMaximumFriction'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonUserJointSetRowAcceleration(const Joint : PNewtonJoint; Acceleration : dFloat);                                                   cdecl; external{$IFDEF __GPC__}name 'NewtonUserJointSetRowAcceleration'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonUserJointSetRowSpringDamperAcceleration(const joint : PNewtonJoint; springK : dFloat; springD : dFloat);                           cdecl; external{$IFDEF __GPC__}name 'NewtonUserJointSetRowSpringDamperAcceleration'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonUserJointSetRowStiffness(const Joint : PNewtonJoint; Stiffness : dFloat);                                                         cdecl; external{$IFDEF __GPC__}name 'NewtonUserJointSetRowStiffness'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonUserJointGetRowForce (const Joint : PNewtonJoint; Row : Integer) : dFloat;                                                            cdecl; external{$IFDEF __GPC__}name 'NewtonUserJointGetRowForce'{$ELSE}NewtonDLL{$ENDIF __GPC__};

// **********************************************************************************************
//
// Mesh joint functions
//
// **********************************************************************************************

function  NewtonMeshCreate (const World : PNewtonWorld) : PNewtonMesh;                                                                           cdecl; external{$IFDEF __GPC__}name 'NewtonMeshCreate'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshCreateFromMesh(const Mesh : PNewtonMesh) : PNewtonMesh;                                                                      cdecl; external{$IFDEF __GPC__}name 'NewtonMeshCreateFromMesh'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshCreateFromCollision (const collision : PNewtonCollision) : PNewtonMesh;                                                      cdecl; external{$IFDEF __GPC__}name 'NewtonMeshCreateFromCollision'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshConvexHull (const NewtonWorld : PNewtonWorld; count : integer; const vertexCloud : PdFloat; strideInBytes : integer;
                                tolerance : dFloat) : PNewtonMesh;                                                                                cdecl; external{$IFDEF __GPC__}name 'NewtonMeshConvexHull'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshCreatePlane (const World : PNewtonWorld; const locationMatrix : PdFloat; width : dFloat; breadth : dFloat; material : Integer;
                                 const textureMatrix0 : PdFloat; const textureMatrix1) : PNewtonMesh;                                             cdecl; external{$IFDEF __GPC__}name 'NewtonMeshCreatePlane'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonMeshDestroy(const mesh : PNewtonMesh);                                                                                           cdecl; external{$IFDEF __GPC__}name 'NewtonMeshDestroy'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonMeshCalculateOOBB(const mesh : PNewtonMesh; const matrix : PdFloat; x : PdFloat; y : PdFloat; z : PdFloat);                          cdecl; external{$IFDEF __GPC__}name 'NewtonMeshCalculateOOBB'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonMesApplyTransform (const Mesh : PNewtonMesh; const Matrix : PdFloat);                                                             cdecl; external{$IFDEF __GPC__}name 'NewtonMesApplyTransform'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonMeshCalculateVertexNormals(const mesh : PNewtonMesh; angleInRadians : dFloat);                                                    cdecl; external{$IFDEF __GPC__}name 'NewtonMeshCalculateVertexNormals'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonMeshApplySphericalMapping(const mesh : PNewtonMesh; material : integer);                                                             cdecl; external{$IFDEF __GPC__}name 'NewtonMeshApplySphericalMapping'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonMeshApplyBoxMapping(const mesh : PNewtonMesh; front,side,top : integer);                                                             cdecl; external{$IFDEF __GPC__}name 'NewtonMeshApplyBoxMapping'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonMeshApplyCylindricalMapping(const mesh : PNewtonMesh; cylinderMaterial,capMaterial : integer);                                       cdecl; external{$IFDEF __GPC__}name 'NewtonMeshApplyCylindricalMapping'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonMeshIsOpenMesh (const mesh : PNewtonMesh) : Integer;                                                                                 cdecl; external{$IFDEF __GPC__}name 'NewtonMeshIsOpenMesh'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonMeshFixTJoints (const mesh : PNewtonMesh);                                                                                       cdecl; external{$IFDEF __GPC__}name 'NewtonMeshFixTJoints'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonMeshPolygonize (const mesh : PNewtonMesh);                                                                                       cdecl; external{$IFDEF __GPC__}name 'NewtonMeshPolygonize'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonMeshTriangulate (const mesh : PNewtonMesh);                                                                                      cdecl; external{$IFDEF __GPC__}name 'NewtonMeshTriangulate'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshUnion (const mesh : PNewtonMesh; clipper : PNewtonMesh; clipperMatrix : PdFloat) : PNewtonMesh;                               cdecl; external{$IFDEF __GPC__}name 'NewtonMeshUnion'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshDifference (const mesh : PNewtonMesh; clipper : PNewtonMesh; clipperMatrix : PdFloat) : PNewtonMesh;                          cdecl; external{$IFDEF __GPC__}name 'NewtonMeshDifference'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshIntersection (const mesh : PNewtonMesh; clipper : PNewtonMesh; clipperMatrix : PdFloat) : PNewtonMesh;                        cdecl; external{$IFDEF __GPC__}name 'NewtonMeshIntersection'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonMeshClip (const mesh : PNewtonMesh; const clipper : PNewtonMesh; const clippermatrix : PdFloat; const topMesh : PNewtonMesh;
                          const bottomMesh : PNewtonMesh);                                                                                       cdecl; external{$IFDEF __GPC__}name 'NewtonMeshClip'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonMeshPlaneClip (const Mesh : PNewtonMesh; const planeMatrix : PdFloat; const PlaneTextureMatrix : PdFloat; PlaneMaterial : Integer;
                               const topMesh : PNewtonMesh; const bottomMesh : PNewtonMesh);                                                     cdecl; external{$IFDEF __GPC__}name 'NewtonMeshPlaneClip'{$ELSE}NewtonDLL{$ENDIF __GPC__};


function  NewtonMeshConvexDecomposition (const Mesh : PNewtonMesh; MaxCount : Integer) : PNewtonMesh;                                                cdecl; external{$IFDEF __GPC__}name 'NewtonMeshConvexDecomposition'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshVoronoiDecomposition (const Mesh : PNewtonMesh; PointCount, PointStrideInBytes : Integer; const PointCloud : PdFloat;
                                          InternalMaterial : Integer; const TextureMatrix : PdFloat) : PNewtonMesh;                                   cdecl; external{$IFDEF __GPC__}name 'NewtonMeshVoronoiDecomposition'{$ELSE}NewtonDLL{$ENDIF __GPC__};


procedure NewtonRemoveUnusedVertices(const mesh : PNewtonMesh; vertexRemapTable : PInteger);                                                         cdecl; external{$IFDEF __GPC__}name 'NewtonRemoveUnusedVertices'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonMeshBeginFace(const mesh : PNewtonMesh);                                                                                         cdecl; external{$IFDEF __GPC__}name 'NewtonMeshBeginFace'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonMeshAddFace(const mesh : PNewtonMesh; vertexCount : integer; const vertex : PdFloat; strideInBytes,materialIndex : integer );             cdecl; external{$IFDEF __GPC__}name 'NewtonMeshAddFace'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonMeshEndFace(const mesh : PNewtonMesh);                                                                                           cdecl; external{$IFDEF __GPC__}name 'NewtonMeshEndFace'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonMeshBuildFromVertexListIndexList(const Mesh : PNewtonMesh;
		FaceCount : Integer; const faceIndexCount : PInteger; const faceMaterialIndex : PInteger;
		const vertex : PdFloat; vertexStrideInBytes : Integer; const vertexIndex : PInteger;
		const normal : PdFloat; normalStrideInBytes : Integer; const normalIndex : PInteger;
		const uv0 : PdFloat; uv0StrideInBytes : Integer; const uv0Index : PInteger;
		const uv1 : PdFloat; uv1StrideInBytes : Integer; const uv1Index : PInteger);                                                                          cdecl; external{$IFDEF __GPC__}name 'NewtonMeshBuildFromVertexListIndexList'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonMeshGetVertexStreams(const Mesh: PNewtonMesh; vertexStrideInByte: Integer; Vertex: PdFloat; normalStrideInByte : integer;
                                     Normal: PdFloat; uvStrideInByte0 : Integer; uv0: PdFloat; uvStrideInByte1 : Integer; uv1 : PdFloat);                 cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetVertexStreams'{$ELSE}NewtonDLL{$ENDIF __GPC__};

procedure NewtonMeshGetIndirectVertexStreams(const mesh : PNewtonMesh; vertexStrideInByte : integer; vertex : PdFloat; vertexIndices : PInteger;
                                             vertexCount : PInteger; normalStrideInByte : integer; normal: PdFloat; normalIndices : PInteger;
                                             normalCount : PInteger; uvStrideInByte0 : integer; uv0: PdFloat; uvIndices0 : PInteger; uvCount0 : PInteger;
                                             uvStrideInByte1 : integer; uv1: PdFloat; uvIndices1 : PInteger; uvCount1 : PInteger);                           cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetIndirectVertexStreams'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonMeshBeginHandle (const mesh : PNewtonMesh) : Pointer;                                                                            cdecl; external{$IFDEF __GPC__}name 'NewtonMeshBeginHandle'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonMeshEndHandle (const mesh : PNewtonMesh; Handle : Pointer);                                                                      cdecl; external{$IFDEF __GPC__}name 'NewtonMeshEndHandle'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonMeshFirstMaterial (const mesh : PNewtonMesh; Handle : Pointer) : integer;                                                            cdecl; external{$IFDEF __GPC__}name 'NewtonMeshFirstMaterial'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshNextMaterial (const mesh : PNewtonMesh; Handle : Pointer; materialID : integer) : integer;                                           cdecl; external{$IFDEF __GPC__}name 'NewtonMeshNextMaterial'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshMaterialGetMaterial (const mesh : PNewtonMesh; Handle : Pointer; materialID : integer) : integer;                                    cdecl; external{$IFDEF __GPC__}name 'NewtonMeshMaterialGetMaterial'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshMaterialGetIndexCount (const mesh : PNewtonMesh; Handle : Pointer; materialID : integer) : integer;                                  cdecl; external{$IFDEF __GPC__}name 'NewtonMeshMaterialGetIndexCount'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonMeshMaterialGetIndexStream(const mesh : PNewtonMesh; Handle : Pointer; materialID : integer; index : PInteger);                          cdecl; external{$IFDEF __GPC__}name 'NewtonMeshMaterialGetIndexStream'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonMeshMaterialGetIndexStreamShort(const mesh : PNewtonMesh; Handle : Pointer; materialID : integer; index : PShort);                   cdecl; external{$IFDEF __GPC__}name 'NewtonMeshMaterialGetIndexStreamShort'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonMeshCreateFirstSingleSegment (const mesh : PNewtonMesh) : PNewtonMesh;                                                           cdecl; external{$IFDEF __GPC__}name 'NewtonMeshCreateFirstSingleSegment'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshCreateNextSingleSegment (const mesh : PNewtonMesh; Segment : PNewtonMesh) : PNewtonMesh;                                     cdecl; external{$IFDEF __GPC__}name 'NewtonMeshCreateNextSingleSegment'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonMeshCreateFirstLayer(const Mesh : PNewtonMesh) : PNewtonMesh;                                                                    cdecl; external{$IFDEF __GPC__}name 'NewtonMeshCreateFirstLayer'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshCreateNextLayer(const Mesh : PNewtonMesh; const Segment : PNewtonMesh) : PNewtonMesh;                                        cdecl; external{$IFDEF __GPC__}name 'NewtonMeshCreateNextLayer'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonMeshGetTotalFaceCount (const mesh : PNewtonMesh) : Integer;                                                                          cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetTotalFaceCount'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshGetTotalIndexCount (const mesh : PNewtonMesh) : Integer;                                                                         cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetTotalIndexCount'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonMeshGetFaces (const mesh : PNewtonMesh; const faceIndexCount : PInteger; faceMaterial : PInteger; faceIndices : PInteger);                   cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetFaces'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonMeshGetPointCount (const Mesh : PNewtonMesh) : Integer;                                                                              cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetPointCount'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshGetPointStrideInByte (const Mesh : PNewtonMesh) : Integer;                                                                       cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetPointStrideInByte'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshGetPointArray (const Mesh : PNewtonMesh): PdFloat;                                                                            cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetPointArray'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonMeshGetNormalArray (const Mesh : PNewtonMesh) : PdFloat;                                                                          cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetNormalArray'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshGetUV0Array (const Mesh : PNewtonMesh) : PdFloat;                                                                             cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetUV0Array'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshGetUV1Array (const Mesh : PNewtonMesh) : PdFloat;                                                                             cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetUV1Array'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonMeshGetVertexCount (const Mesh : PNewtonMesh) : Integer;                                                                             cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetVertexCount'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshGetVertexStrideInByte (const Mesh : PNewtonMesh) : Integer;                                                                      cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetVertexStrideInByte'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshGetVertexArray (const Mesh : PNewtonMesh) : PdFloat;                                                                          cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetVertexArray'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonMeshGetFirstVertex (const Mesh : PNewtonMesh) : Pointer;                                                                         cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetFirstVertex'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshGetNextVertex (const Mesh : PNewtonMesh; const Vertex : Pointer) : Pointer;                                                  cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetNextVertex'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshGetVertexIndex (const Mesh : PNewtonMesh; const Vertex : Pointer) : Integer;                                                     cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetVertexIndex'{$ELSE}NewtonDLL{$ENDIF __GPC__};
//NEWTON_API int NewtonMeshGetVertexPointIndex (const NewtonMesh *mesh, const void* vertex);

function  NewtonMeshGetFirstPoint (const Mesh : PNewtonMesh) : Pointer;                                                                          cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetFirstPoint'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshGetNextPoint (const Mesh : PNewtonMesh; const point : Pointer) : Pointer;                                                    cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetNextPoint'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshGetPointIndex (const Mesh : PNewtonMesh; const point : Pointer) : Integer;                                                       cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetPointIndex'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshGetVertexIndexFromPoint (const Mesh : PNewtonMesh; const point : Pointer) : Integer;                                             cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetVertexIndexFromPoint'{$ELSE}NewtonDLL{$ENDIF __GPC__};

function  NewtonMeshGetFirstEdge (const Mesh : PNewtonMesh) : Pointer;                                                                           cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetFirstEdge'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshGetNextEdge (const Mesh : PNewtonMesh; const Edge : Pointer) : Pointer;                                                      cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetNextEdge'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonMeshGetEdgeIndices (const Mesh : PNewtonMesh; const Edge : Pointer; v0 : PInteger; v1 : PInteger);                                       cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetEdgeIndices'{$ELSE}NewtonDLL{$ENDIF __GPC__};
//NEWTON_API void NewtonMeshGetEdgePointIndices (const Mesh : PNewtonMesh, const void* edge, int* v0, int* v1);

function  NewtonMeshGetFirstFace (const Mesh : PNewtonMesh) : Pointer;                                                                           cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetFirstFace'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshGetNextFace (const Mesh : PNewtonMesh; const Face : Pointer) : Pointer;                                                      cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetNextFace'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshIsFaceOpen (const Mesh : PNewtonMesh; const Face : Pointer) : Integer;                                                           cdecl; external{$IFDEF __GPC__}name 'NewtonMeshIsFaceOpen'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshGetFaceMaterial (const Mesh : PNewtonMesh; const Face : Pointer) : Integer;                                                      cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetFaceMaterial'{$ELSE}NewtonDLL{$ENDIF __GPC__};
function  NewtonMeshGetFaceIndexCount (const Mesh : PNewtonMesh; const Face : Pointer) : Integer;                                                    cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetFaceIndexCount'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonMeshGetFaceIndices (const Mesh : PNewtonMesh; const Face : Pointer; Indices : PInteger);                                             cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetFaceIndices'{$ELSE}NewtonDLL{$ENDIF __GPC__};
procedure NewtonMeshGetFacePointIndices (const Mesh : PNewtonMesh; const Face : Pointer; Indices : PInteger);                                        cdecl; external{$IFDEF __GPC__}name 'NewtonMeshGetFacePointIndices'{$ELSE}NewtonDLL{$ENDIF __GPC__};


implementation

end.
