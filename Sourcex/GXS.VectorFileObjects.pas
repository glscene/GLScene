//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.VectorFileObjects;

(*
  Vector File related objects for GLScene
  The history is logged in a former GLS version of the unit.
*)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.Math,
  System.SysUtils,
  System.Types,

  GXS.XOpenGL,
  GXS.BaseClasses,
  GXS.VectorLists,
  GXS.PersistentClasses,
  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.Strings,
  GXS.GeometryBB,
  GXS.ApplicationFileIO,

  GXS.Scene,
  GXS.Texture,
  GXS.Material,
  GXS.Mesh,
  GXS.Octree,
  GXS.Silhouette,
  GXS.Context,
  GXS.Color,
  GXS.RenderContextInfo,
  GXS.Coordinates,
  GXS.TextureFormat,
  GXS.State,
  GXS.Utils;

type

  TgxMeshObjectList = class;
  TgxFaceGroups = class;

  TgxMeshAutoCentering = (macCenterX, macCenterY, macCenterZ, macUseBarycenter, macRestorePosition);
  TgxMeshAutoCenterings = set of TgxMeshAutoCentering;

  TgxMeshObjectMode = (momTriangles, momTriangleStrip, momFaceGroups);

  (* A base class for mesh objects.
    The class introduces a set of vertices and normals for the object but
    does no rendering of its own. *)
  TgxBaseMeshObject = class(TgxPersistentObject)
  private
    FName: string;
    FVertices: TgxAffineVectorList;
    FNormals: TgxAffineVectorList;
    FVisible: Boolean;
  protected
    procedure SetVertices(const val: TgxAffineVectorList);
    procedure SetNormals(const val: TgxAffineVectorList);
    procedure ContributeToBarycenter(var currentSum: TAffineVector; var nb: Integer); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    // Clears all mesh object data, submeshes, facegroups, etc.
    procedure Clear; virtual;
    // Translates all the vertices by the given delta.
    procedure Translate(const delta: TAffineVector); virtual;
    (* Builds (smoothed) normals for the vertex list.
      If normalIndices is nil, the method assumes a bijection between
      vertices and normals sets, and when performed, Normals and Vertices
      list will have the same number of items (whatever previously was in
      the Normals list is ignored/removed).
      If normalIndices is defined, normals will be added to the list and
      their indices will be added to normalIndices. Already defined
      normals and indices are preserved.
      The only valid modes are currently momTriangles and momTriangleStrip
      (ie. momFaceGroups not supported). *)
    procedure BuildNormals(vertexIndices: TgxIntegerList; mode: TgxMeshObjectMode; normalIndices: TgxIntegerList = nil);
    (* Extracts all mesh triangles as a triangles list.
      The resulting list size is a multiple of 3, each group of 3 vertices
      making up and independant triangle.
      The returned list can be used independantly from the mesh object
      (all data is duplicated) and should be freed by caller.
      If texCoords is specified, per vertex texture coordinates will be
      placed there, when available. *)
    function ExtractTriangles(texCoords: TgxAffineVectorList = nil; normals: TgxAffineVectorList = nil): TgxAffineVectorList; virtual;
    property Name: string read FName write FName;
    property Visible: Boolean read FVisible write FVisible;
    property Vertices: TgxAffineVectorList read FVertices write SetVertices;
    property normals: TgxAffineVectorList read FNormals write SetNormals;
  end;

  TgxSkeletonFrameList = class;
  TgxSkeletonFrameTransform = (sftRotation, sftQuaternion);

  (* Stores position and rotation for skeleton joints.
    If you directly alter some values, make sure to call FlushLocalMatrixList
    so that the local matrices will be recalculated (the call to Flush does
    not recalculate the matrices, but marks the current ones as dirty). *)
  TgxSkeletonFrame = class(TgxPersistentObject)
  private
    FOwner: TgxSkeletonFrameList;
    FName: string;
    FPosition: TgxAffineVectorList;
    FRotation: TgxAffineVectorList;
    FQuaternion: TQuaternionList;
    FLocalMatrixList: PMatrixArray;
    FTransformMode: TgxSkeletonFrameTransform;
  protected
    procedure SetPosition(const val: TgxAffineVectorList);
    procedure SetRotation(const val: TgxAffineVectorList);
    procedure SetQuaternion(const val: TQuaternionList);
  public
    constructor CreateOwned(aOwner: TgxSkeletonFrameList);
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    property Owner: TgxSkeletonFrameList read FOwner;
    property Name: string read FName write FName;
    // Position values for the joints.
    property Position: TgxAffineVectorList read FPosition write SetPosition;
    // Rotation values for the joints.
    property Rotation: TgxAffineVectorList read FRotation write SetRotation;
    (* Quaternions are an alternative to Euler rotations to build the
      global matrices for the skeleton bones. *)
    property Quaternion: TQuaternionList read FQuaternion write SetQuaternion;
    (* TransformMode indicates whether to use Rotation or Quaternion to build
      the local transform matrices. *)
    property TransformMode: TgxSkeletonFrameTransform read FTransformMode write FTransformMode;
    (* Calculate or retrieves an array of local bone matrices.
      This array is calculated on the first call after creation, and the
      first call following a FlushLocalMatrixList. Subsequent calls return
      the same arrays. *)
    function LocalMatrixList: PMatrixArray;
    (* Flushes (frees) then LocalMatrixList data.
      Call this function to allow a recalculation of local matrices. *)
    procedure FlushLocalMatrixList;
    // As the name states; Convert Quaternions to Rotations or vice-versa.
    procedure ConvertQuaternionsToRotations(KeepQuaternions: Boolean = True);
    procedure ConvertRotationsToQuaternions(KeepRotations: Boolean = True);
  end;

  // A list of TgxSkeletonFrame objects.
  TgxSkeletonFrameList = class(TgxPersistentObjectList)
  private
    FOwner: TPersistent;
  protected
    function GetSkeletonFrame(Index: Integer): TgxSkeletonFrame;
  public
    constructor CreateOwned(aOwner: TPersistent);
    destructor Destroy; override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    // As the name states; Convert Quaternions to Rotations or vice-versa.
    procedure ConvertQuaternionsToRotations(KeepQuaternions: Boolean = True; SetTransformMode: Boolean = True);
    procedure ConvertRotationsToQuaternions(KeepRotations: Boolean = True; SetTransformMode: Boolean = True);
    property Owner: TPersistent read FOwner;
    procedure Clear; override;
    property Items[Index: Integer]: TgxSkeletonFrame read GetSkeletonFrame; default;
  end;

  TgxSkeleton = class;
  TgxSkeletonBone = class;

  // A list of skeleton bones.
  TgxSkeletonBoneList = class(TgxPersistentObjectList)
  private
    FSkeleton: TgxSkeleton; // not persistent
  protected
    FGlobalMatrix: TMatrix4f;
    function GetSkeletonBone(Index: Integer): TgxSkeletonBone;
    procedure AfterObjectCreatedByReader(Sender: TObject); override;
  public
    constructor CreateOwned(aOwner: TgxSkeleton);
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    property Skeleton: TgxSkeleton read FSkeleton;
    property Items[Index: Integer]: TgxSkeletonBone read GetSkeletonBone; default;
    // Returns a bone by its BoneID, nil if not found.
    function BoneByID(anID: Integer): TgxSkeletonBone; virtual;
    // Returns a bone by its Name, nil if not found.
    function BoneByName(const aName: string): TgxSkeletonBone; virtual;
    // Number of bones (including all children and self).
    function BoneCount: Integer;
    // Render skeleton wireframe
    procedure BuildList(var mrci: TgxRenderContextInfo); virtual; abstract;
    procedure PrepareGlobalMatrices; virtual;
  end;

  // This list store skeleton root bones exclusively.
  TgxSkeletonRootBoneList = class(TgxSkeletonBoneList)
  public
    procedure WriteToFiler(writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    // Render skeleton wireframe
    procedure BuildList(var mrci: TgxRenderContextInfo); override;
    property GlobalMatrix: TMatrix4f read FGlobalMatrix write FGlobalMatrix;
  end;

  (* A skeleton bone or node and its children.
    This class is the base item of the bones hierarchy in a skeletal model.
    The joint values are stored in a TgxSkeletonFrame, but the calculated bone
    matrices are stored here. *)
  TgxSkeletonBone = class(TgxSkeletonBoneList)
  private
    FOwner: TgxSkeletonBoneList; // indirectly persistent
    FBoneID: Integer;
    FName: string;
    FColor: Cardinal;
  protected
    function GetSkeletonBone(Index: Integer): TgxSkeletonBone;
    procedure SetColor(const val: Cardinal);
  public
    constructor CreateOwned(aOwner: TgxSkeletonBoneList);
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    // Render skeleton wireframe
    procedure BuildList(var mrci: TgxRenderContextInfo); override;
    property Owner: TgxSkeletonBoneList read FOwner;
    property Name: string read FName write FName;
    property BoneID: Integer read FBoneID write FBoneID;
    property Color: Cardinal read FColor write SetColor;
    property Items[Index: Integer]: TgxSkeletonBone read GetSkeletonBone; default;
    // Returns a bone by its BoneID, nil if not found.
    function BoneByID(anID: Integer): TgxSkeletonBone; override;
    function BoneByName(const aName: string): TgxSkeletonBone; override;
    // Set the bone's matrix. Becareful using this.
    procedure SetGlobalMatrix(Matrix: TMatrix4f); // Ragdoll
    // Set the bone's GlobalMatrix. Used for Ragdoll.
    procedure SetGlobalMatrixForRagDoll(RagDollMatrix: TMatrix4f); // Ragdoll
    (* Calculates the global matrix for the bone and its sub-bone.
      Call this function directly only the RootBone. *)
    procedure PrepareGlobalMatrices; override;
    (* Global Matrix for the bone in the current frame.
      Global matrices must be prepared by invoking PrepareGlobalMatrices
      on the root bone. *)
    property GlobalMatrix: TMatrix4f read FGlobalMatrix;
    // Free all sub bones and reset BoneID and Name.
    procedure Clean; override;
  end;

  TgxSkeletonColliderList = class;

  (* A general class storing the base level info required for skeleton
    based collision methods. This class is meant to be inherited from
    to create skeleton driven Verlet Constraints, ODE Geoms, etc.
    Overriden classes should be named as TSCxxxxx. *)
  TgxSkeletonCollider = class(TgxPersistentObject)
  private
    FOwner: TgxSkeletonColliderList;
    FBone: TgxSkeletonBone;
    FBoneID: Integer;
    FLocalMatrix, FGlobalMatrix: TMatrix4f;
    FAutoUpdate: Boolean;
  protected
    procedure SetBone(const val: TgxSkeletonBone);
    procedure SetLocalMatrix(const val: TMatrix4f);
  public
    constructor Create; override;
    constructor CreateOwned(aOwner: TgxSkeletonColliderList);
    procedure WriteToFiler(writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    (* This method is used to align the colliders and their
      derived objects to their associated skeleton bone.
      Override to set up descendant class alignment properties. *)
    procedure AlignCollider; virtual;
    property Owner: TgxSkeletonColliderList read FOwner;
    // The bone that this collider associates with.
    property Bone: TgxSkeletonBone read FBone write SetBone;
    (* Offset and orientation of the collider in the associated
      bone's space. *)
    property LocalMatrix: TMatrix4f read FLocalMatrix write SetLocalMatrix;
    (* Global offset and orientation of the collider. This
      gets set in the AlignCollider method. *)
    property GlobalMatrix: TMatrix4f read FGlobalMatrix;
    property AutoUpdate: Boolean read FAutoUpdate write FAutoUpdate;
  end;

  // List class for storing TgxSkeletonCollider objects.
  TgxSkeletonColliderList = class(TgxPersistentObjectList)
  private
    FOwner: TPersistent;
  protected
    function GetSkeletonCollider(Index: Integer): TgxSkeletonCollider;
  public
    constructor CreateOwned(aOwner: TPersistent);
    destructor Destroy; override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    procedure Clear; override;
    // Calls AlignCollider for each collider in the list.
    procedure AlignColliders;
    property Owner: TPersistent read FOwner;
    property Items[Index: Integer]: TgxSkeletonCollider read GetSkeletonCollider; default;
  end;

  TgxBaseMesh = class;

  // Small structure to store a weighted lerp for use in blending.
  TgxBlendedLerpInfo = record
    frameIndex1, frameIndex2: Integer;
    lerpFactor: Single;
    weight: Single;
    externalPositions: TgxAffineVectorList;
    externalRotations: TgxAffineVectorList;
    externalQuaternions: TQuaternionList;
  end;

  (* Main skeleton object.
    This class stores the bones hierarchy and animation frames.
    It is also responsible for maintaining the "CurrentFrame" and allowing
    various frame blending operations. *)
  TgxSkeleton = class(TgxPersistentObject)
  private
    FOwner: TgxBaseMesh;
    FRootBones: TgxSkeletonRootBoneList;
    FFrames: TgxSkeletonFrameList;
    FCurrentFrame: TgxSkeletonFrame; // not persistent
    FBonesByIDCache: TList;
    FColliders: TgxSkeletonColliderList;
    FRagDollEnabled: Boolean; // ragdoll
    FMorphInvisibleParts: Boolean;
  protected
    procedure SetRootBones(const val: TgxSkeletonRootBoneList);
    procedure SetFrames(const val: TgxSkeletonFrameList);
    function GetCurrentFrame: TgxSkeletonFrame;
    procedure SetCurrentFrame(val: TgxSkeletonFrame);
    procedure SetColliders(const val: TgxSkeletonColliderList);
  public
    constructor CreateOwned(aOwner: TgxBaseMesh);
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    property Owner: TgxBaseMesh read FOwner;
    property RootBones: TgxSkeletonRootBoneList read FRootBones write SetRootBones;
    property Frames: TgxSkeletonFrameList read FFrames write SetFrames;
    property CurrentFrame: TgxSkeletonFrame read GetCurrentFrame write SetCurrentFrame;
    property Colliders: TgxSkeletonColliderList read FColliders write SetColliders;
    procedure FlushBoneByIDCache;
    function BoneByID(anID: Integer): TgxSkeletonBone;
    function BoneByName(const aName: string): TgxSkeletonBone;
    function BoneCount: Integer;
    procedure MorphTo(frameIndex: Integer); overload;
    procedure MorphTo(frame: TgxSkeletonFrame); overload;
    procedure Lerp(frameIndex1, frameIndex2: Integer; lerpFactor: Single);
    procedure BlendedLerps(const lerpInfos: array of TgxBlendedLerpInfo);
    (* Linearly removes the translation component between skeletal frames.
      This function will compute the translation of the first bone (index 0)
      and linearly subtract this translation in all frames between startFrame
      and endFrame. Its purpose is essentially to remove the 'slide' that
      exists in some animation formats (f.i. SMD). *)
    procedure MakeSkeletalTranslationStatic(startFrame, endFrame: Integer);
    (* Removes the absolute rotation component of the skeletal frames.
      Some formats will store frames with absolute rotation information,
      if this correct if the animation is the "main" animation.
      This function removes that absolute information, making the animation
      frames suitable for blending purposes. *)
    procedure MakeSkeletalRotationDelta(startFrame, endFrame: Integer);
    // Applies current frame to morph all mesh objects.
    procedure MorphMesh(normalize: Boolean);
    // Copy bone rotations from reference skeleton.
    procedure Synchronize(reference: TgxSkeleton);
    // Release bones and frames info.
    procedure Clear;
    // Backup and prepare the BoneMatrixInvertedMeshes to use with ragdolls
    procedure StartRagdoll; // ragdoll
    // Restore the BoneMatrixInvertedMeshes to stop the ragdoll
    procedure StopRagdoll; // ragdoll
    (* Turning this option off (by default) alows to increase FPS,
      but may break backwards-compatibility, because some may choose to
      attach other objects to invisible parts. *)
    property MorphInvisibleParts: Boolean read FMorphInvisibleParts write FMorphInvisibleParts;
  end;

  (* Rendering options per TgxMeshObject.
    moroGroupByMaterial : if set, the facegroups will be rendered by material
    in batchs, this will optimize rendering by reducing material switches, but
    also implies that facegroups will not be rendered in the order they are in
    the list. *)
  TgxMeshObjectRenderingOption = (moroGroupByMaterial);
  TgxMeshObjectRenderingOptions = set of TgxMeshObjectRenderingOption;

  TVBOBuffer = (vbVertices, vbNormals, vbColors, vbTexCoords, vbLightMapTexCoords, vbTexCoordsEx);
  TVBOBuffers = set of TVBOBuffer;

  (* Base mesh class.
    Introduces base methods and properties for mesh objects.
    Subclasses are named "TMOxxx". *)
  TgxMeshObject = class(TgxBaseMeshObject)
  private
    FOwner: TgxMeshObjectList;
    FExtentCacheRevision: Cardinal;
    FTexCoords: TgxAffineVectorList; // provision for 3D textures
    FLightMapTexCoords: TgxAffineVectorList; // reserved for 2D surface needs
    FColors: TgxVectorList;
    FFaceGroups: TgxFaceGroups;
    FMode: TgxMeshObjectMode;
    FRenderingOptions: TgxMeshObjectRenderingOptions;
    FArraysDeclared: Boolean; // not persistent
    FLightMapArrayEnabled: Boolean; // not persistent
    FLastLightMapIndex: Integer; // not persistent
    FTexCoordsEx: TList;
    FBinormalsTexCoordIndex: Integer;
    FTangentsTexCoordIndex: Integer;
    FLastXOpenGLTexMapping: Cardinal;
    FUseVBO: Boolean;
    FVerticesVBO: TgxVBOHandle;
    FNormalsVBO: TgxVBOHandle;
    FColorsVBO: TgxVBOHandle;
    FTexCoordsVBO: array of TgxVBOHandle;
    FLightmapTexCoordsVBO: TgxVBOHandle;
    FValidBuffers: TVBOBuffers;
    FExtentCache: TAABB;
    procedure SetUseVBO(const Value: Boolean);
    procedure SetValidBuffers(Value: TVBOBuffers);
  protected
    procedure SetTexCoords(const val: TgxAffineVectorList);
    procedure SetLightmapTexCoords(const val: TgxAffineVectorList);
    procedure SetColors(const val: TgxVectorList);
    procedure BufferArrays;
    procedure DeclareArraysToOpenGL(var mrci: TgxRenderContextInfo; evenIfAlreadyDeclared: Boolean = False);
    procedure DisableOpenGLArrays(var mrci: TgxRenderContextInfo);
    procedure EnableLightMapArray(var mrci: TgxRenderContextInfo);
    procedure DisableLightMapArray(var mrci: TgxRenderContextInfo);
    procedure SetTexCoordsEx(Index: Integer; const val: TgxVectorList);
    function GetTexCoordsEx(Index: Integer): TgxVectorList;
    procedure SetBinormals(const val: TgxVectorList);
    function GetBinormals: TgxVectorList;
    procedure SetBinormalsTexCoordIndex(const val: Integer);
    procedure SetTangents(const val: TgxVectorList);
    function GetTangents: TgxVectorList;
    procedure SetTangentsTexCoordIndex(const val: Integer);
    property ValidBuffers: TVBOBuffers read FValidBuffers write SetValidBuffers;
  public
    // Creates, assigns Owner and adds to list.
    constructor CreateOwned(aOwner: TgxMeshObjectList);
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    procedure Clear; override;
    function ExtractTriangles(texCoords: TgxAffineVectorList = nil; normals: TgxAffineVectorList = nil): TgxAffineVectorList;
      override;
    // Returns number of triangles in the mesh object.
    function TriangleCount: Integer; virtual;
    procedure PrepareMaterialLibraryCache(matLib: TgxMaterialLibrary);
    procedure DropMaterialLibraryCache;
    (* Prepare the texture and materials before rendering.
      Invoked once, before building the list and NOT while building the list. *)
    procedure PrepareBuildList(var mrci: TgxRenderContextInfo); virtual;
    // Similar to regular scene object's BuildList method
    procedure BuildList(var mrci: TgxRenderContextInfo); virtual;
    // The extents of the object (min and max coordinates)
    procedure GetExtents(out min, max: TAffineVector); overload; virtual;
    procedure GetExtents(out aabb: TAABB); overload; virtual;
    // Barycenter from vertices data
    function GetBarycenter: TVector4f;
    // Precalculate whatever is needed for rendering, called once
    procedure Prepare; virtual;
    function PointInObject(const aPoint: TAffineVector): Boolean; virtual;
    // Returns the triangle data for a given triangle
    procedure GetTriangleData(tri: Integer; list: TgxAffineVectorList; var v0, v1, v2: TAffineVector); overload;
    procedure GetTriangleData(tri: Integer; list: TgxVectorList; var v0, v1, v2: TVector4f); overload;
    // Sets the triangle data of a given triangle
    procedure SetTriangleData(tri: Integer; list: TgxAffineVectorList; const v0, v1, v2: TAffineVector); overload;
    procedure SetTriangleData(tri: Integer; list: TgxVectorList; const v0, v1, v2: TVector4f); overload;
    (* Build the tangent space from the mesh object's vertex, normal
      and texcoord data, filling the binormals and tangents where
      specified. *)
    procedure BuildTangentSpace(buildBinormals: Boolean = True; buildTangents: Boolean = True);
    property Owner: TgxMeshObjectList read FOwner;
    property mode: TgxMeshObjectMode read FMode write FMode;
    property texCoords: TgxAffineVectorList read FTexCoords write SetTexCoords;
    property LightMapTexCoords: TgxAffineVectorList read FLightMapTexCoords write SetLightmapTexCoords;
    property Colors: TgxVectorList read FColors write SetColors;
    property FaceGroups: TgxFaceGroups read FFaceGroups;
    property RenderingOptions: TgxMeshObjectRenderingOptions read FRenderingOptions write FRenderingOptions;
    // If set, rendering will use VBO's instead of vertex arrays.
    property UseVBO: Boolean read FUseVBO write SetUseVBO;
    (* The TexCoords Extension is a list of vector lists that are used
      to extend the vertex data applied during rendering.
      The lists are applied to the GL_TEXTURE0_ARB + index texture
      environment. This means that if TexCoordsEx 0 or 1 have data it
      will override the TexCoords or LightMapTexCoords repectively.
      Lists are created on demand, meaning that if you request
      TexCoordsEx[4] it will create the list up to and including 4.
      The extensions are only applied to the texture environment if
      they contain data. *)
    property TexCoordsEx[index: Integer]: TgxVectorList read GetTexCoordsEx write SetTexCoordsEx;
    (* A TexCoordsEx list wrapper for binormals usage,
      returns TexCoordsEx[BinormalsTexCoordIndex]. *)
    property Binormals: TgxVectorList read GetBinormals write SetBinormals;
    (* A TexCoordsEx list wrapper for tangents usage,
      returns TexCoordsEx[BinormalsTexCoordIndex]. *)
    property Tangents: TgxVectorList read GetTangents write SetTangents;
    // Specify the texcoord extension index for binormals (default = 2)
    property BinormalsTexCoordIndex: Integer read FBinormalsTexCoordIndex write SetBinormalsTexCoordIndex;
    // Specify the texcoord extension index for tangents (default = 3)
    property TangentsTexCoordIndex: Integer read FTangentsTexCoordIndex write SetTangentsTexCoordIndex;
  end;

  // A list of TgxMeshObject objects.
  TgxMeshObjectList = class(TgxPersistentObjectList)
  private
    FOwner: TgxBaseMesh;
    // Resturns True if all its MeshObjects use VBOs.
    function GetUseVBO: Boolean;
    procedure SetUseVBO(const Value: Boolean);
  protected
    function GetMeshObject(Index: Integer): TgxMeshObject;
  public
    constructor CreateOwned(aOwner: TgxBaseMesh);
    destructor Destroy; override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    procedure PrepareMaterialLibraryCache(matLib: TgxMaterialLibrary);
    procedure DropMaterialLibraryCache;
    (* Prepare the texture and materials before rendering.
      Invoked once, before building the list and NOT while building the list. *)
    procedure PrepareBuildList(var mrci: TgxRenderContextInfo); virtual;
    // Similar to regular scene object's BuildList method
    procedure BuildList(var mrci: TgxRenderContextInfo); virtual;
    procedure MorphTo(morphTargetIndex: Integer);
    procedure Lerp(morphTargetIndex1, morphTargetIndex2: Integer; lerpFactor: Single);
    function MorphTargetCount: Integer;
    procedure GetExtents(out min, max: TAffineVector);
    procedure Translate(const delta: TAffineVector);
    function ExtractTriangles(texCoords: TgxAffineVectorList = nil; normals: TgxAffineVectorList = nil): TgxAffineVectorList;
    // Returns number of triangles in the meshes of the list.
    function TriangleCount: Integer;
    (* Build the tangent space from the mesh object's vertex, normal
      and texcoord data, filling the binormals and tangents where
      specified. *)
    procedure BuildTangentSpace(buildBinormals: Boolean = True; buildTangents: Boolean = True);
    (* If set, rendering will use VBO's instead of vertex arrays.
      Resturns True if all its MeshObjects use VBOs. *)
    property UseVBO: Boolean read GetUseVBO write SetUseVBO;
    // Precalculate whatever is needed for rendering, called once
    procedure Prepare; virtual;
    function FindMeshByName(MeshName: string): TgxMeshObject;
    property Owner: TgxBaseMesh read FOwner;
    procedure Clear; override;
    property Items[Index: Integer]: TgxMeshObject read GetMeshObject; default;
  end;

  TgxMeshObjectListClass = class of TgxMeshObjectList;
  TgxMeshMorphTargetList = class;

  // A morph target, stores alternate lists of vertices and normals.
  TgxMeshMorphTarget = class(TgxBaseMeshObject)
  private
    FOwner: TgxMeshMorphTargetList;
  public
    constructor CreateOwned(aOwner: TgxMeshMorphTargetList);
    destructor Destroy; override;
    procedure WriteToFiler(writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    property Owner: TgxMeshMorphTargetList read FOwner;
  end;

  // A list of TgxMeshMorphTarget objects.
  TgxMeshMorphTargetList = class(TgxPersistentObjectList)
  private
    FOwner: TPersistent;
  protected
    function GetMeshMorphTarget(Index: Integer): TgxMeshMorphTarget;
  public
    constructor CreateOwned(aOwner: TPersistent);
    destructor Destroy; override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    procedure Translate(const delta: TAffineVector);
    property Owner: TPersistent read FOwner;
    procedure Clear; override;
    property Items[Index: Integer]: TgxMeshMorphTarget read GetMeshMorphTarget; default;
  end;

  (* Mesh object with support for morph targets.
    The morph targets allow to change vertices and normals according to pre-
    existing "morph targets". *)
  TgxMorphableMeshObject = class(TgxMeshObject)
  private
    FMorphTargets: TgxMeshMorphTargetList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    procedure Clear; override;
    procedure Translate(const delta: TAffineVector); override;
    procedure MorphTo(morphTargetIndex: Integer); virtual;
    procedure Lerp(morphTargetIndex1, morphTargetIndex2: Integer; lerpFactor: Single); virtual;
    property MorphTargets: TgxMeshMorphTargetList read FMorphTargets;
  end;

  TgxVertexBoneWeight = packed record
    BoneID: Integer;
    weight: Single;
  end;

  TgxVertexBoneWeightArray = array [0 .. MaxInt div (2 * SizeOf(TgxVertexBoneWeight))] of TgxVertexBoneWeight;
  PgxVertexBoneWeightArray = ^TgxVertexBoneWeightArray;
  TgxVerticesBoneWeights = array [0 .. MaxInt div (2 * SizeOf(PgxVertexBoneWeightArray))] of PgxVertexBoneWeightArray;
  PgxVerticesBoneWeights = ^TgxVerticesBoneWeights;
  TgxVertexBoneWeightDynArray = array of TgxVertexBoneWeight;

  (* A mesh object with vertice bone attachments.
    The class adds per vertex bone weights to the standard morphable mesh.
    The TgxVertexBoneWeight structures are accessed via VerticesBonesWeights,
    they must be initialized by adjusting the BonesPerVertex and
    VerticeBoneWeightCount properties, you can also add vertex by vertex
    by using the AddWeightedBone method.
    When BonesPerVertex is 1, the weight is ignored (set to 1.0). *)
  TgxSkeletonMeshObject = class(TgxMorphableMeshObject)
  private
    FVerticesBonesWeights: PgxVerticesBoneWeights;
    FVerticeBoneWeightCount, FVerticeBoneWeightCapacity: Integer;
    FBonesPerVertex: Integer;
    FLastVerticeBoneWeightCount, FLastBonesPerVertex: Integer; // not persistent
    FBoneMatrixInvertedMeshes: TList; // not persistent
    FBackupInvertedMeshes: TList; // ragdoll
    procedure BackupBoneMatrixInvertedMeshes; // ragdoll
    procedure RestoreBoneMatrixInvertedMeshes; // ragdoll
  protected
    procedure SetVerticeBoneWeightCount(const val: Integer);
    procedure SetVerticeBoneWeightCapacity(const val: Integer);
    procedure SetBonesPerVertex(const val: Integer);
    procedure ResizeVerticesBonesWeights;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    procedure Clear; override;
    property VerticesBonesWeights: PgxVerticesBoneWeights read FVerticesBonesWeights;
    property VerticeBoneWeightCount: Integer read FVerticeBoneWeightCount write SetVerticeBoneWeightCount;
    property VerticeBoneWeightCapacity: Integer read FVerticeBoneWeightCapacity write SetVerticeBoneWeightCapacity;
    property BonesPerVertex: Integer read FBonesPerVertex write SetBonesPerVertex;
    function FindOrAdd(BoneID: Integer; const vertex, normal: TAffineVector): Integer; overload;
    function FindOrAdd(const boneIDs: TgxVertexBoneWeightDynArray; const vertex, normal: TAffineVector): Integer; overload;
    procedure AddWeightedBone(aBoneID: Integer; aWeight: Single);
    procedure AddWeightedBones(const boneIDs: TgxVertexBoneWeightDynArray);
    procedure PrepareBoneMatrixInvertedMeshes;
    procedure ApplyCurrentSkeletonFrame(normalize: Boolean);
  end;

  (* Describes a face group of a TgxMeshObject.
    Face groups should be understood as "a way to use mesh data to render
    a part or the whole mesh object".
    Subclasses implement the actual behaviours, and should have at least
    one "Add" method, taking in parameters all that is required to describe
    a single base facegroup element. *)
  TgxFaceGroup = class(TgxPersistentObject)
  private
    FOwner: TgxFaceGroups;
    FMaterialName: string;
    FMaterialCache: TgxLibMaterial;
    FLightMapIndex: Integer;
    FRenderGroupID: Integer;
    // NOT Persistent, internal use only (rendering options)
  protected
    procedure AttachLightmap(lightMap: TgxTexture; var mrci: TgxRenderContextInfo);
    procedure AttachOrDetachLightmap(var mrci: TgxRenderContextInfo);
  public
    constructor CreateOwned(aOwner: TgxFaceGroups); virtual;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    procedure PrepareMaterialLibraryCache(matLib: TgxMaterialLibrary);
    procedure DropMaterialLibraryCache;
    procedure BuildList(var mrci: TgxRenderContextInfo); virtual; abstract;
    (* Add to the list the triangles corresponding to the facegroup.
      This function is used by TgxMeshObjects ExtractTriangles to retrieve
      all the triangles in a mesh. *)
    procedure AddToTriangles(aList: TgxAffineVectorList; aTexCoords: TgxAffineVectorList = nil;
      aNormals: TgxAffineVectorList = nil); virtual;
    // Returns number of triangles in the facegroup.
    function TriangleCount: Integer; virtual; abstract;
    (* Reverses the rendering order of faces.
      Default implementation does nothing *)
    procedure Reverse; virtual;
    // Precalculate whatever is needed for rendering, called once
    procedure Prepare; virtual;
    property Owner: TgxFaceGroups read FOwner write FOwner;
    property MaterialName: string read FMaterialName write FMaterialName;
    property MaterialCache: TgxLibMaterial read FMaterialCache;
    // Index of lightmap in the lightmap library.
    property LightMapIndex: Integer read FLightMapIndex write FLightMapIndex;
  end;

  (* Known descriptions for face group mesh modes.
    - fgmmTriangles : issue all vertices with GL_TRIANGLES.
    - fgmmTriangleStrip : issue all vertices with GL_TRIANGLE_STRIP.
    - fgmmFlatTriangles : same as fgmmTriangles, but take advantage of having
    the same normal for all vertices of a triangle.
    - fgmmTriangleFan : issue all vertices with GL_TRIANGLE_FAN.
    - fgmmQuads : issue all vertices with GL_QUADS. *)
  TgxFaceGroupMeshMode = (fgmmTriangles, fgmmTriangleStrip, fgmmFlatTriangles, fgmmTriangleFan, fgmmQuads);

  (* A face group based on an indexlist.
    The index list refers to items in the mesh object (vertices, normals, etc.),
    that are all considered in sync, the render is obtained issueing the items
    in the order given by the vertices. *)
  TfgxVertexIndexList = class(TgxFaceGroup)
  private
    FVertexIndices: TgxIntegerList;
    FIndexVBO: TgxVBOElementArrayHandle;
    FMode: TgxFaceGroupMeshMode;
    procedure SetupVBO;
    procedure InvalidateVBO;
  protected
    procedure SetVertexIndices(const val: TgxIntegerList);
    procedure AddToList(Source, destination: TgxAffineVectorList; indices: TgxIntegerList);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    procedure BuildList(var mrci: TgxRenderContextInfo); override;
    procedure AddToTriangles(aList: TgxAffineVectorList; aTexCoords: TgxAffineVectorList = nil;
      aNormals: TgxAffineVectorList = nil); override;
    function TriangleCount: Integer; override;
    procedure Reverse; override;
    procedure Add(idx: Integer);
    procedure GetExtents(var min, max: TAffineVector);
    // If mode is strip or fan, convert the indices to triangle list indices.
    procedure ConvertToList;
    // Return the normal from the 1st three points in the facegroup
    function GetNormal: TAffineVector;
    property mode: TgxFaceGroupMeshMode read FMode write FMode;
    property vertexIndices: TgxIntegerList read FVertexIndices write SetVertexIndices;
  end;

  (* Adds normals and texcoords indices.
    Allows very compact description of a mesh. The Normals ad TexCoords
    indices are optionnal, if missing (empty), VertexIndices will be used. *)
  TFGVertexNormalTexIndexList = class(TfgxVertexIndexList)
  private
    FNormalIndices: TgxIntegerList;
    FTexCoordIndices: TgxIntegerList;
  protected
    procedure SetNormalIndices(const val: TgxIntegerList);
    procedure SetTexCoordIndices(const val: TgxIntegerList);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    procedure BuildList(var mrci: TgxRenderContextInfo); override;
    procedure AddToTriangles(aList: TgxAffineVectorList; aTexCoords: TgxAffineVectorList = nil;
      aNormals: TgxAffineVectorList = nil); override;
    procedure Add(vertexIdx, normalIdx, texCoordIdx: Integer);
    property normalIndices: TgxIntegerList read FNormalIndices write SetNormalIndices;
    property TexCoordIndices: TgxIntegerList read FTexCoordIndices write SetTexCoordIndices;
  end;

  (* Adds per index texture coordinates to its ancestor.
    Per index texture coordinates allows having different texture coordinates
    per triangle, depending on the face it is used in. *)
  TFGIndexTexCoordList = class(TfgxVertexIndexList)
  private
    FTexCoords: TgxAffineVectorList;
  protected
    procedure SetTexCoords(const val: TgxAffineVectorList);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    procedure BuildList(var mrci: TgxRenderContextInfo); override;
    procedure AddToTriangles(aList: TgxAffineVectorList; aTexCoords: TgxAffineVectorList = nil;
      aNormals: TgxAffineVectorList = nil); override;
    procedure Add(idx: Integer; const texCoord: TAffineVector); overload;
    procedure Add(idx: Integer; const s, t: Single); overload;
    property texCoords: TgxAffineVectorList read FTexCoords write SetTexCoords;
  end;

  // A list of TgxFaceGroup objects.
  TgxFaceGroups = class(TgxPersistentObjectList)
  private
    FOwner: TgxMeshObject;
  protected
    function GetFaceGroup(Index: Integer): TgxFaceGroup;
  public
    constructor CreateOwned(aOwner: TgxMeshObject);
    destructor Destroy; override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    procedure PrepareMaterialLibraryCache(matLib: TgxMaterialLibrary);
    procedure DropMaterialLibraryCache;
    property Owner: TgxMeshObject read FOwner;
    procedure Clear; override;
    property Items[Index: Integer]: TgxFaceGroup read GetFaceGroup; default;
    procedure AddToTriangles(aList: TgxAffineVectorList; aTexCoords: TgxAffineVectorList = nil; aNormals: TgxAffineVectorList = nil);
    // Material Library of the owner TgxBaseMesh.
    function MaterialLibrary: TgxMaterialLibrary;
    (* Sort faces by material.
      Those without material first in list, followed by opaque materials,
      then transparent materials. *)
    procedure SortByMaterial;
  end;

  (* Determines how normals orientation is defined in a mesh.
    - mnoDefault : uses default orientation
    - mnoInvert : inverse of default orientation
    - mnoAutoSolid : autocalculate to make the mesh globally solid
    - mnoAutoHollow : autocalculate to make the mesh globally hollow *)
  TMeshNormalsOrientation = (mnoDefault, mnoInvert); // , mnoAutoSolid, mnoAutoHollow);

  (* Abstract base class for different vector file Formatx.
    The actual implementation for these files (3DS, DXF..) must be done
    seperately. The concept for TgxVectorFile is very similar to TGraphic
    (see Delphi Help). *)
  TgxVectorFile = class(TgxDataFile)
  private
    FNormalsOrientation: TMeshNormalsOrientation;
  protected
    procedure SetNormalsOrientation(const val: TMeshNormalsOrientation); virtual;
  public
    constructor Create(aOwner: TPersistent); override;
    function Owner: TgxBaseMesh;
    property NormalsOrientation: TMeshNormalsOrientation read FNormalsOrientation write SetNormalsOrientation;
  end;

  TgxVectorFileClass = class of TgxVectorFile;

  (* GLSM ( GXScene Mesh) vector file.
    This corresponds to the 'native' Scene format, and object persistence
    stream, which should be the 'fastest' of all formats to load, and supports
    all of GXScene features. *)
  TgxGLSMVectorFile = class(TgxVectorFile)
  public
    class function Capabilities: TDataFileCapabilities; override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
  end;

  // Base class for mesh objects.
  TgxBaseMesh = class(TgxSceneObject)
  private
    FNormalsOrientation: TMeshNormalsOrientation;
    FMaterialLibrary: TgxMaterialLibrary;
    FLightmapLibrary: TgxMaterialLibrary;
    FAxisAlignedDimensionsCache: TVector4f;
    FBaryCenterOffsetChanged: Boolean;
    FBaryCenterOffset: TVector4f;
    FUseMeshMaterials: Boolean;
    FOverlaySkeleton: Boolean;
    FIgnoreMissingTextures: Boolean;
    FAutoCentering: TgxMeshAutoCenterings;
    FAutoScaling: TgxCoordinates;
    FMaterialLibraryCachesPrepared: Boolean;
    FConnectivity: TObject;
    FLastLoadedFilename: string;
  protected
    FMeshObjects: TgxMeshObjectList; // a list of mesh objects
    FSkeleton: TgxSkeleton; // skeleton data & frames
    procedure SetUseMeshMaterials(const val: Boolean);
    procedure SetMaterialLibrary(const val: TgxMaterialLibrary);
    procedure SetLightmapLibrary(const val: TgxMaterialLibrary);
    procedure SetNormalsOrientation(const val: TMeshNormalsOrientation);
    procedure SetOverlaySkeleton(const val: Boolean);
    procedure SetAutoScaling(const Value: TgxCoordinates);
    procedure DestroyHandle; override;
    (* Invoked after creating a TgxVectorFile and before loading.
      Triggered by LoadFromFile/Stream and AddDataFromFile/Stream.
      Allows to adjust/transfer subclass-specific features. *)
    procedure PrepareVectorFile(aFile: TgxVectorFile); virtual;
    (* Invoked after a mesh has been loaded/added.
      Triggered by LoadFromFile/Stream and AddDataFromFile/Stream.
      Allows to adjust/transfer subclass-specific features. *)
    procedure PrepareMesh; virtual;
    (* Recursively propagated to mesh object and facegroups.
      Notifies that they all can establish their material library caches. *)
    procedure PrepareMaterialLibraryCache;
    (* Recursively propagated to mesh object and facegroups.
      Notifies that they all should forget their material library caches. *)
    procedure DropMaterialLibraryCache;
    (* Prepare the texture and materials before rendering.
      Invoked once, before building the list and NOT while building the list,
      MaterialLibraryCache can be assumed to having been prepared if materials
      are active. Default behaviour is to prepare build lists for the
      meshobjects. *)
    procedure PrepareBuildList(var mrci: TgxRenderContextInfo); virtual;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function AxisAlignedDimensionsUnscaled: TVector4f; override;
    function BarycenterOffset: TVector4f;
    function BarycenterPosition: TVector4f;
    function BarycenterAbsolutePosition: TVector4f; override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
    procedure DoRender(var rci: TgxRenderContextInfo; renderSelf, renderChildren: Boolean); override;
    procedure StructureChanged; override;
    (* Notifies that geometry data changed, but no re-preparation is needed.
      Using this method will usually be faster, but may result in incorrect
      rendering, reduced performance and/or invalid bounding box data
      (ie. invalid collision detection). Use with caution. *)
    procedure StructureChangedNoPrepare;
    // BEWARE! Utterly inefficient implementation!
    function RayCastIntersect(const rayStart, rayVector: TVector4f; intersectPoint: PVector4f = nil; intersectNormal: PVector4f = nil)
      : Boolean; override;
    function GenerateSilhouette(const SilhouetteParameters: TgxSilhouetteParameters): TgxSilhouette; override;
    (* This method allows fast shadow volumes for GLActors.
      If your actor/mesh doesn't change, you don't need to call this.
      It basically caches the connectivity data. *)
    procedure BuildSilhouetteConnectivityData;
    property MeshObjects: TgxMeshObjectList read FMeshObjects;
    property Skeleton: TgxSkeleton read FSkeleton;
    // Computes the extents of the mesh.
    procedure GetExtents(out min, max: TAffineVector);
    // Computes the barycenter of the mesh.
    function GetBarycenter: TAffineVector;
    (* Invoked after a mesh has been loaded.
      Should auto-center according to the AutoCentering property. *)
    procedure PerformAutoCentering; virtual;
    (* Invoked after a mesh has been loaded.
      Should auto-scale the vertices of the meshobjects to AutoScaling the property. *)
    procedure PerformAutoScaling; virtual;
    (* Loads a vector file.
      A vector files (for instance a ".3DS") stores the definition of
      a mesh as well as materials property.
      Loading a file replaces the current one (if any). *)
    procedure LoadFromFile(const filename: string); virtual;
    (* Loads a vector file from a stream.
      See LoadFromFile.
      The filename attribute is required to identify the type data you're
      streaming (3DS, OBJ, etc.) *)
    procedure LoadFromStream(const filename: string; aStream: TStream); virtual;
    (* Saves to a vector file.
      Note that only some of the vector files formats can be written. *)
    procedure SaveToFile(const filename: string); virtual;
    (* Saves to a vector file in a stream.
      Note that only some of the vector files formats can be written. *)
    procedure SaveToStream(const filename: string; aStream: TStream); virtual;
    (* Loads additionnal data from a file.
      Additionnal data could be more animation frames or morph target.
      The VectorFile importer must be able to handle addition of data
      flawlessly. *)
    procedure AddDataFromFile(const filename: string); virtual;
    // Loads additionnal data from stream. See AddDataFromFile.
    procedure AddDataFromStream(const filename: string; aStream: TStream); virtual;
    (* Returns the filename of the last loaded file, or a blank string if not
      file was loaded (or if the mesh was dinamically built). This does not
      take into account the data added to the mesh (through AddDataFromFile)
      or saved files. *)
    function LastLoadedFilename: string;
    (* Determines if a mesh should be centered and how.
      AutoCentering is performed  only  after loading a mesh, it has
      no effect on already loaded mesh data or when adding from a file/stream.
      If you want to alter mesh data, use direct manipulation methods
      (on the TgxMeshObjects). *)
    property AutoCentering: TgxMeshAutoCenterings read FAutoCentering write FAutoCentering default [];
    (* Scales vertices to a AutoScaling.
      AutoScaling is performed  only  after loading a mesh, it has
      no effect on already loaded mesh data or when adding from a file/stream.
      If you want to alter mesh data, use direct manipulation methods
      (on the TgxMeshObjects). *)
    property AutoScaling: TgxCoordinates read FAutoScaling write FAutoScaling;
    (* Material library where mesh materials will be stored/retrieved.
      If this property is not defined or if UseMeshMaterials is false,
      only the FreeForm's material will be used (and the mesh's materials
      will be ignored. *)
    property MaterialLibrary: TgxMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    (* Defines wether materials declared in the vector file mesh are used.
      You must also define the MaterialLibrary property. *)
    property UseMeshMaterials: Boolean read FUseMeshMaterials write SetUseMeshMaterials default True;
    (* LightMap library where lightmaps will be stored/retrieved.
      If this property is not defined, lightmaps won't be used.
      Lightmaps currently *always* use the second texture unit (unit 1),
      and may interfere with multi-texture materials. *)
    property LightmapLibrary: TgxMaterialLibrary read FLightmapLibrary write SetLightmapLibrary;
    (* If True, exceptions about missing textures will be ignored.
      Implementation is up to the file loader class (ie. this property
      may be ignored by some loaders) *)
    property IgnoreMissingTextures: Boolean read FIgnoreMissingTextures write FIgnoreMissingTextures default False;
    // Normals orientation for owned mesh.
    property NormalsOrientation: TMeshNormalsOrientation read FNormalsOrientation write SetNormalsOrientation
      default mnoDefault;
    // Request rendering of skeleton bones over the mesh.
    property OverlaySkeleton: Boolean read FOverlaySkeleton write SetOverlaySkeleton default False;
  end;

  (* Container objects for a vector file mesh.
    FreeForms allows loading and rendering vector files (like 3DStudio
    ".3DS" file) in GLScene.  Meshes can be loaded with the LoadFromFile
    method.
    A FreeForm may contain more than one mesh, but they will all be handled
    as a single object in a scene. *)
  TgxFreeForm = class(TgxBaseMesh)
  private
    FOctree: TgxOctree;
  protected
    function GetOctree: TgxOctree;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function OctreeRayCastIntersect(const rayStart, rayVector: TVector4f; intersectPoint: PVector4f = nil;
      intersectNormal: PVector4f = nil): Boolean;
    function OctreeSphereSweepIntersect(const rayStart, rayVector: TVector4f; const velocity, radius: Single;
      intersectPoint: PVector4f = nil; intersectNormal: PVector4f = nil): Boolean;
    function OctreeTriangleIntersect(const v1, v2, v3: TAffineVector): Boolean;
    (* Returns true if Point is inside the free form - this will only work
      properly on closed meshes. Requires that Octree has been prepared. *)
    function OctreePointInMesh(const Point: TVector4f): Boolean;
    function OctreeAABBIntersect(const aabb: TAABB; objMatrix, invObjMatrix: TMatrix4f;
      triangles: TgxAffineVectorList = nil): Boolean;
    // TODO:  function OctreeSphereIntersect
    (* Octree support *experimental*.
      Use only if you understand what you're doing! *)
    property Octree: TgxOctree read GetOctree;
    procedure BuildOctree(TreeDepth: Integer = 3);
  published
    property AutoCentering;
    property AutoScaling;
    property MaterialLibrary;
    property LightmapLibrary;
    property UseMeshMaterials;
    property NormalsOrientation;
  end;

  (* Miscellanious actor options.
    aoSkeletonNormalizeNormals : if set the normals of a skeleton-animated
    mesh will be normalized, this is not required if no normals-based texture
    coordinates generation occurs, and thus may be unset to improve performance. *)
  TgxActorOption = (aoSkeletonNormalizeNormals);
  TgxActorOptions = set of TgxActorOption;

const
  cDefaultActorOptions = [aoSkeletonNormalizeNormals];

type

  TgxActor = class;
  TgxActorAnimationReference = (aarMorph, aarSkeleton, aarNone);

  (* An actor animation sequence.
    An animation sequence is a named set of contiguous frames that can be used
    for animating an actor. The referred frames can be either morph or skeletal
    frames (choose which via the Reference property).
    An animation can be directly "played" by the actor by selecting it with
    SwitchAnimation, and can also be "blended" via a TgxAnimationControler. *)
  TgxActorAnimation = class(TCollectionItem)
  private
    FName: string;
    FStartFrame: Integer;
    FEndFrame: Integer;
    FReference: TgxActorAnimationReference;
  protected
    function GetDisplayName: string; override;
    function FrameCount: Integer;
    procedure SetStartFrame(const val: Integer);
    procedure SetEndFrame(const val: Integer);
    procedure SetReference(val: TgxActorAnimationReference);
    procedure SetAsString(const val: string);
    function GetAsString: string;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property AsString: string read GetAsString write SetAsString;
    function OwnerActor: TgxActor;
    (* Linearly removes the translation component between skeletal frames.
      This function will compute the translation of the first bone (index 0)
      and linearly subtract this translation in all frames between startFrame
      and endFrame. Its purpose is essentially to remove the 'slide' that
      exists in some animation formats (f.i. SMD). *)
    procedure MakeSkeletalTranslationStatic;
    (* Removes the absolute rotation component of the skeletal frames.
      Some formats will store frames with absolute rotation information,
      if this correct if the animation is the "main" animation.
      This function removes that absolute information, making the animation
      frames suitable for blending purposes. *)
    procedure MakeSkeletalRotationDelta;
  published
    property Name: string read FName write FName;
    // Index of the initial frame of the animation.
    property startFrame: Integer read FStartFrame write SetStartFrame;
    // Index of the final frame of the animation.
    property endFrame: Integer read FEndFrame write SetEndFrame;
    // Indicates if this is a skeletal or a morph-based animation.
    property reference: TgxActorAnimationReference read FReference write SetReference default aarMorph;
  end;

  TgxActorAnimationName = string;

  // Collection of actor animations sequences.
  TgxActorAnimations = class(TCollection)
  private
    FOwner: TgxActor;
  protected
    function GetOwner: TPersistent; override;
    procedure SetItems(Index: Integer; const val: TgxActorAnimation);
    function GetItems(Index: Integer): TgxActorAnimation;
  public
    constructor Create(aOwner: TgxActor);
    function Add: TgxActorAnimation;
    function FindItemID(ID: Integer): TgxActorAnimation;
    function FindName(const aName: string): TgxActorAnimation;
    function FindFrame(aFrame: Integer; aReference: TgxActorAnimationReference): TgxActorAnimation;
    procedure SetToStrings(aStrings: TStrings);
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromStream(aStream: TStream);
    procedure SaveToFile(const filename: string);
    procedure LoadFromFile(const filename: string);
    property Items[index: Integer]: TgxActorAnimation read GetItems write SetItems; default;
    function Last: TgxActorAnimation;
  end;

  // Base class for skeletal animation control.
  TgxBaseAnimationControler = class(TComponent)
  private
    FEnabled: Boolean;
    FActor: TgxActor;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetEnabled(const val: Boolean);
    procedure SetActor(const val: TgxActor);
    procedure DoChange; virtual;
    function Apply(var lerpInfo: TgxBlendedLerpInfo): Boolean; virtual;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Actor: TgxActor read FActor write SetActor;
  end;

  (* Controls the blending of an additionnal skeletal animation into an actor.
    The animation controler allows animating an actor with several animations
    at a time, for instance, you could use a "run" animation as base animation
    (in TgxActor), blend an animation that makes the arms move differently
    depending on what the actor is carrying, along with an animation that will
    make the head turn toward a target. *)
  TgxAnimationControler = class(TgxBaseAnimationControler)
  private
    FAnimationName: TgxActorAnimationName;
    FRatio: Single;
  protected
    procedure SetAnimationName(const val: TgxActorAnimationName);
    procedure SetRatio(const val: Single);
    procedure DoChange; override;
    function Apply(var lerpInfo: TgxBlendedLerpInfo): Boolean; override;
  published
    property AnimationName: string read FAnimationName write SetAnimationName;
    property Ratio: Single read FRatio write SetRatio;
  end;

  (* Actor frame-interpolation mode.
    - afpNone : no interpolation, display CurrentFrame only
    - afpLinear : perform linear interpolation between current and next frame *)
  TActorFrameInterpolation = (afpNone, afpLinear);

  (* Defines how an actor plays between its StartFrame and EndFrame.
    aamNone : no animation is performed
    aamPlayOnce : play from current frame to EndFrame, once end frame has
    been reached, switches to aamNone
    aamLoop : play from current frame to EndFrame, once end frame has
    been reached, sets CurrentFrame to StartFrame
    aamBounceForward : play from current frame to EndFrame, once end frame
    has been reached, switches to aamBounceBackward
    aamBounceBackward : play from current frame to StartFrame, once start
    frame has been reached, switches to aamBounceForward
    aamExternal : Allows for external animation control *)
  TgxActorAnimationMode = (aamNone, aamPlayOnce, aamLoop, aamBounceForward, aamBounceBackward, aamLoopBackward, aamExternal);

  (* Mesh class specialized in animated meshes.
    The TgxActor provides a quick interface to animated meshes based on morph
    or skeleton frames, it is capable of performing frame interpolation and
    animation blending (via TgxAnimationControler components). *)
  TgxActor = class(TgxBaseMesh)
  private
    FStartFrame, FEndFrame: Integer;
    FReference: TgxActorAnimationReference;
    FCurrentFrame: Integer;
    FCurrentFrameDelta: Single;
    FFrameInterpolation: TActorFrameInterpolation;
    FInterval: Integer;
    FAnimationMode: TgxActorAnimationMode;
    FOnFrameChanged: TNotifyEvent;
    FOnEndFrameReached, FOnStartFrameReached: TNotifyEvent;
    FAnimations: TgxActorAnimations;
    FTargetSmoothAnimation: TgxActorAnimation;
    FControlers: TList;
    FOptions: TgxActorOptions;
  protected
    procedure SetCurrentFrame(val: Integer);
    procedure SetStartFrame(val: Integer);
    procedure SetEndFrame(val: Integer);
    procedure SetReference(val: TgxActorAnimationReference);
    procedure SetAnimations(const val: TgxActorAnimations);
    function StoreAnimations: Boolean;
    procedure SetOptions(const val: TgxActorOptions);
    procedure PrepareMesh; override;
    procedure PrepareBuildList(var mrci: TgxRenderContextInfo); override;
    procedure DoAnimate; virtual;
    procedure RegisterControler(aControler: TgxBaseAnimationControler);
    procedure UnRegisterControler(aControler: TgxBaseAnimationControler);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
    procedure DoProgress(const progressTime: TgxProgressTimes); override;
    procedure LoadFromStream(const filename: string; aStream: TStream); override;
    procedure SwitchToAnimation(anAnimation: TgxActorAnimation; smooth: Boolean = False); overload;
    procedure SwitchToAnimation(const AnimationName: string; smooth: Boolean = False); overload;
    procedure SwitchToAnimation(animationIndex: Integer; smooth: Boolean = False); overload;
    function CurrentAnimation: string;
    (* Synchronize self animation with an other actor.
      Copies Start/Current/End Frame values, CurrentFrameDelta,
      AnimationMode and FrameInterpolation. *)
    procedure Synchronize(referenceActor: TgxActor);
    (* Provides a direct access to FCurrentFrame without any checks.
      Used in TgxActorProxy. *)
    procedure SetCurrentFrameDirect(const Value: Integer);
    function NextFrameIndex: Integer;
    procedure NextFrame(nbSteps: Integer = 1);
    procedure PrevFrame(nbSteps: Integer = 1);
    function FrameCount: Integer;
    (* Indicates whether the actor is currently swithing animations (with
      smooth interpolation). *)
    function isSwitchingAnimation: Boolean;
  published
    property startFrame: Integer read FStartFrame write SetStartFrame default 0;
    property endFrame: Integer read FEndFrame write SetEndFrame default 0;
    (* Reference Frame Animation mode.
      Allows specifying if the model is primarily morph or skeleton based. *)
    property reference: TgxActorAnimationReference read FReference write FReference default aarMorph;
    // Current animation frame.
    property CurrentFrame: Integer read FCurrentFrame write SetCurrentFrame default 0;
    // Value in the [0; 1] range expressing the delta to the next frame.
    property CurrentFrameDelta: Single read FCurrentFrameDelta write FCurrentFrameDelta;
    // Frame interpolation mode (afpNone/afpLinear).
    property FrameInterpolation: TActorFrameInterpolation read FFrameInterpolation write FFrameInterpolation default afpLinear;
    // See TgxActorAnimationMode.
    property AnimationMode: TgxActorAnimationMode read FAnimationMode write FAnimationMode default aamNone;
    // Interval between frames, in milliseconds.
    property Interval: Integer read FInterval write FInterval;
    // Actor and animation miscellanious options.
    property Options: TgxActorOptions read FOptions write SetOptions default cDefaultActorOptions;
    // Triggered after each CurrentFrame change.
    property OnFrameChanged: TNotifyEvent read FOnFrameChanged write FOnFrameChanged;
    // Triggered after EndFrame has been reached by progression or "nextframe"
    property OnEndFrameReached: TNotifyEvent read FOnEndFrameReached write FOnEndFrameReached;
    // Triggered after StartFrame has been reached by progression or "nextframe"
    property OnStartFrameReached: TNotifyEvent read FOnStartFrameReached write FOnStartFrameReached;
    // Collection of animations sequences.
    property Animations: TgxActorAnimations read FAnimations write SetAnimations stored StoreAnimations;
    property AutoCentering;
    property MaterialLibrary;
    property LightmapLibrary;
    property UseMeshMaterials;
    property NormalsOrientation;
    property OverlaySkeleton;
  end;

  TgxVectorFileFormat = class
  public
    VectorFileClass: TgxVectorFileClass;
    Extension: string;
    Description: string;
    DescResID: Integer;
  end;

  // Stores registered vector file Formatx.
  TgxVectorFileFormatsList = class(TgxPersistentObjectList)
  public
    destructor Destroy; override;
    procedure Add(const Ext, Desc: string; DescID: Integer; AClass: TgxVectorFileClass);
    function FindExt(Ext: string): TgxVectorFileClass;
    function FindFromFileName(const filename: string): TgxVectorFileClass;
    procedure Remove(AClass: TgxVectorFileClass);
    procedure BuildFilterStrings(VectorFileClass: TgxVectorFileClass; out descriptions, filters: string;
      formatsThatCanBeOpened: Boolean = True; formatsThatCanBeSaved: Boolean = False);
    function FindExtByIndex(Index: Integer; formatsThatCanBeOpened: Boolean = True;
      formatsThatCanBeSaved: Boolean = False): string;
  end;

  EInvalidVectorFile = class(Exception);

// Read access to the list of registered vector file formats
function GetVectorFileFormats: TgxVectorFileFormatsList;
// A file extension filter suitable for dialog's 'Filter' property
function VectorFileFormatsFilter: string;
// A file extension filter suitable for a savedialog's 'Filter' property
function VectorFileFormatsSaveFilter: string;
(* Returns an extension by its index in the vector files dialogs filter.
  Use VectorFileFormatsFilter to obtain the filter. *)
function VectorFileFormatExtensionByIndex(Index: Integer): string;

procedure RegisterVectorFileFormat(const aExtension, aDescription: string; AClass: TgxVectorFileClass);
procedure UnregisterVectorFileClass(AClass: TgxVectorFileClass);

var
  vVectorFileObjectsAllocateMaterials: Boolean = True;
  // Flag to avoid loading materials (useful for IDE Extentions or scene editors)
  vVectorFileObjectsEnableVBOByDefault: Boolean = True;

// ===========================================================================
implementation
// ===========================================================================

uses
  GXS.MeshUtils,
  GXS.BaseMeshSilhouette;

var
  vVectorFileFormats: TgxVectorFileFormatsList;
  vNextRenderGroupID: Integer = 1;

const
  cAAFHeader: AnsiString = 'AAF';

function GetVectorFileFormats: TgxVectorFileFormatsList;
begin
  if not Assigned(vVectorFileFormats) then
    vVectorFileFormats := TgxVectorFileFormatsList.Create;
  Result := vVectorFileFormats;
end;

function VectorFileFormatsFilter: string;
var
  f: string;
begin
  GetVectorFileFormats.BuildFilterStrings(TgxVectorFile, Result, f);
end;

function VectorFileFormatsSaveFilter: string;
var
  f: string;
begin
  GetVectorFileFormats.BuildFilterStrings(TgxVectorFile, Result, f, False, True);
end;

procedure RegisterVectorFileFormat(const aExtension, aDescription: string; AClass: TgxVectorFileClass);
begin
  RegisterClass(AClass);
  GetVectorFileFormats.Add(aExtension, aDescription, 0, AClass);
end;

procedure UnregisterVectorFileClass(AClass: TgxVectorFileClass);
begin
  if Assigned(vVectorFileFormats) then
    vVectorFileFormats.Remove(AClass);
end;

function VectorFileFormatExtensionByIndex(Index: Integer): string;
begin
  Result := GetVectorFileFormats.FindExtByIndex(index);
end;

destructor TgxVectorFileFormatsList.Destroy;
begin
  Clean;
  inherited;
end;

procedure TgxVectorFileFormatsList.Add(const Ext, Desc: string; DescID: Integer; AClass: TgxVectorFileClass);
var
  newRec: TgxVectorFileFormat;
begin
  newRec := TgxVectorFileFormat.Create;
  with newRec do
  begin
    Extension := AnsiLowerCase(Ext);
    VectorFileClass := AClass;
    Description := Desc;
    DescResID := DescID;
  end;
  inherited Add(newRec);
end;

function TgxVectorFileFormatsList.FindExt(Ext: string): TgxVectorFileClass;
var
  i: Integer;
begin
  Ext := AnsiLowerCase(Ext);
  for i := Count - 1 downto 0 do
    with TgxVectorFileFormat(Items[i]) do
    begin
      if Extension = Ext then
      begin
        Result := VectorFileClass;
        Exit;
      end;
    end;
  Result := nil;
end;

function TgxVectorFileFormatsList.FindFromFileName(const filename: string): TgxVectorFileClass;
var
  Ext: string;
begin
  Ext := ExtractFileExt(filename);
  System.Delete(Ext, 1, 1);
  Result := FindExt(Ext);
  if not Assigned(Result) then
    raise EInvalidVectorFile.CreateFmt(strUnknownExtension, [Ext, 'GLFile' + UpperCase(Ext)]);
end;

procedure TgxVectorFileFormatsList.Remove(AClass: TgxVectorFileClass);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if TgxVectorFileFormat(Items[i]).VectorFileClass.InheritsFrom(AClass) then
      DeleteAndFree(i);
  end;
end;

procedure TgxVectorFileFormatsList.BuildFilterStrings(VectorFileClass: TgxVectorFileClass; out descriptions, filters: string;
  formatsThatCanBeOpened: Boolean = True; formatsThatCanBeSaved: Boolean = False);
var
  k, i: Integer;
  p: TgxVectorFileFormat;
begin
  descriptions := '';
  filters := '';
  k := 0;
  for i := 0 to Count - 1 do
  begin
    p := TgxVectorFileFormat(Items[i]);
    if p.VectorFileClass.InheritsFrom(VectorFileClass) and (p.Extension <> '') and
      ((formatsThatCanBeOpened and (dfcRead in p.VectorFileClass.Capabilities)) or
      (formatsThatCanBeSaved and (dfcWrite in p.VectorFileClass.Capabilities))) then
    begin
      with p do
      begin
        if k <> 0 then
        begin
          descriptions := descriptions + '|';
          filters := filters + ';';
        end;
        if (Description = '') and (DescResID <> 0) then
          Description := LoadStr(DescResID);
        FmtStr(descriptions, '%s%s (*.%s)|*.%2:s', [descriptions, Description, Extension]);
        filters := filters + '*.' + Extension;
        Inc(k);
      end;
    end;
  end;
  if (k > 1) and (not formatsThatCanBeSaved) then
    FmtStr(descriptions, '%s (%s)|%1:s|%s', [sAllFilter, filters, descriptions]);
end;

function TgxVectorFileFormatsList.FindExtByIndex(Index: Integer; formatsThatCanBeOpened: Boolean = True;
  formatsThatCanBeSaved: Boolean = False): string;
var
  i: Integer;
  p: TgxVectorFileFormat;
begin
  Result := '';
  if index > 0 then
  begin
    for i := 0 to Count - 1 do
    begin
      p := TgxVectorFileFormat(Items[i]);
      if (formatsThatCanBeOpened and (dfcRead in p.VectorFileClass.Capabilities)) or
        (formatsThatCanBeSaved and (dfcWrite in p.VectorFileClass.Capabilities)) then
      begin
        if index = 1 then
        begin
          Result := p.Extension;
          Break;
        end
        else
          Dec(index);
      end;
    end;
  end;
end;

// ------------------
// ------------------ TgxBaseMeshObject ------------------
// ------------------

constructor TgxBaseMeshObject.Create;
begin
  FVertices := TgxAffineVectorList.Create;
  FNormals := TgxAffineVectorList.Create;
  FVisible := True;
  inherited Create;
end;

destructor TgxBaseMeshObject.Destroy;
begin
  FNormals.Free;
  FVertices.Free;
  inherited;
end;

procedure TgxBaseMeshObject.Assign(Source: TPersistent);
begin
  if Source is TgxBaseMeshObject then
  begin
    FName := TgxBaseMeshObject(Source).Name;
    FVertices.Assign(TgxBaseMeshObject(Source).FVertices);
    FNormals.Assign(TgxBaseMeshObject(Source).FNormals);
  end
  else
    inherited; // Die!
end;

procedure TgxBaseMeshObject.WriteToFiler(writer: TgxVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(1); // Archive Version 1, added FVisible
    WriteString(FName);
    FVertices.WriteToFiler(writer);
    FNormals.WriteToFiler(writer);
    WriteBoolean(FVisible);
  end;
end;

procedure TgxBaseMeshObject.ReadFromFiler(reader: TgxVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion in [0 .. 1] then
    with reader do
    begin
      FName := ReadString;
      FVertices.ReadFromFiler(reader);
      FNormals.ReadFromFiler(reader);
      if archiveVersion >= 1 then
        FVisible := ReadBoolean
      else
        FVisible := True;
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TgxBaseMeshObject.Clear;
begin
  FNormals.Clear;
  FVertices.Clear;
end;

procedure TgxBaseMeshObject.ContributeToBarycenter(var currentSum: TAffineVector; var nb: Integer);
begin
  AddVector(currentSum, FVertices.Sum);
  nb := nb + FVertices.Count;
end;

procedure TgxBaseMeshObject.Translate(const delta: TAffineVector);
begin
  FVertices.Translate(delta);
end;

procedure TgxBaseMeshObject.BuildNormals(vertexIndices: TgxIntegerList; mode: TgxMeshObjectMode;
  normalIndices: TgxIntegerList = nil);
var
  i, base: Integer;
  n: TAffineVector;
  newNormals: TgxIntegerList;

  function TranslateNewNormal(vertexIndex: Integer; const delta: TAffineVector): Integer;
  var
    pv: PAffineVector;
  begin
    Result := newNormals[vertexIndex];
    if Result < base then
    begin
      Result := normals.Add(NullVector);
      newNormals[vertexIndex] := Result;
    end;
    pv := @normals.list[Result];
    AddVector(pv^, delta);
  end;

begin
  if not Assigned(normalIndices) then
  begin
    // build bijection
    normals.Clear;
    normals.Count := Vertices.Count;
    case mode of
      momTriangles:
        begin
          i := 0;
          while i <= vertexIndices.Count - 3 do
            with normals do
            begin
              with Vertices do
              begin
                CalcPlaneNormal(Items[vertexIndices[i + 0]], Items[vertexIndices[i + 1]], Items[vertexIndices[i + 2]], n);
              end;
              with normals do
              begin
                TranslateItem(vertexIndices[i + 0], n);
                TranslateItem(vertexIndices[i + 1], n);
                TranslateItem(vertexIndices[i + 2], n);
              end;
              Inc(i, 3);
            end;
        end;
      momTriangleStrip:
        begin
          i := 0;
          while i <= vertexIndices.Count - 3 do
            with normals do
            begin
              with Vertices do
              begin
                if (i and 1) = 0 then
                  CalcPlaneNormal(Items[vertexIndices[i + 0]], Items[vertexIndices[i + 1]], Items[vertexIndices[i + 2]], n)
                else
                  CalcPlaneNormal(Items[vertexIndices[i + 0]], Items[vertexIndices[i + 2]], Items[vertexIndices[i + 1]], n);
              end;
              with normals do
              begin
                TranslateItem(vertexIndices[i + 0], n);
                TranslateItem(vertexIndices[i + 1], n);
                TranslateItem(vertexIndices[i + 2], n);
              end;
              Inc(i, 1);
            end;
        end;
    else
      Assert(False);
    end;
    normals.normalize;
  end
  else
  begin
    // add new normals
    base := normals.Count;
    newNormals := TgxIntegerList.Create;
    newNormals.AddSerie(-1, 0, Vertices.Count);
    case mode of
      momTriangles:
        begin
          i := 0;
          while i <= vertexIndices.Count - 3 do
          begin
            with Vertices do
            begin
              CalcPlaneNormal(Items[vertexIndices[i + 0]], Items[vertexIndices[i + 1]], Items[vertexIndices[i + 2]], n);
            end;
            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 0], n));
            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 1], n));
            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 2], n));
            Inc(i, 3);
          end;
        end;
      momTriangleStrip:
        begin
          i := 0;
          while i <= vertexIndices.Count - 3 do
          begin
            with Vertices do
            begin
              if (i and 1) = 0 then
                CalcPlaneNormal(Items[vertexIndices[i + 0]], Items[vertexIndices[i + 1]], Items[vertexIndices[i + 2]], n)
              else
                CalcPlaneNormal(Items[vertexIndices[i + 0]], Items[vertexIndices[i + 2]], Items[vertexIndices[i + 1]], n);
            end;
            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 0], n));
            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 1], n));
            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 2], n));
            Inc(i, 1);
          end;
        end;
    else
      Assert(False);
    end;
    for i := base to normals.Count - 1 do
      NormalizeVector(normals.list^[i]);
    newNormals.Free;
  end;
end;

function TgxBaseMeshObject.ExtractTriangles(texCoords: TgxAffineVectorList = nil; normals: TgxAffineVectorList = nil)
  : TgxAffineVectorList;
begin
  Result := TgxAffineVectorList.Create;
  if (Vertices.Count mod 3) = 0 then
  begin
    Result.Assign(Vertices);
    if Assigned(normals) then
      normals.Assign(Self.normals);
  end;
end;

procedure TgxBaseMeshObject.SetVertices(const val: TgxAffineVectorList);
begin
  FVertices.Assign(val);
end;

procedure TgxBaseMeshObject.SetNormals(const val: TgxAffineVectorList);
begin
  FNormals.Assign(val);
end;

// ------------------
// ------------------ TgxSkeletonFrame ------------------
// ------------------

constructor TgxSkeletonFrame.CreateOwned(aOwner: TgxSkeletonFrameList);
begin
  FOwner := aOwner;
  aOwner.Add(Self);
  Create;
end;

constructor TgxSkeletonFrame.Create;
begin
  inherited Create;
  FPosition := TgxAffineVectorList.Create;
  FRotation := TgxAffineVectorList.Create;
  FQuaternion := TQuaternionList.Create;
  FTransformMode := sftRotation;
end;

destructor TgxSkeletonFrame.Destroy;
begin
  FlushLocalMatrixList;
  FRotation.Free;
  FPosition.Free;
  FQuaternion.Free;
  inherited Destroy;
end;

procedure TgxSkeletonFrame.WriteToFiler(writer: TgxVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(1); // Archive Version 1
    WriteString(FName);
    FPosition.WriteToFiler(writer);
    FRotation.WriteToFiler(writer);
    FQuaternion.WriteToFiler(writer);
    WriteInteger(Integer(FTransformMode));
  end;
end;

procedure TgxSkeletonFrame.ReadFromFiler(reader: TgxVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if (archiveVersion = 0) or (archiveVersion = 1) then
    with reader do
    begin
      FName := ReadString;
      FPosition.ReadFromFiler(reader);
      FRotation.ReadFromFiler(reader);
      if (archiveVersion = 1) then
      begin
        FQuaternion.ReadFromFiler(reader);
        FTransformMode := TgxSkeletonFrameTransform(ReadInteger);
      end;
    end
  else
    RaiseFilerException(archiveVersion);
  FlushLocalMatrixList;
end;

procedure TgxSkeletonFrame.SetPosition(const val: TgxAffineVectorList);
begin
  FPosition.Assign(val);
end;

procedure TgxSkeletonFrame.SetRotation(const val: TgxAffineVectorList);
begin
  FRotation.Assign(val);
end;

procedure TgxSkeletonFrame.SetQuaternion(const val: TQuaternionList);
begin
  FQuaternion.Assign(val);
end;

function TgxSkeletonFrame.LocalMatrixList: PMatrixArray;
var
  i: Integer;
  s, c: Single;
  mat, rmat: TMatrix4f;
  quat: TQuaternion;
begin
  if not Assigned(FLocalMatrixList) then
  begin
    case FTransformMode of
      sftRotation:
        begin
          FLocalMatrixList := AllocMem(SizeOf(TMatrix4f) * Rotation.Count);
          for i := 0 to Rotation.Count - 1 do
          begin
            if Rotation[i].X <> 0 then
            begin
              SinCosine(Rotation[i].X, s, c);
              mat := CreateRotationMatrixX(s, c);
            end
            else
              mat := IdentityHmgMatrix;
            if Rotation[i].Y <> 0 then
            begin
              SinCosine(Rotation[i].Y, s, c);
              rmat := CreateRotationMatrixY(s, c);
              mat := MatrixMultiply(mat, rmat);
            end;
            if Rotation[i].Z <> 0 then
            begin
              SinCosine(Rotation[i].Z, s, c);
              rmat := CreateRotationMatrixZ(s, c);
              mat := MatrixMultiply(mat, rmat);
            end;
            mat.W.X := Position[i].X;
            mat.W.Y := Position[i].Y;
            mat.W.Z := Position[i].Z;
            FLocalMatrixList^[i] := mat;
          end;
        end;
      sftQuaternion:
        begin
          FLocalMatrixList := AllocMem(SizeOf(TMatrix4f) * Quaternion.Count);
          for i := 0 to Quaternion.Count - 1 do
          begin
            quat := Quaternion[i];
            mat := QuaternionToMatrix(quat);
            mat.W.X := Position[i].X;
            mat.W.Y := Position[i].Y;
            mat.W.Z := Position[i].Z;
            mat.W.W := 1;
            FLocalMatrixList^[i] := mat;
          end;
        end;
    end;
  end;
  Result := FLocalMatrixList;
end;

procedure TgxSkeletonFrame.FlushLocalMatrixList;
begin
  if Assigned(FLocalMatrixList) then
  begin
    FreeMem(FLocalMatrixList);
    FLocalMatrixList := nil;
  end;
end;

procedure TgxSkeletonFrame.ConvertQuaternionsToRotations(KeepQuaternions: Boolean = True);
var
  i: Integer;
  t: TTransformations;
  m: TMatrix4f;
begin
  Rotation.Clear;
  for i := 0 to Quaternion.Count - 1 do
  begin
    m := QuaternionToMatrix(Quaternion[i]);
    if MatrixDecompose(m, t) then
      Rotation.Add(t[ttRotateX], t[ttRotateY], t[ttRotateZ])
    else
      Rotation.Add(NullVector);
  end;
  if not KeepQuaternions then
    Quaternion.Clear;
end;

procedure TgxSkeletonFrame.ConvertRotationsToQuaternions(KeepRotations: Boolean = True);
var
  i: Integer;
  mat, rmat: TMatrix4f;
  s, c: Single;
begin
  Quaternion.Clear;
  for i := 0 to Rotation.Count - 1 do
  begin
    mat := IdentityHmgMatrix;
    SinCosine(Rotation[i].X, s, c);
    rmat := CreateRotationMatrixX(s, c);
    mat := MatrixMultiply(mat, rmat);
    SinCosine(Rotation[i].Y, s, c);
    rmat := CreateRotationMatrixY(s, c);
    mat := MatrixMultiply(mat, rmat);
    SinCosine(Rotation[i].Z, s, c);
    rmat := CreateRotationMatrixZ(s, c);
    mat := MatrixMultiply(mat, rmat);
    Quaternion.Add(QuaternionFromMatrix(mat));
  end;
  if not KeepRotations then
    Rotation.Clear;
end;

// ------------------
// ------------------ TgxSkeletonFrameList ------------------
// ------------------

constructor TgxSkeletonFrameList.CreateOwned(aOwner: TPersistent);
begin
  FOwner := aOwner;
  Create;
end;

destructor TgxSkeletonFrameList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TgxSkeletonFrameList.ReadFromFiler(reader: TgxVirtualReader);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

procedure TgxSkeletonFrameList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      FOwner := nil;
      Free;
    end;
  inherited;
end;

function TgxSkeletonFrameList.GetSkeletonFrame(Index: Integer): TgxSkeletonFrame;
begin
  Result := TgxSkeletonFrame(list^[Index]);
end;

procedure TgxSkeletonFrameList.ConvertQuaternionsToRotations(KeepQuaternions: Boolean = True; SetTransformMode: Boolean = True);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].ConvertQuaternionsToRotations(KeepQuaternions);
    if SetTransformMode then
      Items[i].TransformMode := sftRotation;
  end;
end;

procedure TgxSkeletonFrameList.ConvertRotationsToQuaternions(KeepRotations: Boolean = True; SetTransformMode: Boolean = True);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].ConvertRotationsToQuaternions(KeepRotations);
    if SetTransformMode then
      Items[i].TransformMode := sftQuaternion;
  end;
end;

// ------------------
// ------------------ TgxSkeletonBoneList ------------------
// ------------------

constructor TgxSkeletonBoneList.CreateOwned(aOwner: TgxSkeleton);
begin
  FSkeleton := aOwner;
  Create;
end;

constructor TgxSkeletonBoneList.Create;
begin
  inherited;
  FGlobalMatrix := IdentityHmgMatrix;
end;

destructor TgxSkeletonBoneList.Destroy;
begin
  Clean;
  inherited;
end;

procedure TgxSkeletonBoneList.WriteToFiler(writer: TgxVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    // nothing, yet
  end;
end;

procedure TgxSkeletonBoneList.ReadFromFiler(reader: TgxVirtualReader);
var
  archiveVersion, i: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      // nothing, yet
    end
  else
    RaiseFilerException(archiveVersion);
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

procedure TgxSkeletonBoneList.AfterObjectCreatedByReader(Sender: TObject);
begin
  with (Sender as TgxSkeletonBone) do
  begin
    FOwner := Self;
    FSkeleton := Self.Skeleton;
  end;
end;

function TgxSkeletonBoneList.GetSkeletonBone(Index: Integer): TgxSkeletonBone;
begin
  Result := TgxSkeletonBone(list^[Index]);
end;

function TgxSkeletonBoneList.BoneByID(anID: Integer): TgxSkeletonBone;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].BoneByID(anID);
    if Assigned(Result) then
      Break;
  end;
end;

function TgxSkeletonBoneList.BoneByName(const aName: string): TgxSkeletonBone;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].BoneByName(aName);
    if Assigned(Result) then
      Break;
  end;
end;

function TgxSkeletonBoneList.BoneCount: Integer;
var
  i: Integer;
begin
  Result := 1;
  for i := 0 to Count - 1 do
    Inc(Result, Items[i].BoneCount);
end;

procedure TgxSkeletonBoneList.PrepareGlobalMatrices;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].PrepareGlobalMatrices;
end;

// ------------------
// ------------------ TgxSkeletonRootBoneList ------------------
// ------------------

procedure TgxSkeletonRootBoneList.WriteToFiler(writer: TgxVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    // nothing, yet
  end;
end;

procedure TgxSkeletonRootBoneList.ReadFromFiler(reader: TgxVirtualReader);
var
  archiveVersion, i: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      // nothing, yet
    end
  else
    RaiseFilerException(archiveVersion);
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

procedure TgxSkeletonRootBoneList.BuildList(var mrci: TgxRenderContextInfo);
var
  i: Integer;
begin
  // root node setups and restore OpenGL stuff
  mrci.gxStates.Disable(stColorMaterial);
  mrci.gxStates.Disable(stLighting);
  glColor3f(1, 1, 1);
  // render root-bones
  for i := 0 to Count - 1 do
    Items[i].BuildList(mrci);
end;

// ------------------
// ------------------ TgxSkeletonBone ------------------
// ------------------

constructor TgxSkeletonBone.CreateOwned(aOwner: TgxSkeletonBoneList);
begin
  FOwner := aOwner;
  aOwner.Add(Self);
  FSkeleton := aOwner.Skeleton;
  Create;
end;

constructor TgxSkeletonBone.Create;
begin
  FColor := $FFFFFFFF; // opaque white
  inherited;
end;

destructor TgxSkeletonBone.Destroy;
begin
  if Assigned(Owner) then
    Owner.Remove(Self);
  inherited Destroy;
end;

procedure TgxSkeletonBone.WriteToFiler(writer: TgxVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteString(FName);
    WriteInteger(FBoneID);
    WriteInteger(Integer(FColor));
  end;
end;

procedure TgxSkeletonBone.ReadFromFiler(reader: TgxVirtualReader);
var
  archiveVersion, i: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FName := ReadString;
      FBoneID := ReadInteger;
      FColor := Cardinal(ReadInteger);
    end
  else
    RaiseFilerException(archiveVersion);
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

procedure TgxSkeletonBone.BuildList(var mrci: TgxRenderContextInfo);

  procedure IssueColor(Color: Cardinal);
  begin
    glColor4f(GetRValue(Color) / 255, GetGValue(Color) / 255, GetBValue(Color) / 255, ((Color shr 24) and 255) / 255);
  end;

var
  i: Integer;
begin
  // point for self
  mrci.gxStates.PointSize := 5;
  glBegin(GL_POINTS);
  IssueColor(Color);
  glVertex3fv(@GlobalMatrix.W.X);
  glEnd;
  // parent-self bone line
  if Owner is TgxSkeletonBone then
  begin
    glBegin(GL_LINES);
    glVertex3fv(@TgxSkeletonBone(Owner).GlobalMatrix.W.X);
    glVertex3fv(@GlobalMatrix.W.X);
    glEnd;
  end;
  // render sub-bones
  for i := 0 to Count - 1 do
    Items[i].BuildList(mrci);
end;

function TgxSkeletonBone.GetSkeletonBone(Index: Integer): TgxSkeletonBone;
begin
  Result := TgxSkeletonBone(list^[Index]);
end;

procedure TgxSkeletonBone.SetColor(const val: Cardinal);
begin
  FColor := val;
end;

function TgxSkeletonBone.BoneByID(anID: Integer): TgxSkeletonBone;
begin
  if BoneID = anID then
    Result := Self
  else
    Result := inherited BoneByID(anID);
end;

function TgxSkeletonBone.BoneByName(const aName: string): TgxSkeletonBone;
begin
  if Name = aName then
    Result := Self
  else
    Result := inherited BoneByName(aName);
end;

procedure TgxSkeletonBone.Clean;
begin
  BoneID := 0;
  Name := '';
  inherited;
end;

procedure TgxSkeletonBone.PrepareGlobalMatrices;
begin
  if (Skeleton.FRagDollEnabled) then
    Exit; // ragdoll
  FGlobalMatrix := MatrixMultiply(Skeleton.CurrentFrame.LocalMatrixList^[BoneID], TgxSkeletonBoneList(Owner).FGlobalMatrix);
  inherited;
end;

procedure TgxSkeletonBone.SetGlobalMatrix(Matrix: TMatrix4f); // ragdoll
begin
  FGlobalMatrix := Matrix;
end;

procedure TgxSkeletonBone.SetGlobalMatrixForRagDoll(RagDollMatrix: TMatrix4f);
// ragdoll
begin
  FGlobalMatrix := MatrixMultiply(RagDollMatrix, Skeleton.Owner.InvAbsoluteMatrix);
  inherited;
end;

// ------------------
// ------------------ TgxSkeletonCollider ------------------
// ------------------

constructor TgxSkeletonCollider.Create;
begin
  inherited;
  FLocalMatrix := IdentityHmgMatrix;
  FGlobalMatrix := IdentityHmgMatrix;
  FAutoUpdate := True;
end;

constructor TgxSkeletonCollider.CreateOwned(aOwner: TgxSkeletonColliderList);
begin
  Create;
  FOwner := aOwner;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

procedure TgxSkeletonCollider.WriteToFiler(writer: TgxVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    if Assigned(FBone) then
      WriteInteger(FBone.BoneID)
    else
      WriteInteger(-1);
    Write(FLocalMatrix, SizeOf(TMatrix4f));
  end;
end;

procedure TgxSkeletonCollider.ReadFromFiler(reader: TgxVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FBoneID := ReadInteger;
      Read(FLocalMatrix, SizeOf(TMatrix4f));
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TgxSkeletonCollider.AlignCollider;
var
  mat: TMatrix4f;
begin
  if Assigned(FBone) then
  begin
    if Owner.Owner is TgxSkeleton then
      if TgxSkeleton(Owner.Owner).Owner is TgxBaseSceneObject then
        mat := MatrixMultiply(FBone.GlobalMatrix, TgxBaseSceneObject(TgxSkeleton(Owner.Owner).Owner).AbsoluteMatrix)
      else
        mat := FBone.GlobalMatrix;
    MatrixMultiply(FLocalMatrix, mat, FGlobalMatrix);
  end
  else
    FGlobalMatrix := FLocalMatrix;
end;

procedure TgxSkeletonCollider.SetBone(const val: TgxSkeletonBone);
begin
  if val <> FBone then
    FBone := val;
end;

procedure TgxSkeletonCollider.SetLocalMatrix(const val: TMatrix4f);
begin
  FLocalMatrix := val;
end;

// ------------------
// ------------------ TgxSkeletonColliderList ------------------
// ------------------

constructor TgxSkeletonColliderList.CreateOwned(aOwner: TPersistent);
begin
  Create;
  FOwner := aOwner;
end;

destructor TgxSkeletonColliderList.Destroy;
begin
  Clear;
  inherited;
end;

function TgxSkeletonColliderList.GetSkeletonCollider(Index: Integer): TgxSkeletonCollider;
begin
  Result := TgxSkeletonCollider(inherited Get(index));
end;

procedure TgxSkeletonColliderList.ReadFromFiler(reader: TgxVirtualReader);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Count - 1 do
  begin
    Items[i].FOwner := Self;
    if (Owner is TgxSkeleton) and (Items[i].FBoneID <> -1) then
      Items[i].Bone := TgxSkeleton(Owner).BoneByID(Items[i].FBoneID);
  end;
end;

procedure TgxSkeletonColliderList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].FOwner := nil;
    Items[i].Free;
  end;
  inherited;
end;

procedure TgxSkeletonColliderList.AlignColliders;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].AutoUpdate then
      Items[i].AlignCollider;
end;

// ------------------
// ------------------ TgxSkeleton ------------------
// ------------------

constructor TgxSkeleton.CreateOwned(aOwner: TgxBaseMesh);
begin
  FOwner := aOwner;
  Create;
end;

constructor TgxSkeleton.Create;
begin
  inherited Create;
  FRootBones := TgxSkeletonRootBoneList.CreateOwned(Self);
  FFrames := TgxSkeletonFrameList.CreateOwned(Self);
  FColliders := TgxSkeletonColliderList.CreateOwned(Self);
end;

destructor TgxSkeleton.Destroy;
begin
  FlushBoneByIDCache;
  FCurrentFrame.Free;
  FFrames.Free;
  FRootBones.Free;
  FColliders.Free;
  inherited Destroy;
end;

procedure TgxSkeleton.WriteToFiler(writer: TgxVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    if FColliders.Count > 0 then
      WriteInteger(1) // Archive Version 1 : with colliders
    else
      WriteInteger(0); // Archive Version 0
    FRootBones.WriteToFiler(writer);
    FFrames.WriteToFiler(writer);
    if FColliders.Count > 0 then
      FColliders.WriteToFiler(writer);
  end;
end;

procedure TgxSkeleton.ReadFromFiler(reader: TgxVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if (archiveVersion = 0) or (archiveVersion = 1) then
    with reader do
    begin
      FRootBones.ReadFromFiler(reader);
      FFrames.ReadFromFiler(reader);
      if (archiveVersion = 1) then
        FColliders.ReadFromFiler(reader);
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TgxSkeleton.SetRootBones(const val: TgxSkeletonRootBoneList);
begin
  FRootBones.Assign(val);
end;

procedure TgxSkeleton.SetFrames(const val: TgxSkeletonFrameList);
begin
  FFrames.Assign(val);
end;

function TgxSkeleton.GetCurrentFrame: TgxSkeletonFrame;
begin
  if not Assigned(FCurrentFrame) then
    FCurrentFrame := TgxSkeletonFrame(FFrames.Items[0].CreateClone);
  Result := FCurrentFrame;
end;

procedure TgxSkeleton.SetCurrentFrame(val: TgxSkeletonFrame);
begin
  if Assigned(FCurrentFrame) then
    FCurrentFrame.Free;
  FCurrentFrame := TgxSkeletonFrame(val.CreateClone);
end;

procedure TgxSkeleton.SetColliders(const val: TgxSkeletonColliderList);
begin
  FColliders.Assign(val);
end;

procedure TgxSkeleton.FlushBoneByIDCache;
begin
  FBonesByIDCache.Free;
  FBonesByIDCache := nil;
end;

function TgxSkeleton.BoneByID(anID: Integer): TgxSkeletonBone;

  procedure CollectBones(Bone: TgxSkeletonBone);
  var
    i: Integer;
  begin
    if Bone.BoneID >= FBonesByIDCache.Count then
      FBonesByIDCache.Count := Bone.BoneID + 1;
    FBonesByIDCache[Bone.BoneID] := Bone;
    for i := 0 to Bone.Count - 1 do
      CollectBones(Bone[i]);
  end;

var
  i: Integer;
begin
  if not Assigned(FBonesByIDCache) then
  begin
    FBonesByIDCache := TList.Create;
    for i := 0 to RootBones.Count - 1 do
      CollectBones(RootBones[i]);
  end;
  Result := TgxSkeletonBone(FBonesByIDCache[anID])
end;

function TgxSkeleton.BoneByName(const aName: string): TgxSkeletonBone;
begin
  Result := RootBones.BoneByName(aName);
end;

function TgxSkeleton.BoneCount: Integer;
begin
  Result := RootBones.BoneCount;
end;

procedure TgxSkeleton.MorphTo(frameIndex: Integer);
begin
  CurrentFrame := Frames[frameIndex];
end;

procedure TgxSkeleton.MorphTo(frame: TgxSkeletonFrame);
begin
  CurrentFrame := frame;
end;

procedure TgxSkeleton.Lerp(frameIndex1, frameIndex2: Integer; lerpFactor: Single);
begin
  if Assigned(FCurrentFrame) then
    FCurrentFrame.Free;
  FCurrentFrame := TgxSkeletonFrame.Create;
  FCurrentFrame.TransformMode := Frames[frameIndex1].TransformMode;
  with FCurrentFrame do
  begin
    Position.Lerp(Frames[frameIndex1].Position, Frames[frameIndex2].Position, lerpFactor);
    case TransformMode of
      sftRotation:
        Rotation.AngleLerp(Frames[frameIndex1].Rotation, Frames[frameIndex2].Rotation, lerpFactor);
      sftQuaternion:
        Quaternion.Lerp(Frames[frameIndex1].Quaternion, Frames[frameIndex2].Quaternion, lerpFactor);
    end;
  end;
end;

procedure TgxSkeleton.BlendedLerps(const lerpInfos: array of TgxBlendedLerpInfo);
var
  i, n: Integer;
  blendPositions: TgxAffineVectorList;
  blendRotations: TgxAffineVectorList;
  blendQuaternions: TQuaternionList;
begin
  n := High(lerpInfos) - Low(lerpInfos) + 1;
  Assert(n >= 1);
  i := Low(lerpInfos);
  if n = 1 then
  begin
    // use fast lerp (no blend)
    with lerpInfos[i] do
      Lerp(frameIndex1, frameIndex2, lerpFactor);
  end
  else
  begin
    if Assigned(FCurrentFrame) then
      FCurrentFrame.Free;
    FCurrentFrame := TgxSkeletonFrame.Create;
    FCurrentFrame.TransformMode := Frames[lerpInfos[i].frameIndex1].TransformMode;
    with FCurrentFrame do
    begin
      blendPositions := TgxAffineVectorList.Create;
      // lerp first item separately
      Position.Lerp(Frames[lerpInfos[i].frameIndex1].Position, Frames[lerpInfos[i].frameIndex2].Position,
        lerpInfos[i].lerpFactor);
      if lerpInfos[i].weight <> 1 then
        Position.Scale(lerpInfos[i].weight);

      Inc(i);
      // combine the other items
      while i <= High(lerpInfos) do
      begin
        if not Assigned(lerpInfos[i].externalPositions) then
        begin
          blendPositions.Lerp(Frames[lerpInfos[i].frameIndex1].Position, Frames[lerpInfos[i].frameIndex2].Position,
            lerpInfos[i].lerpFactor);
          Position.AngleCombine(blendPositions, 1);
        end
        else
          Position.Combine(lerpInfos[i].externalPositions, 1);
        Inc(i);
      end;
      blendPositions.Free;

      i := Low(lerpInfos);
      case TransformMode of
        sftRotation:
          begin
            blendRotations := TgxAffineVectorList.Create;
            // lerp first item separately
            Rotation.AngleLerp(Frames[lerpInfos[i].frameIndex1].Rotation, Frames[lerpInfos[i].frameIndex2].Rotation,
              lerpInfos[i].lerpFactor);
            Inc(i);
            // combine the other items
            while i <= High(lerpInfos) do
            begin
              if not Assigned(lerpInfos[i].externalRotations) then
              begin
                blendRotations.AngleLerp(Frames[lerpInfos[i].frameIndex1].Rotation, Frames[lerpInfos[i].frameIndex2].Rotation,
                  lerpInfos[i].lerpFactor);
                Rotation.AngleCombine(blendRotations, 1);
              end
              else
                Rotation.AngleCombine(lerpInfos[i].externalRotations, 1);
              Inc(i);
            end;
            blendRotations.Free;
          end;

        sftQuaternion:
          begin
            blendQuaternions := TQuaternionList.Create;
            // Initial frame lerp
            Quaternion.Lerp(Frames[lerpInfos[i].frameIndex1].Quaternion, Frames[lerpInfos[i].frameIndex2].Quaternion,
              lerpInfos[i].lerpFactor);
            Inc(i);
            // Combine the lerped frames together
            while i <= High(lerpInfos) do
            begin
              if not Assigned(lerpInfos[i].externalQuaternions) then
              begin
                blendQuaternions.Lerp(Frames[lerpInfos[i].frameIndex1].Quaternion, Frames[lerpInfos[i].frameIndex2].Quaternion,
                  lerpInfos[i].lerpFactor);
                Quaternion.Combine(blendQuaternions, 1);
              end
              else
                Quaternion.Combine(lerpInfos[i].externalQuaternions, 1);
              Inc(i);
            end;
            blendQuaternions.Free;
          end;
      end;
    end;
  end;
end;

procedure TgxSkeleton.MakeSkeletalTranslationStatic(startFrame, endFrame: Integer);
var
  delta: TAffineVector;
  i: Integer;
  f: Single;
begin
  if endFrame <= startFrame then
    Exit;
  delta := VectorSubtract(Frames[endFrame].Position[0], Frames[startFrame].Position[0]);
  f := -1 / (endFrame - startFrame);
  for i := startFrame to endFrame do
    Frames[i].Position[0] := VectorCombine(Frames[i].Position[0], delta, 1, (i - startFrame) * f);
end;

procedure TgxSkeleton.MakeSkeletalRotationDelta(startFrame, endFrame: Integer);
var
  i, j: Integer;
  v: TAffineVector;
begin
  if endFrame <= startFrame then
    Exit;
  for i := startFrame to endFrame do
  begin
    for j := 0 to Frames[i].Position.Count - 1 do
    begin
      Frames[i].Position[j] := NullVector;
      v := VectorSubtract(Frames[i].Rotation[j], Frames[0].Rotation[j]);
      if VectorNorm(v) < 1E-6 then
        Frames[i].Rotation[j] := NullVector
      else
        Frames[i].Rotation[j] := v;
    end;
  end;
end;

procedure TgxSkeleton.MorphMesh(normalize: Boolean);
var
  i: Integer;
  Mesh: TgxBaseMeshObject;
begin
  if Owner.MeshObjects.Count > 0 then
  begin
    RootBones.PrepareGlobalMatrices;
    if Colliders.Count > 0 then
      Colliders.AlignColliders;

    if FMorphInvisibleParts then
      for i := 0 to Owner.MeshObjects.Count - 1 do
      begin
        Mesh := Owner.MeshObjects.Items[i];
        if (Mesh is TgxSkeletonMeshObject) then
          TgxSkeletonMeshObject(Mesh).ApplyCurrentSkeletonFrame(normalize);
      end
    else
      for i := 0 to Owner.MeshObjects.Count - 1 do
      begin
        Mesh := Owner.MeshObjects.Items[i];
        if (Mesh is TgxSkeletonMeshObject) and Mesh.Visible then
          TgxSkeletonMeshObject(Mesh).ApplyCurrentSkeletonFrame(normalize);
      end
  end;
end;

procedure TgxSkeleton.Synchronize(reference: TgxSkeleton);
begin
  CurrentFrame.Assign(reference.CurrentFrame);
  MorphMesh(True);
end;

procedure TgxSkeleton.Clear;
begin
  FlushBoneByIDCache;
  RootBones.Clean;
  Frames.Clear;
  FCurrentFrame.Free;
  FCurrentFrame := nil;
  FColliders.Clear;
end;

procedure TgxSkeleton.StartRagdoll; // ragdoll
var
  i: Integer;
  Mesh: TgxBaseMeshObject;
begin
  if FRagDollEnabled then
    Exit
  else
    FRagDollEnabled := True;

  if Owner.MeshObjects.Count > 0 then
  begin
    for i := 0 to Owner.MeshObjects.Count - 1 do
    begin
      Mesh := Owner.MeshObjects.Items[i];
      if Mesh is TgxSkeletonMeshObject then
      begin
        TgxSkeletonMeshObject(Mesh).BackupBoneMatrixInvertedMeshes;
        TgxSkeletonMeshObject(Mesh).PrepareBoneMatrixInvertedMeshes;
      end;
    end;
  end;
end;

procedure TgxSkeleton.StopRagdoll; // ragdoll
var
  i: Integer;
  Mesh: TgxBaseMeshObject;
begin
  FRagDollEnabled := False;
  if Owner.MeshObjects.Count > 0 then
  begin
    for i := 0 to Owner.MeshObjects.Count - 1 do
    begin
      Mesh := Owner.MeshObjects.Items[i];
      if Mesh is TgxSkeletonMeshObject then
        TgxSkeletonMeshObject(Mesh).RestoreBoneMatrixInvertedMeshes;
    end;
  end;
end;

// ------------------
// ------------------ TgxMeshObject ------------------
// ------------------

constructor TgxMeshObject.CreateOwned(aOwner: TgxMeshObjectList);
begin
  FOwner := aOwner;
  Create;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

constructor TgxMeshObject.Create;
begin
  FMode := momTriangles;
  FTexCoords := TgxAffineVectorList.Create;
  FLightMapTexCoords := TgxAffineVectorList.Create;
  FColors := TgxVectorList.Create;
  FFaceGroups := TgxFaceGroups.CreateOwned(Self);
  FTexCoordsEx := TList.Create;
  FTangentsTexCoordIndex := 1;
  FBinormalsTexCoordIndex := 2;

  FUseVBO := vVectorFileObjectsEnableVBOByDefault;
  inherited;
end;

destructor TgxMeshObject.Destroy;
var
  i: Integer;
begin
  FVerticesVBO.Free;
  FNormalsVBO.Free;
  FColorsVBO.Free;
  for i := 0 to high(FTexCoordsVBO) do
    FTexCoordsVBO[i].Free;
  FLightmapTexCoordsVBO.Free;

  FFaceGroups.Free;
  FColors.Free;
  FTexCoords.Free;
  FLightMapTexCoords.Free;
  for i := 0 to FTexCoordsEx.Count - 1 do
    TgxVectorList(FTexCoordsEx[i]).Free;
  FTexCoordsEx.Free;
  if Assigned(FOwner) then
    FOwner.Remove(Self);
  inherited;
end;

procedure TgxMeshObject.Assign(Source: TPersistent);
var
  i: Integer;
begin
  inherited Assign(Source);

  if Source is TgxMeshObject then
  begin
    FTexCoords.Assign(TgxMeshObject(Source).FTexCoords);
    FLightMapTexCoords.Assign(TgxMeshObject(Source).FLightMapTexCoords);
    FColors.Assign(TgxMeshObject(Source).FColors);
    FFaceGroups.Assign(TgxMeshObject(Source).FFaceGroups);
    FMode := TgxMeshObject(Source).FMode;
    FRenderingOptions := TgxMeshObject(Source).FRenderingOptions;
    FBinormalsTexCoordIndex := TgxMeshObject(Source).FBinormalsTexCoordIndex;
    FTangentsTexCoordIndex := TgxMeshObject(Source).FTangentsTexCoordIndex;

    // Clear FTexCoordsEx.
    for i := 0 to FTexCoordsEx.Count - 1 do
      TgxVectorList(FTexCoordsEx[i]).Free;

    FTexCoordsEx.Count := TgxMeshObject(Source).FTexCoordsEx.Count;

    // Fill FTexCoordsEx.
    for i := 0 to FTexCoordsEx.Count - 1 do
    begin
      FTexCoordsEx[i] := TgxVectorList.Create;
      TgxVectorList(FTexCoordsEx[i]).Assign(TgxMeshObject(Source).FTexCoordsEx[i]);
    end;
  end;
end;

procedure TgxMeshObject.WriteToFiler(writer: TgxVirtualWriter);
var
  i: Integer;
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(3); // Archive Version 3
    FTexCoords.WriteToFiler(writer);
    FLightMapTexCoords.WriteToFiler(writer);
    FColors.WriteToFiler(writer);
    FFaceGroups.WriteToFiler(writer);
    WriteInteger(Integer(FMode));
    WriteInteger(SizeOf(FRenderingOptions));
    Write(FRenderingOptions, SizeOf(FRenderingOptions));
    WriteInteger(FTexCoordsEx.Count);
    for i := 0 to FTexCoordsEx.Count - 1 do
      TexCoordsEx[i].WriteToFiler(writer);
    WriteInteger(BinormalsTexCoordIndex);
    WriteInteger(TangentsTexCoordIndex);
  end;
end;

procedure TgxMeshObject.ReadFromFiler(reader: TgxVirtualReader);
var
  i, Count, archiveVersion: Integer;
  lOldLightMapTexCoords: TgxTexPointList;
  tc: TTexPoint;
  size, ro: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion in [0 .. 3] then
    with reader do
    begin
      FTexCoords.ReadFromFiler(reader);

      if archiveVersion = 0 then
      begin
        // FLightMapTexCoords did not exist back than.
        FLightMapTexCoords.Clear;
      end
      else if (archiveVersion = 1) or (archiveVersion = 2) then
      begin
        lOldLightMapTexCoords := TgxTexPointList.CreateFromFiler(reader);
        for i := 0 to lOldLightMapTexCoords.Count - 1 do
        begin
          tc := lOldLightMapTexCoords[i];
          FLightMapTexCoords.Add(tc.s, tc.t);
        end;
        lOldLightMapTexCoords.Free;
      end
      else
      begin
        // Load FLightMapTexCoords the normal way.
        FLightMapTexCoords.ReadFromFiler(reader);
      end;

      FColors.ReadFromFiler(reader);
      FFaceGroups.ReadFromFiler(reader);
      FMode := TgxMeshObjectMode(ReadInteger);
      size := ReadInteger;
      ro := 0;
      Read(ro, size);
      FRenderingOptions := TgxMeshObjectRenderingOptions(Byte(ro));
      if archiveVersion >= 2 then
      begin
        Count := ReadInteger;
        for i := 0 to Count - 1 do
          TexCoordsEx[i].ReadFromFiler(reader);
        BinormalsTexCoordIndex := ReadInteger;
        TangentsTexCoordIndex := ReadInteger;
      end;
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TgxMeshObject.Clear;
var
  i: Integer;
begin
  inherited;
  FFaceGroups.Clear;
  FColors.Clear;
  FTexCoords.Clear;
  FLightMapTexCoords.Clear;
  for i := 0 to FTexCoordsEx.Count - 1 do
    TexCoordsEx[i].Clear;
end;

function TgxMeshObject.ExtractTriangles(texCoords: TgxAffineVectorList = nil; normals: TgxAffineVectorList = nil)
  : TgxAffineVectorList;
begin
  case mode of
    momTriangles:
      begin
        Result := inherited ExtractTriangles;
        if Assigned(texCoords) then
          texCoords.Assign(Self.texCoords);
        if Assigned(normals) then
          normals.Assign(Self.normals);
      end;
    momTriangleStrip:
      begin
        Result := TgxAffineVectorList.Create;
        ConvertStripToList(Vertices, Result);
        if Assigned(texCoords) then
          ConvertStripToList(Self.texCoords, texCoords);
        if Assigned(normals) then
          ConvertStripToList(Self.normals, normals);
      end;
    momFaceGroups:
      begin
        Result := TgxAffineVectorList.Create;
        FaceGroups.AddToTriangles(Result, texCoords, normals);
      end;
  else
    Result := nil;
    Assert(False);
  end;
end;

function TgxMeshObject.TriangleCount: Integer;
var
  i: Integer;
begin
  case mode of
    momTriangles:
      Result := (Vertices.Count div 3);
    momTriangleStrip:
      begin
        Result := Vertices.Count - 2;
        if Result < 0 then
          Result := 0;
      end;
    momFaceGroups:
      begin
        Result := 0;
        for i := 0 to FaceGroups.Count - 1 do
          Result := Result + FaceGroups[i].TriangleCount;
      end;
  else
    Result := 0;
    Assert(False);
  end;
end;

procedure TgxMeshObject.PrepareMaterialLibraryCache(matLib: TgxMaterialLibrary);
begin
  FaceGroups.PrepareMaterialLibraryCache(matLib);
end;

procedure TgxMeshObject.DropMaterialLibraryCache;
begin
  FaceGroups.DropMaterialLibraryCache;
end;

procedure TgxMeshObject.GetExtents(out min, max: TAffineVector);
begin
  if FVertices.Revision <> FExtentCacheRevision then
  begin
    FVertices.GetExtents(FExtentCache.min, FExtentCache.max);
    FExtentCacheRevision := FVertices.Revision;
  end;
  min := FExtentCache.min;
  max := FExtentCache.max;
end;

procedure TgxMeshObject.GetExtents(out aabb: TAABB);
begin
  if FVertices.Revision <> FExtentCacheRevision then
  begin
    FVertices.GetExtents(FExtentCache.min, FExtentCache.max);
    FExtentCacheRevision := FVertices.Revision;
  end;
  aabb := FExtentCache;
end;

function TgxMeshObject.GetBarycenter: TVector4f;
var
  dMin, dMax: TAffineVector;
begin
  GetExtents(dMin, dMax);

  Result.X := (dMin.X + dMax.X) / 2;
  Result.Y := (dMin.Y + dMax.Y) / 2;
  Result.Z := (dMin.Z + dMax.Z) / 2;
  Result.W := 0;
end;

procedure TgxMeshObject.Prepare;
var
  i: Integer;
begin
  ValidBuffers := [];
  for i := 0 to FaceGroups.Count - 1 do
    FaceGroups[i].Prepare;
end;

function TgxMeshObject.PointInObject(const aPoint: TAffineVector): Boolean;
var
  min, max: TAffineVector;
begin
  GetExtents(min, max);
  Result := (aPoint.X >= min.X) and (aPoint.Y >= min.Y) and (aPoint.Z >= min.Z) and (aPoint.X <= max.X) and (aPoint.Y <= max.Y)
    and (aPoint.Z <= max.Z);
end;

procedure TgxMeshObject.SetTexCoords(const val: TgxAffineVectorList);
begin
  FTexCoords.Assign(val);
end;

procedure TgxMeshObject.SetLightmapTexCoords(const val: TgxAffineVectorList);
begin
  FLightMapTexCoords.Assign(val);
end;

procedure TgxMeshObject.SetColors(const val: TgxVectorList);
begin
  FColors.Assign(val);
end;

procedure TgxMeshObject.SetTexCoordsEx(Index: Integer; const val: TgxVectorList);
begin
  TexCoordsEx[index].Assign(val);
end;

function TgxMeshObject.GetTexCoordsEx(Index: Integer): TgxVectorList;
var
  i: Integer;
begin
  if index > FTexCoordsEx.Count - 1 then
    for i := FTexCoordsEx.Count - 1 to index do
      FTexCoordsEx.Add(TgxVectorList.Create);
  Result := TgxVectorList(FTexCoordsEx[index]);
end;

procedure TgxMeshObject.SetBinormals(const val: TgxVectorList);
begin
  Binormals.Assign(val);
end;

function TgxMeshObject.GetBinormals: TgxVectorList;
begin
  Result := TexCoordsEx[BinormalsTexCoordIndex];
end;

procedure TgxMeshObject.SetBinormalsTexCoordIndex(const val: Integer);
begin
  Assert(val >= 0);
  if val <> FBinormalsTexCoordIndex then
  begin
    FBinormalsTexCoordIndex := val;
  end;
end;

procedure TgxMeshObject.SetTangents(const val: TgxVectorList);
begin
  Tangents.Assign(val);
end;

function TgxMeshObject.GetTangents: TgxVectorList;
begin
  Result := TexCoordsEx[TangentsTexCoordIndex];
end;

procedure TgxMeshObject.SetTangentsTexCoordIndex(const val: Integer);
begin
  Assert(val >= 0);
  if val <> FTangentsTexCoordIndex then
  begin
    FTangentsTexCoordIndex := val;
  end;
end;

procedure TgxMeshObject.GetTriangleData(tri: Integer; list: TgxAffineVectorList; var v0, v1, v2: TAffineVector);
var
  i, LastCount, Count: Integer;
  fg: TfgxVertexIndexList;
begin
  case mode of
    momTriangles:
      begin
        v0 := list[3 * tri];
        v1 := list[3 * tri + 1];
        v2 := list[3 * tri + 2];
      end;
    momTriangleStrip:
      begin
        v0 := list[tri];
        v1 := list[tri + 1];
        v2 := list[tri + 2];
      end;
    momFaceGroups:
      begin
        Count := 0;
        for i := 0 to FaceGroups.Count - 1 do
        begin
          LastCount := Count;
          fg := TfgxVertexIndexList(FaceGroups[i]);
          Count := Count + fg.TriangleCount;
          if Count > tri then
          begin
            Count := tri - LastCount;
            case fg.mode of
              fgmmTriangles, fgmmFlatTriangles:
                begin
                  v0 := list[fg.vertexIndices[3 * Count]];
                  v1 := list[fg.vertexIndices[3 * Count + 1]];
                  v2 := list[fg.vertexIndices[3 * Count + 2]];
                end;
              fgmmTriangleStrip:
                begin
                  v0 := list[fg.vertexIndices[Count]];
                  v1 := list[fg.vertexIndices[Count + 1]];
                  v2 := list[fg.vertexIndices[Count + 2]];
                end;
              fgmmTriangleFan:
                begin
                  v0 := list[fg.vertexIndices[0]];
                  v1 := list[fg.vertexIndices[Count + 1]];
                  v2 := list[fg.vertexIndices[Count + 2]];
                end;
              fgmmQuads:
                begin
                  if Count mod 2 = 0 then
                  begin
                    v0 := list[fg.vertexIndices[4 * (Count div 2)]];
                    v1 := list[fg.vertexIndices[4 * (Count div 2) + 1]];
                    v2 := list[fg.vertexIndices[4 * (Count div 2) + 2]];
                  end
                  else
                  begin
                    v0 := list[fg.vertexIndices[4 * (Count div 2)]];
                    v1 := list[fg.vertexIndices[4 * (Count div 2) + 2]];
                    v2 := list[fg.vertexIndices[4 * (Count div 2) + 3]];
                  end;
                end;
            else
              Assert(False);
            end;
            Break;
          end;
        end;

      end;
  else
    Assert(False);
  end;
end;

procedure TgxMeshObject.GetTriangleData(tri: Integer; list: TgxVectorList; var v0, v1, v2: TVector4f);
var
  i, LastCount, Count: Integer;
  fg: TfgxVertexIndexList;
begin
  case mode of
    momTriangles:
      begin
        v0 := list[3 * tri];
        v1 := list[3 * tri + 1];
        v2 := list[3 * tri + 2];
      end;
    momTriangleStrip:
      begin
        v0 := list[tri];
        v1 := list[tri + 1];
        v2 := list[tri + 2];
      end;
    momFaceGroups:
      begin
        Count := 0;
        for i := 0 to FaceGroups.Count - 1 do
        begin
          LastCount := Count;
          fg := TfgxVertexIndexList(FaceGroups[i]);
          Count := Count + fg.TriangleCount;
          if Count > tri then
          begin
            Count := tri - LastCount;
            case fg.mode of
              fgmmTriangles, fgmmFlatTriangles:
                begin
                  v0 := list[fg.vertexIndices[3 * Count]];
                  v1 := list[fg.vertexIndices[3 * Count + 1]];
                  v2 := list[fg.vertexIndices[3 * Count + 2]];
                end;
              fgmmTriangleStrip:
                begin
                  v0 := list[fg.vertexIndices[Count]];
                  v1 := list[fg.vertexIndices[Count + 1]];
                  v2 := list[fg.vertexIndices[Count + 2]];
                end;
              fgmmTriangleFan:
                begin
                  v0 := list[fg.vertexIndices[0]];
                  v1 := list[fg.vertexIndices[Count + 1]];
                  v2 := list[fg.vertexIndices[Count + 2]];
                end;
              fgmmQuads:
                begin
                  if Count mod 2 = 0 then
                  begin
                    v0 := list[fg.vertexIndices[4 * (Count div 2)]];
                    v1 := list[fg.vertexIndices[4 * (Count div 2) + 1]];
                    v2 := list[fg.vertexIndices[4 * (Count div 2) + 2]];
                  end
                  else
                  begin
                    v0 := list[fg.vertexIndices[4 * (Count div 2)]];
                    v1 := list[fg.vertexIndices[4 * (Count div 2) + 2]];
                    v2 := list[fg.vertexIndices[4 * (Count div 2) + 3]];
                  end;
                end;
            else
              Assert(False);
            end;
            Break;
          end;
        end;

      end;
  else
    Assert(False);
  end;
end;

procedure TgxMeshObject.SetTriangleData(tri: Integer; list: TgxAffineVectorList; const v0, v1, v2: TAffineVector);
var
  i, LastCount, Count: Integer;
  fg: TfgxVertexIndexList;
begin
  case mode of
    momTriangles:
      begin
        list[3 * tri] := v0;
        list[3 * tri + 1] := v1;
        list[3 * tri + 2] := v2;
      end;
    momTriangleStrip:
      begin
        list[tri] := v0;
        list[tri + 1] := v1;
        list[tri + 2] := v2;
      end;
    momFaceGroups:
      begin
        Count := 0;
        for i := 0 to FaceGroups.Count - 1 do
        begin
          LastCount := Count;
          fg := TfgxVertexIndexList(FaceGroups[i]);
          Count := Count + fg.TriangleCount;
          if Count > tri then
          begin
            Count := tri - LastCount;
            case fg.mode of
              fgmmTriangles, fgmmFlatTriangles:
                begin
                  list[fg.vertexIndices[3 * Count]] := v0;
                  list[fg.vertexIndices[3 * Count + 1]] := v1;
                  list[fg.vertexIndices[3 * Count + 2]] := v2;
                end;
              fgmmTriangleStrip:
                begin
                  list[fg.vertexIndices[Count]] := v0;
                  list[fg.vertexIndices[Count + 1]] := v1;
                  list[fg.vertexIndices[Count + 2]] := v2;
                end;
              fgmmTriangleFan:
                begin
                  list[fg.vertexIndices[0]] := v0;
                  list[fg.vertexIndices[Count + 1]] := v1;
                  list[fg.vertexIndices[Count + 2]] := v2;
                end;
              fgmmQuads:
                begin
                  if Count mod 2 = 0 then
                  begin
                    list[fg.vertexIndices[4 * (Count div 2)]] := v0;
                    list[fg.vertexIndices[4 * (Count div 2) + 1]] := v1;
                    list[fg.vertexIndices[4 * (Count div 2) + 2]] := v2;
                  end
                  else
                  begin
                    list[fg.vertexIndices[4 * (Count div 2)]] := v0;
                    list[fg.vertexIndices[4 * (Count div 2) + 2]] := v1;
                    list[fg.vertexIndices[4 * (Count div 2) + 3]] := v2;
                  end;
                end;
            else
              Assert(False);
            end;
            Break;
          end;
        end;

      end;
  else
    Assert(False);
  end;
end;

procedure TgxMeshObject.SetTriangleData(tri: Integer; list: TgxVectorList; const v0, v1, v2: TVector4f);
var
  i, LastCount, Count: Integer;
  fg: TfgxVertexIndexList;
begin
  case mode of
    momTriangles:
      begin
        list[3 * tri] := v0;
        list[3 * tri + 1] := v1;
        list[3 * tri + 2] := v2;
      end;
    momTriangleStrip:
      begin
        list[tri] := v0;
        list[tri + 1] := v1;
        list[tri + 2] := v2;
      end;
    momFaceGroups:
      begin
        Count := 0;
        for i := 0 to FaceGroups.Count - 1 do
        begin
          LastCount := Count;
          fg := TfgxVertexIndexList(FaceGroups[i]);
          Count := Count + fg.TriangleCount;
          if Count > tri then
          begin
            Count := tri - LastCount;
            case fg.mode of
              fgmmTriangles, fgmmFlatTriangles:
                begin
                  list[fg.vertexIndices[3 * Count]] := v0;
                  list[fg.vertexIndices[3 * Count + 1]] := v1;
                  list[fg.vertexIndices[3 * Count + 2]] := v2;
                end;
              fgmmTriangleStrip:
                begin
                  list[fg.vertexIndices[Count]] := v0;
                  list[fg.vertexIndices[Count + 1]] := v1;
                  list[fg.vertexIndices[Count + 2]] := v2;
                end;
              fgmmTriangleFan:
                begin
                  list[fg.vertexIndices[0]] := v0;
                  list[fg.vertexIndices[Count + 1]] := v1;
                  list[fg.vertexIndices[Count + 2]] := v2;
                end;
              fgmmQuads:
                begin
                  if Count mod 2 = 0 then
                  begin
                    list[fg.vertexIndices[4 * (Count div 2)]] := v0;
                    list[fg.vertexIndices[4 * (Count div 2) + 1]] := v1;
                    list[fg.vertexIndices[4 * (Count div 2) + 2]] := v2;
                  end
                  else
                  begin
                    list[fg.vertexIndices[4 * (Count div 2)]] := v0;
                    list[fg.vertexIndices[4 * (Count div 2) + 2]] := v1;
                    list[fg.vertexIndices[4 * (Count div 2) + 3]] := v2;
                  end;
                end;
            else
              Assert(False);
            end;
            Break;
          end;
        end;

      end;
  else
    Assert(False);
  end;
end;

procedure TgxMeshObject.SetUseVBO(const Value: Boolean);
var
  i: Integer;
begin
  if Value = FUseVBO then
    Exit;

  if FUseVBO then
  begin
    FreeAndNil(FVerticesVBO);
    FreeAndNil(FNormalsVBO);
    FreeAndNil(FColorsVBO);
    for i := 0 to high(FTexCoordsVBO) do
      FreeAndNil(FTexCoordsVBO[i]);
    FreeAndNil(FLightmapTexCoordsVBO);
  end;

  FValidBuffers := [];

  FUseVBO := Value;
end;

procedure TgxMeshObject.SetValidBuffers(Value: TVBOBuffers);
var
  i: Integer;
begin
  if FValidBuffers <> Value then
  begin
    FValidBuffers := Value;
    if Assigned(FVerticesVBO) then
      FVerticesVBO.NotifyChangesOfData;
    if Assigned(FNormalsVBO) then
      FNormalsVBO.NotifyChangesOfData;
    if Assigned(FColorsVBO) then
      FColorsVBO.NotifyChangesOfData;
    for i := 0 to high(FTexCoordsVBO) do
      if Assigned(FTexCoordsVBO[i]) then
        FTexCoordsVBO[i].NotifyChangesOfData;
    if Assigned(FLightmapTexCoordsVBO) then
      FLightmapTexCoordsVBO.NotifyChangesOfData;
  end;
end;

procedure TgxMeshObject.BuildTangentSpace(buildBinormals: Boolean = True; buildTangents: Boolean = True);
var
  i, j: Integer;
  v, n, t: array [0 .. 2] of TAffineVector;
  tangent, binormal: array [0 .. 2] of TVector4f;
  vt, tt: TAffineVector;
  interp, dot: Single;

  procedure SortVertexData(sortidx: Integer);
  begin
    if t[0].v[sortidx] < t[1].v[sortidx] then
    begin
      vt := v[0];
      tt := t[0];
      v[0] := v[1];
      t[0] := t[1];
      v[1] := vt;
      t[1] := tt;
    end;
    if t[0].v[sortidx] < t[2].v[sortidx] then
    begin
      vt := v[0];
      tt := t[0];
      v[0] := v[2];
      t[0] := t[2];
      v[2] := vt;
      t[2] := tt;
    end;
    if t[1].v[sortidx] < t[2].v[sortidx] then
    begin
      vt := v[1];
      tt := t[1];
      v[1] := v[2];
      t[1] := t[2];
      v[2] := vt;
      t[2] := tt;
    end;
  end;

begin
  Tangents.Clear;
  Binormals.Clear;
  if buildTangents then
    Tangents.Count := Vertices.Count;
  if buildBinormals then
    Binormals.Count := Vertices.Count;
  for i := 0 to TriangleCount - 1 do
  begin
    // Get triangle data
    GetTriangleData(i, Vertices, v[0], v[1], v[2]);
    GetTriangleData(i, normals, n[0], n[1], n[2]);
    GetTriangleData(i, texCoords, t[0], t[1], t[2]);

    for j := 0 to 2 do
    begin
      // Compute tangent
      if buildTangents then
      begin
        SortVertexData(1);

        if (t[2].Y - t[0].Y) = 0 then
          interp := 1
        else
          interp := (t[1].Y - t[0].Y) / (t[2].Y - t[0].Y);

        vt := VectorLerp(v[0], v[2], interp);
        interp := t[0].X + (t[2].X - t[0].X) * interp;
        vt := VectorSubtract(vt, v[1]);
        if t[1].X < interp then
          vt := VectorNegate(vt);
        dot := VectorDotProduct(vt, n[j]);
        vt.X := vt.X - n[j].X * dot;
        vt.Y := vt.Y - n[j].Y * dot;
        vt.Z := vt.Z - n[j].Z * dot;
        tangent[j] := VectorMake(VectorNormalize(vt), 0);
      end;

      // Compute Bi-Normal
      if buildBinormals then
      begin
        SortVertexData(0);

        if (t[2].X - t[0].X) = 0 then
          interp := 1
        else
          interp := (t[1].X - t[0].X) / (t[2].X - t[0].X);

        vt := VectorLerp(v[0], v[2], interp);
        interp := t[0].Y + (t[2].Y - t[0].Y) * interp;
        vt := VectorSubtract(vt, v[1]);
        if t[1].Y < interp then
          vt := VectorNegate(vt);
        dot := VectorDotProduct(vt, n[j]);
        vt.X := vt.X - n[j].X * dot;
        vt.Y := vt.Y - n[j].Y * dot;
        vt.Z := vt.Z - n[j].Z * dot;
        binormal[j] := VectorMake(VectorNormalize(vt), 0);
      end;
    end;

    if buildTangents then
      SetTriangleData(i, Tangents, tangent[0], tangent[1], tangent[2]);
    if buildBinormals then
      SetTriangleData(i, Binormals, binormal[0], binormal[1], binormal[2]);
  end;
end;

procedure TgxMeshObject.DeclareArraysToOpenGL(var mrci: TgxRenderContextInfo; evenIfAlreadyDeclared: Boolean = False);
var
  i: Integer;
  currentMapping: Cardinal;
  lists: array [0 .. 4] of pointer;
  tlists: array of pointer;
begin
  if evenIfAlreadyDeclared or (not FArraysDeclared) then
  begin
    FillChar(lists, SizeOf(lists), 0);
    SetLength(tlists, FTexCoordsEx.Count);

    // workaround for ATI bug, disable element VBO if
    // inside a display list
    FUseVBO := FUseVBO and not mrci.gxStates.InsideList;
    /// and GL_ARB_vertex_buffer_object

    if not FUseVBO then
    begin
      lists[0] := Vertices.list;
      lists[1] := normals.list;
      lists[2] := Colors.list;
      lists[3] := texCoords.list;
      lists[4] := LightMapTexCoords.list;

      for i := 0 to FTexCoordsEx.Count - 1 do
        tlists[i] := TexCoordsEx[i].list;
    end
    else
    begin
      BufferArrays;
    end;

    if not mrci.ignoreMaterials then
    begin
      if normals.Count > 0 then
      begin
        if FUseVBO then
          FNormalsVBO.Bind;
        glEnableClientState(GL_NORMAL_ARRAY);
        glNormalPointer(GL_FLOAT, 0, lists[1]);
      end
      else
        glDisableClientState(GL_NORMAL_ARRAY);
      if (Colors.Count > 0) and (not mrci.ignoreMaterials) then
      begin
        if FUseVBO then
          FColorsVBO.Bind;
        glEnableClientState(GL_COLOR_ARRAY);
        glColorPointer(4, GL_FLOAT, 0, lists[2]);
      end
      else
        glDisableClientState(GL_COLOR_ARRAY);
      if texCoords.Count > 0 then
      begin
        if FUseVBO then
          FTexCoordsVBO[0].Bind;
        glEnableClientState(GL_TEXTURE_COORD_ARRAY);
        glTexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), lists[3]);
      end
      else
        glDisableClientState(GL_TEXTURE_COORD_ARRAY);
      /// if GL_ARB_multitexture then
      begin
        if LightMapTexCoords.Count > 0 then
        begin
          if FUseVBO then
            FLightmapTexCoordsVBO.Bind;
          glClientActiveTexture(GL_TEXTURE1);
          glTexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), lists[4]);
          glEnableClientState(GL_TEXTURE_COORD_ARRAY);
        end;
        for i := 0 to FTexCoordsEx.Count - 1 do
        begin
          if TexCoordsEx[i].Count > 0 then
          begin
            if FUseVBO then
              FTexCoordsVBO[i].Bind;
            glClientActiveTexture(GL_TEXTURE0 + i);
            glTexCoordPointer(4, GL_FLOAT, SizeOf(TVector4f), tlists[i]);
            glEnableClientState(GL_TEXTURE_COORD_ARRAY);
          end;
        end;
        glClientActiveTexture(GL_TEXTURE0);
      end;
    end
    else
    begin
      glDisableClientState(GL_NORMAL_ARRAY);
      glDisableClientState(GL_COLOR_ARRAY);
      glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    end;

    if Vertices.Count > 0 then
    begin
      if FUseVBO then
        FVerticesVBO.Bind;
      glEnableClientState(GL_VERTEX_ARRAY);
      glVertexPointer(3, GL_FLOAT, 0, lists[0]);
    end
    else
      glDisableClientState(GL_VERTEX_ARRAY);

    if (LightMapTexCoords.Count = 0) and not FUseVBO then
    /// and GL_EXT_compiled_vertex_array
      glLockArraysEXT(0, Vertices.Count);
    FLastLightMapIndex := -1;
    FArraysDeclared := True;
    FLightMapArrayEnabled := False;
    if mrci.drawState <> dsPicking then
      FLastXOpenGLTexMapping := xglGetBitWiseMapping;
  end
  else
  begin
    if not mrci.ignoreMaterials and not(mrci.drawState = dsPicking) then
      if texCoords.Count > 0 then
      begin
        currentMapping := xglGetBitWiseMapping;
        if FLastXOpenGLTexMapping <> currentMapping then
        begin
          xglEnableClientState(GL_TEXTURE_COORD_ARRAY);
          xglTexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), texCoords.list);
          FLastXOpenGLTexMapping := currentMapping;
        end;
      end;
  end;
end;

procedure TgxMeshObject.DisableOpenGLArrays(var mrci: TgxRenderContextInfo);
var
  i: Integer;
begin
  if FArraysDeclared then
  begin
    DisableLightMapArray(mrci);
    if (LightMapTexCoords.Count = 0) and not FUseVBO then
    /// and GL_EXT_compiled_vertex_array
      glUnlockArraysEXT;
    if Vertices.Count > 0 then
      glDisableClientState(GL_VERTEX_ARRAY);
    if not mrci.ignoreMaterials then
    begin
      if normals.Count > 0 then
        glDisableClientState(GL_NORMAL_ARRAY);
      if (Colors.Count > 0) and (not mrci.ignoreMaterials) then
        glDisableClientState(GL_COLOR_ARRAY);
      if texCoords.Count > 0 then
        glDisableClientState(GL_TEXTURE_COORD_ARRAY);
      /// if GL_ARB_multitexture then
      begin
        if LightMapTexCoords.Count > 0 then
        begin
          glClientActiveTexture(GL_TEXTURE1);
          glDisableClientState(GL_TEXTURE_COORD_ARRAY);
        end;
        for i := 0 to FTexCoordsEx.Count - 1 do
        begin
          if TexCoordsEx[i].Count > 0 then
          begin
            glClientActiveTexture(GL_TEXTURE0 + i);
            glDisableClientState(GL_TEXTURE_COORD_ARRAY);
          end;
        end;
        glClientActiveTexture(GL_TEXTURE0);
      end;
    end;

    if FUseVBO then
    begin
      if Vertices.Count > 0 then
        FVerticesVBO.UnBind;
      if normals.Count > 0 then
        FNormalsVBO.UnBind;
      if Colors.Count > 0 then
        FColorsVBO.UnBind;
      if texCoords.Count > 0 then
        FTexCoordsVBO[0].UnBind;
      if LightMapTexCoords.Count > 0 then
        FLightmapTexCoordsVBO.UnBind;
      if FTexCoordsEx.Count > 0 then
      begin
        for i := 0 to FTexCoordsEx.Count - 1 do
        begin
          if TexCoordsEx[i].Count > 0 then
            FTexCoordsVBO[i].UnBind;
        end;
      end;

    end;
    FArraysDeclared := False;
  end;
end;

procedure TgxMeshObject.EnableLightMapArray(var mrci: TgxRenderContextInfo);
begin
  if (not mrci.ignoreMaterials) then
  /// and GL_ARB_multitexture
  begin
    Assert(FArraysDeclared);
    if not FLightMapArrayEnabled then
    begin
      mrci.gxStates.ActiveTexture := 1;
      mrci.gxStates.ActiveTextureEnabled[ttTexture2D] := True;
      mrci.gxStates.ActiveTexture := 0;
      FLightMapArrayEnabled := True;
    end;
  end;
end;

procedure TgxMeshObject.DisableLightMapArray(var mrci: TgxRenderContextInfo);
begin
  if FLightMapArrayEnabled then
  /// and GL_ARB_multitexture
  begin
    mrci.gxStates.ActiveTexture := 1;
    mrci.gxStates.ActiveTextureEnabled[ttTexture2D] := False;
    mrci.gxStates.ActiveTexture := 0;
    FLightMapArrayEnabled := False;
  end;
end;

procedure TgxMeshObject.PrepareBuildList(var mrci: TgxRenderContextInfo);
var
  i: Integer;
begin
  if (mode = momFaceGroups) and Assigned(mrci.MaterialLibrary) then
  begin
    for i := 0 to FaceGroups.Count - 1 do
      with TgxFaceGroup(FaceGroups.list^[i]) do
      begin
        if MaterialCache <> nil then
          MaterialCache.PrepareBuildList;
      end;
  end;
end;

procedure TgxMeshObject.BufferArrays;
const
  BufferUsage = GL_DYNAMIC_DRAW;
var
  i: Integer;
begin
  if Vertices.Count > 0 then
  begin
    if not Assigned(FVerticesVBO) then
      FVerticesVBO := TgxVBOArrayBufferHandle.Create;
    FVerticesVBO.AllocateHandle;

    if FVerticesVBO.IsDataNeedUpdate then
    begin
      FVerticesVBO.BindBufferData(Vertices.list, SizeOf(TAffineVector) * Vertices.Count, BufferUsage);
      FVerticesVBO.NotifyDataUpdated;
      FVerticesVBO.UnBind;
    end;

    Include(FValidBuffers, vbVertices);
  end;

  if normals.Count > 0 then
  begin
    if not Assigned(FNormalsVBO) then
      FNormalsVBO := TgxVBOArrayBufferHandle.Create;
    FNormalsVBO.AllocateHandle;

    if FNormalsVBO.IsDataNeedUpdate then
    begin
      FNormalsVBO.BindBufferData(normals.list, SizeOf(TAffineVector) * normals.Count, BufferUsage);
      FNormalsVBO.NotifyDataUpdated;
      FNormalsVBO.UnBind;
    end;

    Include(FValidBuffers, vbNormals);
  end;

  if Colors.Count > 0 then
  begin
    if not Assigned(FColorsVBO) then
      FColorsVBO := TgxVBOArrayBufferHandle.Create;
    FColorsVBO.AllocateHandle;

    if FColorsVBO.IsDataNeedUpdate then
    begin
      FColorsVBO.BindBufferData(Colors.list, SizeOf(TVector4f) * Colors.Count, BufferUsage);
      FColorsVBO.NotifyDataUpdated;
      FColorsVBO.UnBind;
    end;

    Include(FValidBuffers, vbColors);
  end;

  if texCoords.Count > 0 then
  begin
    if Length(FTexCoordsVBO) < 1 then
      SetLength(FTexCoordsVBO, 1);

    if not Assigned(FTexCoordsVBO[0]) then
      FTexCoordsVBO[0] := TgxVBOArrayBufferHandle.Create;
    FTexCoordsVBO[0].AllocateHandle;

    if FTexCoordsVBO[0].IsDataNeedUpdate then
    begin
      FTexCoordsVBO[0].BindBufferData(texCoords.list, SizeOf(TAffineVector) * texCoords.Count, BufferUsage);
      FTexCoordsVBO[0].NotifyDataUpdated;
      FTexCoordsVBO[0].UnBind;
    end;

    Include(FValidBuffers, vbTexCoords);
  end;

  if LightMapTexCoords.Count > 0 then
  begin
    if not Assigned(FLightmapTexCoordsVBO) then
      FLightmapTexCoordsVBO := TgxVBOArrayBufferHandle.Create;
    FLightmapTexCoordsVBO.AllocateHandle;

    FLightmapTexCoordsVBO.BindBufferData(LightMapTexCoords.list, SizeOf(TAffineVector) * LightMapTexCoords.Count, BufferUsage);
    FLightmapTexCoordsVBO.NotifyDataUpdated;
    FLightmapTexCoordsVBO.UnBind;

    Include(FValidBuffers, vbLightMapTexCoords);
  end;

  if FTexCoordsEx.Count > 0 then
  begin
    if Length(FTexCoordsVBO) < FTexCoordsEx.Count then
      SetLength(FTexCoordsVBO, FTexCoordsEx.Count);

    for i := 0 to FTexCoordsEx.Count - 1 do
    begin
      if TexCoordsEx[i].Count <= 0 then
        continue;

      if not Assigned(FTexCoordsVBO[i]) then
        FTexCoordsVBO[i] := TgxVBOArrayBufferHandle.Create;
      FTexCoordsVBO[i].AllocateHandle;

      if FTexCoordsVBO[i].IsDataNeedUpdate then
      begin
        FTexCoordsVBO[i].BindBufferData(TexCoordsEx[i].list, SizeOf(TVector4f) * TexCoordsEx[i].Count, BufferUsage);
        FTexCoordsVBO[i].NotifyDataUpdated;
        FTexCoordsVBO[i].UnBind;
      end;
    end;

    Include(FValidBuffers, vbTexCoordsEx);
  end;

  /// CheckOpenGLError;
end;

procedure TgxMeshObject.BuildList(var mrci: TgxRenderContextInfo);
var
  i, j, groupID, nbGroups: Integer;
  gotNormals, gotTexCoords, gotColor: Boolean;
  gotTexCoordsEx: array of Boolean;
  libMat: TgxLibMaterial;
  fg: TgxFaceGroup;
begin
  // Make sure no VBO is bound and states enabled
  FArraysDeclared := False;
  FLastXOpenGLTexMapping := 0;
  gotColor := (Vertices.Count = Colors.Count);
  if gotColor then
  begin
    mrci.gxStates.Enable(stColorMaterial);
    glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
    mrci.gxStates.SetMaterialColors(cmFront, clrBlack, clrGray20, clrGray80, clrBlack, 0);
    mrci.gxStates.SetMaterialColors(cmBack, clrBlack, clrGray20, clrGray80, clrBlack, 0);
  end;
  case mode of
    momTriangles, momTriangleStrip:
      if Vertices.Count > 0 then
      begin
        DeclareArraysToOpenGL(mrci);
        gotNormals := (Vertices.Count = normals.Count);
        gotTexCoords := (Vertices.Count = texCoords.Count);
        SetLength(gotTexCoordsEx, FTexCoordsEx.Count);
        for i := 0 to FTexCoordsEx.Count - 1 do
          gotTexCoordsEx[i] := (TexCoordsEx[i].Count > 0);
        /// and GL_ARB_multitexture;
        if mode = momTriangles then
          glBegin(GL_TRIANGLES)
        else
          glBegin(GL_TRIANGLE_STRIP);
        for i := 0 to Vertices.Count - 1 do
        begin
          if gotNormals then
            glNormal3fv(@normals.list[i]);
          if gotColor then
            glColor4fv(@Colors.list[i]);
          if FTexCoordsEx.Count > 0 then
          begin
            if gotTexCoordsEx[0] then
              glMultiTexCoord4fv(GL_TEXTURE0, @TexCoordsEx[0].list[i])
            else if gotTexCoords then
              glTexCoord2fv(@texCoords.list[i]);
            for j := 1 to FTexCoordsEx.Count - 1 do
              if gotTexCoordsEx[j] then
                glMultiTexCoord4fv(GL_TEXTURE0 + j, @TexCoordsEx[j].list[i]);
          end
          else
          begin
            if gotTexCoords then
              glTexCoord2fv(@texCoords.list[i]);
          end;
          glVertex3fv(@Vertices.list[i]);
        end;
        glEnd;
      end;
    momFaceGroups:
      begin
        if Assigned(mrci.MaterialLibrary) then
        begin
          if moroGroupByMaterial in RenderingOptions then
          begin
            // group-by-material rendering, reduces material switches,
            // but alters rendering order
            groupID := vNextRenderGroupID;
            Inc(vNextRenderGroupID);
            for i := 0 to FaceGroups.Count - 1 do
            begin
              if FaceGroups[i].FRenderGroupID <> groupID then
              begin
                libMat := FaceGroups[i].FMaterialCache;
                if Assigned(libMat) then
                  libMat.Apply(mrci);
                repeat
                  for j := i to FaceGroups.Count - 1 do
                    with FaceGroups[j] do
                    begin
                      if (FRenderGroupID <> groupID) and (FMaterialCache = libMat) then
                      begin
                        FRenderGroupID := groupID;
                        BuildList(mrci);
                      end;
                    end;
                until (not Assigned(libMat)) or (not libMat.UnApply(mrci));
              end;
            end;
          end
          else
          begin
            // canonical rendering (regroups only contiguous facegroups)
            i := 0;
            nbGroups := FaceGroups.Count;
            while i < nbGroups do
            begin
              libMat := FaceGroups[i].FMaterialCache;
              if Assigned(libMat) then
              begin
                libMat.Apply(mrci);
                repeat
                  j := i;
                  while j < nbGroups do
                  begin
                    fg := FaceGroups[j];
                    if fg.MaterialCache <> libMat then
                      Break;
                    fg.BuildList(mrci);
                    Inc(j);
                  end;
                until not libMat.UnApply(mrci);
                i := j;
              end
              else
              begin
                FaceGroups[i].BuildList(mrci);
                Inc(i);
              end;
            end;
          end;
          // restore faceculling
          if (stCullFace in mrci.gxStates.States) then
          begin
            if not mrci.bufferFaceCull then
              mrci.gxStates.Disable(stCullFace);
          end
          else
          begin
            if mrci.bufferFaceCull then
              mrci.gxStates.Enable(stCullFace);
          end;
        end
        else
          for i := 0 to FaceGroups.Count - 1 do
            FaceGroups[i].BuildList(mrci);
      end;
  else
    Assert(False);
  end;
  DisableOpenGLArrays(mrci);
end;

// ------------------
// ------------------ TgxMeshObjectList ------------------
// ------------------

constructor TgxMeshObjectList.CreateOwned(aOwner: TgxBaseMesh);
begin
  FOwner := aOwner;
  Create;
end;

destructor TgxMeshObjectList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TgxMeshObjectList.ReadFromFiler(reader: TgxVirtualReader);
var
  i: Integer;
  Mesh: TgxMeshObject;
begin
  inherited;
  for i := 0 to Count - 1 do
  begin
    Mesh := Items[i];
    Mesh.FOwner := Self;
    if Mesh is TgxSkeletonMeshObject then
      TgxSkeletonMeshObject(Mesh).PrepareBoneMatrixInvertedMeshes;
  end;
end;

procedure TgxMeshObjectList.PrepareMaterialLibraryCache(matLib: TgxMaterialLibrary);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TgxMeshObject(list^[i]).PrepareMaterialLibraryCache(matLib);
end;

procedure TgxMeshObjectList.DropMaterialLibraryCache;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TgxMeshObject(list^[i]).DropMaterialLibraryCache;
end;

procedure TgxMeshObjectList.PrepareBuildList(var mrci: TgxRenderContextInfo);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
      if Visible then
        PrepareBuildList(mrci);
end;

procedure TgxMeshObjectList.BuildList(var mrci: TgxRenderContextInfo);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
      if Visible then
        BuildList(mrci);
end;

procedure TgxMeshObjectList.MorphTo(morphTargetIndex: Integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i] is TgxMorphableMeshObject then
      TgxMorphableMeshObject(Items[i]).MorphTo(morphTargetIndex);
end;

procedure TgxMeshObjectList.Lerp(morphTargetIndex1, morphTargetIndex2: Integer; lerpFactor: Single);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i] is TgxMorphableMeshObject then
      TgxMorphableMeshObject(Items[i]).Lerp(morphTargetIndex1, morphTargetIndex2, lerpFactor);
end;

function TgxMeshObjectList.MorphTargetCount: Integer;
var
  i: Integer;
begin
  Result := MaxInt;
  for i := 0 to Count - 1 do
    if Items[i] is TgxMorphableMeshObject then
      with TgxMorphableMeshObject(Items[i]) do
        if Result > MorphTargets.Count then
          Result := MorphTargets.Count;
  if Result = MaxInt then
    Result := 0;
end;

procedure TgxMeshObjectList.Clear;
var
  i: Integer;
begin
  DropMaterialLibraryCache;
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      FOwner := nil;
      Free;
    end;
  inherited;
end;

function TgxMeshObjectList.GetMeshObject(Index: Integer): TgxMeshObject;
begin
  Result := TgxMeshObject(list^[Index]);
end;

procedure TgxMeshObjectList.GetExtents(out min, max: TAffineVector);
var
  i, k: Integer;
  lMin, lMax: TAffineVector;
const
  cBigValue: Single = 1E30;
  cSmallValue: Single = -1E30;
begin
  SetVector(min, cBigValue, cBigValue, cBigValue);
  SetVector(max, cSmallValue, cSmallValue, cSmallValue);
  for i := 0 to Count - 1 do
  begin
    GetMeshObject(i).GetExtents(lMin, lMax);
    for k := 0 to 2 do
    begin
      if lMin.v[k] < min.v[k] then
        min.v[k] := lMin.v[k];
      if lMax.v[k] > max.v[k] then
        max.v[k] := lMax.v[k];
    end;
  end;
end;

procedure TgxMeshObjectList.Translate(const delta: TAffineVector);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    GetMeshObject(i).Translate(delta);
end;

function TgxMeshObjectList.ExtractTriangles(texCoords: TgxAffineVectorList = nil; normals: TgxAffineVectorList = nil)
  : TgxAffineVectorList;
var
  i: Integer;
  obj: TgxMeshObject;
  objTris: TgxAffineVectorList;
  objTexCoords: TgxAffineVectorList;
  objNormals: TgxAffineVectorList;
begin
  Result := TgxAffineVectorList.Create;
  if Assigned(texCoords) then
    objTexCoords := TgxAffineVectorList.Create
  else
    objTexCoords := nil;
  if Assigned(normals) then
    objNormals := TgxAffineVectorList.Create
  else
    objNormals := nil;
  try
    for i := 0 to Count - 1 do
    begin
      obj := GetMeshObject(i);
      if not obj.Visible then
        continue;
      objTris := obj.ExtractTriangles(objTexCoords, objNormals);
      try
        Result.Add(objTris);
        if Assigned(texCoords) then
        begin
          texCoords.Add(objTexCoords);
          objTexCoords.Count := 0;
        end;
        if Assigned(normals) then
        begin
          normals.Add(objNormals);
          objNormals.Count := 0;
        end;
      finally
        objTris.Free;
      end;
    end;
  finally
    objTexCoords.Free;
    objNormals.Free;
  end;
end;

function TgxMeshObjectList.TriangleCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    Result := Result + Items[i].TriangleCount;
end;

procedure TgxMeshObjectList.Prepare;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Prepare;
end;

function TgxMeshObjectList.FindMeshByName(MeshName: string): TgxMeshObject;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Name = MeshName then
    begin
      Result := Items[i];
      Break;
    end;
end;

procedure TgxMeshObjectList.BuildTangentSpace(buildBinormals, buildTangents: Boolean);
var
  i: Integer;
begin
  if Count <> 0 then
    for i := 0 to Count - 1 do
      GetMeshObject(i).BuildTangentSpace(buildBinormals, buildTangents);
end;

function TgxMeshObjectList.GetUseVBO: Boolean;
var
  i: Integer;
begin
  Result := True;
  if Count <> 0 then
    for i := 0 to Count - 1 do
      Result := Result and GetMeshObject(i).FUseVBO;
end;

procedure TgxMeshObjectList.SetUseVBO(const Value: Boolean);
var
  i: Integer;
begin
  if Count <> 0 then
    for i := 0 to Count - 1 do
      GetMeshObject(i).SetUseVBO(Value);
end;

// ------------------
// ------------------ TgxMeshMorphTarget ------------------
// ------------------

constructor TgxMeshMorphTarget.CreateOwned(aOwner: TgxMeshMorphTargetList);
begin
  FOwner := aOwner;
  Create;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

destructor TgxMeshMorphTarget.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.Remove(Self);
  inherited;
end;

procedure TgxMeshMorphTarget.WriteToFiler(writer: TgxVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    // nothing
  end;
end;

procedure TgxMeshMorphTarget.ReadFromFiler(reader: TgxVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      // nothing
    end
  else
    RaiseFilerException(archiveVersion);
end;

// ------------------
// ------------------ TgxMeshMorphTargetList ------------------
// ------------------

constructor TgxMeshMorphTargetList.CreateOwned(aOwner: TPersistent);
begin
  FOwner := aOwner;
  Create;
end;

destructor TgxMeshMorphTargetList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TgxMeshMorphTargetList.ReadFromFiler(reader: TgxVirtualReader);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

procedure TgxMeshMorphTargetList.Translate(const delta: TAffineVector);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Translate(delta);
end;

procedure TgxMeshMorphTargetList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      FOwner := nil;
      Free;
    end;
  inherited;
end;

function TgxMeshMorphTargetList.GetMeshMorphTarget(Index: Integer): TgxMeshMorphTarget;
begin
  Result := TgxMeshMorphTarget(list^[Index]);
end;

// ------------------
// ------------------ TgxMorphableMeshObject ------------------
// ------------------

constructor TgxMorphableMeshObject.Create;
begin
  inherited;
  FMorphTargets := TgxMeshMorphTargetList.CreateOwned(Self);
end;

destructor TgxMorphableMeshObject.Destroy;
begin
  FMorphTargets.Free;
  inherited;
end;

procedure TgxMorphableMeshObject.WriteToFiler(writer: TgxVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FMorphTargets.WriteToFiler(writer);
  end;
end;

procedure TgxMorphableMeshObject.ReadFromFiler(reader: TgxVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FMorphTargets.ReadFromFiler(reader);
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TgxMorphableMeshObject.Clear;
begin
  inherited;
  FMorphTargets.Clear;
end;

procedure TgxMorphableMeshObject.Translate(const delta: TAffineVector);
begin
  inherited;
  MorphTargets.Translate(delta);
  ValidBuffers := ValidBuffers - [vbVertices];
end;

procedure TgxMorphableMeshObject.MorphTo(morphTargetIndex: Integer);
begin
  if (morphTargetIndex = 0) and (MorphTargets.Count = 0) then
    Exit;
  Assert(Cardinal(morphTargetIndex) < Cardinal(MorphTargets.Count));
  with MorphTargets[morphTargetIndex] do
  begin
    if Vertices.Count > 0 then
    begin
      Self.Vertices.Assign(Vertices);
      ValidBuffers := ValidBuffers - [vbVertices];
    end;
    if normals.Count > 0 then
    begin
      Self.normals.Assign(normals);
      ValidBuffers := ValidBuffers - [vbNormals];
    end;
  end;
end;

procedure TgxMorphableMeshObject.Lerp(morphTargetIndex1, morphTargetIndex2: Integer; lerpFactor: Single);
var
  mt1, mt2: TgxMeshMorphTarget;
begin
  Assert((Cardinal(morphTargetIndex1) < Cardinal(MorphTargets.Count)) and
    (Cardinal(morphTargetIndex2) < Cardinal(MorphTargets.Count)));
  if lerpFactor = 0 then
    MorphTo(morphTargetIndex1)
  else if lerpFactor = 1 then
    MorphTo(morphTargetIndex2)
  else
  begin
    mt1 := MorphTargets[morphTargetIndex1];
    mt2 := MorphTargets[morphTargetIndex2];
    if mt1.Vertices.Count > 0 then
    begin
      Vertices.Lerp(mt1.Vertices, mt2.Vertices, lerpFactor);
      ValidBuffers := ValidBuffers - [vbVertices];
    end;
    if mt1.normals.Count > 0 then
    begin
      normals.Lerp(mt1.normals, mt2.normals, lerpFactor);
      normals.normalize;
      ValidBuffers := ValidBuffers - [vbNormals];
    end;
  end;
end;

// ------------------
// ------------------ TgxSkeletonMeshObject ------------------
// ------------------

constructor TgxSkeletonMeshObject.Create;
begin
  FBoneMatrixInvertedMeshes := TList.Create;
  FBackupInvertedMeshes := TList.Create; // ragdoll
  inherited Create;
end;

destructor TgxSkeletonMeshObject.Destroy;
begin
  Clear;
  FBoneMatrixInvertedMeshes.Free;
  FBackupInvertedMeshes.Free;
  inherited Destroy;
end;

procedure TgxSkeletonMeshObject.WriteToFiler(writer: TgxVirtualWriter);
var
  i: Integer;
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteInteger(FVerticeBoneWeightCount);
    WriteInteger(FBonesPerVertex);
    WriteInteger(FVerticeBoneWeightCapacity);
    for i := 0 to FVerticeBoneWeightCount - 1 do
      Write(FVerticesBonesWeights[i][0], FBonesPerVertex * SizeOf(TgxVertexBoneWeight));
  end;
end;

procedure TgxSkeletonMeshObject.ReadFromFiler(reader: TgxVirtualReader);
var
  archiveVersion, i: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FVerticeBoneWeightCount := ReadInteger;
      FBonesPerVertex := ReadInteger;
      FVerticeBoneWeightCapacity := ReadInteger;
      ResizeVerticesBonesWeights;
      for i := 0 to FVerticeBoneWeightCount - 1 do
        Read(FVerticesBonesWeights[i][0], FBonesPerVertex * SizeOf(TgxVertexBoneWeight));
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TgxSkeletonMeshObject.Clear;
var
  i: Integer;
begin
  inherited;
  FVerticeBoneWeightCount := 0;
  FBonesPerVertex := 0;
  ResizeVerticesBonesWeights;
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
    TgxBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  FBoneMatrixInvertedMeshes.Clear;
end;

procedure TgxSkeletonMeshObject.SetVerticeBoneWeightCount(const val: Integer);
begin
  if val <> FVerticeBoneWeightCount then
  begin
    FVerticeBoneWeightCount := val;
    if FVerticeBoneWeightCount > FVerticeBoneWeightCapacity then
      VerticeBoneWeightCapacity := FVerticeBoneWeightCount + 16;
    FLastVerticeBoneWeightCount := FVerticeBoneWeightCount;
  end;
end;

procedure TgxSkeletonMeshObject.SetVerticeBoneWeightCapacity(const val: Integer);
begin
  if val <> FVerticeBoneWeightCapacity then
  begin
    FVerticeBoneWeightCapacity := val;
    ResizeVerticesBonesWeights;
  end;
end;

procedure TgxSkeletonMeshObject.SetBonesPerVertex(const val: Integer);
begin
  if val <> FBonesPerVertex then
  begin
    FBonesPerVertex := val;
    ResizeVerticesBonesWeights;
  end;
end;

procedure TgxSkeletonMeshObject.ResizeVerticesBonesWeights;
var
  n, m, i, j: Integer;
  newArea: PgxVerticesBoneWeights;
begin
  n := BonesPerVertex * VerticeBoneWeightCapacity;
  if n = 0 then
  begin
    // release everything
    if Assigned(FVerticesBonesWeights) then
    begin
      FreeMem(FVerticesBonesWeights[0]);
      FreeMem(FVerticesBonesWeights);
      FVerticesBonesWeights := nil;
    end;
  end
  else
  begin
    // allocate new area
    GetMem(newArea, VerticeBoneWeightCapacity * SizeOf(PgxVertexBoneWeightArray));
    newArea[0] := AllocMem(n * SizeOf(TgxVertexBoneWeight));
    for i := 1 to VerticeBoneWeightCapacity - 1 do
      newArea[i] := PgxVertexBoneWeightArray(Cardinal(newArea[0]) + Cardinal(i * SizeOf(TgxVertexBoneWeight) * BonesPerVertex));
    // transfer old data
    if FLastVerticeBoneWeightCount < VerticeBoneWeightCount then
      n := FLastVerticeBoneWeightCount
    else
      n := VerticeBoneWeightCount;
    if FLastBonesPerVertex < BonesPerVertex then
      m := FLastBonesPerVertex
    else
      m := BonesPerVertex;
    for i := 0 to n - 1 do
      for j := 0 to m - 1 do
        newArea[i][j] := VerticesBonesWeights[i][j];
    // release old area and switch to new
    if Assigned(FVerticesBonesWeights) then
    begin
      FreeMem(FVerticesBonesWeights[0]);
      FreeMem(FVerticesBonesWeights);
    end;
    FVerticesBonesWeights := newArea;
  end;
  FLastBonesPerVertex := FBonesPerVertex;
end;

procedure TgxSkeletonMeshObject.AddWeightedBone(aBoneID: Integer; aWeight: Single);
begin
  if BonesPerVertex < 1 then
    BonesPerVertex := 1;
  VerticeBoneWeightCount := VerticeBoneWeightCount + 1;
  with VerticesBonesWeights^[VerticeBoneWeightCount - 1]^[0] do
  begin
    BoneID := aBoneID;
    weight := aWeight;
  end;
end;

procedure TgxSkeletonMeshObject.AddWeightedBones(const boneIDs: TgxVertexBoneWeightDynArray);
var
  i: Integer;
  n: Integer;
begin
  n := Length(boneIDs);
  if BonesPerVertex < n then
    BonesPerVertex := n;
  VerticeBoneWeightCount := VerticeBoneWeightCount + 1;
  for i := 0 to n - 1 do
  begin
    with VerticesBonesWeights^[VerticeBoneWeightCount - 1]^[i] do
    begin
      BoneID := boneIDs[i].BoneID;
      weight := boneIDs[i].weight;
    end;
  end;
end;

function TgxSkeletonMeshObject.FindOrAdd(BoneID: Integer; const vertex, normal: TAffineVector): Integer;
var
  i: Integer;
  dynArray: TgxVertexBoneWeightDynArray;
begin
  if BonesPerVertex > 1 then
  begin
    SetLength(dynArray, 1);
    dynArray[0].BoneID := BoneID;
    dynArray[0].weight := 1;
    Result := FindOrAdd(dynArray, vertex, normal);
    Exit;
  end;
  Result := -1;
  for i := 0 to Vertices.Count - 1 do
    if (VerticesBonesWeights^[i]^[0].BoneID = BoneID) and VectorEquals(Vertices.list^[i], vertex) and
      VectorEquals(normals.list^[i], normal) then
    begin
      Result := i;
      Break;
    end;
  if Result < 0 then
  begin
    AddWeightedBone(BoneID, 1);
    Vertices.Add(vertex);
    Result := normals.Add(normal);
  end;
end;

function TgxSkeletonMeshObject.FindOrAdd(const boneIDs: TgxVertexBoneWeightDynArray; const vertex, normal: TAffineVector)
  : Integer;
var
  i, j: Integer;
  bonesMatch: Boolean;
begin
  Result := -1;
  for i := 0 to Vertices.Count - 1 do
  begin
    bonesMatch := True;
    for j := 0 to High(boneIDs) do
    begin
      if (boneIDs[j].BoneID <> VerticesBonesWeights^[i]^[j].BoneID) or (boneIDs[j].weight <> VerticesBonesWeights^[i]^[j].weight)
      then
      begin
        bonesMatch := False;
        Break;
      end;
    end;
    if bonesMatch and VectorEquals(Vertices[i], vertex) and VectorEquals(normals[i], normal) then
    begin
      Result := i;
      Break;
    end;
  end;
  if Result < 0 then
  begin
    AddWeightedBones(boneIDs);
    Vertices.Add(vertex);
    Result := normals.Add(normal);
  end;
end;

procedure TgxSkeletonMeshObject.PrepareBoneMatrixInvertedMeshes;
var
  i, k, boneIndex: Integer;
  invMesh: TgxBaseMeshObject;
  invMat: TMatrix4f;
  Bone: TgxSkeletonBone;
  p: TVector4f;
begin
  // cleanup existing stuff
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
    TgxBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  FBoneMatrixInvertedMeshes.Clear;
  // calculate
  for k := 0 to BonesPerVertex - 1 do
  begin
    invMesh := TgxBaseMeshObject.Create;
    FBoneMatrixInvertedMeshes.Add(invMesh);
    invMesh.Vertices := Vertices;
    invMesh.normals := normals;
    for i := 0 to Vertices.Count - 1 do
    begin
      boneIndex := VerticesBonesWeights^[i]^[k].BoneID;
      Bone := Owner.Owner.Skeleton.RootBones.BoneByID(boneIndex);
      // transform point
      MakePoint(p, Vertices[i]);
      invMat := Bone.GlobalMatrix;
      InvertMatrix(invMat);
      p := VectorTransform(p, invMat);
      invMesh.Vertices[i] := PAffineVector(@p)^;
      // transform normal
      SetVector(p, normals[i]);
      invMat := Bone.GlobalMatrix;
      invMat.W := NullHmgPoint;
      InvertMatrix(invMat);
      p := VectorTransform(p, invMat);
      invMesh.normals[i] := PAffineVector(@p)^;
    end;
  end;
end;

procedure TgxSkeletonMeshObject.BackupBoneMatrixInvertedMeshes; // ragdoll
var
  i: Integer;
  bm: TgxBaseMeshObject;
begin
  // cleanup existing stuff
  for i := 0 to FBackupInvertedMeshes.Count - 1 do
    TgxBaseMeshObject(FBackupInvertedMeshes[i]).Free;
  FBackupInvertedMeshes.Clear;
  // copy current stuff
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
  begin
    bm := TgxBaseMeshObject.Create;
    bm.Assign(TgxBaseMeshObject(FBoneMatrixInvertedMeshes[i]));
    FBackupInvertedMeshes.Add(bm);
    TgxBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  end;
  FBoneMatrixInvertedMeshes.Clear;
end;

procedure TgxSkeletonMeshObject.RestoreBoneMatrixInvertedMeshes; // ragdoll
var
  i: Integer;
  bm: TgxBaseMeshObject;
begin
  // cleanup existing stuff
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
    TgxBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  FBoneMatrixInvertedMeshes.Clear;
  // restore the backup
  for i := 0 to FBackupInvertedMeshes.Count - 1 do
  begin
    bm := TgxBaseMeshObject.Create;
    bm.Assign(TgxBaseMeshObject(FBackupInvertedMeshes[i]));
    FBoneMatrixInvertedMeshes.Add(bm);
    TgxBaseMeshObject(FBackupInvertedMeshes[i]).Free;
  end;
  FBackupInvertedMeshes.Clear;
end;

procedure TgxSkeletonMeshObject.ApplyCurrentSkeletonFrame(normalize: Boolean);
var
  i, j, BoneID: Integer;
  refVertices, refNormals: TgxAffineVectorList;
  n, nt: TVector4f;
  Bone: TgxSkeletonBone;
  Skeleton: TgxSkeleton;
  tempvert, tempnorm: TAffineVector;
begin
  with TgxBaseMeshObject(FBoneMatrixInvertedMeshes[0]) do
  begin
    refVertices := Vertices;
    refNormals := normals;
  end;
  Skeleton := Owner.Owner.Skeleton;
  n.W := 0;
  if BonesPerVertex = 1 then
  begin
    // simple case, one bone per vertex
    for i := 0 to refVertices.Count - 1 do
    begin
      BoneID := VerticesBonesWeights^[i]^[0].BoneID;
      Bone := Skeleton.BoneByID(BoneID);
      Vertices.list^[i] := VectorTransform(refVertices.list^[i], Bone.GlobalMatrix);
      PAffineVector(@n)^ := refNormals.list^[i];
      nt := VectorTransform(n, Bone.GlobalMatrix);
      normals.list^[i] := PAffineVector(@nt)^;
    end;
  end
  else
  begin
    // multiple bones per vertex
    for i := 0 to refVertices.Count - 1 do
    begin
      Vertices.list^[i] := NullVector;
      normals.list^[i] := NullVector;
      for j := 0 to BonesPerVertex - 1 do
      begin
        with TgxBaseMeshObject(FBoneMatrixInvertedMeshes[j]) do
        begin
          refVertices := Vertices;
          refNormals := normals;
        end;
        tempvert := NullVector;
        tempnorm := NullVector;
        if VerticesBonesWeights^[i]^[j].weight <> 0 then
        begin
          BoneID := VerticesBonesWeights^[i]^[j].BoneID;
          Bone := Skeleton.BoneByID(BoneID);
          CombineVector(tempvert, VectorTransform(refVertices.list^[i], Bone.GlobalMatrix),
            VerticesBonesWeights^[i]^[j].weight);
          PAffineVector(@n)^ := refNormals.list^[i];
          n := VectorTransform(n, Bone.GlobalMatrix);
          CombineVector(tempnorm, PAffineVector(@n)^, VerticesBonesWeights^[i]^[j].weight);
        end;
        AddVector(Vertices.list^[i], tempvert);
        AddVector(normals.list^[i], tempnorm);
      end;
    end;
  end;
  if normalize then
    normals.normalize;
end;

// ------------------
// ------------------ TgxFaceGroup ------------------
// ------------------

constructor TgxFaceGroup.CreateOwned(aOwner: TgxFaceGroups);
begin
  FOwner := aOwner;
  FLightMapIndex := -1;
  Create;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

destructor TgxFaceGroup.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.Remove(Self);
  inherited;
end;

procedure TgxFaceGroup.WriteToFiler(writer: TgxVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    if FLightMapIndex < 0 then
    begin
      WriteInteger(0); // Archive Version 0
      WriteString(FMaterialName);
    end
    else
    begin
      WriteInteger(1); // Archive Version 1, added FLightMapIndex
      WriteString(FMaterialName);
      WriteInteger(FLightMapIndex);
    end;
  end;
end;

procedure TgxFaceGroup.ReadFromFiler(reader: TgxVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion in [0 .. 1] then
    with reader do
    begin
      FMaterialName := ReadString;
      if archiveVersion >= 1 then
        FLightMapIndex := ReadInteger
      else
        FLightMapIndex := -1;
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TgxFaceGroup.AttachLightmap(lightMap: TgxTexture; var mrci: TgxRenderContextInfo);
begin
  /// if GL_ARB_multitexture then
  with lightMap do
  begin
    Assert(Image.NativeTextureTarget = ttTexture2D);
    mrci.gxStates.TextureBinding[1, ttTexture2D] := Handle;
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

    mrci.gxStates.ActiveTexture := 0;
  end;
end;

procedure TgxFaceGroup.AttachOrDetachLightmap(var mrci: TgxRenderContextInfo);
var
  libMat: TgxLibMaterial;
begin
///  if GL_ARB_multitexture then
    if (not mrci.ignoreMaterials) and Assigned(mrci.LightmapLibrary) then
    begin
      if Owner.Owner.FLastLightMapIndex <> LightMapIndex then
      begin
        Owner.Owner.FLastLightMapIndex := LightMapIndex;
        if LightMapIndex >= 0 then
        begin
          // attach and activate lightmap
          Assert(LightMapIndex < TgxMaterialLibrary(mrci.LightmapLibrary).Materials.Count);
          libMat := TgxMaterialLibrary(mrci.LightmapLibrary).Materials[LightMapIndex];
          AttachLightmap(libMat.Material.Texture, mrci);
          Owner.Owner.EnableLightMapArray(mrci);
        end
        else
        begin
          // desactivate lightmap
          Owner.Owner.DisableLightMapArray(mrci);
        end;
      end;
    end;
end;

procedure TgxFaceGroup.PrepareMaterialLibraryCache(matLib: TgxMaterialLibrary);
begin
  if (FMaterialName <> '') and (matLib <> nil) then
    FMaterialCache := matLib.Materials.GetLibMaterialByName(FMaterialName)
  else
    FMaterialCache := nil;
end;

procedure TgxFaceGroup.DropMaterialLibraryCache;
begin
  FMaterialCache := nil;
end;

procedure TgxFaceGroup.AddToTriangles(aList: TgxAffineVectorList; aTexCoords: TgxAffineVectorList = nil;
  aNormals: TgxAffineVectorList = nil);
begin
  // nothing
end;

procedure TgxFaceGroup.Reverse;
begin
  // nothing
end;

procedure TgxFaceGroup.Prepare;
begin
  // nothing
end;

// ------------------
// ------------------ TfgxVertexIndexList ------------------
// ------------------

constructor TfgxVertexIndexList.Create;
begin
  inherited;
  FVertexIndices := TgxIntegerList.Create;
  FMode := fgmmTriangles;
end;

destructor TfgxVertexIndexList.Destroy;
begin
  FVertexIndices.Free;
  FIndexVBO.Free;
  inherited;
end;

procedure TfgxVertexIndexList.WriteToFiler(writer: TgxVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FVertexIndices.WriteToFiler(writer);
    WriteInteger(Integer(FMode));
  end;
end;

procedure TfgxVertexIndexList.ReadFromFiler(reader: TgxVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FVertexIndices.ReadFromFiler(reader);
      FMode := TgxFaceGroupMeshMode(ReadInteger);
      InvalidateVBO;
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TfgxVertexIndexList.SetupVBO;
const
  BufferUsage = GL_STATIC_DRAW;
begin
  if not Assigned(FIndexVBO) then
    FIndexVBO := TgxVBOElementArrayHandle.Create;

  FIndexVBO.AllocateHandle;

  if FIndexVBO.IsDataNeedUpdate then
  begin
    FIndexVBO.BindBufferData(vertexIndices.list, SizeOf(Integer) * vertexIndices.Count, BufferUsage);
    FIndexVBO.NotifyDataUpdated;
  end;
end;

procedure TfgxVertexIndexList.SetVertexIndices(const val: TgxIntegerList);
begin
  FVertexIndices.Assign(val);
  InvalidateVBO;
end;

procedure TfgxVertexIndexList.BuildList(var mrci: TgxRenderContextInfo);
const
  cFaceGroupMeshModeToOpenGL: array [TgxFaceGroupMeshMode] of Integer = (GL_TRIANGLES, GL_TRIANGLE_STRIP, GL_TRIANGLES,
    GL_TRIANGLE_FAN, GL_QUADS);
begin
  if vertexIndices.Count = 0 then
    Exit;
  Owner.Owner.DeclareArraysToOpenGL(mrci, False);
  AttachOrDetachLightmap(mrci);

  if Owner.Owner.UseVBO then
  begin
    SetupVBO;

    FIndexVBO.Bind;
    glDrawElements(cFaceGroupMeshModeToOpenGL[mode], vertexIndices.Count, GL_UNSIGNED_INT, nil);
    FIndexVBO.UnBind;
  end
  else
  begin
    glDrawElements(cFaceGroupMeshModeToOpenGL[mode], vertexIndices.Count, GL_UNSIGNED_INT, vertexIndices.list);
  end;
end;

procedure TfgxVertexIndexList.AddToList(Source, destination: TgxAffineVectorList; indices: TgxIntegerList);
var
  i, n: Integer;
begin
  if not Assigned(destination) then
    Exit;
  if indices.Count < 3 then
    Exit;
  case mode of
    fgmmTriangles, fgmmFlatTriangles:
      begin
        n := (indices.Count div 3) * 3;
        if Source.Count > 0 then
        begin
          destination.AdjustCapacityToAtLeast(destination.Count + n);
          for i := 0 to n - 1 do
            destination.Add(Source[indices.list^[i]]);
        end
        else
          destination.AddNulls(destination.Count + n);
      end;
    fgmmTriangleStrip:
      begin
        if Source.Count > 0 then
          ConvertStripToList(Source, indices, destination)
        else
          destination.AddNulls(destination.Count + (indices.Count - 2) * 3);
      end;
    fgmmTriangleFan:
      begin
        n := (indices.Count - 2) * 3;
        if Source.Count > 0 then
        begin
          destination.AdjustCapacityToAtLeast(destination.Count + n);
          for i := 2 to vertexIndices.Count - 1 do
          begin
            destination.Add(Source[indices.list^[0]], Source[indices.list^[i - 1]], Source[indices.list^[i]]);
          end;
        end
        else
          destination.AddNulls(destination.Count + n);
      end;
    fgmmQuads:
      begin
        n := indices.Count div 4;
        if Source.Count > 0 then
        begin
          destination.AdjustCapacityToAtLeast(destination.Count + n * 6);
          i := 0;
          while n > 0 do
          begin
            destination.Add(Source[indices.list^[i]], Source[indices.list^[i + 1]], Source[indices.list^[i + 2]]);
            destination.Add(Source[indices.list^[i]], Source[indices.list^[i + 2]], Source[indices.list^[i + 3]]);
            Inc(i, 4);
            Dec(n);
          end;
        end
        else
          destination.AddNulls(destination.Count + n * 6);
      end;
  else
    Assert(False);
  end;
end;

procedure TfgxVertexIndexList.AddToTriangles(aList: TgxAffineVectorList; aTexCoords: TgxAffineVectorList = nil;
  aNormals: TgxAffineVectorList = nil);
var
  mo: TgxMeshObject;
begin
  mo := Owner.Owner;
  AddToList(mo.Vertices, aList, vertexIndices);
  AddToList(mo.texCoords, aTexCoords, vertexIndices);
  AddToList(mo.normals, aNormals, vertexIndices);
  InvalidateVBO;
end;

function TfgxVertexIndexList.TriangleCount: Integer;
begin
  case mode of
    fgmmTriangles, fgmmFlatTriangles:
      Result := vertexIndices.Count div 3;
    fgmmTriangleFan, fgmmTriangleStrip:
      begin
        Result := vertexIndices.Count - 2;
        if Result < 0 then
          Result := 0;
      end;
    fgmmQuads:
      Result := vertexIndices.Count div 2;
  else
    Result := 0;
    Assert(False);
  end;
end;

procedure TfgxVertexIndexList.Reverse;
begin
  vertexIndices.Reverse;
  InvalidateVBO;
end;

procedure TfgxVertexIndexList.Add(idx: Integer);
begin
  FVertexIndices.Add(idx);
  InvalidateVBO;
end;

procedure TfgxVertexIndexList.GetExtents(var min, max: TAffineVector);
var
  i, k: Integer;
  f: Single;
  ref: PFloatArray;
const
  cBigValue: Single = 1E50;
  cSmallValue: Single = -1E50;
begin
  SetVector(min, cBigValue, cBigValue, cBigValue);
  SetVector(max, cSmallValue, cSmallValue, cSmallValue);
  for i := 0 to vertexIndices.Count - 1 do
  begin
    ref := Owner.Owner.Vertices.ItemAddress[vertexIndices[i]];
    for k := 0 to 2 do
    begin
      f := ref^[k];
      if f < min.v[k] then
        min.v[k] := f;
      if f > max.v[k] then
        max.v[k] := f;
    end;
  end;
end;

procedure TfgxVertexIndexList.ConvertToList;
var
  i: Integer;
  bufList: TgxIntegerList;
begin
  if vertexIndices.Count >= 3 then
  begin
    case mode of
      fgmmTriangleStrip:
        begin
          bufList := TgxIntegerList.Create;
          try
            ConvertStripToList(vertexIndices, bufList);
            vertexIndices := bufList;
          finally
            bufList.Free;
          end;
          FMode := fgmmTriangles;
        end;
      fgmmTriangleFan:
        begin
          bufList := TgxIntegerList.Create;
          try
            for i := 0 to vertexIndices.Count - 3 do
              bufList.Add(vertexIndices[0], vertexIndices[i], vertexIndices[i + 1]);
            vertexIndices := bufList;
          finally
            bufList.Free;
          end;
          FMode := fgmmTriangles;
        end;
    end;
    InvalidateVBO;
  end;
end;

function TfgxVertexIndexList.GetNormal: TAffineVector;
begin
  if vertexIndices.Count < 3 then
    Result := NullVector
  else
    with Owner.Owner.Vertices do
      CalcPlaneNormal(Items[vertexIndices[0]], Items[vertexIndices[1]], Items[vertexIndices[2]], Result);
end;

procedure TfgxVertexIndexList.InvalidateVBO;
begin
  if Assigned(FIndexVBO) then
    FIndexVBO.NotifyChangesOfData;
end;

// ------------------
// ------------------ TFGVertexNormalTexIndexList ------------------
// ------------------

constructor TFGVertexNormalTexIndexList.Create;
begin
  inherited;
  FNormalIndices := TgxIntegerList.Create;
  FTexCoordIndices := TgxIntegerList.Create;
end;

destructor TFGVertexNormalTexIndexList.Destroy;
begin
  FTexCoordIndices.Free;
  FNormalIndices.Free;
  inherited;
end;

procedure TFGVertexNormalTexIndexList.WriteToFiler(writer: TgxVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FNormalIndices.WriteToFiler(writer);
    FTexCoordIndices.WriteToFiler(writer);
  end;
end;

procedure TFGVertexNormalTexIndexList.ReadFromFiler(reader: TgxVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FNormalIndices.ReadFromFiler(reader);
      FTexCoordIndices.ReadFromFiler(reader);
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TFGVertexNormalTexIndexList.SetNormalIndices(const val: TgxIntegerList);
begin
  FNormalIndices.Assign(val);
end;

procedure TFGVertexNormalTexIndexList.SetTexCoordIndices(const val: TgxIntegerList);
begin
  FTexCoordIndices.Assign(val);
end;

procedure TFGVertexNormalTexIndexList.BuildList(var mrci: TgxRenderContextInfo);
var
  i: Integer;
  vertexPool: PAffineVectorArray;
  normalPool: PAffineVectorArray;
  texCoordPool: PAffineVectorArray;
  colorPool: PVectorArray;
  normalIdxList, texCoordIdxList, vertexIdxList: PIntegerVector;
begin
  Assert(((TexCoordIndices.Count = 0) or (vertexIndices.Count <= TexCoordIndices.Count)) and
    ((normalIndices.Count = 0) or (vertexIndices.Count <= normalIndices.Count)));
  vertexPool := Owner.Owner.Vertices.list;
  normalPool := Owner.Owner.normals.list;
  colorPool := Owner.Owner.Colors.list;
  texCoordPool := Owner.Owner.texCoords.list;
  case mode of
    fgmmTriangles, fgmmFlatTriangles:
      glBegin(GL_TRIANGLES);
    fgmmTriangleStrip:
      glBegin(GL_TRIANGLE_STRIP);
    fgmmTriangleFan:
      glBegin(GL_TRIANGLE_FAN);
  else
    Assert(False);
  end;
  vertexIdxList := vertexIndices.list;
  if normalIndices.Count > 0 then
    normalIdxList := normalIndices.list
  else
    normalIdxList := vertexIdxList;
  if TexCoordIndices.Count > 0 then
    texCoordIdxList := TexCoordIndices.list
  else
    texCoordIdxList := vertexIdxList;

  for i := 0 to vertexIndices.Count - 1 do
  begin
    glNormal3fv(@normalPool[normalIdxList^[i]]);
    if Assigned(colorPool) then
      glColor4fv(@colorPool[vertexIdxList^[i]]);
    if Assigned(texCoordPool) then
      glTexCoord2fv(@texCoordPool[texCoordIdxList^[i]]);
    glVertex3fv(@vertexPool[vertexIdxList^[i]]);
  end;

  glEnd;
end;

procedure TFGVertexNormalTexIndexList.AddToTriangles(aList: TgxAffineVectorList; aTexCoords: TgxAffineVectorList = nil;
  aNormals: TgxAffineVectorList = nil);
begin
  AddToList(Owner.Owner.Vertices, aList, vertexIndices);
  AddToList(Owner.Owner.texCoords, aTexCoords, TexCoordIndices);
  AddToList(Owner.Owner.normals, aNormals, normalIndices);
end;

procedure TFGVertexNormalTexIndexList.Add(vertexIdx, normalIdx, texCoordIdx: Integer);
begin
  inherited Add(vertexIdx);
  FNormalIndices.Add(normalIdx);
  FTexCoordIndices.Add(texCoordIdx);
end;

// ------------------
// ------------------ TFGIndexTexCoordList ------------------
// ------------------

constructor TFGIndexTexCoordList.Create;
begin
  inherited;
  FTexCoords := TgxAffineVectorList.Create;
end;

destructor TFGIndexTexCoordList.Destroy;
begin
  FTexCoords.Free;
  inherited;
end;

procedure TFGIndexTexCoordList.WriteToFiler(writer: TgxVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FTexCoords.WriteToFiler(writer);
  end;
end;

procedure TFGIndexTexCoordList.ReadFromFiler(reader: TgxVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FTexCoords.ReadFromFiler(reader);
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TFGIndexTexCoordList.SetTexCoords(const val: TgxAffineVectorList);
begin
  FTexCoords.Assign(val);
end;

procedure TFGIndexTexCoordList.BuildList(var mrci: TgxRenderContextInfo);
var
  i, k: Integer;
  texCoordPool: PAffineVectorArray;
  vertexPool: PAffineVectorArray;
  normalPool: PAffineVectorArray;
  indicesPool: PIntegerArray;
  colorPool: PVectorArray;
  gotColor: Boolean;

begin
  Assert(vertexIndices.Count = texCoords.Count);
  texCoordPool := texCoords.list;
  vertexPool := Owner.Owner.Vertices.list;
  indicesPool := @vertexIndices.list[0];
  colorPool := @Owner.Owner.Colors.list[0];
  gotColor := (Owner.Owner.Vertices.Count = Owner.Owner.Colors.Count);

  case mode of
    fgmmTriangles:
      glBegin(GL_TRIANGLES);
    fgmmFlatTriangles:
      glBegin(GL_TRIANGLES);
    fgmmTriangleStrip:
      glBegin(GL_TRIANGLE_STRIP);
    fgmmTriangleFan:
      glBegin(GL_TRIANGLE_FAN);
    fgmmQuads:
      glBegin(GL_QUADS);
  else
    Assert(False);
  end;
  if Owner.Owner.normals.Count = Owner.Owner.Vertices.Count then
  begin
    normalPool := Owner.Owner.normals.list;
    for i := 0 to vertexIndices.Count - 1 do
    begin
      glTexCoord2fv(@texCoordPool[i]);
      k := indicesPool[i];
      if gotColor then
        glColor4fv(@colorPool[k]);
      glNormal3fv(@normalPool[k]);
      glVertex3fv(@vertexPool[k]);
    end;
  end
  else
  begin
    for i := 0 to vertexIndices.Count - 1 do
    begin
      glTexCoord2fv(@texCoordPool[i]);
      if gotColor then
        glColor4fv(@colorPool[indicesPool[i]]);
      glVertex3fv(@vertexPool[indicesPool[i]]);
    end;
  end;
  glEnd;
///  CheckOpenGLError;
end;

procedure TFGIndexTexCoordList.AddToTriangles(aList: TgxAffineVectorList; aTexCoords: TgxAffineVectorList = nil;
  aNormals: TgxAffineVectorList = nil);
var
  i, n: Integer;
  texCoordList: TgxAffineVectorList;
begin
  AddToList(Owner.Owner.Vertices, aList, vertexIndices);
  AddToList(Owner.Owner.normals, aNormals, vertexIndices);
  texCoordList := Self.texCoords;
  case mode of
    fgmmTriangles, fgmmFlatTriangles:
      begin
        if Assigned(aTexCoords) then
        begin
          n := (vertexIndices.Count div 3) * 3;
          aTexCoords.AdjustCapacityToAtLeast(aTexCoords.Count + n);
          for i := 0 to n - 1 do
            aTexCoords.Add(texCoordList[i]);
        end;
      end;
    fgmmTriangleStrip:
      begin
        if Assigned(aTexCoords) then
          ConvertStripToList(aTexCoords, texCoordList);
      end;
    fgmmTriangleFan:
      begin
        if Assigned(aTexCoords) then
        begin
          aTexCoords.AdjustCapacityToAtLeast(aTexCoords.Count + (vertexIndices.Count - 2) * 3);
          for i := 2 to vertexIndices.Count - 1 do
          begin
            aTexCoords.Add(texCoordList[0], texCoordList[i - 1], texCoordList[i]);
          end;
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TFGIndexTexCoordList.Add(idx: Integer; const texCoord: TAffineVector);
begin
  texCoords.Add(texCoord);
  inherited Add(idx);
end;

procedure TFGIndexTexCoordList.Add(idx: Integer; const s, t: Single);
begin
  texCoords.Add(s, t, 0);
  inherited Add(idx);
end;

// ------------------
// ------------------ TgxFaceGroups ------------------
// ------------------

constructor TgxFaceGroups.CreateOwned(aOwner: TgxMeshObject);
begin
  FOwner := aOwner;
  Create;
end;

destructor TgxFaceGroups.Destroy;
begin
  Clear;
  inherited;
end;

procedure TgxFaceGroups.ReadFromFiler(reader: TgxVirtualReader);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

procedure TgxFaceGroups.Clear;
var
  i: Integer;
  fg: TgxFaceGroup;
begin
  for i := 0 to Count - 1 do
  begin
    fg := GetFaceGroup(i);
    if Assigned(fg) then
    begin
      fg.FOwner := nil;
      fg.Free;
    end;
  end;
  inherited;
end;

function TgxFaceGroups.GetFaceGroup(Index: Integer): TgxFaceGroup;
begin
  Result := TgxFaceGroup(list^[Index]);
end;

procedure TgxFaceGroups.PrepareMaterialLibraryCache(matLib: TgxMaterialLibrary);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TgxFaceGroup(list^[i]).PrepareMaterialLibraryCache(matLib);
end;

procedure TgxFaceGroups.DropMaterialLibraryCache;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TgxFaceGroup(list^[i]).DropMaterialLibraryCache;
end;

procedure TgxFaceGroups.AddToTriangles(aList: TgxAffineVectorList; aTexCoords: TgxAffineVectorList = nil;
  aNormals: TgxAffineVectorList = nil);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].AddToTriangles(aList, aTexCoords, aNormals);
end;

function TgxFaceGroups.MaterialLibrary: TgxMaterialLibrary;
var
  mol: TgxMeshObjectList;
  bm: TgxBaseMesh;
begin
  if Assigned(Owner) then
  begin
    mol := Owner.Owner;
    if Assigned(mol) then
    begin
      bm := mol.Owner;
      if Assigned(bm) then
      begin
        Result := bm.MaterialLibrary;;
        Exit;
      end;
    end;
  end;
  Result := nil;
end;

function CompareMaterials(item1, item2: TObject): Integer;

  function MaterialIsOpaque(fg: TgxFaceGroup): Boolean;
  var
    libMat: TgxLibMaterial;
  begin
    libMat := fg.MaterialCache;
    Result := (not Assigned(libMat)) or (not libMat.Material.Blended);
  end;

var
  fg1, fg2: TgxFaceGroup;
  opaque1, opaque2: Boolean;
begin
  fg1 := TgxFaceGroup(item1);
  opaque1 := MaterialIsOpaque(fg1);
  fg2 := TgxFaceGroup(item2);
  opaque2 := MaterialIsOpaque(fg2);
  if opaque1 = opaque2 then
  begin
    Result := CompareStr(fg1.MaterialName, fg2.MaterialName);
    if Result = 0 then
      Result := fg1.LightMapIndex - fg2.LightMapIndex;
  end
  else if opaque1 then
    Result := -1
  else
    Result := 1;
end;

procedure TgxFaceGroups.SortByMaterial;
begin
  PrepareMaterialLibraryCache(Owner.Owner.Owner.MaterialLibrary);
  Sort(@CompareMaterials);
end;

// ------------------
// ------------------ TgxVectorFile ------------------
// ------------------

constructor TgxVectorFile.Create(aOwner: TPersistent);
begin
  Assert(aOwner is TgxBaseMesh);
  inherited;
end;

function TgxVectorFile.Owner: TgxBaseMesh;
begin
  Result := TgxBaseMesh(GetOwner);
end;

procedure TgxVectorFile.SetNormalsOrientation(const val: TMeshNormalsOrientation);
begin
  FNormalsOrientation := val;
end;

// ------------------
// ------------------ TgxGLSMVectorFile ------------------
// ------------------

class function TgxGLSMVectorFile.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

procedure TgxGLSMVectorFile.LoadFromStream(aStream: TStream);
begin
  Owner.MeshObjects.LoadFromStream(aStream);
end;

procedure TgxGLSMVectorFile.SaveToStream(aStream: TStream);
begin
  Owner.MeshObjects.SaveToStream(aStream);
end;

// ------------------
// ------------------ TgxBaseMesh ------------------
// ------------------

constructor TgxBaseMesh.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  if FMeshObjects = nil then
    FMeshObjects := TgxMeshObjectList.CreateOwned(Self);
  if FSkeleton = nil then
    FSkeleton := TgxSkeleton.CreateOwned(Self);
  FUseMeshMaterials := True;
  FAutoCentering := [];
  FAxisAlignedDimensionsCache.X := -1;
  FBaryCenterOffsetChanged := True;
  FAutoScaling := TgxCoordinates.CreateInitialized(Self, XYZWHmgVector, csPoint);
end;

destructor TgxBaseMesh.Destroy;
begin
  FConnectivity.Free;
  DropMaterialLibraryCache;
  FSkeleton.Free;
  FMeshObjects.Free;
  FAutoScaling.Free;
  inherited Destroy;
end;

procedure TgxBaseMesh.Assign(Source: TPersistent);
begin
  if Source is TgxBaseMesh then
  begin
    FSkeleton.Clear;
    FNormalsOrientation := TgxBaseMesh(Source).FNormalsOrientation;
    FMaterialLibrary := TgxBaseMesh(Source).FMaterialLibrary;
    FLightmapLibrary := TgxBaseMesh(Source).FLightmapLibrary;
    FAxisAlignedDimensionsCache := TgxBaseMesh(Source).FAxisAlignedDimensionsCache;
    FBaryCenterOffset := TgxBaseMesh(Source).FBaryCenterOffset;
    FUseMeshMaterials := TgxBaseMesh(Source).FUseMeshMaterials;
    FOverlaySkeleton := TgxBaseMesh(Source).FOverlaySkeleton;
    FIgnoreMissingTextures := TgxBaseMesh(Source).FIgnoreMissingTextures;
    FAutoCentering := TgxBaseMesh(Source).FAutoCentering;
    FAutoScaling.Assign(TgxBaseMesh(Source).FAutoScaling);
    FSkeleton.Assign(TgxBaseMesh(Source).FSkeleton);
    FSkeleton.RootBones.PrepareGlobalMatrices;
    FMeshObjects.Assign(TgxBaseMesh(Source).FMeshObjects);
  end;
  inherited Assign(Source);
end;

procedure TgxBaseMesh.LoadFromFile(const filename: string);
var
  fs: TStream;
begin
  FLastLoadedFilename := '';
  if filename <> '' then
  begin
    fs := TFileStream.Create(filename, fmOpenRead + fmShareDenyWrite);
    try
      LoadFromStream(filename, fs);
      FLastLoadedFilename := filename;
    finally
      fs.Free;
    end;
  end;
end;

procedure TgxBaseMesh.LoadFromStream(const filename: string; aStream: TStream);
var
  newVectorFile: TgxVectorFile;
  VectorFileClass: TgxVectorFileClass;
begin
  FLastLoadedFilename := '';
  if filename <> '' then
  begin
    MeshObjects.Clear;
    Skeleton.Clear;
    VectorFileClass := GetVectorFileFormats.FindFromFileName(filename);
    newVectorFile := VectorFileClass.Create(Self);
    try
      newVectorFile.ResourceName := filename;
      PrepareVectorFile(newVectorFile);
      if Assigned(Scene) then
        Scene.BeginUpdate;
      try
        newVectorFile.LoadFromStream(aStream);
        FLastLoadedFilename := filename;
      finally
        if Assigned(Scene) then
          Scene.EndUpdate;
      end;
    finally
      newVectorFile.Free;
    end;
    PerformAutoScaling;
    PerformAutoCentering;
    PrepareMesh;
  end;
end;

procedure TgxBaseMesh.SaveToFile(const filename: string);
var
  fs: TStream;
begin
  if filename <> '' then
  begin
    fs := TFileStream.Create(filename, fmCreate);
    try
      SaveToStream(filename, fs);
    finally
      fs.Free;
    end;
  end;
end;

procedure TgxBaseMesh.SaveToStream(const filename: string; aStream: TStream);
var
  newVectorFile: TgxVectorFile;
  VectorFileClass: TgxVectorFileClass;
begin
  if filename <> '' then
  begin
    VectorFileClass := GetVectorFileFormats.FindFromFileName(filename);
    newVectorFile := VectorFileClass.Create(Self);
    try
      newVectorFile.ResourceName := filename;
      PrepareVectorFile(newVectorFile);
      newVectorFile.SaveToStream(aStream);
    finally
      newVectorFile.Free;
    end;
  end;
end;

procedure TgxBaseMesh.AddDataFromFile(const filename: string);
var
  fs: TStream;
begin
  if filename <> '' then
  begin
    fs := TFileStream.Create(filename, fmOpenRead + fmShareDenyWrite);
    try
      AddDataFromStream(filename, fs);
    finally
      fs.Free;
    end;
  end;
end;

procedure TgxBaseMesh.AddDataFromStream(const filename: string; aStream: TStream);
var
  newVectorFile: TgxVectorFile;
  VectorFileClass: TgxVectorFileClass;
begin
  if filename <> '' then
  begin
    VectorFileClass := GetVectorFileFormats.FindFromFileName(filename);
    newVectorFile := VectorFileClass.Create(Self);
    newVectorFile.ResourceName := filename;
    PrepareVectorFile(newVectorFile);
    try
      if Assigned(Scene) then
        Scene.BeginUpdate;
      newVectorFile.LoadFromStream(aStream);
      if Assigned(Scene) then
        Scene.EndUpdate;
    finally
      newVectorFile.Free;
    end;
    PrepareMesh;
  end;
end;

procedure TgxBaseMesh.GetExtents(out min, max: TAffineVector);
var
  i, k: Integer;
  lMin, lMax: TAffineVector;
const
  cBigValue: Single = 1E50;
  cSmallValue: Single = -1E50;
begin
  SetVector(min, cBigValue, cBigValue, cBigValue);
  SetVector(max, cSmallValue, cSmallValue, cSmallValue);
  for i := 0 to MeshObjects.Count - 1 do
  begin
    TgxMeshObject(MeshObjects[i]).GetExtents(lMin, lMax);
    for k := 0 to 2 do
    begin
      if lMin.v[k] < min.v[k] then
        min.v[k] := lMin.v[k];
      if lMax.v[k] > max.v[k] then
        max.v[k] := lMax.v[k];
    end;
  end;
end;

function TgxBaseMesh.GetBarycenter: TAffineVector;
var
  i, nb: Integer;
begin
  Result := NullVector;
  nb := 0;
  for i := 0 to MeshObjects.Count - 1 do
    TgxMeshObject(MeshObjects[i]).ContributeToBarycenter(Result, nb);
  if nb > 0 then
    ScaleVector(Result, 1 / nb);
end;

function TgxBaseMesh.LastLoadedFilename: string;
begin
  Result := FLastLoadedFilename;
end;

procedure TgxBaseMesh.SetMaterialLibrary(const val: TgxMaterialLibrary);
begin
  if FMaterialLibrary <> val then
  begin
    if FMaterialLibraryCachesPrepared then
      DropMaterialLibraryCache;
    if Assigned(FMaterialLibrary) then
    begin
      DestroyHandle;
      FMaterialLibrary.RemoveFreeNotification(Self);
    end;
    FMaterialLibrary := val;
    if Assigned(FMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
    StructureChanged;
  end;
end;

procedure TgxBaseMesh.SetLightmapLibrary(const val: TgxMaterialLibrary);
begin
  if FLightmapLibrary <> val then
  begin
    if Assigned(FLightmapLibrary) then
    begin
      DestroyHandle;
      FLightmapLibrary.RemoveFreeNotification(Self);
    end;
    FLightmapLibrary := val;
    if Assigned(FLightmapLibrary) then
      FLightmapLibrary.FreeNotification(Self);
    StructureChanged;
  end;
end;

procedure TgxBaseMesh.SetNormalsOrientation(const val: TMeshNormalsOrientation);
begin
  if val <> FNormalsOrientation then
  begin
    FNormalsOrientation := val;
    StructureChanged;
  end;
end;

procedure TgxBaseMesh.SetOverlaySkeleton(const val: Boolean);
begin
  if FOverlaySkeleton <> val then
  begin
    FOverlaySkeleton := val;
    NotifyChange(Self);
  end;
end;

procedure TgxBaseMesh.SetAutoScaling(const Value: TgxCoordinates);
begin
  FAutoScaling.SetPoint(Value.DirectX, Value.DirectY, Value.DirectZ);
end;

procedure TgxBaseMesh.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FMaterialLibrary then
      MaterialLibrary := nil
    else if AComponent = FLightmapLibrary then
      LightmapLibrary := nil;
  end;
  inherited;
end;

function TgxBaseMesh.AxisAlignedDimensionsUnscaled: TVector4f;
var
  dMin, dMax: TAffineVector;
begin
  if FAxisAlignedDimensionsCache.X < 0 then
  begin
    MeshObjects.GetExtents(dMin, dMax);
    FAxisAlignedDimensionsCache.X := (dMax.X - dMin.X) / 2;
    FAxisAlignedDimensionsCache.Y := (dMax.Y - dMin.Y) / 2;
    FAxisAlignedDimensionsCache.Z := (dMax.Z - dMin.Z) / 2;
    FAxisAlignedDimensionsCache.W := 0;
  end;
  SetVector(Result, FAxisAlignedDimensionsCache);
end;

function TgxBaseMesh.BarycenterOffset: TVector4f;
var
  dMin, dMax: TAffineVector;
begin
  if FBaryCenterOffsetChanged then
  begin
    MeshObjects.GetExtents(dMin, dMax);

    FBaryCenterOffset.X := (dMin.X + dMax.X) / 2;
    FBaryCenterOffset.Y := (dMin.Y + dMax.Y) / 2;
    FBaryCenterOffset.Z := (dMin.Z + dMax.Z) / 2;
    FBaryCenterOffset.W := 0;
    FBaryCenterOffsetChanged := False;
  end;
  Result := FBaryCenterOffset;
end;

function TgxBaseMesh.BarycenterPosition: TVector4f;
begin
  Result := VectorAdd(Position.DirectVector, BarycenterOffset);
end;

function TgxBaseMesh.BarycenterAbsolutePosition: TVector4f;
begin
  Result := LocalToAbsolute(BarycenterPosition);
end;

procedure TgxBaseMesh.DestroyHandle;
begin
  if Assigned(FMaterialLibrary) then
    MaterialLibrary.DestroyHandles;
  if Assigned(FLightmapLibrary) then
    LightmapLibrary.DestroyHandles;
  inherited;
end;

procedure TgxBaseMesh.PrepareVectorFile(aFile: TgxVectorFile);
begin
  aFile.NormalsOrientation := NormalsOrientation;
end;

procedure TgxBaseMesh.PerformAutoCentering;
var
  delta, min, max: TAffineVector;
begin
  if macUseBarycenter in AutoCentering then
  begin
    delta := VectorNegate(GetBarycenter);
  end
  else
  begin
    GetExtents(min, max);
    if macCenterX in AutoCentering then
      delta.X := -0.5 * (min.X + max.X)
    else
      delta.X := 0;
    if macCenterY in AutoCentering then
      delta.Y := -0.5 * (min.Y + max.Y)
    else
      delta.Y := 0;
    if macCenterZ in AutoCentering then
      delta.Z := -0.5 * (min.Z + max.Z)
    else
      delta.Z := 0;
  end;
  MeshObjects.Translate(delta);

  if macRestorePosition in AutoCentering then
    Position.Translate(VectorNegate(delta));
end;

procedure TgxBaseMesh.PerformAutoScaling;
var
  i: Integer;
  vScal: TAffineFltVector;
begin
  if (FAutoScaling.DirectX <> 1) or (FAutoScaling.DirectY <> 1) or (FAutoScaling.DirectZ <> 1) then
  begin
    MakeVector(vScal, FAutoScaling.DirectX, FAutoScaling.DirectY, FAutoScaling.DirectZ);
    for i := 0 to MeshObjects.Count - 1 do
    begin
      MeshObjects[i].Vertices.Scale(vScal);
    end;
  end;
end;

procedure TgxBaseMesh.PrepareMesh;
begin
  StructureChanged;
end;

procedure TgxBaseMesh.PrepareMaterialLibraryCache;
begin
  if FMaterialLibraryCachesPrepared then
    DropMaterialLibraryCache;
  MeshObjects.PrepareMaterialLibraryCache(FMaterialLibrary);
  FMaterialLibraryCachesPrepared := True;
end;

procedure TgxBaseMesh.DropMaterialLibraryCache;
begin
  if FMaterialLibraryCachesPrepared then
  begin
    MeshObjects.DropMaterialLibraryCache;
    FMaterialLibraryCachesPrepared := False;
  end;
end;

procedure TgxBaseMesh.PrepareBuildList(var mrci: TgxRenderContextInfo);
begin
  MeshObjects.PrepareBuildList(mrci);
  if LightmapLibrary <> nil then
    LightmapLibrary.Materials.PrepareBuildList
end;

procedure TgxBaseMesh.SetUseMeshMaterials(const val: Boolean);
begin
  if val <> FUseMeshMaterials then
  begin
    FUseMeshMaterials := val;
    if FMaterialLibraryCachesPrepared and (not val) then
      DropMaterialLibraryCache;
    StructureChanged;
  end;
end;

procedure TgxBaseMesh.BuildList(var rci: TgxRenderContextInfo);
begin
  MeshObjects.BuildList(rci);
end;

procedure TgxBaseMesh.DoRender(var rci: TgxRenderContextInfo; renderSelf, renderChildren: Boolean);
begin
  if Assigned(LightmapLibrary) then
    xglForbidSecondTextureUnit;
  if renderSelf then
  begin
    // set winding
    case FNormalsOrientation of
      mnoDefault:
        ; // nothing
      mnoInvert:
        rci.gxStates.InvertFrontFace;
    else
      Assert(False);
    end;
    if not rci.ignoreMaterials then
    begin
      if UseMeshMaterials and Assigned(MaterialLibrary) then
      begin
        rci.MaterialLibrary := MaterialLibrary;
        if not FMaterialLibraryCachesPrepared then
          PrepareMaterialLibraryCache;
      end
      else
        rci.MaterialLibrary := nil;
      if Assigned(LightmapLibrary) then
        rci.LightmapLibrary := LightmapLibrary
      else
        rci.LightmapLibrary := nil;
      if rci.amalgamating or not(ListHandleAllocated or (osDirectDraw in ObjectStyle)) then
        PrepareBuildList(rci);
      Material.Apply(rci);
      repeat
        if (osDirectDraw in ObjectStyle) or rci.amalgamating or UseMeshMaterials then
          BuildList(rci)
        else
          rci.gxStates.CallList(GetHandle(rci));
      until not Material.UnApply(rci);
      rci.MaterialLibrary := nil;
    end
    else
    begin
      if (osDirectDraw in ObjectStyle) or rci.amalgamating then
        BuildList(rci)
      else
        rci.gxStates.CallList(GetHandle(rci));
    end;
    if FNormalsOrientation <> mnoDefault then
      rci.gxStates.InvertFrontFace;
  end;
  if Assigned(LightmapLibrary) then
    xglAllowSecondTextureUnit;
  if renderChildren and (Count > 0) then
    Self.renderChildren(0, Count - 1, rci);
end;

procedure TgxBaseMesh.StructureChanged;
begin
  FAxisAlignedDimensionsCache.X := -1;
  FBaryCenterOffsetChanged := True;
  DropMaterialLibraryCache;
  MeshObjects.Prepare;
  inherited;
end;

procedure TgxBaseMesh.StructureChangedNoPrepare;
begin
  inherited StructureChanged;
end;

function TgxBaseMesh.RayCastIntersect(const rayStart, rayVector: TVector4f; intersectPoint: PVector4f = nil;
  intersectNormal: PVector4f = nil): Boolean;
var
  i: Integer;
  tris: TgxAffineVectorList;
  locRayStart, locRayVector, iPoint, iNormal: TVector4f;
  d, minD: Single;
begin
  // BEWARE! Utterly inefficient implementation!
  tris := MeshObjects.ExtractTriangles;
  try
    SetVector(locRayStart, AbsoluteToLocal(rayStart));
    SetVector(locRayVector, AbsoluteToLocal(rayVector));
    minD := -1;
    i := 0;
    while i < tris.Count do
    begin
      if RayCastTriangleIntersect(locRayStart, locRayVector, tris.list^[i], tris.list^[i + 1], tris.list^[i + 2], @iPoint,
        @iNormal) then
      begin
        d := VectorDistance2(locRayStart, iPoint);
        if (d < minD) or (minD < 0) then
        begin
          minD := d;
          if intersectPoint <> nil then
            intersectPoint^ := iPoint;
          if intersectNormal <> nil then
            intersectNormal^ := iNormal;
        end;
      end;
      Inc(i, 3);
    end;
  finally
    tris.Free;
  end;
  Result := (minD >= 0);
  if Result then
  begin
    if intersectPoint <> nil then
      SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
    if intersectNormal <> nil then
    begin
      SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      if NormalsOrientation = mnoInvert then
        NegateVector(intersectNormal^);
    end;
  end;
end;

function TgxBaseMesh.GenerateSilhouette(const SilhouetteParameters: TgxSilhouetteParameters): TgxSilhouette;
var
  mc: TgxBaseMeshConnectivity;
  sil: TgxSilhouette;
begin
  sil := nil;
  if Assigned(FConnectivity) then
  begin
    mc := TgxBaseMeshConnectivity(FConnectivity);
    mc.CreateSilhouette(silhouetteParameters, sil, True);
  end
  else
  begin
    mc := TgxBaseMeshConnectivity.CreateFromMesh(Self);
    try
      mc.CreateSilhouette(silhouetteParameters, sil, True);
    finally
      mc.Free;
    end;
  end;
  Result := sil;
end;

procedure TgxBaseMesh.BuildSilhouetteConnectivityData;
var
  i, j: Integer;
  mo: TgxMeshObject;
begin
  FreeAndNil(FConnectivity);
  // connectivity data works only on facegroups of TfgxVertexIndexList class
  for i := 0 to MeshObjects.Count - 1 do
  begin
    mo := (MeshObjects[i] as TgxMeshObject);
    if mo.mode <> momFaceGroups then
      Exit;
    for j := 0 to mo.FaceGroups.Count - 1 do
      if not mo.FaceGroups[j].InheritsFrom(TfgxVertexIndexList) then
        Exit;
  end;
  FConnectivity := TgxBaseMeshConnectivity.CreateFromMesh(Self);
end;

// ------------------
// ------------------ TgxFreeForm ------------------
// ------------------

constructor TgxFreeForm.Create(aOwner: TComponent);
begin
  inherited;
  // ObjectStyle := [osDirectDraw];
  FUseMeshMaterials := True;
end;

destructor TgxFreeForm.Destroy;
begin
  FOctree.Free;
  inherited Destroy;
end;

function TgxFreeForm.GetOctree: TgxOctree;
begin
  // if not Assigned(FOctree) then     //If auto-created, can never use "if Assigned(GLFreeform1.Octree)"
  // FOctree:=TOctree.Create;        //moved this code to BuildOctree
  Result := FOctree;
end;

procedure TgxFreeForm.BuildOctree(TreeDepth: Integer = 3);
var
  emin, emax: TAffineVector;
  tl: TgxAffineVectorList;
begin
  if not Assigned(FOctree) then // moved here from GetOctree
    FOctree := TgxOctree.Create;

  GetExtents(emin, emax);
  tl := MeshObjects.ExtractTriangles;
  try
    with Octree do
    begin
      DisposeTree;
      InitializeTree(emin, emax, tl, TreeDepth);
    end;
  finally
    tl.Free;
  end;
end;

function TgxFreeForm.OctreeRayCastIntersect(const rayStart, rayVector: TVector4f; intersectPoint: PVector4f = nil;
  intersectNormal: PVector4f = nil): Boolean;
var
  locRayStart, locRayVector: TVector4f;
begin
  Assert(Assigned(FOctree), 'Octree must have been prepared and setup before use.');
  SetVector(locRayStart, AbsoluteToLocal(rayStart));
  SetVector(locRayVector, AbsoluteToLocal(rayVector));
  Result := Octree.RayCastIntersect(locRayStart, locRayVector, intersectPoint, intersectNormal);
  if Result then
  begin
    if intersectPoint <> nil then
      SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
    if intersectNormal <> nil then
    begin
      SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      if NormalsOrientation = mnoInvert then
        NegateVector(intersectNormal^);
    end;
  end;
end;

function TgxFreeForm.OctreePointInMesh(const Point: TVector4f): Boolean;
const
  cPointRadiusStep = 10000;
var
  rayStart, rayVector, hitPoint, hitNormal: TVector4f;
  BRad: double;
  HitCount: Integer;
  hitDot: double;
begin
  Assert(Assigned(FOctree), 'Octree must have been prepared and setup before use.');

  Result := False;

  // Makes calculations sligthly faster by ignoring cases that are guaranteed
  // to be outside the object
  if not PointInObject(Point) then
    Exit;

  BRad := BoundingSphereRadius;

  // This could be a fixed vector, but a fixed vector could have a systemic
  // bug on an non-closed mesh, making it fail constantly for one or several
  // faces.
  rayVector := VectorMake(2 * random - 1, 2 * random - 1, 2 * random - 1);
  rayStart := VectorAdd(VectorScale(rayVector, -BRad), Point);

  HitCount := 0;

  while OctreeRayCastIntersect(rayStart, rayVector, @hitPoint, @hitNormal) do
  begin
    // Are we past our taget?
    if VectorDotProduct(rayVector, VectorSubtract(Point, hitPoint)) < 0 then
    begin
      Result := HitCount > 0;
      Exit;
    end;

    hitDot := VectorDotProduct(hitNormal, rayVector);
    if hitDot < 0 then
      Inc(HitCount)
    else if hitDot > 0 then
      Dec(HitCount);

    // ditDot = 0 is a tricky special case where the ray is just grazing the
    // side of a face - this case means that it doesn't necessarily actually
    // enter the mesh - but it _could_ enter the mesh. If this situation occurs,
    // we should restart the run using a new rayVector - but this implementation
    // currently doesn't.

    // Restart the ray slightly beyond the point it hit the previous face. Note
    // that this step introduces a possible issue with faces that are very close
    rayStart := VectorAdd(hitPoint, VectorScale(rayVector, BRad / cPointRadiusStep));
  end;
end;

function TgxFreeForm.OctreeSphereSweepIntersect(const rayStart, rayVector: TVector4f; const velocity, radius: Single;
  intersectPoint: PVector4f = nil; intersectNormal: PVector4f = nil): Boolean;
var
  locRayStart, locRayVector: TVector4f;
begin
  Assert(Assigned(FOctree), 'Octree must have been prepared and setup before use.');
  SetVector(locRayStart, AbsoluteToLocal(rayStart));
  SetVector(locRayVector, AbsoluteToLocal(rayVector));
  Result := Octree.SphereSweepIntersect(locRayStart, locRayVector, velocity, radius, intersectPoint, intersectNormal);
  if Result then
  begin
    if intersectPoint <> nil then
      SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
    if intersectNormal <> nil then
    begin
      SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      if NormalsOrientation = mnoInvert then
        NegateVector(intersectNormal^);
    end;
  end;
end;

function TgxFreeForm.OctreeTriangleIntersect(const v1, v2, v3: TAffineVector): Boolean;
var
  t1, t2, t3: TAffineVector;
begin
  Assert(Assigned(FOctree), 'Octree must have been prepared and setup before use.');
  SetVector(t1, AbsoluteToLocal(v1));
  SetVector(t2, AbsoluteToLocal(v2));
  SetVector(t3, AbsoluteToLocal(v3));

  Result := Octree.TriangleIntersect(t1, t2, t3);
end;

function TgxFreeForm.OctreeAABBIntersect(const aabb: TAABB; objMatrix, invObjMatrix: TMatrix4f;
  triangles: TgxAffineVectorList = nil): Boolean;
var
  m1to2, m2to1: TMatrix4f;
begin
  Assert(Assigned(FOctree), 'Octree must have been prepared and setup before use.');

  // get matrixes needed
  // object to self
  MatrixMultiply(objMatrix, InvAbsoluteMatrix, m1to2);
  // self to object
  MatrixMultiply(AbsoluteMatrix, invObjMatrix, m2to1);

  Result := Octree.AABBIntersect(aabb, m1to2, m2to1, triangles);
end;

// ------------------
// ------------------ TgxActorAnimation ------------------
// ------------------

constructor TgxActorAnimation.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TgxActorAnimation.Destroy;
begin
  with (Collection as TgxActorAnimations).FOwner do
    if FTargetSmoothAnimation = Self then
      FTargetSmoothAnimation := nil;
  inherited Destroy;
end;

procedure TgxActorAnimation.Assign(Source: TPersistent);
begin
  if Source is TgxActorAnimation then
  begin
    FName := TgxActorAnimation(Source).FName;
    FStartFrame := TgxActorAnimation(Source).FStartFrame;
    FEndFrame := TgxActorAnimation(Source).FEndFrame;
    FReference := TgxActorAnimation(Source).FReference;
  end
  else
    inherited;
end;

function TgxActorAnimation.GetDisplayName: string;
begin
  Result := Format('%d - %s [%d - %d]', [Index, Name, startFrame, endFrame]);
end;

function TgxActorAnimation.FrameCount: Integer;
begin
  case reference of
    aarMorph:
      Result := TgxActorAnimations(Collection).FOwner.MeshObjects.MorphTargetCount;
    aarSkeleton:
      Result := TgxActorAnimations(Collection).FOwner.Skeleton.Frames.Count;
  else
    Result := 0;
    Assert(False);
  end;
end;

procedure TgxActorAnimation.SetStartFrame(const val: Integer);
var
  m: Integer;
begin
  if val < 0 then
    FStartFrame := 0
  else
  begin
    m := FrameCount;
    if val >= m then
      FStartFrame := m - 1
    else
      FStartFrame := val;
  end;
  if FStartFrame > FEndFrame then
    FEndFrame := FStartFrame;
end;

procedure TgxActorAnimation.SetEndFrame(const val: Integer);
var
  m: Integer;
begin
  if val < 0 then
    FEndFrame := 0
  else
  begin
    m := FrameCount;
    if val >= m then
      FEndFrame := m - 1
    else
      FEndFrame := val;
  end;
  if FStartFrame > FEndFrame then
    FStartFrame := FEndFrame;
end;

procedure TgxActorAnimation.SetReference(val: TgxActorAnimationReference);
begin
  if val <> FReference then
  begin
    FReference := val;
    startFrame := startFrame;
    endFrame := endFrame;
  end;
end;

procedure TgxActorAnimation.SetAsString(const val: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.CommaText := val;
    Assert(sl.Count >= 3);
    FName := sl[0];
    FStartFrame := StrToInt(sl[1]);
    FEndFrame := StrToInt(sl[2]);
    if sl.Count = 4 then
    begin
      if LowerCase(sl[3]) = 'morph' then
        reference := aarMorph
      else if LowerCase(sl[3]) = 'skeleton' then
        reference := aarSkeleton
      else
        Assert(False);
    end
    else
      reference := aarMorph;
  finally
    sl.Free;
  end;
end;

function TgxActorAnimation.GetAsString: string;
const
  cAARToString: array [aarMorph .. aarSkeleton] of string = ('morph', 'skeleton');
begin
  Result := Format('"%s",%d,%d,%s', [FName, FStartFrame, FEndFrame, cAARToString[reference]]);
end;

function TgxActorAnimation.OwnerActor: TgxActor;
begin
  Result := ((Collection as TgxActorAnimations).GetOwner as TgxActor);
end;

procedure TgxActorAnimation.MakeSkeletalTranslationStatic;
begin
  OwnerActor.Skeleton.MakeSkeletalTranslationStatic(startFrame, endFrame);
end;

procedure TgxActorAnimation.MakeSkeletalRotationDelta;
begin
  OwnerActor.Skeleton.MakeSkeletalRotationDelta(startFrame, endFrame);
end;

// ------------------
// ------------------ TgxActorAnimations ------------------
// ------------------

constructor TgxActorAnimations.Create(aOwner: TgxActor);
begin
  FOwner := aOwner;
  inherited Create(TgxActorAnimation);
end;

function TgxActorAnimations.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TgxActorAnimations.SetItems(Index: Integer; const val: TgxActorAnimation);
begin
  inherited Items[index] := val;
end;

function TgxActorAnimations.GetItems(Index: Integer): TgxActorAnimation;
begin
  Result := TgxActorAnimation(inherited Items[index]);
end;

function TgxActorAnimations.Last: TgxActorAnimation;
begin
  if Count > 0 then
    Result := TgxActorAnimation(inherited Items[Count - 1])
  else
    Result := nil;
end;

function TgxActorAnimations.Add: TgxActorAnimation;
begin
  Result := (inherited Add) as TgxActorAnimation;
end;

function TgxActorAnimations.FindItemID(ID: Integer): TgxActorAnimation;
begin
  Result := (inherited FindItemID(ID)) as TgxActorAnimation;
end;

function TgxActorAnimations.FindName(const aName: string): TgxActorAnimation;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if CompareText(Items[i].Name, aName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TgxActorAnimations.FindFrame(aFrame: Integer; aReference: TgxActorAnimationReference): TgxActorAnimation;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    with Items[i] do
      if (startFrame <= aFrame) and (endFrame >= aFrame) and (reference = aReference) then
      begin
        Result := Items[i];
        Break;
      end;
end;

procedure TgxActorAnimations.SetToStrings(aStrings: TStrings);

var
  i: Integer;
begin
  with aStrings do
  begin
    BeginUpdate;
    Clear;
    for i := 0 to Self.Count - 1 do
      Add(Self.Items[i].Name);
    EndUpdate;
  end;
end;

procedure TgxActorAnimations.SaveToStream(aStream: TStream);
var
  i: Integer;
begin
  WriteCRLFString(aStream, cAAFHeader);
  WriteCRLFString(aStream, AnsiString(IntToStr(Count)));
  for i := 0 to Count - 1 do
    WriteCRLFString(aStream, AnsiString(Items[i].AsString));
end;

procedure TgxActorAnimations.LoadFromStream(aStream: TStream);
var
  i, n: Integer;
begin
  Clear;
  if ReadCRLFString(aStream) <> cAAFHeader then
    Assert(False);
  n := StrToInt(string(ReadCRLFString(aStream)));
  for i := 0 to n - 1 do
    Add.AsString := string(ReadCRLFString(aStream));
end;

procedure TgxActorAnimations.SaveToFile(const filename: string);
var
  fs: TStream;
begin
  fs := TFileStream.Create(filename, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TgxActorAnimations.LoadFromFile(const filename: string);
var
  fs: TStream;
begin
  fs := TFileStream.Create(filename, fmOpenRead + fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

// ------------------
// ------------------ TgxBaseAnimationControler ------------------
// ------------------

constructor TgxBaseAnimationControler.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FEnabled := True;
end;

destructor TgxBaseAnimationControler.Destroy;
begin
  SetActor(nil);
  inherited Destroy;
end;

procedure TgxBaseAnimationControler.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = FActor) and (Operation = opRemove) then
    SetActor(nil);
  inherited;
end;

procedure TgxBaseAnimationControler.DoChange;
begin
  if Assigned(FActor) then
    FActor.NotifyChange(Self);
end;

procedure TgxBaseAnimationControler.SetEnabled(const val: Boolean);
begin
  if val <> FEnabled then
  begin
    FEnabled := val;
    if Assigned(FActor) then
      DoChange;
  end;
end;

procedure TgxBaseAnimationControler.SetActor(const val: TgxActor);
begin
  if FActor <> val then
  begin
    if Assigned(FActor) then
      FActor.UnRegisterControler(Self);
    FActor := val;
    if Assigned(FActor) then
    begin
      FActor.RegisterControler(Self);
      DoChange;
    end;
  end;
end;

function TgxBaseAnimationControler.Apply(var lerpInfo: TgxBlendedLerpInfo): Boolean;
begin
  // virtual
  Result := False;
end;

// ------------------
// ------------------ TgxAnimationControler ------------------
// ------------------

procedure TgxAnimationControler.DoChange;
begin
  if AnimationName <> '' then
    inherited;
end;

procedure TgxAnimationControler.SetAnimationName(const val: TgxActorAnimationName);
begin
  if FAnimationName <> val then
  begin
    FAnimationName := val;
    DoChange;
  end;
end;

procedure TgxAnimationControler.SetRatio(const val: Single);
begin
  if FRatio <> val then
  begin
    FRatio := ClampValue(val, 0, 1);
    DoChange;
  end;
end;

function TgxAnimationControler.Apply(var lerpInfo: TgxBlendedLerpInfo): Boolean;
var
  anim: TgxActorAnimation;
  baseDelta: Integer;
begin
  if not Enabled then
  begin
    Result := False;
    Exit;
  end;

  anim := Actor.Animations.FindName(AnimationName);
  Result := (anim <> nil);
  if not Result then
    Exit;

  with lerpInfo do
  begin
    if Ratio = 0 then
    begin
      frameIndex1 := anim.startFrame;
      frameIndex2 := frameIndex1;
      lerpFactor := 0;
    end
    else if Ratio = 1 then
    begin
      frameIndex1 := anim.endFrame;
      frameIndex2 := frameIndex1;
      lerpFactor := 0;
    end
    else
    begin
      baseDelta := anim.endFrame - anim.startFrame;
      lerpFactor := anim.startFrame + baseDelta * Ratio;
      frameIndex1 := Trunc(lerpFactor);
      frameIndex2 := frameIndex1 + 1;
      lerpFactor := Frac(lerpFactor);
    end;
    weight := 1;
    externalRotations := nil;
    externalQuaternions := nil;
  end;
end;

// ------------------
// ------------------ TgxActor ------------------
// ------------------

constructor TgxActor.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FFrameInterpolation := afpLinear;
  FAnimationMode := aamNone;
  FInterval := 100; // 10 animation frames per second
  FAnimations := TgxActorAnimations.Create(Self);
  FControlers := nil; // created on request
  FOptions := cDefaultActorOptions;
end;

destructor TgxActor.Destroy;
begin
  inherited Destroy;
  FControlers.Free;
  FAnimations.Free;
end;

procedure TgxActor.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TgxActor then
  begin
    FAnimations.Assign(TgxActor(Source).FAnimations);
    FAnimationMode := TgxActor(Source).FAnimationMode;
    Synchronize(TgxActor(Source));
  end;
end;

procedure TgxActor.RegisterControler(aControler: TgxBaseAnimationControler);
begin
  if not Assigned(FControlers) then
    FControlers := TList.Create;
  FControlers.Add(aControler);
  FreeNotification(aControler);
end;

procedure TgxActor.UnRegisterControler(aControler: TgxBaseAnimationControler);
begin
  Assert(Assigned(FControlers));
  FControlers.Remove(aControler);
  RemoveFreeNotification(aControler);
  if FControlers.Count = 0 then
    FreeAndNil(FControlers);
end;

procedure TgxActor.SetCurrentFrame(val: Integer);
begin
  if val <> CurrentFrame then
  begin
    if val > FrameCount - 1 then
      FCurrentFrame := FrameCount - 1
    else if val < 0 then
      FCurrentFrame := 0
    else
      FCurrentFrame := val;
    FCurrentFrameDelta := 0;
    case AnimationMode of
      aamPlayOnce:
        if (CurrentFrame = endFrame) and (FTargetSmoothAnimation = nil) then
          FAnimationMode := aamNone;
      aamBounceForward:
        if CurrentFrame = endFrame then
          FAnimationMode := aamBounceBackward;
      aamBounceBackward:
        if CurrentFrame = startFrame then
          FAnimationMode := aamBounceForward;
    end;
    StructureChanged;
    if Assigned(FOnFrameChanged) then
      FOnFrameChanged(Self);
  end;
end;

procedure TgxActor.SetCurrentFrameDirect(const Value: Integer);
begin
  FCurrentFrame := Value;
end;

procedure TgxActor.SetStartFrame(val: Integer);
begin
  if (val >= 0) and (val < FrameCount) and (val <> startFrame) then
    FStartFrame := val;
  if endFrame < startFrame then
    FEndFrame := FStartFrame;
  if CurrentFrame < startFrame then
    CurrentFrame := FStartFrame;
end;

procedure TgxActor.SetEndFrame(val: Integer);
begin
  if (val >= 0) and (val < FrameCount) and (val <> endFrame) then
    FEndFrame := val;
  if CurrentFrame > endFrame then
    CurrentFrame := FEndFrame;
end;

procedure TgxActor.SetReference(val: TgxActorAnimationReference);
begin
  if val <> reference then
  begin
    FReference := val;
    startFrame := startFrame;
    endFrame := endFrame;
    CurrentFrame := CurrentFrame;
    StructureChanged;
  end;
end;

procedure TgxActor.SetAnimations(const val: TgxActorAnimations);
begin
  FAnimations.Assign(val);
end;

function TgxActor.StoreAnimations: Boolean;
begin
  Result := (FAnimations.Count > 0);
end;

procedure TgxActor.SetOptions(const val: TgxActorOptions);
begin
  if val <> FOptions then
  begin
    FOptions := val;
    StructureChanged;
  end;
end;

function TgxActor.NextFrameIndex: Integer;
begin
  case AnimationMode of
    aamLoop, aamBounceForward:
      begin
        if FTargetSmoothAnimation <> nil then
          Result := FTargetSmoothAnimation.startFrame
        else
        begin
          Result := CurrentFrame + 1;
          if Result > endFrame then
          begin
            Result := startFrame + (Result - endFrame - 1);
            if Result > endFrame then
              Result := endFrame;
          end;
        end;
      end;
    aamNone, aamPlayOnce:
      begin
        if FTargetSmoothAnimation <> nil then
          Result := FTargetSmoothAnimation.startFrame
        else
        begin
          Result := CurrentFrame + 1;
          if Result > endFrame then
            Result := endFrame;
        end;
      end;
    aamBounceBackward, aamLoopBackward:
      begin
        if FTargetSmoothAnimation <> nil then
          Result := FTargetSmoothAnimation.startFrame
        else
        begin
          Result := CurrentFrame - 1;
          if Result < startFrame then
          begin
            Result := endFrame - (startFrame - Result - 1);
            if Result < startFrame then
              Result := startFrame;
          end;
        end;
      end;
    aamExternal:
      Result := CurrentFrame; // Do nothing
  else
    Result := CurrentFrame;
    Assert(False);
  end;
end;

procedure TgxActor.NextFrame(nbSteps: Integer = 1);
var
  n: Integer;
begin
  n := nbSteps;
  while n > 0 do
  begin
    CurrentFrame := NextFrameIndex;
    Dec(n);
    if Assigned(FOnEndFrameReached) and (CurrentFrame = endFrame) then
      FOnEndFrameReached(Self);
    if Assigned(FOnStartFrameReached) and (CurrentFrame = startFrame) then
      FOnStartFrameReached(Self);
  end;
end;

procedure TgxActor.PrevFrame(nbSteps: Integer = 1);
var
  Value: Integer;
begin
  Value := FCurrentFrame - nbSteps;
  if Value < FStartFrame then
  begin
    Value := FEndFrame - (FStartFrame - Value);
    if Value < FStartFrame then
      Value := FStartFrame;
  end;
  CurrentFrame := Value;
end;

procedure TgxActor.DoAnimate();
var
  i, k: Integer;
  nextFrameIdx: Integer;
  lerpInfos: array of TgxBlendedLerpInfo;
begin
  nextFrameIdx := NextFrameIndex;
  case reference of
    aarMorph:
      if nextFrameIdx >= 0 then
      begin
        case FrameInterpolation of
          afpLinear:
            MeshObjects.Lerp(CurrentFrame, nextFrameIdx, CurrentFrameDelta)
        else
          MeshObjects.MorphTo(CurrentFrame);
        end;
      end;
    aarSkeleton:
      if Skeleton.Frames.Count > 0 then
      begin
        if Assigned(FControlers) and (AnimationMode <> aamExternal) then
        begin
          // Blended Skeletal Lerping
          SetLength(lerpInfos, FControlers.Count + 1);
          if nextFrameIdx >= 0 then
          begin
            case FrameInterpolation of
              afpLinear:
                with lerpInfos[0] do
                begin
                  frameIndex1 := CurrentFrame;
                  frameIndex2 := nextFrameIdx;
                  lerpFactor := CurrentFrameDelta;
                  weight := 1;
                end;
            else
              with lerpInfos[0] do
              begin
                frameIndex1 := CurrentFrame;
                frameIndex2 := CurrentFrame;
                lerpFactor := 0;
                weight := 1;
              end;
            end;
          end
          else
          begin
            with lerpInfos[0] do
            begin
              frameIndex1 := CurrentFrame;
              frameIndex2 := CurrentFrame;
              lerpFactor := 0;
              weight := 1;
            end;
          end;
          k := 1;
          for i := 0 to FControlers.Count - 1 do
            if TgxBaseAnimationControler(FControlers[i]).Apply(lerpInfos[k]) then
              Inc(k);
          SetLength(lerpInfos, k);
          Skeleton.BlendedLerps(lerpInfos);
        end
        else if (nextFrameIdx >= 0) and (AnimationMode <> aamExternal) then
        begin
          // Single Skeletal Lerp
          case FrameInterpolation of
            afpLinear:
              Skeleton.Lerp(CurrentFrame, nextFrameIdx, CurrentFrameDelta);
          else
            Skeleton.SetCurrentFrame(Skeleton.Frames[CurrentFrame]);
          end;
        end;
        Skeleton.MorphMesh(aoSkeletonNormalizeNormals in Options);
      end;
    aarNone:
      ; // do nothing
  end;
end;

procedure TgxActor.BuildList(var rci: TgxRenderContextInfo);
begin
  DoAnimate;
  inherited;
  if OverlaySkeleton then
  begin
    rci.gxStates.Disable(stDepthTest);
    Skeleton.RootBones.BuildList(rci);
  end;
end;

procedure TgxActor.PrepareMesh;
begin
  FStartFrame := 0;
  FEndFrame := FrameCount - 1;
  FCurrentFrame := 0;
  if Assigned(FOnFrameChanged) then
    FOnFrameChanged(Self);
  inherited;
end;

procedure TgxActor.PrepareBuildList(var mrci: TgxRenderContextInfo);
begin
  // no preparation needed for actors, they don't use buildlists
end;

function TgxActor.FrameCount: Integer;
begin
  case reference of
    aarMorph:
      Result := MeshObjects.MorphTargetCount;
    aarSkeleton:
      Result := Skeleton.Frames.Count;
    aarNone:
      Result := 0;
  else
    Result := 0;
    Assert(False);
  end;
end;

procedure TgxActor.DoProgress(const progressTime: TgxProgressTimes);
var
  fDelta: Single;
begin
  inherited;
  if (AnimationMode <> aamNone) and (Interval > 0) then
  begin
    if (startFrame <> endFrame) and (FrameCount > 1) then
    begin
      FCurrentFrameDelta := FCurrentFrameDelta + (progressTime.deltaTime * 1000) / FInterval;
      if FCurrentFrameDelta > 1 then
      begin
        if Assigned(FTargetSmoothAnimation) then
        begin
          SwitchToAnimation(FTargetSmoothAnimation);
          FTargetSmoothAnimation := nil;
        end;
        // we need to step on
        fDelta := Frac(FCurrentFrameDelta);
        NextFrame(Trunc(FCurrentFrameDelta));
        FCurrentFrameDelta := fDelta;
        StructureChanged;
      end
      else if FrameInterpolation <> afpNone then
        StructureChanged;
    end;
  end;
end;

procedure TgxActor.LoadFromStream(const filename: string; aStream: TStream);
begin
  if filename <> '' then
  begin
    Animations.Clear;
    inherited LoadFromStream(filename, aStream);
  end;
end;

procedure TgxActor.SwitchToAnimation(const AnimationName: string; smooth: Boolean = False);
begin
  SwitchToAnimation(Animations.FindName(AnimationName), smooth);
end;

procedure TgxActor.SwitchToAnimation(animationIndex: Integer; smooth: Boolean = False);
begin
  if (animationIndex >= 0) and (animationIndex < Animations.Count) then
    SwitchToAnimation(Animations[animationIndex], smooth);
end;

procedure TgxActor.SwitchToAnimation(anAnimation: TgxActorAnimation; smooth: Boolean = False);
begin
  if Assigned(anAnimation) then
  begin
    if smooth then
    begin
      FTargetSmoothAnimation := anAnimation;
      FCurrentFrameDelta := 0;
    end
    else
    begin
      reference := anAnimation.reference;
      startFrame := anAnimation.startFrame;
      endFrame := anAnimation.endFrame;
      CurrentFrame := startFrame;
    end;
  end;
end;

function TgxActor.CurrentAnimation: string;
var
  aa: TgxActorAnimation;
begin
  aa := Animations.FindFrame(CurrentFrame, reference);
  if Assigned(aa) then
    Result := aa.Name
  else
    Result := '';
end;

procedure TgxActor.Synchronize(referenceActor: TgxActor);
begin
  if Assigned(referenceActor) then
  begin
    if referenceActor.startFrame < FrameCount then
      FStartFrame := referenceActor.startFrame;
    if referenceActor.endFrame < FrameCount then
      FEndFrame := referenceActor.endFrame;
    FReference := referenceActor.reference;
    if referenceActor.CurrentFrame < FrameCount then
      FCurrentFrame := referenceActor.CurrentFrame;
    FCurrentFrameDelta := referenceActor.CurrentFrameDelta;
    FAnimationMode := referenceActor.AnimationMode;
    FFrameInterpolation := referenceActor.FrameInterpolation;
    if referenceActor.FTargetSmoothAnimation <> nil then
      FTargetSmoothAnimation := Animations.FindName(referenceActor.FTargetSmoothAnimation.Name)
    else
      FTargetSmoothAnimation := nil;
    if (Skeleton.Frames.Count > 0) and (referenceActor.Skeleton.Frames.Count > 0) then
      Skeleton.Synchronize(referenceActor.Skeleton);
  end;
end;

function TgxActor.isSwitchingAnimation: Boolean;
begin
  Result := FTargetSmoothAnimation <> nil;
end;

// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------

RegisterVectorFileFormat('glsm', 'GXScene Mesh', TgxGLSMVectorFile);

RegisterClasses([TgxFreeForm, TgxActor, TgxSkeleton, TgxSkeletonFrame, TgxSkeletonBone, TgxSkeletonMeshObject, TgxMeshObject,
  TgxSkeletonFrameList, TgxMeshMorphTarget, TgxMorphableMeshObject, TgxFaceGroup, TfgxVertexIndexList,
  TFGVertexNormalTexIndexList, TgxAnimationControler, TFGIndexTexCoordList, TgxSkeletonCollider, TgxSkeletonColliderList]);

finalization

FreeAndNil(vVectorFileFormats);

end.
