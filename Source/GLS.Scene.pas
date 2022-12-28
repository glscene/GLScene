//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.Scene;

(* Base classes and structures *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,

  GLS.OpenGLTokens,
  GLS.XOpenGL,
  GLS.XCollection,
  GLS.Strings,
  GLS.Context,
  GLS.VectorGeometry,
  GLS.Silhouette,
  GLS.PersistentClasses,
  GLS.PipelineTransformation,
  GLS.State,
  GLS.Graphics,
  GLS.GeometryBB,
  GLS.VectorLists,
  GLS.Texture,
  GLS.Color,
  GLS.BaseClasses,
  GLS.Coordinates,
  GLS.RenderContextInfo,
  GLS.Material,
  GLS.TextureFormat,
  GLS.Selection,
  GLS.VectorTypes,
  GLS.ApplicationFileIO,
  GLS.Utils,
  GLS.Logger;

type
  //Defines which features are taken from the master object.
  TGLProxyObjectOption = (pooEffects, pooObjects, pooTransformation);
  TGLProxyObjectOptions = set of TGLProxyObjectOption;
  TGLCameraInvarianceMode = (cimNone, cimPosition, cimOrientation);
  TGLSceneViewerMode = (svmDisabled, svmDefault, svmNavigation, svmGizmo);

const
  cDefaultProxyOptions = [pooEffects, pooObjects, pooTransformation];
  GLSCENE_REVISION = '$Revision: 1$';
  GLSCENE_VERSION = 'v2023 %s';

type

  TGLNormalDirection = (ndInside, ndOutside);

  // Used to describe the changes in an object, which have to be reflected in the scene
  TGLObjectChange = (ocTransformation, ocAbsoluteMatrix, ocInvAbsoluteMatrix, ocStructure);
  TGLObjectChanges = set of TGLObjectChange;
  TGLObjectBBChange = (oBBcChild, oBBcStructure);
  TGLObjectBBChanges = set of TGLObjectBBChange;

  // Flags for design notification
  TGLSceneOperation = (soAdd, soRemove, soMove, soRename, soSelect, soBeginUpdate, soEndUpdate);

  (* Options for the rendering context.
     roSoftwareMode: force software rendering.
     roDoubleBuffer: enables double-buffering.
     roRenderToWindows: ignored (legacy).
     roTwoSideLighting: enables two-side lighting model.
     roStereo: enables stereo support in the driver (dunno if it works,
         I don't have a stereo device to test...)
     roDestinationAlpha: request an Alpha channel for the rendered output
     roNoColorBuffer: don't request a color buffer (color depth setting ignored)
     roNoColorBufferClear: do not clear the color buffer automatically, if the
         whole viewer is fully repainted each frame, this can improve framerate
     roNoSwapBuffers: don't perform RenderingContext.SwapBuffers after rendering
     roNoDepthBufferClear: do not clear the depth buffer automatically. Useful for early-z culling.
     roForwardContext: force OpenGL forward context *)
  TGLContextOption = (roSoftwareMode, roDoubleBuffer, roStencilBuffer,
    roRenderToWindow, roTwoSideLighting, roStereo,
    roDestinationAlpha, roNoColorBuffer, roNoColorBufferClear,
    roNoSwapBuffers, roNoDepthBufferClear, roDebugContext, roForwardContext, roOpenGL_ES2_Context);
  TGLContextOptions = set of TGLContextOption;

  // IDs for limit determination
  TGLLimitType = (limClipPlanes, limEvalOrder, limLights, limListNesting,
    limModelViewStack, limNameStack, limPixelMapTable, limProjectionStack,
    limTextureSize, limTextureStack, limViewportDims, limAccumAlphaBits,
    limAccumBlueBits, limAccumGreenBits, limAccumRedBits, limAlphaBits,
    limAuxBuffers, limBlueBits, limGreenBits, limRedBits, limIndexBits,
    limStereo, limDoubleBuffer, limSubpixelBits, limDepthBits, limStencilBits,
    limNbTextureUnits);

  TGLBaseSceneObject = class;
  TGLSceneObjectClass = class of TGLBaseSceneObject;
  TGLCustomSceneObject = class;
  TGLScene = class;
  TGLBehaviour = class;
  TGLBehaviourClass = class of TGLBehaviour;
  TGLBehaviours = class;
  TGLEffect = class;
  TGLEffectClass = class of TGLEffect;
  TGLEffects = class;
  TGLSceneBuffer = class;

  (* Possible styles/options for a GLScene object. Allowed styles are:
   osDirectDraw : object shall not make use of compiled call lists, but issue
        direct calls each time a render should be performed.
   osIgnoreDepthBuffer : object is rendered with depth test disabled,
        this is true for its children too.
   osNoVisibilityCulling : whatever the VisibilityCulling setting,
        it will be ignored and the object rendered  *)
  TGLObjectStyle = (
    osDirectDraw,
    osIgnoreDepthBuffer,
    osNoVisibilityCulling);
  TGLObjectStyles = set of TGLObjectStyle;

  // Interface to objects that need initialization
  IGLInitializable = interface
    ['{EA40AE8E-79B3-42F5-ADF1-7A901B665E12}']
    procedure InitializeObject(ASender: TObject; const ARci:
      TGLRenderContextInfo);
  end;

  // Just a list of objects that support IGLInitializable.
  TGLInitializableObjectList = class(TList)
  private
    function GetItems(const Index: Integer): IGLInitializable;
    procedure PutItems(const Index: Integer; const Value: IGLInitializable);
  public
    function Add(const Item: IGLInitializable): Integer;
    property Items[const Index: Integer]: IGLInitializable read GetItems write
    PutItems; default;
  end;

  (* Base class for all scene objects.
     A scene object is part of scene hierarchy (each scene object can have
     multiple children), this hierarchy primarily defines transformations
     (each child coordinates are relative to its parent), but is also used
     for depth-sorting, bounding and visibility culling purposes.
     Subclasses implement either visual scene objects (that are made to be
     visible at runtime, like a Cube) or structural objects (that influence
     rendering or are used for varied structural manipulations,
     like the ProxyObject).
     To add children at runtime, use the AddNewChild method of TGLBaseSceneObject;
     other children manipulations methods and properties are provided (to browse,
     move and delete them). Using the regular TComponent methods is not encouraged *)
  TGLBaseSceneObject = class(TGLCoordinatesUpdateAbleComponent)
  private
    FAbsoluteMatrix, FInvAbsoluteMatrix: TGLMatrix;
    FLocalMatrix: TGLMatrix;
    FObjectStyle: TGLObjectStyles;
    FListHandle: TGLListHandle; // created on 1st use
    FPosition: TGLCoordinates;
    FDirection, FUp: TGLCoordinates;
    FScaling: TGLCoordinates;
    FChanges: TGLObjectChanges;
    FParent: TGLBaseSceneObject;
    FScene: TGLScene;
    FBBChanges: TGLObjectBBChanges;
    FBoundingBoxPersonalUnscaled: THmgBoundingBox;
    FBoundingBoxOfChildren: THmgBoundingBox;
    FBoundingBoxIncludingChildren: THmgBoundingBox;
    FChildren: TGLPersistentObjectList; // created on 1st use
    FVisible: Boolean;
    FUpdateCount: Integer;
    FShowAxes: Boolean;
    FRotation: TGLCoordinates; // current rotation angles
    FIsCalculating: Boolean;
    FObjectsSorting: TGLObjectsSorting;
    FVisibilityCulling: TGLVisibilityCulling;
    FOnProgress: TGLProgressEvent;
    FOnAddedToParent: TNotifyEvent;
    FBehaviours: TGLBehaviours;
    FEffects: TGLEffects;
    FPickable: Boolean;
    FOnPicked: TNotifyEvent;
    FTagObject: TObject;
    FTagFloat: Single;

    objList: TGLPersistentObjectList;
    distList: TGLSingleList;
    ///  FOriginalFiler: TFiler;   //used to allow persistent events in behaviours & effects
    (* If somebody could look at DefineProperties, ReadBehaviours, ReadEffects
     and verify code is safe to use then it could be uncommented *)
    function Get(Index: Integer): TGLBaseSceneObject; inline;
    function GetCount: Integer; inline;
    function GetIndex: Integer; inline;
    procedure SetParent(const val: TGLBaseSceneObject); inline;
    procedure SetIndex(aValue: Integer);
    procedure SetDirection(AVector: TGLCoordinates);
    procedure SetUp(AVector: TGLCoordinates);
    function GetMatrix: PGLMatrix; inline;
    procedure SetPosition(APosition: TGLCoordinates);
    procedure SetPitchAngle(AValue: Single);
    procedure SetRollAngle(AValue: Single);
    procedure SetTurnAngle(AValue: Single);
    procedure SetRotation(aRotation: TGLCoordinates);
    function GetPitchAngle: Single; inline;
    function GetTurnAngle: Single; inline;
    function GetRollAngle: Single; inline;
    procedure SetShowAxes(AValue: Boolean);
    procedure SetScaling(AValue: TGLCoordinates);
    procedure SetObjectsSorting(const val: TGLObjectsSorting);
    procedure SetVisibilityCulling(const val: TGLVisibilityCulling);
    procedure SetBehaviours(const val: TGLBehaviours);
    function GetBehaviours: TGLBehaviours;
    procedure SetEffects(const val: TGLEffects);
    function GetEffects: TGLEffects;
    function GetAbsoluteAffineScale: TAffineVector;
    function GetAbsoluteScale: TGLVector;
    procedure SetAbsoluteAffineScale(const Value: TAffineVector);
    procedure SetAbsoluteScale(const Value: TGLVector);
    function GetAbsoluteMatrix: TGLMatrix; inline;
    procedure SetAbsoluteMatrix(const Value: TGLMatrix);
    procedure SetBBChanges(const Value: TGLObjectBBChanges);
    function GetDirectAbsoluteMatrix: PGLMatrix;
    function GetLocalMatrix: PGLMatrix; inline;
  protected
    procedure Loaded; override;
    procedure SetScene(const Value: TGLScene); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure WriteBehaviours(stream: TStream);
    procedure ReadBehaviours(stream: TStream);
    procedure WriteEffects(stream: TStream);
    procedure ReadEffects(stream: TStream);
    procedure WriteRotations(stream: TStream);
    procedure ReadRotations(stream: TStream);
    function GetVisible: Boolean; virtual;
    function GetPickable: Boolean; virtual;
    procedure SetVisible(aValue: Boolean); virtual;
    procedure SetPickable(aValue: Boolean); virtual;
    procedure SetAbsolutePosition(const v: TGLVector);
    function GetAbsolutePosition: TGLVector; inline;
    procedure SetAbsoluteUp(const v: TGLVector);
    function GetAbsoluteUp: TGLVector;
    procedure SetAbsoluteDirection(const v: TGLVector);
    function GetAbsoluteDirection: TGLVector;
    function GetAbsoluteAffinePosition: TAffineVector;
    procedure SetAbsoluteAffinePosition(const Value: TAffineVector);
    procedure SetAbsoluteAffineUp(const v: TAffineVector);
    function GetAbsoluteAffineUp: TAffineVector;
    procedure SetAbsoluteAffineDirection(const v: TAffineVector);
    function GetAbsoluteAffineDirection: TAffineVector;
    procedure RecTransformationChanged; inline;
    procedure DrawAxes(var rci: TGLRenderContextInfo; pattern: Word);
    procedure GetChildren(AProc: TGetChildProc; Root: TComponent); override;
    // Should the object be considered as blended for sorting purposes?
    function Blended: Boolean; virtual;
    procedure RebuildMatrix;
    procedure SetName(const NewName: TComponentName); override;
    procedure SetParentComponent(Value: TComponent); override;
    procedure DestroyHandle; virtual;
    procedure DestroyHandles;
    procedure DeleteChildCameras;
    procedure DoOnAddedToParent; virtual;
    (* Used to re-calculate BoundingBoxes every time we need it.
       GetLocalUnscaleBB() must return the local BB, not the axis-aligned one.
       By default it is calculated from AxisAlignedBoundingBoxUnscaled and
       BarycenterAbsolutePosition, but for most objects there is a more
       efficient method, that's why it is virtual. *)
    procedure CalculateBoundingBoxPersonalUnscaled(var ANewBoundingBox:
      THmgBoundingBox); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateAsChild(aParentOwner: TGLBaseSceneObject);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    (* Controls and adjusts internal optimizations based on object's style.
       Advanced user only. *)
    property ObjectStyle: TGLObjectStyles read FObjectStyle write FObjectStyle;
    (* Returns the handle to the object's build list.
       Use with caution! Some objects don't support buildlists! *)
    function GetHandle(var rci: TGLRenderContextInfo): Cardinal;
    function ListHandleAllocated: Boolean; inline;
    (* The local transformation (relative to parent).
       If you're *sure* the local matrix is up-to-date, you may use LocalMatrix
       for quicker access. *)
    procedure SetMatrix(const aValue: TGLMatrix); inline;
    property Matrix: PGLMatrix read GetMatrix;
    (* Holds the local transformation (relative to parent).
       If you're not *sure* the local matrix is up-to-date, use Matrix property. *)
    property LocalMatrix: PGLMatrix read GetLocalMatrix;
    (* Forces the local matrix to the specified value.
       AbsoluteMatrix, InverseMatrix, etc. will honour that change, but
       may become invalid if the specified matrix isn't orthonormal (can
       be used for specific rendering or projection effects).
       The local matrix will be reset by the next TransformationChanged,
       position or attitude change. *)
    procedure ForceLocalMatrix(const aMatrix: TGLMatrix); inline;
    // See AbsoluteMatrix.
    function AbsoluteMatrixAsAddress: PGLMatrix;
    (* Holds the absolute transformation matrix.
       If you're not *sure* the absolute matrix is up-to-date,
       use the AbsoluteMatrix property, this one may be nil... *)
    property DirectAbsoluteMatrix: PGLMatrix read GetDirectAbsoluteMatrix;
    (* Calculates the object's absolute inverse matrix.
       Multiplying an absolute coordinate with this matrix gives a local coordinate.
       The current implem uses transposition(AbsoluteMatrix), which is true
       unless you're using some scaling... *)
    function InvAbsoluteMatrix: TGLMatrix; inline;
    //See InvAbsoluteMatrix.
    function InvAbsoluteMatrixAsAddress: PGLMatrix;
    (* The object's absolute matrix by composing all local matrices.
       Multiplying a local coordinate with this matrix gives an absolute coordinate. *)
    property AbsoluteMatrix: TGLMatrix read GetAbsoluteMatrix write SetAbsoluteMatrix;
    // Direction vector in absolute coordinates.
    property AbsoluteDirection: TGLVector read GetAbsoluteDirection write SetAbsoluteDirection;
    property AbsoluteAffineDirection: TAffineVector read GetAbsoluteAffineDirection write SetAbsoluteAffineDirection;
    (* Scale vector in absolute coordinates.
       Warning: SetAbsoluteScale() does not work correctly at the moment. *)
    property AbsoluteScale: TGLVector read GetAbsoluteScale write SetAbsoluteScale;
    property AbsoluteAffineScale: TAffineVector read GetAbsoluteAffineScale write SetAbsoluteAffineScale;
    // Up vector in absolute coordinates.
    property AbsoluteUp: TGLVector read GetAbsoluteUp write SetAbsoluteUp;
    property AbsoluteAffineUp: TAffineVector read GetAbsoluteAffineUp write SetAbsoluteAffineUp;
    // Calculate the right vector in absolute coordinates.
    function AbsoluteRight: TGLVector;
    // Calculate the left vector in absolute coordinates.
    function AbsoluteLeft: TGLVector;
    // Computes and allows to set the object's absolute coordinates.
    property AbsolutePosition: TGLVector read GetAbsolutePosition write SetAbsolutePosition;
    property AbsoluteAffinePosition: TAffineVector read GetAbsoluteAffinePosition write SetAbsoluteAffinePosition;
    function AbsolutePositionAsAddress: PGLVector;
    // Returns the Absolute X Vector expressed in local coordinates.
    function AbsoluteXVector: TGLVector;
    // Returns the Absolute Y Vector expressed in local coordinates.
    function AbsoluteYVector: TGLVector;
    // Returns the Absolute Z Vector expressed in local coordinates.
    function AbsoluteZVector: TGLVector;
    // Converts a vector/point from absolute coordinates to local coordinates.
    function AbsoluteToLocal(const v: TGLVector): TGLVector; overload;
    // Converts a vector from absolute coordinates to local coordinates.
    function AbsoluteToLocal(const v: TAffineVector): TAffineVector; overload;
    // Converts a vector/point from local coordinates to absolute coordinates.
    function LocalToAbsolute(const v: TGLVector): TGLVector; overload;
    // Converts a vector from local coordinates to absolute coordinates.
    function LocalToAbsolute(const v: TAffineVector): TAffineVector; overload;
    // Returns the Right vector (based on Up and Direction)
    function Right: TGLVector; inline;
    // Returns the Left vector (based on Up and Direction)
    function LeftVector: TGLVector; inline;
    // Returns the Right vector (based on Up and Direction)
    function AffineRight: TAffineVector; inline;
    // Returns the Left vector (based on Up and Direction)
    function AffineLeftVector: TAffineVector; inline;
    (* Calculates the object's square distance to a point/object.
       pt is assumed to be in absolute coordinates,
       AbsolutePosition is considered as being the object position. *)
    function SqrDistanceTo(anObject: TGLBaseSceneObject): Single; overload;
    function SqrDistanceTo(const pt: TGLVector): Single; overload;
    function SqrDistanceTo(const pt: TAffineVector): Single; overload;
    (* Computes the object's distance to a point/object.
       Only objects AbsolutePositions are considered. *)
    function DistanceTo(anObject: TGLBaseSceneObject): Single; overload;
    function DistanceTo(const pt: TAffineVector): Single; overload;
    function DistanceTo(const pt: TGLVector): Single; overload;
    (* Calculates the object's barycenter in absolute coordinates.
       Default behaviour is to consider Barycenter=AbsolutePosition
       (whatever the number of children).
       SubClasses where AbsolutePosition is not the barycenter should
       override this method as it is used for distance calculation, during
       rendering for instance, and may lead to visual inconsistencies. *)
    function BarycenterAbsolutePosition: TGLVector; virtual;
    // Calculates the object's barycenter distance to a point.
    function BarycenterSqrDistanceTo(const pt: TGLVector): Single;
    (* Shall returns the object's axis aligned extensions.
       The dimensions are measured from object center and are expressed
       with scale accounted for, in the object's coordinates (not in absolute ones).
       Default value is half the object's Scale. *)
    function AxisAlignedDimensions: TGLVector; virtual;
    function AxisAlignedDimensionsUnscaled: TGLVector; virtual;
    (* Calculates and return the AABB for the object.
       The AABB is currently calculated from the BB.
       There is  no  caching scheme for them. *)
    function AxisAlignedBoundingBox(const AIncludeChilden: Boolean = True): TAABB;
    function AxisAlignedBoundingBoxUnscaled(const AIncludeChilden: Boolean = True): TAABB;
    function AxisAlignedBoundingBoxAbsolute(const AIncludeChilden: Boolean =
      True; const AUseBaryCenter: Boolean = False): TAABB;
    (* Advanced AABB functions that use a caching scheme.
       Also they include children and use BaryCenter. *)
    function AxisAlignedBoundingBoxEx: TAABB;
    function AxisAlignedBoundingBoxAbsoluteEx: TAABB;
    (* Calculates and return the Bounding Box for the object.
       The BB is calculated  each  time this method is invoked,
       based on the AxisAlignedDimensions of the object and that of its
       children. There is  no  caching scheme for them. *)
    function BoundingBox(const AIncludeChilden: Boolean = True; const
      AUseBaryCenter: Boolean = False): THmgBoundingBox;
    function BoundingBoxUnscaled(const AIncludeChilden: Boolean = True; const
      AUseBaryCenter: Boolean = False): THmgBoundingBox;
    function BoundingBoxAbsolute(const AIncludeChilden: Boolean = True; const
      AUseBaryCenter: Boolean = False): THmgBoundingBox;
    (* Advanced BB functions that use a caching scheme.
       Also they include children and use BaryCenter. *)
    function BoundingBoxPersonalUnscaledEx: THmgBoundingBox;
    function BoundingBoxOfChildrenEx: THmgBoundingBox;
    function BoundingBoxIncludingChildrenEx: THmgBoundingBox;
    // Max distance of corners of the BoundingBox.
    function BoundingSphereRadius: Single; inline;
    function BoundingSphereRadiusUnscaled: Single; inline;
    (* Indicates if a point is within an object.
       Given coordinate is an absolute coordinate.
       Linear or surfacic objects shall always return False.
       Default value is based on AxisAlignedDimension and a cube bounding. *)
    function PointInObject(const point: TGLVector): Boolean; virtual;
    (* Request to determine an intersection with a casted ray.
       Given coordinates & vector are in absolute coordinates, rayVector
       must be normalized.
       rayStart may be a point inside the object, allowing retrieval of
       the multiple intersects of the ray.
       When intersectXXX parameters are nil (default) implementation should
       take advantage of this to optimize calculus, if not, and an intersect
       is found, non nil parameters should be defined.
       The intersectNormal needs NOT be normalized by the implementations.
       Default value is based on bounding sphere. *)
    function RayCastIntersect(const rayStart, rayVector: TGLVector;
      intersectPoint: PGLVector = nil;
      intersectNormal: PGLVector = nil): Boolean; virtual;
    (* Request to generate silhouette outlines.
       Default implementation assumes the objects is a sphere of
       AxisAlignedDimensionUnscaled size. Subclasses may choose to return
       nil instead, which will be understood as an empty silhouette. *)
    function GenerateSilhouette(const silhouetteParameters:
      TGLSilhouetteParameters): TGLSilhouette; virtual;
    property Children[Index: Integer]: TGLBaseSceneObject read Get; default;
    property Count: Integer read GetCount;
    property Index: Integer read GetIndex write SetIndex;
    // Creates a new scene object and add it to this object as new child
    function AddNewChild(aChild: TGLSceneObjectClass): TGLBaseSceneObject; virtual;
    // Creates a new scene object and add it to this object as first child
    function AddNewChildFirst(aChild: TGLSceneObjectClass): TGLBaseSceneObject; virtual;
    procedure AddChild(aChild: TGLBaseSceneObject); virtual;
    function GetOrCreateBehaviour(aBehaviour: TGLBehaviourClass): TGLBehaviour;
    function AddNewBehaviour(aBehaviour: TGLBehaviourClass): TGLBehaviour;
    function GetOrCreateEffect(aEffect: TGLEffectClass): TGLEffect;
    function AddNewEffect(aEffect: TGLEffectClass): TGLEffect;
    function HasSubChildren: Boolean;
    procedure DeleteChildren; virtual;
    procedure Insert(aIndex: Integer; aChild: TGLBaseSceneObject); virtual;
    (* Takes a scene object out of the child list, but doesn't destroy it.
       If 'KeepChildren' is true its children will be kept as new children
       in this scene object. *)
    procedure Remove(aChild: TGLBaseSceneObject; keepChildren: Boolean); virtual;
    function IndexOfChild(aChild: TGLBaseSceneObject): Integer;
    function FindChild(const aName: string; ownChildrenOnly: Boolean): TGLBaseSceneObject;
    (* The "safe" version of this procedure checks if indexes are inside
       the list. If not, no exception if raised. *)
    procedure ExchangeChildrenSafe(anIndex1, anIndex2: Integer);
    (* The "regular" version of this procedure does not perform any checks
      and calls FChildren.Exchange directly. User should/can perform range checks manualy. *)
    procedure ExchangeChildren(anIndex1, anIndex2: Integer);
    //These procedures are safe.
    procedure MoveChildUp(anIndex: Integer);
    procedure MoveChildDown(anIndex: Integer);
    procedure MoveChildFirst(anIndex: Integer);
    procedure MoveChildLast(anIndex: Integer);
    procedure DoProgress(const progressTime: TGLProgressTimes); override;
    procedure MoveTo(newParent: TGLBaseSceneObject); virtual;
    procedure MoveUp;
    procedure MoveDown;
    procedure MoveFirst;
    procedure MoveLast;
    procedure BeginUpdate; inline;
    procedure EndUpdate; inline;
    (* Make object-specific geometry description here.
       Subclasses should MAINTAIN OpenGL states (restore the states if
       they were altered). *)
    procedure BuildList(var rci: TGLRenderContextInfo); virtual;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override; final;
    function IsUpdating: Boolean; inline;
    // Moves the object along the Up vector (move up/down)
    procedure Lift(ADistance: Single);
    // Moves the object along the direction vector
    procedure Move(ADistance: Single);
    // Translates the object
    procedure Translate(tx, ty, tz: Single);
    procedure MoveObjectAround(anObject: TGLBaseSceneObject; pitchDelta, turnDelta: Single);
    procedure MoveObjectAllAround(anObject: TGLBaseSceneObject; pitchDelta, turnDelta: Single);
    procedure Pitch(angle: Single);
    procedure Roll(angle: Single);
    procedure Turn(angle: Single);
    (* Sets all rotations to zero and restores default Direction/Up.
       Using this function then applying roll/pitch/turn in the order that
       suits you, you can give an "absolute" meaning to rotation angles
       (they are still applied locally though).
       Scale and Position are not affected. *)
    procedure ResetRotations;
    //Reset rotations and applies them back in the specified order.
    procedure ResetAndPitchTurnRoll(const degX, degY, degZ: Single);
    //Applies rotations around absolute X, Y and Z axis.
    procedure RotateAbsolute(const rx, ry, rz: Single); overload;
    //Applies rotations around the absolute given vector (angle in degrees).
    procedure RotateAbsolute(const axis: TAffineVector; angle: Single); overload;
    // Moves camera along the right vector (move left and right)
    procedure Slide(ADistance: Single);
    // Orients the object toward a target object
    procedure PointTo(const ATargetObject: TGLBaseSceneObject; const AUpVector: TGLVector); overload;
    // Orients the object toward a target absolute position
    procedure PointTo(const AAbsolutePosition, AUpVector: TGLVector); overload;
    procedure Render(var ARci: TGLRenderContextInfo);
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); virtual;
    procedure RenderChildren(firstChildIndex, lastChildIndex: Integer;
      var rci: TGLRenderContextInfo);
    procedure StructureChanged; virtual;
    procedure ClearStructureChanged; inline;
    // Recalculate an orthonormal system
    procedure CoordinateChanged(Sender: TGLCustomCoordinates); override;
    procedure TransformationChanged; inline;
    procedure NotifyChange(Sender: TObject); override;
    property Rotation: TGLCoordinates read FRotation write SetRotation;
    property PitchAngle: Single read GetPitchAngle write SetPitchAngle;
    property RollAngle: Single read GetRollAngle write SetRollAngle;
    property TurnAngle: Single read GetTurnAngle write SetTurnAngle;
    property ShowAxes: Boolean read FShowAxes write SetShowAxes default False;
    property Changes: TGLObjectChanges read FChanges;
    property BBChanges: TGLObjectBBChanges read fBBChanges write SetBBChanges;
    property Parent: TGLBaseSceneObject read FParent write SetParent;
    property Position: TGLCoordinates read FPosition write SetPosition;
    property Direction: TGLCoordinates read FDirection write SetDirection;
    property Up: TGLCoordinates read FUp write SetUp;
    property Scale: TGLCoordinates read FScaling write SetScaling;
    property Scene: TGLScene read FScene;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Pickable: Boolean read FPickable write SetPickable default True;
    property ObjectsSorting: TGLObjectsSorting read FObjectsSorting write
      SetObjectsSorting default osInherited;
    property VisibilityCulling: TGLVisibilityCulling read FVisibilityCulling
      write SetVisibilityCulling default vcInherited;
    property OnProgress: TGLProgressEvent read FOnProgress write FOnProgress;
    property OnPicked: TNotifyEvent read FOnPicked write FOnPicked;
    property OnAddedToParent: TNotifyEvent read FOnAddedToParent write FOnAddedToParent;
    property Behaviours: TGLBehaviours read GetBehaviours write SetBehaviours stored False;
    property Effects: TGLEffects read GetEffects write SetEffects stored False;
    property TagObject: TObject read FTagObject write FTagObject;
  published
    property TagFloat: Single read FTagFloat write FTagFloat;
  end;

  (* Base class for implementing behaviours in TGLScene.
     Behaviours are regrouped in a collection attached to a TGLBaseSceneObject,
     and are part of the "Progress" chain of events. Behaviours allows clean
     application of time-based alterations to objects (movements, shape or
     texture changes...).
     Since behaviours are implemented as classes, there are basicly two kinds
     of strategies for subclasses :
      stand-alone : the subclass does it all, and holds all necessary data
        (covers animation, inertia etc.)
      proxy : the subclass is an interface to and external, shared operator
        (like gravity, force-field effects etc.)

     Some behaviours may be cooperative (like force-fields affects inertia)
     or unique (e.g. only one inertia behaviour per object).
     NOTES : Don't forget to override the ReadFromFiler/WriteToFiler persistence
     methods if you add data in a subclass !
     Subclasses must be registered using the RegisterXCollectionItemClass function *)
  TGLBaseBehaviour = class(TXCollectionItem)
  protected
    procedure SetName(const val: string); override;
    // Override this function to write subclass data.
    procedure WriteToFiler(writer: TWriter); override;
    // Override this function to read subclass data.
    procedure ReadFromFiler(reader: TReader); override;
    (* Returns the TGLBaseSceneObject on which the behaviour should be applied.
       Does NOT check for nil owners. *)
    function OwnerBaseSceneObject: TGLBaseSceneObject;
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure DoProgress(const progressTime: TGLProgressTimes); virtual;
  end;

  (* Ancestor for non-rendering behaviours.
     This class shall never receive any properties, it's just here to differentiate
     rendereing and non-rendering behaviours. Rendereing behaviours are named
     "TGLEffect", non-rendering effects (like inertia) are simply named
     "TGLBehaviour". *)
  TGLBehaviour = class(TGLBaseBehaviour)
  end;

  (* Holds a list of TGLBehaviour objects.
     This object expects itself to be owned by a TGLBaseSceneObject.
     As a TXCollection (and contrary to a TCollection), this list can contain
     objects of varying class, the only constraint being that they should all
     be TGLBehaviour subclasses. *)
  TGLBehaviours = class(TXCollection)
  protected
    function GetBehaviour(index: Integer): TGLBehaviour;
  public
    constructor Create(aOwner: TPersistent); override;
    function GetNamePath: string; override;
    class function ItemsClass: TXCollectionItemClass; override;
    property Behaviour[index: Integer]: TGLBehaviour read GetBehaviour; default;
    function CanAdd(aClass: TXCollectionItemClass): Boolean; override;
    procedure DoProgress(const progressTimes: TGLProgressTimes); inline;
  end;

  (* A rendering effect that can be applied to SceneObjects.
     ObjectEffect is a subclass of behaviour that gets a chance to Render
     an object-related special effect.
     TGLEffect should not be used as base class for custom effects,
     instead you should use the following base classes :
     TGLObjectPreEffect is rendered before owner object render
     TGLObjectPostEffect is rendered after the owner object render
     TGLObjectAfterEffect is rendered at the end of the scene rendering
     NOTES :
     Don't forget to override the ReadFromFiler/WriteToFiler persistence
       methods if you add data in a subclass !
     Subclasses must be registered using the RegisterXCollectionItemClass function *)
  TGLEffect = class(TGLBaseBehaviour)
  protected
    // Override this function to write subclass data.
    procedure WriteToFiler(writer: TWriter); override;
    // Override this function to read subclass data.
    procedure ReadFromFiler(reader: TReader); override;
  public
    procedure Render(var rci: TGLRenderContextInfo); virtual;
  end;

  (* An object effect that gets rendered before owner object's render.
     The current OpenGL matrices and material are that of the owner object. *)
  TGLObjectPreEffect = class(TGLEffect)
  end;

  (*An object effect that gets rendered after owner object's render.
     The current OpenGL matrices and material are that of the owner object. *)
  TGLObjectPostEffect = class(TGLEffect)
  end;

  (*An object effect that gets rendered at scene's end.
    No particular OpenGL matrices or material should be assumed. *)
  TGLObjectAfterEffect = class(TGLEffect)
  end;

  (*Holds a list of object effects.
     This object expects itself to be owned by a TGLBaseSceneObject.  *)
  TGLEffects = class(TXCollection)
  protected
    function GetEffect(index: Integer): TGLEffect;
  public
    constructor Create(aOwner: TPersistent); override;
    function GetNamePath: string; override;
    class function ItemsClass: TXCollectionItemClass; override;
    property ObjectEffect[index: Integer]: TGLEffect read GetEffect; default;
    function CanAdd(aClass: TXCollectionItemClass): Boolean; override;
    procedure DoProgress(const progressTime: TGLProgressTimes);
    procedure RenderPreEffects(var rci: TGLRenderContextInfo); inline;
    //Also take care of registering after effects with the GLSceneViewer.
    procedure RenderPostEffects(var rci: TGLRenderContextInfo); inline;
  end;

  (*Extended base scene object class with a material property.
    The material allows defining a color and texture for the object, see TGLMaterial *)
  TGLCustomSceneObject = class(TGLBaseSceneObject)
  private
    FMaterial: TGLMaterial;
    FHint: string;
  protected
    function Blended: Boolean; override;
    procedure SetGLMaterial(AValue: TGLMaterial); inline;
    procedure DestroyHandle; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    property Material: TGLMaterial read FMaterial write SetGLMaterial;
    property Hint: string read FHint write FHint;
  end;

  (* This class shall be used only as a hierarchy root.
     It exists only as a container and shall never be rotated/scaled etc. as
     the class type is used in parenting optimizations.
     Shall never implement or add any functionality, the "Create" override
     only take cares of disabling the build list. *)
  TGLSceneRootObject = class(TGLBaseSceneObject)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  (*Base class for objects that do not have a published "material".
    Note that the material is available in public properties, but isn't
    applied automatically before invoking BuildList.
    Subclassing should be reserved to structural objects and objects that
    have no material of their own. *)
  TGLImmaterialSceneObject = class(TGLCustomSceneObject)
  public
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
  published
    property ObjectsSorting;
    property VisibilityCulling;
    property Direction;
    property PitchAngle;
    property Position;
    property RollAngle;
    property Scale;
    property ShowAxes;
    property TurnAngle;
    property Up;
    property Visible;
    property Pickable;
    property OnProgress;
    property OnPicked;
    property Behaviours;
    property Effects;
    property Hint;
  end;

  (* Base class for camera invariant objects.
     Camera invariant objects bypass camera settings, such as camera
     position (object is always centered on camera) or camera orientation
     (object always has same orientation as camera). *)
  TGLCameraInvariantObject = class(TGLImmaterialSceneObject)
  private
    FCamInvarianceMode: TGLCameraInvarianceMode;
  protected
    procedure SetCamInvarianceMode(const val: TGLCameraInvarianceMode);
    property CamInvarianceMode: TGLCameraInvarianceMode read FCamInvarianceMode
      write SetCamInvarianceMode;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
  end;

  // Base class for standard scene objects. Publishes the Material property.
  TGLSceneObject = class(TGLCustomSceneObject)
  published
    property Material;
    property ObjectsSorting;
    property VisibilityCulling;
    property Direction;
    property PitchAngle;
    property Position;
    property RollAngle;
    property Scale;
    property ShowAxes;
    property TurnAngle;
    property Up;
    property Visible;
    property Pickable;
    property OnProgress;
    property OnPicked;
    property Behaviours;
    property Effects;
    property Hint;
  end;

  // Event for user-specific rendering in a TGLDirectOpenGL object.
  TGLDirectRenderEvent = procedure(Sender: TObject; var rci: TGLRenderContextInfo)
    of object;

  (* Provides a way to issue direct OpenGL calls during the rendering.
     You can use this object to do your specific rendering task in its OnRender
     event. The OpenGL calls shall restore the OpenGL states they found when
     entering, or exclusively use the GLMisc utility functions to alter the states. *)
  TGLDirectOpenGL = class(TGLImmaterialSceneObject)
  private
    FUseBuildList: Boolean;
    FOnRender: TGLDirectRenderEvent;
    FBlend: Boolean;
  protected
    procedure SetUseBuildList(const val: Boolean);
    function Blended: Boolean; override;
    procedure SetBlend(const val: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TGLVector; override;
  published
    (* Specifies if a build list be made.
       If True, GLScene will generate a build list (OpenGL-side cache),
       ie. OnRender will only be invoked once for the first render, or after
       a StructureChanged call. This is suitable for "static" geometry and
       will usually speed up rendering of things that don't change.
       If false, OnRender will be invoked for each render. This is suitable
       for dynamic geometry (things that change often or constantly). *)
    property UseBuildList: Boolean read FUseBuildList write SetUseBuildList;
    (* Place your specific OpenGL code here.
       The OpenGL calls shall restore the OpenGL states they found when
       entering, or exclusively use the GLMisc utility functions to alter
       the states. *)
    property OnRender: TGLDirectRenderEvent read FOnRender write FOnRender;
    (* Defines if the object uses blending.
       This property will allow direct opengl objects to be flagged as
       blended for object sorting purposes. *)
    property Blend: Boolean read FBlend write SetBlend;
  end;

  (* Scene object that allows other objects to issue rendering at some point.
     This object is used to specify a render point for which other components
     have (rendering) tasks to perform. It doesn't render anything itself
     and is invisible, but other components can register and be notified
     when the point is reached in the rendering phase.
     Callbacks must be explicitly unregistered. *)
  TGLRenderPoint = class(TGLImmaterialSceneObject)
  private
    FCallBacks: array of TGLDirectRenderEvent;
    FFreeCallBacks: array of TNotifyEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    procedure RegisterCallBack(renderEvent: TGLDirectRenderEvent;
      renderPointFreed: TNotifyEvent);
    procedure UnRegisterCallBack(renderEvent: TGLDirectRenderEvent);
    procedure Clear;
  end;

  (* A full proxy object.
     This object literally uses another object's Render method to do its own
     rendering, however, it has a coordinate system and a life of its own.
     Use it for duplicates of an object. *)
  TGLProxyObject = class(TGLBaseSceneObject)
  private
    FMasterObject: TGLBaseSceneObject;
    FProxyOptions: TGLProxyObjectOptions;
  protected
    FRendering: Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure SetMasterObject(const val: TGLBaseSceneObject); virtual;
    procedure SetProxyOptions(const val: TGLProxyObjectOptions);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    function BarycenterAbsolutePosition: TGLVector; override;
    function AxisAlignedDimensions: TGLVector; override;
    function AxisAlignedDimensionsUnscaled: TGLVector; override;
    function RayCastIntersect(const rayStart, rayVector: TGLVector;
      intersectPoint: PGLVector = nil;
      intersectNormal: PGLVector = nil): Boolean; override;
    function GenerateSilhouette(const silhouetteParameters:
      TGLSilhouetteParameters): TGLSilhouette; override;
  published
    // Specifies the Master object which will be proxy'ed.
    property MasterObject: TGLBaseSceneObject read FMasterObject write
      SetMasterObject;
    // Specifies how and what is proxy'ed.
    property ProxyOptions: TGLProxyObjectOptions read FProxyOptions write
      SetProxyOptions default cDefaultProxyOptions;
    property ObjectsSorting;
    property Direction;
    property PitchAngle;
    property Position;
    property RollAngle;
    property Scale;
    property ShowAxes;
    property TurnAngle;
    property Up;
    property Visible;
    property Pickable;
    property OnProgress;
    property OnPicked;
    property Behaviours;
  end;

  TGLProxyObjectClass = class of TGLProxyObject;

  (* Defines the various styles for lightsources.
      lsSpot : a spot light, oriented and with a cutoff zone (note that if
        cutoff is 180, the spot is rendered as an omni source)
      lsOmni : an omnidirectionnal source, punctual and sending light in
        all directions uniformously
      lsParallel : a parallel light, oriented as the light source is (this
        type of light can help speed up rendering) *)
  TGLLightStyle = (lsSpot, lsOmni, lsParallel, lsParallelSpot);

  (* Standard light source.
     The standard GLScene light source covers spotlights, omnidirectionnal and
     parallel sources (see TLightStyle).
     Lights are colored, have distance attenuation parameters and are turned
     on/off through their Shining property.
     Lightsources are managed in a specific object by the TGLScene for rendering
     purposes. The maximum number of light source in a scene is limited by the
     OpenGL implementation (8 lights are supported under most ICDs), though the
     more light you use, the slower rendering may get. If you want to render
     many more light/lightsource, you may have to resort to other techniques
     like lightmapping. *)
  TGLLightSource = class(TGLBaseSceneObject)
  private
    FLightID: Cardinal;
    FSpotDirection: TGLCoordinates;
    FSpotExponent, FSpotCutOff: Single;
    FConstAttenuation, FLinearAttenuation, FQuadraticAttenuation: Single;
    FShining: Boolean;
    FAmbient, FDiffuse, FSpecular: TGLColor;
    FLightStyle: TGLLightStyle;
  protected
    procedure SetAmbient(AValue: TGLColor);
    procedure SetDiffuse(AValue: TGLColor);
    procedure SetSpecular(AValue: TGLColor);
    procedure SetConstAttenuation(AValue: Single);
    procedure SetLinearAttenuation(AValue: Single);
    procedure SetQuadraticAttenuation(AValue: Single);
    procedure SetShining(AValue: Boolean);
    procedure SetSpotDirection(AVector: TGLCoordinates);
    procedure SetSpotExponent(AValue: Single);
    procedure SetSpotCutOff(const val: Single);
    procedure SetLightStyle(const val: TGLLightStyle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    // light sources have different handle types than normal scene objects
    function RayCastIntersect(const rayStart, rayVector: TGLVector;
      intersectPoint: PGLVector = nil;
      intersectNormal: PGLVector = nil): Boolean; override;
    procedure CoordinateChanged(Sender: TGLCustomCoordinates); override;
    function GenerateSilhouette(const silhouetteParameters:
      TGLSilhouetteParameters): TGLSilhouette; override;
    property LightID: Cardinal read FLightID;
    function Attenuated: Boolean;
  published
    property Ambient: TGLColor read FAmbient write SetAmbient;
    property ConstAttenuation: Single read FConstAttenuation write
      SetConstAttenuation;
    property Diffuse: TGLColor read FDiffuse write SetDiffuse;
    property LinearAttenuation: Single read FLinearAttenuation write
      SetLinearAttenuation;
    property QuadraticAttenuation: Single read FQuadraticAttenuation write
      SetQuadraticAttenuation;
    property Position;
    property LightStyle: TGLLightStyle read FLightStyle write SetLightStyle default lsSpot;
    property Shining: Boolean read FShining write SetShining default True;
    property Specular: TGLColor read FSpecular write SetSpecular;
    property SpotCutOff: Single read FSpotCutOff write SetSpotCutOff;
    property SpotDirection: TGLCoordinates read FSpotDirection write
      SetSpotDirection;
    property SpotExponent: Single read FSpotExponent write SetSpotExponent;
    property OnProgress;
  end;

  TGLCameraStyle = (csPerspective, csOrthogonal, csOrtho2D, csCustom,
    csInfinitePerspective, csPerspectiveKeepFOV);
  TGLCameraKeepFOVMode = (ckmHorizontalFOV, ckmVerticalFOV);

  TOnCustomPerspective = procedure(const viewport: TRectangle;
    width, height: Integer; DPI: Integer;
    var viewPortRadius: Single) of object;

  (* Camera object.
     This object is commonly referred by TGLSceneViewer and defines a position,
     direction, focal length, depth of view... all the properties needed for
     defining a point of view and optical characteristics. *)
  TGLCamera = class(TGLBaseSceneObject)
  private
    FFocalLength: Single;
    FDepthOfView: Single;
    FNearPlane: Single; // nearest distance to the camera
    FNearPlaneBias: Single; // scaling bias applied to near plane
    FViewPortRadius: Single; // viewport bounding radius per distance unit
    FTargetObject: TGLBaseSceneObject;
    FLastDirection: TGLVector; // Not persistent
    FCameraStyle: TGLCameraStyle;
    FKeepFOVMode: TGLCameraKeepFOVMode;
    FSceneScale: Single;
    FDeferredApply: TNotifyEvent;
    FOnCustomPerspective: TOnCustomPerspective;
    FDesign: Boolean;
    FFOVY, FFOVX: Double;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetTargetObject(const val: TGLBaseSceneObject);
    procedure SetDepthOfView(AValue: Single);
    procedure SetFocalLength(AValue: Single);
    procedure SetCameraStyle(const val: TGLCameraStyle);
    procedure SetKeepFOVMode(const val: TGLCameraKeepFOVMode);
    procedure SetSceneScale(value: Single);
    function StoreSceneScale: Boolean;
    procedure SetNearPlaneBias(value: Single);
    function StoreNearPlaneBias: Boolean;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    (* Nearest clipping plane for the frustum.
     This value depends on the FocalLength and DepthOfView fields and
     is calculated to minimize Z-Buffer crawling as suggested by the OpenGL documentation. *)
    property NearPlane: Single read FNearPlane;
    // Apply camera transformation
    procedure Apply;
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    function RayCastIntersect(const rayStart, rayVector: TGLVector;
      intersectPoint: PGLVector = nil;
      intersectNormal: PGLVector = nil): Boolean; override;
    procedure ApplyPerspective(const AViewport: TRectangle;
      AWidth, AHeight: Integer; ADPI: Integer);
    procedure AutoLeveling(Factor: Single);
    procedure Reset(aSceneBuffer: TGLSceneBuffer);
    // Position the camera so that the whole scene can be seen
    procedure ZoomAll(aSceneBuffer: TGLSceneBuffer);
    procedure RotateObject(obj: TGLBaseSceneObject; pitchDelta, turnDelta: Single; rollDelta: Single = 0);
    procedure RotateTarget(pitchDelta, turnDelta: Single; rollDelta: Single = 0);
    (* Change camera's position to make it move around its target.
       If TargetObject is nil, nothing happens. This method helps in quickly
       implementing camera controls. Camera's Up and Direction properties are unchanged.
       Angle deltas are in degrees, camera parent's coordinates should be identity.
       Tip : make the camera a child of a "target" dummycube and make
       it a target the dummycube. Now, to pan across the scene, just move
       the dummycube, to change viewing angle, use this method. *)
    procedure MoveAroundTarget(pitchDelta, turnDelta: Single);
    (* Change camera's position to make it move all around its target.
       If TargetObject is nil, nothing happens. This method helps in quickly
       implementing camera controls. Camera's Up and Direction properties are changed.
       Angle deltas are in degrees. *)
    procedure MoveAllAroundTarget(pitchDelta, turnDelta :Single);
    // Moves the camera in eye space coordinates.
    procedure MoveInEyeSpace(forwardDistance, rightDistance, upDistance: Single);
    // Moves the target in eye space coordinates.
    procedure MoveTargetInEyeSpace(forwardDistance, rightDistance, upDistance: Single);
    // Computes the absolute vector corresponding to the eye-space translations.
    function AbsoluteEyeSpaceVector(forwardDistance, rightDistance, upDistance:
      Single): TGLVector;
    (* Adjusts distance from camera to target by applying a ratio.
       If TargetObject is nil, nothing happens. This method helps in quickly
       implementing camera controls. Only the camera's position is changed. *)
    procedure AdjustDistanceToTarget(distanceRatio: Single);
    (* Returns the distance from camera to target.
       If TargetObject is nil, returns 1. *)
    function DistanceToTarget: Single;
    (* Computes the absolute normalized vector to the camera target.
       If no target is defined, AbsoluteDirection is returned. *)
    function AbsoluteVectorToTarget: TGLVector;
    (* Computes the absolute normalized right vector to the camera target.
       If no target is defined, AbsoluteRight is returned. *)
    function AbsoluteRightVectorToTarget: TGLVector;
    (* Computes the absolute normalized up vector to the camera target.
       If no target is defined, AbsoluteUpt is returned. *)
    function AbsoluteUpVectorToTarget: TGLVector;
    (* Calculate an absolute translation vector from a screen vector.
       Ratio is applied to both screen delta, planeNormal should be the
       translation plane's normal. *)
    function ScreenDeltaToVector(deltaX, deltaY: Integer; ratio: Single;
      const planeNormal: TGLVector): TGLVector;
    // Same as ScreenDeltaToVector but optimized for XY plane.
    function ScreenDeltaToVectorXY(deltaX, deltaY: Integer; ratio: Single): TGLVector;
    // Same as ScreenDeltaToVector but optimized for XZ plane.
    function ScreenDeltaToVectorXZ(deltaX, deltaY: Integer; ratio: Single): TGLVector;
    // Same as ScreenDeltaToVector but optimized for YZ plane.
    function ScreenDeltaToVectorYZ(deltaX, deltaY: Integer; ratio: Single): TGLVector;
    // Returns true if a point is in front of the camera.
    function PointInFront(const point: TGLVector): boolean; overload;
    (* Calculates the field of view in degrees, given a viewport dimension
    (width or height). F.i. you may wish to use the minimum of the two. *)
    function GetFieldOfView(const AViewportDimension: single): single;
    (* Sets the FocalLength in degrees, given a field of view and a viewport
       dimension (width or height). *)
    procedure SetFieldOfView(const AFieldOfView, AViewportDimension: single);
  published
    (* Depth of field/view.
       Adjusts the maximum distance, beyond which objects will be clipped
       (ie. not visisble).
       You must adjust this value if you are experiencing disappearing
       objects (increase the value) of Z-Buffer crawling (decrease the
       value). Z-Buffer crawling happens when depth of view is too large
       and the Z-Buffer precision cannot account for all that depth
       accurately : objects farther overlap closer objects and vice-versa.
       Note that this value is ignored in cSOrtho2D mode. *)
    property DepthOfView: Single read FDepthOfView write SetDepthOfView;
    (* Focal Length of the camera.
       Adjusting this value allows for lens zooming effects (use SceneScale
       for linear zooming). This property affects near/far planes clipping. *)
    property FocalLength: Single read FFocalLength write SetFocalLength;
    {Scene scaling for camera point.
       This is a linear 2D scaling of the camera's output, allows for
       linear zooming (use FocalLength for lens zooming). }
    property SceneScale: Single read FSceneScale write SetSceneScale stored StoreSceneScale;
    (* Scaling bias applied to near-plane calculation.
       Values inferior to one will move the nearplane nearer, and also
       reduce medium/long range Z-Buffer precision, values superior
       to one will move the nearplane farther, and also improve medium/long
       range Z-Buffer precision. *)
    property NearPlaneBias: Single read FNearPlaneBias write SetNearPlaneBias stored StoreNearPlaneBias;
    (* If set, camera will point to this object.
       When camera is pointing an object, the Direction vector is ignored
       and the Up vector is used as an absolute vector to the up. *)
    property TargetObject: TGLBaseSceneObject read FTargetObject write SetTargetObject;
    (* Adjust the camera style.
       Three styles are available :
       csPerspective, the default value for perspective projection
       csOrthogonal, for orthogonal (or isometric) projection.
       csOrtho2D, setups orthogonal 2D projection in which 1 unit
         (in x or y) represents 1 pixel.
       csInfinitePerspective, for perspective view without depth limit.
       csKeepCamAnglePerspective, for perspective view with keeping aspect on view resize.
       csCustom, setup is deferred to the OnCustomPerspective event. *)
    property CameraStyle: TGLCameraStyle read FCameraStyle write SetCameraStyle default csPerspective;
    (* Keep camera angle mode.
       When CameraStyle is csKeepCamAnglePerspective, select which camera angle you want to keep.
        kaHeight, for Keep Height oriented camera angle
        kaWidth,  for Keep Width oriented camera angle *)
    property KeepFOVMode: TGLCameraKeepFOVMode read FKeepFOVMode write SetKeepFOVMode default ckmHorizontalFOV;
    (* Custom perspective event.
       This event allows you to specify your custom perpective, either
       with a glFrustrum, a glOrtho or whatever method suits you.
       You must compute viewPortRadius for culling to work.
       This event is only called if CameraStyle is csCustom. *)
    property OnCustomPerspective: TOnCustomPerspective read FOnCustomPerspective write FOnCustomPerspective;
    property Position;
    property Direction;
    property Up;
    property OnProgress;
  end;

  (* Scene component class.
     The scene contains the scene description (lights, geometry...), which is
     basicly a hierarchical scene graph made of TGLBaseSceneObject. It will
     usually contain one or more TGLCamera object, which can be referred by
     a Viewer component for rendering purposes.
     The scene's objects can be accessed directly from code (as regular
     components), but those are edited with a specific editor (double-click
     on the TGLScene component at design-time to invoke it). To add objects
     at runtime, use the AddNewChild method of TGLBaseSceneObject. *)
  TGLScene = class(TGLUpdateAbleComponent)
  private
    FUpdateCount: Integer;
    FObjects: TGLSceneRootObject;
    FBaseContext: TGLContext; //reference, not owned!
    FLights, FBuffers: TGLPersistentObjectList;
    FCurrentGLCamera: TGLCamera;
    FCurrentBuffer: TGLSceneBuffer;
    FObjectsSorting: TGLObjectsSorting;
    FVisibilityCulling: TGLVisibilityCulling;
    FOnBeforeProgress: TGLProgressEvent;
    FOnProgress: TGLProgressEvent;
    FCurrentDeltaTime: Double;
    FInitializableObjects: TGLInitializableObjectList;
  protected
    procedure AddLight(aLight: TGLLightSource);
    procedure RemoveLight(aLight: TGLLightSource);
    // Adds all lights in the subtree (anObj included)
    procedure AddLights(anObj: TGLBaseSceneObject);
    // Removes all lights in the subtree (anObj included)
    procedure RemoveLights(anObj: TGLBaseSceneObject);
    procedure GetChildren(AProc: TGetChildProc; Root: TComponent); override;
    procedure SetChildOrder(AChild: TComponent; Order: Integer); override;
    procedure SetObjectsSorting(const val: TGLObjectsSorting);
    procedure SetVisibilityCulling(const val: TGLVisibilityCulling);
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating: Boolean;
    procedure AddBuffer(aBuffer: TGLSceneBuffer);
    procedure RemoveBuffer(aBuffer: TGLSceneBuffer);
    procedure SetupLights(maxLights: Integer);
    procedure NotifyChange(Sender: TObject); override;
    procedure Progress(const deltaTime, newTime: Double);
    function FindSceneObject(const AName: string): TGLBaseSceneObject;
    (* Calculates, finds and returns the first object intercepted by the ray.
       Returns nil if no intersection was found. This function will be
       accurate only for objects that overrided their RayCastIntersect
       method with accurate code, otherwise, bounding sphere intersections
       will be returned. *)
    function RayCastIntersect(const rayStart, rayVector: TGLVector; intersectPoint: PGLVector = nil;
      intersectNormal: PGLVector = nil): TGLBaseSceneObject;
    procedure ShutdownAllLights;
    // Saves the scene to a file (recommended extension : .GLSM)
    procedure SaveToFile(const fileName: string);
    (* Load the scene from a file.
       Existing objects/lights/cameras are freed, then the file is loaded.
       Delphi's IDE is not handling this behaviour properly yet, ie. if
       you load a scene in the IDE, objects will be properly loaded, but
       no declare will be placed in the code. *)
    procedure LoadFromFile(const fileName: string);
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromStream(aStream: TStream);
    // Saves the scene to a text file
    procedure SaveToTextFile(const fileName: string);
    // Load the scene from a text files. See LoadFromFile for details.
    procedure LoadFromTextFile(const fileName: string);
    property CurrentGLCamera: TGLCamera read FCurrentGLCamera;
    property Lights: TGLPersistentObjectList read FLights;
    property Objects: TGLSceneRootObject read FObjects;
    property CurrentBuffer: TGLSceneBuffer read FCurrentBuffer;
    (* List of objects that request to be initialized when rendering context is active.
      They are removed automaticly from this list once initialized. *)
    property InitializableObjects: TGLInitializableObjectList read
      FInitializableObjects;
    property CurrentDeltaTime: Double read FCurrentDeltaTime;
  published
    // Defines default ObjectSorting option for scene objects.
    property ObjectsSorting: TGLObjectsSorting read FObjectsSorting write
      SetObjectsSorting default osRenderBlendedLast;
    // Defines default VisibilityCulling option for scene objects.
    property VisibilityCulling: TGLVisibilityCulling read FVisibilityCulling
      write SetVisibilityCulling default vcNone;
    property OnBeforeProgress: TGLProgressEvent read FOnBeforeProgress write FOnBeforeProgress;
    property OnProgress: TGLProgressEvent read FOnProgress write FOnProgress;
  end;

  TFogMode = (fmLinear, fmExp, fmExp2);

  (*Fog distance calculation mode. fdDefault: let OpenGL use its default formula
    fdEyeRadial: uses radial "true" distance (best quality)
    fdEyePlane: uses the distance to the projection plane (same as Z-Buffer, faster)
    Requires support of GL_NV_fog_distance extension, otherwise, it is ignored. *)
  TFogDistance = (fdDefault, fdEyeRadial, fdEyePlane);

  (* Parameters for fog environment in a scene.
     The fog descibed by this object is a distance-based fog, ie. the "intensity"
     of the fog is given by a formula depending solely on the distance, this
     intensity is used for blending to a fixed color. *)
  TGLFogEnvironment = class(TGLUpdateAbleObject)
  private
    FSceneBuffer: TGLSceneBuffer;
    FFogColor: TGLColor; // alpha value means the fog density
    FFogStart, FFogEnd: Single;
    FFogMode: TFogMode;
    FFogDistance: TFogDistance;
  protected
    procedure SetFogColor(Value: TGLColor);
    procedure SetFogStart(Value: Single);
    procedure SetFogEnd(Value: Single);
    procedure SetFogMode(Value: TFogMode);
    procedure SetFogDistance(const val: TFogDistance);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure ApplyFog;
    procedure Assign(Source: TPersistent); override;
    function IsAtDefaultValues: Boolean;
  published
    // Color of the fog when it is at 100% intensity.
    property FogColor: TGLColor read FFogColor write SetFogColor;
    // Minimum distance for fog, what is closer is not affected.
    property FogStart: Single read FFogStart write SetFogStart;
    // Maximum distance for fog, what is farther is at 100% fog intensity.
    property FogEnd: Single read FFogEnd write SetFogEnd;
    // The formula used for converting distance to fog intensity.
    property FogMode: TFogMode read FFogMode write SetFogMode default fmLinear;
    (* Adjusts the formula used for calculating fog distances.
      This option is honoured if and only if the OpenGL ICD supports the
      GL_NV_fog_distance extension, otherwise, it is ignored.
      fdDefault: let OpenGL use its default formula
      fdEyeRadial: uses radial "true" distance (best quality)
      fdEyePlane: uses the distance to the projection plane (same as Z-Buffer, faster)*)
    property FogDistance: TFogDistance read FFogDistance write SetFogDistance
      default fdDefault;
  end;

  TGLDepthPrecision = (dpDefault, dp16bits, dp24bits, dp32bits);
  TGLColorDepth = (cdDefault, cd8bits, cd16bits, cd24bits, cdFloat64bits, cdFloat128bits);
  TGLShadeModel = (smDefault, smSmooth, smFlat);

  // Encapsulates a frame/rendering buffer.
  TGLSceneBuffer = class(TGLUpdateAbleObject)
  private
    // Internal state
    FRendering: Boolean;
    FRenderingContext: TGLContext;
    FAfterRenderEffects: TGLPersistentObjectList;
    FViewMatrixStack: array of TGLMatrix;
    FProjectionMatrixStack: array of TGLMatrix;
    FBaseProjectionMatrix: TGLMatrix;
    FCameraAbsolutePosition: TGLVector;
    FViewPort: TRectangle;
    FSelector: TGLBaseSelectTechnique;
    // Options & User Properties
    FFaceCulling, FFogEnable, FLighting: Boolean;
    FDepthTest: Boolean;
    FBackgroundColor: TColor;
    FBackgroundAlpha: Single;
    FAmbientColor: TGLColor;
    FAntiAliasing: TGLAntiAliasing;
    FDepthPrecision: TGLDepthPrecision;
    FColorDepth: TGLColorDepth;
    FContextOptions: TGLContextOptions;
    FShadeModel: TGLShadeModel;
    FRenderDPI: Integer;
    FFogEnvironment: TGLFogEnvironment;
    FAccumBufferBits: Integer;
    FLayer: TGLContextLayer;
    // Cameras
    FCamera: TGLCamera;
    // Freezing
    FFreezeBuffer: Pointer;
    FFreezed: Boolean;
    FFreezedViewPort: TRectangle;
    // Monitoring
    FFrameCount: Longint;
    FFramesPerSecond: Single;
    FFirstPerfCounter: Int64;
    FLastFrameTime: Single;
    // Events
    FOnChange: TNotifyEvent;
    FOnStructuralChange: TNotifyEvent;
    FOnPrepareGLContext: TNotifyEvent;
    FBeforeRender: TNotifyEvent;
    FViewerBeforeRender: TNotifyEvent;
    FPostRender: TNotifyEvent;
    FAfterRender: TNotifyEvent;
    FInitiateRendering: TGLDirectRenderEvent;
    FWrapUpRendering: TGLDirectRenderEvent;
    procedure SetLayer(const Value: TGLContextLayer);
  protected
    procedure SetBackgroundColor(AColor: TColor);
    procedure SetBackgroundAlpha(alpha: Single);
    procedure SetAmbientColor(AColor: TGLColor);
    function GetLimit(Which: TGLLimitType): Integer;
    procedure SetCamera(ACamera: TGLCamera);
    procedure SetContextOptions(Options: TGLContextOptions);
    procedure SetDepthTest(AValue: Boolean);
    procedure SetFaceCulling(AValue: Boolean);
    procedure SetLighting(AValue: Boolean);
    procedure SetAntiAliasing(const val: TGLAntiAliasing);
    procedure SetDepthPrecision(const val: TGLDepthPrecision);
    procedure SetColorDepth(const val: TGLColorDepth);
    procedure SetShadeModel(const val: TGLShadeModel);
    procedure SetFogEnable(AValue: Boolean);
    procedure SetGLFogEnvironment(AValue: TGLFogEnvironment);
    function StoreFog: Boolean;
    procedure SetAccumBufferBits(const val: Integer);
    procedure PrepareRenderingMatrices(const aViewPort: TRectangle;
      resolution: Integer; pickingRect: PRect = nil); inline;
    procedure DoBaseRender(const aViewPort: TRectangle; resolution: Integer;
      drawState: TGLDrawState; baseObject: TGLBaseSceneObject);
    procedure SetupRenderingContext(context: TGLContext);
    procedure SetupRCOptions(context: TGLContext);
    procedure PrepareGLContext;
    procedure DoChange;
    procedure DoStructuralChange;
    // DPI for current/last render
    property RenderDPI: Integer read FRenderDPI;
    property OnPrepareGLContext: TNotifyEvent read FOnPrepareGLContext write
      FOnPrepareGLContext;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure NotifyChange(Sender: TObject); override;
    procedure CreateRC(AWindowHandle: HWND; memoryContext: Boolean;
      BufferCount: integer = 1); overload;
    procedure ClearBuffers; inline;
    procedure DestroyRC;
    function RCInstantiated: Boolean;
    procedure Resize(newLeft, newTop, newWidth, newHeight: Integer);
    // Indicates hardware acceleration support
    function Acceleration: TGLContextAcceleration; inline;
    // ViewPort for current/last render
    property ViewPort: TRectangle read FViewPort;
    // Fills the PickList with objects in Rect area
    procedure PickObjects(const rect: TRect; pickList: TGLPickList;
      objectCountGuess: Integer);
    (* Returns a PickList with objects in Rect area.
       Returned list should be freed by caller.
       Objects are sorted by depth (nearest objects first). *)
    function GetPickedObjects(const rect: TRect; objectCountGuess: Integer =
      64): TGLPickList;
    // Returns the nearest object at x, y coordinates or nil if there is none
    function GetPickedObject(x, y: Integer): TGLBaseSceneObject;
    // Returns the color of the pixel at x, y in the frame buffer
    function GetPixelColor(x, y: Integer): TColor;
    (* Returns the raw depth (Z buffer) of the pixel at x, y in the frame buffer.
       This value does not map to the actual eye-object distance, but to
       a depth buffer value in the [0; 1] range. *)
    function GetPixelDepth(x, y: Integer): Single;
    (* Converts a raw depth (Z buffer value) to frustrum distance.
       This calculation is only accurate for the pixel at the centre of the viewer,
       because it does not take into account that the corners of the frustrum
       are further from the eye than its centre. *)
    function PixelDepthToDistance(aDepth: Single): Single;
    (* Converts a raw depth (Z buffer value) to world distance.
       It also compensates for the fact that the corners of the frustrum
       are further from the eye, than its centre.*)
    function PixelToDistance(x, y: integer): Single;
    // Design time notification
    procedure NotifyMouseMove(Shift: TShiftState; X, Y: Integer);
    (* Renders the scene on the viewer.
       You do not need to call this method, unless you explicitly want a
       render at a specific time. If you just want the control to get
       refreshed, use Invalidate instead. *)
    procedure Render(baseObject: TGLBaseSceneObject); overload;
    procedure Render; overload; inline;
    procedure RenderScene(aScene: TGLScene;
      const viewPortSizeX, viewPortSizeY: Integer;
      drawState: TGLDrawState; baseObject: TGLBaseSceneObject);
    (*Render the scene to a bitmap at given DPI.
      DPI = "dots per inch".
      The "magic" DPI of the screen is 96 under Windows. *)
    procedure RenderToBitmap(ABitmap: TBitmap; DPI: Integer = 0);
    (* Render the scene to a bitmap at given DPI and saves it to a file.
       DPI = "dots per inch".
       The "magic" DPI of the screen is 96 under Windows. *)
    procedure RenderToFile(const AFile: string; DPI: Integer = 0); overload;
    (* Renders to bitmap of given size, then saves it to a file.
       DPI is adjusted to make the bitmap similar to the viewer. *)
    procedure RenderToFile(const AFile: string; bmpWidth, bmpHeight: Integer);
      overload;
    (* Creates a TGLBitmap32 that is a snapshot of current OpenGL content.
       When possible, use this function instead of RenderToBitmap, it won't
       request a redraw and will be significantly faster.
       The returned TGLBitmap32 should be freed by calling code. *)
    function CreateSnapShot: TGLImage;
    // Creates a bitmap that is a snapshot of current OpenGL content.
    function CreateSnapShotBitmap: TBitmap;
    procedure CopyToTexture(aTexture: TGLTexture); overload;
    procedure CopyToTexture(aTexture: TGLTexture; xSrc, ySrc, AWidth, AHeight: Integer;
      xDest, yDest: Integer; glCubeFace: Cardinal = 0); overload;
    // Save as raw float data to a file
    procedure SaveAsFloatToFile(const aFilename: string);
    // Event reserved for viewer-specific uses.
    property ViewerBeforeRender: TNotifyEvent read FViewerBeforeRender write
      FViewerBeforeRender stored False;
    procedure SetViewPort(X, Y, W, H: Integer);
    function Width: Integer;
    function Height: Integer;
    // Indicates if the Viewer is "frozen".
    property Freezed: Boolean read FFreezed;
    (* Freezes rendering leaving the last rendered scene on the buffer. This
       is usefull in windowed applications for temporarily stoping rendering
       (when moving the window, for example). *)
    procedure Freeze;
    // Restarts rendering after it was freezed.
    procedure Melt;
    // Displays a window with info on current OpenGL ICD and context.
    procedure ShowInfo(Modal: boolean = false);
    // Currently Rendering?
    property Rendering: Boolean read FRendering;
    // Adjusts background alpha channel.
    property BackgroundAlpha: Single read FBackgroundAlpha write SetBackgroundAlpha;
    // Returns the projection matrix in use or used for the last rendering.
    function ProjectionMatrix: TGLMatrix; deprecated;
    // Returns the view matrix in use or used for the last rendering.
    function ViewMatrix: TGLMatrix; deprecated;
    function ModelMatrix: TGLMatrix; deprecated;
    (* Returns the base projection matrix in use or used for the last rendering.
       The "base" projection is (as of now) either identity or the pick
       matrix, ie. it is the matrix on which the perspective or orthogonal
       matrix gets applied. *)
    property BaseProjectionMatrix: TGLMatrix read FBaseProjectionMatrix;
    (* Back up current View matrix and replace it with newMatrix.
       This method has no effect on the OpenGL matrix, only on the Buffer's
       matrix, and is intended for special effects rendering. *)
    procedure PushViewMatrix(const newMatrix: TGLMatrix); deprecated;
    // Restore a View matrix previously pushed.
    procedure PopViewMatrix; deprecated;
    procedure PushProjectionMatrix(const newMatrix: TGLMatrix); deprecated;
    procedure PopProjectionMatrix;  deprecated;
    (* Converts a screen pixel coordinate into 3D coordinates for orthogonal projection.
       This function accepts standard canvas coordinates, with (0,0) being
       the top left corner, and returns, when the camera is in orthogonal
       mode, the corresponding 3D world point that is in the camera's plane. *)
    function OrthoScreenToWorld(screenX, screenY: Integer): TAffineVector; overload;
    (* Converts a screen coordinate into world (3D) coordinates.
       This methods wraps a call to gluUnProject.
       Note that screen coord (0,0) is the lower left corner. *)
    function ScreenToWorld(const aPoint: TAffineVector): TAffineVector; overload;
    function ScreenToWorld(const aPoint: TGLVector): TGLVector; overload;
    {Converts a screen pixel coordinate into 3D world coordinates.
       This function accepts standard canvas coordinates, with (0,0) being
       the top left corner. }
    function ScreenToWorld(screenX, screenY: Integer): TAffineVector; overload;
    (* Converts an absolute world coordinate into screen coordinate.
       This methods wraps a call to gluProject.
       Note that screen coord (0,0) is the lower left corner. *)
    function WorldToScreen(const aPoint: TAffineVector): TAffineVector; overload;
    function WorldToScreen(const aPoint: TGLVector): TGLVector; overload;
    // Converts a set of point absolute world coordinates into screen coordinates.
    procedure WorldToScreen(points: PGLVector; nbPoints: Integer); overload;
    (* Calculates the 3D vector corresponding to a 2D screen coordinate.
       The vector originates from the camera's absolute position and is
       expressed in absolute coordinates.
       Note that screen coord (0,0) is the lower left corner. *)
    function ScreenToVector(const aPoint: TAffineVector): TAffineVector; overload;
    function ScreenToVector(const aPoint: TGLVector): TGLVector; overload;
    function ScreenToVector(const x, y: Integer): TGLVector; overload;
    (* Calculates the 2D screen coordinate of a vector from the camera's
       absolute position and is expressed in absolute coordinates.
       Note that screen coord (0,0) is the lower left corner. *)
    function VectorToScreen(const VectToCam: TAffineVector): TAffineVector;
    (* Calculates intersection between a plane and screen vector.
       If an intersection is found, returns True and places result in
       intersectPoint. *)
    function ScreenVectorIntersectWithPlane(
      const aScreenPoint: TGLVector;
      const planePoint, planeNormal: TGLVector;
      var intersectPoint: TGLVector): Boolean;
    (* Calculates intersection between plane XY and screen vector.
       If an intersection is found, returns True and places result in
       intersectPoint. *)
    function ScreenVectorIntersectWithPlaneXY(
      const aScreenPoint: TGLVector; const z: Single;
      var intersectPoint: TGLVector): Boolean;
    (* Calculates intersection between plane YZ and screen vector.
       If an intersection is found, returns True and places result in
       intersectPoint. *)
    function ScreenVectorIntersectWithPlaneYZ(
      const aScreenPoint: TGLVector; const x: Single;
      var intersectPoint: TGLVector): Boolean;
    (* Calculates intersection between plane XZ and screen vector.
       If an intersection is found, returns True and places result in
       intersectPoint. *)
    function ScreenVectorIntersectWithPlaneXZ(
      const aScreenPoint: TGLVector; const y: Single;
      var intersectPoint: TGLVector): Boolean;
    (* Calculates a 3D coordinate from screen position and ZBuffer.
       This function returns a world absolute coordinate from a 2D point
       in the viewer, the depth being extracted from the ZBuffer data
       (DepthTesting and ZBuffer must be enabled for this function to work).
       Note that ZBuffer precision is not linear and can be quite low on
       some boards (either from compression or resolution approximations). *)
    function PixelRayToWorld(x, y: Integer): TAffineVector;
    (* Time (in second) spent to issue rendering order for the last frame.
       Be aware that since execution by the hardware isn't synchronous,
       this value may not be an accurate measurement of the time it took
       to render the last frame, it's a measurement of only the time it
       took to issue rendering orders. *)
    property LastFrameTime: Single read FLastFrameTime;
    (* Current FramesPerSecond rendering speed.
       You must keep the renderer busy to get accurate figures from this
       property.
       This is an average value, to reset the counter, call
       ResetPerfomanceMonitor. *)
    property FramesPerSecond: Single read FFramesPerSecond;
    (* Resets the perfomance monitor and begin a new statistics set.
       See FramesPerSecond. *)
    procedure ResetPerformanceMonitor;
    (* Retrieve one of the OpenGL limits for the current viewer.
       Limits include max texture size, OpenGL stack depth, etc. *)
    property LimitOf[Which: TGLLimitType]: Integer read GetLimit;
    (* Current rendering context.
       The context is a wrapper around platform-specific contexts
       (see TGLContext) and takes care of context activation and handle
       management. *)
    property RenderingContext: TGLContext read FRenderingContext;
    (* The camera from which the scene is rendered.
       A camera is an object you can add and define in a TGLScene component. *)
    property Camera: TGLCamera read FCamera write SetCamera;
    // Specifies the layer plane that the rendering context is bound to.
    property Layer: TGLContextLayer read FLayer write SetLayer
      default clMainPlane;
  published
    // Fog environment options. See TGLFogEnvironment.
    property FogEnvironment: TGLFogEnvironment read FFogEnvironment write
      SetGLFogEnvironment stored StoreFog;
    // Color used for filling the background prior to any rendering.
    property BackgroundColor: TColor read FBackgroundColor write
      SetBackgroundColor default clBtnFace;
    (* Scene ambient color vector.
       This ambient color is defined independantly from all lightsources,
       which can have their own ambient components. *)
    property AmbientColor: TGLColor read FAmbientColor write SetAmbientColor;
    (* Context options allows to setup specifics of the rendering context.
       Not all contexts support all options. *)
    property ContextOptions: TGLContextOptions read FContextOptions write
      SetContextOptions default [roDoubleBuffer, roRenderToWindow, roDebugContext];
    // Number of precision bits for the accumulation buffer.
    property AccumBufferBits: Integer read FAccumBufferBits write
      SetAccumBufferBits default 0;
    (* DepthTest enabling.
       When DepthTest is enabled, objects closer to the camera will hide
       farther ones (via use of Z-Buffering).
       When DepthTest is disabled, the latest objects drawn/rendered overlap
       all previous objects, whatever their distance to the camera.
       Even when DepthTest is enabled, objects may chose to ignore depth
       testing through the osIgnoreDepthBuffer of their ObjectStyle property. *)
    property DepthTest: Boolean read FDepthTest write SetDepthTest default True;
    (* Enable or disable face culling in the renderer.
       Face culling is used in hidden faces removal algorithms : each face
       is given a normal or 'outside' direction. When face culling is enabled,
       only faces whose normal points towards the observer are rendered. *)
    property FaceCulling: Boolean read FFaceCulling write SetFaceCulling default True;
    // Toggle to enable or disable the fog settings.
    property FogEnable: Boolean read FFogEnable write SetFogEnable default
      False;
    (* Toggle to enable or disable lighting calculations.
       When lighting is enabled, objects will be lit according to lightsources,
       when lighting is disabled, objects are rendered in their own colors,
       without any shading.
       Lighting does NOT generate shadows. *)
    property Lighting: Boolean read FLighting write SetLighting default True;
    (* AntiAliasing option.
       Ignored if not hardware supported, currently based on ARB_multisample. *)
    property AntiAliasing: TGLAntiAliasing read FAntiAliasing write
      SetAntiAliasing default aaDefault;
    (* Depth buffer precision.
       Default is highest available (below and including 24 bits) *)
    property DepthPrecision: TGLDepthPrecision read FDepthPrecision write
      SetDepthPrecision default dpDefault;
    (* Color buffer depth.
       Default depth buffer is highest available (below and including 24 bits) *)
    property ColorDepth: TGLColorDepth read FColorDepth write SetColorDepth
      default cdDefault;
    // Shade model. Default is "Smooth".
    property ShadeModel: TGLShadeModel read FShadeModel write SetShadeModel
      default smDefault;
    (* Indicates a change in the scene or buffer options.
       A simple re-render is enough to take into account the changes. *)
    property OnChange: TNotifyEvent read FOnChange write FOnChange stored False;
    (* Indicates a structural change in the scene or buffer options.
       A reconstruction of the RC is necessary to take into account the
       changes (this may lead to a driver switch or lengthy operations). *)
    property OnStructuralChange: TNotifyEvent read FOnStructuralChange write
      FOnStructuralChange stored False;
    (* Triggered before the scene's objects get rendered.
       You may use this event to execute your own OpenGL rendering
       (usually background stuff). *)
    property BeforeRender: TNotifyEvent read FBeforeRender write FBeforeRender
      stored False;
    (* Triggered after BeforeRender, before rendering objects.
       This one is fired after the rci has been initialized and can be used
       to alter it or perform early renderings that require an rci,
       the Sender is the buffer. *)
    property InitiateRendering: TGLDirectRenderEvent read FInitiateRendering write
      FInitiateRendering stored False;
    (* Triggered after rendering all scene objects, before PostRender.
       This is the last point after which the rci becomes unavailable,
       the Sender is the buffer. *)
    property WrapUpRendering: TGLDirectRenderEvent read FWrapUpRendering write
      FWrapUpRendering stored False;
    (* Triggered just after all the scene's objects have been rendered.
       The OpenGL context is still active in this event, and you may use it
       to execute your own OpenGL rendering (usually for HUD, 2D overlays
       or after effects).  *)
    property PostRender: TNotifyEvent read FPostRender write FPostRender stored
      False;
    (* Called after rendering.
       You cannot issue OpenGL calls in this event, if you want to do your own
       OpenGL stuff, use the PostRender event. *)
    property AfterRender: TNotifyEvent read FAfterRender write FAfterRender
      stored False;
  end;

  (* Base class for non-visual viewer.
    Non-visual viewer may actually render visuals, but they are non-visual
    (ie. non interactive) at design time. Such viewers include memory or full-screen viewers. *)
  TGLNonVisualViewer = class(TComponent)
  private
    FBuffer: TGLSceneBuffer;
    FWidth, FHeight: Integer;
    FCubeMapRotIdx: Integer;
    FCubeMapZNear, FCubeMapZFar: Single;
    FCubeMapTranslation: TAffineVector;
    //FCreateTexture : Boolean;
  protected
    procedure SetBeforeRender(const val: TNotifyEvent);
    function GetBeforeRender: TNotifyEvent;
    procedure SetPostRender(const val: TNotifyEvent);
    function GetPostRender: TNotifyEvent;
    procedure SetAfterRender(const val: TNotifyEvent);
    function GetAfterRender: TNotifyEvent;
    procedure SetCamera(const val: TGLCamera);
    function GetCamera: TGLCamera;
    procedure SetBuffer(const val: TGLSceneBuffer);
    procedure SetWidth(const val: Integer);
    procedure SetHeight(const val: Integer);
    procedure SetupCubeMapCamera(Sender: TObject);
    procedure DoOnPrepareGLContext(Sender: TObject);
    procedure PrepareGLContext; virtual;
    procedure DoBufferChange(Sender: TObject); virtual;
    procedure DoBufferStructuralChange(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Render(baseObject: TGLBaseSceneObject = nil); virtual; abstract;
    procedure CopyToTexture(aTexture: TGLTexture); overload; virtual;
    procedure CopyToTexture(aTexture: TGLTexture; xSrc, ySrc, width, height:
      Integer;
      xDest, yDest: Integer); overload;
    // CopyToTexture for Multiple-Render-Target 
    procedure CopyToTextureMRT(aTexture: TGLTexture; BufferIndex: integer);
      overload; virtual;
    procedure CopyToTextureMRT(aTexture: TGLTexture; xSrc, ySrc, width, height:
      Integer;
      xDest, yDest: Integer; BufferIndex: integer); overload;
    (* Renders the 6 texture maps from a scene.
       The viewer is used to render the 6 images, one for each face
       of the cube, from the absolute position of the camera.
       This does NOT alter the content of the Pictures in the image,
       and will only change or define the content of textures as registered by OpenGL. *)
    procedure RenderCubeMapTextures(cubeMapTexture: TGLTexture;
      zNear: Single = 0;
      zFar: Single = 0);
  published
    // Camera from which the scene is rendered.
    property Camera: TGLCamera read GetCamera write SetCamera;
    property Width: Integer read FWidth write SetWidth default 256;
    property Height: Integer read FHeight write SetHeight default 256;
    (* Triggered before the scene's objects get rendered.
       You may use this event to execute your own OpenGL rendering. *)
    property BeforeRender: TNotifyEvent read GetBeforeRender write SetBeforeRender;
    (* Triggered just after all the scene's objects have been rendered.
       The OpenGL context is still active in this event, and you may use it
       to execute your own OpenGL rendering.  *)
    property PostRender: TNotifyEvent read GetPostRender write SetPostRender;
    (* Called after rendering.
       You cannot issue OpenGL calls in this event, if you want to do your own
       OpenGL stuff, use the PostRender event. *)
    property AfterRender: TNotifyEvent read GetAfterRender write SetAfterRender;
    // Access to buffer properties.
    property Buffer: TGLSceneBuffer read FBuffer write SetBuffer;
  end;

  (* Component to render a scene to memory only.
     This component curently requires that the OpenGL ICD supports the
     WGL_ARB_pbuffer extension (indirectly). *)
  TGLMemoryViewer = class(TGLNonVisualViewer)
  private
    FBufferCount: integer;
    procedure SetBufferCount(const Value: integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure InstantiateRenderingContext;
    procedure Render(baseObject: TGLBaseSceneObject = nil); override;
  published
    (* Set BufferCount > 1 for multiple render targets.
       Users should check if the corresponding extension (GL_ATI_draw_buffers)
       is supported. Current hardware limit is BufferCount = 4. *)
    property BufferCount: integer read FBufferCount write SetBufferCount default 1;
  end;

  TInvokeInfoForm = procedure(aSceneBuffer: TGLSceneBuffer; Modal: boolean);

(* Register an event handler triggered by any TGLBaseSceneObject Name change.
   *INCOMPLETE*, currently allows for only 1 (one) event, and is used by
   GLSceneEdit in the IDE. *)
procedure RegisterGLBaseSceneObjectNameChangeEvent(notifyEvent: TNotifyEvent);
(* Deregister an event handler triggered by any TGLBaseSceneObject Name change.
   See RegisterGLBaseSceneObjectNameChangeEvent. *)
procedure DeRegisterGLBaseSceneObjectNameChangeEvent(notifyEvent: TNotifyEvent);
(* Register an event handler triggered by any TGLBehaviour Name change.
   *INCOMPLETE*, currently allows for only 1 (one) event, and is used by
   FBehavioursEditor in the IDE. *)
procedure RegisterGLBehaviourNameChangeEvent(notifyEvent: TNotifyEvent);
(* Deregister an event handler triggered by any TGLBaseSceneObject Name change.
   See RegisterGLBaseSceneObjectNameChangeEvent. *)
procedure DeRegisterGLBehaviourNameChangeEvent(notifyEvent: TNotifyEvent);

// Issues OpenGL calls for drawing X, Y, Z axes in a standard style.
procedure AxesBuildList(var rci: TGLRenderContextInfo; pattern: Word; AxisLen: Single);

// Registers the procedure call used to invoke the info form.
procedure RegisterInfoForm(infoForm: TInvokeInfoForm);
procedure InvokeInfoForm(aSceneBuffer: TGLSceneBuffer; Modal: boolean);

function GetCurrentRenderingObject: TGLBaseSceneObject;

var
  vCounterFrequency: Int64;
{$IFNDEF USE_MULTITHREAD}
var
{$ELSE}
threadvar
{$ENDIF}
  vCurrentRenderingObject: TGLBaseSceneObject;

//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------

function GetCurrentRenderingObject: TGLBaseSceneObject;
begin
  Result := vCurrentRenderingObject;
end;

procedure AxesBuildList(var rci: TGLRenderContextInfo; pattern: Word; axisLen:
  Single);
begin
{$IFDEF USE_OPENGL_DEBUG}
  if GL.GREMEDY_string_marker then
    GL.StringMarkerGREMEDY(13, 'AxesBuildList');
{$ENDIF}
  with rci.GLStates do
  begin
    Disable(stLighting);
    if not rci.ignoreBlendingRequests then
    begin
      Enable(stBlend);
      SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
    end;
    LineWidth := 1;
    Enable(stLineStipple);
    LineStippleFactor := 1;
    LineStipplePattern := Pattern;
    DepthWriteMask := True;
    DepthFunc := cfLEqual;
    if rci.bufferDepthTest then
      Enable(stDepthTest);
  end;
  gl.Begin_(GL_LINES);
  gl.Color3f(0.5, 0.0, 0.0);
  gl.Vertex3f(0, 0, 0);
  gl.Vertex3f(-AxisLen, 0, 0);
  gl.Color3f(1.0, 0.0, 0.0);
  gl.Vertex3f(0, 0, 0);
  gl.Vertex3f(AxisLen, 0, 0);
  gl.Color3f(0.0, 0.5, 0.0);
  gl.Vertex3f(0, 0, 0);
  gl.Vertex3f(0, -AxisLen, 0);
  gl.Color3f(0.0, 1.0, 0.0);
  gl.Vertex3f(0, 0, 0);
  gl.Vertex3f(0, AxisLen, 0);
  gl.Color3f(0.0, 0.0, 0.5);
  gl.Vertex3f(0, 0, 0);
  gl.Vertex3f(0, 0, -AxisLen);
  gl.Color3f(0.0, 0.0, 1.0);
  gl.Vertex3f(0, 0, 0);
  gl.Vertex3f(0, 0, AxisLen);
  gl.End_;
end;

var
  vInfoForm: TInvokeInfoForm = nil;

procedure RegisterInfoForm(infoForm: TInvokeInfoForm);
begin
  vInfoForm := infoForm;
end;

procedure InvokeInfoForm(aSceneBuffer: TGLSceneBuffer; Modal: boolean);
begin
  if Assigned(vInfoForm) then
    vInfoForm(aSceneBuffer, Modal)
  else
    InformationDlg('InfoForm not available.');
end;

//------------------ internal global routines ----------------------------------

var
  vGLBaseSceneObjectNameChangeEvent: TNotifyEvent;
  vGLBehaviourNameChangeEvent: TNotifyEvent;

procedure RegisterGLBaseSceneObjectNameChangeEvent(notifyEvent: TNotifyEvent);
begin
  vGLBaseSceneObjectNameChangeEvent := notifyEvent;
end;

procedure DeRegisterGLBaseSceneObjectNameChangeEvent(notifyEvent: TNotifyEvent);
begin
  vGLBaseSceneObjectNameChangeEvent := nil;
end;

procedure RegisterGLBehaviourNameChangeEvent(notifyEvent: TNotifyEvent);
begin
  vGLBehaviourNameChangeEvent := notifyEvent;
end;

procedure DeRegisterGLBehaviourNameChangeEvent(notifyEvent: TNotifyEvent);
begin
  vGLBehaviourNameChangeEvent := nil;
end;

// ------------------
// ------------------ TGLBaseSceneObject ------------------
// ------------------
constructor TGLBaseSceneObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListHandle := TGLListHandle.Create;
  FObjectStyle := [];
  FChanges := [ocTransformation, ocStructure,
    ocAbsoluteMatrix, ocInvAbsoluteMatrix];
  FPosition := TGLCoordinates.CreateInitialized(Self, NullHmgPoint, csPoint);
  FRotation := TGLCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
  FDirection := TGLCoordinates.CreateInitialized(Self, ZHmgVector, csVector);
  FUp := TGLCoordinates.CreateInitialized(Self, YHmgVector, csVector);
  FScaling := TGLCoordinates.CreateInitialized(Self, XYZHmgVector, csVector);
  FLocalMatrix := IdentityHmgMatrix;
  FVisible := True;
  FPickable := True;
  FObjectsSorting := osInherited;
  FVisibilityCulling := vcInherited;
  FChildren := TGLPersistentObjectList.Create;

  fBBChanges := [oBBcChild, oBBcStructure];
  FBoundingBoxPersonalUnscaled := NullBoundingBox;
  FBoundingBoxOfChildren := NullBoundingBox;
  FBoundingBoxIncludingChildren := NullBoundingBox;
  distList := TGLSingleList.Create;
  objList := TGLPersistentObjectList.Create;
end;

constructor TGLBaseSceneObject.CreateAsChild(aParentOwner: TGLBaseSceneObject);
begin
  Create(aParentOwner);
  aParentOwner.AddChild(Self);
end;

destructor TGLBaseSceneObject.Destroy;
begin
  DeleteChildCameras;
  FEffects.Free;
  FBehaviours.Free;
  FListHandle.Free;
  FPosition.Free;
  FRotation.Free;
  FDirection.Free;
  FUp.Free;
  FScaling.Free;
  if Assigned(FParent) then
    FParent.Remove(Self, False);
  DeleteChildren;
  FChildren.Free;
  objList.Free;
  distList.Free;

  inherited Destroy;
end;

function TGLBaseSceneObject.GetHandle(var rci: TGLRenderContextInfo): Cardinal;
begin
  // Special case.. dirty trixxors
  if not Assigned(FListHandle) then
  begin
    Result := 0;
    Exit;
  end;
  Result := FListHandle.Handle;
  if Result = 0 then
    Result := FListHandle.AllocateHandle;
  if ocStructure in FChanges then
  begin
    ClearStructureChanged;
    FListHandle.NotifyChangesOfData;
  end;
  if FListHandle.IsDataNeedUpdate then
  begin
    rci.GLStates.NewList(Result, GL_COMPILE);
    try
      BuildList(rci);
    finally
      rci.GLStates.EndList;
    end;
    FListHandle.NotifyDataUpdated;
  end;
end;

function TGLBaseSceneObject.ListHandleAllocated: Boolean;
begin
  Result := Assigned(FListHandle)
    and (FListHandle.Handle <> 0)
    and not (ocStructure in FChanges);
end;

procedure TGLBaseSceneObject.DestroyHandle;
begin
  if Assigned(FListHandle) then
    FListHandle.DestroyHandle;
end;

procedure TGLBaseSceneObject.DestroyHandles;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Children[i].DestroyHandles;
  DestroyHandle;
end;

procedure TGLBaseSceneObject.SetBBChanges(const Value: TGLObjectBBChanges);
begin
  if value <> fBBChanges then
  begin
    fBBChanges := Value;
    if Assigned(FParent) then
      FParent.BBChanges := FParent.BBChanges + [oBBcChild];
  end;
end;

function TGLBaseSceneObject.Blended: Boolean;
begin
  Result := False;
end;

procedure TGLBaseSceneObject.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TGLBaseSceneObject.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      NotifyChange(Self);
  end
  else
    Assert(False, strUnBalancedBeginEndUpdate);
end;

procedure TGLBaseSceneObject.BuildList(var rci: TGLRenderContextInfo);
begin
  // nothing
end;

procedure TGLBaseSceneObject.DeleteChildCameras;
var
  i: Integer;
  child: TGLBaseSceneObject;
begin
  i := 0;
  while i < FChildren.Count do
  begin
    child := TGLBaseSceneObject(FChildren.List^[i]);
    child.DeleteChildCameras;
    if child is TGLCamera then
    begin
      Remove(child, True);
      child.Free;
    end
    else
      Inc(i);
  end;
end;

procedure TGLBaseSceneObject.DeleteChildren;
var
  child: TGLBaseSceneObject;
begin
  DeleteChildCameras;
  if Assigned(FScene) then
    FScene.RemoveLights(Self);
  while FChildren.Count > 0 do
  begin
    child := TGLBaseSceneObject(FChildren.Pop);
    child.FParent := nil;
    child.Free;
  end;
  BBChanges := BBChanges + [oBBcChild];
end;

procedure TGLBaseSceneObject.Loaded;
begin
  inherited;
  FPosition.W := 1;
  if Assigned(FBehaviours) then
    FBehaviours.Loaded;
  if Assigned(FEffects) then
    FEffects.Loaded;
end;

procedure TGLBaseSceneObject.DefineProperties(Filer: TFiler);
begin
  inherited;
  (*FOriginalFiler := Filer;*)
  Filer.DefineBinaryProperty('BehavioursData', ReadBehaviours, WriteBehaviours,
    (Assigned(FBehaviours) and (FBehaviours.Count > 0)));
  Filer.DefineBinaryProperty('EffectsData', ReadEffects, WriteEffects,
    (Assigned(FEffects) and (FEffects.Count > 0)));
  (*FOriginalFiler := nil;*)
end;

procedure TGLBaseSceneObject.WriteBehaviours(stream: TStream);
var
  writer: TWriter;
begin
  writer := TWriter.Create(stream, 16384);
  try
    Behaviours.WriteToFiler(writer);
  finally
    writer.Free;
  end;
end;

procedure TGLBaseSceneObject.ReadBehaviours(stream: TStream);
var
  reader: TReader;
begin
  reader := TReader.Create(stream, 16384);
  (* with TReader(FOriginalFiler) do  *)
  try
    (*
      reader.Root                 := Root;
      reader.OnError              := OnError;
      reader.OnFindMethod         := OnFindMethod;
      reader.OnSetName            := OnSetName;
      reader.OnReferenceName      := OnReferenceName;
      reader.OnAncestorNotFound   := OnAncestorNotFound;
      reader.OnCreateComponent    := OnCreateComponent;
      reader.OnFindComponentClass := OnFindComponentClass;
    *)
    Behaviours.ReadFromFiler(reader);
  finally
    reader.Free;
  end;
end;

procedure TGLBaseSceneObject.WriteEffects(stream: TStream);
var
  writer: TWriter;
begin
  writer := TWriter.Create(stream, 16384);
  try
    Effects.WriteToFiler(writer);
  finally
    writer.Free;
  end;
end;

procedure TGLBaseSceneObject.ReadEffects(stream: TStream);
var
  reader: TReader;
begin
  reader := TReader.Create(stream, 16384);
  (*with TReader(FOriginalFiler) do *)
  try
    (*
     reader.Root                 := Root;
     reader.OnError              := OnError;
     reader.OnFindMethod         := OnFindMethod;
     reader.OnSetName            := OnSetName;
     reader.OnReferenceName      := OnReferenceName;
     reader.OnAncestorNotFound   := OnAncestorNotFound;
     reader.OnCreateComponent    := OnCreateComponent;
     reader.OnFindComponentClass := OnFindComponentClass;
    *)
    Effects.ReadFromFiler(reader);
  finally
    reader.Free;
  end;
end;

procedure TGLBaseSceneObject.WriteRotations(stream: TStream);
begin
  stream.Write(FRotation.AsAddress^, 3 * SizeOf(TGLFloat));
end;

procedure TGLBaseSceneObject.ReadRotations(stream: TStream);
begin
  stream.Read(FRotation.AsAddress^, 3 * SizeOf(TGLFloat));
end;

procedure TGLBaseSceneObject.DrawAxes(var rci: TGLRenderContextInfo; pattern: Word);
begin
  AxesBuildList(rci, Pattern, rci.rcci.farClippingDistance - rci.rcci.nearClippingDistance);
end;

procedure TGLBaseSceneObject.GetChildren(AProc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  for i := 0 to FChildren.Count - 1 do
    if not IsSubComponent(TComponent(FChildren.List^[i])) then
      AProc(TComponent(FChildren.List^[i]));
end;

function TGLBaseSceneObject.Get(Index: Integer): TGLBaseSceneObject;
begin
  Result := TGLBaseSceneObject(FChildren[Index]);
end;

function TGLBaseSceneObject.GetCount: Integer;
begin
  Result := FChildren.Count;
end;

function TGLBaseSceneObject.GetDirectAbsoluteMatrix: PGLMatrix;
begin
  Result := @FAbsoluteMatrix;
end;

function TGLBaseSceneObject.HasSubChildren: Boolean;
var
  I: Integer;
begin
  Result := False;
  if Count <> 0 then
    for I := 0 to Count - 1 do
      if IsSubComponent(Children[i]) then
      begin
        Result := True;
        Exit;
      end;
end;

procedure TGLBaseSceneObject.AddChild(aChild: TGLBaseSceneObject);
begin
  if Assigned(FScene) then
    FScene.AddLights(aChild);
  FChildren.Add(aChild);
  aChild.FParent := Self;
  aChild.SetScene(FScene);
  TransformationChanged;
  aChild.TransformationChanged;
  aChild.DoOnAddedToParent;
  BBChanges := BBChanges + [oBBcChild];
end;

function TGLBaseSceneObject.AddNewChild(aChild: TGLSceneObjectClass): TGLBaseSceneObject;
begin
  Result := aChild.Create(Owner);
  AddChild(Result);
end;

function TGLBaseSceneObject.AddNewChildFirst(aChild: TGLSceneObjectClass): TGLBaseSceneObject;
begin
  Result := aChild.Create(Owner);
  Insert(0, Result);
end;

function TGLBaseSceneObject.GetOrCreateBehaviour(aBehaviour: TGLBehaviourClass): TGLBehaviour;
begin
  Result := TGLBehaviour(Behaviours.GetOrCreate(aBehaviour));
end;

function TGLBaseSceneObject.AddNewBehaviour(aBehaviour: TGLBehaviourClass): TGLBehaviour;
begin
  Assert(Behaviours.CanAdd(aBehaviour));
  result := aBehaviour.Create(Behaviours)
end;

function TGLBaseSceneObject.GetOrCreateEffect(aEffect: TGLEffectClass): TGLEffect;
begin
  Result := TGLEffect(Effects.GetOrCreate(aEffect));
end;

function TGLBaseSceneObject.AddNewEffect(aEffect: TGLEffectClass): TGLEffect;
begin
  Assert(Effects.CanAdd(aEffect));
  result := aEffect.Create(Effects)
end;

procedure TGLBaseSceneObject.RebuildMatrix;
begin
  if ocTransformation in Changes then
  begin
    VectorScale(LeftVector, Scale.X, FLocalMatrix.X);
    VectorScale(FUp.AsVector, Scale.Y, FLocalMatrix.Y);
    VectorScale(FDirection.AsVector, Scale.Z, FLocalMatrix.Z);
    SetVector(FLocalMatrix.W, FPosition.AsVector);
    Exclude(FChanges, ocTransformation);
    Include(FChanges, ocAbsoluteMatrix);
    Include(FChanges, ocInvAbsoluteMatrix);
  end;
end;

procedure TGLBaseSceneObject.ForceLocalMatrix(const aMatrix: TGLMatrix);
begin
  FLocalMatrix := aMatrix;
  Exclude(FChanges, ocTransformation);
  Include(FChanges, ocAbsoluteMatrix);
  Include(FChanges, ocInvAbsoluteMatrix);
end;

function TGLBaseSceneObject.AbsoluteMatrixAsAddress: PGLMatrix;
begin
  if ocAbsoluteMatrix in FChanges then
  begin
    RebuildMatrix;
    if Assigned(Parent) (*and (not (Parent is TGLSceneRootObject))*) then
    begin
      MatrixMultiply(FLocalMatrix, TGLBaseSceneObject(Parent).AbsoluteMatrixAsAddress^,
        FAbsoluteMatrix);
    end
    else
      FAbsoluteMatrix := FLocalMatrix;
    Exclude(FChanges, ocAbsoluteMatrix);
    Include(FChanges, ocInvAbsoluteMatrix);
  end;
  Result := @FAbsoluteMatrix;
end;

function TGLBaseSceneObject.InvAbsoluteMatrix: TGLMatrix;
begin
  Result := InvAbsoluteMatrixAsAddress^;
end;

function TGLBaseSceneObject.InvAbsoluteMatrixAsAddress: PGLMatrix;
begin
  if ocInvAbsoluteMatrix in FChanges then
  begin
    if VectorEquals(Scale.DirectVector, XYZHmgVector) then
    begin
      RebuildMatrix;
      if Parent <> nil then
        FInvAbsoluteMatrix :=
          MatrixMultiply(Parent.InvAbsoluteMatrixAsAddress^,
          AnglePreservingMatrixInvert(FLocalMatrix))
      else
        FInvAbsoluteMatrix := AnglePreservingMatrixInvert(FLocalMatrix);
    end
    else
    begin
      FInvAbsoluteMatrix := AbsoluteMatrixAsAddress^;
      InvertMatrix(FInvAbsoluteMatrix);
    end;
    Exclude(FChanges, ocInvAbsoluteMatrix);
  end;
  Result := @FInvAbsoluteMatrix;
end;

function TGLBaseSceneObject.GetAbsoluteMatrix: TGLMatrix;
begin
  Result := AbsoluteMatrixAsAddress^;
end;

procedure TGLBaseSceneObject.SetAbsoluteMatrix(const Value: TGLMatrix);
begin
  if not MatrixEquals(Value, FAbsoluteMatrix) then
  begin
    FAbsoluteMatrix := Value;
    if Parent <> nil then
      SetMatrix(MatrixMultiply(FAbsoluteMatrix,
        Parent.InvAbsoluteMatrixAsAddress^))
    else
      SetMatrix(Value);
  end;
end;

function TGLBaseSceneObject.GetAbsoluteDirection: TGLVector;
begin
  Result := VectorNormalize(AbsoluteMatrixAsAddress^.V[2]);
end;

procedure TGLBaseSceneObject.SetAbsoluteDirection(const v: TGLVector);
begin
  if Parent <> nil then
    Direction.AsVector := Parent.AbsoluteToLocal(v)
  else
    Direction.AsVector := v;
end;

function TGLBaseSceneObject.GetAbsoluteScale: TGLVector;
begin
  Result.X := AbsoluteMatrixAsAddress^.X.X;
  Result.Y := AbsoluteMatrixAsAddress^.Y.Y;
  Result.Z := AbsoluteMatrixAsAddress^.Z.Z;

  Result.W := 0;
end;

procedure TGLBaseSceneObject.SetAbsoluteScale(const Value: TGLVector);
begin
  if Parent <> nil then
    Scale.AsVector := Parent.AbsoluteToLocal(Value)
  else
    Scale.AsVector := Value;
end;

function TGLBaseSceneObject.GetAbsoluteUp: TGLVector;
begin
  Result := VectorNormalize(AbsoluteMatrixAsAddress^.Y);
end;

procedure TGLBaseSceneObject.SetAbsoluteUp(const v: TGLVector);
begin
  if Parent <> nil then
    Up.AsVector := Parent.AbsoluteToLocal(v)
  else
    Up.AsVector := v;
end;

function TGLBaseSceneObject.AbsoluteRight: TGLVector;
begin
  Result := VectorNormalize(AbsoluteMatrixAsAddress^.X);
end;

function TGLBaseSceneObject.AbsoluteLeft: TGLVector;
begin
  Result := VectorNegate(AbsoluteRight);
end;

function TGLBaseSceneObject.GetAbsolutePosition: TGLVector;
begin
  Result := AbsoluteMatrixAsAddress^.W;
end;

procedure TGLBaseSceneObject.SetAbsolutePosition(const v: TGLVector);
begin
  if Assigned(Parent) then
    Position.AsVector := Parent.AbsoluteToLocal(v)
  else
    Position.AsVector := v;
end;

function TGLBaseSceneObject.AbsolutePositionAsAddress: PGLVector;
begin
  Result := @AbsoluteMatrixAsAddress^.W;
end;

function TGLBaseSceneObject.AbsoluteXVector: TGLVector;
begin
  AbsoluteMatrixAsAddress;
  SetVector(Result, PAffineVector(@FAbsoluteMatrix.X)^);
end;

function TGLBaseSceneObject.AbsoluteYVector: TGLVector;
begin
  AbsoluteMatrixAsAddress;
  SetVector(Result, PAffineVector(@FAbsoluteMatrix.Y)^);
end;

function TGLBaseSceneObject.AbsoluteZVector: TGLVector;
begin
  AbsoluteMatrixAsAddress;
  SetVector(Result, PAffineVector(@FAbsoluteMatrix.Z)^);
end;

function TGLBaseSceneObject.AbsoluteToLocal(const v: TGLVector): TGLVector;
begin
  Result := VectorTransform(v, InvAbsoluteMatrixAsAddress^);
end;

function TGLBaseSceneObject.AbsoluteToLocal(const v: TAffineVector):
  TAffineVector;
begin
  Result := VectorTransform(v, InvAbsoluteMatrixAsAddress^);
end;

function TGLBaseSceneObject.LocalToAbsolute(const v: TGLVector): TGLVector;
begin
  Result := VectorTransform(v, AbsoluteMatrixAsAddress^);
end;

function TGLBaseSceneObject.LocalToAbsolute(const v: TAffineVector):
  TAffineVector;
begin
  Result := VectorTransform(v, AbsoluteMatrixAsAddress^);
end;

function TGLBaseSceneObject.Right: TGLVector;
begin
  Result := VectorCrossProduct(FDirection.AsVector, FUp.AsVector);
end;

function TGLBaseSceneObject.LeftVector: TGLVector;
begin
  Result := VectorCrossProduct(FUp.AsVector, FDirection.AsVector);
end;

function TGLBaseSceneObject.BarycenterAbsolutePosition: TGLVector;
begin
  Result := AbsolutePosition;
end;

function TGLBaseSceneObject.SqrDistanceTo(anObject: TGLBaseSceneObject): Single;
begin
  if Assigned(anObject) then
    Result := VectorDistance2(AbsolutePosition, anObject.AbsolutePosition)
  else
    Result := 0;
end;

function TGLBaseSceneObject.SqrDistanceTo(const pt: TGLVector): Single;
begin
  Result := VectorDistance2(pt, AbsolutePosition);
end;

function TGLBaseSceneObject.DistanceTo(anObject: TGLBaseSceneObject): Single;
begin
  if Assigned(anObject) then
    Result := VectorDistance(AbsolutePosition, anObject.AbsolutePosition)
  else
    Result := 0;
end;

function TGLBaseSceneObject.DistanceTo(const pt: TGLVector): Single;
begin
  Result := VectorDistance(AbsolutePosition, pt);
end;

function TGLBaseSceneObject.BarycenterSqrDistanceTo(const pt: TGLVector): Single;
var
  d: TGLVector;
begin
  d := BarycenterAbsolutePosition;
  Result := VectorDistance2(d, pt);
end;

function TGLBaseSceneObject.AxisAlignedDimensions: TGLVector;
begin
  Result := AxisAlignedDimensionsUnscaled();
  ScaleVector(Result, Scale.AsVector);
end;

function TGLBaseSceneObject.AxisAlignedDimensionsUnscaled: TGLVector;
begin
  Result.X := 0.5;
  Result.Y := 0.5;
  Result.Z := 0.5;
  Result.W := 0;
end;

function TGLBaseSceneObject.AxisAlignedBoundingBox(const AIncludeChilden: Boolean): TAABB;
var
  i: Integer;
  aabb: TAABB;
  child: TGLBaseSceneObject;
begin
  SetAABB(Result, AxisAlignedDimensionsUnscaled);
  // not tested for child objects
  if AIncludeChilden then
  begin
    for i := 0 to FChildren.Count - 1 do
    begin
      child := TGLBaseSceneObject(FChildren.List^[i]);
      aabb := child.AxisAlignedBoundingBoxUnscaled(AIncludeChilden);
      AABBTransform(aabb, child.Matrix^);
      AddAABB(Result, aabb);
    end;
  end;
  AABBScale(Result, Scale.AsAffineVector);
end;

function TGLBaseSceneObject.AxisAlignedBoundingBoxUnscaled(
  const AIncludeChilden: Boolean): TAABB;
var
  i: Integer;
  aabb: TAABB;
begin
  SetAABB(Result, AxisAlignedDimensionsUnscaled);
  //not tested for child objects
  if AIncludeChilden then
  begin
    for i := 0 to FChildren.Count - 1 do
    begin
      aabb :=
        TGLBaseSceneObject(FChildren.List^[i]).AxisAlignedBoundingBoxUnscaled(AIncludeChilden);
      AABBTransform(aabb, TGLBaseSceneObject(FChildren.List^[i]).Matrix^);
      AddAABB(Result, aabb);
    end;
  end;
end;

function TGLBaseSceneObject.AxisAlignedBoundingBoxAbsolute(
  const AIncludeChilden: Boolean; const AUseBaryCenter: Boolean): TAABB;
begin
  Result := BBToAABB(BoundingBoxAbsolute(AIncludeChilden, AUseBaryCenter));
end;

function TGLBaseSceneObject.BoundingBox(const AIncludeChilden: Boolean;
  const AUseBaryCenter: Boolean): THmgBoundingBox;
var
  CurrentBaryOffset: TGLVector;
begin
  Result := AABBToBB(AxisAlignedBoundingBox(AIncludeChilden));

  // code not tested...
  if AUseBaryCenter then
  begin
    CurrentBaryOffset :=
      VectorSubtract(AbsoluteToLocal(BarycenterAbsolutePosition),
      Position.AsVector);
    OffsetBBPoint(Result, CurrentBaryOffset);
  end;
end;

function TGLBaseSceneObject.BoundingBoxUnscaled(
  const AIncludeChilden: Boolean;
  const AUseBaryCenter: Boolean): THmgBoundingBox;
var
  CurrentBaryOffset: TGLVector;
begin
  Result := AABBToBB(AxisAlignedBoundingBoxUnscaled(AIncludeChilden));

  // code not tested...
  if AUseBaryCenter then
  begin
    CurrentBaryOffset :=
      VectorSubtract(AbsoluteToLocal(BarycenterAbsolutePosition),
      Position.AsVector);
    OffsetBBPoint(Result, CurrentBaryOffset);
  end;
end;

function TGLBaseSceneObject.BoundingBoxAbsolute(const AIncludeChilden: Boolean;
  const AUseBaryCenter: Boolean): THmgBoundingBox;
var
  I: Integer;
  CurrentBaryOffset: TGLVector;
begin
  Result := BoundingBoxUnscaled(AIncludeChilden, False);
  for I := 0 to 7 do
    Result.BBox[I] := LocalToAbsolute(Result.BBox[I]);

  if AUseBaryCenter then
  begin
    CurrentBaryOffset := VectorSubtract(BarycenterAbsolutePosition,
      AbsolutePosition);
    OffsetBBPoint(Result, CurrentBaryOffset);
  end;
end;

function TGLBaseSceneObject.BoundingSphereRadius: Single;
begin
  Result := VectorLength(AxisAlignedDimensions);
end;

function TGLBaseSceneObject.BoundingSphereRadiusUnscaled: Single;
begin
  Result := VectorLength(AxisAlignedDimensionsUnscaled);
end;

function TGLBaseSceneObject.PointInObject(const point: TGLVector): Boolean;
var
  localPt, dim: TGLVector;
begin
  dim := AxisAlignedDimensions;
  localPt := VectorTransform(point, InvAbsoluteMatrix);
  Result := (Abs(localPt.X * Scale.X) <= dim.X) and
            (Abs(localPt.Y * Scale.Y) <= dim.Y) and
            (Abs(localPt.Z * Scale.Z) <= dim.Z);
end;

procedure TGLBaseSceneObject.CalculateBoundingBoxPersonalUnscaled(var ANewBoundingBox: THmgBoundingBox);
begin
  // Using the standard method to get the local BB.
  ANewBoundingBox := AABBToBB(AxisAlignedBoundingBoxUnscaled(False));
  OffsetBBPoint(ANewBoundingBox, AbsoluteToLocal(BarycenterAbsolutePosition));
end;

function TGLBaseSceneObject.BoundingBoxPersonalUnscaledEx: THmgBoundingBox;
begin
  if oBBcStructure in FBBChanges then
  begin
    CalculateBoundingBoxPersonalUnscaled(FBoundingBoxPersonalUnscaled);
    Exclude(FBBChanges, oBBcStructure);
  end;
  Result := FBoundingBoxPersonalUnscaled;
end;

function TGLBaseSceneObject.AxisAlignedBoundingBoxAbsoluteEx: TAABB;
var
  pBB: THmgBoundingBox;
begin
  pBB := BoundingBoxIncludingChildrenEx;
  BBTransform(pBB, AbsoluteMatrix);
  Result := BBtoAABB(pBB);
end;

function TGLBaseSceneObject.AxisAlignedBoundingBoxEx: TAABB;
begin
  Result := BBtoAABB(BoundingBoxIncludingChildrenEx);
  AABBScale(Result, Scale.AsAffineVector);
end;

function TGLBaseSceneObject.BoundingBoxOfChildrenEx: THmgBoundingBox;
var
  i: Integer;
  pBB: THmgBoundingBox;
begin
  if oBBcChild in FBBChanges then
  begin
    // Computing
    FBoundingBoxOfChildren := NullBoundingBox;
    for i := 0 to FChildren.count - 1 do
    begin
      pBB :=
        TGLBaseSceneObject(FChildren.List^[i]).BoundingBoxIncludingChildrenEx;
      if not BoundingBoxesAreEqual(@pBB, @NullBoundingBox) then
      begin
        // transformation with local matrix
        BBTransform(pbb, TGLBaseSceneObject(FChildren.List^[i]).Matrix^);
        if BoundingBoxesAreEqual(@FBoundingBoxOfChildren, @NullBoundingBox) then
          FBoundingBoxOfChildren := pBB
        else
          AddBB(FBoundingBoxOfChildren, pBB);
      end;
    end;
    exclude(FBBChanges, oBBcChild);
  end;
  result := FBoundingBoxOfChildren;
end;

function TGLBaseSceneObject.BoundingBoxIncludingChildrenEx: THmgBoundingBox;
var
  pBB: THmgBoundingBox;
begin
  if (oBBcStructure in FBBChanges) or (oBBcChild in FBBChanges) then
  begin
    pBB := BoundingBoxPersonalUnscaledEx;
    if BoundingBoxesAreEqual(@pBB, @NullBoundingBox) then
      FBoundingBoxIncludingChildren := BoundingBoxOfChildrenEx
    else
    begin
      FBoundingBoxIncludingChildren := pBB;
      pBB := BoundingBoxOfChildrenEx;
      if not BoundingBoxesAreEqual(@pBB, @NullBoundingBox) then
        AddBB(FBoundingBoxIncludingChildren, pBB);
    end;
  end;
  Result := FBoundingBoxIncludingChildren;
end;

function TGLBaseSceneObject.RayCastIntersect(const rayStart, rayVector: TGLVector;
  intersectPoint: PGLVector = nil;
  intersectNormal: PGLVector = nil): Boolean;
var
  i1, i2, absPos: TGLVector;
begin
  SetVector(absPos, AbsolutePosition);
  if RayCastSphereIntersect(rayStart, rayVector, absPos, BoundingSphereRadius,
    i1, i2) > 0 then
  begin
    Result := True;
    if Assigned(intersectPoint) then
      SetVector(intersectPoint^, i1);
    if Assigned(intersectNormal) then
    begin
      SubtractVector(i1, absPos);
      NormalizeVector(i1);
      SetVector(intersectNormal^, i1);
    end;
  end
  else
    Result := False;
end;

function TGLBaseSceneObject.GenerateSilhouette(const silhouetteParameters: TGLSilhouetteParameters): TGLSilhouette;
const
  cNbSegments = 21;
var
  i, j: Integer;
  d, r, vr, s, c, angleFactor: Single;
  sVec, tVec: TAffineVector;
begin
  r := BoundingSphereRadiusUnscaled;
  d := VectorLength(silhouetteParameters.SeenFrom);
  // determine visible radius
  case silhouetteParameters.Style of
    ssOmni: vr := SphereVisibleRadius(d, r);
    ssParallel: vr := r;
  else
    Assert(False);
    vr := r;
  end;
  // determine a local orthonormal matrix, viewer-oriented
  sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, XVector);
  if VectorLength(sVec) < 1e-3 then
    sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, YVector);
  tVec := VectorCrossProduct(silhouetteParameters.SeenFrom, sVec);
  NormalizeVector(sVec);
  NormalizeVector(tVec);
  // generate the silhouette (outline and capping)
  Result := TGLSilhouette.Create;
  angleFactor := (2 * PI) / cNbSegments;
  vr := vr * 0.98;
  for i := 0 to cNbSegments - 1 do
  begin
    SinCosine(i * angleFactor, vr, s, c);
    Result.Vertices.AddPoint(VectorCombine(sVec, tVec, s, c));
    j := (i + 1) mod cNbSegments;
    Result.Indices.Add(i, j);
    if silhouetteParameters.CappingRequired then
      Result.CapIndices.Add(cNbSegments, i, j)
  end;
  if silhouetteParameters.CappingRequired then
    Result.Vertices.Add(NullHmgPoint);
end;

procedure TGLBaseSceneObject.Assign(Source: TPersistent);
var
  i: Integer;
  child, newChild: TGLBaseSceneObject;
begin
  if Assigned(Source) and (Source is TGLBaseSceneObject) then
  begin
    DestroyHandles;
    FVisible := TGLBaseSceneObject(Source).FVisible;
    TGLBaseSceneObject(Source).RebuildMatrix;
    SetMatrix(TGLBaseSceneObject(Source).FLocalMatrix);
    FShowAxes := TGLBaseSceneObject(Source).FShowAxes;
    FObjectsSorting := TGLBaseSceneObject(Source).FObjectsSorting;
    FVisibilityCulling := TGLBaseSceneObject(Source).FVisibilityCulling;
    FRotation.Assign(TGLBaseSceneObject(Source).FRotation);
    DeleteChildren;
    if Assigned(Scene) then
      Scene.BeginUpdate;
    if Assigned(TGLBaseSceneObject(Source).FChildren) then
    begin
      for i := 0 to TGLBaseSceneObject(Source).FChildren.Count - 1 do
      begin
        child := TGLBaseSceneObject(TGLBaseSceneObject(Source).FChildren[i]);
        newChild := AddNewChild(TGLSceneObjectClass(child.ClassType));
        newChild.Assign(child);
      end;
    end;
    if Assigned(Scene) then
      Scene.EndUpdate;
    OnProgress := TGLBaseSceneObject(Source).OnProgress;
    if Assigned(TGLBaseSceneObject(Source).FBehaviours) then
      Behaviours.Assign(TGLBaseSceneObject(Source).Behaviours)
    else
      FreeAndNil(FBehaviours);
    if Assigned(TGLBaseSceneObject(Source).FEffects) then
      Effects.Assign(TGLBaseSceneObject(Source).Effects)
    else
      FreeAndNil(FEffects);
    Tag := TGLBaseSceneObject(Source).Tag;
    FTagFloat := TGLBaseSceneObject(Source).FTagFloat;
  end
  else
    inherited Assign(Source);
end;

function TGLBaseSceneObject.IsUpdating: Boolean;
begin
  Result := (FUpdateCount <> 0) or (csReading in ComponentState);
end;

function TGLBaseSceneObject.GetParentComponent: TComponent;
begin
  if FParent is TGLSceneRootObject then
    Result := FScene
  else
    Result := FParent;
end;

function TGLBaseSceneObject.HasParent: Boolean;
begin
  Result := assigned(FParent);
end;

procedure TGLBaseSceneObject.Lift(aDistance: Single);
begin
  FPosition.AddScaledVector(aDistance, FUp.AsVector);
  TransformationChanged;
end;

procedure TGLBaseSceneObject.Move(ADistance: Single);
begin
  FPosition.AddScaledVector(ADistance, FDirection.AsVector);
  TransformationChanged;
end;

procedure TGLBaseSceneObject.Slide(ADistance: Single);
begin
  FPosition.AddScaledVector(ADistance, Right);
  TransformationChanged;
end;

procedure TGLBaseSceneObject.ResetRotations;
begin
  FillChar(FLocalMatrix, SizeOf(TGLMatrix), 0);
  FLocalMatrix.X.X := Scale.DirectX;
  FLocalMatrix.Y.Y := Scale.DirectY;
  FLocalMatrix.Z.Z := Scale.DirectZ;
  SetVector(FLocalMatrix.W, Position.DirectVector);
  FRotation.DirectVector := NullHmgPoint;
  FDirection.DirectVector := ZHmgVector;
  FUp.DirectVector := YHmgVector;
  TransformationChanged;
  Exclude(FChanges, ocTransformation);
end;

procedure TGLBaseSceneObject.ResetAndPitchTurnRoll(const degX, degY, degZ: Single);
var
  rotMatrix: TGLMatrix;
  V: TGLVector;
begin
  ResetRotations;
  // set DegX (Pitch)
  rotMatrix := CreateRotationMatrix(Right, degX * cPIdiv180);
  V := VectorTransform(FUp.AsVector, rotMatrix);
  NormalizeVector(V);
  FUp.DirectVector := V;
  V := VectorTransform(FDirection.AsVector, rotMatrix);
  NormalizeVector(V);
  FDirection.DirectVector := V;
  FRotation.DirectX := NormalizeDegAngle(DegX);
  // set DegY (Turn)
  rotMatrix := CreateRotationMatrix(FUp.AsVector, degY * cPIdiv180);
  V := VectorTransform(FUp.AsVector, rotMatrix);
  NormalizeVector(V);
  FUp.DirectVector := V;
  V := VectorTransform(FDirection.AsVector, rotMatrix);
  NormalizeVector(V);
  FDirection.DirectVector := V;
  FRotation.DirectY := NormalizeDegAngle(DegY);
  // set DegZ (Roll)
  rotMatrix := CreateRotationMatrix(Direction.AsVector, degZ * cPIdiv180);
  V := VectorTransform(FUp.AsVector, rotMatrix);
  NormalizeVector(V);
  FUp.DirectVector := V;
  V := VectorTransform(FDirection.AsVector, rotMatrix);
  NormalizeVector(V);
  FDirection.DirectVector := V;
  FRotation.DirectZ := NormalizeDegAngle(DegZ);
  TransformationChanged;
  NotifyChange(self);
end;

procedure TGLBaseSceneObject.RotateAbsolute(const rx, ry, rz: Single);
var
  resMat: TGLMatrix;
  v: TAffineVector;
begin
  resMat := Matrix^;
  // No we build rotation matrices and use them to rotate the obj
  if rx <> 0 then
  begin
    SetVector(v, AbsoluteToLocal(XVector));
    resMat := MatrixMultiply(CreateRotationMatrix(v, -DegToRadian(rx)), resMat);
  end;
  if ry <> 0 then
  begin
    SetVector(v, AbsoluteToLocal(YVector));
    resMat := MatrixMultiply(CreateRotationMatrix(v, -DegToRadian(ry)), resMat);
  end;
  if rz <> 0 then
  begin
    SetVector(v, AbsoluteToLocal(ZVector));
    resMat := MatrixMultiply(CreateRotationMatrix(v, -DegToRadian(rz)), resMat);
  end;
  SetMatrix(resMat);
end;

procedure TGLBaseSceneObject.RotateAbsolute(const axis: TAffineVector; angle: Single);
var
  v: TAffineVector;
begin
  if angle <> 0 then
  begin
    SetVector(v, AbsoluteToLocal(axis));
    SetMatrix(MatrixMultiply(CreateRotationMatrix(v, DegToRadian(angle)), Matrix^));
  end;
end;

procedure TGLBaseSceneObject.Pitch(angle: Single);
var
  r: Single;
  rightVector: TGLVector;
begin
  FIsCalculating := True;
  try
    angle := -DegToRad(angle);
    rightVector := Right;
    FUp.Rotate(rightVector, angle);
    FUp.Normalize;
    FDirection.Rotate(rightVector, angle);
    FDirection.Normalize;
    r := -RadToDeg(ArcTan2(FDirection.Y, VectorLength(FDirection.X, FDirection.Z)));
    if FDirection.X < 0 then
      if FDirection.Y < 0 then
        r := 180 - r
      else
        r := -180 - r;
    FRotation.X := r;
  finally
    FIsCalculating := False;
  end;
  TransformationChanged;
end;

procedure TGLBaseSceneObject.SetPitchAngle(AValue: Single);
var
  diff: Single;
  rotMatrix: TGLMatrix;
begin
  if AValue <> FRotation.X then
  begin
    if not (csLoading in ComponentState) then
    begin
      FIsCalculating := True;
      try
        diff := DegToRadian(FRotation.X - AValue);
        rotMatrix := CreateRotationMatrix(Right, diff);
        FUp.DirectVector := VectorTransform(FUp.AsVector, rotMatrix);
        FUp.Normalize;
        FDirection.DirectVector := VectorTransform(FDirection.AsVector,
          rotMatrix);
        FDirection.Normalize;
        TransformationChanged;
      finally
        FIsCalculating := False;
      end;
    end;
    FRotation.DirectX := NormalizeDegAngle(AValue);
  end;
end;

procedure TGLBaseSceneObject.Roll(angle: Single);
var
  r: Single;
  rightVector, directionVector: TGLVector;
begin
  FIsCalculating := True;
  try
    angle := DegToRadian(angle);
    directionVector := Direction.AsVector;
    FUp.Rotate(directionVector, angle);
    FUp.Normalize;
    FDirection.Rotate(directionVector, angle);
    FDirection.Normalize;

    // calculate new rotation angle from vectors
    rightVector := Right;
    r := -RadToDeg(ArcTan2(rightVector.Y,
              VectorLength(rightVector.X,
                           rightVector.Z)));
    if rightVector.X < 0 then
      if rightVector.Y < 0 then
        r := 180 - r
      else
        r := -180 - r;
    FRotation.Z := r;
  finally
    FIsCalculating := False;
  end;
  TransformationChanged;
end;

procedure TGLBaseSceneObject.SetRollAngle(AValue: Single);
var
  diff: Single;
  rotMatrix: TGLMatrix;
begin
  if AValue <> FRotation.Z then
  begin
    if not (csLoading in ComponentState) then
    begin
      FIsCalculating := True;
      try
        diff := DegToRadian(FRotation.Z - AValue);
        rotMatrix := CreateRotationMatrix(Direction.AsVector, diff);
        FUp.DirectVector := VectorTransform(FUp.AsVector, rotMatrix);
        FUp.Normalize;
        FDirection.DirectVector := VectorTransform(FDirection.AsVector,
          rotMatrix);
        FDirection.Normalize;
        TransformationChanged;
      finally
        FIsCalculating := False;
      end;
    end;
    FRotation.DirectZ := NormalizeDegAngle(AValue);
  end;
end;

procedure TGLBaseSceneObject.Turn(angle: Single);
var
  r: Single;
  upVector: TGLVector;
begin
  FIsCalculating := True;
  try
    angle := DegToRadian(angle);
    upVector := Up.AsVector;
    FUp.Rotate(upVector, angle);
    FUp.Normalize;
    FDirection.Rotate(upVector, angle);
    FDirection.Normalize;
    r := -RadToDeg(ArcTan2(FDirection.X, VectorLength(FDirection.Y, FDirection.Z)));
    if FDirection.X < 0 then
      if FDirection.Y < 0 then
        r := 180 - r
      else
        r := -180 - r;
    FRotation.Y := r;
  finally
    FIsCalculating := False;
  end;
  TransformationChanged;
end;

procedure TGLBaseSceneObject.SetTurnAngle(AValue: Single);
var
  diff: Single;
  rotMatrix: TGLMatrix;
begin
  if AValue <> FRotation.Y then
  begin
    if not (csLoading in ComponentState) then
    begin
      FIsCalculating := True;
      try
        diff := DegToRadian(FRotation.Y - AValue);
        rotMatrix := CreateRotationMatrix(Up.AsVector, diff);
        FUp.DirectVector := VectorTransform(FUp.AsVector, rotMatrix);
        FUp.Normalize;
        FDirection.DirectVector := VectorTransform(FDirection.AsVector, rotMatrix);
        FDirection.Normalize;
        TransformationChanged;
      finally
        FIsCalculating := False;
      end;
    end;
    FRotation.DirectY := NormalizeDegAngle(AValue);
  end;
end;

procedure TGLBaseSceneObject.SetRotation(aRotation: TGLCoordinates);
begin
  FRotation.Assign(aRotation);
  TransformationChanged;
end;

function TGLBaseSceneObject.GetPitchAngle: Single;
begin
  Result := FRotation.X;
end;

function TGLBaseSceneObject.GetTurnAngle: Single;
begin
  Result := FRotation.Y;
end;

function TGLBaseSceneObject.GetRollAngle: Single;
begin
  Result := FRotation.Z;
end;

procedure TGLBaseSceneObject.PointTo(const ATargetObject: TGLBaseSceneObject; const AUpVector: TGLVector);
begin
  PointTo(ATargetObject.AbsolutePosition, AUpVector);
end;

procedure TGLBaseSceneObject.PointTo(const AAbsolutePosition, AUpVector: TGLVector);
var
  absDir, absRight, absUp: TGLVector;
begin
  // first compute absolute attitude for pointing
  absDir := VectorSubtract(AAbsolutePosition, Self.AbsolutePosition);
  NormalizeVector(absDir);
  absRight := VectorCrossProduct(absDir, AUpVector);
  NormalizeVector(absRight);
  absUp := VectorCrossProduct(absRight, absDir);
  // convert absolute to local and adjust object
  if Parent <> nil then
  begin
    FUp.AsVector := Parent.AbsoluteToLocal(absUp);
    FDirection.AsVector := Parent.AbsoluteToLocal(absDir);
  end
  else
  begin
    FUp.AsVector := absUp;
    FDirection.AsVector := absDir;
  end;
  TransformationChanged
end;

procedure TGLBaseSceneObject.SetShowAxes(AValue: Boolean);
begin
  if FShowAxes <> AValue then
  begin
    FShowAxes := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLBaseSceneObject.SetScaling(AValue: TGLCoordinates);
begin
  FScaling.Assign(AValue);
  TransformationChanged;
end;

procedure TGLBaseSceneObject.SetName(const NewName: TComponentName);
begin
  if Name <> NewName then
  begin
    inherited SetName(NewName);
    if Assigned(vGLBaseSceneObjectNameChangeEvent) then
      vGLBaseSceneObjectNameChangeEvent(Self);
  end;
end;

procedure TGLBaseSceneObject.SetParent(const val: TGLBaseSceneObject);
begin
  MoveTo(val);
end;

function TGLBaseSceneObject.GetIndex: Integer;
begin
  if Assigned(FParent) then
    Result := FParent.FChildren.IndexOf(Self)
  else
    Result := -1;
end;

function TGLBaseSceneObject.GetLocalMatrix: PGLMatrix;
begin
  Result := @FLocalMatrix;
end;

procedure TGLBaseSceneObject.SetIndex(aValue: Integer);
var
  LCount: Integer;
  parentBackup: TGLBaseSceneObject;
begin
  if Assigned(FParent) then
  begin
    if aValue < 0 then
      aValue := 0;
    LCount := FParent.Count;
    if aValue >= LCount then
      aValue := LCount - 1;
    if aValue <> Index then
    begin
      if Assigned(FScene) then
        FScene.BeginUpdate;
      parentBackup := FParent;
      parentBackup.Remove(Self, False);
      parentBackup.Insert(AValue, Self);
      if Assigned(FScene) then
        FScene.EndUpdate;
    end;
  end;
end;

procedure TGLBaseSceneObject.SetParentComponent(Value: TComponent);
begin
  inherited;
  if Value = FParent then
    Exit;

  if Value is TGLScene then
    SetParent(TGLScene(Value).Objects)
  else if Value is TGLBaseSceneObject then
    SetParent(TGLBaseSceneObject(Value))
  else
    SetParent(nil);
end;

procedure TGLBaseSceneObject.StructureChanged;
begin
  if not (ocStructure in FChanges) then
  begin
    Include(FChanges, ocStructure);
    NotifyChange(Self);
  end
  else if osDirectDraw in ObjectStyle then
    NotifyChange(Self);
end;

procedure TGLBaseSceneObject.ClearStructureChanged;
begin
  Exclude(FChanges, ocStructure);
  SetBBChanges(BBChanges + [oBBcStructure]);
end;

procedure TGLBaseSceneObject.RecTransformationChanged;
var
  i: Integer;
  list: PGLPointerObjectList;
  matSet: TGLObjectChanges;
begin
  matSet := [ocAbsoluteMatrix, ocInvAbsoluteMatrix];
  if matSet * FChanges <> matSet then
  begin
    FChanges := FChanges + matSet;
    list := FChildren.List;
    for i := 0 to FChildren.Count - 1 do
      TGLBaseSceneObject(list^[i]).RecTransformationChanged;
  end;
end;

procedure TGLBaseSceneObject.TransformationChanged;
begin
  if not (ocTransformation in FChanges) then
  begin
    Include(FChanges, ocTransformation);
    RecTransformationChanged;
    if not (csLoading in ComponentState) then
      NotifyChange(Self);
  end;
end;

procedure TGLBaseSceneObject.MoveTo(newParent: TGLBaseSceneObject);
begin
  if newParent = FParent then
    Exit;
  if Assigned(FParent) then
  begin
    FParent.Remove(Self, False);
    FParent := nil;
  end;
  if Assigned(newParent) then
    newParent.AddChild(Self)
  else
    SetScene(nil);
end;

procedure TGLBaseSceneObject.MoveUp;
begin
  if Assigned(parent) then
    parent.MoveChildUp(parent.IndexOfChild(Self));
end;

procedure TGLBaseSceneObject.MoveDown;
begin
  if Assigned(parent) then
    parent.MoveChildDown(parent.IndexOfChild(Self));
end;

procedure TGLBaseSceneObject.MoveFirst;
begin
  if Assigned(parent) then
    parent.MoveChildFirst(parent.IndexOfChild(Self));
end;

procedure TGLBaseSceneObject.MoveLast;
begin
  if Assigned(parent) then
    parent.MoveChildLast(parent.IndexOfChild(Self));
end;

procedure TGLBaseSceneObject.MoveObjectAround(anObject: TGLBaseSceneObject; pitchDelta, turnDelta: Single);
var
  originalT2C, normalT2C, normalCameraRight, newPos: TGLVector;
  pitchNow, dist: Single;
begin
  if Assigned(anObject) then
  begin
    // normalT2C points away from the direction the camera is looking
    originalT2C := VectorSubtract(AbsolutePosition, anObject.AbsolutePosition);
    SetVector(normalT2C, originalT2C);
    dist := VectorLength(normalT2C);
    NormalizeVector(normalT2C);
    // normalRight points to the camera's right
    // the camera is pitching around this axis.
    normalCameraRight := VectorCrossProduct(AbsoluteUp, normalT2C);
    if VectorLength(normalCameraRight) < 0.001 then
      SetVector(normalCameraRight, XVector) // arbitrary vector
    else
      NormalizeVector(normalCameraRight);
    // calculate the current pitch.
    // 0 is looking down and PI is looking up
    pitchNow := ArcCos(VectorDotProduct(AbsoluteUp, normalT2C));
    pitchNow := ClampValue(pitchNow + DegToRad(pitchDelta), 0 + 0.025, PI - 0.025);
    // creates a new vector pointing up and then rotate it down
    // into the new position
    SetVector(normalT2C, AbsoluteUp);
    RotateVector(normalT2C, normalCameraRight, -pitchNow);
    RotateVector(normalT2C, AbsoluteUp, -DegToRadian(turnDelta));
    ScaleVector(normalT2C, dist);
    newPos := VectorAdd(AbsolutePosition, VectorSubtract(normalT2C,
      originalT2C));
    if Assigned(Parent) then
      newPos := Parent.AbsoluteToLocal(newPos);
    Position.AsVector := newPos;
  end;
end;

procedure TGLBaseSceneObject.MoveObjectAllAround(anObject: TGLBaseSceneObject;
  pitchDelta, turnDelta: Single);
var
  upvector: TGLVector;
  lookat : TGLVector;
  rightvector : TGLVector;
  tempvector: TGLVector;
  T2C: TGLVector;

begin

  // if camera has got a target
  if Assigned(anObject) then
  begin
    //vector camera to target
    lookat := VectorNormalize(VectorSubtract(anObject.AbsolutePosition, AbsolutePosition));
    //camera up vector
    upvector := VectorNormalize(AbsoluteUp);

    // if upvector and lookat vector are colinear, it is necessary to compute new up vector
    if Abs(VectorDotProduct(lookat,upvector))>0.99 then
    begin
      //X or Y vector use to generate upvector
      SetVector(tempvector,1,0,0);
      //if lookat is colinear to X vector use Y vector to generate upvector
      if Abs(VectorDotProduct(tempvector,lookat))>0.99 then
      begin
        SetVector(tempvector,0,1,0);
      end;
      upvector:= VectorCrossProduct(tempvector,lookat);
      rightvector := VectorCrossProduct(lookat,upvector);
    end
    else
    begin
      rightvector := VectorCrossProduct(lookat,upvector);
      upvector:= VectorCrossProduct(rightvector,lookat);
    end;
    //now the up right and look at vector are orthogonal
    // vector Target to camera
    T2C:= VectorSubtract(AbsolutePosition,anObject.AbsolutePosition);
    RotateVector(T2C,rightvector,DegToRadian(-PitchDelta));
    RotateVector(T2C,upvector,DegToRadian(-TurnDelta));
    AbsolutePosition := VectorAdd(anObject.AbsolutePosition, T2C);
    //now update new up vector
    RotateVector(upvector,rightvector,DegToRadian(-PitchDelta));
    AbsoluteUp := upvector;
    AbsoluteDirection := VectorSubtract(anObject.AbsolutePosition,AbsolutePosition);
  end;
end;

procedure TGLBaseSceneObject.CoordinateChanged(Sender: TGLCustomCoordinates);
var
  rightVector: TGLVector;
begin
  if FIsCalculating then
    Exit;
  FIsCalculating := True;
  try
    if Sender = FDirection then
    begin
      if FDirection.VectorLength = 0 then
        FDirection.DirectVector := ZHmgVector;
      FDirection.Normalize;
      // adjust up vector
      rightVector := VectorCrossProduct(FDirection.AsVector, FUp.AsVector);
      // Rightvector is zero if direction changed exactly by 90 degrees,
      // in this case assume a default vector
      if VectorLength(rightVector) < 1e-5 then
      begin
        rightVector := VectorCrossProduct(ZHmgVector, FUp.AsVector);
        if VectorLength(rightVector) < 1e-5 then
          rightVector := VectorCrossProduct(XHmgVector, FUp.AsVector);
      end;
      FUp.DirectVector := VectorCrossProduct(rightVector, FDirection.AsVector);
      FUp.Normalize;
    end
    else if Sender = FUp then
    begin
      if FUp.VectorLength = 0 then
        FUp.DirectVector := YHmgVector;
      FUp.Normalize;
      // adjust up vector
      rightVector := VectorCrossProduct(FDirection.AsVector, FUp.AsVector);
      // Rightvector is zero if direction changed exactly by 90 degrees,
      // in this case assume a default vector
      if VectorLength(rightVector) < 1e-5 then
      begin
        rightVector := VectorCrossProduct(ZHmgVector, FUp.AsVector);
        if VectorLength(rightVector) < 1e-5 then
          rightVector := VectorCrossProduct(XHmgVector, FUp.AsVector);
      end;
      FDirection.DirectVector := VectorCrossProduct(FUp.AsVector, RightVector);
      FDirection.Normalize;
    end;
    TransformationChanged;
  finally
    FIsCalculating := False;
  end;
end;

procedure TGLBaseSceneObject.DoProgress(const progressTime: TGLProgressTimes);
var
  i: Integer;
begin
  for i := FChildren.Count - 1 downto 0 do
    TGLBaseSceneObject(FChildren.List^[i]).DoProgress(progressTime);
  if Assigned(FBehaviours) then
    FBehaviours.DoProgress(progressTime);
  if Assigned(FEffects) then
    FEffects.DoProgress(progressTime);
  if Assigned(FOnProgress) then
    with progressTime do
      FOnProgress(Self, deltaTime, newTime);
end;

procedure TGLBaseSceneObject.Insert(aIndex: Integer; aChild: TGLBaseSceneObject);
begin
  with FChildren do
  begin
    if Assigned(aChild.FParent) then
      aChild.FParent.Remove(aChild, False);
    Insert(aIndex, aChild);
  end;
  aChild.FParent := Self;
  if AChild.FScene <> FScene then
    AChild.DestroyHandles;
  AChild.SetScene(FScene);
  if Assigned(FScene) then
    FScene.AddLights(aChild);
  AChild.TransformationChanged;
  aChild.DoOnAddedToParent;
end;

procedure TGLBaseSceneObject.Remove(aChild: TGLBaseSceneObject; keepChildren: Boolean);
var
  I: Integer;
begin
  if not Assigned(FChildren) then
    Exit;
  if aChild.Parent = Self then
  begin
    if Assigned(FScene) then
      FScene.RemoveLights(aChild);
    if aChild.Owner = Self then
      RemoveComponent(aChild);
    FChildren.Remove(aChild);
    aChild.FParent := nil;
    if keepChildren then
    begin
      BeginUpdate;
      if aChild.Count <> 0 then
        for I := aChild.Count - 1 downto 0 do
          if not IsSubComponent(aChild.Children[I]) then
            aChild.Children[I].MoveTo(Self);
      EndUpdate;
    end
    else
      NotifyChange(Self);
  end;
end;

function TGLBaseSceneObject.IndexOfChild(aChild: TGLBaseSceneObject): Integer;
begin
  Result := FChildren.IndexOf(aChild)
end;

function TGLBaseSceneObject.FindChild(const aName: string;
  ownChildrenOnly: Boolean): TGLBaseSceneObject;
var
  i: integer;
  res: TGLBaseSceneObject;
begin
  res := nil;
  Result := nil;
  for i := 0 to FChildren.Count - 1 do
  begin
    if CompareText(TGLBaseSceneObject(FChildren[i]).Name, aName) = 0 then
    begin
      res := TGLBaseSceneObject(FChildren[i]);
      Break;
    end;
  end;
  if not ownChildrenOnly then
  begin
    for i := 0 to FChildren.Count - 1 do
      with TGLBaseSceneObject(FChildren[i]) do
      begin
        Result := FindChild(aName, ownChildrenOnly);
        if Assigned(Result) then
          Break;
      end;
  end;
  if not Assigned(Result) then
    Result := res;
end;

procedure TGLBaseSceneObject.ExchangeChildren(anIndex1, anIndex2: Integer);
begin
  Assert(Assigned(FChildren), 'No children found!');
  FChildren.Exchange(anIndex1, anIndex2);
  NotifyChange(Self);
end;

procedure TGLBaseSceneObject.ExchangeChildrenSafe(anIndex1, anIndex2: Integer);
begin
  Assert(Assigned(FChildren), 'No children found!');
  if (anIndex1 < FChildren.Count) and (anIndex2 < FChildren.Count) and
    (anIndex1 > -1) and (anIndex2 > -1) and (anIndex1 <> anIndex2) then
  begin
    FChildren.Exchange(anIndex1, anIndex2);
    NotifyChange(Self);
  end;
end;

procedure TGLBaseSceneObject.MoveChildUp(anIndex: Integer);
begin
  Assert(Assigned(FChildren), 'No children found!');
  if anIndex > 0 then
  begin
    FChildren.Exchange(anIndex, anIndex - 1);
    NotifyChange(Self);
  end;
end;

procedure TGLBaseSceneObject.MoveChildDown(anIndex: Integer);
begin
  Assert(Assigned(FChildren), 'No children found!');
  if anIndex < FChildren.Count - 1 then
  begin
    FChildren.Exchange(anIndex, anIndex + 1);
    NotifyChange(Self);
  end;
end;

procedure TGLBaseSceneObject.MoveChildFirst(anIndex: Integer);
begin
  Assert(Assigned(FChildren), 'No children found!');
  if anIndex <> 0 then
  begin
    FChildren.Move(anIndex, 0);
    NotifyChange(Self);
  end;
end;

procedure TGLBaseSceneObject.MoveChildLast(anIndex: Integer);
begin
  Assert(Assigned(FChildren), 'No children found!');
  if anIndex <> FChildren.Count - 1 then
  begin
    FChildren.Move(anIndex, FChildren.Count - 1);
    NotifyChange(Self);
  end;
end;

procedure TGLBaseSceneObject.Render(var ARci: TGLRenderContextInfo);
var
  shouldRenderSelf, shouldRenderChildren: Boolean;
  aabb: TAABB;
  master: TObject;
begin
{$IFDEF USE_OPENGL_DEBUG}
  if gl.GREMEDY_string_marker then
    gl.StringMarkerGREMEDY(
      Length(Name) + Length('.Render'), PChar(TString(Name + '.Render')));
{$ENDIF}

  if (ARci.drawState = dsPicking) and not FPickable then
    exit;
  // visibility culling determination
  if ARci.visibilityCulling in [vcObjectBased, vcHierarchical] then
  begin
    if ARci.visibilityCulling = vcObjectBased then
    begin
      shouldRenderSelf := (osNoVisibilityCulling in ObjectStyle)
        or (not IsVolumeClipped(BarycenterAbsolutePosition,
        BoundingSphereRadius,
        ARci.rcci.frustum));
      shouldRenderChildren := FChildren.Count>0;
    end
    else
    begin // vcHierarchical
      aabb := AxisAlignedBoundingBox;
      shouldRenderSelf := (osNoVisibilityCulling in ObjectStyle)
        or (not IsVolumeClipped(aabb.min, aabb.max, ARci.rcci.frustum));
      shouldRenderChildren := shouldRenderSelf and Assigned(FChildren);
    end;
    if not (shouldRenderSelf or shouldRenderChildren) then
      Exit;
  end
  else
  begin
    Assert(ARci.visibilityCulling in [vcNone, vcInherited], 'Unknown visibility culling option');
    shouldRenderSelf := True;
    shouldRenderChildren := FChildren.Count>0;
  end;
  // Prepare Matrix and PickList stuff
  ARci.PipelineTransformation.Push;

  if ocTransformation in FChanges then
    RebuildMatrix;

  if ARci.proxySubObject then
    ARci.PipelineTransformation.SetModelMatrix(
	  MatrixMultiply(LocalMatrix^, ARci.PipelineTransformation.ModelMatrix^))
  else
    ARci.PipelineTransformation.SetModelMatrix(AbsoluteMatrix);
  master := nil;
  if ARci.drawState = dsPicking then
  begin
    if ARci.proxySubObject then
      master := TGLSceneBuffer(ARci.buffer).FSelector.CurrentObject;
    TGLSceneBuffer(ARci.buffer).FSelector.CurrentObject := Self;
  end;
  // Start rendering
  if shouldRenderSelf then
  begin
    vCurrentRenderingObject := Self;
{$IFNDEF USE_OPTIMIZATIONS}
    if FShowAxes then
      DrawAxes(ARci, $CCCC);
{$ENDIF}
    if Assigned(FEffects) and (FEffects.Count > 0) then
    begin
      ARci.PipelineTransformation.Push;
      FEffects.RenderPreEffects(ARci);
      ARci.PipelineTransformation.Pop;

      ARci.PipelineTransformation.Push;
      if osIgnoreDepthBuffer in ObjectStyle then
      begin
        ARci.GLStates.Disable(stDepthTest);
        DoRender(ARci, True, shouldRenderChildren);
        ARci.GLStates.Enable(stDepthTest);
      end
      else
        DoRender(ARci, True, shouldRenderChildren);

      FEffects.RenderPostEffects(ARci);
      ARci.PipelineTransformation.Pop;
    end
    else
    begin
      if osIgnoreDepthBuffer in ObjectStyle then
      begin
        ARci.GLStates.Disable(stDepthTest);
        DoRender(ARci, True, shouldRenderChildren);
        ARci.GLStates.Enable(stDepthTest);
      end
      else
        DoRender(ARci, True, shouldRenderChildren);
    end;
    vCurrentRenderingObject := nil;
  end
  else
  begin
    if (osIgnoreDepthBuffer in ObjectStyle) and
      TGLSceneBuffer(ARCi.buffer).DepthTest then
    begin
      ARci.GLStates.Disable(stDepthTest);
      DoRender(ARci, False, shouldRenderChildren);
      ARci.GLStates.Enable(stDepthTest);
    end
    else
      DoRender(ARci, False, shouldRenderChildren);
  end;
  // Pop Name & Matrix
  if Assigned(master) then
    TGLSceneBuffer(ARci.buffer).FSelector.CurrentObject := master;
  ARci.PipelineTransformation.Pop;
end;

procedure TGLBaseSceneObject.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
begin
  // start rendering self
  if ARenderSelf then
  begin
    if (osDirectDraw in ObjectStyle) or ARci.amalgamating then
      BuildList(ARci)
    else
      ARci.GLStates.CallList(GetHandle(ARci));
  end;
  // start rendering children (if any)
  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

procedure TGLBaseSceneObject.RenderChildren(firstChildIndex, lastChildIndex:
  Integer;
  var rci: TGLRenderContextInfo);
var
  i: Integer;

  plist: PGLPointerObjectList;
  obj: TGLBaseSceneObject;
  oldSorting: TGLObjectsSorting;
  oldCulling: TGLVisibilityCulling;
begin
  oldCulling := rci.visibilityCulling;
  if Self.VisibilityCulling <> vcInherited then
    rci.visibilityCulling := Self.VisibilityCulling;
  if lastChildIndex = firstChildIndex then
  begin
    obj := TGLBaseSceneObject(FChildren.List^[firstChildIndex]);
    if obj.Visible then
      obj.Render(rci)
  end
  else if lastChildIndex > firstChildIndex then
  begin
    oldSorting := rci.objectsSorting;
    if Self.ObjectsSorting <> osInherited then
      rci.objectsSorting := Self.ObjectsSorting;
    case rci.objectsSorting of
      osNone:
        begin
          plist := FChildren.List;
          for i := firstChildIndex to lastChildIndex do
          begin
            obj := TGLBaseSceneObject(plist^[i]);
            if obj.Visible then
              obj.Render(rci);
          end;
        end;
      osRenderFarthestFirst, osRenderBlendedLast, osRenderNearestFirst:
        begin
          distList.Flush;
          objList.Count := 0;
          distList.GrowthDelta := lastChildIndex + 1; // no reallocations
          objList.GrowthDelta := distList.GrowthDelta;
          //try
            case rci.objectsSorting of
              osRenderBlendedLast:
                // render opaque stuff
                for i := firstChildIndex to lastChildIndex do
                begin
                  obj := TGLBaseSceneObject(FChildren.List^[i]);
                  if obj.Visible then
                  begin
                    if not obj.Blended then
                      obj.Render(rci)
                    else
                    begin
                      objList.Add(obj);
                      distList.Add(1 +
                        obj.BarycenterSqrDistanceTo(rci.cameraPosition));
                    end;
                  end;
                end;
              osRenderFarthestFirst:
                for i := firstChildIndex to lastChildIndex do
                begin
                  obj := TGLBaseSceneObject(FChildren.List^[i]);
                  if obj.Visible then
                  begin
                    objList.Add(obj);
                    distList.Add(1 +
                      obj.BarycenterSqrDistanceTo(rci.cameraPosition));
                  end;
                end;
              osRenderNearestFirst:
                for i := firstChildIndex to lastChildIndex do
                begin
                  obj := TGLBaseSceneObject(FChildren.List^[i]);
                  if obj.Visible then
                  begin
                    objList.Add(obj);
                    distList.Add(-1 -
                      obj.BarycenterSqrDistanceTo(rci.cameraPosition));
                  end;
                end;
            else
              Assert(False);
            end;
            if distList.Count > 0 then
            begin
              if distList.Count > 1 then
                FastQuickSortLists(0, distList.Count - 1, distList, objList);
              plist := objList.List;
              for i := objList.Count - 1 downto 0 do
                TGLBaseSceneObject(plist^[i]).Render(rci);
            end;
          //finally
          //end;
        end;
    else
      Assert(False);
    end;
    rci.objectsSorting := oldSorting;
  end;
  rci.visibilityCulling := oldCulling;
end;

procedure TGLBaseSceneObject.NotifyChange(Sender: TObject);
begin
  if Assigned(FScene) and (not IsUpdating) then
    FScene.NotifyChange(Self);
end;

function TGLBaseSceneObject.GetMatrix: PGLMatrix;
begin
  RebuildMatrix;
  Result := @FLocalMatrix;
end;

procedure TGLBaseSceneObject.SetMatrix(const aValue: TGLMatrix);
begin
  FLocalMatrix := aValue;
  FDirection.DirectVector := VectorNormalize(FLocalMatrix.Z);
  FUp.DirectVector := VectorNormalize(FLocalMatrix.Y);
  Scale.SetVector(VectorLength(FLocalMatrix.X),
    VectorLength(FLocalMatrix.Y),
    VectorLength(FLocalMatrix.Z), 0);
  FPosition.DirectVector := FLocalMatrix.W;
  TransformationChanged;
end;

procedure TGLBaseSceneObject.SetPosition(APosition: TGLCoordinates);
begin
  FPosition.SetPoint(APosition.DirectX, APosition.DirectY, APosition.DirectZ);
end;

procedure TGLBaseSceneObject.SetDirection(AVector: TGLCoordinates);
begin
  if not VectorIsNull(AVector.DirectVector) then
    FDirection.SetVector(AVector.DirectX, AVector.DirectY, AVector.DirectZ);
end;

procedure TGLBaseSceneObject.SetUp(AVector: TGLCoordinates);
begin
  if not VectorIsNull(AVector.DirectVector) then
    FUp.SetVector(AVector.DirectX, AVector.DirectY, AVector.DirectZ);
end;

function TGLBaseSceneObject.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TGLBaseSceneObject.GetPickable: Boolean;
begin
  Result := FPickable;
end;

procedure TGLBaseSceneObject.SetVisible(aValue: Boolean);
begin
  if FVisible <> aValue then
  begin
    FVisible := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLBaseSceneObject.SetPickable(aValue: Boolean);
begin
  if FPickable <> aValue then
  begin
    FPickable := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLBaseSceneObject.SetObjectsSorting(const val: TGLObjectsSorting);
begin
  if FObjectsSorting <> val then
  begin
    FObjectsSorting := val;
    NotifyChange(Self);
  end;
end;

procedure TGLBaseSceneObject.SetVisibilityCulling(const val:
  TGLVisibilityCulling);
begin
  if FVisibilityCulling <> val then
  begin
    FVisibilityCulling := val;
    NotifyChange(Self);
  end;
end;

procedure TGLBaseSceneObject.SetBehaviours(const val: TGLBehaviours);
begin
  Behaviours.Assign(val);
end;

function TGLBaseSceneObject.GetBehaviours: TGLBehaviours;
begin
  if not Assigned(FBehaviours) then
    FBehaviours := TGLBehaviours.Create(Self);
  Result := FBehaviours;
end;

procedure TGLBaseSceneObject.SetEffects(const val: TGLEffects);
begin
  Effects.Assign(val);
end;

function TGLBaseSceneObject.GetEffects: TGLEffects;
begin
  if not Assigned(FEffects) then
    FEffects := TGLEffects.Create(Self);
  Result := FEffects;
end;

procedure TGLBaseSceneObject.SetScene(const value: TGLScene);
var
  i: Integer;
begin
  if value <> FScene then
  begin
    // must be freed, the new scene may be using a non-compatible RC
    if FScene <> nil then
      DestroyHandles;
    FScene := value;
    // propagate for childs
    if Assigned(FChildren) then
      for i := 0 to FChildren.Count - 1 do
        Children[I].SetScene(FScene);
  end;
end;

procedure TGLBaseSceneObject.Translate(tx, ty, tz: Single);
begin
  FPosition.Translate(AffineVectorMake(tx, ty, tz));
end;

function TGLBaseSceneObject.GetAbsoluteAffinePosition: TAffineVector;
var
  temp: TGLVector;
begin
  temp := GetAbsolutePosition;
  Result := AffineVectorMake(temp.X, temp.Y, temp.Z);
end;

function TGLBaseSceneObject.GetAbsoluteAffineDirection: TAffineVector;
var
  temp: TGLVector;
begin
  temp := GetAbsoluteDirection;
  Result := AffineVectorMake(temp.X, temp.Y, temp.Z);
end;

function TGLBaseSceneObject.GetAbsoluteAffineUp: TAffineVector;
var
  temp: TGLVector;
begin
  temp := GetAbsoluteUp;
  Result := AffineVectorMake(temp.X, temp.Y, temp.Z);
end;

procedure TGLBaseSceneObject.SetAbsoluteAffinePosition(const Value:
  TAffineVector);
begin
  SetAbsolutePosition(VectorMake(Value, 1));
end;

procedure TGLBaseSceneObject.SetAbsoluteAffineUp(const v: TAffineVector);
begin
  SetAbsoluteUp(VectorMake(v, 1));
end;

procedure TGLBaseSceneObject.SetAbsoluteAffineDirection(const v: TAffineVector);
begin
  SetAbsoluteDirection(VectorMake(v, 1));
end;

function TGLBaseSceneObject.AffineLeftVector: TAffineVector;
begin
  Result := AffineVectorMake(LeftVector);
end;

function TGLBaseSceneObject.AffineRight: TAffineVector;
begin
  Result := AffineVectorMake(Right);
end;

function TGLBaseSceneObject.DistanceTo(const pt: TAffineVector): Single;
begin
  Result := VectorDistance(AbsoluteAffinePosition, pt);
end;

function TGLBaseSceneObject.SqrDistanceTo(const pt: TAffineVector): Single;
begin
  Result := VectorDistance2(AbsoluteAffinePosition, pt);
end;

procedure TGLBaseSceneObject.DoOnAddedToParent;
begin
  if Assigned(FOnAddedToParent) then
    FOnAddedToParent(self);
end;

function TGLBaseSceneObject.GetAbsoluteAffineScale: TAffineVector;
begin
  Result := AffineVectorMake(GetAbsoluteScale);
end;

procedure TGLBaseSceneObject.SetAbsoluteAffineScale(
  const Value: TAffineVector);
begin
  SetAbsoluteScale(VectorMake(Value, GetAbsoluteScale.W));
end;

// ------------------
// ------------------ TGLBaseBehaviour ------------------
// ------------------
constructor TGLBaseBehaviour.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  // nothing more, yet
end;

destructor TGLBaseBehaviour.Destroy;
begin
  // nothing more, yet
  inherited Destroy;
end;

procedure TGLBaseBehaviour.SetName(const val: string);
begin
  inherited SetName(val);
  if Assigned(vGLBehaviourNameChangeEvent) then
    vGLBehaviourNameChangeEvent(Self);
end;

procedure TGLBaseBehaviour.WriteToFiler(writer: TWriter);
begin
  inherited;

  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    // nothing more, yet
  end;
end;

procedure TGLBaseBehaviour.ReadFromFiler(reader: TReader);
begin
  if Owner.ArchiveVersion > 0 then
    inherited;

  with reader do
  begin
    if ReadInteger <> 0 then
      Assert(False);
    // nothing more, yet
  end;
end;

function TGLBaseBehaviour.OwnerBaseSceneObject: TGLBaseSceneObject;
begin
  Result := TGLBaseSceneObject(Owner.Owner);
end;

procedure TGLBaseBehaviour.DoProgress(const progressTime: TGLProgressTimes);
begin
  // does nothing
end;

// ------------------
// ------------------ TGLBehaviours ------------------
// ------------------
constructor TGLBehaviours.Create(aOwner: TPersistent);
begin
  Assert(aOwner is TGLBaseSceneObject);
  inherited Create(aOwner);
end;

function TGLBehaviours.GetNamePath: string;
var
  s: string;
begin
  Result := ClassName;
  if GetOwner = nil then
    Exit;
  s := GetOwner.GetNamePath;
  if s = '' then
    Exit;
  Result := s + '.Behaviours';
end;

class function TGLBehaviours.ItemsClass: TXCollectionItemClass;
begin
  Result := TGLBehaviour;
end;

function TGLBehaviours.GetBehaviour(index: Integer): TGLBehaviour;
begin
  Result := TGLBehaviour(Items[index]);
end;

function TGLBehaviours.CanAdd(aClass: TXCollectionItemClass): Boolean;
begin
  Result := (not aClass.InheritsFrom(TGLEffect)) and (inherited
    CanAdd(aClass));
end;

procedure TGLBehaviours.DoProgress(const progressTimes: TGLProgressTimes);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TGLBehaviour(Items[i]).DoProgress(progressTimes);
end;

// ------------------
// ------------------ TGLEffect ------------------
// ------------------
procedure TGLEffect.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    // nothing more, yet
  end;
end;

procedure TGLEffect.ReadFromFiler(reader: TReader);
begin
  if Owner.ArchiveVersion > 0 then
    inherited;

  with reader do
  begin
    if ReadInteger <> 0 then
      Assert(False);
    // nothing more, yet
  end;
end;

procedure TGLEffect.Render(var rci: TGLRenderContextInfo);
begin
  // nothing here, this implem is just to avoid "abstract error"
end;

// ------------------
// ------------------ TGLEffects ------------------
// ------------------
constructor TGLEffects.Create(aOwner: TPersistent);
begin
  Assert(aOwner is TGLBaseSceneObject);
  inherited Create(aOwner);
end;

function TGLEffects.GetNamePath: string;
var
  s: string;
begin
  Result := ClassName;
  if GetOwner = nil then
    Exit;
  s := GetOwner.GetNamePath;
  if s = '' then
    Exit;
  Result := s + '.Effects';
end;

class function TGLEffects.ItemsClass: TXCollectionItemClass;
begin
  Result := TGLEffect;
end;

function TGLEffects.GetEffect(index: Integer): TGLEffect;
begin
  Result := TGLEffect(Items[index]);
end;

function TGLEffects.CanAdd(aClass: TXCollectionItemClass): Boolean;
begin
  Result := (aClass.InheritsFrom(TGLEffect)) and (inherited
    CanAdd(aClass));
end;

procedure TGLEffects.DoProgress(const progressTime: TGLProgressTimes);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TGLEffect(Items[i]).DoProgress(progressTime);
end;

procedure TGLEffects.RenderPreEffects(var rci: TGLRenderContextInfo);
var
  i: Integer;
  effect: TGLEffect;
begin
  for i := 0 to Count - 1 do
  begin
    effect := TGLEffect(Items[i]);
    if effect is TGLObjectPreEffect then
      effect.Render(rci);
  end;
end;

procedure TGLEffects.RenderPostEffects(var rci: TGLRenderContextInfo);
var
  i: Integer;
  effect: TGLEffect;
begin
  for i := 0 to Count - 1 do
  begin
    effect := TGLEffect(Items[i]);
    if effect is TGLObjectPostEffect then
      effect.Render(rci)
    else if Assigned(rci.afterRenderEffects) and (effect is TGLObjectAfterEffect) then
      rci.afterRenderEffects.Add(effect);
  end;
end;

// ------------------
// ------------------ TGLCustomSceneObject ------------------
// ------------------
constructor TGLCustomSceneObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaterial := TGLMaterial.Create(Self);
end;

destructor TGLCustomSceneObject.Destroy;
begin
  inherited Destroy;
  FMaterial.Free;
end;

procedure TGLCustomSceneObject.Assign(Source: TPersistent);
begin
  if Source is TGLCustomSceneObject then
  begin
    FMaterial.Assign(TGLCustomSceneObject(Source).FMaterial);
    FHint := TGLCustomSceneObject(Source).FHint;
  end;
  inherited Assign(Source);
end;

function TGLCustomSceneObject.Blended: Boolean;
begin
  Result := Material.Blended;
end;

procedure TGLCustomSceneObject.Loaded;
begin
  inherited;
  FMaterial.Loaded;
end;

procedure TGLCustomSceneObject.SetGLMaterial(AValue: TGLMaterial);
begin
  FMaterial.Assign(AValue);
  NotifyChange(Self);
end;

procedure TGLCustomSceneObject.DestroyHandle;
begin
  inherited;
  FMaterial.DestroyHandles;
end;

procedure TGLCustomSceneObject.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
begin
  // start rendering self
  if ARenderSelf then
    if ARci.ignoreMaterials then
      if (osDirectDraw in ObjectStyle) or ARci.amalgamating then
        BuildList(ARci)
      else
        ARci.GLStates.CallList(GetHandle(ARci))
    else
    begin
      FMaterial.Apply(ARci);
      repeat
        if (osDirectDraw in ObjectStyle) or ARci.amalgamating then
          BuildList(ARci)
        else
          ARci.GLStates.CallList(GetHandle(ARci));
      until not FMaterial.UnApply(ARci);
    end;
  // start rendering children (if any)
  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

// ------------------
// ------------------ TGLSceneRootObject ------------------
// ------------------
constructor TGLSceneRootObject.Create(AOwner: TComponent);
begin
  Assert(AOwner is TGLScene);
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FScene := TGLScene(AOwner);
end;

// ------------------
// ------------------ TGLCamera ------------------
// ------------------
constructor TGLCamera.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FFocalLength := 50;
  FDepthOfView := 100;
  FNearPlaneBias := 1;
  FDirection.Initialize(VectorMake(0, 0, -1, 0));
  FCameraStyle := csPerspective;
  FSceneScale := 1;
  FDesign := False;
  FFOVY := -1;
  FKeepFOVMode := ckmHorizontalFOV;
end;

destructor TGLCamera.Destroy;
begin
  TargetObject := nil;
  inherited;
end;

procedure TGLCamera.Assign(Source: TPersistent);
var
  cam: TGLCamera;
  dir: TGLVector;
begin
  if Assigned(Source) then
  begin
    inherited Assign(Source);

    if Source is TGLCamera then
    begin
      cam := TGLCamera(Source);
      SetDepthOfView(cam.DepthOfView);
      SetFocalLength(cam.FocalLength);
      SetCameraStyle(cam.CameraStyle);
      SetSceneScale(cam.SceneScale);
      SetNearPlaneBias(cam.NearPlaneBias);
      SetScene(cam.Scene);
      SetKeepFOVMode(cam.FKeepFOVMode);

      if Parent <> nil then
      begin
        SetTargetObject(cam.TargetObject);
      end
      else // Design camera
      begin
        Position.AsVector := cam.AbsolutePosition;
        if Assigned(cam.TargetObject) then
        begin
          VectorSubtract(cam.TargetObject.AbsolutePosition, AbsolutePosition, dir);
          NormalizeVector(dir);
          Direction.AsVector := dir;
        end;
      end;
    end;
  end;
end;

function TGLCamera.AbsoluteVectorToTarget: TGLVector;
begin
  if TargetObject <> nil then
  begin
    VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition, Result);
    NormalizeVector(Result);
  end
  else
    Result := AbsoluteDirection;
end;

function TGLCamera.AbsoluteRightVectorToTarget: TGLVector;
begin
  if TargetObject <> nil then
  begin
    VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition, Result);
    Result := VectorCrossProduct(Result, AbsoluteUp);
    NormalizeVector(Result);
  end
  else
    Result := AbsoluteRight;
end;

function TGLCamera.AbsoluteUpVectorToTarget: TGLVector;
begin
  if TargetObject <> nil then
    Result := VectorCrossProduct(AbsoluteRightVectorToTarget,
      AbsoluteVectorToTarget)
  else
    Result := AbsoluteUp;
end;

procedure TGLCamera.Apply;
var
  v, d, v2: TGLVector;
  absPos: TGLVector;
  LM, mat: TGLMatrix;
begin
  if Assigned(FDeferredApply) then
    FDeferredApply(Self)
  else
  begin
    if Assigned(FTargetObject) then
    begin
      v := TargetObject.AbsolutePosition;
      absPos := AbsolutePosition;
      VectorSubtract(v, absPos, d);
      NormalizeVector(d);
      FLastDirection := d;
      LM := CreateLookAtMatrix(absPos, v, Up.AsVector);
    end
    else
    begin
      if Assigned(Parent) then
        mat := Parent.AbsoluteMatrix
      else
        mat := IdentityHmgMatrix;
      absPos := AbsolutePosition;
      v := VectorTransform(Direction.AsVector, mat);
      FLastDirection := v;
      d := VectorTransform(Up.AsVector, mat);
      v2 := VectorAdd(absPos, v);
      LM := CreateLookAtMatrix(absPos, v2, d);
    end;
    with CurrentGLContext.PipelineTransformation do
      SetViewMatrix(MatrixMultiply(LM, ViewMatrix^));
    ClearStructureChanged;
  end;
end;

procedure TGLCamera.ApplyPerspective(const AViewport: TRectangle;
  AWidth, AHeight: Integer; ADPI: Integer);
var
  vLeft, vRight, vBottom, vTop, vFar: Single;
  MaxDim, Ratio, f: Double;
  xmax, ymax: Double;
  mat: TGLMatrix;
const
  cEpsilon: Single = 1e-4;

  function IsPerspective(CamStyle: TGLCameraStyle): Boolean;
  begin
    Result := CamStyle in [csPerspective, csInfinitePerspective, csPerspectiveKeepFOV];
  end;

begin
  if (AWidth <= 0) or (AHeight <= 0) then
    Exit;

  if CameraStyle = csOrtho2D then
  begin
    vLeft := 0;
    vRight := AWidth;
    vBottom := 0;
    vTop := AHeight;
    FNearPlane := -1;
    vFar := 1;
    mat := CreateOrthoMatrix(vLeft, vRight, vBottom, vTop, FNearPlane, vFar);
    with CurrentGLContext.PipelineTransformation do
      SetProjectionMatrix(MatrixMultiply(mat, ProjectionMatrix^));
    FViewPortRadius := VectorLength(AWidth, AHeight) / 2;
  end
  else if CameraStyle = csCustom then
  begin
    FViewPortRadius := VectorLength(AWidth, AHeight) / 2;
    if Assigned(FOnCustomPerspective) then
      FOnCustomPerspective(AViewport, AWidth, AHeight, ADPI, FViewPortRadius);
  end
  else
  begin
    // determine biggest dimension and resolution (height or width)
    MaxDim := AWidth;
    if AHeight > MaxDim then
      MaxDim := AHeight;

    // calculate near plane distance and extensions;
    // Scene ratio is determined by the window ratio. The viewport is just a
    // specific part of the entire window and has therefore no influence on the
    // scene ratio. What we need to know, though, is the ratio between the window
    // borders (left, top, right and bottom) and the viewport borders.
    // Note: viewport.top is actually bottom, because the window (and viewport) origin
    // in OGL is the lower left corner
    if IsPerspective(CameraStyle) then
      f := FNearPlaneBias / (AWidth * FSceneScale)
    else
      f := 100 * FNearPlaneBias / (focalLength * AWidth * FSceneScale);
    // calculate window/viewport ratio for right extent
    Ratio := (2 * AViewport.Width + 2 * AViewport.Left - AWidth) * f;
    // calculate aspect ratio correct right value of the view frustum and take
    // the window/viewport ratio also into account
    vRight := Ratio * AWidth / (2 * MaxDim);
    // the same goes here for the other three extents
    // left extent:
    Ratio := (AWidth - 2 * AViewport.Left) * f;
    vLeft := -Ratio * AWidth / (2 * MaxDim);
    if IsPerspective(CameraStyle) then
      f := FNearPlaneBias / (AHeight * FSceneScale)
    else
      f := 100 * FNearPlaneBias / (focalLength * AHeight * FSceneScale);
    // top extent (keep in mind the origin is left lower corner):
    Ratio := (2 * AViewport.Height + 2 * AViewport.Top - AHeight) * f;
    vTop := Ratio * AHeight / (2 * MaxDim);
    // bottom extent:
    Ratio := (AHeight - 2 * AViewport.Top) * f;
    vBottom := -Ratio * AHeight / (2 * MaxDim);
    FNearPlane := FFocalLength * 2 * ADPI / (25.4 * MaxDim) * FNearPlaneBias;
    vFar := FNearPlane + FDepthOfView;
    // finally create view frustum (perspective or orthogonal)
    case CameraStyle of
      csPerspective:
        begin
          mat := CreateMatrixFromFrustum(vLeft, vRight, vBottom, vTop, FNearPlane, vFar);
        end;
      csPerspectiveKeepFOV:
        begin
          if FFOVY < 0 then // Need Update FOV
          begin
            FFOVY := ArcTan2(vTop - vBottom, 2 * FNearPlane) * 2;
            FFOVX := ArcTan2(vRight - vLeft, 2 * FNearPlane) * 2;
          end;

          case FKeepFOVMode of
            ckmVerticalFOV:
            begin
              ymax := FNearPlane * Tan(FFOVY / 2);
              xmax := ymax * AWidth / AHeight;
            end;
            ckmHorizontalFOV:
            begin
              xmax := FNearPlane * Tan(FFOVX / 2);
              ymax := xmax * AHeight / AWidth;
            end;
            else
            begin
              xmax := 0;
              ymax := 0;
              Assert(False, 'Unknown keep camera angle mode');
            end;
          end;
          mat := CreateMatrixFromFrustum(-xmax, xmax, -ymax, ymax, FNearPlane, vFar);
        end;
      csInfinitePerspective:
        begin
          mat := IdentityHmgMatrix;
          mat.X.X := 2 * FNearPlane / (vRight - vLeft);
          mat.Y.Y := 2 * FNearPlane / (vTop - vBottom);
          mat.Z.X := (vRight + vLeft) / (vRight - vLeft);
          mat.Z.Y := (vTop + vBottom) / (vTop - vBottom);
          mat.Z.Z := cEpsilon - 1;
          mat.Z.W := -1;
          mat.W.Z := FNearPlane * (cEpsilon - 2);
          mat.W.W := 0;
        end;
      csOrthogonal:
        begin
          mat := CreateOrthoMatrix(vLeft, vRight, vBottom, vTop, FNearPlane, vFar);
        end;
    else
      Assert(False);
    end;
    with CurrentGLContext.PipelineTransformation do
      SetProjectionMatrix(MatrixMultiply(mat, ProjectionMatrix^));
      FViewPortRadius := VectorLength(vRight, vTop) / FNearPlane
  end;
end;

//------------------------------------------------------------------------------

procedure TGLCamera.AutoLeveling(Factor: Single);
var
  rightVector, rotAxis: TGLVector;
  angle: Single;
begin
  angle := RadToDeg(ArcCos(VectorDotProduct(FUp.AsVector, YVector)));
  rotAxis := VectorCrossProduct(YHmgVector, FUp.AsVector);
  if (angle > 1) and (VectorLength(rotAxis) > 0) then
  begin
    rightVector := VectorCrossProduct(FDirection.AsVector, FUp.AsVector);
    FUp.Rotate(AffineVectorMake(rotAxis), Angle / (10 * Factor));
    FUp.Normalize;
    // adjust local coordinates
    FDirection.DirectVector := VectorCrossProduct(FUp.AsVector, rightVector);
    FRotation.Z := -RadToDeg(ArcTan2(RightVector.Y,
      VectorLength(RightVector.X, RightVector.Z)));
  end;
end;

//------------------------------------------------------------------------------

procedure TGLCamera.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FTargetObject) then
    TargetObject := nil;
  inherited;
end;


procedure TGLCamera.SetTargetObject(const val: TGLBaseSceneObject);
begin
  if (FTargetObject <> val) then
  begin
    if Assigned(FTargetObject) then
      FTargetObject.RemoveFreeNotification(Self);
    FTargetObject := val;
    if Assigned(FTargetObject) then
      FTargetObject.FreeNotification(Self);
    if not (csLoading in ComponentState) then
      TransformationChanged;
  end;
end;

procedure TGLCamera.Reset(aSceneBuffer: TGLSceneBuffer);
var
  Extent: Single;
begin
  FRotation.Z := 0;
  FFocalLength := 50;
  with aSceneBuffer do
  begin
    ApplyPerspective(FViewport, FViewport.Width, FViewport.Height, FRenderDPI);
    FUp.DirectVector := YHmgVector;
    if FViewport.Height < FViewport.Width then
      Extent := FViewport.Height * 0.25
    else
      Extent := FViewport.Width * 0.25;
  end;
  FPosition.SetPoint(0, 0, FNearPlane * Extent);
  FDirection.SetVector(0, 0, -1, 0);
  TransformationChanged;
end;

procedure TGLCamera.ZoomAll(aSceneBuffer: TGLSceneBuffer);
var
  extent: Single;
begin
  with aSceneBuffer do
  begin
    if FViewport.Height < FViewport.Width then
      Extent := FViewport.Height * 0.25
    else
      Extent := FViewport.Width * 0.25;
    FPosition.DirectVector := NullHmgPoint;
    Move(-FNearPlane * Extent);
    // let the camera look at the scene center
    FDirection.SetVector(-FPosition.X, -FPosition.Y, -FPosition.Z, 0);
  end;
end;

procedure TGLCamera.RotateObject(obj: TGLBaseSceneObject; pitchDelta, turnDelta: Single;
  rollDelta: Single = 0);
var
  resMat: TGLMatrix;
  vDir, vUp, vRight: TGLVector;
  v: TAffineVector;
  position1: TGLVector;
  Scale1: TGLVector;
begin
  // First we need to compute the actual camera's vectors, which may not be
  // directly available if we're in "targeting" mode
  vUp := AbsoluteUp;
  if TargetObject <> nil then
  begin
    vDir := AbsoluteVectorToTarget;
    vRight := VectorCrossProduct(vDir, vUp);
    vUp := VectorCrossProduct(vRight, vDir);
  end
  else
  begin
    vDir := AbsoluteDirection;
    vRight := VectorCrossProduct(vDir, vUp);
  end;
  //save scale & position info
  Scale1 := obj.Scale.AsVector;
  position1 := obj.Position.asVector;
  resMat := obj.Matrix^;
  //get rid of scaling & location info
  NormalizeMatrix(resMat);
  // Now we build rotation matrices and use them to rotate the obj
  if rollDelta <> 0 then
  begin
    SetVector(v, obj.AbsoluteToLocal(vDir));
    resMat := MatrixMultiply(CreateRotationMatrix(v, DegToRadian(rollDelta)), resMat);
  end;
  if turnDelta <> 0 then
  begin
    SetVector(v, obj.AbsoluteToLocal(vUp));
    resMat := MatrixMultiply(CreateRotationMatrix(v, DegToRadian(turnDelta)), resMat);
  end;
  if pitchDelta <> 0 then
  begin
    SetVector(v, obj.AbsoluteToLocal(vRight));
    resMat := MatrixMultiply(CreateRotationMatrix(v, DegToRadian(pitchDelta)), resMat);
  end;
  obj.SetMatrix(resMat);
  //restore scaling & rotation info
  obj.Scale.AsVector := Scale1;
  obj.Position.AsVector := Position1;
end;

procedure TGLCamera.RotateTarget(pitchDelta, turnDelta: Single; rollDelta: Single = 0);
begin
  if Assigned(FTargetObject) then
    RotateObject(FTargetObject, pitchDelta, turnDelta, rollDelta)
end;

procedure TGLCamera.MoveAroundTarget(pitchDelta, turnDelta: Single);
begin
  MoveObjectAround(FTargetObject, pitchDelta, turnDelta);
end;

procedure TGLCamera.MoveAllAroundTarget(pitchDelta, turnDelta :Single);
begin
  MoveObjectAllAround(FTargetObject, pitchDelta, turnDelta);
end;

procedure TGLCamera.MoveInEyeSpace(forwardDistance, rightDistance, upDistance: Single);
var
  trVector: TGLVector;
begin
  trVector := AbsoluteEyeSpaceVector(forwardDistance, rightDistance, upDistance);
  if Assigned(Parent) then
    Position.Translate(Parent.AbsoluteToLocal(trVector))
  else
    Position.Translate(trVector);
end;

procedure TGLCamera.MoveTargetInEyeSpace(forwardDistance, rightDistance, upDistance: Single);
var
  trVector: TGLVector;
begin
  if TargetObject <> nil then
  begin
    trVector := AbsoluteEyeSpaceVector(forwardDistance, rightDistance,
      upDistance);
    TargetObject.Position.Translate(TargetObject.Parent.AbsoluteToLocal(trVector));
  end;
end;

function TGLCamera.AbsoluteEyeSpaceVector(forwardDistance, rightDistance, upDistance: Single): TGLVector;
begin
  Result := NullHmgVector;
  if forwardDistance <> 0 then
    CombineVector(Result, AbsoluteVectorToTarget, forwardDistance);
  if rightDistance <> 0 then
    CombineVector(Result, AbsoluteRightVectorToTarget, rightDistance);
  if upDistance <> 0 then
    CombineVector(Result, AbsoluteUpVectorToTarget, upDistance);
end;

procedure TGLCamera.AdjustDistanceToTarget(distanceRatio: Single);
var
  vect: TGLVector;
begin
  if Assigned(FTargetObject) then
  begin
    // calculate vector from target to camera in absolute coordinates
    vect := VectorSubtract(AbsolutePosition, TargetObject.AbsolutePosition);
    // ratio -> translation vector
    ScaleVector(vect, -(1 - distanceRatio));
    AddVector(vect, AbsolutePosition);
    if Assigned(Parent) then
      vect := Parent.AbsoluteToLocal(vect);
    Position.AsVector := vect;
  end;
end;

function TGLCamera.DistanceToTarget: Single;
var
  vect: TGLVector;
begin
  if Assigned(FTargetObject) then
  begin
    vect := VectorSubtract(AbsolutePosition, TargetObject.AbsolutePosition);
    Result := VectorLength(vect);
  end
  else
    Result := 1;
end;

function TGLCamera.ScreenDeltaToVector(deltaX, deltaY: Integer; ratio: Single;
  const planeNormal: TGLVector): TGLVector;
var
  screenY, screenX: TGLVector;
  screenYoutOfPlaneComponent: Single;
begin
  // calculate projection of direction vector on the plane
  if Assigned(FTargetObject) then
    screenY := VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition)
  else
    screenY := Direction.AsVector;
  screenYoutOfPlaneComponent := VectorDotProduct(screenY, planeNormal);
  screenY := VectorCombine(screenY, planeNormal, 1, -screenYoutOfPlaneComponent);
  NormalizeVector(screenY);
  // calc the screenX vector
  screenX := VectorCrossProduct(screenY, planeNormal);
  // and here, we're done
  Result := VectorCombine(screenX, screenY, deltaX * ratio, deltaY * ratio);
end;

function TGLCamera.ScreenDeltaToVectorXY(deltaX, deltaY: Integer; ratio: Single): TGLVector;
var
  screenY: TGLVector;
  dxr, dyr, d: Single;
begin
  // calculate projection of direction vector on the plane
  if Assigned(FTargetObject) then
    screenY := VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition)
  else
    screenY := Direction.AsVector;
  d := VectorLength(screenY.X, screenY.Y);
  if d <= 1e-10 then
    d := ratio
  else
    d := ratio / d;
  // and here, we're done
  dxr := deltaX * d;
  dyr := deltaY * d;
  Result.X := screenY.Y * dxr + screenY.X * dyr;
  Result.Y := screenY.Y * dyr - screenY.X * dxr;
  Result.Z := 0;
  Result.W := 0;
end;

function TGLCamera.ScreenDeltaToVectorXZ(deltaX, deltaY: Integer; ratio: Single): TGLVector;
var
  screenY: TGLVector;
  d, dxr, dzr: Single;
begin
  // calculate the projection of direction vector on the plane
  if Assigned(fTargetObject) then
    screenY := VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition)
  else
    screenY := Direction.AsVector;
  d := VectorLength(screenY.X, screenY.Z);
  if d <= 1e-10 then
    d := ratio
  else
    d := ratio / d;
  dxr := deltaX * d;
  dzr := deltaY * d;
  Result.X := -screenY.Z * dxr + screenY.X * dzr;
  Result.Y := 0;
  Result.Z := screenY.Z * dzr + screenY.X * dxr;
  Result.W := 0;
end;

function TGLCamera.ScreenDeltaToVectorYZ(deltaX, deltaY: Integer; ratio: Single): TGLVector;
var
  screenY: TGLVector;
  d, dyr, dzr: single;
begin
  // calculate the projection of direction vector on the plane
  if Assigned(fTargetObject) then
    screenY := VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition)
  else
    screenY := Direction.AsVector;
  d := VectorLength(screenY.Y, screenY.Z);
  if d <= 1e-10 then
    d := ratio
  else
    d := ratio / d;
  dyr := deltaX * d;
  dzr := deltaY * d;
  Result.X := 0;
  Result.Y := screenY.Z * dyr + screenY.Y * dzr;
  Result.Z := screenY.Z * dzr - screenY.Y * dyr;
  Result.W := 0;
end;

function TGLCamera.PointInFront(const point: TGLVector): boolean;
begin
  result := PointIsInHalfSpace(point, AbsolutePosition, AbsoluteDirection);
end;

procedure TGLCamera.SetDepthOfView(AValue: Single);
begin
  if FDepthOfView <> AValue then
  begin
    FDepthOfView := AValue;
    FFOVY := - 1;
    if not (csLoading in ComponentState) then
      TransformationChanged;
  end;
end;

procedure TGLCamera.SetFocalLength(AValue: Single);
begin
  if AValue <= 0 then
    AValue := 1;
  if FFocalLength <> AValue then
  begin
    FFocalLength := AValue;
    FFOVY := - 1;
    if not (csLoading in ComponentState) then
      TransformationChanged;
  end;
end;

function TGLCamera.GetFieldOfView(const AViewportDimension: single): single;
begin
  if FFocalLength = 0 then
    result := 0
  else
    result := RadToDeg(2 * ArcTan2(AViewportDimension * 0.5, FFocalLength));
end;

procedure TGLCamera.SetFieldOfView(const AFieldOfView, AViewportDimension: single);
begin
  FocalLength := AViewportDimension / (2 * Tan(DegToRadian(AFieldOfView / 2)));
end;

procedure TGLCamera.SetCameraStyle(const val: TGLCameraStyle);
begin
  if FCameraStyle <> val then
  begin
    FCameraStyle := val;
    FFOVY := -1;
    NotifyChange(Self);
  end;
end;

procedure TGLCamera.SetKeepFOVMode(const val: TGLCameraKeepFOVMode);
begin
  if FKeepFOVMode <> val then
  begin
    FKeepFOVMode := val;
    FFOVY := -1;
    if FCameraStyle = csPerspectiveKeepFOV then
      NotifyChange(Self);
  end;
end;

procedure TGLCamera.SetSceneScale(value: Single);
begin
  if value = 0 then
    value := 1;
  if FSceneScale <> value then
  begin
    FSceneScale := value;
    FFOVY := -1;
    NotifyChange(Self);
  end;
end;

function TGLCamera.StoreSceneScale: Boolean;
begin
  Result := (FSceneScale <> 1);
end;

procedure TGLCamera.SetNearPlaneBias(value: Single);
begin
  if value <= 0 then
    value := 1;
  if FNearPlaneBias <> value then
  begin
    FNearPlaneBias := value;
    FFOVY := -1;
    NotifyChange(Self);
  end;
end;

function TGLCamera.StoreNearPlaneBias: Boolean;
begin
  Result := (FNearPlaneBias <> 1);
end;

procedure TGLCamera.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
begin
  if ARenderChildren and (Count > 0) then
    Self.RenderChildren(0, Count - 1, ARci);
end;

function TGLCamera.RayCastIntersect(const rayStart, rayVector: TGLVector;
  intersectPoint: PGLVector = nil;
  intersectNormal: PGLVector = nil): Boolean;
begin
  Result := False;
end;

// ------------------
// ------------------ TGLImmaterialSceneObject ------------------
// ------------------
procedure TGLImmaterialSceneObject.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
begin
  // start rendering self
  if ARenderSelf then
  begin
    if (osDirectDraw in ObjectStyle) or ARci.amalgamating then
      BuildList(ARci)
    else
      ARci.GLStates.CallList(GetHandle(ARci));
  end;
  // start rendering children (if any)
  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

// ------------------
// ------------------ TGLCameraInvariantObject ------------------
// ------------------
constructor TGLCameraInvariantObject.Create(AOwner: TComponent);
begin
  inherited;
  FCamInvarianceMode := cimNone;
end;

procedure TGLCameraInvariantObject.Assign(Source: TPersistent);
begin
  if Source is TGLCameraInvariantObject then
  begin
    FCamInvarianceMode := TGLCameraInvariantObject(Source).FCamInvarianceMode;
  end;
  inherited Assign(Source);
end;

procedure TGLCameraInvariantObject.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
begin
  if CamInvarianceMode <> cimNone then
    with ARci.PipelineTransformation do
    begin
      Push;
      //try
        // prepare
        case CamInvarianceMode of
          cimPosition:
            begin
              SetViewMatrix(MatrixMultiply(
                CreateTranslationMatrix(ARci.cameraPosition),
                ARci.PipelineTransformation.ViewMatrix^));
            end;
          cimOrientation:
            begin
              // makes the coordinates system more 'intuitive' (Z+ forward)
              SetViewMatrix(CreateScaleMatrix(Vector3fMake(1, -1, -1)))
            end;
        else
          Assert(False);
        end;
        // Apply local transform
        SetModelMatrix(LocalMatrix^);
        if ARenderSelf then
        begin
          if (osDirectDraw in ObjectStyle) or ARci.amalgamating then
            BuildList(ARci)
          else
            ARci.GLStates.CallList(GetHandle(ARci));
        end;
        if ARenderChildren then
          Self.RenderChildren(0, Count - 1, ARci);
      //finally
        Pop;
      //end;
    end
  else
    inherited;
end;

procedure TGLCameraInvariantObject.SetCamInvarianceMode(const val:
  TGLCameraInvarianceMode);
begin
  if FCamInvarianceMode <> val then
  begin
    FCamInvarianceMode := val;
    NotifyChange(Self);
  end;
end;

// ------------------
// ------------------ TGLDirectOpenGL ------------------
// ------------------
constructor TGLDirectOpenGL.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FBlend := False;
end;

procedure TGLDirectOpenGL.Assign(Source: TPersistent);
begin
  if Source is TGLDirectOpenGL then
  begin
    UseBuildList := TGLDirectOpenGL(Source).UseBuildList;
    FOnRender := TGLDirectOpenGL(Source).FOnRender;
    FBlend := TGLDirectOpenGL(Source).Blend;
  end;
  inherited Assign(Source);
end;

procedure TGLDirectOpenGL.BuildList(var rci: TGLRenderContextInfo);
begin
  if Assigned(FOnRender) then
  begin
    xgl.MapTexCoordToMain; // single texturing by default
    OnRender(Self, rci);
  end;
end;

function TGLDirectOpenGL.AxisAlignedDimensionsUnscaled: TGLVector;
begin
  Result := NullHmgPoint;
end;

procedure TGLDirectOpenGL.SetUseBuildList(const val: Boolean);
begin
  if val <> FUseBuildList then
  begin
    FUseBuildList := val;
    if val then
      ObjectStyle := ObjectStyle - [osDirectDraw]
    else
      ObjectStyle := ObjectStyle + [osDirectDraw];
  end;
end;

function TGLDirectOpenGL.Blended: Boolean;
begin
  Result := FBlend;
end;

procedure TGLDirectOpenGL.SetBlend(const val: Boolean);
begin
  if val <> FBlend then
  begin
    FBlend := val;
    StructureChanged;
  end;
end;

// ------------------
// ------------------ TGLRenderPoint ------------------
// ------------------
constructor TGLRenderPoint.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw];
end;

destructor TGLRenderPoint.Destroy;
begin
  Clear;
  inherited;
end;

procedure TGLRenderPoint.BuildList(var rci: TGLRenderContextInfo);
var
  i: Integer;
begin
  for i := 0 to High(FCallBacks) do
    FCallBacks[i](Self, rci);
end;

procedure TGLRenderPoint.RegisterCallBack(renderEvent: TGLDirectRenderEvent;
  renderPointFreed: TNotifyEvent);
var
  n: Integer;
begin
  n := Length(FCallBacks);
  SetLength(FCallBacks, n + 1);
  SetLength(FFreeCallBacks, n + 1);
  FCallBacks[n] := renderEvent;
  FFreeCallBacks[n] := renderPointFreed;
end;

procedure TGLRenderPoint.UnRegisterCallBack(renderEvent: TGLDirectRenderEvent);
type
  TEventContainer = record
    event: TGLDirectRenderEvent;
  end;
var
  i, j, n: Integer;
  refContainer, listContainer: TEventContainer;
begin
  refContainer.event := renderEvent;
  n := Length(FCallBacks);
  for i := 0 to n - 1 do
  begin
    listContainer.event := FCallBacks[i];
    if CompareMem(@listContainer, @refContainer, SizeOf(TEventContainer)) then
    begin
      for j := i + 1 to n - 1 do
      begin
        FCallBacks[j - 1] := FCallBacks[j];
        FFreeCallBacks[j - 1] := FFreeCallBacks[j];
      end;
      SetLength(FCallBacks, n - 1);
      SetLength(FFreeCallBacks, n - 1);
      Break;
    end;
  end;
end;

procedure TGLRenderPoint.Clear;
begin
  while Length(FCallBacks) > 0 do
  begin
    FFreeCallBacks[High(FCallBacks)](Self);
    SetLength(FCallBacks, Length(FCallBacks) - 1);
  end;
end;

// ------------------
// ------------------ TGLProxyObject ------------------
// ------------------
constructor TGLProxyObject.Create(AOwner: TComponent);
begin
  inherited;
  FProxyOptions := cDefaultProxyOptions;
end;

destructor TGLProxyObject.Destroy;
begin
  SetMasterObject(nil);
  inherited;
end;

procedure TGLProxyObject.Assign(Source: TPersistent);
begin
  if Source is TGLProxyObject then
  begin
    SetMasterObject(TGLProxyObject(Source).MasterObject);
  end;
  inherited Assign(Source);
end;

procedure TGLProxyObject.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  gotMaster, masterGotEffects, oldProxySubObject: Boolean;
begin
  if FRendering then
    Exit;
  FRendering := True;
  try
    gotMaster := Assigned(FMasterObject);
    masterGotEffects := gotMaster and (pooEffects in FProxyOptions)
      and (FMasterObject.Effects.Count > 0);
    if gotMaster then
    begin
      if pooObjects in FProxyOptions then
      begin
        oldProxySubObject := ARci.proxySubObject;
        ARci.proxySubObject := True;
        if pooTransformation in FProxyOptions then
          with ARci.PipelineTransformation do
            SetModelMatrix(MatrixMultiply(FMasterObject.Matrix^, ModelMatrix^));
        FMasterObject.DoRender(ARci, ARenderSelf, (FMasterObject.Count > 0));
        ARci.proxySubObject := oldProxySubObject;
      end;
    end;
    // now render self stuff (our children, our effects, etc.)
    if ARenderChildren and (Count > 0) then
      Self.RenderChildren(0, Count - 1, ARci);
    if masterGotEffects then
      FMasterObject.Effects.RenderPostEffects(ARci);
  finally
    FRendering := False;
  end;
  ClearStructureChanged;
end;

function TGLProxyObject.AxisAlignedDimensions: TGLVector;
begin
  If Assigned(FMasterObject) then
  begin
    Result := FMasterObject.AxisAlignedDimensionsUnscaled;
    If (pooTransformation in ProxyOptions) then
      ScaleVector(Result,FMasterObject.Scale.AsVector)
    else
      ScaleVector(Result, Scale.AsVector);
  end
  else
    Result := inherited AxisAlignedDimensions;
end;

function TGLProxyObject.AxisAlignedDimensionsUnscaled: TGLVector;
begin
  if Assigned(FMasterObject) then
  begin
    Result := FMasterObject.AxisAlignedDimensionsUnscaled;
  end
  else
    Result := inherited AxisAlignedDimensionsUnscaled;
end;

function TGLProxyObject.BarycenterAbsolutePosition: TGLVector;
var
  lAdjustVector: TGLVector;
begin
  if Assigned(FMasterObject) then
  begin
    // Not entirely correct, but better than nothing...
    lAdjustVector := VectorSubtract(FMasterObject.BarycenterAbsolutePosition,
      FMasterObject.AbsolutePosition);
    Position.AsVector := VectorAdd(Position.AsVector, lAdjustVector);
    Result := AbsolutePosition;
    Position.AsVector := VectorSubtract(Position.AsVector, lAdjustVector);
  end
  else
    Result := inherited BarycenterAbsolutePosition;
end;

procedure TGLProxyObject.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FMasterObject) then
    MasterObject := nil;
  inherited;
end;

procedure TGLProxyObject.SetMasterObject(const val: TGLBaseSceneObject);
begin
  if FMasterObject <> val then
  begin
    if Assigned(FMasterObject) then
      FMasterObject.RemoveFreeNotification(Self);
    FMasterObject := val;
    if Assigned(FMasterObject) then
      FMasterObject.FreeNotification(Self);
    StructureChanged;
  end;
end;

procedure TGLProxyObject.SetProxyOptions(const val: TGLProxyObjectOptions);
begin
  if FProxyOptions <> val then
  begin
    FProxyOptions := val;
    StructureChanged;
  end;
end;

function TGLProxyObject.RayCastIntersect(const rayStart, rayVector: TGLVector;
  intersectPoint: PGLVector = nil;
  intersectNormal: PGLVector = nil): Boolean;
var
  localRayStart, localRayVector: TGLVector;
begin
  if Assigned(MasterObject) then
  begin
    SetVector(localRayStart, AbsoluteToLocal(rayStart));
    SetVector(localRayStart, MasterObject.LocalToAbsolute(localRayStart));
    SetVector(localRayVector, AbsoluteToLocal(rayVector));
    SetVector(localRayVector, MasterObject.LocalToAbsolute(localRayVector));
    NormalizeVector(localRayVector);

    Result := MasterObject.RayCastIntersect(localRayStart, localRayVector,
      intersectPoint, intersectNormal);
    if Result then
    begin
      if Assigned(intersectPoint) then
      begin
        SetVector(intersectPoint^,
          MasterObject.AbsoluteToLocal(intersectPoint^));
        SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
      end;
      if Assigned(intersectNormal) then
      begin
        SetVector(intersectNormal^,
          MasterObject.AbsoluteToLocal(intersectNormal^));
        SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      end;
    end;
  end
  else
    Result := False;
end;

function TGLProxyObject.GenerateSilhouette(const silhouetteParameters:
  TGLSilhouetteParameters): TGLSilhouette;
begin
  if Assigned(MasterObject) then
    Result := MasterObject.GenerateSilhouette(silhouetteParameters)
  else
    Result := nil;
end;

// ------------------
// ------------------ TGLLightSource ------------------
// ------------------
constructor TGLLightSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShining := True;
  FSpotDirection := TGLCoordinates.CreateInitialized(Self, VectorMake(0, 0, -1, 0), csVector);
  FConstAttenuation := 1;
  FLinearAttenuation := 0;
  FQuadraticAttenuation := 0;
  FSpotCutOff := 180;
  FSpotExponent := 0;
  FLightStyle := lsSpot;
  FAmbient := TGLColor.Create(Self);
  FDiffuse := TGLColor.Create(Self);
  FDiffuse.Initialize(clrWhite);
  FSpecular := TGLColor.Create(Self);
end;

destructor TGLLightSource.Destroy;
begin
  FSpotDirection.Free;
  FAmbient.Free;
  FDiffuse.Free;
  FSpecular.Free;
  inherited Destroy;
end;

procedure TGLLightSource.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
begin
  if ARenderChildren and Assigned(FChildren) then
    Self.RenderChildren(0, Count - 1, ARci);
end;

function TGLLightSource.RayCastIntersect(const rayStart, rayVector: TGLVector;
  intersectPoint: PGLVector = nil;
  intersectNormal: PGLVector = nil): Boolean;
begin
  Result := False;
end;

procedure TGLLightSource.CoordinateChanged(Sender: TGLCustomCoordinates);
begin
  inherited;
  if Sender = FSpotDirection then
    TransformationChanged;
end;

function TGLLightSource.GenerateSilhouette(const silhouetteParameters:
  TGLSilhouetteParameters): TGLSilhouette;
begin
  Result := nil;
end;

procedure TGLLightSource.SetShining(AValue: Boolean);
begin
  if AValue <> FShining then
  begin
    FShining := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLLightSource.SetSpotDirection(AVector: TGLCoordinates);
begin
  FSpotDirection.DirectVector := AVector.AsVector;
  FSpotDirection.W := 0;
  NotifyChange(Self);
end;

procedure TGLLightSource.SetSpotExponent(AValue: Single);
begin
  if FSpotExponent <> AValue then
  begin
    FSpotExponent := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLLightSource.SetSpotCutOff(const val: Single);
begin
  if FSpotCutOff <> val then
  begin
    if ((val >= 0) and (val <= 90)) or (val = 180) then
    begin
      FSpotCutOff := val;
      NotifyChange(Self);
    end;
  end;
end;

procedure TGLLightSource.SetLightStyle(const val: TGLLightStyle);
begin
  if FLightStyle <> val then
  begin
    FLightStyle := val;
    NotifyChange(Self);
  end;
end;

procedure TGLLightSource.SetAmbient(AValue: TGLColor);
begin
  FAmbient.Color := AValue.Color;
  NotifyChange(Self);
end;

procedure TGLLightSource.SetDiffuse(AValue: TGLColor);
begin
  FDiffuse.Color := AValue.Color;
  NotifyChange(Self);
end;

procedure TGLLightSource.SetSpecular(AValue: TGLColor);
begin
  FSpecular.Color := AValue.Color;
  NotifyChange(Self);
end;

procedure TGLLightSource.SetConstAttenuation(AValue: Single);
begin
  if FConstAttenuation <> AValue then
  begin
    FConstAttenuation := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLLightSource.SetLinearAttenuation(AValue: Single);
begin
  if FLinearAttenuation <> AValue then
  begin
    FLinearAttenuation := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLLightSource.SetQuadraticAttenuation(AValue: Single);
begin
  if FQuadraticAttenuation <> AValue then
  begin
    FQuadraticAttenuation := AValue;
    NotifyChange(Self);
  end;
end;

function TGLLightSource.Attenuated: Boolean;
begin
  Result := (LightStyle <> lsParallel)
    and ((ConstAttenuation <> 1) or (LinearAttenuation <> 0) or
    (QuadraticAttenuation <> 0));
end;

// ------------------
// ------------------ TGLScene ------------------
// ------------------
constructor TGLScene.Create(AOwner: TComponent);
begin
  inherited;
  // root creation
  FCurrentBuffer := nil;
  FObjects := TGLSceneRootObject.Create(Self);
  FObjects.Name := 'ObjectRoot';
  FLights := TGLPersistentObjectList.Create;
  FObjectsSorting := osRenderBlendedLast;
  FVisibilityCulling := vcNone;
  // actual maximum number of lights is stored in TGLSceneViewer
  FLights.Count := 8;
  FInitializableObjects := TGLInitializableObjectList.Create;
end;

destructor TGLScene.Destroy;
begin
  InitializableObjects.Free;
  FObjects.DestroyHandles;
  FLights.Free;
  FObjects.Free;
  if Assigned(FBuffers) then
    FreeAndNil(FBuffers);
  inherited Destroy;
end;

procedure TGLScene.AddLight(ALight: TGLLightSource);
var
  i: Integer;
begin
  for i := 0 to FLights.Count - 1 do
    if FLights.List^[i] = nil then
    begin
      FLights.List^[i] := ALight;
      ALight.FLightID := i;
      Break;
    end;
end;

procedure TGLScene.RemoveLight(ALight: TGLLightSource);
var
  idx: Integer;
begin
  idx := FLights.IndexOf(ALight);
  if idx >= 0 then
    FLights[idx] := nil;
end;

procedure TGLScene.AddLights(anObj: TGLBaseSceneObject);
var
  i: Integer;
begin
  if anObj is TGLLightSource then
    AddLight(TGLLightSource(anObj));
  for i := 0 to anObj.Count - 1 do
    AddLights(anObj.Children[i]);
end;

procedure TGLScene.RemoveLights(anObj: TGLBaseSceneObject);
var
  i: Integer;
begin
  if anObj is TGLLightSource then
    RemoveLight(TGLLightSource(anObj));
  for i := 0 to anObj.Count - 1 do
    RemoveLights(anObj.Children[i]);
end;

procedure TGLScene.ShutdownAllLights;

  procedure DoShutdownLight(Obj: TGLBaseSceneObject);
  var
    i: integer;
  begin
    if Obj is TGLLightSource then
      TGLLightSource(Obj).Shining := False;
    for i := 0 to Obj.Count - 1 do
      DoShutDownLight(Obj[i]);
  end;

begin
  DoShutdownLight(FObjects);
end;

procedure TGLScene.AddBuffer(aBuffer: TGLSceneBuffer);
begin
  if not Assigned(FBuffers) then
    FBuffers := TGLPersistentObjectList.Create;
  if FBuffers.IndexOf(aBuffer) < 0 then
  begin
    FBuffers.Add(aBuffer);
    if FBaseContext = nil then
      FBaseContext := TGLSceneBuffer(FBuffers[0]).RenderingContext;
    if (FBuffers.Count > 1) and Assigned(FBaseContext) then
      aBuffer.RenderingContext.ShareLists(FBaseContext);
  end;
end;

procedure TGLScene.RemoveBuffer(aBuffer: TGLSceneBuffer);
var
  i: Integer;
begin
  if Assigned(FBuffers) then
  begin
    i := FBuffers.IndexOf(aBuffer);
    if i >= 0 then
    begin
      if FBuffers.Count = 1 then
      begin
        FreeAndNil(FBuffers);
        FBaseContext := nil;
      end
      else
      begin
        FBuffers.Delete(i);
        FBaseContext := TGLSceneBuffer(FBuffers[0]).RenderingContext;
      end;
    end;
  end;
end;

procedure TGLScene.GetChildren(AProc: TGetChildProc; Root: TComponent);
begin
  FObjects.GetChildren(AProc, Root);
end;

procedure TGLScene.SetChildOrder(AChild: TComponent; Order: Integer);
begin
  (AChild as TGLBaseSceneObject).Index := Order;
end;

function TGLScene.IsUpdating: Boolean;
begin
  Result := (FUpdateCount <> 0) or (csLoading in ComponentState) or (csDestroying in ComponentState);
end;

procedure TGLScene.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TGLScene.EndUpdate;
begin
  Assert(FUpdateCount > 0);
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    NotifyChange(Self);
end;

procedure TGLScene.SetObjectsSorting(const val: TGLObjectsSorting);
begin
  if FObjectsSorting <> val then
  begin
    if val = osInherited then
      FObjectsSorting := osRenderBlendedLast
    else
      FObjectsSorting := val;
    NotifyChange(Self);
  end;
end;

procedure TGLScene.SetVisibilityCulling(const val: TGLVisibilityCulling);
begin
  if FVisibilityCulling <> val then
  begin
    if val = vcInherited then
      FVisibilityCulling := vcNone
    else
      FVisibilityCulling := val;
    NotifyChange(Self);
  end;
end;

procedure TGLScene.ReadState(Reader: TReader);
var
  SaveRoot: TComponent;
begin
  SaveRoot := Reader.Root;
  try
    if Owner <> nil then
      Reader.Root := Owner;
    inherited;
  finally
    Reader.Root := SaveRoot;
  end;
end;

procedure TGLScene.Progress(const deltaTime, newTime: Double);
var
  pt: TGLProgressTimes;
begin
  pt.deltaTime := deltaTime;
  pt.newTime := newTime;
  FCurrentDeltaTime := deltaTime;
  if Assigned(FOnBeforeProgress) then
   FOnBeforeProgress(Self, deltaTime, newTime);
  FObjects.DoProgress(pt);
  if Assigned(FOnProgress) then
   FOnProgress(Self, deltaTime, newTime);
end;

procedure TGLScene.SaveToFile(const fileName: string);
var
  stream: TStream;
begin
  stream := TFileStream.Create(fileName, fmCreate);
  try
    SaveToStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TGLScene.LoadFromFile(const fileName: string);
  procedure CheckResFileStream(Stream: TStream);
  var
    N: Integer;
    B: Byte;
  begin
    N := Stream.Position;
    Stream.Read(B, Sizeof(B));
    Stream.Position := N;
    if B = $FF then
      Stream.ReadResHeader;
  end;

var
  stream: TStream;
begin
  stream := TFileStream.Create(fileName, fmOpenRead);
  try
    CheckResFileStream(stream);
    LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TGLScene.SaveToTextFile(const fileName: string);
var
  mem: TMemoryStream;
  fil: TStream;
begin
  mem := TMemoryStream.Create;
  fil := TFileStream.Create(fileName, fmCreate);
  try
    SaveToStream(mem);
    mem.Position := 0;
    ObjectBinaryToText(mem, fil);
  finally
    fil.Free;
    mem.Free;
  end;
end;

procedure TGLScene.LoadFromTextFile(const fileName: string);
var
  Mem: TMemoryStream;
  Fil: TStream;
begin
  Mem := TMemoryStream.Create;
  Fil := TFileStream.Create(fileName, fmOpenRead);
  try
    ObjectTextToBinary(Fil, Mem);
    Mem.Position := 0;
    LoadFromStream(Mem);
  finally
    Fil.Free;
    Mem.Free;
  end;
end;

procedure TGLScene.LoadFromStream(aStream: TStream);
var
  fixups: TStringList;
  i: Integer;
  obj: TGLBaseSceneObject;
begin
  Fixups := TStringList.Create;
  try
    if Assigned(FBuffers) then
    begin
      for i := 0 to FBuffers.Count - 1 do
        Fixups.AddObject(TGLSceneBuffer(FBuffers[i]).Camera.Name, FBuffers[i]);
    end;
    ShutdownAllLights;
    // will remove Viewer from FBuffers
    Objects.DeleteChildren;
    aStream.ReadComponent(Self);
    for i := 0 to Fixups.Count - 1 do
    begin
      obj := FindSceneObject(fixups[I]);
      if obj is TGLCamera then
        TGLSceneBuffer(Fixups.Objects[i]).Camera := TGLCamera(obj)
      else { can assign default camera (if existing, of course) instead }
        ;
    end;
  finally
    Fixups.Free;
  end;
end;

procedure TGLScene.SaveToStream(aStream: TStream);
begin
  aStream.WriteComponent(Self);
end;

function TGLScene.FindSceneObject(const AName: string): TGLBaseSceneObject;
begin
  Result := FObjects.FindChild(AName, False);
end;

function TGLScene.RayCastIntersect(const rayStart, rayVector: TGLVector;
  intersectPoint: PGLVector = nil;
  intersectNormal: PGLVector = nil): TGLBaseSceneObject;
var
  bestDist2: Single;
  bestHit: TGLBaseSceneObject;
  iPoint, iNormal: TGLVector;
  pINormal: PGLVector;

  function RecursiveDive(baseObject: TGLBaseSceneObject): TGLBaseSceneObject;
  var
    i: Integer;
    curObj: TGLBaseSceneObject;
    dist2: Single;
    fNear, fFar: single;
  begin
    Result := nil;
    for i := 0 to baseObject.Count - 1 do
    begin
      curObj := baseObject.Children[i];
      if curObj.Visible then
      begin
        if RayCastAABBIntersect(rayStart, rayVector,
          curObj.AxisAlignedBoundingBoxAbsoluteEx, fNear, fFar) then
        begin
          if fnear * fnear > bestDist2 then
          begin
            if not PointInAABB(rayStart, curObj.AxisAlignedBoundingBoxAbsoluteEx) then
              continue;
          end;
          if curObj.RayCastIntersect(rayStart, rayVector, @iPoint, pINormal) then
          begin
            dist2 := VectorDistance2(rayStart, iPoint);
            if dist2 < bestDist2 then
            begin
              bestHit := curObj;
              bestDist2 := dist2;
              if Assigned(intersectPoint) then
                intersectPoint^ := iPoint;
              if Assigned(intersectNormal) then
                intersectNormal^ := iNormal;
            end;
          end;
          RecursiveDive(curObj);
        end;
      end;
    end;
  end;

begin
  bestDist2 := 1e20;
  bestHit := nil;
  if Assigned(intersectNormal) then
    pINormal := @iNormal
  else
    pINormal := nil;
  RecursiveDive(Objects);
  Result := bestHit;
end;

procedure TGLScene.NotifyChange(Sender: TObject);
var
  i: Integer;
begin
  if (not IsUpdating) and Assigned(FBuffers) then
    for i := 0 to FBuffers.Count - 1 do
      TGLSceneBuffer(FBuffers[i]).NotifyChange(Self);
end;

procedure TGLScene.SetupLights(maxLights: Integer);
var
  i: Integer;
  lightSource: TGLLightSource;
  nbLights: Integer;
  lPos: TGLVector;
begin
  nbLights := FLights.Count;
  if nbLights > maxLights then
    nbLights := maxLights;
  // setup all light sources
  with CurrentGLContext.GLStates, CurrentGLContext.PipelineTransformation do
  begin
    for i := 0 to nbLights - 1 do
    begin
      lightSource := TGLLightSource(FLights[i]);
      if Assigned(lightSource) then
        with lightSource do
        begin
          LightEnabling[FLightID] := Shining;
          if Shining then
          begin
            if FixedFunctionPipeLight then
            begin
              RebuildMatrix;
              if LightStyle in [lsParallel, lsParallelSpot] then
              begin
                SetModelMatrix(AbsoluteMatrix);
                gl.Lightfv(GL_LIGHT0 + FLightID, GL_POSITION, SpotDirection.AsAddress);
              end
              else
              begin
                SetModelMatrix(Parent.AbsoluteMatrix);
                gl.Lightfv(GL_LIGHT0 + FLightID, GL_POSITION, Position.AsAddress);
              end;
              if LightStyle in [lsSpot, lsParallelSpot] then
              begin
                if FSpotCutOff <> 180 then
                  gl.Lightfv(GL_LIGHT0 + FLightID, GL_SPOT_DIRECTION, FSpotDirection.AsAddress);
              end;
            end;

            lPos := lightSource.AbsolutePosition;
            if LightStyle in [lsParallel, lsParallelSpot] then
              lPos.W := 0.0
            else
              lPos.W := 1.0;
            LightPosition[FLightID] := lPos;
            LightSpotDirection[FLightID] := lightSource.SpotDirection.AsAffineVector;

            LightAmbient[FLightID] := FAmbient.Color;
            LightDiffuse[FLightID] := FDiffuse.Color;
            LightSpecular[FLightID] := FSpecular.Color;

            LightConstantAtten[FLightID] := FConstAttenuation;
            LightLinearAtten[FLightID] := FLinearAttenuation;
            LightQuadraticAtten[FLightID] := FQuadraticAttenuation;

            LightSpotExponent[FLightID] := FSpotExponent;
            LightSpotCutoff[FLightID] := FSpotCutOff;
          end;
        end
      else
        LightEnabling[i] := False;
    end;
    // turn off other lights
    for i := nbLights to maxLights - 1 do
      LightEnabling[i] := False;
    SetModelMatrix(IdentityHmgMatrix);
  end;
end;

// ------------------
// ------------------ TGLFogEnvironment ------------------
// ------------------

// Note: The fog implementation is not conformal with the rest of the scene management
//       because it is viewer bound not scene bound.
constructor TGLFogEnvironment.Create(AOwner: TPersistent);
begin
  inherited;
  FSceneBuffer := (AOwner as TGLSceneBuffer);
  FFogColor := TGLColor.CreateInitialized(Self, clrBlack);
  FFogMode := fmLinear;
  FFogStart := 10;
  FFogEnd := 1000;
  FFogDistance := fdDefault;
end;

destructor TGLFogEnvironment.Destroy;
begin
  FFogColor.Free;
  inherited Destroy;
end;

procedure TGLFogEnvironment.SetFogColor(Value: TGLColor);
begin
  if Assigned(Value) then
  begin
    FFogColor.Assign(Value);
    NotifyChange(Self);
  end;
end;

procedure TGLFogEnvironment.SetFogStart(Value: Single);
begin
  if Value <> FFogStart then
  begin
    FFogStart := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLFogEnvironment.SetFogEnd(Value: Single);
begin
  if Value <> FFogEnd then
  begin
    FFogEnd := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLFogEnvironment.Assign(Source: TPersistent);
begin
  if Source is TGLFogEnvironment then
  begin
    FFogColor.Assign(TGLFogEnvironment(Source).FFogColor);
    FFogStart := TGLFogEnvironment(Source).FFogStart;
    FFogEnd := TGLFogEnvironment(Source).FFogEnd;
    FFogMode := TGLFogEnvironment(Source).FFogMode;
    FFogDistance := TGLFogEnvironment(Source).FFogDistance;
    NotifyChange(Self);
  end;
  inherited;
end;

function TGLFogEnvironment.IsAtDefaultValues: Boolean;
begin
  Result := VectorEquals(FogColor.Color, FogColor.DefaultColor)
    and (FogStart = 10)
    and (FogEnd = 1000)
    and (FogMode = fmLinear)
    and (FogDistance = fdDefault);
end;

procedure TGLFogEnvironment.SetFogMode(Value: TFogMode);
begin
  if Value <> FFogMode then
  begin
    FFogMode := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLFogEnvironment.SetFogDistance(const val: TFogDistance);
begin
  if val <> FFogDistance then
  begin
    FFogDistance := val;
    NotifyChange(Self);
  end;
end;

var
  vImplemDependantFogDistanceDefault: Integer = -1;

procedure TGLFogEnvironment.ApplyFog;
var
  tempActivation: Boolean;
begin
  with FSceneBuffer do
  begin
    if not Assigned(FRenderingContext) then
      Exit;
    tempActivation := not FRenderingContext.Active;
    if tempActivation then
      FRenderingContext.Activate;
  end;

  case FFogMode of
    fmLinear: gl.Fogi(GL_FOG_MODE, GL_LINEAR);
    fmExp:
      begin
        gl.Fogi(GL_FOG_MODE, GL_EXP);
        gl.Fogf(GL_FOG_DENSITY, FFogColor.Alpha);
      end;
    fmExp2:
      begin
        gl.Fogi(GL_FOG_MODE, GL_EXP2);
        gl.Fogf(GL_FOG_DENSITY, FFogColor.Alpha);
      end;
  end;
  gl.Fogfv(GL_FOG_COLOR, FFogColor.AsAddress);
  gl.Fogf(GL_FOG_START, FFogStart);
  gl.Fogf(GL_FOG_END, FFogEnd);
  if gl.NV_fog_distance then
  begin
    case FogDistance of
      fdDefault:
        begin
          if vImplemDependantFogDistanceDefault = -1 then
            gl.GetIntegerv(GL_FOG_DISTANCE_MODE_NV,
              @vImplemDependantFogDistanceDefault)
          else
            gl.Fogi(GL_FOG_DISTANCE_MODE_NV, vImplemDependantFogDistanceDefault);
        end;
      fdEyePlane:
        gl.Fogi(GL_FOG_DISTANCE_MODE_NV, GL_EYE_PLANE_ABSOLUTE_NV);
      fdEyeRadial:
        gl.Fogi(GL_FOG_DISTANCE_MODE_NV, GL_EYE_RADIAL_NV);
    else
      Assert(False);
    end;
  end;

  if tempActivation then
    FSceneBuffer.RenderingContext.Deactivate;
end;

// ------------------
// ------------------ TGLSceneBuffer ------------------
// ------------------
constructor TGLSceneBuffer.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);

  // initialize private state variables
  FFogEnvironment := TGLFogEnvironment.Create(Self);
  FBackgroundColor := clBtnFace;
  FBackgroundAlpha := 1;
  FAmbientColor := TGLColor.CreateInitialized(Self, clrGray20);
  FDepthTest := True;
  FFaceCulling := True;
  FLighting := True;
  FAntiAliasing := aaDefault;
  FDepthPrecision := dpDefault;
  FColorDepth := cdDefault;
  FShadeModel := smDefault;
  FFogEnable := False;
  FLayer := clMainPlane;
  FAfterRenderEffects := TGLPersistentObjectList.Create;
  FContextOptions := [roDoubleBuffer, roRenderToWindow, roDebugContext];
  ResetPerformanceMonitor;
end;

destructor TGLSceneBuffer.Destroy;
begin
  Melt;
  DestroyRC;
  FAmbientColor.Free;
  FAfterRenderEffects.Free;
  FFogEnvironment.Free;
  inherited Destroy;
end;

procedure TGLSceneBuffer.PrepareGLContext;
begin
  if Assigned(FOnPrepareGLContext) then
    FOnPrepareGLContext(Self);
end;

procedure TGLSceneBuffer.SetupRCOptions(context: TGLContext);
const
  cColorDepthToColorBits: array[cdDefault..cdFloat128bits] of Integer =
    (24, 8, 16, 24, 64, 128); // float_type
  cDepthPrecisionToDepthBits: array[dpDefault..dp32bits] of Integer =
    (24, 16, 24, 32);
var
  locOptions: TGLRCOptions;
  locStencilBits, locAlphaBits, locColorBits: Integer;
begin
  locOptions := [];

  if roDoubleBuffer in ContextOptions then
    locOptions := locOptions + [rcoDoubleBuffered];
  if roStereo in ContextOptions then
    locOptions := locOptions + [rcoStereo];
  if roDebugContext in ContextOptions then
    locOptions := locOptions + [rcoDebug];
  if roOpenGL_ES2_Context in ContextOptions then
    locOptions := locOptions + [rcoOGL_ES];
  if roNoColorBuffer in ContextOptions then
    locColorBits := 0
  else
    locColorBits := cColorDepthToColorBits[ColorDepth];
  if roStencilBuffer in ContextOptions then
    locStencilBits := 8
  else
    locStencilBits := 0;
  if roDestinationAlpha in ContextOptions then
    locAlphaBits := 8
  else
    locAlphaBits := 0;
  with context do
  begin
    if roSoftwareMode in ContextOptions then
      Acceleration := chaSoftware
    else
      Acceleration := chaHardware;
    Options := locOptions;
    ColorBits := locColorBits;
    DepthBits := cDepthPrecisionToDepthBits[DepthPrecision];
    StencilBits := locStencilBits;
    AlphaBits := locAlphaBits;
    AccumBits := AccumBufferBits;
    AuxBuffers := 0;
    AntiAliasing := Self.AntiAliasing;
    Layer := Self.Layer;
{    GLStates.ForwardContext := roForwardContext in ContextOptions;}
    PrepareGLContext;
  end;
end;

procedure TGLSceneBuffer.CreateRC(AWindowHandle: HWND; memoryContext:
  Boolean; BufferCount: Integer);
begin
  DestroyRC;
  FRendering := True;

  try
    // will be freed in DestroyWindowHandle
    FRenderingContext := GLContextManager.CreateContext;
    if not Assigned(FRenderingContext) then
      raise Exception.Create('Failed to create RenderingContext.');
    SetupRCOptions(FRenderingContext);

    if Assigned(FCamera) and Assigned(FCamera.FScene) then
      FCamera.FScene.AddBuffer(Self);

    with FRenderingContext do
    begin
      try
        if memoryContext then
          CreateMemoryContext(AWindowHandle, FViewPort.Width, FViewPort.Height,
            BufferCount)
        else
          CreateContext(AWindowHandle);
      except
        FreeAndNil(FRenderingContext);
    	if Assigned(FCamera) and Assigned(FCamera.FScene) then
          FCamera.FScene.RemoveBuffer(Self);
        raise;
      end;
    end;
    FRenderingContext.Activate;
    try
      // this one should NOT be replaced with an assert
      if not gl.VERSION_1_1 then
      begin
        GLSLogger.LogFatalError(strWrongVersion);
        Abort;
      end;
      // define viewport, this is necessary because the first WM_SIZE message
      // is posted before the rendering context has been created
      FRenderingContext.GLStates.ViewPort :=
        Vector4iMake(FViewPort.Left, FViewPort.Top, FViewPort.Width, FViewPort.Height);
      // set up initial context states
      SetupRenderingContext(FRenderingContext);
      FRenderingContext.GLStates.ColorClearValue :=
        ConvertWinColor(FBackgroundColor);
    finally
      FRenderingContext.Deactivate;
    end;
  finally
    FRendering := False;
  end;
end;

procedure TGLSceneBuffer.DestroyRC;
begin
  if Assigned(FRenderingContext) then
  begin
    Melt;
    // for some obscure reason, Mesa3D doesn't like this call... any help welcome
    FreeAndNil(FSelector);
    FreeAndNil(FRenderingContext);
    if Assigned(FCamera) and Assigned(FCamera.FScene) then
      FCamera.FScene.RemoveBuffer(Self);
  end;
end;

function TGLSceneBuffer.RCInstantiated: Boolean;
begin
  Result := Assigned(FRenderingContext);
end;

procedure TGLSceneBuffer.Resize(newLeft, newTop, newWidth, newHeight: Integer);
begin
  if newWidth < 1 then
    newWidth := 1;
  if newHeight < 1 then
    newHeight := 1;
  FViewPort.Left := newLeft;
  FViewPort.Top := newTop;
  FViewPort.Width := newWidth;
  FViewPort.Height := newHeight;
  if Assigned(FRenderingContext) then
  begin
    FRenderingContext.Activate;
    try
      // Part of workaround for MS OpenGL "black borders" bug
      FRenderingContext.GLStates.ViewPort :=
        Vector4iMake(FViewPort.Left, FViewPort.Top, FViewPort.Width, FViewPort.Height);
    finally
      FRenderingContext.Deactivate;
    end;
  end;
end;

function TGLSceneBuffer.Acceleration: TGLContextAcceleration;
begin
  if Assigned(FRenderingContext) then
    Result := FRenderingContext.Acceleration
  else
    Result := chaUnknown;
end;

procedure TGLSceneBuffer.SetupRenderingContext(context: TGLContext);

  procedure SetState(context: TGLContext; bool: Boolean; csState: TGLState); inline;
  begin
    case bool of
      true: context.GLStates.PerformEnable(csState);
      false: context.GLStates.PerformDisable(csState);
    end;
  end;

var
  LColorDepth: Cardinal;
begin
  if not Assigned(context) then
    Exit;

  if not (roForwardContext in ContextOptions) then
  begin
    gl.LightModelfv(GL_LIGHT_MODEL_AMBIENT, FAmbientColor.AsAddress);
    if roTwoSideLighting in FContextOptions then
      gl.LightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE)
    else
      gl.LightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_FALSE);
    gl.Hint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
    case ShadeModel of
      smDefault, smSmooth: gl.ShadeModel(GL_SMOOTH);
      smFlat: gl.ShadeModel(GL_FLAT);
    else
      Assert(False, strErrorEx + strUnknownType);
    end;
  end;

  with context.GLStates do
  begin
    Enable(stNormalize);
    SetState(context, DepthTest, stDepthTest);
    SetState(context, FaceCulling, stCullFace);
    SetState(context, Lighting, stLighting);
    SetState(context, FogEnable, stFog);
    if gl.ARB_depth_clamp then
      Disable(stDepthClamp);
    if not (roForwardContext in ContextOptions) then
    begin
      gl.GetIntegerv(GL_BLUE_BITS, @LColorDepth); // could've used red or green too
      SetState(context, (LColorDepth < 8), stDither);
    end;
    ResetAllGLTextureMatrix;
  end;
end;

function TGLSceneBuffer.GetLimit(Which: TGLLimitType): Integer;
var
  VP: array[0..1] of Double;
begin
  case Which of
    limClipPlanes: gl.GetIntegerv(GL_MAX_CLIP_PLANES, @Result);
    limEvalOrder: gl.GetIntegerv(GL_MAX_EVAL_ORDER, @Result);
    limLights: gl.GetIntegerv(GL_MAX_LIGHTS, @Result);
    limListNesting: gl.GetIntegerv(GL_MAX_LIST_NESTING, @Result);
    limModelViewStack: gl.GetIntegerv(GL_MAX_MODELVIEW_STACK_DEPTH, @Result);
    limNameStack: gl.GetIntegerv(GL_MAX_NAME_STACK_DEPTH, @Result);
    limPixelMapTable: gl.GetIntegerv(GL_MAX_PIXEL_MAP_TABLE, @Result);
    limProjectionStack: gl.GetIntegerv(GL_MAX_PROJECTION_STACK_DEPTH, @Result);
    limTextureSize: gl.GetIntegerv(GL_MAX_TEXTURE_SIZE, @Result);
    limTextureStack: gl.GetIntegerv(GL_MAX_TEXTURE_STACK_DEPTH, @Result);
    limViewportDims:
      begin
        gl.GetDoublev(GL_MAX_VIEWPORT_DIMS, @VP);
        if VP[0] > VP[1] then
          Result := Round(VP[0])
        else
          Result := Round(VP[1]);
      end;
    limAccumAlphaBits: gl.GetIntegerv(GL_ACCUM_ALPHA_BITS, @Result);
    limAccumBlueBits: gl.GetIntegerv(GL_ACCUM_BLUE_BITS, @Result);
    limAccumGreenBits: gl.GetIntegerv(GL_ACCUM_GREEN_BITS, @Result);
    limAccumRedBits: gl.GetIntegerv(GL_ACCUM_RED_BITS, @Result);
    limAlphaBits: gl.GetIntegerv(GL_ALPHA_BITS, @Result);
    limAuxBuffers: gl.GetIntegerv(GL_AUX_BUFFERS, @Result);
    limDepthBits: gl.GetIntegerv(GL_DEPTH_BITS, @Result);
    limStencilBits: gl.GetIntegerv(GL_STENCIL_BITS, @Result);
    limBlueBits: gl.GetIntegerv(GL_BLUE_BITS, @Result);
    limGreenBits: gl.GetIntegerv(GL_GREEN_BITS, @Result);
    limRedBits: gl.GetIntegerv(GL_RED_BITS, @Result);
    limIndexBits: gl.GetIntegerv(GL_INDEX_BITS, @Result);
    limStereo: gl.GetIntegerv(GL_STEREO, @Result);
    limDoubleBuffer: gl.GetIntegerv(GL_DOUBLEBUFFER, @Result);
    limSubpixelBits: gl.GetIntegerv(GL_SUBPIXEL_BITS, @Result);
    limNbTextureUnits:
      if gl.ARB_multitexture then
        gl.GetIntegerv(GL_MAX_TEXTURE_UNITS_ARB, @Result)
      else
        Result := 1;
  else
    Result := 0;
  end;
end;

procedure TGLSceneBuffer.RenderToFile(const aFile: string; DPI: Integer);
var
  aBitmap: TBitmap;
  saveAllowed: Boolean;
  fileName: string;
begin
  Assert((not FRendering), strAlreadyRendering);
  aBitmap := TBitmap.Create;
  try
    aBitmap.Width := FViewPort.Width;
    aBitmap.Height := FViewPort.Height;
    aBitmap.PixelFormat := pf24Bit;
    RenderToBitmap(ABitmap, DPI);
    fileName := aFile;
    if fileName = '' then
      saveAllowed := SavePictureDialog(fileName)
    else
      saveAllowed := True;
    if saveAllowed then
    begin
      if FileExists(fileName) then
        saveAllowed := QuestionDlg(Format('Overwrite file %s?', [fileName]));
      if saveAllowed then
        aBitmap.SaveToFile(fileName);
    end;
  finally
    aBitmap.Free;
  end;
end;

procedure TGLSceneBuffer.RenderToFile(const AFile: string; bmpWidth, bmpHeight:
  Integer);
var
  aBitmap: TBitmap;
  saveAllowed: Boolean;
  fileName: string;
begin
  Assert((not FRendering), strAlreadyRendering);
  aBitmap := TBitmap.Create;
  try
    aBitmap.Width := bmpWidth;
    aBitmap.Height := bmpHeight;
    aBitmap.PixelFormat := pf24Bit;
    RenderToBitmap(aBitmap,
      (GetDeviceLogicalPixelsX(Cardinal(ABitmap.Canvas.Handle)) * bmpWidth) div
      FViewPort.Width);
    fileName := AFile;
    if fileName = '' then
      saveAllowed := SavePictureDialog(fileName)
    else
      saveAllowed := True;
    if saveAllowed then
    begin
      if FileExists(fileName) then
        saveAllowed := QuestionDlg(Format('Overwrite file %s?', [fileName]));
      if SaveAllowed then
        aBitmap.SaveToFile(fileName);
    end;
  finally
    aBitmap.Free;
  end;
end;

function TGLSceneBuffer.CreateSnapShot: TGLBitmap32;
begin
  Result := TGLBitmap32.Create;
  Result.Width := FViewPort.Width;
  Result.Height := FViewPort.Height;
  if Assigned(Camera) and Assigned(Camera.Scene) then
  begin
    FRenderingContext.Activate;
    try
      Result.ReadPixels(Rect(0, 0, FViewPort.Width, FViewPort.Height));
    finally
      FRenderingContext.DeActivate;
    end;
  end;
end;

function TGLSceneBuffer.CreateSnapShotBitmap: TBitmap;
var
  bmp32: TGLBitmap32;
begin
  bmp32 := CreateSnapShot;
  try
    Result := bmp32.Create32BitsBitmap;
  finally
    bmp32.Free;
  end;
end;

procedure TGLSceneBuffer.CopyToTexture(aTexture: TGLTexture);
begin
  CopyToTexture(aTexture, 0, 0, Width, Height, 0, 0);
end;

procedure TGLSceneBuffer.CopyToTexture(aTexture: TGLTexture;
  xSrc, ySrc, AWidth, AHeight: Integer;
  xDest, yDest: Integer;
  glCubeFace: Cardinal = 0);
var
  bindTarget: TGLTextureTarget;
begin
  if RenderingContext <> nil then
  begin
    RenderingContext.Activate;
    try
      if not (aTexture.Image is TGLBlankImage) then
        aTexture.ImageClassName := TGLBlankImage.ClassName;
      if aTexture.Image.Width <> AWidth then
        TGLBlankImage(aTexture.Image).Width := AWidth;
      if aTexture.Image.Height <> AHeight then
        TGLBlankImage(aTexture.Image).Height := AHeight;
      if aTexture.Image.Depth <> 0 then
        TGLBlankImage(aTexture.Image).Depth := 0;
      if TGLBlankImage(aTexture.Image).CubeMap <> (glCubeFace > 0) then
        TGLBlankImage(aTexture.Image).CubeMap := (glCubeFace > 0);

      bindTarget := aTexture.Image.NativeTextureTarget;
      RenderingContext.GLStates.TextureBinding[0, bindTarget] := aTexture.Handle;
      if glCubeFace > 0 then
        gl.CopyTexSubImage2D(glCubeFace,
          0, xDest, yDest, xSrc, ySrc, AWidth, AHeight)
      else
        gl.CopyTexSubImage2D(DecodeTextureTarget(bindTarget),
          0, xDest, yDest, xSrc, ySrc, AWidth, AHeight)
    finally
      RenderingContext.Deactivate;
    end;
  end;
end;

procedure TGLSceneBuffer.SaveAsFloatToFile(const aFilename: string);
var
  Data: pointer;
  DataSize: integer;
  Stream: TMemoryStream;
const
  FloatSize = 4;
begin
  if Assigned(Camera) and Assigned(Camera.Scene) then
  begin
    DataSize := Width * Height * FloatSize * FloatSize;
    GetMem(Data, DataSize);
    FRenderingContext.Activate;
    try
      gl.ReadPixels(0, 0, Width, Height, GL_RGBA, GL_FLOAT, Data);
      gl.CheckError;

      Stream := TMemoryStream.Create;
      try
        Stream.Write(Data^, DataSize);
        Stream.SaveToFile(aFilename);
      finally
        Stream.Free;
      end;
    finally
      FRenderingContext.DeActivate;
      FreeMem(Data);
    end;
  end;
end;

procedure TGLSceneBuffer.SetViewPort(X, Y, W, H: Integer);
begin
  with FViewPort do
  begin
    Left := X;
    Top := Y;
    Width := W;
    Height := H;
  end;
  NotifyChange(Self);
end;

function TGLSceneBuffer.Width: Integer;
begin
  Result := FViewPort.Width;
end;

function TGLSceneBuffer.Height: Integer;
begin
  Result := FViewPort.Height;
end;

procedure TGLSceneBuffer.Freeze;
begin
  if Freezed then
    Exit;
  if RenderingContext = nil then
    Exit;
  Render;
  FFreezed := True;
  RenderingContext.Activate;
  try
    FFreezeBuffer := AllocMem(FViewPort.Width * FViewPort.Height * 4);
    gl.ReadPixels(0, 0, FViewport.Width, FViewPort.Height,
      GL_RGBA, GL_UNSIGNED_BYTE, FFreezeBuffer);
    FFreezedViewPort := FViewPort;
  finally
    RenderingContext.Deactivate;
  end;
end;

procedure TGLSceneBuffer.Melt;
begin
  if not Freezed then
    Exit;
  FreeMem(FFreezeBuffer);
  FFreezeBuffer := nil;
  FFreezed := False;
end;

procedure TGLSceneBuffer.RenderToBitmap(ABitmap: TBitmap; DPI: Integer);
var
  nativeContext: TGLContext;
  aColorBits: Integer;
begin
  Assert((not FRendering), strAlreadyRendering);
  FRendering := True;
  nativeContext := RenderingContext;
  try
    aColorBits := PixelFormatToColorBits(ABitmap.PixelFormat);
    if aColorBits < 8 then
      aColorBits := 8;
    FRenderingContext := GLContextManager.CreateContext;
    SetupRCOptions(FRenderingContext);
    with FRenderingContext do
    begin
      Options := []; // no such things for bitmap rendering
      ColorBits := aColorBits; // honour Bitmap's pixel depth
      AntiAliasing := aaNone; // no AA for bitmap rendering
      CreateContext(ABitmap.Canvas.Handle);
    end;
    try
      FRenderingContext.Activate;
      try
        SetupRenderingContext(FRenderingContext);
        FRenderingContext.GLStates.ColorClearValue := ConvertWinColor(FBackgroundColor);
        // set the desired viewport and limit output to this rectangle
        with FViewport do
        begin
          Left := 0;
          Top := 0;
          Width := ABitmap.Width;
          Height := ABitmap.Height;
          FRenderingContext.GLStates.ViewPort :=
            Vector4iMake(Left, Top, Width, Height);
        end;
        ClearBuffers;
        FRenderDPI := DPI;
        if FRenderDPI = 0 then
          FRenderDPI := GetDeviceLogicalPixelsX(ABitmap.Canvas.Handle);
        // render
        DoBaseRender(FViewport, FRenderDPI, dsPrinting, nil);
        if nativeContext <> nil then
          FViewport := TRectangle(nativeContext.GLStates.ViewPort);
        gl.Finish;
      finally
        FRenderingContext.Deactivate;
      end;
    finally
      FRenderingContext.Free;
    end;
  finally
    FRenderingContext := nativeContext;
    FRendering := False;
  end;
  if Assigned(FAfterRender) then
    if Owner is TComponent then
      if not (csDesigning in TComponent(Owner).ComponentState) then
        FAfterRender(Self);
end;

procedure TGLSceneBuffer.ShowInfo(Modal: boolean);
begin
  if not Assigned(FRenderingContext) then
    Exit;
  // most info is available with active context only
  FRenderingContext.Activate;
  try
    InvokeInfoForm(Self, Modal);
  finally
    FRenderingContext.Deactivate;
  end;
end;

procedure TGLSceneBuffer.ResetPerformanceMonitor;
begin
  FFramesPerSecond := 0;
  FFrameCount := 0;
  FFirstPerfCounter := 0;
end;

procedure TGLSceneBuffer.PushViewMatrix(const newMatrix: TGLMatrix);
var
  n: Integer;
begin
  n := Length(FViewMatrixStack);
  SetLength(FViewMatrixStack, n + 1);
  FViewMatrixStack[n] := RenderingContext.PipelineTransformation.ViewMatrix^;
  RenderingContext.PipelineTransformation.SetViewMatrix(newMatrix);
end;

procedure TGLSceneBuffer.PopViewMatrix;
var
  n: Integer;
begin
  n := High(FViewMatrixStack);
  Assert(n >= 0, 'Unbalanced PopViewMatrix');
  RenderingContext.PipelineTransformation.SetViewMatrix(FViewMatrixStack[n]);
  SetLength(FViewMatrixStack, n);
end;

procedure TGLSceneBuffer.PushProjectionMatrix(const newMatrix: TGLMatrix);
var
  n: Integer;
begin
  n := Length(FProjectionMatrixStack);
  SetLength(FProjectionMatrixStack, n + 1);
  FProjectionMatrixStack[n] := RenderingContext.PipelineTransformation.ProjectionMatrix^;
  RenderingContext.PipelineTransformation.SetProjectionMatrix(newMatrix);
end;

procedure TGLSceneBuffer.PopProjectionMatrix;
var
  n: Integer;
begin
  n := High(FProjectionMatrixStack);
  Assert(n >= 0, 'Unbalanced PopProjectionMatrix');
  RenderingContext.PipelineTransformation.SetProjectionMatrix(FProjectionMatrixStack[n]);
  SetLength(FProjectionMatrixStack, n);
end;

function TGLSceneBuffer.ProjectionMatrix;
begin
  Result := RenderingContext.PipelineTransformation.ProjectionMatrix^;
end;

function TGLSceneBuffer.ViewMatrix: TGLMatrix;
begin
  Result := RenderingContext.PipelineTransformation.ViewMatrix^;
end;

function TGLSceneBuffer.ModelMatrix: TGLMatrix;
begin
  Result := RenderingContext.PipelineTransformation.ModelMatrix^;
end;

function TGLSceneBuffer.OrthoScreenToWorld(screenX, screenY: Integer):
  TAffineVector;
var
  camPos, camUp, camRight: TAffineVector;
  f: Single;
begin
  if Assigned(FCamera) then
  begin
    SetVector(camPos, FCameraAbsolutePosition);
    if Camera.TargetObject <> nil then
    begin
      SetVector(camUp, FCamera.AbsoluteUpVectorToTarget);
      SetVector(camRight, FCamera.AbsoluteRightVectorToTarget);
    end
    else
    begin
      SetVector(camUp, Camera.AbsoluteUp);
      SetVector(camRight, Camera.AbsoluteRight);
    end;
    f := 100 * FCamera.NearPlaneBias / (FCamera.FocalLength *
      FCamera.SceneScale);
    if FViewPort.Width > FViewPort.Height then
      f := f / FViewPort.Width
    else
      f := f / FViewPort.Height;
    SetVector(Result,
      VectorCombine3(camPos, camUp, camRight, 1,
      (screenY - (FViewPort.Height div 2)) * f,
      (screenX - (FViewPort.Width div 2)) * f));
  end
  else
    Result := NullVector;
end;

function TGLSceneBuffer.ScreenToWorld(const aPoint: TAffineVector):
  TAffineVector;
var
  rslt: TGLVector;
begin
  if Assigned(FCamera)
    and UnProject(
    VectorMake(aPoint),
    RenderingContext.PipelineTransformation.ViewProjectionMatrix^,
    PHomogeneousIntVector(@FViewPort)^, rslt) then
    Result := Vector3fMake(rslt)
  else
    Result := aPoint;
end;

function TGLSceneBuffer.ScreenToWorld(const aPoint: TGLVector): TGLVector;
begin
  MakePoint(Result, ScreenToWorld(AffineVectorMake(aPoint)));
end;

function TGLSceneBuffer.ScreenToWorld(screenX, screenY: Integer): TAffineVector;
begin
  Result := ScreenToWorld(AffineVectorMake(screenX, FViewPort.Height - screenY,
    0));
end;

function TGLSceneBuffer.WorldToScreen(const aPoint: TAffineVector): TAffineVector;
var
  rslt: TGLVector;
begin
  RenderingContext.Activate;
  try
    PrepareRenderingMatrices(FViewPort, FRenderDPI);
    if Assigned(FCamera)
      and Project(
      VectorMake(aPoint),
      RenderingContext.PipelineTransformation.ViewProjectionMatrix^,
      TVector4i(FViewPort),
      rslt) then
      Result := Vector3fMake(rslt)
    else
      Result := aPoint;
  finally
    RenderingContext.Deactivate;
  end;
end;

function TGLSceneBuffer.WorldToScreen(const aPoint: TGLVector): TGLVector;
begin
  SetVector(Result, WorldToScreen(AffineVectorMake(aPoint)));
end;

procedure TGLSceneBuffer.WorldToScreen(points: PGLVector; nbPoints: Integer);
var
  i: Integer;
begin
  if Assigned(FCamera) then
  begin
    for i := nbPoints - 1 downto 0 do
    begin
      Project(points^, RenderingContext.PipelineTransformation.ViewProjectionMatrix^, PHomogeneousIntVector(@FViewPort)^, points^);
      Inc(points);
    end;
  end;
end;

function TGLSceneBuffer.ScreenToVector(const aPoint: TAffineVector):
  TAffineVector;
begin
  Result := VectorSubtract(ScreenToWorld(aPoint),
    PAffineVector(@FCameraAbsolutePosition)^);
end;

function TGLSceneBuffer.ScreenToVector(const aPoint: TGLVector): TGLVector;
begin
  SetVector(Result, VectorSubtract(ScreenToWorld(aPoint),
    FCameraAbsolutePosition));
  Result.W := 0;
end;

function TGLSceneBuffer.ScreenToVector(const x, y: Integer): TGLVector;
var
  av: TAffineVector;
begin
  av.X := x;
  av.Y := y;
  av.Z := 0;
  SetVector(Result, ScreenToVector(av));
end;

function TGLSceneBuffer.VectorToScreen(const VectToCam: TAffineVector):
  TAffineVector;
begin
  Result := WorldToScreen(VectorAdd(VectToCam,
    PAffineVector(@FCameraAbsolutePosition)^));
end;

function TGLSceneBuffer.ScreenVectorIntersectWithPlane(
  const aScreenPoint: TGLVector;
  const planePoint, planeNormal: TGLVector;
  var intersectPoint: TGLVector): Boolean;
var
  v: TGLVector;
begin
  if Assigned(FCamera) then
  begin
    SetVector(v, ScreenToVector(aScreenPoint));
    Result := RayCastPlaneIntersect(FCameraAbsolutePosition,
      v, planePoint, planeNormal, @intersectPoint);
    intersectPoint.W := 1;
  end
  else
    Result := False;
end;

function TGLSceneBuffer.ScreenVectorIntersectWithPlaneXY(
  const aScreenPoint: TGLVector; const z: Single;
  var intersectPoint: TGLVector): Boolean;
begin
  Result := ScreenVectorIntersectWithPlane(aScreenPoint, VectorMake(0, 0, z),
    ZHmgVector, intersectPoint);
  intersectPoint.W := 0;
end;

function TGLSceneBuffer.ScreenVectorIntersectWithPlaneYZ(
  const aScreenPoint: TGLVector; const x: Single;
  var intersectPoint: TGLVector): Boolean;
begin
  Result := ScreenVectorIntersectWithPlane(aScreenPoint, VectorMake(x, 0, 0),
    XHmgVector, intersectPoint);
  intersectPoint.W := 0;
end;

function TGLSceneBuffer.ScreenVectorIntersectWithPlaneXZ(
  const aScreenPoint: TGLVector; const y: Single;
  var intersectPoint: TGLVector): Boolean;
begin
  Result := ScreenVectorIntersectWithPlane(aScreenPoint, VectorMake(0, y, 0),
    YHmgVector, intersectPoint);
  intersectPoint.W := 0;
end;

function TGLSceneBuffer.PixelRayToWorld(x, y: Integer): TAffineVector;
var
  dov, np, fp, z, dst, wrpdst: Single;
  vec, cam, targ, rayhit, pix: TAffineVector;
  camAng: real;
begin
  if Camera.CameraStyle = csOrtho2D then
    dov := 2
  else
    dov := Camera.DepthOfView;
  np := Camera.NearPlane;
  fp := Camera.NearPlane + dov;
  z := GetPixelDepth(x, y);
  dst := (fp * np) / (fp - z * dov); //calc from z-buffer value to world depth
  //------------------------
  //z:=1-(fp/d-1)/(fp/np-1);  //calc from world depth to z-buffer value
  //------------------------
  vec.X := x;
  vec.Y := FViewPort.Height - y;
  vec.Z := 0;
  vec := ScreenToVector(vec);
  NormalizeVector(vec);
  SetVector(cam, Camera.AbsolutePosition);
  //targ:=Camera.TargetObject.Position.AsAffineVector;
  //SubtractVector(targ,cam);
  pix.X := FViewPort.Width * 0.5;
  pix.Y := FViewPort.Height * 0.5;
  pix.Z := 0;
  targ := self.ScreenToVector(pix);

  camAng := VectorAngleCosine(targ, vec);
  wrpdst := dst / camAng;
  rayhit := cam;
  CombineVector(rayhit, vec, wrpdst);
  result := rayhit;
end;

procedure TGLSceneBuffer.ClearBuffers;
var
  bufferBits: TGLBitfield;
begin
  if roNoDepthBufferClear in ContextOptions then
    bufferBits := 0
  else
  begin
    bufferBits := GL_DEPTH_BUFFER_BIT;
    CurrentGLContext.GLStates.DepthWriteMask := True;
  end;
  if ContextOptions * [roNoColorBuffer, roNoColorBufferClear] = [] then
  begin
    bufferBits := bufferBits or GL_COLOR_BUFFER_BIT;
    CurrentGLContext.GLStates.SetColorMask(cAllColorComponents);
  end;
  if roStencilBuffer in ContextOptions then
  begin
    bufferBits := bufferBits or GL_STENCIL_BUFFER_BIT;
  end;
  if bufferBits<>0 then
    gl.Clear(BufferBits);
end;


procedure TGLSceneBuffer.NotifyChange(Sender: TObject);
begin
  DoChange;
end;

procedure TGLSceneBuffer.PickObjects(const rect: TRect; pickList: TGLPickList; objectCountGuess: Integer);
var
  I: Integer;
  obj: TGLBaseSceneObject;
begin
  if not Assigned(FCamera) then
    Exit;
  Assert((not FRendering), strAlreadyRendering);
  Assert(Assigned(PickList));
  FRenderingContext.Activate;
  FRendering := True;
  try
    // Creates best selector which techniques is hardware can do
    if not Assigned(FSelector) then
      FSelector := GetBestSelectorClass.Create;

    xgl.MapTexCoordToNull; // turn off
    PrepareRenderingMatrices(FViewPort, RenderDPI, @Rect);
    FSelector.Hits := -1;
    if objectCountGuess > 0 then
      FSelector.ObjectCountGuess := objectCountGuess;
    repeat
      FSelector.Start;
      // render the scene (in select mode, nothing is drawn)
      FRenderDPI := 96;
      if Assigned(FCamera) and Assigned(FCamera.FScene) then
        RenderScene(FCamera.FScene, FViewPort.Width, FViewPort.Height,
          dsPicking, nil);
    until FSelector.Stop;
    FSelector.FillPickingList(PickList);
    for I := 0 to PickList.Count-1 do
    begin
      obj := TGLBaseSceneObject(PickList[I]);
      if Assigned(obj.FOnPicked) then
        obj.FOnPicked(obj);
    end;
  finally
    FRendering := False;
    FRenderingContext.Deactivate;
  end;
end;

function TGLSceneBuffer.GetPickedObjects(const rect: TRect; objectCountGuess:
  Integer = 64): TGLPickList;
begin
  Result := TGLPickList.Create(psMinDepth);
  PickObjects(Rect, Result, objectCountGuess);
end;

function TGLSceneBuffer.GetPickedObject(x, y: Integer): TGLBaseSceneObject;
var
  pkList: TGLPickList;
begin
  pkList := GetPickedObjects(Rect(x - 1, y - 1, x + 1, y + 1));
  try
    if pkList.Count > 0 then
      Result := TGLBaseSceneObject(pkList.Hit[0])
    else
      Result := nil;
  finally
    pkList.Free;
  end;
end;

function TGLSceneBuffer.GetPixelColor(x, y: Integer): TColor;
var
  buf: array[0..2] of Byte;
begin
  if not Assigned(FCamera) then
  begin
    Result := 0;
    Exit;
  end;
  FRenderingContext.Activate;
  try
    gl.ReadPixels(x, FViewPort.Height - y, 1, 1, GL_RGB, GL_UNSIGNED_BYTE,  @buf[0]);
  finally
    FRenderingContext.Deactivate;
  end;
  Result := RGB2Color(buf[0], buf[1], buf[2]);
end;

function TGLSceneBuffer.GetPixelDepth(x, y: Integer): Single;
begin
  if not Assigned(FCamera) then
  begin
    Result := 0;
    Exit;
  end;
  FRenderingContext.Activate;
  try
    gl.ReadPixels(x, FViewPort.Height - y, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT,
      @Result);
  finally
    FRenderingContext.Deactivate;
  end;
end;

function TGLSceneBuffer.PixelDepthToDistance(aDepth: Single): Single;
var
  dov, np, fp: Single;
begin
  if Camera.CameraStyle = csOrtho2D then
    dov := 2
  else
    dov := Camera.DepthOfView; // Depth of View (from np to fp)
  np := Camera.NearPlane; // Near plane distance
  fp := np + dov; // Far plane distance
  Result := (fp * np) / (fp - aDepth * dov);
  // calculate world distance from z-buffer value
end;

function TGLSceneBuffer.PixelToDistance(x, y: integer): Single;
var
  z, dov, np, fp, dst, camAng: Single;
  norm, coord, vec: TAffineVector;
begin
  z := GetPixelDepth(x, y);
  if Camera.CameraStyle = csOrtho2D then
    dov := 2
  else
    dov := Camera.DepthOfView; // Depth of View (from np to fp)
  np := Camera.NearPlane; // Near plane distance
  fp := np + dov; // Far plane distance
  dst := (np * fp) / (fp - z * dov);
  //calculate from z-buffer value to frustrum depth
  coord.X := x;
  coord.Y := y;
  vec := self.ScreenToVector(coord); //get the pixel vector
  coord.X := FViewPort.Width div 2;
  coord.Y := FViewPort.Height div 2;
  norm := self.ScreenToVector(coord); //get the absolute camera direction
  camAng := VectorAngleCosine(norm, vec);
  Result := dst / camAng; //compensate for flat frustrum face
end;

procedure TGLSceneBuffer.NotifyMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  // Nothing
end;

procedure TGLSceneBuffer.PrepareRenderingMatrices(const aViewPort: TRectangle;
  resolution: Integer; pickingRect: PRect = nil);
begin
  RenderingContext.PipelineTransformation.IdentityAll;
  // setup projection matrix
  if Assigned(pickingRect) then
  begin
    CurrentGLContext.PipelineTransformation.SetProjectionMatrix(
      CreatePickMatrix(
      (pickingRect^.Left + pickingRect^.Right) div 2,
      FViewPort.Height - ((pickingRect^.Top + pickingRect^.Bottom) div 2),
      Abs(pickingRect^.Right - pickingRect^.Left),
      Abs(pickingRect^.Bottom - pickingRect^.Top),
      TVector4i(FViewport)));
  end;
  FBaseProjectionMatrix := CurrentGLContext.PipelineTransformation.ProjectionMatrix^;

  if Assigned(FCamera) then
  begin
    FCamera.Scene.FCurrentGLCamera := FCamera;
    // apply camera perpective
    FCamera.ApplyPerspective(
      aViewport,
      FViewPort.Width,
      FViewPort.Height,
      resolution);
    // setup model view matrix
    // apply camera transformation (viewpoint)
    FCamera.Apply;
    FCameraAbsolutePosition := FCamera.AbsolutePosition;
  end;
end;

procedure TGLSceneBuffer.DoBaseRender(const aViewPort: TRectangle; resolution:
  Integer;
  drawState: TGLDrawState; baseObject: TGLBaseSceneObject);
begin
  with RenderingContext.GLStates do
  begin
    PrepareRenderingMatrices(aViewPort, resolution);
    (* if not ForwardContext then *)
    begin
      xgl.MapTexCoordToNull; // force XGL rebind
      xgl.MapTexCoordToMain;
    end;

    if Assigned(FViewerBeforeRender) and (drawState <> dsPrinting) then
      FViewerBeforeRender(Self);
    if Assigned(FBeforeRender) then
      if Owner is TComponent then
        if not (csDesigning in TComponent(Owner).ComponentState) then
          FBeforeRender(Self);

    if Assigned(FCamera) and Assigned(FCamera.FScene) then
    begin
      with FCamera.FScene do
      begin
        SetupLights(MaxLights);
        (* if not ForwardContext then *)
        begin
          if FogEnable then
          begin
            Enable(stFog);
            FogEnvironment.ApplyFog;
          end
          else
            Disable(stFog);
        end;

        RenderScene(FCamera.FScene, aViewPort.Width, aViewPort.Height,
          drawState,
          baseObject);
      end;
    end;
    if Assigned(FPostRender) then
      if Owner is TComponent then
        if not (csDesigning in TComponent(Owner).ComponentState) then
          FPostRender(Self);
  end;
  Assert(Length(FViewMatrixStack) = 0,
    'Unbalance Push/PopViewMatrix.');
  Assert(Length(FProjectionMatrixStack) = 0,
    'Unbalance Push/PopProjectionMatrix.');
end;

procedure TGLSceneBuffer.Render;
begin
  Render(nil);
end;

procedure TGLSceneBuffer.Render(baseObject: TGLBaseSceneObject);
var
  perfCounter, framePerf: Int64;
begin
  if FRendering then
    Exit;
  if not Assigned(FRenderingContext) then
    Exit;

  if Freezed and (FFreezeBuffer <> nil) then
  begin
    RenderingContext.Activate;
    try
      RenderingContext.GLStates.ColorClearValue :=
        ConvertWinColor(FBackgroundColor, FBackgroundAlpha);
      ClearBuffers;
      gl.MatrixMode(GL_PROJECTION);
      gl.LoadIdentity;
      gl.MatrixMode(GL_MODELVIEW);
      gl.LoadIdentity;
      gl.RasterPos2f(-1, -1);
      gl.DrawPixels(FFreezedViewPort.Width, FFreezedViewPort.Height,
        GL_RGBA, GL_UNSIGNED_BYTE, FFreezeBuffer);
      if not (roNoSwapBuffers in ContextOptions) then
        RenderingContext.SwapBuffers;
    finally
      RenderingContext.Deactivate;
    end;
    Exit;
  end;

  QueryPerformanceCounter(framePerf);

  if Assigned(FCamera) and Assigned(FCamera.FScene) then
  begin
    FCamera.AbsoluteMatrixAsAddress;
    FCamera.FScene.AddBuffer(Self);
  end;

  FRendering := True;
  try
    FRenderingContext.Activate;
    try
      if FFrameCount = 0 then
        QueryPerformanceCounter(FFirstPerfCounter);

      FRenderDPI := 96; // default value for screen
      gl.ClearError;
      SetupRenderingContext(FRenderingContext);
      // clear the buffers
      FRenderingContext.GLStates.ColorClearValue :=
        ConvertWinColor(FBackgroundColor, FBackgroundAlpha);
      ClearBuffers;
      gl.CheckError;
      // render
      DoBaseRender(FViewport, RenderDPI, dsRendering, baseObject);

      if not (roNoSwapBuffers in ContextOptions) then
        RenderingContext.SwapBuffers;

      // yes, calculate average frames per second...
      Inc(FFrameCount);
      QueryPerformanceCounter(perfCounter);
      FLastFrameTime := (perfCounter - framePerf) / vCounterFrequency;
      Dec(perfCounter, FFirstPerfCounter);
      if perfCounter > 0 then
        FFramesPerSecond := (FFrameCount * vCounterFrequency) / perfCounter;
      gl.CheckError;
    finally
      FRenderingContext.Deactivate;
    end;
    if Assigned(FAfterRender) and (Owner is TComponent) then
      if not (csDesigning in TComponent(Owner).ComponentState) then
        FAfterRender(Self);
  finally
    FRendering := False;
  end;
end;

procedure TGLSceneBuffer.RenderScene(aScene: TGLScene;
  const viewPortSizeX, viewPortSizeY: Integer;
  drawState: TGLDrawState;
  baseObject: TGLBaseSceneObject);

var
  i: Integer;
  rci: TGLRenderContextInfo;
  rightVector: TGLVector;
begin
  FAfterRenderEffects.Clear;
  aScene.FCurrentBuffer := Self;
  FillChar(rci, SizeOf(rci), 0);
  rci.scene := aScene;
  rci.buffer := Self;
  rci.afterRenderEffects := FAfterRenderEffects;
  rci.objectsSorting := aScene.ObjectsSorting;
  rci.visibilityCulling := aScene.VisibilityCulling;
  rci.bufferFaceCull := FFaceCulling;
  rci.bufferLighting := FLighting;
  rci.bufferFog := FFogEnable;
  rci.bufferDepthTest := FDepthTest;
  rci.drawState := drawState;
  rci.sceneAmbientColor := FAmbientColor.Color;
  rci.primitiveMask := cAllMeshPrimitive;
  with FCamera do
  begin
    rci.cameraPosition := FCameraAbsolutePosition;
    rci.cameraDirection := FLastDirection;
    NormalizeVector(rci.cameraDirection);
    rci.cameraDirection.W := 0;
    rightVector := VectorCrossProduct(rci.cameraDirection, Up.AsVector);
    rci.cameraUp := VectorCrossProduct(rightVector, rci.cameraDirection);
    NormalizeVector(rci.cameraUp);

    with rci.rcci do
    begin
      origin := rci.cameraPosition;
      clippingDirection := rci.cameraDirection;
      viewPortRadius := FViewPortRadius;
      nearClippingDistance := FNearPlane;
      farClippingDistance := FNearPlane + FDepthOfView;
      frustum := RenderingContext.PipelineTransformation.Frustum;
    end;
  end;
  rci.viewPortSize.cx := viewPortSizeX;
  rci.viewPortSize.cy := viewPortSizeY;
  rci.renderDPI := FRenderDPI;
  rci.GLStates := RenderingContext.GLStates;
  rci.PipelineTransformation := RenderingContext.PipelineTransformation;
  rci.proxySubObject := False;
  rci.ignoreMaterials := (roNoColorBuffer in FContextOptions)
    or (rci.drawState = dsPicking);
  rci.amalgamating := rci.drawState = dsPicking;
  rci.GLStates.SetGLColorWriting(not rci.ignoreMaterials);
  if Assigned(FInitiateRendering) then
    FInitiateRendering(Self, rci);

  if aScene.InitializableObjects.Count <> 0 then
  begin
    // First initialize all objects and delete them from the list.
    for I := aScene.InitializableObjects.Count - 1 downto 0 do
    begin
      aScene.InitializableObjects.Items[I].InitializeObject({Self?}aScene, rci);
      aScene.InitializableObjects.Delete(I);
    end;
  end;

  if RenderingContext.IsPraparationNeed then
    RenderingContext.PrepareHandlesData;

  if baseObject = nil then
  begin
    aScene.Objects.Render(rci);
  end
  else
    baseObject.Render(rci);
  rci.GLStates.SetGLColorWriting(True);
  with FAfterRenderEffects do
    if Count > 0 then
      for i := 0 to Count - 1 do
        TGLObjectAfterEffect(Items[i]).Render(rci);
  if Assigned(FWrapUpRendering) then
    FWrapUpRendering(Self, rci);
end;

procedure TGLSceneBuffer.SetBackgroundColor(AColor: TColor);
begin
  if FBackgroundColor <> AColor then
  begin
    FBackgroundColor := AColor;
    NotifyChange(Self);
  end;
end;

procedure TGLSceneBuffer.SetBackgroundAlpha(alpha: Single);
begin
  if FBackgroundAlpha <> alpha then
  begin
    FBackgroundAlpha := alpha;
    NotifyChange(Self);
  end;
end;

procedure TGLSceneBuffer.SetAmbientColor(AColor: TGLColor);
begin
  FAmbientColor.Assign(AColor);
end;

procedure TGLSceneBuffer.SetCamera(ACamera: TGLCamera);
begin
  if FCamera <> ACamera then
  begin
    if Assigned(FCamera) then
    begin
      if Assigned(FCamera.FScene) then
        FCamera.FScene.RemoveBuffer(Self);
      FCamera := nil;
    end;
    if Assigned(ACamera) and Assigned(ACamera.FScene) then
    begin
      FCamera := ACamera;
      FCamera.TransformationChanged;
    end;
    NotifyChange(Self);
  end;
end;

procedure TGLSceneBuffer.SetContextOptions(Options: TGLContextOptions);
begin
  if FContextOptions <> Options then
  begin
    FContextOptions := Options;
    DoStructuralChange;
  end;
end;

procedure TGLSceneBuffer.SetDepthTest(AValue: Boolean);
begin
  if FDepthTest <> AValue then
  begin
    FDepthTest := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLSceneBuffer.SetFaceCulling(AValue: Boolean);
begin
  if FFaceCulling <> AValue then
  begin
    FFaceCulling := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLSceneBuffer.SetLayer(const Value: TGLContextLayer);
begin
  if FLayer <> Value then
  begin
    FLayer := Value;
    DoStructuralChange;
  end;
end;

procedure TGLSceneBuffer.SetLighting(aValue: Boolean);
begin
  if FLighting <> aValue then
  begin
    FLighting := aValue;
    NotifyChange(Self);
  end;
end;

procedure TGLSceneBuffer.SetAntiAliasing(const val: TGLAntiAliasing);
begin
  if FAntiAliasing <> val then
  begin
    FAntiAliasing := val;
    DoStructuralChange;
  end;
end;

procedure TGLSceneBuffer.SetDepthPrecision(const val: TGLDepthPrecision);
begin
  if FDepthPrecision <> val then
  begin
    FDepthPrecision := val;
    DoStructuralChange;
  end;
end;

procedure TGLSceneBuffer.SetColorDepth(const val: TGLColorDepth);
begin
  if FColorDepth <> val then
  begin
    FColorDepth := val;
    DoStructuralChange;
  end;
end;

procedure TGLSceneBuffer.SetShadeModel(const val: TGLShadeModel);
begin
  if FShadeModel <> val then
  begin
    FShadeModel := val;
    NotifyChange(Self);
  end;
end;

procedure TGLSceneBuffer.SetFogEnable(AValue: Boolean);
begin
  if FFogEnable <> AValue then
  begin
    FFogEnable := AValue;
    NotifyChange(Self);
  end;
end;

procedure TGLSceneBuffer.SetGLFogEnvironment(AValue: TGLFogEnvironment);
begin
  FFogEnvironment.Assign(AValue);
  NotifyChange(Self);
end;

function TGLSceneBuffer.StoreFog: Boolean;
begin
  Result := (not FFogEnvironment.IsAtDefaultValues);
end;

procedure TGLSceneBuffer.SetAccumBufferBits(const val: Integer);
begin
  if FAccumBufferBits <> val then
  begin
    FAccumBufferBits := val;
    DoStructuralChange;
  end;
end;

procedure TGLSceneBuffer.DoChange;
begin
  if (not FRendering) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TGLSceneBuffer.DoStructuralChange;
var
  bCall: Boolean;
begin
  if Assigned(Owner) then
    bCall := not (csLoading in TComponent(GetOwner).ComponentState)
  else
    bCall := True;
  if bCall and Assigned(FOnStructuralChange) then
    FOnStructuralChange(Self);
end;

// ------------------
// ------------------ TGLNonVisualViewer ------------------
// ------------------
constructor TGLNonVisualViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidth := 256;
  FHeight := 256;
  FBuffer := TGLSceneBuffer.Create(Self);
  FBuffer.OnChange := DoBufferChange;
  FBuffer.OnStructuralChange := DoBufferStructuralChange;
  FBuffer.OnPrepareGLContext := DoOnPrepareGLContext;
end;

destructor TGLNonVisualViewer.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

procedure TGLNonVisualViewer.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if (Operation = opRemove) and (AComponent = Camera) then
    Camera := nil;
  inherited;
end;

procedure TGLNonVisualViewer.CopyToTexture(aTexture: TGLTexture);
begin
  CopyToTexture(aTexture, 0, 0, Width, Height, 0, 0);
end;

procedure TGLNonVisualViewer.CopyToTexture(aTexture: TGLTexture;
  xSrc, ySrc, width, height: Integer;
  xDest, yDest: Integer);
begin
  Buffer.CopyToTexture(aTexture, xSrc, ySrc, width, height, xDest, yDest);
end;

procedure TGLNonVisualViewer.CopyToTextureMRT(aTexture: TGLTexture;
  BufferIndex: integer);
begin
  CopyToTextureMRT(aTexture, 0, 0, Width, Height, 0, 0, BufferIndex);
end;

procedure TGLNonVisualViewer.CopyToTextureMRT(aTexture: TGLTexture; xSrc,
  ySrc, width, height, xDest, yDest, BufferIndex: integer);
var
  target, handle: Integer;
  buf: Pointer;
  createTexture: Boolean;

  procedure CreateNewTexture;
  begin
    GetMem(buf, Width * Height * 4);
    try // float_type
      gl.ReadPixels(0, 0, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, buf);
      case aTexture.MinFilter of
        miNearest, miLinear:
          gl.TexImage2d(target, 0, aTexture.OpenGLTextureFormat, Width, Height,
            0, GL_RGBA, GL_UNSIGNED_BYTE, buf);
      else
        if gl.SGIS_generate_mipmap and (target = GL_TEXTURE_2D) then
        begin
          // hardware-accelerated when supported
          gl.TexParameteri(target, GL_GENERATE_MIPMAP_SGIS, GL_TRUE);
          gl.TexImage2d(target, 0, aTexture.OpenGLTextureFormat, Width, Height,
            0, GL_RGBA, GL_UNSIGNED_BYTE, buf);
        end
        else
        begin
          gl.TexImage2d(target, 0, aTexture.OpenGLTextureFormat, Width, Height,
            0, GL_RGBA, GL_UNSIGNED_BYTE, buf);
          gl.GenerateMipmap(target);
        end;
      end;
    finally
      FreeMem(buf);
    end;
  end;

begin
  if Buffer.RenderingContext <> nil then
  begin
    Buffer.RenderingContext.Activate;
    try
      target := DecodeTextureTarget(aTexture.Image.NativeTextureTarget);

      CreateTexture := true;

      if aTexture.IsFloatType then
      begin // float_type special treatment
        CreateTexture := false;
        handle := aTexture.Handle;
      end
      else if (target <> GL_TEXTURE_CUBE_MAP_ARB) or (FCubeMapRotIdx = 0) then
      begin
        CreateTexture := not aTexture.IsHandleAllocated;
        if CreateTexture then
          handle := aTexture.AllocateHandle
        else
          handle := aTexture.Handle;
      end
      else
        handle := aTexture.Handle;

      // For MRT
      gl.ReadBuffer(MRT_BUFFERS[BufferIndex]);

      Buffer.RenderingContext.GLStates.TextureBinding[0,
        EncodeGLTextureTarget(target)] := handle;

      if target = GL_TEXTURE_CUBE_MAP_ARB then
        target := GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB + FCubeMapRotIdx;

      if CreateTexture then
        CreateNewTexture
      else
        gl.CopyTexSubImage2D(target, 0, xDest, yDest, xSrc, ySrc, Width, Height);

      gl.ClearError;
    finally
      Buffer.RenderingContext.Deactivate;
    end;
  end;
end;

procedure TGLNonVisualViewer.SetupCubeMapCamera(Sender: TObject);

(*
const
  cFaceMat: array[0..5] of TGLMatrix =
  (
    (X: (X:0; Y:0; Z:-1; W:0);
     Y: (X:0; Y:-1; Z:0; W:0);
     Z: (X:-1; Y:0; Z:0; W:0);
     W: (X:0; Y:0; Z:0; W:1)),
    (X:(X:2.4335928828e-08; Y:0; Z:1; W:0);
     Y:(X:0; Y:-1; Z:0; W:0);
     Z:(X:1; Y:0; Z:-2.4335928828e-08; W:0);
     W:(X:0; Y:0; Z:0; W:1)),
    (X:(X:1; Y:1.2167964414e-08; Z:-1.4805936071e-16; W:0);
     Y:(X:0; Y:-1.2167964414e-08; Z:-1; W:0);
     Z:(X:-1.2167964414e-08; Y:1; Z:-1.2167964414e-08; W:0);
     W:(X:0; Y:0; Z:0; W:1)),
    (X:(X:1; Y:-1.2167964414e-08; Z:-1.4805936071e-16; W:0);
     Y:(X:0; Y:-1.2167964414e-08; Z:1; W:0);
     Z:(X:-1.2167964414e-08; Y:-1; Z:-1.2167964414e-08; W:0);
     W:(X:0; Y:0; Z:0; W:1)),
    (X:(X:1; Y:0; Z:-1.2167964414e-08; W:0);
     Y:(X:0; Y:-1; Z:0; W:0);
     Z:(X:-1.2167964414e-08; Y:0; Z:-1; W:0);
     W:(X:0; Y:0; Z:0; W:1)),
    (X:(X:-1; Y:0; Z:-1.2167964414e-08; W:0);
     Y:(X:0; Y:-1; Z:0; W:0);
     Z:(X:-1.2167964414e-08; Y:0; Z:1; W:0);
     W:(X:0; Y:0; Z:0; W:1))
  );
*)

var
  TM: TGLMatrix;
begin
  // Setup appropriate FOV
  with CurrentGLContext.PipelineTransformation do
  begin
    SetProjectionMatrix(CreatePerspectiveMatrix(90, 1, FCubeMapZNear, FCubeMapZFar));
    TM := CreateTranslationMatrix(FCubeMapTranslation);
  (* SetViewMatrix(MatrixMultiply(cFaceMat[FCubeMapRotIdx], TM)); *)
  end;
end;

procedure TGLNonVisualViewer.RenderCubeMapTextures(cubeMapTexture: TGLTexture;
  zNear: Single = 0;
  zFar: Single = 0);
var
  oldEvent: TNotifyEvent;
begin
  Assert((Width = Height), 'Memory Viewer must render to a square!');
  Assert(Assigned(FBuffer.FCamera), 'Camera not specified');
  Assert(Assigned(cubeMapTexture), 'Texture not specified');

  if zFar <= 0 then
    zFar := FBuffer.FCamera.DepthOfView;
  if zNear <= 0 then
    zNear := zFar * 0.001;

  oldEvent := FBuffer.FCamera.FDeferredApply;
  FBuffer.FCamera.FDeferredApply := SetupCubeMapCamera;
  FCubeMapZNear := zNear;
  FCubeMapZFar := zFar;
  VectorScale(FBuffer.FCamera.AbsolutePosition, -1, FCubeMapTranslation);
  try
    FCubeMapRotIdx := 0;
    while FCubeMapRotIdx < 6 do
    begin
      Render;
      Buffer.CopyToTexture(cubeMapTexture, 0, 0, Width, Height, 0, 0,
        GL_TEXTURE_CUBE_MAP_POSITIVE_X + FCubeMapRotIdx);
      Inc(FCubeMapRotIdx);
    end;
  finally
    FBuffer.FCamera.FDeferredApply := oldEvent;
  end;
end;

procedure TGLNonVisualViewer.SetBeforeRender(const val: TNotifyEvent);
begin
  FBuffer.BeforeRender := val;
end;

function TGLNonVisualViewer.GetBeforeRender: TNotifyEvent;
begin
  Result := FBuffer.BeforeRender;
end;

procedure TGLNonVisualViewer.SetPostRender(const val: TNotifyEvent);
begin
  FBuffer.PostRender := val;
end;

function TGLNonVisualViewer.GetPostRender: TNotifyEvent;
begin
  Result := FBuffer.PostRender;
end;

procedure TGLNonVisualViewer.SetAfterRender(const val: TNotifyEvent);
begin
  FBuffer.AfterRender := val;
end;

function TGLNonVisualViewer.GetAfterRender: TNotifyEvent;
begin
  Result := FBuffer.AfterRender;
end;

procedure TGLNonVisualViewer.SetCamera(const val: TGLCamera);
begin
  FBuffer.Camera := val;
end;

function TGLNonVisualViewer.GetCamera: TGLCamera;
begin
  Result := FBuffer.Camera;
end;

procedure TGLNonVisualViewer.SetBuffer(const val: TGLSceneBuffer);
begin
  FBuffer.Assign(val);
end;

procedure TGLNonVisualViewer.DoOnPrepareGLContext(sender: TObject);
begin
  PrepareGLContext;
end;

procedure TGLNonVisualViewer.PrepareGLContext;
begin
  // nothing, reserved for subclasses
end;

procedure TGLNonVisualViewer.DoBufferChange(Sender: TObject);
begin
  // nothing, reserved for subclasses
end;

procedure TGLNonVisualViewer.DoBufferStructuralChange(Sender: TObject);
begin
  FBuffer.DestroyRC;
end;

procedure TGLNonVisualViewer.SetWidth(const val: Integer);
begin
  if val <> FWidth then
  begin
    FWidth := val;
    if FWidth < 1 then
      FWidth := 1;
    DoBufferStructuralChange(Self);
  end;
end;

procedure TGLNonVisualViewer.SetHeight(const val: Integer);
begin
  if val <> FHeight then
  begin
    FHeight := val;
    if FHeight < 1 then
      FHeight := 1;
    DoBufferStructuralChange(Self);
  end;
end;

// ------------------
// ------------------ TGLMemoryViewer ------------------
// ------------------
constructor TGLMemoryViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 256;
  Height := 256;
  FBufferCount := 1;
end;

procedure TGLMemoryViewer.InstantiateRenderingContext;
begin
  if FBuffer.RenderingContext = nil then
  begin
    FBuffer.SetViewPort(0, 0, Width, Height);
    FBuffer.CreateRC(HWND(0), True, FBufferCount);
  end;
end;

procedure TGLMemoryViewer.Render(baseObject: TGLBaseSceneObject = nil);
begin
  InstantiateRenderingContext;
  FBuffer.Render(baseObject);
end;

procedure TGLMemoryViewer.SetBufferCount(const Value: integer);
const
  MaxAxuBufCount = 4; // Current hardware limit = 4
begin
  if FBufferCount = Value then
    exit;
  FBufferCount := Value;

  if FBufferCount < 1 then
    FBufferCount := 1;

  if FBufferCount > MaxAxuBufCount then
    FBufferCount := MaxAxuBufCount;

  // Request a new Instantiation of RC on next render
  FBuffer.DestroyRC;
end;

// ------------------
// ------------------ TGLInitializableObjectList ------------------
// ------------------
function TGLInitializableObjectList.Add(const Item: IGLInitializable): Integer;
begin
  Result := inherited Add(Pointer(Item));
end;

function TGLInitializableObjectList.GetItems(
  const Index: Integer): IGLInitializable;
begin
  Result := IGLInitializable(inherited Get(Index));
end;

procedure TGLInitializableObjectList.PutItems(const Index: Integer;
  const Value: IGLInitializable);
begin
  inherited Put(Index, Pointer(Value));
end;

//------------------------------------------------------------------------------
initialization
//------------------------------------------------------------------------------

  RegisterClasses([TGLLightSource, TGLCamera, TGLProxyObject,
    TGLScene, TGLDirectOpenGL, TGLRenderPoint, TGLMemoryViewer]);

  // preparation for high resolution timer
  QueryPerformanceFrequency(vCounterFrequency);

end.

