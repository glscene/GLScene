//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.Objects;

(*
  Implementation of basic scene objects plus some management routines:
  - TGLDummyCube, TGLPlane, TGLSprite, TGLPoints, TGLLines, TGLCube,
    TGLSphere, TGLPolygonBase, TGLSuperellipsoid.

  All objects declared in this unit are part of the basic GLScene package,
  these are only simple objects and should be kept simple and lightweight.

  More complex or more specialized versions should be placed in dedicated
  units where they can grow and prosper untammed. "Generic" geometrical
  objects can be found GLS.GeomObjects.
*)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Types,
  System.Classes,
  System.SysUtils,
  System.Math,

  GLS.OpenGLTokens,
  GLS.OpenGLAdapter,
  GLS.VectorTypes,
  GLS.VectorLists,
  GLS.VectorGeometry,
  GLS.Spline,
  GLS.Scene,
  GLS.PipelineTransformation,
  GLS.Context,
  GLS.Silhouette,
  GLS.Color,
  GLS.RenderContextInfo,
  GLS.PersistentClasses,
  GLS.BaseClasses,
  GLS.Nodes,
  GLS.Coordinates,
  GLS.XOpenGL,
  GLS.State;

const
  cDefaultPointSize: Single = 1.0;

type

  TGLVisibilityDeterminationEvent = function(Sender: TObject;
    var rci: TGLRenderContextInfo): Boolean of object;

  PGLVertexRec = ^TGLVertexRec;
  TGLVertexRec = record
    Position: TVector3f;
    Normal: TVector3f;
    Binormal: TVector3f;
    Tangent: TVector3f;
    TexCoord: TVector2f;
  end;

  (* A simple cube, invisible at run-time.
    This is a usually non-visible object -except at design-time- used for
    building hierarchies or groups, when some kind of joint or movement
    mechanism needs be described, you can use DummyCubes.
    DummyCube's barycenter is its children's barycenter.
    The DummyCube can optionnally amalgamate all its children into a single
    display list (see Amalgamate property). *)
  TGLDummyCube = class(TGLCameraInvariantObject)
  private
    FCubeSize: TGLFloat;
    FEdgeColor: TGLColor;
    FVisibleAtRunTime, FAmalgamate: Boolean;
    FGroupList: TGLListHandle;
    FOnVisibilityDetermination: TGLVisibilityDeterminationEvent;
  protected
    procedure SetCubeSize(const val: TGLFloat); inline;
    procedure SetEdgeColor(const val: TGLColor); inline;
    procedure SetVisibleAtRunTime(const val: Boolean); inline;
    procedure SetAmalgamate(const val: Boolean); inline;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function AxisAlignedDimensionsUnscaled: TGLVector; override;
    function RayCastIntersect(const rayStart, rayVector: TGLVector;
      intersectPoint: PGLVector = nil; intersectNormal: PGLVector = nil)
      : Boolean; override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    procedure DoRender(var rci: TGLRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
    procedure StructureChanged; override;
    function BarycenterAbsolutePosition: TGLVector; override;
  published
    property CubeSize: TGLFloat read FCubeSize write SetCubeSize;
    property EdgeColor: TGLColor read FEdgeColor write SetEdgeColor;
    (* If true the dummycube's edges will be visible at runtime.
      The default behaviour of the dummycube is to be visible at design-time
      only, and invisible at runtime. *)
    property VisibleAtRunTime: Boolean read FVisibleAtRunTime
      write SetVisibleAtRunTime default False;
    (* Amalgamate the dummy's children in a single OpenGL entity.
      This activates a special rendering mode, which will compile
      the rendering of all of the dummycube's children objects into a
      single display list. This may provide a significant speed up in some
      situations, however, this means that changes to the children will
      be ignored until you call StructureChanged on the dummy cube.
      Some objects, that have their own display list management, may not
      be compatible with this behaviour. This will also prevents sorting
      and culling to operate as usual.
      In short, this features is best used for static, non-transparent
      geometry, or when the point of view won't change over a large
      number of frames. *)
    property Amalgamate: Boolean read FAmalgamate write SetAmalgamate
      default False;
    (* Camera Invariance Options.
      These options allow to "deactivate" sensitivity to camera, f.i. by
      centering the object on the camera or ignoring camera orientation. *)
    property CamInvarianceMode default cimNone;
    (* Event for custom visibility determination.
      Event handler should return True if the dummycube and its children
      are to be considered visible for the current render. *)
    property OnVisibilityDetermination: TGLVisibilityDeterminationEvent
      read FOnVisibilityDetermination write FOnVisibilityDetermination;
  end;

  TGLPlaneStyle = (psSingleQuad, psTileTexture);
  TGLPlaneStyles = set of TGLPlaneStyle;

  (* A simple plane object.
    Note that a plane is always made of a single quad (two triangles) and the
    tiling is only applied to texture coordinates. *)
  TGLPlane = class(TGLSceneObject)
  private
    FXOffset, FYOffset: TGLFloat;
    FXScope, FYScope: TGLFloat;
    FWidth, FHeight: TGLFloat;
    FXTiles, FYTiles: Cardinal;
    FStyle: TGLPlaneStyles;
    FMesh: array of array of TGLVertexRec;
  protected
    procedure SetHeight(const aValue: Single);
    procedure SetWidth(const aValue: Single);
    procedure SetXOffset(const Value: TGLFloat);
    procedure SetXScope(const Value: TGLFloat);
    function StoreXScope: Boolean;
    procedure SetXTiles(const Value: Cardinal);
    procedure SetYOffset(const Value: TGLFloat);
    procedure SetYScope(const Value: TGLFloat);
    function StoreYScope: Boolean;
    procedure SetYTiles(const Value: Cardinal);
    procedure SetStyle(const val: TGLPlaneStyles);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    function GenerateSilhouette(const silhouetteParameters
      : TGLSilhouetteParameters): TGLSilhouette; override;
    function AxisAlignedDimensionsUnscaled: TGLVector; override;
    function RayCastIntersect(const rayStart, rayVector: TGLVector;
      intersectPoint: PGLVector = nil; intersectNormal: PGLVector = nil)
      : Boolean; override;
    (* Computes the screen coordinates of the smallest rectangle encompassing the plane.
      Returned extents are NOT limited to any physical screen extents. *)
    function ScreenRect(aBuffer: TGLSceneBuffer): TRect;
    (* Computes the signed distance to the point.
      Point coordinates are expected in absolute coordinates. *)
    function PointDistance(const aPoint: TGLVector): Single;
  published
    property Height: TGLFloat read FHeight write SetHeight;
    property Width: TGLFloat read FWidth write SetWidth;
    property XOffset: TGLFloat read FXOffset write SetXOffset;
    property XScope: TGLFloat read FXScope write SetXScope stored StoreXScope;
    property XTiles: Cardinal read FXTiles write SetXTiles default 1;
    property YOffset: TGLFloat read FYOffset write SetYOffset;
    property YScope: TGLFloat read FYScope write SetYScope stored StoreYScope;
    property YTiles: Cardinal read FYTiles write SetYTiles default 1;
    property Style: TGLPlaneStyles read FStyle write SetStyle
      default [psSingleQuad, psTileTexture];
  end;

  (* A rectangular area, perspective projected, but always facing the camera.
    A TGLSprite is perspective projected and as such is scaled with distance,
    if you want a 2D sprite that does not get scaled, see TGLHUDSprite. *)
  TGLSprite = class(TGLSceneObject)
  private
    FWidth: TGLFloat;
    FHeight: TGLFloat;
    FRotation: TGLFloat;
    FAlphaChannel: Single;
    FMirrorU, FMirrorV: Boolean;
  protected
    procedure SetWidth(const val: TGLFloat);
    procedure SetHeight(const val: TGLFloat);
    procedure SetRotation(const val: TGLFloat);
    procedure SetAlphaChannel(const val: Single);
    function StoreAlphaChannel: Boolean;
    procedure SetMirrorU(const val: Boolean);
    procedure SetMirrorV(const val: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TGLVector; override;
    procedure SetSize(const Width, Height: TGLFloat);
    // Set width and height to "size"
    procedure SetSquareSize(const Size: TGLFloat);
  published
    // Sprite Width in 3D world units.
    property Width: TGLFloat read FWidth write SetWidth;
    // Sprite Height in 3D world units.
    property Height: TGLFloat read FHeight write SetHeight;
    (* This the ON-SCREEN rotation of the sprite.
      Rotatation=0 is handled faster. *)
    property Rotation: TGLFloat read FRotation write SetRotation;
    // If different from 1, this value will replace that of Diffuse.Alpha
    property AlphaChannel: Single read FAlphaChannel write SetAlphaChannel
      stored StoreAlphaChannel;
    // Reverses the texture coordinates in the U and V direction to mirror the texture.
    property MirrorU: Boolean read FMirrorU write SetMirrorU default False;
    property MirrorV: Boolean read FMirrorV write SetMirrorV default False;
  end;

  TGLPointStyle = (psSquare, psRound, psSmooth, psSmoothAdditive,
    psSquareAdditive);

  (* Point parameters as in ARB_point_parameters.
    Make sure to read the ARB_point_parameters spec if you want to understand
    what each parameter does. *)
  TGLPointParameters = class(TGLUpdateAbleObject)
  private
    FEnabled: Boolean;
    FMinSize, FMaxSize: Single;
    FFadeTresholdSize: Single;
    FDistanceAttenuation: TGLCoordinates;
  protected
    procedure SetEnabled(const val: Boolean);
    procedure SetMinSize(const val: Single);
    procedure SetMaxSize(const val: Single);
    procedure SetFadeTresholdSize(const val: Single);
    procedure SetDistanceAttenuation(const val: TGLCoordinates);
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Apply;
    procedure UnApply;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property MinSize: Single read FMinSize write SetMinSize stored False;
    property MaxSize: Single read FMaxSize write SetMaxSize stored False;
    property FadeTresholdSize: Single read FFadeTresholdSize
      write SetFadeTresholdSize stored False;
    // Components XYZ are for constant, linear and quadratic attenuation.
    property DistanceAttenuation: TGLCoordinates read FDistanceAttenuation
      write SetDistanceAttenuation;
  end;

  (* Renders a set of non-transparent colored points.
    The points positions and their color are defined through the Positions
    and Colors properties *)
  TGLPoints = class(TGLImmaterialSceneObject)
  private
    FPositions: TAffineVectorList;
    FColors: TVectorList;
    FSize: Single;
    FStyle: TGLPointStyle;
    FPointParameters: TGLPointParameters;
    FStatic, FNoZWrite: Boolean;
  protected
    function StoreSize: Boolean; inline;
    procedure SetNoZWrite(const val: Boolean);
    procedure SetStatic(const val: Boolean);
    procedure SetSize(const val: Single);
    procedure SetPositions(const val: TAffineVectorList); inline;
    procedure SetColors(const val: TVectorList);
    procedure SetStyle(const val: TGLPointStyle);
    procedure SetPointParameters(const val: TGLPointParameters);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    // Points positions.  If empty, a single point is assumed at (0, 0, 0)
    property Positions: TAffineVectorList read FPositions write SetPositions;
    (* Defines the points colors:
      if empty, point color will be opaque white
      if contains a single color, all points will use that color
      if contains N colors, the first N points (at max) will be rendered
      using the corresponding colors *)
    property Colors: TVectorList read FColors write SetColors;
  published
    // If true points do not write their Z to the depth buffer.
    property NoZWrite: Boolean read FNoZWrite write SetNoZWrite;
    (* Tells the component if point coordinates are static.
      If static, changes to the positions should be notified via an
      explicit StructureChanged call, or may not refresh.
      Static sets of points may render faster than dynamic ones. *)
    property Static: Boolean read FStatic write SetStatic;
    // Point size, all points have a fixed size.
    property Size: Single read FSize write SetSize stored StoreSize;
    // Points style.
    property Style: TGLPointStyle read FStyle write SetStyle default psSquare;
    (* Point parameters as of ARB_point_parameters.
      Allows to vary the size and transparency of points depending
      on their distance to the observer. *)
    property PointParameters: TGLPointParameters read FPointParameters
      write SetPointParameters;
  end;

  // Possible aspects for the nodes of a TLine.
  TGLLineNodesAspect = (lnaInvisible, lnaAxes, lnaCube);
  // Available spline modes for a TLine.
  TGLLineSplineMode = (lsmLines, lsmCubicSpline, lsmBezierSpline, lsmNURBSCurve,
    lsmSegments, lsmLoop);
  // Specialized Node for use in a TGLLines objects. Adds a Color property (TGLColor) }

  TGLLinesNode = class(TGLNode)
  private
    FColor: TGLColor;
  protected
    procedure SetColor(const val: TGLColor);
    procedure OnColorChange(Sender: TObject);
    function StoreColor: Boolean;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    (* The node color.
      Can also defined the line color (interpolated between nodes) if
      loUseNodeColorForLines is set (in TGLLines). *)
    property Color: TGLColor read FColor write SetColor stored StoreColor;
  end;

  (* Specialized collection for Nodes in a TGLLines objects. Stores TGLLinesNode items *)
  TGLLinesNodes = class(TGLNodes)
  public
    constructor Create(AOwner: TComponent); overload;
    procedure NotifyChange; override;
  end;

  (* Base class for line objects. Introduces line style properties (width, color...) *)
  TGLLineBase = class(TGLImmaterialSceneObject)
  private
    FLineColor: TGLColor;
    FLinePattern: TGLushort;
    FLineWidth: Single;
    FAntiAliased: Boolean;
  protected
    procedure SetLineColor(const Value: TGLColor);
    procedure SetLinePattern(const Value: TGLushort);
    procedure SetLineWidth(const val: Single);
    function StoreLineWidth: Boolean; inline;
    procedure SetAntiAliased(const val: Boolean);
    (* Setup OpenGL states according to line style.
      You must call RestoreLineStyle after drawing your lines.
      You may use nested calls with SetupLineStyle/RestoreLineStyle *)
    procedure SetupLineStyle(var rci: TGLRenderContextInfo);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
  published
    (* Indicates if OpenGL should smooth line edges.
      Smoothed lines looks better but are poorly implemented in most OpenGL
      drivers and take *lots* of rendering time *)
    property AntiAliased: Boolean read FAntiAliased write SetAntiAliased
      default False;
    // Default color of the lines.
    property LineColor: TGLColor read FLineColor write SetLineColor;
    (* Bitwise line pattern.
      For instance $FFFF (65535) is a white line (stipple disabled), $0000
      is a black line, $CCCC is the stipple used in axes and dummycube, etc. *)
    property LinePattern: TGLushort read FLinePattern write SetLinePattern
      default $FFFF;
    // Default width of the lines.
    property LineWidth: Single read FLineWidth write SetLineWidth
      stored StoreLineWidth;
    property Visible;
  end;

  // Class that defines lines via a series of nodes. Base class, does not render anything
  TGLNodedLines = class(TGLLineBase)
  private
    FNodes: TGLLinesNodes;
    FNodesAspect: TGLLineNodesAspect;
    FNodeColor: TGLColor;
    FNodeSize: Single;
    FOldNodeColor: TColorVector;
  protected
    procedure SetNodesAspect(const Value: TGLLineNodesAspect);
    procedure SetNodeColor(const Value: TGLColor);
    procedure OnNodeColorChanged(Sender: TObject);
    procedure SetNodes(const aNodes: TGLLinesNodes);
    procedure SetNodeSize(const val: Single);
    function StoreNodeSize: Boolean;
    procedure DrawNode(var rci: TGLRenderContextInfo; X, Y, Z: Single;
      Color: TGLColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function AxisAlignedDimensionsUnscaled: TGLVector; override;
    procedure AddNode(const coords: TGLCoordinates); overload;
    procedure AddNode(const X, Y, Z: TGLFloat); overload;
    procedure AddNode(const Value: TGLVector); overload;
    procedure AddNode(const Value: TAffineVector); overload;
  published
    // Default color for nodes. lnaInvisible and lnaAxes ignore this setting
    property NodeColor: TGLColor read FNodeColor write SetNodeColor;
    // The nodes list.
    property Nodes: TGLLinesNodes read FNodes write SetNodes;
    (* Default aspect of line nodes.
      May help you materialize nodes, segments and control points. *)
    property NodesAspect: TGLLineNodesAspect read FNodesAspect
      write SetNodesAspect default lnaAxes;
    // Size for the various node aspects.
    property NodeSize: Single read FNodeSize write SetNodeSize
      stored StoreNodeSize;
  end;

  TGLLinesOption = (loUseNodeColorForLines, loColorLogicXor);
  TGLLinesOptions = set of TGLLinesOption;

  (* Set of 3D line segments.
    You define a 3D Line by adding its nodes in the "Nodes" property. The line
    may be rendered as a set of segment or as a curve (nodes then act as spline
    control points).
    Alternatively, you can also use it to render a set of spacial nodes (points
    in space), just make the lines transparent and the nodes visible by picking
    the node aspect that suits you. *)
  TGLLines = class(TGLNodedLines)
  private
    FDivision: Integer;
    FSplineMode: TGLLineSplineMode;
    FOptions: TGLLinesOptions;
    FNURBSOrder: Integer;
    FNURBSTolerance: Single;
    FNURBSKnots: TSingleList;
  protected
    procedure SetSplineMode(const val: TGLLineSplineMode);
    procedure SetDivision(const Value: Integer);
    procedure SetOptions(const val: TGLLinesOptions);
    procedure SetNURBSOrder(const val: Integer);
    procedure SetNURBSTolerance(const val: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    property NURBSKnots: TSingleList read FNURBSKnots;
    property NURBSOrder: Integer read FNURBSOrder write SetNURBSOrder;
    property NURBSTolerance: Single read FNURBSTolerance
      write SetNURBSTolerance;
  published
    (* Number of divisions for each segment in spline modes.
      Minimum 1 (disabled), ignored in lsmLines mode. *)
    property Division: Integer read FDivision write SetDivision default 10;
    // Default spline drawing mode.
    property SplineMode: TGLLineSplineMode read FSplineMode write SetSplineMode
      default lsmLines;
    (* Rendering options for the line.
      loUseNodeColorForLines: if set lines will be drawn using node
      colors (and color interpolation between nodes), if not, LineColor
      will be used (single color).
      loColorLogicXor: enable logic operation for color of XOR type. *)
    property Options: TGLLinesOptions read FOptions write SetOptions;
  end;

  TGLCubePart = (cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight);
  TGLCubeParts = set of TGLCubePart;

  (* A simple cube object.
    This cube use the same material for each of its faces, ie. all faces look
    the same. If you want a multi-material cube, use a mesh in conjunction
    with a TGLFreeForm and a material library. *)
  TGLCube = class(TGLSceneObject)
  private
    FCubeSize: TAffineVector;
    FParts: TGLCubeParts;
    FNormalDirection: TGLNormalDirection;
    function GetCubeWHD(const Index: Integer): TGLFloat; inline;
    procedure SetCubeWHD(Index: Integer; aValue: TGLFloat); inline;
    procedure SetParts(aValue: TGLCubeParts); inline;
    procedure SetNormalDirection(aValue: TGLNormalDirection); inline;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream); inline;
    procedure WriteData(Stream: TStream); inline;
  public
    constructor Create(AOwner: TComponent); override;
    function GenerateSilhouette(const silhouetteParameters
      : TGLSilhouetteParameters): TGLSilhouette; override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    procedure Assign(Source: TPersistent); override;
    function AxisAlignedDimensionsUnscaled: TGLVector; override;
    function RayCastIntersect(const rayStart, rayVector: TGLVector;
      intersectPoint: PGLVector = nil; intersectNormal: PGLVector = nil)
      : Boolean; override;
  published
    property CubeWidth: TGLFloat index 0 read GetCubeWHD write SetCubeWHD
      stored False;
    property CubeHeight: TGLFloat index 1 read GetCubeWHD write SetCubeWHD
      stored False;
    property CubeDepth: TGLFloat index 2 read GetCubeWHD write SetCubeWHD
      stored False;
    property NormalDirection: TGLNormalDirection read FNormalDirection
      write SetNormalDirection default ndOutside;
    property Parts: TGLCubeParts read FParts write SetParts
      default [cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight];
  end;

  (* Determines how and if normals are smoothed.
    - nsFlat : facetted look
    - nsSmooth : smooth look
    - nsNone : unlighted rendering, usefull for decla texturing *)
  TGLNormalSmoothing = (nsFlat, nsSmooth, nsNone);

  (* Base class for quadric objects.
    Introduces some basic Quadric interaction functions (the actual quadric
    math is part of the GLU library). *)
  TGLQuadricObject = class(TGLSceneObject)
  private
    FNormals: TGLNormalSmoothing;
    FNormalDirection: TGLNormalDirection;
  protected
    procedure SetNormals(aValue: TGLNormalSmoothing);
    procedure SetNormalDirection(aValue: TGLNormalDirection);
    procedure SetupQuadricParams(quadric: PGLUquadricObj);
    procedure SetNormalQuadricOrientation(quadric: PGLUquadricObj);
    procedure SetInvertedQuadricOrientation(quadric: PGLUquadricObj);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Normals: TGLNormalSmoothing read FNormals write SetNormals
      default nsSmooth;
    property NormalDirection: TGLNormalDirection read FNormalDirection
      write SetNormalDirection default ndOutside;
  end;

  TAngleLimit1 = -90 .. 90;
  TAngleLimit2 = 0 .. 360;
  TGLCapType = (ctNone, ctCenter, ctFlat);

  (* A sphere object.
    The sphere can have to and bottom caps, as well as being just a slice
    of sphere. *)
  TGLSphere = class(TGLQuadricObject)
  private
    FRadius: TGLFloat;
    FSlices, FStacks: TGLInt;
    FTop: TAngleLimit1;
    FBottom: TAngleLimit1;
    FStart: TAngleLimit2;
    FStop: TAngleLimit2;
    FTopCap, FBottomCap: TGLCapType;
    procedure SetBottom(aValue: TAngleLimit1);
    procedure SetBottomCap(aValue: TGLCapType);
    procedure SetRadius(const aValue: TGLFloat);
    procedure SetSlices(aValue: TGLInt);
    procedure SetStart(aValue: TAngleLimit2);
    procedure SetStop(aValue: TAngleLimit2);
    procedure SetStacks(aValue: TGLInt);
    procedure SetTop(aValue: TAngleLimit1);
    procedure SetTopCap(aValue: TGLCapType);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TGLVector; override;
    function RayCastIntersect(const rayStart, rayVector: TGLVector;
      intersectPoint: PGLVector = nil; intersectNormal: PGLVector = nil)
      : Boolean; override;
    function GenerateSilhouette(const silhouetteParameters
      : TGLSilhouetteParameters): TGLSilhouette; override;
  published
    property Bottom: TAngleLimit1 read FBottom write SetBottom default -90;
    property BottomCap: TGLCapType read FBottomCap write SetBottomCap
      default ctNone;
    property Radius: TGLFloat read FRadius write SetRadius;
    property Slices: TGLInt read FSlices write SetSlices default 16;
    property Stacks: TGLInt read FStacks write SetStacks default 16;
    property Start: TAngleLimit2 read FStart write SetStart default 0;
    property Stop: TAngleLimit2 read FStop write SetStop default 360;
    property Top: TAngleLimit1 read FTop write SetTop default 90;
    property TopCap: TGLCapType read FTopCap write SetTopCap default ctNone;
  end;

  (* Base class for objects based on a polygon *)
  TGLPolygonBase = class(TGLSceneObject)
  private
    FDivision: Integer;
    FSplineMode: TGLLineSplineMode;
  protected
    FNodes: TGLNodes;
    procedure CreateNodes; dynamic;
    procedure SetSplineMode(const val: TGLLineSplineMode);
    procedure SetDivision(const Value: Integer);
    procedure SetNodes(const aNodes: TGLNodes);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure AddNode(const coords: TGLCoordinates); overload;
    procedure AddNode(const X, Y, Z: TGLFloat); overload;
    procedure AddNode(const Value: TGLVector); overload;
    procedure AddNode(const Value: TAffineVector); overload;
  published
    // The nodes list.
    property Nodes: TGLNodes read FNodes write SetNodes;
    (* Number of divisions for each segment in spline modes.
      Minimum 1 (disabled), ignored in lsmLines mode. *)
    property Division: Integer read FDivision write SetDivision default 10;
    (* Default spline drawing mode.
      This mode is used only for the curve, not for the rotation path. *)
    property SplineMode: TGLLineSplineMode read FSplineMode write SetSplineMode
      default lsmLines;
  end;

  (* A Superellipsoid object. The Superellipsoid can have top and bottom caps,
    as well as being just a slice of Superellipsoid. *)
  TGLSuperellipsoid = class(TGLQuadricObject)
  private
    FRadius, FVCurve, FHCurve: TGLFloat;
    FSlices, FStacks: TGLInt;
    FTop: TAngleLimit1;
    FBottom: TAngleLimit1;
    FStart: TAngleLimit2;
    FStop: TAngleLimit2;
    FTopCap, FBottomCap: TGLCapType;
    procedure SetBottom(aValue: TAngleLimit1);
    procedure SetBottomCap(aValue: TGLCapType);
    procedure SetRadius(const aValue: TGLFloat);
    procedure SetVCurve(const aValue: TGLFloat);
    procedure SetHCurve(const aValue: TGLFloat);
    procedure SetSlices(aValue: TGLInt);
    procedure SetStart(aValue: TAngleLimit2);
    procedure SetStop(aValue: TAngleLimit2);
    procedure SetStacks(aValue: TGLInt);
    procedure SetTop(aValue: TAngleLimit1);
    procedure SetTopCap(aValue: TGLCapType);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TGLVector; override;
    function RayCastIntersect(const rayStart, rayVector: TGLVector;
      intersectPoint: PGLVector = nil; intersectNormal: PGLVector = nil)
      : Boolean; override;
    function GenerateSilhouette(const silhouetteParameters
      : TGLSilhouetteParameters): TGLSilhouette; override;
  published
    property Bottom: TAngleLimit1 read FBottom write SetBottom default -90;
    property BottomCap: TGLCapType read FBottomCap write SetBottomCap
      default ctNone;
    property Radius: TGLFloat read FRadius write SetRadius;
    property VCurve: TGLFloat read FVCurve write SetVCurve;
    property HCurve: TGLFloat read FHCurve write SetHCurve;
    property Slices: TGLInt read FSlices write SetSlices default 16;
    property Stacks: TGLInt read FStacks write SetStacks default 16;
    property Start: TAngleLimit2 read FStart write SetStart default 0;
    property Stop: TAngleLimit2 read FStop write SetStop default 360;
    property Top: TAngleLimit1 read FTop write SetTop default 90;
    property TopCap: TGLCapType read FTopCap write SetTopCap default ctNone;
  end;

// Issues for a unit-size cube stippled wireframe
procedure CubeWireframeBuildList(var rci: TGLRenderContextInfo; Size: TGLFloat;
  Stipple: Boolean; const Color: TColorVector);

const
  TangentAttributeName: PAnsiChar = 'Tangent';
  BinormalAttributeName: PAnsiChar = 'Binormal';

// -------------------------------------------------------------
implementation
// -------------------------------------------------------------

procedure CubeWireframeBuildList(var rci: TGLRenderContextInfo; Size: TGLFloat;
  Stipple: Boolean; const Color: TColorVector);
var
  mi, ma: Single;
begin
{$IFDEF USE_OPENGL_DEBUG}
  if GL.GREMEDY_string_marker then
    gl.StringMarkerGREMEDY(22, 'CubeWireframeBuildList');
{$ENDIF}
  rci.GLStates.Disable(stLighting);
  rci.GLStates.Enable(stLineSmooth);
  if Stipple then
  begin
    rci.GLStates.Enable(stLineStipple);
    rci.GLStates.Enable(stBlend);
    rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
    rci.GLStates.LineStippleFactor := 1;
    rci.GLStates.LineStipplePattern := $CCCC;
  end;
  rci.GLStates.LineWidth := 1;
  ma := 0.5 * Size;
  mi := -ma;

  gl.Color4fv(@Color);
  gl.Begin_(GL_LINE_STRIP);
  // front face
  gl.Vertex3f(ma, mi, mi);
  gl.Vertex3f(ma, ma, mi);
  gl.Vertex3f(ma, ma, ma);
  gl.Vertex3f(ma, mi, ma);
  gl.Vertex3f(ma, mi, mi);
  // partial up back face
  gl.Vertex3f(mi, mi, mi);
  gl.Vertex3f(mi, mi, ma);
  gl.Vertex3f(mi, ma, ma);
  gl.Vertex3f(mi, ma, mi);
  // right side low
  gl.Vertex3f(ma, ma, mi);
  gl.End_;
  gl.Begin_(GL_LINES);
  // right high
  gl.Vertex3f(ma, ma, ma);
  gl.Vertex3f(mi, ma, ma);
  // back low
  gl.Vertex3f(mi, mi, mi);
  gl.Vertex3f(mi, ma, mi);
  // left high
  gl.Vertex3f(ma, mi, ma);
  gl.Vertex3f(mi, mi, ma);
  gl.End_;
end;

// ------------------
// ------------------ TGLDummyCube ------------------
// ------------------

constructor TGLDummyCube.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FCubeSize := 1;
  FEdgeColor := TGLColor.Create(Self);
  FEdgeColor.Initialize(clrWhite);
  FGroupList := TGLListHandle.Create;
  CamInvarianceMode := cimNone;
end;

destructor TGLDummyCube.Destroy;
begin
  FGroupList.Free;
  FEdgeColor.Free;
  inherited;
end;

procedure TGLDummyCube.Assign(Source: TPersistent);
begin
  if Source is TGLDummyCube then
  begin
    FCubeSize := TGLDummyCube(Source).FCubeSize;
    FEdgeColor.Color := TGLDummyCube(Source).FEdgeColor.Color;
    FVisibleAtRunTime := TGLDummyCube(Source).FVisibleAtRunTime;
    NotifyChange(Self);
  end;
  inherited Assign(Source);
end;

function TGLDummyCube.AxisAlignedDimensionsUnscaled: TGLVector;
begin
  Result.X := 0.5 * Abs(FCubeSize);
  Result.Y := Result.X;
  Result.Z := Result.X;
  Result.W := 0;
end;

function TGLDummyCube.RayCastIntersect(const rayStart, rayVector: TGLVector;
  intersectPoint: PGLVector = nil; intersectNormal: PGLVector = nil): Boolean;
begin
  Result := False;
end;

procedure TGLDummyCube.BuildList(var rci: TGLRenderContextInfo);
begin
  if (csDesigning in ComponentState) or (FVisibleAtRunTime) then
    CubeWireframeBuildList(rci, FCubeSize, True, EdgeColor.Color);
end;

procedure TGLDummyCube.DoRender(var rci: TGLRenderContextInfo;
  renderSelf, renderChildren: Boolean);
begin
  if Assigned(FOnVisibilityDetermination) then
    if not FOnVisibilityDetermination(Self, rci) then
      Exit;
  if FAmalgamate and (not rci.amalgamating) then
  begin
    if FGroupList.Handle = 0 then
    begin
      FGroupList.AllocateHandle;
      Assert(FGroupList.Handle <> 0, 'Handle=0 for ' + ClassName);
      rci.GLStates.NewList(FGroupList.Handle, GL_COMPILE);
      rci.amalgamating := True;
      // try
      inherited;
      // finally
      rci.amalgamating := False;
      rci.GLStates.EndList;
      // end;
    end;
    rci.GLStates.CallList(FGroupList.Handle);
  end
  else
  begin
    // proceed as usual
    inherited;
  end;
end;

procedure TGLDummyCube.StructureChanged;
begin
  if FAmalgamate then
    FGroupList.DestroyHandle;
  inherited;
end;

function TGLDummyCube.BarycenterAbsolutePosition: TGLVector;
var
  i: Integer;
begin
  if Count > 0 then
  begin
    Result := Children[0].BarycenterAbsolutePosition;
    for i := 1 to Count - 1 do
      Result := VectorAdd(Result, Children[i].BarycenterAbsolutePosition);
    ScaleVector(Result, 1 / Count);
  end
  else
    Result := AbsolutePosition;
end;

procedure TGLDummyCube.SetCubeSize(const val: TGLFloat);
begin
  if val <> FCubeSize then
  begin
    FCubeSize := val;
    StructureChanged;
  end;
end;

procedure TGLDummyCube.SetEdgeColor(const val: TGLColor);
begin
  if val <> FEdgeColor then
  begin
    FEdgeColor.Assign(val);
    StructureChanged;
  end;
end;

procedure TGLDummyCube.SetVisibleAtRunTime(const val: Boolean);
begin
  if val <> FVisibleAtRunTime then
  begin
    FVisibleAtRunTime := val;
    StructureChanged;
  end;
end;

procedure TGLDummyCube.SetAmalgamate(const val: Boolean);
begin
  if val <> FAmalgamate then
  begin
    FAmalgamate := val;
    if not val then
      FGroupList.DestroyHandle;
    inherited StructureChanged;
  end;
end;

// ------------------
// ------------------ TGLPlane ------------------
// ------------------

constructor TGLPlane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidth := 1;
  FHeight := 1;
  FXTiles := 1;
  FYTiles := 1;
  FXScope := 1;
  FYScope := 1;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FStyle := [psSingleQuad, psTileTexture];
end;

procedure TGLPlane.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLPlane) then
  begin
    FWidth := TGLPlane(Source).FWidth;
    FHeight := TGLPlane(Source).FHeight;
    FXOffset := TGLPlane(Source).FXOffset;
    FXScope := TGLPlane(Source).FXScope;
    FXTiles := TGLPlane(Source).FXTiles;
    FYOffset := TGLPlane(Source).FYOffset;
    FYScope := TGLPlane(Source).FYScope;
    FYTiles := TGLPlane(Source).FYTiles;
    FStyle := TGLPlane(Source).FStyle;
    StructureChanged;
  end;
  inherited Assign(Source);
end;

function TGLPlane.AxisAlignedDimensionsUnscaled: TGLVector;
begin
  Result.X := 0.5 * Abs(FWidth);
  Result.Y := 0.5 * Abs(FHeight);
  Result.Z := 0;
end;

function TGLPlane.RayCastIntersect(const rayStart, rayVector: TGLVector;
  intersectPoint: PGLVector = nil; intersectNormal: PGLVector = nil): Boolean;
var
  locRayStart, locRayVector, ip: TGLVector;
  t: Single;
begin
  locRayStart := AbsoluteToLocal(rayStart);
  locRayVector := AbsoluteToLocal(rayVector);
  if locRayStart.Z >= 0 then
  begin
    // ray start over plane
    if locRayVector.Z < 0 then
    begin
      t := locRayStart.Z / locRayVector.Z;
      ip.X := locRayStart.X - t * locRayVector.X;
      ip.Y := locRayStart.Y - t * locRayVector.Y;
      if (Abs(ip.X) <= 0.5 * Width) and (Abs(ip.Y) <= 0.5 * Height) then
      begin
        Result := True;
        if Assigned(intersectNormal) then
          intersectNormal^ := AbsoluteDirection;
      end
      else
        Result := False;
    end
    else
      Result := False;
  end
  else
  begin
    // ray start below plane
    if locRayVector.Z > 0 then
    begin
      t := locRayStart.Z / locRayVector.Z;
      ip.X := locRayStart.X - t * locRayVector.X;
      ip.Y := locRayStart.Y - t * locRayVector.Y;
      if (Abs(ip.X) <= 0.5 * Width) and (Abs(ip.Y) <= 0.5 * Height) then
      begin
        Result := True;
        if Assigned(intersectNormal) then
          intersectNormal^ := VectorNegate(AbsoluteDirection);
      end
      else
        Result := False;
    end
    else
      Result := False;
  end;
  if Result and Assigned(intersectPoint) then
  begin
    ip.Z := 0;
    ip.W := 1;
    intersectPoint^ := LocalToAbsolute(ip);
  end;
end;

function TGLPlane.GenerateSilhouette(const silhouetteParameters
  : TGLSilhouetteParameters): TGLSilhouette;
var
  hw, hh: Single;
begin
  Result := TGLSilhouette.Create;
  hw := FWidth * 0.5;
  hh := FHeight * 0.5;
  with Result.vertices do
  begin
    AddPoint(hw, hh);
    AddPoint(hw, -hh);
    AddPoint(-hw, -hh);
    AddPoint(-hw, hh);
  end;

  with Result.Indices do
  begin
    Add(0, 1);
    Add(1, 2);
    Add(2, 3);
    Add(3, 0);
  end;

  if silhouetteParameters.CappingRequired then
    with Result.CapIndices do
    begin
      Add(0, 1, 2);
      Add(2, 3, 0);
    end;
end;

procedure TGLPlane.BuildList(var rci: TGLRenderContextInfo);

  procedure EmitVertex(ptr: PGLVertexRec); inline;
  begin
    xgl.TexCoord2fv(@ptr^.TexCoord);
    gl.Vertex3fv(@ptr^.Position);
  end;

var
  hw, hh, posXFact, posYFact, pX, pY1: TGLFloat;
  tx0, tx1, ty0, ty1, texSFact, texTFact: TGLFloat;
  texS, texT1: TGLFloat;
  X, Y: Integer;
  TanLoc, BinLoc: Integer;
  pVertex: PGLVertexRec;
begin
  hw := FWidth * 0.5;
  hh := FHeight * 0.5;

  gl.Normal3fv(@ZVector);
  if GL.ARB_shader_objects and (rci.GLStates.CurrentProgram > 0) then
  begin
    TanLoc := gl.GetAttribLocation(rci.GLStates.CurrentProgram,
      TangentAttributeName);
    BinLoc := gl.GetAttribLocation(rci.GLStates.CurrentProgram,
      BinormalAttributeName);
    if TanLoc > -1 then
      gl.VertexAttrib3fv(TanLoc, @XVector);
    if BinLoc > -1 then
      gl.VertexAttrib3fv(BinLoc, @YVector);
  end;
  // determine tex coords extents
  if psTileTexture in FStyle then
  begin
    tx0 := FXOffset;
    tx1 := FXTiles * FXScope + FXOffset;
    ty0 := FYOffset;
    ty1 := FYTiles * FYScope + FYOffset;
  end
  else
  begin
    tx0 := 0;
    ty0 := tx0;
    tx1 := FXScope;
    ty1 := FYScope;
  end;

  if psSingleQuad in FStyle then
  begin
    // single quad plane
    gl.Begin_(GL_TRIANGLES);
    xgl.TexCoord2f(tx1, ty1);
    gl.Vertex2f(hw, hh);
    xgl.TexCoord2f(tx0, ty1);
    gl.Vertex2f(-hw, hh);
    xgl.TexCoord2f(tx0, ty0);
    gl.Vertex2f(-hw, -hh);
    gl.Vertex2f(-hw, -hh);
    xgl.TexCoord2f(tx1, ty0);
    gl.Vertex2f(hw, -hh);
    xgl.TexCoord2f(tx1, ty1);
    gl.Vertex2f(hw, hh);
    gl.End_;
    Exit;
  end
  else
  begin
    // multi-quad plane (actually built from tri-strips)
    texSFact := (tx1 - tx0) / FXTiles;
    texTFact := (ty1 - ty0) / FYTiles;
    posXFact := FWidth / FXTiles;
    posYFact := FHeight / FYTiles;
    if FMesh = nil then
    begin
      SetLength(FMesh, FYTiles + 1, FXTiles + 1);
      for Y := 0 to FYTiles do
      begin
        texT1 := Y * texTFact;
        pY1 := Y * posYFact - hh;
        for X := 0 to FXTiles do
        begin
          texS := X * texSFact;
          pX := X * posXFact - hw;
          FMesh[Y][X].Position := Vector3fMake(pX, pY1, 0.0);
          FMesh[Y][X].TexCoord := Vector2fMake(texS, texT1);
        end;
      end;
    end;
  end;

  gl.Begin_(GL_TRIANGLES);
  for Y := 0 to FYTiles - 1 do
  begin
    for X := 0 to FXTiles - 1 do
    begin
      pVertex := @FMesh[Y][X];
      EmitVertex(pVertex);

      pVertex := @FMesh[Y][X + 1];
      EmitVertex(pVertex);

      pVertex := @FMesh[Y + 1][X];
      EmitVertex(pVertex);

      pVertex := @FMesh[Y + 1][X + 1];
      EmitVertex(pVertex);

      pVertex := @FMesh[Y + 1][X];
      EmitVertex(pVertex);

      pVertex := @FMesh[Y][X + 1];
      EmitVertex(pVertex);
    end;
  end;
  gl.End_;
end;

procedure TGLPlane.SetWidth(const aValue: Single);
begin
  if aValue <> FWidth then
  begin
    FWidth := aValue;
    FMesh := nil;
    StructureChanged;
  end;
end;

function TGLPlane.ScreenRect(aBuffer: TGLSceneBuffer): TRect;
var
  v: array [0 .. 3] of TGLVector;
  buf: TGLSceneBuffer;
  hw, hh: TGLFloat;
begin
  buf := aBuffer;
  if Assigned(buf) then
  begin
    hw := FWidth * 0.5;
    hh := FHeight * 0.5;
    v[0] := LocalToAbsolute(PointMake(-hw, -hh, 0));
    v[1] := LocalToAbsolute(PointMake(hw, -hh, 0));
    v[2] := LocalToAbsolute(PointMake(hw, hh, 0));
    v[3] := LocalToAbsolute(PointMake(-hw, hh, 0));
    buf.WorldToScreen(@v[0], 4);
    Result.Left := Round(MinFloat([v[0].X, v[1].X, v[2].X, v[3].X]));
    Result.Right := Round(MaxFloat([v[0].X, v[1].X, v[2].X, v[3].X]));
    Result.Top := Round(MinFloat([v[0].Y, v[1].Y, v[2].Y, v[3].Y]));
    Result.Bottom := Round(MaxFloat([v[0].Y, v[1].Y, v[2].Y, v[3].Y]));
  end
  else
    FillChar(Result, SizeOf(TRect), 0);
end;

function TGLPlane.PointDistance(const aPoint: TGLVector): Single;
begin
  Result := VectorDotProduct(VectorSubtract(aPoint, AbsolutePosition),
    AbsoluteDirection);
end;

procedure TGLPlane.SetHeight(const aValue: Single);
begin
  if aValue <> FHeight then
  begin
    FHeight := aValue;
    FMesh := nil;
    StructureChanged;
  end;
end;

procedure TGLPlane.SetXOffset(const Value: TGLFloat);
begin
  if Value <> FXOffset then
  begin
    FXOffset := Value;
    FMesh := nil;
    StructureChanged;
  end;
end;

procedure TGLPlane.SetXScope(const Value: TGLFloat);
begin
  if Value <> FXScope then
  begin
    FXScope := Value;
    if FXScope > 1 then
      FXScope := 1;
    FMesh := nil;
    StructureChanged;
  end;
end;

function TGLPlane.StoreXScope: Boolean;
begin
  Result := (FXScope <> 1);
end;

procedure TGLPlane.SetXTiles(const Value: Cardinal);
begin
  if Value <> FXTiles then
  begin
    FXTiles := Value;
    FMesh := nil;
    StructureChanged;
  end;
end;

procedure TGLPlane.SetYOffset(const Value: TGLFloat);
begin
  if Value <> FYOffset then
  begin
    FYOffset := Value;
    FMesh := nil;
    StructureChanged;
  end;
end;

procedure TGLPlane.SetYScope(const Value: TGLFloat);
begin
  if Value <> FYScope then
  begin
    FYScope := Value;
    if FYScope > 1 then
      FYScope := 1;
    FMesh := nil;
    StructureChanged;
  end;
end;

function TGLPlane.StoreYScope: Boolean;
begin
  Result := (FYScope <> 1);
end;

procedure TGLPlane.SetYTiles(const Value: Cardinal);
begin
  if Value <> FYTiles then
  begin
    FYTiles := Value;
    FMesh := nil;
    StructureChanged;
  end;
end;

procedure TGLPlane.SetStyle(const val: TGLPlaneStyles);
begin
  if val <> FStyle then
  begin
    FStyle := val;
    StructureChanged;
  end;
end;

// ------------------
// ------------------ TGLSprite ------------------
// ------------------

constructor TGLSprite.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FAlphaChannel := 1;
  FWidth := 1;
  FHeight := 1;
end;

procedure TGLSprite.Assign(Source: TPersistent);
begin
  if Source is TGLSprite then
  begin
    FWidth := TGLSprite(Source).FWidth;
    FHeight := TGLSprite(Source).FHeight;
    FRotation := TGLSprite(Source).FRotation;
    FAlphaChannel := TGLSprite(Source).FAlphaChannel;
  end;
  inherited Assign(Source);
end;

function TGLSprite.AxisAlignedDimensionsUnscaled: TGLVector;
begin
  Result.X := 0.5 * Abs(FWidth);
  Result.Y := 0.5 * Abs(FHeight);
  // Sprites turn with the camera and can be considered to have the same depth
  // as width
  Result.Z := 0.5 * Abs(FWidth);
end;

procedure TGLSprite.BuildList(var rci: TGLRenderContextInfo);
var
  vx, vy: TAffineVector;
  W, h: Single;
  mat: TGLMatrix;
  u0, v0, u1, v1: Integer;
begin
  if FAlphaChannel <> 1 then
    rci.GLStates.SetGLMaterialAlphaChannel(GL_FRONT, FAlphaChannel);

  mat := rci.PipelineTransformation.ModelViewMatrix^;
  // extraction of the direction vectors of the matrix
  W := FWidth * 0.5;
  h := FHeight * 0.5;
  vx.X := mat.v[0].X;
  vy.X := mat.v[0].Y;
  vx.Y := mat.v[1].X;
  vy.Y := mat.v[1].Y;
  vx.Z := mat.v[2].X;
  vy.Z := mat.v[2].Y;
  ScaleVector(vx, W / VectorLength(vx));
  ScaleVector(vy, h / VectorLength(vy));
  if FMirrorU then
  begin
    u0 := 1;
    u1 := 0;
  end
  else
  begin
    u0 := 0;
    u1 := 1;
  end;
  if FMirrorV then
  begin
    v0 := 1;
    v1 := 0;
  end
  else
  begin
    v0 := 0;
    v1 := 1;
  end;

  if FRotation <> 0 then
  begin
    gl.PushMatrix;
    gl.Rotatef(FRotation, mat.v[0].Z, mat.v[1].Z, mat.v[2].Z);
  end;
  gl.Begin_(GL_QUADS);
  xgl.TexCoord2f(u1, v1);
  gl.Vertex3f(vx.X + vy.X, vx.Y + vy.Y, vx.Z + vy.Z);
  xgl.TexCoord2f(u0, v1);
  gl.Vertex3f(-vx.X + vy.X, -vx.Y + vy.Y, -vx.Z + vy.Z);
  xgl.TexCoord2f(u0, v0);
  gl.Vertex3f(-vx.X - vy.X, -vx.Y - vy.Y, -vx.Z - vy.Z);
  xgl.TexCoord2f(u1, v0);
  gl.Vertex3f(vx.X - vy.X, vx.Y - vy.Y, vx.Z - vy.Z);
  gl.End_;
  if FRotation <> 0 then
    gl.PopMatrix;
end;

procedure TGLSprite.SetWidth(const val: TGLFloat);
begin
  if FWidth <> val then
  begin
    FWidth := val;
    NotifyChange(Self);
  end;
end;

procedure TGLSprite.SetHeight(const val: TGLFloat);
begin
  if FHeight <> val then
  begin
    FHeight := val;
    NotifyChange(Self);
  end;
end;

procedure TGLSprite.SetRotation(const val: TGLFloat);
begin
  if FRotation <> val then
  begin
    FRotation := val;
    NotifyChange(Self);
  end;
end;

procedure TGLSprite.SetAlphaChannel(const val: Single);
begin
  if val <> FAlphaChannel then
  begin
    if val < 0 then
      FAlphaChannel := 0
    else if val > 1 then
      FAlphaChannel := 1
    else
      FAlphaChannel := val;
    NotifyChange(Self);
  end;
end;

function TGLSprite.StoreAlphaChannel: Boolean;
begin
  Result := (FAlphaChannel <> 1);
end;

procedure TGLSprite.SetMirrorU(const val: Boolean);
begin
  FMirrorU := val;
  NotifyChange(Self);
end;

procedure TGLSprite.SetMirrorV(const val: Boolean);
begin
  FMirrorV := val;
  NotifyChange(Self);
end;

procedure TGLSprite.SetSize(const Width, Height: TGLFloat);
begin
  FWidth := Width;
  FHeight := Height;
  NotifyChange(Self);
end;

procedure TGLSprite.SetSquareSize(const Size: TGLFloat);
begin
  FWidth := Size;
  FHeight := Size;
  NotifyChange(Self);
end;

// ------------------
// ------------------ TGLPointParameters ------------------
// ------------------

constructor TGLPointParameters.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FMinSize := 0;
  FMaxSize := 128;
  FFadeTresholdSize := 1;
  FDistanceAttenuation := TGLCoordinates.CreateInitialized(Self, XHmgVector,
    csVector);
end;

destructor TGLPointParameters.Destroy;
begin
  FDistanceAttenuation.Free;
  inherited;
end;

procedure TGLPointParameters.Assign(Source: TPersistent);
begin
  if Source is TGLPointParameters then
  begin
    FMinSize := TGLPointParameters(Source).FMinSize;
    FMaxSize := TGLPointParameters(Source).FMaxSize;
    FFadeTresholdSize := TGLPointParameters(Source).FFadeTresholdSize;
    FDistanceAttenuation.Assign(TGLPointParameters(Source).DistanceAttenuation);
  end;
end;

procedure TGLPointParameters.DefineProperties(Filer: TFiler);
var
  defaultParams: Boolean;
begin
  inherited;
  defaultParams := (FMaxSize = 128) and (FMinSize = 0) and
    (FFadeTresholdSize = 1);
  Filer.DefineBinaryProperty('PointParams', ReadData, WriteData,
    not defaultParams);
end;

procedure TGLPointParameters.ReadData(Stream: TStream);
begin
  with Stream do
  begin
    Read(FMinSize, SizeOf(Single));
    Read(FMaxSize, SizeOf(Single));
    Read(FFadeTresholdSize, SizeOf(Single));
  end;
end;

procedure TGLPointParameters.WriteData(Stream: TStream);
begin
  with Stream do
  begin
    Write(FMinSize, SizeOf(Single));
    Write(FMaxSize, SizeOf(Single));
    Write(FFadeTresholdSize, SizeOf(Single));
  end;
end;

procedure TGLPointParameters.Apply;
begin
  if Enabled and GL.ARB_point_parameters then
  begin
    gl.PointParameterf(GL_POINT_SIZE_MIN_ARB, FMinSize);
    gl.PointParameterf(GL_POINT_SIZE_MAX_ARB, FMaxSize);
    gl.PointParameterf(GL_POINT_FADE_THRESHOLD_SIZE_ARB, FFadeTresholdSize);
    gl.PointParameterfv(GL_DISTANCE_ATTENUATION_EXT,
      FDistanceAttenuation.AsAddress);
  end;
end;

procedure TGLPointParameters.UnApply;
begin
  if Enabled and GL.ARB_point_parameters then
  begin
    gl.PointParameterf(GL_POINT_SIZE_MIN_ARB, 0);
    gl.PointParameterf(GL_POINT_SIZE_MAX_ARB, 128);
    gl.PointParameterf(GL_POINT_FADE_THRESHOLD_SIZE_ARB, 1);
    gl.PointParameterfv(GL_DISTANCE_ATTENUATION_EXT, @XVector);
  end;
end;

procedure TGLPointParameters.SetEnabled(const val: Boolean);
begin
  if val <> FEnabled then
  begin
    FEnabled := val;
    NotifyChange(Self);
  end;
end;

procedure TGLPointParameters.SetMinSize(const val: Single);
begin
  if val <> FMinSize then
  begin
    if val < 0 then
      FMinSize := 0
    else
      FMinSize := val;
    NotifyChange(Self);
  end;
end;

procedure TGLPointParameters.SetMaxSize(const val: Single);
begin
  if val <> FMaxSize then
  begin
    if val < 0 then
      FMaxSize := 0
    else
      FMaxSize := val;
    NotifyChange(Self);
  end;
end;

procedure TGLPointParameters.SetFadeTresholdSize(const val: Single);
begin
  if val <> FFadeTresholdSize then
  begin
    if val < 0 then
      FFadeTresholdSize := 0
    else
      FFadeTresholdSize := val;
    NotifyChange(Self);
  end;
end;

procedure TGLPointParameters.SetDistanceAttenuation(const val: TGLCoordinates);
begin
  FDistanceAttenuation.Assign(val);
end;

// ------------------
// ------------------ TGLPoints ------------------
// ------------------

constructor TGLPoints.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FStyle := psSquare;
  FSize := cDefaultPointSize;
  FPositions := TAffineVectorList.Create;
  FPositions.Add(NullVector);
  FColors := TVectorList.Create;
  FPointParameters := TGLPointParameters.Create(Self);
end;

destructor TGLPoints.Destroy;
begin
  FPointParameters.Free;
  FColors.Free;
  FPositions.Free;
  inherited;
end;

procedure TGLPoints.Assign(Source: TPersistent);
begin
  if Source is TGLPoints then
  begin
    FSize := TGLPoints(Source).FSize;
    FStyle := TGLPoints(Source).FStyle;
    FPositions.Assign(TGLPoints(Source).FPositions);
    FColors.Assign(TGLPoints(Source).FColors);
    StructureChanged
  end;
  inherited Assign(Source);
end;

procedure TGLPoints.BuildList(var rci: TGLRenderContextInfo);
var
  n: Integer;
  v: TGLVector;
begin
  n := FPositions.Count;
  if n = 0 then
    Exit;

  case FColors.Count of
    0:
      gl.Color4f(1, 1, 1, 1);
    1:
      gl.Color4fv(PGLFloat(FColors.List));
  else
    if FColors.Count < n then
      n := FColors.Count;
    gl.ColorPointer(4, GL_FLOAT, 0, FColors.List);
    gl.EnableClientState(GL_COLOR_ARRAY);
  end;
  if FColors.Count < 2 then
    gl.DisableClientState(GL_COLOR_ARRAY);
  rci.GLStates.Disable(stLighting);
  if n = 0 then
  begin
    v := NullHmgPoint;
    gl.VertexPointer(3, GL_FLOAT, 0, @v);
    n := 1;
  end
  else
    gl.VertexPointer(3, GL_FLOAT, 0, FPositions.List);
  gl.EnableClientState(GL_VERTEX_ARRAY);

  if NoZWrite then
    rci.GLStates.DepthWriteMask := False;
  rci.GLStates.PointSize := FSize;
  PointParameters.Apply;
  if GL.EXT_compiled_vertex_array and (n > 64) then
    gl.LockArrays(0, n);
  case FStyle of
    psSquare:
      begin
        // square point (simplest method, fastest)
        rci.GLStates.Disable(stBlend);
      end;
    psRound:
      begin
        rci.GLStates.Enable(stPointSmooth);
        rci.GLStates.Enable(stAlphaTest);
        rci.GLStates.SetGLAlphaFunction(cfGreater, 0.5);
        rci.GLStates.Disable(stBlend);
      end;
    psSmooth:
      begin
        rci.GLStates.Enable(stPointSmooth);
        rci.GLStates.Enable(stAlphaTest);
        rci.GLStates.SetGLAlphaFunction(cfNotEqual, 0.0);
        rci.GLStates.Enable(stBlend);
        rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      end;
    psSmoothAdditive:
      begin
        rci.GLStates.Enable(stPointSmooth);
        rci.GLStates.Enable(stAlphaTest);
        rci.GLStates.SetGLAlphaFunction(cfNotEqual, 0.0);
        rci.GLStates.Enable(stBlend);
        rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOne);
      end;
    psSquareAdditive:
      begin
        rci.GLStates.Enable(stBlend);
        rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOne);
      end;
  else
    Assert(False);
  end;
  gl.DrawArrays(GL_POINTS, 0, n);
  if GL.EXT_compiled_vertex_array and (n > 64) then
    gl.UnlockArrays;
  PointParameters.UnApply;
  gl.DisableClientState(GL_VERTEX_ARRAY);
  if FColors.Count > 1 then
    gl.DisableClientState(GL_COLOR_ARRAY);
end;

function TGLPoints.StoreSize: Boolean;
begin
  Result := (FSize <> cDefaultPointSize);
end;

procedure TGLPoints.SetNoZWrite(const val: Boolean);
begin
  if FNoZWrite <> val then
  begin
    FNoZWrite := val;
    StructureChanged;
  end;
end;

procedure TGLPoints.SetStatic(const val: Boolean);
begin
  if FStatic <> val then
  begin
    FStatic := val;
    if val then
      ObjectStyle := ObjectStyle - [osDirectDraw]
    else
      ObjectStyle := ObjectStyle + [osDirectDraw];
    StructureChanged;
  end;
end;

procedure TGLPoints.SetSize(const val: Single);
begin
  if FSize <> val then
  begin
    FSize := val;
    StructureChanged;
  end;
end;

procedure TGLPoints.SetPositions(const val: TAffineVectorList);
begin
  FPositions.Assign(val);
  StructureChanged;
end;

procedure TGLPoints.SetColors(const val: TVectorList);
begin
  FColors.Assign(val);
  StructureChanged;
end;

procedure TGLPoints.SetStyle(const val: TGLPointStyle);
begin
  if FStyle <> val then
  begin
    FStyle := val;
    StructureChanged;
  end;
end;

procedure TGLPoints.SetPointParameters(const val: TGLPointParameters);
begin
  FPointParameters.Assign(val);
end;

// ------------------
// ------------------ TGLLineBase ------------------
// ------------------

constructor TGLLineBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLineColor := TGLColor.Create(Self);
  FLineColor.Initialize(clrWhite);
  FLinePattern := $FFFF;
  FAntiAliased := False;
  FLineWidth := 1.0;
end;

destructor TGLLineBase.Destroy;
begin
  FLineColor.Free;
  inherited Destroy;
end;

procedure TGLLineBase.NotifyChange(Sender: TObject);
begin
  if Sender = FLineColor then
    StructureChanged;
  inherited;
end;

procedure TGLLineBase.SetLineColor(const Value: TGLColor);
begin
  FLineColor.Color := Value.Color;
  StructureChanged;
end;

procedure TGLLineBase.SetLinePattern(const Value: TGLushort);
begin
  if FLinePattern <> Value then
  begin
    FLinePattern := Value;
    StructureChanged;
  end;
end;

procedure TGLLineBase.SetLineWidth(const val: Single);
begin
  if FLineWidth <> val then
  begin
    FLineWidth := val;
    StructureChanged;
  end;
end;

function TGLLineBase.StoreLineWidth: Boolean;
begin
  Result := (FLineWidth <> 1.0);
end;

procedure TGLLineBase.SetAntiAliased(const val: Boolean);
begin
  if FAntiAliased <> val then
  begin
    FAntiAliased := val;
    StructureChanged;
  end;
end;

procedure TGLLineBase.Assign(Source: TPersistent);
begin
  if Source is TGLLineBase then
  begin
    LineColor := TGLLineBase(Source).FLineColor;
    LinePattern := TGLLineBase(Source).FLinePattern;
    LineWidth := TGLLineBase(Source).FLineWidth;
    AntiAliased := TGLLineBase(Source).FAntiAliased;
  end;
  inherited Assign(Source);
end;

procedure TGLLineBase.SetupLineStyle(var rci: TGLRenderContextInfo);
begin
  with rci.GLStates do
  begin
    Disable(stLighting);
    if FLinePattern <> $FFFF then
    begin
      Enable(stLineStipple);
      Enable(stBlend);
      SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      LineStippleFactor := 1;
      LineStipplePattern := FLinePattern;
    end
    else
      Disable(stLineStipple);
    if FAntiAliased then
    begin
      Enable(stLineSmooth);
      Enable(stBlend);
      SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
    end
    else
      Disable(stLineSmooth);
    LineWidth := FLineWidth;

    if FLineColor.Alpha <> 1 then
    begin
      if not FAntiAliased then
      begin
        Enable(stBlend);
        SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      end;
      gl.Color4fv(FLineColor.AsAddress);
    end
    else
      gl.Color3fv(FLineColor.AsAddress);

  end;
end;

// ------------------
// ------------------ TGLLinesNode ------------------
// ------------------

constructor TGLLinesNode.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FColor := TGLColor.Create(Self);
  FColor.Initialize((TGLLinesNodes(Collection).GetOwner as TGLLines)
    .NodeColor.Color);
  FColor.OnNotifyChange := OnColorChange;
end;

destructor TGLLinesNode.Destroy;
begin
  FColor.Free;
  inherited Destroy;
end;

procedure TGLLinesNode.Assign(Source: TPersistent);
begin
  if Source is TGLLinesNode then
    FColor.Assign(TGLLinesNode(Source).FColor);
  inherited;
end;

procedure TGLLinesNode.SetColor(const val: TGLColor);
begin
  FColor.Assign(val);
end;

procedure TGLLinesNode.OnColorChange(Sender: TObject);
begin
  (Collection as TGLNodes).NotifyChange;
end;

function TGLLinesNode.StoreColor: Boolean;
begin
  Result := not VectorEquals((TGLLinesNodes(Collection).GetOwner as TGLLines)
    .NodeColor.Color, FColor.Color);
end;

// ------------------
// ------------------ TGLLinesNodes ------------------
// ------------------

constructor TGLLinesNodes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TGLLinesNode);
end;

procedure TGLLinesNodes.NotifyChange;
begin
  if (GetOwner <> nil) then
    (GetOwner as TGLBaseSceneObject).StructureChanged;
end;

// ------------------
// ------------------ TGLNodedLines ------------------
// ------------------

constructor TGLNodedLines.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNodes := TGLLinesNodes.Create(Self);
  FNodeColor := TGLColor.Create(Self);
  FNodeColor.Initialize(clrBlue);
  FNodeColor.OnNotifyChange := OnNodeColorChanged;
  FOldNodeColor := clrBlue;
  FNodesAspect := lnaAxes;
  FNodeSize := 1;
end;

destructor TGLNodedLines.Destroy;
begin
  FNodes.Free;
  FNodeColor.Free;
  inherited Destroy;
end;

procedure TGLNodedLines.SetNodesAspect(const Value: TGLLineNodesAspect);
begin
  if Value <> FNodesAspect then
  begin
    FNodesAspect := Value;
    StructureChanged;
  end;
end;

procedure TGLNodedLines.SetNodeColor(const Value: TGLColor);
begin
  FNodeColor.Color := Value.Color;
  StructureChanged;
end;

procedure TGLNodedLines.OnNodeColorChanged(Sender: TObject);
var
  i: Integer;
begin
  // update color for nodes...
  for i := 0 to Nodes.Count - 1 do
    if VectorEquals(TGLLinesNode(Nodes[i]).Color.Color, FOldNodeColor) then
      TGLLinesNode(Nodes[i]).Color.Assign(FNodeColor);
  SetVector(FOldNodeColor, FNodeColor.Color);
end;

procedure TGLNodedLines.SetNodes(const aNodes: TGLLinesNodes);
begin
  FNodes.Assign(aNodes);
  StructureChanged;
end;

procedure TGLNodedLines.SetNodeSize(const val: Single);
begin
  if val <= 0 then
    FNodeSize := 1
  else
    FNodeSize := val;
  StructureChanged;
end;

function TGLNodedLines.StoreNodeSize: Boolean;
begin
  Result := FNodeSize <> 1;
end;

procedure TGLNodedLines.Assign(Source: TPersistent);
begin
  if Source is TGLNodedLines then
  begin
    SetNodes(TGLNodedLines(Source).FNodes);
    FNodesAspect := TGLNodedLines(Source).FNodesAspect;
    FNodeColor.Color := TGLNodedLines(Source).FNodeColor.Color;
    FNodeSize := TGLNodedLines(Source).FNodeSize;
  end;
  inherited Assign(Source);
end;

procedure TGLNodedLines.DrawNode(var rci: TGLRenderContextInfo; X, Y, Z: Single;
  Color: TGLColor);
begin
  gl.PushMatrix;
  gl.Translatef(X, Y, Z);
  case NodesAspect of
    lnaAxes:
      AxesBuildList(rci, $CCCC, FNodeSize * 0.5);
    lnaCube:
      CubeWireframeBuildList(rci, FNodeSize, False, Color.Color);
  else
    Assert(False)
  end;
  gl.PopMatrix;
end;

function TGLNodedLines.AxisAlignedDimensionsUnscaled: TGLVector;
var
  i: Integer;
begin
  RstVector(Result);
  for i := 0 to Nodes.Count - 1 do
    MaxVector(Result, VectorAbs(Nodes[i].AsVector));
  // EG: commented out, line below looks suspicious, since scale isn't taken
  // into account in previous loop, must have been hiding another bug... somewhere...
  // DivideVector(Result, Scale.AsVector);     //DanB ?
end;

procedure TGLNodedLines.AddNode(const coords: TGLCoordinates);
var
  n: TGLNode;
begin
  n := Nodes.Add;
  if Assigned(coords) then
    n.AsVector := coords.AsVector;
  StructureChanged;
end;

procedure TGLNodedLines.AddNode(const X, Y, Z: TGLFloat);
var
  n: TGLNode;
begin
  n := Nodes.Add;
  n.AsVector := VectorMake(X, Y, Z, 1);
  StructureChanged;
end;

procedure TGLNodedLines.AddNode(const Value: TGLVector);
var
  n: TGLNode;
begin
  n := Nodes.Add;
  n.AsVector := Value;
  StructureChanged;
end;

procedure TGLNodedLines.AddNode(const Value: TAffineVector);
var
  n: TGLNode;
begin
  n := Nodes.Add;
  n.AsVector := VectorMake(Value);
  StructureChanged;
end;

// ------------------
// ------------------ TGLLines ------------------
// ------------------

constructor TGLLines.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDivision := 10;
  FSplineMode := lsmLines;
  FNURBSKnots := TSingleList.Create;
  FNURBSOrder := 0;
  FNURBSTolerance := 50;
end;

destructor TGLLines.Destroy;
begin
  FNURBSKnots.Free;
  inherited Destroy;
end;

procedure TGLLines.SetDivision(const Value: Integer);
begin
  if Value <> FDivision then
  begin
    if Value < 1 then
      FDivision := 1
    else
      FDivision := Value;
    StructureChanged;
  end;
end;

procedure TGLLines.SetOptions(const val: TGLLinesOptions);
begin
  FOptions := val;
  StructureChanged;
end;

procedure TGLLines.SetSplineMode(const val: TGLLineSplineMode);
begin
  if FSplineMode <> val then
  begin
    FSplineMode := val;
    StructureChanged;
  end;
end;

procedure TGLLines.SetNURBSOrder(const val: Integer);
begin
  if val <> FNURBSOrder then
  begin
    FNURBSOrder := val;
    StructureChanged;
  end;
end;

procedure TGLLines.SetNURBSTolerance(const val: Single);
begin
  if val <> FNURBSTolerance then
  begin
    FNURBSTolerance := val;
    StructureChanged;
  end;
end;

procedure TGLLines.Assign(Source: TPersistent);
begin
  if Source is TGLLines then
  begin
    FDivision := TGLLines(Source).FDivision;
    FSplineMode := TGLLines(Source).FSplineMode;
    FOptions := TGLLines(Source).FOptions;
  end;
  inherited Assign(Source);
end;

procedure TGLLines.BuildList(var rci: TGLRenderContextInfo);
var
  i, n: Integer;
  A, B, C: TGLFloat;
  f: Single;
  Spline: TCubicSpline;
  vertexColor: TGLVector;
  nodeBuffer: array of TAffineVector;
  colorBuffer: array of TGLVector;
  nurbsRenderer: PGLUNurbs;
begin
  if Nodes.Count > 1 then
  begin
    // first, we setup the line color & stippling styles
    SetupLineStyle(rci);
    if rci.bufferDepthTest then
      rci.GLStates.Enable(stDepthTest);
    if loColorLogicXor in Options then
    begin
      rci.GLStates.Enable(stColorLogicOp);
      rci.GLStates.LogicOpMode := loXOr;
    end;
    // Set up the control point buffer for Bezier splines and NURBS curves.
    // If required this could be optimized by storing a cached node buffer.
    if (FSplineMode = lsmBezierSpline) or (FSplineMode = lsmNURBSCurve) then
    begin
      SetLength(nodeBuffer, Nodes.Count);
      SetLength(colorBuffer, Nodes.Count);
      for i := 0 to Nodes.Count - 1 do
        with TGLLinesNode(Nodes[i]) do
        begin
          nodeBuffer[i] := AsAffineVector;
          colorBuffer[i] := Color.Color;
        end;
    end;
    if FSplineMode = lsmBezierSpline then
    begin
      // map evaluator
      rci.GLStates.PushAttrib([sttEval]);
      gl.Enable(GL_MAP1_VERTEX_3);
      gl.Enable(GL_MAP1_COLOR_4);
      gl.Map1f(GL_MAP1_VERTEX_3, 0, 1, 3, Nodes.Count, @nodeBuffer[0]);
      gl.Map1f(GL_MAP1_COLOR_4, 0, 1, 4, Nodes.Count, @colorBuffer[0]);
    end;

    // start drawing the line
    if (FSplineMode = lsmNURBSCurve) and (FDivision >= 2) then
    begin
      if (FNURBSOrder > 0) and (FNURBSKnots.Count > 0) then
      begin
        nurbsRenderer := gluNewNurbsRenderer;
        // try
        gluNurbsProperty(nurbsRenderer, GLU_SAMPLING_TOLERANCE,
          FNURBSTolerance);
        gluNurbsProperty(nurbsRenderer, GLU_DISPLAY_MODE, GLU_FILL);
        gluBeginCurve(nurbsRenderer);
        gluNurbsCurve(nurbsRenderer, FNURBSKnots.Count, @FNURBSKnots.List[0], 3,
          @nodeBuffer[0], FNURBSOrder, GL_MAP1_VERTEX_3);
        gluEndCurve(nurbsRenderer);
        // finally
        gluDeleteNurbsRenderer(nurbsRenderer);
        // end;
      end;
    end
    else
    begin
      // lines, cubic splines or bezier
      if FSplineMode = lsmSegments then
        gl.Begin_(GL_LINES)
      else if FSplineMode = lsmLoop then
        gl.Begin_(GL_LINE_LOOP)
      else
        gl.Begin_(GL_LINE_STRIP);
      if (FDivision < 2) or (FSplineMode in [lsmLines, lsmSegments, lsmLoop])
      then
      begin
        // standard line(s), draw directly
        if loUseNodeColorForLines in Options then
        begin
          // node color interpolation
          for i := 0 to Nodes.Count - 1 do
            with TGLLinesNode(Nodes[i]) do
            begin
              gl.Color4fv(Color.AsAddress);
              gl.Vertex3f(X, Y, Z);
            end;
        end
        else
        begin
          // single color
          for i := 0 to Nodes.Count - 1 do
            with Nodes[i] do
              gl.Vertex3f(X, Y, Z);
        end;
      end
      else if FSplineMode = lsmCubicSpline then
      begin
        // cubic spline
        Spline := Nodes.CreateNewCubicSpline;
        // try
        f := 1 / FDivision;
        for i := 0 to (Nodes.Count - 1) * FDivision do
        begin
          Spline.SplineXYZ(i * f, A, B, C);
          if loUseNodeColorForLines in Options then
          begin
            n := (i div FDivision);
            if n < Nodes.Count - 1 then
              VectorLerp(TGLLinesNode(Nodes[n]).Color.Color,
                TGLLinesNode(Nodes[n + 1]).Color.Color, (i mod FDivision) * f,
                vertexColor)
            else
              SetVector(vertexColor, TGLLinesNode(Nodes[Nodes.Count - 1])
                .Color.Color);
            gl.Color4fv(@vertexColor);
          end;
          gl.Vertex3f(A, B, C);
        end;
        // finally
        Spline.Free;
        // end;
      end
      else if FSplineMode = lsmBezierSpline then
      begin
        f := 1 / FDivision;
        for i := 0 to FDivision do
          gl.EvalCoord1f(i * f);
      end;
      gl.End_;
    end;
    rci.GLStates.Disable(stColorLogicOp);
    if FSplineMode = lsmBezierSpline then
      rci.GLStates.PopAttrib;
    if Length(nodeBuffer) > 0 then
    begin
      SetLength(nodeBuffer, 0);
      SetLength(colorBuffer, 0);
    end;
    if FNodesAspect <> lnaInvisible then
    begin
      if not rci.ignoreBlendingRequests then
      begin
        rci.GLStates.Enable(stBlend);
        rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      end;
      for i := 0 to Nodes.Count - 1 do
        with TGLLinesNode(Nodes[i]) do
          DrawNode(rci, X, Y, Z, Color);
    end;
  end;
end;

// ------------------
// ------------------ TGLCube ------------------
// ------------------

constructor TGLCube.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCubeSize := XYZVector;

  FParts := [cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight];
  FNormalDirection := ndOutside;
  ObjectStyle := ObjectStyle + [osDirectDraw];
end;

procedure TGLCube.BuildList(var rci: TGLRenderContextInfo);
var
  v1: TAffineVector;
  v2: TAffineVector;
  v1d: TAffineVector;
  v2d: TAffineVector;
  nd: TGLFloat;
  TanLoc, BinLoc: Integer;
begin
  VectorScale(FCubeSize, 0.5, v2);
  v1 := VectorNegate(v2);
  if FNormalDirection = ndInside then
  begin
    v1d := v2;
    v2d := v1;
    nd := -1
  end
  else
  begin
    v1d := v1;
    v2d := v2;
    nd := 1;
  end;

  if GL.ARB_shader_objects and (rci.GLStates.CurrentProgram > 0) then
  begin
    TanLoc := gl.GetAttribLocation(rci.GLStates.CurrentProgram,
      TangentAttributeName);
    BinLoc := gl.GetAttribLocation(rci.GLStates.CurrentProgram,
      BinormalAttributeName);
  end
  else
  begin
    TanLoc := -1;
    BinLoc := -1;
  end;
  gl.Begin_(GL_QUADS);
  if cpFront in FParts then
  begin
    gl.Normal3f(0, 0, nd);
    if TanLoc > -1 then
      gl.VertexAttrib3f(TanLoc, nd, 0, 0);
    if BinLoc > -1 then
      gl.VertexAttrib3f(BinLoc, 0, nd, 0);
    xgl.TexCoord2fv(@XYTexPoint);
    gl.Vertex3fv(@v2);
    xgl.TexCoord2fv(@YTexPoint);
    gl.Vertex3f(v1d.X, v2d.Y, v2.Z);
    xgl.TexCoord2fv(@NullTexPoint);
    gl.Vertex3f(v1.X, v1.Y, v2.Z);
    xgl.TexCoord2fv(@XTexPoint);
    gl.Vertex3f(v2d.X, v1d.Y, v2.Z);
  end;
  if cpBack in FParts then
  begin
    gl.Normal3f(0, 0, -nd);
    if TanLoc > -1 then
      gl.VertexAttrib3f(TanLoc, -nd, 0, 0);
    if BinLoc > -1 then
      gl.VertexAttrib3f(BinLoc, 0, nd, 0);

    xgl.TexCoord2fv(@YTexPoint);
    gl.Vertex3f(v2.X, v2.Y, v1.Z);
    xgl.TexCoord2fv(@NullTexPoint);
    gl.Vertex3f(v2d.X, v1d.Y, v1.Z);
    xgl.TexCoord2fv(@XTexPoint);
    gl.Vertex3fv(@v1);
    xgl.TexCoord2fv(@XYTexPoint);
    gl.Vertex3f(v1d.X, v2d.Y, v1.Z);
  end;
  if cpLeft in FParts then
  begin
    gl.Normal3f(-nd, 0, 0);
    if TanLoc > -1 then
      gl.VertexAttrib3f(TanLoc, 0, 0, nd);
    if BinLoc > -1 then
      gl.VertexAttrib3f(BinLoc, 0, nd, 0);
    xgl.TexCoord2fv(@XYTexPoint);
    gl.Vertex3f(v1.X, v2.Y, v2.Z);
    xgl.TexCoord2fv(@YTexPoint);
    gl.Vertex3f(v1.X, v2d.Y, v1d.Z);
    xgl.TexCoord2fv(@NullTexPoint);
    gl.Vertex3fv(@v1);
    xgl.TexCoord2fv(@XTexPoint);
    gl.Vertex3f(v1.X, v1d.Y, v2d.Z);
  end;
  if cpRight in FParts then
  begin
    gl.Normal3f(nd, 0, 0);
    if TanLoc > -1 then
      gl.VertexAttrib3f(TanLoc, 0, 0, -nd);
    if BinLoc > -1 then
      gl.VertexAttrib3f(BinLoc, 0, nd, 0);
    xgl.TexCoord2fv(@YTexPoint);
    gl.Vertex3fv(@v2);
    xgl.TexCoord2fv(@NullTexPoint);
    gl.Vertex3f(v2.X, v1d.Y, v2d.Z);
    xgl.TexCoord2fv(@XTexPoint);
    gl.Vertex3f(v2.X, v1.Y, v1.Z);
    xgl.TexCoord2fv(@XYTexPoint);
    gl.Vertex3f(v2.X, v2d.Y, v1d.Z);
  end;
  if cpTop in FParts then
  begin
    gl.Normal3f(0, nd, 0);
    if TanLoc > -1 then
      gl.VertexAttrib3f(TanLoc, nd, 0, 0);
    if BinLoc > -1 then
      gl.VertexAttrib3f(BinLoc, 0, 0, -nd);
    xgl.TexCoord2fv(@YTexPoint);
    gl.Vertex3f(v1.X, v2.Y, v1.Z);
    xgl.TexCoord2fv(@NullTexPoint);
    gl.Vertex3f(v1d.X, v2.Y, v2d.Z);
    xgl.TexCoord2fv(@XTexPoint);
    gl.Vertex3fv(@v2);
    xgl.TexCoord2fv(@XYTexPoint);
    gl.Vertex3f(v2d.X, v2.Y, v1d.Z);
  end;
  if cpBottom in FParts then
  begin
    gl.Normal3f(0, -nd, 0);
    if TanLoc > -1 then
      gl.VertexAttrib3f(TanLoc, -nd, 0, 0);
    if BinLoc > -1 then
      gl.VertexAttrib3f(BinLoc, 0, 0, nd);
    xgl.TexCoord2fv(@NullTexPoint);
    gl.Vertex3fv(@v1);
    xgl.TexCoord2fv(@XTexPoint);
    gl.Vertex3f(v2d.X, v1.Y, v1d.Z);
    xgl.TexCoord2fv(@XYTexPoint);
    gl.Vertex3f(v2.X, v1.Y, v2.Z);
    xgl.TexCoord2fv(@YTexPoint);
    gl.Vertex3f(v1d.X, v1.Y, v2d.Z);
  end;
  gl.End_;
end;

function TGLCube.GenerateSilhouette(const silhouetteParameters
  : TGLSilhouetteParameters): TGLSilhouette;
var
  hw, hh, hd: TGLFloat;
  Connectivity: TGLConnectivity;
  sil: TGLSilhouette;
begin
  Connectivity := TGLConnectivity.Create(True);

  hw := FCubeSize.X * 0.5;
  hh := FCubeSize.Y * 0.5;
  hd := FCubeSize.Z * 0.5;

  if cpFront in FParts then
  begin
    Connectivity.AddQuad(AffineVectorMake(hw, hh, hd),
      AffineVectorMake(-hw, hh, hd), AffineVectorMake(-hw, -hh, hd),
      AffineVectorMake(hw, -hh, hd));
  end;
  if cpBack in FParts then
  begin
    Connectivity.AddQuad(AffineVectorMake(hw, hh, -hd),
      AffineVectorMake(hw, -hh, -hd), AffineVectorMake(-hw, -hh, -hd),
      AffineVectorMake(-hw, hh, -hd));
  end;
  if cpLeft in FParts then
  begin
    Connectivity.AddQuad(AffineVectorMake(-hw, hh, hd),
      AffineVectorMake(-hw, hh, -hd), AffineVectorMake(-hw, -hh, -hd),
      AffineVectorMake(-hw, -hh, hd));
  end;
  if cpRight in FParts then
  begin
    Connectivity.AddQuad(AffineVectorMake(hw, hh, hd),
      AffineVectorMake(hw, -hh, hd), AffineVectorMake(hw, -hh, -hd),
      AffineVectorMake(hw, hh, -hd));
  end;
  if cpTop in FParts then
  begin
    Connectivity.AddQuad(AffineVectorMake(-hw, hh, -hd),
      AffineVectorMake(-hw, hh, hd), AffineVectorMake(hw, hh, hd),
      AffineVectorMake(hw, hh, -hd));
  end;
  if cpBottom in FParts then
  begin
    Connectivity.AddQuad(AffineVectorMake(-hw, -hh, -hd),
      AffineVectorMake(hw, -hh, -hd), AffineVectorMake(hw, -hh, hd),
      AffineVectorMake(-hw, -hh, hd));
  end;
  sil := nil;
  Connectivity.CreateSilhouette(silhouetteParameters, sil, False);
  Result := sil;
  Connectivity.Free;
end;

function TGLCube.GetCubeWHD(const Index: Integer): TGLFloat;
begin
  Result := FCubeSize.v[index];
end;

procedure TGLCube.SetCubeWHD(Index: Integer; aValue: TGLFloat);
begin
  if aValue <> FCubeSize.v[index] then
  begin
    FCubeSize.v[index] := aValue;
    StructureChanged;
  end;
end;

procedure TGLCube.SetParts(aValue: TGLCubeParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

procedure TGLCube.SetNormalDirection(aValue: TGLNormalDirection);
begin
  if aValue <> FNormalDirection then
  begin
    FNormalDirection := aValue;
    StructureChanged;
  end;
end;

procedure TGLCube.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLCube) then
  begin
    FCubeSize := TGLCube(Source).FCubeSize;
    FParts := TGLCube(Source).FParts;
    FNormalDirection := TGLCube(Source).FNormalDirection;
  end;
  inherited Assign(Source);
end;

function TGLCube.AxisAlignedDimensionsUnscaled: TGLVector;
begin
  Result.X := FCubeSize.X * 0.5;
  Result.Y := FCubeSize.Y * 0.5;
  Result.Z := FCubeSize.Z * 0.5;
  Result.W := 0;
end;

function TGLCube.RayCastIntersect(const rayStart, rayVector: TGLVector;
  intersectPoint: PGLVector = nil; intersectNormal: PGLVector = nil): Boolean;
var
  p: array [0 .. 5] of TGLVector;
  rv: TGLVector;
  rs, r: TGLVector;
  i: Integer;
  t: Single;
  eSize: TAffineVector;
begin
  rs := AbsoluteToLocal(rayStart);
  SetVector(rv, VectorNormalize(AbsoluteToLocal(rayVector)));
  eSize.X := FCubeSize.X * 0.5 + 0.0001;
  eSize.Y := FCubeSize.Y * 0.5 + 0.0001;
  eSize.Z := FCubeSize.Z * 0.5 + 0.0001;
  p[0] := XHmgVector;
  p[1] := YHmgVector;
  p[2] := ZHmgVector;
  SetVector(p[3], -1, 0, 0);
  SetVector(p[4], 0, -1, 0);
  SetVector(p[5], 0, 0, -1);
  for i := 0 to 5 do
  begin
    if VectorDotProduct(p[i], rv) > 0 then
    begin
      t := -(p[i].X * rs.X + p[i].Y * rs.Y + p[i].Z * rs.Z + 0.5 * FCubeSize.v
        [i mod 3]) / (p[i].X * rv.X + p[i].Y * rv.Y + p[i].Z * rv.Z);
      MakePoint(r, rs.X + t * rv.X, rs.Y + t * rv.Y, rs.Z + t * rv.Z);
      if (Abs(r.X) <= eSize.X) and (Abs(r.Y) <= eSize.Y) and
        (Abs(r.Z) <= eSize.Z) and
        (VectorDotProduct(VectorSubtract(r, rs), rv) > 0) then
      begin
        if Assigned(intersectPoint) then
          MakePoint(intersectPoint^, LocalToAbsolute(r));
        if Assigned(intersectNormal) then
          MakeVector(intersectNormal^, LocalToAbsolute(VectorNegate(p[i])));
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

procedure TGLCube.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('CubeSize', ReadData, WriteData,
    (FCubeSize.X <> 1) or (FCubeSize.Y <> 1) or (FCubeSize.Z <> 1));
end;

procedure TGLCube.ReadData(Stream: TStream);
begin
  with Stream do
  begin
    Read(FCubeSize, SizeOf(TAffineVector));
  end;
end;

procedure TGLCube.WriteData(Stream: TStream);
begin
  with Stream do
  begin
    Write(FCubeSize, SizeOf(TAffineVector));
  end;
end;

// ------------------
// ------------------ TGLQuadricObject ------------------
// ------------------

constructor TGLQuadricObject.Create(AOwner: TComponent);
begin
  inherited;
  FNormals := nsSmooth;
  FNormalDirection := ndOutside;
end;

procedure TGLQuadricObject.SetNormals(aValue: TGLNormalSmoothing);
begin
  if aValue <> FNormals then
  begin
    FNormals := aValue;
    StructureChanged;
  end;
end;

procedure TGLQuadricObject.SetNormalDirection(aValue: TGLNormalDirection);
begin
  if aValue <> FNormalDirection then
  begin
    FNormalDirection := aValue;
    StructureChanged;
  end;
end;

procedure TGLQuadricObject.SetupQuadricParams(quadric: PGLUquadricObj);
const
  cNormalSmoothinToEnum: array [nsFlat .. nsNone] of Cardinal = (GLU_FLAT,
    GLU_SMOOTH, GLU_NONE);
begin
  gluQuadricDrawStyle(quadric, GLU_FILL);
  gluQuadricNormals(quadric, cNormalSmoothinToEnum[FNormals]);
  SetNormalQuadricOrientation(quadric);
  gluQuadricTexture(quadric, True);
end;

procedure TGLQuadricObject.SetNormalQuadricOrientation(quadric: PGLUquadricObj);
const
  cNormalDirectionToEnum: array [ndInside .. ndOutside] of Cardinal =
    (GLU_INSIDE, GLU_OUTSIDE);
begin
  gluQuadricOrientation(quadric, cNormalDirectionToEnum[FNormalDirection]);
end;

procedure TGLQuadricObject.SetInvertedQuadricOrientation
  (quadric: PGLUquadricObj);
const
  cNormalDirectionToEnum: array [ndInside .. ndOutside] of Cardinal =
    (GLU_OUTSIDE, GLU_INSIDE);
begin
  gluQuadricOrientation(quadric, cNormalDirectionToEnum[FNormalDirection]);
end;

procedure TGLQuadricObject.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLQuadricObject) then
  begin
    FNormals := TGLQuadricObject(Source).FNormals;
    FNormalDirection := TGLQuadricObject(Source).FNormalDirection;
  end;
  inherited Assign(Source);
end;

// ------------------
// ------------------ TGLSphere ------------------
// ------------------

constructor TGLSphere.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRadius := 0.5;
  FSlices := 16;
  FStacks := 16;
  FTop := 90;
  FBottom := -90;
  FStart := 0;
  FStop := 360;
end;

procedure TGLSphere.BuildList(var rci: TGLRenderContextInfo);
var
  v1, v2, N1: TAffineVector;
  AngTop, AngBottom, AngStart, AngStop, StepV, StepH: Double;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Double;
  uTexCoord, uTexFactor, vTexFactor, vTexCoord0, vTexCoord1: Single;
  i, j: Integer;
  DoReverse: Boolean;
begin
  DoReverse := (FNormalDirection = ndInside);
  rci.GLStates.PushAttrib([sttPolygon]);
  if DoReverse then
    rci.GLStates.InvertGLFrontFace;

  // common settings
  AngTop := DegToRad(1.0 * FTop);
  AngBottom := DegToRad(1.0 * FBottom);
  AngStart := DegToRad(1.0 * FStart);
  AngStop := DegToRad(1.0 * FStop);
  StepH := (AngStop - AngStart) / FSlices;
  StepV := (AngTop - AngBottom) / FStacks;
  gl.PushMatrix;
  gl.Scalef(Radius, Radius, Radius);

  // top cap
  if (FTop < 90) and (FTopCap in [ctCenter, ctFlat]) then
  begin
    gl.Begin_(GL_TRIANGLE_FAN);
    SinCosine(AngTop, SinP, CosP);
    xgl.TexCoord2f(0.5, 0.5);
    if DoReverse then
      gl.Normal3f(0, -1, 0)
    else
      gl.Normal3f(0, 1, 0);
    if FTopCap = ctCenter then
      gl.Vertex3f(0, 0, 0)
    else
    begin
      gl.Vertex3f(0, SinP, 0);
      N1 := YVector;
      if DoReverse then
        N1.Y := -N1.Y;
    end;
    v1.Y := SinP;
    Theta := AngStart;
    for i := 0 to FSlices do
    begin
      SinCosine(Theta, SinT, CosT);
      v1.X := CosP * SinT;
      v1.Z := CosP * CosT;
      if FTopCap = ctCenter then
      begin
        N1 := VectorPerpendicular(YVector, v1);
        if DoReverse then
          NegateVector(N1);
      end;
      xgl.TexCoord2f(SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
      gl.Normal3fv(@N1);
      gl.Vertex3fv(@v1);
      Theta := Theta + StepH;
    end;
    gl.End_;
  end;

  // main body
  Phi := AngTop;
  Phi2 := Phi - StepV;
  uTexFactor := 1 / FSlices;
  vTexFactor := 1 / FStacks;

  for j := 0 to FStacks - 1 do
  begin
    Theta := AngStart;
    SinCos(Phi, SinP, CosP);
    SinCos(Phi2, SinP2, CosP2);
    v1.Y := SinP;
    v2.Y := SinP2;
    vTexCoord0 := 1 - j * vTexFactor;
    vTexCoord1 := 1 - (j + 1) * vTexFactor;

    gl.Begin_(GL_TRIANGLE_STRIP);
    for i := 0 to FSlices do
    begin

      SinCos(Theta, SinT, CosT);
      v1.X := CosP * SinT;
      v2.X := CosP2 * SinT;
      v1.Z := CosP * CosT;
      v2.Z := CosP2 * CosT;

      uTexCoord := i * uTexFactor;
      xgl.TexCoord2f(uTexCoord, vTexCoord0);
      if DoReverse then
      begin
        N1 := VectorNegate(v1);
        gl.Normal3fv(@N1);
      end
      else
        gl.Normal3fv(@v1);
      gl.Vertex3fv(@v1);

      xgl.TexCoord2f(uTexCoord, vTexCoord1);
      if DoReverse then
      begin
        N1 := VectorNegate(v2);
        gl.Normal3fv(@N1);
      end
      else
        gl.Normal3fv(@v2);
      gl.Vertex3fv(@v2);
      Theta := Theta + StepH;
    end;
    gl.End_;
    Phi := Phi2;
    Phi2 := Phi2 - StepV;
  end;

  // bottom cap
  if (FBottom > -90) and (FBottomCap in [ctCenter, ctFlat]) then
  begin
    gl.Begin_(GL_TRIANGLE_FAN);
    SinCos(AngBottom, SinP, CosP);
    xgl.TexCoord2f(0.5, 0.5);
    if DoReverse then
      gl.Normal3f(0, 1, 0)
    else
      gl.Normal3f(0, -1, 0);
    if FBottomCap = ctCenter then
      gl.Vertex3f(0, 0, 0)
    else
    begin
      gl.Vertex3f(0, SinP, 0);
      if DoReverse then
        MakeVector(N1, 0, -1, 0)
      else
      begin
        N1 := YVector;
        NegateVector(N1);
      end;
    end;
    v1.Y := SinP;
    Theta := AngStop;
    for i := 0 to FSlices do
    begin
      SinCos(Theta, SinT, CosT);
      v1.X := CosP * SinT;
      v1.Z := CosP * CosT;
      if FBottomCap = ctCenter then
      begin
        N1 := VectorPerpendicular(AffineVectorMake(0, -1, 0), v1);
        if DoReverse then
          NegateVector(N1);
      end;
      xgl.TexCoord2f(SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
      gl.Normal3fv(@N1);
      gl.Vertex3fv(@v1);
      Theta := Theta - StepH;
    end;
    gl.End_;
  end;
  if DoReverse then
    rci.GLStates.InvertGLFrontFace;
  gl.PopMatrix;
  rci.GLStates.PopAttrib;
end;

function TGLSphere.RayCastIntersect(const rayStart, rayVector: TGLVector;
  intersectPoint: PGLVector = nil; intersectNormal: PGLVector = nil): Boolean;
var
  i1, i2: TGLVector;
  localStart, localVector: TGLVector;
begin
  // compute coefficients of quartic polynomial
  SetVector(localStart, AbsoluteToLocal(rayStart));
  SetVector(localVector, AbsoluteToLocal(rayVector));
  NormalizeVector(localVector);
  if RayCastSphereIntersect(localStart, localVector, NullHmgVector, Radius, i1,
    i2) > 0 then
  begin
    Result := True;
    if Assigned(intersectPoint) then
      SetVector(intersectPoint^, LocalToAbsolute(i1));
    if Assigned(intersectNormal) then
    begin
      i1.W := 0; // vector transform
      SetVector(intersectNormal^, LocalToAbsolute(i1));
    end;
  end
  else
    Result := False;
end;

function TGLSphere.GenerateSilhouette(const silhouetteParameters
  : TGLSilhouetteParameters): TGLSilhouette;
var
  i, j: Integer;
  s, C, angleFactor: Single;
  sVec, tVec: TAffineVector;
  Segments: Integer;
begin
  Segments := MaxInteger(FStacks, FSlices);

  // determine a local orthonormal matrix, viewer-oriented
  sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, XVector);
  if VectorLength(sVec) < 1E-3 then
    sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, YVector);
  tVec := VectorCrossProduct(silhouetteParameters.SeenFrom, sVec);
  NormalizeVector(sVec);
  NormalizeVector(tVec);
  // generate the silhouette (outline and capping)
  Result := TGLSilhouette.Create;
  angleFactor := (2 * PI) / Segments;
  for i := 0 to Segments - 1 do
  begin
    SinCosine(i * angleFactor, FRadius, s, C);
    Result.vertices.AddPoint(VectorCombine(sVec, tVec, s, C));
    j := (i + 1) mod Segments;
    Result.Indices.Add(i, j);
    if silhouetteParameters.CappingRequired then
      Result.CapIndices.Add(Segments, i, j)
  end;
  if silhouetteParameters.CappingRequired then
    Result.vertices.Add(NullHmgPoint);
end;

procedure TGLSphere.SetBottom(aValue: TAngleLimit1);
begin
  if FBottom <> aValue then
  begin
    FBottom := aValue;
    StructureChanged;
  end;
end;

procedure TGLSphere.SetBottomCap(aValue: TGLCapType);
begin
  if FBottomCap <> aValue then
  begin
    FBottomCap := aValue;
    StructureChanged;
  end;
end;

procedure TGLSphere.SetRadius(const aValue: TGLFloat);
begin
  if aValue <> FRadius then
  begin
    FRadius := aValue;
    StructureChanged;
  end;
end;

procedure TGLSphere.SetSlices(aValue: TGLInt);
begin
  if aValue <> FSlices then
  begin
    if aValue <= 0 then
      FSlices := 1
    else
      FSlices := aValue;
    StructureChanged;
  end;
end;

procedure TGLSphere.SetStacks(aValue: TGLInt);
begin
  if aValue <> FStacks then
  begin
    if aValue <= 0 then
      FStacks := 1
    else
      FStacks := aValue;
    StructureChanged;
  end;
end;

procedure TGLSphere.SetStart(aValue: TAngleLimit2);
begin
  if FStart <> aValue then
  begin
    Assert(aValue <= FStop);
    FStart := aValue;
    StructureChanged;
  end;
end;

procedure TGLSphere.SetStop(aValue: TAngleLimit2);
begin
  if FStop <> aValue then
  begin
    Assert(aValue >= FStart);
    FStop := aValue;
    StructureChanged;
  end;
end;

procedure TGLSphere.SetTop(aValue: TAngleLimit1);
begin
  if FTop <> aValue then
  begin
    FTop := aValue;
    StructureChanged;
  end;
end;

procedure TGLSphere.SetTopCap(aValue: TGLCapType);
begin
  if FTopCap <> aValue then
  begin
    FTopCap := aValue;
    StructureChanged;
  end;
end;

procedure TGLSphere.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLSphere) then
  begin
    FRadius := TGLSphere(Source).FRadius;
    FSlices := TGLSphere(Source).FSlices;
    FStacks := TGLSphere(Source).FStacks;
    FBottom := TGLSphere(Source).FBottom;
    FTop := TGLSphere(Source).FTop;
    FStart := TGLSphere(Source).FStart;
    FStop := TGLSphere(Source).FStop;
  end;
  inherited Assign(Source);
end;

function TGLSphere.AxisAlignedDimensionsUnscaled: TGLVector;
begin
  Result.X := Abs(FRadius);
  Result.Y := Result.X;
  Result.Z := Result.X;
  Result.W := 0;
end;

// ------------------
// ------------------ TGLPolygonBase ------------------
// ------------------

constructor TGLPolygonBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateNodes;
  FDivision := 10;
  FSplineMode := lsmLines;
end;

procedure TGLPolygonBase.CreateNodes;
begin
  FNodes := TGLNodes.Create(Self);
end;

destructor TGLPolygonBase.Destroy;
begin
  FNodes.Free;
  inherited Destroy;
end;

procedure TGLPolygonBase.Assign(Source: TPersistent);
begin
  if Source is TGLPolygonBase then
  begin
    SetNodes(TGLPolygonBase(Source).FNodes);
    FDivision := TGLPolygonBase(Source).FDivision;
    FSplineMode := TGLPolygonBase(Source).FSplineMode;
  end;
  inherited Assign(Source);
end;

procedure TGLPolygonBase.NotifyChange(Sender: TObject);
begin
  if Sender = Nodes then
    StructureChanged;
  inherited;
end;

procedure TGLPolygonBase.SetDivision(const Value: Integer);
begin
  if Value <> FDivision then
  begin
    if Value < 1 then
      FDivision := 1
    else
      FDivision := Value;
    StructureChanged;
  end;
end;

procedure TGLPolygonBase.SetNodes(const aNodes: TGLNodes);
begin
  FNodes.Assign(aNodes);
  StructureChanged;
end;

procedure TGLPolygonBase.SetSplineMode(const val: TGLLineSplineMode);
begin
  if FSplineMode <> val then
  begin
    FSplineMode := val;
    StructureChanged;
  end;
end;

procedure TGLPolygonBase.AddNode(const coords: TGLCoordinates);
var
  n: TGLNode;
begin
  n := Nodes.Add;
  if Assigned(coords) then
    n.AsVector := coords.AsVector;
  StructureChanged;
end;

procedure TGLPolygonBase.AddNode(const X, Y, Z: TGLFloat);
var
  n: TGLNode;
begin
  n := Nodes.Add;
  n.AsVector := VectorMake(X, Y, Z, 1);
  StructureChanged;
end;

procedure TGLPolygonBase.AddNode(const Value: TGLVector);
var
  n: TGLNode;
begin
  n := Nodes.Add;
  n.AsVector := Value;
  StructureChanged;
end;

procedure TGLPolygonBase.AddNode(const Value: TAffineVector);
var
  n: TGLNode;
begin
  n := Nodes.Add;
  n.AsVector := VectorMake(Value);
  StructureChanged;
end;

// ------------------
// ------------------ TGLSuperellipsoid ------------------
// ------------------

constructor TGLSuperellipsoid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRadius := 0.5;
  FVCurve := 1.0;
  FHCurve := 1.0;
  FSlices := 16;
  FStacks := 16;
  FTop := 90;
  FBottom := -90;
  FStart := 0;
  FStop := 360;
end;

procedure TGLSuperellipsoid.BuildList(var rci: TGLRenderContextInfo);
var
  CosPc1, SinPc1, CosTc2, SinTc2: Double;
  tc1, tc2: Integer;
  v1, v2, vs, N1: TAffineVector;
  AngTop, AngBottom, AngStart, AngStop, StepV, StepH: Double;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Double;
  uTexCoord, uTexFactor, vTexFactor, vTexCoord0, vTexCoord1: Double;
  i, j: Integer;
  DoReverse: Boolean;

begin
  DoReverse := (FNormalDirection = ndInside);
  if DoReverse then
    rci.GLStates.InvertGLFrontFace;

  // common settings
  AngTop := DegToRad(1.0 * FTop);
  AngBottom := DegToRad(1.0 * FBottom);
  AngStart := DegToRad(1.0 * FStart);
  AngStop := DegToRad(1.0 * FStop);
  StepH := (AngStop - AngStart) / FSlices;
  StepV := (AngTop - AngBottom) / FStacks;

  { Even integer used with the Power function, only produce positive points }
  tc1 := trunc(VCurve);
  tc2 := trunc(HCurve);
  if tc1 mod 2 = 0 then
    VCurve := VCurve + 1E-6;
  if tc2 mod 2 = 0 then
    HCurve := HCurve - 1E-6;

  // top cap
  if (FTop < 90) and (FTopCap in [ctCenter, ctFlat]) then
  begin
    gl.Begin_(GL_TRIANGLE_FAN);
    SinCos(AngTop, SinP, CosP);
    xgl.TexCoord2f(0.5, 0.5);
    if DoReverse then
      gl.Normal3f(0, -1, 0)
    else
      gl.Normal3f(0, 1, 0);

    if FTopCap = ctCenter then
      gl.Vertex3f(0, 0, 0)
    else
    begin { FTopCap = ctFlat }
      if (Sign(SinP) = 1) or (tc1 = VCurve) then
        SinPc1 := Power(SinP, VCurve)
      else
        SinPc1 := -Power(-SinP, VCurve);
      gl.Vertex3f(0, SinPc1 * Radius, 0);

      N1 := YVector;
      if DoReverse then
        N1.Y := -N1.Y;
    end; { FTopCap = ctFlat }

    // v1.Y := SinP;
    if (Sign(SinP) = 1) or (tc1 = VCurve) then
      SinPc1 := Power(SinP, VCurve)
    else
      SinPc1 := -Power(-SinP, VCurve);
    v1.Y := SinPc1;

    Theta := AngStart;

    for i := 0 to FSlices do
    begin
      SinCos(Theta, SinT, CosT);
      // v1.X := CosP * SinT;
      if (Sign(CosP) = 1) or (tc1 = VCurve) then
        CosPc1 := Power(CosP, VCurve)
      else
        CosPc1 := -Power(-CosP, VCurve);
      if (Sign(SinT) = 1) or (tc2 = HCurve) then
        SinTc2 := Power(SinT, HCurve)
      else
        SinTc2 := -Power(-SinT, HCurve);
      v1.X := CosPc1 * SinTc2;
      // v1.Z := CosP * CosT;
      if (Sign(CosT) = 1) or (tc2 = HCurve) then
        CosTc2 := Power(CosT, HCurve)
      else
        CosTc2 := -Power(-CosT, HCurve);
      v1.Z := CosPc1 * CosTc2;
      if FTopCap = ctCenter then
      begin
        N1 := VectorPerpendicular(YVector, v1);
        if DoReverse then
          NegateVector(N1);
      end;
      // xgl.TexCoord2f(SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
      xgl.TexCoord2f(SinTc2 * 0.5 + 0.5, CosTc2 * 0.5 + 0.5);
      gl.Normal3fv(@N1);
      vs := v1;
      ScaleVector(vs, Radius);
      gl.Vertex3fv(@vs);
      Theta := Theta + StepH;
    end;
    gl.End_;
  end;

  // main body
  Phi := AngTop;
  Phi2 := Phi - StepV;
  uTexFactor := 1 / FSlices;
  vTexFactor := 1 / FStacks;
  for j := 0 to FStacks - 1 do
  begin
    Theta := AngStart;
    SinCos(Phi, SinP, CosP);
    SinCos(Phi2, SinP2, CosP2);

    if (Sign(SinP) = 1) or (tc1 = VCurve) then
      SinPc1 := Power(SinP, VCurve)
    else
      SinPc1 := -Power(-SinP, VCurve);
    v1.Y := SinPc1;

    if (Sign(SinP2) = 1) or (tc1 = VCurve) then
      SinPc1 := Power(SinP2, VCurve)
    else
      SinPc1 := -Power(-SinP2, VCurve);
    v2.Y := SinPc1;
    vTexCoord0 := 1 - j * vTexFactor;
    vTexCoord1 := 1 - (j + 1) * vTexFactor;
    gl.Begin_(GL_TRIANGLE_STRIP);
    for i := 0 to FSlices do
    begin
      SinCos(Theta, SinT, CosT);

      if (Sign(CosP) = 1) or (tc1 = VCurve) then
        CosPc1 := Power(CosP, VCurve)
      else
        CosPc1 := -Power(-CosP, VCurve);

      if (Sign(SinT) = 1) or (tc2 = HCurve) then
        SinTc2 := Power(SinT, HCurve)
      else
        SinTc2 := -Power(-SinT, HCurve);
      v1.X := CosPc1 * SinTc2;

      if (Sign(CosP2) = 1) or (tc1 = VCurve) then
        CosPc1 := Power(CosP2, VCurve)
      else
        CosPc1 := -Power(-CosP2, VCurve);
      v2.X := CosPc1 * SinTc2;

      if (Sign(CosP) = 1) or (tc1 = VCurve) then
        CosPc1 := Power(CosP, VCurve)
      else
        CosPc1 := -Power(-CosP, VCurve);
      if (Sign(CosT) = 1) or (tc2 = HCurve) then
        CosTc2 := Power(CosT, HCurve)
      else
        CosTc2 := -Power(-CosT, HCurve);
      v1.Z := CosPc1 * CosTc2;
      if (Sign(CosP2) = 1) or (tc1 = VCurve) then
        CosPc1 := Power(CosP2, VCurve)
      else
        CosPc1 := -Power(-CosP2, VCurve);
      v2.Z := CosPc1 * CosTc2;
      uTexCoord := i * uTexFactor;
      xgl.TexCoord2f(uTexCoord, vTexCoord0);
      if DoReverse then
      begin
        N1 := VectorNegate(v1);
        gl.Normal3fv(@N1);
      end
      else
        gl.Normal3fv(@v1);
      vs := v1;
      ScaleVector(vs, Radius);
      gl.Vertex3fv(@vs);

      xgl.TexCoord2f(uTexCoord, vTexCoord1);
      if DoReverse then
      begin
        N1 := VectorNegate(v2);
        gl.Normal3fv(@N1);
      end
      else
        gl.Normal3fv(@v2);
      vs := v2;
      ScaleVector(vs, Radius);
      gl.Vertex3fv(@vs);
      Theta := Theta + StepH;
    end;
    gl.End_;
    Phi := Phi2;
    Phi2 := Phi2 - StepV;
  end;

  // bottom cap
  if (FBottom > -90) and (FBottomCap in [ctCenter, ctFlat]) then
  begin
    gl.Begin_(GL_TRIANGLE_FAN);
    SinCos(AngBottom, SinP, CosP);
    xgl.TexCoord2f(0.5, 0.5);
    if DoReverse then
      gl.Normal3f(0, 1, 0)
    else
      gl.Normal3f(0, -1, 0);
    if FBottomCap = ctCenter then
      gl.Vertex3f(0, 0, 0)
    else
    begin { FTopCap = ctFlat }
      if (Sign(SinP) = 1) or (tc1 = VCurve) then
        SinPc1 := Power(SinP, VCurve)
      else
        SinPc1 := -Power(-SinP, VCurve);
      gl.Vertex3f(0, SinPc1 * Radius, 0);

      if DoReverse then
        MakeVector(N1, 0, -1, 0)
      else
        N1 := YVector;
    end;
    // v1.Y := SinP;
    if (Sign(SinP) = 1) or (tc1 = VCurve) then
      SinPc1 := Power(SinP, VCurve)
    else
      SinPc1 := -Power(-SinP, VCurve);
    v1.Y := SinPc1;

    Theta := AngStop;
    for i := 0 to FSlices do
    begin
      SinCos(Theta, SinT, CosT);
      // v1.X := CosP * SinT;
      if (Sign(CosP) = 1) or (tc1 = VCurve) then
        CosPc1 := Power(CosP, VCurve)
      else
        CosPc1 := -Power(-CosP, VCurve);
      if (Sign(SinT) = 1) or (tc2 = HCurve) then
        SinTc2 := Power(SinT, HCurve)
      else
        SinTc2 := -Power(-SinT, HCurve);
      v1.X := CosPc1 * SinTc2;

      // v1.Z := CosP * CosT;
      if (Sign(CosT) = 1) or (tc2 = HCurve) then
        CosTc2 := Power(CosT, HCurve)
      else
        CosTc2 := -Power(-CosT, HCurve);
      v1.Z := CosPc1 * CosTc2;
      if FBottomCap = ctCenter then
      begin
        N1 := VectorPerpendicular(AffineVectorMake(0, -1, 0), v1);
        if DoReverse then
          NegateVector(N1);
        gl.Normal3fv(@N1);
      end;
      // xgl.TexCoord2f(SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
      xgl.TexCoord2f(SinTc2 * 0.5 + 0.5, CosTc2 * 0.5 + 0.5);
      vs := v1;
      ScaleVector(vs, Radius);
      gl.Vertex3fv(@vs);
      Theta := Theta - StepH;
    end;
    gl.End_;
  end;
  if DoReverse then
    rci.GLStates.InvertGLFrontFace;
end;

// This will probably not work, karamba
// RayCastSphereIntersect -> RayCastSuperellipsoidIntersect ??????
function TGLSuperellipsoid.RayCastIntersect(const rayStart, rayVector: TGLVector;
  intersectPoint: PGLVector = nil; intersectNormal: PGLVector = nil): Boolean;
var
  i1, i2: TGLVector;
  localStart, localVector: TGLVector;
begin
  // compute coefficients of quartic polynomial
  SetVector(localStart, AbsoluteToLocal(rayStart));
  SetVector(localVector, AbsoluteToLocal(rayVector));
  NormalizeVector(localVector);
  if RayCastSphereIntersect(localStart, localVector, NullHmgVector, Radius, i1,
    i2) > 0 then
  begin
    Result := True;
    if Assigned(intersectPoint) then
      SetVector(intersectPoint^, LocalToAbsolute(i1));
    if Assigned(intersectNormal) then
    begin
      i1.W := 0; // vector transform
      SetVector(intersectNormal^, LocalToAbsolute(i1));
    end;
  end
  else
    Result := False;
end;

// This will probably not work ?
function TGLSuperellipsoid.GenerateSilhouette(const silhouetteParameters
  : TGLSilhouetteParameters): TGLSilhouette;
var
  i, j: Integer;
  s, C, angleFactor: Single;
  sVec, tVec: TAffineVector;
  Segments: Integer;
begin
  Segments := MaxInteger(FStacks, FSlices);
  // determine a local orthonormal matrix, viewer-oriented
  sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, XVector);
  if VectorLength(sVec) < 1E-3 then
    sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, YVector);
  tVec := VectorCrossProduct(silhouetteParameters.SeenFrom, sVec);
  NormalizeVector(sVec);
  NormalizeVector(tVec);
  // generate the silhouette (outline and capping)
  Result := TGLSilhouette.Create;
  angleFactor := (2 * PI) / Segments;
  for i := 0 to Segments - 1 do
  begin
    SinCosine(i * angleFactor, FRadius, s, C);
    Result.vertices.AddPoint(VectorCombine(sVec, tVec, s, C));
    j := (i + 1) mod Segments;
    Result.Indices.Add(i, j);
    if silhouetteParameters.CappingRequired then
      Result.CapIndices.Add(Segments, i, j)
  end;
  if silhouetteParameters.CappingRequired then
    Result.vertices.Add(NullHmgPoint);
end;

procedure TGLSuperellipsoid.SetBottom(aValue: TAngleLimit1);
begin
  if FBottom <> aValue then
  begin
    FBottom := aValue;
    StructureChanged;
  end;
end;

procedure TGLSuperellipsoid.SetBottomCap(aValue: TGLCapType);
begin
  if FBottomCap <> aValue then
  begin
    FBottomCap := aValue;
    StructureChanged;
  end;
end;

procedure TGLSuperellipsoid.SetHCurve(const aValue: TGLFloat);
begin
  if aValue <> FHCurve then
  begin
    FHCurve := aValue;
    StructureChanged;
  end;
end;

procedure TGLSuperellipsoid.SetRadius(const aValue: TGLFloat);
begin
  if aValue <> FRadius then
  begin
    FRadius := aValue;
    StructureChanged;
  end;
end;

procedure TGLSuperellipsoid.SetSlices(aValue: TGLInt);
begin
  if aValue <> FSlices then
  begin
    if aValue <= 0 then
      FSlices := 1
    else
      FSlices := aValue;
    StructureChanged;
  end;
end;

procedure TGLSuperellipsoid.SetStacks(aValue: TGLInt);
begin
  if aValue <> FStacks then
  begin
    if aValue <= 0 then
      FStacks := 1
    else
      FStacks := aValue;
    StructureChanged;
  end;
end;

procedure TGLSuperellipsoid.SetStart(aValue: TAngleLimit2);
begin
  if FStart <> aValue then
  begin
    Assert(aValue <= FStop);
    FStart := aValue;
    StructureChanged;
  end;
end;

procedure TGLSuperellipsoid.SetStop(aValue: TAngleLimit2);
begin
  if FStop <> aValue then
  begin
    Assert(aValue >= FStart);
    FStop := aValue;
    StructureChanged;
  end;
end;

procedure TGLSuperellipsoid.SetTop(aValue: TAngleLimit1);
begin
  if FTop <> aValue then
  begin
    FTop := aValue;
    StructureChanged;
  end;
end;

procedure TGLSuperellipsoid.SetTopCap(aValue: TGLCapType);
begin
  if FTopCap <> aValue then
  begin
    FTopCap := aValue;
    StructureChanged;
  end;
end;

procedure TGLSuperellipsoid.SetVCurve(const aValue: TGLFloat);
begin
  if aValue <> FVCurve then
  begin
    FVCurve := aValue;
    StructureChanged;
  end;
end;

procedure TGLSuperellipsoid.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLSuperellipsoid) then
  begin
    FRadius := TGLSuperellipsoid(Source).FRadius;
    FSlices := TGLSuperellipsoid(Source).FSlices;
    FStacks := TGLSuperellipsoid(Source).FStacks;
    FBottom := TGLSuperellipsoid(Source).FBottom;
    FTop := TGLSuperellipsoid(Source).FTop;
    FStart := TGLSuperellipsoid(Source).FStart;
    FStop := TGLSuperellipsoid(Source).FStop;
  end;
  inherited Assign(Source);
end;

function TGLSuperellipsoid.AxisAlignedDimensionsUnscaled: TGLVector;
begin
  Result.X := Abs(FRadius);
  Result.Y := Result.X;
  Result.Z := Result.X;
  Result.W := 0;
end;

// -------------------------------------------------------------
initialization

// -------------------------------------------------------------

RegisterClasses([TGLSphere, TGLCube, TGLPlane, TGLSprite, TGLPoints,
  TGLDummyCube, TGLLines, TGLSuperellipsoid]);

end.
