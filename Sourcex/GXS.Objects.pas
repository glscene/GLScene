//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Objects;

(*
  Implementation of basic scene objects plus some management routines.

  All objects declared in this unit are part of the basic GLScene package,
  these are only simple objects and should be kept simple and lightweight.

  More complex or more specialized versions should be placed in dedicated
  units where they can grow and prosper untammed. "Generic" geometrical
  objects can be found GXS.GeomObjects.
*)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Types,
  System.Classes,
  System.SysUtils,
  System.Math,

  GXS.XOpenGL,
  GXS.BaseClasses,
  GXS.PersistentClasses,
  GXS.VectorGeometry,
  GXS.VectorTypes,
  GXS.VectorLists,
  GXS.Strings,

  GXS.Scene,
  GXS.Context,
  GXS.Silhouette,
  GXS.Color,
  GXS.RenderContextInfo,
  GXS.Nodes,
  GXS.PipelineTransformation,
  GXS.Coordinates;

const
  cDefaultPointSize: Single = 1.0;

type

  TgxVisibilityDeterminationEvent = function(Sender: TObject;
    var rci: TgxRenderContextInfo): Boolean of object;

  PVertexRec = ^TVertexRec;
  TVertexRec = record
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
  TgxDummyCube = class(TgxCameraInvariantObject)
  private
    FCubeSize: Single;
    FEdgeColor: TgxColor;
    FVisibleAtRunTime, FAmalgamate: Boolean;
    FGroupList: TgxListHandle;
    FOnVisibilityDetermination: TgxVisibilityDeterminationEvent;
  protected
    procedure SetCubeSize(const val: Single); inline;
    procedure SetEdgeColor(const val: TgxColor); inline;
    procedure SetVisibleAtRunTime(const val: Boolean); inline;
    procedure SetAmalgamate(const val: Boolean); inline;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function AxisAlignedDimensionsUnscaled: TVector4f; override;
    function RayCastIntersect(const rayStart, rayVector: TVector4f;
      intersectPoint: PVector4f = nil; intersectNormal: PVector4f = nil): Boolean; override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
    procedure DoRender(var rci: TgxRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
    procedure StructureChanged; override;
    function BarycenterAbsolutePosition: TVector4f; override;
  published
    property CubeSize: Single read FCubeSize write SetCubeSize;
    property EdgeColor: TgxColor read FEdgeColor write SetEdgeColor;
    (* If true the dummycube's edges will be visible at runtime.
      The default behaviour of the dummycube is to be visible at design-time
      only, and invisible at runtime. *)
    property VisibleAtRunTime: Boolean read FVisibleAtRunTime write SetVisibleAtRunTime default False;
    (* Amalgamate the dummy's children in a single OpenGL entity.
      This activates a special rendering mode, which will compile
      the rendering of all of the dummycube's children objects into a
      single display list. This may provide a significant speed up in some
      situations, however, this means that changes to the children will
      be ignored untill you call StructureChanged on the dummy cube.
      Some objects, that have their own display list management, may not
      be compatible with this behaviour. This will also prevents sorting
      and culling to operate as usual.
      In short, this features is best used for static, non-transparent
      geometry, or when the point of view won't change over a large
      number of frames. *)
    property Amalgamate: Boolean read FAmalgamate write SetAmalgamate default False;
    (* Camera Invariance Options.
      These options allow to "deactivate" sensitivity to camera, f.i. by
      centering the object on the camera or ignoring camera orientation. *)
    property CamInvarianceMode default cimNone;
    (* Event for custom visibility determination.
      Event handler should return True if the dummycube and its children
      are to be considered visible for the current render. *)
    property OnVisibilityDetermination: TgxVisibilityDeterminationEvent
      read FOnVisibilityDetermination write FOnVisibilityDetermination;
  end;

  TgxPlaneStyle = (psSingleQuad, psTileTexture);
  TgxPlaneStyles = set of TgxPlaneStyle;

  (* A simple plane object.
    Note that a plane is always made of a single quad (two triangles) and the
    tiling is only applied to texture coordinates. *)
  TgxPlane = class(TgxSceneObject)
  private
    FXOffset, FYOffset: Single;
    FXScope, FYScope: Single;
    FWidth, FHeight: Single;
    FXTiles, FYTiles: Cardinal;
    FStyle: TgxPlaneStyles;
    FMesh: array of array of TVertexRec;
  protected
    procedure SetHeight(const aValue: Single);
    procedure SetWidth(const aValue: Single);
    procedure SetXOffset(const Value: Single);
    procedure SetXScope(const Value: Single);
    function StoreXScope: Boolean;
    procedure SetXTiles(const Value: Cardinal);
    procedure SetYOffset(const Value: Single);
    procedure SetYScope(const Value: Single);
    function StoreYScope: Boolean;
    procedure SetYTiles(const Value: Cardinal);
    procedure SetStyle(const val: TgxPlaneStyles);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
    function GenerateSilhouette(const silhouetteParameters
      : TgxSilhouetteParameters): TgxSilhouette; override;
    function AxisAlignedDimensionsUnscaled: TVector4f; override;
    function RayCastIntersect(const rayStart, rayVector: TVector4f;
      intersectPoint: PVector4f = nil; intersectNormal: PVector4f = nil): Boolean; override;
    (* Computes the screen coordinates of the smallest rectangle encompassing the plane.
      Returned extents are NOT limited to any physical screen extents. *)
    function ScreenRect(aBuffer: TgxSceneBuffer): TRect;
    (* Computes the signed distance to the point.
      Point coordinates are expected in absolute coordinates. *)
    function PointDistance(const aPoint: TVector4f): Single;
  published
    property Height: Single read FHeight write SetHeight;
    property Width: Single read FWidth write SetWidth;
    property XOffset: Single read FXOffset write SetXOffset;
    property XScope: Single read FXScope write SetXScope stored StoreXScope;
    property XTiles: Cardinal read FXTiles write SetXTiles default 1;
    property YOffset: Single read FYOffset write SetYOffset;
    property YScope: Single read FYScope write SetYScope stored StoreYScope;
    property YTiles: Cardinal read FYTiles write SetYTiles default 1;
    property Style: TgxPlaneStyles read FStyle write SetStyle default [psSingleQuad, psTileTexture];
  end;

  (* A rectangular area, perspective projected, but always facing the camera.
    A TgxSprite is perspective projected and as such is scaled with distance,
    if you want a 2D sprite that does not get scaled, see TgxHUDSprite. *)
  TgxSprite = class(TgxSceneObject)
  private
    FWidth: Single;
    FHeight: Single;
    FRotation: Single;
    FAlphaChannel: Single;
    FMirrorU, FMirrorV: Boolean;
  protected
    procedure SetWidth(const val: Single);
    procedure SetHeight(const val: Single);
    procedure SetRotation(const val: Single);
    procedure SetAlphaChannel(const val: Single);
    function StoreAlphaChannel: Boolean;
    procedure SetMirrorU(const val: Boolean);
    procedure SetMirrorV(const val: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TVector4f; override;
    procedure SetSize(const Width, Height: Single);
    // Set width and height to "size"
    procedure SetSquareSize(const Size: Single);
  published
    // Sprite Width in 3D world units.
    property Width: Single read FWidth write SetWidth;
    // Sprite Height in 3D world units.
    property Height: Single read FHeight write SetHeight;
    (* This the ON-SCREEN rotation of the sprite.
      Rotatation=0 is handled faster. *)
    property Rotation: Single read FRotation write SetRotation;
    // If different from 1, this value will replace that of Diffuse.Alpha
    property AlphaChannel: Single read FAlphaChannel write SetAlphaChannel stored StoreAlphaChannel;
    (* Reverses the texture coordinates in the U and V direction to mirror
      the texture. *)
    property MirrorU: Boolean read FMirrorU write SetMirrorU default False;
    property MirrorV: Boolean read FMirrorV write SetMirrorV default False;
  end;

  TgxPointStyle = (psSquare, psRound, psSmooth, psSmoothAdditive, psSquareAdditive);

  (* Point parameters as in ARB_point_parameters.
    Make sure to read the ARB_point_parameters spec if you want to understand
    what each parameter does. *)
  TgxPointParameters = class(TgxUpdateAbleObject)
  private
    FEnabled: Boolean;
    FMinSize, FMaxSize: Single;
    FFadeTresholdSize: Single;
    FDistanceAttenuation: TgxCoordinates;
  protected
    procedure SetEnabled(const val: Boolean);
    procedure SetMinSize(const val: Single);
    procedure SetMaxSize(const val: Single);
    procedure SetFadeTresholdSize(const val: Single);
    procedure SetDistanceAttenuation(const val: TgxCoordinates);
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
    property FadeTresholdSize: Single read FFadeTresholdSize write SetFadeTresholdSize stored False;
    // Components XYZ are for constant, linear and quadratic attenuation.
    property DistanceAttenuation: TgxCoordinates read FDistanceAttenuation write SetDistanceAttenuation;
  end;

  (* Renders a set of non-transparent colored points.
    The points positions and their color are defined through the Positions
    and Colors properties. *)
  TgxPoints = class(TgxImmaterialSceneObject)
  private
    FPositions: TgxAffineVectorList;
    FColors: TgxVectorList;
    FSize: Single;
    FStyle: TgxPointStyle;
    FPointParameters: TgxPointParameters;
    FStatic, FNoZWrite: Boolean;
  protected
    function StoreSize: Boolean;
    procedure SetNoZWrite(const val: Boolean);
    procedure SetStatic(const val: Boolean);
    procedure SetSize(const val: Single);
    procedure SetPositions(const val: TgxAffineVectorList);
    procedure SetColors(const val: TgxVectorList);
    procedure SetStyle(const val: TgxPointStyle);
    procedure SetPointParameters(const val: TgxPointParameters);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
    // Points positions. If empty, a single point is assumed at (0, 0, 0)
    property Positions: TgxAffineVectorList read FPositions write SetPositions;
    (* Defines the points colors.
       if empty, point color will be opaque white
       if contains a single color, all points will use that color
       if contains N colors, the first N points (at max) will be rendered
      using the corresponding colors. *)
    property Colors: TgxVectorList read FColors write SetColors;
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
    property Style: TgxPointStyle read FStyle write SetStyle default psSquare;
    (* Point parameters as of ARB_point_parameters.
      Allows to vary the size and transparency of points depending
      on their distance to the observer. *)
    property PointParameters: TgxPointParameters read FPointParameters write SetPointParameters;
  end;

  // Possible aspects for the nodes of a TLine.
  TLineNodesAspect = (lnaInvisible, lnaAxes, lnaCube);

  // Available spline modes for a TLine.
  TgxLineSplineMode = (lsmLines, lsmCubicSpline, lsmBezierSpline, lsmNURBSCurve, lsmSegments, lsmLoop);

  // Specialized Node for use in a TgxLines objects. Adds a Color property (TgxColor).
  TgxLinesNode = class(TgxNode)
  private
    FColor: TgxColor;
  protected
    procedure SetColor(const val: TgxColor);
    procedure OnColorChange(Sender: TObject);
    function StoreColor: Boolean;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    (* The node color.
      Can also defined the line color (interpolated between nodes) if
      loUseNodeColorForLines is set (in TgxLines). *)
    property Color: TgxColor read FColor write SetColor stored StoreColor;
  end;

  // Specialized collection for Nodes in a TgxLines objects. Stores TgxLinesNode items.
  TgxLinesNodes = class(TgxNodes)
  public
    constructor Create(AOwner: TComponent); overload;
    procedure NotifyChange; override;
  end;

  // Base class for line objects. Introduces line style properties (width, color...)
  TgxLineBase = class(TgxImmaterialSceneObject)
  private
    FLineColor: TgxColor;
    FLinePattern: GLushort;
    FLineWidth: Single;
    FAntiAliased: Boolean;
  protected
    procedure SetLineColor(const Value: TgxColor);
    procedure SetLinePattern(const Value: GLushort);
    procedure SetLineWidth(const val: Single);
    function StoreLineWidth: Boolean; inline;
    procedure SetAntiAliased(const val: Boolean);
    (* Setup OpenGL states according to line style.
      You must call RestoreLineStyle after drawing your lines.
      You may use nested calls with SetupLineStyle/RestoreLineStyle. *)
    procedure SetupLineStyle(var rci: TgxRenderContextInfo);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
  published
    (* Indicates if OpenGL should smooth line edges.
      Smoothed lines looks better but are poorly implemented in most OpenGL
      drivers and take *lots* of rendering time. *)
    property AntiAliased: Boolean read FAntiAliased write SetAntiAliased default False;
    // Default color of the lines.
    property LineColor: TgxColor read FLineColor write SetLineColor;
    (* Bitwise line pattern.
      For instance $FFFF (65535) is a white line (stipple disabled), $0000
      is a black line, $CCCC is the stipple used in axes and dummycube, etc. *)
    property LinePattern: GLushort read FLinePattern write SetLinePattern default $FFFF;
    // Default width of the lines.
    property LineWidth: Single read FLineWidth write SetLineWidth stored StoreLineWidth;
    property Visible;
  end;

  // Class that defines lines via a series of nodes. Base class, does not render anything.
  TgxNodedLines = class(TgxLineBase)
  private
    FNodes: TgxLinesNodes;
    FNodesAspect: TLineNodesAspect;
    FNodeColor: TgxColor;
    FNodeSize: Single;
    FOldNodeColor: TgxColorVector;
  protected
    procedure SetNodesAspect(const Value: TLineNodesAspect);
    procedure SetNodeColor(const Value: TgxColor);
    procedure OnNodeColorChanged(Sender: TObject);
    procedure SetNodes(const aNodes: TgxLinesNodes);
    procedure SetNodeSize(const val: Single);
    function StoreNodeSize: Boolean;
    procedure DrawNode(var rci: TgxRenderContextInfo; X, Y, Z: Single; Color: TgxColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function AxisAlignedDimensionsUnscaled: TVector4f; override;
    procedure AddNode(const coords: TgxCoordinates); overload;
    procedure AddNode(const X, Y, Z: Single); overload;
    procedure AddNode(const Value: TVector4f); overload;
    procedure AddNode(const Value: TAffineVector); overload;
  published
    // Default color for nodes. lnaInvisible and lnaAxes ignore this setting.
    property NodeColor: TgxColor read FNodeColor write SetNodeColor;
    // The nodes list.
    property Nodes: TgxLinesNodes read FNodes write SetNodes;
    (* Default aspect of line nodes.
      May help you materialize nodes, segments and control points. *)
    property NodesAspect: TLineNodesAspect read FNodesAspect write SetNodesAspect default lnaAxes;
    // Size for the various node aspects.
    property NodeSize: Single read FNodeSize write SetNodeSize stored StoreNodeSize;
  end;

  TLinesOption = (loUseNodeColorForLines, loColorLogicXor);
  TgxLinesOptions = set of TLinesOption;

  (* Set of 3D line segments.
    You define a 3D Line by adding its nodes in the "Nodes" property. The line
    may be rendered as a set of segment or as a curve (nodes then act as spline
    control points).
    Alternatively, you can also use it to render a set of spacial nodes (points
    in space), just make the lines transparent and the nodes visible by picking
    the node aspect that suits you. *)
  TgxLines = class(TgxNodedLines)
  private
    FDivision: Integer;
    FSplineMode: TgxLineSplineMode;
    FOptions: TgxLinesOptions;
    FNURBSOrder: Integer;
    FNURBSTolerance: Single;
    FNURBSKnots: TgxSingleList;
  protected
    procedure SetSplineMode(const val: TgxLineSplineMode);
    procedure SetDivision(const Value: Integer);
    procedure SetOptions(const val: TgxLinesOptions);
    procedure SetNURBSOrder(const val: Integer);
    procedure SetNURBSTolerance(const val: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
    property NURBSKnots: TgxSingleList read FNURBSKnots;
    property NURBSOrder: Integer read FNURBSOrder write SetNURBSOrder;
    property NURBSTolerance: Single read FNURBSTolerance write SetNURBSTolerance;
  published
    (* Number of divisions for each segment in spline modes.
      Minimum 1 (disabled), ignored in lsmLines mode. *)
    property Division: Integer read FDivision write SetDivision default 10;
    // Default spline drawing mode.
    property SplineMode: TgxLineSplineMode read FSplineMode write SetSplineMode default lsmLines;
    (* Rendering options for the line.
       loUseNodeColorForLines: if set lines will be drawn using node
      colors (and color interpolation between nodes), if not, LineColor
      will be used (single color).
      loColorLogicXor: enable logic operation for color of XOR type.  *)
    property Options: TgxLinesOptions read FOptions write SetOptions;
  end;

  TgxCubePart = (cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight);
  TgxCubeParts = set of TgxCubePart;

  (* A simple cube object.
    This cube use the same material for each of its faces, ie. all faces look
    the same. If you want a multi-material cube, use a mesh in conjunction
    with a TgxFreeForm and a material library. *)
  TgxCube = class(TgxSceneObject)
  private
    FCubeSize: TAffineVector;
    FParts: TgxCubeParts;
    FNormalDirection: TgxNormalDirection;
    function GetCubeWHD(const Index: Integer): Single; inline;
    procedure SetCubeWHD(Index: Integer; AValue: Single); inline;
    procedure SetParts(aValue: TgxCubeParts); inline;
    procedure SetNormalDirection(aValue: TgxNormalDirection); inline;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream); inline;
    procedure WriteData(Stream: TStream); inline;
  public
    constructor Create(AOwner: TComponent); override;
    function GenerateSilhouette(const SilhouetteParameters: TgxSilhouetteParameters): TgxSilhouette; override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
    procedure Assign(Source: TPersistent); override;
    function AxisAlignedDimensionsUnscaled: TVector4f; override;
    function RayCastIntersect(const rayStart, rayVector: TVector4f; intersectPoint: PVector4f = nil;
	  intersectNormal: PVector4f = nil): Boolean; override;
  published
    property CubeWidth: Single index 0 read GetCubeWHD write SetCubeWHD stored False;
    property CubeHeight: Single index 1 read GetCubeWHD write SetCubeWHD stored False;
    property CubeDepth: Single index 2 read GetCubeWHD write SetCubeWHD stored False;
    property NormalDirection: TgxNormalDirection read FNormalDirection write SetNormalDirection default ndOutside;
    property Parts: TgxCubeParts read FParts write SetParts default [cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight];
  end;

  (* Determines how and if normals are smoothed.
    - nsFlat : facetted look
    - nsSmooth : smooth look
    - nsNone : unlighted rendering, usefull for decla texturing *)
  TgxNormalSmoothing = (nsFlat, nsSmooth, nsNone);

  (* Base class for quadric objects.
    Introduces some basic Quadric interaction functions (the actual quadric
    math is part of the GLU library). *)
  TgxQuadricObject = class(TgxSceneObject)
  private
    FNormals: TgxNormalSmoothing;
    FNormalDirection: TgxNormalDirection;
  protected
    procedure SetNormals(aValue: TgxNormalSmoothing);
    procedure SetNormalDirection(aValue: TgxNormalDirection);
    procedure SetupQuadricParams(quadric: GLUquadricObj);
    procedure SetNormalQuadricOrientation(quadric: GLUquadricObj);
    procedure SetInvertedQuadricOrientation(quadric: GLUquadricObj);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Normals: TgxNormalSmoothing read FNormals write SetNormals default nsSmooth;
    property NormalDirection: TgxNormalDirection read FNormalDirection write SetNormalDirection default ndOutside;
  end;

  TAngleLimit1 = -90 .. 90;
  TAngleLimit2 = 0 .. 360;
  TgxCapType = (ctNone, ctCenter, ctFlat);

  (* A sphere object.
    The sphere can have to and bottom caps, as well as being just a slice
    of sphere. *)
  TgxSphere = class(TgxQuadricObject)
  private
    FRadius: Single;
    FSlices, FStacks: GLint;
    FTop: TAngleLimit1;
    FBottom: TAngleLimit1;
    FStart: TAngleLimit2;
    FStop: TAngleLimit2;
    FTopCap, FBottomCap: TgxCapType;
    procedure SetBottom(aValue: TAngleLimit1);
    procedure SetBottomCap(aValue: TgxCapType);
    procedure SetRadius(const aValue: Single);
    procedure SetSlices(aValue: GLint);
    procedure SetStart(aValue: TAngleLimit2);
    procedure SetStop(aValue: TAngleLimit2);
    procedure SetStacks(aValue: GLint);
    procedure SetTop(aValue: TAngleLimit1);
    procedure SetTopCap(aValue: TgxCapType);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TVector4f; override;
    function RayCastIntersect(const rayStart, rayVector: TVector4f;
      intersectPoint: PVector4f = nil; intersectNormal: PVector4f = nil)
      : Boolean; override;
    function GenerateSilhouette(const silhouetteParameters
      : TgxSilhouetteParameters): TgxSilhouette; override;
  published
    property Bottom: TAngleLimit1 read FBottom write SetBottom default -90;
    property BottomCap: TgxCapType read FBottomCap write SetBottomCap
      default ctNone;
    property Radius: Single read FRadius write SetRadius;
    property Slices: GLint read FSlices write SetSlices default 16;
    property Stacks: GLint read FStacks write SetStacks default 16;
    property Start: TAngleLimit2 read FStart write SetStart default 0;
    property Stop: TAngleLimit2 read FStop write SetStop default 360;
    property Top: TAngleLimit1 read FTop write SetTop default 90;
    property TopCap: TgxCapType read FTopCap write SetTopCap default ctNone;
  end;

  // Base class for objects based on a polygon.
  TgxPolygonBase = class(TgxSceneObject)
  private
    FDivision: Integer;
    FSplineMode: TgxLineSplineMode;
  protected
    FNodes: TgxNodes;
    procedure CreateNodes; virtual;
    procedure SetSplineMode(const val: TgxLineSplineMode);
    procedure SetDivision(const Value: Integer);
    procedure SetNodes(const aNodes: TgxNodes);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure AddNode(const coords: TgxCoordinates); overload;
    procedure AddNode(const X, Y, Z: Single); overload;
    procedure AddNode(const Value: TVector4f); overload;
    procedure AddNode(const Value: TAffineVector); overload;
  published
    // The nodes list.
    property Nodes: TgxNodes read FNodes write SetNodes;
    (* Number of divisions for each segment in spline modes.
      Minimum 1 (disabled), ignored in lsmLines mode. *)
    property Division: Integer read FDivision write SetDivision default 10;
    (* Default spline drawing mode.
      This mode is used only for the curve, not for the rotation path. *)
    property SplineMode: TgxLineSplineMode read FSplineMode write SetSplineMode
      default lsmLines;
  end;

  (* A Superellipsoid object. The Superellipsoid can have top and bottom caps,
    as well as being just a slice of Superellipsoid. *)
  TgxSuperellipsoid = class(TgxQuadricObject)
  private
    FRadius, FVCurve, FHCurve: Single;
    FSlices, FStacks: GLInt;
    FTop: TAngleLimit1;
    FBottom: TAngleLimit1;
    FStart: TAngleLimit2;
    FStop: TAngleLimit2;
    FTopCap, FBottomCap: TgxCapType;
    procedure SetBottom(aValue: TAngleLimit1);
    procedure SetBottomCap(aValue: TgxCapType);
    procedure SetRadius(const aValue: Single);
    procedure SetVCurve(const aValue: Single);
    procedure SetHCurve(const aValue: Single);
    procedure SetSlices(aValue: GLInt);
    procedure SetStart(aValue: TAngleLimit2);
    procedure SetStop(aValue: TAngleLimit2);
    procedure SetStacks(aValue: GLint);
    procedure SetTop(aValue: TAngleLimit1);
    procedure SetTopCap(aValue: TgxCapType);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TVector4f; override;
    function RayCastIntersect(const rayStart, rayVector: TVector4f;
      intersectPoint: PVector4f = nil; intersectNormal: PVector4f = nil)
      : Boolean; override;
    function GenerateSilhouette(const silhouetteParameters
      : TgxSilhouetteParameters): TgxSilhouette; override;
  published
    property Bottom: TAngleLimit1 read FBottom write SetBottom default -90;
    property BottomCap: TgxCapType read FBottomCap write SetBottomCap
      default ctNone;
    property Radius: Single read FRadius write SetRadius;
    property VCurve: Single read FVCurve write SetVCurve;
    property HCurve: Single read FHCurve write SetHCurve;
    property Slices: GLInt read FSlices write SetSlices default 16;
    property Stacks: GLInt read FStacks write SetStacks default 16;
    property Start: TAngleLimit2 read FStart write SetStart default 0;
    property Stop: TAngleLimit2 read FStop write SetStop default 360;
    property Top: TAngleLimit1 read FTop write SetTop default 90;
    property TopCap: TgxCapType read FTopCap write SetTopCap default ctNone;
  end;

// Issues for a unit-size cube stippled wireframe.
procedure CubeWireframeBuildList(var rci: TgxRenderContextInfo; Size: Single;
  Stipple: Boolean; const Color: TgxColorVector);

var
  TangentAttributeName: AnsiString = 'Tangent';
  BinormalAttributeName: AnsiString = 'Binormal';

// -------------------------------------------------------------
implementation
// -------------------------------------------------------------

uses
  GXS.Spline,
  GXS.State;

procedure CubeWireframeBuildList(var rci: TgxRenderContextInfo; Size: Single;
  Stipple: Boolean; const Color: TgxColorVector);
var
  mi, ma: Single;
begin
{$IFDEF USE_OPENGL_DEBUG}
  if GL_GREMEDY_string_marker then
    glStringMarkerGREMEDY(22, 'CubeWireframeBuildList');
{$ENDIF}
  rci.gxStates.Disable(stLighting);
  rci.gxStates.Enable(stLineSmooth);
  if stipple then
  begin
    rci.gxStates.Enable(stLineStipple);
    rci.gxStates.Enable(stBlend);
    rci.gxStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
    rci.gxStates.LineStippleFactor := 1;
    rci.gxStates.LineStipplePattern := $CCCC;
  end;
  rci.gxStates.LineWidth := 1;
  ma := 0.5 * Size;
  mi := -ma;

  glColor4fv(@Color);
  glBegin(GL_LINE_STRIP);
  // front face
  glVertex3f(ma, mi, mi);
  glVertex3f(ma, ma, mi);
  glVertex3f(ma, ma, ma);
  glVertex3f(ma, mi, ma);
  glVertex3f(ma, mi, mi);
  // partial up back face
  glVertex3f(mi, mi, mi);
  glVertex3f(mi, mi, ma);
  glVertex3f(mi, ma, ma);
  glVertex3f(mi, ma, mi);
  // right side low
  glVertex3f(ma, ma, mi);
  glEnd;
  glBegin(GL_LINES);
  // right high
  glVertex3f(ma, ma, ma);
  glVertex3f(mi, ma, ma);
  // back low
  glVertex3f(mi, mi, mi);
  glVertex3f(mi, ma, mi);
  // left high
  glVertex3f(ma, mi, ma);
  glVertex3f(mi, mi, ma);
  glEnd;
end;

// ------------------
// ------------------ TgxDummyCube ------------------
// ------------------

constructor TgxDummyCube.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FCubeSize := 1;
  FEdgeColor := TgxColor.Create(Self);
  FEdgeColor.Initialize(clrWhite);
  FGroupList := TgxListHandle.Create;
  CamInvarianceMode := cimNone;
end;

destructor TgxDummyCube.Destroy;
begin
  FGroupList.Free;
  FEdgeColor.Free;
  inherited;
end;

procedure TgxDummyCube.Assign(Source: TPersistent);
begin
  if Source is TgxDummyCube then
  begin
    FCubeSize := TgxDummyCube(Source).FCubeSize;
    FEdgeColor.Color := TgxDummyCube(Source).FEdgeColor.Color;
    FVisibleAtRunTime := TgxDummyCube(Source).FVisibleAtRunTime;
    NotifyChange(Self);
  end;
  inherited Assign(Source);
end;

function TgxDummyCube.AxisAlignedDimensionsUnscaled: TVector4f;
begin
  Result.X := 0.5 * Abs(FCubeSize);
  Result.Y := Result.X;
  Result.Z := Result.X;
  Result.W := 0;
end;

function TgxDummyCube.RayCastIntersect(const rayStart, rayVector: TVector4f;
  intersectPoint: PVector4f = nil; intersectNormal: PVector4f = nil): Boolean;
begin
  Result := False;
end;

procedure TgxDummyCube.BuildList(var rci: TgxRenderContextInfo);
begin
  if (csDesigning in ComponentState) or (FVisibleAtRunTime) then
    CubeWireframeBuildList(rci, FCubeSize, True, EdgeColor.Color);
end;

procedure TgxDummyCube.DoRender(var rci: TgxRenderContextInfo;
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
      rci.gxStates.NewList(FGroupList.Handle, GL_COMPILE);
      rci.amalgamating := True;
      try
        inherited;
      finally
        rci.amalgamating := False;
        rci.gxStates.EndList;
      end;
    end;
    rci.gxStates.CallList(FGroupList.Handle);
  end
  else
  begin
    // proceed as usual
    inherited;
  end;
end;

procedure TgxDummyCube.StructureChanged;
begin
  if FAmalgamate then
    FGroupList.DestroyHandle;
  inherited;
end;

function TgxDummyCube.BarycenterAbsolutePosition: TVector4f;
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

procedure TgxDummyCube.SetCubeSize(const val: Single);
begin
  if val <> FCubeSize then
  begin
    FCubeSize := val;
    StructureChanged;
  end;
end;

procedure TgxDummyCube.SetEdgeColor(const val: TgxColor);
begin
  if val <> FEdgeColor then
  begin
    FEdgeColor.Assign(val);
    StructureChanged;
  end;
end;

procedure TgxDummyCube.SetVisibleAtRunTime(const val: Boolean);
begin
  if val <> FVisibleAtRunTime then
  begin
    FVisibleAtRunTime := val;
    StructureChanged;
  end;
end;

procedure TgxDummyCube.SetAmalgamate(const val: Boolean);
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
// ------------------ TgxPlane ------------------
// ------------------

constructor TgxPlane.Create(AOwner: TComponent);
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

procedure TgxPlane.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TgxPlane) then
  begin
    FWidth := TgxPlane(Source).FWidth;
    FHeight := TgxPlane(Source).FHeight;
    FXOffset := TgxPlane(Source).FXOffset;
    FXScope := TgxPlane(Source).FXScope;
    FXTiles := TgxPlane(Source).FXTiles;
    FYOffset := TgxPlane(Source).FYOffset;
    FYScope := TgxPlane(Source).FYScope;
    FYTiles := TgxPlane(Source).FYTiles;
    FStyle := TgxPlane(Source).FStyle;
    StructureChanged;
  end;
  inherited Assign(Source);
end;

function TgxPlane.AxisAlignedDimensionsUnscaled: TVector4f;
begin
  Result.X := 0.5 * Abs(FWidth);
  Result.Y := 0.5 * Abs(FHeight);
  Result.Z := 0;
end;

function TgxPlane.RayCastIntersect(const rayStart, rayVector: TVector4f;
  intersectPoint: PVector4f = nil; intersectNormal: PVector4f = nil): Boolean;
var
  locRayStart, locRayVector, ip: TVector4f;
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

function TgxPlane.GenerateSilhouette(const silhouetteParameters
  : TgxSilhouetteParameters): TgxSilhouette;
var
  hw, hh: Single;
begin
  Result := TgxSilhouette.Create;

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

procedure TgxPlane.BuildList(var rci: TgxRenderContextInfo);

  procedure EmitVertex(ptr: PVertexRec); {$IFDEF USE_INLINE}inline;{$ENDIF}
  begin
    glTexCoord2fv(@ptr^.TexCoord);
    glVertex3fv(@ptr^.Position);
  end;

var
  hw, hh, posXFact, posYFact, pX, pY1: Single;
  tx0, tx1, ty0, ty1, texSFact, texTFact: Single;
  texS, texT1: Single;
  X, Y: Integer;
  TanLoc, BinLoc: Integer;
  pVertex: PVertexRec;
begin
  hw := FWidth * 0.5;
  hh := FHeight * 0.5;

  glNormal3fv(@ZVector);
  if (rci.gxStates.CurrentProgram > 0) then
  begin
    TanLoc := glGetAttribLocation(rci.gxStates.CurrentProgram, PGLChar(TangentAttributeName));
    BinLoc := glGetAttribLocation(rci.gxStates.CurrentProgram, PGLChar(BinormalAttributeName));
    if TanLoc > -1 then
      glVertexAttrib3fv(TanLoc, @XVector);
    if BinLoc > -1 then
      glVertexAttrib3fv(BinLoc, @YVector);
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
    glBegin(GL_TRIANGLES);
    glTexCoord2f(tx1, ty1);
    glVertex2f(hw, hh);
    glTexCoord2f(tx0, ty1);
    glVertex2f(-hw, hh);
    glTexCoord2f(tx0, ty0);
    glVertex2f(-hw, -hh);

    glVertex2f(-hw, -hh);
    glTexCoord2f(tx1, ty0);
    glVertex2f(hw, -hh);
    glTexCoord2f(tx1, ty1);
    glVertex2f(hw, hh);
    glEnd;
    exit;
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
      SetLength(FMesh, FYTiles+1, FXTiles+1);
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

    glBegin(GL_TRIANGLES);
    for Y := 0 to FYTiles-1 do
    begin
      for X := 0 to FXTiles-1 do
      begin
        pVertex := @FMesh[Y][X];
        EmitVertex(pVertex);

        pVertex := @FMesh[Y][X+1];
        EmitVertex(pVertex);

        pVertex := @FMesh[Y+1][X];
        EmitVertex(pVertex);

        pVertex := @FMesh[Y+1][X+1];
        EmitVertex(pVertex);

        pVertex := @FMesh[Y+1][X];
        EmitVertex(pVertex);

        pVertex := @FMesh[Y][X+1];
        EmitVertex(pVertex);
      end;
    end;
    glEnd;
end;

procedure TgxPlane.SetWidth(const aValue: Single);
begin
  if aValue <> FWidth then
  begin
    FWidth := aValue;
    FMesh := nil;
    StructureChanged;
  end;
end;

function TgxPlane.ScreenRect(aBuffer: TgxSceneBuffer): TRect;
var
  v: array [0 .. 3] of TVector4f;
  buf: TgxSceneBuffer;
  hw, hh: Single;
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

function TgxPlane.PointDistance(const aPoint: TVector4f): Single;
begin
  Result := VectorDotProduct(VectorSubtract(aPoint, AbsolutePosition),
    AbsoluteDirection);
end;

procedure TgxPlane.SetHeight(const aValue: Single);
begin
  if aValue <> FHeight then
  begin
    FHeight := aValue;
    FMesh := nil;
    StructureChanged;
  end;
end;

procedure TgxPlane.SetXOffset(const Value: Single);
begin
  if Value <> FXOffset then
  begin
    FXOffset := Value;
    FMesh := nil;
    StructureChanged;
  end;
end;

procedure TgxPlane.SetXScope(const Value: Single);
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

function TgxPlane.StoreXScope: Boolean;
begin
  Result := (FXScope <> 1);
end;

procedure TgxPlane.SetXTiles(const Value: Cardinal);
begin
  if Value <> FXTiles then
  begin
    FXTiles := Value;
    FMesh := nil;
    StructureChanged;
  end;
end;

procedure TgxPlane.SetYOffset(const Value: Single);
begin
  if Value <> FYOffset then
  begin
    FYOffset := Value;
    FMesh := nil;
    StructureChanged;
  end;
end;

procedure TgxPlane.SetYScope(const Value: Single);
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

function TgxPlane.StoreYScope: Boolean;
begin
  Result := (FYScope <> 1);
end;

procedure TgxPlane.SetYTiles(const Value: Cardinal);
begin
  if Value <> FYTiles then
  begin
    FYTiles := Value;
    FMesh := nil;
    StructureChanged;
  end;
end;

procedure TgxPlane.SetStyle(const val: TgxPlaneStyles);
begin
  if val <> FStyle then
  begin
    FStyle := val;
    StructureChanged;
  end;
end;

// ------------------
// ------------------ TgxSprite ------------------
// ------------------

constructor TgxSprite.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FAlphaChannel := 1;
  FWidth := 1;
  FHeight := 1;
end;

procedure TgxSprite.Assign(Source: TPersistent);
begin
  if Source is TgxSprite then
  begin
    FWidth := TgxSprite(Source).FWidth;
    FHeight := TgxSprite(Source).FHeight;
    FRotation := TgxSprite(Source).FRotation;
    FAlphaChannel := TgxSprite(Source).FAlphaChannel;
  end;
  inherited Assign(Source);
end;

function TgxSprite.AxisAlignedDimensionsUnscaled: TVector4f;
begin
  Result.X := 0.5 * Abs(FWidth);
  Result.Y := 0.5 * Abs(FHeight);
  // Sprites turn with the camera and can be considered to have the same depth
  // as width
  Result.Z := 0.5 * Abs(FWidth);
end;

procedure TgxSprite.BuildList(var rci: TgxRenderContextInfo);
var
  vx, vy: TAffineVector;
  w, h: Single;
  mat: TMatrix4f;
  u0, v0, u1, v1: Integer;
begin
  if FAlphaChannel <> 1 then
    rci.gxStates.SetMaterialAlphaChannel(GL_FRONT, FAlphaChannel);

  mat := rci.PipelineTransformation.ModelViewMatrix^;
  // extraction of the "vecteurs directeurs de la matrice"
  // (dunno how they are named in english)
  w := FWidth * 0.5;
  h := FHeight * 0.5;
  vx.X := mat.X.X;
  vy.X := mat.X.Y;
  vx.Y := mat.Y.X;
  vy.Y := mat.Y.Y;
  vx.Z := mat.Z.X;
  vy.Z := mat.Z.Y;
  ScaleVector(vx, w / VectorLength(vx));
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
    glPushMatrix;
    glRotatef(FRotation, mat.X.Z, mat.Y.Z, mat.Z.Z);
  end;
  glBegin(GL_QUADS);
  glTexCoord2f(u1, v1);
  glVertex3f(vx.X + vy.X, vx.Y + vy.Y, vx.Z + vy.Z);
  glTexCoord2f(u0, v1);
  glVertex3f(-vx.X + vy.X, -vx.Y + vy.Y, -vx.Z + vy.Z);
  glTexCoord2f(u0, v0);
  glVertex3f(-vx.X - vy.X, -vx.Y - vy.Y, -vx.Z - vy.Z);
  glTexCoord2f(u1, v0);
  glVertex3f(vx.X - vy.X, vx.Y - vy.Y, vx.Z - vy.Z);
  glEnd;
  if FRotation <> 0 then
    glPopMatrix;
end;

procedure TgxSprite.SetWidth(const val: Single);
begin
  if FWidth <> val then
  begin
    FWidth := val;
    NotifyChange(Self);
  end;
end;

procedure TgxSprite.SetHeight(const val: Single);
begin
  if FHeight <> val then
  begin
    FHeight := val;
    NotifyChange(Self);
  end;
end;

procedure TgxSprite.SetRotation(const val: Single);
begin
  if FRotation <> val then
  begin
    FRotation := val;
    NotifyChange(Self);
  end;
end;

procedure TgxSprite.SetAlphaChannel(const val: Single);
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

function TgxSprite.StoreAlphaChannel: Boolean;
begin
  Result := (FAlphaChannel <> 1);
end;

procedure TgxSprite.SetMirrorU(const val: Boolean);
begin
  FMirrorU := val;
  NotifyChange(Self);
end;

procedure TgxSprite.SetMirrorV(const val: Boolean);
begin
  FMirrorV := val;
  NotifyChange(Self);
end;

procedure TgxSprite.SetSize(const Width, Height: Single);
begin
  FWidth := Width;
  FHeight := Height;
  NotifyChange(Self);
end;

procedure TgxSprite.SetSquareSize(const Size: Single);
begin
  FWidth := Size;
  FHeight := Size;
  NotifyChange(Self);
end;

// ------------------
// ------------------ TgxPointParameters ------------------
// ------------------

constructor TgxPointParameters.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FMinSize := 0;
  FMaxSize := 128;
  FFadeTresholdSize := 1;
  FDistanceAttenuation := TgxCoordinates.CreateInitialized(Self, XHmgVector,
    csVector);
end;

destructor TgxPointParameters.Destroy;
begin
  FDistanceAttenuation.Free;
  inherited;
end;

procedure TgxPointParameters.Assign(Source: TPersistent);
begin
  if Source is TgxPointParameters then
  begin
    FMinSize := TgxPointParameters(Source).FMinSize;
    FMaxSize := TgxPointParameters(Source).FMaxSize;
    FFadeTresholdSize := TgxPointParameters(Source).FFadeTresholdSize;
    FDistanceAttenuation.Assign(TgxPointParameters(Source).DistanceAttenuation);
  end;
end;

procedure TgxPointParameters.DefineProperties(Filer: TFiler);
var
  defaultParams: Boolean;
begin
  inherited;
  defaultParams := (FMaxSize = 128) and (FMinSize = 0) and
    (FFadeTresholdSize = 1);
  Filer.DefineBinaryProperty('PointParams', ReadData, WriteData,
    not defaultParams);
end;

procedure TgxPointParameters.ReadData(Stream: TStream);
begin
  with Stream do
  begin
    Read(FMinSize, SizeOf(Single));
    Read(FMaxSize, SizeOf(Single));
    Read(FFadeTresholdSize, SizeOf(Single));
  end;
end;

procedure TgxPointParameters.WriteData(Stream: TStream);
begin
  with Stream do
  begin
    Write(FMinSize, SizeOf(Single));
    Write(FMaxSize, SizeOf(Single));
    Write(FFadeTresholdSize, SizeOf(Single));
  end;
end;

procedure TgxPointParameters.Apply;
begin
  if Enabled then //and GL_ARB_point_parameters
  begin
    glPointParameterf(GL_POINT_SIZE_MIN_ARB, FMinSize);
    glPointParameterf(GL_POINT_SIZE_MAX_ARB, FMaxSize);
    glPointParameterf(GL_POINT_FADE_THRESHOLD_SIZE_ARB, FFadeTresholdSize);
    glPointParameterfv(GL_DISTANCE_ATTENUATION_EXT, FDistanceAttenuation.AsAddress);
  end;
end;

procedure TgxPointParameters.UnApply;
begin
  if Enabled  then //and GL_ARB_point_parameters
  begin
    glPointParameterf(GL_POINT_SIZE_MIN_ARB, 0);
    glPointParameterf(GL_POINT_SIZE_MAX_ARB, 128);
    glPointParameterf(GL_POINT_FADE_THRESHOLD_SIZE_ARB, 1);
    glPointParameterfv(GL_DISTANCE_ATTENUATION_EXT, @XVector);
  end;
end;

procedure TgxPointParameters.SetEnabled(const val: Boolean);
begin
  if val <> FEnabled then
  begin
    FEnabled := val;
    NotifyChange(Self);
  end;
end;

procedure TgxPointParameters.SetMinSize(const val: Single);
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

procedure TgxPointParameters.SetMaxSize(const val: Single);
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

procedure TgxPointParameters.SetFadeTresholdSize(const val: Single);
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

procedure TgxPointParameters.SetDistanceAttenuation(const val: TgxCoordinates);
begin
  FDistanceAttenuation.Assign(val);
end;

// ------------------
// ------------------ TgxPoints ------------------
// ------------------

constructor TgxPoints.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FStyle := psSquare;
  FSize := cDefaultPointSize;
  FPositions := TgxAffineVectorList.Create;
  FPositions.Add(NullVector);
  FColors := TgxVectorList.Create;
  FPointParameters := TgxPointParameters.Create(Self);
end;

destructor TgxPoints.Destroy;
begin
  FPointParameters.Free;
  FColors.Free;
  FPositions.Free;
  inherited;
end;

procedure TgxPoints.Assign(Source: TPersistent);
begin
  if Source is TgxPoints then
  begin
    FSize := TgxPoints(Source).FSize;
    FStyle := TgxPoints(Source).FStyle;
    FPositions.Assign(TgxPoints(Source).FPositions);
    FColors.Assign(TgxPoints(Source).FColors);
    StructureChanged
  end;
  inherited Assign(Source);
end;

procedure TgxPoints.BuildList(var rci: TgxRenderContextInfo);
var
  n: Integer;
  v: TVector4f;
begin
  n := FPositions.Count;
  if n = 0 then
    Exit;

  case FColors.Count of
    0: glColor4f(1, 1, 1, 1);
    1: glColor4fv(PGLFloat(FColors.List));
  else
    if FColors.Count < n then
      n := FColors.Count;
    glColorPointer(4, GL_FLOAT, 0, FColors.List);
    glEnableClientState(GL_COLOR_ARRAY);
  end;
  if FColors.Count < 2 then
    glDisableClientState(GL_COLOR_ARRAY);

  rci.gxStates.Disable(stLighting);
  if n = 0 then
  begin
    v := NullHmgPoint;
    glVertexPointer(3, GL_FLOAT, 0, @v);
    n := 1;
  end
  else
    glVertexPointer(3, GL_FLOAT, 0, FPositions.List);
  glEnableClientState(GL_VERTEX_ARRAY);

  if NoZWrite then
    rci.gxStates.DepthWriteMask := boolean(False);
  rci.gxStates.PointSize := FSize;
  PointParameters.Apply;
  if (n > 64) then  /// and GL_EXT_compiled_vertex_array
    glLockArraysEXT(0, n);
  case FStyle of
    psSquare:
      begin
        // square point (simplest method, fastest)
        rci.gxStates.Disable(stBlend);
      end;
    psRound:
      begin
        rci.gxStates.Enable(stPointSmooth);
        rci.gxStates.Enable(stAlphaTest);
        rci.gxStates.SetAlphaFunction(cfGreater, 0.5);
        rci.gxStates.Disable(stBlend);
      end;
    psSmooth:
      begin
        rci.gxStates.Enable(stPointSmooth);
        rci.gxStates.Enable(stAlphaTest);
        rci.gxStates.SetAlphaFunction(cfNotEqual, 0.0);
        rci.gxStates.Enable(stBlend);
        rci.gxStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      end;
    psSmoothAdditive:
      begin
        rci.gxStates.Enable(stPointSmooth);
        rci.gxStates.Enable(stAlphaTest);
        rci.gxStates.SetAlphaFunction(cfNotEqual, 0.0);
        rci.gxStates.Enable(stBlend);
        rci.gxStates.SetBlendFunc(bfSrcAlpha, bfOne);
      end;
    psSquareAdditive:
      begin
        rci.gxStates.Enable(stBlend);
        rci.gxStates.SetBlendFunc(bfSrcAlpha, bfOne);
      end;
  else
    Assert(False);
  end;
  glDrawArrays(GL_POINTS, 0, n);
  if (n > 64) then ///and GL_EXT_compiled_vertex_array
    glUnlockArraysEXT;
  PointParameters.UnApply;
  glDisableClientState(GL_VERTEX_ARRAY);
  if FColors.Count > 1 then
    glDisableClientState(GL_COLOR_ARRAY);
end;

function TgxPoints.StoreSize: Boolean;
begin
  Result := (FSize <> cDefaultPointSize);
end;

procedure TgxPoints.SetNoZWrite(const val: Boolean);
begin
  if FNoZWrite <> val then
  begin
    FNoZWrite := val;
    StructureChanged;
  end;
end;

procedure TgxPoints.SetStatic(const val: Boolean);
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

procedure TgxPoints.SetSize(const val: Single);
begin
  if FSize <> val then
  begin
    FSize := val;
    StructureChanged;
  end;
end;

procedure TgxPoints.SetPositions(const val: TgxAffineVectorList);
begin
  FPositions.Assign(val);
  StructureChanged;
end;

procedure TgxPoints.SetColors(const val: TgxVectorList);
begin
  FColors.Assign(val);
  StructureChanged;
end;

procedure TgxPoints.SetStyle(const val: TgxPointStyle);
begin
  if FStyle <> val then
  begin
    FStyle := val;
    StructureChanged;
  end;
end;

procedure TgxPoints.SetPointParameters(const val: TgxPointParameters);
begin
  FPointParameters.Assign(val);
end;

// ------------------
// ------------------ TgxLineBase ------------------
// ------------------

constructor TgxLineBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLineColor := TgxColor.Create(Self);
  FLineColor.Initialize(clrWhite);
  FLinePattern := $FFFF;
  FAntiAliased := False;
  FLineWidth := 1.0;
end;

destructor TgxLineBase.Destroy;
begin
  FLineColor.Free;
  inherited Destroy;
end;

procedure TgxLineBase.NotifyChange(Sender: TObject);
begin
  if Sender = FLineColor then
    StructureChanged;
  inherited;
end;

procedure TgxLineBase.SetLineColor(const Value: TgxColor);
begin
  FLineColor.Color := Value.Color;
  StructureChanged;
end;

procedure TgxLineBase.SetLinePattern(const Value: GLushort);
begin
  if FLinePattern <> Value then
  begin
    FLinePattern := Value;
    StructureChanged;
  end;
end;

procedure TgxLineBase.SetLineWidth(const val: Single);
begin
  if FLineWidth <> val then
  begin
    FLineWidth := val;
    StructureChanged;
  end;
end;

function TgxLineBase.StoreLineWidth: Boolean;
begin
  Result := (FLineWidth <> 1.0);
end;

procedure TgxLineBase.SetAntiAliased(const val: Boolean);
begin
  if FAntiAliased <> val then
  begin
    FAntiAliased := val;
    StructureChanged;
  end;
end;

procedure TgxLineBase.Assign(Source: TPersistent);
begin
  if Source is TgxLineBase then
  begin
    LineColor := TgxLineBase(Source).FLineColor;
    LinePattern := TgxLineBase(Source).FLinePattern;
    LineWidth := TgxLineBase(Source).FLineWidth;
    AntiAliased := TgxLineBase(Source).FAntiAliased;
  end;
  inherited Assign(Source);
end;

procedure TgxLineBase.SetupLineStyle(var rci: TgxRenderContextInfo);
begin
  with rci.gxStates do
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
      glColor4fv(FLineColor.AsAddress);
    end
    else
      glColor3fv(FLineColor.AsAddress);

  end;
end;

// ------------------
// ------------------ TgxLinesNode ------------------
// ------------------

constructor TgxLinesNode.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FColor := TgxColor.Create(Self);
  FColor.Initialize((TgxLinesNodes(Collection).GetOwner as TgxLines)
    .NodeColor.Color);
  FColor.OnNotifyChange := OnColorChange;
end;

destructor TgxLinesNode.Destroy;
begin
  FColor.Free;
  inherited Destroy;
end;

procedure TgxLinesNode.Assign(Source: TPersistent);
begin
  if Source is TgxLinesNode then
    FColor.Assign(TgxLinesNode(Source).FColor);
  inherited;
end;

procedure TgxLinesNode.SetColor(const val: TgxColor);
begin
  FColor.Assign(val);
end;

procedure TgxLinesNode.OnColorChange(Sender: TObject);
begin
  (Collection as TgxNodes).NotifyChange;
end;

function TgxLinesNode.StoreColor: Boolean;
begin
  Result := not VectorEquals((TgxLinesNodes(Collection).GetOwner as TgxLines)
    .NodeColor.Color, FColor.Color);
end;

// ------------------
// ------------------ TgxLinesNodes ------------------
// ------------------

constructor TgxLinesNodes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TgxLinesNode);
end;

procedure TgxLinesNodes.NotifyChange;
begin
  if (GetOwner <> nil) then
    (GetOwner as TgxBaseSceneObject).StructureChanged;
end;

// ------------------
// ------------------ TgxNodedLines ------------------
// ------------------

constructor TgxNodedLines.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNodes := TgxLinesNodes.Create(Self);
  FNodeColor := TgxColor.Create(Self);
  FNodeColor.Initialize(clrBlue);
  FNodeColor.OnNotifyChange := OnNodeColorChanged;
  FOldNodeColor := clrBlue;
  FNodesAspect := lnaAxes;
  FNodeSize := 1;
end;

destructor TgxNodedLines.Destroy;
begin
  FNodes.Free;
  FNodeColor.Free;
  inherited Destroy;
end;

procedure TgxNodedLines.SetNodesAspect(const Value: TLineNodesAspect);
begin
  if Value <> FNodesAspect then
  begin
    FNodesAspect := Value;
    StructureChanged;
  end;
end;

procedure TgxNodedLines.SetNodeColor(const Value: TgxColor);
begin
  FNodeColor.Color := Value.Color;
  StructureChanged;
end;

procedure TgxNodedLines.OnNodeColorChanged(Sender: TObject);
var
  i: Integer;
begin
  // update color for nodes...
  for i := 0 to Nodes.Count - 1 do
    if VectorEquals(TgxLinesNode(Nodes[i]).Color.Color, FOldNodeColor) then
      TgxLinesNode(Nodes[i]).Color.Assign(FNodeColor);
  SetVector(FOldNodeColor, FNodeColor.Color);
end;

procedure TgxNodedLines.SetNodes(const aNodes: TgxLinesNodes);
begin
  FNodes.Assign(aNodes);
  StructureChanged;
end;

procedure TgxNodedLines.SetNodeSize(const val: Single);
begin
  if val <= 0 then
    FNodeSize := 1
  else
    FNodeSize := val;
  StructureChanged;
end;

function TgxNodedLines.StoreNodeSize: Boolean;
begin
  Result := FNodeSize <> 1;
end;

procedure TgxNodedLines.Assign(Source: TPersistent);
begin
  if Source is TgxNodedLines then
  begin
    SetNodes(TgxNodedLines(Source).FNodes);
    FNodesAspect := TgxNodedLines(Source).FNodesAspect;
    FNodeColor.Color := TgxNodedLines(Source).FNodeColor.Color;
    FNodeSize := TgxNodedLines(Source).FNodeSize;
  end;
  inherited Assign(Source);
end;

procedure TgxNodedLines.DrawNode(var rci: TgxRenderContextInfo; X, Y, Z: Single;
  Color: TgxColor);
begin
  glPushMatrix;
  glTranslatef(X, Y, Z);
  case NodesAspect of
    lnaAxes:
      AxesBuildList(rci, $CCCC, FNodeSize * 0.5);
    lnaCube:
      CubeWireframeBuildList(rci, FNodeSize, False, Color.Color);
  else
    Assert(False)
  end;
  glPopMatrix;
end;

function TgxNodedLines.AxisAlignedDimensionsUnscaled: TVector4f;
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

procedure TgxNodedLines.AddNode(const coords: TgxCoordinates);
var
  n: TgxNode;
begin
  n := Nodes.Add;
  if Assigned(coords) then
    n.AsVector := coords.AsVector;
  StructureChanged;
end;

procedure TgxNodedLines.AddNode(const X, Y, Z: Single);
var
  n: TgxNode;
begin
  n := Nodes.Add;
  n.AsVector := VectorMake(X, Y, Z, 1);
  StructureChanged;
end;

procedure TgxNodedLines.AddNode(const Value: TVector4f);
var
  n: TgxNode;
begin
  n := Nodes.Add;
  n.AsVector := Value;
  StructureChanged;
end;

procedure TgxNodedLines.AddNode(const Value: TAffineVector);
var
  n: TgxNode;
begin
  n := Nodes.Add;
  n.AsVector := VectorMake(Value);
  StructureChanged;
end;

// ------------------
// ------------------ TgxLines ------------------
// ------------------

constructor TgxLines.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDivision := 10;
  FSplineMode := lsmLines;
  FNURBSKnots := TgxSingleList.Create;
  FNURBSOrder := 0;
  FNURBSTolerance := 50;
end;

destructor TgxLines.Destroy;
begin
  FNURBSKnots.Free;
  inherited Destroy;
end;

procedure TgxLines.SetDivision(const Value: Integer);
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

procedure TgxLines.SetOptions(const val: TgxLinesOptions);
begin
  FOptions := val;
  StructureChanged;
end;

procedure TgxLines.SetSplineMode(const val: TgxLineSplineMode);
begin
  if FSplineMode <> val then
  begin
    FSplineMode := val;
    StructureChanged;
  end;
end;

procedure TgxLines.SetNURBSOrder(const val: Integer);
begin
  if val <> FNURBSOrder then
  begin
    FNURBSOrder := val;
    StructureChanged;
  end;
end;

procedure TgxLines.SetNURBSTolerance(const val: Single);
begin
  if val <> FNURBSTolerance then
  begin
    FNURBSTolerance := val;
    StructureChanged;
  end;
end;

procedure TgxLines.Assign(Source: TPersistent);
begin
  if Source is TgxLines then
  begin
    FDivision := TgxLines(Source).FDivision;
    FSplineMode := TgxLines(Source).FSplineMode;
    FOptions := TgxLines(Source).FOptions;
  end;
  inherited Assign(Source);
end;

procedure TgxLines.BuildList(var rci: TgxRenderContextInfo);
var
  i, n: Integer;
  A, B, C: Single;
  f: Single;
  Spline: TCubicSpline;
  vertexColor: TVector4f;
  nodeBuffer: array of TAffineVector;
  colorBuffer: array of TVector4f;
   nurbsRenderer : GLUNurbsObj;
begin
  if Nodes.Count > 1 then
  begin
    // first, we setup the line color & stippling styles
    SetupLineStyle(rci);
    if rci.bufferDepthTest then
      rci.gxStates.Enable(stDepthTest);
    if loColorLogicXor in Options then
    begin
      rci.gxStates.Enable(stColorLogicOp);
      rci.gxStates.LogicOpMode := loXOr;
    end;
    // Set up the control point buffer for Bezier splines and NURBS curves.
    // If required this could be optimized by storing a cached node buffer.
    if (FSplineMode = lsmBezierSpline) or (FSplineMode = lsmNURBSCurve) then
    begin
      SetLength(nodeBuffer, Nodes.Count);
      SetLength(colorBuffer, Nodes.Count);
      for i := 0 to Nodes.Count - 1 do
        with TgxLinesNode(Nodes[i]) do
        begin
          nodeBuffer[i] := AsAffineVector;
          colorBuffer[i] := Color.Color;
        end;
    end;

    if FSplineMode = lsmBezierSpline then
    begin
      // map evaluator
      glPushAttrib(GL_EVAL_BIT);
      glEnable(GL_MAP1_VERTEX_3);
      glEnable(GL_MAP1_COLOR_4);

      glMap1f(GL_MAP1_VERTEX_3, 0, 1, 3, Nodes.Count, @nodeBuffer[0]);
      glMap1f(GL_MAP1_COLOR_4, 0, 1, 4, Nodes.Count, @colorBuffer[0]);
    end;

    // start drawing the line
    if (FSplineMode = lsmNURBSCurve) and (FDivision >= 2) then
    begin
      if (FNURBSOrder > 0) and (FNURBSKnots.Count > 0) then
      begin

        nurbsRenderer := gluNewNurbsRenderer;
        try
          gluNurbsProperty(nurbsRenderer, GLU_SAMPLING_TOLERANCE, FNURBSTolerance);
          gluNurbsProperty(nurbsRenderer, GLU_DISPLAY_MODE, GLU_FILL);
          gluBeginCurve(nurbsRenderer);
          gluNurbsCurve(nurbsRenderer, FNURBSKnots.Count, @FNURBSKnots.List[0],
            3, @nodeBuffer[0], FNURBSOrder, GL_MAP1_VERTEX_3);
          gluEndCurve(nurbsRenderer);
        finally
          gluDeleteNurbsRenderer(nurbsRenderer);
        end;
      end;
    end
    else
    begin
      // lines, cubic splines or bezier
      if FSplineMode = lsmSegments then
        glBegin(GL_LINES)
      else if FSplineMode = lsmLoop then
        glBegin(GL_LINE_LOOP)
      else
        glBegin(GL_LINE_STRIP);
      if (FDivision < 2) or (FSplineMode in [lsmLines, lsmSegments,
        lsmLoop]) then
      begin
        // standard line(s), draw directly
        if loUseNodeColorForLines in Options then
        begin
          // node color interpolation
          for i := 0 to Nodes.Count - 1 do
            with TgxLinesNode(Nodes[i]) do
            begin
              glColor4fv(Color.AsAddress);
              glVertex3f(X, Y, Z);
            end;
        end
        else
        begin
          // single color
          for i := 0 to Nodes.Count - 1 do
            with Nodes[i] do
              glVertex3f(X, Y, Z);
        end;
      end
      else if FSplineMode = lsmCubicSpline then
      begin
        // cubic spline
        Spline := Nodes.CreateNewCubicSpline;
        try
          f := 1 / FDivision;
          for i := 0 to (Nodes.Count - 1) * FDivision do
          begin
            Spline.SplineXYZ(i * f, A, B, C);
            if loUseNodeColorForLines in Options then
            begin
              n := (i div FDivision);
              if n < Nodes.Count - 1 then
                VectorLerp(TgxLinesNode(Nodes[n]).Color.Color,
                  TgxLinesNode(Nodes[n + 1]).Color.Color, 
				  (i mod FDivision) * f, vertexColor)
              else
                SetVector(vertexColor, TgxLinesNode(Nodes[Nodes.Count - 1]).Color.Color);
              glColor4fv(@vertexColor);
            end;
            glVertex3f(A, B, C);
          end;
        finally
          Spline.Free;
        end;
      end
      else if FSplineMode = lsmBezierSpline then
      begin
        f := 1 / FDivision;
        for i := 0 to FDivision do
          glEvalCoord1f(i * f);
      end;
      glEnd;
    end;
    rci.gxStates.Disable(stColorLogicOp);

    if FSplineMode = lsmBezierSpline then
      rci.gxStates.PopAttrib;
    if Length(nodeBuffer) > 0 then
    begin
      SetLength(nodeBuffer, 0);
      SetLength(colorBuffer, 0);
    end;

    if FNodesAspect <> lnaInvisible then
    begin
      if not rci.ignoreBlendingRequests then
      begin
        rci.gxStates.Enable(stBlend);
        rci.gxStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      end;

      for i := 0 to Nodes.Count - 1 do
        with TgxLinesNode(Nodes[i]) do
          DrawNode(rci, X, Y, Z, Color);
    end;
  end;
end;

// ------------------
// ------------------ TgxCube ------------------
// ------------------

constructor TgxCube.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCubeSize := XYZVector;
  FParts := [cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight];
  FNormalDirection := ndOutside;
  ObjectStyle := ObjectStyle + [osDirectDraw];
end;

procedure TgxCube.BuildList(var rci: TgxRenderContextInfo);
var
  v1: TAffineVector;
  v2: TAffineVector;
  v1d: TAffineVector;
  v2d: TAffineVector;
  nd: Single;
  TanLoc, BinLoc: Integer;
begin
  VectorScale(FCubeSize, 0.5, v2);
  v1 := VectorNegate(v2);
  if FNormalDirection = ndInside then
  begin
    v1d := v2;
    v2d := v1;
    nd  := -1
  end
  else begin
    v1d := v1;
    v2d := v2;
    nd  := 1;
  end;

  if (rci.gxStates.CurrentProgram > 0) then  //and GL_ARB_shader_objects
  begin
    TanLoc := glGetAttribLocation(rci.gxStates.CurrentProgram, PGLChar(TangentAttributeName));
    BinLoc := glGetAttribLocation(rci.gxStates.CurrentProgram, PGLChar(BinormalAttributeName));
  end
  else
  begin
    TanLoc := -1;
    BinLoc := -1;
  end;
    glBegin(GL_QUADS);
    if cpFront in FParts then
    begin
      glNormal3f(0, 0, nd);
      if TanLoc > -1 then
        glVertexAttrib3f(TanLoc, nd, 0, 0);
      if BinLoc > -1 then
        glVertexAttrib3f(BinLoc, 0, nd, 0);
      glTexCoord2fv(@XYTexPoint);
      glVertex3fv(@v2);
      glTexCoord2fv(@YTexPoint);
      glVertex3f(v1d.x, v2d.y,  v2.z);
      glTexCoord2fv(@NullTexPoint);
      glVertex3f(v1.x,  v1.y,   v2.z);
      glTexCoord2fv(@XTexPoint);
      glVertex3f(v2d.x, v1d.y,  v2.z);
    end;
    if cpBack in FParts then
    begin
      glNormal3f(0, 0, -nd);
      if TanLoc > -1 then
        glVertexAttrib3f(TanLoc, -nd, 0, 0);
      if BinLoc > -1 then
        glVertexAttrib3f(BinLoc, 0, nd, 0);

      glTexCoord2fv(@YTexPoint);
      glVertex3f(v2.x,   v2.y,   v1.z);
      glTexCoord2fv(@NullTexPoint);
      glVertex3f(v2d.x,  v1d.y,  v1.z);
      glTexCoord2fv(@XYTexPoint);
      glVertex3fv(@v1);
      glTexCoord2fv(@XYTexPoint);
      glVertex3f(v1d.x,  v2d.y,  v1.z);
    end;
    if cpLeft in FParts then
    begin
      glNormal3f(-nd, 0, 0);
      if TanLoc > -1 then
        glVertexAttrib3f(TanLoc, 0, 0, nd);
      if BinLoc > -1 then
        glVertexAttrib3f(BinLoc, 0, nd, 0);
      glTexCoord2fv(@XYTexPoint);
      glVertex3f(v1.x, v2.y, v2.z);
      glTexCoord2fv(@YTexPoint);
      glVertex3f(v1.x, v2d.y, v1d.z);
      glTexCoord2fv(@NullTexPoint);
      glVertex3fv(@v1);
      glTexCoord2fv(@XTexPoint);
      glVertex3f(v1.x, v1d.y, v2d.z);
    end;
    if cpRight in FParts then
    begin
      glNormal3f(nd, 0, 0);
      if TanLoc > -1 then
        glVertexAttrib3f(TanLoc, 0, 0, -nd);
      if BinLoc > -1 then
        glVertexAttrib3f(BinLoc, 0, nd, 0);
      glTexCoord2fv(@YTexPoint);
      glVertex3fv(@v2);
      glTexCoord2fv(@NullTexPoint);
      glVertex3f(v2.x, v1d.y, v2d.z);
      glTexCoord2fv(@XTexPoint);
      glVertex3f(v2.x, v1.y, v1.z);
      glTexCoord2fv(@XYTexPoint);
      glVertex3f(v2.x, v2d.y, v1d.z);
    end;
    if cpTop in FParts then
    begin
      glNormal3f(0, nd, 0);
      if TanLoc > -1 then
        glVertexAttrib3f(TanLoc, nd, 0, 0);
      if BinLoc > -1 then
        glVertexAttrib3f(BinLoc, 0, 0, -nd);
      glTexCoord2fv(@YTexPoint);
      glVertex3f(v1.x, v2.y, v1.z);
      glTexCoord2fv(@NullTexPoint);
      glVertex3f(v1d.x, v2.y, v2d.z);
      glTexCoord2fv(@XTexPoint);
      glVertex3fv(@v2);
      glTexCoord2fv(@XYTexPoint);
      glVertex3f(v2d.x, v2.y, v1d.z);
    end;
    if cpBottom in FParts then
    begin
      glNormal3f(0, -nd, 0);
      if TanLoc > -1 then
        glVertexAttrib3f(TanLoc, -nd, 0, 0);
      if BinLoc > -1 then
        glVertexAttrib3f(BinLoc, 0, 0, nd);
      glTexCoord2fv(@NullTexPoint);
      glVertex3fv(@v1);
      glTexCoord2fv(@XYTexPoint);
      glVertex3f(v2d.x, v1.y, v1d.z);
      glTexCoord2fv(@XYTexPoint);
      glVertex3f(v2.x, v1.y, v2.z);
      glTexCoord2fv(@YTexPoint);
      glVertex3f(v1d.x, v1.y, v2d.z);
    end;
    glEnd;
end;

function TgxCube.GenerateSilhouette(const silhouetteParameters
  : TgxSilhouetteParameters): TgxSilhouette;
var
  hw, hh, hd: Single;
  connectivity: TConnectivity;
  sil: TgxSilhouette;
begin
  connectivity := TConnectivity.Create(True);

  hw := FCubeSize.X * 0.5;
  hh := FCubeSize.Y * 0.5;
  hd := FCubeSize.Z * 0.5;

  if cpFront in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(hw, hh, hd),
      AffineVectorMake(-hw, hh, hd), AffineVectorMake(-hw, -hh, hd),
      AffineVectorMake(hw, -hh, hd));
  end;
  if cpBack in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(hw, hh, -hd),
      AffineVectorMake(hw, -hh, -hd), AffineVectorMake(-hw, -hh, -hd),
      AffineVectorMake(-hw, hh, -hd));
  end;
  if cpLeft in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(-hw, hh, hd),
      AffineVectorMake(-hw, hh, -hd), AffineVectorMake(-hw, -hh, -hd),
      AffineVectorMake(-hw, -hh, hd));
  end;
  if cpRight in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(hw, hh, hd),
      AffineVectorMake(hw, -hh, hd), AffineVectorMake(hw, -hh, -hd),
      AffineVectorMake(hw, hh, -hd));
  end;
  if cpTop in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(-hw, hh, -hd),
      AffineVectorMake(-hw, hh, hd), AffineVectorMake(hw, hh, hd),
      AffineVectorMake(hw, hh, -hd));
  end;
  if cpBottom in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(-hw, -hh, -hd),
      AffineVectorMake(hw, -hh, -hd), AffineVectorMake(hw, -hh, hd),
      AffineVectorMake(-hw, -hh, hd));
  end;
  sil := nil;
  connectivity.CreateSilhouette(silhouetteParameters, sil, False);
  Result := sil;
  connectivity.Free;
end;

function TgxCube.GetCubeWHD(const Index: Integer): Single;
begin
  Result := FCubeSize.V[index];
end;


procedure TgxCube.SetCubeWHD(Index: Integer; AValue: Single);
begin
  if AValue <> FCubeSize.V[index] then
  begin
    FCubeSize.V[index] := AValue;
    StructureChanged;
  end;
end;


procedure TgxCube.SetParts(aValue: TgxCubeParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

procedure TgxCube.SetNormalDirection(aValue: TgxNormalDirection);
begin
  if aValue <> FNormalDirection then
  begin
    FNormalDirection := aValue;
    StructureChanged;
  end;
end;

procedure TgxCube.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TgxCube) then
  begin
    FCubeSize := TgxCube(Source).FCubeSize;
    FParts := TgxCube(Source).FParts;
    FNormalDirection := TgxCube(Source).FNormalDirection;
  end;
  inherited Assign(Source);
end;

function TgxCube.AxisAlignedDimensionsUnscaled: TVector4f;
begin
  Result.X := FCubeSize.X * 0.5;
  Result.Y := FCubeSize.Y * 0.5;
  Result.Z := FCubeSize.Z * 0.5;
  Result.W := 0;
end;

function TgxCube.RayCastIntersect(const rayStart, rayVector: TVector4f;
  intersectPoint: PVector4f = nil; intersectNormal: PVector4f = nil): Boolean;
var
  p: array [0 .. 5] of TVector4f;
  rv: TVector4f;
  rs, r: TVector4f;
  i: Integer;
  t: Single;
  eSize: TAffineVector;
begin
  rs := AbsoluteToLocal(rayStart);
  SetVector(rv, VectorNormalize(AbsoluteToLocal(rayVector)));
  eSize.X := FCubeSize.X*0.5 + 0.0001;
  eSize.Y := FCubeSize.Y*0.5 + 0.0001;
  eSize.Z := FCubeSize.Z*0.5 + 0.0001;
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
      t := -(p[i].X * rs.X + p[i].Y * rs.Y +
             p[i].Z * rs.Z + 0.5 *
        FCubeSize.V[i mod 3]) / (p[i].X * rv.X +
                                 p[i].Y * rv.Y +
                                 p[i].Z * rv.Z);
      MakePoint(r, rs.X + t * rv.X, rs.Y +
                             t * rv.Y, rs.Z +
                             t * rv.Z);
      if (Abs(r.X) <= eSize.X) and
         (Abs(r.Y) <= eSize.Y) and
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

procedure TgxCube.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('CubeSize', ReadData, WriteData,
    (FCubeSize.X <> 1) or (FCubeSize.Y <> 1) or (FCubeSize.Z <> 1));
end;

procedure TgxCube.ReadData(Stream: TStream);
begin
  with Stream do
  begin
    Read(FCubeSize, SizeOf(TAffineVector));
  end;
end;

procedure TgxCube.WriteData(Stream: TStream);
begin
  with Stream do
  begin
    Write(FCubeSize, SizeOf(TAffineVector));
  end;
end;

// ------------------
// ------------------ TgxQuadricObject ------------------
// ------------------

constructor TgxQuadricObject.Create(AOwner: TComponent);
begin
  inherited;
  FNormals := nsSmooth;
  FNormalDirection := ndOutside;
end;

procedure TgxQuadricObject.SetNormals(aValue: TgxNormalSmoothing);
begin
  if aValue <> FNormals then
  begin
    FNormals := aValue;
    StructureChanged;
  end;
end;

procedure TgxQuadricObject.SetNormalDirection(aValue: TgxNormalDirection);
begin
  if aValue <> FNormalDirection then
  begin
    FNormalDirection := aValue;
    StructureChanged;
  end;
end;

procedure TgxQuadricObject.SetupQuadricParams(quadric: GLUquadricObj);
const
  cNormalSmoothinToEnum: array [nsFlat .. nsNone] of Cardinal = (GLU_FLAT,
    GLU_SMOOTH, GLU_NONE);
begin
	gluQuadricDrawStyle(@Quadric, GLU_FILL);
	gluQuadricNormals(@Quadric, cNormalSmoothinToEnum[FNormals]);
   SetNormalQuadricOrientation(Quadric);
	gluQuadricTexture(@Quadric, 1);
end;

procedure TgxQuadricObject.SetNormalQuadricOrientation(quadric: GLUquadricObj);
const
  cNormalDirectionToEnum: array [ndInside .. ndOutside] of GLEnum =
    (GLU_INSIDE, GLU_OUTSIDE);
begin
  gluQuadricOrientation(@quadric, cNormalDirectionToEnum[FNormalDirection]);
end;

procedure TgxQuadricObject.SetInvertedQuadricOrientation(quadric: GLUquadricObj);
const
  cNormalDirectionToEnum: array [ndInside .. ndOutside] of GLEnum =
    (GLU_OUTSIDE, GLU_INSIDE);
begin
  gluQuadricOrientation(@quadric, cNormalDirectionToEnum[FNormalDirection]);
end;

procedure TgxQuadricObject.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TgxQuadricObject) then
  begin
    FNormals := TgxQuadricObject(Source).FNormals;
    FNormalDirection := TgxQuadricObject(Source).FNormalDirection;
  end;
  inherited Assign(Source);
end;

// ------------------
// ------------------ TgxSphere ------------------
// ------------------

constructor TgxSphere.Create(AOwner: TComponent);
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

procedure TgxSphere.BuildList(var rci: TgxRenderContextInfo);
var
  V1, V2, N1 : TAffineVector;
  AngTop, AngBottom, AngStart, AngStop, StepV, StepH: Double;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Double;
  uTexCoord, uTexFactor, vTexFactor, vTexCoord0, vTexCoord1: Single;
  I, J: Integer;
  DoReverse: Boolean;
begin
  DoReverse := (FNormalDirection = ndInside);
   glPushAttrib(GL_POLYGON_BIT);
  if DoReverse then
    rci.gxStates.InvertFrontFace;

  // common settings
  AngTop := DegToRad(1.0 * FTop);
  AngBottom := DegToRad(1.0 * FBottom);
  AngStart := DegToRad(1.0 * FStart);
  AngStop := DegToRad(1.0 * FStop);
  StepH := (AngStop - AngStart) / FSlices;
  StepV := (AngTop - AngBottom) / FStacks;
  glPushMatrix;
  glScalef(Radius, Radius, Radius);

  // top cap
  if (FTop < 90) and (FTopCap in [ctCenter, ctFlat]) then
  begin
    glBegin(GL_TRIANGLE_FAN);
    SinCosine(AngTop, SinP, CosP);
    glTexCoord2f(0.5, 0.5);
    if DoReverse then
      glNormal3f(0, -1, 0)
    else
      glNormal3f(0, 1, 0);
    if FTopCap = ctCenter then
      glVertex3f(0, 0, 0)
    else
    begin
      glVertex3f(0, SinP, 0);
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
      glTexCoord2f(SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
      glNormal3fv(@N1);
      glVertex3fv(@v1);
      Theta := Theta + StepH;
    end;
    glEnd;
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
    V2.Y := SinP2;
    vTexCoord0 := 1 - j * vTexFactor;
    vTexCoord1 := 1 - (j + 1) * vTexFactor;

    glBegin(GL_TRIANGLE_STRIP);
    for i := 0 to FSlices do
    begin

      SinCos(Theta, SinT, CosT);
      v1.X := CosP * SinT;
      V2.X := CosP2 * SinT;
      v1.Z := CosP * CosT;
      V2.Z := CosP2 * CosT;

      uTexCoord := i * uTexFactor;
      glTexCoord2f(uTexCoord, vTexCoord0);
      if DoReverse then
      begin
        N1 := VectorNegate(v1);
        glNormal3fv(@N1);
      end
      else
        glNormal3fv(@v1);
      glVertex3fv(@v1);

      glTexCoord2f(uTexCoord, vTexCoord1);
      if DoReverse then
      begin
        N1 := VectorNegate(V2);
        glNormal3fv(@N1);
      end
      else
        glNormal3fv(@V2);
      glVertex3fv(@V2);

      Theta := Theta + StepH;
    end;
    glEnd;
    Phi := Phi2;
    Phi2 := Phi2 - StepV;
  end;

  // bottom cap
  if (FBottom > -90) and (FBottomCap in [ctCenter, ctFlat]) then
  begin
    glBegin(GL_TRIANGLE_FAN);
    SinCos(AngBottom, SinP, CosP);
    glTexCoord2f(0.5, 0.5);
    if DoReverse then
      glNormal3f(0, 1, 0)
    else
      glNormal3f(0, -1, 0);
    if FBottomCap = ctCenter then
      glVertex3f(0, 0, 0)
    else
    begin
      glVertex3f(0, SinP, 0);
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
      glTexCoord2f(SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
      glNormal3fv(@N1);
      glVertex3fv(@v1);
      Theta := Theta - StepH;
    end;
    glEnd;
  end;
  if DoReverse then
    rci.gxStates.InvertFrontFace;
  glPopMatrix;
  rci.gxStates.PopAttrib;
end;

function TgxSphere.RayCastIntersect(const rayStart, rayVector: TVector4f;
  intersectPoint: PVector4f = nil; intersectNormal: PVector4f = nil): Boolean;
var
  i1, i2: TVector4f;
  localStart, localVector: TVector4f;
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

function TgxSphere.GenerateSilhouette(const silhouetteParameters
  : TgxSilhouetteParameters): TgxSilhouette;
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
  Result := TgxSilhouette.Create;
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

procedure TgxSphere.SetBottom(aValue: TAngleLimit1);
begin
  if FBottom <> aValue then
  begin
    FBottom := aValue;
    StructureChanged;
  end;
end;

procedure TgxSphere.SetBottomCap(aValue: TgxCapType);
begin
  if FBottomCap <> aValue then
  begin
    FBottomCap := aValue;
    StructureChanged;
  end;
end;

procedure TgxSphere.SetRadius(const aValue: Single);
begin
  if aValue <> FRadius then
  begin
    FRadius := aValue;
    StructureChanged;
  end;
end;

procedure TgxSphere.SetSlices(aValue: Integer);
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

procedure TgxSphere.SetStacks(aValue: GLint);
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

procedure TgxSphere.SetStart(aValue: TAngleLimit2);
begin
  if FStart <> aValue then
  begin
    Assert(aValue <= FStop);
    FStart := aValue;
    StructureChanged;
  end;
end;

procedure TgxSphere.SetStop(aValue: TAngleLimit2);
begin
  if FStop <> aValue then
  begin
    Assert(aValue >= FStart);
    FStop := aValue;
    StructureChanged;
  end;
end;

procedure TgxSphere.SetTop(aValue: TAngleLimit1);
begin
  if FTop <> aValue then
  begin
    FTop := aValue;
    StructureChanged;
  end;
end;

procedure TgxSphere.SetTopCap(aValue: TgxCapType);
begin
  if FTopCap <> aValue then
  begin
    FTopCap := aValue;
    StructureChanged;
  end;
end;

procedure TgxSphere.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TgxSphere) then
  begin
    FRadius := TgxSphere(Source).FRadius;
    FSlices := TgxSphere(Source).FSlices;
    FStacks := TgxSphere(Source).FStacks;
    FBottom := TgxSphere(Source).FBottom;
    FTop := TgxSphere(Source).FTop;
    FStart := TgxSphere(Source).FStart;
    FStop := TgxSphere(Source).FStop;
  end;
  inherited Assign(Source);
end;

function TgxSphere.AxisAlignedDimensionsUnscaled: TVector4f;
begin
  Result.X := Abs(FRadius);
  Result.Y := Result.X;
  Result.Z := Result.X;
  Result.W := 0;
end;

// ------------------
// ------------------ TgxPolygonBase ------------------
// ------------------

constructor TgxPolygonBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateNodes;
  FDivision := 10;
  FSplineMode := lsmLines;
end;

procedure TgxPolygonBase.CreateNodes;
begin
  FNodes := TgxNodes.Create(Self);
end;

destructor TgxPolygonBase.Destroy;
begin
  FNodes.Free;
  inherited Destroy;
end;

procedure TgxPolygonBase.Assign(Source: TPersistent);
begin
  if Source is TgxPolygonBase then
  begin
    SetNodes(TgxPolygonBase(Source).FNodes);
    FDivision := TgxPolygonBase(Source).FDivision;
    FSplineMode := TgxPolygonBase(Source).FSplineMode;
  end;
  inherited Assign(Source);
end;

procedure TgxPolygonBase.NotifyChange(Sender: TObject);
begin
  if Sender = Nodes then
    StructureChanged;
  inherited;
end;

procedure TgxPolygonBase.SetDivision(const Value: Integer);
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

procedure TgxPolygonBase.SetNodes(const aNodes: TgxNodes);
begin
  FNodes.Assign(aNodes);
  StructureChanged;
end;

procedure TgxPolygonBase.SetSplineMode(const val: TgxLineSplineMode);
begin
  if FSplineMode <> val then
  begin
    FSplineMode := val;
    StructureChanged;
  end;
end;

procedure TgxPolygonBase.AddNode(const coords: TgxCoordinates);
var
  n: TgxNode;
begin
  n := Nodes.Add;
  if Assigned(coords) then
    n.AsVector := coords.AsVector;
  StructureChanged;
end;

procedure TgxPolygonBase.AddNode(const X, Y, Z: Single);
var
  n: TgxNode;
begin
  n := Nodes.Add;
  n.AsVector := VectorMake(X, Y, Z, 1);
  StructureChanged;
end;

procedure TgxPolygonBase.AddNode(const Value: TVector4f);
var
  n: TgxNode;
begin
  n := Nodes.Add;
  n.AsVector := Value;
  StructureChanged;
end;

procedure TgxPolygonBase.AddNode(const Value: TAffineVector);
var
  n: TgxNode;
begin
  n := Nodes.Add;
  n.AsVector := VectorMake(Value);
  StructureChanged;
end;

// ------------------
// ------------------ TgxSuperellipsoid ------------------
// ------------------

constructor TgxSuperellipsoid.Create(AOwner: TComponent);
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

procedure TgxSuperellipsoid.BuildList(var rci: TgxRenderContextInfo);
var
  CosPc1, SinPc1, CosTc2, SinTc2: Double;
  tc1, tc2: integer;
  v1, v2, vs, N1: TAffineVector;
  AngTop, AngBottom, AngStart, AngStop, StepV, StepH: Double;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Double;
  uTexCoord, uTexFactor, vTexFactor, vTexCoord0, vTexCoord1: Double;
  i, j: Integer;
  DoReverse: Boolean;

begin
  DoReverse := (FNormalDirection = ndInside);
  if DoReverse then
    rci.gxStates.InvertFrontFace;

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
    VCurve := VCurve + 1e-6;
  if tc2 mod 2 = 0 then
    HCurve := HCurve - 1e-6;

  // top cap
  if (FTop < 90) and (FTopCap in [ctCenter, ctFlat]) then
  begin
    glBegin(GL_TRIANGLE_FAN);
    SinCos(AngTop, SinP, CosP);
    glTexCoord2f(0.5, 0.5);
    if DoReverse then
      glNormal3f(0, -1, 0)
    else
      glNormal3f(0, 1, 0);

    if FTopCap = ctCenter then
      glVertex3f(0, 0, 0)
    else
    begin { FTopCap = ctFlat }
      if (Sign(SinP) = 1) or (tc1 = VCurve) then
        SinPc1 := Power(SinP, VCurve)
      else
        SinPc1 := -Power(-SinP, VCurve);
      glVertex3f(0, SinPc1*Radius, 0);

      N1 := YVector;
      if DoReverse then
        N1.Y := -N1.Y;
    end; { FTopCap = ctFlat }

    //  v1.Y := SinP;
    if (Sign(SinP) = 1) or (tc1 = VCurve) then
      SinPc1 := Power(SinP, VCurve)
    else
      SinPc1 := -Power(-SinP, VCurve);
    v1.Y := SinPc1;

    Theta := AngStart;

    for i := 0 to FSlices do
    begin
      SinCos(Theta, SinT, CosT);
      //    v1.X := CosP * SinT;
      if (Sign(CosP) = 1) or (tc1 = VCurve) then
        CosPc1 := Power(CosP, VCurve)
      else
        CosPc1 := -Power(-CosP, VCurve);
      if (Sign(SinT) = 1) or (tc2 = HCurve) then
        SinTc2 := Power(SinT, HCurve)
      else
        SinTc2 := -Power(-SinT, HCurve);
      v1.X := CosPc1 * SinTc2;
      //    v1.Z := CosP * CosT;
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
      //    xglTexCoord2f(SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
      glTexCoord2f(SinTc2 * 0.5 + 0.5, CosTc2 * 0.5 + 0.5);
      glNormal3fv(@N1);
      vs := v1;
      ScaleVector(vs, Radius);
      glVertex3fv(@vs);
      Theta := Theta + StepH;
    end;
    glEnd;
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
    glBegin(GL_TRIANGLE_STRIP);
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
      V2.X := CosPc1 * SinTc2;

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
      V2.Z := CosPc1 * CosTc2;
      uTexCoord := i * uTexFactor;
      glTexCoord2f(uTexCoord, vTexCoord0);
      if DoReverse then
      begin
        N1 := VectorNegate(v1);
        glNormal3fv(@N1);
      end
      else
        glNormal3fv(@v1);
      vs := v1;
      ScaleVector(vs, Radius);
      glVertex3fv(@vs);

      glTexCoord2f(uTexCoord, vTexCoord1);
      if DoReverse then
      begin
        N1 := VectorNegate(V2);
        glNormal3fv(@N1);
      end
      else
        glNormal3fv(@v2);
      vs := v2;
      ScaleVector(vs, Radius);
      glVertex3fv(@vs);
      Theta := Theta + StepH;
    end;
    glEnd;
    Phi := Phi2;
    Phi2 := Phi2 - StepV;
  end;

  // bottom cap
  if (FBottom > -90) and (FBottomCap in [ctCenter, ctFlat]) then
  begin
    glBegin(GL_TRIANGLE_FAN);
    SinCos(AngBottom, SinP, CosP);
    glTexCoord2f(0.5, 0.5);
    if DoReverse then
      glNormal3f(0, 1, 0)
    else
      glNormal3f(0, -1, 0);
    if FBottomCap = ctCenter then
      glVertex3f(0, 0, 0)
    else
    begin { FTopCap = ctFlat }
      if (Sign(SinP) = 1) or (tc1 = VCurve) then
        SinPc1 := Power(SinP, VCurve)
      else
        SinPc1 := -Power(-SinP, VCurve);
      glVertex3f(0, SinPc1*Radius, 0);

      if DoReverse then
        MakeVector(N1, 0, -1, 0)
      else
        N1 := YVector;
    end;
    //  v1.Y := SinP;
    if (Sign(SinP) = 1) or (tc1 = VCurve) then
      SinPc1 := Power(SinP, VCurve)
    else
      SinPc1 := -Power(-SinP, VCurve);
    v1.Y := SinPc1;

    Theta := AngStop;
    for i := 0 to FSlices do
    begin
      SinCos(Theta, SinT, CosT);
      //    v1.X := CosP * SinT;
      if (Sign(CosP) = 1) or (tc1 = VCurve) then
        CosPc1 := Power(CosP, VCurve)
      else
        CosPc1 := -Power(-CosP, VCurve);
      if (Sign(SinT) = 1) or (tc2 = HCurve) then
        SinTc2 := Power(SinT, HCurve)
      else
        SinTc2 := -Power(-SinT, HCurve);
      v1.X := CosPc1 * SinTc2;

      //    v1.Z := CosP * CosT;
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
        glNormal3fv(@N1);
      end;
      //    xglTexCoord2f(SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
      glTexCoord2f(SinTc2 * 0.5 + 0.5, CosTc2 * 0.5 + 0.5);
      vs := v1;
      ScaleVector(vs, Radius);
      glVertex3fv(@vs);
      Theta := Theta - StepH;
    end;
    glEnd;
  end;
  if DoReverse then
    rci.gxStates.InvertFrontFace;
end;

// This will probably not work, karamba
// RayCastSphereIntersect -> RayCastSuperellipsoidIntersect ??????
function TgxSuperellipsoid.RayCastIntersect(const rayStart, rayVector: TVector4f;
  intersectPoint: PVector4f = nil; intersectNormal: PVector4f = nil): Boolean;
var
  i1, i2: TVector4f;
  localStart, localVector: TVector4f;
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

// This will probably not work;
function TgxSuperellipsoid.GenerateSilhouette(const silhouetteParameters
  : TgxSilhouetteParameters): TgxSilhouette;
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
  Result := TgxSilhouette.Create;
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

procedure TgxSuperellipsoid.SetBottom(aValue: TAngleLimit1);
begin
  if FBottom <> aValue then
  begin
    FBottom := aValue;
    StructureChanged;
  end;
end;

procedure TgxSuperellipsoid.SetBottomCap(aValue: TgxCapType);
begin
  if FBottomCap <> aValue then
  begin
    FBottomCap := aValue;
    StructureChanged;
  end;
end;


procedure TgxSuperellipsoid.SetHCurve(const aValue: Single);
begin
  if aValue <> FHCurve then
  begin
    FHCurve := aValue;
    StructureChanged;
  end;
end;

procedure TgxSuperellipsoid.SetRadius(const aValue: Single);
begin
  if aValue <> FRadius then
  begin
    FRadius := aValue;
    StructureChanged;
  end;
end;

procedure TgxSuperellipsoid.SetSlices(aValue: Integer);
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

procedure TgxSuperellipsoid.SetStacks(aValue: GLint);
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

procedure TgxSuperellipsoid.SetStart(aValue: TAngleLimit2);
begin
  if FStart <> aValue then
  begin
    Assert(aValue <= FStop);
    FStart := aValue;
    StructureChanged;
  end;
end;

procedure TgxSuperellipsoid.SetStop(aValue: TAngleLimit2);
begin
  if FStop <> aValue then
  begin
    Assert(aValue >= FStart);
    FStop := aValue;
    StructureChanged;
  end;
end;

procedure TgxSuperellipsoid.SetTop(aValue: TAngleLimit1);
begin
  if FTop <> aValue then
  begin
    FTop := aValue;
    StructureChanged;
  end;
end;

procedure TgxSuperellipsoid.SetTopCap(aValue: TgxCapType);
begin
  if FTopCap <> aValue then
  begin
    FTopCap := aValue;
    StructureChanged;
  end;
end;

procedure TgxSuperellipsoid.SetVCurve(const aValue: Single);
begin
  if aValue <> FVCurve then
  begin
    FVCurve := aValue;
    StructureChanged;
  end;
end;

procedure TgxSuperellipsoid.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TgxSuperellipsoid) then
  begin
    FRadius := TgxSuperellipsoid(Source).FRadius;
    FSlices := TgxSuperellipsoid(Source).FSlices;
    FStacks := TgxSuperellipsoid(Source).FStacks;
    FBottom := TgxSuperellipsoid(Source).FBottom;
    FTop := TgxSuperellipsoid(Source).FTop;
    FStart := TgxSuperellipsoid(Source).FStart;
    FStop := TgxSuperellipsoid(Source).FStop;
  end;
  inherited Assign(Source);
end;

function TgxSuperellipsoid.AxisAlignedDimensionsUnscaled: TVector4f;
begin
  Result.X := Abs(FRadius);
  Result.Y := Result.X;
  Result.Z := Result.X;
  Result.W := 0;
end;

// -------------------------------------------------------------
initialization
// -------------------------------------------------------------

RegisterClasses([TgxSphere, TgxCube, TgxPlane, TgxSprite, TgxPoints,
  TgxDummyCube, TgxLines, TgxSuperellipsoid]);

end.
