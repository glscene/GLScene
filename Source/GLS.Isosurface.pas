//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.Isosurface;
(*
  Polygonising a scalar field by construction of isosurfaces
  Algorithms
  ----------
  Marching Cubes
  - Exploits a coarser Mesh then Marching Tetrahedra but produces less triangles
  - based on "Marching Cubes: A High Resolution 3D Surface
  Construction Algorithm" by W.E.Lorensen and H.E.Cline
  - patent free since 2005

  Marching Tetrahedra
  - Finer Mesh, better feature preservation
  - based on "A new tetrahedral tesselation scheme for isosurface generation"
  by S.L.Chan and E.O.Purisima
  - patent free

  Lookuptables
  - by Paul Bourke (http://paulbourke.net/geometry/polygonise/)

  Overall
  - Simple Data Structures to store Mesh. Vertices are calculated and stored twice
  or even more often.
*)
interface

{$I GLScene.inc}

// uncomment next line to memorize vertex Density value to further use
// (i.e. mesh color generation)
{.$Define UseDensity}

uses
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.Mesh,
  GLS.VectorFileObjects,
  GLS.VectorTypes,
  GLS.VectorTypesExt;

const
  ALLOC_SIZE = 65536;

type
  TSingle3DArray = array of array of array of Single;
  TVertexArray = array of TVector3f;
  TIntegerArray = array of Integer;
  TGLMarchingCube = class(TObject)
  private
    FIsoValue: TxScalarValue;
    // sliceSize:Longword;
    PVertsX: PIntegerArray;
    PVertsY: PIntegerArray;
    PVertsZ: PIntegerArray;
    _Nverts: Integer;
    _Ntrigs: Integer;
    _Sverts: Integer;
    _Strigs: Integer;
    PVertices: PxVertexArray;
    PTriangles: PxTriangleArray;
    _i, _j, _k: Longword;
    _Cube: array [0 .. 7] of TxVoxel;
    _lut_entry: Byte;
    // _case:Byte;
    // _config:Byte;
    // _subconfig:Byte;
    procedure Init_temps;
    procedure Init_all;
    procedure Init_space;
    procedure Clean_temps;
    procedure Clean_all(keepFacets: Boolean = False);
    procedure Clean_space;
    procedure Test_vertex_addiction;
  protected
    FOriginalMC: Boolean; // now only original MC is implemented
    FSizeX: Integer;
    FSizeY: Integer;
    FSizeZ: Integer;
    FxMin: Single;
    FxMax: Single;
    FyMin: Single;
    FyMax: Single;
    FzMin: Single;
    FzMax: Single;
    FStepX: Single;
    FStepY: Single;
    FStepZ: Single;
    VoxelData: PxVoxelData;
    procedure Process_cube;
    (* function test_face(face:byte):Boolean;
      function test_interior(s:Byte):boolean *)
    procedure Compute_Intersection_Points;
    procedure Add_Triangle(trig: array of Integer; N: Byte; v12: Integer = -1);
    function Add_x_vertex: Integer;
    function Add_y_vertex: Integer;
    function Add_z_vertex: Integer;
    function Add_c_vertex: Integer;
    function Get_x_grad(i, j, k: Integer): Single;
    function Get_y_grad(i, j, k: Integer): Single;
    function Get_z_grad(i, j, k: Integer): Single;
    function Get_x_vert(i, j, k: Integer): Integer;
    function Get_y_vert(i, j, k: Integer): Integer;
    function Get_z_vert(i, j, k: Integer): Integer;
    procedure Set_x_vert(a_val, i, j, k: Integer);
    procedure Set_y_vert(a_val, i, j, k: Integer);
    procedure Set_z_vert(a_val, i, j, k: Integer);
    function GetVoxelValue(i, j, k: Integer): TxScalarValue;
    procedure SetVoxelValue(i, j, k: Integer; HfValue: TxScalarValue);
    function GetVoxelData(i, j, k: Integer): TxVoxel;
    function Voxel(i, j, k: Integer): PxVoxel;
    function calc_u(v1, v2: Single): Extended; virtual;
  public
    ScalarField: TxScalarField;
    constructor Create; overload; virtual;
    constructor Create(SizeX, SizeY, SizeZ: Integer;
      AIsoValue: TxScalarValue = 0.0; xMin: Single = -0.5; xMax: Single = 0.5;
      yMin: Single = -0.5; yMax: Single = 0.5; zMin: Single = -0.5;
      zMax: Single = 0.5); overload; virtual;
    procedure ReDim(ASizeX, ASizeY, ASizeZ: Integer;
      xMin, xMax, yMin, yMax, zMin, zMax: Single); virtual;
    destructor Destroy; override;
    procedure Run; overload;
    procedure Run(IsoValue: TxScalarValue); overload;
    function Internal(AValue: TxScalarValue): Boolean; virtual;
    procedure FillVoxelData; overload; virtual;
    procedure FillVoxelData(AIsoValue: TxScalarValue;
      AScalarField: TxScalarField = nil); overload; virtual;
    procedure FillVoxelData(AIsoValue: TxScalarValue;
      AScalarField: TxScalarFieldInt); overload; virtual;
    procedure CalcVertices(Vertices: TGLVertexList; Alpha: Single = 1);
    procedure CalcMeshObject(AMeshObject: TMeshObject; Alpha: Single = 1);
    property IsoValue: TxScalarValue read FIsoValue write FIsoValue;
    // TODO SetIsoValue to Run
  end;

  (* 3D isosurface extractor class. This class allows to calculate and exctract
    isosurfaces from scalar field voxel models using a given isovalue *)
  TIsoSurfaceExtractor = class(TObject)
  private
    Data: TSingle3DArray;
    Dimensions: array ['x' .. 'z'] of Integer;
    // Build Index depending on whether the edges are outside or inside the surface
    function BuildIndex(var ADatavals: array of Single; Isovalue: Single): word;
    function Interpolate(const V0, V1: TAffineVector;
      var Val0, Val1, Isovalue: Single; isPolished: boolean): TVertex;
  public
    constructor Create(); overload;
    constructor Create(Xdim, Ydim, Zdim: Integer;
      var AData: TSingle3DArray); overload;
    destructor Destroy(); override;
    procedure AssignData(Xdim, Ydim, Zdim: Integer; var AData: TSingle3DArray);
    // Launch Marching Cubes
    procedure MarchingCubes(Isovalue: Single; out Vertices: TVertexArray;
      out Triangles: TIntegerArray; isPolished: boolean);
    // Launch Marching Tetrahedra
    procedure MarchingTetrahedra(Isovalue: Single; out Vertices: TVertexArray;
      out Triangles: TIntegerArray; isPolished: boolean);
  end;

// Sphere surface
function SFSphere(X, Y, Z: Extended): TxScalarValue;
// Minkowski space (http://mathworld.wolfram.com)
function SFMinkowski(X, Y, Z: Extended): TxScalarValue;
// Klein Bottle (http://mathworld.wolfram.com)
function SFKleinBottle(X, Y, Z: Extended): TxScalarValue;
// Chmutov-surface-1 (http://mathworld.wolfram.com)
function SFChmutov1(X, Y, Z: Extended): TxScalarValue;
// Chmutov-surface-2 (http://mathworld.wolfram.com)
function SFChmutov2(X, Y, Z: Extended): TxScalarValue;
// Toroidal surface (phantasy!)
function SFToroidal(X, Y, Z: Extended): TxScalarValue;
// Double torus Surface (phantasy!)
function SFDoubleTorus(X, Y, Z: Extended): TxScalarValue;

const
  DemoScalarField: array [0 .. 6] of
  record
    // xMin, xMax, yMin, yMax, zMin, zMax:Single; // default -0.5..0.5
    ScalarField: TxScalarField;
    IsoValue: TxScalarValue
  end = ((ScalarField: SFSphere; IsoValue: 0.3), (ScalarField: SFMinkowski;
  IsoValue: 0.0), (ScalarField: SFKleinBottle; IsoValue: 0.0),
  (ScalarField: SFChmutov1; IsoValue: 3.0), (ScalarField: SFChmutov2;
  IsoValue: 3.0), (ScalarField: SFToroidal; IsoValue: 3.0),
  (ScalarField: SFDoubleTorus; IsoValue: 0.015));

// -------------------------------------------------------------------------
implementation
// -------------------------------------------------------------------------

const
  // Classic Cases for Marching Cube TriTable
  //
  (*
        4----4------5
       /|          /|
      7 |         5 |
     /  |        /  |
    7-----6----6    |
    |   8       |   9
    |   |       |   |
    |   0----0--|---1
    11 /        10 /
    | 3         | 1
    |/          |/
    3-----2-----2
  *)

  MC_TRITABLE: array [0 .. 255, 0 .. 15] of Integer = (
(*   0:                          *)  ( -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*   1: 0,                       *)  (  0,  8,  3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*   2:    1,                    *)  (  0,  1,  9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*   3: 0, 1,                    *)  (  1,  8,  3,  9,  8,  1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*   4:       2,                 *)  (  1,  2, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*   5: 0,    2,                 *)  (  0,  8,  3,  1,  2, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*   6:    1, 2,                 *)  (  9,  2, 10,  0,  2,  9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*   7: 0, 1, 2,                 *)  (  2,  8,  3,  2, 10,  8, 10,  9,  8, -1, -1, -1, -1, -1, -1, -1 ),
(*   8:          3,              *)  (  3, 11,  2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*   9: 0,       3,              *)  (  0, 11,  2,  8, 11,  0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  10:    1,    3,              *)  (  1,  9,  0,  2,  3, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  11: 0, 1,    3,              *)  (  1, 11,  2,  1,  9, 11,  9,  8, 11, -1, -1, -1, -1, -1, -1, -1 ),
(*  12:       2, 3,              *)  (  3, 10,  1, 11, 10,  3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  13: 0,    2, 3,              *)  (  0, 10,  1,  0,  8, 10,  8, 11, 10, -1, -1, -1, -1, -1, -1, -1 ),
(*  14:    1, 2, 3,              *)  (  3,  9,  0,  3, 11,  9, 11, 10,  9, -1, -1, -1, -1, -1, -1, -1 ),
(*  15: 0, 1, 2, 3,              *)  (  9,  8, 10, 10,  8, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  16:             4,           *)  (  4,  7,  8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  17: 0,          4,           *)  (  4,  3,  0,  7,  3,  4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  18:    1,       4,           *)  (  0,  1,  9,  8,  4,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  19: 0, 1,       4,           *)  (  4,  1,  9,  4,  7,  1,  7,  3,  1, -1, -1, -1, -1, -1, -1, -1 ),
(*  20:       2,    4,           *)  (  1,  2, 10,  8,  4,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  21: 0,    2,    4,           *)  (  3,  4,  7,  3,  0,  4,  1,  2, 10, -1, -1, -1, -1, -1, -1, -1 ),
(*  22:    1, 2,    4,           *)  (  9,  2, 10,  9,  0,  2,  8,  4,  7, -1, -1, -1, -1, -1, -1, -1 ),
(*  23: 0, 1, 2,    4,           *)  (  2, 10,  9,  2,  9,  7,  2,  7,  3,  7,  9,  4, -1, -1, -1, -1 ),
(*  24:          3, 4,           *)  (  8,  4,  7,  3, 11,  2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  25: 0,       3, 4,           *)  ( 11,  4,  7, 11,  2,  4,  2,  0,  4, -1, -1, -1, -1, -1, -1, -1 ),
(*  26:    1,    3, 4,           *)  (  9,  0,  1,  8,  4,  7,  2,  3, 11, -1, -1, -1, -1, -1, -1, -1 ),
(*  27: 0, 1,    3, 4,           *)  (  4,  7, 11,  9,  4, 11,  9, 11,  2,  9,  2,  1, -1, -1, -1, -1 ),
(*  28:       2, 3, 4,           *)  (  3, 10,  1,  3, 11, 10,  7,  8,  4, -1, -1, -1, -1, -1, -1, -1 ),
(*  29: 0,    2, 3, 4,           *)  (  1, 11, 10,  1,  4, 11,  1,  0,  4,  7, 11,  4, -1, -1, -1, -1 ),
(*  30:    1, 2, 3, 4,           *)  (  4,  7,  8,  9,  0, 11,  9, 11, 10, 11,  0,  3, -1, -1, -1, -1 ),
(*  31: 0, 1, 2, 3, 4,           *)  (  4,  7, 11,  4, 11,  9,  9, 11, 10, -1, -1, -1, -1, -1, -1, -1 ),
(*  32:                5,        *)  (  9,  5,  4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  33: 0,             5,        *)  (  9,  5,  4,  0,  8,  3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  34:    1,          5,        *)  (  0,  5,  4,  1,  5,  0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  35: 0, 1,          5,        *)  (  8,  5,  4,  8,  3,  5,  3,  1,  5, -1, -1, -1, -1, -1, -1, -1 ),
(*  36:       2,       5,        *)  (  1,  2, 10,  9,  5,  4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  37: 0,    2,       5,        *)  (  3,  0,  8,  1,  2, 10,  4,  9,  5, -1, -1, -1, -1, -1, -1, -1 ),
(*  38:    1, 2,       5,        *)  (  5,  2, 10,  5,  4,  2,  4,  0,  2, -1, -1, -1, -1, -1, -1, -1 ),
(*  39: 0, 1, 2,       5,        *)  (  2, 10,  5,  3,  2,  5,  3,  5,  4,  3,  4,  8, -1, -1, -1, -1 ),
(*  40:          3,    5,        *)  (  9,  5,  4,  2,  3, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  41: 0,       3,    5,        *)  (  0, 11,  2,  0,  8, 11,  4,  9,  5, -1, -1, -1, -1, -1, -1, -1 ),
(*  42:    1,    3,    5,        *)  (  0,  5,  4,  0,  1,  5,  2,  3, 11, -1, -1, -1, -1, -1, -1, -1 ),
(*  43: 0, 1,    3,    5,        *)  (  2,  1,  5,  2,  5,  8,  2,  8, 11,  4,  8,  5, -1, -1, -1, -1 ),
(*  44:       2, 3,    5,        *)  ( 10,  3, 11, 10,  1,  3,  9,  5,  4, -1, -1, -1, -1, -1, -1, -1 ),
(*  45: 0,    2, 3,    5,        *)  (  4,  9,  5,  0,  8,  1,  8, 10,  1,  8, 11, 10, -1, -1, -1, -1 ),
(*  46:    1, 2, 3,    5,        *)  (  5,  4,  0,  5,  0, 11,  5, 11, 10, 11,  0,  3, -1, -1, -1, -1 ),
(*  47: 0, 1, 2, 3,    5,        *)  (  5,  4,  8,  5,  8, 10, 10,  8, 11, -1, -1, -1, -1, -1, -1, -1 ),
(*  48:             4, 5,        *)  (  9,  7,  8,  5,  7,  9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  49: 0,          4, 5,        *)  (  9,  3,  0,  9,  5,  3,  5,  7,  3, -1, -1, -1, -1, -1, -1, -1 ),
(*  50:    1,       4, 5,        *)  (  0,  7,  8,  0,  1,  7,  1,  5,  7, -1, -1, -1, -1, -1, -1, -1 ),
(*  51: 0, 1,       4, 5,        *)  (  1,  5,  3,  3,  5,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  52:       2,    4, 5,        *)  (  9,  7,  8,  9,  5,  7, 10,  1,  2, -1, -1, -1, -1, -1, -1, -1 ),
(*  53: 0,    2,    4, 5,        *)  ( 10,  1,  2,  9,  5,  0,  5,  3,  0,  5,  7,  3, -1, -1, -1, -1 ),
(*  54:    1, 2,    4, 5,        *)  (  8,  0,  2,  8,  2,  5,  8,  5,  7, 10,  5,  2, -1, -1, -1, -1 ),
(*  55: 0, 1, 2,    4, 5,        *)  (  2, 10,  5,  2,  5,  3,  3,  5,  7, -1, -1, -1, -1, -1, -1, -1 ),
(*  56:          3, 4, 5,        *)  (  7,  9,  5,  7,  8,  9,  3, 11,  2, -1, -1, -1, -1, -1, -1, -1 ),
(*  57: 0,       3, 4, 5,        *)  (  9,  5,  7,  9,  7,  2,  9,  2,  0,  2,  7, 11, -1, -1, -1, -1 ),
(*  58:    1,    3, 4, 5,        *)  (  2,  3, 11,  0,  1,  8,  1,  7,  8,  1,  5,  7, -1, -1, -1, -1 ),
(*  59: 0, 1,    3, 4, 5,        *)  ( 11,  2,  1, 11,  1,  7,  7,  1,  5, -1, -1, -1, -1, -1, -1, -1 ),
(*  60:       2, 3, 4, 5,        *)  (  9,  5,  8,  8,  5,  7, 10,  1,  3, 10,  3, 11, -1, -1, -1, -1 ),
(*  61: 0,    2, 3, 4, 5,        *)  (  5,  7,  0,  5,  0,  9,  7, 11,  0,  1,  0, 10, 11, 10,  0, -1 ),
(*  62:    1, 2, 3, 4, 5,        *)  ( 11, 10,  0, 11,  0,  3, 10,  5,  0,  8,  0,  7,  5,  7,  0, -1 ),
(*  63: 0, 1, 2, 3, 4, 5,        *)  ( 11, 10,  5,  7, 11,  5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  64:                   6,     *)  ( 10,  6,  5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  65: 0,                6,     *)  (  0,  8,  3,  5, 10,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  66:    1,             6,     *)  (  9,  0,  1,  5, 10,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  67: 0, 1,             6,     *)  (  1,  8,  3,  1,  9,  8,  5, 10,  6, -1, -1, -1, -1, -1, -1, -1 ),
(*  68:       2,          6,     *)  (  1,  6,  5,  2,  6,  1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  69: 0,    2,          6,     *)  (  1,  6,  5,  1,  2,  6,  3,  0,  8, -1, -1, -1, -1, -1, -1, -1 ),
(*  70:    1, 2,          6,     *)  (  9,  6,  5,  9,  0,  6,  0,  2,  6, -1, -1, -1, -1, -1, -1, -1 ),
(*  71: 0, 1, 2,          6,     *)  (  5,  9,  8,  5,  8,  2,  5,  2,  6,  3,  2,  8, -1, -1, -1, -1 ),
(*  72:          3,       6,     *)  (  2,  3, 11, 10,  6,  5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  73: 0,       3,       6,     *)  ( 11,  0,  8, 11,  2,  0, 10,  6,  5, -1, -1, -1, -1, -1, -1, -1 ),
(*  74:    1,    3,       6,     *)  (  0,  1,  9,  2,  3, 11,  5, 10,  6, -1, -1, -1, -1, -1, -1, -1 ),
(*  75: 0, 1,    3,       6,     *)  (  5, 10,  6,  1,  9,  2,  9, 11,  2,  9,  8, 11, -1, -1, -1, -1 ),
(*  76:       2, 3,       6,     *)  (  6,  3, 11,  6,  5,  3,  5,  1,  3, -1, -1, -1, -1, -1, -1, -1 ),
(*  77: 0,    2, 3,       6,     *)  (  0,  8, 11,  0, 11,  5,  0,  5,  1,  5, 11,  6, -1, -1, -1, -1 ),
(*  78:    1, 2, 3,       6,     *)  (  3, 11,  6,  0,  3,  6,  0,  6,  5,  0,  5,  9, -1, -1, -1, -1 ),
(*  79: 0, 1, 2, 3,       6,     *)  (  6,  5,  9,  6,  9, 11, 11,  9,  8, -1, -1, -1, -1, -1, -1, -1 ),
(*  80:             4,    6,     *)  (  5, 10,  6,  4,  7,  8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  81: 0,          4,    6,     *)  (  4,  3,  0,  4,  7,  3,  6,  5, 10, -1, -1, -1, -1, -1, -1, -1 ),
(*  82:    1,       4,    6,     *)  (  1,  9,  0,  5, 10,  6,  8,  4,  7, -1, -1, -1, -1, -1, -1, -1 ),
(*  83: 0, 1,       4,    6,     *)  ( 10,  6,  5,  1,  9,  7,  1,  7,  3,  7,  9,  4, -1, -1, -1, -1 ),
(*  84:       2,    4,    6,     *)  (  6,  1,  2,  6,  5,  1,  4,  7,  8, -1, -1, -1, -1, -1, -1, -1 ),
(*  85: 0,    2,    4,    6,     *)  (  1,  2,  5,  5,  2,  6,  3,  0,  4,  3,  4,  7, -1, -1, -1, -1 ),
(*  86:    1, 2,    4,    6,     *)  (  8,  4,  7,  9,  0,  5,  0,  6,  5,  0,  2,  6, -1, -1, -1, -1 ),
(*  87: 0, 1, 2,    4,    6,     *)  (  7,  3,  9,  7,  9,  4,  3,  2,  9,  5,  9,  6,  2,  6,  9, -1 ),
(*  88:          3, 4,    6,     *)  (  3, 11,  2,  7,  8,  4, 10,  6,  5, -1, -1, -1, -1, -1, -1, -1 ),
(*  89: 0,       3, 4,    6,     *)  (  5, 10,  6,  4,  7,  2,  4,  2,  0,  2,  7, 11, -1, -1, -1, -1 ),
(*  90:    1,    3, 4,    6,     *)  (  0,  1,  9,  4,  7,  8,  2,  3, 11,  5, 10,  6, -1, -1, -1, -1 ),
(*  91: 0, 1,    3, 4,    6,     *)  (  9,  2,  1,  9, 11,  2,  9,  4, 11,  7, 11,  4,  5, 10,  6, -1 ),
(*  92:       2, 3, 4,    6,     *)  (  8,  4,  7,  3, 11,  5,  3,  5,  1,  5, 11,  6, -1, -1, -1, -1 ),
(*  93: 0,    2, 3, 4,    6,     *)  (  5,  1, 11,  5, 11,  6,  1,  0, 11,  7, 11,  4,  0,  4, 11, -1 ),
(*  94:    1, 2, 3, 4,    6,     *)  (  0,  5,  9,  0,  6,  5,  0,  3,  6, 11,  6,  3,  8,  4,  7, -1 ),
(*  95: 0, 1, 2, 3, 4,    6,     *)  (  6,  5,  9,  6,  9, 11,  4,  7,  9,  7, 11,  9, -1, -1, -1, -1 ),
(*  96:                5, 6,     *)  ( 10,  4,  9,  6,  4, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(*  97: 0,             5, 6,     *)  (  4, 10,  6,  4,  9, 10,  0,  8,  3, -1, -1, -1, -1, -1, -1, -1 ),
(*  98:    1,          5, 6,     *)  ( 10,  0,  1, 10,  6,  0,  6,  4,  0, -1, -1, -1, -1, -1, -1, -1 ),
(*  99: 0, 1,          5, 6,     *)  (  8,  3,  1,  8,  1,  6,  8,  6,  4,  6,  1, 10, -1, -1, -1, -1 ),
(* 100:       2,       5, 6,     *)  (  1,  4,  9,  1,  2,  4,  2,  6,  4, -1, -1, -1, -1, -1, -1, -1 ),
(* 101: 0,    2,       5, 6,     *)  (  3,  0,  8,  1,  2,  9,  2,  4,  9,  2,  6,  4, -1, -1, -1, -1 ),
(* 102:    1, 2,       5, 6,     *)  (  0,  2,  4,  4,  2,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 103: 0, 1, 2,       5, 6,     *)  (  8,  3,  2,  8,  2,  4,  4,  2,  6, -1, -1, -1, -1, -1, -1, -1 ),
(* 104:          3,    5, 6,     *)  ( 10,  4,  9, 10,  6,  4, 11,  2,  3, -1, -1, -1, -1, -1, -1, -1 ),
(* 105: 0,       3,    5, 6,     *)  (  0,  8,  2,  2,  8, 11,  4,  9, 10,  4, 10,  6, -1, -1, -1, -1 ),
(* 106:    1,    3,    5, 6,     *)  (  3, 11,  2,  0,  1,  6,  0,  6,  4,  6,  1, 10, -1, -1, -1, -1 ),
(* 107: 0, 1,    3,    5, 6,     *)  (  6,  4,  1,  6,  1, 10,  4,  8,  1,  2,  1, 11,  8, 11,  1, -1 ),
(* 108:       2, 3,    5, 6,     *)  (  9,  6,  4,  9,  3,  6,  9,  1,  3, 11,  6,  3, -1, -1, -1, -1 ),
(* 109: 0,    2, 3,    5, 6,     *)  (  8, 11,  1,  8,  1,  0, 11,  6,  1,  9,  1,  4,  6,  4,  1, -1 ),
(* 110:    1, 2, 3,    5, 6,     *)  (  3, 11,  6,  3,  6,  0,  0,  6,  4, -1, -1, -1, -1, -1, -1, -1 ),
(* 111: 0, 1, 2, 3,    5, 6,     *)  (  6,  4,  8, 11,  6,  8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 112:             4, 5, 6,     *)  (  7, 10,  6,  7,  8, 10,  8,  9, 10, -1, -1, -1, -1, -1, -1, -1 ),
(* 113: 0,          4, 5, 6,     *)  (  0,  7,  3,  0, 10,  7,  0,  9, 10,  6,  7, 10, -1, -1, -1, -1 ),
(* 114:    1,       4, 5, 6,     *)  ( 10,  6,  7,  1, 10,  7,  1,  7,  8,  1,  8,  0, -1, -1, -1, -1 ),
(* 115: 0, 1,       4, 5, 6,     *)  ( 10,  6,  7, 10,  7,  1,  1,  7,  3, -1, -1, -1, -1, -1, -1, -1 ),
(* 116:       2,    4, 5, 6,     *)  (  1,  2,  6,  1,  6,  8,  1,  8,  9,  8,  6,  7, -1, -1, -1, -1 ),
(* 117: 0,    2,    4, 5, 6,     *)  (  2,  6,  9,  2,  9,  1,  6,  7,  9,  0,  9,  3,  7,  3,  9, -1 ),
(* 118:    1, 2,    4, 5, 6,     *)  (  7,  8,  0,  7,  0,  6,  6,  0,  2, -1, -1, -1, -1, -1, -1, -1 ),
(* 119: 0, 1, 2,    4, 5, 6,     *)  (  7,  3,  2,  6,  7,  2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 120:          3, 4, 5, 6,     *)  (  2,  3, 11, 10,  6,  8, 10,  8,  9,  8,  6,  7, -1, -1, -1, -1 ),
(* 121: 0,       3, 4, 5, 6,     *)  (  2,  0,  7,  2,  7, 11,  0,  9,  7,  6,  7, 10,  9, 10,  7, -1 ),
(* 122:    1,    3, 4, 5, 6,     *)  (  1,  8,  0,  1,  7,  8,  1, 10,  7,  6,  7, 10,  2,  3, 11, -1 ),
(* 123: 0, 1,    3, 4, 5, 6,     *)  ( 11,  2,  1, 11,  1,  7, 10,  6,  1,  6,  7,  1, -1, -1, -1, -1 ),
(* 124:       2, 3, 4, 5, 6,     *)  (  8,  9,  6,  8,  6,  7,  9,  1,  6, 11,  6,  3,  1,  3,  6, -1 ),
(* 125: 0,    2, 3, 4, 5, 6,     *)  (  0,  9,  1, 11,  6,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 126:    1, 2, 3, 4, 5, 6,     *)  (  7,  8,  0,  7,  0,  6,  3, 11,  0, 11,  6,  0, -1, -1, -1, -1 ),
(* 127: 0, 1, 2, 3, 4, 5, 6,     *)  (  7, 11,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 128:                      7,  *)  (  7,  6, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 129: 0,                   7,  *)  (  3,  0,  8, 11,  7,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 130:    1,                7,  *)  (  0,  1,  9, 11,  7,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 131: 0, 1,                7,  *)  (  8,  1,  9,  8,  3,  1, 11,  7,  6, -1, -1, -1, -1, -1, -1, -1 ),
(* 132:       2,             7,  *)  ( 10,  1,  2,  6, 11,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 133: 0,    2,             7,  *)  (  1,  2, 10,  3,  0,  8,  6, 11,  7, -1, -1, -1, -1, -1, -1, -1 ),
(* 134:    1, 2,             7,  *)  (  2,  9,  0,  2, 10,  9,  6, 11,  7, -1, -1, -1, -1, -1, -1, -1 ),
(* 135: 0, 1, 2,             7,  *)  (  6, 11,  7,  2, 10,  3, 10,  8,  3, 10,  9,  8, -1, -1, -1, -1 ),
(* 136:          3,          7,  *)  (  7,  2,  3,  6,  2,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 137: 0,       3,          7,  *)  (  7,  0,  8,  7,  6,  0,  6,  2,  0, -1, -1, -1, -1, -1, -1, -1 ),
(* 138:    1,    3,          7,  *)  (  2,  7,  6,  2,  3,  7,  0,  1,  9, -1, -1, -1, -1, -1, -1, -1 ),
(* 139: 0, 1,    3,          7,  *)  (  1,  6,  2,  1,  8,  6,  1,  9,  8,  8,  7,  6, -1, -1, -1, -1 ),
(* 140:       2, 3,          7,  *)  ( 10,  7,  6, 10,  1,  7,  1,  3,  7, -1, -1, -1, -1, -1, -1, -1 ),
(* 141: 0,    2, 3,          7,  *)  ( 10,  7,  6,  1,  7, 10,  1,  8,  7,  1,  0,  8, -1, -1, -1, -1 ),
(* 142:    1, 2, 3,          7,  *)  (  0,  3,  7,  0,  7, 10,  0, 10,  9,  6, 10,  7, -1, -1, -1, -1 ),
(* 143: 0, 1, 2, 3,          7,  *)  (  7,  6, 10,  7, 10,  8,  8, 10,  9, -1, -1, -1, -1, -1, -1, -1 ),
(* 144:             4,       7,  *)  (  6,  8,  4, 11,  8,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 145: 0,          4,       7,  *)  (  3,  6, 11,  3,  0,  6,  0,  4,  6, -1, -1, -1, -1, -1, -1, -1 ),
(* 146:    1,       4,       7,  *)  (  8,  6, 11,  8,  4,  6,  9,  0,  1, -1, -1, -1, -1, -1, -1, -1 ),
(* 147: 0, 1,       4,       7,  *)  (  9,  4,  6,  9,  6,  3,  9,  3,  1, 11,  3,  6, -1, -1, -1, -1 ),
(* 148:       2,    4,       7,  *)  (  6,  8,  4,  6, 11,  8,  2, 10,  1, -1, -1, -1, -1, -1, -1, -1 ),
(* 149: 0,    2,    4,       7,  *)  (  1,  2, 10,  3,  0, 11,  0,  6, 11,  0,  4,  6, -1, -1, -1, -1 ),
(* 150:    1, 2,    4,       7,  *)  (  4, 11,  8,  4,  6, 11,  0,  2,  9,  2, 10,  9, -1, -1, -1, -1 ),
(* 151: 0, 1, 2,    4,       7,  *)  ( 10,  9,  3, 10,  3,  2,  9,  4,  3, 11,  3,  6,  4,  6,  3, -1 ),
(* 152:          3, 4,       7,  *)  (  8,  2,  3,  8,  4,  2,  4,  6,  2, -1, -1, -1, -1, -1, -1, -1 ),
(* 153: 0,       3, 4,       7,  *)  (  0,  4,  2,  4,  6,  2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 154:    1,    3, 4,       7,  *)  (  1,  9,  0,  2,  3,  4,  2,  4,  6,  4,  3,  8, -1, -1, -1, -1 ),
(* 155: 0, 1,    3, 4,       7,  *)  (  1,  9,  4,  1,  4,  2,  2,  4,  6, -1, -1, -1, -1, -1, -1, -1 ),
(* 156:       2, 3, 4,       7,  *)  (  8,  1,  3,  8,  6,  1,  8,  4,  6,  6, 10,  1, -1, -1, -1, -1 ),
(* 157: 0,    2, 3, 4,       7,  *)  ( 10,  1,  0, 10,  0,  6,  6,  0,  4, -1, -1, -1, -1, -1, -1, -1 ),
(* 158:    1, 2, 3, 4,       7,  *)  (  4,  6,  3,  4,  3,  8,  6, 10,  3,  0,  3,  9, 10,  9,  3, -1 ),
(* 159: 0, 1, 2, 3, 4,       7,  *)  ( 10,  9,  4,  6, 10,  4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 160:                5,    7,  *)  (  4,  9,  5,  7,  6, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 161: 0,             5,    7,  *)  (  0,  8,  3,  4,  9,  5, 11,  7,  6, -1, -1, -1, -1, -1, -1, -1 ),
(* 162:    1,          5,    7,  *)  (  5,  0,  1,  5,  4,  0,  7,  6, 11, -1, -1, -1, -1, -1, -1, -1 ),
(* 163: 0, 1,          5,    7,  *)  ( 11,  7,  6,  8,  3,  4,  3,  5,  4,  3,  1,  5, -1, -1, -1, -1 ),
(* 164:       2,       5,    7,  *)  (  9,  5,  4, 10,  1,  2,  7,  6, 11, -1, -1, -1, -1, -1, -1, -1 ),
(* 165: 0,    2,       5,    7,  *)  (  6, 11,  7,  1,  2, 10,  0,  8,  3,  4,  9,  5, -1, -1, -1, -1 ),
(* 166:    1, 2,       5,    7,  *)  (  7,  6, 11,  5,  4, 10,  4,  2, 10,  4,  0,  2, -1, -1, -1, -1 ),
(* 167: 0, 1, 2,       5,    7,  *)  (  3,  4,  8,  3,  5,  4,  3,  2,  5, 10,  5,  2, 11,  7,  6, -1 ),
(* 168:          3,    5,    7,  *)  (  7,  2,  3,  7,  6,  2,  5,  4,  9, -1, -1, -1, -1, -1, -1, -1 ),
(* 169: 0,       3,    5,    7,  *)  (  9,  5,  4,  0,  8,  6,  0,  6,  2,  6,  8,  7, -1, -1, -1, -1 ),
(* 170:    1,    3,    5,    7,  *)  (  3,  6,  2,  3,  7,  6,  1,  5,  0,  5,  4,  0, -1, -1, -1, -1 ),
(* 171: 0, 1,    3,    5,    7,  *)  (  6,  2,  8,  6,  8,  7,  2,  1,  8,  4,  8,  5,  1,  5,  8, -1 ),
(* 172:       2, 3,    5,    7,  *)  (  9,  5,  4, 10,  1,  6,  1,  7,  6,  1,  3,  7, -1, -1, -1, -1 ),
(* 173: 0,    2, 3,    5,    7,  *)  (  1,  6, 10,  1,  7,  6,  1,  0,  7,  8,  7,  0,  9,  5,  4, -1 ),
(* 174:    1, 2, 3,    5,    7,  *)  (  4,  0, 10,  4, 10,  5,  0,  3, 10,  6, 10,  7,  3,  7, 10, -1 ),
(* 175: 0, 1, 2, 3,    5,    7,  *)  (  7,  6, 10,  7, 10,  8,  5,  4, 10,  4,  8, 10, -1, -1, -1, -1 ),
(* 176:             4, 5,    7,  *)  (  6,  9,  5,  6, 11,  9, 11,  8,  9, -1, -1, -1, -1, -1, -1, -1 ),
(* 177: 0,          4, 5,    7,  *)  (  3,  6, 11,  0,  6,  3,  0,  5,  6,  0,  9,  5, -1, -1, -1, -1 ),
(* 178:    1,       4, 5,    7,  *)  (  0, 11,  8,  0,  5, 11,  0,  1,  5,  5,  6, 11, -1, -1, -1, -1 ),
(* 179: 0, 1,       4, 5,    7,  *)  (  6, 11,  3,  6,  3,  5,  5,  3,  1, -1, -1, -1, -1, -1, -1, -1 ),
(* 180:       2,    4, 5,    7,  *)  (  1,  2, 10,  9,  5, 11,  9, 11,  8, 11,  5,  6, -1, -1, -1, -1 ),
(* 181: 0,    2,    4, 5,    7,  *)  (  0, 11,  3,  0,  6, 11,  0,  9,  6,  5,  6,  9,  1,  2, 10, -1 ),
(* 182:    1, 2,    4, 5,    7,  *)  ( 11,  8,  5, 11,  5,  6,  8,  0,  5, 10,  5,  2,  0,  2,  5, -1 ),
(* 183: 0, 1, 2,    4, 5,    7,  *)  (  6, 11,  3,  6,  3,  5,  2, 10,  3, 10,  5,  3, -1, -1, -1, -1 ),
(* 184:          3, 4, 5,    7,  *)  (  5,  8,  9,  5,  2,  8,  5,  6,  2,  3,  8,  2, -1, -1, -1, -1 ),
(* 185: 0,       3, 4, 5,    7,  *)  (  9,  5,  6,  9,  6,  0,  0,  6,  2, -1, -1, -1, -1, -1, -1, -1 ),
(* 186:    1,    3, 4, 5,    7,  *)  (  1,  5,  8,  1,  8,  0,  5,  6,  8,  3,  8,  2,  6,  2,  8, -1 ),
(* 187: 0, 1,    3, 4, 5,    7,  *)  (  1,  5,  6,  2,  1,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 188:       2, 3, 4, 5,    7,  *)  (  1,  3,  6,  1,  6, 10,  3,  8,  6,  5,  6,  9,  8,  9,  6, -1 ),
(* 189: 0,    2, 3, 4, 5,    7,  *)  ( 10,  1,  0, 10,  0,  6,  9,  5,  0,  5,  6,  0, -1, -1, -1, -1 ),
(* 190:    1, 2, 3, 4, 5,    7,  *)  (  0,  3,  8,  5,  6, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 191: 0, 1, 2, 3, 4, 5,    7,  *)  ( 10,  5,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 192:                   6, 7,  *)  ( 11,  5, 10,  7,  5, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 193: 0,                6, 7,  *)  ( 11,  5, 10, 11,  7,  5,  8,  3,  0, -1, -1, -1, -1, -1, -1, -1 ),
(* 194:    1,             6, 7,  *)  (  5, 11,  7,  5, 10, 11,  1,  9,  0, -1, -1, -1, -1, -1, -1, -1 ),
(* 195: 0, 1,             6, 7,  *)  ( 10,  7,  5, 10, 11,  7,  9,  8,  1,  8,  3,  1, -1, -1, -1, -1 ),
(* 196:       2,          6, 7,  *)  ( 11,  1,  2, 11,  7,  1,  7,  5,  1, -1, -1, -1, -1, -1, -1, -1 ),
(* 197: 0,    2,          6, 7,  *)  (  0,  8,  3,  1,  2,  7,  1,  7,  5,  7,  2, 11, -1, -1, -1, -1 ),
(* 198:    1, 2,          6, 7,  *)  (  9,  7,  5,  9,  2,  7,  9,  0,  2,  2, 11,  7, -1, -1, -1, -1 ),
(* 199: 0, 1, 2,          6, 7,  *)  (  7,  5,  2,  7,  2, 11,  5,  9,  2,  3,  2,  8,  9,  8,  2, -1 ),
(* 200:          3,       6, 7,  *)  (  2,  5, 10,  2,  3,  5,  3,  7,  5, -1, -1, -1, -1, -1, -1, -1 ),
(* 201: 0,       3,       6, 7,  *)  (  8,  2,  0,  8,  5,  2,  8,  7,  5, 10,  2,  5, -1, -1, -1, -1 ),
(* 202:    1,    3,       6, 7,  *)  (  9,  0,  1,  5, 10,  3,  5,  3,  7,  3, 10,  2, -1, -1, -1, -1 ),
(* 203: 0, 1,    3,       6, 7,  *)  (  9,  8,  2,  9,  2,  1,  8,  7,  2, 10,  2,  5,  7,  5,  2, -1 ),
(* 204:       2, 3,       6, 7,  *)  (  1,  3,  5,  3,  7,  5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 205: 0,    2, 3,       6, 7,  *)  (  0,  8,  7,  0,  7,  1,  1,  7,  5, -1, -1, -1, -1, -1, -1, -1 ),
(* 206:    1, 2, 3,       6, 7,  *)  (  9,  0,  3,  9,  3,  5,  5,  3,  7, -1, -1, -1, -1, -1, -1, -1 ),
(* 207: 0, 1, 2, 3,       6, 7,  *)  (  9,  8,  7,  5,  9,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 208:             4,    6, 7,  *)  (  5,  8,  4,  5, 10,  8, 10, 11,  8, -1, -1, -1, -1, -1, -1, -1 ),
(* 209: 0,          4,    6, 7,  *)  (  5,  0,  4,  5, 11,  0,  5, 10, 11, 11,  3,  0, -1, -1, -1, -1 ),
(* 210:    1,       4,    6, 7,  *)  (  0,  1,  9,  8,  4, 10,  8, 10, 11, 10,  4,  5, -1, -1, -1, -1 ),
(* 211: 0, 1,       4,    6, 7,  *)  ( 10, 11,  4, 10,  4,  5, 11,  3,  4,  9,  4,  1,  3,  1,  4, -1 ),
(* 212:       2,    4,    6, 7,  *)  (  2,  5,  1,  2,  8,  5,  2, 11,  8,  4,  5,  8, -1, -1, -1, -1 ),
(* 213: 0,    2,    4,    6, 7,  *)  (  0,  4, 11,  0, 11,  3,  4,  5, 11,  2, 11,  1,  5,  1, 11, -1 ),
(* 214:    1, 2,    4,    6, 7,  *)  (  0,  2,  5,  0,  5,  9,  2, 11,  5,  4,  5,  8, 11,  8,  5, -1 ),
(* 215: 0, 1, 2,    4,    6, 7,  *)  (  9,  4,  5,  2, 11,  3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 216:          3, 4,    6, 7,  *)  (  2,  5, 10,  3,  5,  2,  3,  4,  5,  3,  8,  4, -1, -1, -1, -1 ),
(* 217: 0,       3, 4,    6, 7,  *)  (  5, 10,  2,  5,  2,  4,  4,  2,  0, -1, -1, -1, -1, -1, -1, -1 ),
(* 218:    1,    3, 4,    6, 7,  *)  (  3, 10,  2,  3,  5, 10,  3,  8,  5,  4,  5,  8,  0,  1,  9, -1 ),
(* 219: 0, 1,    3, 4,    6, 7,  *)  (  5, 10,  2,  5,  2,  4,  1,  9,  2,  9,  4,  2, -1, -1, -1, -1 ),
(* 220:       2, 3, 4,    6, 7,  *)  (  8,  4,  5,  8,  5,  3,  3,  5,  1, -1, -1, -1, -1, -1, -1, -1 ),
(* 221: 0,    2, 3, 4,    6, 7,  *)  (  0,  4,  5,  1,  0,  5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 222:    1, 2, 3, 4,    6, 7,  *)  (  8,  4,  5,  8,  5,  3,  9,  0,  5,  0,  3,  5, -1, -1, -1, -1 ),
(* 223: 0, 1, 2, 3, 4,    6, 7,  *)  (  9,  4,  5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 224:                5, 6, 7,  *)  (  4, 11,  7,  4,  9, 11,  9, 10, 11, -1, -1, -1, -1, -1, -1, -1 ),
(* 225: 0,             5, 6, 7,  *)  (  0,  8,  3,  4,  9,  7,  9, 11,  7,  9, 10, 11, -1, -1, -1, -1 ),
(* 226:    1,          5, 6, 7,  *)  (  1, 10, 11,  1, 11,  4,  1,  4,  0,  7,  4, 11, -1, -1, -1, -1 ),
(* 227: 0, 1,          5, 6, 7,  *)  (  3,  1,  4,  3,  4,  8,  1, 10,  4,  7,  4, 11, 10, 11,  4, -1 ),
(* 228:       2,       5, 6, 7,  *)  (  4, 11,  7,  9, 11,  4,  9,  2, 11,  9,  1,  2, -1, -1, -1, -1 ),
(* 229: 0,    2,       5, 6, 7,  *)  (  9,  7,  4,  9, 11,  7,  9,  1, 11,  2, 11,  1,  0,  8,  3, -1 ),
(* 230:    1, 2,       5, 6, 7,  *)  ( 11,  7,  4, 11,  4,  2,  2,  4,  0, -1, -1, -1, -1, -1, -1, -1 ),
(* 231: 0, 1, 2,       5, 6, 7,  *)  ( 11,  7,  4, 11,  4,  2,  8,  3,  4,  3,  2,  4, -1, -1, -1, -1 ),
(* 232:          3,    5, 6, 7,  *)  (  2,  9, 10,  2,  7,  9,  2,  3,  7,  7,  4,  9, -1, -1, -1, -1 ),
(* 233: 0,       3,    5, 6, 7,  *)  (  9, 10,  7,  9,  7,  4, 10,  2,  7,  8,  7,  0,  2,  0,  7, -1 ),
(* 234:    1,    3,    5, 6, 7,  *)  (  3,  7, 10,  3, 10,  2,  7,  4, 10,  1, 10,  0,  4,  0, 10, -1 ),
(* 235: 0, 1,    3,    5, 6, 7,  *)  (  1, 10,  2,  8,  7,  4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 236:       2, 3,    5, 6, 7,  *)  (  4,  9,  1,  4,  1,  7,  7,  1,  3, -1, -1, -1, -1, -1, -1, -1 ),
(* 237: 0,    2, 3,    5, 6, 7,  *)  (  4,  9,  1,  4,  1,  7,  0,  8,  1,  8,  7,  1, -1, -1, -1, -1 ),
(* 238:    1, 2, 3,    5, 6, 7,  *)  (  4,  0,  3,  7,  4,  3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 239: 0, 1, 2, 3,    5, 6, 7,  *)  (  4,  8,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 240:             4, 5, 6, 7,  *)  (  9, 10,  8, 10, 11,  8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 241: 0,          4, 5, 6, 7,  *)  (  3,  0,  9,  3,  9, 11, 11,  9, 10, -1, -1, -1, -1, -1, -1, -1 ),
(* 242:    1,       4, 5, 6, 7,  *)  (  0,  1, 10,  0, 10,  8,  8, 10, 11, -1, -1, -1, -1, -1, -1, -1 ),
(* 243: 0, 1,       4, 5, 6, 7,  *)  (  3,  1, 10, 11,  3, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 244:       2,    4, 5, 6, 7,  *)  (  1,  2, 11,  1, 11,  9,  9, 11,  8, -1, -1, -1, -1, -1, -1, -1 ),
(* 245: 0,    2,    4, 5, 6, 7,  *)  (  3,  0,  9,  3,  9, 11,  1,  2,  9,  2, 11,  9, -1, -1, -1, -1 ),
(* 246:    1, 2,    4, 5, 6, 7,  *)  (  0,  2, 11,  8,  0, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 247: 0, 1, 2,    4, 5, 6, 7,  *)  (  3,  2, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 248:          3, 4, 5, 6, 7,  *)  (  2,  3,  8,  2,  8, 10, 10,  8,  9, -1, -1, -1, -1, -1, -1, -1 ),
(* 249: 0,       3, 4, 5, 6, 7,  *)  (  9, 10,  2,  0,  9,  2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 250:    1,    3, 4, 5, 6, 7,  *)  (  2,  3,  8,  2,  8, 10,  0,  1,  8,  1, 10,  8, -1, -1, -1, -1 ),
(* 251: 0, 1,    3, 4, 5, 6, 7,  *)  (  1, 10,  2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 252:       2, 3, 4, 5, 6, 7,  *)  (  1,  3,  8,  9,  1,  8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 253: 0,    2, 3, 4, 5, 6, 7,  *)  (  0,  9,  1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 254:    1, 2, 3, 4, 5, 6, 7,  *)  (  0,  3,  8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ),
(* 255: 0, 1, 2, 3, 4, 5, 6, 7,  *)  ( -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 )
);


// Marching Cube EdgeTable
const
  MC_EDGETABLE: array [0 .. 11, 0 .. 1] of Integer = ((0, 1), (1, 2), (2, 3),
    (3, 0), (4, 5), (5, 6), (6, 7), (7, 4), (0, 4), (1, 5), (2, 6), (3, 7));

  // Marching Tetrahedra TriTable
  (*
        + 0
         /|\
        / | \
       /  |  0
      3   |   \
     /    2    \
    /     |     \
   +----4--------+ 1
  3 \     |     /
     \    |    /
      5   |   1
       \  |  /
        \ | /
         \|/
        + 2
*)
  MT_TRITABLE: array [0 .. 15, 0 .. 6] of Integer =
    ((-1, -1, -1, -1, -1, -1, -1), (2, 3, 0, -1, -1, -1, -1),
    (4, 1, 0, -1, -1, -1, -1), (2, 4, 1, 3, 4, 2, -1),
    (5, 2, 1, -1, -1, -1, -1), (5, 3, 0, 1, 5, 0, -1), (5, 2, 0, 4, 5, 0, -1),
    (3, 4, 5, -1, -1, -1, -1), (5, 4, 3, -1, -1, -1, -1),
    (0, 5, 4, 0, 2, 5, -1), (0, 5, 1, 0, 3, 5, -1), (1, 2, 5, -1, -1, -1, -1),
    (2, 4, 3, 1, 4, 2, -1), (0, 1, 4, -1, -1, -1, -1),
    (0, 3, 2, -1, -1, -1, -1), (-1, -1, -1, -1, -1, -1, -1));

  // Marching Tetrahedra EdgeTable
  MT_EDGETABLE: array [0 .. 5, 0 .. 1] of Integer = ((0, 1), (1, 2), (2, 0),
    (0, 3), (1, 3), (2, 3));

  // Marching Tetrahedra CubeSplit
  MT_CUBESPLIT: array [0 .. 5, 0 .. 3] of Integer = ((0, 5, 1, 6), (0, 1, 2, 6),
    (0, 2, 3, 6), (0, 3, 7, 6), (0, 7, 4, 6), (0, 4, 5, 6));

// Test surface functions
function SFSphere(X, Y, Z: Extended): TxScalarValue;
begin
  Result := sqr(X) + sqr(Y) + sqr(Z)
end;

function SFToroidal(X, Y, Z: Extended): TxScalarValue;
const
  FScale = 7;
  a = 2.5;
begin
  X := FScale * X;
  Y := FScale * Y;
  Z := FScale * Z;
  Result := (sqr(sqrt(sqr(X) + sqr(Y)) - a) + sqr(Z)) *
    (sqr(sqrt(sqr(Y) + sqr(Z)) - a) + sqr(X)) *
    (sqr(sqrt(sqr(Z) + sqr(X)) - a) + sqr(Y));
end;

function SFDoubleTorus(X, Y, Z: Extended): TxScalarValue;
const
  FScale = 2.25;
begin
  X := FScale * X;
  Y := FScale * Y;
  Z := FScale * Z;
  Result := PowerInteger(X, 8) + PowerInteger(X, 4) - 2 * PowerInteger(X, 6) - 2
    * sqr(X) * sqr(Y) + 2 * PowerInteger(X, 4) * sqr(Y) +
    PowerInteger(Y, 4) + sqr(Z)
end;

function SFChmutov1(X, Y, Z: Extended): TxScalarValue;
const
  FScale = 2.5;
begin
  X := FScale * X;
  Y := FScale * Y;
  Z := FScale * Z;
  Result := 8 * (sqr(X) + sqr(Y) + sqr(Z)) - 8 *
    (PowerInteger(X, 4) + PowerInteger(Y, 4) + PowerInteger(Z, 4));
end;

function SFChmutov2(X, Y, Z: Extended): TxScalarValue;
const
  FScale = 2.5;
begin
  X := FScale * X;
  Y := FScale * Y;
  Z := FScale * Z;
  Result := 2 * (sqr(X) * sqr(3 - 4 * sqr(X)) + sqr(Y) * sqr(3 - 4 * sqr(Y)) +
    sqr(Z) * sqr(3 - 4 * sqr(Z)));
end;

function SFKleinBottle(X, Y, Z: Extended): TxScalarValue;
const
  FScale = 7.5;
begin
  X := FScale * X;
  Y := FScale * Y;
  Z := FScale * Z;
  Result := (sqr(X) + sqr(Y) + sqr(Z) + 2 * Y - 1) *
    (sqr(sqr(X) + sqr(Y) + sqr(Z) - 2 * Y - 1) - 8 * sqr(Z)) + 16 * X * Z *
    (sqr(X) + sqr(Y) + sqr(Z) - 2 * Y - 1);
end;

function SFMinkowski(X, Y, Z: Extended): TxScalarValue;
const
  FScale = 7;
begin
  X := FScale * X;
  Y := FScale * Y;
  Z := FScale * Z;
  Result := (sqr(X) - sqr(Y) - sqr(Z) - 2) * (sqr(X) - sqr(Y) - sqr(Z) + 2) *
    (sqr(X) - sqr(Y) - sqr(Z) - 4) * (sqr(X) - sqr(Y) - sqr(Z) + 4) *
    (sqr(X) - sqr(Y) - sqr(Z));
end;

(* -------------------------------------------------------------------------
   Class IsoSurfaceExtractor
   Purpose: Extract an Isosurface from volume dataset for given Isovalue
   ------------------------------------------------------------------------- *)

function TIsoSurfaceExtractor.BuildIndex(var ADatavals: array of Single;
  Isovalue: Single): word;
var
  i: Integer;
  val: word;
begin
  val := 1;
  Result := 0;
  for i := 1 to Length(ADatavals) do
  begin
    if ADatavals[i - 1] <= Isovalue then // Edge inside surface
      Result := Result + val;
    val := val * 2;
  end;
end;

// Compute intersection point of edge and surface by linear interpolation
function InterpolateRugged(V0, V1: TAffineVector;
  var Val0, Val1, Isovalue: Single): TVertex;
var
  Diff: Single;
  i: Integer;
begin
  if Val0 <= Isovalue then
  begin
    Diff := Val0 / (Val0 + Val1);
    for i := 0 to 2 do
      Result.V[i] := V0.V[i] + Diff * (V1.V[i] - V0.V[i]);
  end
  else
  begin
    Diff := Val1 / (Val0 + Val1);
    for i := 0 to 2 do
      Result.V[i] := V1.V[i] + Diff * (V0.V[i] - V1.V[i]);
  end;
end;

function InterpolatePolished(V0, V1: TAffineVector;
  var Val0, Val1, Isovalue: Single): TVertex;
var
  w0, w1: Single;
begin
  if (Val0 = Val1) then
    w1 := 0.5
  else
    w1 := (Isovalue - Val0) / (Val1 - Val0);
  w0 := 1.0 - w1;
  Result.X := w0 * V0.X + w1 * V1.X;
  Result.Y := w0 * V0.Y + w1 * V1.Y;
  Result.Z := w0 * V0.Z + w1 * V1.Z;
end;

function TIsoSurfaceExtractor.Interpolate(const V0, V1: TAffineVector;
  var Val0, Val1, Isovalue: Single; isPolished: boolean): TVertex;
begin
  if isPolished then
    Result := InterpolatePolished(V0, V1, Val0, Val1, Isovalue)
  else
    Result := InterpolateRugged(V0, V1, Val0, Val1, Isovalue)
end;

procedure TIsoSurfaceExtractor.MarchingTetrahedra(Isovalue: Single;
  out Vertices: TVertexArray; out Triangles: TIntegerArray; isPolished: boolean);
var
  i, j, k: Integer;
  index: word;
  CubeVertices: array of TAffineVector;
  Tetrahedron: array [0 .. 3] of TAffineVector;
  DataTetra: array [0 .. 3] of Single;

  // Add Triangle to List
  procedure AppendTri();
  var
    edge: byte;
    Ver1, Ver2, Ver3: TVertex;
    VListlength: Integer;
    TListlength: Integer;
  begin
    edge := 0;
    while MT_TRITABLE[index, edge] <> -1 do
    begin
      Ver1 := Interpolate(Tetrahedron[MT_EDGETABLE[MT_TRITABLE[index, edge], 0]
        ], Tetrahedron[MT_EDGETABLE[MT_TRITABLE[index, edge], 1]],
        DataTetra[MT_EDGETABLE[MT_TRITABLE[index, edge], 0]],
        DataTetra[MT_EDGETABLE[MT_TRITABLE[index, edge], 1]], Isovalue, isPolished);
      Ver2 := Interpolate(Tetrahedron[MT_EDGETABLE[MT_TRITABLE[index, edge + 1],
        0]], Tetrahedron[MT_EDGETABLE[MT_TRITABLE[index, edge + 1], 1]],
        DataTetra[MT_EDGETABLE[MT_TRITABLE[index, edge + 1], 0]],
        DataTetra[MT_EDGETABLE[MT_TRITABLE[index, edge + 1], 1]], Isovalue, isPolished);
      Ver3 := Interpolate(Tetrahedron[MT_EDGETABLE[MT_TRITABLE[index, edge + 2],
        0]], Tetrahedron[MT_EDGETABLE[MT_TRITABLE[index, edge + 2], 1]],
        DataTetra[MT_EDGETABLE[MT_TRITABLE[index, edge + 2], 0]],
        DataTetra[MT_EDGETABLE[MT_TRITABLE[index, edge + 2], 1]], Isovalue, isPolished);
      VListlength := Length(Vertices) + 3;
      TListlength := Length(Triangles) + 3;
      SetLength(Vertices, VListlength);
      SetLength(Triangles, TListlength);
      Vertices[VListlength - 3] := Ver1;
      Vertices[VListlength - 2] := Ver2;
      Vertices[VListlength - 1] := Ver3;
      Triangles[TListlength - 3] := VListlength - 3;
      Triangles[TListlength - 2] := VListlength - 2;
      Triangles[TListlength - 1] := VListlength - 1;
      edge := edge + 3;
    end;
  end;

// Split Cube in 6 Tetrahedrons and process each tetrahedron
  procedure SplitCube();
  var
    i, j: Integer;
  begin
    for i := 0 to 5 do
    begin
      for j := 0 to 3 do
      begin
        Tetrahedron[j] := CubeVertices[MT_CUBESPLIT[i, j]];
        DataTetra[j] := Data[Trunc(Tetrahedron[j].X),
          Trunc(Tetrahedron[j].Y), Trunc(Tetrahedron[j].Z)];
      end;
      index := BuildIndex(DataTetra, Isovalue);
      AppendTri();
    end;
  end;

begin
(*
      1----2
     /|   /|
    0----3 |
    | 5--|-6
    |/   |/
    4----7
*)
  SetLength(CubeVertices, 8);
  for k := 0 to Dimensions['z'] - 2 do
  begin
    for j := 0 to Dimensions['y'] - 2 do
    begin
      for i := 0 to Dimensions['x'] - 2 do
      begin
        CubeVertices[0] := AffineVectorMake(i, j, k);
        CubeVertices[1] := AffineVectorMake(i, j, k + 1);
        CubeVertices[2] := AffineVectorMake(i + 1, j, k + 1);
        CubeVertices[3] := AffineVectorMake(i + 1, j, k);
        CubeVertices[4] := AffineVectorMake(i, j + 1, k);
        CubeVertices[5] := AffineVectorMake(i, j + 1, k + 1);
        CubeVertices[6] := AffineVectorMake(i + 1, j + 1, k + 1);
        CubeVertices[7] := AffineVectorMake(i + 1, j + 1, k);

        SplitCube();
      end; // for k
    end; // for j
  end; // for i
end; // ccMT

constructor TIsoSurfaceExtractor.Create;
begin
  inherited;
end;

constructor TIsoSurfaceExtractor.Create(Xdim, Ydim, Zdim: Integer;
  var AData: TSingle3DArray);
begin
  Create();
  AssignData(Xdim, Ydim, Zdim, AData);
end;

destructor TIsoSurfaceExtractor.Destroy;
begin
  inherited;
end;

procedure TIsoSurfaceExtractor.AssignData(Xdim, Ydim, Zdim: Integer;
  var AData: TSingle3DArray);
begin
  Dimensions['x'] := Xdim;
  Dimensions['y'] := Ydim;
  Dimensions['z'] := Zdim;

  Data := AData;
end;

//-----------------------------------------------------------------------
procedure TIsoSurfaceExtractor.MarchingCubes(Isovalue: Single;
  out Vertices: TVertexArray; out Triangles: TIntegerArray; isPolished: boolean);
var
  i, j, k: Integer;
  index: word;
  CubeVertices: array [0 .. 7] of TAffineVector;
  CubeData: array [0 .. 7] of Single;

  procedure AppendTri();
  var
    edge: byte;
    Ver1, Ver2, Ver3: TVertex;
    VListlength: Integer;
    TListlength: Integer;
  begin
    edge := 0;
    while MC_TRITABLE[index, edge] <> -1 do
    begin
      Ver1 := Interpolate(CubeVertices[MC_EDGETABLE[MC_TRITABLE[index, edge], 0]
        ], CubeVertices[MC_EDGETABLE[MC_TRITABLE[index, edge], 1]],
        CubeData[MC_EDGETABLE[MC_TRITABLE[index, edge], 0]],
        CubeData[MC_EDGETABLE[MC_TRITABLE[index, edge], 1]], Isovalue, isPolished);
      Ver2 := Interpolate(CubeVertices[MC_EDGETABLE[MC_TRITABLE[index,
        edge + 1], 0]], CubeVertices[MC_EDGETABLE[MC_TRITABLE[index, edge + 1],
        1]], CubeData[MC_EDGETABLE[MC_TRITABLE[index, edge + 1], 0]],
        CubeData[MC_EDGETABLE[MC_TRITABLE[index, edge + 1], 1]], Isovalue, isPolished);
      Ver3 := Interpolate(CubeVertices[MC_EDGETABLE[MC_TRITABLE[index,
        edge + 2], 0]], CubeVertices[MC_EDGETABLE[MC_TRITABLE[index, edge + 2],
        1]], CubeData[MC_EDGETABLE[MC_TRITABLE[index, edge + 2], 0]],
        CubeData[MC_EDGETABLE[MC_TRITABLE[index, edge + 2], 1]], Isovalue, isPolished);
      VListlength := Length(Vertices) + 3;
      TListlength := Length(Triangles) + 3;
      SetLength(Vertices, VListlength);
      SetLength(Triangles, TListlength);
      Vertices[VListlength - 3] := Ver1;
      Vertices[VListlength - 2] := Ver2;
      Vertices[VListlength - 1] := Ver3;
      Triangles[TListlength - 3] := VListlength - 3;
      Triangles[TListlength - 2] := VListlength - 2;
      Triangles[TListlength - 1] := VListlength - 1;
      edge := edge + 3;
    end;
  end;

begin
  (*
      7----6
     /|   /|
    3----2 |
    | 4--|-5
    |/   |/
    0----1
  *)
  for i := 0 to Dimensions['x'] - 2 do
  begin
    for j := 1 to Dimensions['y'] - 1 do
    begin
      for k := 0 to Dimensions['z'] - 2 do
      begin
        CubeVertices[0] := AffineVectorMake(i, j, k);
        CubeVertices[1] := AffineVectorMake(i + 1, j, k);
        CubeVertices[2] := AffineVectorMake(i + 1, j - 1, k);
        CubeVertices[3] := AffineVectorMake(i, j - 1, k);
        CubeVertices[4] := AffineVectorMake(i, j, k + 1);
        CubeVertices[5] := AffineVectorMake(i + 1, j, k + 1);
        CubeVertices[6] := AffineVectorMake(i + 1, j - 1, k + 1);
        CubeVertices[7] := AffineVectorMake(i, j - 1, k + 1);
        CubeData[0] := Data[i, j, k];
        CubeData[1] := Data[i + 1, j, k];
        CubeData[2] := Data[i + 1, j - 1, k];
        CubeData[3] := Data[i, j - 1, k];
        CubeData[4] := Data[i, j, k + 1];
        CubeData[5] := Data[i + 1, j, k + 1];
        CubeData[6] := Data[i + 1, j - 1, k + 1];
        CubeData[7] := Data[i, j - 1, k + 1];

        Index := BuildIndex(CubeData, Isovalue);
        AppendTri();
      end; // for k
    end; // for j
  end; // for i
end;


function TGLMarchingCube.add_c_vertex: Integer;
var
  u: Single;
  vid: Integer;
  procedure VertexAdd(iv: Integer);
  begin
    with PVertices^[_Nverts] do
    begin
      u := u + 1;
      P := VectorAdd(P, PVertices[iv].P);
      N := VectorAdd(N, PVertices[iv].N);
{$IFDEF UseDensity}
      Density := Density + PVertices[iv].Density;
{$ENDIF}
    end
  end;

begin
  test_vertex_addiction;

  u := 0;
  with PVertices^[_Nverts] do
  begin
    P := NullVector;
    N := NullVector;
{$IFDEF UseDensity}
    Density := 0;
{$ENDIF}
  end;
  Inc(_Nverts);

  // Computes the average of the intersection points of the cube
  vid := Get_x_vert(_i, _j, _k);
  if (vid <> -1) then
    VertexAdd(vid);
  vid := Get_y_vert(_i + 1, _j, _k);
  if (vid <> -1) then
    VertexAdd(vid);
  vid := Get_x_vert(_i, _j + 1, _k);
  if (vid <> -1) then
    VertexAdd(vid);
  vid := Get_y_vert(_i, _j, _k);
  if (vid <> -1) then
    VertexAdd(vid);
  vid := Get_x_vert(_i, _j, _k + 1);
  if (vid <> -1) then
    VertexAdd(vid);
  vid := Get_y_vert(_i + 1, _j, _k + 1);
  if (vid <> -1) then
    VertexAdd(vid);
  vid := Get_x_vert(_i, _j + 1, _k + 1);
  if (vid <> -1) then
    VertexAdd(vid);
  vid := Get_y_vert(_i, _j, _k + 1);
  if (vid <> -1) then
    VertexAdd(vid);
  vid := Get_z_vert(_i, _j, _k);
  if (vid <> -1) then
    VertexAdd(vid);
  vid := Get_z_vert(_i + 1, _j, _k);
  if (vid <> -1) then
    VertexAdd(vid);
  vid := Get_z_vert(_i + 1, _j + 1, _k);
  if (vid <> -1) then
    VertexAdd(vid);
  vid := Get_z_vert(_i, _j + 1, _k);
  if (vid <> -1) then
    VertexAdd(vid);

  ScaleVector(PVertices^[_Nverts].P, 1 / u);
  NormalizeVector(PVertices^[_Nverts].N);
{$IFDEF UseDensity}
  PVertices^[_Nverts].Density := PVertices^[_Nverts].Density / u;
{$ENDIF}
  Result := _Nverts - 1;
end;

procedure TGLMarchingCube.add_triangle(trig: array of Integer; N: Byte;
  v12: Integer = -1);

var
  tv: array [0 .. 2] of Integer;
  t, tmod3: Integer;

begin
  for t := 0 to 3 * N - 1 do
  begin
    tmod3 := t mod 3;
    case trig[t] of
      0: tv[tmod3] := Get_x_vert(_i, _j, _k);
      1: tv[tmod3] := Get_y_vert(_i + 1, _j, _k);
      2: tv[tmod3] := Get_x_vert(_i, _j + 1, _k);
      3: tv[tmod3] := Get_y_vert(_i, _j, _k);
      4: tv[tmod3] := Get_x_vert(_i, _j, _k + 1);
      5: tv[tmod3] := Get_y_vert(_i + 1, _j, _k + 1);
      6: tv[tmod3] := Get_x_vert(_i, _j + 1, _k + 1);
      7: tv[tmod3] := Get_y_vert(_i, _j, _k + 1);
      8: tv[tmod3] := Get_z_vert(_i, _j, _k);
      9: tv[tmod3] := Get_z_vert(_i + 1, _j, _k);
      10: tv[tmod3] := Get_z_vert(_i + 1, _j + 1, _k);
      11: tv[tmod3] := Get_z_vert(_i, _j + 1, _k);
      12: tv[tmod3] := v12
    end;

    if (tv[tmod3] = -1) then
      Break;

    if (tmod3 = 2) then
    begin
      if (_Ntrigs >= _Strigs) then
      begin
        _Strigs := 2 * _Strigs;
        ReallocMem(PTriangles, _Strigs * SizeOf(TxTriangle));
      end;

      with PTriangles^[_Ntrigs] do
      begin
        v1 := tv[0];
        v2 := tv[1];
        v3 := tv[2];
      end;
      Inc(_Ntrigs);

    end
  end
end;

function TGLMarchingCube.calc_u(v1, v2: Single): Extended;
begin
  if (abs(FIsoValue - v1) >= 0.00001) then
    Result := 1
  else if (abs(FIsoValue - v2) >= 0.00001) then
    Result := 0
  else if (abs(v1 - v2) >= 0.00001) then
    Result := (FIsoValue - v1) / (v2 - v1)
  else
    Result := 0.5
end;

function TGLMarchingCube.add_x_vertex: Integer;
var
  u: Single;
begin
  test_vertex_addiction;
  u := calc_u(_Cube[0].Density, _Cube[1].Density);

  with PVertices^[_Nverts] do
  begin
    P.X := _Cube[0].P.X + u * FStepX;
    P.Y := _Cube[0].P.Y;
    P.Z := _Cube[0].P.Z;

    N.X := (1 - u) * Get_x_grad(_i, _j, _k) + u * Get_x_grad(_i + 1, _j, _k);
    N.Y := (1 - u) * Get_y_grad(_i, _j, _k) + u * Get_y_grad(_i + 1, _j, _k);
    N.Z := (1 - u) * Get_z_grad(_i, _j, _k) + u * Get_z_grad(_i + 1, _j, _k);
    NormalizeVector(N);
{$IFDEF UseDensity}
    Density := _Cube[1].Density
{$ENDIF}
  end;
  Inc(_Nverts);
  Result := _Nverts - 1;
end;

function TGLMarchingCube.add_y_vertex: Integer;
var
  u: Single;
begin
  test_vertex_addiction;
  u := calc_u(_Cube[0].Density, _Cube[3].Density);

  with PVertices^[_Nverts] do
  begin
    P.X := _Cube[0].P.X;
    P.Y := _Cube[0].P.Y + u * FStepY;
    P.Z := _Cube[0].P.Z;

    N.X := (1 - u) * Get_x_grad(_i, _j, _k) + u * Get_x_grad(_i, _j + 1, _k);
    N.Y := (1 - u) * Get_y_grad(_i, _j, _k) + u * Get_y_grad(_i, _j + 1, _k);
    N.Z := (1 - u) * Get_z_grad(_i, _j, _k) + u * Get_z_grad(_i, _j + 1, _k);
    NormalizeVector(N);
{$IFDEF UseDensity}
    Density := _Cube[3].Density
{$ENDIF}
  end;
  Inc(_Nverts);
  Result := _Nverts - 1;
end;

function TGLMarchingCube.add_z_vertex: Integer;
var
  u: Single;
begin
  test_vertex_addiction;
  u := calc_u(_Cube[0].Density, _Cube[4].Density);

  with PVertices^[_Nverts] do
  begin
    P.X := _Cube[0].P.X;
    P.Y := _Cube[0].P.Y;
    P.Z := _Cube[0].P.Z + u * FStepZ;;

    N.X := (1 - u) * Get_x_grad(_i, _j, _k) + u * Get_x_grad(_i, _j, _k + 1);
    N.Y := (1 - u) * Get_y_grad(_i, _j, _k) + u * Get_y_grad(_i, _j, _k + 1);
    N.Z := (1 - u) * Get_z_grad(_i, _j, _k) + u * Get_z_grad(_i, _j, _k + 1);
    NormalizeVector(N);
{$IFDEF UseDensity}
    Density := _Cube[4].Density
{$ENDIF}
  end;
  Inc(_Nverts);
  Result := _Nverts - 1;
end;

procedure TGLMarchingCube.clean_all(keepFacets: Boolean = False);
begin
  clean_temps;
  clean_space;
  if (not keepFacets) then
    FreeMem(PVertices);
  FreeMem(PTriangles);
  PVertices := nil;
  PTriangles := nil;
  _Nverts := 0;
  _Ntrigs := 0;
  _Sverts := 0;
  _Strigs := 0;
end;

procedure TGLMarchingCube.clean_space;
begin
  if (VoxelData <> nil) then
  begin
    FreeMem(VoxelData);
    VoxelData := nil
  end;
  FSizeX := 0;
  FSizeY := 0;
  FSizeZ := 0
end;

procedure TGLMarchingCube.clean_temps;
begin
  FreeMem(PVertsX);
  PVertsX := nil;
  FreeMem(PVertsY);
  PVertsY := nil;
  FreeMem(PVertsZ);
  PVertsZ := nil;
end;

procedure TGLMarchingCube.compute_intersection_points;
var
  k, j, i: Integer;
begin
  _Cube[0] := getVoxelData(0, 0, 0);
  _Cube[1] := getVoxelData(1, 0, 0);
  _Cube[3] := getVoxelData(0, 1, 0);
  _Cube[4] := getVoxelData(0, 0, 1);
  { _step_x:= _Cube[1].P[0] - _Cube[0].P[0] ;
    _step_y:= _Cube[3].P[1] - _Cube[0].P[1] ;
    _step_z:= _Cube[4].P[2] - _Cube[0].P[2] ; }

  for k := 0 to FSizeZ - 2 do
  begin
    _k := k;
    for j := 0 to FSizeY - 2 do
    begin
      _j := j;
      for i := 0 to FSizeX - 2 do
      begin
        _i := i;
        _Cube[0] := getVoxelData(_i, _j, _k);
        _Cube[1] := getVoxelData(_i + 1, _j, _k);
        _Cube[3] := getVoxelData(_i, _j + 1, _k);
        _Cube[4] := getVoxelData(_i, _j, _k + 1);

        if (Internal(_Cube[0].Density)) then
        begin
          if (not Internal(_Cube[1].Density)) then
            set_x_vert(add_x_vertex(), _i, _j, _k);
          if (not Internal(_Cube[3].Density)) then
            set_y_vert(add_y_vertex(), _i, _j, _k);
          if (not Internal(_Cube[4].Density)) then
            set_z_vert(add_z_vertex(), _i, _j, _k);
        end
        else
        begin
          if (Internal(_Cube[1].Density)) then
            set_x_vert(add_x_vertex(), _i, _j, _k);
          if (Internal(_Cube[3].Density)) then
            set_y_vert(add_y_vertex(), _i, _j, _k);
          if (Internal(_Cube[4].Density)) then
            set_z_vert(add_z_vertex(), _i, _j, _k);
        end
      end
    end
  end
end;

procedure TGLMarchingCube.ReDim(ASizeX, ASizeY, ASizeZ: Integer;
  xMin, xMax, yMin, yMax, zMin, zMax: Single);
begin
  clean_all;
  FSizeX := ASizeX;
  FSizeY := ASizeY;
  FSizeZ := ASizeZ;
  FxMin := xMin;
  FxMax := xMax;
  FyMin := yMin;
  FyMax := yMax;
  FzMin := zMin;
  FzMax := zMax;

  FStepX := (FxMax - FxMin) / (FSizeX - 1);
  FStepY := (FyMax - FyMin) / (FSizeY - 1);
  FStepZ := (FzMax - FzMin) / (FSizeZ - 1);

  VoxelData := nil;
  PVertsX := nil;
  PVertsY := nil;
  PVertsZ := nil;
  _Nverts := 0;
  _Ntrigs := 0;
  _Sverts := 0;
  _Strigs := 0;
  PVertices := nil;
  PTriangles := nil;

  Init_all;
  // FillVoxelData;
end;

constructor TGLMarchingCube.Create;
begin
  FOriginalMC := True; // now only original MC is implemented
  FIsoValue := 0;
  ScalarField := nil;
  // SFSphere;//SFKleinBottle;//SFMinkowski;// SFChmutov2;// SFChmutov1;//SFDoubleTorus;// SFToroidal;

  VoxelData := nil;
  PVertsX := nil;
  PVertsY := nil;
  PVertsZ := nil;
  _Nverts := 0;
  _Ntrigs := 0;
  _Sverts := 0;
  _Strigs := 0;
  PVertices := nil;
  PTriangles := nil;
end;

constructor TGLMarchingCube.Create(SizeX, SizeY, SizeZ: Integer;
  AIsoValue: TxScalarValue = 0.0; xMin: Single = -0.5; xMax: Single = 0.5;
  yMin: Single = -0.5; yMax: Single = 0.5; zMin: Single = -0.5;
  zMax: Single = 0.5);
begin
  FOriginalMC := True; // now only original MC is implemented
  FIsoValue := AIsoValue;
  ScalarField := SFSphere;
  //SFKleinBottle;
  //SFMinkowski;
  //SFChmutov2;
  //SFChmutov1;
  //SFDoubleTorus;
  //SFToroidal;
  ReDim(SizeX, SizeY, SizeZ, xMin, xMax, yMin, yMax, zMin, zMax);
  FillVoxelData;
end;

destructor TGLMarchingCube.Destroy;
begin
  clean_all;
  inherited;
end;

function TGLMarchingCube.getVoxelValue(i, j, k: Integer): TxScalarValue;
begin
  Result := VoxelData^[i + j * FSizeX + k * FSizeX * FSizeY].Density
end;

function TGLMarchingCube.getVoxelData(i, j, k: Integer): TxVoxel;
begin
  Result := VoxelData^[i + j * FSizeX + k * FSizeX * FSizeY]
end;

function TGLMarchingCube.Get_x_grad(i, j, k: Integer): Single;
begin
  if (i > 0) then
    if (i < FSizeX - 1) then
      Result := (getVoxelValue(i + 1, j, k) - getVoxelValue(i - 1, j, k)) / 2
    else
      Result := getVoxelValue(i, j, k) - getVoxelValue(i - 1, j, k)
  else
    Result := getVoxelValue(i + 1, j, k) - getVoxelValue(i, j, k)
end;

function TGLMarchingCube.Get_x_vert(i, j, k: Integer): Integer;
begin
  Result := PVertsX^[i + j * FSizeX + k * FSizeX * FSizeY]
end;

function TGLMarchingCube.Get_y_grad(i, j, k: Integer): Single;
begin
  if (j > 0) then
    if (j < FSizeY - 1) then
      Result := (getVoxelValue(i, j + 1, k) - getVoxelValue(i, j - 1, k)) / 2
    else
      Result := getVoxelValue(i, j, k) - getVoxelValue(i, j - 1, k)
  else
    Result := getVoxelValue(i, j + 1, k) - getVoxelValue(i, j, k)
end;

function TGLMarchingCube.Get_y_vert(i, j, k: Integer): Integer;
begin
  Result := PVertsY^[i + j * FSizeX + k * FSizeX * FSizeY]
end;

function TGLMarchingCube.Get_z_grad(i, j, k: Integer): Single;
begin
  if (k > 0) then
    if (k < FSizeZ - 1) then
      Result := (getVoxelValue(i, j, k + 1) - getVoxelValue(i, j, k - 1)) / 2
    else
      Result := getVoxelValue(i, j, k) - getVoxelValue(i, j, k - 1)
  else
    Result := getVoxelValue(i, j, k + 1) - getVoxelValue(i, j, k)
end;

function TGLMarchingCube.Get_z_vert(i, j, k: Integer): Integer;
begin
  Result := PVertsZ^[i + j * FSizeX + k * FSizeX * FSizeY]
end;

procedure TGLMarchingCube.Init_all;
begin
  Init_temps;
  Init_space;

  if (PVertices <> nil) then
    FreeMem(PVertices);
  if (PTriangles <> nil) then
    FreeMem(PTriangles);
  _Nverts := 0;
  _Ntrigs := 0;
  _Sverts := ALLOC_SIZE;
  _Strigs := ALLOC_SIZE;

  GetMem(PVertices, _Sverts * SizeOf(TxVertex));
  GetMem(PTriangles, _Strigs * SizeOf(TxTriangle));
end;

procedure TGLMarchingCube.Init_space;
begin
  VoxelData := AllocMem(FSizeX * FSizeY * FSizeZ * SizeOf(TxVoxel));
end;

procedure TGLMarchingCube.Init_temps;
var
  spaceSize: Longword;
begin
  spaceSize := FSizeX * FSizeY * FSizeZ;
  GetMem(PVertsX, spaceSize * SizeOf(Integer));
  GetMem(PVertsY, spaceSize * SizeOf(Integer));
  GetMem(PVertsZ, spaceSize * SizeOf(Integer));

  FillChar(PVertsX^, spaceSize * SizeOf(Integer), -1);
  FillChar(PVertsY^, spaceSize * SizeOf(Integer), -1);
  FillChar(PVertsZ^, spaceSize * SizeOf(Integer), -1);
end;

function TGLMarchingCube.Internal(AValue: TxScalarValue): Boolean;
begin
  Result := AValue <= FIsoValue
end;

procedure TGLMarchingCube.process_cube;
var
  nt: Byte;
begin
  if (FOriginalMC) then
  begin
    nt := 0;
    while (MC_TRITABLE[_lut_entry][3 * nt] <> -1) do
      Inc(nt);
    add_triangle(MC_TRITABLE[_lut_entry], nt);
    Exit;
  end;
  /// TODO complete algorithm with various tiling...
end;

procedure TGLMarchingCube.Run;
var
  i, j, k, P: Integer;
begin
  if (PVertsX = nil) then
  begin
    Init_temps;
    _Nverts := 0;
    _Ntrigs := 0;
  end;
  Compute_Intersection_Points;
  for k := 0 to FSizeZ - 2 do
  begin
    _k := k;
    for j := 0 to FSizeY - 2 do
    begin
      _j := j;
      for i := 0 to FSizeX - 2 do
      begin
        _i := i;
        _lut_entry := 0;
        for P := 0 to 7 do
        begin
          _Cube[P] := getVoxelData(i + ((P xor (P shr 1)) and 1),
            j + ((P shr 1) and 1), k + ((P shr 2) and 1));
          // _Cube[p]:= getVoxelData( i+((p^(p>>1))&1), j+((p>>1)&1), k+((p>>2)&1) ) ;
          if (Internal(_Cube[P].Density)) then
            _lut_entry := _lut_entry or (1 shl P);
        end;
        process_cube;
      end
    end
  end;
  clean_temps;
end;

procedure TGLMarchingCube.Run(IsoValue: TxScalarValue);
begin
  FIsoValue := IsoValue;
  Run
end;

procedure TGLMarchingCube.setVoxelValue(i, j, k: Integer; HfValue: TxScalarValue);
begin
  VoxelData^[i + j * FSizeX + k * FSizeX * FSizeY].Density := HfValue
end;

procedure TGLMarchingCube.set_x_vert(a_val, i, j, k: Integer);
begin
  PVertsX^[i + j * FSizeX + k * FSizeX * FSizeY] := a_val
end;

procedure TGLMarchingCube.set_y_vert(a_val, i, j, k: Integer);
begin
  PVertsY^[i + j * FSizeX + k * FSizeX * FSizeY] := a_val
end;

procedure TGLMarchingCube.set_z_vert(a_val, i, j, k: Integer);
begin
  PVertsZ^[i + j * FSizeX + k * FSizeX * FSizeY] := a_val
end;

procedure TGLMarchingCube.test_vertex_addiction;
begin
  if _Nverts >= _Sverts then
  begin
    _Sverts := 2 * _Sverts;
    ReallocMem(PVertices, _Sverts * SizeOf(TxVertex))
  end;
end;

function TGLMarchingCube.voxel(i, j, k: Integer): PxVoxel;
begin
  if (k >= FSizeZ) or (j >= FSizeY) or (i >= FSizeX) then
    Result := nil
  else
    Result := @VoxelData^[i + j * FSizeX + k * FSizeX * FSizeY]
end;

procedure TGLMarchingCube.FillVoxelData;
var
  iX, iY, iZ: Integer;
  X, Y, Z: Single;
begin
  for iX := 0 to FSizeX - 1 do
  begin
    X := FxMin + iX * FStepX;
    for iY := 0 to FSizeY - 1 do
    begin
      Y := FyMin + iY * FStepY;
      for iZ := 0 to FSizeZ - 1 do
        with VoxelData^[iX + iY * FSizeX + iZ * FSizeX * FSizeY] do
        begin
          Z := FzMin + iZ * FStepZ;
          MakeVector(P, X, Y, Z);
          Density := ScalarField(X, Y, Z);
          if Internal(Density) then
            Status := bpInternal
          else
            Status := bpExternal
        end;
    end;
  end;
end;

procedure TGLMarchingCube.FillVoxelData(AIsoValue: TxScalarValue; AScalarField: TxScalarField = nil);
begin
  FIsoValue := AIsoValue;
  if Assigned(AScalarField) then
    ScalarField := AScalarField;
  FillVoxelData;
end;

procedure TGLMarchingCube.FillVoxelData(AIsoValue: TxScalarValue; AScalarField: TxScalarFieldInt);
var
  iX, iY, iZ: Integer;
  X, Y, Z: Single;
begin
  FIsoValue := AIsoValue;
  for iX := 0 to FSizeX - 1 do
  begin
    X := FxMin + iX * FStepX;
    for iY := 0 to FSizeY - 1 do
    begin
      Y := FyMin + iY * FStepY;
      for iZ := 0 to FSizeZ - 1 do
        with VoxelData^[iX + iY * FSizeX + iZ * FSizeX * FSizeY] do
        begin
          Z := FzMin + iZ * FStepZ;
          MakeVector(P, X, Y, Z);
          Density := AScalarField(iX, iY, iZ);
          if Internal(Density) then
            Status := bpInternal
          else
            Status := bpExternal
        end;
    end;
  end;
end;

procedure TGLMarchingCube.CalcVertices(Vertices: TGLVertexList;
  Alpha: Single = 1);
var
  i: Integer;
  vx1, vx2, vx3: TGLVertexData;

  function GetNrmColor(Nrm: TAffineVector): TGLVector;
  begin
    Result.V[0] := 0;
    if Nrm.V[0] > 0.0 then
      Result.V[0] := Result.V[0] + Nrm.V[0];
    if Nrm.V[1] < 0.0 then
      Result.V[0] := Result.V[0] - 0.5 * Nrm.V[1];
    if Nrm.V[2] < 0.0 then
      Result.V[0] := Result.V[0] - 0.5 * Nrm.V[2];

    Result.V[1] := 1;
    if Nrm.V[0] < 0.0 then
      Result.V[1] := Result.V[1] - 0.5 * Nrm.V[0];
    if Nrm.V[1] > 0.0 then
      Result.V[1] := Result.V[1] + Nrm.V[1];
    if Nrm.V[2] < 0.0 then
      Result.V[1] := Result.V[1] - 0.5 * Nrm.V[2];

    Result.V[2] := 0;
    if Nrm.V[0] < 0.0 then
      Result.V[2] := Result.V[2] - 0.5 * Nrm.V[0];
    if Nrm.V[1] < 0.0 then
      Result.V[2] := Result.V[2] - 0.5 * Nrm.V[1];
    if Nrm.V[2] > 0.0 then
      Result.V[2] := Result.V[2] + Nrm.V[2];
    Result.V[3] := 0.3
  end;

  function GetColor(H: TxScalarValue): TGLVector;
  begin
    Result := VectorMake(0.890, 0.855, 0.788, Alpha)
    (*
      if H <= 10 then Result:= VectorMake(0.922, 0.957, 0.980, 1.000)  //<=10
      else if H <= 50 then Result:= VectorMake(0.541, 0.027, 0.027, 1.000) // 10..50
      else if H <= 300 then Result:= VectorMake(0.941, 0.910, 0.859, 1.000) //50..300
      else if H <= 2000 then Result:= VectorMake(0.965, 0.969, 0.973, 1.000) //350.. 2000
      else if H <= 4000 then Result:= VectorMake(0.890, 0.855, 0.788, 1.000) //2000..4000
      else Result:= VectorMake(0.9, 0.9, 0.6, 1.0)
    *)
  end;

begin
  Vertices.Clear;
  Vertices.Capacity := 3 * _Ntrigs;

  for i := 0 to _Ntrigs - 1 do
    with PTriangles^[i] do
    begin
      vx1.coord := PVertices^[v1].P;
      vx1.normal := PVertices^[v1].N;
      vx2.coord := PVertices^[v2].P;
      vx2.normal := PVertices^[v2].N;
      vx3.coord := PVertices^[v3].P;
      vx3.normal := PVertices^[v3].N;
{$IFDEF UseDensity}
      vx1.Color := GetColor(PVertices^[v1].Density); // GetNrmColor(vx1.normal);
      vx2.Color := GetColor(PVertices^[v2].Density); // GetNrmColor(vx2.normal);
      vx3.Color := GetColor(PVertices^[v3].Density); // GetNrmColor(vx3.normal);
{$ELSE}
      vx1.Color := VectorMake(0.890, 0.855, 0.788, Alpha);
      vx2.Color := VectorMake(0.890, 0.855, 0.788, Alpha);
      vx3.Color := VectorMake(0.890, 0.855, 0.788, Alpha);
{$ENDIF}
      // Vertices.AddVertex3(vx1, vx2, vx3); seems to be correct the next line
      Vertices.AddVertex3(vx3, vx2, vx1);
    end;
end;

procedure TGLMarchingCube.CalcMeshObject(AMeshObject: TMeshObject; Alpha: Single);
var
  i: Integer;
begin
  AMeshObject.Clear;
  AMeshObject.Vertices.Capacity := _Nverts;
  AMeshObject.Normals.Capacity := _Nverts;
  AMeshObject.Colors.Capacity := _Nverts;
  with TFGVertexIndexList.CreateOwned(AMeshObject.FaceGroups) do
  begin
    Mode := fgmmTriangles;
    for i := 0 to _Nverts - 1 do
    begin
      AMeshObject.Vertices.Add(PVertices^[i].P);
      AMeshObject.Normals.Add(PVertices^[i].N);
      // AMeshObject.Normals.Add(VectorScale(PVertices^[i].N, -1));
      // AMeshObject.Colors.Add(VectorMake(0.890, 0.855, 0.788, Alpha));
    end;

    for i := 0 to _Ntrigs - 1 do
      // with PTriangles^[i] do VertexIndices.Add(v1, v2, v3);
      with PTriangles^[i] do
        VertexIndices.Add(v3, v2, v1);
  end;
end;



end.
