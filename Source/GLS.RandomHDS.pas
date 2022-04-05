//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.RandomHDS;

(*
  This unit provides tools and objects to generate random Height Data Sources
  that can be used with TGLTerrainRenderer. General properties are defined in
  TGLBaseRandomHDS, but the main object is TGLCustomRandomHDS,
  which defines all the basic functionalities; however, it is an abstract class.
  So far, it has only one descendent, TGLFractalHDS, which implements the fractal
  middle-point displacement algorithm (aka plasma, aka diamond-square).
  The actual algorithms are independent functions called by the objects so they
  can also be used in other contexts. Basically, only the BuildHeightField
  method has to be overriden, and properties
  particular to the algorithm added (see TGLFractalHDS implementation). The
  BuildHeightField should contain a call to the algorithm function (or the algorithm
  itself, and MUST set the following fields: fSize, fMinHeight, fMaxHeight and
  fRangeHeight.

  Landscape generation consists in the following steps:
  1° Generate height field
  2° Modify it through erosion, sea surface, etc.
  3° Compute light and shadows
  4° Build the texture and assign it to a material created for this purpose

  The above classes generate isolated landscapes. They can be tiled in an
  infinite landscape through TGLTiledRndLandscape. The function of this class
  is to maintain a list of landscapes (called thereafter "landtiles"), to build
  and free them when needed.
  The TGLFractalArchipelago is an example of such a landscape generating an
  infinite landscape made of fractal islands.

  Although this structure may appear complex, the programmer just need to
  instanciate a TGLFractalArchipelago and to set its properties to get it running
  transparently. See the FractalLandscape and FractalArchipelago demos to see
  how to use these objects and what the various properties mean.

  Additional comments can be found in the code in the particular procedures.
  These components can be freely used. So far, you have to declare and
  create this component manually in your code and link it to a TGLTerrainRenderer.
  If you know how to make a registered component, please do it.
*)

interface

uses
  Winapi.OpenGL,
  Winapi.Windows,
  System.Classes,
  System.Math,
  System.SysUtils,
  System.UITypes,
  System.Contnrs,
  Vcl.Graphics,
  Vcl.Imaging.jpeg,
  Vcl.Forms,

  GLS.OpenGLTokens,
  GLS.Scene,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.HeightData,
  GLS.TerrainRenderer,
  GLS.Texture,
  GLS.Color,
  GLS.Coordinates,
  GLS.RenderContextInfo,
  GLS.Material,
  GLS.Context;

type
  TSeaErosion = record
    Enabled: boolean;
    BeachHeight: single;
  end;

  TRainErosion = record
    Enabled: boolean;
    ErosionRate: single;
    DepositRate: single;
  end;

  TLifeErosion = record
    Enabled: boolean;
    Robustness: single;
  end;

  TFractionErosion = record
    Enabled: boolean;
    Slope: single;
  end;

  TLandTileInfo = record
    x, z: integer; // Coordinates of the landtile. Used to generate the seed
    State: TGLHeightDataState; // Preparation status of the landtile
  end;

  TSteps = record
    Enabled: boolean;
    Count: integer;
  end;

  TMapOfSingle = array of array of single;
  TMapOfVector = array of array of TGLVector;

  TGLBaseRandomHDS = class;

  // Function type to use for topography-based texture
  TOnDrawTexture = function(const Sender: TGLBaseRandomHDS; x, y: integer; z: double; Normal: TGLVector): TGLColorVector of object;

  TSingleClamp = procedure(var x, y: single) of object;
  TIntegerClamp = procedure(var x, y: integer) of object;

  (* This class introduces all the basic properties of random landscape. No method
    implemented though. It is used as a descendant for
    - TGLCustomRandomLandscape: one tile landscape (cyclic or not)
    - TGLTiledRndLandscape: "infinite" landscapes (grids of TGLCustomRandomLandscape) *)
  TGLBaseRandomHDS = class(TGLHeightDataSource)
  private
    FSteps: TSteps;
    FLandCover: boolean;
    procedure SetOnDrawTexture(const Value: TOnDrawTexture);
    procedure SetSteps(const Value: TSteps);
    procedure SetLandCover(const Value: boolean);
  protected
    FSeed: integer;
    FSize: integer;
    FMaterialName: string;
    FLighting: boolean;
    FLightDirection: TGLVector;
    FTerrainRenderer: TGLTerrainRenderer;
    FLightColor: TGLColorVector;
    FShadows: boolean;
    FSea: boolean;
    FSeaLevel: single;
    FAmbientLight: single;
    FTaskProgress: integer;
    FTextureScale: integer;
    FErosionByFraction: TFractionErosion;
    FLightSmoothing: boolean;
    FCyclic: boolean;
    FSeaTransparency: single;
    FPrimerLandscape: boolean;
    FLandTileInfo: TLandTileInfo;
    FOnDrawTexture: TOnDrawTexture;
    function OnDrawTextureDefault(const Sender: TGLBaseRandomHDS; x, y: integer; z: double; Normal: TGLVector): TGLColorVector;
    procedure SetSeed(const Value: integer);
    procedure SetMaterialName(const Value: string);
    procedure SetLighting(const Value: boolean);
    procedure SetLightDirection(const Value: TGLVector);
    procedure SetTerrainRenderer(const Value: TGLTerrainRenderer); virtual; abstract;
    procedure SetLightColor(const Value: TGLColorVector);
    procedure SetShadows(const Value: boolean);
    procedure SetSea(const Value: boolean);
    procedure SetSeaLevel(const Value: single);
    procedure SetAmbientLight(const Value: single);
    procedure SetErosionByRain(const Value: TRainErosion);
    function GetErosionByRain: TRainErosion;
    procedure SetErosionBySea(const Value: TSeaErosion);
    procedure SetTextureScale(const Value: integer);
    procedure SetErosionByLife(const Value: TLifeErosion);
    procedure SetErosionByFraction(const Value: TFractionErosion);
    procedure SetLightSmoothing(const Value: boolean);
    procedure SetSeaTransparency(const Value: single);
    procedure SetPrimerLandscape(const Value: boolean);
    function GetSeaLevel: single;
    function GetSeaTransparency: single;
    procedure SetLandTileInfo(const Value: TLandTileInfo);
    function GetLandTileInfo: TLandTileInfo;
    procedure SetCyclic(const Value: boolean); virtual; abstract;
  public
    FErosionByRain: TRainErosion;
    FErosionBySea: TSeaErosion;
    FErosionByLife: TLifeErosion;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Usually white, but you can generate e.g.sunset ambiance by setting it to red
    property LightColor: TGLColorVector read FLightColor write SetLightColor;
    // Light is parallel (sun light)
    property LightDirection: TGLVector read FLightDirection write SetLightDirection;
    (* This function must be supplied by the user. Here he/she can define which
      colour to use depending on coordinates, elevation and normal. This provides
      a great flexibility. If no function is supplied (OnDrawTexture=nil), a default
      texture function is used (very basic, just blue and green). *)
    property OnDrawTexture: TOnDrawTexture read FOnDrawTexture write SetOnDrawTexture;
  published
    property AmbientLight: single read FAmbientLight write SetAmbientLight;
    (* If true, the landscape can be tiled to itself seamlessly.
      If false, the landscape is an isolated square. *)
    property Cyclic: boolean read FCyclic write SetCyclic;
    // Erosion parameters. See associated record types
    property ErosionByFraction: TFractionErosion read FErosionByFraction write SetErosionByFraction;
    property ErosionByLife: TLifeErosion read FErosionByLife write SetErosionByLife;
    property ErosionByRain: TRainErosion read FErosionByRain write SetErosionByRain;
    property ErosionBySea: TSeaErosion read FErosionBySea write SetErosionBySea;
    property LandCover: boolean read FLandCover write SetLandCover;
    // Enable or disable all lighting effects
    property Lighting: boolean read FLighting write SetLighting;
    // True by default. You can gain a little speed by disabling it.
    property LightSmoothing: boolean read FLightSmoothing write SetLightSmoothing;
    (* Not used *)
    property MaterialName: string read FMaterialName write SetMaterialName;
    (* If true, the height-field will not be emptied and generation will take the
      existing heights to shape the new landscape *)
    property PrimerLandscape: boolean read FPrimerLandscape write SetPrimerLandscape;
    // Enable the sea surface truncation
    property Sea: boolean read FSea write SetSea;
    // Sea level
    property SeaLevel: single read GetSeaLevel write SetSeaLevel;
    // Depth at which the sea bottom becomes invisible. See DoSea for more information
    property SeaTransparency: single read GetSeaTransparency write SetSeaTransparency;
    (* Seed used by the random generator. Each seed generate a different
      reproductible landscape. *)
    property Seed: integer read FSeed write SetSeed;
    // Enable shadow casting. May take some time for large Depth.
    property Shadows: boolean read FShadows write SetShadows;
    property Steps: TSteps read FSteps write SetSteps;
    // TerrainRenderer used to render the HDS.
    property TerrainRenderer: TGLTerrainRenderer read FTerrainRenderer write SetTerrainRenderer;
    (* Defines how many texture pixels are drawn per height-field cell. The larger
      this number the better the quality of the resulting image, but it takes a
      more time to compute. Good results are got between 1 and 5. *)
    property TextureScale: integer read FTextureScale write SetTextureScale;
  end;

  (* Base structure for all random landscape objects. It can't be used directly
    since its BuildHeightField procedure is abstract. Use one of its descendants instead. *)
  TGLCustomRandomHDS = class(TGLBaseRandomHDS)
  private
    FSlave: boolean;
    FMaxHeight: single;
    FMinHeight: single;
    FRangeHeight: single;
    FTask: string;
    FSingleConstrain: TSingleClamp;
    FIntegerConstrain: TIntegerClamp;
    FKeepNormals: boolean;
    function GetHeight(x, y: integer): single;
    procedure SetHeight(x, y: integer; const Value: single);
    procedure SetKeepNormals(const Value: boolean);
  protected
    procedure SetTerrainRenderer(const Value: TGLTerrainRenderer); override;
    procedure SetCyclic(const Value: boolean); override;
    procedure BoundaryClamp(var x, y: single); overload;
    procedure BoundaryClamp(var x, y: integer); overload;
    procedure CyclicClamp(var x, y: single); overload;
    procedure CyclicClamp(var x, y: integer); overload;
    // TGLTerrainRenderer event handler
    procedure GetTerrainBounds(var l, t, r, b: single);
    // This procedure MUST be called by the descendent of TGLBaseRandomHDS
    procedure SetSize(const aSize: integer);
  public
    FHeight: TMapOfSingle;
    FLightMap: TMapOfSingle;
    FNormal: TMapOfVector;
    // Upper bounds of the tile
    function BoundaryX: integer;
    function BoundaryZ: integer;
    // Generate the heightfield array, based on the topographical properties
    procedure BuildHeightField; overload; virtual; abstract;
    (* Provide an automated way to build a landscape. However, a greater control can
      be achieved by calling the various procedures manually (they are public methods)
      as one gets a sligthly different result depending on the sequence of erosion
      and sea steps. *)
    procedure BuildLandscape;
    (* - Compute the light effects
       - Compute the casted shadows
       - Perform a basic smoothing if TextureScale>1 *)
    procedure BuildLightMap; overload;
    procedure BuildLightMap(const aLightDirection: TGLVector); overload;
    // Normals are needed for lighting and slope-based textures
    procedure BuildNormals;
    (* For every pixel of the texture, computes slope and interpolated height and
      sends these information to a user-supplied function (OnDrawTexture), whose
      result is a TGLColorVector. If no OnDrawTexture is supplied, a basic default
      texture will be used. *)
    procedure BuildTexture;
    // Fill the heightfield with "Empty" values (-999)
    procedure ClearHeightField;
    // Fill the light map with 1
    procedure ClearLightMap;
    (* Constrain x,y to be in the boundaries of the height field array. This is
      done in two way depending on the kind of landscape:
      Cyclic landscapes: 		mod
      Non-cyclic landscape:	clamp *)
    procedure ConstrainCoordinates(var x, y: single); overload;
    procedure ConstrainCoordinates(var x, y: integer); overload;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Enforces an identical height on the opposing edges of the landscape
    procedure DoCyclicBoundaries;
    (* Not yet implemented *)
    procedure DoErosionByFraction;
    (* Just a smoothing. Should be done last as it improves the look of other
      erosion effects. Too much biological erosion can ruin erase their results
      though. Some tweaking may be needed *)
    procedure DoErosionByLife;
    (* Create sharp valleys and canyons. If DepositRate>0, it will also fill the
      low pools, producing flat "lakes" and "ponds".
      Drop some rain on every cell of the landscape and let it run downward,
      taking soil on its way. When it arrives into a pool,
      let it deposit all that has been eroded *)
    procedure DoErosionByRain;
    // Create a beach and a cliff around the islands
    procedure DoErosionBySea;
    (* Cut all elevations lower than sea level. If Transparency>0, the sea surface
      will not be flat, but a slight elevation change (unperceptible in 3D view)
      allow to fake transparency in the OnDrawTexture. *)
    procedure DoSea;
    // Discretise the heigthfield in a chosen number of steps
    procedure DoSteps;
    (* x and y are range-checked and constrained into the array. This slows down
      computation. If you don't need to range-check (this is mainly useful in
      cyclic landscapes when you need a seamless joint), call fHeigth instead
      (this is a protected field, therefore only accessible from TGLFractalHDS
      descendents. *)
    property Heights[x, y: integer]: single read GetHeight write SetHeight;
    // Range checked
    (* A specific implementation of THeightDataSource.InterpolatedHeight *)
    function Interpolate(x, y: single): single;
    // Keep the array of normals for future use
    property KeepNormals: boolean read FKeepNormals write SetKeepNormals;
    (* Property used by TGLTiledRndLandscape to know where the landtile is located
      and other parameters. See tLandTileInfo *)
    property LandTileInfo: TLandTileInfo read GetLandTileInfo write SetLandTileInfo;
    // Range checking
    function PointInMap(const x, y: single): boolean; overload;
    function PointInMap(const x, y: integer): boolean; overload;
    // Store the minimum and maximum elevations
    property MaxHeight: single read FMaxHeight;
    property MinHeight: single read FMinHeight;
    // Vector normal to the terrain at the position
    function Normal(const Position: TGLVector): TGLVector;
    // Max height - min height
    property RangeHeight: single read FRangeHeight;
    (* Scale of the Terrain Renderer. They are set so as giving a identical
      vertical/horitontal ratio with any size. Therefore, Scale.X=Scale.Y=1 and
      only Scale.Z varies. If you want to increase the landscape scale, the best way
      would be to place the Terrain Renderer in a DummyCube and rescale it. *)
    function Scale: TGLCoordinates;
    (* Size of the square height array. With the middle-point algorithm, it is always
      Size = 2^N+1. In a cyclic landscape, the last row and columns are identical
      to the first. *)
    property Size: integer read FSize;
    // A height rescaled between 0 and 1000 for
    function StandardisedHeight(const x, y: integer): single;
    (* When long computations are running, this property contains the operation
      beeing processed. *)
    property Task: string read FTask;
    // A value between 0 and 100 indicating the percentage of completion
    property TaskProgress: integer read FTaskProgress;
    // Use these boundaries with non-cyclic landscapes to constrain camera movements.
    function XMoveBoundary: single;
    function ZMoveBoundary: single;
    // tTerrainRender event handler
    procedure StartPreparingData(heightData: TGLHeightData); override;
  published
    property Cyclic: boolean read FCyclic write SetCyclic;
  end;

  // Random landscape based on the middle-point displacement algorithm
  TGLFractalHDS = class(TGLCustomRandomHDS)
  private
    FAmplitude: integer;
    FDepth: integer;
    FRoughness: single;
    procedure SetAmplitude(const Value: integer);
    procedure SetDepth(const Value: integer);
    procedure SetRoughness(const Value: single);
  public
    procedure BuildHeightField; overload; override;
    procedure BuildHeightField(const aDepth, aSeed, aAmplitude: integer); overload;
    constructor Create(AOwner: TComponent); override;
  published
    // Proportional to the difference between highest and lowest altitude.
    property Amplitude: integer read fAmplitude write SetAmplitude;
    (* Number of levels in the fractal process. Depth defines the size of the
      landscape: Size = 2^Depth+1 . Good results are got with Depth>=6. Above 10
      the landscape takes a lot of time to be generated. *)
    property Depth: integer read fDepth write SetDepth;
    // The lower this parameter, the smoother the landscape. Takes value between 0 and 1
    property Roughness: single read fRoughness write SetRoughness;
  end;

  // TMapOfLandscapes: array of array of TGLBaseRandomHDS;

  TGLLandTile = TGLCustomRandomHDS;

  TRelativeCoordinate = record
    DX, DZ: integer
  end;
  TOnCreateLandTile =  procedure(x, z, Seed: integer; var aLandscape: TGLLandTile) of object;
  TIsDefaultTile =  function(X, Z: integer): boolean of object;

  TGLTiledRndLandscape = class(TGLBaseRandomHDS)
  private
    FLandTileComputing: boolean; // Is a landtile being computed?
    FExtentX: integer;
    FExtentZ: integer;
    FExtentXhalf: integer;
    FExtentZhalf: integer;
    fLandTileSize: integer;
    FSingleConstrain: TSingleClamp;
    FIntegerConstrain: TIntegerClamp;
    FTerrainRenderer: TGLTerrainRenderer;
    FCamera: TGLCamera;
    fOnCreateLandTile: TOnCreateLandTile;
    fOnCreateDefaultTile: TStartPreparingDataEvent;
    FIsDefaultTile: TIsDefaultTile;
    FSeed: integer;
    fBaseSeed: integer;
    fComputedLandTile: TGLLandTile;
    FLandTileCapacity: integer;
    FGenerationRadius: integer;
    FLandTileDensity: single;
    procedure fDefaultOnCreateDefaultTile(heightData: TGLHeightData);
    function fDefaultIsDefaultTile(x, z: integer): boolean;
    procedure SetExtentX(const Value: integer);
    procedure SetExtentZ(const Value: integer);
    procedure SetOnCreateLandTile(const Value: TOnCreateLandTile);
    procedure SetCamera(const Value: TGLCamera);
    procedure SetIsDefaultTile(const Value: TIsDefaultTile);
    procedure SetSeed(const Value: integer);
    procedure SetOnCreateDefaultTile(const Value: TStartPreparingDataEvent);
    function GetTask: string;
    function GetTaskProgress: integer;
    procedure SetLandTileCapacity(const Value: integer);
    procedure SetGenerationRadius(const Value: integer);
    procedure SetLandTileDensity(const Value: single);
  protected
    FGenRadius: array of TRelativeCoordinate;
    FOldCamX: integer;
    FOldCamZ: integer;
    FMapUpdating: boolean;
    FLandTiles: tComponentList;
    procedure BoundaryClamp(var x, z: single); overload;
    procedure BoundaryClamp(var x, z: integer); overload;
    procedure ComputeLandTile(const aX, aZ: integer; var NewLandTile: TGLLandTile); virtual;
    procedure CyclicClamp(var x, z: single); overload;
    procedure CyclicClamp(var x, z: integer); overload;
    // TGLTerrainRenderer event handler
    procedure GetTerrainBounds(var l, t, r, b: single);
    function LandTileSeed(x, z: integer): integer;
    property OnCreateDefaultTile: TStartPreparingDataEvent read fOnCreateDefaultTile write SetOnCreateDefaultTile;
    procedure SetCyclic(const Value: boolean); override;
    // This procedure MUST be called by the descendent of TGLRandomArchipelago
    procedure SetSize(const aSize: integer);
    // Sort landtiles from the closest to the farthest
    function fSortLandscapes(Item1, Item2: Pointer): integer;
    // Preparing Land Tile Data
    procedure PrepareLandTileData(HeightData: TGLHeightData; LandTile: TGLLandTile);
    (* Terrain Render event handler *)
    procedure SetTerrainRenderer(const Value: TGLTerrainRenderer); override;
  public
    procedure ApplyLighting(var aLandTile: TGLLandTile);
    procedure ApplyTexture(var aLandTile: TGLLandTile);
    procedure ApplyTopography(var aLandTile: TGLLandTile);
    procedure CameraPosition(var TileX, TileZ: integer);
    procedure CleanUp;
    (* Constrain x,y to be in the boundaries of the height field array. This is
      done in two way depending on the kind of landscape:
      Cyclic landscapes: 		mod
      Non-cyclic landscape:	clamp *)
    procedure ConstrainCoordinates(var x, z: single); overload;
    procedure ConstrainCoordinates(var x, z: integer); overload;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Compute the landtile containing (x,z)
    procedure FindLandTile(const x, z: single; var TileX, TileZ: integer);
    // Build the first landtile and position the camera. Must be called first.
    procedure Initialize(const aX, aZ: single); virtual;
    (* User-supplied function determining if this landtile will be built by the
      OnCreateDefaultTile or if a landscape will be generated. *)
    property IsDefaultTile: TIsDefaultTile read FIsDefaultTile write SetIsDefaultTile;
    // Number of landtile in memory
    function LandtileCount: integer;
    // Size of a landtile. Must be a power of two
    property LandTileSize: integer read fLandTileSize;
    (* User-specified event handler containing the particular code for tile generation *)
    property OnCreateLandTile: TOnCreateLandTile read fOnCreateLandTile write SetOnCreateLandTile;
    (* When long computations are running, this property contains the operation
      beeing processed. *)
    property Task: string read GetTask;
    // A value between 0 and 100 indicating the percentage of completion
    property TaskProgress: integer read GetTaskProgress;
    // Distance between two landtiles
    function TileDistance(const x1, z1, x2, z2: integer): single;
    (* Square of distance between two landtiles. Use this function to compare
      two distances. *)
    function TileDistanceSquared(const x1, z1, x2, z2: integer): integer;
    (* This procedure check which landtiles must be generated or destroyed as a
      function of camera position. This is let to the descendent classes. *)
    procedure Update;
    property MapUpdating: boolean read fMapUpdating;
    // Use these boundaries with non-cyclic landscapes to constrain camera movements.
    function XMoveBoundary: single;
    function ZMoveBoundary: single;
    procedure StartPreparingData(heightData: TGLHeightData); override;
  published
    property Camera: TGLCamera read FCamera write SetCamera;
    property Cyclic: boolean read FCyclic write SetCyclic;
    (* Dimensions of the "infinite" landscape. Can be set very high. These parameters
      have neither memory nor speed consequence. They are mainly used to compute
      a unique seed for each landtile *)
    property ExtentX: integer read FExtentX write SetExtentX;
    property ExtentZ: integer read FExtentZ write SetExtentZ;
    (* Distance at which a new landtile begin to be built. Increasing this value
      allows for a higher camera speed but it will also increase the memory requirements. *)
    property GenerationRadius: integer read FGenerationRadius write SetGenerationRadius;
    // Number of landtile to keep in memory. Should not be modified.
    property LandTileCapacity: integer read FLandTileCapacity write SetLandTileCapacity;
    // Probability that a given landtile is non-default
    property LandTileDensity: single read FLandTileDensity write SetLandTileDensity;
    // Base seed for the entire archipelago
    property Seed: integer read FSeed write SetSeed;
    // Terrain renderer linked to the HDS. Must be set just after creation.
    property TerrainRenderer: TGLTerrainRenderer read FTerrainRenderer write SetTerrainRenderer;
  end;

  TGLFractalArchipelago = class(TGLTiledRndLandscape)
  private
    FDepth: integer;
    FRoughnessMax: single;
    FRoughnessMin: single;
    FAmplitudeMin: integer;
    FAmplitudeMax: integer;
    FSeaDynamic: boolean;
    FSeaMaterialName: string;
    FWaveAmplitude: single;
    FWaveSpeed: single;
    function GetIslandDensity: single;
    (* PostRender event handler drawing a static water plane between islands
      Code borrowed from Eric's Archipelago GLScene advanced demo *)
    procedure FPostRenderSeaStatic(var rci: TGLRenderContextInfo; var HeightDatas: TList);
    // Sea with waves. Borrowed from GLS Archipelago advanced demo
    procedure FPostRenderSeaDynamic(var rci: TGLRenderContextInfo; var HeightDatas: TList);
    procedure SetIslandDensity(const Value: single);
    procedure SetDepth(const Value: integer);
    procedure SetRoughnessMax(const Value: single);
    procedure SetRoughnessMin(const Value: single);
    procedure SetAmplitudeMax(const Value: integer);
    procedure SetAmplitudeMin(const Value: integer);
    procedure SetSeaDynamic(const Value: boolean);
    procedure SetSeaMaterialName(const Value: string);
    procedure SetWaveAmplitude(const Value: single);
    procedure SetWaveSpeed(const Value: single);
  protected
    procedure SetTerrainRenderer(const Value: TGLTerrainRenderer); override;
    procedure fOnCreateLandTile(aX, aZ, aSeed: integer; var aLandscape: TGLLandTile);
    procedure fOnCreateDefaultTile(heightData: TGLHeightData);
  public
    procedure ComputeLandTile(const aX, aZ: integer; var NewLandTile: TGLLandTile); override;
    constructor Create(AOwner: TComponent); override;
  published
    // Ranges for the amplitude parameter in the fractal algorithm
    property AmplitudeMax: integer read FAmplitudeMax write SetAmplitudeMax;
    property AmplitudeMin: integer read FAmplitudeMin write SetAmplitudeMin;
    // Depth of the fractal algorithm
    property Depth: integer read fDepth write SetDepth;
    (* A wrapper for LandtileDensity. This is the probabilty for a landtile to
      contain an island. *)
    property IslandDensity: single read GetIslandDensity write SetIslandDensity;
    // Ranges for the roughness parameter in the fractal algorithm
    property RoughnessMax: single read FRoughnessMax write SetRoughnessMax;
    property RoughnessMin: single read FRoughnessMin write SetRoughnessMin;
    // If true, the sea will show dynamic waves. Slow.
    property SeaDynamic: boolean read FSeaDynamic write SetSeaDynamic;
    (* Reference to a material in the TerrainRenderer's material library. This
      material will be used to drape the water plane. *)
    property SeaMaterialName: string read FSeaMaterialName write SetSeaMaterialName;
    // Size of the waves
    property WaveAmplitude: single read FWaveAmplitude write SetWaveAmplitude;
    property WaveSpeed: single read FWaveSpeed write SetWaveSpeed;
  end;

(* Texture functions *)
function LoadJPGtexture(const JpgName: string): tBitmap;
function NoisyColor(const Color: tColor; const Noise: single = 0.05): TGLColorVector;
function TextureGreen(const x, y: integer): TGLColorVector;
function TextureBlue(const x, y: integer): TGLColorVector;
function TextureSand(const x, y: integer): TGLColorVector;
function TextureBrownSoil(const x, y: integer): TGLColorVector;
function TextureDarkGreen(const x, y: integer): TGLColorVector;
function TextureDarkGray(const x, y: integer): TGLColorVector;
function TextureWhite(const x, y: integer): TGLColorVector;

(* Random HDS functions *)
(* Fractal algorithm based on the middle-point displacement method. It is built in
  a way that it can be juxtaposed seamlessly to itself (cyclic boundaries) *)
procedure FractalMiddlePointHDS(const aDepth, aSeed, aAmplitude: integer; const aRoughness: single; aCyclic: boolean;
  var z: TMapOfSingle; var MinZ, MaxZ: single);
procedure InitializeRandomGenerator(const Seed: integer);

(* Landscape primers *)
procedure PrimerNull(var z: TMapOfSingle);
procedure PrimerIsland(LowZ, HighZ: single; var z: TMapOfSingle);

const
  VerticalScalingFactor = 128;

// ==========================================================================
implementation
// ==========================================================================

const // Neighbourhood vectors and weight
  NeighX: array [0 .. 8] of integer = (-1, 0, 1, 1, 1, 0, -1, -1, 0);
  NeighY: array [0 .. 8] of integer = (-1, -1, -1, 0, 1, 1, 1, 0, 0);
  NeighW: array [0 .. 8] of single = (1 / 1.4142, 1, 1 / 1.4142, 1, 1 / 1.4142, 1, 1 / 1.4142, 1, 2);
  SumWeights = 4 / 1.4142 + 4 + 2;
  Empty: single = -999;
  VSF = VerticalScalingFactor;

var
  rhdsStartTime: cardinal;
  rhdsLandscapeCounter: cardinal = 0;
  // Counter	:tTickCounter;

function LoadJPGtexture(const JpgName: string): tBitmap;
var
  Jpg: TJPEGImage;
begin
  Result := tBitmap.Create;
  Jpg := TJPEGImage.Create;
  Jpg.LoadFromFile(JpgName);
  Result.Assign(Jpg);
  Jpg.Free;
end;

function NoisyColor(const Color: tColor; const Noise: single = 0.05): TGLColorVector;
var
  r: single;
begin
  Result := ConvertWinColor(Color);
  r := random * Noise;
  AddVector(Result, r);
end;

function TextureSand(const x, y: integer): TGLColorVector;
begin
  Result := NoisyColor($0071D8FF);
end;

function TextureBrownSoil(const x, y: integer): TGLColorVector;
begin
  Result := NoisyColor($00008BBF);
end;

function TextureDarkGreen(const x, y: integer): TGLColorVector;
begin
  Result := NoisyColor($00004000);
end;

function TextureDarkGray(const x, y: integer): TGLColorVector;
begin
  Result := NoisyColor(clDkGray);
end;

function TextureWhite(const x, y: integer): TGLColorVector;
begin
  Result := NoisyColor(clWhite);
end;

function TextureBlue(const x, y: integer): TGLColorVector;
begin
  Result := NoisyColor(clBlue);
end;

function TextureGreen(const x, y: integer): TGLColorVector;
begin
  Result := NoisyColor(clGreen);
end;

procedure InitializeRandomGenerator(const Seed: integer);
var
  i: integer;
begin
  RandSeed := Seed;
  for i := 1 to 50 do
    random; // Pre-heat the generator
end;

//-----------------------------------
// TGLBaseRandomHDS
//-----------------------------------
constructor TGLBaseRandomHDS.Create(AOwner: TComponent);
begin
  inherited;
  Inc(rhdsLandscapeCounter);
  Name := Format('RandomLandscape%d', [rhdsLandscapeCounter]);
  FLightColor := VectorMake(1, 1, 1);
  FLightDirection := VectorMake(-1, 0, -1);
  FAmbientLight := 0.5;
  FTextureScale := 1;
  FMaterialName := '';
  FLighting := True;
  FLightSmoothing := True;
  Cyclic := True;
  FSeed := RandSeed;
  FSeaLevel := 0.0;
  FErosionBySea.BeachHeight := 0.01;
  FErosionBySea.Enabled := False;
  FErosionByRain.Enabled := True;
  FErosionByRain.ErosionRate := 0.5;
  FErosionByRain.DepositRate := FErosionByRain.ErosionRate;
  FErosionByLife.Enabled := True;
  FErosionByLife.Robustness := 1;
  FLandTileInfo.State := hdsNone;
end;

destructor TGLBaseRandomHDS.Destroy;
begin
  inherited;
end;

function TGLBaseRandomHDS.GetSeaLevel: single;
begin
  Result := FSeaLevel / VSF; // factor used in tTerrainRender
end;

function TGLBaseRandomHDS.GetSeaTransparency: single;
begin
  Result := FSeaTransparency / VSF; // factor used in tTerrainRender
end;

function TGLBaseRandomHDS.GetErosionByRain: TRainErosion;
begin
  Result := FErosionByRain;
end;

function TGLBaseRandomHDS.GetLandTileInfo: TLandTileInfo;
begin
  Result := FLandTileInfo;
end;

function TGLBaseRandomHDS.OnDrawTextureDefault(const Sender: TGLBaseRandomHDS; x, y: integer; z: double; Normal: TGLVector)
  : TGLColorVector;
begin
  if z > Sender.SeaLevel * VSF then
    Result := TextureGreen(x, y)
  else
    Result := TextureBlue(x, y);
end;

procedure TGLBaseRandomHDS.SetAmbientLight(const Value: single);
begin
  FAmbientLight := Value;
end;

procedure TGLBaseRandomHDS.SetErosionByFraction(const Value: TFractionErosion);
begin
  FErosionByFraction := Value;
end;

procedure TGLBaseRandomHDS.SetErosionByLife(const Value: TLifeErosion);
begin
  FErosionByLife := Value;
end;

procedure TGLBaseRandomHDS.SetErosionByRain(const Value: TRainErosion);
begin
  FErosionByRain := Value;
end;

procedure TGLBaseRandomHDS.SetErosionBySea(const Value: TSeaErosion);
begin
  FErosionBySea := Value;
end;

procedure TGLBaseRandomHDS.SetLandCover(const Value: boolean);
begin
  FLandCover := Value;
end;

procedure TGLBaseRandomHDS.SetLandTileInfo(const Value: TLandTileInfo);
begin
  FLandTileInfo := Value;
end;

procedure TGLBaseRandomHDS.SetLightColor(const Value: TGLColorVector);
begin
  FLightColor := Value;
end;

procedure TGLBaseRandomHDS.SetLightDirection(const Value: TGLVector);
var
  v: TGLVector;
begin
  v := Value;
  NormalizeVector(v);
  FLightDirection := Value;
end;

procedure TGLBaseRandomHDS.SetLighting(const Value: boolean);
begin
  FLighting := Value;
end;

procedure TGLBaseRandomHDS.SetLightSmoothing(const Value: boolean);
begin
  FLightSmoothing := Value;
end;

procedure TGLBaseRandomHDS.SetMaterialName(const Value: string);
begin
  FMaterialName := Value;
end;

procedure TGLBaseRandomHDS.SetOnDrawTexture(const Value: TOnDrawTexture);
begin
  if @Value <> nil then
    FOnDrawTexture := Value
  else
    FOnDrawTexture := OnDrawTextureDefault; // Basic texture event
end;

procedure TGLBaseRandomHDS.SetPrimerLandscape(const Value: boolean);
begin
  FPrimerLandscape := Value;
end;

procedure TGLBaseRandomHDS.SetSea(const Value: boolean);
begin
  FSea := Value;
end;

procedure TGLBaseRandomHDS.SetSeaLevel(const Value: single);
begin
  FSeaLevel := Value * VSF; // factor used in tTerrainRender
end;

procedure TGLBaseRandomHDS.SetSeaTransparency(const Value: single);
begin
  FSeaTransparency := Value * VSF; // factor used in tTerrainRender
end;

procedure TGLBaseRandomHDS.SetSeed(const Value: integer);
begin
  FSeed := Value;
end;

procedure TGLBaseRandomHDS.SetShadows(const Value: boolean);
begin
  FShadows := Value;
end;

procedure TGLBaseRandomHDS.SetSteps(const Value: TSteps);
begin
  FSteps := Value;
end;

procedure TGLBaseRandomHDS.SetTextureScale(const Value: integer);
begin
  FTextureScale := Value;
end;

// ----------------------------------------------
// TGLCustomRandomHDS
// ----------------------------------------------
procedure TGLCustomRandomHDS.BoundaryClamp(var x, y: single);
begin
  ClampValue(x, 0, FSize);
  ClampValue(y, 0, FSize);
end;

procedure TGLCustomRandomHDS.BoundaryClamp(var x, y: integer);
begin
  if x < 0 then
    x := 0
  else if x > FSize then
    x := FSize;
  if y < 0 then
    y := 0
  else if y > FSize then
    y := FSize;
end;

function TGLCustomRandomHDS.BoundaryX: integer;
begin
  Result := Round(FSize * Scale.x);
end;

function TGLCustomRandomHDS.BoundaryZ: integer;
begin
  Result := Round(FSize * Scale.z);
end;

procedure TGLCustomRandomHDS.BuildLandscape;
begin
  FTask := 'Landscape generation';
  FTaskProgress := 0;

  // Empty all height-field cells
  if not FPrimerLandscape then
    ClearHeightField;

  (* Build the basic fractal height field. It is mandatory and must always be
    called first. *)
  BuildHeightField;

  (* Various operations that reshape the height field. These procedures may be
    called in any order, although the one proposed here is the most natural.
    These procedures are optional *)
  if FErosionByRain.Enabled then
    DoErosionByRain;
  if FErosionByLife.Enabled then
    DoErosionByLife;
  if FErosionBySea.Enabled then
    DoErosionBySea;
  if FSteps.Enabled then
    DoSteps;

  (* Doing sea first would speeds up the following processes
    but the result would be slightly less realistic. In
    particular with transparency, you can have a nice effect
    of submarine valleys prolungating land canyons.
    This procedure is optional *)
  if FSea then
    DoSea;

  if FCyclic then
    DoCyclicBoundaries; // Ensures a seamless fit

  // Compute a normal for each vertex. Used by BuildLightMap and BuildTexture
  if FLandCover then
    BuildNormals;

  (* Add light effects. Either BuildLightMap or ClearLigthMap must be called.
    Used by BuildTexture. *)
  if FLighting and LandCover then
    BuildLightMap
  else
    ClearLightMap;

  (* Builds the actual texture. If it is not used, the terrain will be textured
    with its Material, if defined. *)
  if FLandCover then
    BuildTexture;

  (* Free memory. If you need often to recompute texture, you may want to keep
    one or both maps, providing the heightfield or the light source have not changed. *)
  if not FKeepNormals then
    FNormal := nil;
  FLightMap := nil;

  FTask := ' Updating terrain renderer';
  FTaskProgress := 0;
  Application.ProcessMessages;

  MarkDirty;
  // Tells the HDS that changes have been made (don't forget it or you'll get strange things)
end;

procedure TGLCustomRandomHDS.BuildLightMap;
var
  i, j, k, m, n: integer;
  x, y: single;
  t: single;
  v1, v2: TGLVector;
  l: TGLVector;
  Shade: single;
begin
  if FSize = 0 then
    exit;
  FTask := 'Light-map computation';
  FTaskProgress := 0;
  SetLength(FLightMap, (FSize + 1) * TextureScale, (FSize + 1) * TextureScale);
  l := FLightDirection;
  NormalizeVector(l);
  NegateVector(l);

  // Compute lighting
  for i := 0 to FSize do
  begin
    FTaskProgress := Round(i / FSize * 100);
    for j := 0 to FSize do
    begin
      Application.ProcessMessages;
      Shade := abs(VectorDotProduct(FNormal[i, j], l));
      ClampValue(Shade, 0);
      for k := i * TextureScale to (i + 1) * TextureScale - 1 do
        for n := j * TextureScale to (j + 1) * TextureScale - 1 do
          FLightMap[k, n] := Shade;
    end; // for
  end; // for i

  // Shadows
  if FShadows then
  begin
    FTask := 'Shadow casting';
    FTaskProgress := 0;
    l.x := l.x * Scale.x;
    l.y := l.y * VSF / Scale.y;
    l.z := l.z * Scale.z;
    for j := 0 to FSize do
    begin
      FTaskProgress := Round(j / FSize * 100);
      for i := 0 to FSize do
      begin
        if FLightMap[i * TextureScale, j * TextureScale] > 0 then
        begin // Don't look for shadow if the point is already shadowed
          v1 := VectorMake(i, FHeight[i, j], j); // Starting point
          for k := 2 to Round(FSize * 1.4) do
          begin // Quick and dirty ray-casting
            v2 := VectorCombine(v1, l, 1, k);
            // Casts a ray in direction of the sun
            x := Round(v2.x);
            y := Round(v2.z);
            if Interpolate(x, y) > v2.y then
            begin
              Application.ProcessMessages;
              for m := i * TextureScale to (i + 1) * TextureScale - 1 do
                for n := j * TextureScale to (j + 1) * TextureScale - 1 do
                  FLightMap[m, n] := 0;
              break; // Shadow caster found. No need to continue
            end; // if
          end; // for k
        end; // if
      end; // for j
    end; // for i
  end; // if

  // Smoothing
  if FLightSmoothing then
  begin
    FTask := 'Light-map smoothing';
    FTaskProgress := 0;
    for m := 1 to TextureScale - 1 do
    begin
      FTaskProgress := Round(m / TextureScale * 100);
      for j := 1 to High(FLightMap) - 1 do
      begin
        for i := 1 to High(FLightMap) - 1 do
        begin
          Application.ProcessMessages;
          t := 0;
          for k := 0 to 8 do
          begin
            t := t + FLightMap[i + NeighX[k], j + NeighY[k]] * NeighW[k];
          end; // for k
          FLightMap[i, j] := t / SumWeights;
        end; // for j
      end; // for i
    end; // for m
  end; // if
end;

procedure TGLCustomRandomHDS.BuildLightMap(const aLightDirection: TGLVector);
begin
  FLightDirection := aLightDirection;
  BuildLightMap;
end;

procedure TGLCustomRandomHDS.BuildNormals;
var
  i, j: integer;
  z0: single;
  v1, v2: TGLVector;
  n1: TGLVector;
  Normal: TGLVector;
begin
  FTask := 'Normal computation';

  for i := 0 to FSize do
  begin
    FTaskProgress := Round(i / FSize * 100);
    for j := 0 to FSize do
    begin
      Application.ProcessMessages;
      z0 := FHeight[i, j];
      Normal := NullHmgVector;
      MakeVector(v1, Scale.x, (Heights[i + 1, j] - z0) * Scale.y / VSF, 0);
      MakeVector(v2, 0, (Heights[i, j + 1] - z0) * Scale.y / VSF, Scale.z);
      Normal := VectorCrossProduct(v2, v1);
      NormalizeVector(Normal);
      MakeVector(v1, -Scale.x, (Heights[i - 1, j] - z0) * Scale.y / VSF, 0);
      MakeVector(v2, 0, (Heights[i, j + 1] - z0) * Scale.y / VSF, Scale.z);
      n1 := VectorCrossProduct(v1, v2);
      NormalizeVector(n1);
      Normal := VectorAdd(Normal, n1);
      MakeVector(v1, -Scale.x, (Heights[i - 1, j] - z0) * Scale.y / VSF, 0);
      MakeVector(v2, 0, (Heights[i, j - 1] - z0) * Scale.y / VSF, -Scale.z);
      n1 := VectorCrossProduct(v2, v1);
      NormalizeVector(n1);
      Normal := VectorAdd(Normal, n1);
      MakeVector(v1, Scale.x, (Heights[i + 1, j] - z0) * Scale.y / VSF, 0);
      MakeVector(v2, 0, (Heights[i, j - 1] - z0) * Scale.y / VSF, -Scale.z);
      n1 := VectorCrossProduct(v1, v2);
      NormalizeVector(n1);
      Normal := VectorAdd(Normal, n1);
      FNormal[i, j] := VectorScale(Normal, 0.25);
      // Average of the 4 adjacent normals
    end; // for j
  end; // for i
end;

procedure TGLCustomRandomHDS.BuildTexture;
type
  pRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [word] of TRGBTriple;

var
  Bmp: array of array of tBitmap;
  x0, y0: integer;
  xx, yy: integer;
  x, y: integer;
  nbTiles: integer;
  Side: integer;
  meancol: tColor;
  Line: pRGBTripleArray;

  function MeanColor(color1, color2: tColor): tColor;
  var
    r1, g1, b1: Byte;
    r2, g2, b2: Byte;
  begin
    r1 := (color1 and $000000FF);
    g1 := ((color1 and $0000FF00) shr 8);
    b1 := ((color1 and $00FF0000) shr 16);
    r2 := (color2 and $000000FF);
    g2 := ((color2 and $0000FF00) shr 8);
    b2 := ((color2 and $00FF0000) shr 16);
    Result := RGB((r1 + r2) div 2, (g1 + g2) div 2, (b1 + b2) div 2);
  end;

  procedure MakeRGBTriple(const Color: TGLColorVector; var RGBTriple: TRGBTriple);
  begin
    with RGBTriple do
    begin
      rgbtRed := Round(Color.x * 255);
      rgbtGreen := Round(Color.y * 255);
      rgbtBlue := Round(Color.z * 255);
    end; // with
  end;

  function ComputePixel(const x, y: integer): TRGBTriple;
  var
    i, j: integer;
    Shade: TGLColorVector;
    Cover: TGLColorVector;
    z: double;
  begin
    i := (x0 + x) div TextureScale;
    j := (y0 + y) div TextureScale;
    z := Interpolate((x0 + x) / TextureScale, (y0 + y) / TextureScale);
    Application.ProcessMessages;
    { Cover:=OnDrawTexture(Self,FLandTileInfo.x*fSize+x0+x,
      FLandTileInfo.z*fSize+y0+y,z,fNormal[i,j]); }
    Cover := OnDrawTexture(Self, x0 + x, y0 + y, z, FNormal[i, j]);
    Application.ProcessMessages;
    Shade := VectorScale(FLightColor, FLightMap[x0 + x, y0 + y]);
    Application.ProcessMessages;
    ScaleVector(Shade, Cover);
    Application.ProcessMessages;
    AddVector(Shade, VectorScale(Cover, FAmbientLight));
    Application.ProcessMessages;
    if Shade.x > 1 then
      Shade.x := 1;
    if Shade.y > 1 then
      Shade.y := 1;
    if Shade.z > 1 then
      Shade.z := 1;
    // if x=Side-1 then begin Shade[0]:=1; Shade[1]:=0; Shade[2]:=0; end;
    MakeRGBTriple(Shade, Result);
  end;

begin
  nbTiles := FSize div FTerrainRenderer.TileSize;
  SetLength(Bmp, nbTiles, nbTiles);
  Side := FTerrainRenderer.TileSize * TextureScale;
  FTask := 'Texture creation';
  FTaskProgress := 0;

  // Draw bitmap
  try
    for yy := 0 to (nbTiles) - 1 do
    begin
      FTaskProgress := Round(yy / nbTiles * 100);
      Application.ProcessMessages;
      y0 := yy * Side;
      for xx := 0 to (nbTiles) - 1 do
      begin
        x0 := xx * Side;
        Bmp[xx, yy] := tBitmap.Create;
        with Bmp[xx, yy] do
        begin
          PixelFormat := pf24bit;
          Width := Side;
          Height := Side;
          for y := 0 to Side - 1 do
          begin
            Line := ScanLine[y];
            for x := 0 to Side - 1 do
            begin
              Line[x] := ComputePixel(x, y);
            end; // for x
          end; // for y
        end; // with
      end; // for xx
    end; // for yy

    // Smoothes tile seams
    for yy := 0 to nbTiles - 2 do
    begin
      for xx := 0 to nbTiles - 2 do
      begin
        for x := 0 to Side - 1 do
        begin
          meancol := MeanColor(Bmp[xx, yy].Canvas.Pixels[Side - 1, x], Bmp[xx + 1, yy].Canvas.Pixels[0, x]);

          Bmp[xx, yy].Canvas.Pixels[Side - 1, x] := meancol;
          Bmp[xx + 1, yy].Canvas.Pixels[0, x] := meancol;
          meancol := MeanColor(Bmp[xx, yy].Canvas.Pixels[x, Side - 1], Bmp[xx, yy + 1].Canvas.Pixels[x, 0]);

          Bmp[xx, yy].Canvas.Pixels[x, Side - 1] := meancol;
          Bmp[xx, yy + 1].Canvas.Pixels[x, 0] := meancol;
        end; // for x
      end; // for xx
    end; // for yy

    // Upload into material library
    for yy := 0 to nbTiles - 1 do
    begin
      for xx := 0 to nbTiles - 1 do
      begin
        with FTerrainRenderer.MaterialLibrary.AddTextureMaterial(Format('%s%d%d', [Self.Name, xx, yy]), Bmp[xx, yy]) do
        begin
          // Material.Texture.MinFilter:=miNearest;
          Material.Texture.TextureWrap := twNone;
          Material.MaterialOptions := [moNoLighting];
          // Needed for correct look when lighting is enabled
        end; // with
        // Bmp[xx,yy].SaveToFile(Format('%s%d%d.bmp',[Self.Name,xx,yy]));
      end; // for xx
    end; // for yy
  finally
    for yy := 0 to nbTiles - 2 do
    begin
      for xx := 0 to nbTiles - 2 do
      begin
        Bmp[xx, yy].Free;
      end; // for xx
    end; // for yy
    Bmp := nil;
  end; // finally
end; // *)

(* procedure TGLCustomRandomHDS.BuildTexture2;
  var
  Bmp	:tBitmap;
  Mat	:TGLLibMaterial;
  x,y	:integer;
  i,j	:integer;
  Shade	:TGLColorVector;
  Cover	:TGLColorVector;
  z		:double;
  begin
  if not fTextureCreated then CreateTexture;
  Mat:=FTerrainRenderer.MaterialLibrary.LibMaterialByName(MaterialName);
  Bmp:=TBitmap.Create;
  fTask:='Texture creation';
  fTaskProgress:=0;

  //Draw bitmap
  try
  with Bmp do
  begin
  PixelFormat:=pf24bit;
  Width:=fSize*TextureScale;
  Height:=fSize*TextureScale;
  with Canvas do begin
  for y:=0 to fSize*TextureScale-1 do begin
  fTaskProgress:=Round(y/(fSize*TextureScale)*100);
  Application.ProcessMessages;
  for x:=0 to fSize*TextureScale-1 do
  begin
  i:=x div TextureScale;
  j:=y div TextureScale;
  z:=Interpolate(x/TextureScale,y/TextureScale);
  Cover:=OnDrawTexture(Self,x,y,z,fNormal[i,j]);
  Shade:=VectorScale(fLightColor.Color,fLightMap[x,y]);
  ScaleVector(Shade,Cover);
  AddVector(Shade,VectorScale(Cover,fAmbientLight));

  if Shade[0]>1 then Shade[0]:=1;
  if Shade[1]>1 then Shade[1]:=1;
  if Shade[2]>1 then Shade[2]:=1;
  Pixels[x,y]:=ConvertColorVector(Shade);
  end;//for x
  end;//for y
  end;//with
  end;//with
  //Bmp.SaveToFile('test.bmp');

  // Use it as texture
  with Mat.Material.Texture do
  begin
  Image.Assign(Bmp);
  Image.NotifyChange(Self);
  Enabled:=true;
  //MagFilter:=maNearest;
  //MinFilter:=miNearest;
  end;//with }
  Mat.NotifyUsersOfTexMapChange;
  finally
  Bmp.Free;
  end;//finally
  end;// *)

procedure TGLCustomRandomHDS.ClearHeightField;
begin
  PrimerNull(FHeight);
end;

procedure TGLCustomRandomHDS.ClearLightMap;
var
  x, y: integer;
begin
  SetLength(FLightMap, (FSize + 1) * TextureScale, (FSize + 1) * TextureScale);
  for y := 0 to High(FLightMap) do
  begin
    for x := 0 to High(FLightMap) do
    begin
      FLightMap[x, y] := 1;
    end; // for
  end; // for
end;

procedure TGLCustomRandomHDS.ConstrainCoordinates(var x, y: integer);
begin
  FIntegerConstrain(x, y);
end;

procedure TGLCustomRandomHDS.ConstrainCoordinates(var x, y: single);
begin
  FSingleConstrain(x, y);
end;

constructor TGLCustomRandomHDS.Create(AOwner: TComponent);
begin
  inherited;
  FLandCover := True;
  FOnDrawTexture := OnDrawTextureDefault;
end;

procedure TGLCustomRandomHDS.CyclicClamp(var x, y: single);
var
  ix, iy: integer;
  sx, sy: single;
begin
  ix := Trunc(x);
  sx := Frac(x);
  iy := Trunc(y);
  sy := Frac(y);
  x := (FSize + ix) mod FSize + sx;
  y := (FSize + iy) mod FSize + sy;
end;

procedure TGLCustomRandomHDS.CyclicClamp(var x, y: integer);
begin
  x := (FSize + x) mod FSize;
  y := (FSize + y) mod FSize;
end;

destructor TGLCustomRandomHDS.Destroy;
var
  x, y: integer;
  Mat: TGLLibMaterial;
begin
  FLandTileInfo.State := hdsNone;
  FHeight := nil;
  FLightMap := nil;
  FNormal := nil;
  try
    for y := 0 to (FSize div FTerrainRenderer.TileSize) - 1 do
    begin
      for x := 0 to (FSize div FTerrainRenderer.TileSize) - 1 do
      begin
        Mat := FTerrainRenderer.MaterialLibrary.LibMaterialByName(Format('%s%d%d', [Self.Name, x, y]));
        if Mat <> nil then
          Mat.Material.DestroyHandles;
      end; // for x
    end; // for y
  except
  end;
  if (FSlave) and (Owner <> nil) then
    with LandTileInfo do
      TGLTiledRndLandscape(Owner).MarkDirty(x * FSize, z * FSize, (x + 1) * FSize - 1, (z + 1) * FSize - 1);
  inherited;
end;

procedure TGLCustomRandomHDS.DoCyclicBoundaries;
var
  i: integer;
begin
  for i := 0 to FSize do
  begin
    FHeight[i, FSize] := FHeight[i, 0];
    FHeight[FSize, i] := FHeight[0, i];
  end; // for
end;

procedure TGLCustomRandomHDS.DoErosionByFraction;
begin

end;

procedure TGLCustomRandomHDS.DoErosionByLife;
var
  x, y, i: integer;
  z, z1: single;
begin
  // Smoothing by a 3-by-3 mean filter
  FTask := 'Erosion by life';
  FTaskProgress := 0;
  for y := 0 to FSize do
  begin
    FTaskProgress := Round(y / (FSize) * 100);
    for x := 0 to FSize do
    begin
      Application.ProcessMessages;
      z := FHeight[x, y] * FErosionByLife.Robustness;
      z1 := FErosionByLife.Robustness;
      for i := 0 to 7 do
      begin
        z := z + Heights[x + NeighX[i], y + NeighY[i]] * NeighW[i];
        z1 := z1 + NeighW[i];
      end; // for i
      FHeight[x, y] := z / z1;
    end; // for x
  end; // for y
end;

procedure TGLCustomRandomHDS.DoErosionByRain;
const
  Ks = 0.001; // Soil solubility
var
  j: integer;
  x0, y0: integer;
  x, y: integer;
  x1, y1: integer;
  minx, miny: integer;
  z, z1: single;
  MinZ: single;
  dz, mindz: single;
  Charge: double;
  From, Next: integer;
begin
  FTask := 'Rain erosion simulation';
  FTaskProgress := 0;
  minx := 0;
  miny := 0;
  MinZ := 0;
  Next := 0;

  // Rain
  for y0 := 0 to FSize do
  begin
    FTaskProgress := Round(y0 / (FSize) * 100);
    for x0 := 0 to FSize do
    begin
      Application.ProcessMessages;
      x := x0;
      y := y0;
      z := StandardisedHeight(x, y);
      Charge := 0;
      From := -1;
      while (FHeight[x, y] > FSeaLevel) // Not in the sea
        do
      begin
        mindz := MaxInt;
        for j := 0 to 7 do
        begin // Look for the largest slope
          if j = From then
            continue; // Never go backward
          x1 := (FSize + x + NeighX[j]) mod FSize; // Cyclic landscape
          y1 := (FSize + y + NeighY[j]) mod FSize;
          z1 := StandardisedHeight(x1, y1);
          dz := (z1 - z) * NeighW[j];
          if dz < mindz then
          begin
            minx := x1;
            miny := y1;
            MinZ := z1;
            mindz := dz;
            Next := j;
          end; // if
        end; // for j
        if (StandardisedHeight(minx, miny) <= SeaLevel) then
          break; // In the sea or out of map
        if MinZ < z then
        begin
          FHeight[x, y] := FHeight[x, y] - FErosionByRain.ErosionRate * Ks * FRangeHeight; // Erosion
          x := minx;
          y := miny;
          z := MinZ;
          From := (Next + 4) mod 8; // Opposite direction
          Charge := Charge + 1;
        end // if
        else
        begin // Fallen into a pool? Deposit the charge
          FHeight[x, y] := FHeight[x, y] + MinFloat(MinZ - z, FErosionByRain.DepositRate * Ks * FRangeHeight * Charge);
          break; // Go to next rain drop
        end; // else
      end; // while
    end; // for x0
  end; // for y0
end; // *)

(*
  Variants:

  procedure TGLBaseRandomHDS.DoErosionByRain(const Intensity: single);
  const
  NeighX	:array[0..7] of integer=(-1, 0, 1, 1, 1, 0,-1,-1);
  NeighY	:array[0..7] of integer=(-1,-1,-1, 0, 1, 1, 1, 0);
  NeighW   :array[0..7] of single=(1/1.4142,1,1/1.4142,1,1/1.4142,1,1/1.4142,1);
  type
  tFlow=record
  NextX,NextY	:integer;
  Slope		:single;
  Erosion		:integer;
  end;
  var
  Flow		:array of array of tFlow;
  i,j,jj,swap	:integer;
  x0,y0		:integer;
  x,y		:integer;
  x1,y1		:integer;
  minx,miny	:integer;
  z,z1,minz	:single;
  Charge		:integer;
  N			:integer;
  From,Next	:integer;
  Sig		:integer;
  c			:double;
  OldSlope	:single;
  dz,mindz	:single;
  begin
  c:=1/VSF/sqrt(sqr(Scale.X)+sqr(Scale.Z));
  // Water flow map computation
  SetLength(Flow,fSize+1,fSize+1);
  for y:=0 to fSize do begin
  for x:=0 to fSize do begin
  mindz:=MaxInt;
  Sig:=Sign(random*2-1);
  z:=fHeight[x,y];
  for jj:=0 to 7 do begin // Look for the largest slope
  j:=(8+Sig*jj) mod 8;
  x1:=x+NeighX[j];
  y1:=y+NeighY[j];
  try z1:=Height[x1,y1];
  dz:=(z1-z)*NeighW[j];
  if dz+random*0.03*fRangeHeight<mindz then begin
  minx:=x1;
  miny:=y1;
  minz:=z1;
  mindz:=dz;
  Next:=j;
  end;//if
  except // Out of the map? Then go to next rain drop
  Flow[x,y].NextX:=-99;
  Break;
  end;
  with Flow[x,y] do begin
  Slope:=ArcTan((minz-z)*c);
  if Slope>0 then NextX:=-99
  else begin
  NextX:=minx;
  NextY:=miny;
  Erosion:=0;
  end;//if
  end;//with
  end;//for j
  end;//for
  end;//for
  From:=0;

  //Rain
  for y0:=0 to fSize do begin
  for x0:=0 to fSize do begin
  x:=x0;
  y:=y0;
  OldSlope:=0;
  while (x<>-99)and(fCover[x,y]>0) do begin // Not in the sea
  with Flow[x,y] do begin
  if (Slope*2<OldSlope) then begin
  Dec(Erosion);
  x:=NextX;
  y:=NextY;
  OldSlope:=Slope;
  end//if
  else begin
  //Inc(Erosion);
  Break;
  end;//else
  end;//with
  end;//while
  end;//for x0
  end;//for y0

  //Apply erosion
  for y:=0 to fSize do begin
  for x:=0 to fSize do begin
  //fHeight[x,y]:=fHeight[x,y]+Flow[x,y].Erosion*0.002*Intensity*fRangeHeight;
  fHeight[x,y]:=(Flow[x,y].Erosion)*100+50;
  end;//for
  end;//for

  Flow:=nil;
  end; // *)

(* procedure TGLBaseRandomHDS.DoErosionByRain(const Intensity: single);
  const
  NeighX:array[0..7] of integer=(-1, 0, 1, 1, 1, 0,-1,-1);
  NeighY:array[0..7] of integer=(-1,-1,-1, 0, 1, 1, 1, 0);
  var
  Erosion	:array of array of single;
  Flow		:array[0..7] of single;
  FlowSum	:single;
  j			:integer;
  x,y		:integer;
  x1,y1		:integer;
  z,z1		:single;
  c			:single;
  begin
  c:=1/VSF; // Vertical scale factor
  SetLength(Erosion,fSize+2,fSize+2);
  for y:=0 to fSize+1 do for x:=0 to fSize+1 do Erosion[x,y]:=0;

  //Erosion computation
  for y:=0 to fSize+1 do begin
  for x:=0 to fSize+1 do begin
  z:=fHeight[x,y];
  FlowSum:=0;
  for j:=0 to 7 do begin // Flow to adjacent cells
  x1:=x+NeighX[j];
  y1:=y+NeighY[j];
  try
  z1:=Height[x1,y1]+random*0;
  if z1<z then begin
  Flow[j]:=ArcTan((z-z1)*c);
  FlowSum:=FlowSum+Flow[j];
  end//if
  else Flow[j]:=0;
  except
  Flow[j]:=0;
  end;//except
  end;//for j

  if FlowSum>0 then begin // Erosion and deposition
  Erosion[x,y]:=Erosion[x,y]-1; // Erosion
  for j:=0 to 7 do begin
  if Flow[j]>1e-3 then begin
  x1:=x+NeighX[j];
  y1:=y+NeighY[j];
  Erosion[x1,y1]:=Erosion[x1,y1]+Flow[j]/FlowSum; // Partial deposition
  end;//if
  end;//for
  end;//if

  end;//for x
  end;//for y

  //Apply erosion to each cell
  for y:=0 to fSize do begin
  for x:=0 to fSize do begin
  fHeight[x,y]:=fHeight[x,y]+Erosion[x,y]*0.005*Intensity*fRangeHeight;
  //fHeight[x,y]:=(Erosion[x,y])*100+50;
  end;//for
  end;//for

  Erosion:=nil;
  end; // *)

(* procedure TGLBaseRandomHDS.DoErosionByRain(const Intensity: single);
  const
  NeighX:array[0..7] of integer=(-1, 0, 1, 1, 1, 0,-1,-1);
  NeighY:array[0..7] of integer=(-1,-1,-1, 0, 1, 1, 1, 0);
  var
  Erosion	:array of array of single;
  i,j,jj		:integer;
  x,y		:integer;
  x1,y1		:integer;
  x2,y2		:integer;
  z,z1,z2,dz	:single;
  begin
  SetLength(Erosion,fSize+2,fSize+2);

  for i:=1 to 1 do begin
  //for y:=0 to fSize+1 do for x:=0 to fSize+1 do Erosion[x,y]:=0;

  //Erosion computation
  for y:=5 to fSize-4 do begin
  for x:=5 to fSize-4 do begin
  z:=fHeight[x,y];
  dz:=1;
  for jj:=1 to 2 do begin // Flow to adjacent cells
  j:=jj*2;
  x1:=x+NeighX[j]*5;
  y1:=y+NeighY[j]*5;
  x2:=x+NeighX[j+4]*5;
  y2:=y+NeighY[j+4]*5;
  try
  z1:=Height[x1,y1]+random*0;
  z2:=Height[x2,y2]+random*0;
  dz:=dz*Sign(z-(z1+z2)/2);
  except
  end;//except
  end;//for j
  Erosion[x,y]:=dz;
  end;//for x
  end;//for y

  //Apply erosion to each cell
  for y:=0 to fSize do begin
  for x:=0 to fSize do begin
  fHeight[x,y]:=fHeight[x,y]+Erosion[x,y]*100*Intensity;
  //fHeight[x,y]:=(Erosion[x,y])*1+50;
  end;//for
  end;//for

  end;//for i

  Erosion:=nil;
  end; // *)

procedure TGLCustomRandomHDS.DoErosionBySea;
var
  i, j: integer;
begin
  for i := 0 to FSize do
  begin
    for j := 0 to FSize do
    begin
      Application.ProcessMessages;
      if abs(FHeight[i, j] - FSeaLevel) < FErosionBySea.BeachHeight * VSF then
      begin
        FHeight[i, j] := FSeaLevel + (FHeight[i, j] - FSeaLevel) * 0.3;
      end; // if
    end; // for
  end; // for
end;

procedure TGLCustomRandomHDS.DoSea;
var
  i, j: integer;
begin
  for i := 0 to FSize do
  begin
    for j := 0 to FSize do
    begin
      // if fHeight[i,j]<Lvl then fHeight[i,j]:=Lvl-random*wave;
      if FHeight[i, j] < FSeaLevel - FSeaTransparency then
        FHeight[i, j] := FSeaLevel - 1 // Lvl-c-random*wave
      else if FHeight[i, j] < FSeaLevel then
        FHeight[i, j] := FSeaLevel - (FSeaLevel - FHeight[i, j]) / FSeaTransparency;
    end; // for
  end; // for
end;

procedure TGLCustomRandomHDS.DoSteps;
var
  i, j: integer;
  Stp: single;
begin
  Stp := (FMaxHeight - FSeaLevel) / FSteps.Count; // Step height
  for i := 0 to FSize do
  begin
    for j := 0 to FSize do
    begin
      FHeight[i, j] := Round(FHeight[i, j] / Stp) * Stp;
    end; // for
  end; // for
end;

function TGLCustomRandomHDS.GetHeight(x, y: integer): single;
begin
  FIntegerConstrain(x, y);
  Result := FHeight[x, y];
end;

procedure TGLCustomRandomHDS.GetTerrainBounds(var l, t, r, b: single);
begin
  l := 0;
  b := 0;
  t := FSize;
  r := FSize;
end;

// Copied from GLS.HeightData.InterpolatedHeight
function TGLCustomRandomHDS.Interpolate(x, y: single): single;
var
  ix, iy: integer;
  h1, h2, h3: single;
begin
  ix := Trunc(x);
  x := Frac(x);
  iy := Trunc(y);
  y := Frac(y);
  if x > y then
  begin
    // top-right triangle
    h1 := Heights[ix + 1, iy];
    h2 := Heights[ix, iy];
    h3 := Heights[ix + 1, iy + 1];
    Result := h1 + (h2 - h1) * (1 - x) + (h3 - h1) * y;
  end
  else
  begin
    // bottom-left triangle
    h1 := Heights[ix, iy + 1];
    h2 := Heights[ix + 1, iy + 1];
    h3 := Heights[ix, iy];
    Result := h1 + (h2 - h1) * (x) + (h3 - h1) * (1 - y);
  end;
end;

function TGLCustomRandomHDS.PointInMap(const x, y: single): boolean;
begin
  Result := (x >= 0) and (x <= FSize) and (y >= 0) and (y <= FSize);
end;

function TGLCustomRandomHDS.Normal(const Position: TGLVector): TGLVector;
var
  x, y: integer;
begin
  if (FNormal <> nil) then
  begin
    Result := FTerrainRenderer.AbsoluteToLocal(Position);
    x := Round(Result.x);
    y := Round(Result.y);
    FIntegerConstrain(x, y);
    Result := FNormal[x, y];
  end // if
  else
    raise EAccessViolation.Create('No normal array computed.');
end;

function TGLCustomRandomHDS.PointInMap(const x, y: integer): boolean;
begin
  Result := (x >= 0) and (x <= FSize) and (y >= 0) and (y <= FSize);
end;

function TGLCustomRandomHDS.Scale: TGLCoordinates;
begin
  try
    Result := FTerrainRenderer.Scale;
  except
    raise EAccessViolation.Create('No TerrainRenderer defined');
  end;
end;

procedure TGLCustomRandomHDS.SetCyclic(const Value: boolean);
begin
  FCyclic := Value;
  if FCyclic then
  begin
    FIntegerConstrain := CyclicClamp;
    FSingleConstrain := CyclicClamp;
    if FTerrainRenderer <> nil then
      FTerrainRenderer.OnGetTerrainBounds := nil;
  end
  else
  begin
    FIntegerConstrain := BoundaryClamp;
    FSingleConstrain := BoundaryClamp;
    if FTerrainRenderer <> nil then
      FTerrainRenderer.OnGetTerrainBounds := GetTerrainBounds;
  end; // else
end;

procedure TGLCustomRandomHDS.SetHeight(x, y: integer; const Value: single);
begin
  FIntegerConstrain(x, y);
  FHeight[x, y] := Value;
end;

procedure TGLCustomRandomHDS.SetSize(const aSize: integer);
var
  Tile: integer;
begin
  FSize := aSize;
  if FSize > 32 then
    Tile := 32
  else
    Tile := Round(IntPower(2, Trunc(ln(FSize - 1) / ln(2))));
  SetLength(FHeight, FSize + 1, FSize + 1);
  SetLength(FNormal, FSize + 1, FSize + 1);
  MaxPoolSize := sqr(FSize) * SizeOf(smallint);
  if FTerrainRenderer <> nil then
  begin
    FTerrainRenderer.TileSize := Tile;
    FTerrainRenderer.TilesPerTexture := FSize div FTerrainRenderer.TileSize;
  end; // if
end;

procedure TGLCustomRandomHDS.SetTerrainRenderer(const Value: TGLTerrainRenderer);
begin
  FTerrainRenderer := Value;
  if not FSlave then
  begin
    FTerrainRenderer.OnGetTerrainBounds := GetTerrainBounds;
    FTerrainRenderer.HeightDataSource := Self;
  end; // if
end;

function TGLCustomRandomHDS.StandardisedHeight(const x, y: integer): single;
begin
  Result := (Heights[x, y] - FMinHeight) / FRangeHeight * 1000;
end;

procedure TGLCustomRandomHDS.StartPreparingData(heightData: TGLHeightData);
var
  x, y, x0, y0: integer;
  rasterLine: GLS.HeightData.PSmallIntArray;
  oldType: TGLHeightDataType;
begin
  with heightData do
  begin
    DataState := hdsPreparing;
    oldType := DataType;
    Allocate(hdtSmallInt);

    if XLeft >= 0 then
      x0 := XLeft mod (FSize)
    else
      x0 := (FSize + (XLeft mod (FSize))) mod (FSize);
    if YTop >= 0 then
      y0 := YTop mod (FSize)
    else
      y0 := (FSize + (YTop mod (FSize))) mod (FSize);
    if FLandCover then
    begin
      MaterialName := Format('%s%d%d', [Self.Name, x0 div (heightData.Size - 1), y0 div (heightData.Size - 1)]);
      TextureCoordinatesMode := tcmLocal;
      TextureCoordinatesScale := TexPointMake((Self.FSize) / (heightData.Size - 1), (Self.FSize) / (heightData.Size - 1));
    end // if
    else
    begin
      MaterialName := Self.FMaterialName;
      TextureCoordinatesMode := tcmLocal;
      TextureCoordinatesScale := TexPointMake(FTextureScale, FTextureScale);
    end; // else
    for y := y0 to y0 + heightData.Size - 1 do
    begin
      rasterLine := smallintRaster[y - y0];
      for x := x0 to x0 + heightData.Size - 1 do
      begin
        rasterLine[x - x0] := Round(FHeight[x, y]);
      end; // for
    end; // for
    HeightMin := MinHeight;
    HeightMax := MaxHeight;

    DataState := hdsReady;
    if oldType <> hdtSmallInt then
      DataType := oldType;
  end; // with
  // inherited;
end; // *)

function TGLCustomRandomHDS.XMoveBoundary: single;
begin
  Result := FSize * Scale.x * 0.95;
end;

function TGLCustomRandomHDS.ZMoveBoundary: single;
begin
  Result := FSize * Scale.y * 0.95;
end;

procedure TGLCustomRandomHDS.SetKeepNormals(const Value: boolean);
begin
  FKeepNormals := Value;
end;

// --------------------------------------
// TGLFractalHDS
// --------------------------------------
procedure TGLFractalHDS.BuildHeightField(const aDepth, aSeed, aAmplitude: integer);
begin
  fDepth := aDepth;
  FSeed := aSeed;
  fAmplitude := aAmplitude;
  BuildHeightField;
end;

procedure TGLFractalHDS.BuildHeightField;
begin
  FractalMiddlePointHDS(fDepth, FSeed, fAmplitude, fRoughness, FCyclic, FHeight, FMinHeight, FMaxHeight);
  FRangeHeight := FMaxHeight - FMinHeight;
  Scale.x := 1;
  Scale.y := 1;
  Scale.z := FSize / VSF;
end;

constructor TGLFractalHDS.Create(AOwner: TComponent);
begin
  inherited;
  Depth := 4;
  FSea := True;
  Amplitude := 50;
  fRoughness := 0.4;
end;

procedure TGLFractalHDS.SetAmplitude(const Value: integer);
begin
  fAmplitude := Value;
  FMinHeight := -fAmplitude / 2 * VSF;
  FMaxHeight := -FMinHeight;
  FRangeHeight := fAmplitude * VSF;
end;

procedure TGLFractalHDS.SetDepth(const Value: integer);
begin
  fDepth := Value;
  SetSize(Round(IntPower(2, fDepth)));
end;

procedure TGLFractalHDS.SetRoughness(const Value: single);
begin
  fRoughness := Value;
end;

//-----------------------------------
// TGLRandomLandscape
//-----------------------------------

procedure TGLTiledRndLandscape.ApplyLighting(var aLandTile: TGLLandTile);
begin
  with aLandTile do
  begin
    Lighting := Self.FLighting;
    LightColor := Self.FLightColor;
    LightDirection := Self.FLightDirection;
    LightSmoothing := Self.FLightSmoothing;
    Shadows := Self.Shadows;
  end; // with
end;

procedure TGLTiledRndLandscape.ApplyTexture(var aLandTile: TGLLandTile);
begin
  with aLandTile do
  begin
    LandCover := Self.LandCover;
    MaterialName := Self.FMaterialName;
    TextureScale := Self.FTextureScale;
    if Assigned(Self.OnDrawTexture) then
      FOnDrawTexture := Self.OnDrawTexture;
  end; // with
end;

procedure TGLTiledRndLandscape.ApplyTopography(var aLandTile: TGLLandTile);
begin
  with aLandTile do
  begin
    ErosionByFraction := Self.FErosionByFraction;
    ErosionByLife := Self.FErosionByLife;
    ErosionByRain := Self.FErosionByRain;
    ErosionBySea := Self.FErosionBySea;
    FSea := Self.FSea;
    FSeaLevel := Self.FSeaLevel;
    FSeaTransparency := Self.FSeaTransparency;
  end; // with
end;

procedure TGLTiledRndLandscape.BoundaryClamp(var x, z: single);
begin
  ClampValue(x, 0, FExtentX * fLandTileSize);
  ClampValue(z, 0, FExtentZ * fLandTileSize);
end;

procedure TGLTiledRndLandscape.BoundaryClamp(var x, z: integer);
begin
  if x < 0 then
    x := 0
  else if x > FExtentX * fLandTileSize then
    x := FExtentX * fLandTileSize;
  if z < 0 then
    z := 0
  else if z > ExtentZ * fLandTileSize then
    z := FExtentZ * fLandTileSize;
end;

procedure TGLTiledRndLandscape.CameraPosition(var TileX, TileZ: integer);
begin
  FindLandTile(-Camera.Position.x, Camera.Position.z, TileX, TileZ);
end;

procedure TGLTiledRndLandscape.CleanUp;
var
  i: integer;
begin
  for i := fLandTiles.Count - 1 downto 0 do
  begin
    if TGLLandTile(fLandTiles.Items[i]).LandTileInfo.State = hdsNone then
    begin
      fLandTiles.Delete(i); // Free the Landtile and remove it from the list
      // FTerrainRenderer.MaterialLibrary.Materials.DeleteUnusedMaterials;
    end; // if
  end; // for
end;

procedure TGLTiledRndLandscape.ComputeLandTile(const aX, aZ: integer; var NewLandTile: TGLLandTile);
var
  sx, sz: string;
begin
  FLandTileComputing := True;

  FLandTileInfo.x := aX;
  FLandTileInfo.z := aZ;
  FLandTileInfo.State := hdsPreparing;

  with NewLandTile do
  begin
    Cyclic := False;
    TerrainRenderer := Self.FTerrainRenderer;
    if aX >= 0 then
      sx := 'p'
    else
      sx := 'n';
    if aZ >= 0 then
      sz := 'p'
    else
      sz := 'n';
    Seed := LandTileSeed(aX, aZ);
    Name := Format('Land_%s%d%s%d_', [sx, abs(aX), sz, abs(aZ)]);
    // Generate a unique name
  end; // with
  fComputedLandTile := NewLandTile;
  OnCreateLandTile(aX, aZ, NewLandTile.Seed, NewLandTile);
  with NewLandTile.LandTileInfo do
    FLandTileInfo.State := hdsReady;
  MarkDirty(aX * fLandTileSize, aZ * fLandTileSize, (aX + 1) * fLandTileSize - 1, (aZ + 1) * fLandTileSize - 1);
  fComputedLandTile := nil;
  FLandTileComputing := False;
  fLandTiles.Add(NewLandTile);
  Application.ProcessMessages;
end;

procedure TGLTiledRndLandscape.ConstrainCoordinates(var x, z: single);
begin
  FSingleConstrain(x, z);
end;

procedure TGLTiledRndLandscape.ConstrainCoordinates(var x, z: integer);
begin
  FIntegerConstrain(x, z);
end;

constructor TGLTiledRndLandscape.Create(AOwner: TComponent);
begin
  inherited;
  fLandTiles := tComponentList.Create;
  IsDefaultTile := fDefaultIsDefaultTile;
  OnCreateDefaultTile := fDefaultOnCreateDefaultTile;
  FExtentX := 10000;
  FExtentZ := 10000;
  GenerationRadius := 2;
  FLandTileDensity := 1;
  FLandCover := True;
end;

procedure TGLTiledRndLandscape.CyclicClamp(var x, z: integer);
begin
  exit;
  x := (x + ExtentX) mod ExtentX;
  z := (z + ExtentZ) mod ExtentZ;
end;

procedure TGLTiledRndLandscape.CyclicClamp(var x, z: single);
var
  ix, iz: integer;
  sx, sz: single;
begin
  exit;
  ix := Trunc(ExtentX + x);
  sx := Frac(x);
  iz := Trunc(ExtentZ + z);
  sz := Frac(z);
  x := (ExtentX * fLandTileSize + ix) mod ExtentX * fLandTileSize + sx;
  z := (ExtentZ * fLandTileSize + iz) mod ExtentZ * fLandTileSize + sz;
end;

destructor TGLTiledRndLandscape.Destroy;
begin
  fLandTiles.Free;
  inherited;
end;

function TGLTiledRndLandscape.fDefaultIsDefaultTile(x, z: integer): boolean;
begin
  InitializeRandomGenerator(LandTileSeed(x, z));
  Result := (random >= FLandTileDensity);
end;

procedure TGLTiledRndLandscape.fDefaultOnCreateDefaultTile(heightData: TGLHeightData);
begin
  heightData.DataState := hdsNone;
  // raise EAccessViolation.Create('No DefaultStartPreparingDefaultTile procedure supplied.');
end;

procedure TGLTiledRndLandscape.FindLandTile(const x, z: single; var TileX, TileZ: integer);
begin
  TileX := Floor(x / fLandTileSize);
  TileZ := Floor(z / fLandTileSize);
  FIntegerConstrain(TileX, TileZ);
end;

// Sorting Landscape
//
function TGLTiledRndLandscape.fSortLandscapes(Item1, Item2: Pointer): integer;
var
  x, z: integer;
  d1, d2: single;
begin
  CameraPosition(x, z);
  d1 := sqr(x - TGLLandTile(Item1^).LandTileInfo.x) + sqr(z - TGLLandTile(Item1^).LandTileInfo.z);
  d2 := sqr(x - TGLLandTile(Item2^).LandTileInfo.x) + sqr(z - TGLLandTile(Item2^).LandTileInfo.z);
  Result := Round(d1 - d2);
end;

function TGLTiledRndLandscape.GetTask: string;
begin
  if fComputedLandTile <> nil then
    Result := fComputedLandTile.Task
  else
    Result := 'Idle';
end;

function TGLTiledRndLandscape.GetTaskProgress: integer;
begin
  if fComputedLandTile <> nil then
    Result := fComputedLandTile.TaskProgress
  else
    Result := 0;
end;

procedure TGLTiledRndLandscape.GetTerrainBounds(var l, t, r, b: single);
begin
  l := 0;
  b := 0;
  t := ExtentZ * LandTileSize;
  r := ExtentX * LandTileSize;
end;

procedure TGLTiledRndLandscape.Initialize(const aX, aZ: single);
var
  cx, cz: integer;
  NewLandTile: TGLLandTile;
  x, z, dx, dz: integer;
begin
  fOldCamX := -99999;
  fOldCamZ := -99999;
  with Camera.Position do
  begin
    x := aX;
    z := aZ;
  end; // with
  CameraPosition(cx, cz);
  ComputeLandTile(cx, cz, NewLandTile);
  TerrainRenderer.Scale := NewLandTile.Scale;
  with Camera.Position do
  begin
    x := x * NewLandTile.Scale.x;
    z := z * NewLandTile.Scale.z;
  end; // with

  for z := 0 to FGenerationRadius + 1 do
  begin
    for x := 1 to FGenerationRadius + 1 do
    begin
      if Trunc(sqrt(sqr(x) + sqr(z))) <= FGenerationRadius then
      begin
        dx := x;
        dz := z;
        if not IsDefaultTile(cx + dx, cz + dz) then
          ComputeLandTile(cx + dx, cz + dz, NewLandTile);
        dx := -z;
        dz := x;
        if not IsDefaultTile(cx + dx, cz + dz) then
          ComputeLandTile(cx + dx, cz + dz, NewLandTile);
        dx := -x;
        dz := -z;
        if not IsDefaultTile(cx + dx, cz + dz) then
          ComputeLandTile(cx + dx, cz + dz, NewLandTile);
        dx := z;
        dz := -x;
        if not IsDefaultTile(cx + dx, cz + dz) then
          ComputeLandTile(cx + dx, cz + dz, NewLandTile);
      end; // if
    end; // for
  end; // for
end;

// Generates a unique seed from the tile coordinates
//
function TGLTiledRndLandscape.LandTileSeed(x, z: integer): integer;
begin
  Result := fBaseSeed + z * ExtentX + x;
end;

function TGLTiledRndLandscape.LandtileCount: integer;
begin
  Result := fLandTiles.Count;
end;

// Preparing Land Tile Data
//
procedure TGLTiledRndLandscape.PrepareLandTileData(HeightData: TGLHeightData;
  LandTile: TGLLandTile);
var
  x, y, x0, y0: integer;
  rasterLine: PFloatArray;
  oldType: TGLHeightDataType;
begin
  with HeightData do
  begin
    DataState := hdsPreparing;
    oldType := DataType;
    Allocate(hdtSingle);

    if XLeft >= 0 then
      x0 := XLeft mod (fLandTileSize)
    else
      x0 := (fLandTileSize + (XLeft mod (fLandTileSize))) mod (fLandTileSize);
    if YTop >= 0 then
      y0 := YTop mod (fLandTileSize)
    else
      y0 := (fLandTileSize + (YTop mod (fLandTileSize))) mod (fLandTileSize);

    MaterialName := Format('%s%d%d',
      [LandTile.Name, x0 div FTerrainRenderer.TileSize,
      y0 div FTerrainRenderer.TileSize]);
    TextureCoordinatesMode := tcmLocal;
    TextureCoordinatesScale := TexPointMake((fLandTileSize) / (Size),
      (fLandTileSize) / (Size));
    for y := y0 to y0 + HeightData.Size - 1 do
    begin
      rasterLine := singleRaster[y - y0];
      for x := x0 to x0 + HeightData.Size - 1 do
      begin
        rasterLine[x - x0] := LandTile.FHeight[x, y];
      end; // for
    end; // for

    DataState := hdsReady;
    if oldType <> hdtSingle then
      DataType := oldType;
  end; // with
end;

// Set Camera
//
procedure TGLTiledRndLandscape.SetCamera(const Value: TGLCamera);
begin
  FCamera := Value;
end;

procedure TGLTiledRndLandscape.SetCyclic(const Value: boolean);
begin
  FCyclic := Value;
  if FCyclic then
  begin
    FIntegerConstrain := CyclicClamp;
    FSingleConstrain := CyclicClamp;
    if FTerrainRenderer <> nil then
      FTerrainRenderer.OnGetTerrainBounds := nil;
  end
  else
  begin
    FIntegerConstrain := BoundaryClamp;
    FSingleConstrain := BoundaryClamp;
    if FTerrainRenderer <> nil then
      FTerrainRenderer.OnGetTerrainBounds := GetTerrainBounds;
  end; // else
end;

procedure TGLTiledRndLandscape.SetExtentX(const Value: integer);
begin
  FExtentX := Value;
  FExtentXhalf := FExtentX div 2;
end;

procedure TGLTiledRndLandscape.SetExtentZ(const Value: integer);
begin
  FExtentZ := Value;
  FExtentZhalf := FExtentZ div 2;
end;

procedure TGLTiledRndLandscape.SetGenerationRadius(const Value: integer);
var
  x, z, i: integer;
begin
  FGenerationRadius := Value;
  SetLength(fGenRadius, sqr(FGenerationRadius * 2 + 1));
  i := 0;
  for z := 0 to FGenerationRadius do
  begin
    for x := 1 to FGenerationRadius do
    begin
      if Trunc(sqrt(sqr(x) + sqr(z))) <= FGenerationRadius then
      begin
        fGenRadius[i].dx := x;
        fGenRadius[i].dz := z;
        fGenRadius[i + 1].dx := -z;
        fGenRadius[i + 1].dz := x;
        fGenRadius[i + 2].dx := -x;
        fGenRadius[i + 2].dz := -z;
        fGenRadius[i + 3].dx := z;
        fGenRadius[i + 3].dz := -x;
        Inc(i, 4);
      end; // if
    end; // for
  end; // for
  SetLength(fGenRadius, i - 3);
  fLandTiles.Capacity := (i - 3) * 2;
end;

procedure TGLTiledRndLandscape.SetIsDefaultTile(const Value: TIsDefaultTile);
begin
  FIsDefaultTile := Value;
end;

procedure TGLTiledRndLandscape.SetLandTileCapacity(const Value: integer);
begin
  FLandTileCapacity := Value;
end;

procedure TGLTiledRndLandscape.SetLandTileDensity(const Value: single);
begin
  FLandTileDensity := Value;
end;

procedure TGLTiledRndLandscape.SetOnCreateDefaultTile(const Value: TStartPreparingDataEvent);
begin
  fOnCreateDefaultTile := Value;
end;

procedure TGLTiledRndLandscape.SetOnCreateLandTile(const Value: TOnCreateLandTile);
begin
  fOnCreateLandTile := Value;
end;

procedure TGLTiledRndLandscape.SetSeed(const Value: integer);
begin
  FSeed := Value;
  InitializeRandomGenerator(FSeed);
end;

procedure TGLTiledRndLandscape.SetSize(const aSize: integer);
begin
  fLandTileSize := aSize;
end;

procedure TGLTiledRndLandscape.SetTerrainRenderer(const Value: TGLTerrainRenderer);
begin
  FTerrainRenderer := Value;
  FTerrainRenderer.HeightDataSource := Self;
end;

procedure TGLTiledRndLandscape.StartPreparingData(heightData: TGLHeightData);
var
  i: integer;
  tx, tz: integer;
begin
  with heightData do
  begin
    DataState := hdsPreparing;

    if (System.abs(XLeft) mod (heightData.Size - 1) = 0) and (System.abs(YTop) mod (heightData.Size - 1) = 0) then
    begin
      FindLandTile(XLeft, YTop, tx, tz);

      if IsDefaultTile(tx, tz) then
      begin
        OnCreateDefaultTile(heightData);
        exit;
      end; // if

      { Look if the landtile has already been computed }
      for i := 0 to fLandTiles.Count - 1 do
      begin
        with TGLLandTile(fLandTiles.Items[i]).LandTileInfo do
        begin
          if (x = tx) and (z = tz) then
          begin
            if (State = hdsReady) then
            begin
              TGLLandTile(fLandTiles.Items[i]).StartPreparingData(heightData);
              exit;
            end
            else
              break;
          end; // if
        end; // with
      end; // for
    end; // if

    DataState := hdsNone;
  end; // with
end;

function TGLTiledRndLandscape.TileDistance(const x1, z1, x2, z2: integer): single;
begin
  Result := sqrt(sqr(FExtentXhalf - abs(abs(x1 - x2) - FExtentXhalf)) + sqr(FExtentZhalf - abs(abs(z1 - z2) - FExtentZhalf)));
end;

function TGLTiledRndLandscape.TileDistanceSquared(const x1, z1, x2, z2: integer): integer;
begin
  Result := sqr(FExtentXhalf - abs(abs(x1 - x2) - FExtentXhalf)) + sqr(FExtentZhalf - abs(abs(z1 - z2) - FExtentZhalf));
end;

procedure TGLTiledRndLandscape.Update;
var
  i, j, maxi: integer;
  maxd, d: integer;
  cx, cz: integer;
  cx0, cz0: integer;
  Found: boolean;
  NewLandTile: TGLLandTile;
begin
  CameraPosition(cx0, cz0);
  if fMapUpdating or (fOldCamX = cx0) and (fOldCamZ = cz0) then
    exit;

  for j := 0 to High(fGenRadius) do
  begin
    fMapUpdating := True;
    cx := cx0 + fGenRadius[j].dx;
    cz := cz0 + fGenRadius[j].dz;
    FIntegerConstrain(cx, cz);
    if IsDefaultTile(cx, cz) then
      continue;

    { Look if the landtile has already been computed }
    Found := False;
    for i := 0 to fLandTiles.Count - 1 do
    begin
      with TGLLandTile(fLandTiles.Items[i]).LandTileInfo do
      begin
        if (x = cx) and (z = cz) and (State = hdsReady) then
        begin
          Found := True;
          break;
        end; // if
      end; // with
    end; // for

    { If not, compute it }
    if not Found and not FLandTileComputing then
    begin
      if fLandTiles.Count >= FLandTileCapacity then
      begin // If the tile buffer is full...
        maxd := -1; // ...replace the farthest tile
        maxi := -1;
        for i := 0 to fLandTiles.Count - 1 do
          with TGLLandTile(fLandTiles.Items[i]) do
          begin
            d := sqr(cx0 - LandTileInfo.x) + sqr(cz0 - LandTileInfo.z);
            if d > maxd then
            begin
              maxd := d;
              maxi := i;
            end; // if
          end; // for i
        if sqrt(maxd) > FGenerationRadius + 1 then
        begin
          TGLLandTile(fLandTiles.Items[maxi]).Free;
        end; // if
      end; // if
      ComputeLandTile(cx, cz, NewLandTile);
      fMapUpdating := False;
      exit; // Don't explore further. Let it for the next time step
    end; // if
  end; // for j
  fMapUpdating := False;
  fOldCamX := cx0; // Surrounding completely updated, we can stop checking
  fOldCamZ := cz0;
  fLandTiles.Pack;
end;

function TGLTiledRndLandscape.XMoveBoundary: single;
begin
  Result := ExtentX * LandTileSize * 0.95;
end;

function TGLTiledRndLandscape.ZMoveBoundary: single;
begin
  Result := ExtentZ * LandTileSize * 0.95;
end;

//
// TGLFractalArchipelago
//
procedure TGLFractalArchipelago.ComputeLandTile(const aX, aZ: integer; var NewLandTile: TGLLandTile);
begin
  NewLandTile := TGLFractalHDS.Create(Self);
  NewLandTile.FSlave := True;
  inherited ComputeLandTile(aX, aZ, NewLandTile);
end;

constructor TGLFractalArchipelago.Create(AOwner: TComponent);
begin
  inherited;
  OnCreateLandTile := fOnCreateLandTile;
  IsDefaultTile := FIsDefaultTile;
  IslandDensity := 0.4;
  FWaveAmplitude := 2;
  FWaveSpeed := 20;
  Sea := False; // Sea is drawn by the PostRender event
end;

procedure TGLFractalArchipelago.fOnCreateDefaultTile(heightData: TGLHeightData);
var
  x, y: integer;
  rasterLine: GLS.VectorGeometry.PSingleArray;
  oldType: TGLHeightDataType;
begin
  with heightData do
  begin
    DataState := hdsPreparing;
    oldType := DataType;
    Allocate(hdtSingle);
    MaterialName := FMaterialName;

    for y := 0 to heightData.Size - 1 do
    begin
      rasterLine := singleRaster[y];
      for x := 0 to heightData.Size - 1 do
      begin
        rasterLine[x] := FSeaLevel;
      end; // for
    end; // for

    if oldType <> hdtSingle then
      DataType := oldType;
  end; // with
end;

procedure TGLFractalArchipelago.fOnCreateLandTile(aX, aZ, aSeed: integer; var aLandscape: TGLLandTile);
begin
  InitializeRandomGenerator(aSeed);
  with TGLFractalHDS(aLandscape) do
  begin
    // Initialize the tile
    Seed := random(MaxInt);
    Depth := Self.fDepth;
    Amplitude := random(FAmplitudeMax - FAmplitudeMin) + FAmplitudeMin;
    Roughness := random * (FRoughnessMax - FRoughnessMin) + FRoughnessMin;
    ApplyLighting(aLandscape);
    ApplyTexture(aLandscape);
    ApplyTopography(aLandscape);
    Cyclic := True;
    PrimerLandscape := True;

    // Generate the landscape
    PrimerIsland(SeaLevel - SeaTransparency, random * Amplitude / 2, FHeight);
    // Pre-generate an island
    BuildHeightField;
    if ErosionByRain.Enabled then
      DoErosionByRain;
    if ErosionByLife.Enabled then
      DoErosionByLife;
    if ErosionBySea.Enabled then
      DoErosionBySea;
    if Sea then
      DoSea;
    BuildNormals;
    if Lighting then
      BuildLightMap
    else
      ClearLightMap;
    BuildTexture;
    FNormal := nil;
    FLightMap := nil;
  end; // with
end;

//
// Code borrowed from Archipelago advanced demo
//
procedure TGLFractalArchipelago.FPostRenderSeaDynamic(var rci: TGLRenderContextInfo; var HeightDatas: TList);
var
  i, x, y, s, s2: integer;
  t: single;
  hd: TGLHeightData;
const
  r = 0.75;
  g = 0.75;
  b = 1;

  function WaterPhase(const px, py: single): single;
  begin
    Result := t * 1 + px * 0.16 + py * 0.09;
  end;

  procedure IssuePoint(rx, ry: integer);
  var
    px, py: single;
    alpha, colorRatio, ca, sa: single;
  begin
    px := x + rx + s2;
    py := y + ry + s2;
    if hd.DataState = hdsNone then
    begin
      alpha := 1;
    end
    else
    begin
      alpha := (FSeaLevel - hd.SmallIntHeight(rx, ry)) * (1 / FSeaTransparency);
      alpha := ClampValue(alpha, 0.5, 1);
    end;
    SinCos(WaterPhase(px, py) * FWaveSpeed, sa, ca);
    colorRatio := 1 - alpha * 0.1;
    gl.Color4f(r * colorRatio, g * colorRatio, b, alpha);
    gl.TexCoord2f(px * 0.01 + 0.002 * sa, py * 0.01 + 0.0022 * ca - t * 0.01);
    gl.Vertex3f(px, py, FSeaLevel + FWaveAmplitude * sa * VSF);
  end;

begin
  // if not WaterPlane then Exit;
  t := ((GetTickCount - rhdsStartTime) / 10000);
  FTerrainRenderer.MaterialLibrary.ApplyMaterial(FSeaMaterialName, rci);
  repeat
    // if not WasAboveWater then InverTGLFrontFace;
    gl.PushAttrib(GL_ENABLE_BIT);

    gl.Disable(GL_LIGHTING);
    gl.Disable(GL_NORMALIZE);

    gl.StencilFunc(GL_ALWAYS, 1, 255);
    gl.StencilMask(255);
    gl.StencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);
    gl.Enable(GL_STENCIL_TEST);
    gl.Normal3f(0, 0, 1);

    for i := 0 to HeightDatas.Count - 1 do
    begin
      hd := TGLHeightData(HeightDatas.List[i]);
      if (hd.DataState = hdsReady) and (hd.HeightMin > FSeaLevel) then
        continue;
      x := hd.XLeft;
      y := hd.YTop;
      s := hd.Size - 1;
      s2 := s div 2;
      gl.Begin_(GL_TRIANGLE_FAN);
      IssuePoint(s2, s2);
      IssuePoint(0, 0);
      IssuePoint(s2, 0);
      IssuePoint(s, 0);
      IssuePoint(s, s2);
      IssuePoint(s, s);
      IssuePoint(s2, s);
      IssuePoint(0, s);
      IssuePoint(0, s2);
      IssuePoint(0, 0);
      gl.End_;
    end;
    gl.StencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
    gl.PopAttrib;
    // if not WasAboveWater then InverTGLFrontFace;
    // WaterPolyCount:=heightDatas.Count*8;
  until not FTerrainRenderer.MaterialLibrary.UnApplyMaterial(rci);
end;

procedure TGLFractalArchipelago.FPostRenderSeaStatic(var rci: TGLRenderContextInfo; var HeightDatas: TList);
var
  i, x, y, s, s2: integer;
  hd: TGLHeightData;
  t: single;
const
  r = 0.75;
  g = 0.75;
  b = 1;

  procedure IssuePoint(rx, ry: integer);
  var
    px, py: single;
    alpha, colorRatio: single;
  begin
    px := x + rx + s2;
    py := y + ry + s2;
    if hd.DataState = hdsNone then
    begin
      alpha := 1;
    end
    else
    begin
      alpha := (FSeaLevel - hd.SmallIntHeight(rx, ry)) * (1 / FSeaTransparency);
      alpha := ClampValue(alpha, 0.5, 1);
    end;
    colorRatio := 1 - alpha * 0.1;
    gl.Color4f(r * colorRatio, g * colorRatio, b, alpha);
    gl.TexCoord2f(px * 0.01, py * 0.01 + t);
    gl.Vertex3f(px, py, FSeaLevel);
  end;

begin
  t := Frac(GetTickCount / 1000);

  FTerrainRenderer.MaterialLibrary.ApplyMaterial(FSeaMaterialName, rci);
  repeat
    // if not WasAboveWater then InverTGLFrontFace;
    gl.PushAttrib(GL_ENABLE_BIT);

    gl.Disable(GL_LIGHTING);
    gl.Disable(GL_NORMALIZE);

    gl.StencilFunc(GL_ALWAYS, 1, 255);
    gl.StencilMask(255);
    gl.StencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);
    gl.Enable(GL_STENCIL_TEST);
    gl.Normal3f(0, 0, 1);

    for i := 0 to HeightDatas.Count - 1 do
    begin
      hd := TGLHeightData(HeightDatas.List[i]);
      if (hd.DataState = hdsReady) and (hd.HeightMin > FSeaLevel) then
        continue;
      x := hd.XLeft;
      y := hd.YTop;
      s := hd.Size - 1;
      s2 := s div 2;
      gl.Begin_(GL_TRIANGLE_FAN);
      IssuePoint(s2, s2);
      IssuePoint(0, 0);
      IssuePoint(s2, 0);
      IssuePoint(s, 0);
      IssuePoint(s, s2);
      IssuePoint(s, s);
      IssuePoint(s2, s);
      IssuePoint(0, s);
      IssuePoint(0, s2);
      IssuePoint(0, 0);
      gl.End_;
    end;
    gl.StencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
    gl.PopAttrib;
    // if not WasAboveWater then InverTGLFrontFace;
    // WaterPolyCount := heightDatas.Count*8;
  until not FTerrainRenderer.MaterialLibrary.UnApplyMaterial(rci);
end;

function TGLFractalArchipelago.GetIslandDensity: single;
begin
  Result := FLandTileDensity;
end;

procedure TGLFractalArchipelago.SetAmplitudeMax(const Value: integer);
begin
  FAmplitudeMax := Value;
end;

procedure TGLFractalArchipelago.SetAmplitudeMin(const Value: integer);
begin
  FAmplitudeMin := Value;
end;

procedure TGLFractalArchipelago.SetDepth(const Value: integer);
begin
  fDepth := Value;
  SetSize(Round(IntPower(2, fDepth)));
end;

procedure TGLFractalArchipelago.SetIslandDensity(const Value: single);
begin
  LandTileDensity := Value;
end;

procedure TGLFractalArchipelago.SetRoughnessMax(const Value: single);
begin
  FRoughnessMax := Value;
end;

procedure TGLFractalArchipelago.SetRoughnessMin(const Value: single);
begin
  FRoughnessMin := Value;
end;

procedure TGLFractalArchipelago.SetSeaDynamic(const Value: boolean);
begin
  FSeaDynamic := Value;
  if FSeaDynamic then
    FTerrainRenderer.OnHeightDataPostRender := FPostRenderSeaDynamic
  else
    FTerrainRenderer.OnHeightDataPostRender := FPostRenderSeaStatic;
end;

procedure TGLFractalArchipelago.SetSeaMaterialName(const Value: string);
begin
  FSeaMaterialName := Value;
end;

procedure TGLFractalArchipelago.SetTerrainRenderer(const Value: TGLTerrainRenderer);
begin
  inherited;
  SeaDynamic := FSeaDynamic; // Called to hook the PostRender event handler
end;

procedure TGLFractalArchipelago.SetWaveAmplitude(const Value: single);
begin
  FWaveAmplitude := Value;
end;

procedure TGLFractalArchipelago.SetWaveSpeed(const Value: single);
begin
  FWaveSpeed := Value;
end;

(***************************************************************
 *******              RANDOM HDS ALGORITHMS             ********
 ***************************************************************)
procedure FractalMiddlePointHDS(const aDepth, aSeed, aAmplitude: integer;
  const aRoughness: single; aCyclic: boolean;
  var z: TMapOfSingle; var MinZ, MaxZ: single);
var
  iter, Stp, stp2: integer;
  i, j: integer;
  dz: single;
  Size: integer;

  // Fill variables only if they have not been predefined
  procedure Let(var z: single; const Value: single);
  begin
    if z = Empty then
      z := Value;
  end;

  // Fill variables only if they have not been predefined
  function Get(const x, y: integer; var Value: single): boolean;
  begin
    Value := z[x, y];
    Result := (Value = Empty);
  end;

  function Centre(const x, y, Stp: integer): single;
  begin
    Result := z[x - Stp, y - Stp];
    Result := Result + z[x - Stp, y + Stp];
    Result := Result + z[x + Stp, y - Stp];
    Result := Result + z[x + Stp, y + Stp];
    Result := Result * 0.25;
    if MinZ > Result then
      MinZ := Result;
    if MaxZ < Result then
      MaxZ := Result;
  end;

  function Side(const x, y, Stp: integer): single;
  var
    n: integer;
  begin
    n := 0;
    Result := 0;
    if y - Stp >= 0 then
    begin
      Result := Result + z[x, y - Stp];
      Inc(n);
    end;
    if y + Stp <= Size then
    begin
      Result := Result + z[x, y + Stp];
      Inc(n);
    end;
    if x - Stp >= 0 then
    begin
      Result := Result + z[x - Stp, y];
      Inc(n);
    end;
    if x + Stp <= Size then
    begin
      Result := Result + z[x + Stp, y];
      Inc(n);
    end;
    Result := Result / n;
    if MinZ > Result then
      MinZ := Result;
    if MaxZ < Result then
      MaxZ := Result;
  end;

begin
  InitializeRandomGenerator(aSeed);
  Size := High(z);
  dz := aAmplitude * VSF;
  MinZ := 1E38;
  MaxZ := -1E38;

  if aCyclic then
  begin
    Let(z[0, 0], 0);
    Let(z[0, Size], z[0, 0]);
    Let(z[Size, 0], z[0, 0]);
    Let(z[Size, Size], z[0, 0]);

    // Build Height field
    for iter := 1 TO aDepth do
    begin // iterations
      Stp := Round(Size / IntPower(2, (iter - 1))); // step
      stp2 := Stp div 2; // half step
      dz := dz * aRoughness;
      i := stp2;
      repeat
        j := stp2;
        repeat // Centre
          if z[i, j] = Empty then
          begin
            z[i, j] := Centre(i, j, stp2);
            z[i, j] := z[i, j] + (random * dz * 2 - dz) * 1.4;
          end; // if
          Inc(j, Stp);
        until j > Size - stp2 + 1;
        Inc(i, Stp);
      until i > Size - stp2 + 1;
      i := stp2;
      repeat
        j := 0;
        repeat // Sides
          if z[i, j] = Empty then
          begin
            z[i, j] := Side(i, j, stp2);
            z[i, j] := z[i, j] + random * dz * 2 - dz;
          end; // if
          if z[j, i] = Empty then
          begin
            z[j, i] := Side(j, i, stp2);
            z[j, i] := z[j, i] + random * dz * 2 - dz;
          end; // if
          Inc(j, Stp);
        until j >= Size;
        Let(z[Size, i], z[0, i]);
        Let(z[i, Size], z[i, 0]);
        Inc(i, Stp);
      until i > Size - stp2 + 1;
    end; // for iter
  end // if Cyclic

  else
  begin // Non-cyclic landscape
    Let(z[0, 0], random * dz * 2 - dz);
    Let(z[0, Size], random * dz * 2 - dz);
    Let(z[Size, 0], random * dz * 2 - dz);
    Let(z[Size, Size], random * dz * 2 - dz);

    // Build Height field
    for iter := 1 to aDepth do
    begin // iterations
      Stp := Round(Size / IntPower(2, (iter - 1))); // step
      stp2 := Stp div 2; // half step
      dz := dz * aRoughness;
      i := stp2;
      repeat
        j := stp2;
        repeat // Centre
          if z[i, j] = Empty then
          begin
            z[i, j] := Centre(i, j, stp2);
            z[i, j] := z[i, j] + (random * dz * 2 - dz) * 1.4;
          end;
          Inc(j, Stp);
        until j > Size - stp2 + 1;
        Inc(i, Stp);
      until i > Size - stp2 + 1;
      i := stp2;
      repeat
        j := 0;
        repeat // Sides
          if z[i, j] = Empty then
          begin
            z[i, j] := Side(i, j, stp2);
            z[i, j] := z[i, j] + random * dz * 2 - dz;
          end; // if
          if z[j, i] = Empty then
          begin
            z[j, i] := Side(j, i, stp2);
            z[j, i] := z[j, i] + random * dz * 2 - dz;
          end; // if
          Inc(j, Stp);
        until j > Size;
        Inc(i, Stp);
      until i > Size - stp2 + 1;
    end; // for iter
  end; // else Cyclic
end;

(***************************************************************
 *******              PREDEFINED HEIGHT-FIELD           ********
 ***************************************************************)
procedure PrimerNull(var z: TMapOfSingle);
// Empty field
var
  x, y: integer;
  Size: integer;
begin
  Size := High(z);
  for y := 0 to Size do
  begin
    for x := 0 to Size do
    begin
      z[x, y] := Empty;
    end; // for
  end; // for
end;

(* Ensure that the border of the tile is low (below sea level) and the middle
  is high. *)
procedure PrimerIsland(LowZ, HighZ: single; var z: TMapOfSingle);
var
  i: integer;
  Size: integer;
begin
  Size := High(z);
  PrimerNull(z);
  HighZ := HighZ * VSF;
  LowZ := LowZ * VSF;
  z[Size div 2, Size div 2] := HighZ;
  for i := 0 to Size do
  begin
    z[i, 0] := LowZ;
    z[0, i] := LowZ;
    z[Size, i] := LowZ;
    z[i, Size] := LowZ;
  end; // for i
end;

//----------------------------------------------
initialization
//----------------------------------------------

rhdsStartTime := GetTickCount;

end.
