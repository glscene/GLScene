//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit Formats.LWO;

(* =============================================================

This unit provides functions, constants and now classes for use in
working with Lightwave3D Object files.

Chunk ID constants are defined for all of the Chunk IDs listed
in the Lightwave 7.5 sdk.
It is important to note that this is a constant work-in-progress
and as such there are omissions and may be errors. Feedback and
suggestions would be appreciated.
There are two ways of using this unit. The first uses user-defines
callbacks to handle parsing lwo chunk data. The second way uses
object orientation.

Loading LWO chunk data via callbacks
A function is provided for loading a Lightwave object from a file.
The Loader itself uses a callback mechanism for the loading of
Lightwave object chunks. The callback is called for every chunk
(with the exception of the FORM and LWOB or LWO2 chunks).

The Chunk struct passed in the callback contains members for the
chunk ID, chunk size and pointer to chunk data. This data is
untouched internally so any parsing and numeric formatting
is up to you. This provides maximum flexibility and allows you to
handle the data that you need without loading the entire object
into ram first.

The chunk data memory is freed upon the return of the callback
so do not keep a reference to the chunk data. Copy it to your own
storage.

function LoadLW0(const Filename: string; ReadProc: TLWOReadProc;
 UserData: Pointer): LongWord; cdecl;

 Filename:      The fully qualified filename of the file to be
                loaded.

 ReadCallback:  The address of a TLWOReadCallback procedure
                defined as:
                TLWOReadCallback = procedure(Chunk: TLWChunk;
                  UserData: Pointer); cdecl;
                This procedure will be called for every chunk
                encountered in the Lightwave object file. The
                Chunk parameter is the chunk struct of the chunk
                being loaded. UserData is the pointer supplied
                in the original call to LoadLWO (see below).

 UserData:      A pointer to user supplied data to be passed
                in the ReadCallback.

A non-zero results indicates that the object file was parsed
successfully.

Loading LWO chunks via objects
============================
To load data from a lightwave object file, create an instance of
TLWObjectFile and call its LoadFromFile method.

The data can then be accessed with the Chunks array property and
iterated in combination with the ChunkCount property.

Chunk data is parsed and interfaced by descendents of the TLWChunk
class. I have made handlers for the following chunk types:

TLWLayr  Modeler Layer chunk
TLWPnts  Points chunk
TLWPols  Polygons chunk
TLWPTag  Polygon tag mapping
TLWSurf  Surface subchunk container
TLWTags  Tags (Name tag strings for named items)
TLWVMap  Vertex Mapping

The data for chunks without handlers can be gotten at with the
Data and Size properties of the TLWChunk. Data is a pointer to
the start of the chunk data. This data is unparsed.
Data is nil for descendents.


This should provide enough to move geometry into your favourite
delphi-based 3d engine.


Making chunk handler objects
============================

All chunk types are derived from TLWChunk in the following manner:

TLWChunk

ex:

TLWPTag        <- PTAG chunk type. polygon tag map.

TLWParentChunk <- A base class for chunks that can contain other chunks.
                 This is not necessarily how the data is stored in
                 the file but more for ease of access to data.
 ex:
 TLWPnts <- PNTS chunk type (points)
 TLWLayr <- LAYR chunk type (modeler layer)
 TLWSurf <- SURF chunk type (constains surface attributes as sub chunks)
 TLWSubChunk <- A base class for chunks whose max data len is 65536 bytes.
 TLWDiff   <- DIFF subchunk type (diffuse surface parameter)
 TLWSpec   <- SPEC subchunk type (specularity surface parameter)...
 etc.

Each descendent of TLWChunk or TLWSubChunk is required to override
the GetID class function, the LoadData method and the Clear method
to provide custom handling for chunktype data.

ex:
...
type
  TLWPnts = class (TLWParentChunk)
  private
    FPoints: TVEC12DynArray;
    function GetCount: LongWord;
    protected
    procedure Clear; override;
    procedure LoadData(AStream: TStream; DataStart, DataSize: LongWord); override;
  public
    class function GetID: TID4; override;
    function GetVMap(VMapID: TID4; out VMap: TLWVMap): boolean;
    property Count: LongWord read GetCount;
    property Points: TVEC12DynArray read FPoints;
  end;
...

// Return the the chunk id that is the target of this handler

class function TLWPnts.GetID: TID4;
begin
  result := ID_PNTS;
end;

// Load the point data - the stream is already positioned at the start of the chunk data

procedure TLWPnts.LoadData(AStream: TStream; DataStart, DataSize: LongWord);
begin
  SetLength(FPoints,DataSize div 12); // allocate storage for DataSize div 12 points
  ReadMotorolaNumber(AStream,@FPoints[0],4,DataSize div 4); // read the point data
end;


// Cleanup - Free any memory that you've allocated

procedure TLWPnts.Clear;
begin
  SetLength(FPoints,0);
end;


Utility Functions
=================
A function is provided for converting an array of numbers between
Motorola and Intel format (big endian <-> little endian). Converting
only needs to be done for numeric types that are of 2 or 4 byte
lengths.

procedure ReverseByteOrder(ValueIn: Pointer; Size: integer; Count: integer = 1);

 ValueIn: The address of a number or array of numbers to have their
          bytes swapped.
 Size:    The size in bytes of each numeric type.
 Count:   The count of numbers in the numbers array. The default
          value is 1.

Two routines are provided for reading and writing big endian
(Motorola and misc other processor vendors ) numbers to and from a
stream. These routines handle 2 and 4 byte numeric types and can
also handle arrays.

procedure ReadMotorolaNumber(Stream: TStream; Data: Pointer;
 ElementSize: integer; Count: integer = 1);

function WriteMotorolaNumber(Stream: TStream; Data: Pointer;
 ElementSize: integer; Count: integer = 1): Integer;

Each take a valid TStream descendent, a pointer to the numeric data,
the element size of the data elements (either 2 or 4) and the array
element count if sending an array. The default count is 1.

Notes for improvement of this unit:

- A version ID tag should be visible to all chunks in order to
 provide handling for Lightwave pre 6.0 object files.

- Chunk type handlers should leave memory allocation to
 the base class (TLWChunk) and act more as an interface
 to the data pointed to by Data in TLWChunk. This would
 keep memory allocation very efficient and make implementing
 chunk handlers even easier.

  Author:     Brian Johns brianjohns1@hotmail.com
  Purpose:    Lightwave object support unit for Delphi.
  Notes:      For the Lightwave Object File Format documentation please refer to
  http://www.lightwave3d.com/developer.
  Lightwave3D is a registered trademark of Newtek Incorporated.

===================================================================== *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.Math,
  GLS.VectorGeometry;

type

  TID4 = array [0 .. 3] of AnsiChar;
  PID4 = ^TID4;
  TID4DynArray = array of TID4;

const
  ID_NULL = '#0#0#0#0'; // NULL ID

  ID_LWSC: TID4 = 'LWSC'; // Lightwave scene file
  ID_FORM: TID4 = 'FORM'; // IFF Form
  ID_LWOB: TID4 = 'LWOB'; // Lightwave Object version 1.0 - 5.x
  ID_LWLO: TID4 = 'LWLO'; // Lightwave Layered Object
  ID_LAYR: TID4 = 'LAYR'; // LAYER
  ID_PNTS: TID4 = 'PNTS'; // Points chunk
  ID_SRFS: TID4 = 'SRFS'; // Surface Names chunk
  ID_POLS: TID4 = 'POLS'; // Polygons chunk
  ID_CRVS: TID4 = 'CRVS'; // Curves chunk
  ID_PCHS: TID4 = 'PCHS'; // Patches chunk
  ID_SURF: TID4 = 'SURF'; // Surfaces chunk
  ID_COLR: TID4 = 'COLR'; // Color chunk

  ID_FLAG: TID4 = 'FLAG'; // Surface Flags

  ID_LUMI: TID4 = 'LUMI'; // Luminosity
  ID_DIFF: TID4 = 'DIFF'; // Diffuse
  ID_SPEC: TID4 = 'SPEC'; // Specular
  ID_REFL: TID4 = 'REFL'; // Reflective
  ID_TRAN: TID4 = 'TRAN'; // Transparency

  ID_VLUM: TID4 = 'VLUM'; // Luminosity
  ID_VDIF: TID4 = 'VDIF'; // Diffuse
  ID_VSPC: TID4 = 'VSPC'; // Specularity
  ID_VRFL: TID4 = 'VRFL'; // Reflective
  ID_VTRN: TID4 = 'VTRN'; // Transparency

  ID_GLOS: TID4 = 'GLOS'; // Glossiness SmallInt

  ID_SIDE: TID4 = 'SIDE'; // Sidedness

  ID_RFLT: TID4 = 'RFLT'; // REFLECTION MODE (PRE 6.0)

  ID_RFOP: TID4 = 'RFOP'; // REFLECTION OPTIONS
  ID_RIMG: TID4 = 'RIMG'; // REFLECTION IMAGE
  ID_RSAN: TID4 = 'RSAN'; // REFLECTION MAP SEAM ANGLE
  ID_RIND: TID4 = 'RIND'; // REFRACTIVE INDEX
  ID_EDGE: TID4 = 'EDGE'; // EDGE TRANSPARENCY THRESHOLD
  ID_SMAN: TID4 = 'SMAN'; // SMOOTHING ANGLE RADIANS
  ID_ALPH: TID4 = 'ALPH'; // ALPHA MODE
  ID_CTEX: TID4 = 'CTEX'; // COLOR TEXTURE
  ID_DTEX: TID4 = 'DTEX'; // DIFFUSE TEXTURE
  ID_STEX: TID4 = 'STEX'; // SPECULAR TEXTURE
  ID_RTEX: TID4 = 'RTEX'; // REFLECTIION TEXTURE
  ID_TTEX: TID4 = 'TTEX'; // TRANSPARENCY TEXTURE
  ID_LTEX: TID4 = 'LTEX'; // LUMINANCE TEXTURE
  ID_BTEX: TID4 = 'BTEX'; // BUMP TEXTURE
  ID_TFLG: TID4 = 'TFLG'; // TEXTURE FLAGS
  ID_TSIZ: TID4 = 'TSIZ'; // TEXTURE SIZE
  ID_TCTR: TID4 = 'TCTR'; // TEXTURE CENTER
  ID_TFAL: TID4 = 'TFAL'; // TEXTURE FALLOFF
  ID_TVEL: TID4 = 'TVAL'; // TEXTURE VALUE
  ID_TREF: TID4 = 'TREF'; // TEXTURE REFERENCE
  ID_TCLR: TID4 = 'TCLR'; // TEXTURE COLOR
  ID_TVAL: TID4 = 'TVAL'; // TEXTURE VALUE
  ID_TAMP: TID4 = 'TAMP'; // TEXTURE AMPLITUDE
  ID_TFP0: TID4 = 'TFP0'; // TEXTURE PARAMETERS
  ID_TFP1: TID4 = 'TFP1'; //
  ID_TFP2: TID4 = 'TFP2'; //
  ID_TIP0: TID4 = 'TIP0'; //
  ID_TIP1: TID4 = 'TIP1'; //
  ID_TIP2: TID4 = 'TIP2'; //
  ID_TSP0: TID4 = 'TSP0'; //
  ID_TSP1: TID4 = 'TSP1'; //
  ID_TSP2: TID4 = 'TSP2'; //
  ID_TFRQ: TID4 = 'TFRQ'; //
  ID_TIMG: TID4 = 'TIMG'; // TEXTURE IMG
  ID_TALP: TID4 = 'TALP'; //
  ID_TWRP: TID4 = 'TWRP'; // TEXTURE WRAP
  ID_TAAS: TID4 = 'TAAS'; //
  ID_TOPC: TID4 = 'TOPC'; //
  ID_SHDR: TID4 = 'SHDR'; //
  ID_SDAT: TID4 = 'SDAT'; //
  ID_IMSQ: TID4 = 'IMSQ'; // IMAGE SEQUENCE
  ID_FLYR: TID4 = 'FLYR'; // FLYER SEQUENCE
  ID_IMCC: TID4 = 'IMCC'; //

  SURF_FLAG_LUMINOUS = 1;
  SURF_FLAG_OUTLINE = 2;
  SURF_FLAG_SMOOTHING = 4;
  SURF_FLAG_COLORHIGHLIGHTS = 8;
  SURF_FLAG_COLORFILTER = 16;
  SURF_FLAG_OPAQUEEDGE = 32;
  SURF_FLAG_TRANSPARENTEDGE = 64;
  SURF_FLAG_SHARPTERMINATOR = 128;
  SURF_FLAG_DOUBLESIDED = 256;
  SURF_FLAG_ADDITIVE = 512;
  SURF_FLAG_SHADOWALPHA = 1024;

  CURV_CONTINUITY_FIRST = 1;
  CURV_CONTINUITY_LAST = 2;

  IMSQ_FLAG_LOOP = 1;
  IMSQ_FLAG_INTERLACE = 2;

  ID_LWO2: TID4 = 'LWO2'; // OBJECT
  ID_VMAP: TID4 = 'VMAP'; // VERTEX MAP
  ID_TAGS: TID4 = 'TAGS'; // TAGS?
  ID_PTAG: TID4 = 'PTAG'; // POLYGON TAG MAP
  ID_VMAD: TID4 = 'VMAD'; // DISCONTINUOUS VERTEX MAP
  ID_ENVL: TID4 = 'ENVL'; // ENVELOPE
  ID_CLIP: TID4 = 'CLIP'; // CLIP
  ID_BBOX: TID4 = 'BBOX'; // BOUNDING BOX
  ID_DESC: TID4 = 'DESC'; // DESCRIPTION
  ID_TEXT: TID4 = 'TEXT'; // TEXT
  ID_ICON: TID4 = 'ICON'; // ICON

  ENVL_PRE: TID4 = 'PRE'#0; // PRE-BEHAVIOUR
  ENVL_POST: TID4 = 'POST'; // POST
  ENVL_KEY: TID4 = 'KEY'#0; // KEY
  ENVL_SPAN: TID4 = 'SPAN'; // SPAN
  ENVL_CHAN: TID4 = 'CHAN'; // CHAN
  ENVL_NAME: TID4 = 'NAME'; // NAME

  ID_STIL: TID4 = 'STIL'; // STILL IMAGE FILENAME
  ID_ISEQ: TID4 = 'ISEQ'; // IMAGE SEQUENCE
  ID_ANIM: TID4 = 'ANIM'; // PLUGIN ANIMATION
  ID_STCC: TID4 = 'STCC'; // COLOR CYCLING STILL
  ID_CONT: TID4 = 'CONT'; // CONTRAST
  ID_BRIT: TID4 = 'BRIT'; // BRIGHTNESS
  ID_SATR: TID4 = 'SATR'; // SATURATION
  ID_HUE: TID4 = 'HUE'#0; // HUE
  ID_GAMMA: TID4 = 'GAMM'; // GAMMA
  ID_NEGA: TID4 = 'NEGA'; // NEGATIVE IMAGE
  ID_IFLT: TID4 = 'IFLT'; // IMAGE PLUG-IN FILTER
  ID_PFLT: TID4 = 'PFLT'; // PIXEL PLUG-IN FILTER

  POLS_TYPE_FACE: TID4 = 'FACE'; // FACES
  POLS_TYPE_CURV: TID4 = 'CURV'; // CURVE
  POLS_TYPE_PTCH: TID4 = 'PTCH'; // PATCH
  POLS_TYPE_MBAL: TID4 = 'MBAL'; // METABALL
  POLS_TYPE_BONE: TID4 = 'BONE'; // SKELEGON?

  VMAP_TYPE_PICK: TID4 = 'PICK'; // SELECTION SET
  VMAP_TYPE_WGHT: TID4 = 'WGHT'; // WEIGHT MAP
  VMAP_TYPE_MNVW: TID4 = 'MNVW'; // SUBPATCH WEIGHT MAP
  VMAP_TYPE_TXUV: TID4 = 'TXUV'; // UV MAP
  VMAP_TYPE_RGB: TID4 = 'RGB'#0; // RGB MAP
  VMAP_TYPE_RGBA: TID4 = 'RGBA'; // RGBA MAP
  VMAP_TYPE_MORF: TID4 = 'MORF'; // MORPH MAP: RELATIVE VERTEX DISPLACEMENT
  VMAP_TYPE_SPOT: TID4 = 'SPOT'; // SPOT MAP: ABSOLUTE VERTEX POSITIONS

  PTAG_TYPE_SURF: TID4 = 'SURF'; // SURFACE
  PTAG_TYPE_PART: TID4 = 'PART'; // PARENT PART
  PTAG_TYPE_SMGP: TID4 = 'SMGP'; // SMOOTH GROUP

  PRE_POST_RESET = 0; // RESET
  PRE_POST_CONSTANT = 1; // CONSTANT
  PRE_POST_REPEAT = 2; // REPEAT
  PRE_POST_OSCILLATE = 3; // OSCILLATE
  PRE_POST_OFFSET = 4; // OFFSET REPEAT
  PRE_POST_LINEAR = 5; // LINEAR

  POLS_VCOUNT_MASK = $3FF;
  POLS_FLAGS_MASK = $FC00;

  SIDE_FRONT = 1;
  SIDE_BACK = 2;
  SIDE_FRONT_AND_BACK = SIDE_FRONT and SIDE_BACK;

  RFOP_BACKDROP = 0;
  RFOP_RAYTRACEANDBACKDROP = 1;
  RFOP_SPHERICALMAP = 2;
  RFOP_RAYTRACEANDSPHERICALMAP = 3;

type
  TI1 = ShortInt;
  PI1 = ^TI1;

  TI2 = SmallInt;
  PI2 = ^TI2;

  TI4 = LongInt;
  PI4 = ^TI4;

  TU1 = Byte;
  PU1 = ^TU1;
  TU1DynArray = array of TU1;

  TU2 = Word;
  PU2 = ^TU2;
  TU2Array = array [0 .. 65534] of TU2;
  PU2Array = ^TU2Array;
  TU2DynArray = array of TU2;

  TU4 = LongWord;
  PU4 = ^TU4;
  TU4Array = array [0 .. 65534] of TU4;
  PU4Array = ^TU4Array;
  TU4DynArray = array of TU4;

  TF4 = Single;
  PF4 = ^TF4;
  TF4Array = array [0 .. 65534] of TF4;
  PF4Array = ^TF4Array;
  TF4DynArray = array of TF4;

  TANG4 = TF4;
  PANG4 = ^TANG4;

  // TS0 = PAnsiChar;

  TVec12 = array [0 .. 2] of TF4;
  PVec12 = ^TVec12;

  TVec12Array = array [0 .. 65534] of TVec12;
  PVec12Array = ^TVec12Array;
  TVec12DynArray = array of TVec12;

  TColr12 = TVec12;
  PColr12 = ^TColr12;

  TColr12DynArray = array of TColr12;

  TColr4 = array [0 .. 3] of TU1;
  PColr4 = ^TColr4;

  // Lightwave Chunk Struct - Used in TLWOReadCallback
  PLWChunkRec = ^TLWChunkRec;

  TLWChunkRec = record
    id: TID4;
    size: TU4;
    data: Pointer;
  end;

  // Lightwave SubChunk Struct - Used in TLWOReadCallback
  PLWSubChunkRec = ^TLWSubChunkRec;

  TLWSubChunkRec = record
    id: TID4;
    size: TU2;
    data: Pointer;
  end;

  TLWPolsInfo = record
    norm: TVec12;
    vnorms: TVec12DynArray;
    surfid: TU2;
  end;

  TLWPolsInfoDynArray = array of TLWPolsInfo;

  TLWPntsInfo = record
    npols: TU2;
    pols: TU2DynArray;
  end;

  TLWPntsInfoDynArray = array of TLWPntsInfo;

  TLWPolsDynArray = TU2DynArray;

  TLWPolyTagMapDynArray = TU2DynArray;

  TLWPolyTagMap = record
    poly: TU2;
    tag: TU2;
  end;

  PLWPolyTagMap = ^TLWPolyTagMap;

  // Value Map
  TLWVertexMap = record
    vert: TU2;
    values: TF4DynArray;
  end;

  TLWVertexMapDynArray = array of TLWVertexMap;

  TLWChunkList = class;
  TLWParentChunk = class;

  TLWChunk = class(TPersistent)
  private
    FData: Pointer;
    FID: TID4;
    FSize: TU4;
    FParentChunk: TLWParentChunk;
    FOwner: TLWChunkList;
    function GetRootChunks: TLWChunkList;
    function GetIndex: Integer;
  protected
    procedure Clear; virtual;
    procedure LoadData(AStream: TStream;
      DataStart, DataSize: LongWord); virtual;
    procedure Loaded; virtual;
  public
    destructor Destroy; override;
    class function GetID: TID4; virtual;
    procedure LoadFromStream(AStream: TStream); virtual;
    property data: Pointer read FData;
    property id: TID4 read FID;
    property size: TU4 read FSize;
    // ParentChunk may be nil indicating this is a root chunk. ie. TLWLayr
    property ParentChunk: TLWParentChunk read FParentChunk;
    property RootChunks: TLWChunkList read GetRootChunks;
    property Index: Integer read GetIndex;
    property Owner: TLWChunkList read FOwner;
  end;

  TLWChunkClass = class of TLWChunk;

  TLWSubChunk = class(TLWChunk)
  public
    procedure LoadFromStream(AStream: TStream); override;
  end;

  TLWChunkFind = procedure(AChunk: TLWChunk; Criteria: Pointer;
    var Found: boolean);

  TLWChunkList = class(TList)
  private
    FOwnsItems: boolean;
    FOwner: TObject;
    function GetItem(Index: Integer): TLWChunk;
  protected
    procedure Loaded; virtual;
  public
    constructor Create(AOwnsItems: boolean; AOwner: TObject);
    destructor Destroy; override;
    function Add(AChunk: TLWChunk): Integer;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    function FindChunk(ChunkFind: TLWChunkFind; Criteria: Pointer;
      StartIndex: Integer = 0): Integer;
    property Items[Index: Integer]: TLWChunk read GetItem; default;
    property OwnsItems: boolean read FOwnsItems;
    property Owner: TObject read FOwner;
  end;

  TLWParentChunk = class(TLWChunk)
  private
    FItems: TLWChunkList;
    function GetItems: TLWChunkList;
    function GetFloatParam(Param: TID4): Single;
    function GetWordParam(Param: TID4): Word;
    function GetVec3Param(Param: TID4): TVec12;
    function GetLongParam(Param: TID4): LongWord;
    function GetVXParam(Param: TID4): Word;
  protected
    function GetParamAddr(Param: TID4): Pointer; virtual;
    procedure Clear; override;
    procedure Loaded; override;
  public
    property Items: TLWChunkList read GetItems;
    property ParamAddr[Param: TID4]: Pointer read GetParamAddr;
    property FloatParam[Param: TID4]: Single read GetFloatParam;
    property WordParam[Param: TID4]: Word read GetWordParam;
    property LongParam[Param: TID4]: LongWord read GetLongParam;
    property Vec3Param[Param: TID4]: TVec12 read GetVec3Param;
    property VXParam[Param: TID4]: Word read GetVXParam;
  end;

  TLWVMap = class;

  TLWPnts = class(TLWParentChunk)
  private
    FPnts: TVec12DynArray;
    FPntsInfo: TLWPntsInfoDynArray;
    function GetPntsCount: LongWord;
    function AddPoly(PntIdx, PolyIdx: Integer): Integer;
  protected
    procedure Clear; override;
    procedure LoadData(AStream: TStream;
      DataStart, DataSize: LongWord); override;
  public
    class function GetID: TID4; override;
    function GetVMap(VMapID: TID4; out VMap: TLWVMap): boolean;
    property PntsCount: LongWord read GetPntsCount;
    property Pnts: TVec12DynArray read FPnts;
    property PntsInfo: TLWPntsInfoDynArray read FPntsInfo;
  end;

  TLWPols = class(TLWParentChunk)
  private
    FPolsType: TID4;
    FPols: TLWPolsDynArray;
    FPolsInfo: TLWPolsInfoDynArray;
    FPolsCount: Integer;
    function GetPolsByIndex(AIndex: TU2): Integer;
    function GetIndiceCount: TU4;
    function GetIndice(AIndex: Integer): TU2;
    function GetPolsCount: Integer;
    procedure CalcPolsNormals;
    procedure CalcPntsNormals;
  protected
    procedure Clear; override;
    procedure LoadData(AStream: TStream;
      DataStart, DataSize: LongWord); override;
    procedure Loaded; override;
  public
    class function GetID: TID4; override;
    function GetPolsByPntIdx(VertIdx: TU2; var VertPolys: TU2DynArray): Integer;
    property PolsByIndex[AIndex: TU2]: Integer read GetPolsByIndex;
    property IndiceCount: TU4 read GetIndiceCount;
    property Indices[AIndex: Integer]: TU2 read GetIndice;
    property PolsType: TID4 read FPolsType;
    property PolsCount: Integer read GetPolsCount;
    property PolsInfo: TLWPolsInfoDynArray read FPolsInfo;
  end;

  TLWVMap = class(TLWChunk)
  private
    FDimensions: TU2;
    FName: string;
    FValues: TLWVertexMapDynArray;
    FVMapType: TID4;
    function GetValue(AIndex: TU2): TLWVertexMap;
    function GetValueCount: Integer;
  protected
    procedure Clear; override;
    procedure LoadData(AStream: TStream;
      DataStart, DataSize: LongWord); override;
  public
    class function GetID: TID4; override;
    property Dimensions: TU2 read FDimensions;
    property Name: string read FName;
    property Value[AIndex: TU2]: TLWVertexMap read GetValue;
    property ValueCount: Integer read GetValueCount;
    property VMapType: TID4 read FVMapType;
  end;

  TLWTags = class(TLWChunk)
  private
    FTags: TStrings;
    function GetTags: TStrings;
  protected
    procedure Clear; override;
    procedure LoadData(AStream: TStream;
      DataStart, DataSize: LongWord); override;
  public
    destructor Destroy; override;
    class function GetID: TID4; override;
    function TagToName(tag: TU2): string;
    property Tags: TStrings read GetTags;
  end;

  TLWSurf = class(TLWParentChunk)
  private
    FName: string;
    FSource: string;
    function GetSurfId: Integer;
  protected
    function GetParamAddr(Param: TID4): Pointer; override;
    procedure LoadData(AStream: TStream;
      DataStart, DataSize: LongWord); override;
  public
    destructor Destroy; override;
    class function GetID: TID4; override;
    property surfid: Integer read GetSurfId;
    property Name: string read FName;
    property Source: string read FSource;
  end;

  TLWLayr = class(TLWParentChunk)
  private
    FFlags: TU2;
    FName: string;
    FNumber: TU2;
    FParent: TU2;
    FPivot: TVec12;
  protected
    procedure LoadData(AStream: TStream;
      DataStart, DataSize: LongWord); override;
  public
    destructor Destroy; override;
    class function GetID: TID4; override;
    property Flags: TU2 read FFlags;
    property Name: string read FName;
    property Number: TU2 read FNumber;
    property Parent: TU2 read FParent;
    property Pivot: TVec12 read FPivot;
  end;

  TLWPTag = class(TLWChunk)
  private
    FMapType: TID4;
    FTagMaps: TLWPolyTagMapDynArray;
    FTags: TU2DynArray;
    function AddTag(Value: TU2): Integer;
    function GetTag(AIndex: Integer): TU2;
    function GetTagCount: Integer;
    function GetTagMapCount: Integer;
    function GetTagMaps(AIndex: Integer): TLWPolyTagMap;
    procedure ValidateTagInfo;
  protected
    procedure Clear; override;
    procedure LoadData(AStream: TStream;
      DataStart, DataSize: LongWord); override;
  public
    constructor Create;
    function GetPolsByTag(tag: TU2; var PolyIndices: TU2DynArray): Integer;
    class function GetID: TID4; override;
    property MapType: TID4 read FMapType;
    property TagCount: Integer read GetTagCount;
    property TagMapCount: Integer read GetTagMapCount;
    property TagMaps[AIndex: Integer]: TLWPolyTagMap read GetTagMaps; default;
    property Tags[AIndex: Integer]: TU2 read GetTag;
  end;

  TLWObjectFile = class(TObject)
  private
    FChunks: TLWChunkList;
    FFileName: string;
    function GetChunks: TLWChunkList;
    function GetCount: Integer;
    function GetSurfaceByName(Index: string): TLWSurf;
    function GetSurfaceByTag(Index: TU2): TLWSurf;
  public
    constructor Create;
    destructor Destroy; override;
    function TagToName(tag: TU2): string;
    procedure LoadFromFile(const AFilename: string);
    procedure LoadFromStream(AStream: TStream);
    property ChunkCount: Integer read GetCount;
    property Chunks: TLWChunkList read GetChunks;
    property FileName: string read FFileName;
    property SurfaceByName[Index: string]: TLWSurf read GetSurfaceByName;
    property SurfaceByTag[Index: TU2]: TLWSurf read GetSurfaceByTag;
  end;

  TLWClip = class(TLWParentChunk)
  private
    FClipIndex: TU4;
  protected
    procedure LoadData(AStream: TStream;
      DataStart, DataSize: LongWord); override;
  public
    class function GetID: TID4; override;
    property ClipIndex: TU4 read FClipIndex;
  end;

  TLWContentNotify = procedure(Sender: TObject; var Content: string) of object;

  TLWContentDir = class
  private
    FSubDirs: TStrings;
    FRoot: string;
    function GetSubDirs: TStrings;
    procedure SetRoot(const Value: string);
    procedure SetSubDirs(const Value: TStrings);
    // function ContentSearch(AFilename: string): string;
  public
    destructor Destroy; override;
    function FindContent(AFilename: string): string;
    property Root: string read FRoot write SetRoot;
    property SubDirs: TStrings read GetSubDirs write SetSubDirs;
  end;

  TLWOReadCallback = procedure(Chunk: TLWChunkRec; data: Pointer); cdecl;

procedure RegisterChunkClass(ChunkClass: TLWChunkClass);

function LoadLW0FromStream(Stream: TStream; ReadCallback: TLWOReadCallback;
  UserData: Pointer): LongWord; cdecl;
function LoadLWOFromFile(const AFilename: string;
  ReadCallback: TLWOReadCallback; UserData: Pointer): LongWord;

procedure ReadMotorolaNumber(Stream: TStream; data: Pointer;
  ElementSize: Integer; Count: Integer = 1);
function WriteMotorolaNumber(Stream: TStream; data: Pointer;
  ElementSize: Integer; Count: Integer = 1): Integer;

function ReadS0(Stream: TStream; out Str: string): Integer;
procedure WriteS0(Stream: TStream; data: string);

procedure WriteU4AsVX(Stream: TStream; data: Pointer; Count: Integer);
function ReadVXAsU4(Stream: TStream; data: Pointer; Count: Integer = 1)
  : Integer;

procedure ReverseByteOrder(ValueIn: Pointer; size: Integer; Count: Integer = 1);

function ToDosPath(const Path: string): string;
function ToUnixPath(const Path: string): string;

function ID4ToInt(const id: TID4): Integer;

// ChunkFind procedures
procedure FindChunkById(AChunk: TLWChunk; data: Pointer; var Found: boolean);
procedure FindSurfaceByName(AChunk: TLWChunk; AName: Pointer;
  var Found: boolean);
procedure FindSurfaceByTag(AChunk: TLWChunk; ATag: Pointer; var Found: boolean);

procedure FindVMapByName(AChunk: TLWChunk; AName: Pointer; var Found: boolean);
procedure FindClipByClipIndex(AChunk: TLWChunk; AIndex: Pointer;
  var Found: boolean);

function GetContentDir: TLWContentDir;

// --------------------------------------------------------------------
implementation
// --------------------------------------------------------------------

type
  PWord = ^Word;
  PLongWord = ^LongWord;

var
  ChunkClasses: TList;
  ContentDir: TLWContentDir;

function ToDosPath(const Path: string): string;
var
  i: Integer;
begin
  result := Path;
  for i := 1 to Length(result) do
    if result[i] = '/' then
      result[i] := '\';
end;

function ToUnixPath(const Path: string): string;
var
  i: Integer;
begin
  result := Path;
  for i := 1 to Length(result) do
    if result[i] = '\' then
      result[i] := '/';
end;

function GetContentDir: TLWContentDir;
begin
  if ContentDir = nil then
    ContentDir := TLWContentDir.Create;
  result := ContentDir;
end;

procedure FindChunkById(AChunk: TLWChunk; data: Pointer; var Found: boolean);
begin
  if AChunk.FID = PID4(data)^ then
    Found := true
  else
    Found := false;
end;

procedure FindClipByClipIndex(AChunk: TLWChunk; AIndex: Pointer;
  var Found: boolean);
begin
  if (AChunk is TLWClip) and (TLWClip(AChunk).ClipIndex = PU2(AIndex)^) then
    Found := true;
end;

procedure FindSurfaceByName(AChunk: TLWChunk; AName: Pointer;
  var Found: boolean);
begin
  if (AChunk is TLWSurf) and (TLWSurf(AChunk).Name = PString(AName)^) then
    Found := true;
end;

procedure FindSurfaceByTag(AChunk: TLWChunk; ATag: Pointer; var Found: boolean);
begin
  if (AChunk is TLWSurf) and (TLWSurf(AChunk).surfid = PU2(ATag)^) then
    Found := true;
end;

procedure FindVMapByName(AChunk: TLWChunk; AName: Pointer; var Found: boolean);
begin
  if (AChunk is TLWVMap) and (TLWVMap(AChunk).Name = PString(AName)^) then
    Found := true;
end;

function VecAdd(v1, v2: TVec12): TVec12;
begin
  result[0] := v1[0] + v2[0];
  result[1] := v1[1] + v2[1];
  result[2] := v1[2] + v2[2];
end;

function VecSub(v1, v2: TVec12): TVec12;
begin
  result[0] := v1[0] - v2[0];
  result[1] := v1[1] - v2[1];
  result[2] := v1[2] - v2[2];
end;

function VecCross(v1, v2: TVec12): TVec12;
begin
  result[0] := v1[1] * v2[2] - v1[2] * v2[1];
  result[1] := v1[2] * v2[0] - v1[0] * v2[2];
  result[2] := v1[0] * v2[1] - v1[1] * v2[0];
end;

function VecDot(v1, v2: TVec12): TF4;
begin
  result := v1[0] * v2[0] + v1[1] * v2[1] + v1[2] * v2[2];
end;

function VecNorm(v: TVec12): TVec12;
var
  mag: TF4;
begin
  mag := Sqrt(VecDot(v, v));

  if mag > 0 then
    mag := 1 / mag;

  result[0] := v[0] * mag;
  result[1] := v[1] * mag;
  result[2] := v[2] * mag;
end;

function CalcPlaneNormal(v1, v2, v3: TVec12): TVec12;
var
  e1, e2: TVec12;
begin
  e1 := VecSub(v2, v1);
  e2 := VecSub(v3, v1);
  result := VecCross(e1, e2);
  result := VecNorm(result);
end;

procedure FindSurfByName(Chunk: TLWChunk; var Found: boolean);
begin

end;

(*-----------------------------------------------------------------------------
  Procedure: GetChunkClasses
  Date:      08-Aug-2002
  Arguments: None
  Result:    TClassList

  Singleton access for the chunk class list.
  -----------------------------------------------------------------------------*)
function GetChunkClasses: TList;
begin
  if ChunkClasses = nil then
    ChunkClasses := TList.Create;
  result := ChunkClasses;
end;

procedure UnRegisterChunkClasses;
var
  i: Integer;
begin
  with GetChunkClasses do
    for i := 0 to Count - 1 do
      UnregisterClass(TPersistentClass(Items[i]));
end;

(*-----------------------------------------------------------------------------
  Procedure: RegisterChunkClass
  Date:      08-Aug-2002
  Arguments: ChunkClass: TLWChunkClass
  Result:    None

  Adds a user defined chunk class to the chunk class list.
  -----------------------------------------------------------------------------*)
procedure RegisterChunkClass(ChunkClass: TLWChunkClass);
begin
  GetChunkClasses.Add(ChunkClass);
  // if FindClass(ChunkClass.ClassName) <> nil then
  // UnRegisterClass(ChunkClass);
  // RegisterClass(ChunkClass);
end;

(*-----------------------------------------------------------------------------
  Procedure: GetChunkClass
  Date:      08-Aug-2002
  Arguments: ChunkID: TID4
  Result:    TLWChunkClass

  Returns the chunk class associated with ChunkID.
  -----------------------------------------------------------------------------*)
function GetChunkClass(ChunkID: TID4; ADefault: TLWChunkClass): TLWChunkClass;
var
  i: Integer;
begin

  if ADefault = nil then
    result := TLWChunk
  else
    result := ADefault;

  for i := 0 to ChunkClasses.Count - 1 do
  begin

    if TLWChunkClass(ChunkClasses.Items[i]).GetID = ChunkID then
    begin

      result := TLWChunkClass(ChunkClasses.Items[i]);
      Exit;

    end;

  end;

end;

(*-----------------------------------------------------------------------------
  Procedure: Tokenize
  Date:      08-Aug-2002
  Arguments: const Src: string; Delimiter: Char; Dst: TStrings
  Result:    None

  Breaks up a string into TStrings items when the Delimiter character is
  encountered.
  -----------------------------------------------------------------------------*)
procedure Tokenize(const Src: string; Delimiter: Char; Dst: TStrings);
var
  i, L, SL: Integer;
  SubStr: string;
begin
  if Dst = nil then
    Exit;

  L := Length(Src);
  if (L = 0) or (Dst = nil) then
    Exit;
  SubStr := '';
  for i := 1 to L do
  begin
    if (Src[i] <> Delimiter) then
      SubStr := SubStr + Src[i]
    else
    begin
      SL := Length(SubStr);
      if SL > 0 then
      begin
        Dst.Add(SubStr);
        SubStr := '';
      end;
    end;
  end;
  if Length(SubStr) > 0 then
    Dst.Add(SubStr);
end;

(*-----------------------------------------------------------------------------
  Procedure: LoadLW0FromStream
  Date:      08-Aug-2002
  Arguments: Stream: TStream; ReadCallback: TLWOReadCallback; UserData: Pointer
  Result:    LongWord
  -----------------------------------------------------------------------------*)
function LoadLW0FromStream(Stream: TStream; ReadCallback: TLWOReadCallback;
  UserData: Pointer): LongWord;
var
  Chunk: TLWChunkRec;
  CurId: TID4;
  StartPos, CurSize: TU4;

begin
  try
    Stream.Read(CurId, 4);

    ReadMotorolaNumber(Stream, @CurSize, 4);

    if UpperCase(string(CurId)) = 'FORM' then
    begin
      Stream.Read(CurId, 4);
    end
    else
      raise Exception.Create
        ('Invalid magic number. Not a valid Lightwave Object');
    with Stream do
      while Position < size do
      begin
        Read(Chunk, 8);
        ReverseByteOrder(@Chunk.size, 4);
        StartPos := Position;
        GetMem(Chunk.data, Chunk.size);
        Stream.Read(Chunk.data^, Chunk.size);
        if Assigned(ReadCallback) then
          ReadCallback(Chunk, UserData);
        FreeMem(Chunk.data, Chunk.size);
        Position := StartPos + Chunk.size + (StartPos + Chunk.size) mod 2;
      end;
    Stream.Free;
    result := High(LongWord);
  except
    On E: Exception do
    begin
      Stream.Free;
      result := 0;
    end;
  end;
end;

function LoadLWOFromFile(const AFilename: String;
  ReadCallback: TLWOReadCallback; UserData: Pointer): LongWord;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFilename, fmOpenRead);
  try
    result := LoadLW0FromStream(Stream, ReadCallback, UserData);
  finally
    Stream.Free;
  end;
end;

procedure ReverseByteOrder(ValueIn: Pointer; size: Integer; Count: Integer = 1);
var
  W: Word;
  pB: PByte;
  Blo, Bhi: Byte;
  L: LongWord;
  i: Integer;
begin
  i := 0;

  case size of
    2:
      begin

        while i < Count do
        begin
          W := PU2Array(ValueIn)^[i];

          pB := @W;
          Blo := pB^;
          Inc(pB);
          Bhi := pB^;
          pB^ := Blo;
          Dec(pB);
          pB^ := Bhi;
          PU2Array(ValueIn)^[i] := W;

          Inc(i);
        end;
      end;

    4:
      begin
        while i < Count do
        begin
          L := PU4Array(ValueIn)^[i];
          pB := @W;
          Blo := pB^;
          Inc(pB);
          Bhi := pB^;
          pB^ := Blo;
          Dec(pB);
          pB^ := Bhi;
          PU4Array(ValueIn)^[i] := L;

          Inc(i);
        end;
      end;
  else
    raise Exception.Create('Lightwave.ReverseByteOrder: Invalid Size = ' +
      IntToStr(size));
  end;
end;

procedure ReadMotorolaNumber(Stream: TStream; data: Pointer;
  ElementSize: Integer; Count: Integer = 1);
begin
  Stream.Read(data^, Count * ElementSize);

  if (ElementSize = 2) or (ElementSize = 4) then
    ReverseByteOrder(data, ElementSize, Count);
end;

function WriteMotorolaNumber(Stream: TStream; data: Pointer;
  ElementSize: Integer; Count: Integer = 1): Integer;
var
  TempData: Pointer;
begin
  result := 0;
  if data <> nil then
  begin
    TempData := AllocMem(ElementSize * Count);
    try
      if (ElementSize = 2) or (ElementSize = 4) then
        ReverseByteOrder(TempData, ElementSize, Count);
      result := Stream.Write(data, Count * ElementSize);
    except
      on E: Exception do
      begin
        FreeMem(TempData, Count * ElementSize);
        raise;
      end;
    end;
  end;
end;

function ReadS0(Stream: TStream; out Str: string): Integer;
var
  Buf: array [0 .. 1] of AnsiChar;
  StrBuf: string;
begin
  Stream.Read(Buf, 2);
  StrBuf := '';
  while Buf[1] <> #0 do
  begin
    StrBuf := StrBuf + string(Buf);
    Stream.Read(Buf, 2);
  end;

  if Buf[0] <> #0 then
    StrBuf := StrBuf + Char(Buf[0]);

  Str := Copy(StrBuf, 1, Length(StrBuf));
  result := Length(Str) + 1;
  result := result + (result mod 2);
end;

function ValueOfVX(VX: Pointer): TU4;
var
  TmpU2: TU2;
  TmpU4: TU4;
begin
  if PU1(VX)^ = $FF then
  begin
    TmpU4 := TU4(PU1(VX)^) and $FFFFFFF0;
    ReverseByteOrder(@TmpU4, 4);
  end
  else
  begin
    TmpU2 := TU2(PU2(VX)^);
    ReverseByteOrder(@TmpU2, 2);
    TmpU4 := TmpU2;
  end;
  result := TmpU4;
end;

function ReadVXAsU4(Stream: TStream; data: Pointer; Count: Integer = 1)
  : Integer;
var
  i, ReadCount: Integer;
  BufByte: Byte;
  TempU2: TU2;
begin
  ReadCount := 0;
  for i := 0 to Count - 1 do
  begin

    Stream.Read(BufByte, 1);
    Stream.Position := Stream.Position - 1;

    if BufByte = 255 then
    begin
      Stream.Read(data^, SizeOf(TU4));
      PU4Array(data)^[i] := PU4Array(data)^[i] and $FFFFFFF0;
      ReverseByteOrder(data, SizeOf(TU4));
      Inc(ReadCount, 4);
    end
    else
    begin
      Stream.Read(TempU2, SizeOf(TU2));
      ReverseByteOrder(@TempU2, SizeOf(TU2));
      PU4Array(data)^[i] := TempU2;
      Inc(ReadCount, 2);
    end;

  end;
  result := ReadCount;
end;

function ReadVXAsU2(Stream: TStream; data: Pointer; Count: Integer = 1)
  : Integer;
var
  i, ReadCount: Integer;
  BufByte: Byte;
  TempU2: TU2;
begin
  ReadCount := 0;
  for i := 0 to Count - 1 do
  begin
    Stream.Read(BufByte, 1);
    Stream.Position := Stream.Position - 1;
    if BufByte = 255 then
    begin
      Stream.Position := Stream.Position + 4;
      PU2Array(data)^[i] := 0;
      Inc(ReadCount, 4);
    end
    else
    begin
      Stream.Read(TempU2, SizeOf(TU2));
      ReverseByteOrder(@TempU2, SizeOf(TU2));
      PU2Array(data)^[i] := TempU2;
      Inc(ReadCount, 2);
    end;
  end;
  result := ReadCount;
end;

procedure WriteS0(Stream: TStream; data: string);
begin
  // ToDo: WriteS0
end;

procedure WriteU4AsVX(Stream: TStream; data: Pointer; Count: Integer);
var
  i: Integer;
  TempU2: TU2;
begin
  for i := 0 to Count - 1 do
  begin
    if PU4Array(data)^[i] < 65280 then
    begin
      TempU2 := PU4Array(data)^[i];
      WriteMotorolaNumber(Stream, @TempU2, SizeOf(TU2));
    end
    else
      WriteMotorolaNumber(Stream, data, SizeOf(TU4));
  end;
end;

type
  PInteger = ^Integer;

function ID4ToInt(const id: TID4): Integer;
var
  TmpId: AnsiString;
begin

  TmpId := id;

  TmpId := AnsiString(UpperCase(string(id)));

  result := PInteger(@TmpId)^;

end;

(*********************************** TLWChunk ********************************)

destructor TLWChunk.Destroy;
begin
  Clear;
  inherited;
end;

procedure TLWChunk.Clear;
begin
  FreeMem(FData, FSize);
  FSize := 0;
  FData := nil;
end;

class function TLWChunk.GetID: TID4;
begin
  result := #0#0#0#0;
end;

procedure TLWChunk.LoadData(AStream: TStream; DataStart, DataSize: LongWord);
begin
  GetMem(FData, DataSize);
  AStream.Read(PByteArray(FData)^[0], DataSize);
end;

procedure TLWChunk.LoadFromStream(AStream: TStream);
var
  DataStart: Integer;
  DataSize: TU4;
begin
  with AStream do
  begin

    ReadMotorolaNumber(AStream, @DataSize, 4);

    DataStart := Position;

    FSize := DataSize;

    LoadData(AStream, DataStart, DataSize);

    Position := Cardinal(DataStart) + DataSize +
      (Cardinal(DataStart) + DataSize) mod 2;

  end;
end;


(********************************* TLWChunkList *******************************)

constructor TLWChunkList.Create(AOwnsItems: boolean; AOwner: TObject);
begin
  inherited Create;
  FOwnsItems := AOwnsItems;
  FOwner := AOwner;
end;

destructor TLWChunkList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TLWChunkList.Clear;
begin
  while Count > 0 do
    Delete(Count - 1);
  inherited;
end;

procedure TLWChunkList.Delete(Index: Integer);
begin
  if FOwnsItems then
    Items[Index].Free;
  inherited Delete(Index);
end;

function TLWChunkList.GetItem(Index: Integer): TLWChunk;
begin
  result := TLWChunk(inherited Items[Index]);
end;

(******************************** TLWObjectFile *******************************)

constructor TLWObjectFile.Create;
begin

  inherited;

end;

destructor TLWObjectFile.Destroy;
begin

  FreeAndNil(FChunks);

  inherited;

end;

function TLWObjectFile.GetChunks: TLWChunkList;
begin
  if FChunks = nil then
    FChunks := TLWChunkList.Create(true, Self);
  result := FChunks;
end;

function TLWObjectFile.GetCount: Integer;
begin
  result := Chunks.Count;
end;

function TLWObjectFile.GetSurfaceByName(Index: string): TLWSurf;
var
  SurfIdx: Integer;
begin
  SurfIdx := Chunks.FindChunk(@FindSurfaceByName, @Index, 0);
  if SurfIdx <> -1 then
    result := TLWSurf(Chunks[SurfIdx])
  else
    result := nil;
end;

function TLWObjectFile.GetSurfaceByTag(Index: TU2): TLWSurf;
var
  TagName: string;
begin
  TagName := TagToName(Index);
  result := SurfaceByName[TagName];
end;

procedure TLWObjectFile.LoadFromFile(const AFilename: string);
var
  Stream: TMemoryStream;
begin

  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(AFilename);

    LoadFromStream(Stream);
    Stream.Free;
    FFileName := AFilename;
  except
    on E: Exception do
    begin
      Stream.Free;
      raise;
    end;
  end;

end;

procedure TLWObjectFile.LoadFromStream(AStream: TStream);
var
  CurId: TID4;
  CurSize: LongWord;
  CurPnts, CurPols, CurItems: TLWChunkList;
begin
  CurPols := nil;
  CurPnts := nil;

  AStream.Read(CurId, 4);

  ReadMotorolaNumber(AStream, @CurSize, 4);

  if UpperCase(string(CurId)) = 'FORM' then
  begin

    AStream.Read(CurId, 4);

    if CurId <> 'LWO2' then
      raise Exception.Create
        ('Only Version 6.0+ version objects are supported.');

  end
  else
    raise Exception.Create
      ('Invalid magic number. Not a valid Lightwave Object');

  CurItems := Chunks;

  while AStream.Position < AStream.size do
  begin
    AStream.Read(CurId, 4);

    if (CurId = ID_PTAG) then
    begin
      CurPols.Add(GetChunkClass(CurId, TLWChunk).Create);

      with CurPols[CurPols.Count - 1] do
      begin
        FID := CurId;
        LoadFromStream(AStream);
      end;
    end
    else if (CurId = ID_VMAP) or (CurId = ID_VMAD) then
    begin
      CurPnts.Add(GetChunkClass(CurId, TLWChunk).Create);
      with CurPnts[CurPnts.Count - 1] do
      begin
        FID := CurId;
        LoadFromStream(AStream);
      end;
    end
    else
    begin
      if (CurId = ID_LAYR) or (CurId = ID_SURF) or (CurId = ID_TAGS) or
        (CurId = ID_CLIP) then
        CurItems := Chunks;
      CurItems.Add(GetChunkClass(CurId, TLWChunk).Create);
      with CurItems[CurItems.Count - 1] do
      begin
        FID := CurId;
        LoadFromStream(AStream);
      end;

    end;

    if CurId = ID_LAYR then
      CurItems := TLWParentChunk(CurItems[CurItems.Count - 1]).Items
    else if CurId = ID_POLS then
      CurPols := TLWParentChunk(CurItems[CurItems.Count - 1]).Items
    else if CurId = ID_PNTS then
      CurPnts := TLWParentChunk(CurItems[CurItems.Count - 1]).Items;
  end;
  Chunks.Loaded;
end;

(*********************************** TLWPnts **********************************)

function TLWPnts.AddPoly(PntIdx, PolyIdx: Integer): Integer;
var
  i, L: Integer;
begin
  // DONE: Pnts.AddPoly

  for i := 0 to FPntsInfo[PntIdx].npols - 1 do
  begin
    if FPntsInfo[PntIdx].pols[i] = PolyIdx then
    begin
      result := i;
      Exit;
    end;
  end;

  L := Length(FPntsInfo[PntIdx].pols);
  SetLength(FPntsInfo[PntIdx].pols, L + 1);
  FPntsInfo[PntIdx].npols := L + 1;
  FPntsInfo[PntIdx].pols[L] := PolyIdx;
  result := L;
end;

procedure TLWPnts.Clear;
var
  i: Integer;
begin
  for i := 0 to PntsCount - 1 do
    SetLength(FPntsInfo[i].pols, 0);
  SetLength(FPntsInfo, 0);
  SetLength(FPnts, 0);
end;

function TLWPnts.GetPntsCount: LongWord;
begin
  result := Length(FPnts);
end;

class function TLWPnts.GetID: TID4;
begin
  result := ID_PNTS;
end;

function TLWPnts.GetVMap(VMapID: TID4; out VMap: TLWVMap): boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to Items.Count - 1 do
  begin
    if (Items[i] is TLWVMap) and (TLWVMap(Items[i]).VMapType = VMapID) then
    begin

      result := true;
      VMap := TLWVMap(Items[i]);
      Exit;
    end;

  end;

end;

procedure TLWPnts.LoadData(AStream: TStream; DataStart, DataSize: LongWord);
begin
  SetLength(FPnts, DataSize div 12);
  // allocate storage for DataSize div 12 points
  SetLength(FPntsInfo, DataSize div 12); // Point info
  ReadMotorolaNumber(AStream, @FPnts[0], 4, DataSize div 4);
  // read the point data
end;

(*********************************** TLWPols **********************************)

procedure TLWPols.CalcPolsNormals;
var
  i, j, PolyIdx: Integer;
  Pnts: TLWPnts;
begin
  if IndiceCount = 0 then
    Exit;

  with ParentChunk as TLWLayr do
    Pnts := TLWPnts(Items[Items.FindChunk(@FindChunkById, @ID_PNTS, 0)]);

  for PolyIdx := 0 to FPolsCount - 1 do
  begin
    // DONE: call Pnts.AddPoly
    i := PolsByIndex[PolyIdx];
    with Pnts do
    begin
      for j := 1 to Indices[i] do
        AddPoly(Indices[i + j], PolyIdx);
      SetLength(FPolsInfo[PolyIdx].vnorms, Indices[i]);
      if Indices[PolyIdx] > 2 then
        FPolsInfo[PolyIdx].norm := CalcPlaneNormal(Pnts[Indices[i + 1]],
          Pnts[Indices[i + 2]], Pnts[Indices[i + 3]])
      else
        FPolsInfo[PolyIdx].norm := VecNorm(Pnts[Indices[i + 1]]);
    end;
  end;
end;

procedure TLWPols.Clear;
var
  i: Integer;
begin
  for i := 0 to FPolsCount - 1 do
    SetLength(FPolsInfo[i].vnorms, 0);
  SetLength(FPolsInfo, 0);
  SetLength(FPols, 0);
end;

function TLWPols.GetPolsByIndex(AIndex: TU2): Integer;
var
  i, cnt: Cardinal;
begin
  result := -1;
  i := 0;
  cnt := 0;

  if AIndex = 0 then
  begin
    result := 0;
    Exit;
  end;

  while (i < IndiceCount - 1) and (cnt <> AIndex) do
  begin
    Inc(i, Indices[i] + 1);
    Inc(cnt);
  end;
  if cnt = AIndex then
    result := i;
end;

class function TLWPols.GetID: TID4;
begin
  result := ID_POLS;
end;

function TLWPols.GetIndiceCount: TU4;
begin
  result := Length(FPols);
end;

function TLWPols.GetIndice(AIndex: Integer): TU2;
begin
  result := FPols[AIndex];
end;

function TLWPols.GetPolsCount: Integer;
begin
  result := FPolsCount;
end;

procedure TLWPols.LoadData(AStream: TStream; DataStart, DataSize: LongWord);
var
  EndPos: Integer;
  Idx: TU4;
  TmpU2: TU2;
begin

  Idx := 0;
  EndPos := DataStart + DataSize;

  with AStream do
  begin

    Read(FPolsType, 4);

    // To avoid memory manager hits, set an estimate length of indices
    SetLength(FPols, (DataSize - 4) div 2);

    while Position < EndPos do
    begin

      ReadMotorolaNumber(AStream, @FPols[Idx], 2);
      TmpU2 := FPols[Idx] and POLS_VCOUNT_MASK;

      ReadVXAsU2(AStream, @FPols[Idx + 1], TmpU2);
      Inc(Idx, FPols[Idx] + 1);
      Inc(FPolsCount);
    end;
    // correct length estimate errors if any
    if (Idx + 1) < Cardinal(Length(FPols)) then
      SetLength(FPols, Idx + 1);
  end;
  SetLength(FPolsInfo, FPolsCount);
  CalcPolsNormals;
end;

(*********************************** TLWVMap **********************************)

procedure TLWVMap.Clear;
var
  i: Integer;
begin
  for i := 0 to Length(FValues) - 1 do
    SetLength(FValues[i].values, 0);
  SetLength(FValues, 0);
end;

class function TLWVMap.GetID: TID4;
begin
  result := ID_VMAP;
end;

function TLWVMap.GetValue(AIndex: TU2): TLWVertexMap;
begin
  result := FValues[AIndex];
end;

function TLWVMap.GetValueCount: Integer;
begin
  result := Length(FValues);
end;

procedure TLWVMap.LoadData(AStream: TStream; DataStart, DataSize: LongWord);
var
  Idx: TU4;
begin
  Idx := 0;
  with AStream do
  begin
    Read(FVMapType, 4);
    ReadMotorolaNumber(AStream, @FDimensions, 2);
    ReadS0(AStream, FName);
    if FDimensions > 0 then
    begin
      while Cardinal(Position) < (DataStart + DataSize) do
      begin
        SetLength(FValues, Length(FValues) + 1);
        ReadVXAsU2(AStream, @FValues[Idx].vert, 1);
        SetLength(FValues[Idx].values, Dimensions * 4);
        ReadMotorolaNumber(AStream, @FValues[Idx].values[0], 4, Dimensions);
        Inc(Idx);
      end;
    end;
  end;
end;

(*********************************** TLWTags **********************************)

destructor TLWTags.Destroy;
begin
  inherited;
end;

procedure TLWTags.Clear;
begin
  FreeAndNil(FTags);
end;

class function TLWTags.GetID: TID4;
begin
  result := ID_TAGS;
end;

function TLWTags.GetTags: TStrings;
begin
  if FTags = nil then
    FTags := TStringList.Create;
  result := FTags;
end;

procedure TLWTags.LoadData(AStream: TStream; DataStart, DataSize: LongWord);
var
  EndPos: TU4;
  TmpStr: string;
begin
  EndPos := DataStart + DataSize;
  while Cardinal(AStream.Position) < Cardinal(EndPos) do
  begin
    ReadS0(AStream, TmpStr);
    Tags.Add(TmpStr);
    TmpStr := '';
  end;
end;

function TLWTags.TagToName(tag: TU2): string;
begin
  result := Tags[tag];
end;

(********************************* TLWSubChunk ********************************)

procedure TLWSubChunk.LoadFromStream(AStream: TStream);
var
  DataStart: Integer;
  DataSize: TU2;
begin
  with AStream do
  begin
    ReadMotorolaNumber(AStream, @DataSize, 2);
    DataStart := Position;
    FSize := DataSize;
    LoadData(AStream, DataStart, DataSize);
    Position := DataStart + DataSize + (DataStart + DataSize) mod 2;
  end;
end;

(*********************************** TLWLayr **********************************)

destructor TLWLayr.Destroy;
begin
  inherited;
end;

class function TLWLayr.GetID: TID4;
begin
  result := ID_LAYR;
end;

procedure TLWLayr.LoadData(AStream: TStream; DataStart, DataSize: LongWord);
begin
  ReadMotorolaNumber(AStream, @FNumber, 2);
  ReadMotorolaNumber(AStream, @FFlags, 2);
  ReadMotorolaNumber(AStream, @FPivot, 4, 3);
  ReadS0(AStream, FName);
  if ((DataStart + DataSize) - Cardinal(AStream.Position)) > 2 then
    ReadMotorolaNumber(AStream, @FParent, 2);
end;

(*********************************** TLWSurf **********************************)

destructor TLWSurf.Destroy;
begin
  inherited;
end;

class function TLWSurf.GetID: TID4;
begin
  result := ID_SURF;
end;

function TLWSurf.GetParamAddr(Param: TID4): Pointer;
var
  Idx: Integer;
  sParam: string;
begin
  result := inherited GetParamAddr(Param);
  if (result = nil) and (Source <> '') then
  begin
    sParam := string(Param);
    Idx := RootChunks.FindChunk(@FindSurfaceByName, @sParam, 0);
    if Idx <> -1 then
      result := TLWSurf(RootChunks[Idx]).ParamAddr[Param];
  end;
end;

function TLWSurf.GetSurfId: Integer;
var
  c, SurfIdx: Integer;
begin
  c := 0;
  SurfIdx := Owner.FindChunk(@FindChunkById, @ID_SURF);

  while (SurfIdx <> -1) and (Owner[SurfIdx] <> Self) do
  begin
    SurfIdx := Owner.FindChunk(@FindChunkById, @ID_SURF, SurfIdx + 1);
    Inc(c);
  end;
  result := c;
end;

procedure TLWSurf.LoadData(AStream: TStream; DataStart, DataSize: LongWord);
var
  CurId: TID4;
begin
  ReadS0(AStream, FName);
  ReadS0(AStream, FSource);
  while Cardinal(AStream.Position) < (DataStart + DataSize) do
  begin
    AStream.Read(CurId, 4);
    Items.Add(GetChunkClass(CurId, TLWSubChunk).Create);
    with Items[Items.Count - 1] do
    begin
      FID := CurId;
      LoadFromStream(AStream);
    end;
  end;
end;

(*********************************** TLWPTag **********************************)

constructor TLWPTag.Create;
begin
  inherited;
end;

function TLWPTag.AddTag(Value: TU2): Integer;
var
  i, L: Integer;
begin
  result := -1;
  L := Length(FTags);
  for i := 0 to L - 1 do
    if Value = FTags[i] then
    begin
      result := i;
      Exit;
    end;
  if result = -1 then
  begin
    SetLength(FTags, L + 1);
    FTags[L] := Value;
    result := L;
  end;
end;

procedure TLWPTag.Clear;
begin
  SetLength(FTagMaps, 0);
  SetLength(FTags, 0);
end;

function TLWPTag.GetPolsByTag(tag: TU2; var PolyIndices: TU2DynArray): Integer;
var
  i: Integer;

  procedure AddPoly(Value: TU2);
  var
    L: Integer;
  begin
    L := Length(PolyIndices);
    SetLength(PolyIndices, L + 1);
    PolyIndices[L] := Value;
  end;
begin
  for i := 0 to TagMapCount - 1 do
    if TagMaps[i].tag = tag then
      AddPoly(TagMaps[i].poly);
  result := Length(PolyIndices);
end;

class function TLWPTag.GetID: TID4;
begin
  result := ID_PTAG;
end;

function TLWPTag.GetTag(AIndex: Integer): TU2;
begin
  ValidateTagInfo;
  result := FTags[AIndex];
end;

function TLWPTag.GetTagCount: Integer;
begin
  ValidateTagInfo;
  result := Length(FTags);
end;

function TLWPTag.GetTagMapCount: Integer;
begin
  result := Length(FTagMaps) div 2;
end;

function TLWPTag.GetTagMaps(AIndex: Integer): TLWPolyTagMap;
begin
  result := PLWPolyTagMap(@FTagMaps[AIndex * 2])^;
end;

procedure TLWPTag.LoadData(AStream: TStream; DataStart, DataSize: LongWord);
var
  Idx: Integer;
begin
  Idx := 0;
  with AStream do
  begin
    Read(FMapType, 4);
    SetLength(FTagMaps, (DataSize - 4) div 2);
    while Cardinal(Position) < (DataStart + DataSize) do
    begin
      ReadVXAsU2(AStream, @FTagMaps[Idx]);
      ReadMotorolaNumber(AStream, @FTagMaps[Idx + 1], 2);
      Inc(Idx, 2);
    end;
    // correct length guestimate errors if any
    if (Idx + 1) < Length(FTagMaps) then
      SetLength(FTagMaps, Idx + 1);
  end;
end;

procedure TLWPTag.ValidateTagInfo;
var
  i: Integer;
begin
  if Length(FTags) > 0 then
    Exit;
  for i := 0 to TagMapCount - 1 do
    AddTag(TagMaps[i].tag);
end;

(******************************** TLWParentChunk ******************************)

procedure TLWParentChunk.Clear;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TLWParentChunk.GetFloatParam(Param: TID4): Single;
var
  pdata: Pointer;
begin
  pdata := ParamAddr[Param];
  if pdata <> nil then
  begin

    result := PF4(pdata)^;
    ReverseByteOrder(@result, 4);

  end
  else
    result := 0.0;
end;

function TLWParentChunk.GetItems: TLWChunkList;
begin
  if FItems = nil then
    FItems := TLWChunkList.Create(true, Self);
  result := FItems;
end;

function TLWParentChunk.GetLongParam(Param: TID4): LongWord;
var
  pdata: Pointer;
begin
  pdata := ParamAddr[Param];
  if pdata <> nil then
  begin

    result := PU4(pdata)^;
    ReverseByteOrder(@result, 4);
  end
  else
    result := 0;
end;

function TLWParentChunk.GetParamAddr(Param: TID4): Pointer;
var
  Idx: Integer;
begin
  result := nil;
  Idx := Items.FindChunk(@FindChunkById, @Param, 0);
  if Idx <> -1 then
    result := Items[Idx].data;
end;

function TLWPols.GetPolsByPntIdx(VertIdx: TU2;
  var VertPolys: TU2DynArray): Integer;
var
  i, j, L: Integer;
begin
  L := 0;
  if Length(VertPolys) > 0 then
    SetLength(VertPolys, 0);
  for i := 0 to PolsCount - 1 do
  begin
    for j := 1 to Indices[PolsByIndex[i]] do
    begin
      if Indices[PolsByIndex[i] + j] = VertIdx then
      begin
        L := Length(VertPolys);
        SetLength(VertPolys, L + 1);
        VertPolys[L] := i;
      end;
    end;
  end;
  result := L;
end;

function TLWChunkList.Add(AChunk: TLWChunk): Integer;
begin
  if (FOwner <> nil) and (FOwner is TLWParentChunk) then
    AChunk.FParentChunk := TLWParentChunk(FOwner);

  AChunk.FOwner := Self;
  result := inherited Add(AChunk);
end;

procedure TLWPols.CalcPntsNormals;
var
  i, j, k, PntIdx, PolyIdx, SurfIdx: Integer;
  Pnts: TLWPnts;
  // PTags: TLWPTag;
  TmpAddr: Pointer;
  sman: TF4;
begin
  // Todo: CalcPntsNormals
  if IndiceCount = 0 then
    Exit;
  with ParentChunk as TLWLayr do
    Pnts := TLWPnts(Items[Items.FindChunk(@FindChunkById, @ID_PNTS, 0)]);
  for PolyIdx := 0 to PolsCount - 1 do
  begin
    i := PolsByIndex[PolyIdx];
    SurfIdx := RootChunks.FindChunk(@FindSurfaceByTag,
      @FPolsInfo[PolyIdx].surfid);
    TmpAddr := TLWSurf(RootChunks[SurfIdx]).ParamAddr[ID_SMAN];
    if TmpAddr <> nil then
    begin
      sman := PF4(TmpAddr)^;
      ReverseByteOrder(@sman, 4);
    end
    else
      sman := 0;
    for j := 1 to Indices[i] do
    begin
      FPolsInfo[PolyIdx].vnorms[j - 1] := FPolsInfo[PolyIdx].norm;
      if sman <= 0 then
        continue;
      PntIdx := Indices[i + j];
      for k := 0 to Pnts.PntsInfo[PntIdx].npols - 1 do
      begin
        if Pnts.PntsInfo[PntIdx].pols[k] = PolyIdx then
          continue;
        if ArcCos(VecDot(FPolsInfo[PolyIdx].norm,
          FPolsInfo[Pnts.PntsInfo[PntIdx].pols[k]].norm)) > sman then
          continue;
        FPolsInfo[PolyIdx].vnorms[j - 1] :=
          VecAdd(FPolsInfo[PolyIdx].vnorms[j - 1],
          FPolsInfo[Pnts.PntsInfo[PntIdx].pols[k]].norm);
      end;
      FPolsInfo[PolyIdx].vnorms[j - 1] :=
        VecNorm(FPolsInfo[PolyIdx].vnorms[j - 1]);
    end;
  end;
end;

function TLWChunk.GetRootChunks: TLWChunkList;
var
  Parent: TLWParentChunk;
begin
  result := nil;
  if (FParentChunk = nil) then
  begin

    if (FOwner is TLWChunkList) then
    begin
      result := FOwner;
      Exit;
    end;

  end
  else
  begin
    Parent := FParentChunk;
    while not(Parent.ParentChunk = nil) do
      Parent := Parent.ParentChunk;
    result := Parent.Owner;
  end;
end;

function TLWChunkList.FindChunk(ChunkFind: TLWChunkFind; Criteria: Pointer;
  StartIndex: Integer): Integer;
var
  Found: boolean;
begin
  Found := false;
  result := -1;
  while (StartIndex < Count) and (not Found) do
  begin
    ChunkFind(Items[StartIndex], Criteria, Found);
    if Found then
    begin
      result := StartIndex;
      Exit;
    end;
    Inc(StartIndex);
  end;
end;

function TLWChunk.GetIndex: Integer;
begin
  result := Owner.IndexOf(Self);
end;

procedure TLWChunk.Loaded;
begin
  // do nothing
end;

procedure TLWChunkList.Loaded;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Loaded;
  end;
end;

function TLWParentChunk.GetVec3Param(Param: TID4): TVec12;
var
  pdata: Pointer;
begin
  pdata := ParamAddr[Param];
  if pdata <> nil then
  begin

    result := PVec12(pdata)^;
    ReverseByteOrder(@result, 4, 3);

  end
  else
  begin
    result[0] := 0;
    result[1] := 1;
    result[2] := 2;
  end;
end;

function TLWParentChunk.GetVXParam(Param: TID4): Word;
var
  pdata: Pointer;
begin
  pdata := ParamAddr[Param];
  if pdata <> nil then
    result := ValueOfVX(pdata)
  else
    result := 0;
end;

function TLWParentChunk.GetWordParam(Param: TID4): Word;
var
  pdata: Pointer;
begin
  pdata := ParamAddr[Param];
  if pdata <> nil then
  begin
    result := PU4(pdata)^;
    ReverseByteOrder(@result, 2);
  end
  else
    result := 0;
end;

procedure TLWParentChunk.Loaded;
begin
  Items.Loaded;
end;

procedure TLWPols.Loaded;
begin
  inherited;
  CalcPntsNormals;
end;

function TLWObjectFile.TagToName(tag: TU2): string;
var
  TagsIdx: Integer;
begin
  TagsIdx := Chunks.FindChunk(@FindChunkById, @ID_TAGS);
  if TagsIdx <> -1 then
    result := TLWTags(Chunks[TagsIdx]).TagToName(tag);
end;


(******************** TLWClip ********************)

class function TLWClip.GetID: TID4;
begin
  result := ID_CLIP;
end;

procedure TLWClip.LoadData(AStream: TStream; DataStart, DataSize: LongWord);
var
  CurId: TID4;
begin
  ReadMotorolaNumber(AStream, @FClipIndex, 4);
  while Cardinal(AStream.Position) < (DataStart + DataSize) do
  begin

    AStream.Read(CurId, 4);

    Items.Add(GetChunkClass(CurId, TLWSubChunk).Create);

    with Items[Items.Count - 1] do
    begin

      FID := CurId;
      LoadFromStream(AStream);

    end;

  end;

end;

// TLWContentDir

(* function TLWContentDir.ContentSearch(AFilename: string): string;
  var
  i: Integer;
  begin
  if not FileExists(AFilename) then
  begin
  result := ExtractFileName(AFilename);
  if not FileExists(result) then
  begin
  for i := 0 to SubDirs.Count - 1 do
  begin
  if FileExists(Root+'\'+SubDirs[i]+'\'+result) then
  begin
  result:=Root+'\'+SubDirs[i]+'\'+result;
  Exit;
  end;

  end;
  result := '';
  end;
  end;
  end;
*)

destructor TLWContentDir.Destroy;
begin
  FreeAndNil(FSubDirs);
  inherited;
end;

function TLWContentDir.FindContent(AFilename: string): string;
var
  i: Integer;
begin
  if not FileExists(AFilename) then
  begin
    result := ExtractFileName(AFilename);
    if not FileExists(result) then
    begin
      for i := 0 to SubDirs.Count - 1 do
      begin
        if FileExists(Root + '\' + SubDirs[i] + '\' + result) then
        begin
          result := Root + '\' + SubDirs[i] + '\' + result;
          Exit;
        end;
      end;
      result := '';
    end;
  end;
end;

function TLWContentDir.GetSubDirs: TStrings;
begin
  if FSubDirs = nil then
    FSubDirs := TStringList.Create;
  result := FSubDirs;
end;

procedure TLWContentDir.SetRoot(const Value: string);
begin
  FRoot := Value;
end;

procedure TLWContentDir.SetSubDirs(const Value: TStrings);
begin
  SubDirs.Assign(Value);
end;

initialization

// Pnts
RegisterChunkClass(TLWPnts);

// Pols
RegisterChunkClass(TLWPols);

// VMap
RegisterChunkClass(TLWVMap);

// Tags
RegisterChunkClass(TLWTags);

// PTAG
RegisterChunkClass(TLWPTag);

// SURF
RegisterChunkClass(TLWSurf);

// LAYR
RegisterChunkClass(TLWLayr);

// CLIP
RegisterChunkClass(TLWClip);

finalization

// UnRegisterChunkClasses;
FreeAndNil(ChunkClasses);
FreeAndNil(ContentDir);

end.
