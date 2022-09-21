//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.HeightTileFileHDS;

(* HeightDataSource for the HTF (HeightTileFile) format *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,

  GLS.HeightData;

//----------- GLHeightTileFile types and classes ------------------

(*
  Access to large tiled height data files.
  Performance vs Raw file accesses (for perfect tile match):
  Cached data:
  "Smooth" terrain   1:2 to 1:10
  Random terrain     1:1

  Non-cached data:
  "Smooth" terrain   1:100 to 1:1000
  Random terrain     1:100
*)

type

  TIntegerArray = array [0 .. MaxInt shr 3] of Integer;
  PIntegerArray = ^TIntegerArray;

  TSmallIntArray = array [0 .. MaxInt shr 2] of SmallInt;
  PSmallIntArray = ^TSmallIntArray;

  TShortIntArray = array [0 .. MaxInt shr 2] of ShortInt;
  PShortIntArray = ^TShortIntArray;

  TGLHeightTileInfo = packed record
    left, top, width, height: Integer;
    min, max, average: SmallInt;
    fileOffset: Int64; // offset to tile data in the file
  end;
  PGLHeightTileInfo = ^TGLHeightTileInfo;
  PPHeightTileInfo = ^PGLHeightTileInfo;

  TGLHeightTile = packed record
    info: TGLHeightTileInfo;
    data: array of SmallInt;
  end;

  PGLHeightTile = ^TGLHeightTile;

  THTFHeader = packed record
    FileVersion: array [0 .. 5] of AnsiChar;
    TileIndexOffset: Int64;
    SizeX, SizeY: Integer;
    TileSize: Integer;
    DefaultZ: SmallInt;
  end;

const
  cHTFHashTableSize = 1023;
  cHTFQuadTableSize = 31;

type

  // Interfaces a Tiled file
  TGLHeightTileFile = class(TObject)
  private
    FFile: TStream;
    FHeader: THTFHeader;
    FTileIndex: packed array of TGLHeightTileInfo;
    FTileMark: array of Cardinal;
    FLastMark: Cardinal;
    FHashTable: array [0 .. cHTFHashTableSize] of array of Integer;
    FQuadTable: array [0 .. cHTFQuadTableSize, 0 .. cHTFQuadTableSize] of array of Integer;
    FCreating: Boolean;
    FHeightTile: TGLHeightTile;
    FInBuf: array of ShortInt;
  protected
    function GetTiles(index: Integer): PGLHeightTileInfo;
    function QuadTableX(x: Integer): Integer;
    function QuadTableY(y: Integer): Integer;
    procedure PackTile(aWidth, aHeight: Integer; src: PSmallIntArray);
    procedure UnPackTile(source: PShortIntArray);
    property TileIndexOffset: Int64 read FHeader.TileIndexOffset write FHeader.TileIndexOffset;
  public
    (* Creates a new HTF file.
      Read and data access methods are not available when creating. *)
    constructor CreateNew(const fileName: String; aSizeX, aSizeY, aTileSize: Integer);
    constructor Create(const fileName: String);
	destructor Destroy; override;
    // Returns tile index for corresponding left/top.
    function GetTileIndex(aLeft, aTop: Integer): Integer;
    // Returns tile of corresponding left/top.
    function GetTile(aLeft, aTop: Integer; pTileInfo: PPHeightTileInfo = nil): PGLHeightTile;
    (* Stores and compresses give tile data.
      aLeft and top MUST be a multiple of TileSize, aWidth and aHeight
      MUST be lower or equal to TileSize. *)
    procedure CompressTile(aLeft, aTop, aWidth, aHeight: Integer; aData: PSmallIntArray);
    (* Extract a single row from the HTF file.
      This is NOT the fastest way to access HTF data.
      All of the row must be contained in the world, otherwise result is undefined. *)
    procedure ExtractRow(x, y, len: Integer; dest: PSmallIntArray);
    // Returns the tile that contains x and y.
    function XYTileInfo(anX, anY: Integer): PGLHeightTileInfo;
    (* Returns the height at given coordinates.
      This is definetely NOT the fastest way to access HTF data and should
      only be used as utility function. *)
    function XYHeight(anX, anY: Integer): SmallInt;
    // Clears the list then add all tiles that overlap the rectangular area.
    procedure TilesInRect(aLeft, aTop, aRight, aBottom: Integer; destList: TList);
    function TileCount: Integer;
    property Tiles[index: Integer]: PGLHeightTileInfo read GetTiles;
    function IndexOfTile(aTile: PGLHeightTileInfo): Integer;
    function TileCompressedSize(tileIndex: Integer): Integer;
    property SizeX: Integer read FHeader.SizeX;
    property SizeY: Integer read FHeader.SizeY;
    (* Maximum width and height for a tile.
      Actual tiles may not be square, can assume random layouts, and may overlap. *)
    property TileSize: Integer read FHeader.TileSize;
    property DefaultZ: SmallInt read FHeader.DefaultZ write FHeader.DefaultZ;
  end;

//---------------- TGLHeightTileFileHDS class -----------------------

  // An Height Data Source for the HTF format.
  TGLHeightTileFileHDS = class(TGLHeightDataSource)
  private
    FInfiniteWrap: Boolean;
    FInverted: Boolean;
    FHTFFileName: String;
    FHTF: TGLHeightTileFile;
    FMinElevation: Integer;
  protected
    procedure SetHTFFileName(const val: String);
    procedure SetInfiniteWrap(val: Boolean);
    procedure SetInverted(val: Boolean);
    procedure SetMinElevation(val: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartPreparingData(HeightData: TGLHeightData); override;
    function Width: Integer; override;
    function Height: Integer; override;
    function OpenHTF: TGLHeightTileFile;
    // gives you direct access to the HTF object
  published
    (* FileName of the HTF file.
      Note that it is accessed via the services of GLS.ApplicationFileIO,
      so this may not necessarily be a regular file on a disk... *)
    property HTFFileName: String read FHTFFileName write SetHTFFileName;
    // If true the height field is wrapped indefinetely.
    property InfiniteWrap: Boolean read FInfiniteWrap write SetInfiniteWrap
      default True;
    // If true the height data is inverted.(Top to bottom)
    property Inverted: Boolean read FInverted write SetInverted default True;
    (* Minimum elevation of the tiles that are considered to exist.
      This property can typically be used to hide underwater tiles. *)
    property MinElevation: Integer read FMinElevation write SetMinElevation
      default -32768;
    property MaxPoolSize;
    property DefaultHeight;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

const
  cFileVersion = 'HTF100';

procedure FillSmallInt(p: PSmallInt; count: Integer; v: SmallInt);
var
  I: Integer;
begin
  for I := count - 1 downto 0 do
  begin
    p^ := v;
    Inc(p);
  end;
end;

// ------------------
// ------------------ TGLHeightTileFile ------------------
// ------------------

constructor TGLHeightTileFile.CreateNew(const fileName: String;
  aSizeX, aSizeY, aTileSize: Integer);
begin
  with FHeader do
  begin
    FileVersion := cFileVersion;
    SizeX := aSizeX;
    SizeY := aSizeY;
    TileSize := aTileSize;
  end;
  FFile := TFileStream.Create(fileName, fmCreate);
  FFile.Write(FHeader, SizeOf(FHeader));
  FCreating := True;
  SetLength(FHeightTile.data, aTileSize * aTileSize);
end;

constructor TGLHeightTileFile.Create(const fileName: String);
var
  n, I, key, qx, qy: Integer;
begin
  FFile := TFileStream.Create(fileName, fmOpenRead + fmShareDenyNone);
  // Read Header
  FFile.Read(FHeader, SizeOf(FHeader));
  if FHeader.FileVersion <> cFileVersion then
    raise Exception.Create('Invalid file type');
  // Read TileIndex
  FFile.Position := TileIndexOffset;
  FFile.Read(n, 4);
  SetLength(FTileIndex, n);
  FFile.Read(FTileIndex[0], SizeOf(TGLHeightTileInfo) * n);
  // Prepare HashTable & QuadTable
  for n := 0 to High(FTileIndex) do
  begin
    with FTileIndex[n] do
    begin
      key := left + (top shl 4);
      key := ((key and cHTFHashTableSize) + (key shr 10) + (key shr 20)) and
        cHTFHashTableSize;
      I := Length(FHashTable[key]);
      SetLength(FHashTable[key], I + 1);
      FHashTable[key][I] := n;
      for qx := QuadTableX(left) to QuadTableX(left + width - 1) do
      begin
        for qy := QuadTableY(top) to QuadTableY(top + height - 1) do
        begin
          I := Length(FQuadTable[qx, qy]);
          SetLength(FQuadTable[qx, qy], I + 1);
          FQuadTable[qx, qy][I] := n;
        end;
      end;
    end;
  end;
  FHeightTile.info.left := MaxInt; // mark as not loaded
  SetLength(FHeightTile.data, TileSize * TileSize);
  SetLength(FInBuf, TileSize * (TileSize + 1) * 2);
  SetLength(FTileMark, Length(FTileIndex));
end;

destructor TGLHeightTileFile.Destroy;
var
  n: Integer;
begin
  if FCreating then
  begin
    TileIndexOffset := FFile.Position;
    // write tile index
    n := Length(FTileIndex);
    FFile.Write(n, 4);
    FFile.Write(FTileIndex[0], SizeOf(TGLHeightTileInfo) * n);
    // write data size
    FFile.Position := 0;
    FFile.Write(FHeader, SizeOf(FHeader));
  end;
  FFile.Free;
  inherited Destroy;
end;

function TGLHeightTileFile.QuadTableX(x: Integer): Integer;
begin
  Result := ((x * (cHTFQuadTableSize + 1)) div (SizeX + 1)) and cHTFQuadTableSize;
end;

function TGLHeightTileFile.QuadTableY(y: Integer): Integer;
begin
  Result := ((y * (cHTFQuadTableSize + 1)) div (SizeY + 1)) and cHTFQuadTableSize;
end;

procedure TGLHeightTileFile.PackTile(aWidth, aHeight: Integer; src: PSmallIntArray);
var
  packWidth: Integer;

  function DiffEncode(src: PSmallIntArray; dest: PShortIntArray): Cardinal;
  var
    I: Integer;
    v, delta: SmallInt;
  begin
    Result := Cardinal(dest);
    v := src[0];
    PSmallIntArray(dest)[0] := v;
    dest := PShortIntArray(Cardinal(dest) + 2);
    I := 1;
    while I < packWidth do
    begin
      delta := src[I] - v;
      v := src[I];
      if Abs(delta) <= 127 then
      begin
        dest[0] := ShortInt(delta);
        dest := PShortIntArray(Cardinal(dest) + 1);
      end
      else
      begin
        dest[0] := -128;
        dest := PShortIntArray(Cardinal(dest) + 1);
        PSmallIntArray(dest)[0] := v;
        dest := PShortIntArray(Cardinal(dest) + 2);
      end;
      Inc(I);
    end;
    Result := Cardinal(dest) - Result;
  end;

  function RLEEncode(src: PSmallIntArray; dest: PAnsiChar): Cardinal;
  var
    v: SmallInt;
    I, n: Integer;
  begin
    I := 0;
    Result := Cardinal(dest);
    while (I < packWidth) do
    begin
      v := src[I];
      Inc(I);
      n := 0;
      PSmallIntArray(dest)[0] := v;
      Inc(dest, 2);
      while (src[I] = v) and (I < packWidth) do
      begin
        Inc(n);
        if n = 255 then
        begin
          dest[0] := #255;
          Inc(dest);
          n := 0;
        end;
        Inc(I);
      end;
      if (I < packWidth) or (n > 0) then
      begin
        dest[0] := AnsiChar(n);
        Inc(dest);
      end;
    end;
    Result := Cardinal(dest) - Result;
  end;

var
  y: Integer;
  p: PSmallIntArray;
  buf, bestBuf: array of Byte;
  bestLength, len: Integer;
  leftPack, rightPack: Byte;
  bestMethod: Byte; // 0=RAW, 1=Diff, 2=RLE
  av: Int64;
  v: SmallInt;
begin
  SetLength(buf, TileSize * 4); // worst case situation
  SetLength(bestBuf, TileSize * 4); // worst case situation

  with FHeightTile.info do
  begin
    min := src[0];
    max := src[0];
    av := src[0];
    for y := 1 to aWidth * aHeight - 1 do
    begin
      v := src[y];
      if v < min then
        min := v
      else if v > max then
        max := v;
      av := av + v;
    end;
    average := av div (aWidth * aHeight);

    if min = max then
      Exit; // no need to store anything

  end;

  for y := 0 to aHeight - 1 do
  begin
    p := @src[aWidth * y];
    packWidth := aWidth;
    // Lookup leftPack
    leftPack := 0;
    while (leftPack < 255) and (packWidth > 0) and (p[0] = DefaultZ) do
    begin
      p := PSmallIntArray(Cardinal(p) + 2);
      Dec(packWidth);
      Inc(leftPack);
    end;
    // Lookup rightPack
    rightPack := 0;
    while (rightPack < 255) and (packWidth > 0) and
      (p[packWidth - 1] = DefaultZ) do
    begin
      Dec(packWidth);
      Inc(rightPack);
    end;
    // Default encoding = RAW
    bestLength := packWidth * 2;
    bestMethod := 0;
    Move(p^, bestBuf[0], bestLength);
    // Diff encoding
    len := DiffEncode(p, PShortIntArray(@buf[0]));
    if len < bestLength then
    begin
      bestLength := len;
      bestMethod := 1;
      Move(buf[0], bestBuf[0], bestLength);
    end;
    // RLE encoding
    len := RLEEncode(p, PAnsiChar(@buf[0]));
    if len < bestLength then
    begin
      bestLength := len;
      bestMethod := 2;
      Move(buf[0], bestBuf[0], bestLength);
    end;
    // Write to file
    if (leftPack or rightPack) = 0 then
    begin
      FFile.Write(bestMethod, 1);
      FFile.Write(bestBuf[0], bestLength);
    end
    else
    begin
      if leftPack > 0 then
      begin
        if rightPack > 0 then
        begin
          bestMethod := bestMethod + $C0;
          FFile.Write(bestMethod, 1);
          FFile.Write(leftPack, 1);
          FFile.Write(rightPack, 1);
          FFile.Write(bestBuf[0], bestLength);
        end
        else
        begin
          bestMethod := bestMethod + $80;
          FFile.Write(bestMethod, 1);
          FFile.Write(leftPack, 1);
          FFile.Write(bestBuf[0], bestLength);
        end;
      end
      else
      begin
        bestMethod := bestMethod + $40;
        FFile.Write(bestMethod, 1);
        FFile.Write(rightPack, 1);
        FFile.Write(bestBuf[0], bestLength);
      end;
    end;
  end;
end;

procedure TGLHeightTileFile.UnPackTile(source: PShortIntArray);
var
  unpackWidth, tileWidth: Cardinal;
  src: PShortInt;
  dest: PSmallInt;

  procedure DiffDecode;
  var
    v: SmallInt;
    delta: SmallInt;
    locSrc: PShortInt;
    destEnd, locDest: PSmallInt;
  begin
    locSrc := PShortInt(Cardinal(src) - 1);
    locDest := dest;
    destEnd := PSmallInt(Cardinal(dest) + unpackWidth * 2);
    while Cardinal(locDest) < Cardinal(destEnd) do
    begin
      Inc(locSrc);
      v := PSmallInt(locSrc)^;
      Inc(locSrc, 2);
      locDest^ := v;
      Inc(locDest);
      while (Cardinal(locDest) < Cardinal(destEnd)) do
      begin
        delta := locSrc^;
        if delta <> -128 then
        begin
          v := v + delta;
          Inc(locSrc);
          locDest^ := v;
          Inc(locDest);
        end
        else
          Break;
      end;
    end;
    src := locSrc;
    dest := locDest;
  end;

  procedure RLEDecode;
  var
    n, j: Cardinal;
    v: SmallInt;
    locSrc: PShortInt;
    destEnd, locDest: PSmallInt;
  begin
    locSrc := src;
    locDest := dest;
    destEnd := PSmallInt(Cardinal(dest) + unpackWidth * 2);
    while Cardinal(locDest) < Cardinal(destEnd) do
    begin
      v := PSmallIntArray(locSrc)[0];
      Inc(locSrc, 2);
      repeat
        if Cardinal(locDest) = Cardinal(destEnd) - 2 then
        begin
          locDest^ := v;
          Inc(locDest);
          n := 0;
        end
        else
        begin
          n := Integer(locSrc^ and 255);
          Inc(locSrc);
          for j := 0 to n do
          begin
            locDest^ := v;
            Inc(locDest);
          end;
        end;
      until (n < 255) or (Cardinal(locDest) >= Cardinal(destEnd));
    end;
    src := locSrc;
    dest := locDest;
  end;

var
  y: Integer;
  n: Byte;
  method: Byte;
begin
  dest := @FHeightTile.data[0];

  with FHeightTile.info do
  begin
    if min = max then
    begin
      FillSmallInt(dest, width * height, min);
      Exit;
    end;
    tileWidth := width;
  end;

  src := PShortInt(source);
  n := 0;
  for y := 0 to FHeightTile.info.height - 1 do
  begin
    method := Byte(src^);
    Inc(src);
    unpackWidth := tileWidth;
    // Process left pack if any
    if (method and $80) <> 0 then
    begin
      n := PByte(src)^;
      Inc(src);
      FillSmallInt(dest, n, DefaultZ);
      Dec(unpackWidth, n);
      Inc(dest, n);
    end;
    // Read right pack if any
    if (method and $40) <> 0 then
    begin
      PByte(@n)^ := PByte(src)^;
      Inc(src);
      Dec(unpackWidth, n)
    end
    else
      n := 0;
    // Process main data
    case (method and $3F) of
      1:
        DiffDecode;
      2:
        RLEDecode;
    else
      Move(src^, dest^, unpackWidth * 2);
      Inc(src, unpackWidth * 2);
      Inc(dest, unpackWidth);
    end;
    // Process right pack if any
    if n > 0 then
    begin
      FillSmallInt(dest, n, DefaultZ);
      Inc(dest, n);
    end;
  end;
end;

function TGLHeightTileFile.GetTileIndex(aLeft, aTop: Integer): Integer;
var
  I, key, n: Integer;
  p: PIntegerArray;
begin
  Result := -1;
  key := aLeft + (aTop shl 4);
  key := ((key and cHTFHashTableSize) + (key shr 10) + (key shr 20)) and
    cHTFHashTableSize;
  n := Length(FHashTable[key]);
  if n > 0 then
  begin
    p := @FHashTable[key][0];
    for I := 0 to n - 1 do
    begin
      with FTileIndex[p[I]] do
      begin
        if (left = aLeft) and (top = aTop) then
        begin
          Result := p[I];
          Break;
        end;
      end;
    end;
  end;
end;

function TGLHeightTileFile.GetTile(aLeft, aTop: Integer;
  pTileInfo: PPHeightTileInfo = nil): PGLHeightTile;
var
  I, n: Integer;
  tileInfo: PGLHeightTileInfo;
begin
  with FHeightTile.info do
    if (left = aLeft) and (top = aTop) then
    begin
      Result := @FHeightTile;
      if Assigned(pTileInfo) then
        pTileInfo^ := @Result.info;
      Exit;
    end;
  I := GetTileIndex(aLeft, aTop);
  if I >= 0 then
  begin
    tileInfo := @FTileIndex[I];
    if Assigned(pTileInfo) then
      pTileInfo^ := tileInfo;
    if I < High(FTileIndex) then
      n := FTileIndex[I + 1].fileOffset - tileInfo.fileOffset
    else
      n := TileIndexOffset - tileInfo.fileOffset;
    Result := @FHeightTile;
    FHeightTile.info := tileInfo^;
    FFile.Position := tileInfo.fileOffset;
    FFile.Read(FInBuf[0], n);
    UnPackTile(@FInBuf[0]);
  end
  else
  begin
    Result := nil;
    if Assigned(pTileInfo) then
      pTileInfo^ := nil;
  end;
end;

procedure TGLHeightTileFile.CompressTile(aLeft, aTop, aWidth, aHeight: Integer;
  aData: PSmallIntArray);
begin
  Assert(aWidth <= TileSize);
  Assert(aHeight <= TileSize);
  with FHeightTile.info do
  begin
    left := aLeft;
    top := aTop;
    width := aWidth;
    height := aHeight;
    fileOffset := FFile.Position;
  end;
  PackTile(aWidth, aHeight, aData);
  SetLength(FTileIndex, Length(FTileIndex) + 1);
  FTileIndex[High(FTileIndex)] := FHeightTile.info
end;

procedure TGLHeightTileFile.ExtractRow(x, y, len: Integer;
  dest: PSmallIntArray);
var
  rx: Integer;
  n: Cardinal;
  tileInfo: PGLHeightTileInfo;
  tile: PGLHeightTile;
begin
  while len > 0 do
  begin
    tileInfo := XYTileInfo(x, y);
    if not Assigned(tileInfo) then
      Exit;
    rx := x - tileInfo.left;
    n := Cardinal(tileInfo.width - rx);
    if n > Cardinal(len) then
      n := Cardinal(len);
    tile := GetTile(tileInfo.left, tileInfo.top);
    Move(tile.data[(y - tileInfo.top) * tileInfo.width + rx], dest^, n * 2);
    dest := PSmallIntArray(Cardinal(dest) + n * 2);
    Dec(len, n);
    Inc(x, n);
  end;
end;

function TGLHeightTileFile.XYTileInfo(anX, anY: Integer): PGLHeightTileInfo;
var
  tileList: TList;
begin
  tileList := TList.Create;
  try
    TilesInRect(anX, anY, anX + 1, anY + 1, tileList);
    if tileList.count > 0 then
      Result := PGLHeightTileInfo(tileList.First)
    else
      Result := nil;
  finally
    tileList.Free;
  end;
end;

function TGLHeightTileFile.XYHeight(anX, anY: Integer): SmallInt;
var
  tileInfo: PGLHeightTileInfo;
  tile: PGLHeightTile;
begin
  // Current tile per chance?
  with FHeightTile.info do
  begin
    if (left <= anX) and (left + width > anX) and (top <= anY) and
      (top + height > anY) then
    begin
      Result := FHeightTile.data[(anX - left) + (anY - top) * width];
      Exit;
    end;
  end;
  // Find corresponding tile if any
  tileInfo := XYTileInfo(anX, anY);
  if Assigned(tileInfo) then
    with tileInfo^ do
    begin
      tile := GetTile(left, top);
      Result := tile.data[(anX - left) + (anY - top) * width];
    end
  else
    Result := DefaultZ;
end;

procedure TGLHeightTileFile.TilesInRect(aLeft, aTop, aRight, aBottom: Integer;
  destList: TList);
var
  I, n, qx, qy, idx: Integer;
  p: PIntegerArray;
  tileInfo: PGLHeightTileInfo;
begin
  destList.count := 0;
  // Clamp to world
  if (aLeft > SizeX) or (aRight < 0) or (aTop > SizeY) or (aBottom < 0) then
    Exit;
  if aLeft < 0 then
    aLeft := 0;
  if aRight > SizeX then
    aRight := SizeX;
  if aTop < 0 then
    aTop := 0;
  if aBottom > SizeY then
    aBottom := SizeY;
  // Collect tiles on quads
  Inc(FLastMark);
  for qy := QuadTableY(aTop) to QuadTableY(aBottom) do
  begin
    for qx := QuadTableX(aLeft) to QuadTableX(aRight) do
    begin
      n := High(FQuadTable[qx, qy]);
      p := @FQuadTable[qx, qy][0];
      for I := 0 to n do
      begin
        idx := p[I];
        if FTileMark[idx] <> FLastMark then
        begin
          FTileMark[idx] := FLastMark;
          tileInfo := @FTileIndex[idx];
          with tileInfo^ do
          begin
            if (left <= aRight) and (top <= aBottom) and (aLeft < left + width)
              and (aTop < top + height) then
              destList.Add(tileInfo);
          end;
        end;
      end;
    end;
  end;
end;

function TGLHeightTileFile.TileCount: Integer;
begin
  Result := Length(FTileIndex);
end;

function TGLHeightTileFile.GetTiles(index: Integer): PGLHeightTileInfo;
begin
  Result := @FTileIndex[index];
end;

function TGLHeightTileFile.IndexOfTile(aTile: PGLHeightTileInfo): Integer;
var
  c: Cardinal;
begin
  c := Cardinal(aTile) - Cardinal(@FTileIndex[0]);
  if (c mod SizeOf(TGLHeightTileInfo)) = 0 then
  begin
    Result := (c div SizeOf(TGLHeightTileInfo));
    if (Result < 0) or (Result > High(FTileIndex)) then
      Result := -1;
  end
  else
    Result := -1;
end;

function TGLHeightTileFile.TileCompressedSize(tileIndex: Integer): Integer;
begin
  if tileIndex < High(FTileIndex) then
    Result := FTileIndex[tileIndex + 1].fileOffset - FTileIndex[tileIndex]
      .fileOffset
  else
    Result := TileIndexOffset - FTileIndex[tileIndex].fileOffset;
end;


//-------------------- TGLHeightTileFileHDS -------------------

constructor TGLHeightTileFileHDS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInfiniteWrap := True;
  FInverted := True;
  FMinElevation := -32768;
end;

destructor TGLHeightTileFileHDS.Destroy;
begin
  FHTF.Free;
  inherited Destroy;
end;

procedure TGLHeightTileFileHDS.SetHTFFileName(const val: String);
begin
  if FHTFFileName <> val then
  begin
    MarkDirty;
    FreeAndNil(FHTF);
    FHTFFileName := val;
  end;
end;

procedure TGLHeightTileFileHDS.SetInfiniteWrap(val: Boolean);
begin
  if FInfiniteWrap = val then
    exit;
  FInfiniteWrap := val;
  MarkDirty;
end;

procedure TGLHeightTileFileHDS.SetInverted(val: Boolean);
begin
  if FInverted = val then
    exit;
  FInverted := val;
  MarkDirty;
end;

procedure TGLHeightTileFileHDS.SetMinElevation(val: Integer);
begin
  if FMinElevation <> val then
  begin
    FMinElevation := val;
    MarkDirty;
  end;
end;

// Tries to open the assigned HeightTileFile.
//
function TGLHeightTileFileHDS.OpenHTF: TGLHeightTileFile;
begin
  if not Assigned(FHTF) then
  begin
    if FHTFFileName = '' then
      FHTF := nil
    else
      FHTF := TGLHeightTileFile.Create(FHTFFileName);
  end;
  result := FHTF;
end;

procedure TGLHeightTileFileHDS.StartPreparingData(HeightData: TGLHeightData);
var
  oldType: TGLHeightDataType;
  htfTile: PGLHeightTile;
  htfTileInfo: PGLHeightTileInfo;
  x, y: Integer;
  YPos: Integer;
  inY, outY: Integer;
  PLineIn, PLineOut: ^PSmallIntArray;
  LineDataSize: Integer;
begin
  // access htf data
  if OpenHTF = nil then
  begin
    HeightData.DataState := hdsNone;
    exit;
  end
  else
    Assert(FHTF.TileSize = HeightData.Size,
      'HTF TileSize and HeightData size don''t match.(' +
      IntToStr(FHTF.TileSize) + ' and ' + IntToStr(HeightData.Size) + ')');
  HeightData.DataState := hdsPreparing;
  // retrieve data and place it in the HeightData
  with HeightData do
  begin
    if Inverted then
      YPos := YTop
    else
      YPos := FHTF.SizeY - YTop - Size + 1;
    if InfiniteWrap then
    begin
      x := XLeft mod FHTF.SizeX;
      if x < 0 then
        x := x + FHTF.SizeX;
      y := YPos mod FHTF.SizeY;
      if y < 0 then
        y := y + FHTF.SizeY;
      htfTile := FHTF.GetTile(x, y, @htfTileInfo);
    end
    else
    begin
      htfTile := FHTF.GetTile(XLeft, YPos, @htfTileInfo);
    end;

    if (htfTile = nil) or (htfTileInfo.max <= FMinElevation) then
    begin
      // non-aligned tiles aren't handled (would be slow anyway)
      DataState := hdsNone;
    end
    else
    begin
      oldType := DataType;
      Allocate(hdtSmallInt);

      if Inverted then
        Move(htfTile.data[0], SmallIntData^, DataSize)
      else
      begin // invert the terrain (top to bottom) To compensate for the inverted terrain renderer
        LineDataSize := DataSize div Size;
        for y := 0 to Size - 1 do
        begin
          inY := y * HeightData.Size;
          outY := ((Size - 1) - y) * HeightData.Size;
          PLineIn := @htfTile.data[inY];
          PLineOut := @HeightData.SmallIntData[outY];
          Move(PLineIn^, PLineOut^, LineDataSize);
        end;
      end;
      // ---Move(htfTile.data[0], SmallIntData^, DataSize);---
      if oldType <> hdtSmallInt then
        DataType := oldType;
      TextureCoordinates(HeightData);
      inherited;
      HeightMin := htfTileInfo.min;
      HeightMax := htfTileInfo.max;
    end;
  end;
end;

function TGLHeightTileFileHDS.Width: Integer;
begin
  if OpenHTF = nil then
    result := 0
  else
    result := FHTF.SizeX;
end;

function TGLHeightTileFileHDS.Height: Integer;
begin
  if OpenHTF = nil then
    result := 0
  else
    result := FHTF.SizeY;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

RegisterClasses([TGLHeightTileFileHDS]);

end.
