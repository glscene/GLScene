//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.HeightTileFileHDS;

(* HeightDataSource for the HTF (HeightTileFile) format *)

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,

  GXS.HeightData,
  GXS.HeightTileFile;

type
  // An Height Data Source for the HTF format.
  TgxHeightTileFileHDS = class(TgxHeightDataSource)
  private
    FInfiniteWrap: Boolean;
    FInverted: Boolean;
    FHTFFileName: String;
    FHTF: TgxHeightTileFile;
    FMinElevation: Integer;
  protected
    procedure SetHTFFileName(const val: String);
    procedure SetInfiniteWrap(val: Boolean);
    procedure SetInverted(val: Boolean);
    procedure SetMinElevation(val: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartPreparingData(HeightData: TgxHeightData); override;
    function Width: Integer; override;
    function Height: Integer; override;
    function OpenHTF: TgxHeightTileFile;
    // gives you direct access to the HTF object
  published
    { FileName of the HTF file.
      Note that it is accessed via the services of GXS.ApplicationFileIO,
      so this may not necessarily be a regular file on a disk... }
    property HTFFileName: String read FHTFFileName write SetHTFFileName;
    { If true the height field is wrapped indefinetely. }
    property InfiniteWrap: Boolean read FInfiniteWrap write SetInfiniteWrap
      default True;
    { If true the height data is inverted.(Top to bottom) }
    property Inverted: Boolean read FInverted write SetInverted default True;
    { Minimum elevation of the tiles that are considered to exist.
      This property can typically be used to hide underwater tiles. }
    property MinElevation: Integer read FMinElevation write SetMinElevation
      default -32768;

    property MaxPoolSize;
    property DefaultHeight;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TgxHeightTileFileHDS ------------------
// ------------------

constructor TgxHeightTileFileHDS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInfiniteWrap := True;
  FInverted := True;
  FMinElevation := -32768;
end;

destructor TgxHeightTileFileHDS.Destroy;
begin
  FHTF.Free;
  inherited Destroy;
end;

procedure TgxHeightTileFileHDS.SetHTFFileName(const val: String);
begin
  if FHTFFileName <> val then
  begin
    MarkDirty;
    FreeAndNil(FHTF);
    FHTFFileName := val;
  end;
end;

procedure TgxHeightTileFileHDS.SetInfiniteWrap(val: Boolean);
begin
  if FInfiniteWrap = val then
    exit;
  FInfiniteWrap := val;
  MarkDirty;
end;

procedure TgxHeightTileFileHDS.SetInverted(val: Boolean);
begin
  if FInverted = val then
    exit;
  FInverted := val;
  MarkDirty;
end;

procedure TgxHeightTileFileHDS.SetMinElevation(val: Integer);
begin
  if FMinElevation <> val then
  begin
    FMinElevation := val;
    MarkDirty;
  end;
end;

// OpenHTF
// Tries to open the assigned HeightTileFile.
//
function TgxHeightTileFileHDS.OpenHTF: TgxHeightTileFile;
begin
  if not Assigned(FHTF) then
  begin
    if FHTFFileName = '' then
      FHTF := nil
    else
      FHTF := TgxHeightTileFile.Create(FHTFFileName);
  end;
  result := FHTF;
end;

procedure TgxHeightTileFileHDS.StartPreparingData(HeightData: TgxHeightData);
var
  oldType: TgxHeightDataType;
  htfTile: PHeightTile;
  htfTileInfo: PgxHeightTileInfo;
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

function TgxHeightTileFileHDS.Width: Integer;
begin
  if OpenHTF = nil then
    result := 0
  else
    result := FHTF.SizeX;
end;

function TgxHeightTileFileHDS.Height: Integer;
begin
  if OpenHTF = nil then
    result := 0
  else
    result := FHTF.SizeY;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------


RegisterClasses([TgxHeightTileFileHDS]);

end.
