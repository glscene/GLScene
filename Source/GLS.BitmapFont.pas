//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.BitmapFont;

(* Bitmap Fonts management classes *)

interface

{$I GLScene.inc}

uses
  Winapi.OpengL,
  System.Classes,
  System.SysUtils,
  System.Types,
  Vcl.Graphics,
  Vcl.StdCtrls,
  
  GLS.OpenGLTokens,
  GLS.Scene,
  GLS.VectorGeometry,
  GLS.Context,
  GLS.Texture,
  GLS.State,
  GLS.Utils,
  GLS.Graphics,
  GLS.Color,
  GLS.BaseClasses,
  GLS.RenderContextInfo,
  GLS.TextureFormat,
  GLS.VectorTypes,
  GLS.PersistentClasses;

type

  (* An individual character range in a bitmap font.
     A range allows mapping ASCII characters to character tiles in a font
     bitmap, tiles are enumerated line then column (raster). *)
  TGLBitmapFontRange = class(TCollectionItem)
  private
    function GetStartASCII: WideString;
    function GetStopASCII: WideString;
  protected
    FStartASCII, FStopASCII: WideChar;
    FStartGlyphIdx, FStopGlyphIdx, FCharCount: Integer;
    procedure SetStartASCII(const val: WideString);
    procedure SetStopASCII(const val: WideString);
    procedure SetStartGlyphIdx(val: Integer);
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange;
  published
    property StartASCII: WideString read GetStartASCII write SetStartASCII;
    property StopASCII: WideString read GetStopASCII write SetStopASCII;
    property StartGlyphIdx: Integer read FStartGlyphIdx write SetStartGlyphIdx;
    property StopGlyphIdx: Integer read FStopGlyphIdx;
    property CharCount: Integer read FCharCount;
  end;

  TGLBitmapFontRanges = class(TCollection)
  private
    FCharCount: Integer;
  protected
    FOwner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TGLBitmapFontRange);
    function GetItems(index: Integer): TGLBitmapFontRange;
    function CalcCharacterCount: Integer;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function Add: TGLBitmapFontRange; overload;
    function Add(const StartASCII, StopASCII: WideChar)
      : TGLBitmapFontRange; overload;
    function Add(const StartASCII, StopASCII: AnsiChar)
      : TGLBitmapFontRange; overload;
    function FindItemID(ID: Integer): TGLBitmapFontRange;
    property Items[index: Integer]: TGLBitmapFontRange read GetItems
      write SetItems; default;
    (* Converts an ASCII character into a tile index.
       Return -1 if character cannot be rendered. *)
    function CharacterToTileIndex(aChar: WideChar): Integer;
    function TileIndexToChar(aIndex: Integer): WideChar;
    procedure NotifyChange;
    // Total number of characters in the ranges; cached for performance
    property CharacterCount: Integer read FCharCount;
  end;

  PCharInfo = ^TCharInfo;
  TCharInfo = record
    l, t, w: word;
  end;

  (* Provides access to individual characters in a BitmapFont.
    Only fixed-width bitmap fonts are supported, the characters are enumerated
    in a raster fashion (line then column).
    Transparency is all or nothing, the transparent color being that of the
    top left pixel of the Glyphs bitmap.
    Performance note: as usual, for best performance, you base font bitmap
    dimensions should be close to a power of two, and have at least 1 pixel
    spacing between characters (horizontally and vertically) to avoid artefacts
    when rendering with linear filtering. *)
  TGLCustomBitmapFont = class(TGLUpdateAbleComponent)
  private
    FRanges: TGLBitmapFontRanges;
    FGlyphs: TPicture;
    FCharWidth, FCharHeight: Integer;
    FGlyphsIntervalX, FGlyphsIntervalY: Integer;
    FHSpace, FVSpace, FHSpaceFix: Integer;
    FUsers: TList;
    FMinFilter: TGLMinFilter;
    FMagFilter: TGLMagFilter;
    FTextureWidth, FTextureHeight: Integer;
    FTextRows, FTextCols: Integer;
    FGlyphsAlpha: TGLTextureImageAlpha;
    FTextures: TList;
    FTextureModified: boolean;
    FLastTexture: TGLTextureHandle;
  protected
    FChars: array of TCharInfo;
    FCharsLoaded: boolean;
    procedure ResetCharWidths(w: Integer = -1);
    procedure SetCharWidths(index, value: Integer);
    procedure SetRanges(const val: TGLBitmapFontRanges);
    procedure SetGlyphs(const val: TPicture);
    procedure SetCharWidth(const val: Integer);
    procedure SetCharHeight(const val: Integer);
    procedure SetGlyphsIntervalX(const val: Integer);
    procedure SetGlyphsIntervalY(const val: Integer);
    procedure OnGlyphsChanged(Sender: TObject);
    procedure SetHSpace(const val: Integer);
    procedure SetVSpace(const val: Integer);
    procedure SetMagFilter(AValue: TGLMagFilter);
    procedure SetMinFilter(AValue: TGLMinFilter);
    procedure SetGlyphsAlpha(val: TGLTextureImageAlpha);
    procedure TextureChanged;
    procedure FreeTextureHandle; virtual;
    function TextureFormat: Integer; virtual;
    procedure InvalidateUsers;
    function CharactersPerRow: Integer;
    procedure GetCharTexCoords(Ch: WideChar;
      var TopLeft, BottomRight: TTexPoint);
    procedure GetICharTexCoords(var ARci: TGLRenderContextInfo; Chi: Integer;
      out TopLeft, BottomRight: TTexPoint);
    procedure PrepareImage(var ARci: TGLRenderContextInfo); virtual;
    procedure PrepareParams(var ARci: TGLRenderContextInfo);
    (* A single bitmap containing all the characters.
      The transparent color is that of the top left pixel. *)
    property Glyphs: TPicture read FGlyphs write SetGlyphs;
    // Nb of horizontal pixels between two columns in the Glyphs.
    property GlyphsIntervalX: Integer read FGlyphsIntervalX
      write SetGlyphsIntervalX;
    //  Nb of vertical pixels between two rows in the Glyphs.
    property GlyphsIntervalY: Integer read FGlyphsIntervalY
      write SetGlyphsIntervalY;
    (* Ranges allow converting between ASCII and tile indexes.
      See TGLCustomBitmapFontRange. *)
    property Ranges: TGLBitmapFontRanges read FRanges write SetRanges;
    // Width of a single character.
    property CharWidth: Integer read FCharWidth write SetCharWidth default 16;
    // Pixels in between rendered characters (horizontally).
    property HSpace: Integer read FHSpace write SetHSpace default 1;
    // Pixels in between rendered lines (vertically).
    property VSpace: Integer read FVSpace write SetVSpace default 1;
    (* Horizontal spacing fix offset.
       This property is for internal use, and is added to the hspacing
       of each character when rendering, typically to fix extra spacing. *)
    property HSpaceFix: Integer read FHSpaceFix write FHSpaceFix;
    property MagFilter: TGLMagFilter read FMagFilter write SetMagFilter
      default maLinear;
    property MinFilter: TGLMinFilter read FMinFilter write SetMinFilter
      default miLinear;
    property GlyphsAlpha: TGLTextureImageAlpha read FGlyphsAlpha
      write FGlyphsAlpha default tiaDefault;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterUser(anObject: TGLBaseSceneObject); virtual;
    procedure UnRegisterUser(anObject: TGLBaseSceneObject); virtual;
    (* Renders the given string at current position or at position given by the optional position variable.
       The current matrix is blindly used, meaning you can render all kinds
       of rotated and linear distorted text with this method, OpenGL
       Enable states are also possibly altered. *)
    procedure RenderString(var ARci: TGLRenderContextInfo;
      const aText: UnicodeString; aAlignment: TAlignment;
      aLayout: TTextLayout; const aColor: TColorVector;
      aPosition: PGLVector = nil; aReverseY: boolean = False); overload; virtual;
    (* A simpler canvas-style TextOut helper for RenderString.
       The rendering is reversed along Y by default, to allow direct use
       with TGLCanvas *)
    procedure TextOut(var rci: TGLRenderContextInfo; X, Y: Single;
      const Text: UnicodeString; const Color: TColorVector); overload;
    procedure TextOut(var rci: TGLRenderContextInfo; X, Y: Single;
      const Text: UnicodeString; const Color: TColor); overload;
    function TextWidth(const Text: UnicodeString): Integer;
    function CharacterToTileIndex(aChar: WideChar): Integer; virtual;
    function TileIndexToChar(aIndex: Integer): WideChar; virtual;
    function CharacterCount: Integer; virtual;
    // Get the actual width for this char.
    function GetCharWidth(Ch: WideChar): Integer;
    // Get the actual pixel width for this string.
    function CalcStringWidth(const aText: UnicodeString): Integer;
      overload; virtual;
    // make texture if needed
    procedure CheckTexture(var ARci: TGLRenderContextInfo);
    //  Height of a single character.
    property CharHeight: Integer read FCharHeight write SetCharHeight
      default 16;
    property TextureWidth: Integer read FTextureWidth write FTextureWidth;
    property TextureHeight: Integer read FTextureHeight write FTextureHeight;
  end;

  (* See TGLCustomBitmapFont.
     This class only publuishes some of the properties. *)
  TGLBitmapFont = class(TGLCustomBitmapFont)
  published
    property Glyphs;
    property GlyphsIntervalX;
    property GlyphsIntervalY;
    property Ranges;
    property CharWidth;
    property CharHeight;
    property HSpace;
    property VSpace;
    property MagFilter;
    property MinFilter;
    property GlyphsAlpha;
  end;

  TGLFlatTextOption = (ftoTwoSided);
  TGLFlatTextOptions = set of TGLFlatTextOption;

  (* A 2D text displayed and positionned in 3D coordinates.
     The FlatText uses a character font defined and stored by a TGLBitmapFont
     component. Default character scale is 1 font pixel = 1 space unit. *)
  TGLFlatText = class(TGLImmaterialSceneObject)
  private
    FBitmapFont: TGLCustomBitmapFont;
    FText: UnicodeString;
    FAlignment: TAlignment;
    FLayout: TTextLayout;
    FModulateColor: TGLColor;
    FOptions: TGLFlatTextOptions;
  protected
    procedure SetBitmapFont(const val: TGLCustomBitmapFont);
    procedure SetText(const val: UnicodeString);
    procedure SetAlignment(const val: TAlignment);
    procedure SetLayout(const val: TTextLayout);
    procedure SetModulateColor(const val: TGLColor);
    procedure SetOptions(const val: TGLFlatTextOptions);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var rci: TGLRenderContextInfo;
      renderSelf, renderChildren: boolean); override;
    procedure Assign(Source: TPersistent); override;
  published
    (*  Refers the bitmap font to use.
      The referred bitmap font component stores and allows access to
      individual character bitmaps. *)
    property BitmapFont: TGLCustomBitmapFont read FBitmapFont
      write SetBitmapFont;
    (*  Text to render.
      Be aware that only the characters available in the bitmap font will
      be rendered. CR LF sequences are allowed. *)
    property Text: UnicodeString read FText write SetText;
    (* Controls the text alignment (horizontal).
      Possible values : taLeftJustify, taRightJustify, taCenter *)
    property Alignment: TAlignment read FAlignment write SetAlignment;
    (*  Controls the text layout (vertical).
      Possible values : tlTop, tlCenter, tlBottom *)
    property Layout: TTextLayout read FLayout write SetLayout;
    // Color modulation, can be used for fade in/out too.
    property ModulateColor: TGLColor read FModulateColor write SetModulateColor;
    (* Flat text options.
       ftoTwoSided : when set the text will be visible from its two
       sides even if faceculling is on (at the scene-level). *)
    property Options: TGLFlatTextOptions read FOptions write SetOptions;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLBitmapFontRange ------------------
// ------------------

constructor TGLBitmapFontRange.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TGLBitmapFontRange.Destroy;
begin
  inherited;
end;

procedure TGLBitmapFontRange.Assign(Source: TPersistent);
begin
  if Source is TGLBitmapFontRange then
  begin
    FStartASCII := TGLBitmapFontRange(Source).FStartASCII;
    FStopASCII := TGLBitmapFontRange(Source).FStopASCII;
    FStartGlyphIdx := TGLBitmapFontRange(Source).FStartGlyphIdx;
    NotifyChange;
  end
  else
    inherited;
end;

procedure TGLBitmapFontRange.NotifyChange;
begin
  FCharCount := Integer(FStopASCII) - Integer(FStartASCII) + 1;
  FStopGlyphIdx := FStartGlyphIdx + FCharCount - 1;
  if Assigned(Collection) then
    (Collection as TGLBitmapFontRanges).NotifyChange;
end;

function TGLBitmapFontRange.GetDisplayName: string;
begin
  Result := Format('ASCII [#%d, #%d] -> Glyphs [%d, %d]',
    [Integer(FStartASCII), Integer(FStopASCII), StartGlyphIdx, StopGlyphIdx]);
end;

function TGLBitmapFontRange.GetStartASCII: WideString;
begin
  Result := FStartASCII;
end;

function TGLBitmapFontRange.GetStopASCII: WideString;
begin
  Result := FStopASCII;
end;

procedure TGLBitmapFontRange.SetStartASCII(const val: WideString);
begin
  if (Length(val) > 0) and (val[1] <> FStartASCII) then
  begin
    FStartASCII := val[1];
    if FStartASCII > FStopASCII then
      FStopASCII := FStartASCII;
    NotifyChange;
  end;
end;

procedure TGLBitmapFontRange.SetStopASCII(const val: WideString);
begin
  if (Length(val) > 0) and (FStopASCII <> val[1]) then
  begin
    FStopASCII := val[1];
    if FStopASCII < FStartASCII then
      FStartASCII := FStopASCII;
    NotifyChange;
  end;
end;

procedure TGLBitmapFontRange.SetStartGlyphIdx(val: Integer);
begin
  val := MaxInteger(0, val);
  if val <> FStartGlyphIdx then
  begin
    FStartGlyphIdx := val;
    NotifyChange;
  end;
end;

// ------------------
// ------------------ TGLBitmapFontRanges ------------------
// ------------------
constructor TGLBitmapFontRanges.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  inherited Create(TGLBitmapFontRange);
end;

destructor TGLBitmapFontRanges.Destroy;
begin
  inherited;
end;

function TGLBitmapFontRanges.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TGLBitmapFontRanges.SetItems(index: Integer;
  const val: TGLBitmapFontRange);
begin
  inherited Items[index] := val;
end;

function TGLBitmapFontRanges.GetItems(index: Integer): TGLBitmapFontRange;
begin
  Result := TGLBitmapFontRange(inherited Items[index]);
end;

function TGLBitmapFontRanges.Add: TGLBitmapFontRange;
begin
  Result := (inherited Add) as TGLBitmapFontRange;
end;

function TGLBitmapFontRanges.Add(const StartASCII, StopASCII: WideChar)
  : TGLBitmapFontRange;
begin
  Result := Add;
  Result.StartASCII := StartASCII;
  Result.StopASCII := StopASCII;
end;

function TGLBitmapFontRanges.Add(const StartASCII, StopASCII: AnsiChar)
  : TGLBitmapFontRange;
begin
  Result := Add(CharToWideChar(StartASCII), CharToWideChar(StopASCII));
end;

function TGLBitmapFontRanges.FindItemID(ID: Integer): TGLBitmapFontRange;
begin
  Result := (inherited FindItemID(ID)) as TGLBitmapFontRange;
end;

function TGLBitmapFontRanges.CharacterToTileIndex(aChar: WideChar): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      if (aChar >= FStartASCII) and (aChar <= FStopASCII) then
      begin
        Result := StartGlyphIdx + Integer(aChar) - Integer(FStartASCII);
        Break;
      end;
    end;
end;

function TGLBitmapFontRanges.TileIndexToChar(aIndex: Integer): WideChar;
var
  i: Integer;
begin
  Result := #0;
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      if (aIndex >= StartGlyphIdx) and (aIndex <= StopGlyphIdx) then
      begin
        Result := WideChar(aIndex - StartGlyphIdx + Integer(FStartASCII));
        Break;
      end;
    end;
end;

procedure TGLBitmapFontRanges.Update(Item: TCollectionItem);
begin
  inherited;
  NotifyChange;
end;

procedure TGLBitmapFontRanges.NotifyChange;
begin
  FCharCount := CalcCharacterCount;

  if Assigned(FOwner) then
  begin
    if FOwner is TGLBaseSceneObject then
      TGLBaseSceneObject(FOwner).StructureChanged
    else if FOwner is TGLCustomBitmapFont then
      TGLCustomBitmapFont(FOwner).NotifyChange(Self);
  end;
end;

function TGLBitmapFontRanges.CalcCharacterCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    with Items[i] do
      Inc(Result, Integer(FStopASCII) - Integer(FStartASCII) + 1);
end;

// ------------------
// ------------------ TGLCustomBitmapFont ------------------
// ------------------

constructor TGLCustomBitmapFont.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRanges := TGLBitmapFontRanges.Create(Self);
  FGlyphs := TPicture.Create;
  FGlyphs.OnChange := OnGlyphsChanged;
  FCharWidth := 16;
  FCharHeight := 16;
  FHSpace := 1;
  FVSpace := 1;
  FUsers := TList.Create;
  FMinFilter := miLinear;
  FMagFilter := maLinear;
  FTextures := TList.Create;
  FTextureModified := true;
end;

destructor TGLCustomBitmapFont.Destroy;
begin
  FreeTextureHandle;
  inherited Destroy;
  FTextures.Free;
  FRanges.Free;
  FGlyphs.Free;
  Assert(FUsers.Count = 0);
  FUsers.Free;
end;

function TGLCustomBitmapFont.GetCharWidth(Ch: WideChar): Integer;
var
  chi: Integer;
begin
  chi := CharacterToTileIndex(ch);
  if Length(FChars) = 0 then
    ResetCharWidths;
  if chi >= 0 then
    Result := FChars[chi].w
  else
    Result := 0;
end;

function TGLCustomBitmapFont.CalcStringWidth(const aText
  : UnicodeString): Integer;
var
  i: Integer;
begin
  if aText <> '' then
  begin
    Result := -HSpace + Length(aText) * (HSpaceFix + HSpace);
    for i := 1 to Length(aText) do
      Result := Result + GetCharWidth(aText[i]);
  end
  else
    Result := 0;
end;

procedure TGLCustomBitmapFont.ResetCharWidths(w: Integer = -1);
var
  i: Integer;
begin
  FCharsLoaded := False;
  i := CharacterCount;
  if Length(FChars) < i then
    SetLength(FChars, i);
  if w < 0 then
    w := CharWidth;
  for i := 0 to High(FChars) do
    FChars[i].w := w;
end;

procedure TGLCustomBitmapFont.SetCharWidths(index, value: Integer);
begin
  if index >= 0 then
    FChars[index].w := value;
end;

procedure TGLCustomBitmapFont.SetRanges(const val: TGLBitmapFontRanges);
begin
  FRanges.Assign(val);
  InvalidateUsers;
end;

procedure TGLCustomBitmapFont.SetGlyphs(const val: TPicture);
begin
  FGlyphs.Assign(val);
end;

procedure TGLCustomBitmapFont.SetCharWidth(const val: Integer);
begin
  if val <> FCharWidth then
  begin
    if val > 1 then
      FCharWidth := val
    else
      FCharWidth := 1;
    InvalidateUsers;
  end;
end;

procedure TGLCustomBitmapFont.SetCharHeight(const val: Integer);
begin
  if val <> FCharHeight then
  begin
    if val > 1 then
      FCharHeight := val
    else
      FCharHeight := 1;
    InvalidateUsers;
  end;
end;

procedure TGLCustomBitmapFont.SetGlyphsIntervalX(const val: Integer);
begin
  if val > 0 then
    FGlyphsIntervalX := val
  else
    FGlyphsIntervalX := 0;
  InvalidateUsers;
end;

procedure TGLCustomBitmapFont.SetGlyphsIntervalY(const val: Integer);
begin
  if val > 0 then
    FGlyphsIntervalY := val
  else
    FGlyphsIntervalY := 0;
  InvalidateUsers;
end;

procedure TGLCustomBitmapFont.SetHSpace(const val: Integer);
begin
  if val <> FHSpace then
  begin
    FHSpace := val;
    InvalidateUsers;
  end;
end;

procedure TGLCustomBitmapFont.SetVSpace(const val: Integer);
begin
  if val <> FVSpace then
  begin
    FVSpace := val;
    InvalidateUsers;
  end;
end;

procedure TGLCustomBitmapFont.SetMagFilter(AValue: TGLMagFilter);
begin
  if AValue <> FMagFilter then
  begin
    FMagFilter := AValue;
    TextureChanged;
    InvalidateUsers;
  end;
end;

procedure TGLCustomBitmapFont.SetMinFilter(AValue: TGLMinFilter);
begin
  if AValue <> FMinFilter then
  begin
    FMinFilter := AValue;
    TextureChanged;
    InvalidateUsers;
  end;
end;

procedure TGLCustomBitmapFont.SetGlyphsAlpha(val: TGLTextureImageAlpha);
begin
  if val <> FGlyphsAlpha then
  begin
    FGlyphsAlpha := val;
    TextureChanged;
    InvalidateUsers;
  end;
end;

procedure TGLCustomBitmapFont.OnGlyphsChanged(Sender: TObject);
begin
  InvalidateUsers;
  // when empty, width is 0 and roundup give 1
  if not Glyphs.Graphic.Empty then
  begin
    if FTextureWidth = 0 then
      FTextureWidth := RoundUpToPowerOf2(Glyphs.Width);
    if FTextureHeight = 0 then
      FTextureHeight := RoundUpToPowerOf2(Glyphs.Height);
  end;
end;

procedure TGLCustomBitmapFont.RegisterUser(anObject: TGLBaseSceneObject);
begin
  Assert(FUsers.IndexOf(anObject) < 0);
  FUsers.Add(anObject);
end;

procedure TGLCustomBitmapFont.UnRegisterUser(anObject: TGLBaseSceneObject);
begin
  FUsers.Remove(anObject);
end;

procedure TGLCustomBitmapFont.PrepareImage(var ARci: TGLRenderContextInfo);
var
  bitmap: TBitmap;
  bitmap32: TGLImage;
  cap: Integer;
  X, Y, w, h: Integer;
  t: TGLTextureHandle;
begin
  // only check when really used
  if FTextureWidth = 0 then
  begin
    FTextureWidth := ARci.GLStates.MaxTextureSize;
    if FTextureWidth > 512 then
      FTextureWidth := 512;
    if FTextureWidth < 64 then
      FTextureWidth := 64;
  end;
  if FTextureHeight = 0 then
  begin
    FTextureHeight := ARci.GLStates.MaxTextureSize;
    if FTextureHeight > 512 then
      FTextureHeight := 512;
    if FTextureHeight < 64 then
      FTextureHeight := 64;
  end;

  X := 0;
  Y := 0;
  w := Glyphs.Width;
  h := Glyphs.Height;

  // was an error...
  FTextRows := 1 + (h - 1) div FTextureHeight;
  FTextCols := 1 + (w - 1) div FTextureWidth;

  bitmap := TBitmap.Create;
  with bitmap do
  begin
{$IFDEF MSWINDOWS}
    // due to lazarus doesn't properly support pixel formats
    PixelFormat := pf32bit;
{$ENDIF}
    SetSize(RoundUpToPowerOf2(FTextureWidth),
      RoundUpToPowerOf2(FTextureHeight));
  end;
  bitmap32 := TGLImage.Create;
  while (X < w) and (Y < h) do
  begin
    t := TGLTextureHandle.Create;
    FTextures.Add(t);
    // prepare handle
    t.AllocateHandle;
    // texture registration
    t.Target := ttTexture2D;
    ARci.GLStates.TextureBinding[0, ttTexture2D] := t.Handle;

    // copy data
    bitmap.Canvas.Draw(-X, -Y, Glyphs.Graphic);
    // Clipboard.Assign(bitmap);
    bitmap32.Assign(bitmap);
    bitmap32.Narrow;
    with bitmap32 do
    begin
      case FGlyphsAlpha of
        tiaAlphaFromIntensity:
          SetAlphaFromIntensity;
        tiaSuperBlackTransparent:
          SetAlphaTransparentForColor($000000);
        tiaLuminance:
          SetAlphaFromIntensity;
        tiaLuminanceSqrt:
          begin
            SetAlphaFromIntensity;
            SqrtAlpha;
          end;
        tiaOpaque:
          SetAlphaToValue(255);
        tiaDefault, tiaTopLeftPointColorTransparent:
          SetAlphaTransparentForColor(Data[Width * (Height - 1)]);
      else
        Assert(False);
      end;
      RegisterAsOpenGLTexture(t, not(FMinFilter in [miNearest, miLinear]),
        TextureFormat, cap, cap, cap);
    end;

    PrepareParams(ARci);
    t.NotifyDataUpdated;

    Inc(X, FTextureWidth);
    if X >= w then
    begin
      Inc(Y, FTextureHeight);
      X := 0;
    end;
  end;
  bitmap.Free;
  bitmap32.Free;
end;

procedure TGLCustomBitmapFont.PrepareParams(var ARci: TGLRenderContextInfo);
const
  cTextureMagFilter: array [maNearest .. maLinear] of Cardinal = (GL_NEAREST, GL_LINEAR);
  cTextureMinFilter: array [miNearest .. miLinearMipmapLinear] of Cardinal =
    (GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST, GL_LINEAR_MIPMAP_NEAREST,
    GL_NEAREST_MIPMAP_LINEAR, GL_LINEAR_MIPMAP_LINEAR);
begin

  with ARci.GLStates do
  begin
    UnpackAlignment := 4;
    UnpackRowLength := 0;
    UnpackSkipRows := 0;
    UnpackSkipPixels := 0;
  end;

  begin
    gl.Hint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

    gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

    gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, cTextureMinFilter[FMinFilter]);
    gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, cTextureMagFilter[FMagFilter]);
  end;
end;

function TGLCustomBitmapFont.TileIndexToChar(aIndex: Integer): WideChar;
begin
  Result := FRanges.TileIndexToChar(aIndex);
end;

function TGLCustomBitmapFont.CharacterToTileIndex(aChar: WideChar): Integer;
begin
  Result := FRanges.CharacterToTileIndex(aChar);
end;

procedure TGLCustomBitmapFont.RenderString(var ARci: TGLRenderContextInfo;
  const aText: UnicodeString; aAlignment: TAlignment; aLayout: TTextLayout;
  const aColor: TColorVector; aPosition: PGLVector = nil;
  aReverseY: boolean = False);

  function AlignmentAdjustement(p: Integer): Single;
  var
    i: Integer;
  begin
    i := 0;
    while (p <= Length(aText)) and (aText[p] <> #13) do
    begin
      Inc(p);
      Inc(i);
    end;
    case aAlignment of
      taLeftJustify:
        Result := 0;
      taRightJustify:
        Result := -CalcStringWidth(Copy(aText, p - i, i))
    else // taCenter
      Result := Round(-CalcStringWidth(Copy(aText, p - i, i)) * 0.5);
    end;
  end;

  function LayoutAdjustement: Single;
  var
    i, n: Integer;
  begin
    n := 1;
    for i := 1 to Length(aText) do
      if aText[i] = #13 then
        Inc(n);
    case TTextLayout(aLayout) of
      tlTop:     Result := 0;
      tlBottom:  Result := (n * (CharHeight + VSpace) - VSpace);
    else // tlCenter
      Result := Round((n * (CharHeight + VSpace) - VSpace) * 0.5);
    end;
  end;

var
  i, chi: Integer;
  pch: PCharInfo;
  TopLeft, BottomRight: TTexPoint;
  vTopLeft, vBottomRight: TGLVector;
  deltaV, spaceDeltaH: Single;
  currentChar: WideChar;
begin
  if (aText = '') then
    Exit;
  // prepare texture if necessary
  CheckTexture(ARci);
  // precalcs
  if Assigned(aPosition) then
    MakePoint(vTopLeft, aPosition.X + AlignmentAdjustement(1),
      aPosition.Y + LayoutAdjustement, 0)
  else
    MakePoint(vTopLeft, AlignmentAdjustement(1), LayoutAdjustement, 0);
  deltaV := -(CharHeight + VSpace);
  if aReverseY then
    vBottomRight.Y := vTopLeft.Y + CharHeight
  else
    vBottomRight.Y := vTopLeft.Y - CharHeight;
  vBottomRight.Z := 0;
  vBottomRight.W := 1;
  spaceDeltaH := GetCharWidth(#32) + HSpaceFix + HSpace;
  // set states
  with ARci.GLStates do
  begin
    ActiveTextureEnabled[ttTexture2D] := true;
    Disable(stLighting);
    Enable(stBlend);
    SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
    FLastTexture := nil;
  end;

  // start rendering
  gl.Color4fv(@aColor);
  gl.Begin_(GL_QUADS);
  for i := 1 to Length(aText) do
  begin
    currentChar := WideChar(aText[i]);
    case currentChar of
      #0 .. #12, #14 .. #31:
        ; // ignore
      #13:
        begin
          if Assigned(aPosition) then
            vTopLeft.X := aPosition.X + AlignmentAdjustement(i + 1)
          else
            vTopLeft.X := AlignmentAdjustement(i + 1);
          vTopLeft.Y := vTopLeft.Y + deltaV;
          if aReverseY then
            vBottomRight.Y := vTopLeft.Y + CharHeight
          else
            vBottomRight.Y := vTopLeft.Y - CharHeight;
        end;
      #32:
        vTopLeft.X := vTopLeft.X + spaceDeltaH;
    else
      chi := CharacterToTileIndex(currentChar);
      if chi < 0 then
        continue; // not found
      pch := @FChars[chi];
      if pch.w > 0 then

        begin
          GetICharTexCoords(ARci, chi, TopLeft, BottomRight);
          vBottomRight.X := vTopLeft.X + pch.w;

          gl.TexCoord2fv(@TopLeft);
          gl.Vertex4fv(@vTopLeft);

          gl.TexCoord2f(TopLeft.S, BottomRight.t);
          gl.Vertex2f(vTopLeft.X, vBottomRight.Y);

          gl.TexCoord2fv(@BottomRight);
          gl.Vertex4fv(@vBottomRight);

          gl.TexCoord2f(BottomRight.S, TopLeft.t);
          gl.Vertex2f(vBottomRight.X, vTopLeft.Y);

          vTopLeft.X := vTopLeft.X + pch.w + HSpace;
        end;
    end;
  end;
  gl.End_;
  // unbind texture
  ARci.GLStates.TextureBinding[0, ttTexture2D] := 0;
  ARci.GLStates.ActiveTextureEnabled[ttTexture2D] := False;
end;

procedure TGLCustomBitmapFont.TextOut(var rci: TGLRenderContextInfo; X, Y: Single;
  const Text: UnicodeString; const Color: TColorVector);
var
  V: TGLVector;
begin
  V.X := X;
  V.Y := Y;
  V.Z := 0;
  V.W := 1;
  RenderString(rci, Text, taLeftJustify, tlTop, Color, @V, true);
end;

procedure TGLCustomBitmapFont.TextOut(var rci: TGLRenderContextInfo; X, Y: Single;
  const Text: UnicodeString; const Color: TColor);
begin
  TextOut(rci, X, Y, Text, ConvertWinColor(Color));
end;

function TGLCustomBitmapFont.TextWidth(const Text: UnicodeString): Integer;
begin
  Result := CalcStringWidth(Text);
end;

function TGLCustomBitmapFont.CharactersPerRow: Integer;
begin
  if FGlyphs.Width > 0 then
    Result := (FGlyphs.Width + FGlyphsIntervalX)
      div (FGlyphsIntervalX + FCharWidth)
  else
    Result := 0;
end;

function TGLCustomBitmapFont.CharacterCount: Integer;
begin
  Result := FRanges.CharacterCount;
end;

procedure TGLCustomBitmapFont.GetCharTexCoords(Ch: WideChar;
  var TopLeft, BottomRight: TTexPoint);
var
  chi, tileIndex: Integer;
  ci: TCharInfo;
  r: Integer;
begin
  chi := CharacterToTileIndex(ch);
  if not FCharsLoaded then
  begin
    ResetCharWidths;
    FCharsLoaded := true;
    r := CharactersPerRow;
    for tileIndex := 0 to CharacterCount - 1 do
    begin
      FChars[tileIndex].l := (tileIndex mod r) * (CharWidth + GlyphsIntervalX);
      FChars[tileIndex].t := (tileIndex div r) * (CharHeight + GlyphsIntervalY);
    end;
  end;

  if (chi < 0) or (chi >= CharacterCount) then
  begin
    // invalid char
    TopLeft := NullTexPoint;
    BottomRight := NullTexPoint;
    Exit;
  end;

  ci := FChars[chi];
  ci.l := ci.l mod FTextureWidth;
  ci.t := ci.t mod FTextureHeight;

  TopLeft.S := ci.l / FTextureWidth;
  TopLeft.t := 1 - ci.t / FTextureHeight;
  BottomRight.S := (ci.l + ci.w) / FTextureWidth;
  BottomRight.t := 1 - (ci.t + CharHeight) / FTextureHeight;
end;

// TileIndexToTexCoords it also activates the target texture
procedure TGLCustomBitmapFont.GetICharTexCoords(var ARci: TGLRenderContextInfo;
  Chi: Integer; out TopLeft, BottomRight: TTexPoint);
var
  tileIndex: Integer;
  ci: TCharInfo;
  t: TGLTextureHandle;
  r, c: Integer;
begin
  if not FCharsLoaded then
  begin
    r := CharactersPerRow;
    if r = 0 then
      Exit;
    ResetCharWidths;
    FCharsLoaded := true;
    for tileIndex := 0 to CharacterCount - 1 do
    begin
      FChars[tileIndex].l := (tileIndex mod r) * (CharWidth + GlyphsIntervalX);
      FChars[tileIndex].t := (tileIndex div r) * (CharHeight + GlyphsIntervalY);
    end;
  end;

  if (chi < 0) or (chi >= CharacterCount) then
  begin
    // invalid char
    TopLeft := NullTexPoint;
    BottomRight := NullTexPoint;
    Exit;
  end;

  ci := FChars[chi];

  c := ci.l div FTextureWidth;
  r := ci.t div FTextureHeight;
  ci.l := ci.l mod FTextureWidth;
  ci.t := ci.t mod FTextureHeight;
  t := FTextures[r * FTextCols + c];

  TopLeft.S := ci.l / FTextureWidth;
  TopLeft.t := 1 - ci.t / FTextureHeight;
  BottomRight.S := (ci.l + ci.w) / FTextureWidth;
  BottomRight.t := 1 - (ci.t + CharHeight) / FTextureHeight;

  if t <> FLastTexture then
    begin
      FLastTexture := t;
      gl.End_;
      ARci.GLStates.TextureBinding[0, ttTexture2D] := t.Handle;
      gl.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
      gl.Begin_(GL_QUADS);
    end;
end;

procedure TGLCustomBitmapFont.InvalidateUsers;
var
  i: Integer;
begin
  FCharsLoaded := False;
  FTextureModified := true;
  for i := FUsers.Count - 1 downto 0 do
    TGLBaseSceneObject(FUsers[i]).NotifyChange(Self);
end;

procedure TGLCustomBitmapFont.FreeTextureHandle;
var
  i: Integer;
begin
  FTextureModified := true;
  for i := 0 to FTextures.Count - 1 do
    TObject(FTextures[i]).Free;
  FTextures.Clear;
end;

procedure TGLCustomBitmapFont.TextureChanged;
begin
  FTextureModified := true;
end;

// force texture when needed
procedure TGLCustomBitmapFont.CheckTexture(var ARci: TGLRenderContextInfo);
var
  i: Integer;
begin
  // important: IsDataNeedUpdate might come from another source!
  for i := 0 to FTextures.Count - 1 do
    FTextureModified := FTextureModified or TGLTextureHandle(FTextures[i])
      .IsDataNeedUpdate;

  if FTextureModified then
  begin
    FreeTextureHandle; // instances are recreated in prepare
    PrepareImage(ARci);
    FTextureModified := False;
  end;
end;

function TGLCustomBitmapFont.TextureFormat: Integer;
begin
  Result := GL_RGBA;
end;

// ------------------
// ------------------ TGLFlatText ------------------
// ------------------

constructor TGLFlatText.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FModulateColor := TGLColor.CreateInitialized(Self, clrWhite);
end;

destructor TGLFlatText.Destroy;
begin
  FModulateColor.Free;
  BitmapFont := nil;
  inherited;
end;

procedure TGLFlatText.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FBitmapFont) then
    BitmapFont := nil;
  inherited;
end;

procedure TGLFlatText.SetBitmapFont(const val: TGLCustomBitmapFont);
begin
  if val <> FBitmapFont then
  begin
    if Assigned(FBitmapFont) then
      FBitmapFont.UnRegisterUser(Self);
    FBitmapFont := val;
    if Assigned(FBitmapFont) then
    begin
      FBitmapFont.RegisterUser(Self);
      FBitmapFont.FreeNotification(Self);
    end;
    StructureChanged;
  end;
end;

procedure TGLFlatText.SetText(const val: UnicodeString);
begin
  FText := val;
  StructureChanged;
end;

procedure TGLFlatText.SetAlignment(const val: TAlignment);
begin
  FAlignment := val;
  StructureChanged;
end;

procedure TGLFlatText.SetLayout(const val: TTextLayout);
begin
  FLayout := val;
  StructureChanged;
end;

procedure TGLFlatText.SetModulateColor(const val: TGLColor);
begin
  FModulateColor.Assign(val);
end;

procedure TGLFlatText.SetOptions(const val: TGLFlatTextOptions);
begin
  if val <> FOptions then
  begin
    FOptions := val;
    StructureChanged;
  end;
end;

procedure TGLFlatText.DoRender(var rci: TGLRenderContextInfo;
  renderSelf, renderChildren: boolean);
begin
  if Assigned(FBitmapFont) and (Text <> '') then
  begin
    rci.GLStates.PolygonMode := pmFill;
    if FModulateColor.Alpha <> 1 then
    begin
      rci.GLStates.Enable(stBlend);
      rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
    end;
    if ftoTwoSided in FOptions then
      rci.GLStates.Disable(stCullFace);
    FBitmapFont.RenderString(rci, Text, FAlignment, FLayout,
      FModulateColor.Color);
  end;
  if Count > 0 then
    Self.renderChildren(0, Count - 1, rci);
end;

procedure TGLFlatText.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLFlatText) then
  begin
    BitmapFont := TGLFlatText(Source).BitmapFont;
    Text := TGLFlatText(Source).Text;
    Alignment := TGLFlatText(Source).Alignment;
    Layout := TGLFlatText(Source).Layout;
    ModulateColor := TGLFlatText(Source).ModulateColor;
    Options := TGLFlatText(Source).Options;
  end;
  inherited Assign(Source);
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

RegisterClasses([TGLBitmapFont, TGLFlatText]);

end.
