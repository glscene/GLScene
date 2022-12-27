//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.WindowsFont;

(* TFont Import into a BitmapFont using variable width...*)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.Windows,
  System.Classes,
  System.Math,
  System.SysUtils,
  System.Types,
  System.UITypes,
  VCL.Graphics,

  GLS.Scene,
  GLS.Texture,
  GLS.BitmapFont,
  GLS.RenderContextInfo,
  GLS.VectorLists,
  GLS.Utils,
  GLS.VectorGeometry,
  GLS.OpenGLTokens,
  GLS.ApplicationFileIO,
  GLS.VectorTypes;

type

  (* A bitmap font automatically built from a TFont. 
     It works like a TGLBitmapfont, you set ranges and which chars are assigned
     to which indexes, however here you also set the Font property to any TFont
     available to the system and it renders in GLScene as close to that font
     as posible, on some font types this is 100% on some a slight difference
     in spacing can occur at most 1 pixel per char on some char combinations. 
     Ranges must be sorted in ascending ASCII order and should not overlap.
     As the font texture is automatically layed out, the Ranges StartGlyphIdx
     property is ignored and replaced appropriately. *)
  TGLWindowsBitmapFont = class(TGLCustomBitmapFont)
  private
    FFont: TFont;
    procedure SetList(const AList : TGLIntegerList);
  protected
    procedure SetFont(value: TFont);
    procedure LoadWindowsFont; virtual;
    function  StoreRanges: Boolean;
    procedure PrepareImage(var ARci: TGLRenderContextInfo); override;
    function  TextureFormat: Integer; override;
    procedure StreamlineRanges;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure NotifyChange(Sender: TObject); override;
    function FontTextureWidth: Integer;
    function FontTextureHeight: Integer;
    procedure EnsureString(const s : String); overload;
    procedure EnsureChars(const AStart, AEnd: widechar);
    property Glyphs;
  published
   (* The font used to prepare the texture. 
     Note: the font color is ignored. *)
    property Font: TFont read FFont write SetFont;
    property HSpace;
    property VSpace;
    property MagFilter;
    property MinFilter;
    property Ranges stored StoreRanges;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

const
  cDefaultLast = '}';

var
  Win32PlatformIsUnicode : Boolean;

// ------------------
// ------------------ TGLWindowsBitmapFont ------------------
// ------------------


constructor TGLWindowsBitmapFont.Create(AOwner: TComponent);
begin
  inherited;

  FFont := TFont.Create;
  FFont.Color := clWhite;
  FFont.OnChange := NotifyChange;
  GlyphsAlpha := tiaAlphaFromIntensity;
  EnsureChars(' ', cDefaultLast);
end;


destructor TGLWindowsBitmapFont.Destroy;
begin
  FFont.Free;
  Ranges.Clear; 
  inherited;
end;


function TGLWindowsBitmapFont.FontTextureWidth: Integer;
begin
  Result := Glyphs.Width;
end;

function TGLWindowsBitmapFont.FontTextureHeight: Integer;
begin
  Result := Glyphs.Height;
end;


procedure TGLWindowsBitmapFont.SetFont(value: TFont);
begin
  FFont.Assign(value);
end;


procedure TGLWindowsBitmapFont.NotifyChange(Sender: TObject);
begin
  StreamlineRanges;
  FreeTextureHandle;
  InvalidateUsers;
  inherited;
end;


procedure TGLWindowsBitmapFont.LoadWindowsFont;

  procedure ComputeCharRects(bitmap: TBitmap);
  var
    px, py, cw, n, x, y: Integer;
    PaddedHeight : integer;
    buffer : array[0..2] of WideChar;
    p : PCharInfo;
    r : TRect;
  begin
    buffer[1] := WideChar(#32);
    buffer[2] := WideChar(#0);
    PaddedHeight:= CharHeight + GlyphsIntervalY;
    x := bitmap.Width; y := bitmap.Height;
    px := 0;
    py := 0;
    if y < CharHeight then px := x;
    p  := @FChars[0];
    for n := 0 to CharacterCount - 1 do
    begin
      cw := p.w;
      if cw > 0 then
      begin
        Inc(cw, GlyphsIntervalX);

        if px + cw > x then
        begin
          px := 0;
          Inc(py, PaddedHeight);
          if py + PaddedHeight > y then
          begin
            py := bitmap.Height;
            y  := py + TextureHeight;
            bitmap.Height := y;
            with bitmap.Canvas do
            begin
              Brush.Style := bsSolid;
              Brush.Color := clBlack;
              FillRect(Rect(0, py, x, y));
            end;
          end;
        end;

        if Assigned(bitmap) then
        begin
          //+1 makes right align (padding left);
          // I prefer padding right for compatibility with bitmap font...
          p.l := px;
          //should make it consistent, same as above
          p.t := py;

          r.Left := px;
          r.Top  := py;
          r.Right  := px + cw;
          r.Bottom := py + PaddedHeight;
          buffer[0] := TileIndexToChar(n);
          // Draw the Char, the trailing space is to properly handle the italics.
          // credits to the Unicode version of SynEdit for this function call. GPL/MPL as GLScene
          ExtTextOutW(bitmap.Canvas.Handle, p.l, p.t, ETO_CLIPPED, @r, buffer, 1, nil);
        end;
        Inc(px, cw);
      end
      else
      begin
        p.l := 0;
        p.t := 0;
      end;
      inc(p);
    end;
  end;

  // credits to the Unicode version of SynEdit for this function. GPL/MPL as GLScene
  function GetTextSize(DC: HDC; Str: PWideChar; Count: Integer): TSize;
    var tm: TTextMetric;

  begin
    Result.cx := 0;
    Result.cy := 0;
    GetTextExtentPoint32W(DC, Str, Count, Result);
    if not Win32PlatformIsUnicode then
    begin
      GetTextMetricsW(DC, tm);
      if tm.tmPitchAndFamily and TMPF_TRUETYPE <> 0 then
        Result.cx := Result.cx - tm.tmOverhang
      else
        Result.cx := tm.tmAveCharWidth * Count;
    end;
  end;

var
  bitmap: TBitmap;
  ch: widechar;
  i, cw, nbChars, n: Integer;
begin
  InvalidateUsers;
  Glyphs.OnChange := nil;
  //accessing Bitmap might trigger onchange
  bitmap := Glyphs.Bitmap;

  bitmap.Height      := 0;
  //due to lazarus doesn't properly support pixel formats
     bitmap.PixelFormat := pf32bit;
  with bitmap.Canvas do
  begin
    Font := Self.Font;
    Font.Color := clWhite;
    // get characters dimensions for the font
    // character size without padding; paddings are used from GlyphsInterval
    CharWidth  := Round(MaxInteger(TextWidth('M'), TextWidth('W'), TextWidth('_')));
    CharHeight := TextHeight('"_pI|,');
    // used for padding
    GlyphsIntervalX := 1;
    GlyphsIntervalY := 1;
    if fsItalic in Font.Style then
    begin
      // italics aren't properly acknowledged in font width
      HSpaceFix := -(CharWidth div 3);
      CharWidth := CharWidth - HSpaceFix;
    end
    else
      HSpaceFix := 0;
  end;

  nbChars := CharacterCount;

  // Retrieve width of all characters (texture width)
  ResetCharWidths(0);
  n := 0;
  for i := 0 to nbChars - 1 do
  begin
    ch := TileIndexToChar(i);
    cw := GetTextSize(bitmap.canvas.Handle, @ch, 1).cx-HSpaceFix;
    n  := n + cw + GlyphsIntervalX;
    SetCharWidths(i, cw);
  end;
  //try to make best guess...
  //~total pixels, including some waste (10%)
  n := n * (CharHeight + GlyphsIntervalY) * 11 div 10;
  TextureWidth := min(512, RoundUpToPowerOf2( round(sqrt(n)) ));
  TextureHeight := min(512, RoundUpToPowerOf2( n div TextureWidth));

  bitmap.Width := TextureWidth;

  ComputeCharRects(bitmap);
  FCharsLoaded := true;
  Glyphs.OnChange := OnGlyphsChanged;
end;


function TGLWindowsBitmapFont.StoreRanges: Boolean;
begin
  Result := (Ranges.Count <> 1) or (Ranges[0].StartASCII[1] <> ' ') or (Ranges[0].StopASCII[1] <> cDefaultLast);
end;

type
  TFriendlyRange = class(TGLBitmapFontRange);

procedure TGLWindowsBitmapFont.StreamlineRanges;
var
  I, C: Integer;
begin
  C := 0;
  for I := 0 to Ranges.Count - 1 do
  begin
    TFriendlyRange(Ranges[I]).FStartGlyphIdx := C;
    Inc(C, Ranges[I].CharCount);
    TFriendlyRange(Ranges[I]).FStopGlyphIdx := MaxInteger(C - 1, 0);
  end;
end;

procedure TGLWindowsBitmapFont.SetList(const AList: TGLIntegerList);
var
  i : integer;
  f, n, s : integer;
begin
  //add existing ranges
  for I := 0 to Ranges.Count - 1 do
    with Ranges.Items[I] do
      AList.AddSerie(integer(StartASCII[1]), 1, CharCount);

  AList.SortAndRemoveDuplicates;

  Ranges.Clear;
  Ranges.BeginUpdate;
  if AList.Count > 0 then
  begin
    i := 0;
    while (i < AList.Count) and (AList[i] < 32) do inc(i);
    while i < AList.Count do
    begin
      f := AList[i]; n := f; s := Ranges.CharacterCount;
      while (i < AList.Count) and (n = AList[i]) do
      begin
        inc(i);
        inc(n);
      end;
      Ranges.Add(widechar(f), widechar(pred(n))).StartGlyphIdx := s;
    end;
  end;

  Ranges.EndUpdate;
  TextureChanged;
  InvalidateUsers;
end;

//add characters to internal list
procedure TGLWindowsBitmapFont.EnsureChars(const AStart, AEnd: widechar);
var
  c : WideChar;
  ACharList : TGLIntegerList;
begin
  ACharList := TGLIntegerList.Create;
  for c := AStart to AEnd do
      ACharList.Add(integer(c));
  SetList(ACharList);
  ACharList.Free;
end;

//add characters to internal list
procedure TGLWindowsBitmapFont.EnsureString(const s: String);
var
  i : Integer;
  ACharList : TGLIntegerList;
begin
  ACharList := TGLIntegerList.Create;
  for i := 1 to length(s) do
      ACharList.Add(integer(s[i]));
  SetList(ACharList);
  ACharList.Free;
end;


procedure TGLWindowsBitmapFont.PrepareImage(var ARci: TGLRenderContextInfo);
begin
  LoadWindowsFont;
  inherited PrepareImage(ARci);
end;


function TGLWindowsBitmapFont.TextureFormat: Integer;
begin
  Result := GL_ALPHA;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
  Win32PlatformIsUnicode := (Win32Platform = VER_PLATFORM_WIN32_NT);


  RegisterClasses([TGLWindowsBitmapFont]);

end.

