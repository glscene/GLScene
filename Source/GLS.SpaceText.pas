//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.SpaceText;

(*
  3D Text component.

  Note: You can get valid extents (including AABB's) of this component only
  after it has been rendered for the first time. It means if you ask its
  extents during / after its creation, you will get zeros.

  Also extents are valid only when SpaceText has one line.
*)

interface

{$I GLScene.inc}
{$IFDEF UNIX}{$MESSAGE Error 'Unit not supported'} {$ENDIF}

uses
  Winapi.OpenGL,
  WinApi.Windows,
  WinApi.Messages,
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Types,
  VCL.Dialogs,
  VCL.Graphics,
  VCL.Controls,

  GLS.VectorTypes,
  GLS.OpenGLTokens,
  GLS.Scene,
  GLS.Texture,
  GLS.Context,
  GLS.VectorGeometry,
  GLS.Strings,
  GLS.RenderContextInfo,
  GLS.State;

type

  TGLSpaceTextCharRange = (stcrDefault, stcrAlphaNum, stcrNumbers, stcrWide);

  // Note: haAligned, haCentrically, haFitIn have not been implemented!
  TGLTextHorzAdjust = (haLeft, haCenter, haRight, haAligned, haCentrically, haFitIn);

  TGLTextVertAdjust = (vaTop, vaCenter, vaBottom, vaBaseLine);

  TGLTextAdjust = class(TPersistent)
  private
    FHorz: TGLTextHorzAdjust;
    FVert: TGLTextVertAdjust;
    FOnChange: TNotifyEvent;
    procedure SetHorz(const Value: TGLTextHorzAdjust);
    procedure SetVert(const Value: TGLTextVertAdjust);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Horz: TGLTextHorzAdjust read FHorz write SetHorz default haLeft;
    property Vert: TGLTextVertAdjust read FVert write SetVert
      default vaBaseLine;
  end;

  // holds an entry in the font manager list (used in TGLSpaceText)
  PFontEntry = ^TFontEntry;

  TFontEntry = record
    Name: string;
    FVirtualHandle: TGLVirtualHandleTransf;
    Styles: TFontStyles;
    Extrusion: Single;
    RefCount: Integer;
    allowedDeviation: Single;
    firstChar, lastChar: Integer;
    glyphMetrics: array of TGlyphMetricsFloat;
    FClients: TList;
  end;

  // Renders a text in 3D.
  TGLSpaceText = class(TGLSceneObject)
  private
    FFont: TFont;
    FExtrusion: Single;
    FAllowedDeviation: Single;
    FCharacterRange: TGLSpaceTextCharRange;
    FAdjust: TGLTextAdjust;
    FAspectRatio: Single;
    FOblique: Single;
    FTextHeight: Single;
    FLines: TStringList;
    procedure SetCharacterRange(const val: TGLSpaceTextCharRange);
    procedure SetAllowedDeviation(const val: Single);
    procedure SetExtrusion(AValue: Single);
    procedure SetFont(AFont: TFont);
    function GetText: WideString;
    procedure SetLines(const Value: TStringList);
    procedure SetText(const AText: WideString);
    procedure SetAdjust(const Value: TGLTextAdjust);
    procedure SetAspectRatio(const Value: Single);
    procedure SetOblique(const Value: Single);
    procedure SetTextHeight(const Value: Single);
  protected
    FTextFontEntry: PFontEntry;
    FontChanged: Boolean;
    procedure DestroyHandle; override;
    procedure OnFontChange(sender: TObject);
    procedure GetFirstAndLastChar(var firstChar, lastChar: Integer);
    procedure DoOnLinesChange(sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    function TextWidth(const str: WideString = ''): Single;
    function TextMaxHeight(const str: WideString = ''): Single;
    function TextMaxUnder(const str: WideString = ''): Single;
    (* Note: this fuction is valid only after text has been rendered
      the first time. Before that it returns zeros. *)
    procedure TextMetrics(const str: WideString; out width, maxHeight, maxUnder: Single);
    procedure NotifyFontChanged;
    procedure NotifyChange(sender: TObject); override;
    procedure DefaultHandler(var Message); override;
    function AxisAlignedDimensionsUnscaled: TGLVector; override;
    function BarycenterAbsolutePosition: TGLVector; override;
  published
    (* Adjusts the 3D font extrusion.
      If Extrusion=0, the characters will be flat (2D), values >0 will
      give them a third dimension. *)
    property Extrusion: Single read FExtrusion write SetExtrusion;
    property Font: TFont read FFont write SetFont;
    property Text: WideString read GetText write SetText stored False;
    property Lines: TStringList read FLines write SetLines;
    // Quality related, see Win32 help for wglUseFontOutlines
    property allowedDeviation: Single read FAllowedDeviation
      write SetAllowedDeviation;
    (* Character range to convert.
      Converting less characters saves time and memory... *)
    property CharacterRange: TGLSpaceTextCharRange read FCharacterRange
      write SetCharacterRange default stcrDefault;
    property AspectRatio: Single read FAspectRatio write SetAspectRatio;
    property TextHeight: Single read FTextHeight write SetTextHeight;
    property Oblique: Single read FOblique write SetOblique;
    property Adjust: TGLTextAdjust read FAdjust write SetAdjust;
  end;

  // Manages a list of fonts for which display lists were created.
  TFontManager = class(TList)
  private
    FCurrentBase: Integer;
  protected
    procedure NotifyClients(Clients: TList);
    procedure VirtualHandleAlloc(sender: TGLVirtualHandle;
      var handle: Cardinal);
    procedure VirtualHandleDestroy(sender: TGLVirtualHandle;
      var handle: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;
    function FindFont(AName: string; FStyles: TFontStyles; FExtrusion: Single;
      FAllowedDeviation: Single; FFirstChar, FLastChar: Integer): PFontEntry;
    function GetFontBase(AName: string; FStyles: TFontStyles;
      FExtrusion: Single; allowedDeviation: Single;
      firstChar, lastChar: Integer; client: TObject): PFontEntry;
    procedure Release(entry: PFontEntry; client: TObject);
  end;

function FontManager: TFontManager;
procedure ReleaseFontManager;

var
  vFontManagerMsgID: Cardinal;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

const
  cFontManagerMsg = 'FontManagerMessage';

var
  vFontManager: TFontManager;

function FontManager: TFontManager;
begin
  if not Assigned(vFontManager) then
    vFontManager := TFontManager.Create;
  Result := vFontManager;
end;

procedure ReleaseFontManager;
begin
  if Assigned(vFontManager) then
  begin
    vFontManager.Free;
    vFontManager := nil;
  end;
end;

// ------------------
// ------------------ TGLTextAdjust ------------------
// ------------------

constructor TGLTextAdjust.Create;
begin
  inherited;
  FHorz := haLeft;
  FVert := vaBaseLine;
end;

procedure TGLTextAdjust.Assign(Source: TPersistent);
begin
  if Source is TGLTextAdjust then
  begin
    FHorz := TGLTextAdjust(Source).Horz;
    FVert := TGLTextAdjust(Source).Vert;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TGLTextAdjust.SetHorz(const Value: TGLTextHorzAdjust);
begin
  if FHorz <> Value then
  begin
    FHorz := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TGLTextAdjust.SetVert(const Value: TGLTextVertAdjust);
begin
  if Value <> FVert then
  begin
    FVert := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

// ------------------
// ------------------ TGLSpaceText ------------------
// ------------------

constructor TGLSpaceText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  FFont.Name := 'Arial';
  FontChanged := True;
  CharacterRange := stcrDefault;
  FFont.OnChange := OnFontChange;
  FAdjust := TGLTextAdjust.Create;
  FAdjust.OnChange := OnFontChange;
  FLines := TStringList.Create;
  FLines.OnChange := DoOnLinesChange;
end;

destructor TGLSpaceText.Destroy;
begin
  FAdjust.OnChange := nil;
  FAdjust.Free;
  FFont.OnChange := nil;
  FFont.Free;
  FLines.Free;
  FontManager.Release(FTextFontEntry, Self);
  inherited Destroy;
end;

procedure TGLSpaceText.TextMetrics(const str: WideString;
  out width, maxHeight, maxUnder: Single);
var
  i, firstChar, lastChar, diff: Integer;
  buf: WideString;
  gmf: TGlyphMetricsFloat;
begin
  width := 0;
  maxUnder := 0;
  maxHeight := 0;
  if Assigned(FTextFontEntry) then
  begin
    GetFirstAndLastChar(firstChar, lastChar);
    if str = '' then
      buf := GetText
    else
      buf := str;
    for i := 1 to Length(buf) do
    begin
      diff := Integer(buf[i]) - firstChar;
      if diff > High(FTextFontEntry^.glyphMetrics) then
        continue;
      gmf := FTextFontEntry^.glyphMetrics[diff];
      width := width + gmf.gmfCellIncX;
      if gmf.gmfptGlyphOrigin.y > maxHeight then
        maxHeight := gmf.gmfptGlyphOrigin.y;
      if gmf.gmfptGlyphOrigin.y - gmf.gmfBlackBoxY < maxUnder then
        maxUnder := gmf.gmfptGlyphOrigin.y - gmf.gmfBlackBoxY;
    end;
  end;
end;

function TGLSpaceText.TextWidth(const str: WideString = ''): Single;
var
  mh, mu: Single;
begin
  TextMetrics(str, Result, mh, mu);
end;

function TGLSpaceText.TextMaxHeight(const str: WideString = ''): Single;
var
  w, mu: Single;
begin
  TextMetrics(str, w, Result, mu);
end;

function TGLSpaceText.TextMaxUnder(const str: WideString = ''): Single;
var
  w, mh: Single;
begin
  TextMetrics(str, w, mh, Result);
end;

procedure TGLSpaceText.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TGLSpaceText then
  begin
    FAdjust.Assign(TGLSpaceText(Source).FAdjust);
    FFont.Assign(TGLSpaceText(Source).FFont);
    FAllowedDeviation := TGLSpaceText(Source).allowedDeviation;
    FAspectRatio := TGLSpaceText(Source).FAspectRatio;
    FCharacterRange := TGLSpaceText(Source).CharacterRange;
    FExtrusion := TGLSpaceText(Source).FExtrusion;
    FOblique := TGLSpaceText(Source).FOblique;
    FLines.Text := TGLSpaceText(Source).FLines.Text;
    FTextHeight := TGLSpaceText(Source).FTextHeight;
    StructureChanged;
  end;
end;

procedure TGLSpaceText.BuildList(var rci: TGLRenderContextInfo);
var
  textL, maxUnder, maxHeight: Single;
  charScale: Single;
  i, j, k, c: Integer;
  glBase: Cardinal;
  dirtyLine, cleanLine: WideString;
begin
  if Length(GetText) > 0 then
  begin
    gl.PushMatrix;

    // FAspectRatio ignore
    if FAspectRatio <> 0 then
      gl.Scalef(FAspectRatio, 1, 1);
    if FOblique <> 0 then
      gl.Rotatef(FOblique, 0, 0, 1);

    glBase := FTextFontEntry^.FVirtualHandle.handle;
    case FCharacterRange of
      stcrAlphaNum:
        gl.ListBase(Cardinal(Integer(glBase) - 32));
      stcrNumbers:
        gl.ListBase(Cardinal(Integer(glBase) - Integer('0')));
    else
      gl.ListBase(glBase);
    end;

    rci.GLStates.PushAttrib([sttPolygon]);
    for i := 0 to FLines.Count - 1 do
    begin
      gl.PushMatrix;

      TextMetrics(FLines.Strings[i], textL, maxHeight, maxUnder);
      if (FAdjust.Horz <> haLeft) or (FAdjust.Vert <> vaBaseLine) or
        (FTextHeight <> 0) then
      begin
        if FTextHeight <> 0 then
        begin
          charScale := FTextHeight / maxHeight;
          gl.Scalef(charScale, charScale, 1);
        end;
        case FAdjust.Horz of
          haLeft: ; // nothing
          haCenter: gl.Translatef(-textL * 0.5, 0, 0);
          haRight: gl.Translatef(-textL, 0, 0);
        end;
        case FAdjust.Vert of
          vaBaseLine: ; // nothing;
          vaBottom: gl.Translatef(0, abs(maxUnder), 0);
          vaCenter: gl.Translatef(0, abs(maxUnder) * 0.5 - maxHeight * 0.5, 0);
          vaTop: gl.Translatef(0, -maxHeight, 0);
        end;
      end;

      gl.Translatef(0, -i * (maxHeight + FAspectRatio), 0);
      if FCharacterRange = stcrWide then
      begin
        dirtyLine := FLines.Strings[i];
        SetLength(cleanLine, Length(dirtyLine));
        k := 1;
        for j := 1 to Length(dirtyLine) do
        begin
          c := Integer(dirtyLine[j]);
          if (c >= FTextFontEntry^.firstChar) and
            (c <= FTextFontEntry^.lastChar) then
          begin
            cleanLine[k] := dirtyLine[j];
            Inc(k);
          end;
        end;
        if k > 1 then
          gl.CallLists(k - 1, GL_UNSIGNED_SHORT, PWideChar(cleanLine))
      end
      else
        gl.CallLists(Length(FLines.Strings[i]), GL_UNSIGNED_BYTE,
          PAnsiChar(AnsiString(FLines.Strings[i])));
      gl.PopMatrix;
    end;
    rci.GLStates.PopAttrib();
    gl.PopMatrix;
  end;
end;

procedure TGLSpaceText.DestroyHandle;
begin
  FontChanged := True;
  inherited;
end;

procedure TGLSpaceText.GetFirstAndLastChar(var firstChar, lastChar: Integer);
begin
  case FCharacterRange of
    stcrAlphaNum:
      begin
        firstChar := 32;
        lastChar := 127;
      end;
    stcrNumbers:
      begin
        firstChar := Integer('0');
        lastChar := Integer('9');
      end;
    stcrDefault:
      begin
        firstChar := 0;
        lastChar := 255;
      end;
    stcrWide:
      begin
        firstChar := 0;
        lastChar := $077F;
      end;
  end;
end;

procedure TGLSpaceText.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  firstChar, lastChar: Integer;
begin
  if GetText <> '' then
  begin
    if Assigned(FTextFontEntry) then
      FTextFontEntry^.FVirtualHandle.AllocateHandle;
    if FontChanged or (Assigned(FTextFontEntry) and
      (FTextFontEntry^.FVirtualHandle.IsDataNeedUpdate)) then
      with FFont do
      begin
        FontManager.Release(FTextFontEntry, Self);
        GetFirstAndLastChar(firstChar, lastChar);
        FTextFontEntry := FontManager.GetFontBase(Name, Style, FExtrusion,
          FAllowedDeviation, firstChar, lastChar, Self);
        FontChanged := False;
        FTextFontEntry^.FVirtualHandle.NotifyDataUpdated;
      end;
  end;
  inherited;
end;

procedure TGLSpaceText.SetExtrusion(AValue: Single);
begin
  Assert(AValue >= 0, 'Extrusion must be >=0');
  if FExtrusion <> AValue then
  begin
    FExtrusion := AValue;
    OnFontChange(nil);
  end;
end;

procedure TGLSpaceText.SetAllowedDeviation(const val: Single);
begin
  if FAllowedDeviation <> val then
  begin
    if val > 0 then
      FAllowedDeviation := val
    else
      FAllowedDeviation := 0;
    OnFontChange(nil);
  end;
end;

procedure TGLSpaceText.SetCharacterRange(const val: TGLSpaceTextCharRange);
begin
  if FCharacterRange <> val then
  begin
    FCharacterRange := val;
    OnFontChange(nil);
  end;
end;

procedure TGLSpaceText.SetFont(AFont: TFont);
begin
  FFont.Assign(AFont);
  OnFontChange(nil);
end;

procedure TGLSpaceText.OnFontChange(sender: TObject);
begin
  FontChanged := True;
  StructureChanged;
end;

procedure TGLSpaceText.SetText(const AText: WideString);
begin
  if GetText <> AText then
  begin
    FLines.Text := AText;
    // StructureChanged is Called in DoOnLinesChange.
  end;
end;

procedure TGLSpaceText.DoOnLinesChange(sender: TObject);
begin
  StructureChanged;
end;

function TGLSpaceText.GetText: WideString;
begin
  if FLines.Count = 1 then
    Result := FLines[0]
  else
    Result := FLines.Text;
end;

procedure TGLSpaceText.SetLines(const Value: TStringList);
begin
  FLines.Assign(Value);
end;

procedure TGLSpaceText.SetAdjust(const Value: TGLTextAdjust);
begin
  FAdjust.Assign(Value);
  StructureChanged;
end;

procedure TGLSpaceText.SetAspectRatio(const Value: Single);
begin
  if FAspectRatio <> Value then
  begin
    FAspectRatio := Value;
    StructureChanged;
  end;
end;

procedure TGLSpaceText.SetOblique(const Value: Single);
begin
  if FOblique <> Value then
  begin
    FOblique := Value;
    StructureChanged;
  end;
end;

procedure TGLSpaceText.SetTextHeight(const Value: Single);
begin
  if Value <> FTextHeight then
  begin
    FTextHeight := Value;
    StructureChanged;
  end;
end;

procedure TGLSpaceText.NotifyFontChanged;
begin
  FTextFontEntry := nil;
  FontChanged := True;
end;

procedure TGLSpaceText.NotifyChange(sender: TObject);
begin
  if sender is TFontManager then
    NotifyFontChanged
  else
    inherited;
end;

procedure TGLSpaceText.DefaultHandler(var Message);
begin
  with TMessage(Message) do
  begin
    if Msg = vFontManagerMsgID then
      NotifyFontChanged
    else
      inherited;
  end;
end;

function TGLSpaceText.BarycenterAbsolutePosition: TGLVector;
var
  lWidth, lHeightMax, lHeightMin: Single;
  AdjustVector: TGLVector;
begin
  TextMetrics(Text, lWidth, lHeightMax, lHeightMin);

  case FAdjust.FHorz of
    haLeft:
      AdjustVector.X := lWidth / 2;
    haCenter:
      AdjustVector.X := 0; // Nothing.
    haRight:
      AdjustVector.X := -lWidth / 2;
  else
    begin
      AdjustVector.X := 0;
      Assert(False, strErrorEx + strUnknownType); // Not implemented...
    end;
  end;

  case FAdjust.FVert of
    vaTop:
      AdjustVector.Y := -(abs(lHeightMin) * 0.5 + lHeightMax * 0.5);
    vaCenter:
      AdjustVector.Y := 0; // Nothing.
    vaBottom:
      AdjustVector.Y := (abs(lHeightMin) * 0.5 + lHeightMax * 0.5);
    vaBaseLine:
      AdjustVector.Y := -(abs(lHeightMin) * 0.5 - lHeightMax * 0.5);
  else
    begin
      AdjustVector.Y := 0;
      Assert(False, strErrorEx + strUnknownType); // Not implemented...
    end;
  end;

  AdjustVector.Z := -(FExtrusion / 2);
  AdjustVector.W := 1;
  Result := LocalToAbsolute(AdjustVector);
end;

function TGLSpaceText.AxisAlignedDimensionsUnscaled: TGLVector;
var
  lWidth, lHeightMax, lHeightMin: Single;
  charScale: Single;
begin
  TextMetrics(Text, lWidth, lHeightMax, lHeightMin);

  if FTextHeight = 0 then
    charScale := 1
  else
    charScale := FTextHeight / lHeightMax;

  Result.X := lWidth / 2 * charScale;
  Result.Y := (lHeightMax + abs(lHeightMin)) / 2 * charScale;
  Result.Z := FExtrusion / 2;
  Result.W := 0;
end;

// ------------------
// ------------------ TFontManager ------------------
// ------------------

constructor TFontManager.Create;
begin
  inherited;
end;

destructor TFontManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    TFontEntry(Items[i]^).FVirtualHandle.Free;
    NotifyClients(TFontEntry(Items[i]^).FClients);
    TFontEntry(Items[i]^).FClients.Free;
    TFontEntry(Items[i]^).Name := '';
    FreeMem(Items[i], SizeOf(TFontEntry));
  end;
  inherited Destroy;
end;

procedure TFontManager.VirtualHandleAlloc(sender: TGLVirtualHandle;
  var handle: Cardinal);
begin
  handle := FCurrentBase;
end;

procedure TFontManager.VirtualHandleDestroy(sender: TGLVirtualHandle;
  var handle: Cardinal);
begin
  if handle <> 0 then
    gl.DeleteLists(handle, sender.Tag);
end;

function TFontManager.FindFont(AName: string; FStyles: TFontStyles;
  FExtrusion: Single; FAllowedDeviation: Single; FFirstChar, FLastChar: Integer)
  : PFontEntry;
var
  i: Integer;
begin
  Result := nil;
  // try to find an entry with the required attributes
  for i := 0 to Count - 1 do
    with TFontEntry(Items[i]^) do
      if (CompareText(Name, AName) = 0) and (Styles = FStyles) and
        (Extrusion = FExtrusion) and (allowedDeviation = FAllowedDeviation) and
        (firstChar = FFirstChar) and (lastChar = FLastChar) then
      begin
        // entry found
        Result := Items[i];
        Break;
      end;
end;

function TFontManager.GetFontBase(AName: string; FStyles: TFontStyles;
  FExtrusion: Single; allowedDeviation: Single; firstChar, lastChar: Integer;
  client: TObject): PFontEntry;
var
  NewEntry: PFontEntry;
  MemDC: HDC;
  AFont: TFont;
  nbLists: Integer;
  success: Boolean;
begin
  NewEntry := FindFont(AName, FStyles, FExtrusion, allowedDeviation, firstChar,
    lastChar);
  if Assigned(NewEntry) then
  begin
    Inc(NewEntry^.RefCount);
    if NewEntry^.FClients.IndexOf(client) < 0 then
      NewEntry^.FClients.Add(client);
    Result := NewEntry;
  end
  else
    Result := nil;
  if (Result = nil) or (Assigned(Result) and
    (Result^.FVirtualHandle.handle = 0)) then
  begin
    // no entry found, or entry was purged
    nbLists := lastChar - firstChar + 1;
    if not Assigned(NewEntry) then
    begin
      // no entry found, so create one
      New(NewEntry);
      NewEntry^.Name := AName;
      NewEntry^.FVirtualHandle := TGLVirtualHandleTransf.Create;
      NewEntry^.FVirtualHandle.OnAllocate := VirtualHandleAlloc;
      NewEntry^.FVirtualHandle.OnDestroy := VirtualHandleDestroy;
      NewEntry^.FVirtualHandle.Tag := nbLists;
      NewEntry^.Styles := FStyles;
      NewEntry^.Extrusion := FExtrusion;
      NewEntry^.RefCount := 1;
      NewEntry^.firstChar := firstChar;
      NewEntry^.lastChar := lastChar;
      SetLength(NewEntry^.glyphMetrics, nbLists);
      NewEntry^.allowedDeviation := allowedDeviation;
      NewEntry^.FClients := TList.Create;
      NewEntry^.FClients.Add(client);
      Add(NewEntry);
    end;
    // creates a font to be used while display list creation
    AFont := TFont.Create;
    MemDC := CreateCompatibleDC(0);
    try
      AFont.Name := AName;
      AFont.Style := FStyles;
      SelectObject(MemDC, AFont.handle);
      FCurrentBase := gl.GenLists(nbLists);
      if FCurrentBase = 0 then
        raise Exception.Create('FontManager: no more display lists available');
      NewEntry^.FVirtualHandle.AllocateHandle;
      if lastChar < 256 then
      begin
        success := wglUseFontOutlinesA(MemDC, firstChar, nbLists, FCurrentBase,
          allowedDeviation, FExtrusion, WGL_FONT_POLYGONS,
          @NewEntry^.glyphMetrics[0]);
      end
      else
      begin
        success := wglUseFontOutlinesW(MemDC, firstChar, nbLists, FCurrentBase,
          allowedDeviation, FExtrusion, WGL_FONT_POLYGONS,
          @NewEntry^.glyphMetrics[0]);
      end;
      if not success then
        raise Exception.Create('FontManager: font creation failed');
    finally
      AFont.Free;
      DeleteDC(MemDC);
    end;
    Result := NewEntry;
  end;
end;

procedure TFontManager.Release(entry: PFontEntry; client: TObject);
var
  hMsg: TMessage;
begin
  if Assigned(entry) then
  begin
    Dec(entry^.RefCount);
    if Assigned(client) then
    begin
      hMsg.Msg := vFontManagerMsgID;
      client.DefaultHandler(hMsg);
    end;
    entry^.FClients.Remove(client);
    if entry^.RefCount = 0 then
    begin
      entry^.FVirtualHandle.Free;
      NotifyClients(entry^.FClients);
      entry^.FClients.Free;
      Remove(entry);
      Dispose(entry)
    end;
  end;
end;

procedure TFontManager.NotifyClients(Clients: TList);
var
  i: Integer;
  hMsg: TMessage;
begin
  hMsg.Msg := vFontManagerMsgID;
  for i := 0 to Clients.Count - 1 do
    TObject(Clients[i]).DefaultHandler(hMsg);
end;

// -------------------------------------------------------------
initialization
// -------------------------------------------------------------

vFontManagerMsgID := RegisterWindowMessage(cFontManagerMsg);
RegisterClass(TGLSpaceText);

finalization

ReleaseFontManager;

end.
