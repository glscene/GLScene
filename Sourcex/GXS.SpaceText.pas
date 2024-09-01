//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.SpaceText;
(*
  3D Text component.

  Note: You can get valid extents (including AABB's) of this component only
  after it has been rendered for the first time. It means if you ask its
  extents during / after its creation, you will get zeros.

  Also extents are valid only when SpaceText has one line.
*)
interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.Windows,
  WinApi.Messages,
  System.Classes,
  System.UITypes,
  System.SysUtils,
  FMX.Graphics,

  GXS.VectorGeometry,
  GXS.Strings,
  GXS.VectorTypes,
  GXS.Scene,
  GXS.Texture,
  GXS.Context,
  GXS.RenderContextInfo,
  GXS.State;

type

  TgxSpaceTextCharRange = (stcrDefault, stcrAlphaNum, stcrNumbers, stcrWide);

  // Note: haAligned, haCentrically, haFitIn have not been implemented!
  TgxTextHorzAdjust = (haLeft, haCenter, haRight, haAligned,
    haCentrically, haFitIn);

  TgxTextVertAdjust = (vaTop, vaCenter, vaBottom, vaBaseLine);

  TgxTextAdjust = class(TPersistent)
  private
    FHorz: TgxTextHorzAdjust;
    FVert: TgxTextVertAdjust;
    FOnChange: TNotifyEvent;
    procedure SetHorz(const Value: TgxTextHorzAdjust);
    procedure SetVert(const Value: TgxTextVertAdjust);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Horz: TgxTextHorzAdjust read FHorz write SetHorz default haLeft;
    property Vert: TgxTextVertAdjust read FVert write SetVert default vaBaseLine;
  end;

  // Holds an entry in the font manager list (used in TgxSpaceText)
  PFontEntry = ^TFontEntry;

  TFontEntry = record
    Name: string;
    FVirtualHandle: TgxVirtualHandleTransf;
    Styles: TFontStyles;
    Extrusion: Single;
    RefCount: Integer;
    allowedDeviation: Single;
    firstChar, lastChar: Integer;
    glyphMetrics: array of TGlyphMetricsFloat;
    FClients: TList;
  end;

  // Renders a text in 3D.
  TgxSpaceText = class(TgxSceneObject)
  private
    FFont: TFont;
    FExtrusion: Single;
    FAllowedDeviation: Single;
    FCharacterRange: TgxSpaceTextCharRange;
    FAdjust: TgxTextAdjust;
    FAspectRatio: Single;
    FOblique: Single;
    FTextHeight: Single;
    FLines: TStringList;
    procedure SetCharacterRange(const val: TgxSpaceTextCharRange);
    procedure SetAllowedDeviation(const val: Single);
    procedure SetExtrusion(AValue: Single);
    procedure SetFont(AFont: TFont);
    function GetText: WideString;
    procedure SetLines(const Value: TStringList);
    procedure SetText(const AText: WideString);
    procedure SetAdjust(const Value: TgxTextAdjust);
    procedure SetAspectRatio(const Value: Single);
    procedure SetOblique(const Value: Single);
    procedure SetTextHeight(const Value: Single);
  protected
    FTextFontEntry: PFontEntry;
    FontChanged: Boolean;
    procedure DestroyHandle; override;
    procedure OnFontChange(sender: TObject);
    procedure GetFirstAndLastChar(var firstChar, lastChar: Integer);
    procedure DoOnLinesChange(sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
    procedure DoRender(var ARci: TgxRenderContextInfo;
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
    function AxisAlignedDimensionsUnscaled: TVector4f; override;
    function BarycenterAbsolutePosition: TVector4f; override;
  published
    (* Adjusts the 3D font extrusion.
      If Extrusion=0, the characters will be flat (2D), values >0 will
      give them a third dimension. *)
    property Extrusion: Single read FExtrusion write SetExtrusion;
    property Font: TFont read FFont write SetFont;
    property Text: WideString read GetText write SetText stored False;
    property Lines: TStringList read FLines write SetLines;
    // Quality related, see Win32 help for wglUseFontOutlines
    property allowedDeviation: Single read FAllowedDeviation write SetAllowedDeviation;
    // Character range to convert. Converting less characters saves time and memory...
    property CharacterRange: TgxSpaceTextCharRange read FCharacterRange
      write SetCharacterRange default stcrDefault;
    property AspectRatio: Single read FAspectRatio write SetAspectRatio;
    property TextHeight: Single read FTextHeight write SetTextHeight;
    property Oblique: Single read FOblique write SetOblique;
    property Adjust: TgxTextAdjust read FAdjust write SetAdjust;
  end;

  // Manages a list of fonts for which display lists were created.
  TFontManager = class(TList)
  private
    FCurrentBase: Integer;
  protected
    procedure NotifyClients(Clients: TList);
    procedure VirtualHandleAlloc(sender: TgxVirtualHandle; var handle: Cardinal);
    procedure VirtualHandleDestroy(sender: TgxVirtualHandle; var handle: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;
    function FindFont(AName: string; FStyles: TFontStyles; FExtrusion: Single;
      FAllowedDeviation: Single; FFirstChar, FLastChar: Integer): PFontEntry;
    function GetFontBase(AName: string; FStyles: TFontStyles; FExtrusion: Single;
	   allowedDeviation: Single; firstChar, lastChar: Integer; client: TObject): PFontEntry;
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
  cFontManagerMsg = 'Scene FontManagerMessage';

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
// ------------------ TgxTextAdjust ------------------
// ------------------

constructor TgxTextAdjust.Create;
begin
  inherited;
  FHorz := haLeft;
  FVert := vaBaseLine;
end;

procedure TgxTextAdjust.Assign(Source: TPersistent);
begin
  if Source is TgxTextAdjust then
  begin
    FHorz := TgxTextAdjust(Source).Horz;
    FVert := TgxTextAdjust(Source).Vert;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TgxTextAdjust.SetHorz(const Value: TgxTextHorzAdjust);
begin
  if FHorz <> Value then
  begin
    FHorz := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TgxTextAdjust.SetVert(const Value: TgxTextVertAdjust);
begin
  if Value <> FVert then
  begin
    FVert := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

// ------------------
// ------------------ TgxSpaceText ------------------
// ------------------

constructor TgxSpaceText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  FFont.Family := 'Arial'; //in VCL FFont.Name
  FontChanged := True;
  CharacterRange := stcrDefault;
  FFont.OnChanged := OnFontChange;
  FAdjust := TgxTextAdjust.Create;
  FAdjust.OnChange := OnFontChange;
  FLines := TStringList.Create;
  FLines.OnChange := DoOnLinesChange;
end;

destructor TgxSpaceText.Destroy;
begin
  FAdjust.OnChange := nil;
  FAdjust.Free;
  FFont.OnChanged := nil;
  FFont.Free;
  FLines.Free;
  FontManager.Release(FTextFontEntry, Self);
  inherited Destroy;
end;

procedure TgxSpaceText.TextMetrics(const str: WideString;
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

function TgxSpaceText.TextWidth(const str: WideString = ''): Single;
var
  mh, mu: Single;
begin
  TextMetrics(str, Result, mh, mu);
end;

function TgxSpaceText.TextMaxHeight(const str: WideString = ''): Single;
var
  w, mu: Single;
begin
  TextMetrics(str, w, Result, mu);
end;

function TgxSpaceText.TextMaxUnder(const str: WideString = ''): Single;
var
  w, mh: Single;
begin
  TextMetrics(str, w, mh, Result);
end;

procedure TgxSpaceText.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TgxSpaceText then
  begin
    FAdjust.Assign(TgxSpaceText(Source).FAdjust);
    FFont.Assign(TgxSpaceText(Source).FFont);
    FAllowedDeviation := TgxSpaceText(Source).allowedDeviation;
    FAspectRatio := TgxSpaceText(Source).FAspectRatio;
    FCharacterRange := TgxSpaceText(Source).CharacterRange;
    FExtrusion := TgxSpaceText(Source).FExtrusion;
    FOblique := TgxSpaceText(Source).FOblique;
    FLines.Text := TgxSpaceText(Source).FLines.Text;
    FTextHeight := TgxSpaceText(Source).FTextHeight;
    StructureChanged;
  end;
end;

procedure TgxSpaceText.BuildList(var rci: TgxRenderContextInfo);
var
  textL, maxUnder, maxHeight: Single;
  charScale: Single;
  i, j, k, c: Integer;
  glBase: GLuint;
  dirtyLine, cleanLine: WideString;
begin
  if Length(GetText) > 0 then
  begin
    glPushMatrix;

    // FAspectRatio ignore
    if FAspectRatio <> 0 then
      glScalef(FAspectRatio, 1, 1);
    if FOblique <> 0 then
      glRotatef(FOblique, 0, 0, 1);

    glBase := FTextFontEntry^.FVirtualHandle.handle;
    case FCharacterRange of
      stcrAlphaNum:
        glListBase(GLuint(Integer(glBase) - 32));
      stcrNumbers:
        glListBase(GLuint(Integer(glBase) - Integer('0')));
    else
      glListBase(glBase);
    end;

    glPushAttrib(GL_POLYGON_BIT);
    for i := 0 to FLines.Count - 1 do
    begin
      glPushMatrix;

      TextMetrics(FLines.Strings[i], textL, maxHeight, maxUnder);
      if (FAdjust.Horz <> haLeft) or (FAdjust.Vert <> vaBaseLine) or
        (FTextHeight <> 0) then
      begin
        if FTextHeight <> 0 then
        begin
          charScale := FTextHeight / maxHeight;
          glScalef(charScale, charScale, 1);
        end;
        case FAdjust.Horz of
          haLeft:
            ; // nothing
          haCenter:
            glTranslatef(-textL * 0.5, 0, 0);
          haRight:
            glTranslatef(-textL, 0, 0);
        end;
        case FAdjust.Vert of
          vaBaseLine:
            ; // nothing;
          vaBottom:
            glTranslatef(0, abs(maxUnder), 0);
          vaCenter:
            glTranslatef(0, abs(maxUnder) * 0.5 - maxHeight * 0.5, 0);
          vaTop:
            glTranslatef(0, -maxHeight, 0);
        end;
      end;

      glTranslatef(0, -i * (maxHeight + FAspectRatio), 0);
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
          glCallLists(k - 1, GL_UNSIGNED_SHORT, PWideChar(cleanLine))
      end
      else
        glCallLists(Length(FLines.Strings[i]), GL_UNSIGNED_BYTE,
          PChar(String(FLines.Strings[i])));
      glPopMatrix;
    end;
    rci.gxStates.PopAttrib();
    glPopMatrix;
  end;
end;

procedure TgxSpaceText.DestroyHandle;
begin
  FontChanged := True;
  inherited;
end;

procedure TgxSpaceText.GetFirstAndLastChar(var firstChar, lastChar: Integer);
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

procedure TgxSpaceText.DoRender(var ARci: TgxRenderContextInfo;
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

// SetExtrusion
//

procedure TgxSpaceText.SetExtrusion(AValue: Single);
begin
  Assert(AValue >= 0, 'Extrusion must be >=0');
  if FExtrusion <> AValue then
  begin
    FExtrusion := AValue;
    OnFontChange(nil);
  end;
end;

procedure TgxSpaceText.SetAllowedDeviation(const val: Single);
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

procedure TgxSpaceText.SetCharacterRange(const val: TgxSpaceTextCharRange);
begin
  if FCharacterRange <> val then
  begin
    FCharacterRange := val;
    OnFontChange(nil);
  end;
end;

procedure TgxSpaceText.SetFont(AFont: TFont);
begin
  FFont.Assign(AFont);
  OnFontChange(nil);
end;

procedure TgxSpaceText.OnFontChange(sender: TObject);
begin
  FontChanged := True;
  StructureChanged;
end;

procedure TgxSpaceText.SetText(const AText: WideString);
begin
  if GetText <> AText then
  begin
    FLines.Text := AText;
    // StructureChanged is Called in DoOnLinesChange.
  end;
end;

procedure TgxSpaceText.DoOnLinesChange(sender: TObject);
begin
  StructureChanged;
end;

function TgxSpaceText.GetText: WideString;
begin
  if FLines.Count = 1 then
    Result := FLines[0]
  else
    Result := FLines.Text;
end;

procedure TgxSpaceText.SetLines(const Value: TStringList);
begin
  FLines.Assign(Value);
end;

procedure TgxSpaceText.SetAdjust(const Value: TgxTextAdjust);
begin
  FAdjust.Assign(Value);
  StructureChanged;
end;

procedure TgxSpaceText.SetAspectRatio(const Value: Single);
begin
  if FAspectRatio <> Value then
  begin
    FAspectRatio := Value;
    StructureChanged;
  end;
end;

procedure TgxSpaceText.SetOblique(const Value: Single);
begin
  if FOblique <> Value then
  begin
    FOblique := Value;
    StructureChanged;
  end;
end;

procedure TgxSpaceText.SetTextHeight(const Value: Single);
begin
  if Value <> FTextHeight then
  begin
    FTextHeight := Value;
    StructureChanged;
  end;
end;

procedure TgxSpaceText.NotifyFontChanged;
begin
  FTextFontEntry := nil;
  FontChanged := True;
end;

procedure TgxSpaceText.NotifyChange(sender: TObject);
begin
  if sender is TFontManager then
    NotifyFontChanged
  else
    inherited;
end;

procedure TgxSpaceText.DefaultHandler(var Message);
begin
  with TMessage(Message) do
  begin
    if Msg = vFontManagerMsgID then
      NotifyFontChanged
    else
      inherited;
  end;
end;

function TgxSpaceText.BarycenterAbsolutePosition: TVector4f;
var
  lWidth, lHeightMax, lHeightMin: Single;
  AdjustVector: TVector4f;
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

function TgxSpaceText.AxisAlignedDimensionsUnscaled: TVector4f;
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

procedure TFontManager.VirtualHandleAlloc(sender: TgxVirtualHandle;
  var handle: Cardinal);
begin
  handle := FCurrentBase;
end;

procedure TFontManager.VirtualHandleDestroy(sender: TgxVirtualHandle;
  var handle: Cardinal);
begin
  if handle <> 0 then
    glDeleteLists(handle, sender.Tag);
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
      NewEntry^.FVirtualHandle := TgxVirtualHandleTransf.Create;
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
    // create a font to be used while display list creation
    AFont := TFont.Create;
    MemDC := CreateCompatibleDC(0);
    try
      AFont.Family := AName;
      AFont.Style := FStyles;
      { TODO : E2003 Undeclared identifier: 'handle' }
      (*SelectObject(MemDC, AFont.handle);*)
      FCurrentBase := glGenLists(nbLists);
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
RegisterClass(TgxSpaceText);

finalization

ReleaseFontManager;

end.
