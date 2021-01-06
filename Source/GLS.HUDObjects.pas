//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.HUDObjects;

(* GLScene objects that get rendered in 2D coordinates *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  System.Classes,
  Vcl.StdCtrls,

  GLS.OpenGLTokens,
  GLS.Scene,
  GLS.VectorTypes,
  GLS.Coordinates,
  GLS.PersistentClasses,
  GLS.VectorGeometry,
  GLS.Objects,
  GLS.BitmapFont,
  GLS.Color,
  GLS.RenderContextInfo,
  GLS.Material,
  GLS.Texture;

type

  (*  A rectangular area, NOT perspective projected.
    (x, y) coordinates map directly to the viewport (in pixels) and refer
    the center of the area.
    The coordinate system is that of an equivalent TCanvas, ie. top-left
    point is the origin (0, 0).
    The z component is ignored and Z-Buffer is disabled when rendering.
     Using TGLHUDSprite in 2D only scenes :
    The most convenient way to use a TGLHUDSprite as a simple 2D sprite with
    blending capabilities (transparency or additive), is to set the texture
    mode to tmModulate, in FrontProperties, to use the Emission color to
    control coloring/intensity, and finally use the Diffuse color's alpha
    to control transparency (while setting the other RGB components to 0).
    You can also control aplha-blending by defining a <1 value in the sprite's
    AlphaChannel field. This provides you with hardware accelerated,
    alpha-blended blitting.
    Note : since TGLHUDSprite works in absolute coordinates, TGLProxyObject
    can't be used to duplicate an hud sprite. *)
  TGLHUDSprite = class(TGLSprite)
  private
    FXTiles, FYTiles: Integer;
    function StoreWidth: Boolean;
    function StoreHeight: Boolean;
  protected
    procedure SetXTiles(const val: Integer);
    procedure SetYTiles(const val: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoRender(var rci: TGLRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
  published
    property XTiles: Integer read FXTiles write SetXTiles default 1;
    property YTiles: Integer read FYTiles write SetYTiles default 1;
    // Redeclare them with new default values.
    property Width stored StoreWidth;
    property Height stored StoreHeight;
  end;

  (* A 2D text displayed and positionned in 2D coordinates.
    The HUDText uses a character font defined and stored by a TGLBitmapFont
    component. The text can be scaled and rotated (2D), the layout and
    alignment can also be controled. *)
  TGLHUDText = class(TGLImmaterialSceneObject)
  private
    FBitmapFont: TGLCustomBitmapFont;
    FText: UnicodeString;
    FRotation: Single;
    FAlignment: TAlignment;
    FLayout: TTextLayout;
    FModulateColor: TGLColor;
  protected
    procedure SetBitmapFont(const val: TGLCustomBitmapFont);
    procedure SetText(const val: UnicodeString);
    procedure SetRotation(const val: Single);
    procedure SetAlignment(const val: TAlignment);
    procedure SetLayout(const val: TTextLayout);
    procedure SetModulateColor(const val: TGLColor);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure RenderTextAtPosition(const X, Y, Z: Single;
      var rci: TGLRenderContextInfo);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var rci: TGLRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
  published
    (* Refers the bitmap font to use.
      The referred bitmap font component stores and allows access to
      individual character bitmaps. *)
    property BitmapFont: TGLCustomBitmapFont read FBitmapFont
      write SetBitmapFont;
    (* Text to render.
      Be aware that only the characters available in the bitmap font will
      be rendered. CR LF sequences are allowed. *)
    property Text: UnicodeString read FText write SetText;
    // Rotation angle in degrees (2d).
    property Rotation: Single read FRotation write SetRotation;
    (* Controls the text alignment (horizontal).
      Possible values : taLeftJustify, taRightJustify, taCenter *)
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    (* Controls the text layout (vertical).
      Possible values : tlTop, tlCenter, tlBottom *)
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    // Color modulation, can be used for fade in/out too.
    property ModulateColor: TGLColor read FModulateColor write SetModulateColor;
  end;

  (* Position (X, Y and X) is in absolute coordinates. This component converts
    them to screen coordinates and renderes text there. *)
  TGLAbsoluteHUDText = class(TGLHUDText)
  public
    procedure DoRender(var rci: TGLRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
  end;

  (* Position (X and Y) is expected in a [0..1] range (from Screen size)
    This component converts this position to the actual screen position and
    renders the text there. This way a HUD text always appears to be in the
    the same place, regardless of the currect screen resolution.
    Note: this still does not solve the font scaling problem. *)
  TGLResolutionIndependantHUDText = class(TGLHUDText)
  public
    procedure DoRender(var rci: TGLRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
    constructor Create(AOwner: TComponent); override;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

uses
  GLS.Context,
  GLS.State,
  GLS.XOpenGL;

// ------------------
// ------------------ TGLHUDSprite ------------------
// ------------------


constructor TGLHUDSprite.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  Width := 16;
  Height := 16;
  FXTiles := 1;
  FYTiles := 1;
end;

procedure TGLHUDSprite.SetXTiles(const val: Integer);
begin
  if val <> FXTiles then
  begin
    FXTiles := val;
    StructureChanged;
  end;
end;

procedure TGLHUDSprite.SetYTiles(const val: Integer);
begin
  if val <> FYTiles then
  begin
    FYTiles := val;
    StructureChanged;
  end;
end;

procedure TGLHUDSprite.DoRender(var rci: TGLRenderContextInfo;
  renderSelf, renderChildren: Boolean);
var
  vx, vy, vx1, vy1, f: Single;
  u0, v0, u1, v1: Integer;
begin
  if rci.ignoreMaterials then
    Exit;
  Material.Apply(rci);
  repeat
    if AlphaChannel <> 1 then
    begin
      if stLighting in rci.GLStates.States then
        rci.GLStates.SetGLMaterialAlphaChannel(GL_FRONT, AlphaChannel)
      else
        with Material.GetActualPrimaryMaterial.FrontProperties.Diffuse do
          gl.Color4f(Red, Green, Blue, AlphaChannel);
    end;
    // Prepare matrices
    gl.MatrixMode(GL_MODELVIEW);
    gl.PushMatrix;
    gl.LoadMatrixf(@TGLSceneBuffer(rci.buffer).BaseProjectionMatrix);
    if rci.renderDPI = 96 then
      f := 1
    else
      f := rci.renderDPI / 96;
    gl.Scalef(2 / rci.viewPortSize.cx, 2 / rci.viewPortSize.cy, 1);
    gl.Translatef(f * Position.X - rci.viewPortSize.cx * 0.5,
      rci.viewPortSize.cy * 0.5 - f * Position.Y, Position.Z);
    if Rotation <> 0 then
      gl.Rotatef(Rotation, 0, 0, 1);
    gl.MatrixMode(GL_PROJECTION);
    gl.PushMatrix;
    gl.LoadIdentity;
    rci.GLStates.Disable(stDepthTest);
    rci.GLStates.DepthWriteMask := False;

    // precalc coordinates
    vx := -Width * 0.5 * f;
    vx1 := vx + Width * f;
    vy := +Height * 0.5 * f;
    vy1 := vy - Height * f;

    // Texture coordinates
    if MirrorU then
    begin
      u0 := FXTiles;
      u1 := 0;
    end
    else
    begin
      u0 := 0;
      u1 := FXTiles;
    end;

    if MirrorV then
    begin
      v0 := FYTiles;
      v1 := 0;
    end
    else
    begin
      v0 := 0;
      v1 := FYTiles;
    end;

    // issue quad
    gl.Begin_(GL_QUADS);
    gl.Normal3fv(@YVector);
    xgl.TexCoord2f(u0, v0);
    gl.Vertex2f(vx, vy1);
    xgl.TexCoord2f(u1, v0);
    gl.Vertex2f(vx1, vy1);
    xgl.TexCoord2f(u1, v1);
    gl.Vertex2f(vx1, vy);
    xgl.TexCoord2f(u0, v1);
    gl.Vertex2f(vx, vy);
    gl.End_;

    // restore state
    gl.PopMatrix;
    gl.MatrixMode(GL_MODELVIEW);
    gl.PopMatrix;
  until not Material.UnApply(rci);
  if Count > 0 then
    Self.renderChildren(0, Count - 1, rci);
end;

function TGLHUDSprite.StoreHeight: Boolean;
begin
  Result := Abs(Height - 16) > 0.001;
end;


function TGLHUDSprite.StoreWidth: Boolean;
begin
  Result := Abs(Height - 16) > 0.001;
end;

// ------------------
// ------------------ TGLHUDText ------------------
// ------------------

constructor TGLHUDText.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FModulateColor := TGLColor.CreateInitialized(Self, clrWhite);
end;


destructor TGLHUDText.Destroy;
begin
  FModulateColor.Free;
  BitmapFont := nil;
  inherited;
end;

procedure TGLHUDText.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FBitmapFont) then
    BitmapFont := nil;
  inherited;
end;

procedure TGLHUDText.SetBitmapFont(const val: TGLCustomBitmapFont);
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

procedure TGLHUDText.SetText(const val: UnicodeString);
begin
  FText := val;
  StructureChanged;
end;

procedure TGLHUDText.SetRotation(const val: Single);
begin
  FRotation := val;
  StructureChanged;
end;

procedure TGLHUDText.SetAlignment(const val: TAlignment);
begin
  FAlignment := val;
  StructureChanged;
end;

procedure TGLHUDText.SetLayout(const val: TTextLayout);
begin
  FLayout := val;
  StructureChanged;
end;

procedure TGLHUDText.SetModulateColor(const val: TGLColor);
begin
  FModulateColor.Assign(val);
end;

procedure TGLHUDText.RenderTextAtPosition(const X, Y, Z: Single;
  var rci: TGLRenderContextInfo);
var
  f: Single;
begin
  if Assigned(FBitmapFont) and (Text <> '') then
  begin
    rci.GLStates.PolygonMode := pmFill;
    // Prepare matrices
    gl.MatrixMode(GL_MODELVIEW);
    gl.PushMatrix;
    gl.LoadMatrixf(@TGLSceneBuffer(rci.buffer).BaseProjectionMatrix);
    f := rci.renderDPI / 96;
    gl.Scalef(2 / rci.viewPortSize.cx, 2 / rci.viewPortSize.cy, 1);
    gl.Translatef(X * f - rci.viewPortSize.cx / 2, rci.viewPortSize.cy / 2 -
      Y * f, Z);
    if FRotation <> 0 then
      gl.Rotatef(FRotation, 0, 0, 1);
    gl.Scalef(Scale.DirectX * f, Scale.DirectY * f, 1);
    gl.MatrixMode(GL_PROJECTION);
    gl.PushMatrix;
    gl.LoadIdentity;
    rci.GLStates.Disable(stDepthTest);
    // render text
    FBitmapFont.RenderString(rci, Text, FAlignment, FLayout,
      FModulateColor.Color);
    // restore state
    rci.GLStates.Enable(stDepthTest);
    gl.PopMatrix;
    gl.MatrixMode(GL_MODELVIEW);
    gl.PopMatrix;
  end;
end;

procedure TGLHUDText.DoRender(var rci: TGLRenderContextInfo;
  renderSelf, renderChildren: Boolean);
begin
  RenderTextAtPosition(Position.X, Position.Y, Position.Z, rci);
  if Count > 0 then
    Self.renderChildren(0, Count - 1, rci);
end;

// ------------------
// ------------------ TGLResolutionIndependantHUDText ------------------
// ------------------

constructor TGLResolutionIndependantHUDText.Create(AOwner: TComponent);
begin
  inherited;
  Position.X := 0.5;
  Position.Y := 0.5;
end;

procedure TGLResolutionIndependantHUDText.DoRender(var rci: TGLRenderContextInfo;
  renderSelf, renderChildren: Boolean);
begin
  RenderTextAtPosition(Position.X * rci.viewPortSize.cx,
    Position.Y * rci.viewPortSize.cy, Position.Z, rci);
  if Count > 0 then
    Self.renderChildren(0, Count - 1, rci);
end;

// ------------------
// ------------------ TGLAbsoluteHUDText ------------------
// ------------------

procedure TGLAbsoluteHUDText.DoRender(var rci: TGLRenderContextInfo;
  renderSelf, renderChildren: Boolean);
var
  Temp: TAffineVector;
begin
  Temp := TGLSceneBuffer(rci.buffer).WorldToScreen(Self.AbsoluteAffinePosition);
  Temp.Y := rci.viewPortSize.cy - Temp.Y;
  RenderTextAtPosition(Temp.X, Temp.Y, Temp.Z, rci);
  if Count > 0 then
    Self.renderChildren(0, Count - 1, rci);
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

// class registrations
RegisterClasses([TGLHUDText, TGLHUDSprite, TGLResolutionIndependantHUDText,
  TGLAbsoluteHUDText]);

end.
