//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLSL.LineShaders;

(*
   A shader that renders hidden (back-faced) lines differently from visible
   (front) lines. Polygon offset is used to displace fragments depths a little
   so that there is no z-fighting in rendering the same geometry multiple times.
*)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  System.Classes,

  GLS.VectorTypes,
  GLS.Scene,
  GLS.Color,
  GLS.Material,
  GLS.BaseClasses,
  GLS.RenderContextInfo, 
  GLS.State, 
  GLS.Context;

type
  TGLLineSettings = class(TGLUpdateAbleObject)
  private
    FColor: TGLColor;
    FWidth: Single;
    FPattern: TGLushort;
    FForceMaterial: Boolean;
    procedure SetPattern(const value: TGLushort);
    procedure SetColor(const v: TGLColor);
    procedure SetWidth(const Value: Single);
    procedure SetForceMaterial(v: boolean);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Apply(var rci: TGLRenderContextInfo);
    procedure UnApply(var rci: TGLRenderContextInfo);
  published
    property Width: Single read FWidth write SetWidth;
    property Color: TGLColor read FColor write SetColor;
    property Pattern: TGLushort read FPattern write SetPattern default $FFFF;
    (* Set ForceMaterial to true to enforce the application of the line settings
       for objects that sets their own color, line width and pattern. *)
    property ForceMaterial: Boolean read FForceMaterial write SetForceMaterial
      default false;
  end;

  TGLHiddenLineShader = class(TGLShader)
  private
    FPassCount: integer;
    FLineSmooth: Boolean;
    FSolid: Boolean;
    FBackGroundColor: TGLColor;
    FFrontLine: TGLLineSettings;
    FBackLine: TGLLineSettings;
    FLighting: Boolean;
    FShadeModel: TGLShadeModel;
    procedure SetlineSmooth(v: boolean);
    procedure SetSolid(v: boolean);
    procedure SetBackgroundColor(AColor: TGLColor);
    procedure SetLighting(v: boolean);
    procedure SetShadeModel(const val: TGLShadeModel);
  protected
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FrontLine: TGLLineSettings read FFrontLine write FFrontLine;
    property BackLine: TGLLineSettings read FBackLine write FBackLine;
    // Line smoothing control
    property LineSmooth: Boolean read FlineSmooth write SetlineSmooth default
      false;
    // Solid controls if you can see through the front-line wireframe.
    property Solid: Boolean read FSolid write SetSolid default false;
    // Color used for solid fill.
    property BackgroundColor: TGLColor read FBackgroundColor write
      SetBackgroundColor;
    // When Solid is True, determines if lighting or background color is used.
    property SurfaceLit: Boolean read FLighting write SetLighting default true;
    // Shade model. Default is "Smooth".
    property ShadeModel: TGLShadeModel read FShadeModel write SetShadeModel
      default smDefault;
  end;

  TGLOutlineShader = class(TGLShader)
  private
    FPassCount: integer;
    FLineColor: TGLColor;
    FOutlineSmooth: Boolean;
    FOutlineWidth: Single;
    procedure SetOutlineWidth(v: single);
    procedure SetOutlineSmooth(v: boolean);
  protected
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LineColor: TGLColor read FLineColor write FLineColor;
    // Line smoothing control
    property LineSmooth: Boolean read FOutlineSmooth write SetOutlineSmooth
      default false;
    property LineWidth: Single read FOutlineWidth write SetOutlineWidth;
  end;


// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

uses
  GLS.TextureFormat;

// ------------------
// ------------------ TGLLineSettings ------------------
// ------------------


constructor TGLLineSettings.Create(AOwner: TPersistent);
begin
  inherited;
  FColor := TGLColor.Create(Self);
  FColor.Initialize(clrGray20);
  FWidth := 2;
  Pattern := $FFFF;
  ForceMaterial := false;
end;


destructor TGLLineSettings.Destroy;
begin
  FColor.Free;
  inherited;
end;

procedure TGLLineSettings.SetPattern(const value: TGLushort);
begin
  if FPattern <> value then
  begin
    FPattern := Value;
    NotifyChange(self);
  end;
end;

procedure TGLLineSettings.SetColor(const v: TGLColor);
begin
  FColor.Color := v.Color;
  NotifyChange(Self);
end;

procedure TGLLineSettings.SetWidth(const Value: Single);
begin
  FWidth := Value;
  NotifyChange(Self);
end;

var
  IgnoreMatSave: boolean;


procedure TGLLineSettings.Apply(var rci: TGLRenderContextInfo);
begin
  rci.GLStates.LineWidth := Width;
  gl.Color4fv(Color.AsAddress);
  if Pattern <> $FFFF then
  begin
    rci.GLStates.Enable(stLineStipple);
    rci.GLStates.LineStippleFactor := 1;
    rci.GLStates.LineStipplePattern := Pattern;
  end
  else
    rci.GLStates.Disable(stLineStipple);

  if ForceMaterial then
  begin
    IgnoreMatSave := rci.ignoreMaterials;
    rci.ignoreMaterials := true;
  end;
end;


procedure TGLLineSettings.UnApply(var rci: TGLRenderContextInfo);
begin
  if ForceMaterial then
    rci.ignoreMaterials := IgnoreMatSave;
end;


procedure TGLLineSettings.SetForceMaterial(v: boolean);
begin
  if FForceMaterial <> v then
  begin
    FForceMaterial := v;
    NotifyChange(self);
  end;
end;

// ------------------
// ------------------ TGLHiddenLineShader ------------------
// ------------------

constructor TGLHiddenLineShader.Create(AOwner: TComponent);
begin
  inherited;
  FFrontLine := TGLLineSettings.Create(self);
  FBackLine := TGLLineSettings.Create(self);
  FSolid := false;

  FBackgroundColor := TGLColor.Create(Self);
  FBackgroundColor.Initialize(clrBtnFace);

  FLineSmooth := False;
  FLighting := true;
  FShadeModel := smDefault;
end;

destructor TGLHiddenLineShader.Destroy;
begin
  FFrontLine.Free;
  FBackLine.Free;
  FBackgroundColor.Free;
  inherited;
end;


procedure TGLHiddenLineShader.DoApply(var rci: TGLRenderContextInfo; Sender:
  TObject);
begin
  FPassCount := 1;

  if solid then
    with rci.GLStates do
    begin
      // draw filled front faces in first pass
      PolygonMode := pmFill;
      CullFaceMode := cmBack;

      if FLighting then
      begin
        case ShadeModel of
          smDefault, smSmooth: gl.ShadeModel(GL_SMOOTH);
          smFlat: gl.ShadeModel(GL_FLAT);
        end
      end
      else
      begin
        Disable(stLighting);
        gl.Color4fv(FBackgroundColor.AsAddress); // use background color
      end;
      // enable and adjust polygon offset
      Enable(stPolygonOffsetFill);
    end
  else
    with rci.GLStates do
    begin
      Disable(stLighting);
      // draw back lines in first pass
      FBackLine.Apply(rci);
      CullFaceMode := cmFront;
      PolygonMode := pmLines;
      // enable and adjust polygon offset
      Enable(stPolygonOffsetLine);
    end;

  rci.GLStates.SetPolygonOffset(1, 2);
end;


function TGLHiddenLineShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;

  procedure SetLineSmoothBlend;
  begin
    with rci.GLStates do
    begin
      LineStippleFactor := 1;
      LineStipplePattern := $FFFF;
      if LineSmooth then
      begin
        LineSmoothHint := hintNicest;
        Enable(stLineSmooth);
      end
      else
        Disable(stLineSmooth);

      if LineSmooth or (FBackLine.FColor.Alpha < 1)
        or (FFrontLine.FColor.Alpha < 1) then
      begin
        Enable(stBlend);
        SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      end
      else
        Disable(stBlend);
    end;
  end;

begin
  case FPassCount of
    1:
      with rci.GLStates do begin
        // draw front line in 2nd pass
        FPassCount := 2;

        FBackLine.UnApply(rci);
        FFrontLine.Apply(rci);

        SetLineSmoothBlend;

        if solid and FLighting then
          Disable(stLighting);

        PolygonMode := pmLines;
        CullFaceMode := cmBack;

        if solid then
          rci.GLStates.Disable(stPolygonOffsetFill)
        else
          rci.GLStates.Disable(stPolygonOffsetLine);

        Result := True;
      end;
    2:
      begin
        FFrontLine.UnApply(rci);
        rci.GLStates.PolygonMode := pmFill;
        Result := false;
      end;
  else
    Assert(False);
    Result := False;
  end;
end;


procedure TGLHiddenLineShader.SetBackgroundColor(AColor: TGLColor);
begin
  FBackgroundColor.Color := AColor.Color;
  NotifyChange(Self);
end;


procedure TGLHiddenLineShader.SetlineSmooth(v: boolean);
begin
  if FlineSmooth <> v then
  begin
    FlineSmooth := v;
    NotifyChange(self);
  end;
end;


procedure TGLHiddenLineShader.SetLighting(v: boolean);
begin
  if FLighting <> v then
  begin
    FLighting := v;
    NotifyChange(self);
  end;
end;


procedure TGLHiddenLineShader.SetSolid(v: boolean);
begin
  if FSolid <> v then
  begin
    FSolid := v;
    NotifyChange(self);
  end;
end;


procedure TGLHiddenLineShader.SetShadeModel(const val: TGLShadeModel);
begin
  if FShadeModel <> val then
  begin
    FShadeModel := val;
    NotifyChange(Self);
  end;
end;


(*--------------------------------------
 TGLOutlineShader
 --------------------------------------*)

constructor TGLOutlineShader.Create(AOwner: TComponent);
begin
  inherited;
  FOutlineSmooth := False;
  FOutLineWidth := 2;
  FLineColor := TGLColor.CreateInitialized(Self, clrBlack);
  ShaderStyle := ssLowLevel;
end;

destructor TGLOutlineShader.Destroy;
begin
  FLineColor.Free;
  inherited;
end;

procedure TGLOutlineShader.DoApply(var rci: TGLRenderContextInfo; Sender:
  TObject);
begin
  // We first draw the object as usual in the first pass. This allows objects
  // with color alpha < 1 to be rendered correctly with outline.
  FPassCount := 1;
end;

function TGLOutlineShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  if rci.ignoreMaterials or (stStencilTest in rci.GLStates.States) then
  begin
    Result := False;
    Exit;
  end;
  case FPassCount of
    1:
      with rci.GLStates do
      begin
        // Now set up to draw the outline in the second pass

        Disable(stLighting);
        if FOutlineSmooth then
        begin
          LineSmoothHint := hintNicest;
          Enable(stLineSmooth);
        end
        else
          Disable(stLineSmooth);
        if FOutlineSmooth or (FlineColor.Alpha < 1) then
        begin
          Enable(stBlend);
          SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
        end
        else
          Disable(stBlend);
        gl.Color4fv(FlineColor.AsAddress);
        LineWidth := FOutlineWidth;
        Disable(stLineStipple);
        PolygonMode := pmLines;
        CullFaceMode := cmFront;
        DepthFunc := cfLEqual;
        ActiveTextureEnabled[ttTexture2D] := False;

        FPassCount := 2;
        Result := True; // go for next pass
      end;
    2:
      with rci.GLStates do
      begin
        // Restore settings
        PolygonMode := pmFill;
        CullFaceMode := cmBack;
        DepthFunc := cfLequal;
        Result := False; // we're done
      end;
  else
    Assert(False);
    Result := False;
  end;
end;

procedure TGLOutlineShader.SetOutlineWidth(v: single);
begin
  if FOutlineWidth <> v then
  begin
    FOutlineWidth := v;
    NotifyChange(self);
  end;
end;

procedure TGLOutlineShader.SetOutlineSmooth(v: boolean);
begin
  if FOutlineSmooth <> v then
  begin
    FOutlineSmooth := v;
    NotifyChange(self);
  end;
end;


end.

