//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.HiddenLineShader;

(*
   A shader that renders hidden (back-faced) lines differently from visible
   (front) lines. Polygon offset is used to displace fragments depths a little
   so that there is no z-fighting in rendering the same geometry multiple times.
*)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,

  System.Classes,

  GXS.Material,
  GXS.Scene,
  GXS.Color,
  GXS.BaseClasses, 
  GXS.RenderContextInfo, 
  GXS.State, 
  GXS.Context;

type
  TgxLineSettings = class(TgxUpdateAbleObject)
  private
    FColor: TgxColor;
    FWidth: Single;
    FPattern: GLushort;
    FForceMaterial: Boolean;
    procedure SetPattern(const value: GLushort);
    procedure SetColor(const v: TgxColor);
    procedure SetWidth(const Value: Single);
    procedure SetForceMaterial(v: boolean);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Apply(var rci: TgxRenderContextInfo);
    procedure UnApply(var rci: TgxRenderContextInfo);
  published
    property Width: Single read FWidth write SetWidth;
    property Color: TgxColor read FColor write SetColor;
    property Pattern: GLushort read FPattern write SetPattern default $FFFF;
    (* Set ForceMaterial to true to enforce the application of the line settings
       for objects that sets their own color, line width and pattern. *)
    property ForceMaterial: Boolean read FForceMaterial write SetForceMaterial
      default false;
  end;

  TgxHiddenLineShader = class(TgxShader)
  private
    FPassCount: integer;
    FLineSmooth: Boolean;
    FSolid: Boolean;
    FBackGroundColor: TgxColor;
    FFrontLine: TgxLineSettings;
    FBackLine: TgxLineSettings;
    FLighting: Boolean;
    FShadeModel: TgxShadeModel;
    procedure SetlineSmooth(v: boolean);
    procedure SetSolid(v: boolean);
    procedure SetBackgroundColor(AColor: TgxColor);
    procedure SetLighting(v: boolean);
    procedure SetShadeModel(const val: TgxShadeModel);
  protected
    procedure DoApply(var rci: TgxRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FrontLine: TgxLineSettings read FFrontLine write FFrontLine;
    property BackLine: TgxLineSettings read FBackLine write FBackLine;
    // Line smoothing control
    property LineSmooth: Boolean read FlineSmooth write SetlineSmooth default
      false;
    // Solid controls if you can see through the front-line wireframe.
    property Solid: Boolean read FSolid write SetSolid default false;
    // Color used for solid fill.
    property BackgroundColor: TgxColor read FBackgroundColor write
      SetBackgroundColor;
    // When Solid is True, determines if lighting or background color is used.
    property SurfaceLit: Boolean read FLighting write SetLighting default true;
    // Shade model. Default is "Smooth".
    property ShadeModel: TgxShadeModel read FShadeModel write SetShadeModel
      default smDefault;
  end;

// ------------------------------------------------------------------
implementation
// ------------------

// ------------------ TgxLineSettings ------------------
// ------------------

constructor TgxLineSettings.Create(AOwner: TPersistent);
begin
  inherited;
  FColor := TgxColor.Create(Self);
  FColor.Initialize(clrGray20);
  FWidth := 2;
  Pattern := $FFFF;
  ForceMaterial := false;
end;

destructor TgxLineSettings.Destroy;
begin
  FColor.Free;
  inherited;
end;

procedure TgxLineSettings.SetPattern(const value: GLushort);
begin
  if FPattern <> value then
  begin
    FPattern := Value;
    NotifyChange(self);
  end;
end;

procedure TgxLineSettings.SetColor(const v: TgxColor);
begin
  FColor.Color := v.Color;
  NotifyChange(Self);
end;

procedure TgxLineSettings.SetWidth(const Value: Single);
begin
  FWidth := Value;
  NotifyChange(Self);
end;

var
  IgnoreMatSave: boolean;

procedure TgxLineSettings.Apply(var rci: TgxRenderContextInfo);
begin
  rci.gxStates.LineWidth := Width;
  glColor4fv(Color.AsAddress);
  if Pattern <> $FFFF then
  begin
    rci.gxStates.Enable(stLineStipple);
    rci.gxStates.LineStippleFactor := 1;
    rci.gxStates.LineStipplePattern := Pattern;
  end
  else
    rci.gxStates.Disable(stLineStipple);

  if ForceMaterial then
  begin
    IgnoreMatSave := rci.ignoreMaterials;
    rci.ignoreMaterials := true;
  end;
end;

procedure TgxLineSettings.UnApply(var rci: TgxRenderContextInfo);
begin
  if ForceMaterial then
    rci.ignoreMaterials := IgnoreMatSave;
end;

procedure TgxLineSettings.SetForceMaterial(v: boolean);
begin
  if FForceMaterial <> v then
  begin
    FForceMaterial := v;
    NotifyChange(self);
  end;
end;

// ------------------
// ------------------ TgxHiddenLineShader ------------------
// ------------------

constructor TgxHiddenLineShader.Create(AOwner: TComponent);
begin
  inherited;
  FFrontLine := TgxLineSettings.Create(self);
  FBackLine := TgxLineSettings.Create(self);
  FSolid := false;

  FBackgroundColor := TgxColor.Create(Self);
  FBackgroundColor.Initialize(clrBtnFace);

  FLineSmooth := False;
  FLighting := true;
  FShadeModel := smDefault;
end;

destructor TgxHiddenLineShader.Destroy;
begin
  FFrontLine.Free;
  FBackLine.Free;
  FBackgroundColor.Free;
  inherited;
end;

procedure TgxHiddenLineShader.DoApply(var rci: TgxRenderContextInfo; Sender:
  TObject);
begin
  FPassCount := 1;

  if solid then
    with rci.gxStates do
    begin
      // draw filled front faces in first pass
      PolygonMode := pmFill;
      CullFaceMode := cmBack;

      if FLighting then
      begin
        case ShadeModel of
          smDefault, smSmooth: glShadeModel(GL_SMOOTH);
          smFlat: glShadeModel(GL_FLAT);
        end
      end
      else
      begin
        Disable(stLighting);
        glColor4fv(FBackgroundColor.AsAddress); // use background color
      end;
      // enable and adjust polygon offset
      Enable(stPolygonOffsetFill);
    end
  else
    with rci.gxStates do
    begin
      Disable(stLighting);
      // draw back lines in first pass
      FBackLine.Apply(rci);
      CullFaceMode := cmFront;
      PolygonMode := pmLines;
      // enable and adjust polygon offset
      Enable(stPolygonOffsetLine);
    end;

  rci.gxStates.SetPolygonOffset(1, 2);
end;

function TgxHiddenLineShader.DoUnApply(var rci: TgxRenderContextInfo): Boolean;

  procedure SetLineSmoothBlend;
  begin
    with rci.gxStates do
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
      with rci.gxStates do begin
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
          rci.gxStates.Disable(stPolygonOffsetFill)
        else
          rci.gxStates.Disable(stPolygonOffsetLine);

        Result := True;
      end;
    2:
      begin
        FFrontLine.UnApply(rci);
        rci.gxStates.PolygonMode := pmFill;
        Result := false;
      end;
  else
    Assert(False);
    Result := False;
  end;
end;

procedure TgxHiddenLineShader.SetBackgroundColor(AColor: TgxColor);
begin
  FBackgroundColor.Color := AColor.Color;
  NotifyChange(Self);
end;

procedure TgxHiddenLineShader.SetlineSmooth(v: boolean);
begin
  if FlineSmooth <> v then
  begin
    FlineSmooth := v;
    NotifyChange(self);
  end;
end;

procedure TgxHiddenLineShader.SetLighting(v: boolean);
begin
  if FLighting <> v then
  begin
    FLighting := v;
    NotifyChange(self);
  end;
end;

procedure TgxHiddenLineShader.SetSolid(v: boolean);
begin
  if FSolid <> v then
  begin
    FSolid := v;
    NotifyChange(self);
  end;
end;

procedure TgxHiddenLineShader.SetShadeModel(const val: TgxShadeModel);
begin
  if FShadeModel <> val then
  begin
    FShadeModel := val;
    NotifyChange(Self);
  end;
end;

end.

