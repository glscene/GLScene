//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.MultiMaterialShader;

(*
  A shader that applies a render pass for each material in
  its assigned MaterialLibrary.
*)

interface

uses
  System.Classes,

  GXS.Material,
  GXS.RenderContextInfo,
  GXS.State;

type
  TgxMultiMaterialShader = class(TgxShader)
  private
    FPass: Integer;
    FMaterialLibrary: TgxMaterialLibrary;
    FVisibleAtDesignTime: boolean;
    FShaderActiveAtDesignTime: boolean;
    FShaderStyle: TgxShaderStyle;
    procedure SetVisibleAtDesignTime(const Value: boolean);
    procedure SetShaderStyle(const Value: TgxShaderStyle);
  protected
    procedure SetMaterialLibrary(const val: TgxMaterialLibrary);
    procedure DoApply(var rci: TgxRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): boolean; override;
  public
    constructor Create(aOwner: TComponent); override;
  published
    property MaterialLibrary: TgxMaterialLibrary read FMaterialLibrary
      write SetMaterialLibrary;
    property VisibleAtDesignTime: boolean read FVisibleAtDesignTime
      write SetVisibleAtDesignTime;
    property ShaderStyle: TgxShaderStyle read FShaderStyle write SetShaderStyle;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TgxMultiMaterialShader ------------------
// ------------------

constructor TgxMultiMaterialShader.Create(aOwner: TComponent);
begin
  inherited;
  FShaderStyle := ssReplace;
  FVisibleAtDesignTime := False;
end;

procedure TgxMultiMaterialShader.DoApply(var rci: TgxRenderContextInfo;
  Sender: TObject);
begin
  if not Assigned(FMaterialLibrary) then
    exit;
  FShaderActiveAtDesignTime := FVisibleAtDesignTime;
  FPass := 1;
  if (not(csDesigning in ComponentState)) or FShaderActiveAtDesignTime then
  begin
    rci.ignoreDepthRequests := True;
    rci.gxStates.Enable(stDepthTest);
    rci.gxStates.DepthFunc := cfLEqual;
    if FMaterialLibrary.Materials.Count > 0 then
      FMaterialLibrary.Materials[0].Apply(rci);
    rci.ignoreDepthRequests := False;
  end;
end;

function TgxMultiMaterialShader.DoUnApply
  (var rci: TgxRenderContextInfo): boolean;
begin
  Result := False;
  if not Assigned(FMaterialLibrary) then
    exit;
  if (not(csDesigning in ComponentState)) or FShaderActiveAtDesignTime then
  begin
    if FMaterialLibrary.Materials.Count > 0 then
      // handle multi-pass materials
      if FMaterialLibrary.Materials[FPass - 1].UnApply(rci) then
      begin
        Result := True;
        exit;
      end;
    if (FPass >= FMaterialLibrary.Materials.Count) then
    begin
      rci.gxStates.DepthFunc := cfLess;
      exit;
    end;
    FMaterialLibrary.Materials[FPass].Apply(rci);
    Result := True;
    Inc(FPass);
  end;
end;

procedure TgxMultiMaterialShader.SetMaterialLibrary
  (const val: TgxMaterialLibrary);
begin
  if val <> FMaterialLibrary then
  begin
    FMaterialLibrary := val;
    NotifyChange(Self);
  end;
end;

procedure TgxMultiMaterialShader.SetShaderStyle(const Value: TgxShaderStyle);
begin
  FShaderStyle := Value;
  inherited ShaderStyle := FShaderStyle;
end;

procedure TgxMultiMaterialShader.SetVisibleAtDesignTime(const Value: boolean);
begin
  FVisibleAtDesignTime := Value;
  if csDesigning in ComponentState then
    NotifyChange(Self);
end;

end.
