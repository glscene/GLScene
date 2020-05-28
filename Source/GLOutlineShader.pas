//
// This unit is part of the GLScene Engine, http://glscene.org
//
{
   A simple shader that adds an outline to an object.  
   Limitations: 
       1. Object can be transparent (color alpha < 1) if it doesn't
                   overlap itself. Texture transparency doesn't work.
       2. Doesn't work with objects (e.g. TGLFreeForm) having it's own
                   color array.
       3. Doesn't Works with visible backfaces. 

}
unit GLOutlineShader;

interface

{$I GLScene.inc}

uses
  System.Classes, 
  
  OpenGLTokens,
  GLMaterial,
  GLContext,
  GLColor,
  GLState,
  GLRenderContextInfo;

type

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
    {Line smoothing control }
    property LineSmooth: Boolean read FOutlineSmooth write SetOutlineSmooth
      default false;
    property LineWidth: Single read FOutlineWidth write SetOutlineWidth;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

uses 
  GLTextureFormat;

// ------------------
// ------------------ TGLOutlineShader ------------------
// ------------------

 
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

