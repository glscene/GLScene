//
// The graphics engine GXScene https://github.com/glscene
//
unit GXSL.PostEffects;

(* A collection of components that generate post effects *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,

  GXS.PersistentClasses,
  GXS.VectorGeometry,
  GXS.Strings,
  GXS.Scene,
  GXS.Texture,
  GXS.Graphics,
  GXSL.CustomShader,
  GXS.Context,
  GXS.RenderContextInfo,
  GXS.Material,
  GXS.TextureFormat;

type
  EGLPostShaderHolderException = class(Exception);
  TgxPostShaderHolder = class;

  TgxPostShaderCollectionItem = class(TCollectionItem)
  private
    FShader: TgxShader;
    FPostShaderInterface: IgxPostShader;
    procedure SetShader(const Value: TgxShader);
  protected
    function GetRealOwner: TgxPostShaderHolder;
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Shader: TgxShader read FShader write SetShader;
  end;

  TgxPostShaderCollection = class(TOwnedCollection)
  private
    function GetItems(const Index: Integer): TgxPostShaderCollectionItem;
    procedure SetItems(const Index: Integer;
      const Value: TgxPostShaderCollectionItem);
  public
    procedure Remove(const Item: TgxShader);
    function Add: TgxPostShaderCollectionItem;

    property Items[const Index: Integer]: TgxPostShaderCollectionItem read GetItems write SetItems; default;
  end;

  (* A class that allows several post-shaders to be applied to the scene,
    one after another. It does not provide any optimizations related to
    multi-shader rendering, just a convenient interface. *)
  TgxPostShaderHolder = class(TgxBaseSCeneObject)
  private
    FShaders: TgxPostShaderCollection;
    FTempTexture: TgxTextureHandle;
    FPreviousViewportSize: TgxSize;
    FTempTextureTarget: TgxTextureTarget;
    procedure SetShaders(const Value: TgxPostShaderCollection);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DoRender(var rci : TgxRenderContextInfo;
                       renderSelf, renderChildren : Boolean); override;
  published
    property TempTextureTarget: TgxTextureTarget read FTempTextureTarget write FTempTextureTarget default ttTexture2d;
    property Shaders: TgxPostShaderCollection read FShaders write SetShaders;

    // Publish some stuff from TgxBaseSceneObject.
    property Visible;
    property OnProgress;
  end;


  TgxPostEffectColor = record
    R, G, B, A: GLubyte;
  end;

  TgxPostEffectBuffer = array of TgxPostEffectColor;

  TgxOnCustomPostEffectEvent = procedure(Sender: TObject; var rci : TgxRenderContextInfo; var Buffer: TgxPostEffectBuffer) of object;

  (* Some presets for TgxPostEffect:
       pepNone - does nothing.
       pepGray - makes picture gray.
       pepNegative - inverts all colors.
       pepDistort - simulates shaky TV image.
       pepNoise - just adds random niose.
       pepNightVision - simulates nightvision goggles.
       pepBlur - blurs the scene.
       pepCustom - calls the OnCustomEffect event. *)
  TgxPostEffectPreset = (pepNone, pepGray, pepNegative, pepDistort, pepNoise,
                         pepNightVision, pepBlur, pepCustom);

  (* Provides a simple way to producing post-effects without shaders.
     It is slow as hell, but it's worth it in some cases.*)
  TgxPostEffect = class(TgxBaseSCeneObject)
  private
    FOnCustomEffect: TgxOnCustomPostEffectEvent;
    FPreset: TgxPostEffectPreset;
    FRenderBuffer: TgxPostEffectBuffer;
  protected
    // May be should be private...
    procedure MakeGrayEffect; virtual;
    procedure MakeNegativeEffect; virtual;
    procedure MakeDistortEffect; virtual;
    procedure MakeNoiseEffect; virtual;
    procedure MakeNightVisionEffect; virtual;
    procedure MakeBlurEffect(var rci : TgxRenderContextInfo); virtual;
    procedure DoOnCustomEffect(var rci : TgxRenderContextInfo; var Buffer: TgxPostEffectBuffer); virtual;
  public
    procedure DoRender(var rci : TgxRenderContextInfo;
                       renderSelf, renderChildren : Boolean); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Preset: TgxPostEffectPreset read FPreset write FPreset default pepNone;
    // User creates this effect.
    property OnCustomEffect: TgxOnCustomPostEffectEvent read FOnCustomEffect write FOnCustomEffect;
    // Publish some stuff from TgxBaseSCeneObject.
    property Visible;
    property OnProgress;
  end;

//-----------------------------------------------------------------------------
implementation
//-----------------------------------------------------------------------------

//-------------------------------
// TgxPostEffect
//-------------------------------

procedure TgxPostEffect.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TgxPostEffect then
  begin
    FPreset := TgxPostEffect(Source).FPreset;
  end;
end;

procedure TgxPostEffect.DoOnCustomEffect(
  var rci : TgxRenderContextInfo; var Buffer: TgxPostEffectBuffer);
begin
  if Assigned(FOnCustomEffect) then
    FOnCustomEffect(Self, rci, Buffer);
end;

procedure TgxPostEffect.DoRender(var rci : TgxRenderContextInfo;
                                      renderSelf, renderChildren : Boolean);
var
  NewScreenSize: Integer;
begin
  if (not rci.ignoreMaterials) and (FPreset <> pepNone) and (rci.drawState <> dsPicking) then
  begin
    NewScreenSize := rci.viewPortSize.cx * rci.viewPortSize.cy;
    if NewScreenSize <> Length(FRenderBuffer) then
      SetLength(FRenderBuffer, NewScreenSize);

    glReadPixels(0, 0, rci.viewPortSize.cx, rci.viewPortSize.cy, GL_RGBA, GL_UNSIGNED_BYTE, FRenderBuffer);
    case FPreset of
      // pepNone is handled in the first line.
      pepGray:        MakeGrayEffect;
      pepNegative:    MakeNegativeEffect;
      pepDistort:     MakeDistortEffect;
      pepNoise:       MakeNoiseEffect;
      pepNightVision: MakeNightVisionEffect;
      pepBlur:        MakeBlurEffect(rci);
      pepCustom:      DoOnCustomEffect(rci, FRenderBuffer);
    else
      Assert(False, strErrorEx + strUnknownType);
    end;
    glDrawPixels(rci.viewPortSize.cx, rci.viewPortSize.cy, GL_RGBA, GL_UNSIGNED_BYTE, FRenderBuffer);
  end;

  // Start rendering children (if any).
  if renderChildren then
    Self.RenderChildren(0, Count - 1, rci);
end;

procedure TgxPostEffect.MakeGrayEffect;
var
  I:    Longword;
  gray: GLubyte;
begin
  for I := 0 to High(FRenderBuffer) do
  begin
    gray := Round((0.30 * FRenderBuffer[I].r) +
                  (0.59 * FRenderBuffer[I].g) +
                  (0.11 * FRenderBuffer[I].b));
    FRenderBuffer[I].r := gray;
    FRenderBuffer[I].g := gray;
    FRenderBuffer[I].b := gray;
  end;
end;

procedure TgxPostEffect.MakeNegativeEffect;
var
  I: Longword;
begin
  for I := 0 to High(FRenderBuffer) do
  begin
    FRenderBuffer[I].r := 255 - FRenderBuffer[I].r;
    FRenderBuffer[I].g := 255 - FRenderBuffer[I].g;
    FRenderBuffer[I].b := 255 - FRenderBuffer[I].b;
  end;
end;

procedure TgxPostEffect.MakeDistortEffect;
var
  I: Integer;
  lMaxLength: Integer;
  lNewIndex: Integer;
begin
  lMaxLength := High(FRenderBuffer);

  for I := 0 to lMaxLength do
  begin
    lNewIndex := MaxInteger(0, MinInteger(lMaxLength, I + Random(10) - 5));
    FRenderBuffer[I].r := FRenderBuffer[lNewIndex].r;
    FRenderBuffer[I].g := FRenderBuffer[lNewIndex].g;
    FRenderBuffer[I].b := FRenderBuffer[lNewIndex].b;
  end;
end;

procedure TgxPostEffect.MakeNoiseEffect;
var
  I:   Longword;
  rnd: Single;
begin
  for I := 0 to High(FRenderBuffer) do
  begin
    rnd := 0.25 + Random(75)/100;

    FRenderBuffer[I].r := Round(FRenderBuffer[I].r * rnd);
    FRenderBuffer[I].g := Round(FRenderBuffer[I].g * rnd);
    FRenderBuffer[I].b := Round(FRenderBuffer[I].b * rnd);
  end;
end;

procedure TgxPostEffect.MakeNightVisionEffect;
var
   gray: Single;
   I: Integer;
   lNewIndex, lMaxLength: Integer;
begin
  lMaxLength := High(FRenderBuffer);

  for I := 0 to lMaxLength do
  begin
    lNewIndex := MaxInteger(0, MinInteger(lMaxLength, I + Random(20) - 10));

    gray := 60 + (0.30 * FRenderBuffer[lNewIndex].r) +
                 (0.59 * FRenderBuffer[lNewIndex].g) +
                 (0.11 * FRenderBuffer[lNewIndex].b);

    FRenderBuffer[I].r := Round(gray * 0.25);
    FRenderBuffer[I].g := Round((gray + 4) * 0.6);
    FRenderBuffer[I].b := Round((gray + 4) * 0.11);
  end;
end;

procedure TgxPostEffect.MakeBlurEffect(var rci : TgxRenderContextInfo);
const
  lOffset: Integer = 2;
var
  I: Integer;
  lUp: Integer;
begin
  lUp := rci.viewPortSize.cx * lOffset;
  for I := lUp to High(FRenderBuffer) - lUp do
  begin
    FRenderBuffer[I].r := (FRenderBuffer[I].r + FRenderBuffer[I - lOffset].r +
        FRenderBuffer[I + lOffset].r + FRenderBuffer[I - lUp].r +
        FRenderBuffer[I + lUp].r) div 5;
    FRenderBuffer[I].g := (FRenderBuffer[I].g + FRenderBuffer[I - lOffset].g +
        FRenderBuffer[I + lOffset].g + FRenderBuffer[I - lUp].g +
        FRenderBuffer[I + lUp].r) div 5;
    FRenderBuffer[I].b := (FRenderBuffer[I].b + FRenderBuffer[I - lOffset].b +
        FRenderBuffer[I + lOffset].b + FRenderBuffer[I - lUp].g +
        FRenderBuffer[I + lUp].r) div 5;
  end;
end;

//-------------------------------
// TgxPostShaderCollectionItem
//-------------------------------

procedure TgxPostShaderCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TgxPostShaderCollectionItem then
  begin
    SetShader(TgxPostShaderCollectionItem(Source).FShader);
  end
  else
    inherited; // Die!!!
end;

function TgxPostShaderCollectionItem.GetDisplayName: string;
begin
  if FShader = nil then
    Result := ''
  else
  begin
    if FShader.Name <> '' then
      Result := FShader.Name
    else
      Result := FShader.ClassName;
  end;
end;

type
  // Required for Delphi5 compatibility.
  THackCollection = class(TOwnedCollection)end;

function TgxPostShaderCollectionItem.GetRealOwner: TgxPostShaderHolder;
begin
  if Collection = nil then
    Result := nil
  else
    Result := TgxPostShaderHolder(THackCollection(Collection).GetOwner);
end;

procedure TgxPostShaderCollectionItem.SetShader(const Value: TgxShader);
var
  RealOwner: TgxPostShaderHolder;
begin
  if FShader = Value then Exit;
  RealOwner := GetRealOwner;

  if FShader <> nil then
      FShader.RemoveFreeNotification(RealOwner);

  if not Supports(TObject(Value), IgxPostShader, FPostShaderInterface) then
    raise EGLPostShaderHolderException.Create('Shader must support interface IPostShader!');

  if RealOwner <> nil then
    if FPostShaderInterface.GetTextureTarget <> RealOwner.TempTextureTarget then
      raise EGLPostShaderHolderException.Create(strErrorEx + 'TextureTarget is not compatible!');
  // If RealOwner = nil, we ignore this case and hope it will turn out ok...

  FShader := Value;

  if FShader <> nil then
    if RealOwner <> nil then
      FShader.FreeNotification(RealOwner);
end;

//-------------------------------
// TgxPostShaderHolder
//-------------------------------

procedure TgxPostShaderHolder.Assign(Source: TPersistent);
begin
  if Source is TgxPostShaderHolder then
  begin
    FShaders.Assign(TgxPostShaderHolder(Source).FShaders);
    FTempTextureTarget := TgxPostShaderHolder(Source).FTempTextureTarget;
  end;
  inherited;
end;

constructor TgxPostShaderHolder.Create(Owner: TComponent);
begin
  inherited;
  FTempTexture := TgxTextureHandle.Create;
  FTempTextureTarget :=ttTexture2D;
  FShaders := TgxPostShaderCollection.Create(Self, TgxPostShaderCollectionItem);
end;

destructor TgxPostShaderHolder.Destroy;
begin
  FShaders.Destroy;
  FTempTexture.Destroy;
  inherited;
end;

procedure TgxPostShaderHolder.DoRender(var rci: TgxRenderContextInfo;
  renderSelf, renderChildren: Boolean);
var
  I: Integer;
begin
  if not (rci.ignoreMaterials) and not (csDesigning in ComponentState) and
         (rci.drawState <> dsPicking) then
  begin
    if (FPreviousViewportSize.cx <> rci.viewPortSize.cx) or
       (FPreviousViewportSize.cy <> rci.viewPortSize.cy) then
    begin
      InitTexture(FTempTexture.Handle, rci.viewPortSize,
        FTempTextureTarget);
      FPreviousViewportSize := rci.viewPortSize;
    end;

    if FShaders.Count <> 0 then
    begin
      for I := 0 to FShaders.Count - 1 do
      begin
        Assert(Assigned(FShaders[I].FShader));
        if FShaders[I].FShader.Enabled then
        begin
          rci.gxStates.ActiveTextureEnabled[FTempTextureTarget] := True;
          FShaders[I].FShader.Apply(rci, Self);
          repeat
            CopyScreenToTexture(rci.viewPortSize, DecodeTextureTarget(FTempTextureTarget));
            FShaders[I].FPostShaderInterface.DoUseTempTexture(FTempTexture, FTempTextureTarget);
            DrawTexturedScreenQuad5(rci.viewPortSize);
          until not FShaders[I].FShader.UnApply(rci);
          rci.gxStates.ActiveTextureEnabled[FTempTextureTarget] := False;
        end;
      end;
    end;
  end;
  if renderChildren then
    Self.RenderChildren(0, Count - 1, rci);
end;

procedure TgxPostShaderHolder.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent is TgxShader then
      FShaders.Remove(TgxShader(AComponent));
  end;
end;

procedure TgxPostShaderHolder.SetShaders(
  const Value: TgxPostShaderCollection);
begin
  FShaders.Assign(Value);
end;

//-------------------------------
// TgxPostShaderCollection
//-------------------------------

function TgxPostShaderCollection.Add: TgxPostShaderCollectionItem;
begin
  Result := TgxPostShaderCollectionItem(inherited Add);
end;

function TgxPostShaderCollection.GetItems(
  const Index: Integer): TgxPostShaderCollectionItem;
begin
  Result := TgxPostShaderCollectionItem(GetItem(Index));
end;

procedure TgxPostShaderCollection.Remove(
  const Item: TgxShader);
var
  I: Integer;
begin
  if Count <> 0 then
    for I := Count - 1 downto 0 do
      if GetItems(I).FShader = Item then
        Delete(I);
  // Don't exit because the same shader might be applied more than once.
end;

procedure TgxPostShaderCollection.SetItems(const Index: Integer;
  const Value: TgxPostShaderCollectionItem);
begin
  GetItems(Index).Assign(Value);
end;

//---------------------------------------------------------
initialization
//---------------------------------------------------------

  RegisterClasses([TgxPostEffect, TgxPostShaderHolder,
                   TgxPostShaderCollection, TgxPostShaderCollectionItem]);

end.
