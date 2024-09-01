//
// The graphics engine GXScene https://github.com/glscene
//
unit GXSL.TextureShaders;

(*
    This shader allows to apply multiple textures, gathering them from existing materials.
    This allows saving resources, since you can reference the textures of any material in
    any materialLibrary.
    Note that actually the component references a Material (not a texture) but
    it uses that material's texture. The referenced material settings will be ignored,
    but the texture's settings (like TextureMode, ImageGamma, ImageBrightness) will be used.
    Instead the local material settings (listed in the collection) will be used.
*)

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,

  System.Classes,
  System.SysUtils,

  GXS.Scene,
  GXS.Context,
  GXS.Texture,
  GXS.VectorTypes,
  GXS.XOpenGL,

  GXS.VectorGeometry,
  GXS.Color,
  GXS.Material,
  GXS.Strings,
  GXS.VectorFileObjects,
  GXS.State,
  GXS.PersistentClasses,
  GXS.Coordinates,
  GXS.TextureCombiners,
  GXS.RenderContextInfo,
  GXS.Utils;

type
  TgxShaderTextureSharing = class;

  TgxShaderTextureSharingMaterial = class(TgxInterfacedCollectionItem, IgxMaterialLibrarySupported)
  private
    FTextureMatrix: TMatrix4f;
    FNeedToUpdateTextureMatrix: Boolean;
    FTextureMatrixIsUnitary: Boolean;
    FLibMaterial: TgxLibMaterial;
    FTexOffset: TgxCoordinates2;
    FTexScale: TgxCoordinates2;
    FBlendingMode: TgxBlendingMode;
    FSpecular: TgxColor;
    FAmbient: TgxColor;
    FDiffuse: TgxColor;
    FEmission: TgxColor;
    FShininess: TgxShininess;
    FMaterialLibrary: TgxMaterialLibrary;
    FLibMaterialName: TgxLibMaterialName;
    procedure SetAmbient(const Value: TgxColor);
    procedure SetDiffuse(const Value: TgxColor);
    procedure SetEmission(const Value: TgxColor);
    procedure SetShininess(const Value: TgxShininess);
    procedure SetSpecular(const Value: TgxColor);
    procedure SetMaterialLibrary(const Value: TgxMaterialLibrary);
    procedure SetLibMaterialName(const Value: TgxLibMaterialName);
    procedure SetBlendingMode(const Value: TgxBlendingMode);
    procedure SetLibMaterial(const Value: TgxLibMaterial);
    procedure SetTexOffset(const Value: TgxCoordinates2);
    procedure SetTexScale(const Value: TgxCoordinates2);
    function GetTextureMatrix: TMatrix4f;
    function GetTextureMatrixIsUnitary: Boolean;
  protected
    procedure coordNotifychange(Sender: TObject);
    procedure OtherNotifychange(Sender: TObject);
    function GetDisplayName: string; override;
    function GetTextureSharingShader: TgxShaderTextureSharing;
    // Implementing IgxMaterialLibrarySupported.
    function GetMaterialLibrary: TgxAbstractMaterialLibrary; virtual;
  public
    procedure Apply(var rci: TgxRenderContextInfo);
    procedure UnApply(var rci: TgxRenderContextInfo);
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property LibMaterial: TgxLibMaterial read FLibMaterial write SetLibMaterial;
    property TextureMatrix: TMatrix4f read GetTextureMatrix;
    property TextureMatrixIsUnitary: Boolean read GetTextureMatrixIsUnitary;
  published
    property TexOffset: TgxCoordinates2 read FTexOffset write SetTexOffset;
    property TexScale: TgxCoordinates2 read FTexScale write SetTexScale;
    property BlendingMode: TgxBlendingMode read FBlendingMode write SetBlendingMode;
    property Emission: TgxColor read FEmission write SetEmission;
    property Ambient: TgxColor read FAmbient write SetAmbient;
    property Diffuse: TgxColor read FDiffuse write SetDiffuse;
    property Specular: TgxColor read FSpecular write SetSpecular;
    property Shininess: TgxShininess read FShininess write SetShininess;
    property MaterialLibrary: TgxMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    property LibMaterialName: TgxLibMaterialName read FLibMaterialName write SetLibMaterialName;
  end;

  TgxShaderTextureSharingMaterials = class(TOwnedCollection)
  protected
    function GetItems(const AIndex: Integer): TgxShaderTextureSharingMaterial;
    procedure SetItems(const AIndex: Integer; const Value: TgxShaderTextureSharingMaterial);
    function GetParent: TgxShaderTextureSharing;
  public
    function Add: TgxShaderTextureSharingMaterial;
    constructor Create(AOwner: TgxShaderTextureSharing);
    property Items[const AIndex: Integer]: TgxShaderTextureSharingMaterial read GetItems write SetItems; default;
  end;

  TgxShaderTextureSharing = class(TgxShader)
  private
    FMaterials: TgxShaderTextureSharingMaterials;
    FCurrentPass: Integer;
    procedure SetMaterials(const Value: TgxShaderTextureSharingMaterials);
  protected
    procedure DoApply(var rci: TgxRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddLibMaterial(const ALibMaterial: TgxLibMaterial): TgxShaderTextureSharingMaterial;
    function FindLibMaterial(const ALibMaterial: TgxLibMaterial): TgxShaderTextureSharingMaterial;
  published
    property Materials: TgxShaderTextureSharingMaterials read FMaterials write SetMaterials;
  end;

  // A shader that can setup the texture combiner.
  TgxTexCombineShader = class(TgxShader)
  private
    FCombiners: TStringList;
    FCommandCache: TgxCombinerCache;
    FCombinerIsValid: Boolean; // to avoid reparsing invalid stuff
    FDesignTimeEnabled: Boolean;
    FMaterialLibrary: TgxMaterialLibrary;
    FLibMaterial3Name: TgxLibMaterialName;
    CurrentLibMaterial3: TgxLibMaterial;
    FLibMaterial4Name: TgxLibMaterialName;
    CurrentLibMaterial4: TgxLibMaterial;
    FApplied3, FApplied4: Boolean;
  protected
    procedure SetCombiners(const val: TStringList);
    procedure SetDesignTimeEnabled(const val: Boolean);
    procedure SetMaterialLibrary(const val: TgxMaterialLibrary);
    procedure SetLibMaterial3Name(const val: TgxLibMaterialName);
    procedure SetLibMaterial4Name(const val: TgxLibMaterialName);
    procedure NotifyLibMaterial3Destruction;
    procedure NotifyLibMaterial4Destruction;
    procedure DoInitialize(var rci: TgxRenderContextInfo; Sender: TObject); override;
    procedure DoApply(var rci: TgxRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TgxRenderContextInfo): Boolean; override;
    procedure DoFinalize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NotifyChange(Sender: TObject); override;
  published
    property Combiners: TStringList read FCombiners write SetCombiners;
    property DesignTimeEnabled: Boolean read FDesignTimeEnabled write SetDesignTimeEnabled;
    property MaterialLibrary: TgxMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    property LibMaterial3Name: TgxLibMaterialName read FLibMaterial3Name write SetLibMaterial3Name;
    property LibMaterial4Name: TgxLibMaterialName read FLibMaterial4Name write SetLibMaterial4Name;
  end;

//=======================================================================
implementation
//=======================================================================

//----------------------------------------------------------------------------
// TgxShaderTextureSharingMaterial
//----------------------------------------------------------------------------

procedure TgxShaderTextureSharingMaterial.Apply(var rci: TgxRenderContextInfo);
begin
  if not Assigned(FLibMaterial) then
    Exit;
  xglBeginUpdate;
  if Assigned(FLibMaterial.Shader) then
  begin
    case FLibMaterial.Shader.ShaderStyle of
      ssHighLevel: FLibMaterial.Shader.Apply(rci, FLibMaterial);
      ssReplace:
      begin
        FLibMaterial.Shader.Apply(rci, FLibMaterial);
        Exit;
      end;
    end;
  end;
  if not FLibMaterial.Material.Texture.Disabled then
  begin
    if not (GetTextureMatrixIsUnitary) then
    begin
      rci.gxStates.SetTextureMatrix(TextureMatrix);
    end;
  end;

  if moNoLighting in FLibMaterial.Material.MaterialOptions then
    rci.gxStates.Disable(stLighting);

  if stLighting in rci.gxStates.States then
  begin
    rci.gxStates.SetMaterialColors(cmFront,
      Emission.Color, Ambient.Color, Diffuse.Color, Specular.Color, Shininess);
    rci.gxStates.PolygonMode :=FLibMaterial.Material.PolygonMode;
  end
  else
    FLibMaterial.Material.FrontProperties.ApplyNoLighting(rci, cmFront);
  if (stCullFace in rci.gxStates.States) then
  begin
    case FLibMaterial.Material.FaceCulling of
      fcBufferDefault: if not rci.bufferFaceCull then
        begin
          rci.gxStates.Disable(stCullFace);
          FLibMaterial.Material.BackProperties.Apply(rci, cmBack);
        end;
      fcCull: ; // nothing to do
      fcNoCull:
      begin
        rci.gxStates.Disable(stCullFace);
        FLibMaterial.Material.BackProperties.Apply(rci, cmBack);
      end;
      else
        Assert(False);
    end;
  end
  else
  begin
    // currently NOT culling
    case FLibMaterial.Material.FaceCulling of
      fcBufferDefault:
      begin
        if rci.bufferFaceCull then
          rci.gxStates.Enable(stCullFace)
        else
          FLibMaterial.Material.BackProperties.Apply(rci, cmBack);
      end;
      fcCull: rci.gxStates.Enable(stCullFace);
      fcNoCull: FLibMaterial.Material.BackProperties.Apply(rci, cmBack);
      else
        Assert(False);
    end;
  end;

  // Apply Blending mode
  if not rci.ignoreBlendingRequests then
    case BlendingMode of
      bmOpaque:
      begin
        rci.gxStates.Disable(stBlend);
        rci.gxStates.Disable(stAlphaTest);
      end;
      bmTransparency:
      begin
        rci.gxStates.Enable(stBlend);
        rci.gxStates.Enable(stAlphaTest);
        rci.gxStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      end;
      bmAdditive:
      begin
        rci.gxStates.Enable(stBlend);
        rci.gxStates.Enable(stAlphaTest);
        rci.gxStates.SetBlendFunc(bfSrcAlpha, bfOne);
      end;
      bmAlphaTest50:
      begin
        rci.gxStates.Disable(stBlend);
        rci.gxStates.Enable(stAlphaTest);
        rci.gxStates.SetAlphaFunction(cfGEqual, 0.5);
      end;
      bmAlphaTest100:
      begin
        rci.gxStates.Disable(stBlend);
        rci.gxStates.Enable(stAlphaTest);
        rci.gxStates.SetAlphaFunction(cfGEqual, 1.0);
      end;
      bmModulate:
      begin
        rci.gxStates.Enable(stBlend);
        rci.gxStates.Enable(stAlphaTest);
        rci.gxStates.SetBlendFunc(bfDstColor, bfZero);
      end;
      else
        Assert(False);
    end;
  // Fog switch
  if moIgnoreFog in FLibMaterial.Material.MaterialOptions then
  begin
    if stFog in rci.gxStates.States then
    begin
      rci.gxStates.Disable(stFog);
      Inc(rci.fogDisabledCounter);
    end;
  end;

  if not Assigned(FLibMaterial.Material.TextureEx) then
  begin
    if Assigned(FLibMaterial.Material.Texture) then
      FLibMaterial.Material.Texture.Apply(rci);
  end
  else
  begin
    if Assigned(FLibMaterial.Material.Texture) and not FLibMaterial.Material.TextureEx.IsTextureEnabled(0) then
      FLibMaterial.Material.Texture.Apply(rci)
    else
    if FLibMaterial.Material.TextureEx.Count > 0 then
      FLibMaterial.Material.TextureEx.Apply(rci);
  end;

  if Assigned(FLibMaterial.Shader) then
  begin
    case FLibMaterial.Shader.ShaderStyle of
      ssLowLevel: FLibMaterial.Shader.Apply(rci, FLibMaterial);
    end;
  end;
  xglEndUpdate;
end;

procedure TgxShaderTextureSharingMaterial.coordNotifychange(Sender: TObject);
begin
  FNeedToUpdateTextureMatrix := True;
  GetTextureSharingShader.NotifyChange(Self);
end;

constructor TgxShaderTextureSharingMaterial.Create(Collection: TCollection);
begin
  inherited;
  FSpecular := TgxColor.Create(Self);
  FSpecular.OnNotifyChange := OtherNotifychange;
  FAmbient := TgxColor.Create(Self);
  FAmbient.OnNotifyChange := OtherNotifychange;
  FDiffuse := TgxColor.Create(Self);
  FDiffuse.OnNotifyChange := OtherNotifychange;
  FEmission := TgxColor.Create(Self);
  FEmission.OnNotifyChange := OtherNotifychange;

  FTexOffset := TgxCoordinates2.CreateInitialized(Self, NullHmgVector, csPoint2d);
  FTexOffset.OnNotifyChange := coordNotifychange;

  FTexScale := TgxCoordinates2.CreateInitialized(Self, XYZHmgVector, csPoint2d);
  FTexScale.OnNotifyChange := coordNotifychange;
  FNeedToUpdateTextureMatrix := True;
end;

destructor TgxShaderTextureSharingMaterial.Destroy;
begin
  FSpecular.Free;
  FAmbient.Free;
  FDiffuse.Free;
  FEmission.Free;
  FTexOffset.Free;
  FTexScale.Free;
  inherited;
end;


function TgxShaderTextureSharingMaterial.GetDisplayName: string;
var
  st: string;
begin
  if Assigned(MaterialLibrary) then
    st := MaterialLibrary.Name
  else
    st := '';
  Result := '[' + st + '.' + Self.LibMaterialName + ']';
end;

function TgxShaderTextureSharingMaterial.GetMaterialLibrary: TgxAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

function TgxShaderTextureSharingMaterial.GetTextureMatrix: TMatrix4f;
begin
  if FNeedToUpdateTextureMatrix then
  begin
    if not (TexOffset.Equals(NullHmgVector) and TexScale.Equals(XYZHmgVector)) then
    begin
      FTextureMatrixIsUnitary := False;
      FTextureMatrix := CreateScaleAndTranslationMatrix(TexScale.AsVector, TexOffset.AsVector)
    end
    else
      FTextureMatrixIsUnitary := True;
    FNeedToUpdateTextureMatrix := False;
  end;
  Result := FTextureMatrix;
end;

function TgxShaderTextureSharingMaterial.GetTextureMatrixIsUnitary: Boolean;
begin
  if FNeedToUpdateTextureMatrix then
    GetTextureMatrix;
  Result := FTextureMatrixIsUnitary;
end;

function TgxShaderTextureSharingMaterial.GetTextureSharingShader: TgxShaderTextureSharing;
begin
  if Collection is TgxShaderTextureSharingMaterials then
    Result := TgxShaderTextureSharingMaterials(Collection).GetParent
  else
    Result := nil;
end;

procedure TgxShaderTextureSharingMaterial.OtherNotifychange(Sender: TObject);
begin
  GetTextureSharingShader.NotifyChange(Self);
end;

procedure TgxShaderTextureSharingMaterial.SetAmbient(const Value: TgxColor);
begin
  FAmbient.Assign(Value);
end;

procedure TgxShaderTextureSharingMaterial.SetBlendingMode(const Value: TgxBlendingMode);
begin
  FBlendingMode := Value;
end;

procedure TgxShaderTextureSharingMaterial.SetDiffuse(const Value: TgxColor);
begin
  FDiffuse.Assign(Value);
end;

procedure TgxShaderTextureSharingMaterial.SetEmission(const Value: TgxColor);
begin
  FEmission.Assign(Value);
end;

procedure TgxShaderTextureSharingMaterial.SetLibMaterialName(const Value: TgxLibMaterialName);
begin
  FLibMaterialName := Value;
  if (FLibMaterialName = '') or (FMaterialLibrary = nil) then
    FLibMaterial := nil
  else
    SetLibMaterial(FMaterialLibrary.LibMaterialByName(FLibMaterialName));
end;

procedure TgxShaderTextureSharingMaterial.SetLibMaterial(const Value: TgxLibMaterial);
begin
  FLibMaterial := Value;
  if FLibMaterial <> nil then
  begin
    FLibMaterialName := FLibMaterial.DisplayName;
    FMaterialLibrary := TgxMaterialLibrary(TgxLibMaterials(Value.Collection).Owner);
    if not (csloading in GetTextureSharingShader.ComponentState) then
    begin
      FTexOffset.Assign(FLibMaterial.TextureOffset);
      FTexScale.Assign(FLibMaterial.TextureScale);
      FBlendingMode := FLibMaterial.Material.BlendingMode;
      fEmission.Assign(FLibMaterial.Material.FrontProperties.Emission);
      fAmbient.Assign(FLibMaterial.Material.FrontProperties.Ambient);
      fDiffuse.Assign(FLibMaterial.Material.FrontProperties.Diffuse);
      fSpecular.Assign(FLibMaterial.Material.FrontProperties.Specular);
      fShininess := FLibMaterial.Material.FrontProperties.Shininess;
    end;
  end;
end;


procedure TgxShaderTextureSharingMaterial.SetMaterialLibrary(const Value: TgxMaterialLibrary);
begin
  FMaterialLibrary := Value;
  if (FLibMaterialName = '') or (FMaterialLibrary = nil) then
    FLibMaterial := nil
  else
    SetLibMaterial(FMaterialLibrary.LibMaterialByName(FLibMaterialName));
end;

procedure TgxShaderTextureSharingMaterial.SetShininess(const Value: TgxShininess);
begin
  FShininess := Value;
end;

procedure TgxShaderTextureSharingMaterial.SetSpecular(const Value: TgxColor);
begin
  FSpecular.Assign(Value);
end;

procedure TgxShaderTextureSharingMaterial.SetTexOffset(const Value: TgxCoordinates2);
begin
  FTexOffset.Assign(Value);
  FNeedToUpdateTextureMatrix := True;
end;

procedure TgxShaderTextureSharingMaterial.SetTexScale(const Value: TgxCoordinates2);
begin
  FTexScale.Assign(Value);
  FNeedToUpdateTextureMatrix := True;
end;

procedure TgxShaderTextureSharingMaterial.UnApply(var rci: TgxRenderContextInfo);
begin
  if not Assigned(FLibMaterial) then
    Exit;

  if Assigned(FLibMaterial.Shader) then
  begin
    case FLibMaterial.Shader.ShaderStyle of
      ssLowLevel: FLibMaterial.Shader.UnApply(rci);
      ssReplace:
      begin
        FLibMaterial.Shader.UnApply(rci);
        Exit;
      end;
    end;
  end;

  FLibMaterial.Material.UnApply(rci);

  if not FLibMaterial.Material.Texture.Disabled then
    if not (GetTextureMatrixIsUnitary) then
    begin
      rci.gxStates.ResetTextureMatrix;
    end;

  if Assigned(FLibMaterial.Shader) then
  begin
    case FLibMaterial.Shader.ShaderStyle of
      ssHighLevel: FLibMaterial.Shader.UnApply(rci);
    end;
  end;
end;

//----------------------------
// TgxShaderTextureSharing
//----------------------------

function TgxShaderTextureSharing.AddLibMaterial(const ALibMaterial: TgxLibMaterial): TgxShaderTextureSharingMaterial;
begin
  Result := FMaterials.Add;
  Result.SetLibMaterial(ALibMaterial);
end;

constructor TgxShaderTextureSharing.Create(AOwner: TComponent);
begin
  inherited;
  FMaterials := TgxShaderTextureSharingMaterials.Create(Self);
  ShaderStyle := ssReplace;
end;

destructor TgxShaderTextureSharing.Destroy;
begin
  FMaterials.Free;
  inherited;
end;

procedure TgxShaderTextureSharing.DoApply(var rci: TgxRenderContextInfo; Sender: TObject);
begin
  if Materials.Count > 0 then
  begin
    rci.gxStates.Enable(stDepthTest);
    rci.gxStates.DepthFunc := cfLEqual;
    Materials[0].Apply(rci);
    FCurrentPass := 1;
  end;
end;

function TgxShaderTextureSharing.DoUnApply(var rci: TgxRenderContextInfo): Boolean;
begin
  Result := False;
  if Materials.Count > 0 then
  begin
    Materials[FCurrentPass - 1].UnApply(rci);
    if FCurrentPass < Materials.Count then
    begin
      Materials[FCurrentPass].Apply(rci);
      Inc(FCurrentPass);
      Result := True;
    end
    else
    begin
      rci.gxStates.DepthFunc := cfLess;
      rci.gxStates.Disable(stBlend);
      rci.gxStates.Disable(stAlphaTest);
      FCurrentPass := 0;
    end;
  end;
end;

function TgxShaderTextureSharing.FindLibMaterial(const ALibMaterial: TgxLibMaterial): TgxShaderTextureSharingMaterial;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FMaterials.Count - 1 do
    if FMaterials[I].FLibMaterial = ALibMaterial then
    begin
      Result := FMaterials[I];
      Break;
    end;
end;

procedure TgxShaderTextureSharing.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent is TgxMaterialLibrary then
    begin
      for I := 0 to Materials.Count - 1 do
      begin
        if Materials.Items[I].MaterialLibrary = AComponent then
          Materials.Items[I].MaterialLibrary := nil;
      end;
    end;
  end;
end;

procedure TgxShaderTextureSharing.SetMaterials(const Value: TgxShaderTextureSharingMaterials);
begin
  FMaterials.Assign(Value);
end;

//----------------------------------------
// TgxShaderTextureSharingMaterials
//----------------------------------------

function TgxShaderTextureSharingMaterials.Add: TgxShaderTextureSharingMaterial;
begin
  Result := (inherited Add) as TgxShaderTextureSharingMaterial;
end;

constructor TgxShaderTextureSharingMaterials.Create(AOwner: TgxShaderTextureSharing);
begin
  inherited Create(AOwner, TgxShaderTextureSharingMaterial);
end;

function TgxShaderTextureSharingMaterials.GetItems(const AIndex: Integer): TgxShaderTextureSharingMaterial;
begin
  Result := (inherited Items[AIndex]) as TgxShaderTextureSharingMaterial;
end;

function TgxShaderTextureSharingMaterials.GetParent: TgxShaderTextureSharing;
begin
  Result := TgxShaderTextureSharing(GetOwner);
end;

procedure TgxShaderTextureSharingMaterials.SetItems(const AIndex: Integer; const Value: TgxShaderTextureSharingMaterial);
begin
  inherited Items[AIndex] := Value;
end;


// ------------------
// ------------------ TgxTexCombineShader ------------------
// ------------------

constructor TgxTexCombineShader.Create(AOwner: TComponent);
begin
  inherited;
  ShaderStyle := ssLowLevel;
  FCombiners := TStringList.Create;
  TStringList(FCombiners).OnChange := NotifyChange;
  FCombinerIsValid := True;
  FCommandCache := nil;
end;

destructor TgxTexCombineShader.Destroy;
begin
  if Assigned(currentLibMaterial3) then
    currentLibMaterial3.UnregisterUser(Self);
  if Assigned(currentLibMaterial4) then
    currentLibMaterial4.UnregisterUser(Self);
  inherited;
  FCombiners.Free;
end;

procedure TgxTexCombineShader.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (FMaterialLibrary = AComponent) and (Operation = opRemove) then
  begin
    NotifyLibMaterial3Destruction;
    NotifyLibMaterial4Destruction;
    FMaterialLibrary := nil;
  end;
  inherited;
end;

procedure TgxTexCombineShader.NotifyChange(Sender: TObject);
begin
  FCombinerIsValid := True;
  FCommandCache := nil;
  inherited NotifyChange(Sender);
end;

procedure TgxTexCombineShader.NotifyLibMaterial3Destruction;
begin
  FLibMaterial3Name := '';
  currentLibMaterial3 := nil;
end;

procedure TgxTexCombineShader.NotifyLibMaterial4Destruction;
begin
  FLibMaterial4Name := '';
  currentLibMaterial4 := nil;
end;

procedure TgxTexCombineShader.SetMaterialLibrary(const val: TgxMaterialLibrary);
begin
  FMaterialLibrary := val;
  SetLibMaterial3Name(LibMaterial3Name);
  SetLibMaterial4Name(LibMaterial4Name);
end;

procedure TgxTexCombineShader.SetLibMaterial3Name(const val: TgxLibMaterialName);
var
  newLibMaterial: TgxLibMaterial;
begin
  // locate new libmaterial
  if Assigned(FMaterialLibrary) then
    newLibMaterial := MaterialLibrary.Materials.GetLibMaterialByName(val)
  else
    newLibMaterial := nil;
  FLibMaterial3Name := val;
  // unregister if required
  if newLibMaterial <> currentLibMaterial3 then
  begin
    // unregister from old
    if Assigned(currentLibMaterial3) then
      currentLibMaterial3.UnregisterUser(Self);
    currentLibMaterial3 := newLibMaterial;
    // register with new
    if Assigned(currentLibMaterial3) then
      currentLibMaterial3.RegisterUser(Self);
    NotifyChange(Self);
  end;
end;

procedure TgxTexCombineShader.SetLibMaterial4Name(const val: TgxLibMaterialName);
var
  newLibMaterial: TgxLibMaterial;
begin
  // locate new libmaterial
  if Assigned(FMaterialLibrary) then
    newLibMaterial := MaterialLibrary.Materials.GetLibMaterialByName(val)
  else
    newLibMaterial := nil;
  FLibMaterial4Name := val;
  // unregister if required
  if newLibMaterial <> currentLibMaterial4 then
  begin
    // unregister from old
    if Assigned(currentLibMaterial4) then
      currentLibMaterial4.UnregisterUser(Self);
    currentLibMaterial4 := newLibMaterial;
    // register with new
    if Assigned(currentLibMaterial4) then
      currentLibMaterial4.RegisterUser(Self);
    NotifyChange(Self);
  end;
end;

procedure TgxTexCombineShader.DoInitialize(var rci: TgxRenderContextInfo; Sender: TObject);
begin
end;

procedure TgxTexCombineShader.DoApply(var rci: TgxRenderContextInfo; Sender: TObject);
var
  n, units: Integer;
begin
//  if not GL_ARB_multitexture then Exit;
  FApplied3 := False;
  FApplied4 := False;
  if FCombinerIsValid and (FDesignTimeEnabled or (not (csDesigning in ComponentState))) then
  begin
    try
      if Assigned(currentLibMaterial3) or Assigned(currentLibMaterial4) then
      begin
        glGetIntegerv(GL_MAX_TEXTURE_UNITS_ARB, @n);
        units := 0;
        if Assigned(currentLibMaterial3) and (n >= 3) then
        begin
          with currentLibMaterial3.Material.Texture do
          begin
            if Enabled then
            begin
              if currentLibMaterial3.TextureMatrixIsIdentity then
                ApplyAsTextureN(3, rci)
              else
                ApplyAsTextureN(3, rci, @currentLibMaterial3.TextureMatrix.X.X);
              //                     ApplyAsTextureN(3, rci, currentLibMaterial3);
              Inc(units, 4);
              FApplied3 := True;
            end;
          end;
        end;
        if Assigned(currentLibMaterial4) and (n >= 4) then
        begin
          with currentLibMaterial4.Material.Texture do
          begin
            if Enabled then
            begin
              if currentLibMaterial4.TextureMatrixIsIdentity then
                ApplyAsTextureN(4, rci)
              else
                ApplyAsTextureN(4, rci, @currentLibMaterial4.TextureMatrix.X.X);
              //                     ApplyAsTextureN(4, rci, currentLibMaterial4);
              Inc(units, 8);
              FApplied4 := True;
            end;
          end;
        end;
        if units > 0 then
          xglMapTexCoordToArbitraryAdd(units);
      end;

      if Length(FCommandCache) = 0 then
        FCommandCache := GetTextureCombiners(FCombiners);
      for n := 0 to High(FCommandCache) do
      begin
        rci.gxStates.ActiveTexture := FCommandCache[n].ActiveUnit;
        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
        glTexEnvi(GL_TEXTURE_ENV, FCommandCache[n].Arg1, FCommandCache[n].Arg2);
      end;
      rci.gxStates.ActiveTexture := 0;
    except
      on E: Exception do
      begin
        FCombinerIsValid := False;
        InformationDlg(E.ClassName + ': ' + E.Message);
      end;
    end;
  end;
end;

function TgxTexCombineShader.DoUnApply(var rci: TgxRenderContextInfo): Boolean;
begin
  if FApplied3 then
    with currentLibMaterial3.Material.Texture do
      UnApplyAsTextureN(3, rci, (not currentLibMaterial3.TextureMatrixIsIdentity));
  if FApplied4 then
    with currentLibMaterial4.Material.Texture do
      UnApplyAsTextureN(4, rci, (not currentLibMaterial4.TextureMatrixIsIdentity));
  Result := False;
end;

procedure TgxTexCombineShader.DoFinalize;
begin
end;

procedure TgxTexCombineShader.SetCombiners(const val: TStringList);
begin
  if val <> FCombiners then
  begin
    FCombiners.Assign(val);
    NotifyChange(Self);
  end;
end;

procedure TgxTexCombineShader.SetDesignTimeEnabled(const val: Boolean);
begin
  if val <> FDesignTimeEnabled then
  begin
    FDesignTimeEnabled := val;
    NotifyChange(Self);
  end;
end;


//----------------------------------------------------------------------------
initialization
//----------------------------------------------------------------------------

  RegisterClasses([TgxShaderTextureSharing, TgxShaderTextureSharingMaterials,
                   TgxShaderTextureSharingMaterial]);

end.
