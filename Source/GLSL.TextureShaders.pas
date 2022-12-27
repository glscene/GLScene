//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLSL.TextureShaders;

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
  
  GLS.OpenGLTokens,
  GLS.Scene,
  GLS.Context,
  GLS.Texture,
  GLS.TextureCombiners,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.Color,
  GLS.Material,
  GLS.Strings,
  GLS.VectorFileObjects,
  GLS.XOpenGL,
  GLS.State,
  GLS.PersistentClasses,
  GLS.Coordinates,
  GLS.RenderContextInfo,
  GLS.Utils;

type
  TGLTextureSharingShader = class;

  TGLTextureSharingShaderMaterial = class(TGLInterfacedCollectionItem, IGLMaterialLibrarySupported)
  private
    FTextureMatrix: TGLMatrix;
    FNeedToUpdateTextureMatrix: Boolean;
    FTextureMatrixIsUnitary: Boolean;
    FLibMaterial: TGLLibMaterial;
    FTexOffset: TGLCoordinates2;
    FTexScale: TGLCoordinates2;
    FBlendingMode: TGLBlendingMode;
    FSpecular: TGLColor;
    FAmbient: TGLColor;
    FDiffuse: TGLColor;
    FEmission: TGLColor;
    FShininess: TGLShininess;
    FMaterialLibrary: TGLMaterialLibrary;
    FLibMaterialName: TGLLibMaterialName;
    procedure SetAmbient(const Value: TGLColor);
    procedure SetDiffuse(const Value: TGLColor);
    procedure SetEmission(const Value: TGLColor);
    procedure SetShininess(const Value: TGLShininess);
    procedure SetSpecular(const Value: TGLColor);
    procedure SetMaterialLibrary(const Value: TGLMaterialLibrary);
    procedure SetLibMaterialName(const Value: TGLLibMaterialName);
    procedure SetBlendingMode(const Value: TGLBlendingMode);
    procedure SetLibMaterial(const Value: TGLLibMaterial);
    procedure SetTexOffset(const Value: TGLCoordinates2);
    procedure SetTexScale(const Value: TGLCoordinates2);
    function GetTextureMatrix: TGLMatrix;
    function GetTextureMatrixIsUnitary: Boolean;
  protected
    procedure coordNotifychange(Sender: TObject);
    procedure OtherNotifychange(Sender: TObject);
    function GetDisplayName: string; override;
    function GetTextureSharingShader: TGLTextureSharingShader;
    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TGLAbstractMaterialLibrary; virtual;
  public
    procedure Apply(var rci: TGLRenderContextInfo);
    procedure UnApply(var rci: TGLRenderContextInfo);
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property LibMaterial: TGLLibMaterial read FLibMaterial write SetLibMaterial;
    property TextureMatrix: TGLMatrix read GetTextureMatrix;
    property TextureMatrixIsUnitary: Boolean read GetTextureMatrixIsUnitary;
  published
    property TexOffset: TGLCoordinates2 read FTexOffset write SetTexOffset;
    property TexScale: TGLCoordinates2 read FTexScale write SetTexScale;
    property BlendingMode: TGLBlendingMode read FBlendingMode write SetBlendingMode;
    property Emission: TGLColor read FEmission write SetEmission;
    property Ambient: TGLColor read FAmbient write SetAmbient;
    property Diffuse: TGLColor read FDiffuse write SetDiffuse;
    property Specular: TGLColor read FSpecular write SetSpecular;
    property Shininess: TGLShininess read FShininess write SetShininess;
    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    property LibMaterialName: TGLLibMaterialName read FLibMaterialName write SetLibMaterialName;
  end;

  TGLTextureSharingShaderMaterials = class(TOwnedCollection)
  protected
    function GetItems(const AIndex: Integer): TGLTextureSharingShaderMaterial;
    procedure SetItems(const AIndex: Integer; const Value: TGLTextureSharingShaderMaterial);
    function GetParent: TGLTextureSharingShader;
  public
    function Add: TGLTextureSharingShaderMaterial;
    constructor Create(AOwner: TGLTextureSharingShader);
    property Items[const AIndex: Integer]: TGLTextureSharingShaderMaterial read GetItems write SetItems; default;
  end;

  TGLTextureSharingShader = class(TGLShader)
  private
    FMaterials: TGLTextureSharingShaderMaterials;
    FCurrentPass: Integer;
    procedure SetMaterials(const Value: TGLTextureSharingShaderMaterials);
  protected
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddLibMaterial(const ALibMaterial: TGLLibMaterial): TGLTextureSharingShaderMaterial;
    function FindLibMaterial(const ALibMaterial: TGLLibMaterial): TGLTextureSharingShaderMaterial;
  published
    property Materials: TGLTextureSharingShaderMaterials read FMaterials write SetMaterials;
  end;

(* A shader that allows texture combiner setup. *)

  // A shader that can setup the texture combiner.
  TGLTexCombineShader = class(TGLShader)
  private
    FCombiners: TStringList;
    FCommandCache: TCombinerCache;
    FCombinerIsValid: Boolean; // to avoid reparsing invalid stuff
    FDesignTimeEnabled: Boolean;
    FMaterialLibrary: TGLMaterialLibrary;
    FLibMaterial3Name: TGLLibMaterialName;
    currentLibMaterial3: TGLLibMaterial;
    FLibMaterial4Name: TGLLibMaterialName;
    currentLibMaterial4: TGLLibMaterial;
    FApplied3, FApplied4: Boolean;
  protected
    procedure SetCombiners(const val: TStringList);
    procedure SetDesignTimeEnabled(const val: Boolean);
    procedure SetMaterialLibrary(const val: TGLMaterialLibrary);
    procedure SetLibMaterial3Name(const val: TGLLibMaterialName);
    procedure SetLibMaterial4Name(const val: TGLLibMaterialName);
    procedure NotifyLibMaterial3Destruction;
    procedure NotifyLibMaterial4Destruction;
    procedure DoInitialize(var rci: TGLRenderContextInfo; Sender: TObject); override;
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
    procedure DoFinalize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NotifyChange(Sender: TObject); override;
  published
    property Combiners: TStringList read FCombiners write SetCombiners;
    property DesignTimeEnabled: Boolean read FDesignTimeEnabled write SetDesignTimeEnabled;
    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    property LibMaterial3Name: TGLLibMaterialName read FLibMaterial3Name write SetLibMaterial3Name;
    property LibMaterial4Name: TGLLibMaterialName read FLibMaterial4Name write SetLibMaterial4Name;
  end;

//================================================
implementation
//================================================

//-----------------------------------
// TGLTextureSharingShaderMaterial
//-----------------------------------

procedure TGLTextureSharingShaderMaterial.Apply(var rci: TGLRenderContextInfo);
begin
  if not Assigned(FLibMaterial) then
    Exit;
  xgl.BeginUpdate;
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
      rci.GLStates.SetGLTextureMatrix(TextureMatrix);
    end;
  end;

  if moNoLighting in FLibMaterial.Material.MaterialOptions then
    rci.GLStates.Disable(stLighting);

  if stLighting in rci.GLStates.States then
  begin
    rci.GLStates.SetGLMaterialColors(cmFront,
      Emission.Color, Ambient.Color, Diffuse.Color, Specular.Color, Shininess);
    rci.GLStates.PolygonMode :=FLibMaterial.Material.PolygonMode;
  end
  else
    FLibMaterial.Material.FrontProperties.ApplyNoLighting(rci, cmFront);
  if (stCullFace in rci.GLStates.States) then
  begin
    case FLibMaterial.Material.FaceCulling of
      fcBufferDefault: if not rci.bufferFaceCull then
        begin
          rci.GLStates.Disable(stCullFace);
          FLibMaterial.Material.BackProperties.Apply(rci, cmBack);
        end;
      fcCull: ; // nothing to do
      fcNoCull:
      begin
        rci.GLStates.Disable(stCullFace);
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
          rci.GLStates.Enable(stCullFace)
        else
          FLibMaterial.Material.BackProperties.Apply(rci, cmBack);
      end;
      fcCull: rci.GLStates.Enable(stCullFace);
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
        rci.GLStates.Disable(stBlend);
        rci.GLStates.Disable(stAlphaTest);
      end;
      bmTransparency:
      begin
        rci.GLStates.Enable(stBlend);
        rci.GLStates.Enable(stAlphaTest);
        rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      end;
      bmAdditive:
      begin
        rci.GLStates.Enable(stBlend);
        rci.GLStates.Enable(stAlphaTest);
        rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOne);
      end;
      bmAlphaTest50:
      begin
        rci.GLStates.Disable(stBlend);
        rci.GLStates.Enable(stAlphaTest);
        rci.GLStates.SetGLAlphaFunction(cfGEqual, 0.5);
      end;
      bmAlphaTest100:
      begin
        rci.GLStates.Disable(stBlend);
        rci.GLStates.Enable(stAlphaTest);
        rci.GLStates.SetGLAlphaFunction(cfGEqual, 1.0);
      end;
      bmModulate:
      begin
        rci.GLStates.Enable(stBlend);
        rci.GLStates.Enable(stAlphaTest);
        rci.GLStates.SetBlendFunc(bfDstColor, bfZero);
      end;
      else
        Assert(False);
    end;
  // Fog switch
  if moIgnoreFog in FLibMaterial.Material.MaterialOptions then
  begin
    if stFog in rci.GLStates.States then
    begin
      rci.GLStates.Disable(stFog);
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
  xgl.EndUpdate;
end;

procedure TGLTextureSharingShaderMaterial.coordNotifychange(Sender: TObject);
begin
  FNeedToUpdateTextureMatrix := True;
  GetTextureSharingShader.NotifyChange(Self);
end;

constructor TGLTextureSharingShaderMaterial.Create(Collection: TCollection);
begin
  inherited;
  FSpecular := TGLColor.Create(Self);
  FSpecular.OnNotifyChange := OtherNotifychange;
  FAmbient := TGLColor.Create(Self);
  FAmbient.OnNotifyChange := OtherNotifychange;
  FDiffuse := TGLColor.Create(Self);
  FDiffuse.OnNotifyChange := OtherNotifychange;
  FEmission := TGLColor.Create(Self);
  FEmission.OnNotifyChange := OtherNotifychange;

  FTexOffset := TGLCoordinates2.CreateInitialized(Self, NullHmgVector, csPoint2d);
  FTexOffset.OnNotifyChange := coordNotifychange;

  FTexScale := TGLCoordinates2.CreateInitialized(Self, XYZHmgVector, csPoint2d);
  FTexScale.OnNotifyChange := coordNotifychange;
  FNeedToUpdateTextureMatrix := True;
end;

destructor TGLTextureSharingShaderMaterial.Destroy;
begin
  FSpecular.Free;
  FAmbient.Free;
  FDiffuse.Free;
  FEmission.Free;
  FTexOffset.Free;
  FTexScale.Free;
  inherited;
end;


function TGLTextureSharingShaderMaterial.GetDisplayName: string;
var
  st: string;
begin
  if Assigned(MaterialLibrary) then
    st := MaterialLibrary.Name
  else
    st := '';
  Result := '[' + st + '.' + Self.LibMaterialName + ']';
end;

function TGLTextureSharingShaderMaterial.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

function TGLTextureSharingShaderMaterial.GetTextureMatrix: TGLMatrix;
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

function TGLTextureSharingShaderMaterial.GetTextureMatrixIsUnitary: Boolean;
begin
  if FNeedToUpdateTextureMatrix then
    GetTextureMatrix;
  Result := FTextureMatrixIsUnitary;
end;

function TGLTextureSharingShaderMaterial.GetTextureSharingShader: TGLTextureSharingShader;
begin
  if Collection is TGLTextureSharingShaderMaterials then
    Result := TGLTextureSharingShaderMaterials(Collection).GetParent
  else
    Result := nil;
end;

procedure TGLTextureSharingShaderMaterial.OtherNotifychange(Sender: TObject);
begin
  GetTextureSharingShader.NotifyChange(Self);
end;

procedure TGLTextureSharingShaderMaterial.SetAmbient(const Value: TGLColor);
begin
  FAmbient.Assign(Value);
end;

procedure TGLTextureSharingShaderMaterial.SetBlendingMode(const Value: TGLBlendingMode);
begin
  FBlendingMode := Value;
end;

procedure TGLTextureSharingShaderMaterial.SetDiffuse(const Value: TGLColor);
begin
  FDiffuse.Assign(Value);
end;

procedure TGLTextureSharingShaderMaterial.SetEmission(const Value: TGLColor);
begin
  FEmission.Assign(Value);
end;

procedure TGLTextureSharingShaderMaterial.SetLibMaterialName(const Value: TGLLibMaterialName);
begin
  FLibMaterialName := Value;
  if (FLibMaterialName = '') or (FMaterialLibrary = nil) then
    FLibMaterial := nil
  else
    SetLibMaterial(FMaterialLibrary.LibMaterialByName(FLibMaterialName));
end;

procedure TGLTextureSharingShaderMaterial.SetLibMaterial(const Value: TGLLibMaterial);
begin
  FLibMaterial := Value;
  if FLibMaterial <> nil then
  begin
    FLibMaterialName := FLibMaterial.DisplayName;
    FMaterialLibrary := TGLMaterialLibrary(TGLLibMaterials(Value.Collection).Owner);
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


procedure TGLTextureSharingShaderMaterial.SetMaterialLibrary(const Value: TGLMaterialLibrary);
begin
  FMaterialLibrary := Value;
  if (FLibMaterialName = '') or (FMaterialLibrary = nil) then
    FLibMaterial := nil
  else
    SetLibMaterial(FMaterialLibrary.LibMaterialByName(FLibMaterialName));
end;

procedure TGLTextureSharingShaderMaterial.SetShininess(const Value: TGLShininess);
begin
  FShininess := Value;
end;

procedure TGLTextureSharingShaderMaterial.SetSpecular(const Value: TGLColor);
begin
  FSpecular.Assign(Value);
end;

procedure TGLTextureSharingShaderMaterial.SetTexOffset(const Value: TGLCoordinates2);
begin
  FTexOffset.Assign(Value);
  FNeedToUpdateTextureMatrix := True;
end;

procedure TGLTextureSharingShaderMaterial.SetTexScale(const Value: TGLCoordinates2);
begin
  FTexScale.Assign(Value);
  FNeedToUpdateTextureMatrix := True;
end;

procedure TGLTextureSharingShaderMaterial.UnApply(var rci: TGLRenderContextInfo);
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
      rci.GLStates.ResetGLTextureMatrix;
    end;

  if Assigned(FLibMaterial.Shader) then
  begin
    case FLibMaterial.Shader.ShaderStyle of
      ssHighLevel: FLibMaterial.Shader.UnApply(rci);
    end;
  end;
end;

//-----------------------------------
// TGLTextureSharingShader
//-----------------------------------

function TGLTextureSharingShader.AddLibMaterial(const ALibMaterial: TGLLibMaterial): TGLTextureSharingShaderMaterial;
begin
  Result := FMaterials.Add;
  Result.SetLibMaterial(ALibMaterial);
end;

constructor TGLTextureSharingShader.Create(AOwner: TComponent);
begin
  inherited;
  FMaterials := TGLTextureSharingShaderMaterials.Create(Self);
  ShaderStyle := ssReplace;
end;

destructor TGLTextureSharingShader.Destroy;
begin
  FMaterials.Free;
  inherited;
end;

procedure TGLTextureSharingShader.DoApply(var rci: TGLRenderContextInfo; Sender: TObject);
begin
  if Materials.Count > 0 then
  begin
    rci.GLStates.Enable(stDepthTest);
    rci.GLStates.DepthFunc := cfLEqual;
    Materials[0].Apply(rci);
    FCurrentPass := 1;
  end;
end;

function TGLTextureSharingShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
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
      rci.GLStates.DepthFunc := cfLess;
      rci.GLStates.Disable(stBlend);
      rci.GLStates.Disable(stAlphaTest);
      FCurrentPass := 0;
    end;
  end;
end;

function TGLTextureSharingShader.FindLibMaterial(const ALibMaterial: TGLLibMaterial): TGLTextureSharingShaderMaterial;
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

procedure TGLTextureSharingShader.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent is TGLMaterialLibrary then
    begin
      for I := 0 to Materials.Count - 1 do
      begin
        if Materials.Items[I].MaterialLibrary = AComponent then
          Materials.Items[I].MaterialLibrary := nil;
      end;
    end;
  end;
end;

procedure TGLTextureSharingShader.SetMaterials(const Value: TGLTextureSharingShaderMaterials);
begin
  FMaterials.Assign(Value);
end;

//-----------------------------------
// TGLTextureSharingShaderMaterials
//-----------------------------------

function TGLTextureSharingShaderMaterials.Add: TGLTextureSharingShaderMaterial;
begin
  Result := (inherited Add) as TGLTextureSharingShaderMaterial;
end;

constructor TGLTextureSharingShaderMaterials.Create(AOwner: TGLTextureSharingShader);
begin
  inherited Create(AOwner, TGLTextureSharingShaderMaterial);
end;

function TGLTextureSharingShaderMaterials.GetItems(const AIndex: Integer): TGLTextureSharingShaderMaterial;
begin
  Result := (inherited Items[AIndex]) as TGLTextureSharingShaderMaterial;
end;

function TGLTextureSharingShaderMaterials.GetParent: TGLTextureSharingShader;
begin
  Result := TGLTextureSharingShader(GetOwner);
end;

procedure TGLTextureSharingShaderMaterials.SetItems(const AIndex: Integer; const Value: TGLTextureSharingShaderMaterial);
begin
  inherited Items[AIndex] := Value;
end;

// ------------------
// ------------------ TGLTexCombineShader ------------------
// ------------------

constructor TGLTexCombineShader.Create(AOwner: TComponent);
begin
  inherited;
  ShaderStyle := ssLowLevel;
  FCombiners := TStringList.Create;
  TStringList(FCombiners).OnChange := NotifyChange;
  FCombinerIsValid := True;
  FCommandCache := nil;
end;

destructor TGLTexCombineShader.Destroy;
begin
  if Assigned(currentLibMaterial3) then
    currentLibMaterial3.UnregisterUser(Self);
  if Assigned(currentLibMaterial4) then
    currentLibMaterial4.UnregisterUser(Self);
  inherited;
  FCombiners.Free;
end;

procedure TGLTexCombineShader.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (FMaterialLibrary = AComponent) and (Operation = opRemove) then
  begin
    NotifyLibMaterial3Destruction;
    NotifyLibMaterial4Destruction;
    FMaterialLibrary := nil;
  end;
  inherited;
end;

procedure TGLTexCombineShader.NotifyChange(Sender: TObject);
begin
  FCombinerIsValid := True;
  FCommandCache := nil;
  inherited NotifyChange(Sender);
end;


procedure TGLTexCombineShader.NotifyLibMaterial3Destruction;
begin
  FLibMaterial3Name := '';
  currentLibMaterial3 := nil;
end;


procedure TGLTexCombineShader.NotifyLibMaterial4Destruction;
begin
  FLibMaterial4Name := '';
  currentLibMaterial4 := nil;
end;


procedure TGLTexCombineShader.SetMaterialLibrary(const val: TGLMaterialLibrary);
begin
  FMaterialLibrary := val;
  SetLibMaterial3Name(LibMaterial3Name);
  SetLibMaterial4Name(LibMaterial4Name);
end;


procedure TGLTexCombineShader.SetLibMaterial3Name(const val: TGLLibMaterialName);
var
  newLibMaterial: TGLLibMaterial;
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


procedure TGLTexCombineShader.SetLibMaterial4Name(const val: TGLLibMaterialName);
var
  newLibMaterial: TGLLibMaterial;
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


procedure TGLTexCombineShader.DoInitialize(var rci: TGLRenderContextInfo; Sender: TObject);
begin
end;


procedure TGLTexCombineShader.DoApply(var rci: TGLRenderContextInfo; Sender: TObject);
var
  n, units: Integer;
begin
  if not GL.ARB_multitexture then
    Exit;
  FApplied3 := False;
  FApplied4 := False;
  if FCombinerIsValid and (FDesignTimeEnabled or (not (csDesigning in ComponentState))) then
  begin
    try
      if Assigned(currentLibMaterial3) or Assigned(currentLibMaterial4) then
      begin
        gl.GetIntegerv(GL_MAX_TEXTURE_UNITS_ARB, @n);
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
                ApplyAsTextureN(3, rci, @currentLibMaterial3.TextureMatrix.V[0].X);
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
                ApplyAsTextureN(4, rci, @currentLibMaterial4.TextureMatrix.V[0].X);
              //                     ApplyAsTextureN(4, rci, currentLibMaterial4);
              Inc(units, 8);
              FApplied4 := True;
            end;
          end;
        end;
        if units > 0 then
          xgl.MapTexCoordToArbitraryAdd(units);
      end;

      if Length(FCommandCache) = 0 then
        FCommandCache := GetTextureCombiners(FCombiners);
      for n := 0 to High(FCommandCache) do
      begin
        rci.GLStates.ActiveTexture := FCommandCache[n].ActiveUnit;
        gl.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
        gl.TexEnvi(GL_TEXTURE_ENV, FCommandCache[n].Arg1, FCommandCache[n].Arg2);
      end;
      rci.GLStates.ActiveTexture := 0;
    except
      on E: Exception do
      begin
        FCombinerIsValid := False;
        InformationDlg(E.ClassName + ': ' + E.Message);
      end;
    end;
  end;
end;

function TGLTexCombineShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  if FApplied3 then
    with currentLibMaterial3.Material.Texture do
      UnApplyAsTextureN(3, rci, (not currentLibMaterial3.TextureMatrixIsIdentity));
  if FApplied4 then
    with currentLibMaterial4.Material.Texture do
      UnApplyAsTextureN(4, rci, (not currentLibMaterial4.TextureMatrixIsIdentity));
  Result := False;
end;


procedure TGLTexCombineShader.DoFinalize;
begin
end;


procedure TGLTexCombineShader.SetCombiners(const val: TStringList);
begin
  if val <> FCombiners then
  begin
    FCombiners.Assign(val);
    NotifyChange(Self);
  end;
end;


procedure TGLTexCombineShader.SetDesignTimeEnabled(const val: Boolean);
begin
  if val <> FDesignTimeEnabled then
  begin
    FDesignTimeEnabled := val;
    NotifyChange(Self);
  end;
end;


//================================================
initialization
//================================================

  RegisterClasses([TGLTextureSharingShader, TGLTextureSharingShaderMaterials,
                   TGLTextureSharingShaderMaterial]);

end.
