//
// This unit is part of the GLScene Engine, http://glscene.org
//

unit GLS.ShaderTexCombine;

(* A shader that allows texture combiner setup. *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,

  GLTexture,
  GLMaterial,
  GLRenderContextInfo,
  GLTextureCombiners,
  OpenGLTokens,
  GLState,
  XOpenGL,
  GLContext,
  GLS.Utils;


type

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

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
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

end.

