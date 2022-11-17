//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.MaterialScript;

(* Material Script Batch loader for TGLMaterialLibrary for runtime. *)

interface

{$I GLScene.inc}

uses
  System.SysUtils,
  System.Classes,
  VCL.StdCtrls,

  GLS.VectorTypes,
  GLS.Texture,
  GLS.TextureFormat,
  GLS.Graphics,
  GLS.Utils,
  GLS.Color,
  GLS.Coordinates,
  GLS.Material,
  GLS.State;

type
  TGLShaderItem = class(TCollectionItem)
  private
    FShader: TGLShader;
    FName: string;
    procedure SetShader(const Value: TGLShader);
    procedure SetName(const Value: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Shader: TGLShader read FShader write SetShader;
    property Name: string read FName write SetName;
  end;

  TGLShaderItems = class(TOwnedCollection)
  private
    procedure SetItems(Index: Integer; const Val: TGLShaderItem);
    function GetItems(Index: Integer): TGLShaderItem;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TGLShaderItem read GetItems
      write SetItems; default;
  end;

  TGLMaterialLibraryItem = class(TCollectionItem)
  private
    FMaterialLibrary: TGLMaterialLibrary;
    FName: string;
    procedure SetMaterialLibrary(const Value: TGLMaterialLibrary);
    procedure SetName(const Value: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary
      write SetMaterialLibrary;
    property Name: string read FName write SetName;
  end;

  TGLMaterialLibraryItems = class(TOwnedCollection)
  private
    procedure SetItems(Index: Integer; const Val: TGLMaterialLibraryItem);
    function GetItems(Index: Integer): TGLMaterialLibraryItem;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TGLMaterialLibraryItem read GetItems
      write SetItems; default;

  end;

  TGLMaterialScripter = class(TComponent)
  private
    FShaderItems: TGLShaderItems;
    FMaterialLibraryItems: TGLMaterialLibraryItems;
    FAppend: Boolean;
    FOverwrite: Boolean;
    FScript: TStrings;
    FMemo: TMemo;
    FMaterialLibrary: TGLMaterialLibrary;
    Count: Longint;
    Infini: Longint;
    Done: Boolean;
    NewMat: TGLLibMaterial;
    TmpCoords: TGLCoordinates;
    TmpColor: TGLColor;
    TmpCoords4: TGLCoordinates4;
    TmpStr: string;
    procedure SeTGLShaderItems(const Value: TGLShaderItems);
    procedure SeTGLMaterialLibraryItems(const Value: TGLMaterialLibraryItems);
    procedure SetAppend(const Value: Boolean);
    procedure SetOverwrite(const Value: Boolean);
    procedure SetScript(const Value: TStrings);
    procedure SetMaterialLibrary(const Value: TGLMaterialLibrary);
    procedure SetMemo(const Value: TMemo);
    // error checking
    procedure CheckError;
    function ClassExists(arguement: string): Boolean;
    function CheckRepeatDone: Boolean;
    // extraction functions
    function ExtractValue: string;
    procedure ExtractCoords3;
    procedure ExtractCoords4;
    procedure ExtractColors;
    function DeleteSpaces(Value: string): string;
    function SubstrExists(substr: string): Boolean;
    function ValueExists(Value: string): Boolean;
    // these are our viable scripts
    procedure ZMaterial;
    // internally called scripts for value extraction
    procedure XMaterial;
    procedure XName;
    procedure XShader;
    procedure XTexture2Name;
    procedure XTextureOffset;
    procedure XTextureScale;
    procedure XTexture;
    procedure XCompression;
    procedure XEnvColor;
    procedure XFilteringQuality;
    procedure XImageAlpha;
    procedure XImageBrightness;
    procedure XImageClass;
    procedure XImageGamma;
    procedure XMagFilter;
    procedure XMappingMode;
    procedure XMappingSCoordinates;
    procedure XMappingTCoordinates;
    procedure XMinFilter;
    procedure XNormalMapScale;
    procedure XTextureFormat;
    procedure XTextureMode;
    procedure XTextureWrap;
    procedure XBlendingMode;
    procedure XPolygonMode;
    procedure XFacingCulling;
    procedure XLibMaterialName;
    procedure XMaterialOptions;
    procedure XMaterialLibrary;
    procedure XBackProperties;
    procedure XBackAmbient;
    procedure XBackDiffuse;
    procedure XBackEmission;
    procedure XBackShininess;
    procedure XBackSpecular;
    procedure XFrontProperties;
    procedure XFrontAmbient;
    procedure XFrontDiffuse;
    procedure XFrontEmission;
    procedure XFrontShininess;
    procedure XFrontSpecular;
    procedure XPersistantImage;
    procedure XBlankImage;
    procedure XPictureFileName;
    procedure XPicturePX;
    procedure XPictureNX;
    procedure XPicturePY;
    procedure XPictureNY;
    procedure XPicturePZ;
    procedure XPictureNZ;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    property DebugMemo: TMemo read FMemo write SetMemo;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CompileScript;
  published
    property Script: TStrings read FScript write SetScript;
    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary
      write SetMaterialLibrary;
    property Shaders: TGLShaderItems read FShaderItems write SeTGLShaderItems;
    property MaterialLibraries: TGLMaterialLibraryItems
      read FMaterialLibraryItems write SeTGLMaterialLibraryItems;
    property AppendToMaterialLibrary: Boolean read FAppend write SetAppend;
    property OverwriteToMaterialLibrary: Boolean read FOverwrite
      write SetOverwrite;
  end;

// ----------------------------------------------------------------------
implementation
// ----------------------------------------------------------------------

procedure TGLShaderItem.SetShader(const Value: TGLShader);
begin
  if Assigned(Value) then
  begin
    FShader := Value;
    FName := FShader.Name;
  end;
end;

procedure TGLShaderItem.Assign(Source: TPersistent);
begin
  if Source is TGLShaderItem then
  begin
    FShader := TGLShaderItem(Source).FShader;
  end;
  inherited Destroy;
end;

constructor TGLShaderItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FName := 'Shader';
end;

destructor TGLShaderItem.Destroy;
begin
  inherited Destroy;
end;

function TGLShaderItem.GetDisplayName: String;
begin
  if FName = '' then
    Result := 'Shader'
  else
    Result := FName;
end;

// ------------------------
// TGLShaderItems
// ------------------------

constructor TGLShaderItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TGLShaderItem);
end;

function TGLShaderItems.GetItems(Index: Integer): TGLShaderItem;
begin
  Result := TGLShaderItem(inherited Items[index]);
end;

procedure TGLShaderItems.SetItems(Index: Integer; const Val: TGLShaderItem);
begin
  inherited Items[index] := Val;
end;

procedure TGLMaterialScripter.SeTGLShaderItems(const Value: TGLShaderItems);
begin
  FShaderItems.Assign(Value);
end;

procedure TGLShaderItem.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TGLMaterialScripter.CompileScript;
begin
  Done := False;
  NewMat := nil;
  Count := 0;
  Infini := 0;
  TmpCoords := nil;
  TmpCoords4 := nil;
  TmpColor := nil;
  TmpStr := '';
  repeat
    inc(Count);
    if pos('{', FScript.Strings[Count]) > 0 then
    begin
      if SubstrExists('material') then
        ZMaterial;
    end;
    CheckError;
  until CheckRepeatDone;
end;

procedure TGLMaterialScripter.SetMaterialLibrary(const Value
  : TGLMaterialLibrary);
begin
  if FMaterialLibrary <> nil then
    FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if FMaterialLibrary <> nil then
    FMaterialLibrary.FreeNotification(Self);
end;

procedure TGLMaterialScripter.SetMemo(const Value: TMemo);
begin
  if FMemo <> nil then
    FMemo.RemoveFreeNotification(Self);
  FMemo := Value;
  if FMemo <> nil then
    FMemo.FreeNotification(Self);
end;

procedure TGLMaterialScripter.SetScript(const Value: TStrings);
begin
  if Assigned(Value) then
    FScript.Assign(Value);
end;

procedure TGLMaterialScripter.CheckError;
begin
  if Count >= FScript.Count then
    Done := True;
  if Done then
    raise Exception.Create('User Error : No closing "}"');
  inc(Infini);
  if Infini > 1280000 then
  begin
    raise Exception.Create('Internal Error : Infinate Loop');
    Done := True;
    exit;
  end;
end;

function TGLMaterialScripter.CheckRepeatDone: Boolean;
begin
  CheckRepeatDone := False;
  if pos('}', FScript.Strings[Count]) > 0 then
  begin
    CheckRepeatDone := True;
    inc(Count);
  end;
  if Done then
    CheckRepeatDone := True;
end;

function TGLMaterialScripter.ClassExists(arguement: string): Boolean;
var
  Temp: string;
  i: word;
begin

  ClassExists := False;
  if (pos(UpperCase(arguement), UpperCase(FScript.Strings[Count])) > 0) and
  // check if there is an arguement
    (pos('=', FScript.Strings[Count]) > pos(UpperCase(arguement),
    UpperCase(FScript.Strings[Count]))) and // check if it is before '='
    (pos('=', FScript.Strings[Count]) > 0) then // check if there even is a '='
  begin

    Temp := FScript.Strings[Count];
    for i := 0 to length(Temp) do
      if pos(' ', Temp) = 1 then
        delete(Temp, 1, 1);

    if pos(UpperCase(arguement), UpperCase(Temp)) = 1 then
      if (Temp[length(arguement) + 1] = ' ') or
        (Temp[length(arguement) + 1] = '=') then
      begin
        ClassExists := True;
        if Assigned(FMemo) then
          FMemo.Lines.Add('Stage is at : ' + arguement);
      end;
  end;

end;

function TGLMaterialScripter.SubstrExists(substr: string): Boolean;
begin
  if pos(UpperCase(substr), UpperCase(FScript.Strings[Count])) > 0 then
    Result := True
  else
    Result := False;
end;

constructor TGLMaterialScripter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScript := TStringList.Create;
  FShaderItems := TGLShaderItems.Create(Self);
  FMaterialLibraryItems := TGLMaterialLibraryItems.Create(Self);
  FAppend := True;
  FOverwrite := False;
end;

function TGLMaterialScripter.DeleteSpaces(Value: string): string;
var
  i: byte;
begin
  Result := Value;

  for i := 0 to length(Result) do
    if pos(' ', Result) > 0 then
      delete(Result, pos(' ', Result), 1);
end;

destructor TGLMaterialScripter.Destroy;
begin
  FShaderItems.Free;
  FMaterialLibraryItems.Free;
  FScript.Free;
  inherited Destroy;
end;

procedure TGLMaterialScripter.ExtractColors;
var
  Val: string;
begin
  Val := ExtractValue;
  if pos('(', Val) > 0 then
  begin
    TmpColor.Alpha := GLStrToFloatDef(copy(Val, pos('(', Val) + 1,
      pos(';', Val) - 2));
    delete(Val, 1, pos(';', Val));
    TmpColor.Red := GLStrToFloatDef(copy(Val, 1, pos(';', Val) - 1));
    delete(Val, 1, pos(';', Val));
    TmpColor.Green := GLStrToFloatDef(copy(Val, 1, pos(';', Val) - 1));
    delete(Val, 1, pos(';', Val));
    TmpColor.Blue := GLStrToFloatDef(copy(Val, 1, pos(')', Val) - 1));
  end;
end;

procedure TGLMaterialScripter.ExtractCoords3;
var
  Val: string;
begin
  Val := ExtractValue;
  if pos('(', Val) > 0 then
  begin
    TmpCoords.X := GLStrToFloatDef(copy(Val, pos('(', Val) + 1,
      pos(';', Val) - 2));
    delete(Val, 1, pos(';', Val));
    TmpCoords.Y := GLStrToFloatDef(copy(Val, 1, pos(';', Val) - 1));
    delete(Val, 1, pos(';', Val));
    TmpCoords.Z := GLStrToFloatDef(copy(Val, 1, pos(')', Val) - 1));
  end;
end;

procedure TGLMaterialScripter.ExtractCoords4;
var
  Val: string;
begin
  Val := ExtractValue;
  if pos('(', Val) > 0 then
  begin
    TmpCoords4.W := GLStrToFloatDef(copy(Val, pos('(', Val) + 1,
      pos(';', Val) - 2));
    delete(Val, 1, pos(';', Val));
    TmpCoords4.X := GLStrToFloatDef(copy(Val, 1, pos(';', Val) - 1));
    delete(Val, 1, pos(';', Val));
    TmpCoords4.Y := GLStrToFloatDef(copy(Val, 1, pos(';', Val) - 1));
    delete(Val, 1, pos(';', Val));
    TmpCoords4.Z := GLStrToFloatDef(copy(Val, 1, pos(')', Val) - 1));
  end;
end;

function TGLMaterialScripter.ExtractValue: string;
begin
  ExtractValue := copy(FScript.Strings[Count], pos('=', FScript.Strings[Count])
    + 1, length(FScript.Strings[Count]) - pos('=', FScript.Strings[Count]));
end;

procedure TGLMaterialScripter.XPersistantImage;
begin
  if ClassExists('file') then
  begin
    if (ExtractValue <> '') and (fileexists(ExtractValue)) then
    begin
      with NewMat.Material.Texture.Image as TGLPersistentImage do
        LoadFromFile(ExtractValue);
      NewMat.Material.Texture.Disabled := False;
      if Assigned(FMemo) then
        FMemo.Lines.Add('File loaded : ' + ExtractValue);
    end;
  end;
end;

procedure TGLMaterialScripter.XBlankImage;
begin
  if ClassExists('file') then
  begin
    if (ExtractValue <> '') and (fileexists(ExtractValue)) then
    begin
      with NewMat.Material.Texture.Image as TGLBlankImage do
      // heres the difference
        LoadFromFile(ExtractValue);
      NewMat.Material.Texture.Disabled := False;
      if Assigned(FMemo) then
        FMemo.Lines.Add('File loaded : ' + ExtractValue);
    end;
  end;
end;

procedure TGLMaterialScripter.XPictureFileName;
begin
  if ClassExists('picturefilename') then
    with NewMat.Material.Texture.Image as TGLPicFileImage do
      if fileexists(ExtractValue) then
      begin
        picturefilename := ExtractValue;
        NewMat.Material.Texture.Disabled := False;
      end;
end;

procedure TGLMaterialScripter.XPictureNX;
begin
  if ClassExists('picturenx') then
    if fileexists(ExtractValue) then
      with NewMat.Material.Texture.Image as TGLCubeMapImage do
        Picture[cmtNX].LoadFromFile(ExtractValue);
end;

procedure TGLMaterialScripter.XPictureNY;
begin
  if ClassExists('pictureny') then
    if fileexists(ExtractValue) then
      with NewMat.Material.Texture.Image as TGLCubeMapImage do
        Picture[cmtNY].LoadFromFile(ExtractValue);
end;

procedure TGLMaterialScripter.XPictureNZ;
begin
  if ClassExists('picturenz') then
    if fileexists(ExtractValue) then
      with NewMat.Material.Texture.Image as TGLCubeMapImage do
        Picture[cmtNZ].LoadFromFile(ExtractValue);
end;

procedure TGLMaterialScripter.XPicturePX;
begin
  if ClassExists('picturepx') then
    if fileexists(ExtractValue) then
      with NewMat.Material.Texture.Image as TGLCubeMapImage do
        Picture[cmtPX].LoadFromFile(ExtractValue);
end;

procedure TGLMaterialScripter.XPicturePY;
begin
  if ClassExists('picturepy') then
    if fileexists(ExtractValue) then
      with NewMat.Material.Texture.Image as TGLCubeMapImage do
        Picture[cmtPY].LoadFromFile(ExtractValue);
end;

procedure TGLMaterialScripter.XPicturePZ;
begin
  if ClassExists('picturepz') then
    if fileexists(ExtractValue) then
      with NewMat.Material.Texture.Image as TGLCubeMapImage do
        Picture[cmtPZ].LoadFromFile(ExtractValue);
end;

function TGLMaterialScripter.ValueExists(Value: string): Boolean;
begin
  if UpperCase(TmpStr) = UpperCase(Value) then
    Result := True
  else
    Result := False;
end;

procedure TGLMaterialScripter.XMaterialLibrary;
var
  i: word;
begin
  if ClassExists('materiallibrary') then
    if MaterialLibraries.Count > 0 then
      for i := 0 to MaterialLibraries.Count - 1 do
        if Assigned(MaterialLibraries.Items[i].MaterialLibrary) then
          if UpperCase(MaterialLibraries.Items[i].MaterialLibrary.Name)
            = UpperCase(ExtractValue) then
            NewMat.Material.MaterialLibrary := MaterialLibraries.Items[i]
              .MaterialLibrary;
end;

procedure TGLMaterialScripter.XShader;
var
  i: word;
begin
  if ClassExists('shader') then
    if Shaders.Count > 0 then
      for i := 0 to Shaders.Count - 1 do
        if Assigned(Shaders.Items[i].Shader) then
          if UpperCase(Shaders.Items[i].Shader.Name) = UpperCase(ExtractValue)
          then
            NewMat.Shader := Shaders.Items[i].Shader;
end;

procedure TGLMaterialScripter.ZMaterial;
var
  i: byte;
  exists: Boolean;
begin

  if Assigned(FMaterialLibrary) then
  begin
    NewMat := FMaterialLibrary.Materials.Add;
    repeat

      inc(Count);
      XMaterial;
      if pos('{', FScript.Strings[Count]) > 0 then
        for i := 0 to 2 do
        // need repair : something went wrong, and now we have to check 3 times over :/
        begin
          XTexture;
          XBackProperties;
          XFrontProperties;
        end;
      CheckError;

    until CheckRepeatDone;

    // now we use append and overwrite settings to find out what is what

    TmpStr := NewMat.Name;
    delete(TmpStr, 1, 3); // removes the "TAG" not to confuse the system

    exists := False;
    if FMaterialLibrary.Materials.Count > 0 then
      for i := 0 to FMaterialLibrary.Materials.Count - 1 do
        if TmpStr = FMaterialLibrary.Materials.Items[i].Name then
          exists := True;

    if exists then // does exist
    begin
      if FOverwrite then
      begin
        FMaterialLibrary.Materials.delete
          (FMaterialLibrary.LibMaterialByName(TmpStr).Index);
        NewMat.Name := TmpStr;
      end
      else if FAppend then
      begin
        NewMat.Free;
      end;
    end
    else // doesn't exist
    begin
      NewMat.Name := TmpStr;
      if not FAppend then
        NewMat.Free;
    end;

  end;

end;

/// ////////////////////////
// extraction procedures //
/// ////////////////////////

procedure TGLMaterialScripter.XBackAmbient;
begin
  if ClassExists('ambient') then
  begin
    TmpColor := NewMat.Material.BackProperties.Ambient;
    ExtractColors;
    NewMat.Material.BackProperties.Ambient := TmpColor;
  end;
end;

procedure TGLMaterialScripter.XBackDiffuse;
begin
  if ClassExists('diffuse') then
  begin
    TmpColor := NewMat.Material.BackProperties.Diffuse;
    ExtractColors;
    NewMat.Material.BackProperties.Diffuse := TmpColor;
  end;

end;

procedure TGLMaterialScripter.XBackEmission;
begin
  if ClassExists('emission') then
  begin
    TmpColor := NewMat.Material.BackProperties.Emission;
    ExtractColors;
    NewMat.Material.BackProperties.Emission := TmpColor;
  end;
end;

procedure TGLMaterialScripter.XBackShininess;
begin
  if ClassExists('shininess') then
    if ExtractValue <> '' then
      NewMat.Material.BackProperties.Shininess := strtoint(ExtractValue);
end;

procedure TGLMaterialScripter.XBackSpecular;
begin
  if ClassExists('specular') then
  begin
    TmpColor := NewMat.Material.BackProperties.Specular;
    ExtractColors;
    NewMat.Material.BackProperties.Specular := TmpColor;
  end;
end;

procedure TGLMaterialScripter.XBlendingMode;
begin
  if ClassExists('blendingmode') then
  begin
    TmpStr := ExtractValue;
    if ValueExists('bmOpaque') then
      NewMat.Material.BlendingMode := bmOpaque;
    if ValueExists('bmTransparency') then
      NewMat.Material.BlendingMode := bmTransparency;
    if ValueExists('bmAdditive') then
      NewMat.Material.BlendingMode := bmAdditive;
    if ValueExists('bmAlphaTest100') then
      NewMat.Material.BlendingMode := bmAlphaTest100;
    if ValueExists('bmAlphaTest50') then
      NewMat.Material.BlendingMode := bmAlphaTest50;
  end;
end;

procedure TGLMaterialScripter.XPolygonMode;
begin
  if ClassExists('polygonmode') then
  begin
    TmpStr := ExtractValue;
    if ValueExists('pmFill') then
      NewMat.Material.PolygonMode := pmFill;
    if ValueExists('pmLines') then
      NewMat.Material.PolygonMode := pmLines;
    if ValueExists('pmPoints') then
      NewMat.Material.PolygonMode := pmPoints;
  end;
end;

procedure TGLMaterialScripter.XCompression;
begin
  if ClassExists('compression') then
  begin
    TmpStr := ExtractValue;
    if ValueExists('tcDefault') then
      NewMat.Material.Texture.Compression := tcDefault;
    if ValueExists('tcHighQuality') then
      NewMat.Material.Texture.Compression := tcHighQuality;
    if ValueExists('tcHighSpeed') then
      NewMat.Material.Texture.Compression := tcHighSpeed;
    if ValueExists('tcNone') then
      NewMat.Material.Texture.Compression := tcNone;
    if ValueExists('tcStandard') then
      NewMat.Material.Texture.Compression := tcStandard;
  end;
end;

procedure TGLMaterialScripter.XEnvColor;
begin
  if ClassExists('envcolor') then
  begin
    TmpColor := NewMat.Material.Texture.EnvColor;
    ExtractColors;
    NewMat.Material.Texture.EnvColor := TmpColor;
  end;
end;

procedure TGLMaterialScripter.XFacingCulling;
begin
  if ClassExists('faceculling') then
  begin
    TmpStr := ExtractValue;
    if ValueExists('fcBufferDefault') then
      NewMat.Material.FaceCulling := fcBufferDefault;
    if ValueExists('fcCull') then
      NewMat.Material.FaceCulling := fcCull;
    if ValueExists('fcNoCull') then
      NewMat.Material.FaceCulling := fcNoCull;
  end;
end;

procedure TGLMaterialScripter.XFilteringQuality;
begin
  if ClassExists('filteringquality') then
  begin
    TmpStr := ExtractValue;
    if ValueExists('tfIsotropic') then
      NewMat.Material.Texture.FilteringQuality := tfIsotropic;
    if ValueExists('tfAnisotropic') then
      NewMat.Material.Texture.FilteringQuality := tfAnisotropic;
  end;
end;

procedure TGLMaterialScripter.XFrontAmbient;
begin
  if ClassExists('ambient') then
  begin
    TmpColor := NewMat.Material.frontProperties.Ambient;
    ExtractColors;
    NewMat.Material.frontProperties.Ambient := TmpColor;
  end;
end;

procedure TGLMaterialScripter.XFrontDiffuse;
begin
  if ClassExists('diffuse') then
  begin
    TmpColor := NewMat.Material.frontProperties.Diffuse;
    ExtractColors;
    NewMat.Material.frontProperties.Diffuse := TmpColor;
  end;

end;

procedure TGLMaterialScripter.XFrontEmission;
begin
  if ClassExists('emission') then
  begin
    TmpColor := NewMat.Material.frontProperties.Emission;
    ExtractColors;
    NewMat.Material.frontProperties.Emission := TmpColor;
  end;
end;

procedure TGLMaterialScripter.XFrontShininess;
begin
  if ClassExists('shininess') then
    if ExtractValue <> '' then
      NewMat.Material.frontProperties.Shininess := strtoint(ExtractValue);
end;

procedure TGLMaterialScripter.XFrontSpecular;
begin
  if ClassExists('specular') then
  begin
    TmpColor := NewMat.Material.frontProperties.Specular;
    ExtractColors;
    NewMat.Material.frontProperties.Specular := TmpColor;
  end;
end;

procedure TGLMaterialScripter.XImageAlpha;
begin
  if ClassExists('imagealpha') then
  begin
    TmpStr := ExtractValue;
    if ValueExists('tiaDefault') then
      NewMat.Material.Texture.ImageAlpha := tiaDefault;
    if ValueExists('tiaInverseLuminance') then
      NewMat.Material.Texture.ImageAlpha := tiaInverseLuminance;
    if ValueExists('tiaInverseLuminanceSqrt') then
      NewMat.Material.Texture.ImageAlpha := tiaInverseLuminanceSqrt;
    if ValueExists('tiaLuminance') then
      NewMat.Material.Texture.ImageAlpha := tiaLuminance;
    if ValueExists('tiaLuminanceSqrt') then
      NewMat.Material.Texture.ImageAlpha := tiaLuminanceSqrt;
    if ValueExists('tiaOpaque') then
      NewMat.Material.Texture.ImageAlpha := tiaOpaque;
    if ValueExists('tiaSuperBlackTransparent') then
      NewMat.Material.Texture.ImageAlpha := tiaSuperBlackTransparent;
    if ValueExists('tiaTopLeftPointColorTransparent') then
      NewMat.Material.Texture.ImageAlpha := tiaTopLeftPointColorTransparent;
    if ValueExists('tiaAlphaFromIntensity') then
      NewMat.Material.Texture.ImageAlpha := tiaAlphaFromIntensity;
  end;
end;

procedure TGLMaterialScripter.XImageBrightness;
begin
  if ClassExists('imagebrightness') then
    if ExtractValue <> '' then
      NewMat.Material.Texture.ImageBrightness := GLStrToFloatDef(ExtractValue);
end;

procedure TGLMaterialScripter.XImageGamma;
begin
  if ClassExists('imagegamma') then
    if ExtractValue <> '' then
      NewMat.Material.Texture.ImageGamma := GLStrToFloatDef(ExtractValue);
end;

procedure TGLMaterialScripter.XLibMaterialName;
begin
  if ClassExists('libmaterialname') then
    NewMat.Material.LibMaterialName := ExtractValue;
end;

procedure TGLMaterialScripter.XMagFilter;
begin
  if ClassExists('magfilter') then
  begin
    TmpStr := ExtractValue;
    if ValueExists('maLinear') then
      NewMat.Material.Texture.MagFilter := maLinear;
    if ValueExists('maNearest') then
      NewMat.Material.Texture.MagFilter := maNearest;
  end;
end;

procedure TGLMaterialScripter.XMappingMode;
begin
  if ClassExists('mappingmode') then
  begin
    TmpStr := ExtractValue;
    if ValueExists('tmmUser') then
      NewMat.Material.Texture.MappingMode := tmmUser;
    if ValueExists('tmmCubeMapCamera') then
      NewMat.Material.Texture.MappingMode := tmmCubeMapCamera;
    if ValueExists('tmmCubeMapLight0') then
      NewMat.Material.Texture.MappingMode := tmmCubeMapLight0;
    if ValueExists('tmmCubeMapNormal') then
      NewMat.Material.Texture.MappingMode := tmmCubeMapNormal;
    if ValueExists('tmmCubeMapReflection') then
      NewMat.Material.Texture.MappingMode := tmmCubeMapReflection;
    if ValueExists('tmmEyeLinear') then
      NewMat.Material.Texture.MappingMode := tmmEyeLinear;
    if ValueExists('tmmObjectLinear') then
      NewMat.Material.Texture.MappingMode := tmmObjectLinear;
    if ValueExists('tmmSphere') then
      NewMat.Material.Texture.MappingMode := tmmSphere;
  end;
end;

procedure TGLMaterialScripter.XMappingSCoordinates;
begin
  if ClassExists('mappingscoordinates') then
  begin
    TmpCoords4 := NewMat.Material.Texture.MappingSCoordinates;
    ExtractCoords4;
    NewMat.Material.Texture.MappingSCoordinates := TmpCoords4;
  end;
end;

procedure TGLMaterialScripter.XMappingTCoordinates;
begin
  if ClassExists('mappingtcoordinates') then
  begin
    TmpCoords4 := NewMat.Material.Texture.MappingTCoordinates;
    ExtractCoords4;
    NewMat.Material.Texture.MappingTCoordinates := TmpCoords4;
  end;
end;

procedure TGLMaterialScripter.XMaterialOptions;
var
  a, b: Boolean;
begin
  if ClassExists('materialoptions') then
  begin
    a := False;
    b := False;
    TmpStr := ExtractValue;
    if UpperCase(copy(TmpStr, pos('[', TmpStr) + 1, pos(',', TmpStr) - 2))
      = UpperCase('True') then
      a := True
    else if UpperCase(copy(TmpStr, pos('[', TmpStr) + 1, pos(',', TmpStr) - 2))
      = UpperCase('False') then
      a := False;

    delete(TmpStr, 1, pos(',', TmpStr));

    if UpperCase(copy(TmpStr, 1, pos(']', TmpStr) - 1)) = UpperCase('True') then
      b := True
    else if UpperCase(copy(TmpStr, 1, pos(']', TmpStr) - 1)) = UpperCase('False')
    then
      b := False;

    if a then
      NewMat.Material.MaterialOptions := NewMat.Material.MaterialOptions +
        [moIgnoreFog];
    if b then
      NewMat.Material.MaterialOptions := NewMat.Material.MaterialOptions +
        [moNoLighting];

  end;
end;

procedure TGLMaterialScripter.XMinFilter;
begin
  if ClassExists('minfilter') then
  begin
    TmpStr := ExtractValue;
    if ValueExists('miLinearMipmapLinear') then
      NewMat.Material.Texture.MinFilter := miLinearMipmapLinear;
    if ValueExists('miLinearMipmapNearest') then
      NewMat.Material.Texture.MinFilter := miLinearMipmapNearest;
    if ValueExists('miNearest') then
      NewMat.Material.Texture.MinFilter := miNearest;
    if ValueExists('miNearestMipmapLinear') then
      NewMat.Material.Texture.MinFilter := miNearestMipmapLinear;
    if ValueExists('miNearestMipmapNearest') then
      NewMat.Material.Texture.MinFilter := miNearestMipmapNearest;
    if ValueExists('miLinear') then
      NewMat.Material.Texture.MinFilter := miLinear;
  end;
end;

procedure TGLMaterialScripter.XName;
begin
  if ClassExists('name') then
    NewMat.Name := 'TAG' + ExtractValue;
  // we gonna use for appending and such, quick fix style
end;

procedure TGLMaterialScripter.XNormalMapScale;
begin
  if ClassExists('normalmapscale') then
    if ExtractValue <> '' then
      NewMat.Material.Texture.NormalMapScale := GLStrToFloatDef(ExtractValue);
end;

procedure TGLMaterialScripter.XTexture2Name;
begin
  if ClassExists('texture2name') then
    NewMat.Texture2Name := ExtractValue;
end;

procedure TGLMaterialScripter.XTextureFormat;
begin
  if ClassExists('textureformat') then
  begin
    TmpStr := ExtractValue;
    if ValueExists('tfDefault') then
      NewMat.Material.Texture.TextureFormat := tfDefault;
    if ValueExists('tfIntensity') then
      NewMat.Material.Texture.TextureFormat := tfIntensity;
    if ValueExists('tfLuminance') then
      NewMat.Material.Texture.TextureFormat := tfLuminance;
    if ValueExists('tfLuminanceAlpha') then
      NewMat.Material.Texture.TextureFormat := tfLuminanceAlpha;
    if ValueExists('tfNormalMap') then
      NewMat.Material.Texture.TextureFormat := tfNormalMap;
    if ValueExists('tfRGB') then
      NewMat.Material.Texture.TextureFormat := tfRGB;
    if ValueExists('tfRGB16') then
      NewMat.Material.Texture.TextureFormat := tfRGB16;
    if ValueExists('tfRGBA') then
      NewMat.Material.Texture.TextureFormat := tfRGBA;
    if ValueExists('tfRGBA16') then
      NewMat.Material.Texture.TextureFormat := tfRGBA16;
    if ValueExists('tfAlpha') then
      NewMat.Material.Texture.TextureFormat := tfAlpha;
  end;
end;

procedure TGLMaterialScripter.XTextureMode;
begin
  if ClassExists('texturemode') then
  begin
    TmpStr := ExtractValue;
    if ValueExists('tmDecal') then
      NewMat.Material.Texture.TextureMode := tmDecal;
    if ValueExists('tmModulate') then
      NewMat.Material.Texture.TextureMode := tmModulate;
    if ValueExists('tmReplace') then
      NewMat.Material.Texture.TextureMode := tmReplace;
    if ValueExists('tmBlend') then
      NewMat.Material.Texture.TextureMode := tmBlend;
  end;
end;

procedure TGLMaterialScripter.XTextureOffset;
begin
  if ClassExists('textureoffset') then
  // i hate this, delphi doesn't allow var object reference for procs
  begin
    TmpCoords := NewMat.TextureOffset;
    ExtractCoords3;
    NewMat.TextureOffset := TmpCoords;
  end;
end;

procedure TGLMaterialScripter.XTextureScale;
begin
  if ClassExists('texturescale') then
  begin
    TmpCoords := NewMat.TextureScale;
    ExtractCoords3;
    NewMat.TextureScale := TmpCoords;
  end;
end;

procedure TGLMaterialScripter.XTextureWrap;
begin
  if ClassExists('texturewrap') then
  begin
    TmpStr := ExtractValue;
    if ValueExists('twBoth') then
      NewMat.Material.Texture.TextureWrap := twBoth;
    if ValueExists('twHorizontal') then
      NewMat.Material.Texture.TextureWrap := twHorizontal;
    if ValueExists('twNone') then
      NewMat.Material.Texture.TextureWrap := twNone;
    if ValueExists('twVertical') then
      NewMat.Material.Texture.TextureWrap := twVertical;
  end;
end;

/// ////////////////////////////////////
// sub routines : substr{arguements} //
/// ////////////////////////////////////

procedure TGLMaterialScripter.XTexture;
begin
  if SubstrExists('texture') then
  begin
    if Assigned(FMemo) then
      FMemo.Lines.Add('texture');
    repeat

      inc(Count);
      XCompression;
      XEnvColor;
      XFilteringQuality;
      XImageAlpha;
      XImageBrightness;
      XImageClass;
      XImageGamma;
      XMagFilter;
      XMappingMode;
      XMappingSCoordinates;
      XMappingTCoordinates;
      XMinFilter;
      XNormalMapScale;
      XTextureFormat;
      XTextureMode;
      XTextureWrap;

      CheckError;
    until CheckRepeatDone;
  end;
end;

procedure TGLMaterialScripter.XMaterial;
begin
  XName;
  XShader;
  XTexture2Name;
  XTextureOffset;
  XTextureScale;
  XMaterialOptions;
  XLibMaterialName;
  XBlendingMode;
  XPolygonMode;
  XFacingCulling;
  XMaterialLibrary;
end;

procedure TGLMaterialScripter.XFrontProperties;
begin

  if SubstrExists('frontProperties') then
  begin
    if Assigned(FMemo) then
      FMemo.Lines.Add('frontproperties');
    repeat
      inc(Count);
      XFrontAmbient;
      XFrontDiffuse;
      XFrontEmission;
      XFrontShininess;
      XFrontSpecular;
      CheckError;
    until CheckRepeatDone;
  end;
end;

procedure TGLMaterialScripter.XImageClass;
// reckon this will be most difficult to get right
begin
  if ClassExists('imageclassname') then
  begin
    TmpStr := ExtractValue;
    TmpStr := DeleteSpaces(TmpStr);
    if ValueExists('persistentimage{') then
      repeat
        inc(Count);
        NewMat.Material.Texture.ImageClassName := TGLPersistentImage.ClassName;
        XPersistantImage;
        CheckError;
      until CheckRepeatDone;
    if ValueExists('blankimage{') then
      repeat
        inc(Count);
        NewMat.Material.Texture.ImageClassName := TGLBlankImage.ClassName;
        XBlankImage;
        CheckError;
      until CheckRepeatDone;
    if ValueExists('picfileimage{') then // picturefilename
      repeat
        inc(Count);
        NewMat.Material.Texture.ImageClassName := TGLPicFileImage.ClassName;
        XPictureFileName;
        CheckError;
      until CheckRepeatDone;

    if ValueExists('cubemapimage{') then // px, nx, py, ny, pz, nz
      repeat
        inc(Count);
        NewMat.Material.Texture.ImageClassName := TGLCubeMapImage.ClassName;
        XPicturePX;
        XPictureNX;
        XPicturePY;
        XPictureNY;
        XPicturePZ;
        XPictureNZ;
        NewMat.Material.Texture.Disabled := False;
        CheckError;
      until CheckRepeatDone;
    // procedural noise not supported by GLS.Texture yet
  end;
end;

procedure TGLMaterialScripter.XBackProperties;
begin
  if SubstrExists('BackProperties') then
  begin
    if Assigned(FMemo) then
      FMemo.Lines.Add('backproperties');
    repeat
      inc(Count);
      XBackAmbient;
      XBackDiffuse;
      XBackEmission;
      XBackShininess;
      XBackSpecular;
      CheckError;
    until CheckRepeatDone;
  end;
end;

(* ****************************************
  TGLMaterialLibraryItems
  **************************************** *)

constructor TGLMaterialLibraryItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TGLMaterialLibraryItem);
end;

function TGLMaterialLibraryItems.GetItems(Index: Integer)
  : TGLMaterialLibraryItem;
begin
  Result := TGLMaterialLibraryItem(inherited Items[index]);

end;

procedure TGLMaterialLibraryItems.SetItems(Index: Integer;
  const Val: TGLMaterialLibraryItem);
begin
  inherited Items[index] := Val;
end;

(* ****************************************
  TGLMaterialLibraryItem
  **************************************** *)

procedure TGLMaterialLibraryItem.Assign(Source: TPersistent);
begin
  if Source is TGLMaterialLibraryItem then
  begin
    FMaterialLibrary := TGLMaterialLibraryItem(Source).FMaterialLibrary;
  end;
  inherited Destroy;
end;

constructor TGLMaterialLibraryItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FName := 'MaterialLibrary';
end;

destructor TGLMaterialLibraryItem.Destroy;
begin
  inherited Destroy;
end;

function TGLMaterialLibraryItem.GetDisplayName: String;
begin
  if FName = '' then
    Result := 'MaterialLibrary'
  else
    Result := FName;
end;

procedure TGLMaterialLibraryItem.SetMaterialLibrary
  (const Value: TGLMaterialLibrary);
begin
  if Assigned(Value) then
  begin
    FMaterialLibrary := Value;
    FName := FMaterialLibrary.Name;
  end;
end;

procedure TGLMaterialLibraryItem.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TGLMaterialScripter.SeTGLMaterialLibraryItems
  (const Value: TGLMaterialLibraryItems);
begin
  FMaterialLibraryItems.Assign(Value);
end;

procedure TGLMaterialScripter.SetAppend(const Value: Boolean);
begin
  FAppend := Value;
end;

procedure TGLMaterialScripter.SetOverwrite(const Value: Boolean);
begin
  FOverwrite := Value;
end;

procedure TGLMaterialScripter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FMaterialLibrary then
      FMaterialLibrary := nil
    else if AComponent = FMemo then
      FMemo := nil;
  end;
end;

end.
