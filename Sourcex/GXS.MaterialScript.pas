//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.MaterialScript;

(* Material Script Batch loader for TgxMaterialLibrary for runtime *)

interface

{$I GXS.Scene.inc}

uses
  System.SysUtils,
  System.Classes,
  FMX.StdCtrls,
  FMX.Memo,

  GXS.VectorTypes,
  GXS.Texture,
  GXS.TextureFormat,
  GXS.Graphics,
  GXS.Utils,
  GXS.Color,
  GXS.Coordinates,
  GXS.Material,
  GXS.State;

type
  TgxShaderItem = class(TCollectionItem)
  private
    FShader: TgxShader;
    FName: string;
    procedure SetShader(const Value: TgxShader);
    procedure SetName(const Value: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Shader: TgxShader read FShader write SetShader;
    property Name: string read FName write SetName;
  end;

  TgxShaderItems = class(TOwnedCollection)
  private
    procedure SetItems(Index: Integer; const Val: TgxShaderItem);
    function GetItems(Index: Integer): TgxShaderItem;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TgxShaderItem read GetItems write SetItems; default;
  end;

  TgxMaterialLibraryItem = class(TCollectionItem)
  private
    FMaterialLibrary: TgxMaterialLibrary;
    FName: string;
    procedure SetMaterialLibrary(const Value: TgxMaterialLibrary);
    procedure SetName(const Value: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property MaterialLibrary: TgxMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    property Name: string read FName write SetName;
  end;

  TgxMaterialLibraryItems = class(TOwnedCollection)
  private
    procedure SetItems(Index: Integer; const Val: TgxMaterialLibraryItem);
    function GetItems(Index: Integer): TgxMaterialLibraryItem;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TgxMaterialLibraryItem read GetItems write SetItems; default;
  end;


  TgxMaterialScripter = class(TComponent)
  private
    FShaderItems: TgxShaderItems;
    FMaterialLibraryItems: TgxMaterialLibraryItems;
    FAppend: Boolean;
    FOverwrite: Boolean;
    FScript: TStrings;
    FMemo: TMemo;
    FMaterialLibrary: TgxMaterialLibrary;
    Count: Longint;
    infini: Longint;
    done: Boolean;
    NewMat: TgxLibMaterial;
    tmpcoords: TgxCoordinates;
    tmpcolor: TgxColor;
    tmpcoords4: TgxCoordinates4;
    tmpstr: string;
    procedure SeTgxShaderItems(const Value: TgxShaderItems);
    procedure SeTgxMaterialLibraryItems(const Value: TgxMaterialLibraryItems);
    procedure SetAppend(const Value: Boolean);
    procedure SetOverwrite(const Value: Boolean);
    procedure SetScript(const Value: TStrings);
    procedure SetMaterialLibrary(const Value: TgxMaterialLibrary);
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
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property DebugMemo: TMemo read FMemo write SetMemo;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CompileScript;
  published
    property Script: TStrings read FScript write SetScript;
    property MaterialLibrary: TgxMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    property Shaders: TgxShaderItems read FShaderItems write SeTgxShaderItems;
    property MaterialLibraries: TgxMaterialLibraryItems read FMaterialLibraryItems write SeTgxMaterialLibraryItems;
    property AppendToMaterialLibrary: Boolean read FAppend write SetAppend;
    property OverwriteToMaterialLibrary: Boolean read FOverwrite write SetOverwrite;
  end;

//-----------------------------------------------------------
implementation
//-----------------------------------------------------------

procedure TgxShaderItem.SetShader(const Value: TgxShader);
begin
  if assigned(Value) then
  begin
     FShader := Value;
     FName := FShader.Name;
  end;
end;

procedure TgxShaderItem.Assign(Source: TPersistent);
begin
  if Source is TgxShaderItem then
  begin
     FShader := TgxShaderItem(Source).FShader;
  end;
  inherited Destroy;
end;

constructor TgxShaderItem.Create(Collection: TCollection);
begin
   inherited Create(Collection);
   FName := 'Shader';
end;

destructor TgxShaderItem.Destroy;
begin
  inherited Destroy;
end;

function TgxShaderItem.GetDisplayName : String;
begin
   if FName = '' then
   Result:='Shader'
   else
   Result := FName;
end;

{ TgxShaderItems }

constructor TgxShaderItems.Create(AOwner: TPersistent);
begin
   inherited Create(AOwner, TgxShaderItem);
end;

function TgxShaderItems.GetItems(index: Integer): TgxShaderItem;
begin
   Result:=TgxShaderItem(inherited Items[index]);
end;


procedure TgxShaderItems.SetItems(index: Integer; const val: TgxShaderItem);
begin
   inherited Items[index]:=val;
end;

procedure TgxMaterialScripter.SeTgxShaderItems(const Value: TgxShaderItems);
begin
   FShaderItems.Assign(Value);
end;

procedure TgxShaderItem.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TgxMaterialScripter.CompileScript;
begin

   done := false;
   NewMat := nil;
   count := 0;
   infini := 0;
   tmpcoords := nil;
   tmpcoords4 := nil;
   tmpcolor := nil;
   tmpstr := '';

   repeat

      inc(count);

      if pos('{',FScript.Strings[count]) > 0 then
      begin

         if substrexists('material') then ZMaterial;

      end;
      checkerror;

   until checkrepeatdone;

end;

procedure TgxMaterialScripter.SetMaterialLibrary(
  const Value: TgxMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if FMaterialLibrary <> nil then FMaterialLibrary.FreeNotification(Self);
end;

procedure TgxMaterialScripter.SetMemo(const Value: TMemo);
begin
  if FMemo <> nil then FMemo.RemoveFreeNotification(Self);
  FMemo := Value;
  if FMemo <> nil then FMemo.FreeNotification(Self);
end;

procedure TgxMaterialScripter.SetScript(const Value: TStrings);
begin
  if assigned(value) then
  FScript.Assign(Value);
end;

procedure TgxMaterialScripter.CheckError;
begin
   if count >= FScript.Count then done := true;
   if done then raise exception.Create('User Error : No closing "}"');
   inc(infini);
   if infini > 1280000 then
   begin
      raise exception.Create('Internal Error : Infinate Loop');
      done := true;
      exit;
   end;
end;

function TgxMaterialScripter.CheckRepeatDone: boolean;
begin
   checkrepeatdone := false;
   if pos('}',FScript.Strings[count]) > 0 then
   begin
      checkrepeatdone := true;
      inc(count);
   end;

   if done then checkrepeatdone := true;
end;

function TgxMaterialScripter.ClassExists(arguement: string): boolean;
var temp : string;
    i : word;
begin

   classexists := false;
   if (pos(uppercase(arguement), uppercase(FScript.Strings[count])) > 0) and // check if there is an arguement
      (pos('=', FScript.Strings[count]) > pos(uppercase(arguement), uppercase(FScript.Strings[count])))and // check if it is before '='
      (pos('=', FScript.Strings[count]) > 0) then // check if there even is a '='
      begin

         temp := FScript.Strings[count];
         for i := 0 to length(temp) do
            if pos(' ', temp) = 1 then
            delete(temp,1,1);

         if pos(uppercase(arguement),uppercase(temp)) = 1 then
         if (temp[length(arguement) + 1] = ' ') or (temp[length(arguement) + 1] = '=') then
         begin
            classexists := true;
            if assigned(FMemo) then Fmemo.Lines.Add('Stage is at : ' + arguement);
         end;
      end;

end;
function TgxMaterialScripter.SubstrExists(substr: string): boolean;
begin
   if pos(uppercase(substr),uppercase(FScript.Strings[count])) > 0 then result := true
   else
   result := false;
end;


constructor TgxMaterialScripter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScript := TStringList.Create;
  FShaderItems:=TgxShaderItems.Create(Self);
  FMaterialLibraryItems:=TgxMaterialLibraryItems.Create(Self);
  FAppend := true;
  FOverwrite := false;
end;

function TgxMaterialScripter.DeleteSpaces(value: string): string;
var i : byte;
begin
   result := value;

   for i := 0 to length(result) do
   if pos(' ',result) > 0 then
   delete(result,pos(' ',result), 1);
end;

destructor TgxMaterialScripter.Destroy;
begin
  FShaderItems.Free;
  FMaterialLibraryItems.Free;
  FScript.Free;
  inherited Destroy;
end;

procedure TgxMaterialScripter.ExtractColors;
var val : string;
begin
   val := Extractvalue;
   if pos('(',val) > 0 then
   begin
      tmpcolor.Alpha := GXS.Utils.StrToFloatDef(copy(val, pos('(',val) + 1, pos(';',val) - 2));
      delete(val,1,pos(';',val));
      tmpcolor.Red := GXS.Utils.StrToFloatDef(copy(val, 1, pos(';',val) - 1));
      delete(val,1,pos(';',val));
      tmpcolor.Green := GXS.Utils.StrToFloatDef(copy(val, 1, pos(';',val) - 1));
      delete(val,1,pos(';',val));
      tmpcolor.Blue := GXS.Utils.StrToFloatDef(copy(val, 1, pos(')',val) - 1));
   end;
end;

procedure TgxMaterialScripter.ExtractCoords3;
var val : string;
begin
   val := Extractvalue;
   if pos('(',val) > 0 then
   begin
      tmpcoords.X := GXS.Utils.StrToFloatDef(copy(val, pos('(',val) + 1, pos(';',val) - 2));
      delete(val,1,pos(';',val));
      tmpcoords.Y := GXS.Utils.StrToFloatDef(copy(val, 1, pos(';',val) - 1));
      delete(val,1,pos(';',val));
      tmpcoords.Z := GXS.Utils.StrToFloatDef(copy(val, 1, pos(')',val) - 1));
   end;
end;

procedure TgxMaterialScripter.ExtractCoords4;
var val : string;
begin
   val := Extractvalue;
   if pos('(',val) > 0 then
   begin
      tmpcoords4.W := GXS.Utils.StrToFloatDef(copy(val, pos('(',val) + 1, pos(';',val) - 2));
      delete(val,1,pos(';',val));
      tmpcoords4.X := GXS.Utils.StrToFloatDef(copy(val, 1, pos(';',val) - 1));
      delete(val,1,pos(';',val));
      tmpcoords4.Y := GXS.Utils.StrToFloatDef(copy(val, 1, pos(';',val) - 1));
      delete(val,1,pos(';',val));
      tmpcoords4.Z := GXS.Utils.StrToFloatDef(copy(val, 1, pos(')',val) - 1));
   end;
end;

function TgxMaterialScripter.ExtractValue: string;
begin
   extractvalue := copy(FScript.Strings[count], pos('=',FScript.Strings[count]) + 1, length(FScript.Strings[count]) - pos('=',FScript.Strings[count]));
end;
procedure TgxMaterialScripter.XPersistantImage;
begin
   if classexists('file') then
   begin
      if (extractvalue <> '') and (fileexists(extractvalue)) then
      begin
         with NewMat.Material.Texture.Image as TgxPersistentImage do
            LoadFromFile(extractvalue);
         NewMat.Material.Texture.Disabled := false;
         if assigned(FMemo) then FMemo.Lines.Add('File loaded : ' + extractvalue);
      end;
   end;
end;
procedure TgxMaterialScripter.XBlankImage;
begin
   if classexists('file') then
   begin
      if (extractvalue <> '') and (fileexists(extractvalue)) then
      begin
         with NewMat.Material.Texture.Image as TgxBlankImage do // heres the difference
            LoadFromFile(extractvalue);
         NewMat.Material.Texture.Disabled := false;
         if assigned(FMemo) then FMemo.Lines.Add('File loaded : ' + extractvalue);
      end;
   end;
end;

procedure TgxMaterialScripter.XPictureFileName;
begin
   if classexists('picturefilename') then
      with NewMat.Material.Texture.Image as TgxPicFileImage do
         if fileexists(extractvalue) then
         begin
            picturefilename := extractvalue;
            NewMat.Material.Texture.Disabled := false;
         end;
end;

procedure TgxMaterialScripter.XPictureNX;
begin
   if classexists('picturenx') then
      if fileexists(extractvalue) then
         with NewMat.Material.Texture.Image as TgxCubeMapImage do
         Picture[cmtNX].Bitmap.LoadFromFile(extractvalue);
end;

procedure TgxMaterialScripter.XPictureNY;
begin
   if classexists('pictureny') then
      if fileexists(extractvalue) then
         with NewMat.Material.Texture.Image as TgxCubeMapImage do
         Picture[cmtNY].Bitmap.LoadFromFile(extractvalue);
end;

procedure TgxMaterialScripter.XPictureNZ;
begin
   if classexists('picturenz') then
      if fileexists(extractvalue) then
         with NewMat.Material.Texture.Image as TgxCubeMapImage do
         Picture[cmtNZ].Bitmap.LoadFromFile(extractvalue);
end;

procedure TgxMaterialScripter.XPicturePX;
begin
   if classexists('picturepx') then
      if fileexists(extractvalue) then
         with NewMat.Material.Texture.Image as TgxCubeMapImage do
         Picture[cmtPX].Bitmap.LoadFromFile(extractvalue);
end;

procedure TgxMaterialScripter.XPicturePY;
begin
   if classexists('picturepy') then
      if fileexists(extractvalue) then
         with NewMat.Material.Texture.Image as TgxCubeMapImage do
         Picture[cmtPY].Bitmap.LoadFromFile(extractvalue);
end;

procedure TgxMaterialScripter.XPicturePZ;
begin
   if classexists('picturepz') then
      if fileexists(extractvalue) then
         with NewMat.Material.Texture.Image as TgxCubeMapImage do
         Picture[cmtPZ].Bitmap.LoadFromFile(extractvalue);
end;

function TgxMaterialScripter.ValueExists(value: string): boolean;
begin
   if uppercase(tmpstr) = uppercase(value) then result := true
   else
   result := false;
end;

procedure TgxMaterialScripter.XMaterialLibrary;
var i : word;
begin
   if classexists('materiallibrary') then
   if MaterialLibraries.count > 0 then
      for i := 0 to MaterialLibraries.Count - 1 do
         if assigned(MaterialLibraries.Items[i].MaterialLibrary) then
            if uppercase(MaterialLibraries.Items[i].MaterialLibrary.Name) = uppercase(extractvalue) then
            NewMat.Material.MaterialLibrary := MaterialLibraries.Items[i].MaterialLibrary;
end;

procedure TgxMaterialScripter.XShader;
var i : word;
begin
   if classexists('shader') then
   if Shaders.count > 0 then
      for i := 0 to Shaders.Count - 1 do
         if assigned(Shaders.Items[i].Shader) then
            if uppercase(Shaders.Items[i].Shader.Name) = uppercase(extractvalue) then
            NewMat.Shader := Shaders.Items[i].Shader;
end;

procedure TgxMaterialScripter.ZMaterial;
var i : byte;
    exists : boolean;
begin

   if assigned(FMaterialLibrary) then
   begin
      NewMat := FMaterialLibrary.Materials.Add;
      repeat

         inc(count);
         XMaterial;
         if pos('{',FScript.Strings[count]) > 0 then
         for i := 0 to 2 do // need repair : something went wrong, and now we have to check 3 times over :/
         begin
            XTexture;
            XBackProperties;
            XFrontProperties;
         end;
         checkerror;

      until checkrepeatdone;

      // now we use append and overwrite settings to find out what is what

      tmpstr := NewMat.Name;
      delete(tmpstr,1,3); // removes the "TAG" not to confuse the system

      exists := false;
      if FMaterialLibrary.Materials.Count > 0 then
      for i := 0 to FMaterialLibrary.Materials.Count - 1 do
      if tmpstr = FMaterialLibrary.Materials.Items[i].Name then
         exists := true;

      if Exists then // does exist
      begin
         if FOverwrite then
         begin
            FMaterialLibrary.Materials.Delete(FMaterialLibrary.LibMaterialByName(tmpstr).Index);
            NewMat.Name := tmpstr;
         end
         else
         if FAppend then
         begin
            NewMat.Free;
         end;
      end
      else           // doesn't exist
      begin
         NewMat.Name := tmpstr;
         if not FAppend then
         NewMat.Free;
      end;

   end;

end;

///////////////////////////
// extraction procedures //
///////////////////////////

procedure TgxMaterialScripter.XBackAmbient;
begin
   if classexists('ambient') then
   begin
      tmpcolor := NewMat.Material.BackProperties.Ambient;
      extractcolors;
      NewMat.Material.BackProperties.Ambient := tmpcolor;
   end;
end;

procedure TgxMaterialScripter.XBackDiffuse;
begin
   if classexists('diffuse') then
   begin
      tmpcolor := NewMat.Material.BackProperties.Diffuse;
      extractcolors;
      NewMat.Material.BackProperties.Diffuse := tmpcolor;
   end;

end;

procedure TgxMaterialScripter.XBackEmission;
begin
   if classexists('emission') then
   begin
      tmpcolor := NewMat.Material.BackProperties.Emission;
      extractcolors;
      NewMat.Material.BackProperties.Emission := tmpcolor;
   end;
end;


procedure TgxMaterialScripter.XBackShininess;
begin
   if classexists('shininess') then
   if extractvalue <> '' then
      NewMat.Material.BackProperties.Shininess := strtoint(extractvalue);
end;

procedure TgxMaterialScripter.XBackSpecular;
begin
   if classexists('specular') then
   begin
      tmpcolor := NewMat.Material.BackProperties.Specular;
      extractcolors;
      NewMat.Material.BackProperties.Specular := tmpcolor;
   end;
end;

procedure TgxMaterialScripter.XBlendingMode;
begin
   if classexists('blendingmode') then
   begin
      tmpstr := extractvalue;
      if valueexists('bmOpaque') then Newmat.Material.BlendingMode := bmOpaque;
      if valueexists('bmTransparency') then Newmat.Material.BlendingMode := bmTransparency;
      if valueexists('bmAdditive') then Newmat.Material.BlendingMode := bmAdditive;
      if valueexists('bmAlphaTest100') then Newmat.Material.BlendingMode := bmAlphaTest100;
      if valueexists('bmAlphaTest50') then Newmat.Material.BlendingMode := bmAlphaTest50;
   end;
end;

procedure TgxMaterialScripter.XPolygonMode;
begin
   if classexists('polygonmode') then
   begin
      tmpstr := extractvalue;
      if valueexists('pmFill') then Newmat.Material.PolygonMode := pmFill;
      if valueexists('pmLines') then Newmat.Material.PolygonMode := pmLines;
      if valueexists('pmPoints') then Newmat.Material.PolygonMode := pmPoints;
   end;
end;

procedure TgxMaterialScripter.XCompression;
begin
   if classexists('compression') then
   begin
      tmpstr := extractvalue;
      if valueexists('tcDefault') then Newmat.Material.Texture.Compression := tcDefault;
      if valueexists('tcHighQuality') then Newmat.Material.Texture.Compression := tcHighQuality;
      if valueexists('tcHighSpeed') then Newmat.Material.Texture.Compression := tcHighSpeed;
      if valueexists('tcNone') then Newmat.Material.Texture.Compression := tcNone;
      if valueexists('tcStandard') then Newmat.Material.Texture.Compression := tcStandard;
   end;
end;

procedure TgxMaterialScripter.XEnvColor;
begin
   if classexists('envcolor') then
   begin
      tmpcolor := NewMat.Material.Texture.EnvColor;
      extractcolors;
      NewMat.Material.Texture.EnvColor := tmpcolor;
   end;
end;

procedure TgxMaterialScripter.XFacingCulling;
begin
   if classexists('faceculling') then
   begin
      tmpstr := extractvalue;
      if valueexists('fcBufferDefault') then Newmat.Material.FaceCulling := fcBufferDefault;
      if valueexists('fcCull') then Newmat.Material.FaceCulling := fcCull;
      if valueexists('fcNoCull') then Newmat.Material.FaceCulling := fcNoCull;
   end;
end;

procedure TgxMaterialScripter.XFilteringQuality;
begin
   if classexists('filteringquality') then
   begin
      tmpstr := extractvalue;
      if valueexists('tfIsotropic') then Newmat.Material.Texture.FilteringQuality := tfIsotropic;
      if valueexists('tfAnisotropic') then Newmat.Material.Texture.FilteringQuality := tfAnisotropic;
   end;
end;


procedure TgxMaterialScripter.XfrontAmbient;
begin
   if classexists('ambient') then
   begin
      tmpcolor := NewMat.Material.frontProperties.Ambient;
      extractcolors;
      NewMat.Material.frontProperties.Ambient := tmpcolor;
   end;
end;

procedure TgxMaterialScripter.XfrontDiffuse;
begin
   if classexists('diffuse') then
   begin
      tmpcolor := NewMat.Material.frontProperties.Diffuse;
      extractcolors;
      NewMat.Material.frontProperties.Diffuse := tmpcolor;
   end;

end;

procedure TgxMaterialScripter.XfrontEmission;
begin
   if classexists('emission') then
   begin
      tmpcolor := NewMat.Material.frontProperties.Emission;
      extractcolors;
      NewMat.Material.frontProperties.Emission := tmpcolor;
   end;
end;

procedure TgxMaterialScripter.XfrontShininess;
begin
   if classexists('shininess') then
   if extractvalue <> '' then
      NewMat.Material.frontProperties.Shininess := strtoint(extractvalue);
end;

procedure TgxMaterialScripter.XfrontSpecular;
begin
   if classexists('specular') then
   begin
      tmpcolor := NewMat.Material.frontProperties.Specular;
      extractcolors;
      NewMat.Material.frontProperties.Specular := tmpcolor;
   end;
end;


procedure TgxMaterialScripter.XImageAlpha;
begin
   if classexists('imagealpha') then
   begin
      tmpstr := extractvalue;
      if valueexists('tiaDefault') then Newmat.Material.Texture.ImageAlpha := tiaDefault;
      if valueexists('tiaInverseLuminance') then Newmat.Material.Texture.ImageAlpha := tiaInverseLuminance;
      if valueexists('tiaInverseLuminanceSqrt') then Newmat.Material.Texture.ImageAlpha := tiaInverseLuminanceSqrt;
      if valueexists('tiaLuminance') then Newmat.Material.Texture.ImageAlpha := tiaLuminance;
      if valueexists('tiaLuminanceSqrt') then Newmat.Material.Texture.ImageAlpha := tiaLuminanceSqrt;
      if valueexists('tiaOpaque') then Newmat.Material.Texture.ImageAlpha := tiaOpaque;
      if valueexists('tiaSuperBlackTransparent') then Newmat.Material.Texture.ImageAlpha := tiaSuperBlackTransparent;
      if valueexists('tiaTopLeftPointColorTransparent') then Newmat.Material.Texture.ImageAlpha := tiaTopLeftPointColorTransparent;
      if valueexists('tiaAlphaFromIntensity') then Newmat.Material.Texture.ImageAlpha := tiaAlphaFromIntensity;
   end;
end;

procedure TgxMaterialScripter.XImageBrightness;
begin
   if classexists('imagebrightness') then
   if extractvalue <> '' then
      NewMat.Material.Texture.ImageBrightness := GXS.Utils.StrToFloatDef(extractvalue);
end;


procedure TgxMaterialScripter.XImageGamma;
begin
   if classexists('imagegamma') then
   if extractvalue <> '' then
      NewMat.Material.Texture.ImageGamma := GXS.Utils.StrToFloatDef(extractvalue);
end;

procedure TgxMaterialScripter.XLibMaterialName;
begin
   if classexists('libmaterialname') then NewMat.Material.LibMaterialName := extractvalue;
end;

procedure TgxMaterialScripter.XMagFilter;
begin
   if classexists('magfilter') then
   begin
      tmpstr := extractvalue;
      if valueexists('maLinear') then Newmat.Material.Texture.MagFilter := maLinear;
      if valueexists('maNearest') then Newmat.Material.Texture.MagFilter := maNearest;
   end;
end;

procedure TgxMaterialScripter.XMappingMode;
begin
   if classexists('mappingmode') then
   begin
      tmpstr := extractvalue;
      if valueexists('tmmUser') then Newmat.Material.Texture.MappingMode := tmmUser;
      if valueexists('tmmCubeMapCamera') then Newmat.Material.Texture.MappingMode := tmmCubeMapCamera;
      if valueexists('tmmCubeMapLight0') then Newmat.Material.Texture.MappingMode := tmmCubeMapLight0;
      if valueexists('tmmCubeMapNormal') then Newmat.Material.Texture.MappingMode := tmmCubeMapNormal;
      if valueexists('tmmCubeMapReflection') then Newmat.Material.Texture.MappingMode := tmmCubeMapReflection;
      if valueexists('tmmEyeLinear') then Newmat.Material.Texture.MappingMode := tmmEyeLinear;
      if valueexists('tmmObjectLinear') then Newmat.Material.Texture.MappingMode := tmmObjectLinear;
      if valueexists('tmmSphere') then Newmat.Material.Texture.MappingMode := tmmSphere;
   end;
end;

procedure TgxMaterialScripter.XMappingSCoordinates;
begin
   if classexists('mappingscoordinates') then
   begin
      tmpcoords4 := NewMat.Material.Texture.MappingSCoordinates;
      extractcoords4;
      NewMat.Material.Texture.MappingSCoordinates := tmpcoords4;
   end;
end;

procedure TgxMaterialScripter.XMappingTCoordinates;
begin
   if classexists('mappingtcoordinates') then
   begin
      tmpcoords4 := NewMat.Material.Texture.MappingTCoordinates;
      extractcoords4;
      NewMat.Material.Texture.MappingTCoordinates := tmpcoords4;
   end;
end;


procedure TgxMaterialScripter.XMaterialOptions;
var a,b : boolean;
begin
   if classexists('materialoptions') then
   begin
      a := false;
      b := false;
      tmpstr := extractvalue;
      if uppercase(copy(tmpstr, pos('[',tmpstr) + 1, pos(',',tmpstr) - 2)) = uppercase('true') then a := true
      else
      if uppercase(copy(tmpstr, pos('[',tmpstr) + 1, pos(',',tmpstr) - 2)) = uppercase('false') then a := false;

      delete(tmpstr,1,pos(',',tmpstr));

      if uppercase(copy(tmpstr, 1, pos(']',tmpstr) - 1)) = uppercase('true') then b := true
      else
      if uppercase(copy(tmpstr, 1, pos(']',tmpstr) - 1)) = uppercase('false') then b := false;

      if a then NewMat.Material.MaterialOptions := NewMat.Material.MaterialOptions + [moIgnoreFog];
      if b then NewMat.Material.MaterialOptions := NewMat.Material.MaterialOptions + [moNoLighting];

   end;
end;

procedure TgxMaterialScripter.XMinFilter;
begin
   if classexists('minfilter') then
   begin
      tmpstr := extractvalue;
      if valueexists('miLinearMipmapLinear') then Newmat.Material.Texture.MinFilter := miLinearMipmapLinear;
      if valueexists('miLinearMipmapNearest') then Newmat.Material.Texture.MinFilter := miLinearMipmapNearest;
      if valueexists('miNearest') then Newmat.Material.Texture.MinFilter := miNearest;
      if valueexists('miNearestMipmapLinear') then Newmat.Material.Texture.MinFilter := miNearestMipmapLinear;
      if valueexists('miNearestMipmapNearest') then Newmat.Material.Texture.MinFilter := miNearestMipmapNearest;
      if valueexists('miLinear') then Newmat.Material.Texture.MinFilter := miLinear;
   end;
end;

procedure TgxMaterialScripter.XName;
begin
   if classexists('name') then NewMat.Name := 'TAG' + Extractvalue; // we gonna use for appending and such, quick fix style
end;

procedure TgxMaterialScripter.XNormalMapScale;
begin
   if classexists('normalmapscale') then
   if extractvalue <> '' then
      NewMat.Material.Texture.NormalMapScale := GXS.Utils.StrToFloatDef(extractvalue);
end;


procedure TgxMaterialScripter.XTexture2Name;
begin
   if classexists('texture2name') then NewMat.Texture2Name := ExtractValue;
end;

procedure TgxMaterialScripter.XTextureFormat;
begin
   if classexists('textureformat') then
   begin
      tmpstr := extractvalue;
      if valueexists('tfDefault') then Newmat.Material.Texture.TextureFormat := tfDefault;
      if valueexists('tfIntensity') then Newmat.Material.Texture.TextureFormat := tfIntensity;
      if valueexists('tfLuminance') then Newmat.Material.Texture.TextureFormat := tfLuminance;
      if valueexists('tfLuminanceAlpha') then Newmat.Material.Texture.TextureFormat := tfLuminanceAlpha;
      if valueexists('tfNormalMap') then Newmat.Material.Texture.TextureFormat := tfNormalMap;
      if valueexists('tfRGB') then Newmat.Material.Texture.TextureFormat := tfRGB;
      if valueexists('tfRGB16') then Newmat.Material.Texture.TextureFormat := tfRGB16;
      if valueexists('tfRGBA') then Newmat.Material.Texture.TextureFormat := tfRGBA;
      if valueexists('tfRGBA16') then Newmat.Material.Texture.TextureFormat := tfRGBA16;
      if valueexists('tfAlpha') then Newmat.Material.Texture.TextureFormat := tfAlpha;
   end;
end;

procedure TgxMaterialScripter.XTextureMode;
begin
   if classexists('texturemode') then
   begin
      tmpstr := extractvalue;
      if valueexists('tmDecal') then Newmat.Material.Texture.TextureMode := tmDecal;
      if valueexists('tmModulate') then Newmat.Material.Texture.TextureMode := tmModulate;
      if valueexists('tmReplace') then Newmat.Material.Texture.TextureMode := tmReplace;
      if valueexists('tmBlend') then Newmat.Material.Texture.TextureMode := tmBlend;
   end;
end;

procedure TgxMaterialScripter.XTextureOffset;
begin
   if classexists('textureoffset') then // i hate this, delphi doesn't allow var object reference for procs
   begin
      tmpcoords := Newmat.TextureOffset;
      extractcoords3;
      Newmat.TextureOffset := tmpcoords;
   end;
end;

procedure TgxMaterialScripter.XTextureScale;
begin
   if classexists('texturescale') then
   begin
      tmpcoords := Newmat.TextureScale;
      extractcoords3;
      NewMat.TextureScale := tmpcoords;
   end;
end;

procedure TgxMaterialScripter.XTextureWrap;
begin
   if classexists('texturewrap') then
   begin
      tmpstr := extractvalue;
      if valueexists('twBoth') then Newmat.Material.Texture.TextureWrap := twBoth;
      if valueexists('twHorizontal') then Newmat.Material.Texture.TextureWrap := twHorizontal;
      if valueexists('twNone') then Newmat.Material.Texture.TextureWrap := twNone;
      if valueexists('twVertical') then Newmat.Material.Texture.TextureWrap := twVertical;
   end;
end;

///////////////////////////////////////
// sub routines : substr{arguements} //
///////////////////////////////////////

procedure TgxMaterialScripter.XTexture;
begin
   if substrexists('texture') then
   begin
      if assigned(FMemo) then FMemo.Lines.Add('texture');
      repeat

         inc(count);
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

         checkerror;
      until checkrepeatdone;
   end;
end;

procedure TgxMaterialScripter.XMaterial;
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


procedure TgxMaterialScripter.XfrontProperties;
begin

   if substrexists('frontProperties') then
   begin
      if assigned(FMemo) then FMemo.Lines.Add('frontproperties');

      repeat

         inc(count);

         XfrontAmbient;
         XfrontDiffuse;
         XfrontEmission;
         XfrontShininess;
         XfrontSpecular;

         checkerror;

      until checkrepeatdone;
   end;
end;

procedure TgxMaterialScripter.XImageClass; // reckon this will be most difficult to get right
begin
   if classexists('imageclassname') then
   begin

      tmpstr := extractvalue;
      tmpstr := deletespaces(tmpstr);

      if valueexists('persistentimage{') then // loadfromfile
      repeat
         inc(count);
         Newmat.Material.Texture.ImageClassName := TgxPersistentImage.ClassName;
         XPersistantImage;
         checkerror;
      until checkrepeatdone;

      if valueexists('blankimage{') then // loadfromfile
      repeat
         inc(count);
         Newmat.Material.Texture.ImageClassName := TgxBlankImage.ClassName;
         XBlankImage;
         checkerror;
      until checkrepeatdone;

      if valueexists('picfileimage{') then //picturefilename
      repeat
         inc(count);
         Newmat.Material.Texture.ImageClassName := TgxPicFileImage.ClassName;
         XPictureFilename;
         checkerror;
      until checkrepeatdone;

      if valueexists('cubemapimage{') then  // px, nx, py, ny, pz, nz
      repeat
         inc(count);
         Newmat.Material.Texture.ImageClassName := TgxCubeMapImage.ClassName;
         XPicturePX;
         XPictureNX;
         XPicturePY;
         XPictureNY;
         XPicturePZ;
         XPictureNZ;
         NewMat.Material.Texture.Disabled := false;
         checkerror;
      until checkrepeatdone;

      // procedural noise not supported by GXS.Texture yet
   end;
end;

procedure TgxMaterialScripter.XBackProperties;
begin

   if substrexists('BackProperties') then
   begin
      if assigned(FMemo) then FMemo.Lines.Add('backproperties');

      repeat

         inc(count);

         XBackAmbient;
         XBackDiffuse;
         XBackEmission;
         XBackShininess;
         XBackSpecular;

         checkerror;

      until checkrepeatdone;
   end;
end;


{ TgxMaterialLibraryItems }

constructor TgxMaterialLibraryItems.Create(AOwner: TPersistent);
begin
   inherited Create(AOwner, TgxMaterialLibraryItem);
end;

function TgxMaterialLibraryItems.GetItems(index: Integer): TgxMaterialLibraryItem;
begin
   Result:=TgxMaterialLibraryItem(inherited Items[index]);

end;

procedure TgxMaterialLibraryItems.SetItems(index: Integer;
  const val: TgxMaterialLibraryItem);
begin
   inherited Items[index]:=val;
end;

{ TgxMaterialLibraryItem }

procedure TgxMaterialLibraryItem.Assign(Source: TPersistent);
begin
  if Source is TgxMaterialLibraryItem then
  begin
     FMaterialLibrary := TgxMaterialLibraryItem(Source).FMaterialLibrary;
  end;
  inherited Destroy;
end;

constructor TgxMaterialLibraryItem.Create(Collection: TCollection);
begin
   inherited Create(Collection);
   FName := 'MaterialLibrary';
end;

destructor TgxMaterialLibraryItem.Destroy;
begin
  inherited Destroy;
end;

function TgxMaterialLibraryItem.GetDisplayName: String;
begin
   if FName = '' then
   Result:='MaterialLibrary'
   else
   Result := FName;
end;

procedure TgxMaterialLibraryItem.SetMaterialLibrary(
  const Value: TgxMaterialLibrary);
begin
  if assigned(Value) then
  begin
     FMaterialLibrary := Value;
     FName := FMaterialLibrary.Name;
  end;
end;

procedure TgxMaterialLibraryItem.SetName(const Value: String);
begin
   FName := Value;
end;

procedure TgxMaterialScripter.SeTgxMaterialLibraryItems(
  const Value: TgxMaterialLibraryItems);
begin
   FMaterialLibraryItems.Assign(Value);
end;

procedure TgxMaterialScripter.SetAppend(const Value: boolean);
begin
  FAppend := Value;
end;

procedure TgxMaterialScripter.SetOverwrite(const Value: boolean);
begin
  FOverwrite := Value;
end;

procedure TgxMaterialScripter.Notification(AComponent: TComponent;
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