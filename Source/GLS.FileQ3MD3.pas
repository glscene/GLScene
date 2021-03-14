//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.FileQ3MD3;

(* Helper classes and methods for Quake3 MD3 actors *)

interface

uses
  System.Classes,
  System.SysUtils,

  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.VectorFileObjects,
  GLS.VectorLists,
  GLS.Material,
  GLS.PersistentClasses,
  Formats.MD3;

type
  (* This class is used to extract the tag transform information
    stored in the MD3 files. The data is used to offset each
    part of the model based on the parent parts animation state. *)
  TMD3TagList = class
  private
    FTags: array of TMD3Tag;
    FNumTags, FNumFrames: Integer;
    function GetTag(index: Integer): TMD3Tag;
  public
    procedure LoadFromFile(const FileName: String);
    procedure LoadFromStream(AStream: TStream);
    function GetTransform(const TagName: string; Frame: Integer): TGLMatrix;
    property TagCount: Integer read FNumTags;
    property FrameCount: Integer read FNumFrames;
    property Tags[index: Integer]: TMD3Tag read GetTag;
  end;

(* These procedures are helpers to load the Quake3 animation file data
   into an animation list. The NamePrefix parameter is used to determine
   which class of animation is extracted. eg NamePrefix='TORSO' will load
   all animations starting with 'TORSO_' like 'TORSO_STAND' *)
procedure LoadQ3Anims(Animations: TGLActorAnimations; const FileName: string;
  const NamePrefix: string); overload;
procedure LoadQ3Anims(Animations: TGLActorAnimations; Strings: TStrings;
  const NamePrefix: string); overload;

(* Quake3 Skin loading procedure. Use this procedure to apply textures
   to a GLActor. This doens't use the actors original preloaded materials so
   it may be a good idea to clear the actors material library before
   running this to keep everything nice and clean. *)
procedure LoadQ3Skin(const FileName: string; Actor: TGLActor);

// -----------------------------------------------------------------------
implementation
// -----------------------------------------------------------------------

procedure LoadQ3Anims(Animations: TGLActorAnimations; const FileName: string;
  const NamePrefix: string);
var
  AnimStrings: TStrings;
begin
  AnimStrings := TStringList.Create;
  AnimStrings.LoadFromFile(FileName);
  LoadQ3Anims(Animations, AnimStrings, NamePrefix);
  AnimStrings.Free;
end;

procedure LoadQ3Anims(Animations: TGLActorAnimations; Strings: TStrings;
  const NamePrefix: string);
var
  anim: TStringList;
  val: array [0 .. 3] of Integer;
  strindex, valindex, i: Integer;
  GotValues: Boolean;
  commatext, str1: string;
  TorsoStartFrame, LegsStartFrame: Integer;
  // Used to Fix LEGS Frame values red from CFG file

  function StrIsNumber(const str: string): Boolean;
  var
    i: Integer;
  begin
    result := false;
    for i := 1 to Length(str) do
      if (Ord(str[i]) >= Ord('0')) and (Ord(str[i]) <= Ord('9')) then
        result := true
      else
      begin
        result := false;
        break;
      end;
  end;

begin
  anim := TStringList.Create;
  TorsoStartFrame := 0;
  LegsStartFrame := 0;
  FillChar(val[0], SizeOf(val), $00);
  for strindex := 0 to Strings.Count - 1 do
  begin
    commatext := Strings.Strings[strindex];
    while Pos('  ', commatext) > 0 do
      commatext := StringReplace(commatext, '  ', ' ', [rfReplaceAll]);
    commatext := StringReplace(commatext, ' ', ',', [rfReplaceAll]);
    anim.commatext := commatext;
    GotValues := false;
    valindex := 0;
    str1 := '';
    if anim.Count >= 5 then
    begin
      for i := 0 to anim.Count - 1 do
      begin
        if GotValues then
        begin

          // Store start values to Fix LEGS
          if (TorsoStartFrame = 0) and
            (Pos('TORSO_', Uppercase(anim.Strings[i])) > 0) then
            TorsoStartFrame := val[0];
          if (LegsStartFrame = 0) and
            (Pos('LEGS_', Uppercase(anim.Strings[i])) > 0) then
            LegsStartFrame := val[0];

          if (anim.Strings[i] <> '//') and
            (Pos(NamePrefix + '_', anim.Strings[i]) > 0) then
          begin
            str1 := StringReplace(anim.Strings[i], '//', '', [rfReplaceAll]);
            break;
          end;
        end
        else
        begin
          if StrIsNumber(anim.Strings[i]) then
          begin
            val[valindex] := StrToInt(anim.Strings[i]);
            Inc(valindex);
            if valindex = 4 then
              GotValues := true;
          end
          else
            break;
        end;
      end;
    end;
    if GotValues and (str1 <> '') then
    begin
      // Values ready for new animation.
      with Animations.Add do
      begin
        // Fix frame value for Legs
        if Uppercase(NamePrefix) = 'LEGS' then
          val[0] := val[0] - LegsStartFrame + TorsoStartFrame;

        Name := str1;
        StartFrame := val[0];
        EndFrame := val[0] + val[1] - 1;
        Reference := aarMorph;
        // Need a way in TGLActorAnimation to tell whether it is
        // a looping type animation or a play once type and
        // the framerate (interval) it uses. Both of these can
        // be determined here and loaded.
      end;
    end;
  end;
  anim.Free;
end;

procedure LoadQ3Skin(const FileName: string; Actor: TGLActor);
const
  // This list can be expanded if necessary
  ExtList: array [0 .. 3] of string = ('.jpg', '.jpeg', '.tga', '.bmp');
var
  SkinStrings, temp: TStrings;
  i, j: Integer;
  libmat: TGLLibMaterial;
  mesh: TMeshObject;
  texture, textureNoDir: string;
  textureFound, meshFound: Boolean;

  function GetMeshObjectByName(MeshObjects: TGLMeshObjectList;
    const Name: string; out mesh: TMeshObject): Boolean;
  var
    i: Integer;
  begin
    result := false;
    if (trim(Name) = '') or not Assigned(MeshObjects) then
      exit;
    for i := 0 to MeshObjects.Count - 1 do
    begin
      if MeshObjects[i].Name = Name then
      begin
        mesh := MeshObjects[i];
        result := true;
        break;
      end;
    end;
  end;

begin
  if (not FileExists(FileName)) or (not Assigned(Actor)) then
    exit;
  if (not Assigned(Actor.MaterialLibrary)) then
    exit;

  SkinStrings := TStringList.Create;
  temp := TStringList.Create;
  temp.LoadFromFile(FileName);

  for i := 0 to temp.Count - 1 do
  begin
    SkinStrings.commatext := temp.Strings[i];
    if SkinStrings.Count > 1 then
    begin
      libmat := Actor.MaterialLibrary.Materials.GetLibMaterialByName
        (SkinStrings.Strings[1]);
      meshFound := GetMeshObjectByName(Actor.MeshObjects,
        SkinStrings.Strings[0], mesh);
      if meshFound then
      begin
        if not Assigned(libmat) then
        begin
          libmat := Actor.MaterialLibrary.Materials.Add;
          libmat.Name := SkinStrings.Strings[1];

          // Search for the texture file
          textureFound := false;
          for j := 0 to Length(ExtList) - 1 do
          begin
            texture := StringReplace(SkinStrings.Strings[1], '/', '\',
              [rfReplaceAll]);
            texture := ChangeFileExt(texture, ExtList[j]);
            if FileExists(texture) then
            begin
              libmat.Material.texture.Image.LoadFromFile(texture);
              libmat.Material.texture.Disabled := false;
              textureFound := true;
            end
            else
            begin
              textureNoDir := ExtractFileName(texture);
              if FileExists(textureNoDir) then
              begin
                libmat.Material.texture.Image.LoadFromFile(textureNoDir);
                libmat.Material.texture.Disabled := false;
                textureFound := true;
              end;
            end;
            if textureFound then
              break;
          end;
        end;
        for j := 0 to mesh.FaceGroups.Count - 1 do
          mesh.FaceGroups[j].MaterialName := libmat.Name;
      end;
    end;
  end;

  temp.Free;
  SkinStrings.Free;
end;

// ------------------
// ------------------ TMD3TagList ------------------
// ------------------

procedure TMD3TagList.LoadFromFile(const FileName: String);
var
  fs: TStream;
begin
  if FileName <> '' then
  begin
    fs := TFileStream.Create(FileName, fmOpenRead + fmShareDenyWrite);
    try
      LoadFromStream(fs);
    finally
      fs.Free;
    end;
  end;
end;

procedure TMD3TagList.LoadFromStream(AStream: TStream);
var
  MD3Header: TMD3Header;
begin
  // Load the MD3 header
  AStream.Read(MD3Header, SizeOf(MD3Header));

  // Test for correct file ID and version
  Assert(MD3Header.fileID = 'IDP3', 'Incorrect MD3 file ID');
  Assert(MD3Header.version = 15, 'Incorrect MD3 version number');

  // Get the tags from the file
  FNumTags := MD3Header.numTags;
  FNumFrames := MD3Header.numFrames;
  SetLength(FTags, FNumTags * FNumFrames);
  AStream.Position := MD3Header.tagStart;
  AStream.Read(FTags[0], FNumTags * FNumFrames * SizeOf(TMD3Tag));
end;

function TMD3TagList.GetTag(index: Integer): TMD3Tag;
begin
  result := FTags[index];
end;

function TMD3TagList.GetTransform(const TagName: string;
  Frame: Integer): TGLMatrix;
var
  TagIdx, i, j: Integer;
  Tag: TMD3Tag;
begin
  result := IdentityHMGMatrix;
  TagIdx := -1;
  for i := 0 to FNumTags do
    if lowercase(trim(TagName)) = lowercase(trim(string(FTags[i].strName))) then
    begin
      TagIdx := i;
      break;
    end;
  if TagIdx = -1 then
    exit;
  Tag := FTags[TagIdx + Frame * FNumTags];
  for j := 0 to 2 do
    for i := 0 to 2 do
      result.V[i].V[j] := Tag.rotation.V[i].V[j];
  for i := 0 to 2 do
    result.V[3].V[i] := Tag.vPosition.V[i];
end;

end.
