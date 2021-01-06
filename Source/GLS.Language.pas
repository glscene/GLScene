//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.Language;

(*
  Language created to localize your application.
  In Delphi, the text is encoded using Ansi cp1251 and can not be encoded \ decoding.
  In Lazarus has the ability to upload text from any encoding.
*)

interface

{$I GLScene.inc}

uses
  System.Classes, 
  System.IniFiles, 
  System.SysUtils,
  GLS.Utils;

type

  TGLLanguageEntry = record
    ID: String; //**< identifier
    Text: String; //**< translation
  end;

  TGLLanguageEntryArray = array of TGLLanguageEntry;

  (* Class TGLLanguage is used only for downloading and translation,
    as in the final product it's no need for the text processing *)
  TGLLanguage = class
  private
    FCurrentLanguageFile: String;
    Entry: TGLLanguageEntryArray; //**< Entrys of Chosen Language
  public
    function FindID(const ID: String): integer;
    function Translate(const ID: String): String;
    procedure LoadLanguageFromFile(const Language: String);
    property CurrentLanguageFile: String read FCurrentLanguageFile;
  end;

  (* Advanced class is designed for loading and processing,
    will be useful for language editors *)
  TGLLanguageExt = class(TGLLanguage)
  private
    function GetEntry(Index: integer): TGLLanguageEntry;
    procedure SetEntry(Index: integer; const aValue: TGLLanguageEntry);
    function GetCount: integer;
  public
    procedure AddConst(const ID: String; const Text: String);
    procedure AddConsts(aValues: TStrings);
    procedure ChangeConst(const ID: String; const Text: String);
    property Items[Index: integer]: TGLLanguageEntry read GetEntry write SetEntry;
    property Count: integer read GetCount;
    procedure SaveLanguageFromFile(const Language: String); overload;
    procedure SaveLanguageFromFile; overload;
  end;

  // Abstract class for control Language
  TGLSLanguage = class(TComponent)
  private
    FLanguage: TGLLanguageExt;
    FLanguageList: TStrings;
    procedure SetLanguage(aValue: TGLLanguageExt);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadLanguageFromFile(const Language: String);
    procedure SaveLanguageFromFile(const Language: String); overload;
    procedure SaveLanguageFromFile; overload;
    function Translate(const ID: String): String;
    property Language: TGLLanguageExt read FLanguage write SetLanguage;
  end;

//-----------------------------------------------------------------------
implementation
//-----------------------------------------------------------------------

uses
  GLS.Logger;

//----------------------------
// TGLLanguage
//----------------------------

// Load the specified LanguageFile
procedure TGLLanguage.LoadLanguageFromFile(const Language: String);
var
  IniFile: TMemIniFile;
  E: integer; // entry
  S: TStringList;
  I: integer;
begin
  If Language = '' then
    Exit;
  if not FileExists(string(Language)) then
  begin
{$IFDEF USE_LOGGING}
    GLSLogger.LogFatalError(ExtractFileName(string(Language)) +
      ' Languagefile missing!');
{$ENDIF}
    Exit;
  end;
  SetLength(Entry, 0);
  FCurrentLanguageFile := Language;
  IniFile := TMemIniFile.Create(string(Language));
  S := TStringList.Create;

  IniFile.ReadSectionValues('Text', S);

  // Problem Solving with symbols wrap (#13#10)
  I := 0;
  for E := 0 to S.Count - 1 do
  begin
    If S.Names[E] = '' then
    begin
      S.Strings[I] := S.Strings[I] + #13#10 + GetValueFromStringsIndex(S, E);
    end
    else
      I := E;
  end;

  SetLength(Entry, S.Count);
  for E := 0 to high(Entry) do
    If S.Names[E] <> '' then
    begin
      Entry[E].ID := S.Names[E];
      Entry[E].Text := GetValueFromStringsIndex(S, E);
    end;
  S.Free;
  IniFile.Free;
end;

(* Find the index of ID an array of language entry.
   @returns the index on success, -1 otherwise *)
function TGLLanguage.FindID(const ID: String): integer;
var
  Index: integer;
begin
  for Index := 0 to High(Entry) do
  begin
    if UpperCase(string(ID)) = UpperCase(string(Entry[Index].ID)) then
    begin
      Result := Index;
      Exit;
    end;
  end;
  Result := -1;
end;

(*  Translate the Text.
   If Text is an ID, text will be translated according to the current language
   setting. If Text is not a known ID, it will be returned as is.
   @param Text either an ID or an UTF-8 encoded string *)
function TGLLanguage.Translate(const ID: String): String;
var
  EntryIndex: integer;
begin
  // fallback result in case Text is not a known ID
  Result := ID;

  // Check if ID exists
  EntryIndex := FindID(ID);
  if (EntryIndex >= 0) then
  begin
    Result := Entry[EntryIndex].Text;
    Exit;
  end;
end;

// Add a Constant ID that will be Translated but not Loaded from the LanguageFile
procedure TGLLanguageExt.AddConst(const ID: String; const Text: String);
begin
  SetLength(Entry, Length(Entry) + 1);
  Entry[high(Entry)].ID := ID;
  Entry[high(Entry)].Text := Text;
end;

procedure TGLLanguageExt.AddConsts(aValues: TStrings);
var
  I: integer;
begin
  if aValues <> nil then
    for I := 0 to aValues.Count - 1 do
      If aValues.Names[I] <> '' then
        AddConst(aValues.Names[I],GetValueFromStringsIndex(aValues, I));
end;

procedure TGLLanguageExt.ChangeConst(const ID: String;
  const Text: String);
var
  I: integer;
begin
  for I := 0 to high(Entry) do
  begin
    if Entry[I].ID = ID then
    begin
      Entry[I].Text := Text;
      Break;
    end;
  end;
end;

function TGLLanguageExt.GetEntry(Index: integer): TGLLanguageEntry;
begin
  Result := Entry[Index];
end;

procedure TGLLanguageExt.SetEntry(Index: integer; const aValue: TGLLanguageEntry);
begin
  Entry[Index] := aValue;
end;

function TGLLanguageExt.GetCount: integer;
begin
  Result := high(Entry) + 1;
end;

procedure TGLLanguageExt.SaveLanguageFromFile(const Language: String);
var
  IniFile: TMemIniFile;
  E: integer; // entry
begin
  if Language = '' then
    Exit;

  IniFile := TMemIniFile.Create(string(Language));

  for E := 0 to Count - 1 do
  begin
    IniFile.WriteString('Text', string(Items[E].ID), string(Items[E].Text));
  end;
  IniFile.UpdateFile;
  IniFile.Free;
end;

procedure TGLLanguageExt.SaveLanguageFromFile;
begin
  SaveLanguageFromFile(CurrentLanguageFile);
end;

//----------------------------
// TGLSLanguage
//----------------------------

constructor TGLSLanguage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLanguage := TGLLanguageExt.Create;
  FLanguageList := TStringList.Create;
end;

destructor TGLSLanguage.Destroy;
begin
  FLanguage.Free;
  FLanguageList.Free;
  inherited Destroy;
end;

procedure TGLSLanguage.LoadLanguageFromFile(const Language: String);
begin
  FLanguage.LoadLanguageFromFile(Language);
end;

procedure TGLSLanguage.SetLanguage(aValue: TGLLanguageExt);
begin
  if aValue <> nil then
    FLanguage := aValue;
end;

procedure TGLSLanguage.SaveLanguageFromFile(const Language: String);
begin
  if Language = '' then
    Exit;

  FLanguage.SaveLanguageFromFile(Language);
end;

procedure TGLSLanguage.SaveLanguageFromFile;
begin
  FLanguage.SaveLanguageFromFile;
end;

function TGLSLanguage.Translate(const ID: String): String;
begin
  Result := FLanguage.Translate(ID);
end;

// ------------------------------------------------------------------------------
initialization
// ------------------------------------------------------------------------------

RegisterClass(TGLSLanguage);

end.
