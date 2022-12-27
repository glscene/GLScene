//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLS.ScriptBase;

(*
  An abstract scripting interface for GLScene
  This unit provides the base methods for compiling and executing scripts as
  well as calling scripted functions. No scripting APIs are implemented here,
  only abstracted functions.
*)

interface

uses
  System.Classes,
  GLS.XCollection;

type
  TGLScriptState = (
    ssUncompiled, // The script has yet to be compiled.
    ssCompileErrors, // Errors occurred while compiling.
    ssCompiled, // The script has been compiled and ready to be executed/started.
    ssRunningErrors, // Errors occured while the script was running.
    ssRunning); // The script is currently active and running without error.

   (* The base script class that defines the abstract functions and properties.
    Don't use this class directly, use the script classes descended from this
    base class. *)
  TGLScriptBase = class(TXCollectionItem)
  private
    FText: TStringList;
    FDescription: String;
    FErrors: TStringList; // not persistent
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    function GetState: TGLScriptState; virtual; abstract;
    procedure SetText(const Value: TStringList);
    procedure Notification(AComponent: TComponent; Operation: TOperation); virtual;
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Compile; virtual; abstract;
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;
    procedure Execute; virtual; abstract;
    procedure Invalidate; virtual; abstract;
    function Call(aName: String; aParams: array of Variant): Variant; virtual; abstract;
    property Errors: TStringList read FErrors;
    property State: TGLScriptState read GetState;
  published
    property Text: TStringList read FText write SetText;
    property Description: String read FDescription write FDescription;
  end;

  // XCollection descendant for storing and handling scripts.
  TGLScripts = class(TXCollection)
  protected
    function GetItems(index: Integer): TGLScriptBase;
  public
    procedure Assign(Source: TPersistent); override;
    class function ItemsClass: TXCollectionItemClass; override;
    function CanAdd(aClass: TXCollectionItemClass): Boolean; override;
    property Items[index: Integer]: TGLScriptBase read GetItems; default;
  end;

  (* Encapsulation of the scripts XCollection to help with script handling at
     design-time. Links the scripts to Delphi's persistence model. *)
  TGLScriptLibrary = class(TComponent)
  private
    FScripts: TGLScripts;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure WriteScriptsData(Stream: TStream);
    procedure ReadScriptsData(Stream: TStream);
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Scripts: TGLScripts read FScripts;
  end;

//========================================================
implementation
//========================================================

// ---------------
// --------------- TGLScriptBase ---------------
// ---------------

constructor TGLScriptBase.Create(aOwner: TXCollection);
begin
  inherited;
  FText := TStringList.Create;
  FErrors := TStringList.Create;
end;

destructor TGLScriptBase.Destroy;
begin
  FText.Free;
  FErrors.Free;
  inherited;
end;

procedure TGLScriptBase.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TGLScriptBase then
  begin
    Text.Assign(TGLScriptBase(Source).Text);
    Description := TGLScriptBase(Source).Description;
  end;
end;

procedure TGLScriptBase.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
begin
  inherited;
  archiveVersion := reader.ReadInteger;
  Assert(archiveVersion = 0);
  with reader do
  begin
    FText.Text := ReadString;
    FDescription := ReadString;
  end;
end;

procedure TGLScriptBase.WriteToFiler(writer: TWriter);
begin
  inherited;
  writer.WriteInteger(0);
  with writer do
  begin
    WriteString(FText.Text);
    WriteString(FDescription);
  end;
end;

procedure TGLScriptBase.SetText(const Value: TStringList);
begin
  Text.Assign(Value);
end;

procedure TGLScriptBase.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  // Virtual
end;

// ---------------
// --------------- TGLScripts ---------------
// ---------------

procedure TGLScripts.Assign(Source: TPersistent);
begin
  inherited;
  // Nothing yet
end;

function TGLScripts.GetItems(index: Integer): TGLScriptBase;
begin
  Result := TGLScriptBase(inherited GetItems(index));
end;

class function TGLScripts.ItemsClass: TXCollectionItemClass;
begin
  Result := TGLScriptBase;
end;

function TGLScripts.CanAdd(aClass: TXCollectionItemClass): Boolean;
begin
  Result := aClass.InheritsFrom(TGLScriptBase);
end;


// ---------------
// --------------- TGLScriptLibrary ---------------
// ---------------

constructor TGLScriptLibrary.Create(aOwner: TComponent);
begin
  inherited;
  FScripts := TGLScripts.Create(Self);
end;

destructor TGLScriptLibrary.Destroy;
begin
  FScripts.Free;
  inherited;
end;

procedure TGLScriptLibrary.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('ScriptsData', ReadScriptsData, WriteScriptsData,
    (Scripts.Count > 0));
end;

procedure TGLScriptLibrary.WriteScriptsData(Stream: TStream);
var
  writer: TWriter;
begin
  writer := TWriter.Create(Stream, 16384);
  try
    Scripts.WriteToFiler(writer);
  finally
    writer.Free;
  end;
end;

procedure TGLScriptLibrary.ReadScriptsData(Stream: TStream);
var
  reader: TReader;
begin
  reader := TReader.Create(Stream, 16384);
  try
    Scripts.ReadFromFiler(reader);
  finally
    reader.Free;
  end;
end;

procedure TGLScriptLibrary.Loaded;
begin
  inherited;
  Scripts.Loaded;
end;

procedure TGLScriptLibrary.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
begin
  if Assigned(Scripts) then
    for i := 0 to Scripts.Count - 1 do
      Scripts[i].Notification(AComponent, Operation);
  inherited;
end;

initialization

RegisterClasses([TGLScriptLibrary, TGLScripts, TGLScriptBase]);

end.
