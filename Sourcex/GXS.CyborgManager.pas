unit GXS.CyborgManager;

(*
  The CyborgManager is a class that provides a way to manage
  AI smart objects and thier swarms or proxies at runtime.
  You need to have the model filename and it will load it
  into the list or return the already existing model
*)

interface

uses
  System.SysUtils,
  System.Classes,

  GXS.SmartObjects,

  GXS.Scene,
  GXS.Objects,
  GXS.FileGLTF,
  GXS.File3DS,
  GXS.FilePly,
  GXS.FileOBJ;

type
  TgxCyborgManager = class
  private
    FMasterObject: TgxDummyCube;
    FModelList: TStringList;
    FModelPath: string;
    (* Changing the path to the models and refresh the
      already existing freeforms *)
    procedure SetPathToModel(const Value: String);
  public
    (* It will create the list, assign the path to the
      models and the master object where the new models will be loaded *)
    constructor Create(AMaster: TgxDummyCube; APath: string); virtual;
    (* It will free all the loaded models *)
    destructor Destroy; override;
    (* It will load a new model if it's not in the list and then return the new
      freeform.  If it,s already in the list, it will return the existing freeform *)
    function LoadModel(AFilename: string): TgxCyborg;
    property MasterObject: TgxDummyCube read FMasterObject;
    property ModelList: TStringList read FModelList;
    property Path: string read FModelPath write SetPathToModel;
  end;

implementation //-------------------------------------------------------------

constructor TgxCyborgManager.Create(AMaster: TgxDummyCube; APath: string);
begin
  // Set the master object
  FMasterObject := AMaster;
  // Create the model list
  FModelList := TStringList.Create;
  FModelList.CaseSensitive := false;
  FModelList.Sorted := true;
  // Set the path to the models
  SetPathToModel(APath);
end;

// --------------------------------
destructor TgxCyborgManager.Destroy;
var
  i: Integer;
begin
  // Destroy every models
  for i := 0 to Pred(FModelList.Count) do
    FModelList.Objects[i].Destroy;
  // Destroy the list
  FModelList.Destroy;
  inherited;
end;

// --------------------------------------------------------------
function TgxCyborgManager.LoadModel(AFilename: string): TgxCyborg;
var
  i: Integer;
  NewFreeForm: TgxCyborg;
begin
  with FModelList do
  begin
    if Find(AFilename, i) then
      Result := TgxCyborg(Objects[i])
    else
    begin
      NewFreeForm := TgxCyborg(FMasterObject.AddNewChild(TgxCyborg));
      NewFreeForm.LoadFromFile(FModelPath + AFilename);
      FModelList.AddObject(AFilename, NewFreeForm);
      Result := NewFreeForm;
    end;
  end;
end;

// ----------------------------------------------------------
procedure TgxCyborgManager.SetPathToModel(const Value: String);
var
  Len: Integer;
  i: Integer;
begin
  // Set the path
  FModelPath := Value;
  Len := Length(Value);
  // Correct it if there is no '\'
  if (Len > 0) then
    if (Value[Len - 1] <> '\') then
      FModelPath := Value + '\';
  // Reload the models
  for i := 0 to Pred(FModelList.Count) do
    TgxCyborg(FModelList.Objects[i])
      .LoadFromFile(FModelPath + FModelList.Strings[i]);
end;

initialization //-------------------------------------------------------------

// RegisterClasses([TgxCyborgManager]);

end.
