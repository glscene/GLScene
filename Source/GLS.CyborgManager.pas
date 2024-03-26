unit GLS.CyborgManager;
(*
  The CyborgManager is a class that provides a way to manage
  AI models for master objects and thier proxies at runtime.
  You just need to have the model filename and it will load it
  into the list or return the already existing model
*)

interface

uses
  System.SysUtils,
  System.Classes,
  GLS.Scene,
  GLS.Objects,
  GLS.SmartObjects,
  GLS.File3DS,
  GLS.FilePly,
  GLS.FileOBJ;

type

  TGLCyborgManager = class
  private
    FMasterObject: TGLDummyCube;
    FModelList: TStringList;
    FModelPath: string;
    (* Changing the path to the models and refresh the
      already existing freeforms *)
    procedure SetPathToModel(const Value: String);
  public
    (* It will create the list, assign the path to the
      models and the master object where the new models will be loaded *)
    constructor Create(AMaster: TGLDummyCube; APath: string); virtual;
    (* It will free all the loaded models *)
    destructor Destroy; override;
    (* It will load a new model if it's not in the list and then return the new
      freeform.  If it,s already in the list, it will return the existing freeform *)
    function LoadModel(AFilename: string): TGLCyborg;
    property MasterObject: TGLDummyCube read FMasterObject;
    property ModelList: TStringList read FModelList;
    property Path: string read FModelPath write SetPathToModel;
  end;

// ============================================================================
implementation
// ============================================================================

constructor TGLCyborgManager.Create(AMaster: TGLDummyCube; APath: string);
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
destructor TGLCyborgManager.Destroy;
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
function TGLCyborgManager.LoadModel(AFilename: string): TGLCyborg;
var
  i: Integer;
  NewFreeForm: TGLCyborg;
begin
  with FModelList do
  begin
    if Find(AFilename, i) then
      Result := TGLCyborg(Objects[i])
    else
    begin
      NewFreeForm := TGLCyborg(FMasterObject.AddNewChild(TGLCyborg));
      NewFreeForm.LoadFromFile(FModelPath + AFilename);
      FModelList.AddObject(AFilename, NewFreeForm);
      Result := NewFreeForm;
    end;
  end;
end;

// ----------------------------------------------------------
procedure TGLCyborgManager.SetPathToModel(const Value: String);
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
    TGLCyborg(FModelList.Objects[i])
      .LoadFromFile(FModelPath + FModelList.Strings[i]);
end;

end.
