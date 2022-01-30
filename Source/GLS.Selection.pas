//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.Selection;

(* Picking and selection of objects *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,

  GLS.OpenGLTokens,
  GLS.Context,
  GLS.VectorLists,
  GLS.VectorGeometry,
  GLS.BaseClasses,
  GLS.PersistentClasses;

const
  MAX_OBJECT_STACK_DEPTH = 512;

type
  TPickSubObjects = array of LongInt;

  TPickRecord = class
  public
    AObject: TGLUpdateAbleComponent;
    SubObjects: TPickSubObjects;
    ZMin, ZMax: Single;
  end;

  TPickSortType = (psDefault, psName, psMinDepth, psMaxDepth);

  (* List class for object picking.
     This list is used to store the results of a PickObjects call. *)
  TGLPickList = class(TGLPersistentObjectList)
  private
    function GetFar(aValue: Integer): Single;
    function GetHit(aValue: Integer): TObject;
    function GetNear(aValue: Integer): Single;
    function GetSubObjects(aValue: Integer): TPickSubObjects;
  public
    constructor Create(aSortType: TPickSortType); reintroduce;
    procedure AddHit(obj: TObject; const subObj: TPickSubObjects; zMin, zMax: Single);
    procedure Clear; override;
    function FindObject(AObject: TObject): Integer;
    property FarDistance[Index: Integer]: Single read GetFar;
    property Hit[Index: Integer]: TObject read GetHit; default;
    property NearDistance[Index: Integer]: Single read GetNear;
    property SubObjects[Index: Integer]: TPickSubObjects read GetSubObjects;
  end;

  TGLBaseSelectTechnique = class
  protected
    FObjectStack: array of TObject;
    FNameStack: array[0..255] of Cardinal;
    FCurrentName: Integer;
    FStackPosition: Integer;
    FObjectCountGuess: Integer;
    FHits: Integer;
    function GetObject: TObject; virtual; abstract;
    procedure SetObject(Value: TObject); virtual; abstract;
    function GetHits: Integer; virtual; abstract;
    procedure SetHits(Value: Integer); virtual; abstract;
    procedure SetObjectCountGuess(Value: Integer); virtual; abstract;
    function GetItems(Value: Integer): TObject; virtual; abstract;
  public
    class function IsSupported: Boolean; virtual; abstract;
    procedure Start; virtual; abstract;
    function Stop: Boolean; virtual; abstract;
    procedure PushObject(AName: TObject); virtual; abstract;
    procedure PopObject(); virtual; abstract;
    procedure LoadObject(AName: TObject); virtual; abstract;
    procedure FillPickingList(var AList: TGLPickList); virtual; abstract;
    property CurrentObject: TObject read GetObject write SetObject;
    property ObjectCountGuess: Integer read FObjectCountGuess write SetObjectCountGuess;
    property Hits: Integer read GetHits write SetHits;
  end;

  TGLBaseSelectTechniqueClass = class of TGLBaseSelectTechnique;

  TGLSelectRenderModeTechnique = class(TGLBaseSelectTechnique)
  private
    FBuffer: array of Cardinal;
  protected
    function GetObject: TObject; override;
    procedure SetObject(Value: TObject); override;
    function GetHits: Integer; override;
    procedure SetHits(Value: Integer); override;
    procedure SetObjectCountGuess(Value: Integer); override;
  public
    class function IsSupported: Boolean; override;
    procedure Start; override;
    function Stop: Boolean; override;
    procedure FillPickingList(var AList: TGLPickList); override;
    property ObjectCountGuess;
    property Hits;
    property CurrentObject;
  end;

function GetBestSelectorClass: TGLBaseSelectTechniqueClass; inline;

//------------------------------------------------------------
implementation
//------------------------------------------------------------

function GetBestSelectorClass: TGLBaseSelectTechniqueClass;
begin
//  if TGLSelectRenderToTextureTechnique.IsSupported then
//    Result := TGLSelectRenderToTextureTechnique
//  else
    Result := TGLSelectRenderModeTechnique;
end;

// ------------------
// ------------------ TGLPickList ------------------
// ------------------

var
  vPickListSortFlag: TPickSortType;

constructor TGLPickList.Create(aSortType: TPickSortType);
begin
  vPickListSortFlag := aSortType;
  inherited Create;
end;

function Comparefunction(item1, item2: TObject): Integer;
var
  diff: Single;
begin
  Result := 0;
  case vPickListSortFlag of
    psName:
      Result := CompareText(TComponent(TPickRecord(Item1).AObject).Name,
        TComponent(TPickRecord(Item1).AObject).Name);
    psMinDepth:
      begin
        Diff := TPickRecord(Item1).ZMin - TPickRecord(Item2).ZMin;
        if Diff < 0 then
          Result := -1
        else if Diff > 0 then
          Result := 1
        else
          Result := 0;
      end;
    psMaxDepth:
      begin
        Diff := TPickRecord(Item1).ZMax - TPickRecord(Item2).ZMax;
        if Diff < 0 then
          Result := -1
        else if Diff > 0 then
          Result := 1
        else
          Result := 0;
      end;
  end;
end;

procedure TGLPickList.AddHit(obj: TObject;
  const subObj: TPickSubObjects; zMin, zMax: Single);
var
  newRecord: TPickRecord;
begin
  newRecord := TPickRecord.Create;
  newRecord.AObject := TGLUpdateAbleComponent(obj);
  newRecord.SubObjects := subObj;
  newRecord.zMin := zMin;
  newRecord.zMax := zMax;
  Add(newRecord);
  if vPickListSortFlag <> psDefault then
    Sort(@Comparefunction);
end;

procedure TGLPickList.Clear;
begin
  DoClean;
  inherited;
end;

function TGLPickList.FindObject(aObject: TObject): Integer;
var
  i: Integer;
begin
  Result := -1;
  if Assigned(AObject) then
    for i := 0 to Count - 1 do
    begin
      if Hit[i] = AObject then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function TGLPickList.GetFar(aValue: Integer): Single;
begin
  Result := TPickRecord(Items[AValue]).ZMax;
end;

function TGLPickList.GetHit(aValue: Integer): TObject;
begin
  Result := TPickRecord(Items[AValue]).AObject;
end;

function TGLPickList.GetNear(aValue: Integer): Single;
begin
  Result := TPickRecord(Items[AValue]).ZMin;
end;

function TGLPickList.GetSubObjects(aValue: Integer): TPickSubobjects;
begin
  Result := TPickRecord(Items[AValue]).SubObjects;
end;

// ------------------
// ------------------ TGLSelectRenderModeTechnique ------------------
// ------------------

function TGLSelectRenderModeTechnique.GetHits: Integer;
begin
  Result := FHits;
end;

procedure TGLSelectRenderModeTechnique.SetHits(Value: Integer);
begin
  FHits := Value;
end;

procedure TGLSelectRenderModeTechnique.SetObjectCountGuess(Value: Integer);
begin
  if Value<8 then
    Value := 8;
  FObjectCountGuess := Value;
end;

class function TGLSelectRenderModeTechnique.IsSupported: Boolean;
begin
  Result := gl.VERSION_1_1;
end;

procedure TGLSelectRenderModeTechnique.Start;
begin
  SetLength(FBuffer, FObjectCountGuess * 4 + 32);
  gl.SelectBuffer(FObjectCountGuess * SizeOf(Cardinal), @FBuffer[0]);
  gl.RenderMode(GL_SELECT);
  gl.InitNames;
  FCurrentName := 0;
  SetLength(FObjectStack, MAX_OBJECT_STACK_DEPTH);
  FStackPosition := 0;
  gl.PushName(0);
end;

function TGLSelectRenderModeTechnique.Stop: Boolean;
begin
  gl.Flush;
  FHits := gl.RenderMode(GL_RENDER);
  Result := FHits > -1;
  if not Result then
    Inc(FObjectCountGuess);
end;

procedure TGLSelectRenderModeTechnique.FillPickingList(var AList: TGLPickList);
var
  subObj: TPickSubObjects;
  next, current, subObjIndex: Cardinal;
  szmin, szmax: Single;
  I: Integer;
begin
  if not Assigned(AList) then
    AList := TGLPickList.Create(psDefault)
  else
    AList.Clear;
  if FHits > -1 then
  begin
    next := 0;
    for I := 0 to FHits - 1 do
    begin
      current := next;
      next := current + FBuffer[current] + 3;
      szmin := (FBuffer[current + 1] shr 1) * (1 / MaxInt);
      szmax := (FBuffer[current + 2] shr 1) * (1 / MaxInt);
      subObj := nil;
      subObjIndex := current + 4;
      if subObjIndex < next then
      begin
        SetLength(subObj, FBuffer[current] - 1);
        while subObjIndex < next do
        begin
          subObj[subObjIndex - current - 4] := FBuffer[subObjIndex];
          inc(subObjIndex);
        end;
      end;
      AList.AddHit(FObjectStack[FBuffer[current + 3]], subObj, szmin, szmax);
    end;
  end;
  // Restore initial object stack length
  SetLength(FObjectStack, MAX_OBJECT_STACK_DEPTH);
end;

function TGLSelectRenderModeTechnique.GetObject: TObject;
begin
  Result := FObjectStack[FCurrentName];
end;

procedure TGLSelectRenderModeTechnique.SetObject(Value: TObject);
begin
  // Grow Object stack length if needed
  if FCurrentName >= Length(FObjectStack) then
    SetLength(FObjectStack, Length(FObjectStack) * 2);
  FObjectStack[FCurrentName] := Value;
  gl.LoadName(FCurrentName);
  Inc(FCurrentName);
end;

end.
