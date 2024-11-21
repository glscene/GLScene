//
// The graphics rendering engine GLScene http://glscene.org
//
unit GXS.Python.Script;
(*
  Python implementation for the GXScene scripting layer.
  This unit is experimental.
*)
interface

uses
  System.Classes,
  System.SysUtils,

  GXS.XCollection,
  GXS.ScriptBase,
  Stage.Manager,

  Python.Engine; // from ... current version of pyscript

type
  (* This class only adds manager registration logic to the TPythonEngine
    class to enable the XCollection items (ie. TgxScriptPython) retain it's
    assigned compiler from design to run -time. *)
  TgxPythonEngine = class(TPythonEngine)
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
  end;

  // Implements Python scripting functionality through the abstracted GLS.ScriptBase
  TgxScriptPython = class(TgxScriptBase)
  private
    FEngine: TgxPythonEngine;
    FEngineName: String;
    FCompiled, FStarted: Boolean;
  protected
    procedure SetEngine(const Value: TgxPythonEngine);
    procedure ReadFromFiler(reader: TReader); override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetState: TgxScriptState; override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Compile; override;
    procedure Start; override;
    procedure Stop; override;
    procedure Execute; override;
    procedure Invalidate; override;
    function Call(aName: String; aParams: array of Variant): Variant; override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function ItemCategory: String; override;
  published
    property Engine: TgxPythonEngine read FEngine write SetEngine;
  end;

procedure Register;

// --------------------------------------------------
implementation
// --------------------------------------------------


// ----------
// ---------- TgxPythonEngine ----------
// ----------

constructor TgxPythonEngine.Create(AOnwer: TComponent);
begin
  inherited;
  RegisterManager(Self);
end;

destructor TgxPythonEngine.Destroy;
begin
  DeregisterManager(Self);
  inherited;
end;


// ---------------
// --------------- TgxScriptPython ---------------
// ---------------

destructor TgxScriptPython.Destroy;
begin
  Invalidate;
  inherited;
end;

procedure TgxScriptPython.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TgxScriptPython then
  begin
    Engine := TgxScriptPython(Source).Engine;
  end;
end;

procedure TgxScriptPython.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
begin
  inherited;
  archiveVersion := reader.ReadInteger;
  Assert(archiveVersion = 0);

  with reader do
  begin
    FEngineName := ReadString;
  end;
end;

procedure TgxScriptPython.WriteToFiler(writer: TWriter);
begin
  inherited;
  writer.WriteInteger(0); // archiveVersion

  with writer do
  begin
    if Assigned(FEngine) then
      WriteString(FEngine.GetNamePath)
    else
      WriteString('');
  end;
end;

procedure TgxScriptPython.Loaded;
var
  temp: TComponent;
begin
  inherited;
  if FEngineName <> '' then
  begin
    temp := FindManager(TGLPythonEngine, FEngineName);
    if Assigned(temp) then
      Engine := TGLPythonEngine(temp);
    FEngineName := '';
  end;
end;

procedure TgxScriptPython.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = Engine) and (Operation = opRemove) then
    Engine := nil;
end;

class function TgxScriptPython.FriendlyName: String;
begin
  Result := 'TgxScriptPython';
end;

class function TgxScriptPython.FriendlyDescription: String;
begin
  Result := 'Python script';
end;

class function TgxScriptPython.ItemCategory: String;
begin
  Result := '';
end;

procedure TgxScriptPython.Compile;
begin
  Invalidate;
  if Assigned(Engine) then
  begin
    Engine.ExecStrings(Text);
    FCompiled := True;
    FStarted := False;
  end
  else
    raise Exception.Create('No engine assigned!');
end;

procedure TgxScriptPython.Execute;
begin
  Compile;
end;

procedure TgxScriptPython.Invalidate;
begin
  FStarted := False;
  FCompiled := False;
end;

procedure TgxScriptPython.Start;
begin
  Compile;
  FStarted := True;
end;

procedure TgxScriptPython.Stop;
begin
  FStarted := False;
end;

function TgxScriptPython.Call(aName: String; aParams: array of Variant)
  : Variant;
var
  func: PPyObject;
  args: array of TVarRec;
  i: Integer;
begin
  if State = ssUncompiled then
    Start;
  if State = ssRunning then
  begin
    func := Engine.FindFunction('__main__', aName);
    if Assigned(func) then
      if Length(aParams) > 0 then
      begin
        SetLength(args, Length(aParams));
        for i := 0 to Length(aParams) - 1 do
        begin
          args[i].VType := vtVariant;
          args[i].VVariant := @aParams[i];
        end;
        Result := Engine.EvalFunction(func, args);
      end
      else
        Result := Engine.EvalFunctionNoArgs(func);
  end;
end;

procedure TgxScriptPython.SetEngine(const Value: TgxPythonEngine);
begin
  if Value <> FEngine then
  begin
    FEngine := Value;
    Invalidate;
  end;
end;

function TgxScriptPython.GetState: TgxScriptState;
begin
  Result := ssUncompiled;
  if Assigned(Engine) and FCompiled and FStarted then
    Result := ssRunning;
end;

procedure Register;
begin
  RegisterClasses([TgxPythonEngine, TgxScriptPython]);
  RegisterComponents('GXScene Python', [TgxPythonEngine]);
end;

// --------------------------------------------------
initialization
// --------------------------------------------------

RegisterXCollectionItemClass(TgxScriptPython);

// --------------------------------------------------
finalization
// --------------------------------------------------

UnregisterXCollectionItemClass(TgxScriptPython);

end.
