//
// Graphic Scene Engine, http://glscene.org
//
(*
  DelphiWebScript implementation
*)
unit DWSx.Script;

interface

uses
  System.Classes,
  System.SysUtils,
  dwsComp,
  dwsExprs,
  dwsSymbols,

  GXS.XCollection,
  GXS.ScriptBase,
  GXS.Manager;

type
  (*
    This class only adds manager registration logic to the TDelphiWebScriptII
    class to enable the XCollection items (ie. TgxScriptDWS) retain it's
    assigned compiler from design to run -time.
  *)
  TgxDelphiWebScript = class(TDelphiWebScriptII)
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
  end;

  (* Implements DelphiWebScriptII scripting functionality through the
    abstracted GXS.ScriptBase . *)
  TgxScriptDWS = class(TgxScriptBase)
  private
    FDWS2Program: TProgram;
    FCompiler: TgxDelphiWebScriptII;
    FCompilerName: String;
  protected
    procedure SetCompiler(const Value: TgxDelphiWebScriptII);
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
    property DWS2Program: TProgram read FDWS2Program;
  published
    property Compiler: TgxDelphiWebScriptII read FCompiler write SetCompiler;

  end;

procedure Register;

// --------------------------------------------------
implementation

// --------------------------------------------------

// ---------------
// --------------- Miscellaneous ---------------
// ---------------

procedure Register;
begin
  RegisterClasses([TgxDelphiWebScriptII, TgxScriptDWS]);
  RegisterComponents('GXScene DWSx', [TgxDelphiWebScript]);
end;


// ----------
// ---------- TgxDelphiWebScript ----------
// ----------

constructor TgxDelphiWebScript.Create(AOnwer: TComponent);
begin
  inherited;
  RegisterManager(Self);
end;

destructor TgxDelphiWebScript.Destroy;
begin
  DeregisterManager(Self);
  inherited;
end;


// ---------------
// --------------- TgxScriptDWS ---------------
// ---------------

destructor TgxScriptDWS.Destroy;
begin
  Invalidate;
  inherited;
end;

procedure TgxScriptDWS.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TgxScriptDWS then
  begin
    Compiler := TgxScriptDWS(Source).Compiler;
  end;
end;

procedure TgxScriptDWS.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
begin
  inherited;
  archiveVersion := reader.ReadInteger;
  Assert(archiveVersion = 0);

  with reader do
  begin
    FCompilerName := ReadString;
  end;
end;

procedure TgxScriptDWS.WriteToFiler(writer: TWriter);
begin
  inherited;
  writer.WriteInteger(0); // archiveVersion

  with writer do
  begin
    if Assigned(FCompiler) then
      WriteString(FCompiler.GetNamePath)
    else
      WriteString('');
  end;
end;

procedure TgxScriptDWS.Loaded;
var
  temp: TComponent;
begin
  inherited;
  if FCompilerName <> '' then
  begin
    temp := FindManager(TgxDelphiWebScript, FCompilerName);
    if Assigned(temp) then
      Compiler := TgxDelphiWebScript(temp);
    FCompilerName := '';
  end;
end;

procedure TgxScriptDWS.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = Compiler) and (Operation = opRemove) then
    Compiler := nil;
end;

class function TgxScriptDWS.FriendlyName: String;
begin
  Result := 'GXS.ScriptDWS';
end;

class function TgxScriptDWS.FriendlyDescription: String;
begin
  Result := 'DelphiWebScript script';
end;

class function TgxScriptDWS.ItemCategory: String;
begin
  Result := '';
end;

procedure TgxScriptDWS.Compile;
begin
  Invalidate;
  if Assigned(Compiler) then
    FDWS2Program := Compiler.Compile(Text.Text)
  else
    raise Exception.Create('No compiler assigned!');
end;

procedure TgxScriptDWS.Execute;
begin
  if (State = ssUncompiled) then
    Compile
  else if (State = ssRunning) then
    Stop;
  if (State = ssCompiled) then
    FDWS2Program.Execute;
end;

procedure TgxScriptDWS.Invalidate;
begin
  if (State <> ssUncompiled) or Assigned(FDWSProgram) then
  begin
    Stop;
    FreeAndNil(FDWSProgram);
  end;
end;

procedure TgxScriptDWS.Start;
begin
  if (State = ssUncompiled) then
    Compile;
  if (State = ssCompiled) then
    FDWS2Program.BeginProgram(False);
end;

procedure TgxScriptDWS.Stop;
begin
  if (State = ssRunning) then
    FDWS2Program.EndProgram;
end;

function TgxScriptDWS.Call(aName: String; aParams: array of Variant): Variant;
var
  Symbol: TSymbol;
  Output: IInfo;
begin
  if (State <> ssRunning) then
    Start;
  if State = ssRunning then
  begin
    Symbol := FDWSProgram.Table.FindSymbol(aName);
    if Assigned(Symbol) then
    begin
      if Symbol is TFuncSymbol then
      begin
        Output := FDWSProgram.Info.Func[aName].Call(aParams);
        if Assigned(Output) then
          Result := Output.Value;
      end
      else
        raise Exception.Create('Expected TFuncSymbol but found ' + Symbol.ClassName +
          ' for ' + aName);
    end
    else
      raise Exception.Create('Symbol not found for ' + aName);
  end;
end;

procedure TgxScriptDWS.SetCompiler(const Value: TgxDelphiWebScript);
begin
  if Value <> FCompiler then
  begin
    FCompiler := Value;
    Invalidate;
  end;
end;

function TgxScriptDWS.GetState: TgxScriptState;
begin
  Result := ssUncompiled;
  if Assigned(FDWSProgram) then
  begin
    case FDWSProgram.ProgramState of
      psReadyToRun:
        Result := ssCompiled;
      psRunning:
        Result := ssRunning;
    else
      if FDWSProgram.Msgs.HasErrors then
      begin
        if FDWSProgram.Msgs.HasCompilerErrors then
          Result := ssCompileErrors
        else if FDWS2Program.Msgs.HasExecutionErrors then
          Result := ssRunningErrors;
        Errors.Text := FDWS2Program.Msgs.AsInfo;
      end;
    end;
  end;
end;

// --------------------------------------------------
initialization

// --------------------------------------------------

RegisterXCollectionItemClass(TgxScriptDWS);

// --------------------------------------------------
finalization
// --------------------------------------------------

UnregisterXCollectionItemClass(TgxScriptDWS);

end.
