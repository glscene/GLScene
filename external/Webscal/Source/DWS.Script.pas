//
// The graphics rendering engine GLScene http://glscene.org
//
unit DWS.Script;

(* DelphiWebScript implementation for the GLScene scripting layer *)

interface

uses
  System.Classes,
  System.SysUtils,

  GLS.XCollection,
  GLS.ScriptBase,
  dwsComp,
  dwsExprs,
  dwsSymbols,
  GLS.Manager;

type
  (* This class only adds manager registration logic to the TDelphiWebScriptII
    class to enable the XCollection items (ie. TGLScriptDWS) retain it's
    assigned compiler from design to run -time. *)
  TGLDelphiWebScript = class(TDelphiWebScript)
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
  end;

  // Implements DelphiWebScript scripting functionality through the abstracted GLS.ScriptBase
  TGLScriptDWS = class(TGLScriptBase)
  private
    FDWSProgram: TProgram;
    FCompiler: TGLDelphiWebScript;
    FCompilerName: String;
  protected
    procedure SetCompiler(const Value: TGLDelphiWebScriptII);
    procedure ReadFromFiler(reader: TReader); override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function GetState: TGLScriptState; override;
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
    property Compiler: TGLDelphiWebScriptII read FCompiler write SetCompiler;
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
  RegisterClasses([TGLDelphiWebScript, TGLScriptDWS]);
  RegisterComponents('GLScene DWS', [TGLDelphiWebScript]);
end;


// ----------
// ---------- TGLDelphiWebScript ----------
// ----------

constructor TGLDelphiWebScript.Create(AOnwer: TComponent);
begin
  inherited;
  RegisterManager(Self);
end;

destructor TGLDelphiWebScript.Destroy;
begin
  DeregisterManager(Self);
  inherited;
end;


// ---------------
// --------------- TGLScriptDWS ---------------
// ---------------

destructor TGLScriptDWS.Destroy;
begin
  Invalidate;
  inherited;
end;

procedure TGLScriptDWS.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TGLScriptDWS then
  begin
    Compiler := TGLScriptDWS(Source).Compiler;
  end;
end;

procedure TGLScriptDWS.ReadFromFiler(reader: TReader);
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

procedure TGLScriptDWS.WriteToFiler(writer: TWriter);
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

procedure TGLScriptDWS.Loaded;
var
  temp: TComponent;
begin
  inherited;
  if FCompilerName <> '' then
  begin
    temp := FindManager(TGLDelphiWebScript, FCompilerName);
    if Assigned(temp) then
      Compiler := TGLDelphiWebScript(temp);
    FCompilerName := '';
  end;
end;

procedure TGLScriptDWS.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = Compiler) and (Operation = opRemove) then
    Compiler := nil;
end;

class function TGLScriptDWS.FriendlyName: String;
begin
  Result := 'GLScriptDWS';
end;

class function TGLScriptDWS.FriendlyDescription: String;
begin
  Result := 'DelphiWebScript script';
end;

class function TGLScriptDWS.ItemCategory: String;
begin
  Result := '';
end;

procedure TGLScriptDWS.Compile;
begin
  Invalidate;
  if Assigned(Compiler) then
    FDWS2Program := Compiler.Compile(Text.Text)
  else
    raise Exception.Create('No compiler assigned!');
end;

procedure TGLScriptDWS.Execute;
begin
  if (State = ssUncompiled) then
    Compile
  else if (State = ssRunning) then
    Stop;
  if (State = ssCompiled) then
    FDWS2Program.Execute;
end;

procedure TGLScriptDWS.Invalidate;
begin
  if (State <> ssUncompiled) or Assigned(FDWSProgram) then
  begin
    Stop;
    FreeAndNil(FDWSProgram);
  end;
end;

procedure TGLScriptDWS.Start;
begin
  if (State = ssUncompiled) then
    Compile;
  if (State = ssCompiled) then
    FDWS2Program.BeginProgram(False);
end;

procedure TGLScriptDWS.Stop;
begin
  if (State = ssRunning) then
    FDWS2Program.EndProgram;
end;

function TGLScriptDWS.Call(aName: String; aParams: array of Variant): Variant;
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
        raise Exception.Create('Expected TFuncSymbol but found ' +
          Symbol.ClassName + ' for ' + aName);
    end
    else
      raise Exception.Create('Symbol not found for ' + aName);
  end;
end;

procedure TGLScriptDWS.SetCompiler(const Value: TGLDelphiWebScript);
begin
  if Value <> FCompiler then
  begin
    FCompiler := Value;
    Invalidate;
  end;
end;

function TGLScriptDWS.GetState: TGLScriptState;
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
        else if FDWSProgram.Msgs.HasExecutionErrors then
          Result := ssRunningErrors;
        Errors.Text := FDWSProgram.Msgs.AsInfo;
      end;
    end;
  end;
end;

// --------------------------------------------------
initialization
// --------------------------------------------------

RegisterXCollectionItemClass(TGLScriptDWS);

// --------------------------------------------------
finalization
// --------------------------------------------------

UnregisterXCollectionItemClass(TGLScriptDWS);

end.
