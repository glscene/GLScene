//
// The graphics rendering engine GLScene http://glscene.org
//
unit DWS.Objects;
(*
  Base classes and logic for DelphiWebScript enabled objects
  in GLScene for vcl https://bitbucket.org/egrange/dwscript/src/master/
*)
interface

uses
  System.Classes,
  System.SysUtils,

  GLS.OpenGLTokens,
  GLS.Scene,
  GLS.XCollection,
  GLS.BaseClasses,
  GLS.Manager,

  DWS.Script,
  dwsComp,
  dwsExprs,
  dwsSymbols;

type
  (* A DelphiWebScript enabled behaviour. This behaviour also calls
    on the OnProgress and OnBeginProgram procedures in the script if
    they are found. Once compiled and executed the program remains
    active until killed, deactivated or the script is invalidated. *)
  TGLDWSActiveBehaviour = class(TGLBehaviour)
  private
    FActive: Boolean;
    FScript: TStringList;
    FDWSProgram: TProgram;
    FCompiler: TGLDelphiWebScript;
    FCompilerName: String;
    procedure SetActive(const Value: Boolean);
    procedure SetScript(const Value: TStringList);
    procedure SetCompiler(const Value: TGLDelphiWebScript);
    procedure CompileProgram;
    procedure BeginProgram;
    procedure EndProgram;
    procedure KillProgram;
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    class function FriendlyName: String; override;
    procedure DoProgress(const ProgressTimes: TGLProgressTimes); override;
    procedure InvalidateScript;
    property DWSProgram: TProgram read FDWSProgram;
  published
    property Active: Boolean read FActive write SetActive;
    property Script: TStringList read FScript write SetScript;
    property Compiler: TGLDelphiWebScript read FCompiler write SetCompiler;
  end;

procedure Register;

// --------------------------------------------------
implementation
// --------------------------------------------------

// ----------
// ---------- Miscellaneous ----------
// ----------

procedure Register;
begin
  RegisterClasses([TGLDWSActiveBehaviour]);
end;


// ----------
// ---------- TGLDWSActiveBehaviour ----------
// ----------

constructor TGLDWSActiveBehaviour.Create(AOwner: TXCollection);
begin
  inherited;
  FScript := TStringList.Create;
end;

destructor TGLDWSActiveBehaviour.Destroy;
begin
  KillProgram;
  FScript.Free;
  inherited;
end;

class function TGLDWSActiveBehaviour.FriendlyName: String;
begin
  Result := 'DWS Active Script';
end;

procedure TGLDWSActiveBehaviour.DoProgress(const ProgressTimes: TGLProgressTimes);
var
  Symbol: TSymbol;
begin
  inherited;
  if Assigned(FDWSProgram) then
  begin
    if FDWSProgram.ProgramState = psRunning then
    begin
      Symbol := DWSProgram.Table.FindSymbol('OnProgress');
      if Assigned(Symbol) then
        if Symbol is TFuncSymbol then
          DWSProgram.Info.Func['OnProgress']
            .Call([ProgressTimes.newTime, ProgressTimes.deltaTime]);
    end;
  end;
end;

procedure TGLDWSActiveBehaviour.Loaded;
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
    CompileProgram;
    if Active then
      BeginProgram;
  end;
end;

procedure TGLDWSActiveBehaviour.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    Active := ReadBoolean;
    FCompilerName := ReadString;
    Script.Text := ReadString;
  end;
end;

procedure TGLDWSActiveBehaviour.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    WriteBoolean(FActive);
    if Assigned(FCompiler) then
      WriteString(FCompiler.GetNamePath)
    else
      WriteString('');
    WriteString(Script.Text);
  end;
end;

procedure TGLDWSActiveBehaviour.CompileProgram;
begin
  if Assigned(Compiler) then
  begin
    KillProgram;
    FDWS2Program := Compiler.Compile(Script.Text);
    if Active then
      BeginProgram;
  end;
end;

procedure TGLDWSActiveBehaviour.BeginProgram;
var
  Symbol: TSymbol;
  ObjectID: Variant;
  Obj: TGLBaseSceneObject;
begin
  if Assigned(DWSProgram) then
  begin
    if DWSProgram.ProgramState = psReadyToRun then
    begin
      DWSProgram.BeginProgram;
      if FDWSProgram.ProgramState = psRunning then
      begin
        Symbol := DWSProgram.Table.FindSymbol('OnBeginProgram');
        if Assigned(Symbol) then
          if Symbol is TFuncSymbol then
          begin
            Obj := OwnerBaseSceneObject;
            if Assigned(Obj) then
            begin
              ObjectID := DWSProgram.Info.RegisterExternalObject(Obj,
                False, False);
              DWSProgram.Info.Func['OnBeginProgram'].Call([ObjectID]);
            end;
          end;
      end;
    end;
  end;
end;

procedure TGLDWSActiveBehaviour.EndProgram;
begin
  if Assigned(DWSProgram) then
  begin
    if DWSProgram.ProgramState = psRunning then
      DWSProgram.EndProgram;
  end;
end;

procedure TGLDWSActiveBehaviour.KillProgram;
begin
  if Assigned(DWSProgram) then
  begin
    EndProgram;
    FreeAndNil(FDWSProgram);
  end;
end;

procedure TGLDWSActiveBehaviour.InvalidateScript;
begin
  KillProgram;
  CompileProgram;
end;

procedure TGLDWSActiveBehaviour.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    EndProgram;
    FActive := Value;
    if Active then
      BeginProgram;
  end;
end;

procedure TGLDWSActiveBehaviour.SetScript(const Value: TStringList);
begin
  if Assigned(Value) then
  begin
    KillProgram;
    FScript.Assign(Value);
    if Assigned(Compiler) then
    begin
      CompileProgram;
      if Active then
        BeginProgram;
    end;
  end;
end;

procedure TGLDWSActiveBehaviour.SetCompiler(const Value: TGLDelphiWebScript);
begin
  if Value <> FCompiler then
  begin
    if Assigned(FCompiler) then
      KillProgram;
    FCompiler := Value;
    if Assigned(FCompiler) then
    begin
      RegisterManager(FCompiler);
      CompileProgram;
      if Active then
        BeginProgram;
    end;
  end;
end;

// --------------------------------------------------
initialization
// --------------------------------------------------

RegisterXCollectionItemClass(TGLDWSActiveBehaviour);

// --------------------------------------------------
finalization

// --------------------------------------------------

UnregisterXCollectionItemClass(TGLDWSActiveBehaviour);

end.
