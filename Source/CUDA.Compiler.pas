//
// The graphics rendering engine GLScene http://glscene.org
//
unit CUDA.Compiler;

(*
  Component allows to compile the CUDA-source (*.cu) file.
  in design- and runtime.
  To work requires the presence of CUDA Toolkit 3.X and MS Visual Studio C++.
*)

interface

uses
  Winapi.Windows,
  Winapi.ShellAPI,
  Winapi.TlHelp32,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  VCL.Dialogs,

  CUDA.Parser,
  GLS.ApplicationFileIO;

type
  TGLCUDACompilerOutput = (codeUndefined, codePtx, codeCubin, codeGpu);

  (*
    compute_10 Basic features
    compute_11 + atomic memory operations on global memory
    compute_12 + atomic memory operations on shared memory
               + vote instructions
    compute_13 + double precision floating point support
    Compute_20 + FERMI support
  *)
  TGLCUDAVirtArch = (compute_10, compute_11, compute_12, compute_13, compute_20);

  (*
    sm_10 ISA_1 Basic features
    sm_11 + atomic memory operations on global memory
    sm_12 + atomic memory operations on shared memory
          + vote instructions
    sm_13 + double precision floating point support
    sm_20 + FERMI support.
    sm_21 + Unknown
  *)
  TGLCUDARealArch = (sm_10, sm_11, sm_12, sm_13, sm_20, sm_21);
  TGLCUDARealArchs = set of TGLCUDARealArch;

  TGLCUDACompiler = class(TComponent)
  private
    FNVCCPath: string;
    FCppCompilerPath: string;
    FProduct: TStringList;
    FProjectModule: string;
    FSourceCodeFile: string;
    FConsoleContent: string;
    FOutputCodeType: TGLCUDACompilerOutput;
    FVirtualArch: TGLCUDAVirtArch;
    FRealArch: TGLCUDARealArchs;
    FMaxRegisterCount: Integer;
    FModuleInfo: TCUDAModuleInfo;
    procedure SetMaxRegisterCount(Value: Integer);
    procedure SetOutputCodeType(const Value: TGLCUDACompilerOutput);
    function StoreProjectModule: Boolean;
    procedure SetRealArch(AValue: TGLCUDARealArchs);
    procedure SetNVCCPath(const AValue: string);
    procedure SetCppCompilerPath(const AValue: string);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetSourceCodeFile(const AFileName: string);
    function Compile: Boolean;
    //  Product of compilation.
    property Product: TStringList read FProduct write FProduct;
    property ModuleInfo: TCUDAModuleInfo read FModuleInfo;
    property ConsoleContent: string read FConsoleContent;
  published
    //  NVidia CUDA Compiler.
    property NVCCPath: string read FNVCCPath write SetNVCCPath;
    (*  Microsoft Visual Studio Compiler.
      Pascal compiler is still not done. *)
    property CppCompilerPath: string read FCppCompilerPath
      write SetCppCompilerPath;
    //  Full file name of source code file.
    property SourceCodeFile: string read FSourceCodeFile;
    (*  Disign-time only property.
      Make choose of one of the Project module as CUDA kernel source *)
    property ProjectModule: string read FProjectModule write FProjectModule
      stored StoreProjectModule;
    (*  Output code type for module kernel
      - Ptx - Parallel Thread Execution
      - Cubin - CUDA Binary *)
    property OutputCodeType: TGLCUDACompilerOutput read FOutputCodeType
      write setOutputCodeType default codePtx;
    (*  In the CUDA naming scheme,
        GPUs are named sm_xy,
        where x denotes the GPU generation number,
        and y the version in that generation. *)
    property RealArchitecture: TGLCUDARealArchs read FRealArch
      write SetRealArch default [sm_13];
    //  Virtual architecture.
    property VirtualArchitecture: TGLCUDAVirtArch read FVirtualArch
      write FVirtualArch default compute_13;
    //  Maximum registers that kernel can use.
    property MaxRegisterCount: Integer read FMaxRegisterCount
      write SetMaxRegisterCount default 32;
  end;

  TFindCuFileFunc = function(var AModuleName: string): Boolean;

var
  vFindCuFileFunc: TFindCuFileFunc;

//------------------------------------------------------------------
implementation
//------------------------------------------------------------------


// ------------------
// ------------------ TGLCUDACompiler ------------------
// ------------------

constructor TGLCUDACompiler.Create(AOwner: TComponent);
var
  path: string;
begin
  inherited Create(AOwner);
  FOutputCodeType := codePtx;
  FVirtualArch := compute_13;
  FRealArch := [sm_13];
  FMaxRegisterCount := 32;
  FNVCCPath := '';
  path := GetEnvironmentVariable('CUDA_BIN_PATH');
  if Length(path) > 0 then
  begin
    path := IncludeTrailingPathDelimiter(path);
    if FileExists(path + 'nvcc.exe') then
      FNVCCPath := path;
  end;
  path := 'C:\Program Files\Microsoft Visual Studio 2015\VC\bin\';
  if FileExists(path + 'cl.exe') then
    FCppCompilerPath := path
  else
        FCppCompilerPath := '';
  FProjectModule := 'none';
  FModuleInfo := TCUDAModuleInfo.Create;
end;

destructor TGLCUDACompiler.Destroy;
begin
  FModuleInfo.Destroy;
  inherited;
end;

procedure TGLCUDACompiler.Loaded;
var
  LStr: string;
begin
  inherited;
  if (FProjectModule <> 'none') and Assigned(vFindCuFileFunc) then
  begin
    LStr := FProjectModule;
    if vFindCuFileFunc(LStr) then
      FSourceCodeFile := LStr
    else
      FSourceCodeFile := '';
  end;
end;

procedure TGLCUDACompiler.Assign(Source: TPersistent);
var
  compiler: TGLCUDACompiler;
begin
  if Source is TGLCUDACompiler then
  begin
    compiler := TGLCUDACompiler(Source);
    FSourceCodeFile := compiler.FSourceCodeFile;
    FOutputCodeType := compiler.FOutputCodeType;
    FVirtualArch := compiler.FVirtualArch;
  end;
  inherited Assign(Source);
end;

function TGLCUDACompiler.Compile: Boolean;
const
  ReadBufferSize = 1048576; // 1 MB Buffer
  cSM: array[TGLCUDARealArch] of string =
    ('sm_10', 'sm_11', 'sm_12', 'sm_13', 'sm_20', 'sm_21');
var
  tepmPath, tempFile, tempFileExt: string;
  commands, nvcc, pathfile, msg: string;
  rArch: TGLCUDARealArch;
  CodeSource: TStringList;

  Security: TSecurityAttributes;
  ReadPipe, WritePipe: THandle;
  start: TStartUpInfo;
  ProcessInfo: TProcessInformation;
  Buffer: PAnsiChar;
  TotalBytesRead, BytesRead: DWORD;
  Apprunning, n, BytesLeftThisMessage, TotalBytesAvail: Integer;
begin
  if not FileExists(FSourceCodeFile) then
  begin
    if csDesigning in ComponentState then
      MessageDlg('Source File Not Found', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0)
    else
     {$IFDEF USE_LOGGING}
       LogError('Source File Not Found');
     {$ENDIF}
      exit(false);
  end;
  CodeSource := TStringList.Create;
  CodeSource.LoadFromFile(FSourceCodeFile);
  Result := false;
  FConsoleContent := '';

  if FileExists(FNVCCPath + 'nvcc.exe') and
    FileExists(FCppCompilerPath + 'cl.exe') and Assigned(FProduct) then
  begin
    tepmPath := GetEnvironmentVariable('TEMP');
    tepmPath := IncludeTrailingPathDelimiter(tepmPath);
    tempFile := tepmPath + 'temp';
    CodeSource.SaveToFile(tempFile + '.cu');
    commands := '"' + tempFile + '.cu" ';

    commands := commands + '-arch ';
    case FVirtualArch of
      compute_10:
        commands := commands + 'compute_10 ';
      compute_11:
        commands := commands + 'compute_11 ';
      compute_12:
        commands := commands + 'compute_12 ';
      compute_13:
        commands := commands + 'compute_13 ';
      compute_20:
        commands := commands + 'compute_20 ';
    end;

    commands := commands + '-code ';
    for rArch in FRealArch do
      commands := commands + cSM[rArch] + ', ';
    commands[Length(commands)-1] := ' ';

    commands := commands + '-ccbin ';
    pathfile := Copy(FCppCompilerPath, 1, Length(FCppCompilerPath) - 1);
    commands := commands + '"' + pathfile + '" ';
    commands := commands + '-Xcompiler "/EHsc /W3 /nologo /O2 /Zi /MT " ';
    commands := commands + '-maxrregcount=' + IntToStr(FMaxRegisterCount) + ' ';
    commands := commands + '-m32 ';
    case FOutputCodeType of
      codePtx:
        begin
          commands := commands + '--ptx ';
          tempFileExt := 'ptx';
        end;
      codeCubin:
        begin
          commands := commands + '--cubin ';
          tempFileExt := 'cubin';
        end;
      codeGpu:
        begin
          commands := commands + '--gpu ';
          tempFileExt := 'gpu';
        end;
    end;
    commands := commands + '-o "' + tempFile + '.' + tempFileExt + '" ';
    commands := commands + #00;
    nvcc := FNVCCPath + 'nvcc.exe ';

    with Security do
    begin
      nlength := SizeOf(TSecurityAttributes);
      binherithandle := true;
      lpsecuritydescriptor := nil;
    end;

    if CreatePipe(ReadPipe, WritePipe, @Security, 0) then
    begin
      // Redirect In- and Output through STARTUPINFO structure

      Buffer := AllocMem(ReadBufferSize + 1);
      FillChar(start, SizeOf(start), #0);
      start.cb := SizeOf(start);
      start.hStdOutput := WritePipe;
      start.hStdInput := ReadPipe;
      start.hStdError := WritePipe;
      start.dwFlags := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
      start.wShowWindow := SW_HIDE;

      // Creates a Console Child Process with redirected input and output
      if CreateProcess(nil, PChar(nvcc+commands), @Security, @Security, true,
        CREATE_NO_WINDOW or NORMAL_PRIORITY_CLASS, nil, nil, start,
        ProcessInfo) then
      begin
        n := 0;
        TotalBytesRead := 0;
        repeat
          // Increase counter to prevent an endless loop if the process is dead
          Inc(n, 1);

          // wait for end of child process
          Apprunning := WaitForSingleObject(ProcessInfo.hProcess, 100);
          Application.ProcessMessages;

          // it is important to read from time to time the output information
          // so that the pipe is not blocked by an overflow. New information
          // can be written from the console app to the pipe only if there is
          // enough buffer space.

          if not PeekNamedPipe(ReadPipe, @Buffer[TotalBytesRead],
            ReadBufferSize, @BytesRead, @TotalBytesAvail,
            @BytesLeftThisMessage) then
            break
          else if BytesRead > 0 then
            ReadFile(ReadPipe, Buffer[TotalBytesRead], BytesRead,
              BytesRead, nil);
          TotalBytesRead := TotalBytesRead + BytesRead;
        until (Apprunning <> WAIT_TIMEOUT) or (n > 150);

        Buffer[TotalBytesRead] := #00;
        OemToCharA(Buffer, Buffer);
      end
      else
      begin
        if csDesigning in ComponentState then
          MessageDlg('Fail Run NVCC', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0)
        else
         {$IFDEF USE_LOGGING}
            LogError('Fail Run NVCC');
         {$ENDIF}
      end;

      pathfile := tempFile + '.' + tempFileExt;
      if FileExists(pathfile) then
      begin
        FProduct.LoadFromFile(pathfile);
        FModuleInfo.ParseModule(CodeSource, FProduct);

        if csDesigning in ComponentState then
          FProduct.OnChange(Self);
        DeleteFile(pathfile);
        Result := true;
        FConsoleContent := string(StrPas(Buffer));
        msg := Format('Success Compilation', [FConsoleContent]);
        if csDesigning in ComponentState then
          MessageDlg(msg, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0)
        else
         {$IFDEF USE_LOGGING}
           LogInfo(msg);
         {$ENDIF}
      end
      else
      begin
        msg := Format('Fail Compilation', [StrPas(Buffer)]);
        if csDesigning in ComponentState then
          MessageDlg(msg, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0)
        else
          {$IFDEF USE_LOGGING}
            LogError(msg);
          {$ENDIF}
      end;
      FreeMem(Buffer);
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
      CloseHandle(ReadPipe);
      CloseHandle(WritePipe);
    end
    else
    begin
      if csDesigning in ComponentState then
        MessageDlg('Fail Create Pipe', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0)
      else
       {$IFDEF USE_LOGGING}
        GLSLogger.LogError(strFailCreatePipe);
       {$ENDIF}
    end;

    pathfile := tempFile + '.cu';
    DeleteFile(pathfile);
  end;
  CodeSource.Free;
end;

procedure TGLCUDACompiler.SetCppCompilerPath(const AValue: string);
begin
  if FileExists(AValue + 'cl.exe') then
    FCppCompilerPath := AValue;
end;

procedure TGLCUDACompiler.setMaxRegisterCount(Value: Integer);
begin
  if Value <> FMaxRegisterCount then
  begin
    Value := 4 * (Value div 4);
    if Value < 4 then
      Value := 4;
    if Value > 128 then
      Value := 128;
    FMaxRegisterCount := Value;
  end;
end;

procedure TGLCUDACompiler.SetNVCCPath(const AValue: string);
begin
  if FileExists(AValue + 'nvcc.exe') then
    FNVCCPath := AValue;
end;

procedure TGLCUDACompiler.setOutputCodeType(const Value
  : TGLCUDACompilerOutput);
begin
  if Value = codeUndefined then
    exit;
  FOutputCodeType := Value;
end;

procedure TGLCUDACompiler.SetRealArch(AValue: TGLCUDARealArchs);
begin
  if AValue = [] then
    AValue := [sm_10];
  FRealArch := AValue;
end;

procedure TGLCUDACompiler.SetSourceCodeFile(const AFileName: string);
begin
  if FileStreamExists(AFileName) then
    FSourceCodeFile := AFileName;
end;

function TGLCUDACompiler.StoreProjectModule: Boolean;
begin
  Result := FProjectModule <> 'none';
end;

end.
