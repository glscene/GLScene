// DXPExpertModule
{
   Provides MenuItems, common resources, and event handling for the DXP Expert.

   Licensed under MPL (http://www.mozilla.org/MPL/)

   Copyright 2003 - Eric Grange
}
unit DXPExpertModule;

interface

uses
  Windows, SysUtils, Forms, Classes, Menus, ToolsAPI, Dialogs, ActnList, ImgList,
  Graphics, Controls, DXPFPCConfig;

type
  TDMDXPExpertModule = class(TDataModule)
    PMFreePascal: TPopupMenu;
    MIExecute: TMenuItem;
    N2: TMenuItem;
    MICompile: TMenuItem;
    MIBuild: TMenuItem;
    ActionList: TActionList;
    ACFPCExecute: TAction;
    ACFPCBuild: TAction;
    ACFPCCompile: TAction;
    ACFPCOptions: TAction;
    PMDXP: TPopupMenu;
    MenuItem1: TMenuItem;
    ACDXPOptions: TAction;
    N1: TMenuItem;
    View1: TMenuItem;
    MICompilerMessages: TMenuItem;
    ACViewCompilerMessages: TAction;
    N3: TMenuItem;
    Options1: TMenuItem;
    procedure ACDXPOptionsExecute(Sender: TObject);
    procedure ACFPCCompileExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure ACViewCompilerMessagesExecute(Sender: TObject);
    procedure ACFPCExecuteExecute(Sender: TObject);
    procedure ACFPCBuildExecute(Sender: TObject);
    procedure ACFPCOptionsExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);

  private
    { Déclarations privées }
    FFPCConfig : TDXPFPCConfig;
    FFPCConfigFileName : String;
    FFPCCFGBackedUp : Boolean;

    procedure AddMenuInIDE(popup : TPopupMenu; const aDelphiMenu : String);
    function GetProjectGroup : IOTAProjectGroup;
    function GetProject : IOTAProject;
    function GetModule(const moduleName : String) : IOTAModule;
    function ProjectBinaryName : String;

    function FPCConfig : TDXPFPCConfig;
    function FPCCommandLine(const extraOptions : String = '') : String;
    procedure BackupFPCConfigFile;
    procedure RestoreFPCConfigFile;
    function FPCErrorFile : String;
    function FPCLinkerErrorFile : String;
    //: Returns True if compilation succeeded
    function FPCCompile(const compileType : String; const extraOptions : String = '') : Boolean;

  public
    { Déclarations publiques }
    FOTAServices : IOTAServices;
    FNTAServices : INTAServices;

    procedure HookIDE;
    procedure UnHookIDE;

    procedure WarpTo(const moduleName : String; col, line : Integer);

    function FPCLocateFile(const fileName : String) : String;
  end;

implementation

{$R *.dfm}

uses DXPGlobals, FDXPOptions, DXPUtils, FDXPCompileLog, FDXPFPCOptions,
   FDXPProgress;

procedure TDMDXPExpertModule.DataModuleCreate(Sender: TObject);
begin
   FFPCConfig:=nil; // initialized upon request
end;

procedure TDMDXPExpertModule.DataModuleDestroy(Sender: TObject);
begin
   FFPCConfig.Free;
end;

procedure TDMDXPExpertModule.AddMenuInIDE(popup : TPopupMenu; const aDelphiMenu : String);
var
   k, i : Integer;
   mm : TMainMenu;
begin
   mm:=FNTAServices.MainMenu;
   k:=-1;
   if aDelphiMenu<>'' then for i:=0 to mm.Items.Count-1 do begin
      if mm.Items[i].Name=aDelphiMenu then begin
         k:=i;
         Break;
      end;
   end;
   if k>=0 then
      mm.Items.Insert(k+1, popup.Items)
   else mm.Items.Add(popup.Items);
   popup.Items.Caption:=Copy(popup.Name, 3, MaxInt);
   popup.Images:=mm.Images;
end;

procedure TDMDXPExpertModule.HookIDE;
begin
   ActionList.Images:=FNTAServices.MainMenu.Images;
   AddMenuInIDE(PMFreePascal, 'RunMenu');
   AddMenuInIDE(PMDXP, '');
end;

procedure TDMDXPExpertModule.UnHookIDE;
var
   mm : TMainMenu;
begin
   mm:=FNTAServices.MainMenu;
   mm.Items.Remove(PMFreePascal.Items);
   mm.Items.Remove(PMDXP.Items);
end;

function TDMDXPExpertModule.GetProjectGroup : IOTAProjectGroup;
var
   IModuleServices : IOTAModuleServices;
   i : Integer;
begin
   Result:=nil;
   IModuleServices:=BorlandIDEServices as IOTAModuleServices;
   for i:=0 to IModuleServices.ModuleCount-1 do
      if Supports(IModuleServices.Modules[i], IOTAProjectGroup, Result) then
         Break;
end;

function TDMDXPExpertModule.GetProject : IOTAProject;
var
   grp : IOTAProjectGroup;
begin
   grp:=GetProjectGroup;
   if grp<>nil then
      Result:=grp.ActiveProject
   else Result:=nil;
end;

function TDMDXPExpertModule.ProjectBinaryName : String;
var
   prj : IOTAProject;
begin
   prj:=GetProject;
   if Assigned(prj) then
      Result:=ChangeFileExt(prj.FileName, '.exe')
   else Result:='';
end;

function TDMDXPExpertModule.GetModule(const moduleName : String) : IOTAModule;
var
   i : Integer;
   modules : IOTAModuleServices;
begin
   modules:=(BorlandIDEServices as IOTAModuleServices);
   Result:=nil;
   for i:=0 to modules.ModuleCount-1 do begin
      if CompareText(ExtractFileName(modules.Modules[i].FileName), moduleName)=0 then begin
         Result:=modules.Modules[i];
         Break;
      end;
   end;
end;

procedure TDMDXPExpertModule.WarpTo(const moduleName : String; col, line : Integer);
var
   i, j : Integer;
   module : IOTAModule;
   editor : IOTASourceEditor;
   editPos : TOTAEditPos;
   view : IOTAEditView;
   fileName : String;
begin
   module:=GetModule(moduleName);
   if not Assigned(module) then begin
      fileName:=FindFileInPaths(moduleName, vFPC_SourcePaths);
      if fileName<>'' then begin
         (BorlandIDEServices as IOTAActionServices).OpenFile(fileName);
         module:=GetModule(moduleName);
      end;
   end;
   if Assigned(module) then begin
      for i:=0 to module.ModuleFileCount-1 do begin
         module.ModuleFileEditors[i].QueryInterface(IOTASourceEditor, editor);
         if Assigned(editor) then begin
            editor:=(module.ModuleFileEditors[i] as IOTASourceEditor);
            editor.Show;
            editPos.Col:=col;
            editPos.Line:=line;
            for j:=0 to editor.EditViewCount-1 do begin
               view:=editor.EditViews[j];
               view.CursorPos:=editPos;
               view.MoveViewToCursor;
               view.Paint;
            end;
         end;
      end;
   end;
end;

function TDMDXPExpertModule.FPCLocateFile(const fileName : String) : String;

   function LocateInDirectory(const fileName, directory : String) : String;
   var
      sr : TSearchRec;
   begin
      if directory<>'' then begin
         if directory[Length(directory)]='\' then begin
            if FindFirst(directory+fileName, faAnyFile, sr)=0 then
               Result:=directory+sr.Name
            else Result:='';
         end else begin
            if FindFirst(directory+'\'+fileName, faAnyFile, sr)=0 then
               Result:=directory+'\'+sr.Name
            else Result:='';
         end;
         FindClose(sr);
      end else Result:='';
   end;

var
   i : Integer;
   paths : TStringList;
   prj : IOTAProject;
begin
   prj:=GetProject;
   if prj<>nil then begin
      Result:=LocateInDirectory(fileName, ExtractFilePath(prj.FileName));
      if Result<>'' then Exit;
   end;
   paths:=TStringList.Create;
   try
      StringToPaths(vFPC_SourcePaths, paths);
      for i:=0 to paths.Count-1 do begin
         Result:=LocateInDirectory(fileName, MacroExpandPath(paths[i]));
         if Result<>'' then Exit;
      end;
   finally
      paths.Free;
   end;
   Result:=fileName;
end;

function TDMDXPExpertModule.FPCConfig : TDXPFPCConfig;
var
   cfgFileName : String;
begin
   cfgFileName:=ChangeFileExt(GetProject.FileName, '.fpc-cfg');
   if cfgFileName<>FFPCConfigFileName then
      FreeAndNil(FFPCConfig);
   if not Assigned(FFPCConfig) then begin
      FFPCConfig:=TDXPFPCConfig.Create;
      FFPCConfigFileName:=cfgFileName;
      if FileExists(FFPCConfigFileName) then
         FFPCConfig.LoadFromFile(FFPCConfigFileName);
   end;
   Result:=FFPCConfig;
end;

procedure TDMDXPExpertModule.BackupFPCConfigFile;
var
   cfgFile, cfgFileBkp : String;
   sl : TStringList;
begin
   FFPCCFGBackedUp:=False;
   cfgFile:=vFPC_BinaryPath+'\fpc.cfg';
   if FileExists(cfgFile) then begin
      sl:=TStringList.Create;
      try
         sl.LoadFromFile(cfgFile);
         if (sl.Count>0) and (sl[0]<>'# DXP') then begin
            cfgFileBkp:=vFPC_BinaryPath+'\fpc.cfg.bak';
            DeleteFile(cfgFileBkp);
            RenameFile(cfgFile, cfgFileBkp);
            FFPCCFGBackedUp:=True;
         end;
      finally
         sl.Free;
      end;
   end;
end;

procedure TDMDXPExpertModule.RestoreFPCConfigFile;
var
   cfgFile, cfgFileBkp : String;
begin
   if FFPCCFGBackedUp then begin
      FFPCCFGBackedUp:=False;
      cfgFile:=vFPC_BinaryPath+'\fpc.cfg';
      cfgFileBkp:=vFPC_BinaryPath+'\fpc.cfg.bak';
      DeleteFile(cfgFile);
      RenameFile(cfgFileBkp, cfgFile);
   end;
end;

function TDMDXPExpertModule.FPCCommandLine(const extraOptions : String = '') : String;
var
   i : Integer;
   prj : IOTAProject;
   paths : TStringList;
   pathName : String;
   cfgFile : TStringList;
   configOptions : String;
   config : TDXPFPCConfig;
begin
   Result:='';
   prj:=GetProject;
   if not Assigned(prj) then Exit;
   configOptions:='';
   config:=FPCConfig;
   cfgFile:=TStringList.Create;
   try
      for i:=0 to config.Options.Count-1 do
         configOptions:=configOptions+' '+config.Options[i];
      cfgFile.CommaText:=configOptions;
      cfgFile.Insert(0, '# DXP');
      cfgFile.Insert(1, '-Sd');
//      cfgFile.Insert(1, '-Mobjfpc');
      cfgFile.Insert(2, '-l');
      cfgFile.Insert(2, '-k -Map d:\map.txt');
      cfgFile.Insert(2, '-CX');
      Result:= vFPC_BinaryPath+'\fpc.exe '+extraOptions
              +' -Fe'+FPCErrorFile+' 2> '+FPCLinkerErrorFile;
      paths:=TStringList.Create;
      try
         StringToPaths(vFPC_LibraryPaths, paths);
         for i:=0 to paths.Count-1 do begin
            pathName:=MacroExpandPath(paths[i]);
//            cfgFile.Add('-Fu'+pathName);
            cfgFile.Add('-Fo'+pathName);
            cfgFile.Add('-Fl'+pathName);
         end;
         StringToPaths(vFPC_SourcePaths, paths);
         for i:=0 to paths.Count-1 do begin
            pathName:=MacroExpandPath(paths[i]);
            cfgFile.Add('-Fu'+pathName);
            cfgFile.Add('-Fi'+pathName);
         end;
      finally
         paths.Free;
      end;
      Result:=Result+' "'+prj.FileName+'"';
      cfgFile.SaveToFile(vFPC_BinaryPath+'\fpc.cfg');
   finally
      cfgFile.Free;
   end;
end;

function TDMDXPExpertModule.FPCErrorFile : String;
begin
   Result:='c:\dxp.tmp';
end;

function TDMDXPExpertModule.FPCLinkerErrorFile : String;
begin
   Result:='c:\dxp-link.tmp';
end;

function TDMDXPExpertModule.FPCCompile(const compileType : String;
                                       const extraOptions : String = '') : Boolean;
var
   res : Integer;
   cmdLine, verbose, verboseLink : String;
   prj : IOTAProject;
   progress : TDXPProgress;
begin
   Result:=False;
   prj:=GetProject;
   if prj=nil then Exit;
   LoadDXPGlobals;
   BackupFPCConfigFile;
   progress:=TDXPProgress.Create(nil);
   try
      progress.SetProject(ProjectBinaryName);
      progress.SetStatus('Compiling');
      progress.SetStat(0, 0, 0, 0);
      progress.Show;
      Application.ProcessMessages;
      cmdLine:=FPCCommandLine(extraOptions);
      if cmdLine='' then Exit;
      try
         verbose:=FPCErrorFile;
         verboseLink:=FPCLinkerErrorFile;
         DeleteFile(verbose);
         Screen.Cursor:=crHourGlass;
         try
            res:=ExecuteAndWait(cmdLine, SW_SHOWMINNOACTIVE, vFPC_TimeOut, True);
         finally
            Screen.Cursor:=crDefault;
         end;
         if res=-1 then
            progress.SetStatus('Failed to start compiler')
         else begin
            if res=0 then
               Result:=True;
            DXPCompileLog.ExecuteOnFPC(prj.FileName, verbose, verboseLink, Self,
                                       progress);
            with DXPCompileLog.MERaw.Lines do begin
               Insert(0, cmdLine);
               Insert(1, '');
            end;
         end;
         if Result then
            progress.SetStatus(compileType+' successful')
         else progress.SetStatus(compileType+' failed');
         progress.Timer.Enabled:=False;
         progress.BUOk.Enabled:=True;
         while progress.Visible do begin
            Sleep(100);
            Application.ProcessMessages;
         end;
      finally
         DeleteFile(verbose);
         DeleteFile(verboseLink);
      end;
   finally
      progress.Release;
      RestoreFPCConfigFile;
   end;
end;

procedure TDMDXPExpertModule.ACFPCExecuteExecute(Sender: TObject);
begin
   if FPCCompile('Compile & Execute') then
      WinExec(PAnsiChar(AnsiString(ProjectBinaryName)), SW_SHOW);
end;

procedure TDMDXPExpertModule.ACFPCCompileExecute(Sender: TObject);
begin
   (BorlandIDEServices as IOTAModuleServices).SaveAll;
   FPCCompile('Compilation');
end;

procedure TDMDXPExpertModule.ACFPCBuildExecute(Sender: TObject);
begin
   (BorlandIDEServices as IOTAModuleServices).SaveAll;
   FPCCompile('Build', '-B');
end;

procedure TDMDXPExpertModule.ACDXPOptionsExecute(Sender: TObject);
begin
   LoadDXPGlobals;
   with TDXPOptions.Create(nil) do begin
      try
         if Execute then
            StoreDXPGlobals;
      finally
         Free;
      end;
   end;
end;

procedure TDMDXPExpertModule.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
   gotProject : Boolean;
begin
   gotProject:=(GetProject<>nil);
   ACFPCCompile.Enabled:=gotProject;
   ACFPCCompile.ShortCut:=ShortCut(VK_F9, [ssCtrl, ssShift]);
   ACFPCBuild.Enabled:=gotProject;
   ACFPCExecute.Enabled:=gotProject;
   ACFPCExecute.ShortCut:=ShortCut(VK_F9, [ssShift]);
   ACFPCOptions.Enabled:=gotProject;
   ACViewCompilerMessages.Checked:=DXPCompileLogVisible;
   Handled:=True;
end;

procedure TDMDXPExpertModule.ACViewCompilerMessagesExecute(
  Sender: TObject);
begin
   if DXPCompileLog.Visible then
      DXPCompileLog.Hide
   else DXPCompileLog.Show;
end;

procedure TDMDXPExpertModule.ACFPCOptionsExecute(Sender: TObject);
var
   config : TDXPFPCConfig;
begin
   with TDXPFPCOptions.Create(nil) do begin
      config:=FPCConfig;
      if Execute(config.Options) then
         config.SaveToFile(FFPCConfigFileName);
      Free;
   end;
end;

end.
