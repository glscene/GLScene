unit FDXPCompileLog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DXPExpertModule, ComCtrls, FDXPProgress;

type
  TDXPCompileLog = class(TForm)
    PageControl: TPageControl;
    TSRaw: TTabSheet;
    MERaw: TMemo;
    TSConfigFile: TTabSheet;
    MECfgFile: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    DXPExpertModule : TDMDXPExpertModule;
    procedure ExecuteOnFPC(const prjFileName, logFileName, linkLogFileName : String;
                           expertModule : TDMDXPExpertModule;
                           progress : TDXPProgress);
  end;

function DXPCompileLog : TDXPCompileLog;
procedure ReleaseDXPCompileLog;
function DXPCompileLogVisible : Boolean;

implementation

{$R *.dfm}

uses ToolsAPI, DXPExpertUnit, DXPGlobals;

var
   vDXPCompileLog : TDXPCompileLog;

// DXPCompileLog
//
function DXPCompileLog : TDXPCompileLog;
begin
   if vDXPCompileLog=nil then
      vDXPCompileLog:=TDXPCompileLog.Create(nil);
   Result:=vDXPCompileLog;
end;

// ReleaseDXPCompileLog
//
procedure ReleaseDXPCompileLog;
begin
   if Assigned(vDXPCompileLog) then begin
      if vDXPCompileLog.Visible then
         vDXPCompileLog.Close
      else vDXPCompileLog.Free;
      vDXPCompileLog:=nil;
   end;
end;

// DXPCompileLogVisible
//
function DXPCompileLogVisible : Boolean;
begin
   if Assigned(vDXPCompileLog) then
      Result:=vDXPCompileLog.Visible
   else Result:=False;
end;

// FormClose
//
procedure TDXPCompileLog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   vDXPCompileLog:=nil;
   Action:=caFree;   
end;

// ExecuteOnFPC
//
procedure TDXPCompileLog.ExecuteOnFPC(const prjFileName, logFileName, linkLogFileName : String;
                                      expertModule : TDMDXPExpertModule;
                                      progress : TDXPProgress);
var
   i, pOpen, pClose, pComma, pDots, colNb, lineNb : Integer;
   line, fName, msgText, location, msgType : String;
   msgServices : IOTAMessageServices;
   msgGroup : IOTAMessageGroup;
   lineRef : Pointer;
   linkerErrors : TStrings;
   nbErrors, nbWarnings, nbHints, nbNotes : Integer;
begin
   DXPExpertModule:=expertModule;
   msgServices:=(BorlandIDEServices as IOTAMessageServices);
   msgServices.ClearToolMessages;
   msgServices.ClearCompilerMessages;
   msgGroup:=msgServices.GetGroup('FreePascal');
   if msgGroup=nil then
      msgGroup:=msgServices.AddMessageGroup('FreePascal')
   else msgServices.ClearToolMessages(msgGroup);
   if FileExists(logFileName) then begin
      MERaw.Lines.LoadFromFile(logFileName);
   end else begin
      MERaw.Lines.Text:='No compiler error output.';
      msgServices.AddCompilerMessage(prjFileName, 'No compiler output', 'FPC',
                                     otamkFatal, 0, 0, nil, lineRef);
   end;
   if FileExists(vFPC_BinaryPath+'\fpc.cfg') then
      MECfgFile.Lines.LoadFromFile(vFPC_BinaryPath+'\fpc.cfg')
   else MECfgFile.Clear;
   MERaw.Lines.Insert(0, DateTimeToStr(Now));
   nbErrors:=0;
   nbWarnings:=0;
   nbHints:=0;
   nbNotes:=0;
   // parse
   for i:=0 to MERaw.Lines.Count-1 do begin
      line:=MERaw.Lines[i];
      pOpen:=Pos('(', line);
      pClose:=Pos(') ', line);
      pComma:=Pos(',', line);
      if (pOpen>0) and (pClose>0) and (pComma>pOpen) and (pComma<pClose) then begin
         fName:=Copy(line, 1, pOpen-1);
         location:=Copy(line, pOpen+1, pClose-pOpen-1);
         msgText:=Copy(line, pClose+2, MaxInt);

         pDots:=Pos(':', msgText);
         msgType:=LowerCase(Copy(msgText, 1, pDots-1));
         if CompareText(msgType, 'warning')=0 then
            Inc(nbWarnings)
         else if CompareText(msgType, 'hint')=0 then
            Inc(nbHints)
         else if CompareText(msgType, 'note')=0 then
            Inc(nbNotes)
         else Inc(nbErrors);

         pComma:=Pos(',', location);
         lineNb:=StrToIntDef(Copy(location, 1, pComma-1), 1);
         colNb:=StrToIntDef(Copy(location, pComma+1, MaxInt), 1);

         msgServices.AddToolMessage(DMDXPExpertModule.FPCLocateFile(fName),
                                    msgText, 'FPC', lineNb, colNb, nil,
                                    lineRef, msgGroup);
      end else if CompareText(Copy(line, 1, 6), 'Fatal:')=0 then begin
         msgServices.AddToolMessage(prjFileName, line, 'FPC',
                                    0, 0, nil, lineRef, msgGroup);
         Inc(nbErrors);
      end;
   end;
   // Linker errors
   if FileExists(linkLogFileName) then begin
      linkerErrors:=TStringList.Create;
      try
         linkerErrors.LoadFromFile(linkLogFileName);
         for i:=0 to linkerErrors.Count-1 do begin
            line:=linkerErrors[i];
            if line<>'' then begin
               msgServices.AddToolMessage(prjFileName, line, 'LD',
                                          0, 0, nil, lineRef, msgGroup);
            end;
            Inc(nbErrors);
         end;
      finally
         linkerErrors.Free;
      end;
   end;
   progress.SetStat(nbErrors, nbWarnings, nbHints, nbNotes);
   msgServices.ShowMessageView(msgGroup);
   if vFPC_ShowCompileLog then
      Show;
end;

end.
