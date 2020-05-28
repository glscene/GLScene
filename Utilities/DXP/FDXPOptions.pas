// FDXPOptions
{
   General DXP options.

   Licensed under MPL (http://www.mozilla.org/MPL/)

   Copyright 2003 - Eric Grange
}
unit FDXPOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TDXPOptions = class(TForm)
    Panel1: TPanel;
    PageControl1: TPageControl;
    TSFreePascal: TTabSheet;
    BUOk: TButton;
    BUCancel: TButton;
    Label1: TLabel;
    EDFPCBinary: TEdit;
    BUFPCBinary: TButton;
    Label2: TLabel;
    EDFPCSourcePaths: TEdit;
    BUFPCSource: TButton;
    CBShowCompileLog: TCheckBox;
    Label3: TLabel;
    EDFPCRoot: TEdit;
    BUFPCRoot: TButton;
    Label4: TLabel;
    EDFPCLibraryPaths: TEdit;
    BUFPCLibrary: TButton;
    procedure BUFPCBinaryClick(Sender: TObject);
    procedure BUFPCSourceClick(Sender: TObject);
    procedure BUFPCRootClick(Sender: TObject);
    procedure BUFPCLibraryClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    function Execute : Boolean;
  end;

implementation

{$R *.dfm}

uses DXPGlobals, FDXPDirectoryDialog, FDXPPathsDialog;

// Execute
//
function TDXPOptions.Execute : Boolean;
begin
   // prepare the dialog

   // FPC
   EDFPCRoot.Text:=vFPC_RootPath;
   EDFPCBinary.Text:=vFPC_BinaryPath;
   EDFPCLibraryPaths.Text:=vFPC_LibraryPaths;
   EDFPCSourcePaths.Text:=vFPC_SourcePaths;
   CBShowCompileLog.Checked:=vFPC_ShowCompileLog;

   Result:=(ShowModal=mrOk);

   if Result then begin
      // store values

      // FPC
      vFPC_RootPath:=EDFPCRoot.Text;
      vFPC_BinaryPath:=EDFPCBinary.Text;
      vFPC_LibraryPaths:=EDFPCLibraryPaths.Text;
      vFPC_SourcePaths:=EDFPCSourcePaths.Text;
      vFPC_ShowCompileLog:=CBShowCompileLog.Checked;
   end;
end;

procedure TDXPOptions.BUFPCRootClick(Sender: TObject);
begin
   DXPDirectoryDialog(EDFPCRoot);
   if EDFPCBinary.Text='' then
      EDFPCBinary.Text:=EDFPCRoot.Text+'\bin\win32';
end;

procedure TDXPOptions.BUFPCBinaryClick(Sender: TObject);
begin
   DXPDirectoryDialog(EDFPCBinary);
end;

procedure TDXPOptions.BUFPCLibraryClick(Sender: TObject);
var
   buf : String;
begin
   buf:=EDFPCLibraryPaths.Text;
   if DXPPathsDialog(buf) then
      EDFPCLibraryPaths.Text:=buf;
end;

procedure TDXPOptions.BUFPCSourceClick(Sender: TObject);
var
   buf : String;
begin
   buf:=EDFPCSourcePaths.Text;
   if DXPPathsDialog(buf) then
      EDFPCSourcePaths.Text:=buf;
end;

end.
