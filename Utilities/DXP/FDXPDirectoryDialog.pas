// FDXPOptions
{
   Directory selection dialog for DXP.

   Licensed under MPL (http://www.mozilla.org/MPL/)

   Copyright 2003 - Eric Grange
}
unit FDXPDirectoryDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ShellCtrls;

type
  TDXPDirectoryDialog = class(TForm)
    ShellTreeView: TShellTreeView;
    BUOk: TButton;
    BUCancel: TButton;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    function Execute(edit : TEdit) : Boolean;
  end;

function DXPDirectoryDialog(edit : TEdit) : Boolean;

implementation

{$R *.dfm}

function DXPDirectoryDialog(edit : TEdit) : Boolean;
begin
   with TDXPDirectoryDialog.Create(nil) do begin
      try
         Result:=Execute(edit);
      finally
         Free;
      end;
   end;
end;

function TDXPDirectoryDialog.Execute(edit : TEdit) : Boolean;
begin
   try
      ShellTreeView.Path:=edit.Text;
   except
      // ignore issues
   end;
   Result:=(ShowModal=mrOk);
   if Result then
      edit.Text:=ShellTreeView.Path;
end;

end.
