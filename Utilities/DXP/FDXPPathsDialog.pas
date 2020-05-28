// FDXPOptions
{
   DXP Paths Dialog.

   Licensed under MPL (http://www.mozilla.org/MPL/)

   Copyright 2003 - Eric Grange
}
unit FDXPPathsDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ActnList;

type
  TDXPPathsDialog = class(TForm)
    Panel1: TPanel;
    BBReplace: TBitBtn;
    BBAdd: TBitBtn;
    BBRemove: TBitBtn;
    EDPath: TEdit;
    BBPickPath: TBitBtn;
    BBDown: TBitBtn;
    BBUp: TBitBtn;
    Panel2: TPanel;
    BUOk: TButton;
    BUCancel: TButton;
    LBPaths: TListBox;
    ActionList: TActionList;
    procedure BBUpClick(Sender: TObject);
    procedure BBDownClick(Sender: TObject);
    procedure BBReplaceClick(Sender: TObject);
    procedure BBAddClick(Sender: TObject);
    procedure BBRemoveClick(Sender: TObject);
    procedure BBPickPathClick(Sender: TObject);
    procedure LBPathsClick(Sender: TObject);
    procedure LBPathsKeyPress(Sender: TObject; var Key: Char);
    procedure EDPathChange(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    function Execute(var paths : String) : Boolean;
  end;

function DXPPathsDialog(var paths : String) : Boolean;

implementation

{$R *.dfm}

uses FDXPDirectoryDialog, DXPUtils;

// DXPPathsDialog
//
function DXPPathsDialog(var paths : String) : Boolean;
begin
   with TDXPPathsDialog.Create(nil) do begin
      try
         Result:=Execute(paths);
      finally
         Free;
      end;
   end;
end;

// Execute
//
function TDXPPathsDialog.Execute(var paths : String) : Boolean;
begin
   StringToPaths(paths, LBPaths.Items);
   LBPathsClick(Self);
   Result:=(ShowModal=mrOk);
   if Result then
      paths:=PathsToString(LBPaths.Items);
end;

procedure TDXPPathsDialog.LBPathsClick(Sender: TObject);
var
   sel : Boolean;
begin
   with LBPaths do begin
      sel:=(ItemIndex>=0);
      BBUp.Enabled:=(ItemIndex>0);
      BBDown.Enabled:=sel and (ItemIndex<Items.Count-1);
      BBReplace.Enabled:=sel and (EDPath.Text<>'') and (EDPath.Text<>Items[ItemIndex]);
      BBAdd.Enabled:=(EDPath.Text<>'') and (Items.IndexOf(EDPath.Text)<0);
      BBRemove.Enabled:=sel;
      if (Sender<>nil) and sel then
         EDPath.Text:=Items[ItemIndex];
   end;
end;

procedure TDXPPathsDialog.BBUpClick(Sender: TObject);
var
   i : Integer;
begin
   with LBPaths do begin
      i:=ItemIndex;
      Items.Move(i, i-1);
      ItemIndex:=i-1;
   end;
   LBPathsClick(Self);
end;

procedure TDXPPathsDialog.BBDownClick(Sender: TObject);
var
   i : Integer;
begin
   with LBPaths do begin
      i:=ItemIndex;
      Items.Move(i, i+1);
      ItemIndex:=i+1;
   end;
   LBPathsClick(Self);
end;

procedure TDXPPathsDialog.BBReplaceClick(Sender: TObject);
begin
   with LBPaths do
      Items[ItemIndex]:=EDPath.Text;
   LBPathsClick(Self);
end;

procedure TDXPPathsDialog.BBAddClick(Sender: TObject);
begin
   with LBPaths do begin
      Items.Add(EDPath.Text);
      ItemIndex:=Items.Count-1;
   end;
   LBPathsClick(Self);
end;

procedure TDXPPathsDialog.BBRemoveClick(Sender: TObject);
var
   i : Integer;
begin
   with LBPaths do begin
      i:=ItemIndex;
      Items.Delete(i);
      if i<Items.Count then
         ItemIndex:=i
      else ItemIndex:=Items.Count-1;
   end;
   LBPathsClick(Self);
end;

procedure TDXPPathsDialog.BBPickPathClick(Sender: TObject);
begin
   DXPDirectoryDialog(EDPath);
end;

procedure TDXPPathsDialog.LBPathsKeyPress(Sender: TObject; var Key: Char);
begin
   LBPathsClick(Self);
end;

procedure TDXPPathsDialog.EDPathChange(Sender: TObject);
begin
   LBPathsClick(nil);
end;

end.
