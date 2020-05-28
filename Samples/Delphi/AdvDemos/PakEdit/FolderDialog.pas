unit FolderDialog;

interface

uses Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, 
  Buttons, Vcl.ExtCtrls;

type
  TFDialog = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    Edit1: TEdit;
  private
     
  public
     
  end;

var
  FDialog: TFDialog;

implementation

{$R *.dfm}

end.
