unit FolderSelect;

interface


uses
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Shell.ShellCtrls;

type
  TFolderSel = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    ShellView: TShellTreeView;
    Label1: TLabel;
  private
     
  public
     
  end;

var
  FolderSel: TFolderSel;

implementation

{$R *.dfm}

end.
