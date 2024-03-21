unit fdInterface;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TfrmSandbox = class(TForm)
    PanelLeft: TPanel;
    tvBench: TTreeView;
    PanelInterface: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSandbox: TfrmSandbox;

implementation

{$R *.dfm}

end.
