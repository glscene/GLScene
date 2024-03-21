unit fdComputing;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TfrmComputing = class(TForm)
    PanelLeft: TPanel;
    tvBench: TTreeView;
    PanelComputing: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmComputing: TfrmComputing;

implementation

{$R *.dfm}

end.
