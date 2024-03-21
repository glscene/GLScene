unit fdSpecialsFX;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.ExtCtrls;

type
  TfrmSpecialsFX = class(TForm)
    PanelLeft: TPanel;
    tvBench: TTreeView;
    PanelSpecialsFX: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSpecialsFX: TfrmSpecialsFX;

implementation

{$R *.dfm}

end.
