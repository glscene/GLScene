unit fdRendering;

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
  TFormRendering = class(TForm)
    PanelLeft: TPanel;
    tvBench: TTreeView;
    PanelRendering: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormRendering: TFormRendering;

implementation

{$R *.dfm}

end.
