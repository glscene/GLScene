unit fdUtilities;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TFormUtilities = class(TForm)
    PanelLeft: TPanel;
    tvBench: TTreeView;
    PanelUtilities: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormUtilities: TFormUtilities;

implementation

{$R *.dfm}

end.
