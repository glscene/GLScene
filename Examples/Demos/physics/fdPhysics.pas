unit fdPhysics;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TFormPhysics = class(TForm)
    PanelLeft: TPanel;
    tvBench: TTreeView;
    PanelPhysics: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormPhysics: TFormPhysics;

implementation

{$R *.dfm}

end.
