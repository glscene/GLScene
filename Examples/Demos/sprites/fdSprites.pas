unit fdSprites;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TFormSprites = class(TForm)
    PanelLeft: TPanel;
    tvBench: TTreeView;
    PanelSprites: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSprites: TFormSprites;

implementation

{$R *.dfm}

end.
