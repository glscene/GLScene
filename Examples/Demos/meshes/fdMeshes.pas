unit fdMeshes;

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
  Vcl.ExtCtrls,
  Vcl.Grids,
  Vcl.Samples.DirOutln;

type
  TFormMeshes = class(TForm)
    PanelLeft: TPanel;
    tvBench: TTreeView;
    PanelMeshes: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMeshes: TFormMeshes;

implementation

{$R *.dfm}

end.
