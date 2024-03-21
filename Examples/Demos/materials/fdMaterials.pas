unit fdMaterials;

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
  TfrmMaterials = class(TForm)
    PanelLeft: TPanel;
    tvMaterials: TTreeView;
    PanelMaterials: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMaterials: TfrmMaterials;

implementation

{$R *.dfm}

end.
