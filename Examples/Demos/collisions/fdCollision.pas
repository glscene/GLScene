unit fdCollision;

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
  TFormCollisions = class(TForm)
    PanelLeft: TPanel;
    tvCollisions: TTreeView;
    PanelCollisions: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCollisions: TFormCollisions;

implementation

{$R *.dfm}

end.
