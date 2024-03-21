unit fdBehaviours;

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
  TFormBehaviours = class(TForm)
    PanelLeft: TPanel;
    TreeView: TTreeView;
    PanelBehaviours: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormBehaviours: TFormBehaviours;

implementation

{$R *.dfm}

end.
