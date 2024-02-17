unit fdCollision;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TFormCollisions = class(TForm)
    PanelLeft: TPanel;
    tvCollisions: TTreeView;
    PageControl: TPageControl;
    tsBoxedin: TTabSheet;
    tsBoxSphere: TTabSheet;
    tsFacevface: TTabSheet;
    tsMeshHit: TTabSheet;
    tsVolcano: TTabSheet;
    tsOcclusionQuery: TTabSheet;
    tsOctree: TTabSheet;
    tsRaybox: TTabSheet;
    tsRaycast: TTabSheet;
    tsSphere: TTabSheet;
    tsTriangleBox: TTabSheet;
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
