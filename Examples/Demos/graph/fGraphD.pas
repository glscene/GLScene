unit fGraphD;

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
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ComCtrls,

  fFxyD,
  fHeightFieldD,
  fPointsD,
  fProjectionD,
  fSplinesD;

type
  TFormGraphD = class(TForm)
    PanelLeft: TPanel;
    tvGraph: TTreeView;
    MainMenu: TMainMenu;
    PageControl: TPageControl;
    tsFxy: TTabSheet;
    tsHeightField: TTabSheet;
    tsPoints: TTabSheet;
    tsProjection: TTabSheet;
    tsSplines: TTabSheet;
    procedure tvGraphClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;

var
  FormGraphD: TFormGraphD;

implementation

{$R *.dfm}

procedure TFormGraphD.FormCreate(Sender: TObject);
begin
  // Fxy
  FormFxy := TFormFxy.Create(tsFxy);
  FormFxy.Parent := tsFxy;
  FormFxy.Align := alClient;
  FormFxy.BorderStyle := bsNone;
  FormFxy.Show;

  // HeightField
  FormHeightField := TFormHeightField.Create(tsHeightField);
  FormHeightField.Parent := tsHeightField;
  FormHeightField.Align := alClient;
  FormHeightField.BorderStyle := bsNone;
  FormHeightField.Show;

  // Points
  FormPoints := TFormPoints.Create(tsPoints);
  FormPoints.Parent := tsPoints;
  FormPoints.Align := alClient;
  FormPoints.BorderStyle := bsNone;
  FormPoints.Show;

   // Projection
  FormProjection := TFormProjection.Create(tsProjection);
  FormProjection.Parent := tsProjection;
  FormProjection.Align := alClient;
  FormProjection.BorderStyle := bsNone;
  FormProjection.Show;

  // Splines
  FormSplines := TFormSplines.Create(tsSplines);
  FormSplines.Parent := tsSplines;
  FormSplines.Align := alClient;
  FormSplines.BorderStyle := bsNone;
  FormSplines.Show;
end;

procedure TFormGraphD.FormShow(Sender: TObject);
begin
  PageControl.ActivePage := tsFxy;
end;

procedure TFormGraphD.tvGraphClick(Sender: TObject);
begin
  tvGraph.Items[0].DropHighlighted := False;
   case tvGraph.Selected.Index of
    0:
      PageControl.ActivePage := tsFxy;
    1:
      PageControl.ActivePage := tsHeightField;
    2:
      PageControl.ActivePage := tsPoints;
    3:
      PageControl.ActivePage := tsProjection;
    4:
      PageControl.ActivePage := tsSplines;
  end;
end;

end.
