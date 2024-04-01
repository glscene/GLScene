unit fdGraph;

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
  TFormGraph = class(TForm)
    PanelLeft: TPanel;
    tvGraph: TTreeView;
    MainMenu: TMainMenu;
    procedure tvGraphClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  FormGraph: TFormGraph;

implementation

{$R *.dfm}

procedure TFormGraph.FormCreate(Sender: TObject);
begin
  tvGraph.Select(tvGraph.Items[0]);  // goto to column 0
  tvGraphClick(Sender);
end;

procedure TFormGraph.tvGraphClick(Sender: TObject);
begin
  tvGraph.Items[0].DropHighlighted := False;
  case tvGraph.Selected.Index of
    0:
      begin // Fxy
        FormFxy := TFormFxy.Create(FormGraph);
        FormFxy.Parent := FormGraph;
        FormFxy.Align := alClient;
        FormFxy.BorderStyle := bsNone;
        FormFxy.Show;
      end;
    1:
      begin // HeightField
        FormHeightField := TFormHeightField.Create(FormGraph);
        FormHeightField.Parent := FormGraph;
        FormHeightField.Align := alClient;
        FormHeightField.BorderStyle := bsNone;
        FormHeightField.Show;
      end;
    2:
      begin // Points
        FormPoints := TFormPoints.Create(FormGraph);
        FormPoints.Parent := FormGraph;
        FormPoints.Align := alClient;
        FormPoints.BorderStyle := bsNone;
        FormPoints.Show;
      end;
    3:
      begin // Projection
        FormProjection := TFormProjection.Create(FormGraph);
        FormProjection.Parent := FormGraph;
        FormProjection.Align := alClient;
        FormProjection.BorderStyle := bsNone;
        FormProjection.Show;
      end;
    4:
      begin
        // Splines
        FormSplines := TFormSplines.Create(FormGraph);
        FormSplines.Parent := FormGraph;
        FormSplines.Align := alClient;
        FormSplines.BorderStyle := bsNone;
        FormSplines.Show;
      end;
  end;
end;

end.
