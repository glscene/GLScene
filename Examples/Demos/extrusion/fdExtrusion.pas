unit fdExtrusion;

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
  Vcl.Menus,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,

  fBendingD,
  fCutoutStarD,
  fNutsnBoltsD,
  fPawnD,
  fTentaclesD;

type
  TFormExtrusion = class(TForm)
    PanelLeft: TPanel;
    tvExtrusion: TTreeView;
    PageControl: TPageControl;
    tsBending: TTabSheet;
    tsCutoutStar: TTabSheet;
    tsNutsnBolts: TTabSheet;
    tsPawn: TTabSheet;
    tsTentacles: TTabSheet;
    MainMenu: TMainMenu;
    procedure tvExtrusionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;

var
  FormExtrusion: TFormExtrusion;

//--------------------------------------------------------------
implementation
//--------------------------------------------------------------
{$R *.dfm}

procedure TFormExtrusion.FormCreate(Sender: TObject);
begin
  // Bending
  FormBendingCyl := TFormBendingCyl.Create(tsBending);
  FormBendingCyl.Parent := tsBending;
  FormBendingCyl.Align := alClient;
  FormBendingCyl.BorderStyle := bsNone;
  FormBendingCyl.Show;

  // CutoutStar
  FormCutoutStar := TFormCutoutStar.Create(tsCutoutStar);
  FormCutoutStar.Parent := tsCutoutStar;
  FormCutoutStar.Align := alClient;
  FormCutoutStar.BorderStyle := bsNone;
  FormCutoutStar.Show;

  // NutsnBolts
  FormNutsnBolts := TFormNutsnBolts.Create(tsNutsnBolts);
  FormNutsnBolts.Parent := tsNutsnBolts;
  FormNutsnBolts.Align := alClient;
  FormNutsnBolts.BorderStyle := bsNone;
  FormNutsnBolts.Show;

  // Pawn
  FormPawn := TFormPawn.Create(tsPawn);
  FormPawn.Parent := tsPawn;
  FormPawn.Align := alClient;
  FormPawn.BorderStyle := bsNone;
  FormPawn.Show;

  // Tentacles
  FormTentacles := TFormTentacles.Create(tsTentacles);
  FormTentacles.Parent := tsTentacles;
  FormTentacles.Align := alClient;
  FormTentacles.BorderStyle := bsNone;
  FormTentacles.Show;
end;

//--------------------------------------------------------------

procedure TFormExtrusion.FormShow(Sender: TObject);
begin
  PageControl.ActivePage := tsBending; //tsPawn;
end;

procedure TFormExtrusion.tvExtrusionClick(Sender: TObject);
begin
   tvExtrusion.Items[0].DropHighlighted := False;
   case tvExtrusion.Selected.Index of
    0:
      PageControl.ActivePage := tsBending;
    1:
      PageControl.ActivePage := tsCutoutStar;
    2:
      PageControl.ActivePage := tsNutsnBolts;
    3:
      PageControl.ActivePage := tsPawn;
    4:
      PageControl.ActivePage := tsTentacles;
  end;
end;

end.
