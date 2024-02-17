unit fdBench;

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

  fCanvasD,
  fMegacubeD,
  fMegaglassD,
  fSmokingD,
  fVolcanoD,
  fWhirlD;

type
  TFormBench = class(TForm)
    PanelLeft: TPanel;
    tvBench: TTreeView;
    PageControl: TPageControl;
    tsCanvas: TTabSheet;
    tsMegacube: TTabSheet;
    tsMegaglasscube: TTabSheet;
    tsSmoking: TTabSheet;
    tsVolcano: TTabSheet;
    tsWhirlwind: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tvBenchClick(Sender: TObject);
  private

  public

  end;

var
  FormBench: TFormBench;

//----------------------------------------------------------------------
implementation
//----------------------------------------------------------------------

{$R *.dfm}

procedure TFormBench.FormCreate(Sender: TObject);
begin
   // Canvas
  FormCanvas := TFormCanvas.Create(tsCanvas);
  FormCanvas.Parent := tsCanvas;
  FormCanvas.Align := alClient;
  FormCanvas.BorderStyle := bsNone;
  FormCanvas.GLSceneViewer.Height := 256;
  FormCanvas.GLSceneViewer.Width := 256;
  FormCanvas.Show;

  // Megacube
  FormMegacube := TFormMegacube.Create(tsMegacube);
  FormMegacube.Parent := tsMegacube;
  FormMegacube.Align := alClient;
  FormMegacube.BorderStyle := bsNone;
  FormMegacube.Show;

  // Megaglasscube
  FormMegaglasscube := TFormMegaglasscube.Create(tsMegaglasscube);
  FormMegaglasscube.Parent := tsMegaglasscube;
  FormMegaglasscube.Align := alClient;
  FormMegaglasscube.BorderStyle := bsNone;
  FormMegaglasscube.Show;

   // Smoking
  FormSmoking := TFormSmoking.Create(tsSmoking);
  FormSmoking.Parent := tsSmoking;
  FormSmoking.Align := alClient;
  FormSmoking.BorderStyle := bsNone;
  FormSmoking.Show;

  // Volcano
  FormVolcano := TFormVolcano.Create(tsVolcano);
  FormVolcano.Parent := tsVolcano;
  FormVolcano.Align := alClient;
  FormVolcano.BorderStyle := bsNone;
  FormVolcano.Show;

  // Whirlwind
  FormWhirl := TFormWhirl.Create(tsWhirlwind);
  FormWhirl.Parent := tsWhirlwind;
  FormWhirl.Align := alClient;
  FormWhirl.BorderStyle := bsNone;
  FormWhirl.Show;

end;

//----------------------------------------------------------------------

procedure TFormBench.FormShow(Sender: TObject);
begin
   PageControl.ActivePage := tsCanvas;
end;

//----------------------------------------------------------------------

procedure TFormBench.tvBenchClick(Sender: TObject);
begin
  tvBench.Items[0].DropHighlighted := False;
   case tvBench.Selected.Index of
    0:
      PageControl.ActivePage := tsCanvas;
    1:
      PageControl.ActivePage := tsMegacube;
    2:
      PageControl.ActivePage := tsMegaglasscube;
    3:
      PageControl.ActivePage := tsSmoking;
    4:
      PageControl.ActivePage := tsVolcano;
    5:
      PageControl.ActivePage := tsWhirlwind;
  end;
end;


end.
