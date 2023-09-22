unit fCudaD;

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

  fFastFourierD,
  fPostProcessingD,
  fScalarProductD,
  fSimpleTexD,
  fFluidsD,
  fVertexGenD;

type
  TFormCudaD = class(TForm)
    PanelLeft: TPanel;
    tvCuda: TTreeView;
    MainMenu: TMainMenu;
    PageControl: TPageControl;
    tsFastFourierTrans: TTabSheet;
    tsPostProcessing: TTabSheet;
    tsScalarProduct: TTabSheet;
    tsSimpleTexture: TTabSheet;
    tsVertexDataGen: TTabSheet;
    tsStableFluids: TTabSheet;
    procedure tvCudaClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;

var
  FormCudaD: TFormCudaD;

implementation

{$R *.dfm}

procedure TFormCudaD.FormCreate(Sender: TObject);
begin
  // FastFourierTrans
  FormFFT := TFormFFT.Create(tsFastFourierTrans);
  FormFFT.Parent := tsFastFourierTrans;
  FormFFT.Align := alClient;
  FormFFT.BorderStyle := bsNone;
  FormFFT.Show;

  // PostProcessing
  FormPP := TFormPP.Create(tsPostProcessing);
  FormPP.Parent := tsPostProcessing;
  FormPP.Align := alClient;
  FormPP.BorderStyle := bsNone;
  FormPP.Show;

  // ScalarProduct
  FormSP := TFormSP.Create(tsScalarProduct);
  FormSP.Parent := tsScalarProduct;
  FormSP.Align := alClient;
  FormSP.BorderStyle := bsNone;
  FormSP.Show;

  // SimpleTexture
  FormST := TFormST.Create(tsSimpleTexture);
  FormST.Parent := tsSimpleTexture;
  FormST.Align := alClient;
  FormST.BorderStyle := bsNone;
  FormST.Show;

   // StableFluids
  FormSF := TFormSF.Create(tsStableFluids);
  FormSF.Parent := tsStableFluids;
  FormSF.Align := alClient;
  FormSF.BorderStyle := bsNone;
  FormSF.Show;

  // VertexDataGen
  FormVDG := TFormVDG.Create(tsVertexDataGen);
  FormVDG.Parent := tsVertexDataGen;
  FormVDG.Align := alClient;
  FormVDG.BorderStyle := bsNone;
  FormVDG.Show;
end;

procedure TFormCudaD.FormShow(Sender: TObject);
begin
  PageControl.ActivePage := tsVertexDataGen;
end;

procedure TFormCudaD.tvCudaClick(Sender: TObject);
begin
  tvCuda.Items[0].DropHighlighted := False;
   case tvCuda.Selected.Index of
    0:
      PageControl.ActivePage := tsFastFourierTrans;
    1:
      PageControl.ActivePage := tsPostProcessing;
    2:
      PageControl.ActivePage := tsScalarProduct;
    3:
      PageControl.ActivePage := tsSimpleTexture;
    4:
      PageControl.ActivePage := tsStableFluids;
    5:
      PageControl.ActivePage := tsVertexDataGen;
  end;
end;

end.
