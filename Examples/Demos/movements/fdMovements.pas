unit fdMovements;

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

  fColumnD,
  fEventsD,
  fHierarchD,
  fManualD,
  fObjmoveD,
  fPointtoD,
  fPongD,
  fSmoothNaviD,
  fTweeningD;

type
  TFormMovements = class(TForm)
    PanelLeft: TPanel;
    tvMovements: TTreeView;
    PageControl: TPageControl;
    tsMovements: TTabSheet;
    procedure tvMovementsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  FormMovements: TFormMovements;

implementation

{$R *.dfm}

procedure TFormMovements.FormCreate(Sender: TObject);
begin
  PageControl.ActivePage := tsMovements;
  tvMovements.Select(tvMovements.Items[0]);  // goto to column 0
  tvMovementsClick(Sender);
end;

//---------------------------------------------------------------------------
procedure TFormMovements.tvMovementsClick(Sender: TObject);
begin
  tvMovements.Items[0].DropHighlighted := False;
  case tvMovements.Selected.Index of
    0:
      begin // column
        FormColumn := TFormColumn.Create(tsMovements);
        FormColumn.Parent := tsMovements;
        FormColumn.Align := alClient;
        FormColumn.BorderStyle := bsNone;
        FormColumn.Show;
      end;
    1:
      begin // events
        FormEvents := TFormEvents.Create(tsMovements);
        FormEvents.Parent := tsMovements;
        FormEvents.Align := alClient;
        FormEvents.BorderStyle := bsNone;
        FormEvents.Show;
      end;
    2:
      begin // hierarch
        FormHierarchy := TFormHierarchy.Create(tsMovements);
        FormHierarchy.Parent := tsMovements;
        FormHierarchy.Align := alClient;
        FormHierarchy.BorderStyle := bsNone;
        FormHierarchy.Show;
      end;
    3:
      begin // manual
        FormManual := TFormManual.Create(tsMovements);
        FormManual.Parent := tsMovements;
        FormManual.Align := alClient;
        FormManual.BorderStyle := bsNone;
        FormManual.Show;
      end;
    4:
      begin // objmove
        FormObjmove := TFormObjmove.Create(tsMovements);
        FormObjmove.Parent := tsMovements;
        FormObjmove.Align := alClient;
        FormObjmove.BorderStyle := bsNone;
        FormObjmove.Show;
      end;
    5:
      begin // pointto
        FormPointto := TFormPointto.Create(tsMovements);
        FormPointto.Parent := tsMovements;
        FormPointto.Align := alClient;
        FormPointto.BorderStyle := bsNone;
        FormPointto.Show;
      end;
     6:
       begin // pong
        FormPong := TFormPong.Create(tsMovements);
        FormPong.Parent := tsMovements;
        FormPong.Align := alClient;
      //  FormPong.BorderStyle := bsNone;  // the game must be over
        FormPong.Show;
      end;
     7:
      begin // smoothnavigator
        FormSmoothNavi := TFormSmoothNavi.Create(tsMovements);
        FormSmoothNavi.Parent := tsMovements;
        FormSmoothNavi.Align := alClient;
        FormSmoothNavi.BorderStyle := bsNone;
        FormSmoothNavi.Show;
      end;
      8:
      begin // tweening
        FormTweening := TFormTweening.Create(tsMovements);
        FormTweening.Parent := tsMovements;
        FormTweening.Align := alClient;
        FormTweening.BorderStyle := bsNone;
        FormTweening.Show;
      end;

  end;
end;

end.
