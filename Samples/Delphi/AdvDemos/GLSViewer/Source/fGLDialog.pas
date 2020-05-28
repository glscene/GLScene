unit fGLDialog;

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
  Vcl.StdCtrls, 
  Vcl.ExtCtrls,

  fGLForm;

type
  TGLDialog = class(TGLForm)
    PanelTop: TPanel;
    PanelMiddle: TPanel;
    PanelBottom: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ButtonHelp: TButton;
    Memo: TMemo;
    procedure ButtonHelpClick(Sender: TObject);
  public
     
    function Execute: boolean; virtual;
    procedure ReadIniFile; override;
    procedure WriteIniFile; override;
  private
     
  end;

var
  GLDialog: TGLDialog;

implementation

{$R *.dfm}

procedure TGLDialog.ButtonHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

function TGLDialog.Execute: boolean;
begin
  Result := ShowModal = mrOk;
end;

procedure TGLDialog.ReadIniFile;
begin
  inherited;
  //
end;

procedure TGLDialog.WriteIniFile;
begin
  //
  inherited;
end;

end.
