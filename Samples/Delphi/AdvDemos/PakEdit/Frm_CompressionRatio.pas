unit Frm_CompressionRatio;

interface

uses
  Windows, Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Dialogs, Vcl.StdCtrls, GLVfsPAK;

type
  TFrmCompressionRatio = class(TForm)
    BtnForOk: TButton;
    BtnForCancel: TButton;
    CbForRatio: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure BtnForCancelClick(Sender: TObject);
    procedure BtnForOkClick(Sender: TObject);
  private
     
    FCompressionRatio: TZCompressedMode;
    FDefaultCbrMode: TZCompressedMode;
    procedure SetDefaultCbrMode(const Value: TZCompressedMode);

  public
     
    property CompressionRatio: TZCompressedMode read FCompressionRatio
      write FCompressionRatio;
    property DefaultCbrMode: TZCompressedMode read FDefaultCbrMode
      write SetDefaultCbrMode;
  end;

var
  FrmCompressionRatio: TFrmCompressionRatio;

function SelectCompressionRatio(const ADefaultCbrMode: TZCompressedMode = Auto): TZCompressedMode;

implementation

uses
  TypInfo;

{$R *.dfm}

// SelectCompressionRatio
//
function SelectCompressionRatio(const ADefaultCbrMode: TZCompressedMode = Auto): TZCompressedMode;
begin
  if not Assigned(FrmCompressionRatio) then
    FrmCompressionRatio.Create(nil);
  FrmCompressionRatio.DefaultCbrMode := ADefaultCbrMode;
  FrmCompressionRatio.ShowModal;
  Result := FrmCompressionRatio.CompressionRatio;
end;

// TFrmCompressionRatio.FormCreate
//
procedure TFrmCompressionRatio.FormCreate(Sender: TObject);
var
  I: TZCompressedMode;
begin
  FCompressionRatio := Auto;
  FDefaultCbrMode := Auto;
  for I := Low(TZCompressedMode) to High(TZCompressedMode) do
    CbForRatio.Items.Add(GetEnumName(TypeInfo(TZCompressedMode), Ord(I)));
end;

procedure TFrmCompressionRatio.SetDefaultCbrMode(
  const Value: TZCompressedMode);
begin
  FDefaultCbrMode := Value;
  CbForRatio.ItemIndex := ord(DefaultCbrMode);
end;

// TFrmCompressionRatio.BtnForCancelClick
//
procedure TFrmCompressionRatio.BtnForCancelClick(Sender: TObject);
begin
  FCompressionRatio := None;
end;

// TFrmCompressionRatio.BtnForOkClick
//
procedure TFrmCompressionRatio.BtnForOkClick(Sender: TObject);
begin
  FCompressionRatio := TZCompressedMode(CbForRatio.ItemIndex);
end;

end.
