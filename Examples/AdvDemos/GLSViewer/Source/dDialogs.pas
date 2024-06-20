unit dDialogs;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Dialogs,
  Vcl.ExtDlgs;

type
  TdmDialogs = class(TDataModule)
    sdTextures: TSaveDialog;
    odTextures: TOpenDialog;
    SaveDialog: TSaveDialog;
    opDialog: TOpenPictureDialog;
    ColorDialog: TColorDialog;
    OpenDialog: TOpenDialog;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmDialogs: TdmDialogs;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
