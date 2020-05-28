// Simple effects gallery.
program PFXGallery;

uses
  Forms,
  UPFXGallery in 'UPFXGallery.pas' {FrmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
