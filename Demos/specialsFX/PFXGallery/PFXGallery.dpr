// Simple effects gallery.
program PFXGallery;

uses
  Forms,
  fPFXGallery in 'fPFXGallery.pas' {FormPFXGallery};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPFXGallery, FormPFXGallery);
  Application.Run;
end.
