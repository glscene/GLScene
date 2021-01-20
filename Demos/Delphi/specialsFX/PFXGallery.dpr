// Simple effects gallery.
program PFXGallery;

uses
  Forms,
  PFXGalleryFm in 'PFXGalleryFm.pas' {FormPFXGallery};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPFXGallery, FormPFXGallery);
  Application.Run;
end.
