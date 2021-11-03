// Simple effects gallery.
program PFXGalleryD;

uses
  Forms,
  fPFXGalleryD in 'fPFXGalleryD.pas' {FormPFXGallery};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPFXGallery, FormPFXGallery);
  Application.Run;
end.
