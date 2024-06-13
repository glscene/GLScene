//
// Datamodule for GLSViewer
//
unit dImages;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Dialogs,
  Vcl.ExtDlgs,
  Vcl.BaseImageCollection,
  Vcl.ImageCollection,
  System.ImageList,
  Vcl.ImgList,
  Vcl.VirtualImageList,

  GLS.BaseClasses,
  GLS.Material;

type
  TDataModuleImages = class(TDataModule)
    VirtualImageList: TVirtualImageList;
    ImageCollection: TImageCollection;
    LightmapLib: TGLMaterialLibrary;
  private

  public

  end;

var
  DataModuleImages: TDataModuleImages;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
