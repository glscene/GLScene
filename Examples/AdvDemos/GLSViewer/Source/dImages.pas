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
  Vcl.Controls,

  GLS.BaseClasses,
  GLS.Material;

type
  TdmImages = class(TDataModule)
    VirtualImageList: TVirtualImageList;
    ImageCollection: TImageCollection;
    LightmapLib: TGLMaterialLibrary;
    ImageListGLS: TImageList;
    MaterialLib: TGLMaterialLibrary;
    MLTree: TGLMaterialLibrary;
  private

  public

  end;

var
  dmImages: TdmImages;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
