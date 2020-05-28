object dmGLSViewer: TdmGLSViewer
  OldCreateOrder = False
  Height = 334
  Width = 390
  object ColorDialog: TColorDialog
    Color = 14540253
    Options = [cdFullOpen, cdAnyColor]
    Left = 46
    Top = 24
  end
  object OpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 150
    Top = 24
  end
  object SaveDialog: TSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 232
    Top = 24
  end
  object odTextures: TOpenDialog
    DefaultExt = 'glml'
    Filter = 'GLScene Material Library (*.glml)|*.glml|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 46
    Top = 80
  end
  object sdTextures: TSaveDialog
    DefaultExt = 'glml'
    Filter = 'GLScene Material Library (*.glml)|*.glml|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 150
    Top = 80
  end
  object opDialog: TOpenPictureDialog
    Filter = 
      'All (*.tga;*.dds;*.gif;*.png;*.jpg;*.jpeg;*.bmp;*.jpg;*.jpeg;*.g' +
      'if;*.png;*.tif;*.tiff;*.ico;*.emf;*.wmf)|*.tga;*.dds;*.gif;*.png' +
      ';*.jpg;*.jpeg;*.bmp;*.jpg;*.jpeg;*.gif;*.png;*.tif;*.tiff;*.ico;' +
      '*.emf;*.wmf|Targa (*.tga)|*.tga|Microsoft DirectDraw Surface (*.' +
      'dds)|*.dds|GIF Image (*.gif)|*.gif|Portable Network Graphics (*.' +
      'png)|*.png|JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg' +
      ')|*.jpeg|Bitmaps (*.bmp)|*.bmp|JPEG Images (*.jpg)|*.jpg|JPEG Im' +
      'ages (*.jpeg)|*.jpeg|GIF Images (*.gif)|*.gif|PNG Images (*.png)' +
      '|*.png|TIFF Images (*.tif)|*.tif|TIFF Images (*.tiff)|*.tiff|Ico' +
      'ns (*.ico)|*.ico|Enhanced Metafiles (*.emf)|*.emf|Metafiles (*.w' +
      'mf)|*.wmf'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 230
    Top = 80
  end
end
