object dmGLSViewer: TdmGLSViewer
  Height = 418
  Width = 488
  PixelsPerInch = 120
  object ColorDialog: TColorDialog
    Color = 14540253
    Options = [cdFullOpen, cdAnyColor]
    Left = 178
    Top = 22
  end
  object OpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 60
    Top = 22
  end
  object SaveDialog: TSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 170
    Top = 110
  end
  object odTextures: TOpenDialog
    DefaultExt = 'glml'
    Filter = 'GLScene Material Library (*.glml)|*.glml|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 58
    Top = 196
  end
  object sdTextures: TSaveDialog
    DefaultExt = 'glml'
    Filter = 'GLScene Material Library (*.glml)|*.glml|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 172
    Top = 196
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
    Left = 64
    Top = 108
  end
end
