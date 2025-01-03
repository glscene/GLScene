object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Form4'
  ClientHeight = 576
  ClientWidth = 788
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  TextHeight = 15
  object ImgView: TImgView32
    Left = 0
    Top = 0
    Width = 657
    Height = 576
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 16
    SizeGrip = sgNone
    OverSize = 0
    TabOrder = 0
    TabStop = True
    ExplicitWidth = 656
    ExplicitHeight = 590
  end
  object PnlControl: TPanel
    Left = 657
    Top = 0
    Width = 131
    Height = 576
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 656
    ExplicitHeight = 590
    object PnlImage: TPanel
      Left = 0
      Top = 0
      Width = 131
      Height = 130
      Align = alTop
      TabOrder = 0
      Visible = False
      object LblScale: TLabel
        Left = 8
        Top = 24
        Width = 30
        Height = 15
        Caption = 'Scale:'
      end
      object ScaleCombo: TComboBox
        Left = 16
        Top = 40
        Width = 105
        Height = 23
        DropDownCount = 9
        TabOrder = 0
        Text = '100%'
        Items.Strings = (
          '    25%'
          '    50%'
          '    75%'
          '  100%'
          '  200%'
          '  300%'
          '  400%'
          '  800%'
          '1600%')
      end
      object PnlImageHeader: TPanel
        Left = 1
        Top = 1
        Width = 129
        Height = 16
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Image Properties'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
      object CbxImageInterpolate: TCheckBox
        Left = 16
        Top = 72
        Width = 97
        Height = 17
        Caption = 'Interpolated'
        TabOrder = 2
      end
      object CbxOptRedraw: TCheckBox
        Left = 16
        Top = 96
        Width = 105
        Height = 17
        Caption = 'Optimize Repaints'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
    end
    object PnlBitmapLayer: TPanel
      Left = 0
      Top = 130
      Width = 131
      Height = 168
      Align = alTop
      TabOrder = 1
      Visible = False
      object LblOpacity: TLabel
        Left = 8
        Top = 24
        Width = 44
        Height = 15
        Caption = 'Opacity:'
      end
      object PnlBitmapLayerHeader: TPanel
        Left = 1
        Top = 1
        Width = 129
        Height = 16
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Bitmap Layer Properties'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object GbrLayerOpacity: TGaugeBar
        Left = 16
        Top = 40
        Width = 105
        Height = 12
        Backgnd = bgPattern
        HandleSize = 16
        Max = 255
        ShowArrows = False
        ShowHandleGrip = True
        Style = rbsMac
        Position = 255
      end
      object CbxLayerInterpolate: TCheckBox
        Left = 16
        Top = 64
        Width = 97
        Height = 17
        Caption = '&Interpolated'
        TabOrder = 2
      end
      object BtnLayerRescale: TButton
        Left = 16
        Top = 112
        Width = 105
        Height = 17
        Caption = 'Rescale'
        TabOrder = 3
      end
      object BtnLayerResetScale: TButton
        Left = 16
        Top = 136
        Width = 105
        Height = 17
        Caption = 'Scale to 100%'
        TabOrder = 4
      end
      object CbxCropped: TCheckBox
        Left = 16
        Top = 88
        Width = 97
        Height = 17
        Caption = '&Cropped'
        TabOrder = 5
      end
    end
    object PnlMagnification: TPanel
      Left = 0
      Top = 408
      Width = 131
      Height = 168
      Align = alTop
      TabOrder = 2
      Visible = False
      object LblMagifierOpacity: TLabel
        Left = 8
        Top = 24
        Width = 44
        Height = 15
        Caption = 'Opacity:'
      end
      object LblMagnification: TLabel
        Left = 8
        Top = 64
        Width = 77
        Height = 15
        Caption = 'Magnification:'
      end
      object LblRotation: TLabel
        Left = 8
        Top = 104
        Width = 48
        Height = 15
        Caption = 'Rotation:'
      end
      object PnlMagnificationHeader: TPanel
        Left = 1
        Top = 1
        Width = 129
        Height = 16
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Magnifier (All) Properties'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object GbrMagnOpacity: TGaugeBar
        Left = 16
        Top = 40
        Width = 105
        Height = 12
        Backgnd = bgPattern
        HandleSize = 16
        Max = 255
        ShowArrows = False
        ShowHandleGrip = True
        Style = rbsMac
        Position = 255
      end
      object GbrMagnMagnification: TGaugeBar
        Left = 16
        Top = 80
        Width = 105
        Height = 12
        Backgnd = bgPattern
        HandleSize = 16
        Max = 50
        ShowArrows = False
        ShowHandleGrip = True
        Style = rbsMac
        Position = 10
      end
      object GbrMagnRotation: TGaugeBar
        Left = 16
        Top = 120
        Width = 105
        Height = 12
        Backgnd = bgPattern
        HandleSize = 16
        Max = 180
        Min = -180
        ShowArrows = False
        ShowHandleGrip = True
        Style = rbsMac
        Position = 0
      end
      object CbxMagnInterpolate: TCheckBox
        Left = 16
        Top = 144
        Width = 97
        Height = 17
        Caption = 'Interpolated'
        TabOrder = 4
      end
    end
    object PnlButtonMockup: TPanel
      Left = 0
      Top = 298
      Width = 131
      Height = 110
      Align = alTop
      TabOrder = 3
      Visible = False
      object LblBorderRadius: TLabel
        Left = 8
        Top = 24
        Width = 76
        Height = 15
        Caption = 'Border Radius:'
      end
      object LblBorderWidth: TLabel
        Left = 8
        Top = 64
        Width = 73
        Height = 15
        Caption = 'Border Width:'
      end
      object PnlButtonMockupHeader: TPanel
        Left = 1
        Top = 1
        Width = 129
        Height = 16
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Button (All) Properties'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object GbrBorderRadius: TGaugeBar
        Left = 16
        Top = 40
        Width = 105
        Height = 12
        Backgnd = bgPattern
        HandleSize = 16
        Max = 20
        Min = 1
        ShowArrows = False
        ShowHandleGrip = True
        Style = rbsMac
        Position = 5
      end
      object GbrBorderWidth: TGaugeBar
        Left = 16
        Top = 80
        Width = 105
        Height = 12
        Backgnd = bgPattern
        HandleSize = 16
        Max = 30
        Min = 10
        ShowArrows = False
        ShowHandleGrip = True
        Style = rbsMac
        Position = 20
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 80
    Top = 10
    object MnuFile: TMenuItem
      Caption = 'File'
      object MnuFileNew: TMenuItem
        Caption = 'New...'
      end
      object MnuFileOpen: TMenuItem
        Caption = 'Open...'
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object MnuPrint: TMenuItem
        Caption = 'Print'
      end
    end
    object MnuLayers: TMenuItem
      Caption = 'Layers'
      object MnuNewBitmapLayer: TMenuItem
        Caption = 'New Bitmap Layer'
      end
      object MnuNewBitmapRGBA: TMenuItem
        Caption = 'New Bitmap Layer with Alpha Channel'
      end
      object MnuNewCustomLayer: TMenuItem
        Caption = 'New Custom Layer'
        object MnuSimpleDrawing: TMenuItem
          Caption = 'Simple Drawing Layer'
        end
        object MnuButtonMockup: TMenuItem
          Caption = 'Button Mockup'
        end
        object MnuMagnifier: TMenuItem
          Caption = 'Magnifier'
        end
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MnuFlatten: TMenuItem
        Caption = 'Flatten Layers'
      end
    end
    object MimArrange: TMenuItem
      Caption = 'Selection'
      object MnuBringFront: TMenuItem
        Tag = 1
        Caption = 'Bring to Front'
      end
      object MnuSendBack: TMenuItem
        Tag = 2
        Caption = 'Send to Back'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MnuLevelUp: TMenuItem
        Tag = 3
        Caption = 'Up One Level'
      end
      object MnuLevelDown: TMenuItem
        Tag = 4
        Caption = 'Down one Level'
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object MnuScaled: TMenuItem
        Caption = 'Scaled'
        Checked = True
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MnuFlipHorz: TMenuItem
        Caption = 'Flip Horizontally'
      end
      object MnuFlipVert: TMenuItem
        Caption = 'Flip Vertically'
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MnuRotate90: TMenuItem
        Caption = 'Rotate 90'
      end
      object MnuRotate180: TMenuItem
        Caption = 'Rotate 180'
      end
      object MnuRotate270: TMenuItem
        Caption = 'Rotate 270'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MnuDelete: TMenuItem
        Caption = 'Delete'
      end
    end
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 
      'All (*.tga;*.dds;*.dib;*.tif;*.gif;*.png;*.png;*.gif;*.png;*.jpg' +
      ';*.jpeg;*.bmp;*.tif;*.tiff;*.ico;*.emf;*.wmf)|*.tga;*.dds;*.dib;' +
      '*.tif;*.gif;*.png;*.png;*.gif;*.png;*.jpg;*.jpeg;*.bmp;*.tif;*.t' +
      'iff;*.ico;*.emf;*.wmf|Targa (*.tga)|*.tga|Microsoft DirectDraw S' +
      'urface (*.dds)|*.dds|Device Independent Bitmap (*.dib)|*.dib|All' +
      ' graphics (*.tif;*.gif;*.png)|*.tif;*.gif;*.png|PNG graphics fro' +
      'm DevExpress (*.png)|*.png|GIF Image (*.gif)|*.gif|Portable Netw' +
      'ork Graphics (*.png)|*.png|JPEG Image File (*.jpg)|*.jpg|JPEG Im' +
      'age File (*.jpeg)|*.jpeg|Bitmaps (*.bmp)|*.bmp|TIFF Images (*.ti' +
      'f)|*.tif|TIFF Images (*.tiff)|*.tiff|Icons (*.ico)|*.ico|Enhance' +
      'd Metafiles (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf'
    Left = 80
    Top = 70
  end
  object SaveDialog: TSaveDialog
    Left = 80
    Top = 130
  end
end
