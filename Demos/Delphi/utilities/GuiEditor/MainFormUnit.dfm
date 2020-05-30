object Form1: TForm1
  Left = 140
  Top = 116
  Caption = 'Gui Skin Editor and Manager'
  ClientHeight = 325
  ClientWidth = 553
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 99
    Height = 13
    Caption = 'Layouts in collection:'
  end
  object ListBox: TListBox
    Left = 8
    Top = 56
    Width = 233
    Height = 281
    ItemHeight = 13
    PopupMenu = ListPopup
    TabOrder = 0
    OnClick = ListBoxClick
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 248
    Top = 8
    Width = 297
    Height = 329
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 142.783203125000000000
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 8
    Top = 24
    Width = 153
    Height = 21
    TabOrder = 2
    Text = 'Newly Added'
    OnChange = Edit3Change
    OnKeyPress = Edit3KeyPress
  end
  object Button1: TButton
    Left = 168
    Top = 24
    Width = 33
    Height = 25
    Caption = 'Add'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 208
    Top = 24
    Width = 33
    Height = 25
    Caption = 'Edit'
    TabOrder = 4
    OnClick = Button2Click
  end
  object MainMenu1: TMainMenu
    Left = 160
    Top = 160
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = 'Open...'
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = 'Save...'
        OnClick = Save1Click
      end
      object Close1: TMenuItem
        Caption = 'Close'
        OnClick = Close1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Import1: TMenuItem
        Caption = 'Import...'
        OnClick = Import1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object EditLayout1: TMenuItem
        Caption = 'Edit Layout'
        OnClick = EditLayout1Click
      end
    end
    object Image1: TMenuItem
      Caption = 'Image'
      object Load1: TMenuItem
        Caption = 'Load...'
        OnClick = Load1Click
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.layout'
    Filter = 'Layouts|*.layout'
    Title = 'Open Layout'
    Left = 32
    Top = 208
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '*.layout'
    Filter = 'Layouts|*.layout'
    Title = 'Save Layout'
    Left = 64
    Top = 272
  end
  object GLGuiLayout1: TGLGuiLayout
    BitmapFont = WindowsBitmapFont1
    Material.MaterialLibrary = GLMaterialLibrary1
    Material.LibMaterialName = 'LibMaterial'
    GuiComponents = <>
    Left = 32
    Top = 160
  end
  object ImportDialog: TOpenDialog
    DefaultExt = '*.layout'
    Filter = 'Layouts|*.layout'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 176
    Top = 208
  end
  object ListPopup: TPopupMenu
    Left = 104
    Top = 208
    object Add1: TMenuItem
      Caption = 'Add'
      OnClick = Add1Click
    end
    object Remove1: TMenuItem
      Caption = 'Remove'
      OnClick = Remove1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Edit2: TMenuItem
      Caption = 'Edit'
      OnClick = Edit2Click
    end
  end
  object GLScene1: TGLScene
    Left = 56
    Top = 64
    object HUDSprite1: TGLHUDSprite
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial'
      Position.Coordinates = {0000164300001643000000000000803F}
      Width = 200.000000000000000000
      Height = 200.000000000000000000
      Rotation = 0.000000000000000000
    end
    object GLPanel1: TGLPanel
      Autosize = False
      RedrawAtOnce = False
      GuiLayout = GLGuiLayout1
      Rotation = 0.000000000000000000
      NoZWrite = False
      DoChangesOnProgress = False
      Width = 200.000000000000000000
      Height = 200.000000000000000000
      Left = 50.000000000000000000
      Top = 50.000000000000000000
      Position.Coordinates = {0000484200004842000000000000803F}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
    end
  end
  object WindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'Verdana'
    Font.Style = []
    Ranges = <
      item
        StartASCII = 'A'
        StopASCII = 'Z'
        StartGlyphIdx = 0
      end
      item
        StartASCII = 'a'
        StopASCII = 'z'
        StartGlyphIdx = 26
      end
      item
        StartASCII = '0'
        StopASCII = '9'
        StartGlyphIdx = 52
      end
      item
        StartASCII = ' '
        StopASCII = ' '
        StartGlyphIdx = 62
      end>
    Left = 160
    Top = 64
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 144
    Top = 272
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Tag = 0
        Material.Texture.Disabled = False
      end>
    Left = 104
    Top = 112
  end
end
