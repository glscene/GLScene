object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Gui Demo'
  ClientHeight = 381
  ClientWidth = 537
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 537
    Height = 381
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 150.586914062500000000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    OnMouseUp = GLSceneViewer1MouseUp
    TabOrder = 0
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 40
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000204100002041000020410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLForm1: TGLForm
      Autosize = False
      RedrawAtOnce = False
      GuiLayout = GLGuiLayout1
      GuiLayoutName = 'form'
      Rotation = 0.000000000000000000
      AlphaChannel = 0.500000000000000000
      NoZWrite = False
      DoChangesOnProgress = False
      Width = 300.000000000000000000
      Height = 170.000000000000000000
      Left = 100.000000000000000000
      Top = 100.000000000000000000
      Position.Coordinates = {0000C8420000C842000000000000803F}
      BitmapFont = WindowsBitmapFont1
      DefaultColor = clMaroon
      Caption = 'Gui Form'
      TitleColor = clWhite
      TitleOffset = 2.000000000000000000
      object GLButton1: TGLButton
        Autosize = False
        RedrawAtOnce = False
        GuiLayout = GLGuiLayout1
        GuiLayoutName = 'panel'
        Rotation = 0.000000000000000000
        NoZWrite = False
        DoChangesOnProgress = False
        Width = 140.000000000000000000
        Height = 25.000000000000000000
        Left = 80.000000000000000000
        Top = 120.000000000000000000
        Position.Coordinates = {0000344300005C43000000000000803F}
        BitmapFont = WindowsBitmapFont1
        DefaultColor = clBlack
        Caption = 'Set Title'
        Focused = False
        FocusedColor = clBlack
        Group = -1
        Pressed = False
        OnButtonClick = GLButton1ButtonClick
        GuiLayoutNamePressed = 'panel'
        AllowUp = True
      end
      object GLEdit1: TGLEdit
        Autosize = False
        RedrawAtOnce = False
        GuiLayout = GLGuiLayout1
        GuiLayoutName = 'panel'
        Rotation = 0.000000000000000000
        NoZWrite = False
        DoChangesOnProgress = False
        Width = 180.000000000000000000
        Height = 21.000000000000000000
        Left = 100.000000000000000000
        Top = 70.000000000000000000
        Position.Coordinates = {0000484300002A43000000000000803F}
        BitmapFont = WindowsBitmapFont1
        DefaultColor = clBlack
        Caption = 'New Title'
        Focused = False
        FocusedColor = clBlack
        EditChar = '*'
        SelStart = 0
      end
      object GLLabel1: TGLLabel
        Autosize = False
        RedrawAtOnce = False
        GuiLayout = GLGuiLayout1
        Rotation = 0.000000000000000000
        NoZWrite = False
        DoChangesOnProgress = False
        Width = 80.000000000000000000
        Height = 21.000000000000000000
        Left = 10.000000000000000000
        Top = 70.000000000000000000
        Position.Coordinates = {0000DC4200002A43000000000000803F}
        BitmapFont = WindowsBitmapFont1
        DefaultColor = clBlack
        Caption = 'Title'
        Alignment = taLeftJustify
        TextLayout = tlCenter
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      Position.Coordinates = {0000A04000004040000080400000803F}
      Left = 240
      Top = 152
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 80
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 376
    Top = 16
  end
  object WindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = '@Arial Unicode MS'
    Font.Style = []
    Left = 40
    Top = 136
  end
  object MainMenu1: TMainMenu
    Left = 440
    Top = 88
    object Font1: TMenuItem
      Caption = 'Font'
      object WindowsFont1: TMenuItem
        Caption = 'Set New Font...'
        OnClick = WindowsFont1Click
      end
    end
    object miFPS: TMenuItem
      Caption = 'FPS'
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 440
    Top = 16
  end
  object GLGuiLayout1: TGLGuiLayout
    BitmapFont = WindowsBitmapFont1
    Material.MaterialLibrary = GLMaterialLibrary1
    Material.LibMaterialName = 'Gui'
    GuiComponents = <
      item
        Elements = <
          item
            TopLeft.Coordinates = {00003041000082420000000000000000}
            BottomRight.Coordinates = {000020420000BC420000000000000000}
            Align = GLAlCenter
            Name = 'center1'
          end
          item
            BottomRight.Coordinates = {000088410000F0410000000000000000}
            Align = GLAlTopLeft
            Name = 'TopLeft1'
          end
          item
            TopLeft.Coordinates = {00008841000000000000000000000000}
            BottomRight.Coordinates = {0000E0420000F0410000000000000000}
            Align = GLAlTop
            Name = 'Top1'
          end
          item
            TopLeft.Coordinates = {000000000000F8410000000000000000}
            BottomRight.Coordinates = {000000410000DA420000000000000000}
            Align = GLAlLeft
            Name = 'Left1'
          end
          item
            TopLeft.Coordinates = {0000E042000000000000000000000000}
            BottomRight.Coordinates = {000000430000F0410000000000000000}
            Align = GLAlTopRight
            Name = 'TopRight1'
          end
          item
            TopLeft.Coordinates = {0000F0420000F0410000000000000000}
            BottomRight.Coordinates = {000000430000DE420000000000000000}
            Align = GLAlRight
            Name = 'Right1'
          end
          item
            TopLeft.Coordinates = {000000000000E2420000000000000000}
            BottomRight.Coordinates = {00007041000000430000000000000000}
            Align = GLAlBottomLeft
            Name = 'BottomLeft1'
          end
          item
            TopLeft.Coordinates = {000070410000F0420000000000000000}
            BottomRight.Coordinates = {0000E242000000430000000000000000}
            Align = GLAlBottom
            Name = 'Bottom1'
          end
          item
            TopLeft.Coordinates = {0000E2420000E2420000000000000000}
            BottomRight.Coordinates = {00000043000000430000000000000000}
            Align = GLAlBottomRight
            Name = 'BottomRight1'
          end>
        Name = 'form'
      end
      item
        Elements = <
          item
            TopLeft.Coordinates = {00003041000082420000000000000000}
            BottomRight.Coordinates = {000020420000BC420000000000000000}
            Align = GLAlCenter
            Name = 'center1'
          end
          item
            TopLeft.Coordinates = {00000041000078420000000000000000}
            BottomRight.Coordinates = {00005041000086420000000000000000}
            Align = GLAlTopLeft
            Name = 'TopLeft1'
          end
          item
            TopLeft.Coordinates = {00003041000078420000000000000000}
            BottomRight.Coordinates = {00002042000084420000000000000000}
            Align = GLAlTop
            Name = 'Top1'
          end
          item
            TopLeft.Coordinates = {00000041000082420000000000000000}
            BottomRight.Coordinates = {000040410000BC420000000000000000}
            Align = GLAlLeft
            Name = 'Left1'
          end
          item
            TopLeft.Coordinates = {00001842000078420000000000000000}
            BottomRight.Coordinates = {00002C42000086420000000000000000}
            Align = GLAlTopRight
            Name = 'TopRight1'
          end
          item
            TopLeft.Coordinates = {00001C42000082420000000000000000}
            BottomRight.Coordinates = {00002C420000BC420000000000000000}
            Align = GLAlRight
            Name = 'Right1'
          end
          item
            TopLeft.Coordinates = {000000410000B8420000000000000000}
            BottomRight.Coordinates = {000050410000C2420000000000000000}
            Align = GLAlBottomLeft
            Name = 'BottomLeft1'
          end
          item
            TopLeft.Coordinates = {000030410000BA420000000000000000}
            BottomRight.Coordinates = {000020420000C2420000000000000000}
            Align = GLAlBottom
            Name = 'Bottom1'
          end
          item
            TopLeft.Coordinates = {000018420000B8420000000000000000}
            BottomRight.Coordinates = {00002C420000C2420000000000000000}
            Align = GLAlBottomRight
            Name = 'BottomRight1'
          end>
        Name = 'panel'
      end
      item
        Elements = <
          item
            TopLeft.Coordinates = {000024420000F8410000000000000000}
            BottomRight.Coordinates = {00003042000010420000000000000000}
            Align = GLAlTopLeft
            Name = 'TopLeft1'
          end
          item
            TopLeft.Coordinates = {000030420000F8410000000000000000}
            BottomRight.Coordinates = {00005842000010420000000000000000}
            Align = GLAlTop
            Name = 'Top1'
          end
          item
            TopLeft.Coordinates = {000058420000F8410000000000000000}
            BottomRight.Coordinates = {00006442000010420000000000000000}
            Align = GLAlTopRight
            Name = 'TopRight1'
          end
          item
            TopLeft.Coordinates = {00002442000010420000000000000000}
            BottomRight.Coordinates = {00003042000034420000000000000000}
            Align = GLAlLeft
            Name = 'Left1'
          end
          item
            TopLeft.Coordinates = {00003042000010420000000000000000}
            BottomRight.Coordinates = {00005842000030420000000000000000}
            Align = GLAlCenter
            Name = 'Center1'
          end
          item
            TopLeft.Coordinates = {00005842000010420000000000000000}
            BottomRight.Coordinates = {00006442000030420000000000000000}
            Align = GLAlRight
            Name = 'Right1'
          end
          item
            TopLeft.Coordinates = {00002442000030420000000000000000}
            BottomRight.Coordinates = {00003042000044420000000000000000}
            Align = GLAlBottomLeft
            Name = 'BottomLeft1'
          end
          item
            TopLeft.Coordinates = {00003042000030420000000000000000}
            BottomRight.Coordinates = {00005842000044420000000000000000}
            Align = GLAlBottom
            Name = 'Bottom1'
          end
          item
            TopLeft.Coordinates = {00005842000030420000000000000000}
            BottomRight.Coordinates = {00006442000044420000000000000000}
            Align = GLAlBottomRight
            Name = 'BottomRight1'
          end>
        Name = 'button_up'
      end
      item
        Elements = <
          item
            TopLeft.Coordinates = {000068420000F8410000000000000000}
            BottomRight.Coordinates = {00007442000010420000000000000000}
            Align = GLAlTopLeft
            Name = 'TopLeft1'
          end
          item
            TopLeft.Coordinates = {000074420000F8410000000000000000}
            BottomRight.Coordinates = {00008E42000010420000000000000000}
            Align = GLAlTop
            Name = 'Top1'
          end
          item
            TopLeft.Coordinates = {00008E420000F8410000000000000000}
            BottomRight.Coordinates = {00009442000010420000000000000000}
            Align = GLAlTopRight
            Name = 'TopRight1'
          end
          item
            TopLeft.Coordinates = {00006842000010420000000000000000}
            BottomRight.Coordinates = {00007442000034420000000000000000}
            Align = GLAlLeft
            Name = 'Left1'
          end
          item
            TopLeft.Coordinates = {00007442000010420000000000000000}
            BottomRight.Coordinates = {00008E42000034420000000000000000}
            Align = GLAlCenter
            Name = 'Center1'
          end
          item
            TopLeft.Coordinates = {00008E42000010420000000000000000}
            BottomRight.Coordinates = {00009442000034420000000000000000}
            Align = GLAlRight
            Name = 'Right1'
          end
          item
            TopLeft.Coordinates = {00006842000034420000000000000000}
            BottomRight.Coordinates = {00007442000044420000000000000000}
            Align = GLAlBottomLeft
            Name = 'BottomLeft1'
          end
          item
            TopLeft.Coordinates = {00007442000034420000000000000000}
            BottomRight.Coordinates = {00008E42000044420000000000000000}
            Align = GLAlBottom
            Name = 'Bottom1'
          end
          item
            TopLeft.Coordinates = {00008E42000034420000000000000000}
            BottomRight.Coordinates = {00009442000044420000000000000000}
            Align = GLAlBottomRight
            Name = 'BottomRight1'
          end>
        Name = 'button_down'
      end
      item
        Elements = <
          item
            TopLeft.Coordinates = {000096420000F8410000000000000000}
            BottomRight.Coordinates = {00009A42000004420000000000000000}
            Align = GLAlTopLeft
            Name = 'TopLeft1'
          end
          item
            TopLeft.Coordinates = {00009A420000F8410000000000000000}
            BottomRight.Coordinates = {0000AE42000004420000000000000000}
            Align = GLAlTop
            Name = 'Top1'
          end
          item
            TopLeft.Coordinates = {0000AE420000F8410000000000000000}
            BottomRight.Coordinates = {0000B242000004420000000000000000}
            Align = GLAlTopRight
            Name = 'TopRight1'
          end
          item
            TopLeft.Coordinates = {00009642000004420000000000000000}
            BottomRight.Coordinates = {00009A4200002C420000000000000000}
            Align = GLAlLeft
            Name = 'Left1'
          end
          item
            TopLeft.Coordinates = {00009A42000004420000000000000000}
            BottomRight.Coordinates = {0000AE4200002C420000000000000000}
            Align = GLAlCenter
            Name = 'Center1'
          end
          item
            TopLeft.Coordinates = {0000AE42000004420000000000000000}
            BottomRight.Coordinates = {0000B24200002C420000000000000000}
            Align = GLAlRight
            Name = 'Right1'
          end
          item
            TopLeft.Coordinates = {0000964200002C420000000000000000}
            BottomRight.Coordinates = {00009A42000034420000000000000000}
            Align = GLAlBottomLeft
            Name = 'BottomLeft1'
          end
          item
            TopLeft.Coordinates = {00009A4200002C420000000000000000}
            BottomRight.Coordinates = {0000AE42000034420000000000000000}
            Align = GLAlBottom
            Name = 'BottomCenter1'
          end
          item
            TopLeft.Coordinates = {0000AE4200002C420000000000000000}
            BottomRight.Coordinates = {0000B242000034420000000000000000}
            Align = GLAlBottomRight
            Name = 'BottomRight1'
          end>
        Name = 'checkbox_checked'
      end
      item
        Elements = <
          item
            TopLeft.Coordinates = {0000964200003C420000000000000000}
            BottomRight.Coordinates = {00009A42000044420000000000000000}
            Align = GLAlTopLeft
            Name = 'TopLeft1'
          end
          item
            TopLeft.Coordinates = {00009A4200003C420000000000000000}
            BottomRight.Coordinates = {0000AE42000044420000000000000000}
            Align = GLAlTop
            Name = 'Top1'
          end
          item
            TopLeft.Coordinates = {0000AE4200003C420000000000000000}
            BottomRight.Coordinates = {0000B242000044420000000000000000}
            Align = GLAlTopRight
            Name = 'TopRight1'
          end
          item
            TopLeft.Coordinates = {00009642000044420000000000000000}
            BottomRight.Coordinates = {00009A4200006C420000000000000000}
            Align = GLAlLeft
            Name = 'Left1'
          end
          item
            TopLeft.Coordinates = {00009A42000044420000000000000000}
            BottomRight.Coordinates = {0000AE4200006C420000000000000000}
            Align = GLAlCenter
            Name = 'Center1'
          end
          item
            TopLeft.Coordinates = {0000AE42000044420000000000000000}
            BottomRight.Coordinates = {0000B24200006C420000000000000000}
            Align = GLAlRight
            Name = 'Right1'
          end
          item
            TopLeft.Coordinates = {0000964200006C420000000000000000}
            BottomRight.Coordinates = {00009A42000074420000000000000000}
            Align = GLAlBottomLeft
            Name = 'BottomLeft1'
          end
          item
            TopLeft.Coordinates = {00009A4200006C420000000000000000}
            BottomRight.Coordinates = {0000AE42000074420000000000000000}
            Align = GLAlBottom
            Name = 'BottomCenter1'
          end
          item
            TopLeft.Coordinates = {0000AE4200006C420000000000000000}
            BottomRight.Coordinates = {0000B242000074420000000000000000}
            Align = GLAlBottomRight
            Name = 'BottomRight1'
          end>
        Name = 'checkbox_unchecked'
      end
      item
        Elements = <
          item
            TopLeft.Coordinates = {0000B44200003C420000000000000000}
            BottomRight.Coordinates = {0000B842000044420000000000000000}
            Align = GLAlTopLeft
            Name = 'TopLeft1'
          end
          item
            TopLeft.Coordinates = {0000B84200003C420000000000000000}
            BottomRight.Coordinates = {0000CC42000044420000000000000000}
            Align = GLAlTop
            Name = 'Top1'
          end
          item
            TopLeft.Coordinates = {0000CC4200003C420000000000000000}
            BottomRight.Coordinates = {0000D042000044420000000000000000}
            Align = GLAlTopRight
            Name = 'TopRight1'
          end
          item
            TopLeft.Coordinates = {0000B442000044420000000000000000}
            BottomRight.Coordinates = {0000B84200006C420000000000000000}
            Align = GLAlLeft
            Name = 'Left1'
          end
          item
            TopLeft.Coordinates = {0000B842000044420000000000000000}
            BottomRight.Coordinates = {0000CC4200006C420000000000000000}
            Align = GLAlCenter
            Name = 'Center1'
          end
          item
            TopLeft.Coordinates = {0000CC42000044420000000000000000}
            BottomRight.Coordinates = {0000D04200006C420000000000000000}
            Align = GLAlRight
            Name = 'Right1'
          end
          item
            TopLeft.Coordinates = {0000B44200006C420000000000000000}
            BottomRight.Coordinates = {0000B842000074420000000000000000}
            Align = GLAlBottomLeft
            Name = 'BottomLeft1'
          end
          item
            TopLeft.Coordinates = {0000B84200006C420000000000000000}
            BottomRight.Coordinates = {0000CC42000074420000000000000000}
            Align = GLAlBottom
            Name = 'BottomCenter1'
          end
          item
            TopLeft.Coordinates = {0000CC4200006C420000000000000000}
            BottomRight.Coordinates = {0000D042000074420000000000000000}
            Align = GLAlBottomRight
            Name = 'BottomRight1'
          end>
        Name = 'edit'
      end>
    FileName = 'Default.layout'
    Left = 280
    Top = 16
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Gui'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = 'DefaultSkin.bmp'
        Material.Texture.ImageAlpha = tiaOpaque
        Material.Texture.Disabled = False
      end>
    TexturePaths = '..\\..\\..\\..\\media\\'
    Left = 120
    Top = 16
  end
end
