object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Gui Paint'
  ClientHeight = 429
  ClientWidth = 548
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
    Width = 548
    Height = 429
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 153.757293701171900000
    PenAsTouch = False
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
    object GuiRoot: TGLBaseControl
      Autosize = False
      RedrawAtOnce = False
      Rotation = 0.000000000000000000
      NoZWrite = False
      DoChangesOnProgress = False
      Width = 10000.000000000000000000
      Height = 10000.000000000000000000
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
        Height = 300.000000000000000000
        Left = 100.000000000000000000
        Top = 100.000000000000000000
        Position.Coordinates = {0000C8420000C842000000000000803F}
        BitmapFont = WindowsBitmapFont1
        DefaultColor = clMaroon
        Caption = 'Paint'
        TitleColor = clWhite
        OnMoving = GLForm1Moving
        TitleOffset = 2.000000000000000000
        object PenButton: TGLButton
          Autosize = False
          RedrawAtOnce = False
          GuiLayout = GLGuiLayout1
          GuiLayoutName = 'button'
          Rotation = 0.000000000000000000
          NoZWrite = False
          DoChangesOnProgress = False
          Width = 40.000000000000000000
          Height = 40.000000000000000000
          Left = 10.000000000000000000
          Top = 32.000000000000000000
          Position.Coordinates = {0000DC4200000443000000000000803F}
          BitmapFont = WindowsBitmapFont1
          DefaultColor = clBlack
          Focused = False
          FocusedColor = clBlack
          Group = 1
          BitBtn.MaterialLibrary = GLMaterialLibrary1
          BitBtn.LibMaterialName = 'Pen'
          Pressed = True
          OnButtonClick = PenButtonButtonClick
          GuiLayoutNamePressed = 'buttonpressed'
          LogicWidth = 24.000000000000000000
          LogicHeight = 20.000000000000000000
          YOffset = 1.000000000000000000
          AllowUp = True
        end
        object BrushButton: TGLButton
          Autosize = False
          RedrawAtOnce = False
          GuiLayout = GLGuiLayout1
          GuiLayoutName = 'button'
          Rotation = 0.000000000000000000
          NoZWrite = False
          DoChangesOnProgress = False
          Width = 40.000000000000000000
          Height = 40.000000000000000000
          Left = 50.000000000000000000
          Top = 32.000000000000000000
          Position.Coordinates = {0000164300000443000000000000803F}
          BitmapFont = WindowsBitmapFont1
          DefaultColor = clBlack
          Focused = False
          FocusedColor = clBlack
          Group = 1
          BitBtn.MaterialLibrary = GLMaterialLibrary1
          BitBtn.LibMaterialName = 'Brush'
          Pressed = False
          OnButtonClick = BrushButtonButtonClick
          GuiLayoutNamePressed = 'buttonpressed'
          LogicWidth = 22.000000000000000000
          LogicHeight = 20.000000000000000000
          YOffset = 1.000000000000000000
          AllowUp = True
        end
        object GLPanel1: TGLPanel
          Autosize = False
          RedrawAtOnce = False
          GuiLayout = GLGuiLayout1
          GuiLayoutName = 'button'
          Rotation = 0.000000000000000000
          NoZWrite = False
          DoChangesOnProgress = False
          Width = 280.000000000000000000
          Height = 214.000000000000000000
          Left = 10.000000000000000000
          Top = 76.000000000000000000
          Position.Coordinates = {0000DC4200003043000000000000803F}
          object GLCanvas: TGLCustomControl
            Autosize = False
            RedrawAtOnce = False
            GuiLayout = GLGuiLayout1
            GuiLayoutName = 'button'
            Rotation = 0.000000000000000000
            NoZWrite = False
            DoChangesOnProgress = False
            Width = 274.000000000000000000
            Height = 208.000000000000000000
            Left = 3.000000000000000000
            Top = 3.000000000000000000
            Position.Coordinates = {0000E24200003343000000000000803F}
            OnMouseDown = GLCanvasMouseDown
            OnMouseMove = GLCanvasMouseMove
            BitmapFont = WindowsBitmapFont1
            DefaultColor = clBlack
            Focused = False
            FocusedColor = clBlack
            OnRender = GLCanvasRender
            Centered = False
            MaxInvalidRenderCount = 0
          end
        end
        object WhiteButton: TGLButton
          Autosize = False
          RedrawAtOnce = False
          GuiLayout = GLGuiLayout1
          GuiLayoutName = 'button'
          Rotation = 0.000000000000000000
          NoZWrite = False
          DoChangesOnProgress = False
          Width = 20.000000000000000000
          Height = 20.000000000000000000
          Left = 90.000000000000000000
          Top = 32.000000000000000000
          Position.Coordinates = {00003E4300000443000000000000803F}
          BitmapFont = WindowsBitmapFont1
          DefaultColor = clBlack
          Focused = False
          FocusedColor = clBlack
          Group = 2
          BitBtn.MaterialLibrary = GLMaterialLibrary1
          BitBtn.LibMaterialName = 'White'
          Pressed = False
          OnButtonClick = WhiteButtonButtonClick
          GuiLayoutNamePressed = 'buttonpressed'
          LogicWidth = 13.000000000000000000
          LogicHeight = 10.000000000000000000
          AllowUp = True
        end
        object BlackButton: TGLButton
          Autosize = False
          RedrawAtOnce = False
          GuiLayout = GLGuiLayout1
          GuiLayoutName = 'button'
          Rotation = 0.000000000000000000
          NoZWrite = False
          DoChangesOnProgress = False
          Width = 20.000000000000000000
          Height = 20.000000000000000000
          Left = 110.000000000000000000
          Top = 32.000000000000000000
          Position.Coordinates = {0000524300000443000000000000803F}
          BitmapFont = WindowsBitmapFont1
          DefaultColor = clBlack
          Focused = False
          FocusedColor = clBlack
          Group = 2
          BitBtn.MaterialLibrary = GLMaterialLibrary1
          BitBtn.LibMaterialName = 'Black'
          Pressed = True
          OnButtonClick = BlackButtonButtonClick
          GuiLayoutNamePressed = 'buttonpressed'
          LogicWidth = 13.000000000000000000
          LogicHeight = 10.000000000000000000
          AllowUp = True
        end
        object RedButton: TGLButton
          Autosize = False
          RedrawAtOnce = False
          GuiLayout = GLGuiLayout1
          GuiLayoutName = 'button'
          Rotation = 0.000000000000000000
          NoZWrite = False
          DoChangesOnProgress = False
          Width = 20.000000000000000000
          Height = 20.000000000000000000
          Left = 130.000000000000000000
          Top = 32.000000000000000000
          Position.Coordinates = {0000664300000443000000000000803F}
          BitmapFont = WindowsBitmapFont1
          DefaultColor = clBlack
          Focused = False
          FocusedColor = clBlack
          Group = 2
          BitBtn.MaterialLibrary = GLMaterialLibrary1
          BitBtn.LibMaterialName = 'Red'
          Pressed = False
          OnButtonClick = RedButtonButtonClick
          GuiLayoutNamePressed = 'buttonpressed'
          LogicWidth = 13.000000000000000000
          LogicHeight = 10.000000000000000000
          AllowUp = True
        end
        object GreenButton: TGLButton
          Autosize = False
          RedrawAtOnce = False
          GuiLayout = GLGuiLayout1
          GuiLayoutName = 'button'
          Rotation = 0.000000000000000000
          NoZWrite = False
          DoChangesOnProgress = False
          Width = 20.000000000000000000
          Height = 20.000000000000000000
          Left = 150.000000000000000000
          Top = 32.000000000000000000
          Position.Coordinates = {00007A4300000443000000000000803F}
          BitmapFont = WindowsBitmapFont1
          DefaultColor = clBlack
          Focused = False
          FocusedColor = clBlack
          Group = 2
          BitBtn.MaterialLibrary = GLMaterialLibrary1
          BitBtn.LibMaterialName = 'Green'
          Pressed = False
          OnButtonClick = GreenButtonButtonClick
          GuiLayoutNamePressed = 'buttonpressed'
          LogicWidth = 13.000000000000000000
          LogicHeight = 10.000000000000000000
          AllowUp = True
        end
        object BlueButton: TGLButton
          Autosize = False
          RedrawAtOnce = False
          GuiLayout = GLGuiLayout1
          GuiLayoutName = 'button'
          Rotation = 0.000000000000000000
          NoZWrite = False
          DoChangesOnProgress = False
          Width = 20.000000000000000000
          Height = 20.000000000000000000
          Left = 170.000000000000000000
          Top = 32.000000000000000000
          Position.Coordinates = {0000874300000443000000000000803F}
          BitmapFont = WindowsBitmapFont1
          DefaultColor = clBlack
          Focused = False
          FocusedColor = clBlack
          Group = 2
          BitBtn.MaterialLibrary = GLMaterialLibrary1
          BitBtn.LibMaterialName = 'Blue'
          Pressed = False
          OnButtonClick = BlueButtonButtonClick
          GuiLayoutNamePressed = 'buttonpressed'
          LogicWidth = 13.000000000000000000
          LogicHeight = 10.000000000000000000
          AllowUp = True
        end
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
    Left = 104
    Top = 16
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 424
    Top = 88
  end
  object WindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 40
    Top = 80
  end
  object MainMenu1: TMainMenu
    Left = 328
    Top = 16
    object File1: TMenuItem
      Caption = '&File'
      object miOpen1: TMenuItem
        Caption = '&Open...'
        OnClick = miOpen1Click
      end
      object miSave1: TMenuItem
        Caption = '&Save...'
        OnClick = miSave1Click
      end
    end
    object miFont1: TMenuItem
      Caption = 'Font'
      object miWindowsFont1: TMenuItem
        Caption = 'Set New Font...'
        OnClick = miWindowsFont1Click
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
    Left = 488
    Top = 88
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
    FileName = '..\..\media\default.layout'
    Left = 40
    Top = 144
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Gui'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = 'defaultskin.bmp'
        Material.Texture.ImageAlpha = tiaOpaque
        Material.Texture.TextureMode = tmReplace
        Material.Texture.Disabled = False
      end
      item
        Name = 'Brush'
        Tag = 0
        Material.BlendingMode = bmTransparency
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = 'brush.bmp'
        Material.Texture.ImageAlpha = tiaTopLeftPointColorTransparent
        Material.Texture.TextureMode = tmReplace
        Material.Texture.Disabled = False
      end
      item
        Name = 'Pen'
        Tag = 0
        Material.BlendingMode = bmTransparency
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = 'pen.bmp'
        Material.Texture.ImageAlpha = tiaTopLeftPointColorTransparent
        Material.Texture.TextureMode = tmReplace
        Material.Texture.Disabled = False
      end
      item
        Name = 'White'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmReplace
      end
      item
        Name = 'Black'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.Texture.TextureMode = tmReplace
      end
      item
        Name = 'Red'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
      end
      item
        Name = 'Green'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {000000000000803F000000000000803F}
      end
      item
        Name = 'Blue'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
      end>
    TexturePaths = '..\\..\\..\\..\\media\\'
    Left = 200
    Top = 16
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'bmp'
    Filter = 'Bitmaps|*.bmp'
    Title = 'Open Bitmap'
    Left = 392
    Top = 16
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'bmp'
    Filter = 'Bitmaps|*.bmp'
    Title = 'Save Bitmap'
    Left = 464
    Top = 16
  end
end
