object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Console'
  ClientHeight = 434
  ClientWidth = 559
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 209
    Top = 0
    Width = 350
    Height = 434
    Align = alClient
    Beveled = True
    MinSize = 40
    ExplicitLeft = 117
    ExplicitWidth = 442
    ExplicitHeight = 417
  end
  object Viewer: TGLSceneViewer
    Left = 209
    Top = 0
    Width = 350
    Height = 434
    Camera = GLCamera1
    Buffer.BackgroundColor = clMoneyGreen
    Buffer.AmbientColor.Color = {9A99993E9A99993E9A99993E0000803F}
    FieldOfView = 148.109207153320300000
    Align = alClient
    OnMouseDown = ViewerMouseDown
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 209
    Height = 434
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 1
    object Splitter2: TSplitter
      Left = 1
      Top = 233
      Width = 207
      Height = 200
      Align = alClient
    end
    object GroupBox1: TGroupBox
      Left = 1
      Top = 233
      Width = 207
      Height = 200
      Align = alClient
      Caption = 'Console options'
      TabOrder = 0
      object Label1: TLabel
        Left = 61
        Top = 79
        Width = 88
        Height = 13
        Caption = ' Typed Commands'
      end
      object Label2: TLabel
        Left = 69
        Top = 102
        Width = 75
        Height = 13
        Caption = 'Console Output'
      end
      object CheckBox1: TCheckBox
        Left = 8
        Top = 16
        Width = 196
        Height = 17
        TabStop = False
        Caption = 'AutoCompleteCommandsOnKeyPress'
        TabOrder = 0
        OnClick = CheckBox1Click
      end
      object CheckBox2: TCheckBox
        Left = 8
        Top = 32
        Width = 193
        Height = 17
        TabStop = False
        Caption = 'AutoCompleteCommandsOnEnter'
        TabOrder = 1
        OnClick = CheckBox2Click
      end
      object CheckBox3: TCheckBox
        Left = 8
        Top = 48
        Width = 195
        Height = 17
        TabStop = False
        Caption = 'ShowConsoleHelpIfUnknownCommand'
        TabOrder = 2
        OnClick = CheckBox3Click
      end
      object Button1: TButton
        Left = 8
        Top = 72
        Width = 44
        Height = 25
        Caption = 'Save'
        TabOrder = 3
        TabStop = False
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 8
        Top = 96
        Width = 44
        Height = 25
        Caption = 'Save'
        TabOrder = 4
        TabStop = False
        OnClick = Button2Click
      end
      object Button6: TButton
        Left = 155
        Top = 71
        Width = 44
        Height = 25
        Caption = 'Load'
        TabOrder = 5
        TabStop = False
        OnClick = Button6Click
      end
      object Button7: TButton
        Left = 155
        Top = 95
        Width = 44
        Height = 25
        Caption = 'Load'
        TabOrder = 6
        TabStop = False
        OnClick = Button7Click
      end
    end
    object ListBox1: TListBox
      Left = 1
      Top = 1
      Width = 207
      Height = 232
      TabStop = False
      Style = lbOwnerDrawFixed
      Align = alTop
      Enabled = False
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemHeight = 13
      Items.Strings = (
        'Instructions:'
        '  Click anywhere'
        '   to actiate the console'
        ''
        'Type "Help" and press Enter'
        'to get started'
        ''
        'Controls:'
        '  Up-down one line: Home <-> End '
        '  Up-down 1 page PageUp <-> PageDown'
        '  Enter comman: Return (enter)'
        '  Next-Prev command: Up <-> Down'
        '  Auto-Complete Command = Ctrl'
        ''
        'Have fun!'
        ''
        'Da Stranger')
      ParentFont = False
      TabOrder = 1
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = Scene
    OnProgress = GLCadencer1Progress
    Left = 240
    Top = 80
  end
  object Scene: TGLScene
    Left = 240
    Top = 16
    object GLCube1: TGLCube
      Position.Coordinates = {000000000000003F0000A0C00000803F}
      BehavioursData = {
        0458434F4C02010201060B54474C42496E657274696102001200000000020002
        00050000000000000080FF3F0200080500000000000000A00240050000000000
        0000A00140050000000000000080004009020008020008}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      Left = 328
      Top = 216
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
    object GLConsole1: TGLConsole
      HudSprite.Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
      HudSprite.Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F0000003F}
      HudSprite.Material.BlendingMode = bmTransparency
      HudSprite.Material.Texture.TextureMode = tmModulate
      HudSprite.Material.Texture.Disabled = False
      HudSprite.Rotation = 0.000000000000000000
      HudText.Position.Coordinates = {0000404000000040000000000000803F}
      HudText.Rotation = 0.000000000000000000
      HudText.ModulateColor.Color = {00000000000000000000803F0000803F}
      Options = []
    end
  end
  object Font1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Ranges = <
      item
        StartASCII = #0
        StopASCII = #255
        StartGlyphIdx = 0
      end>
    Left = 440
    Top = 16
  end
  object Timer1: TTimer
    Left = 440
    Top = 80
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = Viewer
    FormCaption = 'Console - %FPS'
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 336
    Top = 16
  end
end
