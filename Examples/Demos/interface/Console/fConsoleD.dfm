object FormConsole: TFormConsole
  Left = 258
  Top = 155
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Console'
  ClientHeight = 730
  ClientWidth = 1153
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnResize = FormResize
  PixelsPerInch = 168
  TextHeight = 24
  object Splitter1: TSplitter
    Left = 366
    Top = 0
    Width = 787
    Height = 730
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Beveled = True
    MinSize = 70
  end
  object Viewer: TGLSceneViewer
    Left = 366
    Top = 0
    Width = 787
    Height = 730
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.BackgroundColor = clMoneyGreen
    Buffer.AmbientColor.Color = {9A99993E9A99993E9A99993E0000803F}
    FieldOfView = 164.399627685546900000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = ViewerMouseDown
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 366
    Height = 730
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 1
    object Splitter2: TSplitter
      Left = 1
      Top = 407
      Width = 364
      Height = 322
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      MinSize = 53
      ExplicitLeft = 2
      ExplicitTop = 408
      ExplicitWidth = 362
      ExplicitHeight = 320
    end
    object GroupBox1: TGroupBox
      Left = 1
      Top = 407
      Width = 364
      Height = 322
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Caption = 'Console options'
      TabOrder = 0
      object Label1: TLabel
        Left = 107
        Top = 138
        Width = 161
        Height = 24
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = ' Typed Commands'
      end
      object Label2: TLabel
        Left = 121
        Top = 179
        Width = 131
        Height = 24
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Console Output'
      end
      object CheckBox1: TCheckBox
        Left = 14
        Top = 28
        Width = 343
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabStop = False
        Caption = 'AutoCompleteCommandsOnKeyPress'
        TabOrder = 0
        OnClick = CheckBox1Click
        OnKeyDown = FormKeyDown
        OnKeyPress = FormKeyPress
      end
      object CheckBox2: TCheckBox
        Left = 14
        Top = 56
        Width = 338
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabStop = False
        Caption = 'AutoCompleteCommandsOnEnter'
        TabOrder = 1
        OnClick = CheckBox2Click
        OnKeyDown = FormKeyDown
        OnKeyPress = FormKeyPress
      end
      object CheckBox3: TCheckBox
        Left = 14
        Top = 84
        Width = 341
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabStop = False
        Caption = 'ShowConsoleHelpIfUnknownCommand'
        TabOrder = 2
        OnClick = CheckBox3Click
        OnKeyDown = FormKeyDown
        OnKeyPress = FormKeyPress
      end
      object Button1: TButton
        Left = 14
        Top = 126
        Width = 77
        Height = 44
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Save'
        TabOrder = 3
        TabStop = False
        OnClick = Button1Click
        OnKeyDown = FormKeyDown
        OnKeyPress = FormKeyPress
      end
      object Button2: TButton
        Left = 14
        Top = 168
        Width = 77
        Height = 44
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Save'
        TabOrder = 4
        TabStop = False
        OnClick = Button2Click
        OnKeyDown = FormKeyDown
        OnKeyPress = FormKeyPress
      end
      object Button6: TButton
        Left = 271
        Top = 124
        Width = 77
        Height = 44
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Load'
        TabOrder = 5
        TabStop = False
        OnClick = Button6Click
      end
      object Button7: TButton
        Left = 271
        Top = 166
        Width = 77
        Height = 44
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Load'
        TabOrder = 6
        TabStop = False
        OnClick = Button7Click
      end
    end
    object ListBox1: TListBox
      Left = 1
      Top = 1
      Width = 364
      Height = 406
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabStop = False
      Style = lbOwnerDrawFixed
      Align = alTop
      Enabled = False
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemHeight = 23
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
    Left = 472
    Top = 16
  end
  object Timer1: TTimer
    Left = 472
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
    Left = 344
    Top = 16
  end
end
