object FormTweening: TFormTweening
  Left = 325
  Top = 237
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Tweener'
  ClientHeight = 1092
  ClientWidth = 1400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 168
  TextHeight = 23
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 226
    Width = 1400
    Height = 866
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.BackgroundColor = clMedGray
    Buffer.Lighting = False
    FieldOfView = 166.826065063476600000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1400
    Height = 226
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 1
    object Label2: TLabel
      Left = 11
      Top = 135
      Width = 149
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Time to run (ms):'
    end
    object Label1: TLabel
      Left = 292
      Top = 135
      Width = 149
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Time to run (ms):'
    end
    object Label3: TLabel
      Left = 602
      Top = 81
      Width = 492
      Height = 22
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = '- Use CTRL + (Left/Right) mouse to move points A and B'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 602
      Top = 114
      Width = 618
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 
        '- Note that tweening can be used for any data (Position, Color, ' +
        'etc.)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
    end
    object Label5: TLabel
      Left = 602
      Top = 147
      Width = 618
      Height = 51
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 
        '- Note that tweening is time dependant (it will play at the same' +
        ' speed on different computers)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
      WordWrap = True
    end
    object Button1: TButton
      Left = 11
      Top = 28
      Width = 271
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Go to state A'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 292
      Top = 28
      Width = 272
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Go to state B'
      TabOrder = 1
      OnClick = Button2Click
    end
    object UseCurrentPosition: TCheckBox
      Left = 574
      Top = 35
      Width = 380
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Use current position as initial value'
      TabOrder = 2
    end
    object EaseTypeA: TComboBox
      Left = 11
      Top = 82
      Width = 271
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Style = csDropDownList
      TabOrder = 3
    end
    object EaseTypeB: TComboBox
      Left = 292
      Top = 82
      Width = 272
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Style = csDropDownList
      TabOrder = 4
    end
    object TimeA: TSpinEdit
      Left = 165
      Top = 130
      Width = 117
      Height = 34
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      MaxValue = 0
      MinValue = 0
      TabOrder = 5
      Value = 1000
    end
    object TimeB: TSpinEdit
      Left = 446
      Top = 130
      Width = 118
      Height = 34
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      MaxValue = 0
      MinValue = 0
      TabOrder = 6
      Value = 2000
    end
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 146
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      CameraStyle = csOrtho2D
    end
    object GLPlane1: TGLPlane
      Material.FrontProperties.Diffuse.Color = {B1B0303EF9F8F83DC3C2423F0000803F}
      OnProgress = GLPlane1Progress
      Height = 50.000000000000000000
      Width = 50.000000000000000000
    end
    object PointA: TGLPoints
      Position.Coordinates = {0000164400009643000000000000803F}
      NoZWrite = False
      Static = False
      Size = 10.000000000000000000
      Style = psRound
      object GLFlatText1: TGLFlatText
        Position.Coordinates = {000000000000A041000000000000803F}
        BitmapFont = GLWindowsBitmapFont1
        Text = 'Point A'
        Alignment = taCenter
        Layout = tlCenter
        Options = []
      end
    end
    object PointB: TGLPoints
      Position.Coordinates = {0000484200004842000000000000803F}
      NoZWrite = False
      Static = False
      Size = 10.000000000000000000
      Style = psRound
      object GLFlatText2: TGLFlatText
        Position.Coordinates = {000000000000A041000000000000803F}
        BitmapFont = GLWindowsBitmapFont1
        Text = 'Point B'
        Alignment = taCenter
        Layout = tlCenter
        Options = []
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 24
    Top = 226
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 160
    Top = 146
  end
end
