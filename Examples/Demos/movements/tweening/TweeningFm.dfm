object FormTweening: TFormTweening
  Left = 325
  Top = 237
  Caption = 'Tweener'
  ClientHeight = 482
  ClientWidth = 716
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 129
    Width = 716
    Height = 353
    Camera = GLCamera1
    Buffer.BackgroundColor = clMedGray
    Buffer.Lighting = False
    FieldOfView = 148.366622924804700000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 716
    Height = 129
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 1
    object Label2: TLabel
      Left = 6
      Top = 77
      Width = 82
      Height = 13
      Caption = 'Time to run (ms):'
    end
    object Label1: TLabel
      Left = 167
      Top = 77
      Width = 82
      Height = 13
      Caption = 'Time to run (ms):'
    end
    object Label3: TLabel
      Left = 344
      Top = 46
      Width = 281
      Height = 13
      AutoSize = False
      Caption = '- Use CTRL + (Left/Right) mouse to move points A and B'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 344
      Top = 65
      Width = 353
      Height = 13
      AutoSize = False
      Caption = 
        '- Note that tweening can be used for any data (Position, Color, ' +
        'etc.)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
    end
    object Label5: TLabel
      Left = 344
      Top = 84
      Width = 353
      Height = 29
      AutoSize = False
      Caption = 
        '- Note that tweening is time dependant (it will play at the same' +
        ' speed on different computers)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
      WordWrap = True
    end
    object Button1: TButton
      Left = 6
      Top = 16
      Width = 155
      Height = 25
      Caption = 'Go to state A'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 167
      Top = 16
      Width = 155
      Height = 25
      Caption = 'Go to state B'
      TabOrder = 1
      OnClick = Button2Click
    end
    object UseCurrentPosition: TCheckBox
      Left = 328
      Top = 20
      Width = 217
      Height = 17
      Caption = 'Use current position as initial value'
      TabOrder = 2
    end
    object EaseTypeA: TComboBox
      Left = 6
      Top = 47
      Width = 155
      Height = 21
      Style = csDropDownList
      TabOrder = 3
    end
    object EaseTypeB: TComboBox
      Left = 167
      Top = 47
      Width = 155
      Height = 21
      Style = csDropDownList
      TabOrder = 4
    end
    object TimeA: TSpinEdit
      Left = 94
      Top = 74
      Width = 67
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 5
      Value = 1000
    end
    object TimeB: TSpinEdit
      Left = 255
      Top = 74
      Width = 67
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 6
      Value = 2000
    end
  end
  object GLScene1: TGLScene
    Left = 544
    Top = 8
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
    Left = 576
    Top = 8
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 608
    Top = 8
  end
end
