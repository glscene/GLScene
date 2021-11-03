object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Newton Simple Spawn'
  ClientHeight = 481
  ClientWidth = 729
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 120
  TextHeight = 17
  object GLSceneViewer1: TGLSceneViewer
    Left = 131
    Top = 0
    Width = 598
    Height = 481
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 156.511001586914100000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 563
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 131
    Height = 481
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alLeft
    Caption = ' '
    TabOrder = 1
    object Button1: TButton
      Left = 18
      Top = 10
      Width = 93
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'AddCube'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 18
      Top = 49
      Width = 93
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'AddSphere'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 18
      Top = 88
      Width = 93
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'AddCone'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 18
      Top = 126
      Width = 93
      Height = 32
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'AddCylinder'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 18
      Top = 165
      Width = 93
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'AddCapsule'
      TabOrder = 4
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 18
      Top = 204
      Width = 93
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Remove All'
      TabOrder = 5
      OnClick = Button6Click
    end
  end
  object GLScene1: TGLScene
    Left = 128
    Top = 8
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Floor
      Position.Coordinates = {0000000000004040000020C10000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
    object Floor: TGLCube
      BehavioursData = {
        0458434F4C02010201060C54474C4E47445374617469630200060C5374617469
        6320466C6F6F7202000201060D474C4E47444D616E6167657231080009050000
        0000000AD7A3F83F1200000000}
      CubeSize = {000020410000003F00002041}
    end
    object GLDummyCube1: TGLDummyCube
      Position.Coordinates = {0000000000004040000000000000803F}
      CubeSize = 1.000000000000000000
    end
    object GLResolutionIndependantHUDText1: TGLResolutionIndependantHUDText
      Position.Coordinates = {0000003F6666663F000000000000803F}
      Text = 'Bodycount:=1'
      Rotation = 0.000000000000000000
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 216
    Top = 8
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Newton Simple Spawn - %FPS'
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
    Left = 216
    Top = 72
  end
  object GLNGDManager1: TGLNGDManager
    NewtonSurfaceItem = <>
    NewtonSurfacePair = <>
    NewtonJoint = <>
    Left = 440
    Top = 8
  end
end
