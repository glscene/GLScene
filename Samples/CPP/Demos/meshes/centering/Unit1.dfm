object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Centering'
  ClientHeight = 291
  ClientWidth = 552
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 41
    Width = 527
    Height = 250
    Camera = GLCamera1
    Buffer.BackgroundColor = 11447982
    FieldOfView = 69.555664062500000000
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 481
    ExplicitHeight = 204
  end
  object TrackBar1: TTrackBar
    Left = 527
    Top = 41
    Width = 25
    Height = 250
    Align = alRight
    Max = 80
    Min = -80
    Orientation = trVertical
    Frequency = 10
    TabOrder = 1
    ThumbLength = 10
    OnChange = TrackBar1Change
    ExplicitLeft = 481
    ExplicitTop = 34
    ExplicitHeight = 211
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 552
    Height = 41
    Align = alTop
    TabOrder = 2
    ExplicitLeft = -63
    ExplicitWidth = 569
    object Label1: TLabel
      Left = 56
      Top = 8
      Width = 67
      Height = 13
      Caption = 'Centered X, Y'
    end
    object Label2: TLabel
      Left = 232
      Top = 8
      Width = 54
      Height = 13
      Caption = 'Centered Y'
    end
    object Label3: TLabel
      Left = 408
      Top = 8
      Width = 80
      Height = 13
      Caption = 'Centered X, Y, Z'
    end
  end
  object GLScene1: TGLScene
    Left = 256
    Top = 48
    object DummyCube3: TGLDummyCube
      Position.Coordinates = {000000C000000000000000000000803F}
      ShowAxes = True
      CubeSize = 1.000000000000000000
      VisibleAtRunTime = True
      object FreeForm3: TGLFreeForm
        Material.FrontProperties.Diffuse.Color = {0000803F8180003F8180803E0000803F}
      end
    end
    object DummyCube2: TGLDummyCube
      ShowAxes = True
      CubeSize = 1.000000000000000000
      VisibleAtRunTime = True
      object FreeForm2: TGLFreeForm
        Material.FrontProperties.Diffuse.Color = {0000803F8180003F8180803E0000803F}
      end
    end
    object DummyCube1: TGLDummyCube
      Position.Coordinates = {0000004000000000000000000000803F}
      ShowAxes = True
      CubeSize = 1.000000000000000000
      VisibleAtRunTime = True
      object FreeForm1: TGLFreeForm
        Material.FrontProperties.Diffuse.Color = {0000803F8180003F8180803E0000803F}
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00004040000020410000A0400000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DCCamera: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 180.000000000000000000
        TargetObject = DummyCube2
        Position.Coordinates = {0000000000000040000070410000803F}
        Left = 256
        Top = 96
      end
    end
  end
end
