object Form1: TForm1
  Left = 160
  Top = 105
  Caption = 'Centering'
  ClientHeight = 252
  ClientWidth = 569
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 41
    Width = 544
    Height = 211
    Camera = GLCamera1
    Buffer.BackgroundColor = 11447982
    FieldOfView = 60.750083923339840000
    Align = alClient
    TabOrder = 0
  end
  object TrackBar1: TTrackBar
    Left = 544
    Top = 41
    Width = 25
    Height = 211
    Align = alRight
    Max = 80
    Min = -80
    Orientation = trVertical
    Frequency = 10
    TabOrder = 1
    ThumbLength = 10
    OnChange = TrackBar1Change
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 569
    Height = 41
    Align = alTop
    TabOrder = 2
    object Label1: TLabel
      Left = 56
      Top = 8
      Width = 68
      Height = 14
      Caption = 'Centered X, Y'
    end
    object Label2: TLabel
      Left = 232
      Top = 8
      Width = 55
      Height = 14
      Caption = 'Centered Y'
    end
    object Label3: TLabel
      Left = 408
      Top = 8
      Width = 80
      Height = 14
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
