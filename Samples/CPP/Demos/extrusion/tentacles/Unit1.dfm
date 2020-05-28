object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Tentacles'
  ClientHeight = 373
  ClientWidth = 541
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
    Top = 0
    Width = 541
    Height = 373
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 149.984252929687500000
    Align = alClient
    TabOrder = 0
  end
  object PanelFPS: TPanel
    Left = 208
    Top = 8
    Width = 121
    Height = 17
    Caption = 'FPS'
    TabOrder = 1
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object DCBase: TGLDummyCube
      Position.Coordinates = {00000000000000C0000000000000803F}
      CubeSize = 1.000000000000000000
      object Sphere1: TGLSphere
        Material.FrontProperties.Ambient.Color = {00000000CDCC4C3E000000000000803F}
        Material.FrontProperties.Diffuse.Color = {938C0C3E938E0E3F938C0C3E0000803F}
        Material.FrontProperties.Emission.Color = {0000000039B4483E000000000000803F}
        Scale.Coordinates = {000000400000003F0000004000000000}
        Radius = 1.000000000000000000
        Slices = 32
        Stacks = 8
      end
      object Pipe1: TGLPipe
        Nodes = <>
        Radius = 1.000000000000000000
      end
      object Pipe2: TGLPipe
        Nodes = <>
        Radius = 1.000000000000000000
      end
      object Pipe3: TGLPipe
        Nodes = <>
        Radius = 1.000000000000000000
      end
      object Pipe4: TGLPipe
        Nodes = <>
        Radius = 1.000000000000000000
      end
      object Pipe5: TGLPipe
        Nodes = <>
        Radius = 1.000000000000000000
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00004842000020420000F0410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DCTarget: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DCTarget
      Position.Coordinates = {0000C0400000A040000080400000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 56
    Top = 16
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 48
  end
end
