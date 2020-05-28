object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Custom Quad'
  ClientHeight = 347
  ClientWidth = 471
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
    Width = 471
    Height = 347
    Camera = GLCamera1
    FieldOfView = 133.244598388671900000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 32
    Top = 8
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      BehavioursData = {
        0458434F4C02010201060B54474C42496E657274696102001200000000020002
        00050000000000000080FF3F0200080500000000000000B40540050000000000
        0000000000050000000000000000000009020008020008}
      object DirectOpenGL1: TGLDirectOpenGL
        Direction.Coordinates = {000000000000803F2EBD3BB300000000}
        Up.Coordinates = {000000002EBD3BB3000080BF00000000}
        UseBuildList = False
        OnRender = DirectOpenGL1Render
        Blend = False
      end
    end
    object Torus1: TGLTorus
      Direction.Coordinates = {000000000000803F2EBD3BB300000000}
      Up.Coordinates = {000000002EBD3BB3000080BF00000000}
      MajorRadius = 1.299999952316284000
      MinorRadius = 0.100000001490116100
      Rings = 36
      StopAngle = 360.000000000000000000
      Parts = [toSides, toStartDisk, toStopDisk]
      BehavioursData = {
        0458434F4C02010201060B54474C42496E657274696102001200000000020002
        00050000000000000080FF3F0200080500000000000000000000050000000000
        00000000000500000000000000F0034009020008020008}
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000A0410000A0410000A0410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 75.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {00004040000000400000803F0000803F}
      Left = 200
      Top = 136
    end
  end
  object GLMaterialLibrary: TGLMaterialLibrary
    TexturePaths = '..\\..\\..\\..\\media\\'
    Left = 136
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 32
    Top = 72
  end
end
