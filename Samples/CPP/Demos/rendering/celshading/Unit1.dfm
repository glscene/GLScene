object Form1: TForm1
  Left = 192
  Top = 114
  Caption = 'Cel Shading'
  ClientHeight = 420
  ClientWidth = 480
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 480
    Height = 420
    Camera = GLCamera1
    FieldOfView = 153.215011596679700000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 8
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = GLDummyCube1
        Position.Coordinates = {00000000000000400000A0400000803F}
        Direction.Coordinates = {00000000000000000000803F00000000}
        Up.Coordinates = {000000000000803F0000008000000000}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          LightStyle = lsOmni
          SpotCutOff = 180.000000000000000000
        end
      end
    end
    object GLActor1: TGLActor
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'TexturedCellMat'
      Direction.Coordinates = {000000000000803F2EBD3BB300000000}
      PitchAngle = 90.000000000000000000
      Up.Coordinates = {000000002EBD3BB3000080BF00000000}
      Interval = 100
    end
    object GLTorus1: TGLTorus
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'ColoredCelMat'
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      MajorRadius = 2.500000000000000000
      MinorRadius = 0.250000000000000000
      StopAngle = 360.000000000000000000
      Parts = [toSides, toStartDisk, toStopDisk]
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'TexturedCellMat'
        Tag = 0
        Material.FrontProperties.Emission.Color = {0000803F0000003F000000000000803F}
        Material.MaterialOptions = [moNoLighting]
        Material.Texture.Disabled = False
        Shader = GLTexturedCelShader
      end
      item
        Name = 'ColoredCelMat'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Shader = GLColoredCelShader
      end>
    Left = 136
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 32
    Top = 56
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpNormal
    Left = 304
    Top = 8
  end
  object GLTexturedCelShader: TGLCelShader
    CelShaderOptions = [csoOutlines, csoTextured]
    OutlineWidth = 3.000000000000000000
    Left = 136
    Top = 56
  end
  object GLColoredCelShader: TGLCelShader
    CelShaderOptions = [csoOutlines]
    OutlineWidth = 3.000000000000000000
    Left = 304
    Top = 56
  end
end
