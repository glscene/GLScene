object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Pong'
  ClientHeight = 235
  ClientWidth = 534
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
    Width = 534
    Height = 235
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    FieldOfView = 133.897399902343800000
    Align = alClient
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 32
    Top = 8
    object GLShadowVolume: TGLShadowVolume
      Lights = <
        item
          LightSource = GLLightSource1
        end>
      Occluders = <
        item
          Caster = Ball
        end
        item
          Caster = Pad
        end>
      Options = [svoCacheSilhouettes, svoWorldScissorClip, svoDesignVisible]
      object Plane1: TGLPlane
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'Mat'
        Position.Coordinates = {0000000000000000000000BF0000803F}
        Height = 10.000000000000000000
        Width = 15.000000000000000000
        object Cube1: TGLCube
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'Edge'
          Position.Coordinates = {000000000000A0400000803E0000803F}
          CubeSize = {000078410000003F0000003F}
        end
        object Cube2: TGLCube
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'Edge'
          Position.Coordinates = {0000F0C0000000000000803E0000803F}
          CubeSize = {0000003F000020410000003F}
        end
        object Cube3: TGLCube
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'Edge'
          Position.Coordinates = {0000F040000000000000803E0000803F}
          CubeSize = {0000003F000020410000003F}
        end
      end
      object Ball: TGLSphere
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'Ball'
        Radius = 0.400000005960464500
        Slices = 12
        Stacks = 9
      end
      object Pad: TGLCube
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'Pad'
        Position.Coordinates = {00000000666696C0000080BE0000803F}
        CubeSize = {000000400000003F0000003F}
      end
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = DummyCube1
        Position.Coordinates = {00000000000070C1000020410000803F}
        Direction.Coordinates = {000000000000803F0000000000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00004040000080BF0000A0400000803F}
      Specular.Color = {0000803F0000803F0000803F0000803F}
      SpotCutOff = 180.000000000000000000
    end
    object SpaceText1: TGLSpaceText
      Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
      Material.FrontProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
      Material.FrontProperties.Emission.Color = {0000000000000000A1A0203F0000803F}
      Material.FrontProperties.Shininess = 75
      Direction.Coordinates = {00000000EA4677BFEE83843E00000000}
      Position.Coordinates = {0000C0BF00000000000060400000803F}
      Scale.Coordinates = {000000400000803F0000803F00000000}
      Up.Coordinates = {00000000EE83843EEA46773F00000000}
      Extrusion = 1.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      Lines.Strings = (
        '000')
      allowedDeviation = 1.000000000000000000
      CharacterRange = stcrNumbers
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Mat'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {000000000000803F000000000000803F}
      end
      item
        Name = 'Edge'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803FBDBCBC3EF1F0F03D0000803F}
      end
      item
        Name = 'Ball'
        Tag = 0
        Material.FrontProperties.Shininess = 75
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
      end
      item
        Name = 'Pad'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
      end>
    Left = 40
    Top = 72
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 344
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 0.050000000000000000
    OnProgress = GLCadencer1Progress
    Left = 120
    Top = 8
  end
end
