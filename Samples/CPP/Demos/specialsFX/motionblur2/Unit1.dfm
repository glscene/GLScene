object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Motion Blur2'
  ClientHeight = 344
  ClientWidth = 501
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 501
    Height = 344
    Camera = Cam
    Buffer.BackgroundColor = clWhite
    Buffer.AmbientColor.Color = {0000000000000000000000000000803F}
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Buffer.AntiAliasing = aaNone
    FieldOfView = 138.596145629882800000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 56
    Top = 16
    object Light: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00000000000048C30000FA430000803F}
      LightStyle = lsOmni
      Specular.Color = {0000803F0000803F0000803F0000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCube1: TGLCube
      Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
      Material.FrontProperties.Shininess = 128
      Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
      CubeSize = {0000A0400000803F0000803F}
      object GLSphere1: TGLSphere
        Material.FrontProperties.Diffuse.Color = {000000000000803F000000000000803F}
        Material.FrontProperties.Shininess = 128
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Position.Coordinates = {0000004000004040000000000000803F}
        Radius = 0.699999988079071000
      end
      object GLTorus1: TGLTorus
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
        Material.FrontProperties.Shininess = 128
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Position.Coordinates = {000040C00000A0400000803F0000803F}
        MajorRadius = 0.699999988079071000
        MinorRadius = 0.300000011920929000
        StopAngle = 360.000000000000000000
        Parts = [toSides, toStartDisk, toStopDisk]
      end
      object GLIcosahedron1: TGLIcosahedron
        Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
        Material.FrontProperties.Shininess = 128
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Position.Coordinates = {000080400000C0C0000040C00000803F}
        Scale.Coordinates = {00000040000000400000004000000000}
      end
      object GLTeapot1: TGLTeapot
        Material.FrontProperties.Diffuse.Color = {0000803FCFCECE3E0000803F0000803F}
        Material.FrontProperties.Shininess = 128
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Position.Coordinates = {0000A04000008040000080400000803F}
        Scale.Coordinates = {00000040000000400000004000000000}
      end
    end
    object GLMotionBlur1: TGLMotionBlur
    end
    object GLCube2: TGLCube
      Material.FrontProperties.Diffuse.Color = {000000000000803F0000803F0000803F}
      Direction.Coordinates = {6C61D83E7C1BC43EDC46523F00000000}
      PitchAngle = 25.000000000000000000
      RollAngle = 25.000000000000000000
      TurnAngle = 25.000000000000000000
      Up.Coordinates = {7E1BC4BEAC99653F287562BE00000000}
    end
    object GLCube3: TGLCube
      Position.Coordinates = {0000A04000008040000080400000803F}
    end
    object Cam: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 65.000000000000000000
      TargetObject = GLCube3
      Position.Coordinates = {00000000000070C1000000000000803F}
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 0.020000000000000000
    OnProgress = GLCadencer1Progress
    Left = 144
    Top = 16
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Tag = 0
      end>
    Left = 56
    Top = 80
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'GLScene Motion Blur - %FPS'
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
    Left = 360
    Top = 16
  end
end
