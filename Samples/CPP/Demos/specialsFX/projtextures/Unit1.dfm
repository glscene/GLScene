object Form1: TForm1
  Left = 198
  Top = 138
  Caption = 'Projected Textures'
  ClientHeight = 444
  ClientWidth = 684
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 684
    Height = 444
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Camera = GLCamera1
    Buffer.BackgroundColor = clSilver
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Buffer.DepthPrecision = dp32bits
    FieldOfView = 154.614669799804700000
    Align = alClient
    OnMouseDown = viewerMouseDown
    OnMouseMove = viewerMouseMove
    OnMouseUp = viewerMouseUp
    TabOrder = 0
  end
  object scene: TGLScene
    Left = 40
    Top = 24
    object ProjLight: TGLProjectedTextures
      Emitters = <>
      Style = ptsOriginal
      object scenery: TGLDummyCube
        CubeSize = 1.000000000000000000
        object GLCube2: TGLCube
          Scale.Coordinates = {000020C1000020C1000020C100000000}
          NormalDirection = ndInside
        end
        object light2: TGLDummyCube
          Position.Coordinates = {0000803F0000803F000000400000803F}
          CubeSize = 0.200000002980232200
          EdgeColor.Color = {000000000000803F000000000000803F}
          object GLSphere3: TGLSphere
            Material.FrontProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
            Material.FrontProperties.Emission.Color = {0000803F0000803F000000000000803F}
            Radius = 0.050000000745058060
            Slices = 8
            Stacks = 8
          end
          object emitter2: TGLTextureEmitter
            FOVy = 90.000000000000000000
            Aspect = 1.000000000000000000
          end
        end
        object Light: TGLDummyCube
          Position.Coordinates = {0000000000000000000000400000803F}
          CubeSize = 0.200000002980232200
          EdgeColor.Color = {0000803F0000803F000000000000803F}
          object GLLightSource1: TGLLightSource
            ConstAttenuation = 1.000000000000000000
            SpotCutOff = 180.000000000000000000
            SpotDirection.Coordinates = {00000000000000000000803F00000000}
          end
          object GLSphere2: TGLSphere
            Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
            Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
            Radius = 0.050000000745058060
            Slices = 8
            Stacks = 8
          end
          object emitter1: TGLTextureEmitter
            FOVy = 90.000000000000000000
            Aspect = 1.000000000000000000
          end
        end
        object GLCube1: TGLCube
          CubeSize = {0000403F0000403F0000403F}
        end
        object GLPlane1: TGLPlane
          Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
          Material.FrontProperties.Diffuse.Color = {FFFEFE3EFFFEFE3EFFFEFE3E0000803F}
          Material.MaterialOptions = [moNoLighting]
          Material.Texture.TextureMode = tmModulate
          Material.Texture.Disabled = False
          Position.Coordinates = {000000000000003F0000C0BF0000803F}
          Height = 4.000000000000000000
          Width = 4.000000000000000000
        end
        object GLPlane2: TGLPlane
          Material.Texture.Disabled = False
          Direction.Coordinates = {0000803F000000000000000000000000}
          Position.Coordinates = {000000C00000003F0000003F0000803F}
          Height = 4.000000000000000000
          Width = 4.000000000000000000
        end
        object GLPlane3: TGLPlane
          Material.FrontProperties.Diffuse.Color = {FFFEFE3EFFFEFE3EFFFEFE3E0000803F}
          Material.MaterialOptions = [moNoLighting]
          Material.Texture.TextureMode = tmModulate
          Material.Texture.Disabled = False
          Direction.Coordinates = {000000000000803F0000000000000000}
          Position.Coordinates = {000000000000C0BF0000003F0000803F}
          Up.Coordinates = {00000000000000000000803F00000000}
          Height = 4.000000000000000000
          Width = 4.000000000000000000
        end
        object GLSphere1: TGLSphere
          Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FE1E0E03D0000803F}
          Position.Coordinates = {CDCC4C3E0000003F0000003F0000803F}
          Radius = 0.300000011920929000
        end
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      SceneScale = 2.000000000000000000
      TargetObject = scenery
      Position.Coordinates = {000040400000003F000020400000803F}
      Left = 328
      Top = 216
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = scene
    OnProgress = GLCadencer1Progress
    Left = 112
    Top = 24
  end
  object matLib: TGLMaterialLibrary
    Materials = <
      item
        Name = 'spot'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.ImageAlpha = tiaOpaque
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureWrap = twNone
        Material.Texture.Disabled = False
      end
      item
        Name = 'spot2'
        Tag = 0
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureWrap = twNone
        Material.Texture.Disabled = False
      end>
    Left = 40
    Top = 80
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 112
    Top = 80
  end
end
