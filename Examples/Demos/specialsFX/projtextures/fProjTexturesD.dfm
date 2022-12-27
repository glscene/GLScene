object FormProjTextures: TFormProjTextures
  Left = 202
  Top = 106
  Caption = 'Projected Textures'
  ClientHeight = 499
  ClientWidth = 762
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  TextHeight = 13
  object viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 762
    Height = 499
    Camera = camera
    Buffer.BackgroundColor = clSilver
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    Buffer.DepthPrecision = dp32bits
    FieldOfView = 157.335983276367200000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = viewerMouseDown
    OnMouseUp = viewerMouseUp
    TabOrder = 0
    ExplicitWidth = 755
    ExplicitHeight = 489
  end
  object scene: TGLScene
    Left = 56
    Top = 24
    object ProjLight: TGLProjectedTextures
      Emitters = <>
      Style = ptsOriginal
      object scenery: TGLDummyCube
        CubeSize = 1.000000000000000000
        object GLCube2: TGLCube
          Scale.Coordinates = {000040C0000040C0000040C000000000}
          Visible = False
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
            Slices = 4
            Stacks = 4
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
            Slices = 4
            Stacks = 4
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
    object camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = scenery
      Position.Coordinates = {000040400000003F000020400000803F}
      Left = 328
      Top = 216
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = scene
    OnProgress = GLCadencer1Progress
    Left = 56
    Top = 200
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
    TexturePaths = '..\..\..\..\media\'
    Left = 56
    Top = 88
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = viewer
    FormCaption = 'Projected Textures - %FPS'
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
    Left = 56
    Top = 144
  end
end
