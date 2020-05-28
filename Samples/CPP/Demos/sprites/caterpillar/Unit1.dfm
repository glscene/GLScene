object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'GLScene Caterpillar'
  ClientHeight = 315
  ClientWidth = 452
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 452
    Height = 315
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.DepthTest = False
    FieldOfView = 144.774841308593800000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    ObjectsSorting = osRenderFarthestFirst
    Left = 24
    Top = 24
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {000020410000A040000020410000803F}
    end
    object Sprite1: TGLSprite
      Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
      Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
      Material.FrontProperties.Emission.Color = {0000803F0000803F000000000000803F}
      Material.BlendingMode = bmAdditive
      Material.Texture.ImageClassName = 'TGLPicFileImage'
      Material.Texture.MinFilter = miLinear
      Material.Texture.TextureMode = tmModulate
      Material.Texture.Disabled = False
      Material.MaterialLibrary = GLMaterialLibrary1
      Width = 4.000000000000000000
      Height = 4.000000000000000000
      Rotation = 0.000000000000000000
    end
    object GLDummyCube1: TGLDummyCube
      ObjectsSorting = osNone
      CubeSize = 1.000000000000000000
      object Sprite2: TGLSprite
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'BlueBall'
        Position.Coordinates = {0000A04000000000000000000000803F}
        Width = 2.000000000000000000
        Height = 2.000000000000000000
        Rotation = 0.000000000000000000
      end
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'BlueBall'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Emission.Color = {F3F2F23EF3F2F23E0000803F0000803F}
        Material.BlendingMode = bmAdditive
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
        Material.MaterialLibrary = GLMaterialLibrary1
      end>
    Left = 96
    Top = 88
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 96
    Top = 24
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'GLScene Caterpillar - %FPS'
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
    Left = 248
    Top = 24
  end
end
