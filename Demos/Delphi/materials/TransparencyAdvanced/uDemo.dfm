object Form1: TForm1
  Left = 304
  Top = 111
  Caption = 'Transparency Advanced'
  ClientHeight = 634
  ClientWidth = 808
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 808
    Height = 634
    Camera = GLCamera1
    AfterRender = GLSceneViewer1AfterRender
    Buffer.BackgroundColor = 10776320
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow, roDestinationAlpha, roNoColorBufferClear, roNoDepthBufferClear]
    Buffer.FaceCulling = False
    Buffer.ShadeModel = smFlat
    FieldOfView = 162.073303222656300000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 24
    Top = 24
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = ObjectContainer
      Position.Coordinates = {CDCCCC3F00000000CDCCCC3F0000803F}
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000040400000A0400000803F0000803F}
      SpotCutOff = 180.000000000000000000
    end
    object ClearFrameBuffer: TGLDirectOpenGL
      UseBuildList = False
      OnRender = ClearFrameBufferRender
      Blend = False
    end
    object ObjectContainer: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Surround: TGLDummyCube
        CubeSize = 1.000000000000000000
        CamInvarianceMode = cimPosition
        object GLCylinder1: TGLCylinder
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'Surround'
          Position.Coordinates = {00000000000000C0000000000000803F}
          Normals = nsNone
          BottomRadius = 4.000000000000000000
          Height = 8.000000000000000000
          Slices = 32
          TopRadius = 4.000000000000000000
          Parts = [cySides]
        end
      end
      object GLDisk1: TGLDisk
        Direction.Coordinates = {000000000000803F0000000000000000}
        Position.Coordinates = {00000000666666BF000000000000803F}
        Up.Coordinates = {0000000000000000000080BF00000000}
        Loops = 6
        OuterRadius = 2.000000000000000000
        Slices = 32
        SweepAngle = 360.000000000000000000
      end
      object GLMesh1: TGLMesh
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'Transparent'
        Direction.Coordinates = {FFFF7F3F000000002CBD3BB300000000}
        Position.Coordinates = {000080BE000000000000003F0000803F}
        Scale.Coordinates = {00000040000000400000004000000000}
        TurnAngle = 90.000000000000000000
        Up.Coordinates = {00000000FFFF7F3F0000000000000000}
        Mode = mmTriangleStrip
        VertexMode = vmVNC
      end
      object GLMesh2: TGLMesh
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'Transparent'
        Direction.Coordinates = {D7B35D3F00000000010000BF00000000}
        Position.Coordinates = {000080BF000000000000003F0000803F}
        Scale.Coordinates = {00000040000000400000004000000000}
        TurnAngle = 120.000000000000000000
        Up.Coordinates = {00000000FFFF7F3F0000000000000000}
        Mode = mmTriangleStrip
        VertexMode = vmVNC
      end
      object GLMesh3: TGLMesh
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'Transparent'
        Direction.Coordinates = {D7B35DBF00000000010000BF00000000}
        Position.Coordinates = {0000003F000000000000003F0000803F}
        Scale.Coordinates = {00000040000000400000004000000000}
        TurnAngle = -120.000000000000000000
        Up.Coordinates = {00000000FFFF7F3F0000000000000000}
        Mode = mmTriangleStrip
        VertexMode = vmVNC
      end
      object GLMesh4: TGLMesh
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'Transparent'
        Direction.Coordinates = {6E61D83E00000000C903683F00000000}
        Position.Coordinates = {0000803E000000009A9999BE0000803F}
        Scale.Coordinates = {00000040000000400000004000000000}
        TurnAngle = 25.000000000000000000
        Mode = mmTriangleStrip
        VertexMode = vmVNC
      end
      object GLMesh5: TGLMesh
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'Transparent'
        Direction.Coordinates = {010080A7000000000000803F00000000}
        Position.Coordinates = {0000003F00000000000080BF0000803F}
        Scale.Coordinates = {00000040000000400000004000000000}
        Mode = mmTriangleStrip
        VertexMode = vmVNC
      end
    end
    object LayeredFrameBuffer: TGLFBORenderer
      Active = False
      ColorTextureName = 'ColorLayers'
      DepthTextureName = 'DepthLayers'
      MaterialLibrary = GLMaterialLibrary1
      BackgroundColor.Color = {00000000DFDEDE3EA5A4243F0000803F}
      ClearOptions = []
      Camera = GLCamera1
      RootObject = CustomRederer
      TargetVisibility = tvFBOOnly
      EnabledRenderBuffers = []
      PostGenerateMipmap = False
    end
    object CustomRederer: TGLDirectOpenGL
      Visible = False
      UseBuildList = False
      OnRender = CustomRedererRender
      Blend = False
    end
    object ScreenQuad: TGLHUDSprite
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'Final'
      Visible = False
      Rotation = 0.000000000000000000
    end
    object GLHUDText1: TGLHUDText
      Position.Coordinates = {0000A0400000A040000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
      Text = '1'
      Rotation = 0.000000000000000000
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 24
    Top = 80
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Transparent'
        Tag = 0
        Material.DepthProperties.DepthCompareFunction = cfAlways
        Material.BlendingMode = bmCustom
        Material.MaterialOptions = [moNoLighting]
      end
      item
        Name = 'Surround'
        Tag = 0
        Material.MaterialOptions = [moNoLighting]
        Material.Texture.ImageClassName = 'TGLCompositeImage'
        Material.Texture.Image.Width = 256
        Material.Texture.Image.Height = 256
        Material.Texture.Image.Depth = 0
        Material.Texture.TextureWrap = twSeparate
        Material.Texture.TextureWrapS = twMirrorRepeat
        Material.Texture.TextureWrapR = twMirrorRepeat
        Material.Texture.Disabled = False
        TextureScale.Coordinates = {000080400000803F0000803F00000000}
      end
      item
        Name = 'ColorLayers'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.Depth = 6
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.MagFilter = maNearest
        Material.Texture.MinFilter = miNearest
        Material.Texture.Disabled = False
      end
      item
        Name = 'DepthLayers'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.Depth = 6
        Material.Texture.Image.ColorFormat = 6408
        Material.Texture.MagFilter = maNearest
        Material.Texture.MinFilter = miNearest
        Material.Texture.TextureFormat = tfExtended
        Material.Texture.TextureFormatEx = tfDEPTH_COMPONENT24
        Material.Texture.Disabled = False
      end
      item
        Name = 'Final'
        Tag = 0
        Shader = GLSLShader1
      end>
    Left = 104
    Top = 24
  end
  object GLSLShader1: TGLSLShader
    Enabled = False
    OnApply = GLSLShader1Apply
    ShaderStyle = ssReplace
    Left = 104
    Top = 80
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    ZoomSpeed = 1.000000000000000000
    FormCaption = 'Transparency Advanced - %FPS'
    Options = [snoShowFPS]
    KeyCombinations = <
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 24
    Top = 136
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 24
    Top = 192
  end
end
