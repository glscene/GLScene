object Form1: TForm1
  Left = 296
  Top = 145
  Caption = 'Simple GLSL Shader'
  ClientHeight = 551
  ClientWidth = 787
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 787
    Height = 551
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Camera = Cam
    Buffer.BackgroundColor = 4194304
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow, roDestinationAlpha]
    FieldOfView = 147.615402221679700000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 176
    Top = 40
    object GLDOInitialize: TGLDirectOpenGL
      UseBuildList = False
      OnRender = GLDOInitializeRender
      Blend = False
    end
    object Scene: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCube1: TGLCube
        Material.FrontProperties.Diffuse.Color = {9998183F00000000000000000000803F}
        Position.Coordinates = {000040C000000000000000000000803F}
        Visible = False
        CubeSize = {0000403F0000403F0000403F}
      end
      object GLSphere1: TGLSphere
        Material.FrontProperties.Ambient.Color = {00000000000000001283003F0000803F}
        Material.FrontProperties.Diffuse.Color = {00000000000000008180003F0000803F}
        Material.FrontProperties.Emission.Color = {00000000000000001283003F0000803F}
        Material.FrontProperties.Shininess = 16
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
        Material.FaceCulling = fcCull
        Visible = False
        Radius = 0.500000000000000000
      end
      object GLCone1: TGLCone
        Material.FrontProperties.Diffuse.Color = {0000803FA5A4243F000000000000803F}
        Position.Coordinates = {0000404000000000000000000000803F}
        Visible = False
        BottomRadius = 0.500000000000000000
        Height = 1.000000000000000000
      end
      object GLFreeForm1: TGLFreeForm
        Material.FrontProperties.Ambient.Color = {D3D2D23EE3E2E23EE3E2E23E0000803F}
        Material.FrontProperties.Diffuse.Color = {000000009B9A1A3F000000000000803F}
        Material.Texture.Disabled = False
        Direction.Coordinates = {000000000000803F0000000000000000}
        Position.Coordinates = {000000000000803F000000000000803F}
        Scale.Coordinates = {8FC2753C8FC2753C8FC2753C00000000}
        Up.Coordinates = {0000000000000000000080BF00000000}
        Visible = False
        AutoCentering = [macCenterX, macCenterY, macCenterZ, macUseBarycenter]
        UseMeshMaterials = False
      end
    end
    object DummyLight: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Light: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {0000000000002041000000000000803F}
        LightStyle = lsOmni
        Specular.Color = {0000803F0000803F0000803F0000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object GLHUDText1: TGLHUDText
      BitmapFont = GLWindowsBitmapFont1
      Text = 'Keyboard [ 1,2,3,4,5,6,7,8,9,0 ]'
      Rotation = 0.000000000000000000
    end
    object Cam: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 80.000000000000000000
      TargetObject = Scene
      Position.Coordinates = {00000000000000000000A0C00000803F}
      Direction.Coordinates = {00000000000000800000803F00000000}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 0.020000000000000000
    OnProgress = GLCadencer1Progress
    Left = 296
    Top = 40
  end
  object GLUserShader1: TGLUserShader
    OnDoApply = GLUserShader1DoApply
    OnDoUnApply = GLUserShader1DoUnApply
    ShaderStyle = ssHighLevel
    Left = 304
    Top = 120
  end
  object Timer1: TTimer
    Interval = 800
    OnTimer = Timer1Timer
    Left = 408
    Top = 40
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Left = 176
    Top = 120
  end
  object MatLib: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Brick01'
        Tag = 0
        Material.Texture.Disabled = False
        Shader = GLUserShader1
      end
      item
        Name = 'Brick02'
        Tag = 0
        Material.Texture.Disabled = False
        Shader = GLUserShader1
      end
      item
        Name = 'Brick03'
        Tag = 0
        Material.Texture.MinFilter = miLinear
        Material.Texture.Disabled = False
        Shader = GLUserShader1
      end>
    Left = 408
    Top = 120
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Simple GLSL Shader - %FPS'
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
    Left = 112
    Top = 264
  end
end
