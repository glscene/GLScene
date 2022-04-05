object fRagDoll: TfRagDoll
  Left = 0
  Top = 0
  Caption = 'RagDoll'
  ClientHeight = 512
  ClientWidth = 682
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 682
    Height = 512
    Camera = GLCamera1
    Buffer.BackgroundColor = 15648684
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    FieldOfView = 147.342132568359400000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 24
    object ODEScene: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Actor1: TGLActor
        Material.FrontProperties.Diffuse.Color = {0000803FE0DF5F3FB5B4343E0000803F}
        AnimationMode = aamLoop
        Interval = 100
        MaterialLibrary = GLMaterialLibrary1
      end
      object Targetrag: TGLDummyCube
        Position.Coordinates = {00000000000000000000C0400000803F}
        CubeSize = 1.000000000000000000
      end
      object GLShadowPlane1: TGLShadowPlane
        Material.FrontProperties.Diffuse.Color = {F5F4743E9392123F9F9E9E3E0000803F}
        Position.Coordinates = {0000000000000000CDCC4CBD0000803F}
        Height = 100.000000000000000000
        Width = 100.000000000000000000
        XTiles = 50
        YTiles = 50
        Style = [psTileTexture]
        ShadowingObject = ODEScene
        ShadowedLight = GLLightSource1
        ShadowColor.Color = {0AD7A33E48E1FA3E1F85EB3E0000803E}
      end
    end
    object GLLightSource1: TGLLightSource
      Ambient.Color = {BEC0403FBEC0403FBEC0403F0000803F}
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00004041000048C2000048420000803F}
      LightStyle = lsOmni
      Specular.Color = {0000803F0000803F0000803F0000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLHUDText1: TGLHUDText
      BitmapFont = GLWindowsBitmapFont1
      Text = 
        'Keys: Q,W,E,A,S,D -> Apply forces  | X,C -> Start, stop ragdoll ' +
        '| F1,F2, F3 -> Switch animation | Return -> Explode'
      Rotation = 0.000000000000000000
      ModulateColor.Color = {0000000000000000000000000000803F}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 75.000000000000000000
      TargetObject = Targetrag
      Position.Coordinates = {0000B8C10000B8C1000080410000803F}
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 0.020000000000000000
    FixedDeltaTime = 0.016900000000000000
    SleepLength = 1
    Left = 24
    Top = 72
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Left = 120
    Top = 72
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 120
    Top = 24
  end
end
