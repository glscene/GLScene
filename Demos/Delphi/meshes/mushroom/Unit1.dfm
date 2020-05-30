object Form1: TForm1
  Left = 180
  Top = 93
  BorderWidth = 3
  Caption = 'Mushroom'
  ClientHeight = 295
  ClientWidth = 467
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  DesignSize = (
    467
    295)
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 467
    Height = 295
    Camera = GLCamera1
    Buffer.FogEnvironment.FogColor.Color = {CECD4D3FCECD4D3FCECD4D3F0000803F}
    Buffer.FogEnvironment.FogStart = 30.000000000000000000
    Buffer.FogEnvironment.FogEnd = 90.000000000000000000
    Buffer.BackgroundColor = 13487565
    Buffer.FogEnable = True
    FieldOfView = 142.548431396484400000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Button1: TButton
    Left = 144
    Top = 277
    Width = 137
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'I want more mushrooms !'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = Button1Click
  end
  object GLScene1: TGLScene
    VisibilityCulling = vcObjectBased
    Left = 16
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000FA4400409C4500007A450000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Disk1: TGLDisk
        Material.FrontProperties.Diffuse.Color = {938C0C3E938E0E3F938C0C3E0000803F}
        Material.FrontProperties.Emission.Color = {0000803E0000803E0000803E0000803F}
        Material.Texture.Image.Picture.Data = {07544269746D617000000000}
        Material.Texture.ImageAlpha = tiaOpaque
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmReplace
        Material.Texture.Disabled = False
        Direction.Coordinates = {000000000000803F2EBD3BB300000000}
        Up.Coordinates = {000000002EBD3BB3000080BF00000000}
        Loops = 3
        OuterRadius = 75.000000000000000000
        Slices = 9
        SweepAngle = 360.000000000000000000
      end
      object FreeForm1: TGLFreeForm
        Material.FrontProperties.Diffuse.Color = {0AD7633FD7A3F03ECDCC4C3E0000803F}
        Direction.Coordinates = {000000000000803F0000000000000000}
        Position.Coordinates = {0000000000004040000000000000803F}
        Scale.Coordinates = {CDCCCC3DCDCCCC3DCDCCCC3D00000000}
        Up.Coordinates = {0000803F000000000000008000000000}
        AutoCentering = [macCenterX, macCenterY, macCenterZ]
        UseMeshMaterials = False
        NormalsOrientation = mnoInvert
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 500.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {0000A041000040410000F0410000803F}
      Left = 200
      Top = 104
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 248
    Top = 16
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 96
    Top = 16
  end
end
