object Form1: TForm1
  Left = 271
  Top = 212
  Caption = 'Cross Cursor'
  ClientHeight = 553
  ClientWidth = 782
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  TextHeight = 13
  object vp: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 782
    Height = 553
    Camera = Camera
    Buffer.ContextOptions = [roDoubleBuffer]
    FieldOfView = 149.647628784179700000
    PenAsTouch = False
    Align = alClient
    OnMouseMove = vpMouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object Player: TGLDummyCube
      ShowAxes = True
      CubeSize = 1.000000000000000000
      object Actor: TGLCone
        Direction.Coordinates = {00000000000080BF0000000000000000}
        PitchAngle = -90.000000000000000000
        Up.Coordinates = {00000000000000000000803F00000000}
        BottomRadius = 0.500000000000000000
        Height = 1.000000000000000000
      end
    end
    object PlaneTarget: TGLPlane
      Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
      Material.FrontProperties.Emission.Color = {0000803F00000000000000000000803F}
      Material.BlendingMode = bmTransparency
      Material.Texture.ImageClassName = 'TGLPicFileImage'
      Material.Texture.Image.PictureFileName = 'tp.tga'
      Material.Texture.MinFilter = miLinear
      Material.Texture.TextureMode = tmModulate
      Material.Texture.TextureWrap = twNone
      Material.Texture.TextureFormat = tfRGBA16
      Material.Texture.Disabled = False
      Direction.Coordinates = {000000000000803F0000000000000000}
      ShowAxes = True
      Up.Coordinates = {0000000000000000000080BF00000000}
      Height = 1.000000000000000000
      Width = 1.000000000000000000
    end
    object Camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 75.000000000000000000
      TargetObject = Player
      Position.Coordinates = {0000000000002041000080BF0000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    FixedDeltaTime = 0.025000000000000000
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 40
  end
end
