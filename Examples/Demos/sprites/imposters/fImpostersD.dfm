object FormImposters: TFormImposters
  Left = 196
  Top = 109
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  ClientHeight = 859
  ClientWidth = 1229
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 168
  TextHeight = 24
  object vp: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 1229
    Height = 859
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = Camera
    Buffer.BackgroundColor = 15000804
    Buffer.AmbientColor.Color = {0000000000000000000000000000803F}
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow]
    FieldOfView = 148.779846191406300000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 8
    object dcWorld: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object dcCamera: TGLDummyCube
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {00000000000000000000803F0000803F}
      object Camera: TGLCamera
        DepthOfView = 1000.000000000000000000
        FocalLength = 120.000000000000000000
        TargetObject = dcCamera
        Position.Coordinates = {000080400000C040000040400000803F}
      end
    end
    object dirOGL: TGLDirectOpenGL
      UseBuildList = False
      OnRender = dirOGLRender
      Blend = False
      object ff: TGLFreeForm
        Material.BlendingMode = bmAlphaTest50
        Material.MaterialOptions = [moNoLighting]
        Material.Texture.ImageClassName = 'TGLCompositeImage'
        Material.Texture.Image.Width = 256
        Material.Texture.Image.Height = 256
        Material.Texture.Image.Depth = 0
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
        Material.FaceCulling = fcNoCull
        Visible = False
      end
    end
  end
  object Сadencer: TGLCadencer
    Scene = GLScene1
    OnProgress = СadencerProgress
    Left = 120
    Top = 8
  end
  object GLAsyncTimer1: TGLAsyncTimer
    Enabled = True
    Interval = 800
    OnTimer = GLAsyncTimer1Timer
    ThreadPriority = tpIdle
    Left = 24
    Top = 80
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 248
    Top = 8
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = vp
    FormCaption = ' - %FPS'
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
    Top = 102
  end
end
