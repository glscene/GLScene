object FormEiffie: TFormEiffie
  Left = 193
  Top = 127
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  ClientHeight = 698
  ClientWidth = 1134
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 168
  TextHeight = 24
  object SceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 1134
    Height = 698
    Cursor = crHandPoint
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = cam
    Buffer.BackgroundColor = 15000804
    Buffer.AmbientColor.Color = {0000000000000000000000000000803F}
    FieldOfView = 142.050003051757800000
    PenAsTouch = False
    Align = alClient
    OnClick = SceneViewerClick
    TabOrder = 0
  end
  object GLScene: TGLScene
    Left = 64
    Top = 8
    object dcCamera: TGLDummyCube
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {00000000000000000000803F0000803F}
      object cam: TGLCamera
        DepthOfView = 1000.000000000000000000
        FocalLength = 120.000000000000000000
        TargetObject = dcCamera
        Position.Coordinates = {000080400000C040000040400000803F}
      end
    end
    object dogl: TGLDirectOpenGL
      UseBuildList = False
      OnRender = doglRender
      Blend = False
      object hud: TGLHUDSprite
        Visible = False
        Rotation = 0.000000000000000000
      end
    end
  end
  object GLCadencer: TGLCadencer
    Scene = GLScene
    Mode = cmApplicationIdle
    SleepLength = 1
    OnProgress = GLCadencerProgress
    Left = 54
    Top = 134
  end
  object GLAsyncTimer: TGLAsyncTimer
    Enabled = True
    OnTimer = GLAsyncTimerTimer
    Left = 226
    Top = 8
  end
end
