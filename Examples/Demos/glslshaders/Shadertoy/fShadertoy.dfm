object Form1: TForm1
  Left = 193
  Top = 127
  ClientHeight = 399
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 13
  object vp: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 640
    Height = 399
    Cursor = crHandPoint
    Camera = cam
    Buffer.BackgroundColor = 15000804
    Buffer.AmbientColor.Color = {0000000000000000000000000000803F}
    FieldOfView = 117.945869445800800000
    PenAsTouch = False
    Align = alClient
    OnClick = vpClick
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object dc_cam: TGLDummyCube
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {00000000000000000000803F0000803F}
      object cam: TGLCamera
        DepthOfView = 1000.000000000000000000
        FocalLength = 120.000000000000000000
        TargetObject = dc_cam
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
  object cad: TGLCadencer
    Scene = GLScene1
    Mode = cmApplicationIdle
    SleepLength = 1
    OnProgress = cadProgress
    Left = 40
    Top = 8
  end
  object at: TGLAsyncTimer
    Enabled = True
    OnTimer = atTimer
    Left = 72
    Top = 8
  end
end
