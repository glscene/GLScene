object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Actor'
  ClientHeight = 439
  ClientWidth = 573
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 38
    Width = 573
    Height = 382
    Camera = GLCamera1
    Buffer.BackgroundColor = clSilver
    FieldOfView = 51.048950195312500000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 420
    Width = 573
    Height = 19
    Panels = <
      item
        Width = 200
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 573
    Height = 38
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 2
    object SBPlay: TSpeedButton
      Left = 188
      Top = 8
      Width = 23
      Height = 24
      Hint = 'Play Actor'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33333333333333333333EEEEEEEEEEEEEEE333FFFFFFFFFFFFF3E00000000000
        00E337777777777777F3E0F77777777770E337F33333333337F3E0F333333333
        70E337F3333F333337F3E0F33303333370E337F3337FF33337F3E0F333003333
        70E337F33377FF3337F3E0F33300033370E337F333777FF337F3E0F333000033
        70E337F33377773337F3E0F33300033370E337F33377733337F3E0F333003333
        70E337F33377333337F3E0F33303333370E337F33373333337F3E0F333333333
        70E337F33333333337F3E0FFFFFFFFFFF0E337FFFFFFFFFFF7F3E00000000000
        00E33777777777777733EEEEEEEEEEEEEEE33333333333333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = SBPlayClick
    end
    object SBStop: TSpeedButton
      Left = 214
      Top = 8
      Width = 23
      Height = 24
      Hint = 'Stop Actor'
      Enabled = False
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33333333333333333333EEEEEEEEEEEEEEE333FFFFFFFFFFFFF3E00000000000
        00E337777777777777F3E0F77777777770E337F33333333337F3E0F333333333
        70E337F33333333337F3E0F33333333370E337F333FFFFF337F3E0F330000033
        70E337F3377777F337F3E0F33000003370E337F3377777F337F3E0F330000033
        70E337F3377777F337F3E0F33000003370E337F3377777F337F3E0F330000033
        70E337F33777773337F3E0F33333333370E337F33333333337F3E0F333333333
        70E337F33333333337F3E0FFFFFFFFFFF0E337FFFFFFFFFFF7F3E00000000000
        00E33777777777777733EEEEEEEEEEEEEEE33333333333333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = SBStopClick
    end
    object SBFrameToFrame: TSpeedButton
      Left = 240
      Top = 8
      Width = 23
      Height = 24
      Hint = 'Play Frame to Frame'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33333333333333333333EEEEEEEEEEEEEEE333FFFFFFFFFFFFF3E00000000000
        00E337777777777777F3E0F77777777770E337F33333333337F3E0F333333333
        70E337F33333333337F3E0F33333333370E337F333FF3F3337F3E0F330030333
        70E337F3377F7FF337F3E0F33003003370E337F3377F77FF37F3E0F330030003
        70E337F3377F777337F3E0F33003003370E337F3377F773337F3E0F330030333
        70E337F33773733337F3E0F33333333370E337F33333333337F3E0F333333333
        70E337F33333333337F3E0FFFFFFFFFFF0E337FFFFFFFFFFF7F3E00000000000
        00E33777777777777733EEEEEEEEEEEEEEE33333333333333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = SBFrameToFrameClick
    end
    object Label1: TLabel
      Left = 8
      Top = 14
      Width = 54
      Height = 13
      Caption = 'Animation :'
    end
    object LabelFPS: TLabel
      Left = 460
      Top = 12
      Width = 18
      Height = 13
      Caption = 'FPS'
    end
    object CBAnimations: TComboBox
      Left = 64
      Top = 10
      Width = 109
      Height = 21
      Hint = 'Change State'
      Style = csDropDownList
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnChange = CBAnimationsChange
    end
    object BBLoadWeapon: TBitBtn
      Left = 351
      Top = 8
      Width = 89
      Height = 24
      Caption = 'Load Weapon'
      TabOrder = 1
      OnClick = BBLoadWeaponClick
    end
    object CBSmooth: TCheckBox
      Left = 280
      Top = 11
      Width = 65
      Height = 17
      Caption = 'Smooth'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 48
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLLightSource1: TGLLightSource
        Ambient.Color = {0000803F0000803F0000803F0000803F}
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {0000204100000000000020410000803F}
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
      object GLCamera1: TGLCamera
        DepthOfView = 1000.000000000000000000
        FocalLength = 400.000000000000000000
        TargetObject = DummyCube1
        Position.Coordinates = {00009041000080410000C0400000803F}
        Direction.Coordinates = {2EF964BF2EF9E43E0000000000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
      end
      object Disk1: TGLDisk
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Material.Texture.Image.PictureFileName = '..\\..\\..\\..\\media\\clover.jpg'
        Material.Texture.MinFilter = miLinear
        Material.Texture.Disabled = False
        Position.Coordinates = {0000000000000000000080BF0000803F}
        Loops = 1
        OuterRadius = 3.000000000000000000
        Slices = 8
        SweepAngle = 360.000000000000000000
      end
    end
    object Actor1: TGLActor
      Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
      Material.Texture.MinFilter = miLinear
      Material.Texture.TextureMode = tmModulate
      Material.Texture.Disabled = False
      Interval = 100
      object Actor2: TGLActor
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.MinFilter = miLinear
        Material.Texture.Disabled = False
        Interval = 100
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 0
    Left = 96
    Top = 48
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 24
    Top = 112
  end
end
