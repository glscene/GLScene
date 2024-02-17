object FormActor: TFormActor
  Left = 145
  Top = 99
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Actor'
  ClientHeight = 852
  ClientWidth = 1195
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 168
  TextHeight = 24
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 67
    Width = 1195
    Height = 752
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = Camera
    Buffer.BackgroundColor = clSilver
    FieldOfView = 86.457061767578130000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 819
    Width = 1195
    Height = 33
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Panels = <
      item
        Width = 350
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1195
    Height = 67
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 2
    object SBPlay: TSpeedButton
      Left = 329
      Top = 14
      Width = 40
      Height = 42
      Hint = 'Play Actor'
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
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
      Left = 375
      Top = 14
      Width = 40
      Height = 42
      Hint = 'Stop Actor'
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
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
      Left = 420
      Top = 14
      Width = 40
      Height = 42
      Hint = 'Play Frame to Frame'
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
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
    object lblAnimation: TLabel
      Left = 14
      Top = 25
      Width = 94
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Animation :'
    end
    object lblDiskSlices: TLabel
      Left = 816
      Top = 21
      Width = 90
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Disk Slices'
    end
    object cbxAnimations: TComboBox
      Left = 112
      Top = 18
      Width = 191
      Height = 32
      Hint = 'Change State'
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Style = csDropDownList
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnChange = cbxAnimationsChange
    end
    object BBLoadWeapon: TBitBtn
      Left = 614
      Top = 14
      Width = 156
      Height = 42
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Load Weapon'
      TabOrder = 1
      OnClick = BBLoadWeaponClick
    end
    object chbSmooth: TCheckBox
      Left = 490
      Top = 19
      Width = 114
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Smooth'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = chbSmoothClick
    end
    object cbxDiskSlices: TComboBox
      Left = 945
      Top = 19
      Width = 163
      Height = 32
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 3
      OnChange = cbxDiskSlicesChange
      Items.Strings = (
        '3'
        '4'
        '5'
        '6'
        '12'
        '64')
    end
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 48
    object DummyCube: TGLDummyCube
      CubeSize = 1.000000000000000000
      object LightSource: TGLLightSource
        Ambient.Color = {0000803F0000803F0000803F0000803F}
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {0000204100000000000020410000803F}
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
      object Camera: TGLCamera
        DepthOfView = 1000.000000000000000000
        FocalLength = 400.000000000000000000
        TargetObject = DummyCube
        Position.Coordinates = {00009041000080410000C0400000803F}
        Direction.Coordinates = {2EF964BF2EF9E43E0000000000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
      end
      object DiskRing: TGLDisk
        Material.Texture.MinFilter = miLinear
        Position.Coordinates = {0000000000000000000080BF0000803F}
        InnerRadius = 2.000000000000000000
        Loops = 1
        OuterRadius = 5.000000000000000000
        Slices = 5
        SweepAngle = 360.000000000000000000
      end
    end
    object Actor1: TGLActor
      Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
      Material.Texture.MinFilter = miLinear
      Material.Texture.TextureMode = tmModulate
      Material.Texture.Disabled = False
      Interval = 100
      OnFrameChanged = Actor1FrameChanged
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
    OnProgress = GLCadencer1Progress
    Left = 96
    Top = 48
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 24
    Top = 112
  end
end
