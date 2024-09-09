object FormPortal: TFormPortal
  Left = 74
  Top = 84
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Portal'
  ClientHeight = 924
  ClientWidth = 1432
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 168
  DesignSize = (
    1432
    924)
  TextHeight = 24
  object Label1: TLabel
    Left = 14
    Top = 14
    Width = 135
    Height = 32
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Maze Map'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -28
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 532
    Top = 14
    Width = 108
    Height = 32
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '3D View'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -28
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 42
    Top = 546
    Width = 421
    Height = 66
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 
      'To modify map, edit cells with keyboard :'#13#10'- any non-empty cell ' +
      'is a wall'#13#10'- click '#39'process'#39' to commit changes or check '#39'auto'#39
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 518
    Top = 56
    Width = 914
    Height = 912
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.FogEnvironment.FogColor.Color = {00000000000000008180003F0000803F}
    Buffer.FogEnvironment.FogStart = 1.000000000000000000
    Buffer.FogEnvironment.FogEnd = 10.000000000000000000
    Buffer.BackgroundColor = clNavy
    FieldOfView = 167.485137939453100000
    PenAsTouch = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseDown = GLSceneViewer1MouseDown
    TabOrder = 0
  end
  object BUForward: TButton
    Left = 168
    Top = 630
    Width = 156
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Forward (Z/W)'
    TabOrder = 1
    OnClick = BUForwardClick
  end
  object BUTurnLeft: TButton
    Left = 14
    Top = 658
    Width = 142
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Turn Left (Q/A)'
    TabOrder = 2
    OnClick = BUTurnLeftClick
  end
  object BUTurnRight: TButton
    Left = 336
    Top = 658
    Width = 156
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'TurnRight (D)'
    TabOrder = 3
    OnClick = BUTurnRightClick
  end
  object BUBackward: TButton
    Left = 168
    Top = 686
    Width = 156
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Backward (S)'
    TabOrder = 4
    OnClick = BUBackwardClick
  end
  object SGMap: TStringGrid
    Left = 14
    Top = 56
    Width = 476
    Height = 476
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    BorderStyle = bsNone
    ColCount = 16
    DefaultColWidth = 28
    DefaultRowHeight = 28
    FixedCols = 0
    RowCount = 16
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    ScrollBars = ssNone
    TabOrder = 5
    OnSetEditText = SGMapSetEditText
    ColWidths = (
      28
      28
      28
      28
      28
      28
      28
      28
      28
      28
      28
      28
      28
      28
      28
      28)
    RowHeights = (
      28
      28
      28
      28
      28
      28
      28
      28
      28
      28
      28
      28
      28
      28
      28
      28)
  end
  object BBProcess: TButton
    Left = 350
    Top = 14
    Width = 131
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Process'
    TabOrder = 6
    OnClick = BBProcessClick
  end
  object CBAuto: TCheckBox
    Left = 266
    Top = 14
    Width = 72
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Auto'
    TabOrder = 7
  end
  object CBFog: TCheckBox
    Left = 1120
    Top = 14
    Width = 86
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Fog'
    TabOrder = 8
    OnClick = CBFogClick
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 56
    object GLLightSource1: TGLLightSource
      Ambient.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000048420000C8420000C8420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DummyCube1: TGLDummyCube
      Position.Coordinates = {00000000000000000000C0400000803F}
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        Position.Coordinates = {000000000000003F000000000000803F}
        Left = 264
        Top = 144
      end
    end
    object Portal1: TGLPortal
      Position.Coordinates = {00000000000000BF000000000000803F}
      MaterialLibrary = GLMaterialLibrary1
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 120
    Top = 56
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 208
    Top = 56
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 0
    OnProgress = GLCadencer1Progress
    Left = 24
    Top = 120
  end
end
