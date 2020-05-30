object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Portal'
  ClientHeight = 492
  ClientWidth = 749
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    749
    492)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 75
    Height = 18
    Caption = 'Maze Map'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 304
    Top = 8
    Width = 61
    Height = 18
    Caption = '3D View'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 24
    Top = 312
    Width = 241
    Height = 42
    Caption = 
      'To modify map, edit cells with keyboard :'#13#10'- any non-empty cell ' +
      'is a wall'#13#10'- click '#39'process'#39' to commit changes or check '#39'auto'#39
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 286
    Top = 32
    Width = 463
    Height = 456
    Camera = GLCamera1
    Buffer.FogEnvironment.FogColor.Color = {00000000000000008180003F0000803F}
    Buffer.FogEnvironment.FogStart = 1.000000000000000000
    Buffer.FogEnvironment.FogEnd = 10.000000000000000000
    Buffer.BackgroundColor = clNavy
    FieldOfView = 155.261871337890600000
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object BUForward: TButton
    Left = 96
    Top = 360
    Width = 89
    Height = 25
    Caption = 'Forward (Z/W)'
    TabOrder = 1
  end
  object BUTurnLeft: TButton
    Left = 8
    Top = 376
    Width = 81
    Height = 25
    Caption = 'Turn Left (Q/A)'
    TabOrder = 2
  end
  object BUTurnRight: TButton
    Left = 192
    Top = 376
    Width = 89
    Height = 25
    Caption = 'TurnRight (D)'
    TabOrder = 3
  end
  object BUBackward: TButton
    Left = 96
    Top = 407
    Width = 89
    Height = 25
    Caption = 'Backward (S)'
    TabOrder = 4
  end
  object SGMap: TStringGrid
    Left = 8
    Top = 32
    Width = 272
    Height = 272
    BorderStyle = bsNone
    ColCount = 16
    DefaultColWidth = 16
    DefaultRowHeight = 16
    FixedCols = 0
    RowCount = 16
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    ScrollBars = ssNone
    TabOrder = 5
  end
  object BBProcess: TButton
    Left = 200
    Top = 8
    Width = 75
    Height = 17
    Caption = 'Process'
    TabOrder = 6
    OnClick = BBProcessClick
  end
  object CBAuto: TCheckBox
    Left = 152
    Top = 8
    Width = 41
    Height = 17
    Caption = 'Auto'
    TabOrder = 7
  end
  object CBFog: TCheckBox
    Left = 472
    Top = 8
    Width = 49
    Height = 17
    Caption = 'Fog'
    TabOrder = 8
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
        Up.Coordinates = {000000800000803F0000000000000000}
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
    Left = 208
    Top = 56
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 0
    Left = 24
    Top = 120
  end
end
