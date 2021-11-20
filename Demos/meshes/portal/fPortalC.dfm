object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Portal'
  ClientHeight = 615
  ClientWidth = 936
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 120
  DesignSize = (
    936
    615)
  TextHeight = 17
  object Label1: TLabel
    Left = 10
    Top = 10
    Width = 97
    Height = 24
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Maze Map'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 380
    Top = 10
    Width = 78
    Height = 24
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '3D View'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 30
    Top = 390
    Width = 314
    Height = 48
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 
      'To modify map, edit cells with keyboard :'#13#10'- any non-empty cell ' +
      'is a wall'#13#10'- click '#39'process'#39' to commit changes or check '#39'auto'#39
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 358
    Top = 40
    Width = 578
    Height = 570
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.FogEnvironment.FogColor.Color = {00000000000000008180003F0000803F}
    Buffer.FogEnvironment.FogStart = 1.000000000000000000
    Buffer.FogEnvironment.FogEnd = 10.000000000000000000
    Buffer.BackgroundColor = clNavy
    FieldOfView = 160.098739624023400000
    PenAsTouch = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object BUForward: TButton
    Left = 120
    Top = 450
    Width = 111
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Forward (Z/W)'
    TabOrder = 1
  end
  object BUTurnLeft: TButton
    Left = 10
    Top = 470
    Width = 101
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Turn Left (Q/A)'
    TabOrder = 2
  end
  object BUTurnRight: TButton
    Left = 240
    Top = 470
    Width = 111
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'TurnRight (D)'
    TabOrder = 3
  end
  object BUBackward: TButton
    Left = 120
    Top = 509
    Width = 111
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Backward (S)'
    TabOrder = 4
  end
  object SGMap: TStringGrid
    Left = 10
    Top = 40
    Width = 340
    Height = 340
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    BorderStyle = bsNone
    ColCount = 16
    DefaultColWidth = 20
    DefaultRowHeight = 20
    FixedCols = 0
    RowCount = 16
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    ScrollBars = ssNone
    TabOrder = 5
  end
  object BBProcess: TButton
    Left = 250
    Top = 10
    Width = 94
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Process'
    TabOrder = 6
    OnClick = BBProcessClick
  end
  object CBAuto: TCheckBox
    Left = 190
    Top = 10
    Width = 51
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Auto'
    TabOrder = 7
  end
  object CBFog: TCheckBox
    Left = 590
    Top = 10
    Width = 61
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
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
