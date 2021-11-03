object FormTorqueC: TFormTorqueC
  Left = 125
  Top = 119
  BorderWidth = 5
  Caption = 'Torque'
  ClientHeight = 509
  ClientWidth = 939
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 98
    Width = 939
    Height = 360
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    FieldOfView = 121.890792846679700000
    PenAsTouch = False
    Align = alClient
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 939
    Height = 98
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    TabOrder = 1
    object lHexahedron: TLabel
      Left = 350
      Top = 10
      Width = 145
      Height = 48
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Alignment = taCenter
      Caption = 'Hexahedron has a small constant'#13#10'and linear damping'
      WordWrap = True
    end
    object lDodecahedron: TLabel
      Left = 590
      Top = 10
      Width = 119
      Height = 64
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Alignment = taCenter
      Caption = 'Dodecahedron has a'#13#10'small constant and'#13#10'quadratic damping'
      WordWrap = True
    end
    object lOctagedron: TLabel
      Left = 160
      Top = 10
      Width = 106
      Height = 48
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Alignment = taCenter
      Caption = 'Octahedron has a'#13#10'only quadratic damping'
      WordWrap = True
    end
    object lTetrahedron: TLabel
      Left = 10
      Top = 13
      Width = 105
      Height = 48
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Tetrahedron has an only quadratic damping'
      WordWrap = True
    end
    object lIcosahedron: TLabel
      Left = 755
      Top = 10
      Width = 146
      Height = 48
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Alignment = taCenter
      Caption = 'Icosahedron has a small constant'#13#10'and linear damping'
      WordWrap = True
    end
    object CheckBox1: TCheckBox
      Left = 10
      Top = 69
      Width = 101
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Double Mass'
      TabOrder = 0
      OnClick = CheckBox1Click
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 458
    Width = 939
    Height = 51
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    Caption = 
      'Move your mouse over an object and it will start spinning. Click' +
      ' to get info.'
    TabOrder = 2
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 80
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 100.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {000020410000A040000000000000803F}
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000704100002041000020C10000803F}
      SpotCutOff = 180.000000000000000000
    end
    object HUDText: TGLHUDText
      Position.Coordinates = {00009643000048430000F0410000803F}
      BitmapFont = GLBitmapFont1
      Text = 'Hello'
      Rotation = 0.000000000000000000
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Tetrahedron: TGLTetrahedron
        Material.FrontProperties.Diffuse.Color = {BEBEBE3E999F1F3F999F1F3F0000803F}
        Material.BlendingMode = bmTransparency
        Direction.Coordinates = {2EF9E43E000000002EF9643F00000000}
        Position.Coordinates = {0000000000000000000080400000803F}
        Scale.Coordinates = {0000003F0000003F0000003F00000000}
      end
      object Octahedron: TGLOctahedron
        Material.FrontProperties.Diffuse.Color = {CDCC4C3FF8FEFE3EACC8483E0000803F}
        Position.Coordinates = {0000000000000000000000400000803F}
        Scale.Coordinates = {CDCC4C3FCDCC4C3FCDCC4C3F00000000}
      end
      object Cube: TGLCube
        Material.FrontProperties.Diffuse.Color = {F8FEFE3E0000803F000000000000803F}
        Direction.Coordinates = {0000003F00000000D7B35D3F00000000}
        TurnAngle = 30.000000000000000000
      end
      object Hexahedron: TGLHexahedron
        Material.FrontProperties.Diffuse.Color = {9A99593F9A99593FCDCCCC3D0000803F}
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Direction.Coordinates = {0000003F00000000D7B35D3F00000000}
        Position.Coordinates = {0000A0C000000000333333C00000803F}
        Scale.Coordinates = {0000003F0000003F0000003F00000000}
        TurnAngle = 30.000000000000000000
      end
      object Dodecahedron: TGLDodecahedron
        Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
        Position.Coordinates = {0000000000000000000000C00000803F}
        Scale.Coordinates = {9A99993F9A99993F9A99993F00000000}
      end
      object Icosahedron: TGLIcosahedron
        Material.FrontProperties.Diffuse.Color = {14AE073F8FC2F53DD7A3F03E0000803F}
        Position.Coordinates = {0000000000000000000080C00000803F}
        Scale.Coordinates = {9A99993F9A99993F9A99993F00000000}
      end
      object Teapot: TGLTeapot
        Material.FrontProperties.Diffuse.Color = {1F856B3F14AE473F52B81E3F0000803F}
        Direction.Coordinates = {010000BF00000000D7B35D3F00000000}
        Position.Coordinates = {0000404000000000000000000000803F}
        Scale.Coordinates = {0000C03F0000C03F0000C03F00000000}
        TurnAngle = -30.000000000000000000
      end
      object Torus: TGLTorus
        Material.FrontProperties.Diffuse.Color = {85EB513F85EB113F1F85EB3E0000803F}
        Material.FaceCulling = fcCull
        Position.Coordinates = {0000C03F00000040CDCCCC3F0000803F}
        MajorRadius = 0.400000005960464500
        MinorRadius = 0.100000001490116100
        StopAngle = 360.000000000000000000
        Parts = [toSides, toStartDisk, toStopDisk]
      end
      object Superellipsoid: TGLSuperellipsoid
        Material.FrontProperties.Diffuse.Color = {0000803F0000003F000000000000803F}
        Material.Texture.ImageClassName = 'TGLPicFileImage'
        Position.Coordinates = {000090C000000000000000000000803F}
        Scale.Coordinates = {0000C03F0000C03F0000C03F00000000}
        Radius = 0.500000000000000000
        VCurve = 1.000000000000000000
        HCurve = 1.000000000000000000
        Slices = 8
        Stacks = 8
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 88
    Top = 80
  end
  object GLBitmapFont1: TGLBitmapFont
    GlyphsIntervalX = 1
    GlyphsIntervalY = 1
    Ranges = <>
    CharWidth = 30
    CharHeight = 30
    HSpace = 3
    VSpace = 6
    Left = 184
    Top = 80
  end
end
