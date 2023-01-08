object Form1: TForm1
  Left = 216
  Top = 3
  Caption = 'Fountain Particles'
  ClientHeight = 645
  ClientWidth = 886
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  Position = poScreenCenter
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 678
    Height = 626
    Camera = Cam
    VSync = vsmSync
    Buffer.FogEnvironment.FogStart = 25.000000000000000000
    Buffer.FogEnvironment.FogEnd = 25.000000000000000000
    Buffer.BackgroundColor = clBlack
    Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
    FieldOfView = 147.915893554687500000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 678
    Top = 0
    Width = 208
    Height = 626
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object Panel2: TPanel
      Left = 0
      Top = 545
      Width = 208
      Height = 81
      Align = alBottom
      TabOrder = 0
      object Label1: TLabel
        Left = 6
        Top = 6
        Width = 61
        Height = 13
        Caption = 'Color Start'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label2: TLabel
        Left = 96
        Top = 6
        Width = 56
        Height = 13
        Caption = 'Color End'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label3: TLabel
        Left = 6
        Top = 62
        Width = 71
        Height = 13
        Caption = 'BackGround'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object PStartColor: TPanel
        Left = 8
        Top = 24
        Width = 73
        Height = 17
        Color = clRed
        Ctl3D = True
        DoubleBuffered = False
        ParentBackground = False
        ParentCtl3D = False
        ParentDoubleBuffered = False
        TabOrder = 0
        OnClick = PStartColorClick
      end
      object PEndColor: TPanel
        Left = 96
        Top = 24
        Width = 73
        Height = 17
        Color = clYellow
        ParentBackground = False
        TabOrder = 1
        OnClick = PEndColorClick
      end
      object PBackColor: TPanel
        Left = 96
        Top = 58
        Width = 73
        Height = 17
        Color = clBlack
        ParentBackground = False
        TabOrder = 2
        OnClick = PBackColorClick
      end
    end
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 208
      Height = 545
      ActivePage = TabSheet1
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      object TabSheet1: TTabSheet
        Caption = 'Fontaine Setting'
        object Label14: TLabel
          Left = 8
          Top = 472
          Width = 105
          Height = 13
          Caption = 'Particles Size Max'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label13: TLabel
          Left = 8
          Top = 432
          Width = 102
          Height = 13
          Caption = 'Particles Size Min'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label12: TLabel
          Left = 8
          Top = 392
          Width = 74
          Height = 13
          Caption = 'Times Factor'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label11: TLabel
          Left = 8
          Top = 352
          Width = 62
          Height = 13
          Caption = 'Life Factor'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label10: TLabel
          Left = 8
          Top = 312
          Width = 64
          Height = 13
          Caption = 'Angle Start'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label9: TLabel
          Left = 8
          Top = 272
          Width = 73
          Height = 13
          Caption = 'Velocity Max'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label8: TLabel
          Left = 8
          Top = 232
          Width = 70
          Height = 13
          Caption = 'Velocity Min'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label7: TLabel
          Left = 8
          Top = 192
          Width = 77
          Height = 13
          Caption = 'Max Particles'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label6: TLabel
          Left = 8
          Top = 152
          Width = 29
          Height = 13
          Caption = 'Floor'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label5: TLabel
          Left = 8
          Top = 112
          Width = 94
          Height = 13
          Caption = 'Bounding Factor'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label4: TLabel
          Left = 8
          Top = 72
          Width = 77
          Height = 13
          Caption = 'Particle Mass'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label15: TLabel
          Left = 8
          Top = 32
          Width = 33
          Height = 13
          Caption = 'Scale'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object EdPSizeMax: TEdit
          Left = 8
          Top = 488
          Width = 129
          Height = 21
          TabOrder = 0
          Text = '130'
          OnChange = EdPSizeMaxChange
        end
        object EdPSizeMin: TEdit
          Left = 8
          Top = 448
          Width = 129
          Height = 21
          TabOrder = 1
          Text = '110'
          OnChange = EdPSizeMinChange
        end
        object EdTimesFact: TEdit
          Left = 8
          Top = 408
          Width = 129
          Height = 21
          TabOrder = 2
          Text = '0.00005'
          OnChange = EdTimesFactChange
        end
        object EdLifeFact: TEdit
          Left = 8
          Top = 368
          Width = 129
          Height = 21
          TabOrder = 3
          Text = '0.025'
          OnChange = EdLifeFactChange
        end
        object EdAngleStart: TEdit
          Left = 8
          Top = 328
          Width = 129
          Height = 21
          TabOrder = 4
          Text = '360'
          OnChange = EdAngleStartChange
        end
        object EdVelMax: TEdit
          Left = 8
          Top = 288
          Width = 129
          Height = 21
          TabOrder = 5
          Text = '15'
          OnChange = EdVelMaxChange
        end
        object EdVelMin: TEdit
          Left = 8
          Top = 248
          Width = 129
          Height = 21
          TabOrder = 6
          Text = '5'
          OnChange = EdVelMinChange
        end
        object EdMaxP: TEdit
          Left = 8
          Top = 208
          Width = 129
          Height = 21
          TabOrder = 7
          Text = '60'
          OnChange = EdMaxPChange
        end
        object EdFloor: TEdit
          Left = 8
          Top = 168
          Width = 129
          Height = 21
          TabOrder = 8
          Text = '0.0'
          OnChange = EdFloorChange
        end
        object EdBound: TEdit
          Left = 8
          Top = 128
          Width = 129
          Height = 21
          TabOrder = 9
          Text = '100.0'
          OnChange = EdBoundChange
        end
        object EdMass: TEdit
          Left = 8
          Top = 88
          Width = 129
          Height = 21
          TabOrder = 10
          Text = '5.0'
          OnChange = EdMassChange
        end
        object CheckActived: TCheckBox
          Left = 0
          Top = 8
          Width = 65
          Height = 17
          Caption = 'Actived'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          State = cbChecked
          TabOrder = 11
          OnClick = CheckActivedClick
        end
        object CheckBound: TCheckBox
          Left = 72
          Top = 8
          Width = 73
          Height = 17
          Caption = 'Bounding'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 12
          OnClick = CheckBoundClick
        end
        object TrackBar1: TTrackBar
          Left = 0
          Top = 48
          Width = 150
          Height = 15
          Min = 1
          PageSize = 1
          Position = 2
          TabOrder = 13
          ThumbLength = 12
          OnChange = TrackBar1Change
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Fontaine Style'
        ImageIndex = 1
        object Panel3: TPanel
          Left = 0
          Top = 0
          Width = 200
          Height = 517
          Align = alClient
          TabOrder = 0
          ExplicitHeight = 513
          object RadioButton1: TRadioButton
            Left = 8
            Top = 16
            Width = 73
            Height = 17
            Caption = 'Style Fire'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = RadioButtonClick
          end
          object RadioButton2: TRadioButton
            Left = 8
            Top = 40
            Width = 81
            Height = 17
            Caption = 'Style Water'
            TabOrder = 1
            OnClick = RadioButtonClick
          end
          object RadioButton3: TRadioButton
            Left = 8
            Top = 64
            Width = 73
            Height = 17
            Caption = 'Style Ball'
            TabOrder = 2
            OnClick = RadioButtonClick
          end
        end
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 626
    Width = 886
    Height = 19
    Panels = <>
  end
  object GLScene1: TGLScene
    Left = 32
    Top = 36
    object Scene: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLTorus1: TGLTorus
        Material.MaterialLibrary = GLMatlibColors
        Material.LibMaterialName = 'G'
        MajorRadius = 1.500000000000000000
        MinorRadius = 0.250000000000000000
        StopAngle = 360.000000000000000000
        Parts = [toSides, toStartDisk, toStopDisk]
      end
      object GLPlane1: TGLPlane
        Material.MaterialLibrary = GLMatlibColors
        Material.LibMaterialName = 'B'
        Position.Coordinates = {0000000000000000000080C00000803F}
        Height = 20.000000000000000000
        Width = 20.000000000000000000
      end
    end
    object Light: TGLLightSource
      Ambient.Color = {BEC0403FBEC0403FBEC0403F0000803F}
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000000000000000000020410000803F}
      LightStyle = lsOmni
      Specular.Color = {BEC0403FBEC0403FBEC0403F0000803F}
      SpotCutOff = 180.000000000000000000
      SpotDirection.Coordinates = {000000000000803F0000000000000000}
    end
    object Cam: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 90.000000000000000000
      TargetObject = Scene
      Position.Coordinates = {0000000000007041000040400000803F}
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    MaxDeltaTime = 0.020000000000000000
    FixedDeltaTime = 0.016500000000000000
    SleepLength = 0
    OnProgress = GLCadencer1Progress
    Left = 24
    Top = 124
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 360
    Top = 52
  end
  object ColorDialog1: TColorDialog
    Left = 172
    Top = 288
  end
  object MainMenu1: TMainMenu
    Left = 226
    Top = 44
    object File1: TMenuItem
      Caption = 'File'
      object Texture1: TMenuItem
        Caption = 'Texture...'
        OnClick = Texture1Click
      end
      object Close1: TMenuItem
        Caption = 'Close'
        OnClick = Close1Click
      end
    end
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 226
    Top = 124
  end
  object GLMatlibColors: TGLMaterialLibrary
    Materials = <
      item
        Name = 'C'
        Tag = 0
        Material.BackProperties.Ambient.Color = {0000803F00000000000000000000803F}
        Material.BackProperties.Diffuse.Color = {0000803F00000000000000000000803F}
        Material.BackProperties.Emission.Color = {0000803F00000000000000000000803F}
        Material.BackProperties.Specular.Color = {0000803F00000000000000000000803F}
        Material.FrontProperties.Ambient.Color = {0000803F00000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
        Material.FrontProperties.Emission.Color = {0000803F00000000000000000000803F}
        Material.FrontProperties.Specular.Color = {0000803F00000000000000000000803F}
      end
      item
        Name = 'Csh'
        Tag = 0
        Material.BackProperties.Ambient.Color = {0000803F0000803E000000000000803F}
        Material.BackProperties.Diffuse.Color = {0000803F0000803E000000000000803F}
        Material.BackProperties.Emission.Color = {0000803F0000803E000000000000803F}
        Material.BackProperties.Specular.Color = {0000803F0000803E000000000000803F}
        Material.FrontProperties.Emission.Color = {CDCC0C3FEC51B83DEC51B83D0000803F}
      end
      item
        Name = 'D'
        Tag = 0
        Material.BackProperties.Ambient.Color = {0000803F0000003F000000000000803F}
        Material.BackProperties.Diffuse.Color = {0000803F0000003F000000000000803F}
        Material.BackProperties.Emission.Color = {0000803F0000003F000000000000803F}
        Material.BackProperties.Specular.Color = {0000803F0000003F000000000000803F}
        Material.FrontProperties.Ambient.Color = {0000803F0000003F000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000003F000000000000803F}
        Material.FrontProperties.Emission.Color = {0000803F0000003F000000000000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000003F000000000000803F}
      end
      item
        Name = 'Dsh'
        Tag = 0
        Material.BackProperties.Ambient.Color = {8FC2353F6666263FB81E853E0000803F}
        Material.BackProperties.Diffuse.Color = {8FC2353F6666263FB81E853E0000803F}
        Material.BackProperties.Emission.Color = {8FC2353F6666263FB81E853E0000803F}
        Material.BackProperties.Specular.Color = {8FC2353F6666263FB81E853E0000803F}
        Material.FrontProperties.Emission.Color = {0AD7633FD7A3F03ECDCC4C3E0000803F}
      end
      item
        Name = 'E'
        Tag = 0
        Material.BackProperties.Ambient.Color = {0000803F0000803F000000000000803F}
        Material.BackProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
        Material.BackProperties.Emission.Color = {0000803F0000803F000000000000803F}
        Material.BackProperties.Specular.Color = {0000803F0000803F000000000000803F}
        Material.FrontProperties.Ambient.Color = {0000803F0000803F000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F000000000000803F}
        Material.FrontProperties.Emission.Color = {0000803F0000803F000000000000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F000000000000803F}
      end
      item
        Name = 'F'
        Tag = 0
        Material.BackProperties.Ambient.Color = {000000000000003F000000000000803F}
        Material.BackProperties.Diffuse.Color = {000000000000003F000000000000803F}
        Material.BackProperties.Emission.Color = {000000000000003F000000000000803F}
        Material.BackProperties.Specular.Color = {000000000000003F000000000000803F}
        Material.FrontProperties.Ambient.Color = {000000000000003F000000000000803F}
        Material.FrontProperties.Diffuse.Color = {000000000000003F000000000000803F}
        Material.FrontProperties.Emission.Color = {000000000000003F000000000000803F}
        Material.FrontProperties.Specular.Color = {000000000000003F000000000000803F}
      end
      item
        Name = 'Fsh'
        Tag = 0
        Material.BackProperties.Ambient.Color = {000000000000003F0000003F0000803F}
        Material.BackProperties.Diffuse.Color = {000000000000003F0000003F0000803F}
        Material.BackProperties.Emission.Color = {000000000000003F0000003F0000803F}
        Material.BackProperties.Specular.Color = {000000000000003F0000003F0000803F}
        Material.FrontProperties.Emission.Color = {0AD7A33E48E1FA3E1F85EB3E0000803F}
      end
      item
        Name = 'G'
        Tag = 0
        Material.BackProperties.Ambient.Color = {ACC8483E9A99193FCDCC4C3F0000803F}
        Material.BackProperties.Diffuse.Color = {ACC8483E9A99193FCDCC4C3F0000803F}
        Material.BackProperties.Emission.Color = {ACC8483E9A99193FCDCC4C3F0000803F}
        Material.BackProperties.Specular.Color = {ACC8483E9A99193FCDCC4C3F0000803F}
        Material.FrontProperties.Ambient.Color = {ACC8483E9A99193FCDCC4C3F0000803F}
        Material.FrontProperties.Diffuse.Color = {ACC8483E9A99193FCDCC4C3F0000803F}
        Material.FrontProperties.Emission.Color = {ACC8483E9A99193FCDCC4C3F0000803F}
        Material.FrontProperties.Specular.Color = {ACC8483E9A99193FCDCC4C3F0000803F}
      end
      item
        Name = 'Gsh'
        Tag = 0
        Material.BackProperties.Ambient.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Material.BackProperties.Diffuse.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Material.BackProperties.Emission.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Material.BackProperties.Specular.Color = {938C0C3EDCD6D63E938E0E3F0000803F}
        Material.FrontProperties.Emission.Color = {AE47613ED7A3303F52B85E3F0000803F}
      end
      item
        Name = 'A'
        Tag = 0
        Material.BackProperties.Ambient.Color = {00000000000000000000803F0000803F}
        Material.BackProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Material.BackProperties.Emission.Color = {00000000000000000000803F0000803F}
        Material.BackProperties.Specular.Color = {00000000000000000000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Material.FrontProperties.Emission.Color = {00000000000000000000803F0000803F}
        Material.FrontProperties.Specular.Color = {00000000000000000000803F0000803F}
      end
      item
        Name = 'Ash'
        Tag = 0
        Material.BackProperties.Ambient.Color = {938C0C3E938C0C3E938E0E3F0000803F}
        Material.BackProperties.Diffuse.Color = {938C0C3E938C0C3E938E0E3F0000803F}
        Material.BackProperties.Emission.Color = {938C0C3E938C0C3E938E0E3F0000803F}
        Material.BackProperties.Specular.Color = {938C0C3E938C0C3E938E0E3F0000803F}
        Material.FrontProperties.Emission.Color = {9A99993E9A99993E0000803F0000803F}
      end
      item
        Name = 'B'
        Tag = 0
        Material.BackProperties.Ambient.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.BackProperties.Diffuse.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.BackProperties.Emission.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.BackProperties.Specular.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.FrontProperties.Ambient.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.FrontProperties.Diffuse.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.FrontProperties.Emission.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
        Material.FrontProperties.Specular.Color = {A19E9E3ECFBC3C3EA19E9E3E0000803F}
      end>
    Left = 165
    Top = 183
  end
end
