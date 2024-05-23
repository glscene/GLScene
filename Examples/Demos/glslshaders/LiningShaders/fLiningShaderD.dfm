object FormLiningShader: TFormLiningShader
  Left = 206
  Top = 117
  Margins.Left = 4
  Margins.Top = 4
  Margins.Right = 4
  Margins.Bottom = 4
  Caption = 'Lining Shaders'
  ClientHeight = 540
  ClientWidth = 939
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 168
  TextHeight = 24
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 668
    Height = 540
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Camera = Camera
    Buffer.AntiAliasing = aa4xHQ
    FieldOfView = 159.017044067382800000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
    ExplicitLeft = -11
  end
  object Panel1: TPanel
    Left = 668
    Top = 0
    Width = 271
    Height = 540
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alRight
    BevelOuter = bvSpace
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label1: TLabel
      Left = 29
      Top = 396
      Width = 94
      Height = 24
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Drag with:'
    end
    object Label2: TLabel
      Left = 29
      Top = 424
      Width = 230
      Height = 24
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'LMB - move around scene'
    end
    object Label3: TLabel
      Left = 29
      Top = 452
      Width = 163
      Height = 24
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'RMB - rotate torus'
    end
    object Bevel1: TBevel
      Left = 20
      Top = 382
      Width = 229
      Height = 16
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Shape = bsTopLine
    end
    object CheckBox1: TCheckBox
      Left = 11
      Top = 326
      Width = 228
      Height = 30
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Outline Shader Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object GroupBox1: TGroupBox
      Left = 15
      Top = 102
      Width = 241
      Height = 193
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Shader on Torus'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object Bevel2: TBevel
        Left = 14
        Top = 91
        Width = 204
        Height = 15
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Shape = bsTopLine
      end
      object CheckBox2: TCheckBox
        Left = 18
        Top = 28
        Width = 170
        Height = 29
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Caption = 'Enabled'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = CheckBox2Click
      end
      object CheckBox3: TCheckBox
        Left = 18
        Top = 98
        Width = 170
        Height = 29
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Caption = 'Solid'
        TabOrder = 1
        OnClick = CheckBox3Click
      end
      object CheckBox4: TCheckBox
        Left = 18
        Top = 56
        Width = 198
        Height = 29
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Caption = 'Dotted Hidden Line'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = CheckBox4Click
      end
      object CheckBox5: TCheckBox
        Left = 46
        Top = 126
        Width = 137
        Height = 29
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Caption = 'Surface Lit'
        TabOrder = 3
        OnClick = CheckBox5Click
      end
      object CheckBox6: TCheckBox
        Left = 46
        Top = 154
        Width = 109
        Height = 29
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Caption = 'Flat'
        TabOrder = 4
        OnClick = CheckBox6Click
      end
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 269
      Height = 76
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Lining Shaders'
      Color = clBtnHighlight
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -34
      Font.Name = 'Trebuchet MS'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
    end
  end
  object GLScene1: TGLScene
    ObjectsSorting = osRenderFarthestFirst
    Left = 22
    Top = 8
    object LightSource: TGLLightSource
      Ambient.Color = {0000803F0000803F0000803F0000803F}
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00002041000000410000E0400000803F}
      Specular.Color = {0000803F0000803F0000803F0000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Torus: TGLTorus
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial2'
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      MajorRadius = 2.500000000000000000
      MinorRadius = 1.500000000000000000
      StopAngle = 360.000000000000000000
      Parts = [toSides, toStartDisk, toStopDisk]
    end
    object Sphere: TGLSphere
      ShowAxes = True
      Radius = 0.500000000000000000
    end
    object AnnulusOutlined: TGLAnnulus
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial'
      Position.Coordinates = {0000E04000000000000000000000803F}
      Scale.Coordinates = {00000040000000400000004000000000}
      BottomRadius = 0.500000000000000000
      Height = 1.000000000000000000
      BottomInnerRadius = 0.300000011920929000
      TopInnerRadius = 0.300000011920929000
      TopRadius = 0.500000000000000000
    end
    object AnnulusPink: TGLAnnulus
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial4'
      Position.Coordinates = {0000E0C000000000000000000000803F}
      Scale.Coordinates = {00000040000000400000004000000000}
      BottomRadius = 0.500000000000000000
      Height = 1.000000000000000000
      BottomInnerRadius = 0.300000011920929000
      TopInnerRadius = 0.300000011920929000
      TopRadius = 0.500000000000000000
    end
    object AnnulusDotted: TGLAnnulus
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial1'
      Position.Coordinates = {00000000000000000000E0C00000803F}
      Scale.Coordinates = {00000040000000400000004000000000}
      BottomRadius = 0.500000000000000000
      Height = 1.000000000000000000
      BottomInnerRadius = 0.300000011920929000
      TopInnerRadius = 0.300000011920929000
      TopRadius = 0.500000000000000000
    end
    object CubeGreen: TGLCube
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial5'
      Position.Coordinates = {00000000000000000000E0400000803F}
      Scale.Coordinates = {00000040000000400000004000000000}
    end
    object CubeTransparent: TGLCube
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial3'
      Position.Coordinates = {0000000000008040000000000000803F}
      CubeSize = {000040400000404000004040}
    end
    object Camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Torus
      Position.Coordinates = {00006041000020410000C0400000803F}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {9190903D9190903D8786863E0000803F}
        Material.FrontProperties.Diffuse.Color = {9796163F0000803F0000803F0000803F}
        Material.FrontProperties.Shininess = 128
        Shader = GLOutlineShader1
      end
      item
        Name = 'LibMaterial1'
        Tag = 0
        Shader = GLHiddenLineShader1
      end
      item
        Name = 'LibMaterial2'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {C5C4443EBFBEBE3EB5B4343E0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803FEBEAEA3E8180803C0000803F}
        Material.FrontProperties.Shininess = 128
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.BlendingMode = bmTransparency
        Shader = GLHiddenLineShader2
      end
      item
        Name = 'LibMaterial3'
        Tag = 0
        Shader = GLHiddenLineShader3
      end
      item
        Name = 'LibMaterial4'
        Tag = 0
        Shader = GLHiddenLineShader4
      end
      item
        Name = 'LibMaterial5'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {9998183E9796163F9190103E17D92E3F}
        Material.BlendingMode = bmTransparency
        Shader = GLHiddenLineShader5
      end>
    Left = 22
    Top = 124
  end
  object GLOutlineShader1: TGLOutlineShader
    LineColor.Color = {0000803F00000000000000000000803F}
    LineSmooth = True
    LineWidth = 4.000000000000000000
    Left = 106
    Top = 380
  end
  object GLHiddenLineShader1: TGLHiddenLineShader
    FrontLine.Width = 1.000000000000000000
    FrontLine.Color.Color = {938C0C3E938C0C3E938E0E3F0000803F}
    FrontLine.Pattern = 10101
    BackLine.Width = 2.000000000000000000
    LineSmooth = True
    Solid = True
    Left = 446
    Top = 8
  end
  object GLHiddenLineShader2: TGLHiddenLineShader
    FrontLine.Width = 2.000000000000000000
    BackLine.Width = 1.000000000000000000
    BackLine.Color.Color = {000000000000003F000000000000803F}
    BackLine.Pattern = 65280
    LineSmooth = True
    SurfaceLit = False
    Left = 124
    Top = 65534
  end
  object GLHiddenLineShader3: TGLHiddenLineShader
    FrontLine.Width = 5.000000000000000000
    FrontLine.Color.Color = {EC51B83E0000803ECDCC4C3E9A99193F}
    BackLine.Width = 5.000000000000000000
    BackLine.Color.Color = {0000003F0000003F0000003F9A99993E}
    LineSmooth = True
    Left = 586
    Top = 16
  end
  object GLHiddenLineShader4: TGLHiddenLineShader
    FrontLine.Width = 2.000000000000000000
    FrontLine.Color.Color = {B81E053F14AEC73E14AEC73E0000803F}
    BackLine.Width = 1.000000000000000000
    BackLine.Color.Color = {E4DB5B3FEBE0E03E9A93133F0000803F}
    LineSmooth = True
    Left = 226
    Top = 8
  end
  object GLHiddenLineShader5: TGLHiddenLineShader
    FrontLine.Width = 2.000000000000000000
    FrontLine.Color.Color = {9A93133FE4DB5B3FEBE0E03E3333333F}
    BackLine.Width = 2.000000000000000000
    BackLine.Color.Color = {000000000000003F000000000000803F}
    BackLine.Pattern = 65280
    LineSmooth = True
    Solid = True
    BackgroundColor.Color = {938C0C3E938E0E3F938C0C3E3333333F}
    SurfaceLit = False
    Left = 310
    Top = 12
  end
end
