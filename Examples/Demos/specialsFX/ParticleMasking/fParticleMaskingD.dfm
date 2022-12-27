object FormParticleMasking: TFormParticleMasking
  Left = 250
  Top = 151
  Caption = 'Particle Masking'
  ClientHeight = 756
  ClientWidth = 958
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 120
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 720
    Top = 0
    Width = 10
    Height = 711
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alRight
    Beveled = True
    MinSize = 200
    ResizeStyle = rsLine
  end
  object MaskBox: TGroupBox
    Left = 730
    Top = 0
    Width = 228
    Height = 711
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alRight
    Caption = 'Particle Masks'
    TabOrder = 0
    object XImage: TImage
      Left = 20
      Top = 40
      Width = 188
      Height = 188
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Stretch = True
    end
    object XLabel: TLabel
      Left = 20
      Top = 20
      Width = 44
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'X Mask'
    end
    object YLabel: TLabel
      Left = 20
      Top = 250
      Width = 45
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Y Mask'
    end
    object ZLabel: TLabel
      Left = 20
      Top = 500
      Width = 44
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Z Mask'
    end
    object YImage: TImage
      Left = 20
      Top = 280
      Width = 188
      Height = 188
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Stretch = True
    end
    object ZImage: TImage
      Left = 20
      Top = 520
      Width = 188
      Height = 188
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Stretch = True
    end
    object Button1: TButton
      Left = 80
      Top = 15
      Width = 124
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'View from XMask'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 90
      Top = 245
      Width = 124
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'View from YMask'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 90
      Top = 495
      Width = 124
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'View from ZMask'
      TabOrder = 2
      OnClick = Button3Click
    end
  end
  object SceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 720
    Height = 711
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = Camera
    Buffer.BackgroundColor = clBackground
    FieldOfView = 176.777465820312500000
    PenAsTouch = False
    Align = alClient
    OnMouseMove = SceneViewerMouseMove
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 711
    Width = 958
    Height = 45
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    TabOrder = 2
    object Label1: TLabel
      Left = 10
      Top = 14
      Width = 36
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Depth'
    end
    object Label2: TLabel
      Left = 110
      Top = 14
      Width = 58
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Character'
    end
    object Label3: TLabel
      Left = 220
      Top = 14
      Width = 67
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Pitch Angle'
    end
    object Label4: TLabel
      Left = 380
      Top = 14
      Width = 62
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Roll Angle'
    end
    object Label5: TLabel
      Left = 530
      Top = 14
      Width = 65
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Turn Angle'
    end
    object Button4: TButton
      Left = 1290
      Top = 4
      Width = 54
      Height = 37
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'GO'
      TabOrder = 0
      OnClick = Button4Click
    end
    object Edit1: TEdit
      Left = 50
      Top = 10
      Width = 41
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 1
      Text = '50'
    end
    object Edit2: TEdit
      Left = 180
      Top = 10
      Width = 31
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 2
      Text = 'A'
      OnChange = Edit2Change
    end
    object Edit3: TEdit
      Left = 300
      Top = 10
      Width = 71
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 3
      Text = '0'
      OnChange = Edit3Change
    end
    object Edit4: TEdit
      Left = 450
      Top = 10
      Width = 71
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 4
      Text = '0'
      OnChange = Edit4Change
    end
    object Edit5: TEdit
      Left = 610
      Top = 9
      Width = 71
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 5
      Text = '0'
      OnChange = Edit5Change
    end
    object CheckBox1: TCheckBox
      Left = 689
      Top = 8
      Width = 121
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Target Sphere'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = CheckBox1Click
    end
  end
  object GLScene: TGLScene
    ObjectsSorting = osRenderFarthestFirst
    Left = 8
    Top = 8
    object Target: TGLDummyCube
      CubeSize = 1.000000000000000000
      EffectsData = {
        0458434F4C02010201061254474C536F75726365504658456666656374020202
        001200000000020002000609504C4D616E61676572050000000000000080FF3F
        0206020008020008020008050000000000000000000005000000000000000000
        00050000000000FAEDEBF43F02000200090500000000000000000000080200}
    end
    object XPlane: TGLPlane
      Material.MaterialLibrary = MatLib
      Material.LibMaterialName = 'XMask'
      Direction.Coordinates = {0000803F000000002EBD3BB300000000}
      Position.Coordinates = {000040C000000000000000000000803F}
      TurnAngle = 90.000000000000000000
      Up.Coordinates = {00000000FFFF7F3F0000000000000000}
      Height = 3.000000000000000000
      Width = 3.000000000000000000
    end
    object YPlane: TGLPlane
      Material.MaterialLibrary = MatLib
      Material.LibMaterialName = 'YMask'
      Direction.Coordinates = {000000000000803F31BD3BB300000000}
      PitchAngle = 90.000000000000000000
      Position.Coordinates = {00000000000040C0000000000000803F}
      RollAngle = 180.000000000000000000
      Up.Coordinates = {010000B331BD3B330000803F00000000}
      Height = 3.000000000000000000
      Width = 3.000000000000000000
    end
    object ZPlane: TGLPlane
      Material.MaterialLibrary = MatLib
      Material.LibMaterialName = 'ZMask'
      Direction.Coordinates = {00000000010000B30000803F00000000}
      Position.Coordinates = {0000000000000000000040C00000803F}
      Up.Coordinates = {000000000000803F0100003300000000}
      Hint = '0'
      Height = 3.000000000000000000
      Width = 3.000000000000000000
    end
    object PFXRenderer: TGLParticleFXRenderer
    end
    object Sphere: TGLSphere
      Direction.Coordinates = {0000000000000000FFFF7F3F00000000}
      Position.Coordinates = {0000004000000000000000000000803F}
      Visible = False
      Radius = 0.250000000000000000
      object GLArrowLine1: TGLArrowLine
        Direction.Coordinates = {0000803F000000002EBD3BB300000000}
        Position.Coordinates = {CDCCCC3D00000000000000000000803F}
        TurnAngle = 90.000000000000000000
        Up.Coordinates = {00000000FFFF7F3F0000000000000000}
        BottomRadius = 0.100000001490116100
        Height = 0.500000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.500000000000000000
        BottomArrowHeadRadius = 0.200000002980232200
      end
    end
    object Camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 10.000000000000000000
      TargetObject = Target
      CameraStyle = csOrthogonal
      Position.Coordinates = {0000803F00000040000040400000803F}
      object Light: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLCadencer: TGLCadencer
    Scene = GLScene
    OnProgress = GLCadencerProgress
    Left = 72
    Top = 8
  end
  object MatLib: TGLMaterialLibrary
    Materials = <
      item
        Name = 'XMask'
        Tag = 0
        Material.Texture.Image.Picture.Data = {07544269746D617000000000}
        Material.Texture.ImageAlpha = tiaInverseLuminance
        Material.Texture.MagFilter = maNearest
        Material.Texture.MinFilter = miNearest
        Material.Texture.Disabled = False
      end
      item
        Name = 'YMask'
        Tag = 0
        Material.Texture.Image.Picture.Data = {07544269746D617000000000}
        Material.Texture.ImageAlpha = tiaInverseLuminance
        Material.Texture.MagFilter = maNearest
        Material.Texture.MinFilter = miNearest
        Material.Texture.Disabled = False
      end
      item
        Name = 'ZMask'
        Tag = 0
        Material.Texture.Image.Picture.Data = {07544269746D617000000000}
        Material.Texture.ImageAlpha = tiaInverseLuminance
        Material.Texture.MagFilter = maNearest
        Material.Texture.MinFilter = miNearest
        Material.Texture.Disabled = False
      end>
    Left = 40
    Top = 8
  end
  object WinFont: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 104
    Top = 8
  end
  object PLManager: TGLPointLightPFXManager
    Cadencer = GLCadencer
    Renderer = PFXRenderer
    OnCreateParticle = PLManagerCreateParticle
    Friction = 1.000000000000000000
    ColorMode = scmFade
    ParticleSize = 0.200000002980232200
    ColorInner.Color = {00000000000000000000000000000000}
    LifeColors = <
      item
        ColorInner.Color = {9A99993E9A99993E0000803F0000803F}
        LifeTime = 3.000000000000000000
        SizeScale = 1.000000000000000000
      end>
    Left = 136
    Top = 8
  end
  object GLEParticleMasksManager1: TGLEParticleMasksManager
    ParticleMasks = <
      item
        Scale.Coordinates = {0000A0400000A0400000A04000000000}
        Name = 'mask'
        MaterialLibrary = MatLib
        XMask = 'XMask'
        YMask = 'YMask'
        ZMask = 'ZMask'
        BackgroundColor = clBlack
        MaskColor = clWhite
      end>
    Left = 168
    Top = 8
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpNormal
    Left = 200
    Top = 8
  end
end
