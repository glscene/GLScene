object FormTiles: TFormTiles
  Left = 124
  Top = 96
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Tiles'
  ClientHeight = 711
  ClientWidth = 1068
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 168
  TextHeight = 24
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 844
    Height = 711
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera
    FieldOfView = 163.988067626953100000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 844
    Top = 0
    Width = 224
    Height = 711
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alRight
    BevelOuter = bvLowered
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label1: TLabel
      Left = 14
      Top = 14
      Width = 110
      Height = 22
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Tile Materials'
    end
    object Label2: TLabel
      Left = 14
      Top = 238
      Width = 198
      Height = 156
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 
        'Shift + Left button: pan'#13#10#13#10'Left button : paint with current mat' +
        'erial'#13#10#13#10'Mouse wheel: zoom'
      WordWrap = True
    end
    object CBMaterial: TComboBox
      Left = 14
      Top = 42
      Width = 198
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Style = csDropDownList
      TabOrder = 0
    end
    object BUPack: TButton
      Left = 42
      Top = 126
      Width = 131
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Pack'
      TabOrder = 1
      OnClick = BUPackClick
    end
    object CBShowGrid: TCheckBox
      Left = 14
      Top = 434
      Width = 142
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Show Grid'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CBShowGridClick
    end
    object CBSortByMaterials: TCheckBox
      Left = 14
      Top = 476
      Width = 184
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Sort by materials'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CBSortByMaterialsClick
    end
  end
  object GLScene: TGLScene
    Left = 24
    Top = 16
    object LightSource: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000048C2000020C20000F0410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object dcTarget: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCamera: TGLCamera
        DepthOfView = 500.000000000000000000
        FocalLength = 50.000000000000000000
        NearPlaneBias = 0.200000002980232200
        TargetObject = dcTarget
        Position.Coordinates = {00000000000080C0000040400000803F}
        Direction.Coordinates = {0000803F000000000000008000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
      end
    end
    object TilePlane: TGLTilePlane
      NoZWrite = False
      MaterialLibrary = GLMaterialLibrary
    end
    object DirectOpenGL: TGLDirectOpenGL
      UseBuildList = False
      OnRender = DirectOpenGLRender
      Blend = False
    end
    object XYZGrid: TGLXYZGrid
      AntiAliased = True
      XSamplingScale.Min = -30.000000000000000000
      XSamplingScale.Max = 30.000000000000000000
      XSamplingScale.Step = 1.000000000000000000
      YSamplingScale.Min = -30.000000000000000000
      YSamplingScale.Max = 30.000000000000000000
      YSamplingScale.Step = 1.000000000000000000
      ZSamplingScale.Min = -10.000000000000000000
      ZSamplingScale.Max = 10.000000000000000000
      ZSamplingScale.Step = 1.000000000000000000
    end
    object dcSelection: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLLines1: TGLLines
        AntiAliased = True
        LineColor.Color = {0000000000000000000000000000803F}
        LineWidth = 6.000000000000000000
        NodeColor.Color = {0000803F0000803F0000803F0000803F}
        Nodes = <
          item
          end
          item
            X = 1.000000000000000000
          end
          item
            X = 1.000000000000000000
            Y = 1.000000000000000000
          end
          item
            Y = 1.000000000000000000
          end
          item
          end>
        NodesAspect = lnaCube
        NodeSize = 0.150000005960464500
        Options = []
      end
    end
    object DummyCube: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
  end
  object GLMaterialLibrary: TGLMaterialLibrary
    Materials = <
      item
        Name = 'base'
        Tag = 0
      end
      item
        Name = 'blue'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {00000000000000000000003F0000803F}
        Material.FrontProperties.Emission.Color = {00000000000000000000003F0000803F}
      end
      item
        Name = 'red'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000003F}
        Material.FrontProperties.Emission.Color = {0000003F00000000000000000000803F}
      end
      item
        Name = 'beigemarble'
        Tag = 0
        Material.Texture.Disabled = False
      end
      item
        Name = 'marbletiles'
        Tag = 0
        Material.Texture.Disabled = False
        TextureScale.Coordinates = {0000803E0000803E0000803F00000000}
      end
      item
        Name = 'walkway'
        Tag = 0
        Material.Texture.Disabled = False
        TextureScale.Coordinates = {0000003F0000003F0000803F00000000}
      end>
    Left = 94
    Top = 212
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 80
    Top = 16
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene
    OnProgress = GLCadencer1Progress
    Left = 80
    Top = 64
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Tiles - %FPS'
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 264
    Top = 16
  end
end
