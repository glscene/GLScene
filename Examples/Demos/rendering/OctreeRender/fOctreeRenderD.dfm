object frmOctreeDemo: TfrmOctreeDemo
  Left = 309
  Top = 207
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Octree Demo'
  ClientHeight = 968
  ClientWidth = 1362
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 168
  TextHeight = 24
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 142
    Width = 1362
    Height = 826
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    Buffer.FaceCulling = False
    FieldOfView = 159.414718627929700000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1362
    Height = 142
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    TabOrder = 1
    DesignSize = (
      1362
      142)
    object Label3: TLabel
      Left = 28
      Top = 14
      Width = 126
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Leaf Threshold'
    end
    object Label2: TLabel
      Left = 210
      Top = 14
      Width = 695
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 
        '(Green = Colliding with other object, Red = inside query box/sph' +
        'ere, Yellow = both)'
    end
    object LabelCollisions: TLabel
      Left = 918
      Top = 60
      Width = 80
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Collisions'
    end
    object TrackBar_LeafThreshold: TTrackBar
      Left = 14
      Top = 42
      Width = 263
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Max = 20
      Min = 3
      Position = 10
      TabOrder = 0
      ThumbLength = 35
      TickStyle = tsNone
      OnChange = TrackBar_LeafThresholdChange
    end
    object Button_ResetOctreeSize: TButton
      Left = 476
      Top = 47
      Width = 184
      Height = 37
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Reset Octree Size'
      TabOrder = 1
      OnClick = Button_ResetOctreeSizeClick
    end
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 64
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
      Blend = False
    end
    object GLCube1: TGLCube
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'CommonMaterial'
      CubeSize = {0000A0400000A04000000041}
    end
    object GLSphere1: TGLSphere
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'CommonMaterial'
      Position.Coordinates = {0000C0400000C0400000C0400000803F}
      Radius = 2.500000000000000000
    end
    object GLPlane1: TGLPlane
      Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3FCDCC4C3E}
      Material.BlendingMode = bmTransparency
      Direction.Coordinates = {0000000000000080000080BF00000000}
      Position.Coordinates = {0000000000000000000020410000803F}
      Up.Coordinates = {6A41323FD9BD373F0000000000000000}
      Visible = False
      Height = 40.000000000000000000
      Width = 40.000000000000000000
    end
    object GLLines1: TGLLines
      Nodes = <>
      Options = []
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 75.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000004200006041000090410000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 16
    Top = 120
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    ZoomSpeed = 1.250000000000000000
    FormCaption = 'GLScene Octree - %FPS'
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
    Left = 16
    Top = 176
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'CommonMaterial'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3FCDCCCC3E}
        Material.DepthProperties.DepthTest = False
        Material.DepthProperties.DepthWrite = False
        Material.BlendingMode = bmTransparency
        Material.FaceCulling = fcCull
      end>
    Left = 16
    Top = 224
  end
end
