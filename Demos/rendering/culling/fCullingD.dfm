object FormCulling: TFormCulling
  Left = 188
  Top = 112
  Caption = 'Culling'
  ClientHeight = 608
  ClientWidth = 795
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Arial'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 630
    Height = 608
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.BackgroundColor = 12040119
    FieldOfView = 161.319946289062500000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 630
    Top = 0
    Width = 165
    Height = 608
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alRight
    TabOrder = 1
    DesignSize = (
      165
      608)
    object Label1: TLabel
      Left = 31
      Top = 10
      Width = 98
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akTop, akRight]
      Caption = 'Culling Mode:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 36
      Top = 160
      Width = 59
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akTop, akRight]
      Caption = 'Objects:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object RBNone: TRadioButton
      Left = 34
      Top = 41
      Width = 111
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akTop, akRight]
      Caption = 'None'
      TabOrder = 0
      OnClick = RBNoneClick
    end
    object RBObject: TRadioButton
      Left = 34
      Top = 70
      Width = 111
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akTop, akRight]
      Caption = 'Object based'
      TabOrder = 1
      OnClick = RBNoneClick
    end
    object RBHierarchical: TRadioButton
      Left = 34
      Top = 99
      Width = 111
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akTop, akRight]
      Caption = 'Hierarchical'
      TabOrder = 2
      Visible = False
      OnClick = RBNoneClick
    end
    object RBActors: TRadioButton
      Left = 40
      Top = 260
      Width = 101
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Actors'
      TabOrder = 3
      OnClick = RBSpheresClick
    end
    object RBSpheres: TRadioButton
      Left = 38
      Top = 213
      Width = 101
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Spheres'
      Checked = True
      TabOrder = 4
      TabStop = True
      OnClick = RBSpheresClick
    end
  end
  object GLScene: TGLScene
    Left = 16
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000C8420000A042000070420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DCTarget: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = DCTarget
        Position.Coordinates = {0000A04000000040000040400000803F}
        Left = 256
        Top = 144
      end
    end
    object DCSpheres: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object DCActors: TGLDummyCube
      Visible = False
      CubeSize = 1.000000000000000000
    end
    object ACReference: TGLActor
      Material.MaterialLibrary = GLMaterialLibrary
      Material.LibMaterialName = 'ActorTexture'
      Direction.Coordinates = {000000000000803F2EBD3BB300000000}
      Position.Coordinates = {CDCC4C3DCDCC4C3DCDCC4C3D0000803F}
      Scale.Coordinates = {CDCC4C3DCDCC4C3DCDCC4C3D00000000}
      Up.Coordinates = {000000002EBD3BB3000080BF00000000}
      AnimationMode = aamLoop
      Interval = 100
    end
  end
  object GLCadencer: TGLCadencer
    Scene = GLScene
    OnProgress = GLCadencerProgress
    Left = 48
    Top = 16
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 48
  end
  object GLMaterialLibrary: TGLMaterialLibrary
    Materials = <
      item
        Name = 'ActorTexture'
        Tag = 0
        Material.Texture.Disabled = False
      end>
    Left = 48
    Top = 48
  end
end
