object Form1: TForm1
  Left = 264
  Top = 142
  Caption = 'Culling'
  ClientHeight = 342
  ClientWidth = 569
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 416
    Height = 342
    Camera = GLCamera1
    Buffer.BackgroundColor = 12040119
    FieldOfView = 147.402404785156300000
    Align = alClient
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 416
    Top = 0
    Width = 153
    Height = 342
    Align = alRight
    TabOrder = 1
    DesignSize = (
      153
      342)
    object Label1: TLabel
      Left = 17
      Top = 15
      Width = 40
      Height = 16
      Anchors = [akTop, akRight]
      Caption = 'Mode'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitLeft = 49
    end
    object Label2: TLabel
      Left = 19
      Top = 137
      Width = 54
      Height = 16
      Anchors = [akTop, akRight]
      Caption = 'Objects'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitLeft = 51
    end
    object LabelFPS: TLabel
      Left = 16
      Top = 240
      Width = 24
      Height = 13
      Caption = 'FPS'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object RBNone: TRadioButton
      Left = 19
      Top = 37
      Width = 102
      Height = 19
      Anchors = [akTop, akRight]
      Caption = 'None'
      TabOrder = 0
      OnClick = RBNoneClick
    end
    object RBObject: TRadioButton
      Left = 19
      Top = 64
      Width = 102
      Height = 19
      Anchors = [akTop, akRight]
      Caption = 'Object based'
      TabOrder = 1
      OnClick = RBNoneClick
    end
    object RBHierarchical: TRadioButton
      Left = 19
      Top = 91
      Width = 102
      Height = 20
      Anchors = [akTop, akRight]
      Caption = 'Hierarchical'
      TabOrder = 2
      OnClick = RBNoneClick
    end
    object RBSpheres: TRadioButton
      Left = 24
      Top = 159
      Width = 93
      Height = 19
      Caption = 'Spheres'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = RBSpheresClick
    end
    object RBActors: TRadioButton
      Left = 24
      Top = 184
      Width = 93
      Height = 20
      Caption = 'Actors'
      TabOrder = 4
      OnClick = RBSpheresClick
    end
  end
  object GLScene: TGLScene
    Left = 24
    Top = 32
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
    Left = 112
    Top = 40
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 104
  end
  object GLMaterialLibrary: TGLMaterialLibrary
    Materials = <
      item
        Name = 'ActorTexture'
        Tag = 0
        Material.Texture.Disabled = False
      end>
    Left = 112
    Top = 104
  end
end
