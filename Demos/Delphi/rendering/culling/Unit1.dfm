object Form1: TForm1
  Left = 188
  Top = 112
  Caption = 'Culling'
  ClientHeight = 486
  ClientWidth = 636
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object Viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 504
    Height = 486
    Camera = GLCamera1
    Buffer.BackgroundColor = 12040119
    FieldOfView = 156.746032714843700000
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 449
    ExplicitHeight = 419
  end
  object Panel2: TPanel
    Left = 504
    Top = 0
    Width = 132
    Height = 486
    Align = alRight
    TabOrder = 1
    ExplicitLeft = 449
    ExplicitHeight = 419
    DesignSize = (
      132
      486)
    object Label1: TLabel
      Left = 25
      Top = 8
      Width = 75
      Height = 15
      Anchors = [akTop, akRight]
      Caption = 'Culling Mode:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 29
      Top = 128
      Width = 47
      Height = 15
      Anchors = [akTop, akRight]
      Caption = 'Objects:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object RBNone: TRadioButton
      Left = 27
      Top = 33
      Width = 89
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'None'
      TabOrder = 0
      OnClick = RBNoneClick
    end
    object RBObject: TRadioButton
      Left = 27
      Top = 56
      Width = 89
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Object based'
      TabOrder = 1
      OnClick = RBNoneClick
    end
    object RBHierarchical: TRadioButton
      Left = 27
      Top = 79
      Width = 89
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Hierarchical'
      TabOrder = 2
      Visible = False
      OnClick = RBNoneClick
    end
    object RBActors: TRadioButton
      Left = 32
      Top = 208
      Width = 81
      Height = 17
      Caption = 'Actors'
      TabOrder = 3
      OnClick = RBSpheresClick
    end
    object RBSpheres: TRadioButton
      Left = 30
      Top = 170
      Width = 81
      Height = 17
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
