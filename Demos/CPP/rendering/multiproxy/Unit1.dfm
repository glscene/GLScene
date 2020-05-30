object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Multiproxy'
  ClientHeight = 356
  ClientWidth = 590
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 60
    Width = 590
    Height = 296
    Camera = GLCamera
    Buffer.BackgroundColor = clGray
    FieldOfView = 111.908126831054700000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 590
    Height = 60
    Align = alTop
    BevelOuter = bvLowered
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object LabelFPS: TLabel
      Left = 464
      Top = 8
      Width = 19
      Height = 14
      Caption = 'FPS'
    end
    object RBUseLODs: TRadioButton
      Left = 88
      Top = 8
      Width = 129
      Height = 17
      Caption = 'Use 3 Levels of Detail'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RBUseLODsClick
    end
    object RBHighRes: TRadioButton
      Left = 88
      Top = 24
      Width = 129
      Height = 17
      Caption = 'Force High Resolution'
      TabOrder = 1
      OnClick = RBUseLODsClick
    end
    object CBColorize: TCheckBox
      Left = 240
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Colorize LODs'
      TabOrder = 2
      OnClick = RBUseLODsClick
    end
    object RBLowRes: TRadioButton
      Left = 88
      Top = 37
      Width = 129
      Height = 17
      Caption = 'Force Low Resolution'
      TabOrder = 3
      OnClick = RBUseLODsClick
    end
  end
  object GLScene: TGLScene
    Left = 24
    Top = 72
    object DCTarget: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLParticles: TGLParticles
        Direction.Coordinates = {0000000000000000FFFF7F3F00000000}
        Position.Coordinates = {0000000000000040000000000000803F}
        RollAngle = -5.000000000000000000
        Up.Coordinates = {B97EB23D9E067F3F0000000000000000}
        CubeSize = 1.000000000000000000
        object MPSphere: TGLMultiProxy
          OnProgress = MPSphereProgress
          MasterObjects = <
            item
              MasterObject = SPHighRes
              DistanceMax = 20.000000000000000000
            end
            item
              MasterObject = SPMedRes
              DistanceMin = 20.000000000000000000
              DistanceMax = 60.000000000000000000
            end
            item
              MasterObject = SPLowRes
              DistanceMin = 60.000000000000000000
              DistanceMax = 99999.000000000000000000
            end>
        end
      end
    end
    object DCReferences: TGLDummyCube
      Visible = False
      CubeSize = 1.000000000000000000
      object SPHighRes: TGLSphere
        Radius = 0.500000000000000000
        Slices = 32
        Stacks = 32
      end
      object SPMedRes: TGLSphere
        Radius = 0.500000000000000000
      end
      object SPLowRes: TGLSphere
        Radius = 0.500000000000000000
        Slices = 8
        Stacks = 8
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00004842000020420000F0410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera: TGLCamera
      DepthOfView = 200.000000000000000000
      FocalLength = 100.000000000000000000
      TargetObject = DCTarget
      Position.Coordinates = {0000F04100004040000000400000803F}
    end
  end
  object GLCadencer: TGLCadencer
    Scene = GLScene
    TimeMultiplier = 0.500000000000000000
    Left = 24
    Top = 136
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 112
    Top = 72
  end
end
