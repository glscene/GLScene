object FormGameMenu: TFormGameMenu
  Left = 192
  Top = 114
  Caption = 'Game Menu'
  ClientHeight = 449
  ClientWidth = 558
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 558
    Height = 422
    Camera = GLCamera1
    Buffer.BackgroundColor = clGray
    FieldOfView = 153.337402343750000000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object MainPanel: TPanel
    Left = 0
    Top = 422
    Width = 558
    Height = 27
    Align = alBottom
    TabOrder = 1
    OnResize = MainPanelResize
    object Label1: TLabel
      Left = 144
      Top = 6
      Width = 234
      Height = 13
      Caption = 'Press "W" or "S" to navigate or "Enter" to select '
    end
    object ShowTitleCheckbox: TCheckBox
      Left = 8
      Top = 6
      Width = 81
      Height = 17
      Caption = 'Show Title'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = ShowTitleCheckboxClick
      OnKeyPress = FormKeyPress
    end
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 8
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCube1: TGLCube
        Position.Coordinates = {0000803F00000000000000000000803F}
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000404000004040000040400000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {0000204100002041000020410000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -32
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 128
    Top = 8
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 72
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Tag = 0
        Material.Texture.Disabled = False
      end>
    Left = 128
    Top = 72
  end
end
