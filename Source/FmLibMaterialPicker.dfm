object GLLibMaterialPickerForm: TGLLibMaterialPickerForm
  Left = 326
  Top = 157
  BorderStyle = bsDialog
  Caption = 'LibMaterial Picker'
  ClientHeight = 261
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 152
    Top = 8
    Width = 78
    Height = 13
    Caption = 'Material Preview'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 88
    Height = 13
    Caption = 'Available Materials'
  end
  object LBMaterials: TListBox
    Left = 8
    Top = 24
    Width = 137
    Height = 223
    ItemHeight = 13
    TabOrder = 0
    OnClick = LBMaterialsClick
    OnDblClick = LBMaterialsDblClick
    OnKeyPress = LBMaterialsKeyPress
  end
  object BBOk: TBitBtn
    Left = 376
    Top = 24
    Width = 75
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
  end
  object BBCancel: TBitBtn
    Left = 376
    Top = 56
    Width = 75
    Height = 25
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 1
  end
  inline MPPreview: TRMaterialPreview
    Left = 157
    Top = 25
    Width = 202
    Height = 222
    AutoSize = True
    TabOrder = 3
    ExplicitLeft = 157
    ExplicitTop = 25
    ExplicitHeight = 222
    inherited CBObject: TComboBox
      Height = 21
      ExplicitHeight = 21
    end
    inherited CBBackground: TComboBox
      Height = 21
      ExplicitHeight = 21
    end
    inherited GLSceneViewer: TGLSceneViewer
      Top = 19
      ExplicitTop = 19
    end
    inherited GLScene: TGLScene
      inherited World: TGLDummyCube
        inherited Cube: TGLCube
          Material.MaterialLibrary = nil
          Material.LibMaterialName = ''
          Direction.Coordinates = {FCFAF0B1D8B35D3FFEFFFF3E00000000}
          Up.Coordinates = {D7B35DBFFFFF7F3ED7B3DDBE00000000}
        end
        inherited Sphere: TGLSphere
          Material.MaterialLibrary = nil
          Material.LibMaterialName = ''
        end
        inherited Cone: TGLCone
          Material.MaterialLibrary = nil
          Material.LibMaterialName = ''
        end
        inherited Teapot: TGLTeapot
          Material.MaterialLibrary = nil
          Material.LibMaterialName = ''
          Scale.Coordinates = {00000040000000400000004000000000}
        end
      end
      inherited Light: TGLDummyCube
        Position.Coordinates = {0000000000004040000020410000803F}
        inherited LightSource: TGLLightSource
          Position.Coordinates = {0000000000004040000020410000803F}
          Specular.Color = {0000803F0000803F0000803F0000803F}
        end
        inherited FireSphere: TGLSphere
          Material.FrontProperties.Ambient.Color = {A3A2223FCDCC4C3ECDCC4C3E0000803F}
          Material.FrontProperties.Emission.Color = {D3D2523FA1A0203F000000000000803F}
        end
      end
      inherited Camera: TGLCamera
        Position.Coordinates = {0000000000000000000020410000803F}
      end
    end
    inherited GLMaterialLibrary: TGLMaterialLibrary
      Materials = <
        item
          Name = 'LibMaterial1'
          Tag = 0
        end>
    end
  end
end
