object FormObjmove: TFormObjmove
  Left = 87
  Top = 128
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Moving Objects with Mouse'
  ClientHeight = 926
  ClientWidth = 1325
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnKeyUp = FormKeyUp
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 168
  TextHeight = 30
  object Scene: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 1325
    Height = 826
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = Camera
    Buffer.BackgroundColor = clBackground
    FieldOfView = 48.448417663574220000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = SceneMouseDown
    OnMouseMove = SceneMouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 826
    Width = 1325
    Height = 68
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object GroupBox1: TGroupBox
      Left = 0
      Top = -6
      Width = 1325
      Height = 74
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alBottom
      Caption = 'Options'
      TabOrder = 0
      object ShowAxes: TCheckBox
        Left = 9
        Top = 32
        Width = 254
        Height = 29
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Show selection axes'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = ShowAxesClick
      end
      object Button1: TButton
        Left = 1026
        Top = 30
        Width = 271
        Height = 44
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Near: (0,0) Eye -> Obj'
        TabOrder = 1
      end
      object ButtonReset: TButton
        Left = 308
        Top = 28
        Width = 120
        Height = 36
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Reset'
        TabOrder = 2
        OnClick = ButtonResetClick
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 894
    Width = 1325
    Height = 32
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Panels = <
      item
        Text = 
          'Select and move cubes with mouse. Default movement is on the XY ' +
          'plane. Shift + Drag moves on the XZ plane.'
        Width = 50
      end>
  end
  object GLScene1: TGLScene
    Left = 226
    Top = 8
    object Floor: TGLCube
      Material.FrontProperties.Diffuse.Color = {C5C4C43ECDCCCC3E8382023FE3A53B3F}
      Position.Coordinates = {00000000000000005C8F82BF0000803F}
      CubeSize = {00000040000000400AD7233C}
    end
    object Camera: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 917.929199218750000000
      SceneScale = 0.800000011920929000
      NearPlaneBias = 0.001000000047497451
      TargetObject = DummyCube
      Position.Coordinates = {0000B8410000A041000080410000803F}
      Direction.Coordinates = {2EF964BF2EF9E43E0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
    object DummyCube: TGLDummyCube
      Position.Coordinates = {0000803F0000803F0000003F0000803F}
      Visible = False
      CubeSize = 0.200000002980232200
      EdgeColor.Color = {DEDD5D3FDEDD5D3FE9E8683F0000803F}
    end
    object TopLight: TGLLightSource
      Ambient.Color = {0000003F0000003F0000003F0000803F}
      ConstAttenuation = 0.800000011920929000
      Diffuse.Color = {EAE9693FEAE9693FEAE9693F0000803F}
      Position.Coordinates = {0000804100005041000040410000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Cube1: TGLCube
      Material.BackProperties.Diffuse.Color = {8382023F8584043FB1B0303F8195633F}
      Material.FrontProperties.Diffuse.Color = {8382023F8584043FB1B0303F6DE75B3F}
      Material.BlendingMode = bmTransparency
      Material.Texture.ImageAlpha = tiaAlphaFromIntensity
      Material.Texture.TextureMode = tmModulate
      Direction.Coordinates = {0000803F000000000000000000000000}
      Position.Coordinates = {CDCCCC3DCDCCCC3D666666BF0000803F}
      Scale.Coordinates = {CDCC4C3ECDCC4C3ECDCC4C3E00000000}
    end
    object Cube2: TGLCube
      Position.Coordinates = {CDCCCCBECDCCCC3E000000BF0000803F}
      CubeSize = {0000803E0000803E0000803E}
    end
    object XArrow: TGLArrowLine
      Material.FrontProperties.Ambient.Color = {00000000000000000000803F0000803F}
      Direction.Coordinates = {0000803F000000000000000000000000}
      Position.Coordinates = {CDCCCC3D000080BF000080BF0000803F}
      Up.Coordinates = {000000002EBD3BB30000803F00000000}
      BottomRadius = 0.009999999776482582
      Height = 2.200000047683716000
      TopRadius = 0.009999999776482582
      TopArrowHeadHeight = 0.200000002980232200
      TopArrowHeadRadius = 0.050000000745058060
      BottomArrowHeadHeight = 0.500000000000000000
      BottomArrowHeadRadius = 0.050000000745058060
    end
    object YArrow: TGLArrowLine
      Material.FrontProperties.Ambient.Color = {000000000000003F000000000000803F}
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {000080BFCDCCCC3D000080BF0000803F}
      Up.Coordinates = {2EBD3BB3000000000000803F00000000}
      BottomRadius = 0.009999999776482582
      Height = 2.200000047683716000
      TopRadius = 0.009999999776482582
      TopArrowHeadHeight = 0.200000002980232200
      TopArrowHeadRadius = 0.050000000745058060
      BottomArrowHeadHeight = 0.500000000000000000
      BottomArrowHeadRadius = 0.200000002980232200
    end
    object ZArrow: TGLArrowLine
      Material.FrontProperties.Ambient.Color = {0000803F00000000000000000000803F}
      Position.Coordinates = {000080BF000080BF000000BF0000803F}
      BottomRadius = 0.009999999776482582
      Height = 1.000000000000000000
      TopRadius = 0.009999999776482582
      TopArrowHeadHeight = 0.200000002980232200
      TopArrowHeadRadius = 0.050000000745058060
      BottomArrowHeadHeight = 0.500000000000000000
      BottomArrowHeadRadius = 0.050000000745058060
    end
    object SpaceTextX: TGLSpaceText
      Direction.Coordinates = {F30435BFF30435BF0000000000000000}
      Position.Coordinates = {0000C03F000080BF000080BF0000803F}
      Scale.Coordinates = {9A99993E9A99993E9A99993E00000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Extrusion = 0.200000002980232200
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Lines.Strings = (
        'X')
      CharacterRange = stcrAlphaNum
    end
    object SpaceTextY: TGLSpaceText
      Direction.Coordinates = {F40435BFF20435BF0000000000000000}
      Position.Coordinates = {000080BF9A99D93F000080BF0000803F}
      Scale.Coordinates = {9A99993E9A99993E9A99993E00000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Extrusion = 0.200000002980232200
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Lines.Strings = (
        'Y')
      CharacterRange = stcrAlphaNum
    end
    object SpaceTextZ: TGLSpaceText
      Direction.Coordinates = {F304353FF304353F0000000000000000}
      Position.Coordinates = {000000C0000000C0CDCCCCBE0000803F}
      Scale.Coordinates = {9A99993E9A99993E9A99993E00000000}
      Up.Coordinates = {00000080000000000000803F00000000}
      Extrusion = 0.200000002980232200
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Lines.Strings = (
        'Z')
      CharacterRange = stcrAlphaNum
    end
    object HUDText: TGLHUDText
      Position.Coordinates = {0000A0400000A040000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
      Text = 'TopText'
      Rotation = 0.000000000000000000
    end
    object HUDTextObj: TGLHUDText
      Position.Coordinates = {0000C0400000A841000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
      Text = 'ObjText'
      Rotation = 0.000000000000000000
    end
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 448
    Top = 8
  end
end
