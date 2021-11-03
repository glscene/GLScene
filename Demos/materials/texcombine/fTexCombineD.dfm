object FormTexCombine: TFormTexCombine
  Left = 117
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Texure Combine'
  ClientHeight = 528
  ClientWidth = 726
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Image1: TImage
    Left = 10
    Top = 100
    Width = 160
    Height = 160
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Stretch = True
  end
  object Image2: TImage
    Left = 190
    Top = 100
    Width = 160
    Height = 160
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Stretch = True
  end
  object Image3: TImage
    Left = 370
    Top = 100
    Width = 160
    Height = 160
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Stretch = True
  end
  object Label1: TLabel
    Left = 330
    Top = 28
    Width = 87
    Height = 24
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Textures'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Image4: TImage
    Left = 550
    Top = 100
    Width = 160
    Height = 160
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Stretch = True
  end
  object Label3: TLabel
    Left = 10
    Top = 270
    Width = 38
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Result'
  end
  object Label4: TLabel
    Left = 250
    Top = 270
    Width = 147
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Texture Combiners code'
  end
  object Label2: TLabel
    Left = 10
    Top = 50
    Width = 87
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Primary Color :'
  end
  object SceneViewer: TGLSceneViewer
    Left = 10
    Top = 290
    Width = 225
    Height = 225
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera
    PostRender = SceneViewerPostRender
    Buffer.BackgroundColor = clGray
    Buffer.Lighting = False
    FieldOfView = 132.075027465820300000
    PenAsTouch = False
    TabOrder = 0
  end
  object BUApply: TButton
    Left = 250
    Top = 484
    Width = 94
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Apply'
    TabOrder = 1
    OnClick = BUApplyClick
  end
  object PATex1: TPanel
    Left = 220
    Top = 155
    Width = 101
    Height = 51
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    BevelOuter = bvLowered
    Caption = 'Unavailable'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 8421440
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
    TabOrder = 2
  end
  object PATex2: TPanel
    Left = 400
    Top = 155
    Width = 101
    Height = 51
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    BevelOuter = bvLowered
    Caption = 'Unavailable'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 8421440
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
    TabOrder = 3
  end
  object PATex3: TPanel
    Left = 580
    Top = 155
    Width = 101
    Height = 51
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    BevelOuter = bvLowered
    Caption = 'Unavailable'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 8421440
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
    TabOrder = 4
  end
  object CBTex0: TCheckBox
    Left = 10
    Top = 80
    Width = 71
    Height = 20
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Tex0'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CBTex0Click
  end
  object CBTex1: TCheckBox
    Left = 190
    Top = 80
    Width = 91
    Height = 20
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Tex1'
    Checked = True
    State = cbChecked
    TabOrder = 6
    OnClick = CBTex0Click
  end
  object CBTex2: TCheckBox
    Left = 370
    Top = 80
    Width = 71
    Height = 20
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Tex2'
    TabOrder = 7
    OnClick = CBTex0Click
  end
  object CBTex3: TCheckBox
    Left = 550
    Top = 80
    Width = 71
    Height = 20
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Tex3'
    TabOrder = 8
    OnClick = CBTex0Click
  end
  object Panel1: TPanel
    Left = 250
    Top = 290
    Width = 461
    Height = 181
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    BevelOuter = bvLowered
    BorderWidth = 1
    Caption = 'Panel1'
    TabOrder = 9
    object MECombiner: TMemo
      Left = 2
      Top = 2
      Width = 457
      Height = 177
      Hint = 
        'Syntax Examples:'#13#10#13#10'   Tex1:=Tex0;   // replace texture 1 with t' +
        'exture 0'#13#10'   Tex1:=Tex0+Tex1; // additive blending between textu' +
        'res 0 and 1'#13#10'   Tex1:=Tex0-Tex1; // subtractive blending between' +
        ' textures 0 and 1'#13#10'   Tex1:=Tex0*Tex1; // modulation between tex' +
        'tures 0 and 1'#13#10'   Tex1:=Tex0+Tex1-0.5; // signed additive blendi' +
        'ng between textures 0 and 1'#13#10'   Tex1:=Interpolate(Tex0, Tex1, Pr' +
        'imaryColor); // interpolation between textures 0 and 1 using pri' +
        'mary color as factor'#13#10'   Tex1:=Dot3(Tex0, Tex1); // dot3 product' +
        ' between textures 0 and 1'
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      BorderStyle = bsNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        'Tex0:=Tex0;'
        'Tex1:=Tex0+Tex1;')
      ParentFont = False
      ParentShowHint = False
      ScrollBars = ssBoth
      ShowHint = True
      TabOrder = 0
    end
  end
  object PAPrimary: TPanel
    Left = 101
    Top = 48
    Width = 30
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Color = 13421772
    TabOrder = 10
    OnClick = PAPrimaryClick
    OnDblClick = PAPrimaryClick
  end
  object GLScene: TGLScene
    Left = 64
    Top = 272
    object GLDummyCube: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLHUDSprite: TGLHUDSprite
      Material.MaterialLibrary = GLMaterialLibrary
      Material.LibMaterialName = 'Tex0'
      Position.Coordinates = {0000B4420000B442000000000000803F}
      Width = 128.000000000000000000
      Height = 128.000000000000000000
      Rotation = 0.000000000000000000
    end
    object GLCamera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube
      Position.Coordinates = {00000000000000000000A0400000803F}
    end
  end
  object GLMaterialLibrary: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Tex0'
        Tag = 0
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
        Texture2Name = 'Tex1'
        Shader = GLTexCombineShader
      end
      item
        Name = 'Tex1'
        Tag = 0
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'Tex2'
        Tag = 0
      end
      item
        Name = 'Tex3'
        Tag = 0
      end>
    Left = 32
    Top = 272
  end
  object GLTexCombineShader: TGLTexCombineShader
    DesignTimeEnabled = False
    MaterialLibrary = GLMaterialLibrary
    LibMaterial3Name = 'Tex2'
    LibMaterial4Name = 'Tex3'
    Left = 96
    Top = 272
  end
  object ColorDialog: TColorDialog
    Options = [cdFullOpen, cdSolidColor, cdAnyColor]
    Left = 184
    Top = 16
  end
end
