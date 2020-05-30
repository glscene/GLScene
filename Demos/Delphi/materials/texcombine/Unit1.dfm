object Form1: TForm1
  Left = 117
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Texure Combine'
  ClientHeight = 422
  ClientWidth = 581
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 80
    Width = 128
    Height = 128
    Stretch = True
  end
  object Image2: TImage
    Left = 152
    Top = 80
    Width = 128
    Height = 128
    Stretch = True
  end
  object Image3: TImage
    Left = 296
    Top = 80
    Width = 128
    Height = 128
    Stretch = True
  end
  object Label1: TLabel
    Left = 264
    Top = 22
    Width = 68
    Height = 19
    Caption = 'Textures'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Image4: TImage
    Left = 440
    Top = 80
    Width = 128
    Height = 128
    Stretch = True
  end
  object Label3: TLabel
    Left = 8
    Top = 216
    Width = 30
    Height = 13
    Caption = 'Result'
  end
  object Label4: TLabel
    Left = 200
    Top = 216
    Width = 115
    Height = 13
    Caption = 'Texture Combiners code'
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 67
    Height = 13
    Caption = 'Primary Color :'
  end
  object SceneViewer: TGLSceneViewer
    Left = 8
    Top = 232
    Width = 180
    Height = 180
    Camera = GLCamera
    PostRender = SceneViewerPostRender
    Buffer.BackgroundColor = clGray
    Buffer.Lighting = False
    FieldOfView = 121.890792846679700000
    TabOrder = 0
  end
  object BUApply: TButton
    Left = 200
    Top = 387
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 1
    OnClick = BUApplyClick
  end
  object PATex1: TPanel
    Left = 176
    Top = 124
    Width = 81
    Height = 41
    BevelOuter = bvLowered
    Caption = 'Unavailable'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 8421440
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
    TabOrder = 2
  end
  object PATex2: TPanel
    Left = 320
    Top = 124
    Width = 81
    Height = 41
    BevelOuter = bvLowered
    Caption = 'Unavailable'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 8421440
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
    TabOrder = 3
  end
  object PATex3: TPanel
    Left = 464
    Top = 124
    Width = 81
    Height = 41
    BevelOuter = bvLowered
    Caption = 'Unavailable'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 8421440
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
    TabOrder = 4
  end
  object CBTex0: TCheckBox
    Left = 8
    Top = 64
    Width = 57
    Height = 16
    Caption = 'Tex0'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CBTex0Click
  end
  object CBTex1: TCheckBox
    Left = 152
    Top = 64
    Width = 73
    Height = 16
    Caption = 'Tex1'
    Checked = True
    State = cbChecked
    TabOrder = 6
    OnClick = CBTex0Click
  end
  object CBTex2: TCheckBox
    Left = 296
    Top = 64
    Width = 57
    Height = 16
    Caption = 'Tex2'
    TabOrder = 7
    OnClick = CBTex0Click
  end
  object CBTex3: TCheckBox
    Left = 440
    Top = 64
    Width = 57
    Height = 16
    Caption = 'Tex3'
    TabOrder = 8
    OnClick = CBTex0Click
  end
  object Panel1: TPanel
    Left = 200
    Top = 232
    Width = 369
    Height = 145
    BevelOuter = bvLowered
    BorderWidth = 1
    Caption = 'Panel1'
    TabOrder = 9
    object MECombiner: TMemo
      Left = 2
      Top = 2
      Width = 365
      Height = 141
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
      Align = alClient
      BorderStyle = bsNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
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
    Left = 88
    Top = 38
    Width = 65
    Height = 17
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
