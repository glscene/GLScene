object FormCombine: TFormCombine
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Texture Combine'
  ClientHeight = 803
  ClientWidth = 1066
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -21
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 168
  TextHeight = 30
  object Image1: TImage
    Left = 14
    Top = 140
    Width = 224
    Height = 224
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Stretch = True
  end
  object Image2: TImage
    Left = 266
    Top = 140
    Width = 224
    Height = 224
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Stretch = True
  end
  object Image3: TImage
    Left = 518
    Top = 140
    Width = 224
    Height = 224
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Stretch = True
  end
  object Label1: TLabel
    Left = 462
    Top = 39
    Width = 124
    Height = 35
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Textures'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -30
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Image4: TImage
    Left = 770
    Top = 140
    Width = 224
    Height = 224
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Stretch = True
  end
  object Label3: TLabel
    Left = 14
    Top = 378
    Width = 56
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Result'
  end
  object Label4: TLabel
    Left = 350
    Top = 378
    Width = 223
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Texture Combiners code'
  end
  object Label2: TLabel
    Left = 14
    Top = 70
    Width = 136
    Height = 30
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Primary Color :'
  end
  object Shape1: TShape
    Left = 182
    Top = 70
    Width = 88
    Height = 32
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Pen.Width = 2
    OnMouseDown = Shape1MouseDown
  end
  object SceneViewer: TGLSceneViewer
    Left = 14
    Top = 410
    Width = 315
    Height = 315
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Camera = GLCamera
    PostRender = SceneViewerPostRender
    Buffer.BackgroundColor = clGray
    Buffer.Lighting = False
    FieldOfView = 144.774841308593800000
    PenAsTouch = False
    TabOrder = 0
  end
  object BUApply: TButton
    Left = 350
    Top = 677
    Width = 131
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Apply'
    TabOrder = 1
    OnClick = BUApplyClick
  end
  object PATex1: TPanel
    Left = 308
    Top = 217
    Width = 142
    Height = 72
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    BevelOuter = bvLowered
    Caption = 'Unavailable'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 8421440
    Font.Height = -21
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
    TabOrder = 2
  end
  object PATex2: TPanel
    Left = 560
    Top = 217
    Width = 142
    Height = 72
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    BevelOuter = bvLowered
    Caption = 'Unavailable'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 8421440
    Font.Height = -21
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
    TabOrder = 3
  end
  object PATex3: TPanel
    Left = 812
    Top = 217
    Width = 142
    Height = 72
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    BevelOuter = bvLowered
    Caption = 'Unavailable'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 8421440
    Font.Height = -21
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
    TabOrder = 4
  end
  object CBTex0: TCheckBox
    Left = 14
    Top = 112
    Width = 100
    Height = 28
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Tex0'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CBTex0Click
  end
  object CBTex1: TCheckBox
    Left = 266
    Top = 112
    Width = 128
    Height = 28
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Tex1'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object CBTex2: TCheckBox
    Left = 518
    Top = 112
    Width = 100
    Height = 28
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Tex2'
    TabOrder = 7
  end
  object CBTex3: TCheckBox
    Left = 770
    Top = 112
    Width = 100
    Height = 28
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Tex3'
    TabOrder = 8
  end
  object Panel1: TPanel
    Left = 350
    Top = 406
    Width = 646
    Height = 254
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    BevelOuter = bvLowered
    BorderWidth = 1
    Caption = 'Panel1'
    TabOrder = 9
    object MECombiner: TMemo
      Left = 2
      Top = 2
      Width = 642
      Height = 250
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
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      BorderStyle = bsNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
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
  object GLScene: TGLScene
    Left = 160
    Top = 15
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
    Left = 100
    Top = 155
  end
  object GLTexCombineShader: TGLTexCombineShader
    DesignTimeEnabled = False
    MaterialLibrary = GLMaterialLibrary
    LibMaterial3Name = 'Tex2'
    LibMaterial4Name = 'Tex3'
    Left = 237
    Top = 298
  end
  object ColorDialog: TColorDialog
    Options = [cdFullOpen, cdSolidColor, cdAnyColor]
    Left = 230
    Top = 20
  end
end
