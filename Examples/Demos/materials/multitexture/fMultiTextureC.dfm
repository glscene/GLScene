object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'MultiTexture'
  ClientHeight = 519
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 17
  object Image1: TImage
    Left = 514
    Top = 70
    Width = 160
    Height = 160
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Stretch = True
    OnClick = Image1Click
  end
  object Image2: TImage
    Left = 514
    Top = 260
    Width = 160
    Height = 160
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Stretch = True
    OnClick = Image2Click
  end
  object Label1: TLabel
    Left = 520
    Top = 50
    Width = 89
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Texture Map 1'
  end
  object Label2: TLabel
    Left = 520
    Top = 240
    Width = 89
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Texture Map 2'
  end
  object Label3: TLabel
    Left = 520
    Top = 460
    Width = 72
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Map 2 Scale'
  end
  object Label4: TLabel
    Left = 540
    Top = 10
    Width = 118
    Height = 24
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'MultiTexture'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 10
    Top = 10
    Width = 491
    Height = 491
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    FieldOfView = 156.976486206054700000
    PenAsTouch = False
    Enabled = False
    TabOrder = 0
  end
  object TrackBar1: TTrackBar
    Left = 516
    Top = 473
    Width = 158
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Max = 30
    Min = 5
    Position = 10
    TabOrder = 1
    ThumbLength = 13
    OnChange = TrackBar1Change
  end
  object CBClampTex2: TCheckBox
    Left = 520
    Top = 430
    Width = 121
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Clamp Texture 2'
    TabOrder = 2
  end
  object GLScene1: TGLScene
    Left = 48
    Top = 32
    object Plane1: TGLPlane
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'Base'
      Position.Coordinates = {0000000000000000000080BF0000803F}
      Height = 1.000000000000000000
      Width = 1.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      Left = 192
      Top = 192
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Base'
        Tag = 0
        Material.Texture.TextureMode = tmReplace
        Material.Texture.Disabled = False
        Texture2Name = 'Second'
      end
      item
        Name = 'Second'
        Tag = 0
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end>
    TexturePaths = '..\..\..\..\media\'
    Left = 152
    Top = 32
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 264
    Top = 32
  end
end
