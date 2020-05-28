object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'MultiTexture'
  ClientHeight = 415
  ClientWidth = 550
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 411
    Top = 56
    Width = 128
    Height = 128
    Stretch = True
    OnClick = Image1Click
  end
  object Image2: TImage
    Left = 411
    Top = 208
    Width = 128
    Height = 128
    Stretch = True
    OnClick = Image2Click
  end
  object Label1: TLabel
    Left = 416
    Top = 40
    Width = 70
    Height = 13
    Caption = 'Texture Map 1'
  end
  object Label2: TLabel
    Left = 416
    Top = 192
    Width = 70
    Height = 13
    Caption = 'Texture Map 2'
  end
  object Label3: TLabel
    Left = 416
    Top = 368
    Width = 57
    Height = 13
    Caption = 'Map 2 Scale'
  end
  object Label4: TLabel
    Left = 432
    Top = 8
    Width = 93
    Height = 19
    Caption = 'MultiTexture'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 8
    Top = 8
    Width = 393
    Height = 393
    Camera = GLCamera1
    FieldOfView = 151.447769165039100000
    Enabled = False
    TabOrder = 0
  end
  object TrackBar1: TTrackBar
    Left = 413
    Top = 378
    Width = 126
    Height = 25
    Max = 30
    Min = 5
    Position = 10
    TabOrder = 1
    ThumbLength = 10
    OnChange = TrackBar1Change
  end
  object CBClampTex2: TCheckBox
    Left = 416
    Top = 344
    Width = 97
    Height = 17
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
