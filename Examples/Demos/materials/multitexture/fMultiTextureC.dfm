object FormMultiTexture: TFormMultiTexture
  Left = 0
  Top = 0
  Margins.Left = 4
  Margins.Top = 4
  Margins.Right = 4
  Margins.Bottom = 4
  Caption = 'MultiTexture'
  ClientHeight = 727
  ClientWidth = 972
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 168
  TextHeight = 23
  object Image1: TImage
    Left = 720
    Top = 98
    Width = 224
    Height = 224
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Stretch = True
    OnClick = Image1Click
  end
  object Image2: TImage
    Left = 720
    Top = 364
    Width = 224
    Height = 224
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Stretch = True
    OnClick = Image2Click
  end
  object Label1: TLabel
    Left = 728
    Top = 70
    Width = 122
    Height = 23
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Caption = 'Texture Map 1'
  end
  object Label2: TLabel
    Left = 728
    Top = 336
    Width = 122
    Height = 23
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Caption = 'Texture Map 2'
  end
  object Label3: TLabel
    Left = 728
    Top = 644
    Width = 102
    Height = 23
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Caption = 'Map 2 Scale'
  end
  object Label4: TLabel
    Left = 756
    Top = 14
    Width = 168
    Height = 33
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Caption = 'MultiTexture'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -28
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 14
    Top = 14
    Width = 687
    Height = 687
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Camera = GLCamera1
    FieldOfView = 163.436340332031300000
    PenAsTouch = False
    Enabled = False
    TabOrder = 0
  end
  object TrackBar1: TTrackBar
    Left = 722
    Top = 662
    Width = 222
    Height = 44
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Max = 30
    Min = 5
    Position = 10
    TabOrder = 1
    ThumbLength = 18
    OnChange = TrackBar1Change
  end
  object CBClampTex2: TCheckBox
    Left = 728
    Top = 602
    Width = 169
    Height = 29
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
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
