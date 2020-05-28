object Form1: TForm1
  Left = 201
  Top = 129
  Caption = 'Simple Spherical Pano Viewer - Use mouse or arrow keys to pan'
  ClientHeight = 345
  ClientWidth = 634
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnKeyDown = FormKeyDown
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 46
    Width = 634
    Height = 299
    Cursor = crHandPoint
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Camera = GLCamera1
    Buffer.FaceCulling = False
    Buffer.Lighting = False
    FieldOfView = 150.041778564453100000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 634
    Height = 46
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object LabelYaw: TLabel
      Left = 249
      Top = 8
      Width = 35
      Height = 15
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Yaw: 0'
    end
    object LabelPitch: TLabel
      Left = 249
      Top = 24
      Width = 40
      Height = 15
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Pitch: 0'
    end
    object Label1: TLabel
      Left = 113
      Top = 8
      Width = 71
      Height = 15
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Focal Length'
    end
    object Label2: TLabel
      Left = 329
      Top = 8
      Width = 188
      Height = 30
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Hold left mouse button to pan'#13#10'Zoom in/out with the mouse wheel'
    end
    object BtnLoad: TButton
      Left = 8
      Top = 8
      Width = 89
      Height = 30
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Load Image...'
      TabOrder = 0
      OnClick = BtnLoadClick
    end
    object TrackBar1: TTrackBar
      Left = 108
      Top = 23
      Width = 126
      Height = 16
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Max = 100
      Min = 10
      Frequency = 10
      Position = 40
      TabOrder = 1
      ThumbLength = 10
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = TrackBar1Change
    end
  end
  object GLScene1: TGLScene
    Left = 48
    Top = 64
    object GLCamera1: TGLCamera
      DepthOfView = 200.000000000000000000
      FocalLength = 40.000000000000000000
      Left = 328
      Top = 216
    end
    object Sphere1: TGLSphere
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial'
      Normals = nsFlat
      Radius = 2.000000000000000000
      Slices = 64
    end
  end
  object OpenPictureDialog1: TOpenPictureDialog
    DefaultExt = '*.jpg'
    Left = 136
    Top = 64
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Tag = 0
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureFormat = tfRGB
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
        TextureOffset.Coordinates = {000000000000803F0000000000000000}
        TextureScale.Coordinates = {000080BF0000803F0000803F00000000}
      end>
    Left = 48
    Top = 128
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 136
    Top = 128
  end
end
