object Form1: TForm1
  Left = 237
  Top = 172
  Caption = 'Mandelbrot'
  ClientHeight = 497
  ClientWidth = 639
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  TextHeight = 13
  object Viewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 639
    Height = 497
    Camera = GLCamera
    Buffer.BackgroundColor = 16492697
    FieldOfView = 157.247146606445300000
    PenAsTouch = False
    Align = alClient
    TabOrder = 0
  end
  object Scene: TGLScene
    Left = 16
    Top = 16
    object Mandelbrot: TGLDirectOpenGL
      UseBuildList = False
      OnRender = MandelbrotRender
      Blend = False
    end
    object GLHUDText: TGLHUDText
      Position.Coordinates = {0000204100002041000000000000803F}
      BitmapFont = GLWindowsBitmapFont
      Rotation = 0.000000000000000000
    end
    object GLCamera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Mandelbrot
    end
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 344
    Top = 16
  end
  object GLCadencer: TGLCadencer
    Scene = Scene
    OnProgress = GLCadencerProgress
    Left = 96
    Top = 16
  end
  object GLMatLib: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Tag = 0
        Material.Texture.Disabled = False
      end>
    Left = 24
    Top = 72
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'bmp'
    Filter = 
      'All graphic files|*.bmp; *.jpg; *.jpeg; *.tga|JPEG|*.jpg; *.jpeg' +
      '|TGA|*.tga|Bitmaps|*.bmp'
    Title = 'Open Bitmap'
    Left = 472
    Top = 16
  end
  object GLWindowsBitmapFont: TGLWindowsBitmapFont
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    Left = 192
    Top = 16
  end
end
