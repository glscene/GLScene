object Form1: TForm1
  Left = 239
  Top = 100
  Caption = 'Multi Pass'
  ClientHeight = 325
  ClientWidth = 430
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 430
    Height = 325
    Camera = GLCamera1
    FieldOfView = 145.794540405273400000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object BUBind: TButton
    Left = 168
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Bind Shaders'
    TabOrder = 1
    OnClick = BUBindClick
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00002041000000410000E0400000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Torus1: TGLTorus
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial'
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      MajorRadius = 2.500000000000000000
      MinorRadius = 1.500000000000000000
      StopAngle = 360.000000000000000000
      Parts = [toSides, toStartDisk, toStopDisk]
    end
    object Sphere1: TGLSphere
      ShowAxes = True
      Radius = 0.500000000000000000
    end
    object GLAnnulus1: TGLAnnulus
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial1'
      Position.Coordinates = {0000E04000000000000000000000803F}
      Scale.Coordinates = {00000040000000400000004000000000}
      BottomRadius = 0.500000000000000000
      Height = 1.000000000000000000
      BottomInnerRadius = 0.300000011920929000
      TopInnerRadius = 0.300000011920929000
      TopRadius = 0.500000000000000000
    end
    object GLAnnulus2: TGLAnnulus
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial2'
      Position.Coordinates = {0000E0C000000000000000000000803F}
      Scale.Coordinates = {00000040000000400000004000000000}
      BottomRadius = 0.500000000000000000
      Height = 1.000000000000000000
      BottomInnerRadius = 0.300000011920929000
      TopInnerRadius = 0.300000011920929000
      TopRadius = 0.500000000000000000
    end
    object GLCube1: TGLCube
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial2'
      Position.Coordinates = {00000000000000000000E0400000803F}
      Scale.Coordinates = {00000040000000400000004000000000}
    end
    object GLSphere1: TGLSphere
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial1'
      Position.Coordinates = {00000000000000000000E0C00000803F}
      Radius = 1.500000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Torus1
      Position.Coordinates = {00006041000020410000C0400000803F}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Tag = 0
      end
      item
        Name = 'LibMaterial1'
        Tag = 0
      end
      item
        Name = 'LibMaterial2'
        Tag = 0
      end>
    Left = 16
    Top = 48
  end
end
