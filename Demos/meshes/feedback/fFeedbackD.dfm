object FormFeedback: TFormFeedback
  Left = 192
  Top = 107
  Caption = 'Feedback'
  ClientHeight = 221
  ClientWidth = 445
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  PixelsPerInch = 120
  TextHeight = 16
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 445
    Height = 221
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Camera = GLCamera1
    Buffer.AmbientColor.Color = {0000000000000000000000000000803F}
    Buffer.FaceCulling = False
    FieldOfView = 131.307571411132800000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Button1: TButton
    Left = 190
    Top = 10
    Width = 94
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Build Mesh'
    TabOrder = 1
    OnClick = Button1Click
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 8
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = GLDummyCube1
        Position.Coordinates = {0000803F00000040000040400000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          SpotCutOff = 180.000000000000000000
        end
      end
    end
    object GLFreeForm1: TGLFreeForm
    end
    object GLFeedback1: TGLFeedback
      MaxBufferSize = 1048576
      Active = False
      Mode = fm3DColorTexture
      Visible = False
      object MeshObject1: TGLDummyCube
        Visible = False
        CubeSize = 1.000000000000000000
        object GLCube1: TGLCube
          Material.FrontProperties.Emission.Color = {9A93133FEBE0E03EE4DB5B3F0000803F}
          Position.Coordinates = {CDCC0CBFCDCC0CBF000000000000803F}
        end
        object GLDodecahedron1: TGLDodecahedron
          Material.FrontProperties.Emission.Color = {9A99593F9A99593FCDCCCC3D0000803F}
          Position.Coordinates = {CDCC0C3FCDCC0CBF000000000000803F}
        end
      end
      object MeshObject2: TGLDummyCube
        Visible = False
        CubeSize = 1.000000000000000000
        object GLSphere1: TGLSphere
          Material.FrontProperties.Emission.Color = {000000000000003F000000000000803F}
          Position.Coordinates = {00000000CDCC0C3F000000000000803F}
          Radius = 0.500000000000000000
        end
      end
    end
  end
end
