object DataModule1: TDataModule1
  OnCreate = DataModuleCreate
  Height = 351
  Width = 680
  PixelsPerInch = 168
  object GLScene1: TGLScene
    Left = 22
    Top = 11
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000484200004842000048420000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Teapot1: TGLTeapot
      Material.Texture.ImageClassName = 'TGLCubeMapImage'
      Material.Texture.TextureMode = tmReplace
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Teapot1
      Position.Coordinates = {0000404000000040000000400000803F}
    end
  end
end
