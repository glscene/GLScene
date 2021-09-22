object RFaceEditor: TRFaceEditor
  Left = 0
  Top = 0
  Width = 305
  Height = 180
  AutoSize = True
  TabOrder = 0
  PixelsPerInch = 96
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 60
    Height = 20
    Caption = 'Shininess'
  end
  object PageControl: TPageControl
    Left = 0
    Top = 50
    Width = 305
    Height = 130
    ActivePage = TSEmission
    Images = ImageList
    MultiLine = True
    TabOrder = 0
    object TSAmbient: TTabSheet
      BorderWidth = 3
      Caption = 'Ambient'
      inline CEAmbiant: TRColorEditor
        Left = 0
        Top = 0
        Width = 289
        Height = 95
        AutoSize = True
        TabOrder = 0
        TabStop = True
      end
    end
    object TSDiffuse: TTabSheet
      BorderWidth = 3
      Caption = 'Diffuse'
      ImageIndex = 1
      inline CEDiffuse: TRColorEditor
        Left = 0
        Top = 0
        Width = 289
        Height = 95
        AutoSize = True
        TabOrder = 0
        TabStop = True
      end
    end
    object TSEmission: TTabSheet
      BorderWidth = 3
      Caption = 'Emission'
      ImageIndex = 2
      inline CEEmission: TRColorEditor
        Left = 0
        Top = 0
        Width = 289
        Height = 95
        AutoSize = True
        TabOrder = 0
        TabStop = True
      end
    end
    object TSSpecular: TTabSheet
      BorderWidth = 3
      Caption = 'Specular'
      ImageIndex = 3
      inline CESpecular: TRColorEditor
        Left = 0
        Top = 0
        Width = 289
        Height = 95
        AutoSize = True
        TabOrder = 0
        TabStop = True
      end
    end
  end
  object Edit: TEdit
    Left = 219
    Top = 3
    Width = 41
    Height = 21
    TabOrder = 1
    Text = '0'
  end
  object TrackBar: TTrackBar
    Left = 63
    Top = 3
    Width = 150
    Height = 20
    Max = 255
    PageSize = 10
    Frequency = 32
    TabOrder = 2
    ThumbLength = 10
    TickMarks = tmTopLeft
  end
  object ImageList: TImageList
    Left = 264
    Top = 8
  end
end
