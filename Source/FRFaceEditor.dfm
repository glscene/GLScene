object RFaceEditor: TRFaceEditor
  Left = 0
  Top = 0
  Width = 355
  Height = 247
  TabOrder = 0
  object Label1: TLabel
    Left = 3
    Top = 214
    Width = 60
    Height = 20
    Caption = 'Shininess'
  end
  object PageControl: TPageControl
    Left = 0
    Top = 56
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
        Width = 341
        Height = 98
        AutoSize = True
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 341
        ExplicitHeight = 98
        inherited ColorEditorPaintBox: TPaintBox
          Width = 341
          Height = 98
          ExplicitWidth = 341
          ExplicitHeight = 98
        end
        inherited Panel1: TPanel
          Left = 52
          Top = 3
          ExplicitLeft = 52
          ExplicitTop = 3
        end
        inherited ColorDialog: TColorDialog
          Left = 264
          Top = 72
        end
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
  inline TBEShininess: TRTrackBarEdit
    Left = 50
    Top = 0
    Width = 201
    Height = 33
    TabOrder = 1
    ExplicitLeft = 50
    ExplicitWidth = 201
    ExplicitHeight = 33
    inherited TrackBar: TTrackBar
      Left = -2
      Top = 1
      Max = 128
      Frequency = 16
      OnChange = TBEShininessTrackBarChange
      ExplicitLeft = -2
      ExplicitTop = 1
    end
    inherited Edit: TEdit
      Left = 160
      ExplicitLeft = 160
    end
  end
  object TrackBar: TTrackBar
    Left = 69
    Top = 213
    Width = 150
    Height = 20
    Max = 128
    PageSize = 10
    Frequency = 16
    TabOrder = 2
    ThumbLength = 10
    TickMarks = tmTopLeft
    OnChange = TrackBarChange
  end
  object Edit: TEdit
    Left = 222
    Top = 213
    Width = 41
    Height = 28
    TabOrder = 3
    Text = '0'
    OnChange = EditChange
    OnExit = TrackBarChange
  end
  object ImageList: TImageList
    Left = 264
    Top = 8
  end
end
