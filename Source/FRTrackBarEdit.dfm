object RTrackBarEdit: TRTrackBarEdit
  Left = 0
  Top = 0
  Width = 203
  Height = 29
  TabOrder = 0
  object TrackBar: TTrackBar
    Left = 0
    Top = 0
    Width = 150
    Height = 20
    Max = 255
    PageSize = 10
    Frequency = 32
    TabOrder = 0
    ThumbLength = 10
    TickMarks = tmTopLeft
    OnChange = TrackBarChange
  end
  object Edit: TEdit
    Left = 152
    Top = 0
    Width = 41
    Height = 28
    TabOrder = 1
    Text = '0'
    OnChange = EditChange
    OnExit = TrackBarChange
  end
end
