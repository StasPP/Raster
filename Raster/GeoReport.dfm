object Form3: TForm3
  Left = 495
  Top = 246
  BorderStyle = bsDialog
  Caption = #1054#1090#1095#1077#1090' '#1087#1086' '#1087#1088#1080#1074#1103#1082#1077
  ClientHeight = 488
  ClientWidth = 462
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 462
    Height = 438
    Align = alClient
    Lines.Strings = (
      ' ')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 438
    Width = 462
    Height = 50
    Align = alBottom
    TabOrder = 1
    object SaveAsdb: TSpeedButton
      Left = 85
      Top = 8
      Width = 177
      Height = 33
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1082#1072#1082' '#1087#1086#1076#1083#1086#1078#1082#1091
      Glyph.Data = {
        A2010000424DA201000000000000760000002800000018000000190000000100
        0400000000002C01000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD80000000000000DDDDD
        DDDDD03300000088030DDDDDDDDDD03300000088030DDDDD0000003300000088
        030DDDDD0F0FF03300000000030DD7000F0F403333333333330DD7CC0F0FF033
        00000000330DD7CC0F0F403088888888030DD7CC0F0FF03088888888030DD7CC
        0F0F403088888888030DD7FE0F0FF03088888888030DD7FF0F0F403088888888
        000DD7FE0F0FF03088888888080DD7FF0F0F400000000000000DD7FF0F0FFFFF
        0FFFFF0E20DDD7FE000000000000000FE0DDD7FF0F0FFFFF0FFFFF0EF0DDD7FE
        000000000000000FE0DDD7FEFEFEFEFEFEFEFEFEF0DDD7FFEFEFEFEFEFEFEFEF
        E0DDD7FFFFFFFFFFFFFFFFFFF0DDD777777777777777777777DDDDDDDDDDDDDD
        DDDDDDDDDDDD}
      OnClick = SaveAsdbClick
    end
    object Button2: TButton
      Left = 272
      Top = 8
      Width = 177
      Height = 33
      Caption = #1047#1072#1082#1088#1099#1090#1100
      TabOrder = 0
      OnClick = Button2Click
    end
  end
end
