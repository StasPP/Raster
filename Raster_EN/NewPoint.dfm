object Form2: TForm2
  Left = 421
  Top = 360
  BorderStyle = bsToolWindow
  Caption = 'New Point'
  ClientHeight = 281
  ClientWidth = 308
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 161
    Width = 308
    Height = 79
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Label5: TLabel
      Left = 0
      Top = 10
      Width = 145
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'Geo/Grid Coordinates:'
    end
    object Label3: TLabel
      Left = 5
      Top = 60
      Width = 80
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'X:'
    end
    object Label4: TLabel
      Left = 5
      Top = 36
      Width = 80
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Y:'
    end
    object XX: TEdit
      Left = 93
      Top = 56
      Width = 105
      Height = 21
      TabOrder = 0
      Text = '0'
    end
    object YY: TEdit
      Left = 93
      Top = 32
      Width = 105
      Height = 21
      TabOrder = 1
      Text = '0'
    end
    object Acont: TCheckBox
      Left = 204
      Top = 48
      Width = 92
      Height = 17
      Caption = 'Autocontrol'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 308
    Height = 161
    Align = alTop
    TabOrder = 1
    object Image1: TImage
      Left = 16
      Top = 16
      Width = 100
      Height = 100
    end
    object Bevel1: TBevel
      Left = 15
      Top = 15
      Width = 102
      Height = 102
    end
    object Label8: TLabel
      Left = 144
      Top = 32
      Width = 153
      Height = 13
      AutoSize = False
      Caption = 'Image coordinates:'
    end
    object Label1: TLabel
      Left = 144
      Top = 60
      Width = 10
      Height = 13
      Caption = 'X:'
    end
    object Label2: TLabel
      Left = 144
      Top = 92
      Width = 10
      Height = 13
      Caption = 'Y:'
    end
    object SpeedButton11: TSpeedButton
      Left = 64
      Top = 134
      Width = 30
      Height = 22
      Glyph.Data = {
        4E010000424D4E01000000000000760000002800000011000000120000000100
        040000000000D800000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        DDDDD0000000DDDDDDDDDDDDD44DD0000000DDDDDDDDDDDD4844D0000000DDDD
        DDDDDDD48444D0000000DDDDDDDDDD48444DD0000000DDDDD000048444DDD000
        0000DDD00888F7444DDDD0000000DD0788888F707DDDD0000000DD08888888F0
        DDDDD0000000D088888888880DDDD0000000D080000000080DDDD0000000D087
        000000080DDDD0000000D08F888888880DDDD0000000DD0FF8888880DDDDD000
        0000DD07FFF88870DDDDD0000000DDD00888800DDDDDD0000000DDDDD0000DDD
        DDDDD0000000DDDDDDDDDDDDDDDDD0000000}
      OnClick = SpeedButton11Click
    end
    object SpeedButton10: TSpeedButton
      Left = 32
      Top = 134
      Width = 30
      Height = 22
      Glyph.Data = {
        4E010000424D4E01000000000000760000002800000011000000120000000100
        040000000000D800000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        DDDDD0000000DDDDDDDDDDDDD44DD0000000DDDDDDDDDDDD4844D0000000DDDD
        DDDDDDD48444D0000000DDDDDDDDDD48444DD0000000DDDDD000048444DDD000
        0000DDD00888F7444DDDD0000000DD0788008F707DDDD0000000DD08880088F0
        DDDDD0000000D088880088880DDDD0000000D080000000080DDDD0000000D087
        000000080DDDD0000000D08F880088880DDDD0000000DD0FF8008880DDDDD000
        0000DD07FF708870DDDDD0000000DDD00888800DDDDDD0000000DDDDD0000DDD
        DDDDD0000000DDDDDDDDDDDDDDDDD0000000}
      OnClick = SpeedButton10Click
    end
    object Label7: TLabel
      Left = 144
      Top = 10
      Width = 59
      Height = 13
      Caption = 'New Point'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object SpeedButton1: TSpeedButton
      Left = 33
      Top = 5
      Width = 65
      Height = 9
      Flat = True
      Glyph.Data = {
        16010000424D16010000000000007600000028000000190000000A0000000100
        040000000000A000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        DDDDDDDDDDDDD0000000D77777777777777777777777D0000000D80000000000
        000000000007D0000000DDD7000000000000000007DDD0000000DDDDD7000000
        00000007DDDDD0000000DDDDDDD70000000007DDDDDDD0000000DDDDDDDDD700
        0007DDDDDDDDD0000000DDDDDDDDDDD707DDDDDDDDDDD0000000DDDDDDDDDDDD
        DDDDDDDDDDDDD0000000DDDDDDDDDDDDDDDDDDDDDDDDD0000000}
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 33
      Top = 118
      Width = 65
      Height = 9
      Flat = True
      Glyph.Data = {
        16010000424D16010000000000007600000028000000190000000A0000000100
        040000000000A000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        DDDDDDDDDDDDD0000000DDDDDDDDDDDDDDDDDDDDDDDDD0000000DDDDDDDDDDD7
        07DDDDDDDDDDD0000000DDDDDDDDD7000007DDDDDDDDD0000000DDDDDDD70000
        000007DDDDDDD0000000DDDDD700000000000007DDDDD0000000DDD700000000
        0000000007DDD0000000D80000000000000000000007D0000000D77777777777
        777777777777D0000000DDDDDDDDDDDDDDDDDDDDDDDDD0000000}
      OnClick = SpeedButton2Click
    end
    object SpeedButton3: TSpeedButton
      Left = 118
      Top = 33
      Width = 9
      Height = 65
      Flat = True
      Glyph.Data = {
        3E010000424D3E0100000000000076000000280000000A000000190000000100
        040000000000C800000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDD00
        0000D78DDDDDDD000000D70DDDDDDD000000D707DDDDDD000000D700DDDDDD00
        0000D7007DDDDD000000D7000DDDDD000000D70007DDDD000000D70000DDDD00
        0000D700007DDD000000D700000DDD000000D7000007DD000000D7000000DD00
        0000D7000007DD000000D700000DDD000000D700007DDD000000D70000DDDD00
        0000D70007DDDD000000D7000DDDDD000000D7007DDDDD000000D700DDDDDD00
        0000D707DDDDDD000000D70DDDDDDD000000D77DDDDDDD000000DDDDDDDDDD00
        0000}
      OnClick = SpeedButton3Click
    end
    object SpeedButton4: TSpeedButton
      Left = 5
      Top = 33
      Width = 9
      Height = 65
      Flat = True
      Glyph.Data = {
        3E010000424D3E0100000000000076000000280000000A000000190000000100
        040000000000C800000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDD00
        0000DDDDDDD87D000000DDDDDDD07D000000DDDDDD707D000000DDDDDD007D00
        0000DDDDD7007D000000DDDDD0007D000000DDDD70007D000000DDDD00007D00
        0000DDD700007D000000DDD000007D000000DD7000007D000000DD0000007D00
        0000DD7000007D000000DDD000007D000000DDD700007D000000DDDD00007D00
        0000DDDD70007D000000DDDDD0007D000000DDDDD7007D000000DDDDDD007D00
        0000DDDDDD707D000000DDDDDDD07D000000DDDDDDD77D000000DDDDDDDDDD00
        0000}
      OnClick = SpeedButton4Click
    end
    object X: TSpinEdit
      Left = 160
      Top = 56
      Width = 121
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = XChange
    end
    object Y: TSpinEdit
      Left = 160
      Top = 88
      Width = 121
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = XChange
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 240
    Width = 308
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Button2: TButton
      Left = 176
      Top = 8
      Width = 121
      Height = 25
      Caption = 'Cancel'
      TabOrder = 0
      OnClick = Button2Click
    end
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 161
      Height = 25
      Caption = 'OK'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
end
