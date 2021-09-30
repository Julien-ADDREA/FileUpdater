object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Updater'
  ClientHeight = 100
  ClientWidth = 300
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelAction: TLabel
    Left = 25
    Top = 18
    Width = 16
    Height = 13
    Caption = 'n/a'
  end
  object LabelDetails: TLabel
    Left = 25
    Top = 69
    Width = 3
    Height = 13
  end
  object ProgressBarAction: TProgressBar
    Left = 25
    Top = 53
    Width = 250
    Height = 10
    TabOrder = 0
  end
  object ProgressBarGlobal: TProgressBar
    Left = 25
    Top = 37
    Width = 250
    Height = 10
    TabOrder = 1
  end
end
