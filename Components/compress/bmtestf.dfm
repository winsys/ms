�
 TFORM1 0=  TPF0TForm1Form1LeftTopoWidthCHeight� Caption!Compressed Bitmaps from Resources
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style ShowHint	OnCreate
FormCreatePixelsPerInchx
TextHeight TImageImage1Left
Top
Width(Height� Stretch	  TMemoMemo1Left
Top
Width/Height� Lines.Strings*Program for testing loading of compressed %resources (see also Selfextr.dpr and Selfxsml.dpr). -Before you do ANYTHING, read the comments at 1the top of the  BMTESTF.PAS unit in this project. %Then follow the instructions therein. TabOrder  TButtonButton1Left
Top� Width\HeightHint+Creates a compressed bitmap file (do first)Caption&CreateTabOrder OnClickButton1Click  TButtonButton2Left� Top� Width\HeightHint?Don't do this until you've followed instructions #1..#5 in fullCaption&LoadTabOrderOnClickButton2Click  TEditMyBitmapLeftlTop� WidthdHeightTabOrderTextMyBitmap  	TCompress	Compress1	RegNumber MakeDirectoriesExceptionOnFileErrorKey LeftpTop(   