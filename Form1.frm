VERSION 5.00
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "COMCTL32.OCX"
Begin VB.Form Form1 
   Caption         =   "VB FastImage Processing Demo"
   ClientHeight    =   7575
   ClientLeft      =   165
   ClientTop       =   735
   ClientWidth     =   9660
   LinkTopic       =   "Form1"
   ScaleHeight     =   7575
   ScaleWidth      =   9660
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton Command12 
      Caption         =   "CallBack Sample"
      Height          =   615
      Left            =   8160
      TabIndex        =   13
      Top             =   720
      Width           =   1335
   End
   Begin ComctlLib.ProgressBar ProgressBar1 
      Height          =   375
      Left            =   120
      TabIndex        =   12
      Top             =   6600
      Width           =   7935
      _ExtentX        =   13996
      _ExtentY        =   661
      _Version        =   327682
      Appearance      =   1
   End
   Begin VB.CommandButton Command11 
      Caption         =   "ligh soften kernel Example"
      Height          =   495
      Left            =   8160
      TabIndex        =   11
      Top             =   6120
      Width           =   1335
   End
   Begin VB.CommandButton Command10 
      Caption         =   "Medium soften kernel Example"
      Height          =   495
      Left            =   8160
      TabIndex        =   10
      Top             =   5520
      Width           =   1335
   End
   Begin VB.CommandButton Command9 
      Caption         =   "Clear"
      Height          =   255
      Left            =   8520
      TabIndex        =   9
      Top             =   360
      Width           =   735
   End
   Begin VB.CommandButton Command8 
      Caption         =   "Emboss kernel Example"
      Height          =   495
      Left            =   8160
      TabIndex        =   8
      Top             =   4320
      Width           =   1335
   End
   Begin VB.CommandButton Command7 
      Caption         =   "FishEye"
      Height          =   375
      Left            =   8160
      TabIndex        =   7
      Top             =   3840
      Width           =   1335
   End
   Begin VB.CommandButton Command6 
      Caption         =   "Blur"
      Height          =   375
      Left            =   8160
      TabIndex        =   6
      Top             =   3360
      Width           =   1335
   End
   Begin VB.CommandButton Command5 
      Caption         =   "Lighten"
      Height          =   375
      Left            =   8160
      TabIndex        =   5
      Top             =   2880
      Width           =   1335
   End
   Begin VB.CommandButton Command4 
      Caption         =   "Darken"
      Height          =   375
      Left            =   8160
      TabIndex        =   4
      Top             =   2400
      Width           =   1335
   End
   Begin VB.CommandButton Command3 
      Caption         =   "GrayScale"
      Height          =   375
      Left            =   8160
      TabIndex        =   3
      Top             =   1920
      Width           =   1335
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Heavy soften kernel Example"
      Height          =   495
      Left            =   8160
      TabIndex        =   2
      Top             =   4920
      Width           =   1335
   End
   Begin VB.CommandButton Command1 
      Caption         =   "InvertDC"
      Height          =   375
      Left            =   8160
      TabIndex        =   1
      Top             =   1440
      Width           =   1335
   End
   Begin VB.PictureBox Picture1 
      Height          =   6015
      Left            =   240
      Picture         =   "Form1.frx":0000
      ScaleHeight     =   397
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   517
      TabIndex        =   0
      Top             =   360
      Width           =   7815
   End
   Begin VB.Menu mFIL 
      Caption         =   "File"
      Begin VB.Menu mLoad 
         Caption         =   "Load image"
      End
      Begin VB.Menu mKuit 
         Caption         =   "Exit"
      End
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'===========================================
' This code show how to make fast image processing
'  and check the progression thanks to a CallBack function
'
'The filter engine use 3*3 KernelMatrix
'and a devide and adcolor parameters
'
'
'so it's possible to generate infinite filters
'now
'  If u want to modifiate this code please let me know
' if u want tips for more filters write me at
' Johna.pop@caramail.com
'============================================









Private PIC_WIDTH As Integer
Private PIC_HEIGHT As Integer

Private Sub Command1_Click()

Call Module1.GetPixelData
Module1.InvertRVB RVBarray(0, 0), PIC_WIDTH, PIC_HEIGHT ', AddressOf Module1.Progress
Module1.Blit
End Sub

Private Sub Command10_Click()
Dim KER(9) As Single
Dim v As Variant


v = Array(10, 10, 10, _
           10, 20, 10, _
          10, 10, 10)
    
For I = 0 To 8
   KER(I) = v(I)
Next I

Call Module1.GetPixelData
Module1.ApplyKernelToRVB RVBarray(0, 0), PIC_WIDTH, PIC_HEIGHT, KER(0), 99, 0, AddressOf Module1.Progress
Module1.Blit
End Sub

Private Sub Command11_Click()
Dim KER(9) As Single
Dim v As Variant


v = Array(6, 12, 6, _
           12, 25, 12, _
          6, 12, 6)
    
For I = 0 To 8
   KER(I) = v(I)
Next I

Call Module1.GetPixelData
Module1.ApplyKernelToRVB RVBarray(0, 0), PIC_WIDTH, PIC_HEIGHT, KER(0), 97, 0, AddressOf Module1.Progress
Module1.Blit
End Sub

Private Sub Command12_Click()
Module1.Draw Picture1.hdc, PIC_WIDTH, PIC_HEIGHT, AddressOf Module1.Progress
End Sub

Private Sub Command2_Click()
Dim KER(9) As Single
Dim v As Variant


v = Array(11, 11, 11, _
           11, 11, 11, _
          11, 11, 11)
    
For I = 0 To 8
   KER(I) = v(I)
Next I

Call Module1.GetPixelData
Module1.ApplyKernelToRVB RVBarray(0, 0), PIC_WIDTH, PIC_HEIGHT, KER(0), 99, 0, AddressOf Module1.Progress
Module1.Blit
End Sub

Private Sub Command3_Click()
Call Module1.GetPixelData
Module1.GrayIntensityRVB RVBarray(0, 0), PIC_WIDTH, PIC_HEIGHT
Module1.Blit
End Sub




Private Sub Command4_Click()
Call Module1.GetPixelData
Module1.DarKenRVB RVBarray(0, 0), PIC_WIDTH, PIC_HEIGHT, 5
Module1.Blit
End Sub

Private Sub Command5_Click()
Call Module1.GetPixelData
Module1.LightenRVB RVBarray(0, 0), PIC_WIDTH, PIC_HEIGHT, 5
Module1.Blit
End Sub

Private Sub Command6_Click()
Call Module1.GetPixelData
Module1.BlurRVB RVBarray(0, 0), PIC_WIDTH, PIC_HEIGHT, 3, AddressOf Module1.Progress
Module1.Blit
End Sub

Private Sub Command7_Click()
Call Module1.GetPixelData
Module1.MorphFishEyeRVB RVBarray(0, 0), PIC_WIDTH, PIC_HEIGHT, 10, AddressOf Module1.Progress
Module1.Blit
End Sub

Private Sub Command8_Click()
Dim KER(9) As Single
Dim v As Variant
'example of emboss kernel
v = Array(0, 0, -1, _
          0, 1, 0, _
          0, 0, 0)
          
    
For I = 0 To 8
   KER(I) = v(I)
Next I

Call Module1.GetPixelData
Module1.ApplyKernelToRVB RVBarray(0, 0), PIC_WIDTH, PIC_HEIGHT, KER(0), 1, 127, AddressOf Module1.Progress
Module1.Blit
End Sub

Private Sub Command9_Click()
 Picture1.Cls
End Sub

Private Sub Form_Load()
PIC_WIDTH = Picture1.ScaleWidth
PIC_HEIGHT = Picture1.ScaleHeight
End Sub

Private Sub Form_Unload(Cancel As Integer)
  MsgBox "IF YOU LIKE THIS WORK FEEL FREE TO SUPPORT IT ON PSC", vbInformation
  Module1.FreeMemomy
End Sub

Private Sub mKuit_Click()
 Unload Me
End Sub

Private Sub mLoad_Click()
 Dim cd As New Dialog
 cd.VBGetOpenFileName ""
 If InStr(UCase(cd.Filename), ".JPG") Or _
 InStr(UCase(cd.Filename), ".JPG") Or _
 InStr(UCase(cd.Filename), ".GIF") Then
 
  Picture1.Picture = LoadPicture(cd.Filename)
  
 End If
 
End Sub

