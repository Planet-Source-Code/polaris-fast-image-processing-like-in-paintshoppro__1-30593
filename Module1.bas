Attribute VB_Name = "Module1"
'Sample function Exported by the Filter.dll


'See the C++ code for more details
 Declare Function Draw Lib "Filter.dll" (ByVal DC As Long, ByVal WI As Integer, ByVal HI As Integer, lpCallback As Any) As Long
 Declare Sub InvertRVB Lib "Filter.dll" (ByRef myRVBarray As RVB, ByVal Width As Integer, ByVal Height As Integer) ', lpCallback As Any)
 Declare Sub GrayIntensityRVB Lib "Filter.dll" (ByRef myRVBarray As RVB, ByVal Width As Integer, ByVal Height As Integer) ', lpCallback As Any)
 Declare Sub DarKenRVB Lib "Filter.dll" (ByRef myRVBarray As RVB, ByVal Width As Integer, ByVal Height As Integer, ByVal level As Long) ', lpCallback As Any)
 Declare Sub LightenRVB Lib "Filter.dll" (ByRef myRVBarray As RVB, ByVal Width As Integer, ByVal Height As Integer, ByVal level As Long) ', lpCallback As Any)
 Declare Sub BlurRVB Lib "Filter.dll" (ByRef myRVBarray As RVB, ByVal Width As Integer, ByVal Height As Integer, ByVal BlurRadius As Long, lpCallback As Any)
 Declare Sub MorphFishEyeRVB Lib "Filter.dll" (ByRef myRVBarray As RVB, ByVal Width As Integer, ByVal Height As Integer, ByVal CurvingLevel As Single, lpCallback As Any)

'the most important
 Declare Sub ApplyKernelToRVB Lib "Filter.dll" (ByRef myRVBarray As RVB, ByVal Width As Integer, ByVal Height As Integer, ByRef KernelArray As Single, ByVal DevideColor As Single, ByVal AddColor As Single, lpCallback As Any)





'API'z for getting all Pixel Data
 Declare Function CreateCompatibleDC Lib "gdi32" (ByVal hdc As Long) As Long
 Declare Function CreateDIBSection Lib "gdi32" (ByVal hdc As Long, pBitmapInfo As BITMAPINFO, ByVal un As Long, ByVal lplpVoid As Long, ByVal handle As Long, ByVal dw As Long) As Long
 Declare Function SelectObject Lib "gdi32" (ByVal hdc As Long, ByVal hObject As Long) As Long
 Declare Function BitBlt Lib "gdi32" (ByVal hDestDC As Long, ByVal X As Long, ByVal Y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long, ByVal dwRop As Long) As Long
 Declare Function GetDIBits Lib "gdi32" (ByVal aHDC As Long, ByVal HBitmap As Long, ByVal nStartScan As Long, ByVal nNumScans As Long, lpBits As Any, lpBI As BITMAPINFO, ByVal wUsage As Long) As Long
 Declare Function SetDIBitsToDevice Lib "gdi32" (ByVal hdc As Long, ByVal X As Long, ByVal Y As Long, ByVal dx As Long, ByVal dy As Long, ByVal SrcX As Long, ByVal SrcY As Long, ByVal Scan As Long, ByVal NumScans As Long, Bits As Any, BitsInfo As BITMAPINFO, ByVal wUsage As Long) As Long

 Declare Function DeleteDC Lib "gdi32" (ByVal hdc As Long) As Long
 Declare Function DeleteObject Lib "gdi32" (ByVal hObject As Long) As Long



Public Const BI_RGB = 0&
Public Const DIB_RGB_COLORS = 0 '  color table in RGBs
Public Type BITMAPINFOHEADER '40 bytes
        biSize As Long
        biWidth As Long
        biHeight As Long
        biPlanes As Integer
        biBitCount As Integer
        biCompression As Long
        biSizeImage As Long
        biXPelsPerMeter As Long
        biYPelsPerMeter As Long
        biClrUsed As Long
        biClrImportant As Long
End Type
Public Type RGBQUAD
        rgbBlue As Byte
        rgbGreen As Byte
        rgbRed As Byte
        rgbReserved As Byte
End Type
Public Type BITMAPINFO
        bmiHeader As BITMAPINFOHEADER
        bmiColors As RGBQUAD
End Type





'for holding Pixel color
Public Type RVB
    Rouge As Byte
    Vert As Byte
    Bleu As Byte
    Reserved As Byte
End Type

Public RVBarray() As RVB


'For the GetBit procedure
Public iBitmap As Long
Public bi24BitInfo As BITMAPINFO
Public TempDC As Long





'=========================================================
'
'  CallBack Function
'  The C++ Dll call that function for update the progression level
'   this function must be declared at a module section
'========================================================
Function Progress(ByVal percent As Integer) As Boolean
  
  
  'Form1.Caption = CStr(percent) + "%"
  Form1.ProgressBar1.Value = percent
  Progress = True
  
End Function




Public Sub GetPixelData()

  'fill the bitmapinfoheader structure
    
  With bi24BitInfo.bmiHeader
        .biBitCount = 32  'we use 32 bit alignement for RGB and the unused 8 bytes
        .biCompression = BI_RGB
        .biPlanes = 1
        .biSize = Len(bi24BitInfo.bmiHeader)
        .biWidth = Form1.Picture1.ScaleWidth
        .biHeight = Form1.Picture1.ScaleHeight
    End With
    ReDim RVBarray(0 To bi24BitInfo.bmiHeader.biWidth - 1, 0 To bi24BitInfo.bmiHeader.biHeight - 1)
    
    TempDC = CreateCompatibleDC(0)
    iBitmap = CreateDIBSection(TempDC, bi24BitInfo, DIB_RGB_COLORS, ByVal 0&, ByVal 0&, ByVal 0&)
    SelectObject TempDC, iBitmap
    BitBlt TempDC, 0, 0, bi24BitInfo.bmiHeader.biWidth, bi24BitInfo.bmiHeader.biHeight, Form1.Picture1.hdc, 0, 0, vbSrcCopy
    
    'Finaly get All the pixels color
    GetDIBits TempDC, iBitmap, 0, bi24BitInfo.bmiHeader.biHeight, RVBarray(0, 0), bi24BitInfo, DIB_RGB_COLORS
   
   'IMPORTANT if someone knows how to Get all pixels in a RVB structure send allmodified
   'to the device  in C++
    'let me know cauz it would be possible to pass only a
    'Device context and a addressof a callback function
End Sub


Sub Blit()
    'Send the pixeldata to the device
    'it would be great to do it in C++ unfortunately all my tries failed
    SetDIBitsToDevice Form1.Picture1.hdc, 0, 0, bi24BitInfo.bmiHeader.biWidth, bi24BitInfo.bmiHeader.biHeight, 0, 0, 0, bi24BitInfo.bmiHeader.biHeight, RVBarray(0, 0), bi24BitInfo, DIB_RGB_COLORS
    Form1.ProgressBar1.Value = 0
End Sub


Sub FreeMemomy()
  'free the temp device and his attached bitmap
    If TempDC > 0 Then
     DeleteDC TempDC
     DeleteObject iBitmap
     Erase RVBarray
    
    End If

End Sub
