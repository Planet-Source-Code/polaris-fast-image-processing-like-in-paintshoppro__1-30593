VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Dialog"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

' ==========================================================================
' Class:    CommonDialog
' Filename: Comdlg.cls
' Author:   johna pop, based on original by Bruce McKinney
' Date:     20 April 2000
' ==========================================================================


' ==========================================================================
' API declares:
' ==========================================================================
Enum EErrorCommonDialog
    eeBaseCommonDialog = 13450  ' CommonDialog
End Enum
Private Declare Function SHGetPathFromIDList Lib "shell32.dll" Alias "SHGetPathFromIDListA" (ByVal pidl As Long, ByVal pszPath As String) As Long


Private Declare Function SFolder Lib "shell32.dll" Alias "SHBrowseForFolderA" (lpBI As BROWSEINFO) As Long
Private Declare Function lstrlen Lib "kernel32" Alias "lstrlenA" (ByVal lpString As String) As Long
Private Declare Function GlobalAlloc Lib "kernel32" (ByVal wFlags As Long, ByVal dwBytes As Long) As Long
Private Declare Function GlobalCompact Lib "kernel32" (ByVal dwMinFree As Long) As Long
Private Declare Function GlobalFree Lib "kernel32" (ByVal hMem As Long) As Long
Private Declare Function GlobalLock Lib "kernel32" (ByVal hMem As Long) As Long
Private Declare Function GlobalReAlloc Lib "kernel32" (ByVal hMem As Long, ByVal dwBytes As Long, ByVal wFlags As Long) As Long
Private Declare Function GlobalSize Lib "kernel32" (ByVal hMem As Long) As Long
Private Declare Function GlobalUnlock Lib "kernel32" (ByVal hMem As Long) As Long
Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" ( _
    lpvDest As Any, lpvSource As Any, ByVal cbCopy As Long)
Private Declare Sub CopyMemoryStr Lib "kernel32" Alias "RtlMoveMemory" ( _
    lpvDest As Any, ByVal lpvSource As String, ByVal cbCopy As Long)

Private Const MAX_PATH = 260
Private Const MAX_FILE = 260
Private Name As String
Private Filta, Title, MDIR As String
Private BROSA As String
Private BRO As BROWSEINFO
Private Type BROWSEINFO
  hwndOwner As Long
  pidlRoot As Long
  pszDisplayName As String
  lpszTitle As String
  ulFlags As Long
  lpfn As Long
  lParam As Long
  iImage As Long
End Type


Private Type OPENFILENAME
    lStructSize As Long          ' Filled with UDT size
    hwndOwner As Long            ' Tied to Owner
    hInstance As Long            ' Ignored (used only by templates)
    lpstrFilter As String        ' Tied to Filter
    lpstrCustomFilter As String  ' Ignored (exercise for reader)
    nMaxCustFilter As Long       ' Ignored (exercise for reader)
    nFilterIndex As Long         ' Tied to FilterIndex
    lpstrFile As String          ' Tied to FileName
    nMaxFile As Long             ' Handled internally
    lpstrFileTitle As String     ' Tied to FileTitle
    nMaxFileTitle As Long        ' Handled internally
    lpstrInitialDir As String    ' Tied to InitDir
    lpstrTitle As String         ' Tied to DlgTitle
    Flags As Long                ' Tied to Flags
    nFileOffset As Integer       ' Ignored (exercise for reader)
    nFileExtension As Integer    ' Ignored (exercise for reader)
    lpstrDefExt As String        ' Tied to DefaultExt
    lCustData As Long            ' Ignored (needed for hooks)
    lpfnHook As Long             ' Ignored (good luck with hooks)
    lpTemplateName As Long       ' Ignored (good luck with templates)
End Type

Private Declare Function GetOpenFileName Lib "ComDLG32" _
    Alias "GetOpenFileNameA" (FILE As OPENFILENAME) As Long
Private Declare Function GetSaveFileName Lib "ComDLG32" _
    Alias "GetSaveFileNameA" (FILE As OPENFILENAME) As Long
Private Declare Function GetFileTitle Lib "ComDLG32" _
    Alias "GetFileTitleA" (ByVal szFile As String, _
    ByVal szTitle As String, ByVal cbBuf As Long) As Long

 Enum EOpenFile
    OFN_READONLY = &H1
    OFN_OVERWRITEPROMPT = &H2
    OFN_HIDEREADONLY = &H4
    OFN_NOCHANGEDIR = &H8
    OFN_SHOWHELP = &H10
    OFN_ENABLEHOOK = &H20
    OFN_ENABLETEMPLATE = &H40
    OFN_ENABLETEMPLATEHANDLE = &H80
    OFN_NOVALIDATE = &H100
    OFN_ALLOWMULTISELECT = &H200
    OFN_EXTENSIONDIFFERENT = &H400
    OFN_PATHMUSTEXIST = &H800
    OFN_FILEMUSTEXIST = &H1000
    OFN_CREATEPROMPT = &H2000
    OFN_SHAREAWARE = &H4000
    OFN_NOREADONLYRETURN = &H8000
    OFN_NOTESTFILECREATE = &H10000
    OFN_NONETWORKBUTTON = &H20000
    OFN_NOLONGNAMES = &H40000
    OFN_EXPLORER = &H80000
    OFN_NODEREFERENCELINKS = &H100000
    OFN_LONGNAMES = &H200000
End Enum

Private Type TCHOOSECOLOR
    lStructSize As Long
    hwndOwner As Long
    hInstance As Long
    rgbResult As Long
    lpCustColors As Long
    Flags As Long
    lCustData As Long
    lpfnHook As Long
    lpTemplateName As Long
End Type

Private Declare Function ChooseColor Lib "COMDLG32.DLL" _
    Alias "ChooseColorA" (COLOR As TCHOOSECOLOR) As Long

 Enum EChooseColor
    CC_RGBINIT = &H1
    CC_FULLOPEN = &H2
    CC_PREVENTFULLOPEN = &H4
    CC_ColorShowHelp = &H8
' Win95 only
    CC_SOLIDCOLOR = &H80
    CC_ANYCOLOR = &H100
' End Win95 only
    CC_ENABLEHOOK = &H10
    CC_ENABLETEMPLATE = &H20
    CC_ENABLETEMPLATEHANDLE = &H40
End Enum
Private Declare Function GetSysColor Lib "user32" (ByVal nIndex As Long) As Long

Private Type TCHOOSEFONT
    lStructSize As Long         ' Filled with UDT size
    hwndOwner As Long           ' Caller's window handle
    hdc As Long                 ' Printer DC/IC or NULL
    lpLogFont As Long           ' Pointer to LOGFONT
    iPointSize As Long          ' 10 * size in points of font
    Flags As Long               ' Type flags
    rgbColors As Long           ' Returned text color
    lCustData As Long           ' Data passed to hook function
    lpfnHook As Long            ' Pointer to hook function
    lpTemplateName As Long      ' Custom template name
    hInstance As Long           ' Instance handle for template
    lpszStyle As String         ' Return style field
    nFontType As Integer        ' Font type bits
    iAlign As Integer           ' Filler
    nSizeMin As Long            ' Minimum point size allowed
    nSizeMax As Long            ' Maximum point size allowed
End Type
Private Declare Function ChooseFont Lib "ComDLG32" _
    Alias "ChooseFontA" (chfont As TCHOOSEFONT) As Long

Private Const LF_FACESIZE = 32
Private Type LOGFONT
    lfHeight As Long
    lfWidth As Long
    lfEscapement As Long
    lfOrientation As Long
    lfWeight As Long
    lfItalic As Byte
    lfUnderline As Byte
    lfStrikeOut As Byte
    lfCharSet As Byte
    lfOutPrecision As Byte
    lfClipPrecision As Byte
    lfQuality As Byte
    lfPitchAndFamily As Byte
    lfFaceName(LF_FACESIZE) As Byte
End Type

 Enum EChooseFont
    CF_SCREENFONTS = &H1
    CF_PRINTERFONTS = &H2
    CF_BOTH = &H3
    CF_FontShowHelp = &H4
    CF_USESTYLE = &H80
    CF_EFFECTS = &H100
    CF_ANSIONLY = &H400
    CF_NOVECTORFONTS = &H800
    CF_NoOemFonts = CF_NOVECTORFONTS
    CF_NOSIMULATIONS = &H1000
    CF_LIMITSIZE = &H2000
    CF_FIXEDPITCHONLY = &H4000
    CF_WYSIWYG = &H8000  ' Must also have ScreenFonts And PrinterFonts
    CF_FORCEFONTEXIST = &H10000
    CF_SCALABLEONLY = &H20000
    CF_TTONLY = &H40000
    CF_NOFACESEL = &H80000
    CF_NOSTYLESEL = &H100000
    CF_NOSIZESEL = &H200000
    ' Win95 only
    CF_SELECTSCRIPT = &H400000
    CF_NOSCRIPTSEL = &H800000
    CF_NOVERTFONTS = &H1000000

    CF_INITTOLOGFONTSTRUCT = &H40
    CF_APPLY = &H200
    CF_ENABLEHOOK = &H8
    CF_ENABLETEMPLATE = &H10
    CF_ENABLETEMPLATEHANDLE = &H20
    CF_FontNotSupported = &H238
End Enum

' These are extra nFontType bits that are added to what is returned to the
' EnumFonts callback routine

 Enum EFontType
    SIMULATED_FONTTYPE = &H8000
    PRINTER_FONTTYPE = &H4000
    SCREEN_FONTTYPE = &H2000
    BOLD_FONTTYPE = &H100
    ITALIC_FONTTYPE = &H200
    REGULAR_FONTTYPE = &H400
End Enum

Private Type TPRINTDLG
    lStructSize As Long
    hwndOwner As Long
    hDevMode As Long
    hDevNames As Long
    hdc As Long
    Flags As Long
    nFromPage As Integer
    nToPage As Integer
    nMinPage As Integer
    nMaxPage As Integer
    nCopies As Integer
    hInstance As Long
    lCustData As Long
    lpfnPrintHook As Long
    lpfnSetupHook As Long
    lpPrintTemplateName As Long
    lpSetupTemplateName As Long
    hPrintTemplate As Long
    hSetupTemplate As Long
End Type

'  DEVMODE collation selections
Private Const DMCOLLATE_FALSE = 0
Private Const DMCOLLATE_TRUE = 1

Private Declare Function PrintDlg Lib "COMDLG32.DLL" _
    Alias "PrintDlgA" (prtdlg As TPRINTDLG) As Integer

 Enum EPrintDialog
    PD_ALLPAGES = &H0
    PD_SELECTION = &H1
    PD_PAGENUMS = &H2
    PD_NOSELECTION = &H4
    PD_NOPAGENUMS = &H8
    PD_COLLATE = &H10
    PD_PRINTTOFILE = &H20
    PD_PRINTSETUP = &H40
    PD_NOWARNING = &H80
    PD_RETURNDC = &H100
    PD_RETURNIC = &H200
    PD_RETURNDEFAULT = &H400
    PD_SHOWHELP = &H800
    PD_ENABLEPRINTHOOK = &H1000
    PD_ENABLESETUPHOOK = &H2000
    PD_ENABLEPRINTTEMPLATE = &H4000
    PD_ENABLESETUPTEMPLATE = &H8000
    PD_ENABLEPRINTTEMPLATEHANDLE = &H10000
    PD_ENABLESETUPTEMPLATEHANDLE = &H20000
    PD_USEDEVMODECOPIES = &H40000
    PD_USEDEVMODECOPIESANDCOLLATE = &H40000
    PD_DISABLEPRINTTOFILE = &H80000
    PD_HIDEPRINTTOFILE = &H100000
    PD_NONETWORKBUTTON = &H200000
End Enum

Private Type DEVNAMES
    wDriverOffset As Integer
    wDeviceOffset As Integer
    wOutputOffset As Integer
    wDefault As Integer
End Type

Private Const CCHDEVICENAME = 32
Private Const CCHFORMNAME = 32
Private Type DevMode
    dmDeviceName As String * CCHDEVICENAME
    dmSpecVersion As Integer
    dmDriverVersion As Integer
    dmSize As Integer
    dmDriverExtra As Integer
    dmFields As Long
    dmOrientation As Integer
    dmPaperSize As Integer
    dmPaperLength As Integer
    dmPaperWidth As Integer
    dmScale As Integer
    dmCopies As Integer
    dmDefaultSource As Integer
    dmPrintQuality As Integer
    dmColor As Integer
    dmDuplex As Integer
    dmYResolution As Integer
    dmTTOption As Integer
    dmCollate As Integer
    dmFormName As String * CCHFORMNAME
    dmUnusedPadding As Integer
    dmBitsPerPel As Integer
    dmPelsWidth As Long
    dmPelsHeight As Long
    dmDisplayFlags As Long
    dmDisplayFrequency As Long
End Type

' New Win95 Page Setup dialogs are up to you
Private Type POINTL
    X As Long
    Y As Long
End Type
Private Type RECT
    Left As Long
    Top As Long
    Right As Long
    Bottom As Long
End Type


Private Type TPAGESETUPDLG
    lStructSize                 As Long
    hwndOwner                   As Long
    hDevMode                    As Long
    hDevNames                   As Long
    Flags                       As Long
    ptPaperSize                 As POINTL
    rtMinMargin                 As RECT
    rtMargin                    As RECT
    hInstance                   As Long
    lCustData                   As Long
    lpfnPageSetupHook           As Long
    lpfnPagePaintHook           As Long
    lpPageSetupTemplateName     As Long
    hPageSetupTemplate          As Long
End Type

' EPaperSize constants same as vbPRPS constants
 Enum EPaperSize
    epsLetter = 1          ' Letter, 8 1/2 x 11 in.
    epsLetterSmall         ' Letter Small, 8 1/2 x 11 in.
    epsTabloid             ' Tabloid, 11 x 17 in.
    epsLedger              ' Ledger, 17 x 11 in.
    epsLegal               ' Legal, 8 1/2 x 14 in.
    epsStatement           ' Statement, 5 1/2 x 8 1/2 in.
    epsExecutive           ' Executive, 7 1/2 x 10 1/2 in.
    epsA3                  ' A3, 297 x 420 mm
    epsA4                  ' A4, 210 x 297 mm
    epsA4Small             ' A4 Small, 210 x 297 mm
    epsA5                  ' A5, 148 x 210 mm
    epsB4                  ' B4, 250 x 354 mm
    epsB5                  ' B5, 182 x 257 mm
    epsFolio               ' Folio, 8 1/2 x 13 in.
    epsQuarto              ' Quarto, 215 x 275 mm
    eps10x14               ' 10 x 14 in.
    eps11x17               ' 11 x 17 in.
    epsNote                ' Note, 8 1/2 x 11 in.
    epsEnv9                ' Envelope #9, 3 7/8 x 8 7/8 in.
    epsEnv10               ' Envelope #10, 4 1/8 x 9 1/2 in.
    epsEnv11               ' Envelope #11, 4 1/2 x 10 3/8 in.
    epsEnv12               ' Envelope #12, 4 1/2 x 11 in.
    epsEnv14               ' Envelope #14, 5 x 11 1/2 in.
    epsCSheet              ' C size sheet
    epsDSheet              ' D size sheet
    epsESheet              ' E size sheet
    epsEnvDL               ' Envelope DL, 110 x 220 mm
    epsEnvC3               ' Envelope C3, 324 x 458 mm
    epsEnvC4               ' Envelope C4, 229 x 324 mm
    epsEnvC5               ' Envelope C5, 162 x 229 mm
    epsEnvC6               ' Envelope C6, 114 x 162 mm
    epsEnvC65              ' Envelope C65, 114 x 229 mm
    epsEnvB4               ' Envelope B4, 250 x 353 mm
    epsEnvB5               ' Envelope B5, 176 x 250 mm
    epsEnvB6               ' Envelope B6, 176 x 125 mm
    epsEnvItaly            ' Envelope, 110 x 230 mm
    epsenvmonarch          ' Envelope Monarch, 3 7/8 x 7 1/2 in.
    epsEnvPersonal         ' Envelope, 3 5/8 x 6 1/2 in.
    epsFanfoldUS           ' U.S. Standard Fanfold, 14 7/8 x 11 in.
    epsFanfoldStdGerman    ' German Standard Fanfold, 8 1/2 x 12 in.
    epsFanfoldLglGerman    ' German Legal Fanfold, 8 1/2 x 13 in.
    epsUser = 256          ' User-defined
End Enum

' EPrintQuality constants same as vbPRPQ constants
Enum EPrintQuality
    epqDraft = -1
    epqLow = -2
    epqMedium = -3
    epqHigh = -4
End Enum

Enum eOrientation
    eoPortrait = 1
    eoLandscape
End Enum

Private Declare Function PageSetupDlg Lib "ComDLG32" _
    Alias "PageSetupDlgA" (lppage As TPAGESETUPDLG) As Boolean

Enum EPageSetup
    PSD_DEFAULTMINMARGINS = &H0 ' Default (printer's)
    PSD_INWININIINTLMEASURE = &H0
    PSD_MINMARGINS = &H1
    PSD_MARGINS = &H2
    PSD_INTHOUSANDTHSOFINCHES = &H4
    PSD_INHUNDREDTHSOFMILLIMETERS = &H8
    PSD_DISABLEMARGINS = &H10
    PSD_DISABLEPRINTER = &H20
    PSD_NOWARNING = &H80
    PSD_DISABLEORIENTATION = &H100
    PSD_RETURNDEFAULT = &H400
    PSD_DISABLEPAPER = &H200
    PSD_SHOWHELP = &H800
    PSD_ENABLEPAGESETUPHOOK = &H2000
    PSD_ENABLEPAGESETUPTEMPLATE = &H8000
    PSD_ENABLEPAGESETUPTEMPLATEHANDLE = &H20000
    PSD_ENABLEPAGEPAINTHOOK = &H40000
    PSD_DISABLEPAGEPAINTING = &H80000
End Enum

Enum EPageSetupUnits
    epsuInches
    epsuMillimeters
End Enum

' Common dialog errors

Private Declare Function CommDlgExtendedError Lib "ComDLG32" () As Long

Enum EDialogError
    CDERR_DIALOGFAILURE = &HFFFF

    CDERR_GENERALCODES = &H0
    CDERR_STRUCTSIZE = &H1
    CDERR_INITIALIZATION = &H2
    CDERR_NOTEMPLATE = &H3
    CDERR_NOHINSTANCE = &H4
    CDERR_LOADSTRFAILURE = &H5
    CDERR_FINDRESFAILURE = &H6
    CDERR_LOADRESFAILURE = &H7
    CDERR_LOCKRESFAILURE = &H8
    CDERR_MEMALLOCFAILURE = &H9
    CDERR_MEMLOCKFAILURE = &HA
    CDERR_NOHOOK = &HB
    CDERR_REGISTERMSGFAIL = &HC

    PDERR_PRINTERCODES = &H1000
    PDERR_SETUPFAILURE = &H1001
    PDERR_PARSEFAILURE = &H1002
    PDERR_RETDEFFAILURE = &H1003
    PDERR_LOADDRVFAILURE = &H1004
    PDERR_GETDEVMODEFAIL = &H1005
    PDERR_INITFAILURE = &H1006
    PDERR_NODEVICES = &H1007
    PDERR_NODEFAULTPRN = &H1008
    PDERR_DNDMMISMATCH = &H1009
    PDERR_CREATEICFAILURE = &H100A
    PDERR_PRINTERNOTFOUND = &H100B
    PDERR_DEFAULTDIFFERENT = &H100C

    CFERR_CHOOSEFONTCODES = &H2000
    CFERR_NOFONTS = &H2001
    CFERR_MAXLESSTHANMIN = &H2002

    FNERR_FILENAMECODES = &H3000
    FNERR_SUBCLASSFAILURE = &H3001
    FNERR_INVALIDFILENAME = &H3002
    FNERR_BUFFERTOOSMALL = &H3003

    CCERR_CHOOSECOLORCODES = &H5000
End Enum

' Hook and notification support:
Private Type NMHDR
    hwndFrom As Long
    idfrom As Long
    code As Long
End Type
'// Structure used for all file based OpenFileName notifications
Private Type OFNOTIFY
    hdr As NMHDR
    lpOFN As Long           ' Long pointer to OFN structure
    pszFile As String ';        // May be NULL
End Type

'// Structure used for all object based OpenFileName notifications
Private Type OFNOTIFYEX
    hdr As NMHDR
    lpOFN As Long       ' Long pointer to OFN structure
    psf As Long
    LPVOID As Long          '// May be NULL
End Type

Private Type OFNOTIFYshort
    hdr As NMHDR
    lpOFN As Long
End Type

' Messages:
Private Const WM_INITDIALOG = &H110
Private Const WM_NOTIFY = &H4E
Private Const WM_USER = &H400
Private Const WM_GETDLGCODE = &H87
Private Const WM_NCDESTROY = &H82


' Notification codes:
Private Const H_MAX As Long = &HFFFF + 1
Private Const CDN_FIRST = (H_MAX - 601)
Private Const CDN_LAST = (H_MAX - 699)

'// Notifications when Open or Save dialog status changes
Private Const CDN_INITDONE = (CDN_FIRST - &H0)
Private Const CDN_SELCHANGE = (CDN_FIRST - &H1)
Private Const CDN_FOLDERCHANGE = (CDN_FIRST - &H2)
Private Const CDN_SHAREVIOLATION = (CDN_FIRST - &H3)
Private Const CDN_HELP = (CDN_FIRST - &H4)
Private Const CDN_FILEOK = (CDN_FIRST - &H5)
Private Const CDN_TYPECHANGE = (CDN_FIRST - &H6)
Private Const CDN_INCLUDEITEM = (CDN_FIRST - &H7)

Private Const CDM_FIRST = (WM_USER + 100)
Private Const CDM_LAST = (WM_USER + 200)

Private Const DWL_MSGRESULT = 0
Private Declare Function SetWindowLong Lib "user32" Alias "SetWindowLongA" (ByVal hwnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long

' ==========================================================================
' Implementation:
' ==========================================================================

' Array of custom colors lasts for life of app
Private alCustom(0 To 15) As Long, fNotFirst As Boolean

Enum EPrintRange
    eprAll
    eprPageNumbers
    eprSelection
End Enum
Private m_lApiReturn As Long
Private m_lExtendedError As Long
Private m_dvmode As DevMode
Private m_oEventSink As Object
Private COLa As Long

 Function DialogHook( _
        ByVal hDlg As Long, _
        ByVal msg As Long, _
        ByVal wParam As Long, _
        ByVal lParam As Long _
    )
Dim tNMH As NMHDR
Dim tOFNs As OFNOTIFYshort
Dim tOF As OPENFILENAME

    If Not (m_oEventSink Is Nothing) Then
        Select Case msg
        Case WM_INITDIALOG
            DialogHook = m_oEventSink.InitDialog(hDlg)
        Case WM_NOTIFY
            CopyMemory tNMH, ByVal lParam, Len(tNMH)
            Select Case tNMH.code
            Case CDN_SELCHANGE
                ' Changed selected file:
                DialogHook = m_oEventSink.FileChange(hDlg)
            Case CDN_FOLDERCHANGE
                ' Changed folder:
                DialogHook = m_oEventSink.FolderChange(hDlg)
            Case CDN_FILEOK
                ' Clicked OK:
                If Not m_oEventSink.ConfirmOK() Then
                    SetWindowLong hDlg, DWL_MSGRESULT, 1
                    DialogHook = 1
                Else
                    SetWindowLong hDlg, DWL_MSGRESULT, 0
                End If
            Case CDN_HELP
                ' Help clicked
            Case CDN_TYPECHANGE
                DialogHook = m_oEventSink.TypeChange(hDlg)
            Case CDN_INCLUDEITEM
                ' Hmmm
            End Select
        Case WM_NCDESTROY
            m_oEventSink.DialogClose
        End Select
    End If
End Function


 Property Get APIReturn() As Long
    'return object's APIReturn property
    APIReturn = m_lApiReturn
End Property
Public Property Get ExtendedError() As Long
    'return object's ExtendedError property
    ExtendedError = m_lExtendedError
End Property



Function VBGetOpenFileName(Filename As String, _
                           Optional FileTitle As String, _
                           Optional FileMustExist As Boolean = True, _
                           Optional MultiSelect As Boolean = False, _
                           Optional ReadOnly As Boolean = False, _
                           Optional HideReadOnly As Boolean = False, _
                           Optional FILTER As String = "All (*.*)| *.*", _
                           Optional FilterIndex As Long = 1, _
                           Optional InitDir As String, _
                           Optional DlgTitle As String, _
                           Optional DefaultExt As String, _
                           Optional Owner As Long = -1, _
                           Optional Flags As Long = 0 _
                        ) As Boolean

    Dim opfile As OPENFILENAME, S As String, afFlags As Long
    
    m_lApiReturn = 0
    m_lExtendedError = 0

With opfile
    .lStructSize = Len(opfile)
    
    ' Add in specific flags and strip out non-VB flags
    
    .Flags = (-FileMustExist * OFN_FILEMUSTEXIST) Or _
            (-MultiSelect * OFN_ALLOWMULTISELECT) Or _
             (-ReadOnly * OFN_READONLY) Or _
             (-HideReadOnly * OFN_HIDEREADONLY) Or _
             (Flags And CLng(Not (OFN_ENABLEHOOK Or _
                                  OFN_ENABLETEMPLATE)))
    ' Owner can take handle of owning window
    If Owner <> -1 Then .hwndOwner = Owner
    ' InitDir can take initial directory string
    .lpstrInitialDir = MDIR
    ' DefaultExt can take default extension
    .lpstrDefExt = DefaultExt
    ' DlgTitle can take dialog box title
    If Title = "" Then Title = "johna open"
    
    .lpstrTitle = Title
     If Filta = "" Then Filta = "All (*.*)| *.*"
    FILTER = Filta
    ' To make Windows-style filter, replace | and : with nulls
    Dim ch As String, I As Integer
    For I = 1 To Len(FILTER)
        ch = Mid$(FILTER, I, 1)
        If ch = "|" Or ch = ":" Then
            S = S & vbNullChar
        Else
            S = S & ch
        End If
    Next
    ' Put double null at end
    S = S & vbNullChar & vbNullChar
    .lpstrFilter = S
    .nFilterIndex = FilterIndex

    ' Pad file and file title buffers to maximum path
    S = Filename & String$(MAX_PATH - Len(Filename), 0)
    .lpstrFile = S
    .nMaxFile = MAX_PATH
    S = FileTitle & String$(MAX_FILE - Len(FileTitle), 0)
    .lpstrFileTitle = S
    .nMaxFileTitle = MAX_FILE
    ' All other fields set to zero
    
    
    m_lApiReturn = GetOpenFileName(opfile)
    Set m_oEventSink = Nothing
    Select Case m_lApiReturn
    Case 1
        ' Success
        VBGetOpenFileName = True
        Filename = StrZToStr(.lpstrFile)
        Name = StrZToStr(.lpstrFile)
        FileTitle = StrZToStr(.lpstrFileTitle)
        Flags = .Flags
        ' Return the filter index
        FilterIndex = .nFilterIndex
        ' Look up the filter the user selected and return that
        FILTER = FilterLookup(.lpstrFilter, FilterIndex)
        If (.Flags And OFN_READONLY) Then ReadOnly = True
    Case 0
        ' Cancelled
        VBGetOpenFileName = False
        Filename = ""
        FileTitle = ""
        Flags = 0
        FilterIndex = -1
        FILTER = ""
    Case Else
        ' Extended error
        m_lExtendedError = CommDlgExtendedError()
        VBGetOpenFileName = False
        Filename = ""
        FileTitle = ""
        Flags = 0
        FilterIndex = -1
        FILTER = ""
    End Select
    Set m_oEventSink = Nothing
End With
End Function
Private Function lHookAddress(lPtr As Long) As Long
    'Debug.Print lPtr
    lHookAddress = lPtr
End Function
 Function StrZToStr(S As String) As String
    StrZToStr = Left$(S, lstrlen(S))
End Function

Function VBGetSaveFileName(Optional Filename As String, _
                           Optional FileTitle As String, _
                           Optional OverWritePrompt As Boolean = True, _
                           Optional FILTER As String = "All (*.*)| *.*", _
                           Optional FilterIndex As Long = 1, _
                           Optional InitDir As String, _
                           Optional DlgTitle As String, _
                           Optional DefaultExt As String, _
                           Optional Owner As Long = -1, _
                           Optional Flags As Long _
                        ) As Boolean
            
    Dim opfile As OPENFILENAME, S As String

    m_lApiReturn = 0
    m_lExtendedError = 0

With opfile
    .lStructSize = Len(opfile)
    
    ' Add in specific flags and strip out non-VB flags
    .Flags = (-OverWritePrompt * OFN_OVERWRITEPROMPT) Or _
             OFN_HIDEREADONLY Or _
             (Flags And CLng(Not (OFN_ENABLEHOOK Or _
                                  OFN_ENABLETEMPLATE)))
    ' Owner can take handle of owning window
    If Owner <> -1 Then .hwndOwner = Owner
    ' InitDir can take initial directory string
    .lpstrInitialDir = InitDir
    ' DefaultExt can take default extension
    .lpstrDefExt = DefaultExt
    ' DlgTitle can take dialog box title
    If Title = "" Then Title = "johna Save File"
    
    .lpstrTitle = Title
    If Filta = "" Then Filta = "All (*.joh*)| *.joh*"
    FILTER = Filta
        
    ' Make new filter with bars (|) replacing nulls and double null at end
    Dim ch As String, I As Integer
    For I = 1 To Len(FILTER)
        ch = Mid$(FILTER, I, 1)
        If ch = "|" Or ch = ":" Then
            S = S & vbNullChar
        Else
            S = S & ch
        End If
    Next
    ' Put double null at end
    S = S & vbNullChar & vbNullChar
    .lpstrFilter = S
    .nFilterIndex = FilterIndex

    ' Pad file and file title buffers to maximum path
    S = Filename & String$(MAX_PATH - Len(Filename), 0)
    .lpstrFile = S
    .nMaxFile = MAX_PATH
    S = FileTitle & String$(MAX_FILE - Len(FileTitle), 0)
    .lpstrFileTitle = S
    .nMaxFileTitle = MAX_FILE
    ' All other fields zero
    
    m_lApiReturn = GetSaveFileName(opfile)
    Set m_oEventSink = Nothing
    Select Case m_lApiReturn
    Case 1
        VBGetSaveFileName = True
        Filename = StrZToStr(.lpstrFile)
        Name = StrZToStr(.lpstrFile)
        FileTitle = StrZToStr(.lpstrFileTitle)
        Flags = .Flags
        ' Return the filter index
        FilterIndex = .nFilterIndex
        ' Look up the filter the user selected and return that
        FILTER = FilterLookup(.lpstrFilter, FilterIndex)
        Dim ts
        ts = Right(FILTER, 4)
        Name = Name + ts
        Filename = Filename + ts
    Case 0
        ' Cancelled:
        VBGetSaveFileName = False
        Filename = ""
        FileTitle = ""
        Flags = 0
        FilterIndex = 0
        FILTER = ""
    Case Else
        ' Extended error:
        VBGetSaveFileName = False
        m_lExtendedError = CommDlgExtendedError()
        Filename = ""
        FileTitle = ""
        Flags = 0
        FilterIndex = 0
        FILTER = ""
    End Select
End With
End Function

 Function FilterLookup(ByVal sFilters As String, ByVal iCur As Long) As String
    Dim iStart As Long, iEnd As Long, S As String
    iStart = 1
    If sFilters = "" Then Exit Function
    Do
        ' Cut out both parts marked by null character
        iEnd = InStr(iStart, sFilters, vbNullChar)
        If iEnd = 0 Then Exit Function
        iEnd = InStr(iEnd + 1, sFilters, vbNullChar)
        If iEnd Then
            S = Mid$(sFilters, iStart, iEnd - iStart)
        Else
            S = Mid$(sFilters, iStart)
        End If
        iStart = iEnd + 1
        If iCur = 1 Then
            FilterLookup = S
            Exit Function
        End If
        iCur = iCur - 1
    Loop While iCur
End Function

Function VBGetFileTitle(sFIle As String) As String
    Dim sFileTitle As String, cFileTitle As Integer

    cFileTitle = MAX_PATH
    sFileTitle = String$(MAX_PATH, 0)
    cFileTitle = GetFileTitle(sFIle, sFileTitle, MAX_PATH)
    If cFileTitle Then
        VBGetFileTitle = ""
    Else
        VBGetFileTitle = Left$(sFileTitle, InStr(sFileTitle, vbNullChar) - 1)
    End If

End Function

' ChooseColor wrapper
Function VBChooseColor(COLOR As Long, _
                       Optional AnyColor As Boolean = True, _
                       Optional FullOpen As Boolean = False, _
                       Optional DisableFullOpen As Boolean = False, _
                       Optional Owner As Long = -1, _
                       Optional Flags As Long _
                    ) As Boolean

    Dim chclr As TCHOOSECOLOR
    chclr.lStructSize = Len(chclr)
    
    ' Color must get reference variable to receive result
    ' Flags can get reference variable or constant with bit flags
    ' Owner can take handle of owning window
    If Owner <> -1 Then chclr.hwndOwner = Owner

    ' Assign color (default uninitialized value of zero is good default)
    chclr.rgbResult = COLOR

    ' Mask out unwanted bits
    Dim afMask As Long
    afMask = CLng(Not (CC_ENABLEHOOK Or _
                       CC_ENABLETEMPLATE))
    ' Pass in flags
    chclr.Flags = afMask And (CC_RGBINIT Or _
                  IIf(AnyColor, CC_ANYCOLOR, CC_SOLIDCOLOR) Or _
                  (-FullOpen * CC_FULLOPEN) Or _
                  (-DisableFullOpen * CC_PREVENTFULLOPEN))
    
    ' If first time, initialize to white
    If fNotFirst = False Then InitColors

    chclr.lpCustColors = VarPtr(alCustom(0))
    ' All other fields zero
    
    m_lApiReturn = ChooseColor(chclr)
    Set m_oEventSink = Nothing
    
    Select Case m_lApiReturn
    Case 1
        ' Success
        VBChooseColor = True
        COLOR = chclr.rgbResult
        COLa = COLOR
    Case 0
        ' Cancelled
        VBChooseColor = False
        COLOR = -1
    Case Else
        ' Extended error
        m_lExtendedError = CommDlgExtendedError()
        VBChooseColor = False
        COLOR = -1
    End Select

End Function

Friend Sub InitColors()
    Dim I As Integer
    ' Initialize with first 16 system interface colors
    For I = 0 To 15
        alCustom(I) = GetSysColor(I)
    Next
    fNotFirst = True

End Sub
Public Property Get COULEUR() As Long
COULEUR = COLa

End Property





' Property to read or modify custom colors (use to save colors in registry)
 Property Get CustomColor(I As Integer) As Long
    ' If first time, initialize to white
    If fNotFirst = False Then InitColors
    If I >= 0 And I <= 15 Then
        CustomColor = alCustom(I)
    Else
        CustomColor = -1
    End If
End Property

 Property Let CustomColor(I As Integer, iValue As Long)
    ' If first time, initialize to system colors
    If fNotFirst = False Then InitColors
    If I >= 0 And I <= 15 Then
        alCustom(I) = iValue
    End If
End Property

' ChooseFont wrapper

' PrintDlg wrapper
Function VBPrintDlg(hdc As Long, _
                    Optional PrintRange As EPrintRange = eprAll, _
                    Optional DisablePageNumbers As Boolean, _
                    Optional FromPage As Long = 1, _
                    Optional ToPage As Long = &HFFFF, _
                    Optional DisableSelection As Boolean, _
                    Optional Copies As Integer, _
                    Optional ShowPrintToFile As Boolean, _
                    Optional DisablePrintToFile As Boolean = True, _
                    Optional PrintToFile As Boolean, _
                    Optional Collate As Boolean, _
                    Optional PreventWarning As Boolean, _
                    Optional Owner As Long, _
                    Optional Printer As Object, _
                    Optional Flags As Long _
                ) As Boolean
    Dim afFlags As Long, afMask As Long
    
    m_lApiReturn = 0
    m_lExtendedError = 0
    
    ' Set PRINTDLG flags
    afFlags = (-DisablePageNumbers * PD_NOPAGENUMS) Or _
              (-DisablePrintToFile * PD_DISABLEPRINTTOFILE) Or _
              (-DisableSelection * PD_NOSELECTION) Or _
              (-PrintToFile * PD_PRINTTOFILE) Or _
              (-(Not ShowPrintToFile) * PD_HIDEPRINTTOFILE) Or _
              (-PreventWarning * PD_NOWARNING) Or _
              (-Collate * PD_COLLATE) Or _
              PD_USEDEVMODECOPIESANDCOLLATE Or _
              PD_RETURNDC
    If PrintRange = eprPageNumbers Then
        afFlags = afFlags Or PD_PAGENUMS
    ElseIf PrintRange = eprSelection Then
        afFlags = afFlags Or PD_SELECTION
    End If
    ' Mask out unwanted bits
    afMask = CLng(Not (PD_ENABLEPRINTHOOK Or _
                       PD_ENABLEPRINTTEMPLATE))
    afMask = afMask And _
             CLng(Not (PD_ENABLESETUPHOOK Or _
                       PD_ENABLESETUPTEMPLATE))
    
    ' Fill in PRINTDLG structure
    Dim pd As TPRINTDLG
    pd.lStructSize = Len(pd)
    pd.hwndOwner = Owner
    pd.Flags = afFlags And afMask
    pd.nFromPage = FromPage
    pd.nToPage = ToPage
    pd.nMinPage = 1
    pd.nMaxPage = &HFFFF
    
    ' Show Print dialog
    m_lApiReturn = PrintDlg(pd)
    Set m_oEventSink = Nothing
    Select Case m_lApiReturn
    Case 1
        VBPrintDlg = True
        ' Return dialog values in parameters
        hdc = pd.hdc
        If (pd.Flags And PD_PAGENUMS) Then
            PrintRange = eprPageNumbers
        ElseIf (pd.Flags And PD_SELECTION) Then
            PrintRange = eprSelection
        Else
            PrintRange = eprAll
        End If
        FromPage = pd.nFromPage
        ToPage = pd.nToPage
        PrintToFile = (pd.Flags And PD_PRINTTOFILE)
        ' Get DEVMODE structure from PRINTDLG
        Dim pDevMode As Long
        pDevMode = GlobalLock(pd.hDevMode)
        CopyMemory m_dvmode, ByVal pDevMode, Len(m_dvmode)
        Call GlobalUnlock(pd.hDevMode)
        ' Get Copies and Collate settings from DEVMODE structure
        Copies = m_dvmode.dmCopies
        Collate = (m_dvmode.dmCollate = DMCOLLATE_TRUE)
                
        ' Set default printer properties
        On Error Resume Next
        If Not (Printer Is Nothing) Then
            Printer.Copies = Copies
            Printer.Orientation = m_dvmode.dmOrientation
            Printer.PaperSize = m_dvmode.dmPaperSize
            Printer.PrintQuality = m_dvmode.dmPrintQuality
        End If
        On Error GoTo 0
    Case 0
        ' Cancelled
        VBPrintDlg = False
    Case Else
        ' Extended error:
        m_lExtendedError = CommDlgExtendedError()
        VBPrintDlg = False
    End Select
    
End Function
Friend Property Get DevMode() As DevMode
    DevMode = m_dvmode
End Property

' PageSetupDlg wrapper
Function VBPageSetupDlg(Optional Owner As Long, _
                        Optional DisableMargins As Boolean, _
                        Optional DisableOrientation As Boolean, _
                        Optional DisablePaper As Boolean, _
                        Optional DisablePrinter As Boolean, _
                        Optional LeftMargin As Long, _
                        Optional MinLeftMargin As Long, _
                        Optional RightMargin As Long, _
                        Optional MinRightMargin As Long, _
                        Optional TopMargin As Long, _
                        Optional MinTopMargin As Long, _
                        Optional BottomMargin As Long, _
                        Optional MinBottomMargin As Long, _
                        Optional PaperSize As EPaperSize = epsLetter, _
                        Optional Orientation As eOrientation = eoPortrait, _
                        Optional PrintQuality As EPrintQuality = epqDraft, _
                        Optional Units As EPageSetupUnits = epsuInches, _
                        Optional Printer As Object, _
                        Optional Flags As Long _
                    ) As Boolean
    Dim afFlags As Long, afMask As Long
        
    m_lApiReturn = 0
    m_lExtendedError = 0
    ' Mask out unwanted bits
    afMask = Not (PSD_ENABLEPAGEPAINTHOOK Or _
                  PSD_ENABLEPAGESETUPHOOK Or _
                  PSD_ENABLEPAGESETUPTEMPLATE)
    ' Set TPAGESETUPDLG flags
    afFlags = (-DisableMargins * PSD_DISABLEMARGINS) Or _
              (-DisableOrientation * PSD_DISABLEORIENTATION) Or _
              (-DisablePaper * PSD_DISABLEPAPER) Or _
              (-DisablePrinter * PSD_DISABLEPRINTER) Or _
              PSD_MARGINS Or PSD_MINMARGINS And afMask
    Dim lUnits As Long
    If Units = epsuInches Then
        afFlags = afFlags Or PSD_INTHOUSANDTHSOFINCHES
        lUnits = 1000
    Else
        afFlags = afFlags Or PSD_INHUNDREDTHSOFMILLIMETERS
        lUnits = 100
    End If
    
    Dim psd As TPAGESETUPDLG
    ' Fill in PRINTDLG structure
    psd.lStructSize = Len(psd)
    psd.hwndOwner = Owner
    psd.rtMargin.Top = TopMargin * lUnits
    psd.rtMargin.Left = LeftMargin * lUnits
    psd.rtMargin.Bottom = BottomMargin * lUnits
    psd.rtMargin.Right = RightMargin * lUnits
    psd.rtMinMargin.Top = MinTopMargin * lUnits
    psd.rtMinMargin.Left = MinLeftMargin * lUnits
    psd.rtMinMargin.Bottom = MinBottomMargin * lUnits
    psd.rtMinMargin.Right = MinRightMargin * lUnits
    psd.Flags = afFlags
    
    ' Show Print dialog
    If PageSetupDlg(psd) Then
        VBPageSetupDlg = True
        ' Return dialog values in parameters
        TopMargin = psd.rtMargin.Top / lUnits
        LeftMargin = psd.rtMargin.Left / lUnits
        BottomMargin = psd.rtMargin.Bottom / lUnits
        RightMargin = psd.rtMargin.Right / lUnits
        MinTopMargin = psd.rtMinMargin.Top / lUnits
        MinLeftMargin = psd.rtMinMargin.Left / lUnits
        MinBottomMargin = psd.rtMinMargin.Bottom / lUnits
        MinRightMargin = psd.rtMinMargin.Right / lUnits
        
        ' Get DEVMODE structure from PRINTDLG
        Dim dvmode As DevMode, pDevMode As Long
        pDevMode = GlobalLock(psd.hDevMode)
        CopyMemory dvmode, ByVal pDevMode, Len(dvmode)
        Call GlobalUnlock(psd.hDevMode)
        PaperSize = dvmode.dmPaperSize
        Orientation = dvmode.dmOrientation
        PrintQuality = dvmode.dmPrintQuality
        ' Set default printer properties
        On Error Resume Next
        If Not (Printer Is Nothing) Then
            Printer.Copies = dvmode.dmCopies
            Printer.Orientation = dvmode.dmOrientation
            Printer.PaperSize = dvmode.dmPaperSize
            Printer.PrintQuality = dvmode.dmPrintQuality
        End If
        On Error GoTo 0
    End If
    Set m_oEventSink = Nothing
End Function

#If fComponent = 0 Then
Private Sub ErrRaise(E As Long)
    Dim sText As String, sSource As String
    If E > 1000 Then
        sSource = App.EXEName & ".CommonDialog"
        Err.Raise COMError(E), sSource, sText
    Else
        ' Raise standard Visual Basic error
        sSource = App.EXEName & ".VBError"
        Err.Raise E, sSource
    End If
End Sub
#End If


Private Sub StrToBytes(aB() As Byte, S As String)
    If IsArrayEmpty(aB) Then
        ' Assign to empty array
        aB = StrConv(S, vbFromUnicode)
    Else
        Dim cab As Long
        ' Copy to existing array, padding or truncating if necessary
        cab = UBound(aB) - LBound(aB) + 1
        If Len(S) < cab Then S = S & String$(cab - Len(S), 0)
        'If UnicodeTypeLib Then
        '    Dim st As String
        '    st = StrConv(s, vbFromUnicode)
        '    CopyMemoryStr ab(LBound(ab)), st, cab
        'Else
            CopyMemoryStr aB(LBound(aB)), S, cab
        'End If
    End If
End Sub


 Function BytesToStr(aB() As Byte) As String
    BytesToStr = StrConv(aB, vbUnicode)
End Function

Private Function COMError(E As Long) As Long
    COMError = E Or vbObjectError
End Function
'
 Function IsArrayEmpty(va As Variant) As Boolean
    Dim v As Variant
    On Error Resume Next
    v = va(LBound(va))
    IsArrayEmpty = (Err <> 0)
End Function




Public Property Get Filename() As String
  Filename = Name
End Property

Public Property Let Filename(nom As String)
   Name = nom
End Property


Public Property Get FILTER() As String
FILTER = Filta

End Property

Public Property Let FILTER(ByVal vNewValue As String)
  Filta = vNewValue
End Property


Public Property Get DialogTitle() As String
   DialogTitle = Title
End Property

Public Property Let DialogTitle(ByVal vNewValue As String)
    Title = vNewValue
End Property


Function About()

Call MsgBox("Johna comdlg32 by john gréguy Pierre", 64, "About it")
End Function





Public Property Get Folder() As String
      Folder = BROSA
End Property
Public Property Get Path() As String
Path = MDIR

End Property

Public Property Let Path(ByVal vNewValue As String)
MDIR = vNewValue
End Property



Public Sub GetFolDER(HAND As Long)
Dim ret As Long
Dim physpath  As String
BRO.hwndOwner = HAND
BRO.pszDisplayName = Space(260)
BRO.lpszTitle = "Please choose a folder."  ' Message displayed to the user
BRO.ulFlags = 0
 ret = SFolder(BRO)
  physpath = Space(260)
  ret = SHGetPathFromIDList(ret, physpath)
  physpath = Left(physpath, InStr(physpath, vbNullChar) - 1)
    
  BROSA = physpath
End Sub
