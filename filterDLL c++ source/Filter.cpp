// Filter.cpp : Defines the entry point for the DLL application.
//

/*===================================================
' This Dll show how to call a VB CallBackfunction from
'  a c++ routines declared in a dll
'
'
'It also shows how to manage VB array for making
' fast image processing method and boost VB
'
'C/C++ Variable name Visual Basic Variable Name 
'		Short					Integer 
'		Long					Long 
'		LPLong					Long 
'		Float					Single 
'		Double					Double 
'		unsigned __int8/BYTE    Byte 
'		BStr					String 
'
'
'For passing an array from VB to C++
'  we use a pointer in the C++ function that hold the starting
'  element 
'   EX:  dim MyArray(14,14) as integer
'         to pass this array in a C++ function call AddTen
'         in VB Addten(Myarray(0,0))
'
'         The C++ function would be like that
'          void _stdcall AddTen(short *MyArray)
'				{
'					short Temp=*MyAray //Copy our variable
'					Temp+=10;  add ten
'					*MyArray=Temp;  Put it back
'				    MyArray++;  increment by one
'				}
'
'
'With that way it's now possible to create Fast image processing Routines
'Check my sample filter Routines for more details
'
'
'IMPORTANT i'm waiting for image processing routines to make
'a huge graphic library for VB people
'  i'm interesting in any Fx filter found with my ApplyKernelRVB  
'   to use it
'     ==>firt fill an RVB array variable in VB ex Pixel(250,250) for a 251*251 picture
'
'     ==>second Fill a convolution Kernel Mask like Kernel(0 to 8) in VB
'           for example for soften Kernel=(11,11,11,11,11,11,11,11,11)
'                   DevideValue=99  and Addcolor=0
'     ==>Third
'       call ApplyKernelToRVB Pixel(0,0),251,251,Kernel(0),99,0
'

'
'  With this method there exist unlimited possible effect
'   any FX found write me at Johna.pop@caramail.com
'
'
'
'If someone know algorythm for oilpainting Filter,OilBrushPainting or any artistic effect
'Write me at Johna.pop@caramail.com
'
'
'For the correct declaration of that routines in VB see the VB code sample
'
'=================================================================*/













#include "stdafx.h"
#include <stdlib.h> //for malloc
#include <math.h>

//for progress actualy the VB CallBack Function declaration
typedef BOOL (CALLBACK* PROGRESS) (int);

/*Image processing useful class*/
struct RVB
{
 
 BYTE  Rouge,Vert,Bleu,Reserved;

inline RVB::Invert()
//
// Desc:   invert RVB
//
{
	Rouge=255-Rouge;
	Vert=255-Vert;
	Bleu=255-Bleu;
	
}


   inline RVB::Color(BYTE R,BYTE G,BYTE B)
 {
    Rouge =R;
	Vert = G;
	Bleu = B;

 }
   //RVB from a long value
	   	   inline RVB::Color(long COLOR)
 {
    Rouge =GetRValue(COLOR);
	Vert = GetGValue(COLOR);
	Bleu = GetBValue(COLOR);

 }

		   inline RVB::Color(BYTE X)
//
// Desc:   Makes gray RVB with intensity X
//
{
	Rouge = X;
	Vert = X;
	Bleu = X;
}

		   inline RVB::Color(float R, float G, float B)
//
// Desc:   Colors a RVB given the float values R, G, and B.
//         If the float is < 0 or > 255, the color will be set
//         to 0 or 255 respectively.
//
{
	if (R < 0) R = 0;
	if (R > 255) R = 255;
	if (G < 0) G = 0;
	if (G > 255) G = 255;
	if (B < 0) B = 0;
	if (B > 255) B = 255;

	Rouge = (BYTE)R;
	Vert = (BYTE)G;
	Bleu = (BYTE)B;
}


		   inline BYTE RVB::intensity()
//
// Desc:   Returns the intensity of a RGB RVB.  This is the Luminance (Y)
//         component of the YIQ color model (used in TV displays).  Intensity
//         is most commonly used as the basis for color image analysis.  This
//         intensity 
//
{
	return BYTE(0.3*Rouge + 0.59*Vert + 0.11*Bleu);
}




		   inline void RVB::lighten(UINT i)
//
// Desc:   Lightens a RVB by i.
//
{
	if (Rouge >= 255-i) Rouge = 255;
	else Rouge += i;
	if (Vert >= 255-i) Vert = 255;
	else Vert += i;
	if (Bleu >= 255-i) Bleu = 255;
	else Bleu += i;
}

inline RVB::darken(UINT i)
//
// Desc:   Darkens a RVB by i.
//
{
	if (Rouge <= i) Rouge = 0;
	else Rouge -= i;
	if (Vert <= i) Vert = 0;
	else Vert -= i;
	if (Bleu <= i) Bleu = 0;
	else Bleu -= i;
}
};



BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
    return TRUE;
}



/*=================================================================
/Sample function that call a VB function pointer by using the operator
/AddressOf from VB
/The call Back function must be like this prototype
/  
/EX:in VB

 Function Progress(ByVal percent As Integer) As Boolean
  
  
  Form1.Caption = CStr(percent) + "%"
  ProgressBar1.Value = percent
  Progress = True
  
End Function

  MAKE SURE THE FUNCTION IS INSIDE A VB MODULE FILE

  and inside VB this function can be called like This

  Firts==> Function declaration inside VB
     Declare Function Draw Lib "Filter.dll" (ByVal DC As Long, ByVal WI As Integer, ByVal HI As Integer, lpCallback As Any) As Long
  And
    Calling the function like this


=================================================================*/

long _stdcall  Draw(HDC dc,short WI, short HI,PROGRESS &LpProgress)

{
    int I,J;
	DWORD Z;
	long ZZ=0;

	
	for (I=0;I<WI;I++)
	{
        for (J=0;J<HI;J++)
		SetPixelV(dc,I,J,I*J );


		Z=(long)(100*(float)(DWORD)I/(float)(WI-1));
		LpProgress((int)Z);
	}
    
	return 1;
}







/*=======================================================
/  Image processing sample function
/
/
 /========================================================*/

void _stdcall  InvertRVB(RVB *myarray, short width, short height)//,PROGRESS &LpProgress) 
{ 
int x=0; //Create us a variable 

x= ((width-1) * (height-1)); //This is the total number of entries in the array. 
RVB  Val; //Temporary Value 


for(int i = 0; i<=x; i++) //Run a loop through the data. There should 
//be a less-than sign in the prev. line - but it messed up the HTML... 
{ 
	Val = *myarray; //Copy our variable 
	
	Val.Invert();
	
	*myarray = Val; //Put our variable back 
	myarray++; //Increment the address by one.
	
	
}
}



void _stdcall LightenRVB(RVB *myarray, short width, short height,UINT LightLevel) 
{ 
int x=0; //Create us a variable 
x= ((width-1) * (height-1)); //This is the total number of entries in the array. 
RVB  Val; //Temporary Value 


for(int i = 0; i<=x; i++) //Run a loop through the data. There should 
//be a less-than sign in the prev. line - but it messed up the HTML... 
{ 
	Val = *myarray; //Copy our variable 
	
	Val.lighten(LightLevel);
	
	*myarray = Val; //Put our variable back 
	myarray++; //Increment the address by one.
}
}


void _stdcall DarKenRVB(RVB *myarray, short width, short height,UINT DarkLevel) 
{ 
int x=0; //Create us a variable 
x= ((width-1) * (height-1)); //This is the total number of entries in the array. 
RVB  Val; //Temporary Value 


for(int i = 0; i<=x; i++) //Run a loop through the data. There should 
//be a less-than sign in the prev. line - but it messed up the HTML... 
{ 
	Val = *myarray; //Copy our variable 
	
	Val.darken(DarkLevel);
	
	*myarray = Val; //Put our variable back 
	myarray++; //Increment the address by one.
}
}


void _stdcall GrayIntensityRVB(RVB *myarray, short width, short height) 
{ 
int x=0; //Create us a variable 
x= ((width-1) * (height-1)); //This is the total number of entries in the array. 
RVB  Val; //Temporary Value 
BYTE C;

for(int i = 0; i<=x; i++) //Run a loop through the data. There should 
//be a less-than sign in the prev. line - but it messed up the HTML... 
{ 
	Val = *myarray; //Copy our variable 
	
	C=Val.intensity();
    Val.Color(C);
	
	*myarray = Val; //Put our variable back 
	myarray++; //Increment the address by one.
}
}





void _stdcall BlurRVB(RVB *myarray, short width, short height,int BlurRadius,PROGRESS &LpProgress) 
{ 

	
int x=0; //Create us a variable 


x= ((width-1) * (height-1)); //This is the total number of entries in the array. 
RVB  Val; //Temporary Value 
DWORD ZZ;

UINT Z=0;
int II,JJ;
float R,G,B;
int RAD=BlurRadius;
int NumRVBs;
int I,J;
if (BlurRadius<1) goto END;

for( I = 0; I<height; I++) //Run a loop through the data. There should 
//be a less-than sign in the prev. line - but it messed up the HTML... 
{ 
  
	for( J=0;J<width;J++)
	{
        
		/*Z=J*width+I+1;
		Val=myarray[Z];
		Val.Invert();*/
		R=0;
		G=0;
		B=0;
		NumRVBs=0;
		
		
		for(int K=-RAD;K<=RAD;K++)
		{

		  for(int L=-RAD;L<=RAD;L++)
		  {

             II=I+K;
			 JJ=J+L;
                     //Increments RVBRGB value
			  if (II>= 0 && II < (int)height && JJ >= 0 && JJ < (int)width)
						{
							Val=myarray[II*width+JJ];
							R += (float)Val.Rouge;
							G += (float)Val.Vert;
							B += (float)Val.Bleu;
							NumRVBs++;
						}
		  } //End for



		} //End for 
        Val.Color(R/NumRVBs,G/NumRVBs,B/NumRVBs);
        myarray[I*width+J]=Val;

	}
	//update the progression level
	ZZ=(long)(100*(float)(DWORD)I/(float)(height-1));
		LpProgress((int)ZZ);
}


END:;

}




void _stdcall MorphFishEyeRVB(RVB *myarray, short width, short height,float CurvingVAL,PROGRESS &LpProgress) 
{ 

	
 //This is the total number of entries in the array. 
RVB  Val; //Temporary Value 

UINT Z=0;

	// dynamic array allocation
    int length=width*height;
	RVB* TAB2 = NULL;
	TAB2 = (RVB*)malloc(length * sizeof(RVB));

	for(int  i=0;i<length;i++)
		TAB2[i]=myarray[i];


int NumRVBs;
int I,J;

	double		 r, a;								// polar coordinates (radius, angle)
	unsigned int R  = (min(width, height)) / 2; // maxium radius of polar coordinates
	
	double		 w = 0.001 * (double)CurvingVAL;		// curvature [0.001,0.1]
													//   m_iCurvature will return [1,100]
	double		 s = R / log(w*R+1);				// transformation coefficient
													//   set according to largest radius
													//   and curvature w

	int w2 = width  / 2;
	int h2 = height / 2;
    BOOL inverse=FALSE;
	int	x, y;								// converted cartesian coordinates
	DWORD ZZ;
	int MaxZZ=abs(-1*h2)+((int)height-h2);


for (I = -1*h2; I < ((int)height-h2); I++) //Run a loop through the data. There should 
//be a less-than sign in the prev. line - but it messed up the HTML... 
{ 
  
	for (J = -1*w2; J < ((int)width-w2); J++)
	{
        
		/*Z=J*width+I+1;
		Val=myarray[Z];
		Val.Invert();*/
		
		NumRVBs=0;
		
		
		
       // convert to polar
			r = sqrt(I*I + J*J);
			a = atan2((float)I, J);		// calculates arctan(i/j)

			// if we're inside R (i.e. inside the circle) do...
			if (r <= R)
			{
				// make transformation using Basu and Licardie model [DEVE01]
				if (!inverse)
					r = (exp(r/s)-1)/w;
				else
					r = s * log(1+w*r);
		
				// convert back to cartesian
				x = int  (r * cos(a));
				y = int  (r * sin(a));

				// move origin back to bottom left
				
				x += w2;
				y += h2;
				
				Val=TAB2[y*width+x];
				

				myarray[(I+h2)*width+(J+w2)]=Val;

			}
			//We could colorize to Black or White

	}
	//update the progression level
	ZZ=(long)(100*(float)(DWORD)abs(Z)/(float)(MaxZZ-1));
		LpProgress((int)ZZ);
		Z++;
}


free( TAB2);

}




/*===========================================================
' This method apply a 3*3 MasK kernel to a RVB pixel array
' each pixel is multiplied by a kernel  and is divide by the
' devide parameter and finaly added by the AddColor parameter
'
'
'
'=============================================================*/



void _stdcall ApplyKernelToRVB(RVB *myarray, short width, short height,float *KernelArray,float DevideVal,float AddColor,PROGRESS &LpProgress) 
{ 

	
//RVB  Val; //Temporary Value 


float R,G,B;


int I,J;
DWORD Z;
	long ZZ=0;




	// dynamic array allocation
    int length=width*height;
	RVB* TAB2 = NULL;
	TAB2 = (RVB*)malloc(length * sizeof(RVB));

	for( int i=0;i<length;i++)
		TAB2[i]=myarray[i];


for( I = 1; I<height-1; I++) //Run a loop through the data. There should 
//be a less-than sign in the prev. line - but it messed up the HTML... 
{ 
  
	for( J=1;J<width-1;J++)
	{
        
		/*Z=J*width+I+1;
		Val=myarray[Z];
		Val.Invert();*/
		R=0;
		G=0;
		B=0;
	
		
						
						
							
		R=R+(float)TAB2[(I-1)*width+(J-1)].Rouge*KernelArray[0]+
		  (float)TAB2[(I)*width+(J-1)].Rouge*KernelArray[1]+
		  (float)TAB2[(I+1)*width+(J-1)].Rouge*KernelArray[2]+
	(float)TAB2[(I-1)*width+(J)].Rouge*KernelArray[3]+
		  (float)TAB2[(I)*width+(J)].Rouge*KernelArray[4]+
		  (float)TAB2[(I+1)*width+(J)].Rouge*KernelArray[5]+
	(float)TAB2[(I-1)*width+(J+1)].Rouge*KernelArray[6]+
		  (float)TAB2[(I)*width+(J+1)].Rouge*KernelArray[7]+
		  (float)TAB2[(I+1)*width+(J+1)].Rouge*KernelArray[8];


			G=G+(float)TAB2[(I-1)*width+(J-1)].Vert*KernelArray[0]+
		  (float)TAB2[(I)*width+(J-1)].Vert*KernelArray[1]+
		  (float)TAB2[(I+1)*width+(J-1)].Vert*KernelArray[2]+
	(float)TAB2[(I-1)*width+(J)].Vert*KernelArray[3]+
		  (float)TAB2[(I)*width+(J)].Vert*KernelArray[4]+
		  (float)TAB2[(I+1)*width+(J)].Vert*KernelArray[5]+
	(float)TAB2[(I-1)*width+(J+1)].Vert*KernelArray[6]+
		  (float)TAB2[(I)*width+(J+1)].Vert*KernelArray[7]+
		  (float)TAB2[(I+1)*width+(J+1)].Vert*KernelArray[8];


		B=B+(float)TAB2[(I-1)*width+(J-1)].Bleu*KernelArray[0]+
		  (float)TAB2[(I)*width+(J-1)].Bleu*KernelArray[1]+
		  (float)TAB2[(I+1)*width+(J-1)].Bleu*KernelArray[2]+
	(float)TAB2[(I-1)*width+(J)].Bleu*KernelArray[3]+
		  (float)TAB2[(I)*width+(J)].Bleu*KernelArray[4]+
		  (float)TAB2[(I+1)*width+(J)].Bleu*KernelArray[5]+
	(float)TAB2[(I-1)*width+(J+1)].Bleu*KernelArray[6]+
		  (float)TAB2[(I)*width+(J+1)].Bleu*KernelArray[7]+
		  (float)TAB2[(I+1)*width+(J+1)].Bleu*KernelArray[8];














        
		R=R/DevideVal+AddColor;
		G=G/DevideVal+AddColor;
		B=B/DevideVal+AddColor;
        
		if (R>255)
			R=255;
		if (R<0)
			R=0;

			if (G>255)
			G=255;
		if (G<0)
			G=0;

			if (B>255)
			B=255;
		if (B<0)
			B=0;

	
		
        myarray[I*width+J].Rouge=(BYTE)R;

        myarray[I*width+J].Vert=(BYTE)G;
        myarray[I*width+J].Bleu=(BYTE)B;
		
	/*	for (row = 0; row < height; row++)
	{
		for (col = 0; col < width; col++
//[row * width * 3 + col * 3*/

	}
	Z=(long)(100*(float)(DWORD)I/(float)(height-1));
		LpProgress((int)Z);
}


free(TAB2);

}   //End apply kernel

