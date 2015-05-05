// ***********************************************************************
//  ECE 362 - Experiment 7 - Fall 2014     
//
// Dual-channel LED bar graph display                    
// ***********************************************************************
//	 	   			 		  			 		  		
// Completed by: <   >
//               <   >
//               <   >
//
//
// Academic Honesty Statement:  In entering my name above, I hereby certify
// that I am the individual who created this HC(S)12 source file and that I 
// have not copied the work of any other student (past or present) while 
// completing it. I understand that if I fail to honor this agreement, I will 
// receive a grade of ZERO and be subject to possible disciplinary action.
//
// ***********************************************************************

#include <hidef.h>           /* common defines and macros */
#include "derivative.h"      /* derivative-specific definitions */
#include <mc9s12c32.h>

// All funtions after main should be initialized here

// Note: inchar and outchar can be used for debugging purposes

char inchar(void);
void outchar(char x);
void shift(int);
			 		  		
//  Variable declarations  	   			 		  			 		       
int tenthsec = 0;  // one-tenth second flag
int leftpb = 0;    // left pushbutton flag
int rghtpb = 0;    // right pushbutton flag
int runstp = 0;    // run/stop flag                         
int rticnt = 0;    // RTICNT (variable)
int prevpb = 0;    // previous state of pushbuttons (variable)
int val0 = 0;
int val1 = 0;
int d = 0;
	 	   		
// Initializations
 
void  initializations(void) {

// Set the PLL speed (bus clock = 24 MHz)

  		CLKSEL = CLKSEL & 0x80; // disengage PLL from system
  		PLLCTL = PLLCTL | 0x40; // turn on PLL
  		SYNR = 0x02;            // set PLL multiplier
  		REFDV = 0;              // set PLL divider
  		while (!(CRGFLG & 0x08)){  }
  		CLKSEL = CLKSEL | 0x80; // engage PLL
  
// Disable watchdog timer (COPCTL register)

      COPCTL = 0x40;    //COP off - RTI and COP stopped in BDM-mode

// Initialize asynchronous serial port (SCI) for 9600 baud, no interrupts

      SCIBDH =  0x00; //set baud rate to 9600
      SCIBDL =  0x9C; //24,000,000 / 16 / 156 = 9600 (approx)  
      SCICR1 =  0x00; //$9C = 156
      SCICR2 =  0x0C; //initialize SCI for program-driven operation
         
//  Initialize Port AD pins 7 and 6 for use as digital inputs

	    DDRAD = 0; 		//program port AD for input mode
      ATDDIEN = 0xC0; //program PAD7 and PAD6 pins as digital inputs
         
//  Add additional port pin initializations here  (e.g., Other DDRs, Ports) 


//  Define bar graph segment thresholds (THRESH1..THRESH5)
//  NOTE: These are binary fractions
    #define THRESH1 0x40; // 0.25
    #define THRESH2 0x60; // 0.375
    #define THRESH3 0x80; // 0.5
    #define THRESH4 0xA0; // 0.625
    #define THRESH5 0xC0; // 0.75

//  Add RTI/interrupt initializations here
    CRGINT = 0x80; // Turn on RTI interrupts
    RTICTL = 0x70; // Run interrupts at 8.192ms
    ATDCTL3 = 0x10;
    DDRT = 0xFF;

}
	 		  			 		  		
 
// Main (non-terminating loop)
 
void main(void) {
	initializations(); 		  			 		  		
	EnableInterrupts;


  for(;;) {

// Main program loop (state machine)
// Start of main program-driven polling loop

	 	   			 		  			 		  		
//  If the "tenth second" flag is set, then
//    - clear the "tenth second" flag
//    - if "run/stop" flag is set, then
//       - initiate ATD coversion sequence
//       - apply thresholds to converted values
//       - determine 5-bit bar graph bit settings for each input channel
//       - transmit 10-bit data to external shift register
//    - endif
//  Endif
    if(tenthsec)
    {
        tenthsec = 0;
        if(runstp)
        {
            ATDCTL2 = 0x80; 
            ATDCTL5 = 0x10; //Current guess
            
            if(ATDDR0H >= 0xC0){
              outchar(0x35);
              val0 = 0x1F;
            } else if(ATDDR0H >= 0xA0){
              outchar(0x34);
              val0 = 0x0F;
            } else if(ATDDR0H >= 0x80){
              outchar(0x33);
              val0 = 0x07;
            } else if(ATDDR0H >= 0x60){
              outchar(0x32);
              val0 = 0x03;
            } else if(ATDDR0H >= 0x40){
              outchar(0x31);
              val0 = 0x01;
            } else if(ATDDR0H < 0x40){
              outchar(0x30);
              val0 = 0x00;
            }
            
            if(ATDDR1H >= 0xC0){
              outchar(0x35);
              val1 = 0x1F;
            } else if(ATDDR1H >= 0xA0){
              outchar(0x34);
              val1 = 0x0F;
            } else if(ATDDR1H >= 0x80){
              outchar(0x33);
              val1 = 0x07;
            } else if(ATDDR1H >= 0x60){
              outchar(0x32);
              val1 = 0x03;
            } else if(ATDDR1H >= 0x40){
              outchar(0x31);
              val1 = 0x01;
            } else if(ATDDR1H < 0x40){
              outchar(0x30);
              val1 = 0x00;
            }
            outchar(0x0A);
            outchar(0x0D);
            
            shift(val0);
            shift(val1);
        }
    }
              
	 	   			 		  			 		  		
//  If the left pushbutton ("stop BGD") flag is set, then:
//    - clear the left pushbutton flag
//    - clear the "run/stop" flag (and "freeze" BGD)
//    - turn on left LED/turn off right LED (on docking module)
//  Endif
   	if(leftpb)
   	{
   	    leftpb = 0;
   	    runstp = 0;
   	    PTT = 0x02;
   	}

//  If the right pushbutton ("start BGD") flag is set, then
//    - clear the right pushbutton flag
//    - set the "run/stop" flag (enable BGD updates)
//    - turn off left LED/turn on right LED (on docking module)
//  Endif
   	if(rghtpb)
   	{
   	    rghtpb = 0;
   	    runstp = 1;
   	    PTT = 0x01;
   	}	 	   			 		  			 		  		

  } /* loop forever */
  
}  /* make sure that you never leave main */



// ***********************************************************************                       
// RTI interrupt service routine: rti_isr
//
//  Initialized for 5-10 ms (approx.) interrupt rate - note: you need to
//    add code above to do this
//
//  Samples state of pushbuttons (PAD7 = left, PAD6 = right)
//
//  If change in state from "high" to "low" detected, set pushbutton flag
//     leftpb (for PAD7 H -> L), rghtpb (for PAD6 H -> L)
//     Recall that pushbuttons are momentary contact closures to ground
//
//  Also, keeps track of when one-tenth of a second's worth of RTI interrupts
//     accumulate, and sets the "tenth second" flag         	   			 		  			 		  		
 
interrupt 7 void RTI_ISR(void)
{
 // set CRGFLG bit to clear RTI device flag
  	CRGFLG = CRGFLG | 0x80;
  	
  	if(prevpb && !PORTAD0_PTAD6) 
  	{
  	    prevpb = 0;
  	    rghtpb = 1;
  	}
  	else if(prevpb && !PORTAD0_PTAD7)
  	{
  	    prevpb = 0;
  	    leftpb = 1;
  	}
  	else 
  	{
  	    prevpb = PORTAD0_PTAD7 && PORTAD0_PTAD6;
  	}
  	
  	if(rticnt++ > 10)
  	{
  	    rticnt = 0;
  	    tenthsec = 1;
  	} 
}

// ***********************************************************************
//                              My Shifting Function
// ***********************************************************************
void shift(int x) {
   d = 5;
   
   while(d--) {
    if(x & 0x01 == 0x01){
      // It is set to one
      PTT_PTT3 = 1;
    } else {
      // It is set to zero
      PTT_PTT3 = 0;
    }
    PTT_PTT4 = 1;
    PTT_PTT4 = 0;
    x /= 2;
   }
}



// ***********************************************************************
// Character I/O Library Routines for 9S12C32 (for debugging only)
// ***********************************************************************
// Name:         inchar
// Description:  inputs ASCII character from SCI serial port and returns it
// ***********************************************************************
char  inchar(void) {
  /* receives character from the terminal channel */
        while (!(SCISR1 & 0x20)); /* wait for RDR input */
    return SCIDRL;
 
}

// ***********************************************************************
// Name:         outchar
// Description:  outputs ASCII character passed in outchar()
//                  to the SCI serial port
// ***********************************************************************/
void outchar(char ch) {
  /* transmits a character to the terminal channel */
    while (!(SCISR1 & 0x80));  /* wait for TDR empty */
    SCIDRL = ch;
}

