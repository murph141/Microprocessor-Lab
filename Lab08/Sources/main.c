/*
***********************************************************************
 ECE 362 - Experiment 8 - Fall 2014
***********************************************************************
	 	   			 		  			 		  		
 Completed by: <   >
               <   >
               <   >


 Academic Honesty Statement:  In entering my name above, I hereby certify
 that I am the individual who created this HC(S)12 source file and that I 
 have not copied the work of any other student (past or present) while 
 completing it. I understand that if I fail to honor this agreement, I will 
 receive a grade of ZERO and be subject to possible disciplinary action.

***********************************************************************

 The objective of this experiment is to implement a reaction time assessment
 tool that measures, with millisecond accuracy, response to a visual
 stimulus -- here, both a YELLOW LED and the message "Go Team!" displayed on 
 the LCD screen.  The TIM module will be used to generate periodic 
 interrupts every 1.000 ms, to serve as the time base for the reaction measurement.  
 The RTI module will provide a periodic interrupt at a 2.048 ms rate to serve as 
 a time base for sampling the pushbuttons and incrementing the variable "random" 
 (used to provide a random delay for starting a reaction time test). The SPI
 will be used to shift out data to an 8-bit SIPO shift register.  The shift
 register will perform the serial to parallel data conversion for the LCD.

 The following design kit resources will be used:

 - left LED (PT1): indicates test stopped (ready to start reaction time test)
 - right LED (PT0): indicates a reaction time test is in progress
 - left pushbutton (PAD7): starts reaction time test
 - right pushbutton (PAD6): stops reaction time test (turns off right LED
                    and turns left LED back on, and displays test results)
 - LCD: displays status and result messages
 - Shift Register: performs SPI -> parallel conversion for LCD interface

 When the right pushbutton is pressed, the reaction time is displayed
 (refreshed in place) on the first line of the LCD as "RT = NNN ms"
 followed by an appropriate message on the second line 
 e.g., 'Ready to start!' upon reset, 'Way to go HAH!!' if a really 
 fast reaction time is recorded, etc.). The GREEN LED should be turned on
 for a reaction time less than 250 milliseconds and the RED LED should be
 turned on for a reaction time greater than 1 second.

***********************************************************************
*/

#include <hidef.h>      /* common defines and macros */
#include "derivative.h"      /* derivative-specific definitions */
#include <mc9s12c32.h>

/* All funtions after main should be initialized here */
char inchar(void);
void outchar(char x);
void tdisp();
void shiftout(char x);
void lcdwait(void);
void send_byte(char x);
void send_i(char x);
void chgline(char x);
void print_c(char x);
void pmsglcd(char[]);
int helper(int x);

/* Variable declarations */  	   			 		  			 		       
char goteam 	= 0;  // "go team" flag (used to start reaction timer)
char leftpb	= 0;  // left pushbutton flag
char rghtpb	= 0;  // right pushbutton flag
char prevpb	= 0;  // previous pushbutton state
char runstp	= 0;  // run/stop flag
int random	= 0;  // random variable (2 bytes)
int react	= 0;  // reaction time (3 packed BCD digits)

/* ASCII character definitions */
#define CR 0x0D	// ASCII return character   

/* LCD COMMUNICATION BIT MASKS */
#define RS 0x04		// RS pin mask (PTT[2])
#define RW 0x08		// R/W pin mask (PTT[3])
#define LCDCLK 0x10	// LCD EN/CLK pin mask (PTT[4])

/* LCD INSTRUCTION CHARACTERS */
#define LCDON 0x0F	// LCD initialization command
#define LCDCLR 0x01	// LCD clear display command
#define TWOLINE 0x38	// LCD 2-line enable command
#define CURMOV 0xFE	// LCD cursor move instruction
#define LINE1 0x80	// LCD line 1 cursor position
#define LINE2 0xC0	// LCD line 2 cursor position

/* LED BIT MASKS */
#define GREEN 0x20
#define RED 0x40
#define YELLOW 0x80
char x;
	 	   		
/*
***********************************************************************
 Initializations
***********************************************************************
*/

void  initializations(void) {

/* Set the PLL speed (bus clock = 24 MHz) */
  CLKSEL = CLKSEL & 0x80; // disengage PLL from system
  PLLCTL = PLLCTL | 0x40; // turn on PLL
  SYNR = 0x02;            // set PLL multiplier
  REFDV = 0;              // set PLL divider
  while (!(CRGFLG & 0x08)){  }
  CLKSEL = CLKSEL | 0x80; // engage PLL

/* Disable watchdog timer (COPCTL register) */
  COPCTL = 0x40;   //COP off, RTI and COP stopped in BDM-mode

/* Initialize asynchronous serial port (SCI) for 9600 baud, no interrupts */
  SCIBDH =  0x00; //set baud rate to 9600
  SCIBDL =  0x9C; //24,000,000 / 16 / 156 = 9600 (approx)  
  SCICR1 =  0x00; //$9C = 156
  SCICR2 =  0x0C; //initialize SCI for program-driven operation
  DDRB   =  0x10; //set PB4 for output mode
  PORTB  =  0x10; //assert DTR pin on COM port         
         
/* Add additional port pin initializations here */
  DDRT = 0xFF;
  DDRM = 0xFF;
  DDRAD = 0x00;
  ATDDIEN = 0xC0;

/* Initialize SPI for baud rate of 6 Mbs */
  SPIBR_SPR0 = 0;
  SPIBR_SPR1 = 0;
  SPIBR_SPR2 = 0;
  
  SPIBR_SPPR0 = 1;
  SPIBR_SPPR1 = 0;
  SPIBR_SPPR2 = 0;
  
  SPICR1 = 0x50;
  SPICR2 = 0;
  
/* Initialize digital I/O port pins */
  // See above

/* Initialize the LCD
     - pull LCDCLK high (idle)
     - pull R/W' low (write state)
     - turn on LCD (LCDON instruction)
     - enable two-line mode (TWOLINE instruction)
     - clear LCD (LCDCLR instruction)
     - wait for 2ms so that the LCD can wake up     
*/
  PTT_PTT4 = 1;
  PTT_PTT3 = 0;
  send_i(LCDON);
  send_i(TWOLINE);
  send_i(LCDCLR);
  lcdwait();

/* Initialize RTI for 2.048 ms interrupt rate */	
  RTICTL = 0x1F;
  CRGINT = 0x80;

/* Initialize TIM Ch 7 (TC7) for periodic interrupts every 1.000 ms
     - enable timer subsystem
     - set channel 7 for output compare
     - set appropriate pre-scale factor and enable counter reset after OC7
     - set up channel 7 to generate 1 ms interrupt rate
     - initially disable TIM Ch 7 interrupts      
*/
  TSCR1_TEN = 1;
  TIOS_IOS7 = 1;
  TSCR2_PR0 = 1;
  TSCR2_PR1 = 1;
  TSCR2_PR2 = 0;
  TC7 = 3000;
  TSCR2_TCRE = 1;
  TIE_C7I = 0;
}
	 		  			 		  		
/*
***********************************************************************
 Main
***********************************************************************
*/

void main(void) {
  DisableInterrupts;
	initializations(); 		  			 		  		
	EnableInterrupts;



  for(;;) {

/* write your code here */

/*  If the left pushbutton ("start reaction test") flag is set, then:
     - clear left pushbutton flag
     - set the "run/stop" flag
     - display message "Ready, Set..." on the first line of the LCD
     - turn off the left LED (PT1)
     - turn on the right LED (PT0)
    Endif   
*/
    if(leftpb)
    {
        goteam = 0;
        leftpb = 0;
        runstp = 1;
        
        send_i(LCDCLR);
        chgline(LINE1);
        pmsglcd("Ready, Set...");
        
        PTT_PTT1 = 0;
        PTT_PTT0 = 1;
        PTT_PTT5 = 0;
        PTT_PTT6 = 0;
        PTT_PTT7 = 0;
    }
        
      

/*  If the "run/stop" flag is set, then:
     - If the "goteam" flag is NOT set, then:
        + If "random" = $0000, then:
          - set the "goteam" flag
          - clear TCNT register (of TIM)
          - clear "react" variable (2 bytes)
          - enable TIM Ch7 interrupts
          - turn on YELLOW LED 
          - display message "Go Team!" on the second line of the LCD
       + Endif
     - Endif
    Endif     
*/  
    if(runstp)
    {
        if(!goteam)
        {
            if(random == 0x0000)
            {  
                goteam = 1;
                TSCR2_TCRE = 1;
                react = 0;
                TIE_C7I = 1;
                PTT_PTT7 = 1;
                chgline(LINE2);
                pmsglcd("Go Team!");
            }
        }
    }
  


/*  If the right pushbutton ("stop reaction test") flag is set, then:
     - clear right pushbutton flag
     - clear the "run/stop" flag
     - clear the "goteam" flag
     - turn off yellow LED 
     - disable TIM Ch 7 interrupts
     - call "tdisp" to display reaction time message
     - turn off right LED (PT0)
     - turn on left LED (PT1)
    Endif
*/
    if(rghtpb)
    {
        rghtpb = 0;
        runstp = 0;
        goteam = 0;
        PTT_PTT7= 0;
        TIE_C7I = 0;
        tdisp();
        PTT_PTT0 = 0;
        PTT_PTT1 = 1;
    }


/*  If "react" = 999 (the maximum 3-digit BCD value), then:
     - clear the "run/stop" flag
     - turn off yellow LED, turn on red LED
     - disable TIM Ch 7 interrupts
     - display message "Time = 999 ms" on the first line of the LCD
     - display message "Too slow!" on the second line of the LCD 
     - turn off right LED (PT0)
     - turn on left LED (PT1)
    Endif
*/  
    if(react == 0x0999)
    {
        react = 0;
        runstp = 0;
        PTT_PTT7 = 0;
        PTT_PTT6 = 1;
        TIE_C7I = 0;
        send_i(LCDCLR);
        chgline(LINE1);
        pmsglcd("Time = 999 ms");
        chgline(LINE2);
        pmsglcd("Too slow!");
        PTT_PTT0 = 0;
        PTT_PTT1 = 1;
    }

  } /* loop forever */
  
}  /* do not leave main */




/*
***********************************************************************
 RTI interrupt service routine: RTI_ISR

  Initialized for 2.048 ms interrupt rate

  Samples state of pushbuttons (PAD7 = left, PAD6 = right)

  If change in state from "high" to "low" detected, set pushbutton flag
     leftpb (for PAD7 H -> L), rghtpb (for PAD6 H -> L)
     Recall that pushbuttons are momentary contact closures to ground

  Also, increments 2-byte variable "random" each time interrupt occurs
  NOTE: Will need to truncate "random" to 12-bits to get a reasonable delay 
***********************************************************************
*/

interrupt 7 void RTI_ISR(void)
{
  	// clear RTI interrupt flag
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
  	
  	
  	random++;
  	random &= 0x0FFF;
}

/*
*********************************************************************** 
  TIM Channel 7 interrupt service routine
  Initialized for 1.00 ms interrupt rate
  Increment (3-digit) BCD variable "react" by one
***********************************************************************
*/

interrupt 15 void TIM_ISR(void)
{
	// clear TIM CH 7 interrupt flag
 	TFLG1 = TFLG1 | 0x80;                                               	
 	if(runstp)
 	{  
 	  react = helper(react);
 	}
}

/*
*********************************************************************** 
  tdisp: Display "RT = NNN ms" on the first line of the LCD and display 
         an appropriate message on the second line depending on the 
         speed of the reaction.  
         
         Also, this routine should set the green LED if the reaction 
         time was less than 250 ms.

         NOTE: The messages should be less than 16 characters since
               the LCD is a 2x16 character LCD.
***********************************************************************
*/
 
void tdisp()
{
    send_i(LCDCLR);
    chgline(LINE1);
    pmsglcd("RT = ");
    
    x = ((react >> 8) + 48);
    send_byte(x);
    x = (((react & 0x00F0) >> 4) + 48);
    send_byte(x);
    x = ((react & 0x000F) + 48);
    send_byte(x);
    
    pmsglcd(" ms");
    
    chgline(LINE2);
    
    if(react < 0x0250)
    {
        pmsglcd("Go Whatsamatta U");
        PTT_PTT5 = 1;
    }
    else
    {
        pmsglcd("Get HKN Coffee");

    }
}



int helper(int x)
{
    asm
    {
        ldaa 1, sp
        adda #1
        daa
        staa 1,sp
        ldaa 0,sp
        adca #0
        daa
        staa 0,sp
    }
    
    return x;
}
/*
***********************************************************************
  shiftout: Transmits the character x to external shift 
            register using the SPI.  It should shift MSB first.  
             
            MISO = PM[4]
            SCK  = PM[5]
***********************************************************************
*/
 
void shiftout(char x)

{
 
  // read the SPTEF bit, continue if bit is 1
  // write data to SPI data register
  // wait for 30 cycles for SPI data to shift out 
  while(!SPISR_SPTEF);
  
  SPIDR = x;
  
  asm
  {
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
  }    
}

/*
***********************************************************************
  lcdwait: Delay for approx 2 ms
***********************************************************************
*/

void lcdwait()
{
  asm 
  {
        pshx
        ldx #16000
    start:
        dbne x, start
        pulx
  }
}

/*
*********************************************************************** 
  send_byte: writes character x to the LCD
***********************************************************************
*/

void send_byte(char x)
{
     // shift out character
     // pulse LCD clock line low->high->low
     // wait 2 ms for LCD to process data
     shiftout(x);
     PTT_PTT4 = 0;
     PTT_PTT4 = 1;
     PTT_PTT4 = 0;
     lcdwait();
}

/*
***********************************************************************
  send_i: Sends instruction byte x to LCD  
***********************************************************************
*/

void send_i(char x)
{
      // set the register select line low (instruction data)
      // send byte
      PTT_PTT2 = 0;
      send_byte(x);
}

/*
***********************************************************************
  chgline: Move LCD cursor to position x
  NOTE: Cursor positions are encoded in the LINE1/LINE2 variables
***********************************************************************
*/

void chgline(char x)
{
    send_i(CURMOV);
    send_i(x);
}

/*
***********************************************************************
  print_c: Print (single) character x on LCD            
***********************************************************************
*/
 
void print_c(char x)
{
    PTT_PTT2 = 1;
    send_byte(x);
}

/*
***********************************************************************
  pmsglcd: print character string str[] on LCD
***********************************************************************
*/

void pmsglcd(char str[])
{
    int i = 0;
    
    while(str[i] !='\0')
    {
      print_c(str[i++]);
    }
}

/*
***********************************************************************
 Character I/O Library Routines for 9S12C32 (for debugging only)
***********************************************************************
 Name:         inchar
 Description:  inputs ASCII character from SCI serial port and returns it
 Example:      char ch1 = inchar();
***********************************************************************
*/

char inchar(void) {
  /* receives character from the terminal channel */
        while (!(SCISR1 & 0x20)); /* wait for input */
    return SCIDRL;
}

/*
***********************************************************************
 Name:         outchar
 Description:  outputs ASCII character x to SCI serial port
 Example:      outchar('x');
***********************************************************************
*/

void outchar(char x) {
  /* sends a character to the terminal channel */
    while (!(SCISR1 & 0x80));  /* wait for output buffer empty */
    SCIDRL = x;
}
