&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:   multiMonthDatePicker.w

  Description: 

  Input Parameters:
      handle to fill-in widget od data-type of "Date"


  Author: James Bowen - BowTech Dynmanics Limited 

  Created: 25/06/2021

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

USING System.Windows.Forms.*.     
     
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

 &GLOBAL-DEFINE xcMonths 'January,Febuary,March,April,May,June,July,August,September,October,November,December'
 &GLOBAL-DEFINE xcDays 'Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday'

DEFINE INPUT PARAMETER dateWidgetHandle AS HANDLE NO-UNDO.

DEFINE VARIABLE daInitialDate   AS DATE    NO-UNDO.
DEFINE VARIABLE daCurrentMonth  AS DATE    NO-UNDO.
DEFINE VARIABLE daSelectedDate  AS DATE    NO-UNDO.
DEFINE VARIABLE renderOnce      AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE parentWindow    AS HANDLE      NO-UNDO.        

DEFINE TEMP-TABLE ttCalenderDay NO-UNDO
    FIELD WIDGET-HANDLE   AS HANDLE XML-NODE-TYPE "HIDDEN".

PROCEDURE LockWindowUpdate EXTERNAL "user32.dll" :
  DEFINE INPUT  PARAMETER hWndLock AS LONG.
  DEFINE RETURN PARAMETER IsLocked AS LONG.
END PROCEDURE.


PROCEDURE GetParent external "user32.dll":
    def input parameter hwnd as long.
    def return parameter WinLong as long.
end procedure.

PROCEDURE SetWindowLongA external "user32.dll":
    def input parameter hwnd as long.
    def input parameter nIndex as long.
    def input parameter dwNewLong as long.
END procedure.

PROCEDURE SetWindowPos external "user32.dll":
    def input parameter hwnd as long.
    def input parameter InsAfter as long.
    def input parameter x as short.
    def input parameter y as short.
    def input parameter cx as short.
    def input parameter xy as short.
    def input parameter uFlags as unsigned-short.
END procedure.


PROCEDURE GetWindowLongA EXTERNAL "user32.dll":
  DEFINE INPUT  PARAMETER phwnd       AS long.
  DEFINE INPUT  PARAMETER cindex      AS long.
  DEFINE RETURN PARAMETER currentlong AS LONG.
END PROCEDURE.

/** 64bit, BitWise Operators to supperceed the Proextra **/

FUNCTION bitAND RETURNS INT64 (INPUT X AS INTEGER, INPUT Y AS INTEGER):
   DEFINE VARIABLE b1 AS INT64 NO-UNDO.
   DEFINE VARIABLE b2 AS INT64 NO-UNDO.
   DEFINE VARIABLE n  AS INT64 NO-UNDO.
   DEFINE VARIABLE Z  AS INT64 NO-UNDO.

   DO n = 1 TO 64:
     ASSIGN
       b1 = GET-BITS(X, n, 1)
       b2 = GET-BITS(Y, n, 1)
       .
       IF b1 = 1 AND b2 = 1 THEN PUT-BITS(Z, n, 1) = 1.
   END.

   RETURN Z.
END FUNCTION.

FUNCTION bitNOT RETURNS INT64 (INPUT X AS INTEGER):
   DEFINE VARIABLE b AS INTEGER NO-UNDO.
   DEFINE VARIABLE n AS INTEGER NO-UNDO.
   DEFINE VARIABLE Z AS INT64 NO-UNDO.

   DO n = 1 TO 64:
       b = GET-BITS(X, n, 1).
       IF b EQ 0 THEN
         PUT-BITS(Z, n, 1) = 1.
   END.

   RETURN Z.
END FUNCTION.

FUNCTION Centre RETURNS LOGICAL ( INPUT h AS HANDLE ) :
    DEFINE VARIABLE reps AS INTEGER     NO-UNDO.
    
    reps = (h:WIDTH-PIXELS - FONT-TABLE:GET-TEXT-WIDTH-PIXELS(TRIM(h:SCREEN-VALUE),h:FONT) - 8 /* allow for 3-D borders */ ) / FONT-TABLE:GET-TEXT-WIDTH-PIXELS(' ',h:FONT).
    reps = reps / 2.
    
    h:SCREEN-VALUE = FILL(' ',reps) + TRIM(h:SCREEN-VALUE).
    
    RETURN yes.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bntAdvanceMonth bntAdvanceYear ~
bntDecrimentMonth bntDecrimentYear calender2 calender3 calender1 cbMonth ~
cbYear bntClose bntTpday 
&Scoped-Define DISPLAYED-OBJECTS cbMonth cbYear finInitialDate fiPreMonth ~
fiPostMonth 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD custWeekDay C-Win 
FUNCTION custWeekDay RETURNS INTEGER PRIVATE
  ( INPUT pDate AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bntAdvanceMonth  NO-FOCUS
     LABEL ">" 
     SIZE 4.5 BY .88.

DEFINE BUTTON bntAdvanceYear  NO-FOCUS
     LABEL ">>" 
     SIZE 4.5 BY .88.

DEFINE BUTTON bntClose AUTO-END-KEY 
     LABEL "Close" 
     SIZE 13.63 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON bntDecrimentMonth  NO-FOCUS
     LABEL "<" 
     SIZE 4.5 BY .88.

DEFINE BUTTON bntDecrimentYear  NO-FOCUS
     LABEL "<<" 
     SIZE 4.5 BY .88.

DEFINE BUTTON bntTpday  NO-FOCUS
     LABEL "Today" 
     SIZE 8 BY .88.

DEFINE VARIABLE cbMonth AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEM-PAIRS "Months",""
     DROP-DOWN-LIST
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE cbYear AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 21
     DROP-DOWN-LIST
     SIZE 9.5 BY 1 NO-UNDO.

DEFINE VARIABLE finInitialDate AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42.5 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiPostMonth AS CHARACTER FORMAT "X(256)":U INITIAL "Next Month" 
      VIEW-AS TEXT 
     SIZE 14.5 BY .63
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiPreMonth AS CHARACTER FORMAT "X(256)":U INITIAL "Prev Month" 
      VIEW-AS TEXT 
     SIZE 16 BY .63
     FONT 6 NO-UNDO.

DEFINE RECTANGLE calender1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 8.

DEFINE RECTANGLE calender2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 8.

DEFINE RECTANGLE calender3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 8.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     bntAdvanceMonth AT ROW 1.5 COL 98.5 WIDGET-ID 20
     bntAdvanceYear AT ROW 1.5 COL 103.5 WIDGET-ID 38
     bntDecrimentMonth AT ROW 1.5 COL 7 WIDGET-ID 22
     bntDecrimentYear AT ROW 1.5 COL 2 WIDGET-ID 36
     cbMonth AT ROW 1.5 COL 43.75 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     cbYear AT ROW 1.5 COL 61.38 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     bntClose AT ROW 10.69 COL 94.13 WIDGET-ID 30
     finInitialDate AT ROW 10.75 COL 2 NO-LABEL WIDGET-ID 40 NO-TAB-STOP 
     bntTpday AT ROW 1.5 COL 37.25 HELP
          "Today" WIDGET-ID 26
     fiPreMonth AT ROW 1.75 COL 12.5 NO-LABEL WIDGET-ID 24 NO-TAB-STOP 
     fiPostMonth AT ROW 1.75 COL 83 NO-LABEL WIDGET-ID 28 NO-TAB-STOP 
     calender2 AT ROW 2.5 COL 37.5 WIDGET-ID 4
     calender3 AT ROW 2.5 COL 73 WIDGET-ID 12
     calender1 AT ROW 2.5 COL 2 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 108.13 BY 11.06 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 11.06
         WIDTH              = 108.13
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 164.13
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 164.13
         SHOW-IN-TASKBAR    = no
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         TOP-ONLY           = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
ASSIGN 
       calender1:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       calender2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       calender3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN finInitialDate IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       finInitialDate:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiPostMonth IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fiPostMonth:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiPreMonth IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fiPreMonth:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
     
  parentWindow:SENSITIVE = TRUE.
  
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON PARENT-WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
    /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bntAdvanceMonth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bntAdvanceMonth C-Win
ON CHOOSE OF bntAdvanceMonth IN FRAME DEFAULT-FRAME /* > */
DO:
  
  DO WITH FRAME {&FRAME-NAME}:
  
    daCurrentMonth = ADD-INTERVAL(daCurrentMonth, 1 , 'months').
  
    cbMonth:SCREEN-VALUE = STRING( MONTH(daCurrentMonth) ).  
    
    RUN populateYearCombo(INPUT daCurrentMonth).
    
    RUN renderCalender IN THIS-PROCEDURE.
    
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bntAdvanceYear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bntAdvanceYear C-Win
ON CHOOSE OF bntAdvanceYear IN FRAME DEFAULT-FRAME /* >> */
DO:
  
  DO WITH FRAME {&FRAME-NAME}:
  
    daCurrentMonth = ADD-INTERVAL(daCurrentMonth, 1 , 'Years').
  
    cbMonth:SCREEN-VALUE = STRING( MONTH(daCurrentMonth) ).  
    
    RUN populateYearCombo(INPUT daCurrentMonth).
    
    RUN renderCalender IN THIS-PROCEDURE.
    
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bntClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bntClose C-Win
ON CHOOSE OF bntClose IN FRAME DEFAULT-FRAME /* Close */
DO:
  parentWindow:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bntDecrimentMonth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bntDecrimentMonth C-Win
ON CHOOSE OF bntDecrimentMonth IN FRAME DEFAULT-FRAME /* < */
DO:
  DO WITH FRAME {&FRAME-NAME}:
  
    daCurrentMonth = ADD-INTERVAL(daCurrentMonth, -1 , 'months').
  
    cbMonth:SCREEN-VALUE = STRING( MONTH(daCurrentMonth) ).  
    
    RUN populateYearCombo(INPUT daCurrentMonth).
    
    RUN renderCalender IN THIS-PROCEDURE.
    
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bntDecrimentYear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bntDecrimentYear C-Win
ON CHOOSE OF bntDecrimentYear IN FRAME DEFAULT-FRAME /* << */
DO:
  DO WITH FRAME {&FRAME-NAME}:
  
    daCurrentMonth = ADD-INTERVAL(daCurrentMonth, -1 , 'Years').
  
    cbMonth:SCREEN-VALUE = STRING( MONTH(daCurrentMonth) ).  
    
    RUN populateYearCombo(INPUT daCurrentMonth).
    
    RUN renderCalender IN THIS-PROCEDURE.
    
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bntTpday
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bntTpday C-Win
ON CHOOSE OF bntTpday IN FRAME DEFAULT-FRAME /* Today */
DO:
  RUN dateSelected (INPUT TODAY).
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bntTpday C-Win
ON LEFT-MOUSE-DBLCLICK OF bntTpday IN FRAME DEFAULT-FRAME /* Today */
DO:
  MESSAGE "Double Click" VIEW-AS ALERT-BOX INFO.
  
  daSelectedDate = TODAY.
  
  RUN renderCalender.
  
  RUN populateMonthCombo(INPUT daSelectedDate).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbMonth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbMonth C-Win
ON VALUE-CHANGED OF cbMonth IN FRAME DEFAULT-FRAME
DO:
  
    DO WITH FRAME {&FRAME-NAME}:
    
        daCurrentMonth = DATE( INTEGER(cbMonth:SCREEN-VALUE), 1, INTEGER(cbYear:SCREEN-VALUE) ).
        
        RUN populateYearCombo( INPUT daCurrentMonth).
        RUN renderCalender IN THIS-PROCEDURE.        
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbYear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbYear C-Win
ON VALUE-CHANGED OF cbYear IN FRAME DEFAULT-FRAME
DO:
    
    DO WITH FRAME {&FRAME-NAME}:
    
        daCurrentMonth = DATE( INTEGER(cbMonth:SCREEN-VALUE), 1, INTEGER(cbYear:SCREEN-VALUE) ).
        
        RUN populateYearCombo( INPUT daCurrentMonth).
        RUN renderCalender IN THIS-PROCEDURE.        
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.
      
    RUN removeBorder.
  
    RUN initialise IN THIS-PROCEDURE. 
    
    RUN renderCalender  IN THIS-PROCEDURE.                  
  
 
               
  
       
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dateSelected C-Win 
PROCEDURE dateSelected PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER dateSelected AS DATE NO-UNDO.
    
    daSelectedDate = dateSelected.
    
    dateWidgetHandle:SCREEN-VALUE = STRING(daSelectedDate,'99/99/9999').
  
    parentWindow:SENSITIVE = TRUE.
  
    APPLY "close" TO THIS-PROCEDURE.
    
    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFullDate C-Win 
PROCEDURE displayFullDate PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
    
         finInitialDate:SCREEN-VALUE = SUBSTITUTE("&1, &2 &3 &4",
            ENTRY( custWeekDay(daInitialDate),{&xcDays}),
            DAY(daInitialDate),
            ENTRY( MONTH(daInitialDate),{&xcMonths}),
            YEAR(daInitialDate)).

    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY cbMonth cbYear finInitialDate fiPreMonth fiPostMonth 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE bntAdvanceMonth bntAdvanceYear bntDecrimentMonth bntDecrimentYear 
         calender2 calender3 calender1 cbMonth cbYear bntClose bntTpday 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE highlightDay C-Win 
PROCEDURE highlightDay PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER dynButton      AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER calenderPannel AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE dynRect AS HANDLE      NO-UNDO.
    
    // Create a rectectangle to highlight the currentl day
    CREATE RECT dynRect ASSIGN
        X         = dynButton:X 
        Y         = dynButton:Y 
        WIDTH-PIXEL = dynButton:WIDTH-PIXEL 
        HEIGHT-PIXEL = dynButton:HEIGHT-PIXEL
        BGCOLOR = 12
        FGCOLOR = 12
        ROUNDED = TRUE
        FILLED = TRUE
        FRAME         = FRAME DEFAULT-FRAME:HANDLE
        VISIBLE       = TRUE.
        
    // Resize existing button to be smaller
    dynButton:WIDTH-PIXEL  = dynButton:WIDTH-PIXEL  - 4.
    dynButton:HEIGHT-PIXEL = dynButton:HEIGHT-PIXEL - 4.
    dynButton:X            = dynButton:X + 2.
    dynButton:Y            = dynButton:Y + 2.        

    CREATE ttCalenderDay.

    ASSIGN 
        ttCalenderDay.WIDGET-HANDLE = dynRect:HANDLE.  
                    
    RETURN.                     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialise C-Win 
PROCEDURE initialise PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE parentHandle AS HANDLE      NO-UNDO.        
    
    IF dateWidgetHandle:DATA-TYPE NE "DATE" THEN
        APPLY "END-ERROR":U TO SELF.
        
    
    daInitialDate  = DATE( dateWidgetHandle:SCREEN-VALUE ).
    
    //MESSAGE dateWidgetHandle:PRIVATE-DATA. 

    //parentHandle = dateWidgetHandle:INSTANTIATING-PROCEDURE.
    
    //MESSAGE parentHandle:INTERNAL-ENTRIES .
    
    ASSIGN
        daInitialDate  = TODAY WHEN daInitialDate EQ ?       
        parentWindow   = dateWidgetHandle:WINDOW
        daCurrentMonth = DATE(MONTH(daInitialDate), 1, YEAR(daInitialDate)).    
    
    /** Disable Parenet window **/
    parentWindow:SENSITIVE = FALSE.
        
    RUN populateMonthCombo(INPUT daInitialDate).    
    RUN populateYearCombo(INPUT daInitialDate).    
    
    RUN displayFullDate IN THIS-PROCEDURE.
    
    DEFINE VARIABLE dynRect AS HANDLE      NO-UNDO.    
     
    // Create a rectectangle arround the whole window.
    CREATE RECT dynRect ASSIGN
        X         = 0
        Y         = 0
        WIDTH-PIXEL  = FRAME {&Frame-NAME}:WIDTH-PIXEL
        HEIGHT-PIXEL = FRAME {&Frame-NAME}:HEIGHT-PIXEL
        BGCOLOR = ?
        FGCOLOR = ?
        ROUNDED = FALSE
        FILLED  = FALSE
        FRAME         = FRAME DEFAULT-FRAME:HANDLE
        VISIBLE       = TRUE.
    
    
    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE populateMonthCombo C-Win 
PROCEDURE populateMonthCombo PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE monthEntry     AS INTEGER     NO-UNDO.
    DEFINE VARIABLE yearEntry      AS INTEGER     NO-UNDO.
    DEFINE VARIABLE calenderMonth  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE calenderYear   AS CHARACTER   NO-UNDO.

    DEFINE INPUT PARAMETER daInitialDate AS DATE NO-UNDO.
    
    IF daInitialDate EQ ? THEN
        RETURN.
    
    
    DO WITH FRAME {&frame-name}:
    
        DO monthEntry = 1 TO 12:
            calenderMonth = calenderMonth + SUBSTITUTE("&1,&2", ENTRY(monthEntry,{&xcMonths}), monthEntry).
            
            IF monthEntry NE 12 THEN
                calenderMonth = calenderMonth + ','.
        
        END.
        
        cbMonth:LIST-ITEM-PAIRS = calenderMonth.
        
        cbMonth:SCREEN-VALUE = STRING(MONTH(daInitialDate)) .   
    
    END.       
    
    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE populateYearCombo C-Win 
PROCEDURE populateYearCombo PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE monthEntry     AS INTEGER     NO-UNDO.
    DEFINE VARIABLE yearEntry      AS INTEGER     NO-UNDO.
    DEFINE VARIABLE calenderMonth  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE calenderYear   AS CHARACTER   NO-UNDO.
    
    DEFINE INPUT PARAMETER daSelectedDate AS DATE NO-UNDO.
    
    DO WITH FRAME {&frame-name}:
    
        DO yearEntry = -8 TO 5:
        
              calenderYear = calenderYear + STRING(yearEntry + YEAR(daSelectedDate)) + ',' .
        
        END.
        
        calenderYear = RIGHT-TRIM(calenderYear, ',').
        
        cbYear:LIST-ITEMS = calenderYear.
        
        cbYear:SCREEN-VALUE = STRING(YEAR(daSelectedDate) ) .
    END.       
    
    RETURN.    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE removeBorder C-Win 
PROCEDURE removeBorder PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


    &GLOBAL-DEFINE GWL_STYLE -16
    &GLOBAL-DEFINE WS_MAXIMIZEBOX 0x10000
    &GLOBAL-DEFINE WS_MINIMIZEBOX 0x20000
    &GLOBAL-DEFINE WS_THICKFRAME  0x40000
    &GLOBAL-DEFINE WS_CAPTION 0xC00000
    &GLOBAL-DEFINE WS_BORDER 0x800000
    &GLOBAL-DEFINE WS_BORDER 0x800000
    &GLOBAL-DEFINE SWP_NOMOVE 0x2
    &GLOBAL-DEFINE SWP_NOSIZE 0x1
    &GLOBAL-DEFINE SWP_NOZORDER  0x4
    &GLOBAL-DEFINE SWP_FRAMECHANGED   0x20

    DEFINE VARIABLE inNewWidth    AS INTEGER     NO-UNDO.
    DEFINE VARIABLE inNewHeight   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iWindowStyle  AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iNewPositionX AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iNewPositionY AS INTEGER     NO-UNDO.
    DEFINE VARIABLE hParentWindow AS HANDLE      NO-UNDO.
    DEFINE VARIABLE iWinHWND      AS INTEGER     NO-UNDO.

    RUN GetParent(INPUT {&WINDOW-NAME}:HWND, OUTPUT iWinHWND).

    /** Get the current Window's style.. **/
    RUN GetWindowLongA( iWinHWND, {&GWL_STYLE}, OUTPUT iWindowStyle).

    iWindowStyle = bitAND(INPUT iWindowStyle, INPUT bitNOT({&WS_CAPTION})).      /** Remove the Caption (Title Bar) **/
    //iWindowStyle = bitAND(INPUT iWindowStyle, INPUT bitNOT({&WS_BORDER})).       /** Remove the boarder **/
    iWindowStyle = bitAND(INPUT iWindowStyle, INPUT bitNOT({&WS_THICKFRAME})).   /** Remove the Thick Frame **/

    /** Set the window Style**/
    RUN SetWindowLongA(iWinHWND, {&GWL_STYLE}, iWindowStyle).

    ASSIGN
        inNewWidth = {&WINDOW-NAME}:WIDTH-PIXELS   
        inNewHeight = {&WINDOW-NAME}:HEIGHT-PIXELS.
        
        {&WINDOW-NAME}:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS.
        {&WINDOW-NAME}:WIDTH-PIXELS  = FRAME {&FRAME-NAME}:WIDTH-PIXELS.
        
    hParentWindow = dateWidgetHandle:WINDOW.
    
    ASSIGN              
        iNewPositionX = hParentWindow:X + dateWidgetHandle:X + (SystemInformation:FrameBorderSize:WIDTH * 1)
        iNewPositionY = hParentWindow:Y + dateWidgetHandle:Y + dateWidgetHandle:HEIGHT-PIXELS + SystemInformation:FrameBorderSize:HEIGHT + SystemInformation:CaptionHeight .
     
     RUN SetWindowPos(iWinHWND, 0, iNewPositionX, iNewPositionY, inNewWidth, inNewHeight,  {&SWP_NOZORDER} + {&SWP_FRAMECHANGED } ). /* SWP_NOZORDER | SWP_FRAMECHANGED */
                         
     RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE renderCalender C-Win 
PROCEDURE renderCalender PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE iDay        AS INTEGER     NO-UNDO.
DEFINE VARIABLE dynButton   AS HANDLE      NO-UNDO.
DEFINE VARIABLE txtLabel     AS HANDLE      NO-UNDO.
DEFINE VARIABLE iCOl AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRow AS INTEGER     NO-UNDO.

DEFINE VARIABLE daPrevMonth AS DATE     NO-UNDO.
DEFINE VARIABLE daNextMonth AS DATE     NO-UNDO.

DEFINE VARIABLE daStartDate AS DATE        NO-UNDO.
DEFINE VARIABLE IsLocked AS INTEGER     NO-UNDO.

&SCOPED-DEFINE xiButtonWidth 39
&SCOPED-DEFINE xiButtonHeight 34
&SCOPED-DEFINE xiDayCaptionHeight 24
&SCOPED-DEFINE xcDaysOfWeek "Mon,Tue,Wed,Thr,Fri,Sat,Sun"
&SCOPED-DEFINE xcFontColourRed 12
&SCOPED-DEFINE xcPadding 4




    DO WITH FRAME {&frame-name}:
    
        ASSIGN        
            daPrevMonth = ADD-INTERVAL(daCurrentMonth, -1, 'months')    
            daNextMonth = ADD-INTERVAL(daCurrentMonth,  1, 'months').
            
        fiPreMonth:SCREEN-VALUE  = ENTRY( MONTH(daPrevMonth), {&xcMonths} ).    
        fiPostMonth:SCREEN-VALUE = ENTRY( MONTH(daNextMonth), {&xcMonths} ).
        
        Centre(fiPreMonth:HANDLE).
        Centre(fiPostMonth:HANDLE).
        
    END.
    
             RUN LockWindowUpdate(FRAME {&FRAME-NAME}:HWND, 
                         OUTPUT IsLocked).
                         
    FRAME {&frame-name}:VISIBLE = NO. 
    
    IF NOT renderOnce THEN
    DO:
        DO iCol = 0 TO 6:    
        
            CREATE TEXT txtLabel ASSIGN
                  X            = calender1:X + (iCol * {&xiButtonWidth}) + {&xcPadding}
                  Y            = calender1:Y + 3
                  WIDTH-PIXEL  = {&xiButtonWidth} + 1
                  HEIGHT-PIXEL = {&xiDayCaptionHeight}
                  SCREEN-VALUE =  ENTRY(iCol + 1, {&xcDaysOfWeek} )
                  FRAME        = FRAME DEFAULT-FRAME:HANDLE
                  FONT         = 6
                  FGCOLOR      = IF (iCol EQ 5 OR iCol EQ 6) THEN {&xcFontColourRed} ELSE ? 
                  VISIBLE      = TRUE
                  .
                  
             Centre(txtLabel:HANDLE).     
                  
            CREATE TEXT txtLabel ASSIGN
                  X            = calender2:X + (iCol * {&xiButtonWidth}) + {&xcPadding}
                  Y            = calender2:Y + 3
                  WIDTH-PIXEL  = {&xiButtonWidth} + 1
                  HEIGHT-PIXEL = {&xiDayCaptionHeight}
                  SCREEN-VALUE =  ENTRY(iCol + 1, {&xcDaysOfWeek} )
                  FRAME        = FRAME DEFAULT-FRAME:HANDLE
                  FONT         = 6
                  FGCOLOR      = IF (iCol EQ 5 OR iCol EQ 6) THEN {&xcFontColourRed} ELSE ? 
                  VISIBLE      = TRUE
                  .              
            Centre(txtLabel:HANDLE).                   
                  
            CREATE TEXT txtLabel ASSIGN
                  X            = calender3:X + (iCol * {&xiButtonWidth}) + {&xcPadding}
                  Y            = calender3:Y + 3
                  WIDTH-PIXEL  = {&xiButtonWidth} + 1
                  HEIGHT-PIXEL = {&xiDayCaptionHeight}
                  SCREEN-VALUE =  ENTRY(iCol + 1, {&xcDaysOfWeek} )
                  FRAME        = FRAME DEFAULT-FRAME:HANDLE
                  FONT         = 6
                  FGCOLOR      = IF (iCol EQ 5 OR iCol EQ 6) THEN {&xcFontColourRed} ELSE ?  
                  VISIBLE      = TRUE
                  .              
                  
            Centre(txtLabel:HANDLE).                   
        END.
        
        renderOnce = TRUE.
    END.


    
    FOR EACH ttCalenderDay:
        
        IF VALID-HANDLE(ttCalenderDay.WIDGET-HANDLE) THEN
            DELETE WIDGET ttCalenderDay.WIDGET-HANDLE. 
            
        DELETE ttCalenderDay.            
            
    END.
    

    
    ASSIGN
        daStartDate = daPrevMonth.
        
    DO WHILE custWeekDay(daStartDate) NE 1:
        daStartDate = daStartDate - 1.
    END.
    

    DO iRow = 0 TO 5:

        DO iCol = 0 TO 6:
        
            CREATE BUTTON dynButton ASSIGN
              X             = calender1:X + (iCol * {&xiButtonWidth}) + {&xcPadding}
              Y             = calender1:Y + 28 + (iRow * {&xiButtonHeight}) + {&xcPadding}
              WIDTH-PIXEL   = {&xiButtonWidth}
              HEIGHT-PIXEL  = {&xiButtonHeight} 
              LABEL         = STRING(DAY(daStartDate))
              NO-FOCUS      = TRUE
              FLAT-BUTTON   = IF (daStartDate EQ TODAY) THEN FALSE ELSE TRUE
              FRAME         = FRAME DEFAULT-FRAME:HANDLE
              SENSITIVE     = ( MONTH(daStartDate) = MONTH(daPrevMonth) ) 
              VISIBLE       = TRUE
              //FONT          = IF (custWeekDay(daStartDate) >= 6 ) THEN 6 ELSE ?
              TOOLTIP       = STRING(daStartDate,'99/99/9999')
              TRIGGERS  :
                ON CHOOSE PERSISTENT RUN dateSelected(INPUT daStartDate ).
              END TRIGGERS.
            
            CREATE ttCalenderDay.
                
            ASSIGN 
                ttCalenderDay.WIDGET-HANDLE = dynButton:HANDLE.
                
            IF daInitialDate EQ daStartDate THEN
                RUN highlightDay(INPUT dynButton:HANDLE, 
                                 INPUT calender3:HANDLE).                  
            
            daStartDate = daStartDate + 1.                
        END.                
    END.          
    
    
    ASSIGN
        daStartDate = daCurrentMonth.
    
    DO WHILE custWeekDay(daStartDate) NE 1:
        daStartDate = daStartDate - 1.
    END.
                
    DO iRow = 0 TO 5:
        DO iCol = 0 TO 6:                
                  
            CREATE BUTTON dynButton ASSIGN
                  X            = calender2:X + (iCol * {&xiButtonWidth}) + {&xcPadding}
                  Y            = calender2:Y + 28 + (iRow * {&xiButtonHeight}) + {&xcPadding}
                  WIDTH-PIXEL  = {&xiButtonWidth}
                  HEIGHT-PIXEL = {&xiButtonHeight} 
                  LABEL        = STRING( DAY(daStartDate) )
                  NO-FOCUS     = TRUE
                  FLAT-BUTTON  = IF (daStartDate EQ TODAY) THEN FALSE ELSE TRUE
                  FRAME        = FRAME DEFAULT-FRAME:HANDLE
                  SENSITIVE    = ( MONTH(daStartDate) = MONTH(daCurrentMonth) ) 
                  VISIBLE      = TRUE
                  //FONT         = IF (custWeekDay(daStartDate) >= 6 ) THEN 6 ELSE ?
                  TOOLTIP      = STRING(daStartDate,'99/99/9999')
                  TRIGGERS:
                    ON CHOOSE PERSISTENT RUN dateSelected(INPUT daStartDate ).
                  END TRIGGERS.
                  
            CREATE ttCalenderDay.
            
            ASSIGN 
                ttCalenderDay.WIDGET-HANDLE = dynButton:HANDLE.         
                
            IF daInitialDate EQ daStartDate THEN
                RUN highlightDay(INPUT dynButton:HANDLE, 
                                 INPUT calender2:HANDLE).
                
            daStartDate = daStartDate + 1.
        END.                
    END.         
    
    
    ASSIGN
        daStartDate = daNextMonth.
    
   DO WHILE custWeekDay(daStartDate) NE 1:
        daStartDate = daStartDate - 1.
    END.   

    DO iRow = 0 TO 5:
        DO iCol = 0 TO 6:                
                
            CREATE BUTTON dynButton ASSIGN
                  X            = calender3:X + (iCol * {&xiButtonWidth}) + {&xcPadding}
                  Y            = calender3:Y + 28 + (iRow * {&xiButtonHeight}) + {&xcPadding}
                  WIDTH-PIXEL  = {&xiButtonWidth}
                  HEIGHT-PIXEL = {&xiButtonHeight} 
                  LABEL        = STRING(DAY(daStartDate))
                  NO-FOCUS     = TRUE
                  FLAT-BUTTON  = IF (daStartDate EQ TODAY) THEN FALSE ELSE TRUE
                  FRAME        = FRAME DEFAULT-FRAME:HANDLE
                  SENSITIVE    = ( MONTH(daStartDate) = MONTH(daNextMonth) ) 
                  VISIBLE      = TRUE
                  //FONT         = IF (custWeekDay(daStartDate) >= 6 ) THEN 6 ELSE ?
                  TOOLTIP      = STRING(daStartDate,'99/99/9999')
                  TRIGGERS  :
                    ON CHOOSE PERSISTENT RUN dateSelected(INPUT daStartDate).
                  END TRIGGERS.   
                  
            IF daInitialDate EQ daStartDate THEN
                RUN highlightDay(INPUT dynButton:HANDLE, 
                                 INPUT calender3:HANDLE).                      
                  
            CREATE ttCalenderDay.
            
            ASSIGN 
                ttCalenderDay.WIDGET-HANDLE = dynButton:HANDLE.  
                
            daStartDate = daStartDate + 1.

        END.
    END.
    
   IF IsLocked NE 0 THEN 
     RUN LockWindowUpdate( 0, OUTPUT IsLocked).
    
    FRAME {&FRAME-NAME}:VISIBLE = YES.
    
    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION custWeekDay C-Win 
FUNCTION custWeekDay RETURNS INTEGER PRIVATE
  ( INPUT pDate AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE inWeekDay AS INTEGER     NO-UNDO.

    inWeekDay = WEEKDAY(pDate) - 1.

    IF inWeekDay = 0 THEN
        inWeekDay = 7.

    RETURN inWeekDay.   /* Function return value. */
    
    
    
    // 1 = 7
    // 2 = 1
    // 3 = 2
    // 4 = 3
    // 5 = 4
    // 6 = 5
    // 7 = 6

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

