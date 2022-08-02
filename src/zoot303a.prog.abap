*&---------------------------------------------------------------------*
*& Report ZOOT303A
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zoot303a.

*----------------------------------------------------------------------
*  Title:
*    Objects 303: Design Patterns: Observer pattern
*  Selection text values:
*    PBRAND   - Brand
*    PCARGOW  - Cargo weight (truck only)
*    PCOLOR   - Color
*    PEVW     - Empty vehicle weight
*    PHEADING - Starting compass heading
*    PLOCATN  - Starting location
*    PMODEL   - Model
*    PPSNGRS  - Passengers (car only)
*    PPLATE   - License plate
*    PSPEEDU  - Speed unit
*    PSPEED01 - Sequence of speed increments
*    PSPEED02 - (none)
*    PSPEED03 - (none)
*    PTURN01  - Sequence of turns
*    PTURN02  - (none)
*    PTURN03  - (none)
*    PWGHTU   - Weight unit
*    PYEAR    - Year
*    XBNAV    - Equipped with basic navigation
*    XGPS     - Equipped with GPS navigation
*    XNONAV   - Equipped with no navigation
*  Text symbol values:
*    (none)
*
*  J. McDonough - October 2012
*
* This program will show the current attributes for a vehicle using
* values supplied by the user.  An invalid value specified for
* starting compass heading will default to North.  An invalid value
* specified for a turn will be ignored.
*
* Other program components:
*   GUI Status SELECTION_SCREEN - A copy of GUI status %_00
*     of program RSSYSTDB with the following changes:
*
*                                                            Application
*   Function   Text              Icon             Menu Bar     Toolbar Function keys
*   ---------- ----------------- ---------------- ------------ ------- --------------------
*   NEWCAR     Add new car       ICON_CREATE      (none)       item 12 F5 (unused)
*   NEWTRUCK   Add new truck     ICON_CREATE      (none)       item 13 F6 (unused)
*   ---------- ----------------- ---------------- ------------ ------- --------------------
*
* Differences with preceding version:
*   This version illustrates the "observer" design pattern, which
*   is implemented in ABAP using statements in the language itself.
*
*   Class "truck" has been changed to include instance event
*   "weight_exceeds_2_axle_limit" to be raised when the determination
*   is made that the gross weight of the truck exceeds the maximum
*   2-axle weight limit.  This is the "observed" class.  In addition,
*   new private method "check_axle_weight" has been added, which is
*   invoked by the instance constructor method, to check its gross
*   weight, and to issue a broadcast to all registered observers when
*   the gross weight exceeds the maximum 2-axle weight limit.  Also,
*   class "truck" now has a static constructor which invokes method
*   "get_2_axle_weight_limit" of class "truck_axle_weight_monitor"
*   as well as static attributes for retaining information about axle
*   weight limits returned by that call.
*
*   New class "truck_axle_weight_monitor" has been added, which
*   keeps track of the number of trucks having a gross weight
*   exceeding the maximum 2-axle weight limit.  This becomes the
*   "observer" class.  This is a singleton class with a static
*   constructor which creates a "truck_axle_weight_monitor"
*   instance into its corresponding attribute named singleton and
*   then through the SET HANDLER statement notifies the run-time
*   system to invoke its method "add_to_over_2_axle_limit_count"
*   anytime any "truck" instance raises the the event
*   "weight_exceeds_2_axle_limit".  It includes static methods
*   "get_2_axle_weight_limit", which will return to the caller the
*   maximum weight limit and its unit for 2-axle trucks, and
*   "show_over_2_axle_limit_count" which will issue a popup message
*   indicating the number of trucks exceeding the 2-axle weight limit,
*   using a message severity of warning when the number is greater
*   than zero, but otherwise issuing the message with severity
*   information.
*
*   A "truck" instance is oblivious to any observers it has. Accordingly,
*   the observed "truck" instance and "truck_axle_weight_monitor" instance
*   observer are "loosely coupled" -- there are no direct instance method
*   calls from the "truck" instance to the "truck_axle_weight_monitor"
*   instance.
*
*   Classic event "end-of-selection" was changed first to invoke method
*   "show_over_2_axle_limit_count" of class "truck_axle_weight_monitor"
*   prior to showing the report.
*
*   To see differences with preceding version -
*     1) Invoke transaction SE39.
*     2) From menu, selection Utilities > Settings.
*     3) On tab ABAP Editor, subtab Splitscreen, select checkmark to
*        indicate "Ignore Indentations" and press enter.
*     4) Specify the name of the object representing the preceding version
*        in the Left Program slot, this object in the Right Program slot,
*        and press Display.
*     5) Press the "Comparison On" button appearing on the button bar.
*     6) Alternate pressing the "Next Difference from Cursor" and "Next
*        Identical Section from Cursor" (easiest to do this by holding
*        CTRL+SHIFT and alternately pressing F9 and F11).  Both the left
*        and right sections are scrolled forward together as the next equal
*        or different line is located.
*
*----------------------------------------------------------------------
*======================================================================
*
*   I n t e r f a c e   D e f i n i t i o n s
*
*======================================================================
*----------------------------------------------------------------------
*
* Interface: simple_navigation
*
*----------------------------------------------------------------------
INTERFACE simple_navigation.
  TYPES        : turn_type      TYPE char1
               , heading_type   TYPE char1
               .
  CONSTANTS    : left_turn      TYPE simple_navigation=>turn_type
                                               VALUE 'L'
               , right_turn     TYPE simple_navigation=>turn_type
                                               VALUE 'R'
               , u_turn         TYPE simple_navigation=>turn_type
                                               VALUE 'U'
               , compass        TYPE char4     VALUE 'NESW'
               .
  METHODS      : change_heading
                   IMPORTING
                     turn
                       TYPE simple_navigation=>turn_type
               , get_heading
                   EXPORTING
                     heading
                       TYPE simple_navigation=>heading_type
               .
ENDINTERFACE. " simple_navigation
*======================================================================
*
*   C l a s s   D e f i n i t i o n s
*
*======================================================================
*----------------------------------------------------------------------
*
* Class: navigator definition
*
*----------------------------------------------------------------------
CLASS navigator DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES   : simple_navigation
      .
    ALIASES      : change_heading
                     FOR simple_navigation~change_heading
                 , get_heading
                     FOR simple_navigation~get_heading
                 .
    CONSTANTS    : class_id       TYPE seoclsname VALUE 'NAVIGATOR'
                 .
    CLASS-METHODS: class_constructor
      .
    METHODS      : constructor
      IMPORTING
        heading
          TYPE simple_navigation=>heading_type
      .
* protected section.
  PRIVATE SECTION.
    CONSTANTS    : compass_offset_limit_lo
                                  TYPE int4      VALUE 00
                 .
    CLASS-DATA   : compass_offset_limit_hi
                                  TYPE int4
                 .
    DATA         : heading        TYPE simple_navigation=>heading_type
                 .
ENDCLASS. " navigator definition
*----------------------------------------------------------------------
*
* Class: gps definition
*
*----------------------------------------------------------------------
CLASS gps DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES   : simple_navigation
      .
    ALIASES      : change_heading
                     FOR simple_navigation~change_heading
                 , get_heading
                     FOR simple_navigation~get_heading
                 .
    CONSTANTS    : class_id       TYPE seoclsname VALUE 'GPS'
                 .
    METHODS      : constructor
      IMPORTING
        heading
          TYPE simple_navigation=>heading_type
      .
* protected section.
  PRIVATE SECTION.
    CONSTANTS    : degrees_90     TYPE int4      VALUE 90
                 , degrees_180    TYPE int4      VALUE 180
                 , degrees_360    TYPE int4      VALUE 360
                 .
    DATA         : bearing        TYPE int4
                 .
ENDCLASS. " gps definition
*----------------------------------------------------------------------
*
* Class: dead_reckoning definition
*
*----------------------------------------------------------------------
CLASS dead_reckoning DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES   : simple_navigation
      .
    ALIASES      : change_heading
                     FOR simple_navigation~change_heading
                 , get_heading
                     FOR simple_navigation~get_heading
                 .
    CONSTANTS    : class_id       TYPE seoclsname VALUE 'DEAD_RECKONING'
                 .
    METHODS      : constructor
      IMPORTING
        heading                                             "#EC *
          TYPE simple_navigation=>heading_type
      .
* protected section.
  PRIVATE SECTION.
    CONSTANTS    : unknown_direction
                                  TYPE char01    VALUE '?'
                 .
    DATA         : heading        TYPE simple_navigation=>heading_type
                 .
ENDCLASS. " dead_reckoning definition
*----------------------------------------------------------------------
*
* Class: vehicle definition
*
*----------------------------------------------------------------------
CLASS vehicle DEFINITION ABSTRACT.
  PUBLIC SECTION.
    TYPES        : brand_type     TYPE f4txt
                 , color_type     TYPE f4txt
                 , location_type  TYPE f4txt
                 , model_type     TYPE f4txt
                 , license_plate_type
                                  TYPE f4txt
                 , navigator_type TYPE seoclsname
                 , speed_type     TYPE int4
                 , speed_unit_type
                                  TYPE char3
                 , year_type      TYPE num4
                 , serial_type    TYPE num4
                 , weight_type    TYPE int4
                 , weight_unit_type
                                  TYPE char3
                 , description_type
                                  TYPE char15
                 .
    CLASS-METHODS: class_constructor
      .
    METHODS      : accelerate
                     IMPORTING
                       acceleration
                         TYPE vehicle=>speed_type
                 , change_heading
                     IMPORTING
                       turn
                         TYPE simple_navigation=>turn_type
                 , get_characteristics
                     EXPORTING
                       serial_number
                         TYPE vehicle=>serial_type
                       license_plate
                         TYPE vehicle=>license_plate_type
                       brand
                         TYPE vehicle=>brand_type
                       model
                         TYPE vehicle=>model_type
                       year
                         TYPE vehicle=>year_type
                       color
                         TYPE vehicle=>color_type
                       location
                         TYPE vehicle=>location_type
                       speed_unit
                         TYPE vehicle=>speed_unit_type
                       weight_unit
                         TYPE vehicle=>weight_unit_type
                       navigation_type
                         TYPE vehicle=>navigator_type
                 , get_description ABSTRACT
                     EXPORTING
                       description
                         TYPE vehicle=>description_type
                 , get_gross_weight ABSTRACT
                     EXPORTING
                       gross_weight
                         TYPE vehicle=>weight_type
                 , get_heading
                     EXPORTING
                       heading
                         TYPE simple_navigation=>heading_type
                 , get_speed
                     EXPORTING
                       speed
                         TYPE vehicle=>speed_type
                 , constructor
                     IMPORTING
                       license_plate
                         TYPE vehicle=>license_plate_type
                       brand
                         TYPE vehicle=>brand_type
                       model
                         TYPE vehicle=>model_type
                       year
                         TYPE vehicle=>year_type
                       color
                         TYPE vehicle=>color_type
                       location
                         TYPE vehicle=>location_type
                       speed_unit
                         TYPE vehicle=>speed_unit_type
                       heading
                         TYPE simple_navigation=>heading_type
                       tare_weight
                         TYPE vehicle=>weight_type
                       weight_unit
                         TYPE vehicle=>weight_unit_type
                       basic_navigation
                         TYPE checkbox
                       gps_navigation
                         TYPE checkbox
                       no_navigation                        "#EC *
                         TYPE checkbox
                 .
  PROTECTED SECTION.
    DATA         : tare_weight    TYPE vehicle=>weight_type
                 .
  PRIVATE SECTION.
    CLASS-DATA   : last_serial_value
                                  TYPE vehicle=>serial_type
                 .
    DATA         : license_plate  TYPE vehicle=>license_plate_type
                 , brand          TYPE vehicle=>brand_type
                 , model          TYPE vehicle=>model_type
                 , year           TYPE vehicle=>year_type
                 , color          TYPE vehicle=>color_type
                 , location       TYPE vehicle=>location_type
                 , speed          TYPE vehicle=>speed_type
                 , speed_unit     TYPE vehicle=>speed_unit_type
                 , weight_unit    TYPE vehicle=>weight_unit_type
                 , serial_number  TYPE vehicle=>serial_type
                 , navigation_type
                                  TYPE vehicle=>navigator_type
                 , navigation_unit
                                  TYPE REF
                                    TO simple_navigation
                 .
    CLASS-METHODS: get_serial_number
      EXPORTING
        serial_number
          TYPE vehicle=>serial_type
      .
ENDCLASS. " vehicle definition
*----------------------------------------------------------------------
*
* Class: car definition
*
*----------------------------------------------------------------------
CLASS car DEFINITION FINAL INHERITING FROM vehicle.
  PUBLIC SECTION.
    TYPES        : passengers_type
                                  TYPE int4
                 .
    METHODS      : constructor
                     IMPORTING
                       license_plate
                         TYPE vehicle=>license_plate_type
                       brand
                         TYPE vehicle=>brand_type
                       model
                         TYPE vehicle=>model_type
                       year
                         TYPE vehicle=>year_type
                       color
                         TYPE vehicle=>color_type
                       location
                         TYPE vehicle=>location_type
                       speed_unit
                         TYPE vehicle=>speed_unit_type
                       heading
                         TYPE simple_navigation=>heading_type
                       tare_weight
                         TYPE vehicle=>weight_type
                       weight_unit
                         TYPE vehicle=>weight_unit_type
                       passengers
                         TYPE car=>passengers_type
                       basic_navigation
                         TYPE checkbox
                       gps_navigation
                         TYPE checkbox
                       no_navigation
                         TYPE checkbox
                 , get_description  REDEFINITION
                 , get_gross_weight REDEFINITION
                 .
* protected section.
  PRIVATE SECTION.
    CONSTANTS    : descriptor     TYPE string    VALUE 'Car' "#EC *
                 .
    DATA         : passengers     TYPE car=>passengers_type
                 .
ENDCLASS. " car definition
*----------------------------------------------------------------------
*
* Class: truck definition
*
*----------------------------------------------------------------------
CLASS truck DEFINITION FINAL INHERITING FROM vehicle.
  PUBLIC SECTION.
    EVENTS       : weight_exceeds_2_axle_limit
                 .
    CLASS-METHODS: class_constructor
      .
    METHODS      : constructor
                     IMPORTING
                       license_plate
                         TYPE vehicle=>license_plate_type
                       brand
                         TYPE vehicle=>brand_type
                       model
                         TYPE vehicle=>model_type
                       year
                         TYPE vehicle=>year_type
                       color
                         TYPE vehicle=>color_type
                       location
                         TYPE vehicle=>location_type
                       speed_unit
                         TYPE vehicle=>speed_unit_type
                       heading
                         TYPE simple_navigation=>heading_type
                       tare_weight
                         TYPE vehicle=>weight_type
                       weight_unit
                         TYPE vehicle=>weight_unit_type
                       cargo_weight
                         TYPE vehicle=>weight_type
                       basic_navigation
                         TYPE checkbox
                       gps_navigation
                         TYPE checkbox
                       no_navigation
                         TYPE checkbox
                 , get_description  REDEFINITION
                 , get_gross_weight REDEFINITION
                 .
* protected section.
  PRIVATE SECTION.
    CONSTANTS    : descriptor     TYPE string    VALUE 'Truck' "#EC *
                 .
    CLASS-DATA   : weight_limit_for_2_axles
                                  TYPE vehicle=>weight_type
                 , weight_limit_2_axles_unit
                                  TYPE vehicle=>weight_unit_type
                 .
    DATA         : cargo_weight   TYPE vehicle=>weight_type
                 .
    METHODS      : check_axle_weight
      .
ENDCLASS. " truck definition
*----------------------------------------------------------------------
*
* Class: truck_axle_weight_monitor definition
*
*----------------------------------------------------------------------
CLASS truck_axle_weight_monitor DEFINITION          FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    TYPES        : count_type     TYPE int4
                 .
    CLASS-DATA   : singleton      TYPE REF
                                    TO truck_axle_weight_monitor
                                    READ-ONLY
                 .
    CLASS-METHODS: class_constructor
                 , get_2_axle_weight_limit
                     EXPORTING
                       maximum_weight
                         TYPE vehicle=>weight_type
                       weight_unit
                         TYPE vehicle=>weight_unit_type
                 .
    METHODS      : show_over_2_axle_limit_count
      .
  PRIVATE SECTION.
    CONSTANTS    : weight_limit_in_lbs_2_axles
                                  TYPE vehicle=>weight_type
                                                 VALUE 40000
                 , weight_limit_2_axles_unit
                                  TYPE vehicle=>weight_unit_type
                                                 VALUE 'LB'
                 .
    DATA         : over_2_axle_limit_count
                                  TYPE truck_axle_weight_monitor=>count_type
                 .
    METHODS      : add_to_over_2_axle_limit_count
        FOR EVENT weight_exceeds_2_axle_limit
        OF truck
     .
ENDCLASS. " truck_axle_weight_monitor definition
*----------------------------------------------------------------------
*
* Class: report definition
*
*----------------------------------------------------------------------
CLASS report DEFINITION          FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CONSTANTS    : execute        TYPE syucomm   VALUE 'ONLI'
                 , add_new_car    TYPE syucomm   VALUE 'NEWCAR'
                 , add_new_truck  TYPE syucomm   VALUE 'NEWTRUCK'
                 , selection_screen_status_name
                                  TYPE sypfkey   VALUE 'SELECTION_SCREEN'
                 .
    CLASS-DATA   : singleton      TYPE REF
                                    TO report
                                    READ-ONLY
                 .
    CLASS-METHODS: class_constructor
      .
    METHODS      : register_car_entry
                     IMPORTING
                       license_plate
                         TYPE vehicle=>license_plate_type
                       brand
                         TYPE vehicle=>brand_type
                       year
                         TYPE vehicle=>year_type
                       model
                         TYPE vehicle=>model_type
                       color
                         TYPE vehicle=>color_type
                       location
                         TYPE vehicle=>location_type
                       heading
                         TYPE simple_navigation=>heading_type
                       turn01
                         TYPE simple_navigation=>turn_type
                       turn02
                         TYPE simple_navigation=>turn_type
                       turn03
                         TYPE simple_navigation=>turn_type
                       speed01
                         TYPE vehicle=>speed_type
                       speed02
                         TYPE vehicle=>speed_type
                       speed03
                         TYPE vehicle=>speed_type
                       speed_unit
                         TYPE vehicle=>speed_unit_type
                       tare_weight
                         TYPE vehicle=>weight_type
                       weight_unit
                         TYPE vehicle=>weight_unit_type
                       passengers
                         TYPE car=>passengers_type
                       basic_navigation
                         TYPE checkbox
                       gps_navigation
                         TYPE checkbox
                       no_navigation
                         TYPE checkbox
                 , register_truck_entry
                     IMPORTING
                       license_plate
                         TYPE vehicle=>license_plate_type
                       brand
                         TYPE vehicle=>brand_type
                       year
                         TYPE vehicle=>year_type
                       model
                         TYPE vehicle=>model_type
                       color
                         TYPE vehicle=>color_type
                       location
                         TYPE vehicle=>location_type
                       heading
                         TYPE simple_navigation=>heading_type
                       turn01
                         TYPE simple_navigation=>turn_type
                       turn02
                         TYPE simple_navigation=>turn_type
                       turn03
                         TYPE simple_navigation=>turn_type
                       speed01
                         TYPE vehicle=>speed_type
                       speed02
                         TYPE vehicle=>speed_type
                       speed03
                         TYPE vehicle=>speed_type
                       speed_unit
                         TYPE vehicle=>speed_unit_type
                       tare_weight
                         TYPE vehicle=>weight_type
                       weight_unit
                         TYPE vehicle=>weight_unit_type
                       cargo_weight
                         TYPE vehicle=>weight_type
                       basic_navigation
                         TYPE checkbox
                       gps_navigation
                         TYPE checkbox
                       no_navigation
                         TYPE checkbox
                 , show_report
                 .
* protected section.
  PRIVATE SECTION.
    TYPES        : BEGIN OF output_row                      "#EC *
                 ,   serial_number
                                  TYPE vehicle=>serial_type
                 ,   license_plate
                                  TYPE vehicle=>license_plate_type
                 ,   brand        TYPE vehicle=>brand_type
                 ,   model        TYPE vehicle=>model_type
                 ,   year         TYPE vehicle=>year_type
                 ,   color        TYPE vehicle=>color_type
                 ,   location     TYPE vehicle=>location_type
                 ,   heading      TYPE simple_navigation=>heading_type
                 ,   speed        TYPE vehicle=>speed_type
                 ,   speed_unit   TYPE vehicle=>speed_unit_type
                 ,   weight       TYPE vehicle=>weight_type
                 ,   weight_unit  TYPE vehicle=>weight_unit_type
                 ,   description  TYPE vehicle=>description_type
                 ,   navigation_type
                                  TYPE vehicle=>navigator_type
                 , END   OF output_row
                 , output_list    TYPE STANDARD TABLE
                                    OF report=>output_row
                 .
    DATA         : output_stack   TYPE report=>output_list
                 , vehicle_stack  TYPE STANDARD TABLE
                                    OF REF TO vehicle
                 .
    METHODS      : build_report
                 , present_report
                 , set_column_titles
                     IMPORTING
                       alv_grid
                         TYPE REF
                           TO cl_salv_table
                 .
ENDCLASS. " report definition
*======================================================================
*
*   C l a s s   I m p l e m e n t a t i o n s
*
*======================================================================
*----------------------------------------------------------------------
*
* Class: navigator implementation
*
*----------------------------------------------------------------------
CLASS navigator IMPLEMENTATION.
*----------------------------------------------------------------------
* Method: change_heading
*----------------------------------------------------------------------
  METHOD change_heading.
    DATA         : compass_offset TYPE int4
                 .
*   Any turn value other than a valid navigator turn will be ignored:
    CHECK turn EQ simple_navigation=>left_turn
       OR turn EQ simple_navigation=>right_turn
       OR turn EQ simple_navigation=>u_turn.
*   Convert current heading to string offset:
    FIND me->heading IN simple_navigation=>compass MATCH OFFSET compass_offset.
*   Adjust string offset based on turn; A left turn decrements string
*   offset by 1; A right turn increments it by 1; A U-turn increments it
*   by 2:
    CASE turn.
      WHEN simple_navigation=>left_turn.
        SUBTRACT 01 FROM compass_offset.
      WHEN simple_navigation=>right_turn.
        ADD      01 TO   compass_offset.
      WHEN simple_navigation=>u_turn.
        ADD      02 TO   compass_offset.
    ENDCASE.
*   Adjust numeric value to accommodate underflow or overflow:
    IF compass_offset LT navigator=>compass_offset_limit_lo.
      ADD      04 TO   compass_offset.
    ENDIF.
    IF compass_offset GT navigator=>compass_offset_limit_hi.
      SUBTRACT 04 FROM compass_offset.
    ENDIF.
*   Reset heading:
    me->heading                   = simple_navigation=>compass+compass_offset(01).
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: get_heading
*----------------------------------------------------------------------
  METHOD get_heading.
    heading                       = me->heading.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: constructor
*----------------------------------------------------------------------
  METHOD constructor.
*   Any heading value other than a valid navigator compass heading will
*   default to the first navigator compass heading:
    IF simple_navigation=>compass CA heading.
      me->heading                 = heading.
    ELSE.
      me->heading                 = simple_navigation=>compass+00(01).
    ENDIF.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: class_constructor
*----------------------------------------------------------------------
  METHOD class_constructor.
*   The constant containing the compass points is now defined in the
*   interface.  Here we are setting the value for field
*   compass_offset_limit_hi:
    navigator=>compass_offset_limit_hi
                                  = strlen( simple_navigation=>compass ) - 01.
  ENDMETHOD.
ENDCLASS. " navigator implementation
*----------------------------------------------------------------------
*
* Class: gps implementation
*
*----------------------------------------------------------------------
CLASS gps IMPLEMENTATION.
*----------------------------------------------------------------------
* Method: change_heading
*----------------------------------------------------------------------
  METHOD change_heading.
*   Any turn value other than a valid navigator turn will be ignored:
    CHECK turn EQ simple_navigation=>left_turn
       OR turn EQ simple_navigation=>right_turn
       OR turn EQ simple_navigation=>u_turn.
*   Adjust heading based on turn; A left turn decrements heading
*   by 90; A right turn increments it by 90; A U-turn increments it
*   by 180:
    CASE turn.
      WHEN simple_navigation=>left_turn.
        SUBTRACT gps=>degrees_90  FROM me->bearing.
      WHEN simple_navigation=>right_turn.
        ADD      gps=>degrees_90  TO   me->bearing.
      WHEN simple_navigation=>u_turn.
        ADD      gps=>degrees_180 TO   me->bearing.
    ENDCASE.
*   Adjust to accommodate underflow or overflow:
    IF me->bearing LT 00.
      ADD      gps=>degrees_360   TO   me->bearing.
    ENDIF.
    IF me->bearing GE gps=>degrees_360.
      SUBTRACT gps=>degrees_360   FROM me->bearing.
    ENDIF.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: get_heading
*----------------------------------------------------------------------
  METHOD get_heading.
    DATA         : compass_offset TYPE int4
                 .
    compass_offset                = me->bearing / gps=>degrees_90.
    heading                       = simple_navigation=>compass+compass_offset(01).
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: constructor
*----------------------------------------------------------------------
  METHOD constructor.
    DATA         : compass_offset TYPE int4
                 .
*   Any heading value other than a valid navigator compass heading will
*   default to north (00 degrees):
    FIND heading IN simple_navigation=>compass MATCH OFFSET compass_offset.
    me->bearing                   = compass_offset * gps=>degrees_90.
  ENDMETHOD.
ENDCLASS. " gps implementation
*----------------------------------------------------------------------
*
* Class: dead_reckoning implementation
*
*----------------------------------------------------------------------
CLASS dead_reckoning IMPLEMENTATION.
*----------------------------------------------------------------------
* Method: change_heading
*----------------------------------------------------------------------
  METHOD change_heading.                                    "#EC *
*   We have no ability to indicate or change direction with dead reckoning:
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: get_heading
*----------------------------------------------------------------------
  METHOD get_heading.
    heading                       = me->heading.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: constructor
*----------------------------------------------------------------------
  METHOD constructor.
*   We have no ability to indicate or change direction with dead reckoning:
    me->heading                   = dead_reckoning=>unknown_direction.
  ENDMETHOD.
ENDCLASS. " dead_reckoning implementation
*----------------------------------------------------------------------
*
* Class: vehicle implementation
*
*----------------------------------------------------------------------
CLASS vehicle IMPLEMENTATION.
*----------------------------------------------------------------------
* Method: accelerate
*----------------------------------------------------------------------
  METHOD accelerate.
    ADD acceleration              TO me->speed.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: change_heading
*----------------------------------------------------------------------
  METHOD change_heading.
    CALL METHOD me->navigation_unit->change_heading
      EXPORTING
        turn = turn.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: get_characteristics
*----------------------------------------------------------------------
  METHOD get_characteristics.
    serial_number                 = me->serial_number.
    license_plate                 = me->license_plate.
    brand                         = me->brand.
    model                         = me->model.
    year                          = me->year.
    color                         = me->color.
    location                      = me->location.
    speed_unit                    = me->speed_unit.
    weight_unit                   = me->weight_unit.
    navigation_type               = me->navigation_type.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: get_heading
*----------------------------------------------------------------------
  METHOD get_heading.
    CALL METHOD me->navigation_unit->get_heading
      IMPORTING
        heading = heading.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: get_speed
*----------------------------------------------------------------------
  METHOD get_speed.
    speed                         = me->speed.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: constructor
*----------------------------------------------------------------------
  METHOD constructor.
    CONSTANTS    : selected       TYPE checkbox  VALUE 'X'
                 , default_navigation
                                  TYPE vehicle=>navigator_type
                                                 VALUE dead_reckoning=>class_id
                 .
    CALL METHOD vehicle=>get_serial_number
      IMPORTING
        serial_number = me->serial_number.
    me->license_plate             = license_plate.
    me->brand                     = brand.
    me->model                     = model.
    me->year                      = year.
    me->color                     = color.
    me->location                  = location.
    me->speed_unit                = speed_unit.
    me->tare_weight               = tare_weight.
    me->weight_unit               = weight_unit.
*   We need to create a navigator object for this vehicle:
    CASE selected.
      WHEN gps_navigation.
        me->navigation_type       = gps=>class_id.
      WHEN basic_navigation.
        me->navigation_type       = navigator=>class_id.
      WHEN OTHERS.
        me->navigation_type       = default_navigation.
    ENDCASE.
    CREATE OBJECT me->navigation_unit TYPE (me->navigation_type)
      EXPORTING
        heading                   = heading
        .
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: class_constructor
*----------------------------------------------------------------------
  METHOD class_constructor.
    vehicle=>last_serial_value    = 1000. " set default starting point
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: get_serial_number
*----------------------------------------------------------------------
  METHOD get_serial_number.
    ADD 01 TO vehicle=>last_serial_value.
    serial_number                 = vehicle=>last_serial_value.
  ENDMETHOD.
ENDCLASS. " vehicle implementation
*----------------------------------------------------------------------
*
* Class: car implementation
*
*----------------------------------------------------------------------
CLASS car IMPLEMENTATION.
*----------------------------------------------------------------------
* Method: constructor
*----------------------------------------------------------------------
  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        license_plate    = license_plate
        brand            = brand
        model            = model
        year             = year
        color            = color
        location         = location
        speed_unit       = speed_unit
        heading          = heading
        tare_weight      = tare_weight
        weight_unit      = weight_unit
        basic_navigation = basic_navigation
        gps_navigation   = gps_navigation
        no_navigation    = no_navigation.
    me->passengers                = passengers.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: get_description
*----------------------------------------------------------------------
  METHOD get_description.
    description                   = me->descriptor.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: get_gross_weight
*----------------------------------------------------------------------
  METHOD get_gross_weight.
    CONSTANTS    : average_adult_weight_in_lbs
                                  TYPE vehicle=>weight_type
                                                 VALUE 180
                 , average_adult_weight_unit
                                  TYPE msehi     VALUE 'LB'
                 .
    DATA         : average_passenger_weight
                                  TYPE vehicle=>weight_type
                 , registered_weight_unit
                                  TYPE msehi
                 .
*   We have the average weight of an adult passenger expressed in pounds.
*   We need this to be expressed in the weight unit specified for this
*   car:
    average_passenger_weight      = average_adult_weight_in_lbs.
*   Get weight unit used when registering this car:
    CALL METHOD me->get_characteristics
      IMPORTING
        weight_unit = registered_weight_unit.
    IF registered_weight_unit NE average_adult_weight_unit.
*     Convert to registered weight unit.  We will ignore any
*     exceptions of unit conversion:
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING
          input    = average_passenger_weight
          unit_in  = average_adult_weight_unit
          unit_out = registered_weight_unit
        IMPORTING
          output   = average_passenger_weight
        EXCEPTIONS
          OTHERS   = 0.
    ENDIF.
    gross_weight                  = me->tare_weight
                                  + me->passengers * average_passenger_weight.
  ENDMETHOD.
ENDCLASS. " car implementation
*----------------------------------------------------------------------
*
* Class: truck implementation
*
*----------------------------------------------------------------------
CLASS truck IMPLEMENTATION.
*----------------------------------------------------------------------
* Method: class_constructor
*----------------------------------------------------------------------
  METHOD class_constructor.
*   This method call to class truck_axle_weight_monitor causes that
*   class to process its class_constructor, which enables it to
*   register itself as an observer of the weight_exceeds_2_axle_limit
*   event whenever it is raised by the truck class:
    CALL METHOD truck_axle_weight_monitor=>get_2_axle_weight_limit
      IMPORTING
        maximum_weight = truck=>weight_limit_for_2_axles
        weight_unit    = truck=>weight_limit_2_axles_unit.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: constructor
*----------------------------------------------------------------------
  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        license_plate    = license_plate
        brand            = brand
        model            = model
        year             = year
        color            = color
        location         = location
        speed_unit       = speed_unit
        heading          = heading
        tare_weight      = tare_weight
        weight_unit      = weight_unit
        basic_navigation = basic_navigation
        gps_navigation   = gps_navigation
        no_navigation    = no_navigation.
    me->cargo_weight              = cargo_weight.
*   The following method call enables the truck class to raise the
*   weight_exceeds_2_axle_limit event when it detects its gross weight
*   exceeds the 2-axle weight limit:
    CALL METHOD me->check_axle_weight.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: get_description
*----------------------------------------------------------------------
  METHOD get_description.
    description                   = me->descriptor.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: get_gross_weight
*----------------------------------------------------------------------
  METHOD get_gross_weight.
    gross_weight                  = me->tare_weight
                                  + me->cargo_weight.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: check_axle_weight
*----------------------------------------------------------------------
  METHOD check_axle_weight.
    DATA         : normalized_gross_weight
                                  TYPE vehicle=>weight_type
                 , registered_weight_unit
                                  TYPE msehi
                 .
*   The truck class already has information about the maximum weight
*   for a 2-axle truck.  We want to check whether the weight for
*   this truck exceeds that limit, and if so we want to notify any
*   observers who want to know this.
    normalized_gross_weight       = me->tare_weight
                                  + me->cargo_weight.
*   Get weight unit used when registering this truck:
    CALL METHOD me->get_characteristics
      IMPORTING
        weight_unit = registered_weight_unit.
    IF registered_weight_unit NE truck=>weight_limit_2_axles_unit.
*     Convert from registered weight unit into 2-axle weight limit unit.
*     We will ignore any exceptions of unit conversion:
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING
          input    = normalized_gross_weight
          unit_in  = registered_weight_unit
          unit_out = truck=>weight_limit_2_axles_unit
        IMPORTING
          output   = normalized_gross_weight
        EXCEPTIONS
          OTHERS   = 0.
    ENDIF.
    IF normalized_gross_weight GT truck=>weight_limit_for_2_axles.
*     We want to notify any observers that the weight of this truck
*     exceeds the 2-axle weight limit:
      RAISE EVENT weight_exceeds_2_axle_limit.
    ENDIF.
  ENDMETHOD.
ENDCLASS. " truck implementation
*----------------------------------------------------------------------
*
* Class: truck_axle_weight_monitor implementation
*
*----------------------------------------------------------------------
CLASS truck_axle_weight_monitor IMPLEMENTATION.
*----------------------------------------------------------------------
* Method: add_to_over_2_axle_limit_count
*----------------------------------------------------------------------
  METHOD add_to_over_2_axle_limit_count.
*   This method is invoked in response to the weight_exceeds_2_axle_limit
*   event raised by the truck class.  Accordingly, we want to increment
*   the counter keeping track of the number of trucks which have a gross
*   weight exceeding the 2-axle weight limit.
    ADD 01                        TO me->over_2_axle_limit_count.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: class_constructor
*----------------------------------------------------------------------
  METHOD class_constructor.
*   Create singleton instance of this class:
    CREATE OBJECT truck_axle_weight_monitor=>singleton.
*   We want to register this singleton instance as an observer of all instances
*   of the truck class, specifically of the weight_exceeds_2_axle_limit event
*   that can be raised by instances of the truck class, and furthermore, when
*   that event is raised by any instance of the truck class, we want method
*   add_to_over_2_axle_limit_count of this singleton instance to be invoked:
    SET HANDLER truck_axle_weight_monitor=>singleton->add_to_over_2_axle_limit_count
            FOR ALL INSTANCES.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: get_2_axle_weight_limit
*----------------------------------------------------------------------
  METHOD get_2_axle_weight_limit.
*   This will be the first method of this class to be invoked, and enables the
*   class_constructor method to perform its processing, an imperative first
*   step so that this class can register itself as an observer of the truck
*   class.
    maximum_weight                = truck_axle_weight_monitor=>weight_limit_in_lbs_2_axles.
    weight_unit                   = truck_axle_weight_monitor=>weight_limit_2_axles_unit.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: show_over_2_axle_limit_count
*----------------------------------------------------------------------
  METHOD show_over_2_axle_limit_count.
    CONSTANTS    : severity_information
                                  TYPE symsgty   VALUE 'I'
                 , severity_warning
                                  TYPE symsgty   VALUE 'W'
                 .
    DATA         : message_severity
                                  TYPE symsgty
                 .
    IF me->over_2_axle_limit_count GT 00.
      message_severity            = severity_warning.
    ELSE.
      message_severity            = severity_information.
    ENDIF.
    MESSAGE i398(00) WITH me->over_2_axle_limit_count
                          'trucks exceed the 2-axle weight limit of' "#EC *
                          me->weight_limit_in_lbs_2_axles
                          me->weight_limit_2_axles_unit
             DISPLAY LIKE message_severity.
  ENDMETHOD.
ENDCLASS. " truck_axle_weight_monitor implementation
*----------------------------------------------------------------------
*
* Class: report implementation
*
*----------------------------------------------------------------------
CLASS report IMPLEMENTATION.
*----------------------------------------------------------------------
* Method:  build_report
*----------------------------------------------------------------------
  METHOD build_report.
    DATA         : output_entry   LIKE LINE
                                    OF report=>output_stack
                 , vehicle_entry  TYPE REF
                                    TO vehicle
                 .
*   Loop through all the vehicle objects held in the vehicle
*   objects table.  For each one, get its characteristics and
*   place a corresponding entry in the report:
    LOOP AT me->vehicle_stack
       INTO     vehicle_entry.
      CALL METHOD vehicle_entry->get_characteristics
        IMPORTING
          serial_number   = output_entry-serial_number
          license_plate   = output_entry-license_plate
          brand           = output_entry-brand
          model           = output_entry-model
          year            = output_entry-year
          color           = output_entry-color
          location        = output_entry-location
          speed_unit      = output_entry-speed_unit
          weight_unit     = output_entry-weight_unit
          navigation_type = output_entry-navigation_type.
      CALL METHOD vehicle_entry->get_heading
        IMPORTING
          heading = output_entry-heading.
      CALL METHOD vehicle_entry->get_speed
        IMPORTING
          speed = output_entry-speed.
      CALL METHOD vehicle_entry->get_gross_weight
        IMPORTING
          gross_weight = output_entry-weight.
      CALL METHOD vehicle_entry->get_description
        IMPORTING
          description = output_entry-description.
      APPEND     output_entry
          TO me->output_stack.
    ENDLOOP.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: class_constructor
*----------------------------------------------------------------------
  METHOD class_constructor.
    CREATE OBJECT report=>singleton.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: present_report
*----------------------------------------------------------------------
  METHOD present_report.
    DATA         : alv_grid       TYPE REF
                                    TO cl_salv_table
                 .
*   Create alv grid:
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = alv_grid
          CHANGING
            t_table      = me->output_stack.
      CATCH cx_salv_msg.
        MESSAGE e398(00) WITH 'Failure to create alv grid object' "#EC *
                              space
                              space
                              space
                              .
    ENDTRY.
*   Set column titles:
    CALL METHOD me->set_column_titles
      EXPORTING
        alv_grid = alv_grid.
*   Display alv grid:
    CALL METHOD alv_grid->display.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: register_car_entry
*----------------------------------------------------------------------
  METHOD register_car_entry.
    DATA         : vehicle_entry  TYPE REF
                                    TO vehicle
                 .
*   Create a new vehicle object for tracking this car:
    CREATE OBJECT vehicle_entry
      TYPE car
      EXPORTING
        license_plate    = license_plate
        brand            = brand
        model            = model
        year             = year
        color            = color
        location         = location
        speed_unit       = speed_unit
        heading          = heading
        tare_weight      = tare_weight
        weight_unit      = weight_unit
        passengers       = passengers
        basic_navigation = basic_navigation
        gps_navigation   = gps_navigation
        no_navigation    = no_navigation.
*   Put this new car object in the vehicle objects table:
    APPEND     vehicle_entry
        TO me->vehicle_stack.
*   Set the attributes of this car entry:
    CALL METHOD vehicle_entry->accelerate
      EXPORTING
        : acceleration            = speed01
        , acceleration            = speed02
        , acceleration            = speed03
        .
    CALL METHOD vehicle_entry->change_heading
      EXPORTING
        : turn                    = turn01
        , turn                    = turn02
        , turn                    = turn03
        .
*   Notify user that we have registered a car entry:
    MESSAGE s398(00) WITH 'Car entry registered for'        "#EC *
                          license_plate
                          space
                          space
                          .
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: register_truck_entry
*----------------------------------------------------------------------
  METHOD register_truck_entry.
    DATA         : vehicle_entry  TYPE REF
                                    TO vehicle
                 .
*   Create a new vehicle object for tracking this truck:
    CREATE OBJECT vehicle_entry
      TYPE truck
      EXPORTING
        license_plate    = license_plate
        brand            = brand
        model            = model
        year             = year
        color            = color
        location         = location
        speed_unit       = speed_unit
        heading          = heading
        tare_weight      = tare_weight
        weight_unit      = weight_unit
        cargo_weight     = cargo_weight
        basic_navigation = basic_navigation
        gps_navigation   = gps_navigation
        no_navigation    = no_navigation.
*   Put this new truck object in the vehicle objects table:
    APPEND     vehicle_entry
        TO me->vehicle_stack.
*   Set the attributes of this truck entry:
    CALL METHOD vehicle_entry->accelerate
      EXPORTING
        : acceleration            = speed01
        , acceleration            = speed02
        , acceleration            = speed03
        .
    CALL METHOD vehicle_entry->change_heading
      EXPORTING
        : turn                    = turn01
        , turn                    = turn02
        , turn                    = turn03
        .
*   Notify user that we have registered a truck entry:
    MESSAGE s398(00) WITH 'Truck entry registered for'      "#EC *
                          license_plate
                          space
                          space
                          .
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: set_column_titles
*----------------------------------------------------------------------
  METHOD set_column_titles.
    CONSTANTS    : column_name_serial_number
                                  TYPE lvc_fname VALUE 'SERIAL_NUMBER'
                 , column_title_serial_number               "#EC *
                                  TYPE string    VALUE `Serial`
                 , column_name_license_plate
                                  TYPE lvc_fname VALUE 'LICENSE_PLATE'
                 , column_title_license_plate               "#EC *
                                  TYPE string    VALUE `License plate`
                 , column_name_brand
                                  TYPE lvc_fname VALUE 'BRAND'
                 , column_title_brand                       "#EC *
                                  TYPE string    VALUE `Brand`
                 , column_name_model
                                  TYPE lvc_fname VALUE 'MODEL'
                 , column_title_model                       "#EC *
                                  TYPE string    VALUE `Model`
                 , column_name_year
                                  TYPE lvc_fname VALUE 'YEAR'
                 , column_title_year                        "#EC *
                                  TYPE string    VALUE `Year`
                 , column_name_color
                                  TYPE lvc_fname VALUE 'COLOR'
                 , column_title_color                       "#EC *
                                  TYPE string    VALUE `Color`
                 , column_name_location
                                  TYPE lvc_fname VALUE 'LOCATION'
                 , column_title_location                    "#EC *
                                  TYPE string    VALUE `Location`
                 , column_name_heading
                                  TYPE lvc_fname VALUE 'HEADING'
                 , column_title_heading                     "#EC *
                                  TYPE string    VALUE `Heading`
                 , column_name_speed
                                  TYPE lvc_fname VALUE 'SPEED'
                 , column_title_speed                       "#EC *
                                  TYPE string    VALUE `Speed`
                 , column_name_speed_unit
                                  TYPE lvc_fname VALUE 'SPEED_UNIT'
                 , column_title_speed_unit                  "#EC *
                                  TYPE string    VALUE `SUoM`
                 , column_name_weight
                                  TYPE lvc_fname VALUE 'WEIGHT'
                 , column_title_weight                      "#EC *
                                  TYPE string    VALUE `Weight`
                 , column_name_weight_unit
                                  TYPE lvc_fname VALUE 'WEIGHT_UNIT'
                 , column_title_weight_unit                 "#EC *
                                  TYPE string    VALUE `WUoM`
                 , column_name_description
                                  TYPE lvc_fname VALUE 'DESCRIPTION'
                 , column_title_description                 "#EC *
                                  TYPE string    VALUE `Descriptor`
                 , column_name_navigation_type
                                  TYPE lvc_fname VALUE 'NAVIGATION_TYPE'
                 , column_title_navigation_type             "#EC *
                                  TYPE string    VALUE `Navigation type`
                 , minimum_column_width
                                  TYPE int4      VALUE 08
                 .
    DATA         : grid_columns   TYPE REF
                                    TO cl_salv_columns_table
                 , grid_column_stack
                                  TYPE salv_t_column_ref
                 , grid_column_entry
                                  LIKE LINE
                                    OF grid_column_stack
                 , grid_column_title_short
                                  TYPE scrtext_s
                 , grid_column_width
                                  TYPE lvc_outlen
                 .
*   Set alv grid column titles:
    CALL METHOD alv_grid->get_columns
      RECEIVING
        value = grid_columns.
    CALL METHOD grid_columns->get
      RECEIVING
        value = grid_column_stack.
    LOOP AT grid_column_stack
       INTO grid_column_entry.
      CLEAR grid_column_width.
      CASE grid_column_entry-columnname.
        WHEN column_name_serial_number.
          grid_column_title_short = column_title_serial_number.
        WHEN column_name_license_plate.
          grid_column_title_short = column_title_license_plate.
        WHEN column_name_brand.
          grid_column_title_short = column_title_brand.
        WHEN column_name_model.
          grid_column_title_short = column_title_model.
        WHEN column_name_year.
          grid_column_title_short = column_title_year.
        WHEN column_name_color.
          grid_column_title_short = column_title_color.
        WHEN column_name_location.
          grid_column_title_short = column_title_location.
        WHEN column_name_heading.
          grid_column_title_short = column_title_heading.
          grid_column_width       = minimum_column_width.
        WHEN column_name_speed.
          grid_column_title_short = column_title_speed.
          grid_column_width       = minimum_column_width.
        WHEN column_name_speed_unit.
          grid_column_title_short = column_title_speed_unit.
          grid_column_width       = minimum_column_width.
        WHEN column_name_weight.
          grid_column_title_short = column_title_weight.
        WHEN column_name_weight_unit.
          grid_column_title_short = column_title_weight_unit.
          grid_column_width       = minimum_column_width.
        WHEN column_name_description.
          grid_column_title_short = column_title_description.
        WHEN column_name_navigation_type.
          grid_column_title_short = column_title_navigation_type.
        WHEN OTHERS.
          CLEAR grid_column_title_short.
      ENDCASE.
      CALL METHOD grid_column_entry-r_column->set_short_text
        EXPORTING
          value = grid_column_title_short.
      IF grid_column_width GT 00.
        CALL METHOD grid_column_entry-r_column->set_output_length
          EXPORTING
            value = grid_column_width.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
*----------------------------------------------------------------------
* Method: show_report
*----------------------------------------------------------------------
  METHOD show_report.
    CALL METHOD: me->build_report
               , me->present_report
               .
  ENDMETHOD.
ENDCLASS. " report implementation
*======================================================================
*
*   S c r e e n   C o m p o n e n t s
*
*======================================================================
*----------------------------------------------------------------------
* Selection screen definition
*----------------------------------------------------------------------
SELECTION-SCREEN : BEGIN OF BLOCK block_a WITH FRAME.
PARAMETERS       :   pplate       TYPE vehicle=>license_plate_type
                 ,   pbrand       TYPE vehicle=>brand_type
                 ,   pmodel       TYPE vehicle=>model_type
                 ,   pyear        TYPE vehicle=>year_type
                 ,   pcolor       TYPE vehicle=>color_type
                 ,   plocatn      TYPE vehicle=>location_type
                 ,   pheading     TYPE simple_navigation=>heading_type
                 ,   pturn01      TYPE simple_navigation=>turn_type
                 ,   pturn02      TYPE simple_navigation=>turn_type
                 ,   pturn03      TYPE simple_navigation=>turn_type
                 ,   pspeedu      TYPE vehicle=>speed_unit_type
                 ,   pspeed01     TYPE vehicle=>speed_type
                 ,   pspeed02     TYPE vehicle=>speed_type
                 ,   pspeed03     TYPE vehicle=>speed_type
                 ,   pwghtu       TYPE vehicle=>weight_unit_type
                 ,   pevw         TYPE vehicle=>weight_type
                 ,   pcargow      TYPE vehicle=>weight_type
                 ,   ppsngrs      TYPE car=>passengers_type
                 ,   xbnav        RADIOBUTTON GROUP nav
                 ,   xgps         RADIOBUTTON GROUP nav
                 ,   xnonav       RADIOBUTTON GROUP nav
                 .
SELECTION-SCREEN : END   OF BLOCK block_a.
*======================================================================
*
*   C l a s s i c   P r o c e d u r a l   E v e n t s
*
*======================================================================
*----------------------------------------------------------------------
* Event: initialization
*----------------------------------------------------------------------
INITIALIZATION.
  SET PF-STATUS report=>selection_screen_status_name.
*----------------------------------------------------------------------
* Event: at selection-screen
*----------------------------------------------------------------------
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ report=>execute
     OR sy-ucomm EQ report=>add_new_car
     OR sy-ucomm EQ report=>add_new_truck.
  CASE sy-ucomm.
    WHEN report=>add_new_car.
      CALL METHOD report=>singleton->register_car_entry
        EXPORTING
          license_plate    = pplate
          brand            = pbrand
          model            = pmodel
          year             = pyear
          color            = pcolor
          location         = plocatn
          heading          = pheading
          turn01           = pturn01
          turn02           = pturn02
          turn03           = pturn03
          speed01          = pspeed01
          speed02          = pspeed02
          speed03          = pspeed03
          speed_unit       = pspeedu
          tare_weight      = pevw
          weight_unit      = pwghtu
          passengers       = ppsngrs
          basic_navigation = xbnav
          gps_navigation   = xgps
          no_navigation    = xnonav.
*     This implicitly returns to the initial selection screen.
    WHEN report=>add_new_truck.
      CALL METHOD report=>singleton->register_truck_entry
        EXPORTING
          license_plate    = pplate
          brand            = pbrand
          model            = pmodel
          year             = pyear
          color            = pcolor
          location         = plocatn
          heading          = pheading
          turn01           = pturn01
          turn02           = pturn02
          turn03           = pturn03
          speed01          = pspeed01
          speed02          = pspeed02
          speed03          = pspeed03
          speed_unit       = pspeedu
          tare_weight      = pevw
          weight_unit      = pwghtu
          cargo_weight     = pcargow
          basic_navigation = xbnav
          gps_navigation   = xgps
          no_navigation    = xnonav.
*     This implicitly returns to the initial selection screen.
    WHEN OTHERS.
*     No action; Execute will trigger start-of-selection event.
  ENDCASE.
*----------------------------------------------------------------------
* Event: start-of-selection
*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
* Event: end-of-selection
*----------------------------------------------------------------------
END-OF-SELECTION.
  CALL METHOD truck_axle_weight_monitor=>singleton->show_over_2_axle_limit_count.
  CALL METHOD report=>singleton->show_report.
