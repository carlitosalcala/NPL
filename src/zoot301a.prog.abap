*&---------------------------------------------------------------------*
*& Report ZOOT301A
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZOOT301A.

*----------------------------------------------------------------------
*  Title:
*    Objects 301: Design Patterns: Singleton pattern, step 1
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
*   Class "report" is changed from being purely static to
*   being capable of creating exactly one instance.  This
*   demonstrates the Singleton pattern, where a class capable
*   of being instantiated is limited to only one instance.  In
*   this case the single instance is created via the static
*   (class) constructor, which will be invoked only once during
*   the lifetime of this program.  A new private static attribute
*   "singleton" is defined as type "ref to report" to hold this
*   new object reference.  The ability to create an instance
*   of the "report" class is facilitated by the removal
*   of the "abstract" qualifier from its class definition
*   statement.  Multiple instances of "report" are prevented by
*   the "create private" clause retained on the class definition
*   statement, meaning that no other entities can create any
*   instances of the "report" class other than the "report" class
*   itself, and its "create object" statement will be executed only
*   once during processing of the static constructor.  Using the static
*   constructor is only one of many ways to insure a singleton object.
*   Also, a new static method "get_singleton_instance" enables external
*   entities to retrieve a reference to the singleton "report" object.
*
*   Calls by the ABAP classic event blocks to public methods of the
*   "report" class, which have been changed from static methods to
*   instance methods, are changed in the following ways:
*     1) A new global variable is defined as "type ref to report"
*        to hold a reference to the "report" instance.
*     2) The ABAP classic event block "initialization" now invokes
*        method "get_singleton_instance" of class "report", using the
*        returned reference to populate the new global variable.
*     3) References to former static methods "register_car_entry",
*        "register_truck_entry" and "show_report" of the "report"
*        class are changed to reference these now instance methods
*        via the new global reference variable.
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
interface simple_navigation.
    types        : turn_type      type char1
                 , heading_type   type char1
                 .
    constants    : left_turn      type simple_navigation=>turn_type
                                                 value 'L'
                 , right_turn     type simple_navigation=>turn_type
                                                 value 'R'
                 , u_turn         type simple_navigation=>turn_type
                                                 value 'U'
                 , compass        type char4     value 'NESW'
                 .
    methods      : change_heading
                     importing
                       turn
                         type simple_navigation=>turn_type
                 , get_heading
                     exporting
                       heading
                         type simple_navigation=>heading_type
                 .
endinterface. " simple_navigation
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
class navigator definition final.
  public section.
    interfaces   : simple_navigation
                 .
    aliases      : change_heading
                     for simple_navigation~change_heading
                 , get_heading
                     for simple_navigation~get_heading
                 .
    constants    : class_id       type seoclsname value 'NAVIGATOR'
                 .
    class-methods: class_constructor
                 .
    methods      : constructor
                     importing
                       heading
                         type simple_navigation=>heading_type
                 .
* protected section.
  private section.
    constants    : compass_offset_limit_lo
                                  type int4      value 00
                 .
    class-data   : compass_offset_limit_hi
                                  type int4
                 .
    data         : heading        type simple_navigation=>heading_type
                 .
endclass. " navigator definition
*----------------------------------------------------------------------
*
* Class: gps definition
*
*----------------------------------------------------------------------
class gps definition final.
  public section.
    interfaces   : simple_navigation
                 .
    aliases      : change_heading
                     for simple_navigation~change_heading
                 , get_heading
                     for simple_navigation~get_heading
                 .
    constants    : class_id       type seoclsname value 'GPS'
                 .
    methods      : constructor
                     importing
                       heading
                         type simple_navigation=>heading_type
                 .
* protected section.
  private section.
    constants    : degrees_90     type int4      value 90
                 , degrees_180    type int4      value 180
                 , degrees_360    type int4      value 360
                 .
    data         : bearing        type int4
                 .
endclass. " gps definition
*----------------------------------------------------------------------
*
* Class: dead_reckoning definition
*
*----------------------------------------------------------------------
class dead_reckoning definition final.
  public section.
    interfaces   : simple_navigation
                 .
    aliases      : change_heading
                     for simple_navigation~change_heading
                 , get_heading
                     for simple_navigation~get_heading
                 .
    constants    : class_id       type seoclsname value 'DEAD_RECKONING'
                 .
    methods      : constructor
                     importing
                       heading                                   "#EC *
                         type simple_navigation=>heading_type
                 .
* protected section.
  private section.
    constants    : unknown_direction
                                  type char01    value '?'
                 .
    data         : heading        type simple_navigation=>heading_type
                 .
endclass. " dead_reckoning definition
*----------------------------------------------------------------------
*
* Class: vehicle definition
*
*----------------------------------------------------------------------
class vehicle definition abstract.
  public section.
    types        : brand_type     type f4txt
                 , color_type     type f4txt
                 , location_type  type f4txt
                 , model_type     type f4txt
                 , license_plate_type
                                  type f4txt
                 , navigator_type type seoclsname
                 , speed_type     type int4
                 , speed_unit_type
                                  type char3
                 , year_type      type num4
                 , serial_type    type num4
                 , weight_type    type int4
                 , weight_unit_type
                                  type char3
                 , description_type
                                  type char15
                 .
    class-methods: class_constructor
                 .
    methods      : accelerate
                     importing
                       acceleration
                         type vehicle=>speed_type
                 , change_heading
                     importing
                       turn
                         type simple_navigation=>turn_type
                 , get_characteristics
                     exporting
                       serial_number
                         type vehicle=>serial_type
                       license_plate
                         type vehicle=>license_plate_type
                       brand
                         type vehicle=>brand_type
                       model
                         type vehicle=>model_type
                       year
                         type vehicle=>year_type
                       color
                         type vehicle=>color_type
                       location
                         type vehicle=>location_type
                       speed_unit
                         type vehicle=>speed_unit_type
                       weight_unit
                         type vehicle=>weight_unit_type
                       navigation_type
                         type vehicle=>navigator_type
                 , get_description abstract
                     exporting
                       description
                         type vehicle=>description_type
                 , get_gross_weight abstract
                     exporting
                       gross_weight
                         type vehicle=>weight_type
                 , get_heading
                     exporting
                       heading
                         type simple_navigation=>heading_type
                 , get_speed
                     exporting
                       speed
                         type vehicle=>speed_type
                 , constructor
                     importing
                       license_plate
                         type vehicle=>license_plate_type
                       brand
                         type vehicle=>brand_type
                       model
                         type vehicle=>model_type
                       year
                         type vehicle=>year_type
                       color
                         type vehicle=>color_type
                       location
                         type vehicle=>location_type
                       speed_unit
                         type vehicle=>speed_unit_type
                       heading
                         type simple_navigation=>heading_type
                       tare_weight
                         type vehicle=>weight_type
                       weight_unit
                         type vehicle=>weight_unit_type
                       basic_navigation
                         type checkbox
                       gps_navigation
                         type checkbox
                       no_navigation                       "#EC *
                         type checkbox
                 .
  protected section.
    data         : tare_weight    type vehicle=>weight_type
                 .
  private section.
    class-data   : last_serial_value
                                  type vehicle=>serial_type
                 .
    data         : license_plate  type vehicle=>license_plate_type
                 , brand          type vehicle=>brand_type
                 , model          type vehicle=>model_type
                 , year           type vehicle=>year_type
                 , color          type vehicle=>color_type
                 , location       type vehicle=>location_type
                 , speed          type vehicle=>speed_type
                 , speed_unit     type vehicle=>speed_unit_type
                 , weight_unit    type vehicle=>weight_unit_type
                 , serial_number  type vehicle=>serial_type
                 , navigation_type
                                  type vehicle=>navigator_type
                 , navigation_unit
                                  type ref
                                    to simple_navigation
                 .
    class-methods: get_serial_number
                     exporting
                       serial_number
                         type vehicle=>serial_type
                 .
endclass. " vehicle definition
*----------------------------------------------------------------------
*
* Class: car definition
*
*----------------------------------------------------------------------
class car definition final inheriting from vehicle.
  public section.
    types        : passengers_type
                                  type int4
                 .
    methods      : constructor
                     importing
                       license_plate
                         type vehicle=>license_plate_type
                       brand
                         type vehicle=>brand_type
                       model
                         type vehicle=>model_type
                       year
                         type vehicle=>year_type
                       color
                         type vehicle=>color_type
                       location
                         type vehicle=>location_type
                       speed_unit
                         type vehicle=>speed_unit_type
                       heading
                         type simple_navigation=>heading_type
                       tare_weight
                         type vehicle=>weight_type
                       weight_unit
                         type vehicle=>weight_unit_type
                       passengers
                         type car=>passengers_type
                       basic_navigation
                         type checkbox
                       gps_navigation
                         type checkbox
                       no_navigation
                         type checkbox
                 , get_description  redefinition
                 , get_gross_weight redefinition
                 .
* protected section.
  private section.
    constants    : descriptor     type string    value 'Car'     "#EC *
                 .
    data         : passengers     type car=>passengers_type
                 .
endclass. " car definition
*----------------------------------------------------------------------
*
* Class: truck definition
*
*----------------------------------------------------------------------
class truck definition final inheriting from vehicle.
  public section.
    methods      : constructor
                     importing
                       license_plate
                         type vehicle=>license_plate_type
                       brand
                         type vehicle=>brand_type
                       model
                         type vehicle=>model_type
                       year
                         type vehicle=>year_type
                       color
                         type vehicle=>color_type
                       location
                         type vehicle=>location_type
                       speed_unit
                         type vehicle=>speed_unit_type
                       heading
                         type simple_navigation=>heading_type
                       tare_weight
                         type vehicle=>weight_type
                       weight_unit
                         type vehicle=>weight_unit_type
                       cargo_weight
                         type vehicle=>weight_type
                       basic_navigation
                         type checkbox
                       gps_navigation
                         type checkbox
                       no_navigation
                         type checkbox
                 , get_description  redefinition
                 , get_gross_weight redefinition
                 .
* protected section.
  private section.
    constants    : descriptor     type string    value 'Truck'   "#EC *
                 .
    data         : cargo_weight   type vehicle=>weight_type
                 .
endclass. " truck definition
*----------------------------------------------------------------------
*
* Class: report definition
*
*----------------------------------------------------------------------
class report definition          final create private.
  public section.
    constants    : execute        type syucomm   value 'ONLI'
                 , add_new_car    type syucomm   value 'NEWCAR'
                 , add_new_truck  type syucomm   value 'NEWTRUCK'
                 , selection_screen_status_name
                                  type sypfkey   value 'SELECTION_SCREEN'
                 .
    class-methods: class_constructor
                 , get_singleton_instance
                     exporting
                       instance
                         type ref
                           to report
                 .
    methods      : register_car_entry
                     importing
                       license_plate
                         type vehicle=>license_plate_type
                       brand
                         type vehicle=>brand_type
                       year
                         type vehicle=>year_type
                       model
                         type vehicle=>model_type
                       color
                         type vehicle=>color_type
                       location
                         type vehicle=>location_type
                       heading
                         type simple_navigation=>heading_type
                       turn01
                         type simple_navigation=>turn_type
                       turn02
                         type simple_navigation=>turn_type
                       turn03
                         type simple_navigation=>turn_type
                       speed01
                         type vehicle=>speed_type
                       speed02
                         type vehicle=>speed_type
                       speed03
                         type vehicle=>speed_type
                       speed_unit
                         type vehicle=>speed_unit_type
                       tare_weight
                         type vehicle=>weight_type
                       weight_unit
                         type vehicle=>weight_unit_type
                       passengers
                         type car=>passengers_type
                       basic_navigation
                         type checkbox
                       gps_navigation
                         type checkbox
                       no_navigation
                         type checkbox
                 , register_truck_entry
                     importing
                       license_plate
                         type vehicle=>license_plate_type
                       brand
                         type vehicle=>brand_type
                       year
                         type vehicle=>year_type
                       model
                         type vehicle=>model_type
                       color
                         type vehicle=>color_type
                       location
                         type vehicle=>location_type
                       heading
                         type simple_navigation=>heading_type
                       turn01
                         type simple_navigation=>turn_type
                       turn02
                         type simple_navigation=>turn_type
                       turn03
                         type simple_navigation=>turn_type
                       speed01
                         type vehicle=>speed_type
                       speed02
                         type vehicle=>speed_type
                       speed03
                         type vehicle=>speed_type
                       speed_unit
                         type vehicle=>speed_unit_type
                       tare_weight
                         type vehicle=>weight_type
                       weight_unit
                         type vehicle=>weight_unit_type
                       cargo_weight
                         type vehicle=>weight_type
                       basic_navigation
                         type checkbox
                       gps_navigation
                         type checkbox
                       no_navigation
                         type checkbox
                 , show_report
                 .
* protected section.
  private section.
    types        : begin of output_row                           "#EC *
                 ,   serial_number
                                  type vehicle=>serial_type
                 ,   license_plate
                                  type vehicle=>license_plate_type
                 ,   brand        type vehicle=>brand_type
                 ,   model        type vehicle=>model_type
                 ,   year         type vehicle=>year_type
                 ,   color        type vehicle=>color_type
                 ,   location     type vehicle=>location_type
                 ,   heading      type simple_navigation=>heading_type
                 ,   speed        type vehicle=>speed_type
                 ,   speed_unit   type vehicle=>speed_unit_type
                 ,   weight       type vehicle=>weight_type
                 ,   weight_unit  type vehicle=>weight_unit_type
                 ,   description  type vehicle=>description_type
                 ,   navigation_type
                                  type vehicle=>navigator_type
                 , end   of output_row
                 , output_list    type standard table
                                    of report=>output_row
                 .
    class-data   : singleton      type ref
                                    to report
                 .
    data         : output_stack   type report=>output_list
                 , vehicle_stack  type standard table
                                    of ref to vehicle
                 .
    methods      : build_report
                 , present_report
                 , set_column_titles
                     importing
                       alv_grid
                         type ref
                           to cl_salv_table
                 .
endclass. " report definition
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
class navigator implementation.
*----------------------------------------------------------------------
* Method: change_heading
*----------------------------------------------------------------------
  method change_heading.
    data         : compass_offset type int4
                 .
*   Any turn value other than a valid navigator turn will be ignored:
    check turn eq simple_navigation=>left_turn
       or turn eq simple_navigation=>right_turn
       or turn eq simple_navigation=>u_turn.
*   Convert current heading to string offset:
    find me->heading in simple_navigation=>compass match offset compass_offset.
*   Adjust string offset based on turn; A left turn decrements string
*   offset by 1; A right turn increments it by 1; A U-turn increments it
*   by 2:
    case turn.
      when simple_navigation=>left_turn.
        subtract 01 from compass_offset.
      when simple_navigation=>right_turn.
        add      01 to   compass_offset.
      when simple_navigation=>u_turn.
        add      02 to   compass_offset.
    endcase.
*   Adjust numeric value to accommodate underflow or overflow:
    if compass_offset lt navigator=>compass_offset_limit_lo.
      add      04 to   compass_offset.
    endif.
    if compass_offset gt navigator=>compass_offset_limit_hi.
      subtract 04 from compass_offset.
    endif.
*   Reset heading:
    me->heading                   = simple_navigation=>compass+compass_offset(01).
  endmethod.
*----------------------------------------------------------------------
* Method: get_heading
*----------------------------------------------------------------------
  method get_heading.
    heading                       = me->heading.
  endmethod.
*----------------------------------------------------------------------
* Method: constructor
*----------------------------------------------------------------------
  method constructor.
*   Any heading value other than a valid navigator compass heading will
*   default to the first navigator compass heading:
    if simple_navigation=>compass ca heading.
      me->heading                 = heading.
    else.
      me->heading                 = simple_navigation=>compass+00(01).
    endif.
  endmethod.
*----------------------------------------------------------------------
* Method: class_constructor
*----------------------------------------------------------------------
  method class_constructor.
*   The constant containing the compass points is now defined in the
*   interface.  Here we are setting the value for field
*   compass_offset_limit_hi:
    navigator=>compass_offset_limit_hi
                                  = strlen( simple_navigation=>compass ) - 01.
  endmethod.
endclass. " navigator implementation
*----------------------------------------------------------------------
*
* Class: gps implementation
*
*----------------------------------------------------------------------
class gps implementation.
*----------------------------------------------------------------------
* Method: change_heading
*----------------------------------------------------------------------
  method change_heading.
*   Any turn value other than a valid navigator turn will be ignored:
    check turn eq simple_navigation=>left_turn
       or turn eq simple_navigation=>right_turn
       or turn eq simple_navigation=>u_turn.
*   Adjust heading based on turn; A left turn decrements heading
*   by 90; A right turn increments it by 90; A U-turn increments it
*   by 180:
    case turn.
      when simple_navigation=>left_turn.
        subtract gps=>degrees_90  from me->bearing.
      when simple_navigation=>right_turn.
        add      gps=>degrees_90  to   me->bearing.
      when simple_navigation=>u_turn.
        add      gps=>degrees_180 to   me->bearing.
    endcase.
*   Adjust to accommodate underflow or overflow:
    if me->bearing lt 00.
      add      gps=>degrees_360   to   me->bearing.
    endif.
    if me->bearing ge gps=>degrees_360.
      subtract gps=>degrees_360   from me->bearing.
    endif.
  endmethod.
*----------------------------------------------------------------------
* Method: get_heading
*----------------------------------------------------------------------
  method get_heading.
    data         : compass_offset type int4
                 .
    compass_offset                = me->bearing / gps=>degrees_90.
    heading                       = simple_navigation=>compass+compass_offset(01).
  endmethod.
*----------------------------------------------------------------------
* Method: constructor
*----------------------------------------------------------------------
  method constructor.
    data         : compass_offset type int4
                 .
*   Any heading value other than a valid navigator compass heading will
*   default to north (00 degrees):
    find heading in simple_navigation=>compass match offset compass_offset.
    me->bearing                   = compass_offset * gps=>degrees_90.
  endmethod.
endclass. " gps implementation
*----------------------------------------------------------------------
*
* Class: dead_reckoning implementation
*
*----------------------------------------------------------------------
class dead_reckoning implementation.
*----------------------------------------------------------------------
* Method: change_heading
*----------------------------------------------------------------------
  method change_heading.                                         "#EC *
*   We have no ability to indicate or change direction with dead reckoning:
  endmethod.
*----------------------------------------------------------------------
* Method: get_heading
*----------------------------------------------------------------------
  method get_heading.
    heading                       = me->heading.
  endmethod.
*----------------------------------------------------------------------
* Method: constructor
*----------------------------------------------------------------------
  method constructor.
*   We have no ability to indicate or change direction with dead reckoning:
    me->heading                   = dead_reckoning=>unknown_direction.
  endmethod.
endclass. " dead_reckoning implementation
*----------------------------------------------------------------------
*
* Class: vehicle implementation
*
*----------------------------------------------------------------------
class vehicle implementation.
*----------------------------------------------------------------------
* Method: accelerate
*----------------------------------------------------------------------
  method accelerate.
    add acceleration              to me->speed.
  endmethod.
*----------------------------------------------------------------------
* Method: change_heading
*----------------------------------------------------------------------
  method change_heading.
    call method me->navigation_unit->change_heading
      exporting
        turn                      = turn.
  endmethod.
*----------------------------------------------------------------------
* Method: get_characteristics
*----------------------------------------------------------------------
  method get_characteristics.
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
  endmethod.
*----------------------------------------------------------------------
* Method: get_heading
*----------------------------------------------------------------------
  method get_heading.
    call method me->navigation_unit->get_heading
      importing
        heading                   = heading
        .
  endmethod.
*----------------------------------------------------------------------
* Method: get_speed
*----------------------------------------------------------------------
  method get_speed.
    speed                         = me->speed.
  endmethod.
*----------------------------------------------------------------------
* Method: constructor
*----------------------------------------------------------------------
  method constructor.
    constants    : selected       type checkbox  value 'X'
                 , default_navigation
                                  type vehicle=>navigator_type
                                                 value dead_reckoning=>class_id
                 .
    call method vehicle=>get_serial_number
      importing
        serial_number             = me->serial_number.
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
    case selected.
      when gps_navigation.
        me->navigation_type       = gps=>class_id.
      when basic_navigation.
        me->navigation_type       = navigator=>class_id.
      when others.
        me->navigation_type       = default_navigation.
    endcase.
    create object me->navigation_unit type (me->navigation_type)
      exporting
        heading                   = heading
        .
  endmethod.
*----------------------------------------------------------------------
* Method: class_constructor
*----------------------------------------------------------------------
  method class_constructor.
    vehicle=>last_serial_value    = 1000. " set default starting point
  endmethod.
*----------------------------------------------------------------------
* Method: get_serial_number
*----------------------------------------------------------------------
  method get_serial_number.
    add 01 to vehicle=>last_serial_value.
    serial_number                 = vehicle=>last_serial_value.
  endmethod.
endclass. " vehicle implementation
*----------------------------------------------------------------------
*
* Class: car implementation
*
*----------------------------------------------------------------------
class car implementation.
*----------------------------------------------------------------------
* Method: constructor
*----------------------------------------------------------------------
  method constructor.
    call method super->constructor
      exporting
        license_plate             = license_plate
        brand                     = brand
        model                     = model
        year                      = year
        color                     = color
        location                  = location
        speed_unit                = speed_unit
        heading                   = heading
        tare_weight               = tare_weight
        weight_unit               = weight_unit
        basic_navigation          = basic_navigation
        gps_navigation            = gps_navigation
        no_navigation             = no_navigation
        .
    me->passengers                = passengers.
  endmethod.
*----------------------------------------------------------------------
* Method: get_description
*----------------------------------------------------------------------
  method get_description.
    description                   = me->descriptor.
  endmethod.
*----------------------------------------------------------------------
* Method: get_gross_weight
*----------------------------------------------------------------------
  method get_gross_weight.
    constants    : average_adult_weight_in_lbs
                                  type vehicle=>weight_type
                                                 value 180
                 , average_adult_weight_unit
                                  type msehi     value 'LB'
                 .
    data         : average_passenger_weight
                                  type vehicle=>weight_type
                 , registered_weight_unit
                                  type msehi
                 .
*   We have the average weight of an adult passenger expressed in pounds.
*   We need this to be expressed in the weight unit specified for this
*   car:
    average_passenger_weight      = average_adult_weight_in_lbs.
*   Get weight unit used when registering this car:
    call method me->get_characteristics
      importing
        weight_unit               = registered_weight_unit.
    if registered_weight_unit ne average_adult_weight_unit.
*     Convert to registered weight unit.  We will ignore any
*     exceptions of unit conversion:
      call function 'UNIT_CONVERSION_SIMPLE'
        exporting
          input                   = average_passenger_weight
          unit_in                 = average_adult_weight_unit
          unit_out                = registered_weight_unit
        importing
          output                  = average_passenger_weight
        exceptions
          others                  = 0
          .
    endif.
    gross_weight                  = me->tare_weight
                                  + me->passengers * average_passenger_weight.
  endmethod.
endclass. " car implementation
*----------------------------------------------------------------------
*
* Class: truck implementation
*
*----------------------------------------------------------------------
class truck implementation.
*----------------------------------------------------------------------
* Method: constructor
*----------------------------------------------------------------------
  method constructor.
    call method super->constructor
      exporting
        license_plate             = license_plate
        brand                     = brand
        model                     = model
        year                      = year
        color                     = color
        location                  = location
        speed_unit                = speed_unit
        heading                   = heading
        tare_weight               = tare_weight
        weight_unit               = weight_unit
        basic_navigation          = basic_navigation
        gps_navigation            = gps_navigation
        no_navigation             = no_navigation
        .
    me->cargo_weight              = cargo_weight.
  endmethod.
*----------------------------------------------------------------------
* Method: get_description
*----------------------------------------------------------------------
  method get_description.
    description                   = me->descriptor.
  endmethod.
*----------------------------------------------------------------------
* Method: get_gross_weight
*----------------------------------------------------------------------
  method get_gross_weight.
    gross_weight                  = me->tare_weight
                                  + me->cargo_weight.
  endmethod.
endclass. " truck implementation
*----------------------------------------------------------------------
*
* Class: report implementation
*
*----------------------------------------------------------------------
class report implementation.
*----------------------------------------------------------------------
* Method:  build_report
*----------------------------------------------------------------------
  method build_report.
    data         : output_entry   like line
                                    of report=>output_stack
                 , vehicle_entry  type ref
                                    to vehicle
                 .
*   Loop through all the vehicle objects held in the vehicle
*   objects table.  For each one, get its characteristics and
*   place a corresponding entry in the report:
    loop at me->vehicle_stack
       into     vehicle_entry.
      call method vehicle_entry->get_characteristics
        importing
          serial_number           = output_entry-serial_number
          license_plate           = output_entry-license_plate
          brand                   = output_entry-brand
          model                   = output_entry-model
          year                    = output_entry-year
          color                   = output_entry-color
          location                = output_entry-location
          speed_unit              = output_entry-speed_unit
          weight_unit             = output_entry-weight_unit
          navigation_type         = output_entry-navigation_type
          .
      call method vehicle_entry->get_heading
        importing
          heading                 = output_entry-heading
          .
      call method vehicle_entry->get_speed
        importing
          speed                   = output_entry-speed
          .
      call method vehicle_entry->get_gross_weight
        importing
          gross_weight            = output_entry-weight
          .
      call method vehicle_entry->get_description
        importing
          description             = output_entry-description
          .
      append     output_entry
          to me->output_stack.
    endloop.
  endmethod.
*----------------------------------------------------------------------
* Method: class_constructor
*----------------------------------------------------------------------
  method class_constructor.
    create object report=>singleton.
  endmethod.
*----------------------------------------------------------------------
* Method: get_singleton_instance
*----------------------------------------------------------------------
  method get_singleton_instance.
    instance                      = report=>singleton.
  endmethod.
*----------------------------------------------------------------------
* Method: present_report
*----------------------------------------------------------------------
  method present_report.
    data         : alv_grid       type ref
                                    to cl_salv_table
                 .
*   Create alv grid:
    try.
      call method cl_salv_table=>factory
        importing
          r_salv_table            = alv_grid
        changing
          t_table                 = me->output_stack
          .
    catch cx_salv_msg.
      message e398(00) with 'Failure to create alv grid object'  "#EC *
                            space
                            space
                            space
                            .
    endtry.
*   Set column titles:
    call method me->set_column_titles
      exporting
        alv_grid                  = alv_grid
        .
*   Display alv grid:
    call method alv_grid->display.
  endmethod.
*----------------------------------------------------------------------
* Method: register_car_entry
*----------------------------------------------------------------------
  method register_car_entry.
    data         : vehicle_entry  type ref
                                    to vehicle
                 .
*   Create a new vehicle object for tracking this car:
    create object vehicle_entry
             type car
      exporting
        license_plate             = license_plate
        brand                     = brand
        model                     = model
        year                      = year
        color                     = color
        location                  = location
        speed_unit                = speed_unit
        heading                   = heading
        tare_weight               = tare_weight
        weight_unit               = weight_unit
        passengers                = passengers
        basic_navigation          = basic_navigation
        gps_navigation            = gps_navigation
        no_navigation             = no_navigation
        .
*   Put this new car object in the vehicle objects table:
    append     vehicle_entry
        to me->vehicle_stack.
*   Set the attributes of this car entry:
    call method vehicle_entry->accelerate
      exporting
        : acceleration            = speed01
        , acceleration            = speed02
        , acceleration            = speed03
        .
    call method vehicle_entry->change_heading
      exporting
        : turn                    = turn01
        , turn                    = turn02
        , turn                    = turn03
        .
*   Notify user that we have registered a car entry:
    message s398(00) with 'Car entry registered for'             "#EC *
                          license_plate
                          space
                          space
                          .
  endmethod.
*----------------------------------------------------------------------
* Method: register_truck_entry
*----------------------------------------------------------------------
  method register_truck_entry.
    data         : vehicle_entry  type ref
                                    to vehicle
                 .
*   Create a new vehicle object for tracking this truck:
    create object vehicle_entry
             type truck
      exporting
        license_plate             = license_plate
        brand                     = brand
        model                     = model
        year                      = year
        color                     = color
        location                  = location
        speed_unit                = speed_unit
        heading                   = heading
        tare_weight               = tare_weight
        weight_unit               = weight_unit
        cargo_weight              = cargo_weight
        basic_navigation          = basic_navigation
        gps_navigation            = gps_navigation
        no_navigation             = no_navigation
        .
*   Put this new truck object in the vehicle objects table:
    append     vehicle_entry
        to me->vehicle_stack.
*   Set the attributes of this truck entry:
    call method vehicle_entry->accelerate
      exporting
        : acceleration            = speed01
        , acceleration            = speed02
        , acceleration            = speed03
        .
    call method vehicle_entry->change_heading
      exporting
        : turn                    = turn01
        , turn                    = turn02
        , turn                    = turn03
        .
*   Notify user that we have registered a truck entry:
    message s398(00) with 'Truck entry registered for'           "#EC *
                          license_plate
                          space
                          space
                          .
  endmethod.
*----------------------------------------------------------------------
* Method: set_column_titles
*----------------------------------------------------------------------
  method set_column_titles.
    constants    : column_name_serial_number
                                  type lvc_fname value 'SERIAL_NUMBER'
                 , column_title_serial_number                    "#EC *
                                  type string    value `Serial`
                 , column_name_license_plate
                                  type lvc_fname value 'LICENSE_PLATE'
                 , column_title_license_plate                    "#EC *
                                  type string    value `License plate`
                 , column_name_brand
                                  type lvc_fname value 'BRAND'
                 , column_title_brand                            "#EC *
                                  type string    value `Brand`
                 , column_name_model
                                  type lvc_fname value 'MODEL'
                 , column_title_model                            "#EC *
                                  type string    value `Model`
                 , column_name_year
                                  type lvc_fname value 'YEAR'
                 , column_title_year                             "#EC *
                                  type string    value `Year`
                 , column_name_color
                                  type lvc_fname value 'COLOR'
                 , column_title_color                            "#EC *
                                  type string    value `Color`
                 , column_name_location
                                  type lvc_fname value 'LOCATION'
                 , column_title_location                         "#EC *
                                  type string    value `Location`
                 , column_name_heading
                                  type lvc_fname value 'HEADING'
                 , column_title_heading                          "#EC *
                                  type string    value `Heading`
                 , column_name_speed
                                  type lvc_fname value 'SPEED'
                 , column_title_speed                            "#EC *
                                  type string    value `Speed`
                 , column_name_speed_unit
                                  type lvc_fname value 'SPEED_UNIT'
                 , column_title_speed_unit                       "#EC *
                                  type string    value `SUoM`
                 , column_name_weight
                                  type lvc_fname value 'WEIGHT'
                 , column_title_weight                           "#EC *
                                  type string    value `Weight`
                 , column_name_weight_unit
                                  type lvc_fname value 'WEIGHT_UNIT'
                 , column_title_weight_unit                      "#EC *
                                  type string    value `WUoM`
                 , column_name_description
                                  type lvc_fname value 'DESCRIPTION'
                 , column_title_description                      "#EC *
                                  type string    value `Descriptor`
                 , column_name_navigation_type
                                  type lvc_fname value 'NAVIGATION_TYPE'
                 , column_title_navigation_type                  "#EC *
                                  type string    value `Navigation type`
                 , minimum_column_width
                                  type int4      value 08
                 .
    data         : grid_columns   type ref
                                    to cl_salv_columns_table
                 , grid_column_stack
                                  type salv_t_column_ref
                 , grid_column_entry
                                  like line
                                    of grid_column_stack
                 , grid_column_title_short
                                  type scrtext_s
                 , grid_column_width
                                  type lvc_outlen
                 .
*   Set alv grid column titles:
    call method alv_grid->get_columns
      receiving
        value                     = grid_columns.
    call method grid_columns->get
      receiving
        value                     = grid_column_stack.
    loop at grid_column_stack
       into grid_column_entry.
      clear grid_column_width.
      case grid_column_entry-columnname.
        when column_name_serial_number.
          grid_column_title_short = column_title_serial_number.
        when column_name_license_plate.
          grid_column_title_short = column_title_license_plate.
        when column_name_brand.
          grid_column_title_short = column_title_brand.
        when column_name_model.
          grid_column_title_short = column_title_model.
        when column_name_year.
          grid_column_title_short = column_title_year.
        when column_name_color.
          grid_column_title_short = column_title_color.
        when column_name_location.
          grid_column_title_short = column_title_location.
        when column_name_heading.
          grid_column_title_short = column_title_heading.
          grid_column_width       = minimum_column_width.
        when column_name_speed.
          grid_column_title_short = column_title_speed.
          grid_column_width       = minimum_column_width.
        when column_name_speed_unit.
          grid_column_title_short = column_title_speed_unit.
          grid_column_width       = minimum_column_width.
        when column_name_weight.
          grid_column_title_short = column_title_weight.
        when column_name_weight_unit.
          grid_column_title_short = column_title_weight_unit.
          grid_column_width       = minimum_column_width.
        when column_name_description.
          grid_column_title_short = column_title_description.
        when column_name_navigation_type.
          grid_column_title_short = column_title_navigation_type.
        when others.
          clear grid_column_title_short.
      endcase.
      call method grid_column_entry-r_column->set_short_text
        exporting
          value                   = grid_column_title_short.
      if grid_column_width gt 00.
        call method grid_column_entry-r_column->set_output_length
          exporting
            value                 = grid_column_width.
      endif.
    endloop.
  endmethod.
*----------------------------------------------------------------------
* Method: show_report
*----------------------------------------------------------------------
  method show_report.
    call method: me->build_report
               , me->present_report
               .
  endmethod.
endclass. " report implementation
*======================================================================
*
*   S c r e e n   C o m p o n e n t s
*
*======================================================================
*----------------------------------------------------------------------
* Selection screen definition
*----------------------------------------------------------------------
selection-screen : begin of block block_a with frame.
parameters       :   pplate       type vehicle=>license_plate_type
                 ,   pbrand       type vehicle=>brand_type
                 ,   pmodel       type vehicle=>model_type
                 ,   pyear        type vehicle=>year_type
                 ,   pcolor       type vehicle=>color_type
                 ,   plocatn      type vehicle=>location_type
                 ,   pheading     type simple_navigation=>heading_type
                 ,   pturn01      type simple_navigation=>turn_type
                 ,   pturn02      type simple_navigation=>turn_type
                 ,   pturn03      type simple_navigation=>turn_type
                 ,   pspeedu      type vehicle=>speed_unit_type
                 ,   pspeed01     type vehicle=>speed_type
                 ,   pspeed02     type vehicle=>speed_type
                 ,   pspeed03     type vehicle=>speed_type
                 ,   pwghtu       type vehicle=>weight_unit_type
                 ,   pevw         type vehicle=>weight_type
                 ,   pcargow      type vehicle=>weight_type
                 ,   ppsngrs      type car=>passengers_type
                 ,   xbnav        radiobutton group nav
                 ,   xgps         radiobutton group nav
                 ,   xnonav       radiobutton group nav
                 .
selection-screen : end   of block block_a.
*----------------------------------------------------------------------
* Global data definitions
*----------------------------------------------------------------------
data             : report         type ref
                                    to report
                 .
*======================================================================
*
*   C l a s s i c   P r o c e d u r a l   E v e n t s
*
*======================================================================
*----------------------------------------------------------------------
* Event: initialization
*----------------------------------------------------------------------
initialization.
  set pf-status report=>selection_screen_status_name.
  call method report=>get_singleton_instance
    importing
      instance                    = report.
*----------------------------------------------------------------------
* Event: at selection-screen
*----------------------------------------------------------------------
at selection-screen.
  check sy-ucomm eq report=>execute
     or sy-ucomm eq report=>add_new_car
     or sy-ucomm eq report=>add_new_truck.
  case sy-ucomm.
    when report=>add_new_car.
      call method report->register_car_entry
        exporting
          license_plate           = pplate
          brand                   = pbrand
          model                   = pmodel
          year                    = pyear
          color                   = pcolor
          location                = plocatn
          heading                 = pheading
          turn01                  = pturn01
          turn02                  = pturn02
          turn03                  = pturn03
          speed01                 = pspeed01
          speed02                 = pspeed02
          speed03                 = pspeed03
          speed_unit              = pspeedu
          tare_weight             = pevw
          weight_unit             = pwghtu
          passengers              = ppsngrs
          basic_navigation        = xbnav
          gps_navigation          = xgps
          no_navigation           = xnonav
          .
*     This implicitly returns to the initial selection screen.
    when report=>add_new_truck.
      call method report->register_truck_entry
        exporting
          license_plate           = pplate
          brand                   = pbrand
          model                   = pmodel
          year                    = pyear
          color                   = pcolor
          location                = plocatn
          heading                 = pheading
          turn01                  = pturn01
          turn02                  = pturn02
          turn03                  = pturn03
          speed01                 = pspeed01
          speed02                 = pspeed02
          speed03                 = pspeed03
          speed_unit              = pspeedu
          tare_weight             = pevw
          weight_unit             = pwghtu
          cargo_weight            = pcargow
          basic_navigation        = xbnav
          gps_navigation          = xgps
          no_navigation           = xnonav
          .
*     This implicitly returns to the initial selection screen.
    when others.
*     No action; Execute will trigger start-of-selection event.
  endcase.
*----------------------------------------------------------------------
* Event: start-of-selection
*----------------------------------------------------------------------
start-of-selection.
*----------------------------------------------------------------------
* Event: end-of-selection
*----------------------------------------------------------------------
end-of-selection.
  call method report->show_report.
