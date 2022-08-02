*&---------------------------------------------------------------------*
*& Report ZOOT101D
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZOOT101D.

*----------------------------------------------------------------------
*  Title:
*    Objects 101: Car report using two local static classes
*  Selection text values:
*    PBRAND   - Brand
*    PCOLOR   - Color
*    PHEADING - Starting compass heading
*    PLOCATN  - Starting location
*    PMODEL   - Model
*    PPLATE   - License plate
*    PSPEEDU  - Speed unit
*    PSPEED01 - Sequence of speed increments
*    PSPEED02 - (none)
*    PSPEED03 - (none)
*    PTURN01  - Sequence of turns
*    PTURN02  - (none)
*    PTURN03  - (none)
*    PYEAR    - Year
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
*   (none)
*
* Differences with preceding version:
*   Local class "navigator" has been extracted from local class "car",
*   encapsulating attributes and methods relevant to navigation.  As
*   with car in the preceding version, the program can accommodate
*   only one navigation unit for the car, so all attributes and methods
*   of the "navigator" class are defined as static (class-method,
*   class-data), eliminating the need to create an instance of a
*   "navigator" class.  Visibility for a class component is marked as
*   restrictive as possible and only as visible as necessary.
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
*   C l a s s   D e f i n i t i o n s
*
*======================================================================
*----------------------------------------------------------------------
*
* Class: navigator definition
*
*----------------------------------------------------------------------
class navigator definition abstract final create private.
  public section.
    types        : turn_type      type char1
                 , heading_type   type char1
                 .
    constants    : left_turn      type navigator=>turn_type
                                                 value 'L'
                 , right_turn     type navigator=>turn_type
                                                 value 'R'
                 , u_turn         type navigator=>turn_type
                                                 value 'U'
                 .
    class-methods: change_heading
                     importing
                       turn
                         type navigator=>turn_type
                 , get_heading
                     exporting
                       heading
                         type navigator=>heading_type
                 , set_heading
                     importing
                       heading
                         type navigator=>heading_type
                 .
* protected section.
  private section.
    constants    : compass        type char4     value 'NESW'
                 , compass_offset_limit_lo
                                  type int4      value 00
                 , compass_offset_limit_hi
                                  type int4      value 03
                 .
    class-data   : heading        type navigator=>heading_type
                 .
endclass. " navigator definition
*----------------------------------------------------------------------
*
* Class: car definition
*
*----------------------------------------------------------------------
class car definition abstract final create private.
  public section.
    types        : brand_type     type f4txt
                 , color_type     type f4txt
                 , location_type  type f4txt
                 , model_type     type f4txt
                 , license_plate_type
                                  type f4txt
                 , speed_type     type int4
                 , speed_unit_type
                                  type char3
                 , year_type      type num4
                 .
    class-methods: accelerate
                     importing
                       acceleration
                         type car=>speed_type
                 , change_heading
                     importing
                       turn
                         type navigator=>turn_type
                 , get_characteristics
                     exporting
                       license_plate
                         type car=>license_plate_type
                       brand
                         type car=>brand_type
                       model
                         type car=>model_type
                       year
                         type car=>year_type
                       color
                         type car=>color_type
                       location
                         type car=>location_type
                       speed_unit
                         type car=>speed_unit_type
                 , get_heading
                     exporting
                       heading
                         type navigator=>heading_type
                 , get_speed
                     exporting
                       speed
                         type car=>speed_type
                 , set_characteristics
                     importing
                       license_plate
                         type car=>license_plate_type
                       brand
                         type car=>brand_type
                       model
                         type car=>model_type
                       year
                         type car=>year_type
                       color
                         type car=>color_type
                       location
                         type car=>location_type
                       speed_unit
                         type car=>speed_unit_type
                 , set_heading
                     importing
                       heading
                         type navigator=>heading_type
                 .
* protected section.
  private section.
    class-data   : license_plate  type car=>license_plate_type
                 , brand          type car=>brand_type
                 , model          type car=>model_type
                 , year           type car=>year_type
                 , color          type car=>color_type
                 , location       type car=>location_type
                 , speed          type car=>speed_type
                 , speed_unit     type car=>speed_unit_type
                 .
endclass. " car definition
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
    check turn eq navigator=>left_turn
       or turn eq navigator=>right_turn
       or turn eq navigator=>u_turn.
*   Convert current heading to string offset:
    find navigator=>heading in navigator=>compass match offset compass_offset.
*   Adjust string offset based on turn; A left turn decrements string
*   offset by 1; A right turn increments it by 1; A U-turn increments it
*   by 2:
    case turn.
      when navigator=>left_turn.
        subtract 01 from compass_offset.
      when navigator=>right_turn.
        add      01 to   compass_offset.
      when navigator=>u_turn.
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
    navigator=>heading            = navigator=>compass+compass_offset(01).
  endmethod.
*----------------------------------------------------------------------
* Method: get_heading
*----------------------------------------------------------------------
  method get_heading.
    heading                       = navigator=>heading.
  endmethod.
*----------------------------------------------------------------------
* Method: set_heading
*----------------------------------------------------------------------
  method set_heading.
*   Any heading value other than a valid navigator compass heading will
*   default to the first navigator compass heading:
    if navigator=>compass ca heading.
      navigator=>heading          = heading.
    else.
      navigator=>heading          = navigator=>compass+00(01).
    endif.
  endmethod.
endclass. " navigator implementation
*----------------------------------------------------------------------
*
* Class: car implementation
*
*----------------------------------------------------------------------
class car implementation.
*----------------------------------------------------------------------
* Method: accelerate
*----------------------------------------------------------------------
  method accelerate.
    add acceleration              to car=>speed.
  endmethod.
*----------------------------------------------------------------------
* Method: change_heading
*----------------------------------------------------------------------
  method change_heading.
    call method navigator=>change_heading
      exporting
        turn                      = turn.
  endmethod.
*----------------------------------------------------------------------
* Method: get_characteristics
*----------------------------------------------------------------------
  method get_characteristics.
    license_plate                 = car=>license_plate.
    brand                         = car=>brand.
    model                         = car=>model.
    year                          = car=>year.
    color                         = car=>color.
    location                      = car=>location.
    speed_unit                    = car=>speed_unit.
  endmethod.
*----------------------------------------------------------------------
* Method: get_heading
*----------------------------------------------------------------------
  method get_heading.
    call method navigator=>get_heading
      importing
        heading                   = heading
        .
  endmethod.
*----------------------------------------------------------------------
* Method: get_speed
*----------------------------------------------------------------------
  method get_speed.
    speed                         = car=>speed.
  endmethod.
*----------------------------------------------------------------------
* Method: set_characteristics
*----------------------------------------------------------------------
  method set_characteristics.
    car=>license_plate            = license_plate.
    car=>brand                    = brand.
    car=>model                    = model.
    car=>year                     = year.
    car=>color                    = color.
    car=>location                 = location.
    car=>speed_unit               = speed_unit.
  endmethod.
*----------------------------------------------------------------------
* Method: set_heading
*----------------------------------------------------------------------
  method set_heading.
    call method navigator=>set_heading
      exporting
        heading                   = heading
        .
  endmethod.
endclass. " car implementation
*======================================================================
*
*   C l a s s i c   P r o c e d u r a l   D a t a   D e f i n i t i o n s
*
*======================================================================
    types        : begin of output_row
                 ,   license_plate
                                  type car=>license_plate_type
                 ,   brand        type car=>brand_type
                 ,   model        type car=>model_type
                 ,   year         type car=>year_type
                 ,   color        type car=>color_type
                 ,   location     type car=>location_type
                 ,   heading      type navigator=>heading_type
                 ,   speed        type car=>speed_type
                 ,   speed_unit   type car=>speed_unit_type
                 , end   of output_row
                 , output_list    type standard table
                                    of output_row
                 .
    constants    : column_name_license_plate
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
                                  type string    value `Unit`
                 , minimum_column_width
                                  type int4      value 08
                 , execute        type syucomm   value 'ONLI'
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
                 , output_stack   type output_list
                 , output_entry   like line
                                    of output_stack
                 , alv_grid       type ref
                                    to cl_salv_table
                 .
*======================================================================
*
*   S c r e e n   C o m p o n e n t s
*
*======================================================================
*----------------------------------------------------------------------
* Selection screen definition
*----------------------------------------------------------------------
selection-screen : begin of block block_a with frame.
parameters       :   pplate       type car=>license_plate_type
                 ,   pbrand       type car=>brand_type
                 ,   pmodel       type car=>model_type
                 ,   pyear        type car=>year_type
                 ,   pcolor       type car=>color_type
                 ,   plocatn      type car=>location_type
                 ,   pheading     type navigator=>heading_type
                 ,   pturn01      type navigator=>turn_type
                 ,   pturn02      type navigator=>turn_type
                 ,   pturn03      type navigator=>turn_type
                 ,   pspeedu      type car=>speed_unit_type
                 ,   pspeed01     type car=>speed_type
                 ,   pspeed02     type car=>speed_type
                 ,   pspeed03     type car=>speed_type
                 .
selection-screen : end   of block block_a.
*======================================================================
*
*   C l a s s i c   P r o c e d u r a l   E v e n t s
*
*======================================================================
*----------------------------------------------------------------------
* Event: at selection-screen
*----------------------------------------------------------------------
at selection-screen.
  check sy-ucomm eq execute.
  perform register_car_entry using pplate
                                   pbrand
                                   pmodel
                                   pyear
                                   pcolor
                                   plocatn
                                   pspeedu
                                   pheading
                                   pspeed01
                                   pspeed02
                                   pspeed03
                                   pturn01
                                   pturn02
                                   pturn03
                                   .
*----------------------------------------------------------------------
* Event: start-of-selection
*----------------------------------------------------------------------
start-of-selection.
*----------------------------------------------------------------------
* Event: end-of-selection
*----------------------------------------------------------------------
end-of-selection.
  perform show_report.
*======================================================================
*
*   C l a s s i c   P r o c e d u r a l   S u b r o u t i n e s
*
*======================================================================
*----------------------------------------------------------------------
* Subroutine: build_report
*----------------------------------------------------------------------
form build_report.
* Create output entries:
  call method car=>get_characteristics
    importing
      license_plate               = output_entry-license_plate
      brand                       = output_entry-brand
      model                       = output_entry-model
      year                        = output_entry-year
      color                       = output_entry-color
      location                    = output_entry-location
      speed_unit                  = output_entry-speed_unit
      .
  call method car=>get_heading
    importing
      heading                     = output_entry-heading
      .
  call method car=>get_speed
    importing
      speed                       = output_entry-speed
      .
  append output_entry
      to output_stack.
endform.
*----------------------------------------------------------------------
* Subroutine: present_report
*----------------------------------------------------------------------
form present_report.
* Create alv grid:
  try.
    call method cl_salv_table=>factory
      importing
        r_salv_table              = alv_grid
      changing
        t_table                   = output_stack
        .
  catch cx_salv_msg.
    message e398(00) with 'Failure to create alv grid object'    "#EC *
                          space
                          space
                          space
                          .
  endtry.
* Set column titles:
  perform set_column_titles.
* Display alv grid:
  call method alv_grid->display.
endform.
*----------------------------------------------------------------------
* Subroutine: set_column_titles
*----------------------------------------------------------------------
form set_column_titles.
* Set alv grid column titles:
  call method alv_grid->get_columns
    receiving
      value                       = grid_columns.
  call method grid_columns->get
    receiving
      value                       = grid_column_stack.
  loop at grid_column_stack
     into grid_column_entry.
    clear grid_column_width.
    case grid_column_entry-columnname.
      when column_name_license_plate.
        grid_column_title_short   = column_title_license_plate.
      when column_name_brand.
        grid_column_title_short   = column_title_brand.
      when column_name_model.
        grid_column_title_short   = column_title_model.
      when column_name_year.
        grid_column_title_short   = column_title_year.
      when column_name_color.
        grid_column_title_short   = column_title_color.
      when column_name_location.
        grid_column_title_short   = column_title_location.
      when column_name_heading.
        grid_column_title_short   = column_title_heading.
        grid_column_width         = minimum_column_width.
      when column_name_speed.
        grid_column_title_short   = column_title_speed.
        grid_column_width         = minimum_column_width.
      when column_name_speed_unit.
        grid_column_title_short   = column_title_speed_unit.
        grid_column_width         = minimum_column_width.
      when others.
        clear grid_column_title_short.
    endcase.
    call method grid_column_entry-r_column->set_short_text
      exporting
        value                     = grid_column_title_short.
    if grid_column_width gt 00.
      call method grid_column_entry-r_column->set_output_length
        exporting
          value                   = grid_column_width.
    endif.
  endloop.
endform.
*----------------------------------------------------------------------
* Subroutine: show_report
*----------------------------------------------------------------------
form show_report.
  perform: build_report
         , present_report
         .
endform.
*----------------------------------------------------------------------
* Subroutine: register_car_entry
*----------------------------------------------------------------------
form register_car_entry using license_plate
                              brand
                              model
                              year
                              color
                              location
                              speed_unit
                              heading
                              speed01
                              speed02
                              speed03
                              turn01
                              turn02
                              turn03
                              .
  call method car=>set_characteristics
    exporting
      license_plate               = license_plate
      brand                       = brand
      model                       = model
      year                        = year
      color                       = color
      location                    = location
      speed_unit                  = speed_unit
      .
  call method car=>set_heading
    exporting
      heading                     = heading
      .
  call method car=>accelerate
    exporting
      : acceleration              = speed01
      , acceleration              = speed02
      , acceleration              = speed03
      .
  call method car=>change_heading
    exporting
      : turn                      = turn01
      , turn                      = turn02
      , turn                      = turn03
      .
endform.
