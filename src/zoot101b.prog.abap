*&---------------------------------------------------------------------*
*& Report ZOOT101B
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZOOT101B.

*----------------------------------------------------------------------
*  Title:
*    Objects 101: Car report using no local classes, step 2
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
*   This version prepares the program for transformation to using objects.
*   Here we have removed the processing as much as possible from the
*   classic event blocks and removed any references to screen fields from
*   the subroutines.  Accordingly, we replaced the multiple perform
*   statements in the "at selection-screen" event block with a single
*   perform statement to a newly defined subroutine "register_car_entry",
*   moved the multiple perform statements into this new subroutine, and
*   pass to it the screen fields used by the multiple perform statements.
*   Also, we changed subroutine "set_characteristics" to accept parameters.
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
*   C l a s s i c   P r o c e d u r a l   D a t a   D e f i n i t i o n s
*
*======================================================================
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
                 , turn_type      type char1
                 , heading_type   type char1
                 , begin of output_row
                 ,   license_plate
                                  type license_plate_type
                 ,   brand        type brand_type
                 ,   model        type model_type
                 ,   year         type year_type
                 ,   color        type color_type
                 ,   location     type location_type
                 ,   heading      type heading_type
                 ,   speed        type speed_type
                 ,   speed_unit   type speed_unit_type
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
                 , left_turn      type char1     value 'L'
                 , right_turn     type char1     value 'R'
                 , u_turn         type char1     value 'U'
                 , compass        type char4     value 'NESW'
                 , compass_offset_limit_lo
                                  type int4      value 00
                 , compass_offset_limit_hi
                                  type int4      value 03
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
                 , license_plate  type license_plate_type
                 , brand          type brand_type
                 , model          type model_type
                 , year           type year_type
                 , color          type color_type
                 , location       type location_type
                 , heading        type heading_type
                 , speed          type speed_type
                 , speed_unit     type speed_unit_type
                 , compass_offset type int4
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
parameters       :   pplate       type license_plate_type
                 ,   pbrand       type brand_type
                 ,   pmodel       type model_type
                 ,   pyear        type year_type
                 ,   pcolor       type color_type
                 ,   plocatn      type location_type
                 ,   pheading     type heading_type
                 ,   pturn01      type turn_type
                 ,   pturn02      type turn_type
                 ,   pturn03      type turn_type
                 ,   pspeedu      type speed_unit_type
                 ,   pspeed01     type speed_type
                 ,   pspeed02     type speed_type
                 ,   pspeed03     type speed_type
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
* Subroutine: accelerate
*----------------------------------------------------------------------
form accelerate using acceleration
                                  type speed_type.
  add acceleration                to speed.
endform.
*----------------------------------------------------------------------
* Subroutine: change_heading
*----------------------------------------------------------------------
form change_heading using turn    type turn_type.
  check turn eq left_turn
     or turn eq right_turn
     or turn eq u_turn.
* Convert current heading to string offset:
  find heading in compass match offset compass_offset.
* Adjust string offset based on turn; A left turn decrements string
* offset by 1; A right turn increments it by 1; A U-turn increments it
* by 2:
  case turn.
    when left_turn.
      subtract 01 from compass_offset.
    when right_turn.
      add      01 to   compass_offset.
    when u_turn.
      add      02 to   compass_offset.
  endcase.
* Adjust numeric value to accommodate underflow or overflow:
  if compass_offset lt compass_offset_limit_lo.
    add      04 to   compass_offset.
  endif.
  if compass_offset gt compass_offset_limit_hi.
    subtract 04 from compass_offset.
  endif.
* Reset heading:
  heading                         = compass+compass_offset(01).
endform.
*----------------------------------------------------------------------
* Subroutine: set_characteristics
*----------------------------------------------------------------------
form set_characteristics using ulicense_plate
                               ubrand
                               umodel
                               uyear
                               ucolor
                               ulocation
                               uspeed_unit
                               .
  license_plate                   = ulicense_plate.
  brand                           = ubrand.
  model                           = umodel.
  year                            = uyear.
  color                           = ucolor.
  location                        = ulocation.
  speed_unit                      = uspeed_unit.
endform.
*----------------------------------------------------------------------
* Subroutine: set_heading
*----------------------------------------------------------------------
form set_heading using start_heading
                                  type heading_type.
  if compass ca start_heading.
    heading                       = start_heading.
  else.
    heading                       = compass+00(01).
  endif.
endform.
*----------------------------------------------------------------------
* Subroutine: build_report
*----------------------------------------------------------------------
form build_report.
* Create output entries:
  output_entry-license_plate      = license_plate.
  output_entry-brand              = brand.
  output_entry-model              = model.
  output_entry-year               = year.
  output_entry-color              = color.
  output_entry-location           = location.
  output_entry-heading            = heading.
  output_entry-speed              = speed.
  output_entry-speed_unit         = speed_unit.
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
  perform set_characteristics using license_plate
                                    brand
                                    model
                                    year
                                    color
                                    location
                                    speed_unit
                                    .
  perform set_heading using heading.
  perform accelerate using: speed01
                          , speed02
                          , speed03
                          .
  perform change_heading using: turn01
                              , turn02
                              , turn03
                              .
endform.
