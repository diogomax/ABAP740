
```
" Teste
DATA( lt_materials_fert ) = FILTER #( lt_all_materials USING KEY mtart WHERE mtart = 'FERT' ).
```

```
IF line_exists( vendors[ id = '00AED' ] ).
  vendors[ id = '00AED' ].
ENDIF.
```

```
wa = itab[ KEY key INDEX idx ].
```

```
DATA(idx) = line_index( itab[ … ] ).
```

```
wa = itab[ col1 = … col2 = … ].
wa = itab[ KEY key col1 = … col2 = … ].
wa = itab[ KEY key COMPONENTS col1 = … col2 = … ].
```

```
DATA itab TYPE RANGE OF i.
itab = VALUE #( sign = 'I'  option = 'BT' 
( low = 1  high = 10 ) 
( low = 21 high = 30 ) 
( low = 41 high = 50 ) 
option = ‘GE’ ( low = 61 )  ).
```

```
""""""""" FOR 
DATA(gt_citys) = VALUE ty_citys( FOR ls_ship IN gt_ships ( ls_ship–city ) ).
DATA(lt_cc_guid) = VALUE crmt_object_guid_tab( FOR ls_cc_rel IN lt_cc_rel ( CONV #( ls_cc_rel-zzheader_guid ) ) ).
```

```
DATA(lt_delete_survey) = VALUE crmt_survey_du_tab(
                             FOR ls_srv_guid IN lt_survey_com
                               ( survey_guid = ls_srv_guid-survey_guid )
                             ).
```

```
DATA(lt_selection) = VALUE usmdz5_t_bapi6200sl(
  FOR ls_selopt IN shlp-selopt[]
  ( infoobject = ls_selopt-shlpfield
    sign       = ls_selopt-sign
    option     = ls_selopt-option
    low        = ls_selopt-low
    high       = ls_selopt-high )
).
```

```
rt_tc_param =
  VALUE ztr_t_param(
    FOR ls_prex IN lt_all_prex
      LET key = zif
          name = gasse
          name2 = lv_parname2
          IN parsfakey = key
             parname = name
             parname2 = name2
    ( parname3 = ls_prex-zzprefix_id
      parval1  = ls_prex-zzprefix_id )
  ).
```

```
DATA lt_serv_range    TYPE RANGE OF ztr_t_srn_i_srv-parent_id.
  lt_serv_range = VALUE #(
    FOR ls_serv_aux IN lt_serv_netz_aux (
      sign   = 'I'
      option = 'EQ'
      low    = ls_serv_aux )
  ).
```

```
  et_serv = VALUE ztr_t_srn_i_srv_t(
              FOR ls_serv IN lt_serv_netz
                WHERE ( parent_id IN lt_serv_range )
              ( ls_serv )
            ).
```

```		
DATA(lt_guids) = VALUE crmt_object_guid_tab(
  FOR ls_doc_flow IN lt_doc_flow
  ( ls_doc_flow-objkey_a )
).

lt_guids = VALUE #(
  BASE lt_guids
  FOR ls_doc_flow IN lt_doc_flow
  ( ls_doc_flow-objkey_b )
).

lt_new_flights =
  VALUE #(
    FOR ls_scarr in lt_scarr
    FOR ls_flight IN lt_flights WHERE ( carrid = ls_scarr-carrid )
    (
      carrier = ls_scarr-carrname
      connect = ls_flight-connid
      fldate  = ls_flight-fldate
    )
  ).
```

```
    DATA(lv_rfcdest) = COND char32( WHEN sy-sysid = 'ZZE' THEN 'DZRCLTN100'
                                    WHEN sy-sysid = 'ZZF' THEN 'TZECLTN100'
                                    ELSE ''  ).
									
    TYPES: BEGIN OF ty_test_prod,                                               
             ordered_prod TYPE crmd_orderadm_i-ordered_prod,                    
           END OF ty_test_prod.                                                 
    DATA lt_test_prod TYPE TABLE OF ty_test_prod.                               
    lt_test_prod = VALUE #( ( ordered_prod = 'CONFERENCE' )              
                            ( ordered_prod = 'TELWEB5' )              
                            ( ordered_prod = 'TELWEB15' )             
                            ( ordered_prod = 'TELWEB50' ) ).           
									
	ls_item-product_id = COND #( WHEN line_exists( lt_test_prod[ sy-tabix ] )
								 THEN lt_test_prod[ sy-tabix ]
                                 ELSE lt_test_prod[ 2 ] ).

```

```
DATA(lv_lines) = REDUCE i( INIT x = 0 FOR wa IN gt_itab WHERE( F1 = ‘XYZ’ ) NEXT x = x + 1 ).
```

```
DATA(text) =
NEW class( )->meth(
	 SWITCH #( sy-langu
			  WHEN 'D' THEN 'DE'
			  WHEN 'E' THEN 'EN'
			   ELSE THROW cx_langu_not_supported( ) ) ).
```

```
MOVE-CORRESPONDING ls_line1 TO ls_line2.
ls_line2 = CORRESPONDING #( BASE ( ls_line2 ) ls_line1 ).
```
```
*ALPHA = IN|OUT|RAW|(val)]
*ld_message = |{ ld_delivery_number ALPHA = OUT }|.
*    [WIDTH     = len]
*    [ALIGN     = LEFT|RIGHT|CENTER|(val)]
*    [PAD       = c]
*    [CASE      = RAW|UPPER|LOWER|(val)]
*    [SIGN      = LEFT|LEFTPLUS|LEFTSPACE|RIGHT|RIGHTPLUS|RIGHTSPACE|(val)]
*    [EXPONENT  = exp]
*    [DECIMALS  = dec]
*    [ZERO      = YES|NO|(val)]
*    [XSD       = YES|NO|(val)]
*    [STYLE     =  SIMPLE|SIGN_AS_POSTFIX|SCALE_PRESERVING
*                 |SCIENTIFIC|SCIENTIFIC_WITH_LEADING_ZERO
*                 |SCALE_PRESERVING_SCIENTIFIC|ENGINEERING
*                 |(val)]
*    [CURRENCY  = cur]
*    [NUMBER    = RAW|USER|ENVIRONMENT|(val)]
*    [DATE      = RAW|ISO|USER|ENVIRONMENT|(val)]
*    [TIME      = RAW|ISO|USER|ENVIRONMENT|(val)]
*    [TIMESTAMP = SPACE|ISO|USER|ENVIRONMENT|(val)]
*    [TIMEZONE  = tz]
*    [COUNTRY   = cty]
	
SY-ZONLO = CET
```


```
DATA flights TYPE TABLE OF spfli WITH EMPTY KEY.

SELECT * FROM  spfli
         WHERE carrid = ‘…’
         INTO TABLE @flights.

DATA members LIKE flights.
LOOP AT flights INTO DATA(flight)
     GROUP BY ( carrier = flight-carrid cityfr = flight-cityfrom )
              ASCENDING
              ASSIGNING FIELD-SYMBOL(<group>).
  CLEAR members.
  LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<flight>).
    members = VALUE #( BASE members ( <flight> ) ).
  ENDLOOP.
  cl_demo_output=>write( members ).
ENDLOOP.
cl_demo_output=>display( ).
```
```
LOOP AT gt_items ASSIGNING FIELD-SYMBOL(<fs_item>)
     GROUP BY ( contract_start = <fs_item>-contract_start )
              ASCENDING
              ASSIGNING FIELD-SYMBOL(<fs_group>).
  LOOP AT GROUP <fs_group> ASSIGNING FIELD-SYMBOL(<fs_item_grp>).
    lt_items = VALUE #( BASE lt_items ( <fs_item_grp> ) ).
  ENDLOOP.
  IF <fs_main_item>-contract_start = <fs_group>-contract_start.
    CONTINUE.
  ENDIF.
  me->change_contract( it_items = lt_items ).
ENDLOOP.
```
```
DATA(row_with_max_snocoun) =
  REDUCE ls_student1(
    INIT max = VALUE #( )
    FOR row IN lt_table4
    NEXT max = COND #(
      WHEN row-snocount > max-snocount
      THEN row
      ELSE max ) ).


     " In case that the value is a decimal numeric value,
     " no value-text is maintained in the model.
     " So, copy the value to value-text, delete the ".0"
     " https://sapintegrationhub.com/abap/check-if-the-field-content-is-numeric/
     " if the solution is to simple, maybe use this in the future:
     " https://www.toolbox.com/tech/enterprise-software/question/looking-for-is-num-in-abap-070413/
     IF ( ls_cstic-value_txt IS INITIAL ) AND ( ls_cstic-value CO ' 1234567890.' ).
        DATA(lv_cstic_val) = ls_cstic-value.
        lv_cstic_val = replace( val   = lv_cstic_val
                                regex = '\.0'
                                with  = ''
                                occ   =   1 ).
        ls_cstic-value_txt = lv_cstic_val.
     ELSEIF ( ls_cstic-value_txt IS INITIAL ).
        ls_cstic-value_txt = ls_cstic-value.
     ENDIF.
```
```
" Factorial
    fact = COND #( WHEN n < 0
                     THEN 0
                   ELSE
                     REDUCE #(
                       INIT f = 1
                       FOR  i = 1 UNTIL i > n
                       NEXT f = f * i ) ).
```
``` 
	it_sap = VALUE #(
		BASE it_sap
		( LINES OF VALUE #( FOR <line> IN
								FILTER #( it_archive EXCEPT IN it_sap
										WHERE sokey  = sokey
										  AND sopono = sopono
										  AND socono = socono )
							( VALUE #( BASE <line> flag = 'D' ) ) ) ) ).
	ASSERT it_sap = VALUE lty_file_tab(
		( sokey = 'Key1' sopono = 'PO#12' socono = 'Three' flag = 'A' )
		( sokey = 'Key2' sopono = 'PO#34' socono = 'Four'  flag = 'D' ) ).
```
* Concatenate from table
lw_output_h-sl_text = REDUCE #( INIT text = `` FOR <line> IN lt_tline NEXT text = text && <line>-tdline ).
```
```
        " |<STD_PL_PREFIX>-<MAIN_PRODUCT_ID>-<LEISTUNGSMERKMAL_ID>|<EFF_INTERVAL>|<SCALE_PRICE_ID>|
        lv_context = |\|{ gv_std_pl }-{ gs_hp-ordered_prod }-{ gs_lm-ordered_prod }\|{ gs_lm-da_frequency }\|{ gv_scale_id }\||.
```
