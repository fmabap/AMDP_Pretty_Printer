 CLASS zcl_app_test_amdp DEFINITION
   PUBLIC
   FINAL
   CREATE PUBLIC .

   PUBLIC SECTION.

   INTERFACES if_amdp_marker_hdb.

   CLASS-METHODS sel_data.

   CLASS-METHODS write_data.

   PROTECTED SECTION.
   PRIVATE SECTION.
 ENDCLASS.




 CLASS zcl_app_test_amdp IMPLEMENTATION.

   METHOD write_data.
   "Hello World 'keine Ahnung' 'bla
   WRITE / 'Hello World'. "Kommentar
   ENDMETHOD.

   METHOD sel_data
   BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT
   "opt
* sqlscript
   OPTIONS READ-ONLY
   USING sflight.
  -- Flight
  declare lv_bla nvarchar(4);
  lv_bla = 'BlA';                        ---Blub

  call "CLASS=>PROCESS"(
       iv_capid     => :IV_CAPID,              --Blub
    iv_base_step     =>     :IV_BASE_STEP,
       cv_operation_id =>     cv_operation_id
          );

   lt_carrid2 = select "SFLIGHT"."CARRID" from sflight;

   lt_sflight = select * from sflight;

   lt_carrid = select "CARRID" from sflight;

   lt_carrid2 = select "SFLIGHT"."CARRID" from sflight;
  lt_carrid3 = select max( concat( "SFLIGHT"."CARRID", connid ) )from sflight;
 lt_carrid4 = select max( ( concat( sflight, connid ) ) )from sflight;
 lt_carrid4 = select max( ( concat( sflight, connid )))from sflight;
 lt_carrid5 = select concat( sflight, connid ) from sflight;
 lt_carrid5 = select concat( sflight, connid ) from sflight with hint ( NO_INLINE );
 lt_carrid5 = select concat( sflight, connid ) from sflight with hint ( NO_INLINE
  );
 lt_carrid5 = select concat( sflight, connid ) from sflight with hint ( NO_INLINE
  ) ;
 lt_carrid5 = select concat( sflight, connid ) from sflight;

*lt_test = select 'Bla Blup Data' from public.dummy;
   lt_test = select 'Bla Blup Data' from public.dummy;

   -- Hello World 'keine Ahnung' 'bla blub'
   lt_test2 = select 'Bla Blup Data' from public.dummy; /*schaun mer mal */

   /* schaun mer
   noch mehr */

   lt_test3 = select 'Bla Blup Data' from public.dummy; /*schaun mer mal */

   lt_test3 = select 'Bla Blup Data' from public.dummy; -- schaun mer mal

 et_exp_part = select :bal,
                :LT_EXP_PART."/BLA/SAP_OBJ_KEY_REF",
                :LT_EXP_PART."/BLA/SAP_OBJ_VRS_REF"
             from :LT_EXP_PART
           inner join "/BLA/30_IC_MAT_TYP_SNR_USE" as snrclsp
               on snrclsp.mandt = session_context( 'CLIENT')
              and snrclsp.matnr = :LT_EXP_PART.matnr
           union  all
             select :LT_EXP_PART.run_no,
                :LT_EXP_PART."/BLA/SAP_OBJ_KEY_REF",
                :LT_EXP_PART."/BLA/SAP_OBJ_VRS_REF"
             from :LT_EXP_PART
            where not exists( select 1
                      from "/BLA/30_IC_MAT_TYP_SNR_USE" as snrclsp
                       where snrclsp.mandt = session_context( 'CLIENT')
                       and snrclsp.matnr = :LT_EXP_PART.matnr
                    );

  Lt_test4 =  select carrid, connid, fldate
   from sflight
   where sflight.mandt =  SESSION_CONTEXT('CLIENT')
   and sflight.connid = 'LH' union all
   select spfli.carrid, spfli.connid, sflight.FLDATE
   from spfli left outer join sflight
   on sflight.mandt = spfli.mandt
   and sflight.CARRID = spfli.carrid
   and sflight.CONNID = spfli.connid
   where spfli.mandt = SESSION_CONTEXT('CLIENT')
   and spfli.carrid = 'AB' union all
   select spfli.carrid, spfli.connid, sflight.FLDATE
   from spfli inner join sflight
   on sflight.mandt = spfli.mandt
   and sflight.CARRID = spfli.carrid
   and sflight.CONNID = spfli.connid
   where spfli.mandt = SESSION_CONTEXT('CLIENT')
   and spfli.carrid = 'AB';

 lt_spfli1 = SELECT carrid, connid, countryfr, countryto,
 ROW_NUMBER ( ) OVER( ORDER BY "CARRID", CONNID asc ) AS "ROW_ID"
 FROM SPFLI WHERE mandt = session_context( 'CLIENT' );


 lt_spfli2 = SELECT carrid, connid, countryfr, countryto,
 ROW_NUMBER ( ) OVER( PARTITION BY carrid, connid ORDER BY "CARRID", CONNID asc ) AS "ROW_ID"
 FROM SPFLI WHERE mandt = session_context( 'CLIENT' );
   --ENDMETHOD.
   ENDMETHOD.
 ENDCLASS.