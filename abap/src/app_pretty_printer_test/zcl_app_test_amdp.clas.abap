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
    "Hello World 'test1' 'test2
    WRITE / 'Hello World'.    "#EC CI_USE_WANTED "Comment

  ENDMETHOD.

  METHOD sel_data
  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT
  "opt
* sqlscript
  OPTIONS READ-ONLY
  USING spfli sflight. -- Flight
    declare lv_bla nvarchar(4);

    lt_sflight = select * from sflight;

    lt_carrid = select "CARRID" from sflight;

    lt_carrid2 = select "SFLIGHT"."CARRID" from sflight;

    lt_carrid2 = select DISTINCT "SFLIGHT"."CARRID" from sflight;

    lt_carrid3 = select max( concat( "SFLIGHT"."CARRID", connid ) )from sflight;

    lt_carrid4 = select max( case when carrid <> connid then connid else carrid end  )from sflight;

    lt_carrid5 = select max( case when carrid <> connid then
    connid
    when carrid > connid then
    carrid
    else carrid
    end
    ) as max_value
    from sflight
    ORDER BY max_value asc;

    if IS_EMPTY ( :lt_carrid5 ) then
    return;
    elseif is_empty ( :lt_carrid4 ) then
    return;
    else
    return;
    end if;

    lt_spfli1 = SELECT carrid, connid, countryfr, countryto,
    ROW_NUMBER ( ) OVER( ORDER BY "CARRID", CONNID asc ) AS "ROW_ID"
    FROM SPFLI WHERE mandt = session_context( 'CLIENT' );


    lt_spfli2 = SELECT carrid, connid, countryfr, countryto,
    ROW_NUMBER ( ) OVER( PARTITION BY carrid, connid ORDER BY "CARRID", CONNID asc ) AS "ROW_ID"
    FROM SPFLI WHERE mandt = session_context( 'CLIENT' );

*lt_test = select 'Bla Blup Data' from public.dummy;
    lt_test = select 'Bla Blup Data' from public.dummy;

    -- Hello World  'bla blub'
    lt_test2 = select 'Bla Blup Data' from public.dummy; /*test*/

    /* test1
    test2 */

    lt_test3 = select 'Bla Blup Data' from public.dummy; /*test */

    lt_test3 = select 'Bla Blup Data' from public.dummy; -- test

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

    lt_bla =  SELECT RIGHT( carrid,  4  )  FROM sflight;
    --ENDMETHOD.
  ENDMETHOD.
ENDCLASS.
