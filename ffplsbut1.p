/* *ffdepohcm.p - Mfg/Pro Rapor Formati                                    */
/*V8:ConvertMode=FullGUIReport                                             */
/* COPYRIGHT qad.inc. ALL RIGHTS RESERVED. THIS IS AN UNPUBLISHED WORK.    */

/*aycan github denemesi */
{mfdtitle.i "f "}
/*
DEF VAR ptsite LIKE pt_site INITIAL "ERMETAL" NO-UNDO.

DEF VAR tarih AS DATE INIT TODAY NO-UNDO.
DEF VAR parcaKodu AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR eskiYil AS INT NO-UNDO.
DEF VAR eskiTarih AS CHAR FORMAT "x(11)" NO-UNDO.
DEF VAR fiyat AS DEC NO-UNDO.
DEF VAR ciroTop AS DEC NO-UNDO.
DEF VAR ciroTop2 AS DEC NO-UNDO.
DEF VAR baslik AS CHAR FORMAT "x(30)" NO-UNDO.

*/

/*-----------------------------------------------------*/
DEF VAR tar1 AS DATE INIT 01/01/19.
DEF VAR fiyat AS DEC NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR a AS INT NO-UNDO.

DEF VAR ii AS INT NO-UNDO.
DEF VAR aa AS INT NO-UNDO.

DEF VAR xxx AS CHAR FORMAT "x(10)".
DEF VAR yyy AS CHAR FORMAT "x(10)".
DEF VAR zzz AS CHAR FORMAT "x(10)".

DEF VAR tanim AS CHAR FORMAT "x(48)".
DEF VAR toplam LIKE mrp_qty.

DEF VAR ey AS DEC INIT 0 EXTENT 12.
DEF VAR ef AS DEC INIT 0 EXTENT 12.
DEF VAR usd AS DEC INIT 0 EXTENT 12.
DEF VAR eur AS DEC INIT 0 EXTENT 12.
DEF VAR aylar AS CHAR EXTENT 12.


define new shared temp-table tmp1 no-undo
        field tmp1_part like mrp_part
        FIELD tmp1_nbr LIKE mrp_nbr
        field tmp1_ay as CHAR
        field tmp1_ay1 as DATE
        field tmp1_qty LIKE mrp_qty
        field tmp1_yil as CHAR
        FIELD tmp1_type AS CHAR FORMAT "x(36)"
        FIELD tmp1_stokyeri LIKE pt_loc
        FIELD tmp1_urungr AS CHAR FORMAT "x(18)"
        FIELD tmp1_pm AS CHAR
        FIELD tmp1_urungr1 AS CHAR FORMAT "x(18)"
        FIELD tmp1_detay AS CHAR FORMAT "x(18)"
        FIELD tmp1_songrup AS CHAR FORMAT "x(24)".

define new shared temp-table tmp2 no-undo
        FIELD tmp2_part like mrp_part
        FIELD tmp2_nbr LIKE mrp_nbr
        field tmp2_ay as CHAR
        field tmp2_ay1 as DATE
        field tmp2_qty LIKE mrp_qty
        field tmp2_yil as CHAR
        FIELD tmp2_type AS CHAR FORMAT "x(36)"
        FIELD tmp2_stokyeri LIKE pt_loc
        FIELD tmp2_urungr AS CHAR FORMAT "x(18)"
        FIELD tmp2_pm AS CHAR
        FIELD tmp2_urungr1 AS CHAR FORMAT "x(18)"
        FIELD tmp2_detay AS CHAR FORMAT "x(18)"
        FIELD tmp2_songrup AS CHAR FORMAT "x(24)".
/*
   DISP  tmp2_part LABEL "Parça Kodu" tanim FORMAT "x(80)" LABEL "Taným" tmp2_ay LABEL "Ay" tmp2_yil Label "Yýl" tmp2_qty LABEL "Miktar" FORMAT "->>>,>>>,>>9.99" "    "
          tmp2_urungr LABEL "Üretim Hattý" tmp2_pm LABEL "P/M" tmp2_stokyeri LABEL "Stok Yeri" tmp2_urungr1 LABEL "Ürün Grubu Tip" tmp2_songrup LABEL "Type" stnlcari LABEL "Son Satýnalma Cari"
          stnlfiy LABEL "Satýnalma Fiyatý" "    " stnlcurr LABEL "Satýnalma Kur" satcari LABEL "Son Satýþ Cari" "    " satfiy LABEL "Satýþ Fiyatý" "   " satcurr LABEL "Satýþ Kuru"
        WITH WIDTH 450.
*/

DEF TEMP-TABLE temp3
        FIELD temp3_part AS CHAR FORMAT "x(10)"
        FIELD temp3_tanim AS CHAR FORMAT "x(50)"
        FIELD temp3_yil AS CHAR
        FIELD temp3_ay AS CHAR
        FIELD temp3_ay1 AS DATE /*tarih ilgili ayýn il günü*/
        FIELD temp3_miktar AS DEC FORMAT "->>>,>>>,>>9.99"
        FIELD temp3_urungr AS CHAR /*ÜRETÝM HATTI*/
        FIELD temp3_urungr1 AS CHAR /*ÜRÜN GRUBU TÝP*/
        FIELD temp3_pm AS CHAR
        FIELD temp3_stokyeri AS CHAR
        FIELD temp3_songrup AS CHAR /*TYPE*/
        FIELD temp3_stnlcari AS CHAR 
        FIELD temp3_stnlfiy AS DEC /*SATINALMA FÝYATI*/
        FIELD temp3_stnlcurr AS CHAR FORMAT "x(18)" /*SATINALMA KUR*/
        FIELD temp3_satcari AS CHAR /*SON SATIÞ CARÝ*/
        FIELD temp3_satfiy AS DEC /*SATIÞ FÝYATI*/
        FIELD temp3_satcurr AS CHAR FORMAT "x(18)". /*SATIÞ KURU*/


DEF TEMP-TABLE temp4
        FIELD temp4_part AS CHAR FORMAT "x(10)"
        FIELD temp4_tanim AS CHAR FORMAT "x(30)"
        FIELD temp4_yil AS CHAR
        FIELD temp4_ay AS CHAR
        FIELD temp4_ay1 AS DATE /*tarih ilgili ayýn il günü*/
        FIELD temp4_stnlcari AS CHAR
        FIELD temp4_stnlfiy AS DEC /*SATINALMA FÝYATI*/
        FIELD temp4_stnlcurr AS CHAR FORMAT "x(18)" /*SATINALMA KUR*/
        FIELD temp4_urungr1 AS CHAR
        FIELD temp4_pm AS CHAR

        FIELD temp4_miktar AS DEC FORMAT "->>,>>>,>>9.99" EXTENT 12
        FIELD temp4_birfiyat AS DEC FORMAT "->>,>>>,>>9.99" EXTENT 12
        FIELD temp4_tutartl AS DEC FORMAT "->>,>>>,>>9.99" EXTENT 12.

/*
 ASSIGN temp4_part
                   temp4_tanim
                   temp4_yil
                   temp4_ay1
                   temp4_stnlcari /* satýnalma cari */
                   temp4_urungr /* üretim hattý */
                   temp4_urungr1 /* satýnalma imalat */
                   temp4_stnlfiy /* satýnalma birim fiyatý */
                   temp4_stnlcurr /* satýnalma kuru */                
    END.
                   temp4_miktar[MONTH(temp4_ay1)]   = temp4_miktar[MONTH(temp4_ay1)]  % temp4_miktar[MONTH(temp4_ay1)] + temp4_miktar.
                   temp4_birfiyat[MONTH(temp4_ay1)] = temp4_stnlfiy * ey[MONTH(temp4_ay1)].
                   temp4_tutartl[MONTH(temp4_ay1)]  = temp4_tutartl[MONTH(temp4_ay1)] + temp4_birfiyat[MONTH(temp4_ay1)] * temp4_miktar[MONTH(temp4_ay1)].
*/


/*------------------------------------------------------*/


/* KRÝTER GÝRÝÞ FORMU TANIMI */

FORM  	  
        tar1 COLON 25 LABEL "Baþlangýç"
        " "  COLON 25
        ey[1] LABEL "EY1" COLON 25 ef[1] LABEL "EF1" COLON 45 usd[1] LABEL "USD1" COLON 65 eur[1] LABEL "EUR1" COLON 85
        ey[2] LABEL "EY2" COLON 25 ef[2] LABEL "EF2" COLON 45 usd[2] LABEL "USD2" COLON 65 eur[2] LABEL "EUR2" COLON 85
        ey[3] LABEL "EY3" COLON 25 ef[3] LABEL "EF3" COLON 45 usd[3] LABEL "USD3" COLON 65 eur[3] LABEL "EUR3" COLON 85
        ey[4] LABEL "EY4" COLON 25 ef[4] LABEL "EF4" COLON 45 usd[4] LABEL "USD4" COLON 65 eur[4] LABEL "EUR4" COLON 85
        ey[5] LABEL "EY5" COLON 25 ef[5] LABEL "EF5" COLON 45 usd[5] LABEL "USD5" COLON 65 eur[5] LABEL "EUR5" COLON 85
        ey[6] LABEL "EY6" COLON 25 ef[6] LABEL "EF6" COLON 45 usd[6] LABEL "USD6" COLON 65 eur[6] LABEL "EUR6" COLON 85
        ey[7] LABEL "EY7" COLON 25 ef[7] LABEL "EF7" COLON 45 usd[7] LABEL "USD7" COLON 65 eur[7] LABEL "EUR7" COLON 85
        ey[8] LABEL "EY8" COLON 25 ef[8] LABEL "EF8" COLON 45 usd[8] LABEL "USD8" COLON 65 eur[8] LABEL "EUR8" COLON 85
        ey[9] LABEL "EY9" COLON 25 ef[9] LABEL "EF9" COLON 45 usd[9] LABEL "USD9" COLON 65 eur[9] LABEL "EUR9" COLON 85
        ey[10] LABEL "EY10" COLON 25 ef[10] LABEL "EF10" COLON 45 usd[10] LABEL "USD10" COLON 65 eur[10] LABEL "EUR10" COLON 85
        ey[11] LABEL "EY11" COLON 25 ef[11] LABEL "EF11" COLON 45 usd[11] LABEL "USD11" COLON 65 eur[11] LABEL "EUR11" COLON 85
        ey[12] LABEL "EY12" COLON 25 ef[12] LABEL "EF12" COLON 45 usd[12] LABEL "USD12" COLON 65 eur[12] LABEL "EUR12" COLON 85
        " " COLON 25
SKIP
with frame a side-labels width 100.

/* SET EXTERNAL LABELS */
setFrameLabels(frame a:handle).

setFrameLabels(frame a:handle).

form
with frame d down width 132.

/*find first icc_ctrl where icc_domain = global_domain no-lock.
ptsite = icc_site.*/

{wbrp01.i}
repeat:

    

if c-application-mode <> "WEB" then
    update
        tar1
        ey[1] ey[2] ey[3] ey[4] ey[5] ey[6] ey[7] ey[8] ey[9] ey[10] ey[11] ey[12]
        ef[1] ef[2] ef[3] ef[4] ef[5] ef[6] ef[7] ef[8] ef[9] ef[10] ef[11] ef[12]
        usd[1] usd[2] usd[3] usd[4] usd[5] usd[6] usd[7] usd[8] usd[9] usd[10] usd[11] usd[12]
        eur[1] eur[2] eur[3] eur[4] eur[5] eur[6] eur[7] eur[8] eur[9] eur[10] eur[11] eur[12]
    with frame a.

   {wbrp06.i &command = update
             &fields = " tar1 "
             &frm = "a"}
             
    if (c-application-mode <> "WEB") or
      (c-application-mode = "WEB" and
      (c-web-request begins "DATA"))
    then do:

      bcdparm = "".
      {mfquoter.i tar1       }
        
end.

 /* OUTPUT DESTINATION SELECTION */
   {gpselout.i &printType = "printer"
               &printWidth = 132
               &pagedFlag = " "
               &stream = " "
               &appendToFile = " "
               &streamedOutputToTerminal = " "
               &withBatchOption = "yes"
               &displayStatementType = 1
               &withCancelMessage = "yes"
               &pageBottomMargin = 6
               &withEmail = "yes"
               &withWinprint = "yes"
               &defineVariables = "yes"}
               
{mfphead.i}

aylar[1] = "Ocak".
aylar[2] = "Þubat".
aylar[3] = "Mart".
aylar[4] = "Nisan".
aylar[5] = "Mayýs".
aylar[6] = "Haziran".
aylar[7] = "Temmuz".
aylar[8] = "Aðustos".
aylar[9] = "Eylül".
aylar[10] = "Ekim".
aylar[11] = "Kasým".
aylar[12] = "Aralýk".

FOR EACH mrp_det WHERE mrp_domain = "PLASMOT" AND mrp_due_date >= tar1 NO-LOCK:
    CREATE tmp1.
        ASSIGN tmp1_part    = mrp_part
               tmp1_qty     = mrp_qty.
               tmp1_nbr     = mrp_nbr.
               tmp1_yil     = STRING(YEAR(mrp_due_date)).
               tmp1_type    = trim(mrp_type) + "_" + trim(mrp_detail).
               /*tmp1_ay1     = DATE(("01" + "/" + month(mrp_due_date) + "/" + YEAR(mrp_due_date)).*/
               tmp1_ay1     = DATE((string("01/") + string(MONTH(mrp_due_date)) + "/" + STRING(YEAR(mrp_due_date)- 2000,"99"))).
               IF month(mrp_due_date) = 1  THEN tmp1_ay = "OCAK".
               IF month(mrp_due_date) = 2  THEN tmp1_ay = "ÞUBAT".
               IF month(mrp_due_date) = 3  THEN tmp1_ay = "MART".
               IF month(mrp_due_date) = 4  THEN tmp1_ay = "NÝSAN".
               IF month(mrp_due_date) = 5  THEN tmp1_ay = "MAYIS".
               IF month(mrp_due_date) = 6  THEN tmp1_ay = "HAZÝRAN".
               IF month(mrp_due_date) = 7  THEN tmp1_ay = "TEMMUZ".
               IF month(mrp_due_date) = 8  THEN tmp1_ay = "AÐUSTOS".
               IF month(mrp_due_date) = 9  THEN tmp1_ay = "EYLÜL".
               IF month(mrp_due_date) = 10 THEN tmp1_ay = "EKÝM".
               IF month(mrp_due_date) = 11 THEN tmp1_ay = "KASIM".
               IF month(mrp_due_date) = 12 THEN tmp1_ay = "ARALIK".
               find first pt_mstr where pt_domain = "PLASMOT" and pt_part = mrp_part no-lock no-error.
               IF AVAIL pt_mstr THEN
               DO:
                   tmp1_stokyeri = pt_loc.
                   tmp1_urungr   = pt_prod_line.
                   tmp1_pm       = pt_pm_code.
               END.
               find first wo_mstr where wo_domain = "PLASMOT" and wo_nbr = mrp_nbr no-lock no-error.
               if avail wo_mstr then
               do:
                   find first pt_mstr where pt_domain = "PLASMOT" and pt_part = wo_part no-lock no-error.
                   if avail pt_mstr then
                   do:
                      tmp1_urungr1 = pt_prod_line.
                   end.
               end.
               else do:
                    find first so_mstr where so_domain = "PLASMOT" and so_nbr = mrp_nbr no-lock no-error.
                    if avail so_mstr then
                    do:
                       tmp1_urungr1 = "SATIÞ".
                    end.
                    else do:
                       find first po_mstr where po_domain = "PLASMOT" and po_nbr = mrp_nbr no-lock no-error.
                       if avail po_mstr then
                       do:
                          tmp1_urungr1 = "SATINALMA".
                       end.
                    end.
               end. 
               tmp1_detay = mrp_detail.
END.



FOR EACH tmp1 NO-LOCK:
    IF tmp1_urungr = "004" AND (tmp1_urungr1 = "302" OR tmp1_urungr1 = "402") THEN tmp1_songrup = tmp1_songrup + "PLS_HM_IHT".
    IF tmp1_urungr = "105" AND (tmp1_urungr1 = "302" OR tmp1_urungr1 = "402") THEN tmp1_songrup = tmp1_songrup + "PLS_AMB_IHT".
    IF tmp1_urungr = "103" AND tmp1_stokyeri = "2003" AND (tmp1_urungr1 = "302" OR tmp1_urungr1 = "402") THEN tmp1_songrup = tmp1_songrup + "PLS_YRD_MLZ_IHT". 
    IF tmp1_pm = "P" AND (tmp1_urungr = "102" OR tmp1_urungr = "103") AND (tmp1_stokyeri = "3001" OR tmp1_stokyeri = "2004" OR tmp1_stokyeri = "2005") AND (tmp1_detay = "Ýþ Emri Komponenti" OR tmp1_detay = "Müþteri Programý") THEN tmp1_songrup = tmp1_songrup + "PLS_AL_SAT".
    IF tmp1_urungr = "004" AND (tmp1_urungr1 = "102" OR tmp1_urungr1 = "103") THEN tmp1_songrup = tmp1_songrup + "TED_HM_IHT".
    IF tmp1_urungr = "105" AND (tmp1_urungr1 = "102" OR tmp1_urungr1 = "103") THEN tmp1_songrup = tmp1_songrup + "TED_AMB_IHT".
    IF tmp1_urungr = "103" AND tmp1_stokyeri = "2003" AND (tmp1_urungr1 = "102" OR tmp1_urungr1 = "103") AND tmp1_detay = "Ýþ Emri Komponenti" THEN tmp1_songrup = tmp1_songrup + "TED_YRD_MLZ_IHT".
END.

toplam = 0.
FOR EACH tmp1 NO-LOCK BREAK BY tmp1_songrup BY tmp1_yil BY tmp1_ay BY tmp1_part:
    toplam = toplam + tmp1_qty.
    IF LAST-OF(tmp1_part) THEN
    DO:
        CREATE tmp2.
            ASSIGN tmp2_part        = tmp1_part
                   tmp2_qty         = toplam
                   tmp2_nbr         = tmp1_nbr
                   tmp2_yil         = tmp1_yil
                   tmp2_ay          = tmp1_ay
                   tmp2_ay1         = tmp1_ay1
                   tmp2_type        = tmp1_type
                   tmp2_stokyeri    = tmp1_stokyeri
                   tmp2_urungr      = tmp1_urungr
                   tmp2_pm          = tmp1_pm
                   tmp2_urungr1     = tmp1_urungr1
                   tmp2_detay       = tmp1_detay
                   tmp2_songrup     = tmp1_songrup.
                   toplam = 0.
    END.
END.        
                                                                                             

/*OUTPUT TO "c:\sil\samet.txt".*/

FOR EACH tmp2 NO-LOCK:
    tanim = "".
    FIND FIRST pt_mstr WHERE pt_domain = "PLASMOT" AND tmp2_part = pt_part NO-LOCK NO-ERROR.
    IF AVAIL pt_mstr THEN
    DO:
        tanim = TRIM(pt_desc1) + pt_desc2.
    END.

/*Son Satýnalma Cari*/
DEF VAR stnlcari AS CHAR FORMAT "x(36)".
stnlcari = "".
find first ad_mstr where ad_domain = "PLASMOT" and ad_addr = tmp2_nbr and tmp2_detay = "Stc Prg SE" no-lock no-error.
if avail ad_mstr then
do:
    stnlcari = tmp2_nbr + " " + ad_name.
end.
else do:
    FIND LAST tr_hist WHERE tr_domain = "PLASMOT" AND tr_part = tmp2_part AND tr_type = "RCT-PO" AND tr_effdate >= TODAY - 360  NO-LOCK NO-ERROR.
    if avail tr_hist then
    do:
      find first ad_mstr where ad_domain = "PLASMOT" and ad_addr = tr_addr no-lock no-error.
      if avail ad_mstr then
      do:
         stnlcari = tr_addr + " " + ad_name.
      end.
      else do:
         stnlcari = tr_addr.
      end.
    END.
END.
/*Son Satýnalma*/

/*Satýnalma Fiyatý*/
define variable stnlfiy as DEC.
DEF VAR stnlcurr AS CHAR FORMAT "x(16)". 
stnlfiy = 0.
stnlcurr = "".
    IF tmp2_detay = "Stc Prg SE"  THEN
    DO:
       FIND FIRST pod_det WHERE pod_domain = "PLASMOT" AND pod_nbr = tmp2_nbr AND pod_part = tmp2_part AND pod_pr_list <> "" NO-LOCK NO-ERROR.
       IF AVAIL pod_det THEN
       DO:
           FOR EACH pc_mstr WHERE pc_domain = "PLASMOT" AND pc_list = pod_pr_list AND pc_part = tmp2_part NO-LOCK BREAK BY pc_list:
               stnlfiy = pc_amt[1].
               stnlcurr = pc_curr.
           END.
       END.
    END.
    IF stnlfiy = 0 THEN
    DO:
        FIND LAST tr_hist WHERE tr_domain = "PLASMOT" AND tr_part = tmp2_part AND tr_type = "RCT-PO" AND tr_effdate >= TODAY - 360 AND NOT tr_nbr BEGINS "I" NO-LOCK NO-ERROR.
        IF AVAIL tr_hist THEN
        DO:
            FIND FIRST pod_det WHERE pod_domain = "PLASMOT" AND pod_nbr = tr_nbr AND pod_part = tmp2_part AND pod_pr_list <> "" NO-LOCK NO-ERROR.
            IF AVAIL pod_det THEN
            DO:
                 FOR EACH pc_mstr WHERE pc_domain = "PLASMOT" AND pc_list = pod_pr_list AND pc_part = tmp2_part NO-LOCK BREAK BY pc_list:
                     stnlfiy = pc_amt[1].
                     stnlcurr = pc_curr.
                 END.   
            END.
        END.
    END.
    IF stnlfiy = 0 THEN
    DO:
        find first sct_det where sct_domain = "PLASMOT" and sct_part = tmp2_part and sct_sim = "Ortalama" no-lock no-error.
        if avail sct_det then
        do:
           stnlfiy = sct_cst_tot.
           stnlcurr = "ORTALAMA".
        end.
        for each zpin_mstr where zpin_domain = "PLASMOT" and zpin_part = tmp2_part and zpin_year <= year(TODAY) no-lock:
              IF zpin_year = year(TODAY) THEN
              DO:
                  IF zpin_per > MONTH(TODAY) THEN NEXT. 
              END.
              if zpin_cst_tot[1] <> 0 then
              do:
                    stnlfiy = zpin_cst_tot[1].
                    stnlcurr = "ISMM".
              end.  
        end.
    END.
/*Satýnalma Fiyatý*/

/*Son Satýþ Cari*/
define variable satcari AS CHAR FORMAT "x(36)".
satcari = "".
find first ad_mstr where ad_domain = "PLASMOT" and ad_addr = tmp2_nbr and tmp2_detay = "Müþteri Programý" no-lock no-error.
if avail ad_mstr then
do:
   stnlcari = tmp2_nbr + " " + ad_name.
end.
else do:
    FIND LAST tr_hist WHERE tr_domain = "PLASMOT" AND tr_part = tmp2_part AND tr_type = "ISS-SO" AND tr_effdate >= TODAY - 360  NO-LOCK NO-ERROR.
    if avail tr_hist then
    do:
       find first ad_mstr where ad_domain = "PLASMOT" and ad_addr = tr_addr no-lock no-error.
       if avail ad_mstr then
       do:
          satcari = tr_addr + " " + ad_name.
       end.
       else do:
          satcari = tr_addr.
       end.
    end.
end.
/*Son Satýþ Cari*/

/*Son Satýþ*/
define variable satfiy as DEC.
DEF VAR satcurr AS CHAR FORMAT "x(18)".
satfiy = 0.
satcurr = "".
    IF tmp2_detay = "Müþteri Programý"  THEN
    DO:
       FIND FIRST sod_det WHERE sod_domain = "PLASMOT" AND sod_nbr = tmp2_nbr AND sod_part = tmp2_part AND sod_pr_list <> "" NO-LOCK NO-ERROR.
       IF AVAIL sod_det THEN
       DO:
           FOR EACH pc_mstr WHERE pc_domain = "PLASMOT" AND pc_list = sod_pr_list AND pc_part = tmp2_part NO-LOCK BREAK BY pc_list:
               satfiy = pc_amt[1].
               satcurr = pc_curr.
           END.
       END.
    END.
    IF satfiy = 0 THEN
    DO:
        FIND LAST tr_hist WHERE tr_domain = "PLASMOT" AND tr_part = tmp2_part AND tr_type = "ISS-SO" AND tr_effdate >= TODAY - 360 AND NOT tr_nbr BEGINS "I" NO-LOCK NO-ERROR.
        IF AVAIL tr_hist THEN
        DO:
            FIND FIRST sod_det WHERE sod_domain = "PLASMOT" AND sod_nbr = tr_nbr AND sod_part = tmp2_part AND sod_pr_list <> "" NO-LOCK NO-ERROR.
            IF AVAIL sod_det THEN
            DO:
                 FOR EACH pc_mstr WHERE pc_domain = "PLASMOT" AND pc_list = sod_pr_list AND pc_part = tmp2_part NO-LOCK BREAK BY pc_list:
                     satfiy = pc_amt[1].
                     satcurr = pc_curr.
                 END.   
            END.
        END.
    END.
    IF satfiy = 0 THEN
    DO:
        find first sct_det where sct_domain = "PLASMOT" and sct_part = tmp2_part and sct_sim = "Ortalama" no-lock no-error.
        if avail sct_det then
        do:
           satfiy = sct_cst_tot.
           satcurr = "Ortalama".
        end.
        for each zpin_mstr where zpin_domain = "PLASMOT" and zpin_part = tmp2_part and zpin_year <= year(TODAY) no-lock:
              IF zpin_year = year(TODAY) THEN
              DO:
                  IF zpin_per > MONTH(TODAY) THEN NEXT. 
              END.
              if zpin_cst_tot[1] <> 0 then
              do:
                    satfiy = zpin_cst_tot[1].
                    satcurr = "ISMM".

              end.  
        end.
    END.

/*Son Satýþ*/

    /*
        DEF TEMP-TABLE temp3
            FIELD temp3_part AS CHAR FORMAT "x(20)"
            FIELD temp3_tanim AS CHAR FORMAT "x(80)"
            FIELD temp3_yil AS CHAR
            FIELD temp3_ay AS CHAR
            FIELD temp3_ay1 AS DATE /*tarih ilgili ayýn il günü*/
            FIELD temp3_miktar AS DEC FORMAT "->>>,>>>,>>9.99"
            FIELD temp3_urungr AS CHAR /*ÜRETÝM HATTI*/
            FIELD temp3_urungr1 AS CHAR /*ÜRÜN GRUBU TÝP*/
            FIELD temp3_pm AS CHAR
            FIELD temp3_stokyeri AS CHAR
            FIELD temp3_songrup AS CHAR /*TYPE*/
            FIELD temp3_stnlcari AS CHAR 
            FIELD temp3_stnlfiy AS DEC /*SATINALMA FÝYATI*/
            FIELD temp3_stnlcurr AS DEC /*SATINALMA KUR*/
            FIELD temp3_satcari AS CHAR /*SON SATIÞ CARÝ*/
            FIELD temp3_satfiy AS DEC /*SATIÞ FÝYATI*/
            FIELD temp3_satcurr AS DEC. /*SATIÞ KURU*/
    */

    CREATE temp3.
           temp3_part       = tmp2_part.
           temp3_tanim      = tanim.
           temp3_yil        = tmp2_yil.
           temp3_ay         = tmp2_ay.
           temp3_ay1        = tmp2_ay1.
           temp3_miktar     = tmp2_qty.
           temp3_urungr     = tmp2_urungr.
           temp3_urungr1    = tmp2_urungr1.
           temp3_pm         = tmp2_pm.
           temp3_stokyeri   = tmp2_stokyeri.
           temp3_songrup    = tmp2_songrup.
           temp3_stnlcari   = stnlcari.
           temp3_stnlfiy    = stnlfiy.
           temp3_stnlcurr   = stnlcurr.
           temp3_satcari    = satcari.
           temp3_satfiy     = satfiy.
           temp3_satcurr    = satcurr.

/*
    DISP  tmp2_part LABEL "Parça Kodu" tanim FORMAT "x(80)" LABEL "Taným" tmp2_ay LABEL "Ay" tmp2_yil Label "Yýl" tmp2_qty LABEL "Miktar" FORMAT "->>>,>>>,>>9.99" "    "
          tmp2_urungr LABEL "Üretim Hattý" tmp2_pm LABEL "P/M" tmp2_stokyeri LABEL "Stok Yeri" tmp2_urungr1 LABEL "Ürün Grubu Tip" tmp2_songrup LABEL "Type" stnlcari LABEL "Son Satýnalma Cari"
          stnlfiy LABEL "Satýnalma Fiyatý" "    " stnlcurr LABEL "Satýnalma Kur" satcari LABEL "Son Satýþ Cari" "    " satfiy LABEL "Satýþ Fiyatý" "   " satcurr LABEL "Satýþ Kuru" "   " tmp2_ay1 LABEL "Tarih" 
        WITH WIDTH 450.
        
*/

END.

/*
DEF TEMP-TABLE temp4
        FIELD temp4_part AS CHAR FORMAT "x(10)"
        FIELD temp4_tanim AS CHAR FORMAT "x(50)"
        FIELD temp4_yil AS CHAR EXTENT 12
        FIELD temp4_ay AS CHAR EXTENT 12
        FIELD temp4_ay1 AS DATE EXTENT 12 /*tarih ilgili ayýn il günü*/
        FIELD temp4_miktar AS DEC FORMAT "->>>,>>>,>>9.99" EXTENT 12
        FIELD temp4_urungr AS CHAR EXTENT 12 /*ÜRETÝM HATTI*/
        FIELD temp4_urungr1 AS CHAR EXTENT 12 /*ÜRÜN GRUBU TÝP*/
        FIELD temp4_pm AS CHAR EXTENT 12
        FIELD temp4_stokyeri AS CHAR EXTENT 12
        FIELD temp4_songrup AS CHAR EXTENT 12 /*TYPE*/
        FIELD temp4_stnlcari AS CHAR EXTENT 12
        FIELD temp4_stnlfiy AS DEC EXTENT 12 /*SATINALMA FÝYATI*/
        FIELD temp4_stnlcurr AS CHAR FORMAT "x(18)" EXTENT 12 /*SATINALMA KUR*/
        FIELD temp4_satcari AS CHAR EXTENT 12 /*SON SATIÞ CARÝ*/
        FIELD temp4_satfiy AS DEC EXTENT 12 /*SATIÞ FÝYATI*/
        FIELD temp4_satcurr AS CHAR FORMAT "x(18)" EXTENT 12. /*SATIÞ KURU*/
*/

/* ÝLK TARÝHÝ BAZ ALARAK ÖNCE REFERANS LÝSTE OLUÞTURUYORUM */
FOR EACH temp3 WHERE temp3_songru BEGINS "TED" BREAK BY temp3_yil BY temp3_ay1 BY temp3_part:
    IF FIRST-OF(temp3_part) THEN
    DO:
        CREATE temp4.
                   temp4_part       = temp3_part.
                   temp4_tanim      = temp3_tanim.
                   temp4_yil        = temp3_yil.
                   temp4_ay1        = temp3_ay1.
                   temp4_stnlcari   = temp3_stnlcari. /* satýnalma cari */
                   temp4_urungr     = temp3_urungr.  /* üretim hattý */
                   temp4_urungr1    = temp3_urungr1. /* satýnalma imalat */
                   temp4_stnlfiy    = temp3_stnlfiy. /* satýnalma birim fiyatý */
                   temp4_stnlcurr   = temp3_stnlcurr. /* satýnalma kuru */   
                   temp4_pm         = temp3_pm. /* satýnalma - imalat */
    END.
                   temp4_miktar[MONTH(temp4_ay1)]   = temp4_miktar[MONTH(temp4_ay1)] + temp4_miktar[MONTH(temp4_ay1)]. /* + temp4_miktar. */

                   IF temp4_stnlcurr BEGINS "YTL" THEN
                   DO:
                        temp4_birfiyat[MONTH(temp4_ay1)] = temp4_stnlfiy * 1. /* YTL FIAYYTA BÝRÝM FÝYAT AYNI KALACAK */
                   END.
                   ELSE IF temp4_stnlcurr BEGINS "US" THEN
                   DO:
                        temp4_birfiyat[MONTH(temp4_ay1)] = temp4_stnlfiy * usd[MONTH(temp4_ay1)]. /* USD FKURUNU ALARAK FÝYAT OLUÞTUR */
                   END.
                   ELSE IF temp4_stnlcurr BEGINS "EU" THEN
                   DO:
                        temp4_birfiyat[MONTH(temp4_ay1)] = temp4_stnlfiy * eur[MONTH(temp4_ay1)]. /* EURO KURUNU ALARAK TL FÝYAT OLUÞUYOR*/
                   END.
                   ELSE IF temp4_stnlcurr BEGINS "ISMM" THEN
                   DO:
                        temp4_birfiyat[MONTH(temp4_ay1)] = temp4_stnlfiy * ey[MONTH(temp4_ay1)]. /* ISMM EY KURUNU BAZ ALIYORUM*/
                   END.
                   ELSE IF temp4_stnlcurr BEGINS "ORTA" THEN
                   DO:
                        temp4_birfiyat[MONTH(temp4_ay1)] = temp4_stnlfiy * ef[MONTH(temp4_ay1)]. /* ORTA EF KURUNU BAZ ALIYORUM */
                   END.
                   ELSE DO:
                        temp4_birfiyat[MONTH(temp4_ay1)] = temp4_stnlfiy * 0. /* HÝÇ FÝYAT YOK ÝSE 0 BAZ ALIYORUM */
                   END.

                   temp4_tutartl[MONTH(temp4_ay1)]  = temp4_tutartl[MONTH(temp4_ay1)] + temp4_birfiyat[MONTH(temp4_ay1)] * temp4_miktar[MONTH(temp4_ay1)].
END.

/*
PUT "Son.Stnalm.Cari" AT 1.
PUT "Parça Kodu"      AT 30.
PUT "Taným"           AT 45.
PUT "Üretim Hattý"    AT 100.
PUT "Stnlm/Ýmlt"      AT 115.
PUT "Stnalm.Fiyatý"   AT 130.
PUT "Miktar"          AT 145.
PUT "StnalmKuru"      AT 155.
PUT "Tarih"           AT 170.
*/


ii = 0.
aa = 158.
PUT "StnlCari" AT 1.
PUT "Parça Kodu" AT 20.
PUT "Taným" AT 40.
PUT "ÜretimYeri" AT 77.
PUT "P/M" AT 89.
PUT "StBirFiyat" AT 100. 
PUT "StnParaBirimi" AT 122.

DO ii = 1 TO 12:
   xxx = aylar[ii] + "_Mik".
   PUT xxx AT aa.
   aa = aa + 15.
   yyy = aylar[ii] + "_BirFiy".
   PUT yyy AT aa.
   aa = aa + 15.
   zzz = aylar[ii] + "_Tutar".
   PUT zzz AT aa.
   aa = aa + 15.
END.



PUT "---------------  -----------------  ------------------------------------  ------------  ------  --------------------  -------------  -----------------------   ----------------------                                 ----------------------------- " AT 1.

i = 0.
a = 150.
FOR EACH temp4 NO-LOCK:
    PUT temp4_stnlcari AT 1.
    PUT temp4_part AT 20.
    PUT temp4_tanim AT 40.
    PUT temp4_urungr AT 80. /* ütryim yeri*/
    PUT temp4_pm AT 90. /* SATINALMA - PLANLAMA */
    PUT temp4_stnlfiy AT 100. /* SATINALMA BÝRÝM FÝYAT */
    PUT temp4_stnlcurr AT 125. /* SATINALMA KUR Ý */

    DO  i = 1 TO 12:
        PUT temp4_miktar[i] AT a.
        a = a + 15.
        PUT temp4_birfiyat[i] AT a.
        a = a + 15.
        PUT temp4_tutartl[i] AT a.
        a = a + 15.
    END.
    a = 150.
END.


/* REPORT TRAILER  */
  {mfrtrail.i}
  {xxreset.i}
  {xxgrptrm.i}
       
end.
/*V8-*/
{wbrp04.i &frame-spec = a}
/*V8+*/
