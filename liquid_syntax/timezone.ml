type info = {
  abbr : string;
  name : string;
  utc_offset : float;
  gmt_offset : float;
}

type t =
  | ACDT
  | ACST
  | ACT
  | ACWST
  | ADT
  | AEDT
  | AEST
  | AFT
  | AKDT
  | AKST
  | AMST
  | AMT
  | ART
  | AST
  | AT
  | AWST
  | AZOST
  | AZOT
  | AZT
  | BDT
  | BIT
  | BNT
  | BOT
  | BRST
  | BRT
  | BST
  | BTT
  | CAT
  | CCT
  | CDT
  | CEST
  | CET
  | CHADT
  | CHAST
  | CHOST
  | CHOT
  | CHST
  | CHUT
  | CIST
  | CIT
  | CKT
  | CLST
  | CLT
  | COST
  | COT
  | CST
  | CT
  | CVT
  | CWST
  | CXT
  | DAVT
  | DDUT
  | EASST
  | EAST
  | EAT
  | ECT
  | EDT
  | EEST
  | EET
  | EGST
  | EGT
  | EIT
  | EST
  | ET
  | FET
  | FJT
  | FKST
  | FKT
  | FNT
  | GALT
  | GAMT
  | GET
  | GFT
  | GILT
  | GIT
  | GMT
  | GST
  | GYT
  | HADT
  | HAST
  | HKT
  | HMT
  | HOVST
  | HOVT
  | ICT
  | IDT
  | IOT
  | IRDT
  | IRKT
  | IRST
  | IST
  | JST
  | KGT
  | KOST
  | KRAT
  | KST
  | LHDT
  | LHST
  | LINT
  | MAGT
  | MART
  | MAWT
  | MDT
  | MHT
  | MIST
  | MIT
  | MMT
  | MSK
  | MST
  | MT
  | MUT
  | MVT
  | MYT
  | NCT
  | NDT
  | NFT
  | NPT
  | NRT
  | NST
  | NT
  | NUT
  | NZDT
  | NZST
  | OMST
  | ORAT
  | PDT
  | PET
  | PETT
  | PGT
  | PHOT
  | PhST
  | PHT
  | PKT
  | PMDT
  | PMST
  | PONT
  | PST
  | PT
  | PWT
  | PYST
  | PYT
  | RET
  | ROTT
  | SAKT
  | SAMT
  | SAST
  | SBT
  | SCT
  | SGT
  | SLST
  | SRET
  | SRT
  | SST
  | SYOT
  | TAHT
  | TFT
  | THA
  | TJT
  | TKT
  | TLT
  | TMT
  | TOT
  | TRT
  | TVT
  | ULAST
  | ULAT
  | USZ1
  | UYST
  | UYT
  | UZT
  | VET
  | VLAT
  | VOLT
  | VOST
  | VUT
  | WAKT
  | WAST
  | WAT
  | WEST
  | WET
  | WFT
  | WGST
  | WIB
  | WIT
  | WST
  | YAKT
  | YEKT
  | Custom of int

let default = GMT

(* I just hacked this together, if you have some very important timezone needs write your own timezone code *)
let info_from_timezone = function
  | ACDT ->
      {
        abbr = "ACDT";
        name = "Australian Central Daylight Savings Time";
        utc_offset = 10.5;
        gmt_offset = 10.5;
      }
  | ACST ->
      {
        abbr = "ACST";
        name = "Australian Central Standard Time";
        utc_offset = 9.5;
        gmt_offset = 9.5;
      }
  | ACT ->
      { abbr = "ACT"; name = "Acre Time"; utc_offset = -5.; gmt_offset = -5. }
  | ACWST ->
      {
        abbr = "ACWST";
        name = "Australian Central Western Standard Time";
        utc_offset = 8.75;
        gmt_offset = 8.75;
      }
  | ADT ->
      {
        abbr = "ADT";
        name = "Atlantic Daylight Time";
        utc_offset = -3.;
        gmt_offset = -3.;
      }
  | AEDT ->
      {
        abbr = "AEDT";
        name = "Australian Eastern Daylight Savings Time";
        utc_offset = 11.;
        gmt_offset = 11.;
      }
  | AEST ->
      {
        abbr = "AEST";
        name = "Australian Eastern Standard Time";
        utc_offset = 10.;
        gmt_offset = 10.;
      }
  | AFT ->
      {
        abbr = "AFT";
        name = "Afghanistan Time";
        utc_offset = 4.5;
        gmt_offset = 4.5;
      }
  | AKDT ->
      {
        abbr = "AKDT";
        name = "Alaska Daylight Time";
        utc_offset = -8.;
        gmt_offset = -8.;
      }
  | AKST ->
      {
        abbr = "AKST";
        name = "Alaska Standard Time";
        utc_offset = -9.;
        gmt_offset = -9.;
      }
  | AMST ->
      {
        abbr = "AMST";
        name = "Amazon Summer Time";
        utc_offset = -3.;
        gmt_offset = -3.;
      }
  | AMT ->
      { abbr = "AMT"; name = "Armenia Time"; utc_offset = 4.; gmt_offset = 4. }
  | ART ->
      {
        abbr = "ART";
        name = "Argentina Time";
        utc_offset = -3.;
        gmt_offset = -3.;
      }
  | AST ->
      {
        abbr = "AST";
        name = "Atlantic Standard Time";
        utc_offset = -4.;
        gmt_offset = -4.;
      }
  | AT ->
      {
        abbr = "AT";
        name = "Atlantic Time";
        utc_offset = -4.;
        gmt_offset = -4.;
      }
  | AWST ->
      {
        abbr = "AWST";
        name = "Australian Western Standard Time";
        utc_offset = 8.;
        gmt_offset = 8.;
      }
  | AZOST ->
      {
        abbr = "AZOST";
        name = "Azores Summer Time";
        utc_offset = 0.;
        gmt_offset = 0.;
      }
  | AZOT ->
      {
        abbr = "AZOT";
        name = "Azores Standard Time";
        utc_offset = -1.;
        gmt_offset = -1.;
      }
  | AZT ->
      {
        abbr = "AZT";
        name = "Azerbaijan Time";
        utc_offset = 4.;
        gmt_offset = 4.;
      }
  | BDT ->
      { abbr = "BDT"; name = "Brunei Time"; utc_offset = 8.; gmt_offset = 8. }
  | BIT ->
      {
        abbr = "BIT";
        name = "Baker Island Time";
        utc_offset = -12.;
        gmt_offset = -12.;
      }
  | BNT ->
      {
        abbr = "BNT";
        name = "Brunei Darussalam Time";
        utc_offset = 8.;
        gmt_offset = 8.;
      }
  | BOT ->
      {
        abbr = "BOT";
        name = "Bolivia Time";
        utc_offset = -4.;
        gmt_offset = -4.;
      }
  | BRST ->
      {
        abbr = "BRST";
        name = "Brasilia Summer Time";
        utc_offset = -2.;
        gmt_offset = -2.;
      }
  | BRT ->
      {
        abbr = "BRT";
        name = "Brasilia Time";
        utc_offset = -3.;
        gmt_offset = -3.;
      }
  | BST ->
      {
        abbr = "BST";
        name = "British Summer Time";
        utc_offset = 1.;
        gmt_offset = 1.;
      }
  | BTT ->
      { abbr = "BTT"; name = "Bhutan Time"; utc_offset = 6.; gmt_offset = 6. }
  | CAT ->
      {
        abbr = "CAT";
        name = "Central Africa Time";
        utc_offset = 2.;
        gmt_offset = 2.;
      }
  | CCT ->
      {
        abbr = "CCT";
        name = "Cocos Islands Time";
        utc_offset = 6.5;
        gmt_offset = 6.5;
      }
  | CDT ->
      {
        abbr = "CDT";
        name = "Central Daylight Time";
        utc_offset = -5.;
        gmt_offset = -5.;
      }
  | CEST ->
      {
        abbr = "CEST";
        name = "Central European Summer Time";
        utc_offset = 2.;
        gmt_offset = 2.;
      }
  | CET ->
      {
        abbr = "CET";
        name = "Central European Time";
        utc_offset = 1.;
        gmt_offset = 1.;
      }
  | CHADT ->
      {
        abbr = "CHADT";
        name = "Chatham Daylight Time";
        utc_offset = 13.75;
        gmt_offset = 13.75;
      }
  | CHAST ->
      {
        abbr = "CHAST";
        name = "Chatham Standard Time";
        utc_offset = 12.75;
        gmt_offset = 12.75;
      }
  | CHOST ->
      {
        abbr = "CHOST";
        name = "Choibalsan Summer Time";
        utc_offset = 9.;
        gmt_offset = 9.;
      }
  | CHOT ->
      {
        abbr = "CHOT";
        name = "Choibalsan Standard Time";
        utc_offset = 8.;
        gmt_offset = 8.;
      }
  | CHST ->
      {
        abbr = "CHST";
        name = "Chamorro Standard Time";
        utc_offset = 10.;
        gmt_offset = 10.;
      }
  | CHUT ->
      { abbr = "CHUT"; name = "Chuuk Time"; utc_offset = 10.; gmt_offset = 10. }
  | CIST ->
      {
        abbr = "CIST";
        name = "Clipperton Island Standard Time";
        utc_offset = -8.;
        gmt_offset = -8.;
      }
  | CIT ->
      {
        abbr = "CIT";
        name = "Central Indonesia Time";
        utc_offset = 8.;
        gmt_offset = 8.;
      }
  | CKT ->
      {
        abbr = "CKT";
        name = "Cook Island Time";
        utc_offset = -10.;
        gmt_offset = -10.;
      }
  | CLST ->
      {
        abbr = "CLST";
        name = "Chile Summer Time";
        utc_offset = -3.;
        gmt_offset = -3.;
      }
  | CLT ->
      {
        abbr = "CLT";
        name = "Chile Standard Time";
        utc_offset = -4.;
        gmt_offset = -4.;
      }
  | COST ->
      {
        abbr = "COST";
        name = "Colombia Summer Time";
        utc_offset = -4.;
        gmt_offset = -4.;
      }
  | COT ->
      {
        abbr = "COT";
        name = "Colombia Time";
        utc_offset = -5.;
        gmt_offset = -5.;
      }
  | CST ->
      {
        abbr = "CST";
        name = "Central Standard Time";
        utc_offset = -6.;
        gmt_offset = -6.;
      }
  | CT ->
      { abbr = "CT"; name = "Central Time"; utc_offset = -5.; gmt_offset = -5. }
  | CVT ->
      {
        abbr = "CVT";
        name = "Cape Verde Time";
        utc_offset = -1.;
        gmt_offset = -1.;
      }
  | CWST ->
      {
        abbr = "CWST";
        name = "Central Western Standard Time";
        utc_offset = 8.75;
        gmt_offset = 8.75;
      }
  | CXT ->
      {
        abbr = "CXT";
        name = "Christmas Island Time";
        utc_offset = 7.;
        gmt_offset = 7.;
      }
  | DAVT ->
      { abbr = "DAVT"; name = "Davis Time"; utc_offset = 7.; gmt_offset = 7. }
  | DDUT ->
      {
        abbr = "DDUT";
        name = "Dumont d'Urville Time";
        utc_offset = 10.;
        gmt_offset = 10.;
      }
  | EASST ->
      {
        abbr = "EASST";
        name = "Easter Island Summer Time";
        utc_offset = -5.;
        gmt_offset = -5.;
      }
  | EAST ->
      {
        abbr = "EAST";
        name = "Easter Island Standard Time";
        utc_offset = -6.;
        gmt_offset = -6.;
      }
  | EAT ->
      {
        abbr = "EAT";
        name = "East Africa Time";
        utc_offset = 3.;
        gmt_offset = 3.;
      }
  | ECT ->
      {
        abbr = "ECT";
        name = "Ecuador Time";
        utc_offset = -5.;
        gmt_offset = -5.;
      }
  | EDT ->
      {
        abbr = "EDT";
        name = "Eastern Daylight Time";
        utc_offset = -4.;
        gmt_offset = -4.;
      }
  | EEST ->
      {
        abbr = "EEST";
        name = "Eastern European Summer Time";
        utc_offset = 3.;
        gmt_offset = 3.;
      }
  | EET ->
      {
        abbr = "EET";
        name = "Eastern European Time";
        utc_offset = 2.;
        gmt_offset = 2.;
      }
  | EGST ->
      {
        abbr = "EGST";
        name = "Eastern Greenland Summer Time";
        utc_offset = 0.;
        gmt_offset = 0.;
      }
  | EGT ->
      {
        abbr = "EGT";
        name = "Eastern Greenland Time";
        utc_offset = -1.;
        gmt_offset = -1.;
      }
  | EIT ->
      {
        abbr = "EIT";
        name = "Eastern Indonesian Time";
        utc_offset = 9.;
        gmt_offset = 9.;
      }
  | EST ->
      {
        abbr = "EST";
        name = "Eastern Standard Time";
        utc_offset = -5.;
        gmt_offset = -5.;
      }
  | ET ->
      { abbr = "ET"; name = "Eastern Time"; utc_offset = -5.; gmt_offset = -5. }
  | FET ->
      {
        abbr = "FET";
        name = "Further-eastern European Time";
        utc_offset = 3.;
        gmt_offset = 3.;
      }
  | FJT ->
      { abbr = "FJT"; name = "Fiji Time"; utc_offset = 12.; gmt_offset = 12. }
  | FKST ->
      {
        abbr = "FKST";
        name = "Falkland Islands Summer Time";
        utc_offset = -3.;
        gmt_offset = -3.;
      }
  | FKT ->
      {
        abbr = "FKT";
        name = "Falkland Islands Time";
        utc_offset = -4.;
        gmt_offset = -4.;
      }
  | FNT ->
      {
        abbr = "FNT";
        name = "Fernando de Noronha Time";
        utc_offset = -2.;
        gmt_offset = -2.;
      }
  | GALT ->
      {
        abbr = "GALT";
        name = "Galapagos Time";
        utc_offset = -6.;
        gmt_offset = -6.;
      }
  | GAMT ->
      {
        abbr = "GAMT";
        name = "Gambier Islands";
        utc_offset = -9.;
        gmt_offset = -9.;
      }
  | GET ->
      {
        abbr = "GET";
        name = "Georgia Standard Time";
        utc_offset = 4.;
        gmt_offset = 4.;
      }
  | GFT ->
      {
        abbr = "GFT";
        name = "French Guiana Time";
        utc_offset = -3.;
        gmt_offset = -3.;
      }
  | GILT ->
      {
        abbr = "GILT";
        name = "Gilbert Island Time";
        utc_offset = 12.;
        gmt_offset = 12.;
      }
  | GIT ->
      {
        abbr = "GIT";
        name = "Gambier Island Time";
        utc_offset = -9.;
        gmt_offset = -9.;
      }
  | GMT ->
      {
        abbr = "GMT";
        name = "Greenwich Mean Time";
        utc_offset = 0.;
        gmt_offset = 0.;
      }
  | GST ->
      {
        abbr = "GST";
        name = "Gulf Standard Time";
        utc_offset = 4.;
        gmt_offset = 4.;
      }
  | GYT ->
      { abbr = "GYT"; name = "Guyana Time"; utc_offset = -4.; gmt_offset = -4. }
  | HADT ->
      {
        abbr = "HADT";
        name = "Hawaii-Aleutian Daylight Time";
        utc_offset = -9.;
        gmt_offset = -9.;
      }
  | HAST ->
      {
        abbr = "HAST";
        name = "Hawaii-Aleutian Standard Time";
        utc_offset = -10.;
        gmt_offset = -10.;
      }
  | HKT ->
      {
        abbr = "HKT";
        name = "Hong Kong Time";
        utc_offset = 8.;
        gmt_offset = 8.;
      }
  | HMT ->
      {
        abbr = "HMT";
        name = "Heard and McDonald Islands Time";
        utc_offset = 5.;
        gmt_offset = 5.;
      }
  | HOVST ->
      {
        abbr = "HOVST";
        name = "Khovd Summer Time";
        utc_offset = 8.;
        gmt_offset = 8.;
      }
  | HOVT ->
      {
        abbr = "HOVT";
        name = "Khovd Standard Time";
        utc_offset = 7.;
        gmt_offset = 7.;
      }
  | ICT ->
      {
        abbr = "ICT";
        name = "Indochina Time";
        utc_offset = 7.;
        gmt_offset = 7.;
      }
  | IDT ->
      {
        abbr = "IDT";
        name = "Israel Daylight Time";
        utc_offset = 3.;
        gmt_offset = 3.;
      }
  | IOT ->
      {
        abbr = "IOT";
        name = "Indian Chagos Time";
        utc_offset = 6.;
        gmt_offset = 6.;
      }
  | IRDT ->
      {
        abbr = "IRDT";
        name = "Iran Daylight Time";
        utc_offset = 4.5;
        gmt_offset = 4.5;
      }
  | IRKT ->
      { abbr = "IRKT"; name = "Irkutsk Time"; utc_offset = 8.; gmt_offset = 8. }
  | IRST ->
      {
        abbr = "IRST";
        name = "Iran Standard Time";
        utc_offset = 3.5;
        gmt_offset = 3.5;
      }
  | IST ->
      {
        abbr = "IST";
        name = "Irish Standard Time";
        utc_offset = 1.;
        gmt_offset = 1.;
      }
  | JST ->
      {
        abbr = "JST";
        name = "Japan Standard Time";
        utc_offset = 9.;
        gmt_offset = 9.;
      }
  | KGT ->
      {
        abbr = "KGT";
        name = "Kyrgyzstan time";
        utc_offset = 6.;
        gmt_offset = 6.;
      }
  | KOST ->
      {
        abbr = "KOST";
        name = "Kosrae Time";
        utc_offset = 11.;
        gmt_offset = 11.;
      }
  | KRAT ->
      {
        abbr = "KRAT";
        name = "Krasnoyarsk Time";
        utc_offset = 7.;
        gmt_offset = 7.;
      }
  | KST ->
      {
        abbr = "KST";
        name = "Korea Standard Time";
        utc_offset = 9.;
        gmt_offset = 9.;
      }
  | LHDT ->
      {
        abbr = "LHDT";
        name = "Lord Howe Daylight Time";
        utc_offset = 11.;
        gmt_offset = 11.;
      }
  | LHST ->
      {
        abbr = "LHST";
        name = "Lord Howe Standard Time";
        utc_offset = 10.5;
        gmt_offset = 10.5;
      }
  | LINT ->
      {
        abbr = "LINT";
        name = "Line Islands Time";
        utc_offset = 14.;
        gmt_offset = 14.;
      }
  | MAGT ->
      {
        abbr = "MAGT";
        name = "Magadan Time";
        utc_offset = 11.;
        gmt_offset = 11.;
      }
  | MART ->
      {
        abbr = "MART";
        name = "Marquesas Islands Time";
        utc_offset = -9.5;
        gmt_offset = -9.5;
      }
  | MAWT ->
      {
        abbr = "MAWT";
        name = "Mawson Station Time";
        utc_offset = 5.;
        gmt_offset = 5.;
      }
  | MDT ->
      {
        abbr = "MDT";
        name = "Mountain Daylight Time";
        utc_offset = -6.;
        gmt_offset = -6.;
      }
  | MHT ->
      {
        abbr = "MHT";
        name = "Marshall Islands";
        utc_offset = 12.;
        gmt_offset = 12.;
      }
  | MIST ->
      {
        abbr = "MIST";
        name = "Macquarie Island Station Time";
        utc_offset = 11.;
        gmt_offset = 11.;
      }
  | MIT ->
      {
        abbr = "MIT";
        name = "Marquesas Islands Time";
        utc_offset = -9.5;
        gmt_offset = -9.5;
      }
  | MMT ->
      {
        abbr = "MMT";
        name = "Myanmar Standard Time";
        utc_offset = 6.5;
        gmt_offset = 6.5;
      }
  | MSK ->
      { abbr = "MSK"; name = "Moscow Time"; utc_offset = 3.; gmt_offset = 3. }
  | MST ->
      {
        abbr = "MST";
        name = "Mountain Standard Time";
        utc_offset = -6.;
        gmt_offset = -6.;
      }
  | MT ->
      {
        abbr = "MT";
        name = "Mountain Time";
        utc_offset = -6.;
        gmt_offset = -6.;
      }
  | MUT ->
      {
        abbr = "MUT";
        name = "Mauritius Time";
        utc_offset = 4.;
        gmt_offset = 4.;
      }
  | MVT ->
      { abbr = "MVT"; name = "Maldives Time"; utc_offset = 5.; gmt_offset = 5. }
  | MYT ->
      { abbr = "MYT"; name = "Malaysia Time"; utc_offset = 8.; gmt_offset = 8. }
  | NCT ->
      {
        abbr = "NCT";
        name = "New Caledonia Time";
        utc_offset = 11.;
        gmt_offset = 11.;
      }
  | NDT ->
      {
        abbr = "NDT";
        name = "Newfoundland Daylight Time";
        utc_offset = -2.5;
        gmt_offset = -2.5;
      }
  | NFT ->
      {
        abbr = "NFT";
        name = "Norfolk Time";
        utc_offset = 11.;
        gmt_offset = 11.;
      }
  | NPT ->
      {
        abbr = "NPT";
        name = "Nepal Time";
        utc_offset = 5.75;
        gmt_offset = 5.75;
      }
  | NRT ->
      { abbr = "NRT"; name = "Nauru Time"; utc_offset = 12.; gmt_offset = 12. }
  | NST ->
      {
        abbr = "NST";
        name = "Newfoundland Standard Time";
        utc_offset = -3.5;
        gmt_offset = -3.5;
      }
  | NT ->
      {
        abbr = "NT";
        name = "Newfoundland Time";
        utc_offset = -3.5;
        gmt_offset = -3.5;
      }
  | NUT ->
      { abbr = "NUT"; name = "Niue Time"; utc_offset = -11.; gmt_offset = -11. }
  | NZDT ->
      {
        abbr = "NZDT";
        name = "New Zealand Daylight Time";
        utc_offset = 13.;
        gmt_offset = 13.;
      }
  | NZST ->
      {
        abbr = "NZST";
        name = "New Zealand Standard Time";
        utc_offset = 12.;
        gmt_offset = 12.;
      }
  | OMST ->
      { abbr = "OMST"; name = "Omsk Time"; utc_offset = 6.; gmt_offset = 6. }
  | ORAT ->
      { abbr = "ORAT"; name = "Oral Time"; utc_offset = 5.; gmt_offset = 5. }
  | PDT ->
      {
        abbr = "PDT";
        name = "Pacific Daylight Time";
        utc_offset = -7.;
        gmt_offset = -7.;
      }
  | PET ->
      { abbr = "PET"; name = "Peru Time"; utc_offset = -5.; gmt_offset = -5. }
  | PETT ->
      {
        abbr = "PETT";
        name = "Kamchatka Time";
        utc_offset = 12.;
        gmt_offset = 12.;
      }
  | PGT ->
      {
        abbr = "PGT";
        name = "Papua New Guinea Time";
        utc_offset = 10.;
        gmt_offset = 10.;
      }
  | PHOT ->
      {
        abbr = "PHOT";
        name = "Phoenix Island Time";
        utc_offset = 13.;
        gmt_offset = 13.;
      }
  | PhST ->
      {
        abbr = "PhST";
        name = "Philippine Standard Time";
        utc_offset = 8.;
        gmt_offset = 8.;
      }
  | PHT ->
      {
        abbr = "PHT";
        name = "Philippine Time";
        utc_offset = 8.;
        gmt_offset = 8.;
      }
  | PKT ->
      {
        abbr = "PKT";
        name = "Pakistan Standard Time";
        utc_offset = 5.;
        gmt_offset = 5.;
      }
  | PMDT ->
      {
        abbr = "PMDT";
        name = "Saint Pierre and Miquelon Daylight time";
        utc_offset = -2.;
        gmt_offset = -2.;
      }
  | PMST ->
      {
        abbr = "PMST";
        name = "Saint Pierre and Miquelon Standard Time";
        utc_offset = -3.;
        gmt_offset = -3.;
      }
  | PONT ->
      {
        abbr = "PONT";
        name = "Pohnpei Standard Time";
        utc_offset = 11.;
        gmt_offset = 11.;
      }
  | PST ->
      {
        abbr = "PST";
        name = "Pacific Standard Time";
        utc_offset = -8.;
        gmt_offset = -8.;
      }
  | PT ->
      { abbr = "PT"; name = "Pacific Time"; utc_offset = -8.; gmt_offset = -8. }
  | PWT ->
      { abbr = "PWT"; name = "Palau Time"; utc_offset = 9.; gmt_offset = 9. }
  | PYST ->
      {
        abbr = "PYST";
        name = "Paraguay Summer Time";
        utc_offset = -3.;
        gmt_offset = -3.;
      }
  | PYT ->
      {
        abbr = "PYT";
        name = "Paraguay Time";
        utc_offset = -4.;
        gmt_offset = -4.;
      }
  | RET ->
      { abbr = "RET"; name = "Réunion Time"; utc_offset = 4.; gmt_offset = 4. }
  | ROTT ->
      {
        abbr = "ROTT";
        name = "Rothera Research Station Time";
        utc_offset = -3.;
        gmt_offset = -3.;
      }
  | SAKT ->
      {
        abbr = "SAKT";
        name = "Sakhalin Island time";
        utc_offset = 11.;
        gmt_offset = 11.;
      }
  | SAMT ->
      { abbr = "SAMT"; name = "Samara Time"; utc_offset = 4.; gmt_offset = 4. }
  | SAST ->
      {
        abbr = "SAST";
        name = "South African Standard Time";
        utc_offset = 2.;
        gmt_offset = 2.;
      }
  | SBT ->
      {
        abbr = "SBT";
        name = "Solomon Islands Time";
        utc_offset = 11.;
        gmt_offset = 11.;
      }
  | SCT ->
      {
        abbr = "SCT";
        name = "Seychelles Time";
        utc_offset = 4.;
        gmt_offset = 4.;
      }
  | SGT ->
      {
        abbr = "SGT";
        name = "Singapore Time";
        utc_offset = 8.;
        gmt_offset = 8.;
      }
  | SLST ->
      {
        abbr = "SLST";
        name = "Sri Lanka Standard Time";
        utc_offset = 5.5;
        gmt_offset = 5.5;
      }
  | SRET ->
      {
        abbr = "SRET";
        name = "Srednekolymsk Time";
        utc_offset = 11.;
        gmt_offset = 11.;
      }
  | SRT ->
      {
        abbr = "SRT";
        name = "Suriname Time";
        utc_offset = -3.;
        gmt_offset = -3.;
      }
  | SST ->
      {
        abbr = "SST";
        name = "Samoa Standard Time";
        utc_offset = -11.;
        gmt_offset = -11.;
      }
  | SYOT ->
      {
        abbr = "SYOT";
        name = "Showa Station Time";
        utc_offset = 3.;
        gmt_offset = 3.;
      }
  | TAHT ->
      {
        abbr = "TAHT";
        name = "Tahiti Time";
        utc_offset = -10.;
        gmt_offset = -10.;
      }
  | TFT ->
      {
        abbr = "TFT";
        name = "French Southern and Antarctic Time";
        utc_offset = 5.;
        gmt_offset = 5.;
      }
  | THA ->
      {
        abbr = "THA";
        name = "Thailand Standard Time";
        utc_offset = 7.;
        gmt_offset = 7.;
      }
  | TJT ->
      {
        abbr = "TJT";
        name = "Tajikistan Time";
        utc_offset = 5.;
        gmt_offset = 5.;
      }
  | TKT ->
      {
        abbr = "TKT";
        name = "Tokelau Time";
        utc_offset = 13.;
        gmt_offset = 13.;
      }
  | TLT ->
      {
        abbr = "TLT";
        name = "Timor Leste Time";
        utc_offset = 9.;
        gmt_offset = 9.;
      }
  | TMT ->
      {
        abbr = "TMT";
        name = "Turkmenistan Time";
        utc_offset = 5.;
        gmt_offset = 5.;
      }
  | TOT ->
      { abbr = "TOT"; name = "Tonga Time"; utc_offset = 13.; gmt_offset = 13. }
  | TRT ->
      { abbr = "TRT"; name = "Turkey Time"; utc_offset = 3.; gmt_offset = 3. }
  | TVT ->
      { abbr = "TVT"; name = "Tuvalu Time"; utc_offset = 12.; gmt_offset = 12. }
  | ULAST ->
      {
        abbr = "ULAST";
        name = "Ulaanbaatar Summer Time";
        utc_offset = 9.;
        gmt_offset = 9.;
      }
  | ULAT ->
      {
        abbr = "ULAT";
        name = "Ulaanbaatar Standard Time";
        utc_offset = 8.;
        gmt_offset = 8.;
      }
  | USZ1 ->
      {
        abbr = "USZ1";
        name = "Kaliningrad Time";
        utc_offset = 2.;
        gmt_offset = 2.;
      }
  | UYST ->
      {
        abbr = "UYST";
        name = "Uruguay Summer Time";
        utc_offset = -2.;
        gmt_offset = -2.;
      }
  | UYT ->
      {
        abbr = "UYT";
        name = "Uruguay Standard Time";
        utc_offset = -3.;
        gmt_offset = -3.;
      }
  | UZT ->
      {
        abbr = "UZT";
        name = "Uzbekistan Time";
        utc_offset = 5.;
        gmt_offset = 5.;
      }
  | VET ->
      {
        abbr = "VET";
        name = "Venezuelan Standard Time";
        utc_offset = -4.;
        gmt_offset = -4.;
      }
  | VLAT ->
      {
        abbr = "VLAT";
        name = "Vladivostok Time";
        utc_offset = 10.;
        gmt_offset = 10.;
      }
  | VOLT ->
      {
        abbr = "VOLT";
        name = "Volgograd Time";
        utc_offset = 4.;
        gmt_offset = 4.;
      }
  | VOST ->
      {
        abbr = "VOST";
        name = "Vostok Station Time";
        utc_offset = 6.;
        gmt_offset = 6.;
      }
  | VUT ->
      {
        abbr = "VUT";
        name = "Vanuatu Time";
        utc_offset = 11.;
        gmt_offset = 11.;
      }
  | WAKT ->
      {
        abbr = "WAKT";
        name = "Wake Island Time";
        utc_offset = 12.;
        gmt_offset = 12.;
      }
  | WAST ->
      {
        abbr = "WAST";
        name = "West Africa Summer Time";
        utc_offset = 2.;
        gmt_offset = 2.;
      }
  | WAT ->
      {
        abbr = "WAT";
        name = "West Africa Time";
        utc_offset = 1.;
        gmt_offset = 1.;
      }
  | WEST ->
      {
        abbr = "WEST";
        name = "Western European Summer Time";
        utc_offset = 1.;
        gmt_offset = 1.;
      }
  | WET ->
      {
        abbr = "WET";
        name = "Western European Time";
        utc_offset = 0.;
        gmt_offset = 0.;
      }
  | WFT ->
      {
        abbr = "WFT";
        name = "Wallis and Futuna Time";
        utc_offset = 12.;
        gmt_offset = 12.;
      }
  | WGST ->
      {
        abbr = "WGST";
        name = "West Greenland Time";
        utc_offset = -3.;
        gmt_offset = -3.;
      }
  | WIB ->
      {
        abbr = "WIB";
        name = "Western Indonesia Time";
        utc_offset = 7.;
        gmt_offset = 7.;
      }
  | WIT ->
      {
        abbr = "WIT";
        name = "Eastern Indonesia Time";
        utc_offset = 9.;
        gmt_offset = 9.;
      }
  | WST ->
      {
        abbr = "WST";
        name = "Western Standard Time";
        utc_offset = 8.;
        gmt_offset = 8.;
      }
  | YAKT ->
      { abbr = "YAKT"; name = "Yakutsk Time"; utc_offset = 9.; gmt_offset = 9. }
  | YEKT ->
      {
        abbr = "YEKT";
        name = "Yekaterinburg Time";
        utc_offset = 5.;
        gmt_offset = 5.;
      }
  | Custom offset ->
      {
        abbr = "CUSTOM";
        name = "Custom";
        utc_offset = Int.to_float offset;
        gmt_offset = Int.to_float offset;
      }
