open Base
open Tools

let has_encoded_text text =
  let exp = ~/"&#?[a-zA-Z0-9]{2,10};" in
  Re2.matches exp text

type encode_decode = Encode | Decode
let encode_decode_url reps encode_decode url =
  let folder acc (d, e) =
    let p, w = match encode_decode with Encode -> (d, e) | Decode -> (e, d) in
    String.substr_replace_all acc ~pattern:p ~with_:w
  in
  List.fold reps ~init:url ~f:folder

let encode_decode_text reps encode_decode text =
    let folder acc (unicode_value, number_encoded, name_encoded) =
      match encode_decode with
      | Encode ->
        String.substr_replace_all acc ~pattern:unicode_value ~with_:number_encoded
      | Decode -> (
        acc
        |> String.substr_replace_all ~pattern:number_encoded ~with_:unicode_value
        |> String.substr_replace_all ~pattern:name_encoded ~with_:unicode_value
      )
    in
    List.fold reps ~init:text ~f:folder


let url_reps =
  [ ("%", "%25")
  ; (":", "%3A")
  ; ("/", "%2F")
  ; ("?", "%3F")
  ; ("#", "%23")
  ; ("[", "%5B")
  ; ("]", "%5D")
  ; ("@", "%40")
  ; ("!", "%21")
  ; ("$", "%24")
  ; ("&", "%26")
  ; ("\'", "%27")
  ; ("(", "%28")
  ; (")", "%29")
  ; ("*", "%2A")
  ; ("+", "%2B")
  ; (",", "%2C")
  ; (";", "%3B")
  ; ("=", "%3D")
  ; (" ", "+")
  ]

let text_reps =
  [ ("&", "&#38;", "&amp;")
  ; ("\"", "&#34;", "&quot;")
  ; ("'", "&#39;", "&apos;")
  ; ("<", "&#60;", "&lt;")
  ; (">", "&#62;", "&gt;")

  ; ("Œ", "&#338;", "&OElig;")
  ; ("œ", "&#339;", "&oelig;")
  ; ("Š", "&#352;", "&Scaron;")
  ; ("š", "&#353;", "&scaron;")
  ; ("Ÿ", "&#376;", "&Yuml;")
  ; ("ƒ", "&#402;", "&fnof;")
  ; ("ˆ", "&#710;", "&circ;")
  ; ("˜", "&#732;", "&tilde;")
  ; (" ", "&#8194;", "&ensp;")
  ; (" ", "&#8195;", "&emsp;")
  ; (" ", "&#8201;", "&thinsp;")
  ; ("‌", "&#8204;", "&zwnj;")
  ; ("‍", "&#8205;", "&zwj;")
  ; ("‎", "&#8206;", "&lrm;")
  ; ("‏", "&#8207;", "&rlm;")
  ; ("–", "&#8211;", "&ndash;")
  ; ("—", "&#8212;", "&mdash;")
  ; ("‘", "&#8216;", "&lsquo;")
  ; ("’", "&#8217;", "&rsquo;")
  ; ("‚", "&#8218;", "&sbquo;")
  ; ("“", "&#8220;", "&ldquo;")
  ; ("”", "&#8221;", "&rdquo;")
  ; ("„", "&#8222;", "&bdquo;")
  ; ("†", "&#8224;", "&dagger;")
  ; ("‡", "&#8225;", "&Dagger;")
  ; ("•", "&#8226;", "&bull;")
  ; ("…", "&#8230;", "&hellip;")
  ; ("‰", "&#8240;", "&permil;")
  ; ("′", "&#8242;", "&prime;")
  ; ("″", "&#8243;", "&Prime;")
  ; ("‹", "&#8249;", "&lsaquo;")
  ; ("›", "&#8250;", "&rsaquo;")
  ; ("‾", "&#8254;", "&oline;")
  ; ("€", "&#8364;", "&euro;")
  ; ("™", "&#8482;", "&trade;")
  ; ("←", "&#8592;", "&larr;")
  ; ("↑", "&#8593;", "&uarr;")
  ; ("→", "&#8594;", "&rarr;")
  ; ("↓", "&#8595;", "&darr;")
  ; ("↔", "&#8596;", "&harr;")
  ; ("↵", "&#8629;", "&crarr;")
  ; ("⌈", "&#8968;", "&lceil;")
  ; ("⌉", "&#8969;", "&rceil;")
  ; ("⌊", "&#8970;", "&lfloor;")
  ; ("⌋", "&#8971;", "&rfloor;")
  ; ("◊", "&#9674;", "&loz;")
  ; ("♠", "&#9824;", "&spades;")
  ; ("♣", "&#9827;", "&clubs;")
  ; ("♥", "&#9829;", "&hearts;")
  ; ("♦", "&#9830;", "&diams;")
  ; ("∀", "&#8704;", "&forall;")
  ; ("∂", "&#8706;", "&part;")
  ; ("∃", "&#8707;", "&exist;")
  ; ("∅", "&#8709;", "&empty;")
  ; ("∇", "&#8711;", "&nabla;")
  ; ("∈", "&#8712;", "&isin;")
  ; ("∉", "&#8713;", "&notin;")
  ; ("∋", "&#8715;", "&ni;")
  ; ("∏", "&#8719;", "&prod;")
  ; ("∑", "&#8721;", "&sum;")
  ; ("−", "&#8722;", "&minus;")
  ; ("∗", "&#8727;", "&lowast;")
  ; ("√", "&#8730;", "&radic;")
  ; ("∝", "&#8733;", "&prop;")
  ; ("∞", "&#8734;", "&infin;")
  ; ("∠", "&#8736;", "&ang;")
  ; ("∧", "&#8743;", "&and;")
  ; ("∨", "&#8744;", "&or;")
  ; ("∩", "&#8745;", "&cap;")
  ; ("∪", "&#8746;", "&cup;")
  ; ("∫", "&#8747;", "&int;")
  ; ("∴", "&#8756;", "&there4;")
  ; ("∼", "&#8764;", "&sim;")
  ; ("≅", "&#8773;", "&cong;")
  ; ("≈", "&#8776;", "&asymp;")
  ; ("≠", "&#8800;", "&ne;")
  ; ("≡", "&#8801;", "&equiv;")
  ; ("≤", "&#8804;", "&le;")
  ; ("≥", "&#8805;", "&ge;")
  ; ("⊂", "&#8834;", "&sub;")
  ; ("⊃", "&#8835;", "&sup;")
  ; ("⊄", "&#8836;", "&nsub;")
  ; ("⊆", "&#8838;", "&sube;")
  ; ("⊇", "&#8839;", "&supe;")
  ; ("⊕", "&#8853;", "&oplus;")
  ; ("⊗", "&#8855;", "&otimes;")
  ; ("⊥", "&#8869;", "&perp;")
  ; ("⋅", "&#8901;", "&sdot;")

  ; ("Α", "&#913;", "&Alpha;")
  ; ("Β", "&#914;", "&Beta;")
  ; ("Γ", "&#915;", "&Gamma;")
  ; ("Δ", "&#916;", "&Delta;")
  ; ("Ε", "&#917;", "&Epsilon;")
  ; ("Ζ", "&#918;", "&Zeta;")
  ; ("Η", "&#919;", "&Eta;")
  ; ("Θ", "&#920;", "&Theta;")
  ; ("Ι", "&#921;", "&Iota;")
  ; ("Κ", "&#922;", "&Kappa;")
  ; ("Λ", "&#923;", "&Lambda;")
  ; ("Μ", "&#924;", "&Mu;")
  ; ("Ν", "&#925;", "&Nu;")
  ; ("Ξ", "&#926;", "&Xi;")
  ; ("Ο", "&#927;", "&Omicron;")
  ; ("Π", "&#928;", "&Pi;")
  ; ("Ρ", "&#929;", "&Rho;")
  ; ("Σ", "&#931;", "&Sigma;")
  ; ("Τ", "&#932;", "&Tau;")
  ; ("Υ", "&#933;", "&Upsilon;")
  ; ("Φ", "&#934;", "&Phi;")
  ; ("Χ", "&#935;", "&Chi;")
  ; ("Ψ", "&#936;", "&Psi;")
  ; ("Ω", "&#937;", "&Omega;")
  ; ("α", "&#945;", "&alpha;")
  ; ("β", "&#946;", "&beta;")
  ; ("γ", "&#947;", "&gamma;")
  ; ("δ", "&#948;", "&delta;")
  ; ("ε", "&#949;", "&epsilon;")
  ; ("ζ", "&#950;", "&zeta;")
  ; ("η", "&#951;", "&eta;")
  ; ("θ", "&#952;", "&theta;")
  ; ("ι", "&#953;", "&iota;")
  ; ("κ", "&#954;", "&kappa;")
  ; ("λ", "&#955;", "&lambda;")
  ; ("μ", "&#956;", "&mu;")
  ; ("ν", "&#957;", "&nu;")
  ; ("ξ", "&#958;", "&xi;")
  ; ("ο", "&#959;", "&omicron;")
  ; ("π", "&#960;", "&pi;")
  ; ("ρ", "&#961;", "&rho;")
  ; ("ς", "&#962;", "&sigmaf;")
  ; ("σ", "&#963;", "&sigma;")
  ; ("τ", "&#964;", "&tau;")
  ; ("υ", "&#965;", "&upsilon;")
  ; ("φ", "&#966;", "&phi;")
  ; ("χ", "&#967;", "&chi;")
  ; ("ψ", "&#968;", "&psi;")
  ; ("ω", "&#969;", "&omega;")
  ; ("ϑ", "&#977;", "&thetasym;")
  ; ("ϒ", "&#978;", "&upsih;")
  ; ("ϖ", "&#982;", "&piv;")
  ; ("À", "&#192;", "&Agrave;")
  ; ("Á", "&#193;", "&Aacute;")
  ; ("Â", "&#194;", "&Acirc;")
  ; ("Ã", "&#195;", "&Atilde;")
  ; ("Ä", "&#196;", "&Auml;")
  ; ("Å", "&#197;", "&Aring;")
  ; ("Æ", "&#198;", "&AElig;")
  ; ("Ç", "&#199;", "&Ccedil;")
  ; ("È", "&#200;", "&Egrave;")
  ; ("É", "&#201;", "&Eacute;")
  ; ("Ê", "&#202;", "&Ecirc;")
  ; ("Ë", "&#203;", "&Euml;")
  ; ("Ì", "&#204;", "&Igrave;")
  ; ("Í", "&#205;", "&Iacute;")
  ; ("Î", "&#206;", "&Icirc;")
  ; ("Ï", "&#207;", "&Iuml;")
  ; ("Ð", "&#208;", "&ETH;")
  ; ("Ñ", "&#209;", "&Ntilde;")
  ; ("Ò", "&#210;", "&Ograve;")
  ; ("Ó", "&#211;", "&Oacute;")
  ; ("Ô", "&#212;", "&Ocirc;")
  ; ("Õ", "&#213;", "&Otilde;")
  ; ("Ö", "&#214;", "&Ouml;")
  ; ("Ø", "&#216;", "&Oslash;")
  ; ("Ù", "&#217;", "&Ugrave;")
  ; ("Ú", "&#218;", "&Uacute;")
  ; ("Û", "&#219;", "&Ucirc;")
  ; ("Ü", "&#220;", "&Uuml;")
  ; ("Ý", "&#221;", "&Yacute;")
  ; ("Þ", "&#222;", "&THORN;")
  ; ("ß", "&#223;", "&szlig;")
  ; ("à", "&#224;", "&agrave;")
  ; ("á", "&#225;", "&aacute;")
  ; ("â", "&#226;", "&acirc;")
  ; ("ã", "&#227;", "&atilde;")
  ; ("ä", "&#228;", "&auml;")
  ; ("å", "&#229;", "&aring;")
  ; ("æ", "&#230;", "&aelig;")
  ; ("ç", "&#231;", "&ccedil;")
  ; ("è", "&#232;", "&egrave;")
  ; ("é", "&#233;", "&eacute;")
  ; ("ê", "&#234;", "&ecirc;")
  ; ("ë", "&#235;", "&euml;")
  ; ("ì", "&#236;", "&igrave;")
  ; ("í", "&#237;", "&iacute;")
  ; ("î", "&#238;", "&icirc;")
  ; ("ï", "&#239;", "&iuml;")
  ; ("ð", "&#240;", "&eth;")
  ; ("ñ", "&#241;", "&ntilde;")
  ; ("ò", "&#242;", "&ograve;")
  ; ("ó", "&#243;", "&oacute;")
  ; ("ô", "&#244;", "&ocirc;")
  ; ("õ", "&#245;", "&otilde;")
  ; ("ö", "&#246;", "&ouml;")
  ; ("ø", "&#248;", "&oslash;")
  ; ("ù", "&#249;", "&ugrave;")
  ; ("ú", "&#250;", "&uacute;")
  ; ("û", "&#251;", "&ucirc;")
  ; ("ü", "&#252;", "&uuml;")
  ; ("ý", "&#253;", "&yacute;")
  ; ("þ", "&#254;", "&thorn;")
  ; ("ÿ", "&#255;", "&yuml;")
  ; ("&nbsp;", "&#160;", "&nbsp;")
  ; ("¡", "&#161;", "&iexcl;")
  ; ("¢", "&#162;", "&cent;")
  ; ("£", "&#163;", "&pound;")
  ; ("¤", "&#164;", "&curren;")
  ; ("¥", "&#165;", "&yen;")
  ; ("¦", "&#166;", "&brvbar;")
  ; ("§", "&#167;", "&sect;")
  ; ("¨", "&#168;", "&uml;")
  ; ("©", "&#169;", "&copy;")
  ; ("ª", "&#170;", "&ordf;")
  ; ("«", "&#171;", "&laquo;")
  ; ("¬", "&#172;", "&not;")
  ; ("­", "&#173;", "&shy;")
  ; ("®", "&#174;", "&reg;")
  ; ("¯", "&#175;", "&macr;")
  ; ("°", "&#176;", "&deg;")
  ; ("±", "&#177;", "&plusmn;")
  ; ("²", "&#178;", "&sup2;")
  ; ("³", "&#179;", "&sup3;")
  ; ("´", "&#180;", "&acute;")
  ; ("µ", "&#181;", "&micro;")
  ; ("¶", "&#182;", "&para;")
  ; ("·", "&#183;", "&middot;")
  ; ("¸", "&#184;", "&cedil;")
  ; ("¹", "&#185;", "&sup1;")
  ; ("º", "&#186;", "&ordm;")
  ; ("»", "&#187;", "&raquo;")
  ; ("¼", "&#188;", "&frac14;")
  ; ("½", "&#189;", "&frac12;")
  ; ("¾", "&#190;", "&frac34;")
  ; ("¿", "&#191;", "&iquest;")
  ; ("×", "&#215;", "&times;")
  ; ("÷", "&#247;", "&divide;")
  ]

let encode_url = encode_decode_url url_reps Encode
let decode_url = encode_decode_url url_reps Decode
let encode_text = encode_decode_text text_reps Encode
let decode_text = encode_decode_text text_reps Decode

let escape_url url =
  encode_url url
  |> String.substr_replace_all ~pattern:"+" ~with_:"%20"
  |> String.substr_replace_all ~pattern:"%26" ~with_:"&"

let escape_param_url url =
  encode_url url
  |> String.substr_replace_all ~pattern:"+" ~with_:"%20"
