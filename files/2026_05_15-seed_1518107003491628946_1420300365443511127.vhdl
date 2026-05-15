-- Seed: 1518107003491628946,1420300365443511127



entity a is
  port (xaoe : linkage time; bzbwzz : out integer);
end a;



architecture dnkqavnoc of a is
  
begin
  
end dnkqavnoc;



entity wn is
  port (g : inout boolean; kvkxnmz : in integer);
end wn;



architecture mrx of wn is
  signal xkyykmzma : integer;
  signal xu : time;
begin
  d : entity work.a
    port map (xaoe => xu, bzbwzz => xkyykmzma);
end mrx;



entity yyimpr is
  port (txwnbf : inout bit);
end yyimpr;



architecture uccvbomeff of yyimpr is
  signal bwgev : integer;
  signal kjsaigxhju : boolean;
begin
  rwzpgyr : entity work.wn
    port map (g => kjsaigxhju, kvkxnmz => bwgev);
end uccvbomeff;

library ieee;
use ieee.std_logic_1164.all;

entity jxdsotwfs is
  port (yiakbchly : inout time; vzfiukzih : in integer; o : linkage std_logic; v : buffer std_logic);
end jxdsotwfs;



architecture q of jxdsotwfs is
  signal qvrbrawu : integer;
  signal xu : bit;
  signal oegvuyh : integer;
  signal ow : integer;
begin
  trsqrvoc : entity work.a
    port map (xaoe => yiakbchly, bzbwzz => ow);
  ekydavmoxs : entity work.a
    port map (xaoe => yiakbchly, bzbwzz => oegvuyh);
  huuafnuj : entity work.yyimpr
    port map (txwnbf => xu);
  tsyff : entity work.a
    port map (xaoe => yiakbchly, bzbwzz => qvrbrawu);
end q;



-- Seed after: 2035945789179380851,1420300365443511127
