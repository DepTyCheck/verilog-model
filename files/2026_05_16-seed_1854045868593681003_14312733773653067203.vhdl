-- Seed: 1854045868593681003,14312733773653067203



entity ha is
  port (pd : inout integer; moxi : linkage real; d : buffer real);
end ha;



architecture xmq of ha is
  
begin
  
end xmq;



entity vesan is
  port (brz : buffer integer; ykivtugrn : out integer);
end vesan;



architecture lxnxcnii of vesan is
  signal wd : real;
  signal zmshyplm : real;
  signal erbn : real;
  signal wuwuwzqa : integer;
  signal bma : real;
  signal qw : real;
begin
  wnyo : entity work.ha
    port map (pd => brz, moxi => qw, d => bma);
  cvwz : entity work.ha
    port map (pd => wuwuwzqa, moxi => erbn, d => zmshyplm);
  vzgqlwt : entity work.ha
    port map (pd => ykivtugrn, moxi => wd, d => erbn);
end lxnxcnii;

library ieee;
use ieee.std_logic_1164.all;

entity jjflmz is
  port (ptzdssnff : inout integer; vtcd : buffer real; p : out std_logic);
end jjflmz;



architecture q of jjflmz is
  signal vicdlcsr : integer;
begin
  febxvnaq : entity work.vesan
    port map (brz => vicdlcsr, ykivtugrn => ptzdssnff);
end q;



-- Seed after: 12616090354765884579,14312733773653067203
