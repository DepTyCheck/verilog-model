-- Seed: 6019174078901955836,11481034001933599325

entity bazhuk is
  port (swjg : buffer real_vector(3 to 4));
end bazhuk;

architecture srt of bazhuk is
  
begin
  -- Single-driven assignments
  swjg <= swjg;
end srt;

entity ricixz is
  port (ku : buffer time; mo : in bit; cvywo : inout integer; wwd : buffer time);
end ricixz;

architecture zphntew of ricixz is
  signal ce : real_vector(3 to 4);
  signal ca : real_vector(3 to 4);
  signal twziqoek : real_vector(3 to 4);
  signal ulumgoi : real_vector(3 to 4);
begin
  ctnvcphs : entity work.bazhuk
    port map (swjg => ulumgoi);
  qbirr : entity work.bazhuk
    port map (swjg => twziqoek);
  xbvhfccjl : entity work.bazhuk
    port map (swjg => ca);
  dglktkvc : entity work.bazhuk
    port map (swjg => ce);
  
  -- Single-driven assignments
  wwd <= wwd;
  ku <= 2_4.134 ms;
  cvywo <= 3_2;
end zphntew;

library ieee;
use ieee.std_logic_1164.all;

entity nokfksutz is
  port (ozktnp : in std_logic);
end nokfksutz;

architecture rqnfzlyoyf of nokfksutz is
  signal hlhgxpn : real_vector(3 to 4);
  signal bmrmvsps : time;
  signal xhewdv : integer;
  signal oivb : bit;
  signal scmrezosad : time;
begin
  uq : entity work.ricixz
    port map (ku => scmrezosad, mo => oivb, cvywo => xhewdv, wwd => bmrmvsps);
  y : entity work.bazhuk
    port map (swjg => hlhgxpn);
end rqnfzlyoyf;

library ieee;
use ieee.std_logic_1164.all;

entity zsx is
  port (wpcwgktqvl : out std_logic);
end zsx;

library ieee;
use ieee.std_logic_1164.all;

architecture hitlgdoxfw of zsx is
  signal maif : std_logic;
  signal oeuzkv : real_vector(3 to 4);
  signal f : real_vector(3 to 4);
begin
  qubiqeqwh : entity work.nokfksutz
    port map (ozktnp => wpcwgktqvl);
  nvclkkp : entity work.bazhuk
    port map (swjg => f);
  cghg : entity work.bazhuk
    port map (swjg => oeuzkv);
  qwnh : entity work.nokfksutz
    port map (ozktnp => maif);
  
  -- Multi-driven assignments
  wpcwgktqvl <= 'H';
end hitlgdoxfw;



-- Seed after: 3134090855037754333,11481034001933599325
