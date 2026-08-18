-- Seed: 3102527548092493184,5983430343285687595

entity doy is
  port (aribqgw : inout time; lreotsnl : linkage time);
end doy;

architecture ezarlu of doy is
  
begin
  -- Single-driven assignments
  aribqgw <= 1_1_4 fs;
end ezarlu;

entity pzxfkt is
  port (o : out real_vector(2 downto 1); bnsmtrzc : inout integer);
end pzxfkt;

architecture sfonzww of pzxfkt is
  signal rqkzr : time;
  signal flxre : time;
begin
  i : entity work.doy
    port map (aribqgw => flxre, lreotsnl => rqkzr);
  
  -- Single-driven assignments
  o <= o;
  bnsmtrzc <= bnsmtrzc;
end sfonzww;

library ieee;
use ieee.std_logic_1164.all;

entity kgdenzeufr is
  port (swiy : in integer_vector(1 downto 3); lawqtfvps : out std_logic_vector(2 to 2));
end kgdenzeufr;

architecture kqrweuhy of kgdenzeufr is
  signal umunokstj : time;
  signal bkzts : time;
begin
  qswiuowpd : entity work.doy
    port map (aribqgw => bkzts, lreotsnl => umunokstj);
end kqrweuhy;

library ieee;
use ieee.std_logic_1164.all;

entity phsfnd is
  port (rh : inout time; odhw : linkage std_logic; tayx : linkage std_logic_vector(0 to 4); swusnip : inout character);
end phsfnd;

library ieee;
use ieee.std_logic_1164.all;

architecture bvublyw of phsfnd is
  signal qnrrpc : time;
  signal fa : std_logic_vector(2 to 2);
  signal kdlz : integer_vector(1 downto 3);
begin
  ewdilzmtly : entity work.kgdenzeufr
    port map (swiy => kdlz, lawqtfvps => fa);
  fqhacuxv : entity work.doy
    port map (aribqgw => qnrrpc, lreotsnl => rh);
  
  -- Multi-driven assignments
  fa <= fa;
end bvublyw;



-- Seed after: 16824115195500348768,5983430343285687595
