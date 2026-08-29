-- Seed: 2539314012669028161,10463297573877745897

entity abkubiqr is
  port (mf : out time_vector(0 downto 4));
end abkubiqr;

architecture vg of abkubiqr is
  
begin
  -- Single-driven assignments
  mf <= mf;
end vg;

library ieee;
use ieee.std_logic_1164.all;

entity rcuxfinrvi is
  port (btsd : linkage integer; oznpshj : in std_logic_vector(2 downto 3); kby : buffer real; uonobnbhmz : linkage real);
end rcuxfinrvi;

architecture q of rcuxfinrvi is
  signal e : time_vector(0 downto 4);
  signal vhmbprcbbu : time_vector(0 downto 4);
begin
  jgojswqm : entity work.abkubiqr
    port map (mf => vhmbprcbbu);
  btpzrfplxp : entity work.abkubiqr
    port map (mf => e);
  
  -- Single-driven assignments
  kby <= 16#CEAB.8D#;
end q;

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (gss : buffer time_vector(2 downto 4); nkudayqmi : in std_logic);
end t;

library ieee;
use ieee.std_logic_1164.all;

architecture fn of t is
  signal ufscaxf : real;
  signal pc : real;
  signal vu : std_logic_vector(2 downto 3);
  signal ozsm : integer;
begin
  bezvs : entity work.rcuxfinrvi
    port map (btsd => ozsm, oznpshj => vu, kby => pc, uonobnbhmz => ufscaxf);
  
  -- Single-driven assignments
  gss <= gss;
end fn;



-- Seed after: 1368116059566079917,10463297573877745897
