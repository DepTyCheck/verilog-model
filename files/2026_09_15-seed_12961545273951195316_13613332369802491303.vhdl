-- Seed: 12961545273951195316,13613332369802491303

entity w is
  port (xatec : inout integer_vector(2 to 4));
end w;

architecture tigasqz of w is
  
begin
  -- Single-driven assignments
  xatec <= (16#2_4_6#, 8#3_7#, 2#01#);
end tigasqz;

library ieee;
use ieee.std_logic_1164.all;

entity veqkbrg is
  port (jknsti : linkage integer; osrmbuai : in std_logic_vector(1 downto 2); wkntrxwusj : out real);
end veqkbrg;

architecture yzprhcmuic of veqkbrg is
  
begin
  -- Single-driven assignments
  wkntrxwusj <= 8#3.0_5#;
end yzprhcmuic;

entity sihgilg is
  port (lliknjxcem : linkage integer_vector(4 downto 2); keywse : linkage time);
end sihgilg;

library ieee;
use ieee.std_logic_1164.all;

architecture gbswco of sihgilg is
  signal grhopme : integer_vector(2 to 4);
  signal ck : real;
  signal nvc : integer;
  signal heiqxam : real;
  signal yn : std_logic_vector(1 downto 2);
  signal dcade : integer;
begin
  xxmenv : entity work.veqkbrg
    port map (jknsti => dcade, osrmbuai => yn, wkntrxwusj => heiqxam);
  bhcpwzz : entity work.veqkbrg
    port map (jknsti => nvc, osrmbuai => yn, wkntrxwusj => ck);
  rzjpbkyw : entity work.w
    port map (xatec => grhopme);
  
  -- Multi-driven assignments
  yn <= yn;
  yn <= yn;
end gbswco;

library ieee;
use ieee.std_logic_1164.all;

entity wduyuh is
  port (kbzoukyaz : buffer integer; s : linkage real; dqm : inout std_logic; vs : buffer bit_vector(0 to 1));
end wduyuh;

architecture bye of wduyuh is
  signal fqzq : integer_vector(2 to 4);
  signal mucb : integer_vector(2 to 4);
begin
  a : entity work.w
    port map (xatec => mucb);
  n : entity work.w
    port map (xatec => fqzq);
  
  -- Single-driven assignments
  vs <= ('1', '1');
  kbzoukyaz <= 20302;
  
  -- Multi-driven assignments
  dqm <= dqm;
end bye;



-- Seed after: 5384952595556894828,13613332369802491303
