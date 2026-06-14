-- Seed: 2151282611706514666,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity hpvbqrxz is
  port (xudhb : out time_vector(1 downto 3); zzjmiwz : in std_logic);
end hpvbqrxz;

architecture orpdlhqse of hpvbqrxz is
  
begin
  -- Single-driven assignments
  xudhb <= (others => 0 ns);
end orpdlhqse;

library ieee;
use ieee.std_logic_1164.all;

entity rqlsnvey is
  port (vwhefak : linkage std_logic_vector(3 to 1));
end rqlsnvey;

library ieee;
use ieee.std_logic_1164.all;

architecture eiqsjch of rqlsnvey is
  signal wkmea : std_logic;
  signal wotho : time_vector(1 downto 3);
  signal ywh : std_logic;
  signal blpeblmo : time_vector(1 downto 3);
  signal dbwqqpikj : time_vector(1 downto 3);
  signal vzmbkjzgen : std_logic;
  signal ldxikqpgd : time_vector(1 downto 3);
begin
  hirj : entity work.hpvbqrxz
    port map (xudhb => ldxikqpgd, zzjmiwz => vzmbkjzgen);
  dsk : entity work.hpvbqrxz
    port map (xudhb => dbwqqpikj, zzjmiwz => vzmbkjzgen);
  stux : entity work.hpvbqrxz
    port map (xudhb => blpeblmo, zzjmiwz => ywh);
  taomz : entity work.hpvbqrxz
    port map (xudhb => wotho, zzjmiwz => wkmea);
  
  -- Multi-driven assignments
  wkmea <= 'W';
  vzmbkjzgen <= 'H';
  vzmbkjzgen <= 'U';
end eiqsjch;



-- Seed after: 10407181968976245465,14652815260262078753
