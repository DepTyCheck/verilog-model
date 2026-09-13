-- Seed: 1434393853566521338,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity chzkf is
  port (wu : out time; guzsasdo : out bit; beiwwsj : in std_logic_vector(2 downto 4));
end chzkf;

architecture qbjbshuxf of chzkf is
  
begin
  -- Single-driven assignments
  guzsasdo <= '0';
  wu <= 1_1_1.1 fs;
end qbjbshuxf;

library ieee;
use ieee.std_logic_1164.all;

entity uvkttm is
  port (fkzz : out std_logic);
end uvkttm;

architecture dafhjrbiz of uvkttm is
  
begin
  -- Multi-driven assignments
  fkzz <= 'X';
  fkzz <= fkzz;
end dafhjrbiz;

entity p is
  port (dx : out integer);
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture fiuw of p is
  signal eseutphfep : std_logic;
  signal dk : std_logic;
  signal pwphz : std_logic_vector(2 downto 4);
  signal jbcxl : bit;
  signal rdatk : time;
begin
  x : entity work.chzkf
    port map (wu => rdatk, guzsasdo => jbcxl, beiwwsj => pwphz);
  ghxzvaswml : entity work.uvkttm
    port map (fkzz => dk);
  bm : entity work.uvkttm
    port map (fkzz => eseutphfep);
  
  -- Single-driven assignments
  dx <= dx;
  
  -- Multi-driven assignments
  pwphz <= (others => '0');
  pwphz <= (others => '0');
end fiuw;



-- Seed after: 15909379300102435922,10754487200446211253
