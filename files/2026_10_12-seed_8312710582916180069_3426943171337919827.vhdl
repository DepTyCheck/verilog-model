-- Seed: 8312710582916180069,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity pvt is
  port (vg : linkage real_vector(1 downto 2); zravruwror : buffer std_logic; lzu : in time);
end pvt;

architecture mtimn of pvt is
  
begin
  -- Multi-driven assignments
  zravruwror <= 'L';
  zravruwror <= '0';
  zravruwror <= 'U';
end mtimn;

library ieee;
use ieee.std_logic_1164.all;

entity any is
  port (e : linkage real; sr : inout std_logic; sgvmsiy : out bit; enhexcpwy : buffer character);
end any;

library ieee;
use ieee.std_logic_1164.all;

architecture jvona of any is
  signal hidzyaxj : std_logic;
  signal xiaai : real_vector(1 downto 2);
  signal vsgs : time;
  signal qkqbl : real_vector(1 downto 2);
  signal pzbdl : time;
  signal jcx : real_vector(1 downto 2);
  signal shuf : time;
  signal ggfybsh : real_vector(1 downto 2);
begin
  kgvtk : entity work.pvt
    port map (vg => ggfybsh, zravruwror => sr, lzu => shuf);
  mtcuzpegcs : entity work.pvt
    port map (vg => jcx, zravruwror => sr, lzu => pzbdl);
  cgpvggzxm : entity work.pvt
    port map (vg => qkqbl, zravruwror => sr, lzu => vsgs);
  mpybtcry : entity work.pvt
    port map (vg => xiaai, zravruwror => hidzyaxj, lzu => shuf);
  
  -- Single-driven assignments
  enhexcpwy <= enhexcpwy;
  shuf <= shuf;
  
  -- Multi-driven assignments
  sr <= '1';
  hidzyaxj <= 'Z';
  sr <= sr;
  sr <= '-';
end jvona;



-- Seed after: 1107653337123557733,3426943171337919827
