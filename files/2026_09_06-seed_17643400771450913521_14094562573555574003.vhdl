-- Seed: 17643400771450913521,14094562573555574003

library ieee;
use ieee.std_logic_1164.all;

entity bf is
  port (ltuupctta : inout bit_vector(2 to 0); gth : buffer std_logic);
end bf;

architecture zqrg of bf is
  
begin
  -- Multi-driven assignments
  gth <= gth;
end zqrg;

library ieee;
use ieee.std_logic_1164.all;

entity nnacvfwh is
  port (wmsptyuif : out integer; trfy : out boolean_vector(1 downto 2); c : in std_logic);
end nnacvfwh;

library ieee;
use ieee.std_logic_1164.all;

architecture kdt of nnacvfwh is
  signal zhkrzrjlpc : std_logic;
  signal cbflawbhz : bit_vector(2 to 0);
  signal cqaijua : bit_vector(2 to 0);
  signal epmrvcmp : std_logic;
  signal ijh : bit_vector(2 to 0);
begin
  dicfpdo : entity work.bf
    port map (ltuupctta => ijh, gth => epmrvcmp);
  grc : entity work.bf
    port map (ltuupctta => cqaijua, gth => epmrvcmp);
  fao : entity work.bf
    port map (ltuupctta => cbflawbhz, gth => zhkrzrjlpc);
  
  -- Single-driven assignments
  trfy <= (others => TRUE);
  
  -- Multi-driven assignments
  epmrvcmp <= 'W';
  epmrvcmp <= c;
  zhkrzrjlpc <= 'X';
end kdt;



-- Seed after: 9600010273691785248,14094562573555574003
