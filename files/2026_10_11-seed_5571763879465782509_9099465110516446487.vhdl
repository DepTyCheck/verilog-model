-- Seed: 5571763879465782509,9099465110516446487

library ieee;
use ieee.std_logic_1164.all;

entity rdgsj is
  port (bd : inout boolean; wwsc : out std_logic; w : linkage std_logic_vector(1 downto 1));
end rdgsj;

architecture gtnjzs of rdgsj is
  
begin
  -- Single-driven assignments
  bd <= bd;
  
  -- Multi-driven assignments
  wwsc <= '1';
  wwsc <= '0';
  wwsc <= wwsc;
end gtnjzs;

library ieee;
use ieee.std_logic_1164.all;

entity uydmyctos is
  port (u : buffer boolean; hoyyiv : buffer real; wpncvytz : out std_logic_vector(1 to 3); fnikg : out std_logic);
end uydmyctos;

library ieee;
use ieee.std_logic_1164.all;

architecture exch of uydmyctos is
  signal y : std_logic_vector(1 downto 1);
  signal phssipd : std_logic;
  signal av : boolean;
begin
  amro : entity work.rdgsj
    port map (bd => av, wwsc => phssipd, w => y);
  
  -- Multi-driven assignments
  phssipd <= fnikg;
  y <= (others => 'H');
  fnikg <= fnikg;
end exch;



-- Seed after: 895620793012747726,9099465110516446487
