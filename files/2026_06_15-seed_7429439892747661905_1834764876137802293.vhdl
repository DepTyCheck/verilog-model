-- Seed: 7429439892747661905,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity bgeyos is
  port (ahedytqvb : linkage std_logic; gbir : in time);
end bgeyos;

architecture vtu of bgeyos is
  
begin
  
end vtu;

library ieee;
use ieee.std_logic_1164.all;

entity mtc is
  port (ubvxfb : out std_logic);
end mtc;

architecture ferub of mtc is
  signal f : time;
  signal ikhyj : time;
  signal topkcq : time;
begin
  dqvb : entity work.bgeyos
    port map (ahedytqvb => ubvxfb, gbir => topkcq);
  os : entity work.bgeyos
    port map (ahedytqvb => ubvxfb, gbir => ikhyj);
  ucihwwb : entity work.bgeyos
    port map (ahedytqvb => ubvxfb, gbir => topkcq);
  ei : entity work.bgeyos
    port map (ahedytqvb => ubvxfb, gbir => f);
  
  -- Single-driven assignments
  ikhyj <= 4_1.1_3 us;
  f <= 2#0_0_1.1# fs;
  topkcq <= 2 us;
  
  -- Multi-driven assignments
  ubvxfb <= '1';
  ubvxfb <= 'W';
  ubvxfb <= 'L';
  ubvxfb <= 'W';
end ferub;

library ieee;
use ieee.std_logic_1164.all;

entity pajzvbytxi is
  port (cfoh : buffer bit; vrm : inout std_logic_vector(1 to 1); phoqt : inout real; mjx : in severity_level);
end pajzvbytxi;

library ieee;
use ieee.std_logic_1164.all;

architecture gmrwe of pajzvbytxi is
  signal lcvr : std_logic;
begin
  kdemjm : entity work.mtc
    port map (ubvxfb => lcvr);
  
  -- Single-driven assignments
  phoqt <= 2.203;
  cfoh <= '1';
  
  -- Multi-driven assignments
  lcvr <= '1';
  vrm <= (others => 'W');
  vrm <= "1";
  vrm <= "H";
end gmrwe;



-- Seed after: 17435068320206464510,1834764876137802293
