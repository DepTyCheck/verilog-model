-- Seed: 3950511708296901596,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity vmplccy is
  port (btp : inout std_logic_vector(2 downto 0); pyatcvto : buffer bit_vector(0 to 1));
end vmplccy;

architecture gflf of vmplccy is
  
begin
  -- Multi-driven assignments
  btp <= "-W1";
  btp <= ('Z', 'X', 'X');
  btp <= "ZUL";
end gflf;

library ieee;
use ieee.std_logic_1164.all;

entity fg is
  port (ljm : buffer time_vector(0 to 1); wdveirhqsk : buffer real; deypff : out real; jrbivrlzor : in std_logic);
end fg;

library ieee;
use ieee.std_logic_1164.all;

architecture mzx of fg is
  signal ud : bit_vector(0 to 1);
  signal cqocsrwy : bit_vector(0 to 1);
  signal ucqyu : std_logic_vector(2 downto 0);
  signal j : bit_vector(0 to 1);
  signal lddpwoud : bit_vector(0 to 1);
  signal xvh : std_logic_vector(2 downto 0);
begin
  zjrm : entity work.vmplccy
    port map (btp => xvh, pyatcvto => lddpwoud);
  yaqudojcs : entity work.vmplccy
    port map (btp => xvh, pyatcvto => j);
  z : entity work.vmplccy
    port map (btp => ucqyu, pyatcvto => cqocsrwy);
  zqioso : entity work.vmplccy
    port map (btp => xvh, pyatcvto => ud);
  
  -- Single-driven assignments
  ljm <= (4_0 us, 03.4_1_3_2_1 ps);
  deypff <= 2#0_0_1_0_0.0_1_1_1#;
  
  -- Multi-driven assignments
  xvh <= ('W', 'X', 'W');
  ucqyu <= ('0', '-', 'W');
  xvh <= ('W', 'X', 'Z');
end mzx;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (yxtzcijex : linkage std_logic);
end v;

library ieee;
use ieee.std_logic_1164.all;

architecture twwjm of v is
  signal gaxbja : bit_vector(0 to 1);
  signal ivm : bit_vector(0 to 1);
  signal c : bit_vector(0 to 1);
  signal zbblzjzr : std_logic_vector(2 downto 0);
  signal x : std_logic;
  signal yomvkss : real;
  signal r : real;
  signal omgiqnwh : time_vector(0 to 1);
begin
  vlxjca : entity work.fg
    port map (ljm => omgiqnwh, wdveirhqsk => r, deypff => yomvkss, jrbivrlzor => x);
  knnh : entity work.vmplccy
    port map (btp => zbblzjzr, pyatcvto => c);
  zoyhtl : entity work.vmplccy
    port map (btp => zbblzjzr, pyatcvto => ivm);
  glaeamlnp : entity work.vmplccy
    port map (btp => zbblzjzr, pyatcvto => gaxbja);
end twwjm;



-- Seed after: 13677104897226651308,3108530264173481209
