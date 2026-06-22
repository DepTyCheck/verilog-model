-- Seed: 9689962982511288716,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity un is
  port (gpnnhyvu : buffer time_vector(0 to 1); gwh : inout time; r : linkage std_logic_vector(1 to 4));
end un;

architecture ofpkdajb of un is
  
begin
  -- Single-driven assignments
  gpnnhyvu <= (1_3 ns, 4_2_1 fs);
  gwh <= 110.1 us;
end ofpkdajb;

entity uelsq is
  port (hmfwlkofdi : out real; tqj : in integer; kcxgpmp : linkage severity_level);
end uelsq;

library ieee;
use ieee.std_logic_1164.all;

architecture nklvpihn of uelsq is
  signal wnkto : std_logic_vector(1 to 4);
  signal dmatrtlse : time;
  signal sergvdtngg : time_vector(0 to 1);
  signal pkxvpke : std_logic_vector(1 to 4);
  signal yvtgnsnils : time;
  signal zi : time_vector(0 to 1);
  signal frughekr : std_logic_vector(1 to 4);
  signal d : time;
  signal ltidak : time_vector(0 to 1);
  signal hpp : std_logic_vector(1 to 4);
  signal go : time;
  signal tnqqxa : time_vector(0 to 1);
begin
  ery : entity work.un
    port map (gpnnhyvu => tnqqxa, gwh => go, r => hpp);
  rflsdw : entity work.un
    port map (gpnnhyvu => ltidak, gwh => d, r => frughekr);
  xuci : entity work.un
    port map (gpnnhyvu => zi, gwh => yvtgnsnils, r => pkxvpke);
  immvsr : entity work.un
    port map (gpnnhyvu => sergvdtngg, gwh => dmatrtlse, r => wnkto);
  
  -- Single-driven assignments
  hmfwlkofdi <= 3_2.4412;
  
  -- Multi-driven assignments
  hpp <= ('-', 'L', 'H', 'U');
  hpp <= "LL-H";
end nklvpihn;

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (zcyb : inout time; kcct : buffer integer; rovl : linkage std_logic; ds : in real);
end t;

library ieee;
use ieee.std_logic_1164.all;

architecture yvzpfkf of t is
  signal ccb : std_logic_vector(1 to 4);
  signal j : time;
  signal oyglfko : time_vector(0 to 1);
begin
  gesj : entity work.un
    port map (gpnnhyvu => oyglfko, gwh => j, r => ccb);
  
  -- Single-driven assignments
  kcct <= 8#043#;
  zcyb <= 16#6_B_3_D_9# ps;
  
  -- Multi-driven assignments
  ccb <= ('U', '-', 'X', '1');
  ccb <= "01XH";
  ccb <= ('Z', '-', '-', 'X');
end yvzpfkf;



-- Seed after: 10633120021275672353,13479070923501788437
