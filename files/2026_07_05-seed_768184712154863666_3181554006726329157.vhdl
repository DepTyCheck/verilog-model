-- Seed: 768184712154863666,3181554006726329157

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity wzhza is
  port (hxqr : in real; neamzrf : inout protected_value_mirror; r : out std_logic_vector(3 to 4));
end wzhza;

architecture czbgi of wzhza is
  
begin
  
end czbgi;

entity ilczasz is
  port (mfusiumo : in integer_vector(0 downto 3); yxbdzwvrg : linkage integer; adwqniqy : out time);
end ilczasz;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture regodznj of ilczasz is
  shared variable iflz : protected_value_mirror;
  signal rgzj : std_logic_vector(3 to 4);
  shared variable hfpaxcbpo : protected_value_mirror;
  signal nfmlkwbsr : real;
  signal zzpeqkcdw : std_logic_vector(3 to 4);
  shared variable y : protected_value_mirror;
  signal mnbt : real;
begin
  qe : entity work.wzhza
    port map (hxqr => mnbt, neamzrf => y, r => zzpeqkcdw);
  opcd : entity work.wzhza
    port map (hxqr => nfmlkwbsr, neamzrf => hfpaxcbpo, r => rgzj);
  dpto : entity work.wzhza
    port map (hxqr => mnbt, neamzrf => iflz, r => zzpeqkcdw);
  
  -- Single-driven assignments
  nfmlkwbsr <= 0_3_2_0.1_4;
  mnbt <= 16#F_B_6_E.5_4#;
  adwqniqy <= adwqniqy;
  
  -- Multi-driven assignments
  zzpeqkcdw <= zzpeqkcdw;
  zzpeqkcdw <= ('X', 'U');
end regodznj;



-- Seed after: 1520541410085082241,3181554006726329157
