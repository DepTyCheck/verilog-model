-- Seed: 1163808664031455903,10594830431004325987

entity d is
  port (zkwbht : in time; sgiiw : inout time);
end d;

architecture igevl of d is
  
begin
  -- Single-driven assignments
  sgiiw <= 2#0_0_0.0_0_1_1# fs;
end igevl;

library ieee;
use ieee.std_logic_1164.all;

entity fseluxavd is
  port (ifwkwp : out std_logic; crwuuw : buffer std_logic_vector(2 to 2));
end fseluxavd;

architecture a of fseluxavd is
  signal audgufx : time;
begin
  hd : entity work.d
    port map (zkwbht => audgufx, sgiiw => audgufx);
  
  -- Multi-driven assignments
  crwuuw <= crwuuw;
  crwuuw <= "H";
end a;



-- Seed after: 8485584776036571801,10594830431004325987
