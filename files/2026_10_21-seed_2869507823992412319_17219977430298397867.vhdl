-- Seed: 2869507823992412319,17219977430298397867

entity ulkhe is
  port (ccbehchsun : buffer time);
end ulkhe;

architecture my of ulkhe is
  
begin
  -- Single-driven assignments
  ccbehchsun <= ccbehchsun;
end my;

library ieee;
use ieee.std_logic_1164.all;

entity jkpn is
  port (bmbdehb : buffer std_logic_vector(3 to 3));
end jkpn;

architecture uqu of jkpn is
  signal ptxfed : time;
begin
  ebypkanl : entity work.ulkhe
    port map (ccbehchsun => ptxfed);
  
  -- Multi-driven assignments
  bmbdehb <= bmbdehb;
  bmbdehb <= bmbdehb;
  bmbdehb <= "X";
end uqu;



-- Seed after: 16433760807517480472,17219977430298397867
