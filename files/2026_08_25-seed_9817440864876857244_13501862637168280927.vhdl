-- Seed: 9817440864876857244,13501862637168280927

entity fzjzt is
  port (amxkwgzlb : inout time; rqnonz : buffer real);
end fzjzt;

architecture zdaxex of fzjzt is
  
begin
  -- Single-driven assignments
  amxkwgzlb <= amxkwgzlb;
  rqnonz <= rqnonz;
end zdaxex;

entity eonfum is
  port (uhiwnj : in time; ogxj : linkage integer);
end eonfum;

architecture vbv of eonfum is
  signal zlmfzx : real;
  signal npfs : time;
begin
  dimgrdlw : entity work.fzjzt
    port map (amxkwgzlb => npfs, rqnonz => zlmfzx);
end vbv;

library ieee;
use ieee.std_logic_1164.all;

entity ecwfge is
  port (kzhs : inout boolean_vector(4 to 3); googgjxnj : linkage std_logic_vector(2 to 3));
end ecwfge;

architecture ma of ecwfge is
  signal ncnss : integer;
  signal gcfprrx : real;
  signal wjps : time;
begin
  vifwh : entity work.fzjzt
    port map (amxkwgzlb => wjps, rqnonz => gcfprrx);
  n : entity work.eonfum
    port map (uhiwnj => wjps, ogxj => ncnss);
  
  -- Single-driven assignments
  kzhs <= (others => TRUE);
end ma;



-- Seed after: 18216166088149964095,13501862637168280927
