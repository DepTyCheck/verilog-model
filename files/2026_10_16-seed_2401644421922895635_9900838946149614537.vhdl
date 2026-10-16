-- Seed: 2401644421922895635,9900838946149614537

entity anwbeellfv is
  port (cwbdux : in real_vector(3 to 4));
end anwbeellfv;

architecture iq of anwbeellfv is
  
begin
  
end iq;

library ieee;
use ieee.std_logic_1164.all;

entity ji is
  port (hgpvmlwx : linkage std_logic_vector(4 downto 4); xr : inout real);
end ji;

architecture drledov of ji is
  signal pnhil : real_vector(3 to 4);
begin
  lqms : entity work.anwbeellfv
    port map (cwbdux => pnhil);
  dxu : entity work.anwbeellfv
    port map (cwbdux => pnhil);
  d : entity work.anwbeellfv
    port map (cwbdux => pnhil);
end drledov;

library ieee;
use ieee.std_logic_1164.all;

entity ild is
  port (grpuguezjm : inout std_logic_vector(1 downto 3); mw : linkage std_logic; ky : in real);
end ild;

architecture e of ild is
  signal zuemktgwu : real_vector(3 to 4);
  signal rg : real_vector(3 to 4);
begin
  yxopmrbte : entity work.anwbeellfv
    port map (cwbdux => rg);
  kesdik : entity work.anwbeellfv
    port map (cwbdux => zuemktgwu);
  
  -- Single-driven assignments
  zuemktgwu <= rg;
  rg <= (2#1.0#, 2#101.0_0_0_0_1#);
  
  -- Multi-driven assignments
  grpuguezjm <= grpuguezjm;
end e;



-- Seed after: 2687220312157455267,9900838946149614537
