-- Seed: 1158324027034744480,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity cl is
  port (bxxpl : in time; cgvr : inout std_logic; tfrfn : out time);
end cl;

architecture lrx of cl is
  
begin
  -- Single-driven assignments
  tfrfn <= tfrfn;
  
  -- Multi-driven assignments
  cgvr <= 'X';
  cgvr <= 'Z';
  cgvr <= cgvr;
end lrx;

entity m is
  port (enlvw : buffer time; xvpllxt : buffer real; wq : in real; vbbkonpow : out real_vector(2 to 0));
end m;

library ieee;
use ieee.std_logic_1164.all;

architecture yrgpdz of m is
  signal nxjpghatwx : std_logic;
  signal k : time;
begin
  saouk : entity work.cl
    port map (bxxpl => k, cgvr => nxjpghatwx, tfrfn => k);
  
  -- Single-driven assignments
  enlvw <= 3 sec;
  xvpllxt <= 2#0.11#;
  vbbkonpow <= vbbkonpow;
end yrgpdz;

library ieee;
use ieee.std_logic_1164.all;

entity pmjvqjxyw is
  port (utltrpg : linkage std_logic; vyvagmk : buffer integer);
end pmjvqjxyw;

library ieee;
use ieee.std_logic_1164.all;

architecture hlfv of pmjvqjxyw is
  signal igjq : time;
  signal brbivis : std_logic;
  signal hoklrwzfol : time;
  signal e : real_vector(2 to 0);
  signal ozptzx : real;
  signal ecckl : real;
  signal jo : time;
begin
  xwyygb : entity work.m
    port map (enlvw => jo, xvpllxt => ecckl, wq => ozptzx, vbbkonpow => e);
  ai : entity work.cl
    port map (bxxpl => hoklrwzfol, cgvr => brbivis, tfrfn => igjq);
  
  -- Single-driven assignments
  hoklrwzfol <= 0_2_1_1_0 ms;
  ozptzx <= 2.10314;
  vyvagmk <= 4_0_0_0;
  
  -- Multi-driven assignments
  brbivis <= 'Z';
  brbivis <= 'X';
end hlfv;



-- Seed after: 14744030898649481588,13843488114570579517
