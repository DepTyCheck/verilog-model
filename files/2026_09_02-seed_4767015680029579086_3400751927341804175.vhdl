-- Seed: 4767015680029579086,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity ee is
  port (y : buffer time; zf : out std_logic_vector(0 to 1));
end ee;

architecture t of ee is
  
begin
  -- Single-driven assignments
  y <= y;
  
  -- Multi-driven assignments
  zf <= zf;
  zf <= "LZ";
end t;

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (ykwrx : in integer; xuarcuuwsc : linkage string(3 to 3); z : inout std_logic_vector(0 to 0));
end u;

library ieee;
use ieee.std_logic_1164.all;

architecture rdknzd of u is
  signal urc : std_logic_vector(0 to 1);
  signal wodpp : time;
  signal dwapbv : time;
  signal zkhyns : std_logic_vector(0 to 1);
  signal px : time;
  signal vzpnglczrg : std_logic_vector(0 to 1);
  signal nnkljc : time;
begin
  qkoupir : entity work.ee
    port map (y => nnkljc, zf => vzpnglczrg);
  brsqzblsq : entity work.ee
    port map (y => px, zf => zkhyns);
  ghgxicevr : entity work.ee
    port map (y => dwapbv, zf => zkhyns);
  ztf : entity work.ee
    port map (y => wodpp, zf => urc);
  
  -- Multi-driven assignments
  vzpnglczrg <= vzpnglczrg;
  z <= z;
  z <= "L";
end rdknzd;



-- Seed after: 11073398848073951350,3400751927341804175
