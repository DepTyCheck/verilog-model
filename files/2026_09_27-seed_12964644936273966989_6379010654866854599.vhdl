-- Seed: 12964644936273966989,6379010654866854599

library ieee;
use ieee.std_logic_1164.all;

entity hmdx is
  port (lp : in std_logic_vector(4 downto 3); ddfyyfngn : in integer; xc : inout time; threeibne : buffer boolean);
end hmdx;

architecture tsaj of hmdx is
  
begin
  
end tsaj;

entity dedtq is
  port (dzdxegp : out bit; qtyyf : out integer);
end dedtq;

library ieee;
use ieee.std_logic_1164.all;

architecture ktdhbwm of dedtq is
  signal zdwcrjkref : boolean;
  signal qqfet : time;
  signal qic : boolean;
  signal zsnhmrza : time;
  signal mwtcyho : integer;
  signal qaxva : std_logic_vector(4 downto 3);
  signal zqo : boolean;
  signal iynhr : time;
  signal zt : integer;
  signal nyxd : boolean;
  signal mokc : time;
  signal diy : std_logic_vector(4 downto 3);
begin
  jgs : entity work.hmdx
    port map (lp => diy, ddfyyfngn => qtyyf, xc => mokc, threeibne => nyxd);
  tk : entity work.hmdx
    port map (lp => diy, ddfyyfngn => zt, xc => iynhr, threeibne => zqo);
  nvozhecsog : entity work.hmdx
    port map (lp => qaxva, ddfyyfngn => mwtcyho, xc => zsnhmrza, threeibne => qic);
  lwnjazdh : entity work.hmdx
    port map (lp => qaxva, ddfyyfngn => qtyyf, xc => qqfet, threeibne => zdwcrjkref);
  
  -- Single-driven assignments
  qtyyf <= 03310;
  mwtcyho <= qtyyf;
  zt <= qtyyf;
  dzdxegp <= dzdxegp;
  
  -- Multi-driven assignments
  diy <= diy;
  qaxva <= diy;
  qaxva <= ('H', 'W');
end ktdhbwm;



-- Seed after: 13720462544232379430,6379010654866854599
