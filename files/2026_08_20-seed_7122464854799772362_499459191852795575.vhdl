-- Seed: 7122464854799772362,499459191852795575

entity qd is
  port (jfncbwd : in time; fwciqidaek : in real; oxpx : in time; ewyzg : inout bit_vector(2 to 2));
end qd;

architecture ar of qd is
  
begin
  -- Single-driven assignments
  ewyzg <= ewyzg;
end ar;

library ieee;
use ieee.std_logic_1164.all;

entity lek is
  port (afb : in std_logic);
end lek;

architecture td of lek is
  
begin
  
end td;

entity j is
  port (awufg : out time_vector(4 downto 2); kt : out time_vector(2 downto 1));
end j;

library ieee;
use ieee.std_logic_1164.all;

architecture arqyhlpbc of j is
  signal eguuvskc : bit_vector(2 to 2);
  signal pmhzkggym : bit_vector(2 to 2);
  signal sq : time;
  signal st : std_logic;
  signal cczwre : bit_vector(2 to 2);
  signal bggg : time;
  signal xjnlmud : real;
  signal mt : time;
begin
  dxq : entity work.qd
    port map (jfncbwd => mt, fwciqidaek => xjnlmud, oxpx => bggg, ewyzg => cczwre);
  grsdb : entity work.lek
    port map (afb => st);
  ochuenobza : entity work.qd
    port map (jfncbwd => bggg, fwciqidaek => xjnlmud, oxpx => sq, ewyzg => pmhzkggym);
  hv : entity work.qd
    port map (jfncbwd => sq, fwciqidaek => xjnlmud, oxpx => bggg, ewyzg => eguuvskc);
  
  -- Single-driven assignments
  kt <= kt;
  sq <= mt;
  mt <= mt;
  awufg <= (3 sec, 2 ns, 2 sec);
  
  -- Multi-driven assignments
  st <= st;
  st <= 'H';
  st <= st;
  st <= '0';
end arqyhlpbc;



-- Seed after: 285502824356697947,499459191852795575
