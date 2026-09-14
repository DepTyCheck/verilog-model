-- Seed: 1005822787316512612,13196211255131729027

entity buoy is
  port (kdwhrb : in time);
end buoy;

architecture cq of buoy is
  
begin
  
end cq;

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port ( mdimxazpow : in std_logic
  ; lcilayxgui : inout std_logic_vector(2 downto 0)
  ; afvhm : linkage std_logic
  ; bdwxbuzie : buffer std_logic_vector(1 to 1)
  );
end e;

architecture hbd of e is
  
begin
  -- Multi-driven assignments
  lcilayxgui <= ('Z', '0', 'Z');
  lcilayxgui <= ('H', '1', 'U');
  bdwxbuzie <= "Z";
  bdwxbuzie <= bdwxbuzie;
end hbd;

entity pvf is
  port (hslxc : out character; ywhcoqvv : linkage time_vector(1 to 0); pkllomzm : in time);
end pvf;

architecture ory of pvf is
  
begin
  -- Single-driven assignments
  hslxc <= hslxc;
end ory;

library ieee;
use ieee.std_logic_1164.all;

entity qw is
  port (lsox : buffer std_logic_vector(3 to 3); j : in std_logic);
end qw;

library ieee;
use ieee.std_logic_1164.all;

architecture ftz of qw is
  signal u : time;
  signal a : std_logic;
  signal qhrksc : std_logic_vector(2 downto 0);
  signal sjudmllpwo : time;
begin
  orlzrxjy : entity work.buoy
    port map (kdwhrb => sjudmllpwo);
  rtek : entity work.e
    port map (mdimxazpow => j, lcilayxgui => qhrksc, afvhm => j, bdwxbuzie => lsox);
  luxvillzo : entity work.e
    port map (mdimxazpow => j, lcilayxgui => qhrksc, afvhm => a, bdwxbuzie => lsox);
  qnrz : entity work.buoy
    port map (kdwhrb => u);
  
  -- Single-driven assignments
  sjudmllpwo <= 2#0_0_0_0_0.0_1_0_1# us;
  u <= sjudmllpwo;
  
  -- Multi-driven assignments
  lsox <= lsox;
end ftz;



-- Seed after: 6745487037592681485,13196211255131729027
