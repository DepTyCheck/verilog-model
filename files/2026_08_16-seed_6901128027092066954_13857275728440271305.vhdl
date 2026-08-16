-- Seed: 6901128027092066954,13857275728440271305

library ieee;
use ieee.std_logic_1164.all;

entity ttqdh is
  port (z : out std_logic_vector(4 downto 0); xwq : linkage boolean; fxbgqfutv : linkage std_logic; vlbcgj : in time);
end ttqdh;

architecture vbqzzzwuyt of ttqdh is
  
begin
  -- Multi-driven assignments
  z <= "XUHW0";
end vbqzzzwuyt;

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (ue : buffer std_logic);
end h;

architecture qzmixdiw of h is
  
begin
  -- Multi-driven assignments
  ue <= ue;
  ue <= ue;
  ue <= 'Z';
end qzmixdiw;

library ieee;
use ieee.std_logic_1164.all;

entity ayyls is
  port (ffweq : inout time; v : linkage std_logic_vector(4 downto 2); ia : in real; m : linkage real);
end ayyls;

library ieee;
use ieee.std_logic_1164.all;

architecture hrwsedch of ayyls is
  signal msaka : std_logic;
  signal i : boolean;
  signal zfawmfxu : time;
  signal nx : std_logic;
  signal prmwk : boolean;
  signal dyfzj : std_logic_vector(4 downto 0);
begin
  syxoyh : entity work.ttqdh
    port map (z => dyfzj, xwq => prmwk, fxbgqfutv => nx, vlbcgj => zfawmfxu);
  tomdyhiq : entity work.ttqdh
    port map (z => dyfzj, xwq => i, fxbgqfutv => nx, vlbcgj => ffweq);
  yvla : entity work.h
    port map (ue => msaka);
  
  -- Single-driven assignments
  ffweq <= 40.22 ms;
  
  -- Multi-driven assignments
  nx <= 'H';
end hrwsedch;

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (iuqpwnatq : inout std_logic; iamph : out integer_vector(3 to 2));
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture plh of p is
  signal nbjloetfpw : std_logic;
  signal kdlfxlf : real;
  signal mcnv : std_logic_vector(4 downto 2);
  signal affewidlkb : time;
begin
  ebhr : entity work.ayyls
    port map (ffweq => affewidlkb, v => mcnv, ia => kdlfxlf, m => kdlfxlf);
  je : entity work.h
    port map (ue => iuqpwnatq);
  xcs : entity work.h
    port map (ue => nbjloetfpw);
  
  -- Single-driven assignments
  iamph <= (others => 0);
end plh;



-- Seed after: 14292408038975990488,13857275728440271305
