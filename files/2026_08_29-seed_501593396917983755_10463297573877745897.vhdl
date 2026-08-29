-- Seed: 501593396917983755,10463297573877745897

library ieee;
use ieee.std_logic_1164.all;

entity rkyjzmppdc is
  port (olcnmla : out std_logic_vector(2 downto 2); yl : inout bit_vector(2 downto 2); zgh : out std_logic_vector(3 downto 4));
end rkyjzmppdc;

architecture mdhfl of rkyjzmppdc is
  
begin
  -- Single-driven assignments
  yl <= (others => '1');
  
  -- Multi-driven assignments
  olcnmla <= "X";
  zgh <= (others => '0');
end mdhfl;

library ieee;
use ieee.std_logic_1164.all;

entity uxi is
  port (acfbkz : inout bit; hxtnfiq : buffer std_logic; tlheanh : out bit; dcx : linkage real);
end uxi;

library ieee;
use ieee.std_logic_1164.all;

architecture nkhmexu of uxi is
  signal y : bit_vector(2 downto 2);
  signal gjlcdtmhao : std_logic_vector(2 downto 2);
  signal yuqtxkwe : std_logic_vector(3 downto 4);
  signal mggpwj : bit_vector(2 downto 2);
  signal ap : std_logic_vector(2 downto 2);
begin
  cuaor : entity work.rkyjzmppdc
    port map (olcnmla => ap, yl => mggpwj, zgh => yuqtxkwe);
  yyy : entity work.rkyjzmppdc
    port map (olcnmla => gjlcdtmhao, yl => y, zgh => yuqtxkwe);
  
  -- Single-driven assignments
  tlheanh <= tlheanh;
  acfbkz <= '0';
end nkhmexu;



-- Seed after: 16217679139254451903,10463297573877745897
