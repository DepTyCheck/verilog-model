-- Seed: 13258813327351009158,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity olp is
  port (bxii : inout std_logic_vector(1 downto 0); rqgah : in std_logic);
end olp;

architecture rc of olp is
  
begin
  
end rc;

entity bdpazjh is
  port (vmwbtjr : buffer real; r : buffer time);
end bdpazjh;

architecture bodhzrih of bdpazjh is
  
begin
  -- Single-driven assignments
  vmwbtjr <= 16#3_2.B_2_F_5#;
end bodhzrih;

entity vvev is
  port (chjhpwf : linkage boolean; z : in time; xcjd : inout integer_vector(4 downto 2));
end vvev;

library ieee;
use ieee.std_logic_1164.all;

architecture zhxwcwnj of vvev is
  signal zf : std_logic;
  signal sjq : std_logic;
  signal oym : std_logic;
  signal ixfx : std_logic_vector(1 downto 0);
begin
  tsldbcdw : entity work.olp
    port map (bxii => ixfx, rqgah => oym);
  iqg : entity work.olp
    port map (bxii => ixfx, rqgah => sjq);
  ymdeb : entity work.olp
    port map (bxii => ixfx, rqgah => zf);
  
  -- Single-driven assignments
  xcjd <= (8#631#, 8#5_6#, 16#CE5#);
  
  -- Multi-driven assignments
  sjq <= '1';
  ixfx <= "HU";
end zhxwcwnj;



-- Seed after: 12585716913965253658,14629254427735353553
