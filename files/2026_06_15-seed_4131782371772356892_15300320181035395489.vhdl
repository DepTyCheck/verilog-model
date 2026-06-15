-- Seed: 4131782371772356892,15300320181035395489

entity vloamxpc is
  port (qlqymkn : in integer; wzzmrgkteq : out integer_vector(1 downto 3); uplpmfgjr : linkage boolean);
end vloamxpc;

architecture vtllkdo of vloamxpc is
  
begin
  -- Single-driven assignments
  wzzmrgkteq <= (others => 0);
end vtllkdo;

library ieee;
use ieee.std_logic_1164.all;

entity wjtjwuj is
  port (sgjzkyxtus : inout std_logic; eobiwxl : buffer std_logic; vhuyy : in time);
end wjtjwuj;

architecture elu of wjtjwuj is
  signal meudranmk : boolean;
  signal coh : integer_vector(1 downto 3);
  signal zd : boolean;
  signal q : integer_vector(1 downto 3);
  signal efgsrulo : boolean;
  signal xsbfektrqi : integer_vector(1 downto 3);
  signal mamkans : boolean;
  signal uadcfdapep : integer_vector(1 downto 3);
  signal pwgsahux : integer;
begin
  kzy : entity work.vloamxpc
    port map (qlqymkn => pwgsahux, wzzmrgkteq => uadcfdapep, uplpmfgjr => mamkans);
  xguosyhxhi : entity work.vloamxpc
    port map (qlqymkn => pwgsahux, wzzmrgkteq => xsbfektrqi, uplpmfgjr => efgsrulo);
  jfkkjlvjg : entity work.vloamxpc
    port map (qlqymkn => pwgsahux, wzzmrgkteq => q, uplpmfgjr => zd);
  lmrhpv : entity work.vloamxpc
    port map (qlqymkn => pwgsahux, wzzmrgkteq => coh, uplpmfgjr => meudranmk);
  
  -- Single-driven assignments
  pwgsahux <= 3_3_2_4;
end elu;



-- Seed after: 1900218637600178145,15300320181035395489
