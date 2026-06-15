-- Seed: 9062836839241369459,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity lnrm is
  port (heawbvp : inout std_logic_vector(0 downto 0); yly : linkage real);
end lnrm;

architecture rpvrsyxxww of lnrm is
  
begin
  -- Multi-driven assignments
  heawbvp <= "U";
  heawbvp <= "0";
  heawbvp <= (others => 'W');
end rpvrsyxxww;

entity airn is
  port (vtvjzdb : inout time; yqioyyqe : inout time_vector(2 downto 3); ye : linkage time; d : linkage real);
end airn;

library ieee;
use ieee.std_logic_1164.all;

architecture qz of airn is
  signal zndsgpsgk : real;
  signal rl : real;
  signal tqii : std_logic_vector(0 downto 0);
begin
  qwbzkklh : entity work.lnrm
    port map (heawbvp => tqii, yly => rl);
  ofhk : entity work.lnrm
    port map (heawbvp => tqii, yly => zndsgpsgk);
  z : entity work.lnrm
    port map (heawbvp => tqii, yly => d);
  
  -- Multi-driven assignments
  tqii <= "1";
  tqii <= "U";
  tqii <= (others => 'W');
  tqii <= (others => 'H');
end qz;



-- Seed after: 3265865328517451211,15300320181035395489
