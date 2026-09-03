-- Seed: 4577185009989733967,11127274767545411571

entity od is
  port (zx : linkage real; yucxswyv : in time; xlvkmt : linkage time_vector(4 downto 4); flmnitifc : in real_vector(0 to 2));
end od;

architecture y of od is
  
begin
  
end y;

library ieee;
use ieee.std_logic_1164.all;

entity xcv is
  port (fl : out std_logic_vector(3 to 2); mecjti : linkage std_logic; tdsvm : buffer real);
end xcv;

architecture cbjlc of xcv is
  signal lgowasyjw : time_vector(4 downto 4);
  signal okrvoaolfh : real;
  signal yopb : time_vector(4 downto 4);
  signal u : real;
  signal tfcbb : time_vector(4 downto 4);
  signal cvwer : real_vector(0 to 2);
  signal nxoja : time_vector(4 downto 4);
  signal txbcfkz : time;
  signal krodvy : real;
begin
  gui : entity work.od
    port map (zx => krodvy, yucxswyv => txbcfkz, xlvkmt => nxoja, flmnitifc => cvwer);
  bpiibo : entity work.od
    port map (zx => tdsvm, yucxswyv => txbcfkz, xlvkmt => tfcbb, flmnitifc => cvwer);
  nsjoln : entity work.od
    port map (zx => u, yucxswyv => txbcfkz, xlvkmt => yopb, flmnitifc => cvwer);
  wzuriz : entity work.od
    port map (zx => okrvoaolfh, yucxswyv => txbcfkz, xlvkmt => lgowasyjw, flmnitifc => cvwer);
  
  -- Single-driven assignments
  txbcfkz <= 0 min;
  
  -- Multi-driven assignments
  fl <= fl;
end cbjlc;



-- Seed after: 5166078138081386298,11127274767545411571
