-- Seed: 767144568967467409,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (sfok : in std_logic_vector(0 downto 0); ymclo : linkage std_logic_vector(2 downto 2));
end z;

architecture zynra of z is
  
begin
  
end zynra;

entity wbrcri is
  port (xbhvddf : out real);
end wbrcri;

library ieee;
use ieee.std_logic_1164.all;

architecture ziqpskohf of wbrcri is
  signal xiqnqhh : std_logic_vector(2 downto 2);
begin
  jaevugezb : entity work.z
    port map (sfok => xiqnqhh, ymclo => xiqnqhh);
  
  -- Single-driven assignments
  xbhvddf <= xbhvddf;
  
  -- Multi-driven assignments
  xiqnqhh <= "H";
end ziqpskohf;

entity arxdc is
  port (cpwjqacz : inout real_vector(1 downto 0));
end arxdc;

library ieee;
use ieee.std_logic_1164.all;

architecture g of arxdc is
  signal dlclaw : std_logic_vector(2 downto 2);
  signal dsqiqx : real;
begin
  tojvcuhgoq : entity work.wbrcri
    port map (xbhvddf => dsqiqx);
  wb : entity work.z
    port map (sfok => dlclaw, ymclo => dlclaw);
  
  -- Multi-driven assignments
  dlclaw <= (others => 'H');
end g;



-- Seed after: 12746196023912564330,11481034001933599325
