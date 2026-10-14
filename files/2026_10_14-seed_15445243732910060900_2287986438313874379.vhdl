-- Seed: 15445243732910060900,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (ibfrryxme : inout std_logic_vector(2 downto 2));
end z;

architecture eexnou of z is
  
begin
  -- Multi-driven assignments
  ibfrryxme <= (others => 'Z');
end eexnou;

entity oewaqekk is
  port (hlm : inout time; vpmyaopjg : inout time);
end oewaqekk;

library ieee;
use ieee.std_logic_1164.all;

architecture igfpex of oewaqekk is
  signal llbrp : std_logic_vector(2 downto 2);
begin
  jcw : entity work.z
    port map (ibfrryxme => llbrp);
  tw : entity work.z
    port map (ibfrryxme => llbrp);
  
  -- Single-driven assignments
  vpmyaopjg <= 2#1_0_0_1_1# ns;
  hlm <= 2#01.0# fs;
  
  -- Multi-driven assignments
  llbrp <= "Z";
  llbrp <= llbrp;
  llbrp <= (others => 'L');
  llbrp <= llbrp;
end igfpex;



-- Seed after: 16063775848600222487,2287986438313874379
