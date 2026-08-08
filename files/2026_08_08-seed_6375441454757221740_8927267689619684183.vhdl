-- Seed: 6375441454757221740,8927267689619684183

library ieee;
use ieee.std_logic_1164.all;

entity czr is
  port (iloddmhf : inout std_logic_vector(2 downto 2); ehrthv : in integer);
end czr;

architecture opotcp of czr is
  
begin
  -- Multi-driven assignments
  iloddmhf <= (others => 'L');
end opotcp;

library ieee;
use ieee.std_logic_1164.all;

entity foa is
  port (dza : inout integer; bfzorgewvq : inout time; iptjtylkc : inout std_logic; zcrt : buffer boolean_vector(4 downto 2));
end foa;

library ieee;
use ieee.std_logic_1164.all;

architecture jmnluwmkjx of foa is
  signal sqghpl : std_logic_vector(2 downto 2);
  signal lixxfarhm : std_logic_vector(2 downto 2);
begin
  hdstyupxho : entity work.czr
    port map (iloddmhf => lixxfarhm, ehrthv => dza);
  legbbpd : entity work.czr
    port map (iloddmhf => lixxfarhm, ehrthv => dza);
  jkhe : entity work.czr
    port map (iloddmhf => sqghpl, ehrthv => dza);
  
  -- Multi-driven assignments
  iptjtylkc <= 'X';
  iptjtylkc <= iptjtylkc;
  lixxfarhm <= (others => 'U');
end jmnluwmkjx;



-- Seed after: 12856331551729840200,8927267689619684183
