-- Seed: 3745513553621442433,6140041381800297705

library ieee;
use ieee.std_logic_1164.all;

entity nzozu is
  port (quxrkekhvt : out time_vector(1 downto 3); kutl : in time; uydi : in std_logic_vector(3 downto 1));
end nzozu;

architecture ahdwigyh of nzozu is
  
begin
  -- Single-driven assignments
  quxrkekhvt <= (others => 0 ns);
end ahdwigyh;

entity hzfa is
  port (uebq : inout time; tjkmvkr : out boolean_vector(1 to 0); enoiqax : in character; qnhqxynlvf : buffer real_vector(4 to 0));
end hzfa;

library ieee;
use ieee.std_logic_1164.all;

architecture gtrpgtdkzs of hzfa is
  signal dc : std_logic_vector(3 downto 1);
  signal qykvhjou : time_vector(1 downto 3);
  signal kxzie : std_logic_vector(3 downto 1);
  signal zkmsdjpa : time_vector(1 downto 3);
begin
  dxpnd : entity work.nzozu
    port map (quxrkekhvt => zkmsdjpa, kutl => uebq, uydi => kxzie);
  dvb : entity work.nzozu
    port map (quxrkekhvt => qykvhjou, kutl => uebq, uydi => dc);
  
  -- Single-driven assignments
  qnhqxynlvf <= (others => 0.0);
  tjkmvkr <= (others => TRUE);
end gtrpgtdkzs;

library ieee;
use ieee.std_logic_1164.all;

entity mioou is
  port (rbkdd : out time; jzwk : buffer std_logic_vector(0 to 2));
end mioou;

architecture lcmv of mioou is
  
begin
  -- Single-driven assignments
  rbkdd <= rbkdd;
  
  -- Multi-driven assignments
  jzwk <= ('U', '0', 'X');
  jzwk <= jzwk;
  jzwk <= ('Z', '-', 'U');
  jzwk <= "ZW1";
end lcmv;



-- Seed after: 11099177091738636004,6140041381800297705
