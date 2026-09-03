-- Seed: 2478688180989203503,11127274767545411571

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (msxyqzpsmd : buffer std_logic_vector(4 downto 1); a : in time);
end t;

architecture e of t is
  
begin
  -- Multi-driven assignments
  msxyqzpsmd <= "LWWL";
  msxyqzpsmd <= "LWLX";
  msxyqzpsmd <= msxyqzpsmd;
end e;

library ieee;
use ieee.std_logic_1164.all;

entity nectoq is
  port (gv : out std_logic_vector(1 to 1); eyoknszloq : buffer string(4 downto 4); fhjm : buffer std_logic);
end nectoq;

library ieee;
use ieee.std_logic_1164.all;

architecture ikmkztnbh of nectoq is
  signal e : time;
  signal ljotz : std_logic_vector(4 downto 1);
begin
  izbsjdmaqg : entity work.t
    port map (msxyqzpsmd => ljotz, a => e);
  
  -- Single-driven assignments
  eyoknszloq <= (others => 'g');
  e <= e;
  
  -- Multi-driven assignments
  ljotz <= ('-', 'L', '0', 'Z');
end ikmkztnbh;



-- Seed after: 5496179785099824344,11127274767545411571
