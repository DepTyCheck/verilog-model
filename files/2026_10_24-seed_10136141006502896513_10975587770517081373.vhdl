-- Seed: 10136141006502896513,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity eqkdoqiskf is
  port (pmzi : in std_logic_vector(2 downto 4); jsiowyc : out std_logic; kljotgw : in time; mfrrlw : inout std_logic_vector(1 to 3));
end eqkdoqiskf;

architecture pvuz of eqkdoqiskf is
  
begin
  -- Multi-driven assignments
  jsiowyc <= 'Z';
end pvuz;

library ieee;
use ieee.std_logic_1164.all;

entity epttzemgvz is
  port (mviolvf : inout real; vdaamn : in integer_vector(4 downto 4); ude : inout std_logic; emtvwvkzb : buffer std_logic);
end epttzemgvz;

library ieee;
use ieee.std_logic_1164.all;

architecture bdygbrmfb of epttzemgvz is
  signal yi : std_logic;
  signal rqcfzjxg : std_logic_vector(1 to 3);
  signal t : time;
  signal za : std_logic_vector(2 downto 4);
begin
  j : entity work.eqkdoqiskf
    port map (pmzi => za, jsiowyc => emtvwvkzb, kljotgw => t, mfrrlw => rqcfzjxg);
  mz : entity work.eqkdoqiskf
    port map (pmzi => za, jsiowyc => yi, kljotgw => t, mfrrlw => rqcfzjxg);
  
  -- Single-driven assignments
  t <= 8#3.4_3_3_0_6# ms;
  
  -- Multi-driven assignments
  emtvwvkzb <= ude;
end bdygbrmfb;



-- Seed after: 4658384628683430307,10975587770517081373
