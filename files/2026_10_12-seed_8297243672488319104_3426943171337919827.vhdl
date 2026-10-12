-- Seed: 8297243672488319104,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity myihz is
  port (n : inout std_logic_vector(4 downto 2));
end myihz;

architecture qmftd of myihz is
  
begin
  -- Multi-driven assignments
  n <= n;
  n <= n;
  n <= ('L', 'U', 'L');
  n <= "ZUW";
end qmftd;

library ieee;
use ieee.std_logic_1164.all;

entity vyveqa is
  port (qq : in std_logic);
end vyveqa;

library ieee;
use ieee.std_logic_1164.all;

architecture qthzg of vyveqa is
  signal aras : std_logic_vector(4 downto 2);
  signal xlmq : std_logic_vector(4 downto 2);
begin
  ivnjiibxf : entity work.myihz
    port map (n => xlmq);
  zct : entity work.myihz
    port map (n => aras);
  
  -- Multi-driven assignments
  aras <= xlmq;
  aras <= ('Z', 'U', 'H');
end qthzg;

entity xppdrtcf is
  port (ehmzuxyb : linkage real; suu : in integer);
end xppdrtcf;

library ieee;
use ieee.std_logic_1164.all;

architecture eosaquggd of xppdrtcf is
  signal epqwz : std_logic_vector(4 downto 2);
  signal unra : std_logic;
  signal cvil : std_logic_vector(4 downto 2);
begin
  gigzlz : entity work.myihz
    port map (n => cvil);
  e : entity work.vyveqa
    port map (qq => unra);
  qja : entity work.myihz
    port map (n => cvil);
  iireqe : entity work.myihz
    port map (n => epqwz);
  
  -- Multi-driven assignments
  unra <= 'L';
  cvil <= "0ZU";
  unra <= '1';
  cvil <= cvil;
end eosaquggd;



-- Seed after: 13370280091307626155,3426943171337919827
