-- Seed: 9810166532948517378,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity ksncrsq is
  port (znryreys : in time; yuuyk : out std_logic);
end ksncrsq;

architecture vencihw of ksncrsq is
  
begin
  -- Multi-driven assignments
  yuuyk <= 'L';
  yuuyk <= 'Z';
end vencihw;

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (czbexpfliv : in std_logic);
end x;

library ieee;
use ieee.std_logic_1164.all;

architecture ocwnlwtj of x is
  signal xayiahpufu : time;
  signal ivijkcjbdr : std_logic;
  signal taese : time;
begin
  wwaud : entity work.ksncrsq
    port map (znryreys => taese, yuuyk => ivijkcjbdr);
  zisdv : entity work.ksncrsq
    port map (znryreys => xayiahpufu, yuuyk => ivijkcjbdr);
  
  -- Multi-driven assignments
  ivijkcjbdr <= czbexpfliv;
  ivijkcjbdr <= '-';
  ivijkcjbdr <= czbexpfliv;
  ivijkcjbdr <= 'X';
end ocwnlwtj;

entity elxpqrlq is
  port (ysatgnitm : out boolean);
end elxpqrlq;

library ieee;
use ieee.std_logic_1164.all;

architecture ujb of elxpqrlq is
  signal ctznxsr : std_logic;
begin
  jxr : entity work.x
    port map (czbexpfliv => ctznxsr);
  
  -- Multi-driven assignments
  ctznxsr <= ctznxsr;
  ctznxsr <= 'L';
  ctznxsr <= ctznxsr;
  ctznxsr <= ctznxsr;
end ujb;



-- Seed after: 13695708283214315384,13501862637168280927
