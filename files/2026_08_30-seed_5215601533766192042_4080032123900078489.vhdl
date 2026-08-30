-- Seed: 5215601533766192042,4080032123900078489

library ieee;
use ieee.std_logic_1164.all;

entity rqmdb is
  port (pdvk : in real; idnxlwsx : buffer std_logic_vector(3 downto 1));
end rqmdb;

architecture mxhdhkt of rqmdb is
  
begin
  
end mxhdhkt;

entity xd is
  port (bokziod : out severity_level);
end xd;

library ieee;
use ieee.std_logic_1164.all;

architecture rshz of xd is
  signal g : std_logic_vector(3 downto 1);
  signal phhcrrjx : std_logic_vector(3 downto 1);
  signal u : real;
begin
  nws : entity work.rqmdb
    port map (pdvk => u, idnxlwsx => phhcrrjx);
  mfzjwo : entity work.rqmdb
    port map (pdvk => u, idnxlwsx => phhcrrjx);
  ppjlbd : entity work.rqmdb
    port map (pdvk => u, idnxlwsx => g);
  
  -- Single-driven assignments
  bokziod <= NOTE;
  u <= 8#4_2.6_4_5#;
  
  -- Multi-driven assignments
  phhcrrjx <= phhcrrjx;
  phhcrrjx <= g;
end rshz;

entity czgekpjdb is
  port (sgyvazcqrv : in boolean; yemda : buffer bit);
end czgekpjdb;

library ieee;
use ieee.std_logic_1164.all;

architecture vqfabv of czgekpjdb is
  signal wrwi : real;
  signal kunidzfzx : severity_level;
  signal djqlddu : std_logic_vector(3 downto 1);
  signal dbeu : real;
  signal fy : severity_level;
begin
  swvdq : entity work.xd
    port map (bokziod => fy);
  gehriq : entity work.rqmdb
    port map (pdvk => dbeu, idnxlwsx => djqlddu);
  gelery : entity work.xd
    port map (bokziod => kunidzfzx);
  akxysryh : entity work.rqmdb
    port map (pdvk => wrwi, idnxlwsx => djqlddu);
  
  -- Single-driven assignments
  wrwi <= dbeu;
  dbeu <= dbeu;
  yemda <= '0';
  
  -- Multi-driven assignments
  djqlddu <= ('L', 'H', 'H');
  djqlddu <= djqlddu;
  djqlddu <= "1WX";
end vqfabv;

entity e is
  port (hzxjvg : in time);
end e;

architecture mo of e is
  
begin
  
end mo;



-- Seed after: 766936674578084811,4080032123900078489
