-- Seed: 12015612182469259697,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity jszv is
  port (bte : inout std_logic_vector(0 to 3); d : buffer std_logic);
end jszv;

architecture gq of jszv is
  
begin
  -- Multi-driven assignments
  d <= 'Z';
  d <= d;
  d <= 'X';
  d <= 'L';
end gq;

library ieee;
use ieee.std_logic_1164.all;

entity trdpk is
  port (s : buffer real; c : buffer bit; areuyvahsz : buffer integer_vector(4 downto 3); pvztvgi : linkage std_logic);
end trdpk;

architecture plwykug of trdpk is
  
begin
  -- Single-driven assignments
  areuyvahsz <= areuyvahsz;
  s <= 231.4;
end plwykug;

entity lzltohclxj is
  port (wc : buffer time);
end lzltohclxj;

library ieee;
use ieee.std_logic_1164.all;

architecture qyenrsk of lzltohclxj is
  signal pl : integer_vector(4 downto 3);
  signal xppu : bit;
  signal qjsnkqhin : real;
  signal lsliilurli : std_logic;
  signal bsdj : std_logic_vector(0 to 3);
begin
  ktsuxzprq : entity work.jszv
    port map (bte => bsdj, d => lsliilurli);
  imxmsye : entity work.trdpk
    port map (s => qjsnkqhin, c => xppu, areuyvahsz => pl, pvztvgi => lsliilurli);
  j : entity work.jszv
    port map (bte => bsdj, d => lsliilurli);
  
  -- Multi-driven assignments
  lsliilurli <= lsliilurli;
  bsdj <= bsdj;
  bsdj <= "-HUZ";
end qyenrsk;

library ieee;
use ieee.std_logic_1164.all;

entity wsrpb is
  port (a : buffer std_logic_vector(4 downto 4); pheghyk : inout std_logic; kdoh : inout time);
end wsrpb;

library ieee;
use ieee.std_logic_1164.all;

architecture zj of wsrpb is
  signal frfjmg : std_logic;
  signal mvwnjxrd : std_logic_vector(0 to 3);
begin
  ntmolcd : entity work.jszv
    port map (bte => mvwnjxrd, d => frfjmg);
  
  -- Single-driven assignments
  kdoh <= 4.231 ps;
  
  -- Multi-driven assignments
  mvwnjxrd <= "U0X-";
  pheghyk <= pheghyk;
  pheghyk <= '0';
end zj;



-- Seed after: 2698923221953305481,7198033922882419595
