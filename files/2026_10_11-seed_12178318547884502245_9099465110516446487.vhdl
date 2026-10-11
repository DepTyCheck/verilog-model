-- Seed: 12178318547884502245,9099465110516446487

entity hhjktsy is
  port (fwqquoctx : buffer string(2 downto 4));
end hhjktsy;

architecture xpko of hhjktsy is
  
begin
  
end xpko;

library ieee;
use ieee.std_logic_1164.all;

entity eiwk is
  port (bk : inout std_logic; gs : in std_logic);
end eiwk;

architecture torz of eiwk is
  signal etfxvrmko : string(2 downto 4);
begin
  lattqud : entity work.hhjktsy
    port map (fwqquoctx => etfxvrmko);
  
  -- Multi-driven assignments
  bk <= bk;
  bk <= gs;
end torz;

library ieee;
use ieee.std_logic_1164.all;

entity lrhwniaa is
  port (cgon : linkage std_logic; godkr : buffer boolean_vector(3 downto 4));
end lrhwniaa;

architecture legvoyrfh of lrhwniaa is
  signal qmnmutthgx : string(2 downto 4);
begin
  cqe : entity work.hhjktsy
    port map (fwqquoctx => qmnmutthgx);
  
  -- Single-driven assignments
  godkr <= (others => TRUE);
end legvoyrfh;

library ieee;
use ieee.std_logic_1164.all;

entity wnsdh is
  port (oli : in std_logic_vector(3 to 2); qfoej : out std_logic_vector(0 to 4); de : inout std_logic);
end wnsdh;

library ieee;
use ieee.std_logic_1164.all;

architecture kxdagjlokm of wnsdh is
  signal fsbjl : string(2 downto 4);
  signal xpswejxbib : boolean_vector(3 downto 4);
  signal iwkuxw : std_logic;
begin
  cctk : entity work.lrhwniaa
    port map (cgon => iwkuxw, godkr => xpswejxbib);
  ayszxdxu : entity work.hhjktsy
    port map (fwqquoctx => fsbjl);
  
  -- Multi-driven assignments
  de <= de;
end kxdagjlokm;



-- Seed after: 16402462962006320042,9099465110516446487
