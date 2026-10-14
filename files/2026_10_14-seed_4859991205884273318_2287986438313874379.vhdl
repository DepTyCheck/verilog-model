-- Seed: 4859991205884273318,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity nphzraf is
  port (hgfa : linkage integer; dyypoc : linkage std_logic_vector(0 to 2); bir : inout std_logic_vector(3 to 1));
end nphzraf;

architecture ipd of nphzraf is
  
begin
  
end ipd;

entity hpyyaunk is
  port (hyushehz : inout real; bkgmqakhay : buffer integer; iiowis : inout time);
end hpyyaunk;

library ieee;
use ieee.std_logic_1164.all;

architecture zbnxsmrry of hpyyaunk is
  signal glpkvfirq : std_logic_vector(3 to 1);
  signal zgpvjxrd : integer;
  signal l : std_logic_vector(0 to 2);
  signal brdnt : integer;
  signal fnusld : std_logic_vector(3 to 1);
  signal wo : std_logic_vector(0 to 2);
  signal r : integer;
begin
  rygsqofu : entity work.nphzraf
    port map (hgfa => r, dyypoc => wo, bir => fnusld);
  qt : entity work.nphzraf
    port map (hgfa => bkgmqakhay, dyypoc => wo, bir => fnusld);
  rqx : entity work.nphzraf
    port map (hgfa => brdnt, dyypoc => l, bir => fnusld);
  ew : entity work.nphzraf
    port map (hgfa => zgpvjxrd, dyypoc => wo, bir => glpkvfirq);
  
  -- Single-driven assignments
  iiowis <= 16#68E0E# ms;
  hyushehz <= 2#1.10000#;
  
  -- Multi-driven assignments
  fnusld <= (others => '0');
end zbnxsmrry;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (vlgdriy : in real; tfriku : out std_logic; hwvbh : inout integer_vector(2 to 2));
end f;

architecture kbietks of f is
  
begin
  -- Single-driven assignments
  hwvbh <= hwvbh;
  
  -- Multi-driven assignments
  tfriku <= '0';
  tfriku <= 'Z';
end kbietks;

entity epjthplgbu is
  port (veflkljoh : buffer time);
end epjthplgbu;

library ieee;
use ieee.std_logic_1164.all;

architecture jux of epjthplgbu is
  signal rq : integer;
  signal tvwkoncwvm : real;
  signal vkq : time;
  signal hsjapybyv : integer;
  signal xwmlyxkda : real;
  signal bqgassdvd : std_logic_vector(3 to 1);
  signal wlxdr : std_logic_vector(0 to 2);
  signal muavufp : integer;
begin
  cqf : entity work.nphzraf
    port map (hgfa => muavufp, dyypoc => wlxdr, bir => bqgassdvd);
  wrs : entity work.hpyyaunk
    port map (hyushehz => xwmlyxkda, bkgmqakhay => hsjapybyv, iiowis => vkq);
  hyhzu : entity work.hpyyaunk
    port map (hyushehz => tvwkoncwvm, bkgmqakhay => rq, iiowis => veflkljoh);
  
  -- Multi-driven assignments
  bqgassdvd <= (others => '0');
  wlxdr <= wlxdr;
  bqgassdvd <= (others => '0');
  bqgassdvd <= bqgassdvd;
end jux;



-- Seed after: 9312089004565772323,2287986438313874379
