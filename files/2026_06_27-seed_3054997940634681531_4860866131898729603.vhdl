-- Seed: 3054997940634681531,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity iue is
  port (ubucaqatb : buffer std_logic; efwr : out integer; fp : buffer integer);
end iue;

architecture tbivyyq of iue is
  
begin
  -- Single-driven assignments
  fp <= 2#1#;
  
  -- Multi-driven assignments
  ubucaqatb <= 'U';
  ubucaqatb <= 'X';
  ubucaqatb <= 'Z';
end tbivyyq;

entity uii is
  port (bsu : buffer severity_level; zjhmjnv : inout time_vector(0 to 1));
end uii;

library ieee;
use ieee.std_logic_1164.all;

architecture lssoyskonh of uii is
  signal shtgxu : integer;
  signal yjbvcmdcmj : integer;
  signal cbmoxoqloy : integer;
  signal h : integer;
  signal foz : std_logic;
begin
  evgfupsn : entity work.iue
    port map (ubucaqatb => foz, efwr => h, fp => cbmoxoqloy);
  jbcrhnc : entity work.iue
    port map (ubucaqatb => foz, efwr => yjbvcmdcmj, fp => shtgxu);
  
  -- Single-driven assignments
  zjhmjnv <= (1 min, 124 ns);
  bsu <= ERROR;
  
  -- Multi-driven assignments
  foz <= 'U';
  foz <= '1';
  foz <= 'X';
  foz <= 'X';
end lssoyskonh;

library ieee;
use ieee.std_logic_1164.all;

entity jp is
  port (dltucjnxf : linkage std_logic);
end jp;

library ieee;
use ieee.std_logic_1164.all;

architecture v of jp is
  signal kfacbysuw : integer;
  signal twghzdh : integer;
  signal bs : std_logic;
  signal gu : integer;
  signal crqle : integer;
  signal ibzui : std_logic;
begin
  lshxcoofzr : entity work.iue
    port map (ubucaqatb => ibzui, efwr => crqle, fp => gu);
  muybxvpfg : entity work.iue
    port map (ubucaqatb => bs, efwr => twghzdh, fp => kfacbysuw);
end v;



-- Seed after: 7793572264706062924,4860866131898729603
