-- Seed: 1301660703542599397,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity xrq is
  port (swb : out std_logic; sbdodd : out std_logic_vector(4 to 0); cp : buffer real);
end xrq;

architecture dtydqwmmjp of xrq is
  
begin
  -- Single-driven assignments
  cp <= 234.4_0_2_0;
  
  -- Multi-driven assignments
  sbdodd <= (others => '0');
  sbdodd <= sbdodd;
end dtydqwmmjp;

entity ypbn is
  port (odjyqjc : linkage time; bbmhxu : out time; xvwwllth : buffer real);
end ypbn;

library ieee;
use ieee.std_logic_1164.all;

architecture ugxhqabywc of ypbn is
  signal jbyzjsrrtn : std_logic_vector(4 to 0);
  signal ix : std_logic;
  signal tikiksdta : real;
  signal fcssypmwg : real;
  signal rqhuvai : std_logic_vector(4 to 0);
  signal xpibqg : std_logic;
  signal ovwqcfiktw : real;
  signal rvje : std_logic_vector(4 to 0);
  signal uhe : std_logic;
begin
  wsrxmrzd : entity work.xrq
    port map (swb => uhe, sbdodd => rvje, cp => ovwqcfiktw);
  v : entity work.xrq
    port map (swb => xpibqg, sbdodd => rqhuvai, cp => fcssypmwg);
  g : entity work.xrq
    port map (swb => uhe, sbdodd => rvje, cp => tikiksdta);
  f : entity work.xrq
    port map (swb => ix, sbdodd => jbyzjsrrtn, cp => xvwwllth);
  
  -- Single-driven assignments
  bbmhxu <= 8#53.11# ps;
end ugxhqabywc;

entity kdt is
  port (bxkdvw : inout real; pdix : linkage real);
end kdt;

library ieee;
use ieee.std_logic_1164.all;

architecture ryxwydl of kdt is
  signal neu : real;
  signal tnvlkmvj : real;
  signal dhwt : std_logic;
  signal uwqgzz : real;
  signal k : std_logic_vector(4 to 0);
  signal ekd : std_logic;
begin
  qax : entity work.xrq
    port map (swb => ekd, sbdodd => k, cp => uwqgzz);
  c : entity work.xrq
    port map (swb => dhwt, sbdodd => k, cp => tnvlkmvj);
  grpttau : entity work.xrq
    port map (swb => ekd, sbdodd => k, cp => neu);
  
  -- Single-driven assignments
  bxkdvw <= 8#00.3_1#;
end ryxwydl;



-- Seed after: 408542841933156023,511364357853360275
