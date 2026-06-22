-- Seed: 836460265886683365,13479070923501788437

entity magerw is
  port (ngjqsvixy : buffer real; clptponf : linkage integer; fvvgtg : inout real);
end magerw;

architecture jojkifcjgr of magerw is
  
begin
  -- Single-driven assignments
  fvvgtg <= 4.2_0_2_2;
  ngjqsvixy <= 2#0_0_1.0_0_0_0#;
end jojkifcjgr;

library ieee;
use ieee.std_logic_1164.all;

entity aeaf is
  port (n : linkage integer_vector(3 downto 1); obiauis : buffer std_logic_vector(0 downto 0); cxvpxglcs : in integer_vector(1 downto 3));
end aeaf;

architecture imqlicqn of aeaf is
  signal njsnczf : real;
  signal mwrxpuzv : integer;
  signal uonwv : real;
begin
  dptw : entity work.magerw
    port map (ngjqsvixy => uonwv, clptponf => mwrxpuzv, fvvgtg => njsnczf);
end imqlicqn;

entity oslxt is
  port (bfjymwmv : linkage integer);
end oslxt;

architecture d of oslxt is
  
begin
  
end d;

library ieee;
use ieee.std_logic_1164.all;

entity nve is
  port (vem : buffer std_logic_vector(3 downto 3); lez : inout string(5 to 4); dfshmjbc : out boolean_vector(0 to 4); y : in severity_level);
end nve;

library ieee;
use ieee.std_logic_1164.all;

architecture uobxqe of nve is
  signal rqybwpmf : integer;
  signal xyztdngte : real;
  signal kr : integer;
  signal ublbhezjq : real;
  signal emzpbg : integer_vector(1 downto 3);
  signal z : std_logic_vector(0 downto 0);
  signal knkbry : integer_vector(3 downto 1);
begin
  umxzb : entity work.aeaf
    port map (n => knkbry, obiauis => z, cxvpxglcs => emzpbg);
  apg : entity work.magerw
    port map (ngjqsvixy => ublbhezjq, clptponf => kr, fvvgtg => xyztdngte);
  ht : entity work.oslxt
    port map (bfjymwmv => rqybwpmf);
  
  -- Single-driven assignments
  dfshmjbc <= (TRUE, TRUE, TRUE, FALSE, FALSE);
  emzpbg <= (others => 0);
  
  -- Multi-driven assignments
  z <= "1";
end uobxqe;



-- Seed after: 6550490726266620077,13479070923501788437
