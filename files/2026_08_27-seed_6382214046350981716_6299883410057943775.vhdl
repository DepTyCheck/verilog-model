-- Seed: 6382214046350981716,6299883410057943775

entity fdnd is
  port (nvhoezt : linkage boolean; ybihqxqqt : buffer time_vector(4 downto 0); xgml : linkage real);
end fdnd;

architecture chdsjj of fdnd is
  
begin
  -- Single-driven assignments
  ybihqxqqt <= (0 sec, 8#1_4# ps, 16#BE# ms, 3 sec, 3 us);
end chdsjj;

library ieee;
use ieee.std_logic_1164.all;

entity heqfjnas is
  port (wy : in std_logic_vector(0 downto 0); uwipaib : in string(5 downto 3); utrpo : inout std_logic; bfgkkeg : out boolean);
end heqfjnas;

architecture mphrlsp of heqfjnas is
  signal opqltcxa : real;
  signal dmtirozhf : time_vector(4 downto 0);
  signal shbasd : boolean;
  signal pcrbmdheae : real;
  signal xfpvxr : time_vector(4 downto 0);
  signal axipvehzjm : boolean;
begin
  iazeccro : entity work.fdnd
    port map (nvhoezt => axipvehzjm, ybihqxqqt => xfpvxr, xgml => pcrbmdheae);
  azmtnsy : entity work.fdnd
    port map (nvhoezt => shbasd, ybihqxqqt => dmtirozhf, xgml => opqltcxa);
  
  -- Single-driven assignments
  bfgkkeg <= FALSE;
  
  -- Multi-driven assignments
  utrpo <= '1';
  utrpo <= 'X';
  utrpo <= utrpo;
end mphrlsp;



-- Seed after: 4489030924026030249,6299883410057943775
