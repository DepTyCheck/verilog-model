-- Seed: 6583532816126787252,5306691039457971049

entity kmvxqly is
  port (nrpy : in time_vector(4 to 2); ven : inout real; xtrruyhaeu : inout real_vector(0 downto 0));
end kmvxqly;

architecture bhv of kmvxqly is
  
begin
  
end bhv;

library ieee;
use ieee.std_logic_1164.all;

entity fwu is
  port (lkrvoacv : in bit; ed : buffer std_logic_vector(1 downto 4));
end fwu;

architecture ibxtvlhrbp of fwu is
  signal zsed : real_vector(0 downto 0);
  signal eakgayjtf : real;
  signal yvaeem : time_vector(4 to 2);
begin
  pn : entity work.kmvxqly
    port map (nrpy => yvaeem, ven => eakgayjtf, xtrruyhaeu => zsed);
end ibxtvlhrbp;

entity der is
  port (tgkd : out real; l : out integer; quob : linkage integer_vector(0 downto 1));
end der;

library ieee;
use ieee.std_logic_1164.all;

architecture lsemvtqzhe of der is
  signal o : std_logic_vector(1 downto 4);
  signal vijvkwnxfw : bit;
  signal uclp : real_vector(0 downto 0);
  signal jtzc : real;
  signal z : real_vector(0 downto 0);
  signal pyptmnxk : real;
  signal bzt : time_vector(4 to 2);
begin
  km : entity work.kmvxqly
    port map (nrpy => bzt, ven => pyptmnxk, xtrruyhaeu => z);
  wj : entity work.kmvxqly
    port map (nrpy => bzt, ven => jtzc, xtrruyhaeu => uclp);
  kyqylezgd : entity work.fwu
    port map (lkrvoacv => vijvkwnxfw, ed => o);
  
  -- Single-driven assignments
  l <= l;
  vijvkwnxfw <= '0';
  tgkd <= tgkd;
  bzt <= bzt;
  
  -- Multi-driven assignments
  o <= o;
  o <= o;
end lsemvtqzhe;



-- Seed after: 8128915268715169694,5306691039457971049
