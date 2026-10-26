-- Seed: 18145649380721826377,6562834012285671529

library ieee;
use ieee.std_logic_1164.all;

entity kpupxs is
  port (b : buffer std_logic_vector(1 to 1); nrc : linkage bit_vector(1 to 2); h : inout time);
end kpupxs;

architecture mppbjfue of kpupxs is
  
begin
  -- Single-driven assignments
  h <= 22141.23 ps;
  
  -- Multi-driven assignments
  b <= b;
  b <= "0";
  b <= b;
end mppbjfue;

entity ippg is
  port (ukmzal : out bit; nok : buffer real; uxq : buffer real);
end ippg;

library ieee;
use ieee.std_logic_1164.all;

architecture wjq of ippg is
  signal vgu : time;
  signal iz : bit_vector(1 to 2);
  signal kekdhgnzr : std_logic_vector(1 to 1);
begin
  vk : entity work.kpupxs
    port map (b => kekdhgnzr, nrc => iz, h => vgu);
  
  -- Single-driven assignments
  uxq <= uxq;
  ukmzal <= '1';
  nok <= uxq;
end wjq;

entity yjxvvyevsa is
  port (ldjlqvnd : in time);
end yjxvvyevsa;

library ieee;
use ieee.std_logic_1164.all;

architecture lpqislmcj of yjxvvyevsa is
  signal jvb : real;
  signal mruuk : real;
  signal etr : bit;
  signal wpzon : time;
  signal och : bit_vector(1 to 2);
  signal gygdw : std_logic_vector(1 to 1);
begin
  qhois : entity work.kpupxs
    port map (b => gygdw, nrc => och, h => wpzon);
  qkst : entity work.ippg
    port map (ukmzal => etr, nok => mruuk, uxq => jvb);
  
  -- Multi-driven assignments
  gygdw <= (others => '0');
  gygdw <= (others => 'L');
end lpqislmcj;



-- Seed after: 17466198866868719598,6562834012285671529
