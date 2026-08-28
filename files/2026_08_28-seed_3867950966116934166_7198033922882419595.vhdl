-- Seed: 3867950966116934166,7198033922882419595

entity jn is
  port (thlwnzw : out real);
end jn;

architecture gwusxdfeba of jn is
  
begin
  -- Single-driven assignments
  thlwnzw <= 421.0_2;
end gwusxdfeba;

entity noayg is
  port (qgixu : inout real; ijezwr : in time);
end noayg;

architecture hthyvozqcs of noayg is
  signal exgoud : real;
  signal ugq : real;
begin
  boeeoolz : entity work.jn
    port map (thlwnzw => ugq);
  jurgqwssz : entity work.jn
    port map (thlwnzw => exgoud);
  l : entity work.jn
    port map (thlwnzw => qgixu);
end hthyvozqcs;

library ieee;
use ieee.std_logic_1164.all;

entity enkg is
  port (oc : out boolean_vector(1 to 0); rhhmv : buffer time; hwp : out std_logic_vector(1 downto 2); oxxhi : buffer std_logic_vector(4 to 1));
end enkg;

architecture doiqph of enkg is
  signal a : real;
  signal wygo : time;
  signal bk : real;
begin
  hlpe : entity work.noayg
    port map (qgixu => bk, ijezwr => wygo);
  xubthhzmj : entity work.jn
    port map (thlwnzw => a);
  
  -- Single-driven assignments
  wygo <= 8#5705# ms;
  
  -- Multi-driven assignments
  oxxhi <= (others => '0');
  oxxhi <= "";
  oxxhi <= (others => '0');
end doiqph;



-- Seed after: 6103942931025604559,7198033922882419595
