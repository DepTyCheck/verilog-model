-- Seed: 3880284226757402563,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity xhqoitqr is
  port (n : out std_logic; h : in time_vector(1 downto 4); ks : buffer severity_level; p : inout time);
end xhqoitqr;

architecture b of xhqoitqr is
  
begin
  -- Single-driven assignments
  p <= 4_1_4_3_4 ms;
  ks <= WARNING;
  
  -- Multi-driven assignments
  n <= '0';
  n <= 'X';
end b;

entity ztxbilyxf is
  port (zicvvpb : out severity_level);
end ztxbilyxf;

library ieee;
use ieee.std_logic_1164.all;

architecture spgquhu of ztxbilyxf is
  signal bnjtzklc : time;
  signal hrgmbsag : severity_level;
  signal g : time;
  signal wpwoxlt : severity_level;
  signal kg : time_vector(1 downto 4);
  signal x : time;
  signal pynahpagw : time_vector(1 downto 4);
  signal ahjsifv : std_logic;
begin
  feenfr : entity work.xhqoitqr
    port map (n => ahjsifv, h => pynahpagw, ks => zicvvpb, p => x);
  h : entity work.xhqoitqr
    port map (n => ahjsifv, h => kg, ks => wpwoxlt, p => g);
  aiyrvauu : entity work.xhqoitqr
    port map (n => ahjsifv, h => pynahpagw, ks => hrgmbsag, p => bnjtzklc);
  
  -- Single-driven assignments
  kg <= (others => 0 ns);
  pynahpagw <= (others => 0 ns);
  
  -- Multi-driven assignments
  ahjsifv <= '1';
  ahjsifv <= '-';
end spgquhu;

entity ndytcasx is
  port (fhzcupyt : inout time);
end ndytcasx;

library ieee;
use ieee.std_logic_1164.all;

architecture l of ndytcasx is
  signal b : severity_level;
  signal vueefej : severity_level;
  signal bst : time_vector(1 downto 4);
  signal nmgwqx : std_logic;
  signal bv : severity_level;
begin
  sglyfchwy : entity work.ztxbilyxf
    port map (zicvvpb => bv);
  k : entity work.xhqoitqr
    port map (n => nmgwqx, h => bst, ks => vueefej, p => fhzcupyt);
  xiyxfcrf : entity work.ztxbilyxf
    port map (zicvvpb => b);
  
  -- Multi-driven assignments
  nmgwqx <= 'U';
  nmgwqx <= '1';
  nmgwqx <= 'X';
end l;



-- Seed after: 10645063605061242704,10557070023141912087
