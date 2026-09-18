-- Seed: 6116512137392330527,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (kqh : out std_logic_vector(1 to 0); fqmputlj : in integer; hv : in std_logic_vector(2 to 4));
end t;

architecture vsicwme of t is
  
begin
  -- Multi-driven assignments
  kqh <= (others => '0');
  kqh <= (others => '0');
  kqh <= (others => '0');
end vsicwme;

entity uxtpju is
  port (xizoxet : inout integer);
end uxtpju;

library ieee;
use ieee.std_logic_1164.all;

architecture mvu of uxtpju is
  signal vspgbf : std_logic_vector(2 to 4);
  signal rr : integer;
  signal ryngvwot : std_logic_vector(1 to 0);
  signal tjqnfh : std_logic_vector(2 to 4);
  signal rwmi : integer;
  signal jpgdekph : std_logic_vector(1 to 0);
begin
  xuxkxogb : entity work.t
    port map (kqh => jpgdekph, fqmputlj => rwmi, hv => tjqnfh);
  gmibz : entity work.t
    port map (kqh => ryngvwot, fqmputlj => rwmi, hv => tjqnfh);
  ulrskgjzh : entity work.t
    port map (kqh => jpgdekph, fqmputlj => rr, hv => vspgbf);
  
  -- Single-driven assignments
  xizoxet <= xizoxet;
  rr <= xizoxet;
  rwmi <= 2#0_1#;
  
  -- Multi-driven assignments
  jpgdekph <= jpgdekph;
  jpgdekph <= "";
end mvu;



-- Seed after: 11575407159163357957,3316342841050048249
